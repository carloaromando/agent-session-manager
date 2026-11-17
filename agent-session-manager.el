;;; agent-session-manager.el --- Manage agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Carlo Aromando
;; SPDX-License-Identifier: BSD-3-Clause
;; Author: Carlo Aromando
;; Package-Requires: ((emacs "29.1") (agent-shell "0.60.0"))

;;; Commentary:
;;
;; An ibuffer-style list of agent-shell sessions, grouped by project.
;;
;; Open sessions come from `agent-shell-buffers'.  Closed ones come from a
;; per-agent backend; today only pi is registered, reading the session map
;; that the pi-acp adapter keeps.  The two lists are merged on the ACP
;; session id, and the open side always wins.
;;
;; RET opens an open session or resumes a closed one, `r' renames, `d'/`x'
;; delete, `k' kills a buffer without touching disk, `g' refreshes.
;;
;; Names you give sessions live in `agent-session-manager-names-file'.
;; Nothing is ever written into the agent's own data.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'iso8601)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function projectile-project-root "projectile")
(declare-function shell-maker-set-buffer-name "shell-maker")

(defgroup agent-session-manager nil
  "Manage agent-shell sessions."
  :group 'tools)


;;;; Customization

(defcustom agent-session-manager-names-file
  (locate-user-emacs-file "agent-session-manager.eld")
  "File holding the names given to sessions, keyed by ACP session id."
  :type 'file
  :group 'agent-session-manager)

(defcustom agent-session-manager-pi-map-file
  (expand-file-name "~/.pi/pi-acp/session-map.json")
  "Session map written by the pi-acp adapter."
  :type 'file
  :group 'agent-session-manager)

(defcustom agent-session-manager-free-group-name "Free"
  "Heading for sessions that belong to no project."
  :type 'string
  :group 'agent-session-manager)

(defcustom agent-session-manager-name-max-width 48
  "Width at which a derived session name is truncated.
Keep it below the Name column so long names do not push the columns that
follow out of line."
  :type 'integer
  :group 'agent-session-manager)

(defconst agent-session-manager--scan-max-bytes 262144
  "How much of a session file to read when deriving its name.")

(defconst agent-session-manager--scan-max-lines 500
  "How many lines of a session file to scan when deriving its name.")


;;;; Faces

(defface agent-session-manager-group
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for project headings.")

(defface agent-session-manager-status
  '((((background light)) :background "grey88")
    (((background dark)) :background "grey25"))
  "Quiet badge behind every status, whatever the status says.")

(defface agent-session-manager-ready
  '((t :inherit (success agent-session-manager-status)))
  "Face for sessions waiting for your next prompt.")

(defface agent-session-manager-busy
  '((t :inherit (warning agent-session-manager-status)))
  "Face for sessions the agent is working on.")

(defface agent-session-manager-blocked
  '((t :inherit (error agent-session-manager-status)))
  "Face for sessions waiting on a permission answer.")

(defface agent-session-manager-starting
  '((t :inherit (shadow agent-session-manager-status) :slant italic))
  "Face for shells that have no session id yet.")

(defface agent-session-manager-closed
  '((t :inherit (shadow agent-session-manager-status)))
  "Face for sessions with no live buffer.")

(defface agent-session-manager-missing
  '((t :inherit (shadow agent-session-manager-status) :strike-through t))
  "Face for the status of a session whose file is gone.")

(defface agent-session-manager-missing-name
  '((t :inherit shadow :strike-through t))
  "Face for the name of a session whose file is gone.
Carries no badge: that belongs to the status cell alone.")


;;;; Backends
;;
;; A backend knows how to reach one agent's closed sessions.  It is a plist
;; with four functions:
;;
;;   :sessions     no arguments, returns a list of session alists carrying
;;                 :id, :cwd, :file, :created and :updated
;;   :resume       called with a session, resumes it
;;   :delete       called with a session, removes it from disk
;;   :default-name called with a session, returns a name or nil

(defvar agent-session-manager-backends nil
  "Alist of agent identifier to backend plist.")

(defun agent-session-manager--backend (agent)
  "Return the backend registered for AGENT, or nil."
  (alist-get agent agent-session-manager-backends))


;;;; pi backend

(defun agent-session-manager--read-json (file)
  "Parse FILE as JSON, or return nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case err
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil)
        (error
         (error "Cannot read %s: %s" file (error-message-string err)))))))

(defun agent-session-manager--parse-time (string)
  "Return STRING parsed as an ISO 8601 time, or nil."
  (when (and string (not (string-empty-p string)))
    (encode-time (iso8601-parse string))))

(defun agent-session-manager--pi-sessions ()
  "Return the sessions pi-acp knows about."
  (let ((entries (alist-get 'sessions
                            (agent-session-manager--read-json
                             agent-session-manager-pi-map-file))))
    (mapcar
     (lambda (entry)
       (let* ((session (cdr entry))
              (file (alist-get 'sessionFile session)))
         (list (cons :id (alist-get 'sessionId session))
               (cons :agent 'pi)
               (cons :cwd (alist-get 'cwd session))
               (cons :file file)
               (cons :created (agent-session-manager--pi-created file))
               (cons :updated (agent-session-manager--parse-time
                               (alist-get 'updatedAt session))))))
     entries)))

(defvar agent-session-manager--pi-scan-cache (make-hash-table :test #'equal)
  "Scan results keyed by session file and its modification time.")

(defun agent-session-manager--pi-scan-1 (file)
  "Scan FILE for the facts we show: its creation time and its opening prompt.

Only the first `agent-session-manager--scan-max-bytes' bytes are read,
and the scan stops as soon as both facts are in hand, so a long transcript
costs the same as a short one.  The last line of the prefix is dropped
because a byte cut lands mid-line.  Returns an alist of :created and :name,
either of which may be nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 agent-session-manager--scan-max-bytes)
    (goto-char (point-max))
    (unless (bolp)
      (delete-region (line-beginning-position) (point-max)))
    (goto-char (point-min))
    (let ((created nil)
          (name nil)
          (lines 0))
      (while (and (not (eobp))
                  (not (and created name))
                  (< lines agent-session-manager--scan-max-lines))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p line)
            ;; We are after two specific records.  A line we cannot read is
            ;; skipped rather than fatal: the worst case is a session shown
            ;; without its date or its derived name.
            (when-let* ((record (ignore-error json-error
                                  (json-parse-string line
                                                     :object-type 'alist
                                                     :array-type 'list
                                                     :null-object nil
                                                     :false-object nil)))
                        (type (alist-get 'type record)))
              (cond
               ((and (null created) (equal type "session"))
                (setq created (agent-session-manager--parse-time
                               (alist-get 'timestamp record))))
               ((and (null name)
                     (equal type "message")
                     (equal (map-nested-elt record '(message role)) "user"))
                (setq name (agent-session-manager--trim-name
                            (seq-some (lambda (part)
                                        (and (equal (alist-get 'type part) "text")
                                             (alist-get 'text part)))
                                      (map-nested-elt record '(message content))))))))))
        (setq lines (1+ lines))
        (forward-line 1))
      (list (cons :created created)
            (cons :name name)))))

(defun agent-session-manager--pi-scan (file)
  "Return the cached scan of FILE, or nil when it cannot be read."
  (when (and file (file-readable-p file))
    (let ((key (cons file (file-attribute-modification-time
                           (file-attributes file)))))
      (or (gethash key agent-session-manager--pi-scan-cache)
          (puthash key (agent-session-manager--pi-scan-1 file)
                   agent-session-manager--pi-scan-cache)))))

(defun agent-session-manager--pi-created (file)
  "Return the time FILE's session was created, or nil."
  (alist-get :created (agent-session-manager--pi-scan file)))

(defun agent-session-manager--pi-default-name (session)
  "Return the first thing the user said in SESSION, or nil."
  (alist-get :name (agent-session-manager--pi-scan
                    (alist-get :file session))))

(defun agent-session-manager--pi-resume (session)
  "Resume SESSION."
  (agent-shell-resume-session (alist-get :id session)))

(defun agent-session-manager--pi-delete (session)
  "Delete SESSION: its file to trash, then its entry in the map."
  (when-let* ((file (alist-get :file session)))
    (when (file-exists-p file)
      ;; Bound here so the session goes to the trash whatever the user's
      ;; global setting is: losing a transcript should take two steps.
      (let ((delete-by-moving-to-trash t))
        (delete-file file t))))
  (let* ((map-file agent-session-manager-pi-map-file)
         (json (agent-session-manager--read-json map-file))
         (id (alist-get :id session)))
    (when json
      (setf (alist-get 'sessions json)
            (seq-remove (lambda (entry)
                          (equal (alist-get 'sessionId (cdr entry)) id))
                        (alist-get 'sessions json)))
      (with-temp-file map-file
        (insert (json-serialize json :null-object nil :false-object nil))))))

(setf (alist-get 'pi agent-session-manager-backends)
      (list :sessions #'agent-session-manager--pi-sessions
            :resume #'agent-session-manager--pi-resume
            :delete #'agent-session-manager--pi-delete
            :default-name #'agent-session-manager--pi-default-name))


;;;; Names you give sessions

(defvar agent-session-manager--pending-names (make-hash-table :test #'eq)
  "Names given to sessions that had no id yet, keyed by buffer.")

(defun agent-session-manager--read-names ()
  "Return the saved names as an alist of id to name."
  (let ((file agent-session-manager-names-file))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case err
            (read (current-buffer))
          (error
           (error "Cannot read %s: %s" file (error-message-string err))))))))

(defun agent-session-manager--write-names (names)
  "Write NAMES to `agent-session-manager-names-file'."
  (let ((file agent-session-manager-names-file))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";; Session names for agent-session-manager.  Edited by Emacs.\n")
      (prin1 names (current-buffer))
      (insert "\n"))))

(defun agent-session-manager--saved-name (id)
  "Return the name saved for ID, or nil."
  (when id
    (alist-get id (agent-session-manager--read-names) nil nil #'equal)))

(defun agent-session-manager--save-name (id name)
  "Save NAME for ID, or drop the entry when NAME is nil."
  (let ((names (agent-session-manager--read-names)))
    (setf (alist-get id names nil t #'equal) name)
    (agent-session-manager--write-names names)))


;;;; Reading sessions

(defun agent-session-manager--trim-name (text)
  "Return TEXT's first line, trimmed to a readable width."
  (let ((line (string-trim (car (split-string (or text "") "\n")))))
    (cond
     ((string-empty-p line) nil)
     ((> (length line) agent-session-manager-name-max-width)
      (concat (substring line 0 (- agent-session-manager-name-max-width 3))
              "..."))
     (t line))))

(defun agent-session-manager--shell-title (buffer)
  "Return the session title agent-shell tracks for BUFFER, or nil.
This is the one place we read agent-shell's internal state: there is no
public accessor for the title.  It breaks loudly if upstream changes."
  (agent-session-manager--trim-name
   (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                   '(:session :title))))

(defun agent-session-manager--project-root (directory)
  "Return the project root holding DIRECTORY, or nil when there is none."
  (when (and directory (file-directory-p directory))
    (let ((default-directory (file-name-as-directory directory)))
      (cond
       ((and (bound-and-true-p projectile-mode)
             (fboundp 'projectile-project-root))
        (projectile-project-root))
       (t
        (when-let* ((project (project-current nil default-directory)))
          (project-root project)))))))

(defun agent-session-manager--open-sessions ()
  "Return a session alist for every live agent-shell buffer."
  (mapcar
   (lambda (buffer)
     (let ((id (agent-shell-session-id :shell-buffer buffer))
           (cwd (buffer-local-value 'default-directory buffer)))
       (list (cons :id id)
             (cons :buffer buffer)
             (cons :agent (map-elt (agent-shell-get-config buffer) :identifier))
             (cons :cwd cwd)
             (cons :status (if id
                               (agent-shell-status :shell-buffer buffer)
                             'starting))
             (cons :title (agent-session-manager--shell-title buffer)))))
   (agent-shell-buffers)))

(defun agent-session-manager--closed-sessions ()
  "Return the sessions every backend knows about."
  (seq-mapcat
   (lambda (entry)
     (funcall (plist-get (cdr entry) :sessions)))
   agent-session-manager-backends))

(defun agent-session-manager--merge (open closed)
  "Merge OPEN and CLOSED sessions on their id.

An open session keeps its buffer, status and title, and takes the dates
and the file path from the stored side when they are there.  A live
session is on disk too from the moment the agent opens it, so its dates
show while you work, not only once it is closed."
  (let ((stored (make-hash-table :test #'equal))
        (open-ids nil))
    (dolist (session closed)
      (puthash (alist-get :id session) session stored))
    (dolist (session open)
      (when-let* ((id (alist-get :id session)))
        (push id open-ids)))
    (append
     (mapcar
      (lambda (session)
        (if-let* ((id (alist-get :id session))
                  (disk (gethash id stored)))
            ;; Appended, so the keys the open side already carries win.
            (append session
                    (list (cons :file (alist-get :file disk))
                          (cons :created (alist-get :created disk))
                          (cons :updated (alist-get :updated disk))))
          session))
      open)
     (seq-remove (lambda (session)
                   (member (alist-get :id session) open-ids))
                 closed))))

(defun agent-session-manager--status (session)
  "Return the status to show for SESSION."
  (or (alist-get :status session)
      (if (and (alist-get :file session)
               (not (file-exists-p (alist-get :file session))))
          'missing
        'closed)))

(defun agent-session-manager--default-name (session)
  "Return the name SESSION's backend derives for it, or nil."
  (when-let* ((backend (agent-session-manager--backend
                        (alist-get :agent session)))
              (derive (plist-get backend :default-name)))
    (funcall derive session)))

(defun agent-session-manager--sessions ()
  "Return every session, decorated and grouped, ready to print."
  (let* ((sessions (agent-session-manager--merge
                    (agent-session-manager--open-sessions)
                    (agent-session-manager--closed-sessions)))
         (counters (make-hash-table :test #'equal)))
    (mapcar
     (lambda (session)
       (let* ((id (alist-get :id session))
              (root (agent-session-manager--project-root
                     (alist-get :cwd session)))
              (group (if root
                         (file-name-nondirectory (directory-file-name root))
                       agent-session-manager-free-group-name))
              (name (or (agent-session-manager--saved-name id)
                        (alist-get :title session)
                        (agent-session-manager--default-name session)
                        (let ((n (1+ (gethash group counters 0))))
                          (puthash group n counters)
                          (format "New Session %d" n)))))
         (append (list (cons :root root)
                       (cons :group group)
                       (cons :name name)
                       (cons :status (agent-session-manager--status session)))
                 session)))
     sessions)))


;;;; Printing

(defun agent-session-manager--group-p (id)
  "Return non-nil when tabulated list ID is a project heading."
  (and (consp id) (eq (car id) 'group)))

(defun agent-session-manager--status-face (status)
  "Return the face for STATUS."
  (pcase status
    ('ready 'agent-session-manager-ready)
    ('busy 'agent-session-manager-busy)
    ('blocked 'agent-session-manager-blocked)
    ('starting 'agent-session-manager-starting)
    ('closed 'agent-session-manager-closed)
    ('missing 'agent-session-manager-missing)))

(defun agent-session-manager--format-time (time)
  "Return TIME as a short stamp, or a dash when it is nil."
  (if time (format-time-string "%Y-%m-%d %H:%M" time) "-"))

(defun agent-session-manager--columns (session)
  "Return the printed columns of SESSION."
  (let* ((status (alist-get :status session))
         (name (alist-get :name session)))
    (vector
     (propertize (symbol-name status)
                 'face (agent-session-manager--status-face status))
     (if (eq status 'missing)
         (propertize name 'face 'agent-session-manager-missing-name)
       name)
     (format "%s" (or (alist-get :agent session) "-"))
     (agent-session-manager--format-time (alist-get :created session))
     (agent-session-manager--format-time (alist-get :updated session)))))

(defun agent-session-manager--sort-key (session)
  "Return the string SESSION sorts on inside its group."
  (downcase (alist-get :name session)))

(defvar-local agent-session-manager--collapsed nil
  "Names of the project headings folded shut in this buffer.")

(defun agent-session-manager--collapsed-p (group)
  "Return non-nil when GROUP is folded shut."
  (member group agent-session-manager--collapsed))

(defun agent-session-manager--entries ()
  "Return tabulated list entries, project headings included."
  (let* ((sessions (agent-session-manager--sessions))
         (groups (seq-uniq (mapcar (lambda (s) (alist-get :group s)) sessions)))
         (free agent-session-manager-free-group-name)
         (ordered (append
                   (sort (seq-remove (lambda (g) (equal g free)) groups)
                         #'string-lessp)
                   (seq-filter (lambda (g) (equal g free)) groups)))
         (entries nil))
    (dolist (group ordered)
      (let ((collapsed (agent-session-manager--collapsed-p group)))
        (push (list (cons 'group group)
                    (vector (concat (if collapsed "\u25b6" "\u25bc") " " group)
                            "" "" "" ""))
              entries)
        (unless collapsed
          (dolist (session (sort (seq-filter
                                  (lambda (s) (equal (alist-get :group s) group))
                                  sessions)
                                 (lambda (a b)
                                   (string-lessp
                                    (agent-session-manager--sort-key a)
                                    (agent-session-manager--sort-key b)))))
            (push (list session (agent-session-manager--columns session))
                  entries)))))
    (nreverse entries)))

(defun agent-session-manager--print-entry (id columns)
  "Print entry ID with COLUMNS, drawing project headings our own way.

A heading sits at column zero, left of its sessions, which keep the two
columns `tabulated-list-padding' reserves for their delete mark.  Every
heading but the first gets a blank line above it."
  (if (agent-session-manager--group-p id)
      (let ((start (point)))
        (unless (= (point) (point-min))
          (insert "\n"))
        (insert (propertize (aref columns 0)
                            'face 'agent-session-manager-group))
        (insert "\n")
        (put-text-property start (point) 'tabulated-list-id id))
    (tabulated-list-print-entry id columns)))


;;;; Commands

(defun agent-session-manager--session-at-point ()
  "Return the session on this line, or signal when there is none."
  (let ((id (tabulated-list-get-id)))
    (when (or (null id) (agent-session-manager--group-p id))
      (user-error "Not on a session"))
    id))

(defun agent-session-manager--agent-config (identifier)
  "Return the known agent configuration whose identifier is IDENTIFIER."
  (let ((entries (if (functionp agent-shell-agent-configs)
                     (funcall agent-shell-agent-configs)
                   agent-shell-agent-configs)))
    (seq-find (lambda (config)
                (eq (map-elt config :identifier) identifier))
              (mapcar (lambda (entry)
                        (if (functionp entry) (funcall entry) entry))
                      entries))))

(defun agent-session-manager-refresh ()
  "Rebuild the list from agent-shell and from disk."
  (interactive)
  (setq tabulated-list-entries (agent-session-manager--entries))
  (tabulated-list-print t t))

(defun agent-session-manager-toggle-group ()
  "Fold or unfold the project heading on this line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless (agent-session-manager--group-p id)
      (user-error "Not on a project"))
    (let ((group (cdr id)))
      (if (agent-session-manager--collapsed-p group)
          (setq agent-session-manager--collapsed
                (delete group agent-session-manager--collapsed))
        (push group agent-session-manager--collapsed)))
    (agent-session-manager-refresh)))

(defun agent-session-manager--resume (session &optional ask)
  "Resume SESSION, asking which agent to use when ASK is non-nil.

Without ASK the session reopens under the agent that produced it, which
is the agent whose backend listed it.  `agent-shell-resume-session' would
otherwise put up a picker, so we hand it the configuration ourselves."
  (let* ((root (alist-get :root session))
         (cwd (alist-get :cwd session))
         (directory (or root cwd))
         (agent (alist-get :agent session))
         (config (unless ask (agent-session-manager--agent-config agent))))
    (unless (and directory (file-directory-p directory))
      (user-error "Directory is gone: %s" (or directory "unknown")))
    (when (and (not ask) (not config))
      (user-error "No agent configuration for %s; use M-RET to pick one" agent))
    (let ((default-directory (file-name-as-directory directory)))
      ;; The only lever agent-shell gives us over its picker.  It breaks
      ;; loudly if upstream renames it.
      (cl-letf (((symbol-function 'agent-shell--auto-preferred-config)
                 (lambda (&rest _) config)))
        (agent-shell-resume-session (alist-get :id session))))))

(defun agent-session-manager-visit ()
  "Act on this line: fold a project, open a session, resume a closed one."
  (interactive)
  (if (agent-session-manager--group-p (tabulated-list-get-id))
      (agent-session-manager-toggle-group)
    (let* ((session (agent-session-manager--session-at-point))
           (buffer (alist-get :buffer session))
           (status (alist-get :status session)))
      (cond
       ((buffer-live-p buffer)
        (pop-to-buffer buffer))
       ((eq status 'missing)
        (user-error "Session file is gone (%s); remove the entry with d then x"
                    (alist-get :file session)))
       ((eq status 'starting)
        (user-error "Session has no id yet"))
       (t
        (agent-session-manager--resume session))))))

(defun agent-session-manager-visit-with-agent ()
  "Resume the session on this line, picking the agent yourself."
  (interactive)
  (let ((session (agent-session-manager--session-at-point)))
    (when (buffer-live-p (alist-get :buffer session))
      (user-error "Session is already open"))
    (when (memq (alist-get :status session) '(missing starting))
      (user-error "Nothing to resume"))
    (agent-session-manager--resume session t)))

(defun agent-session-manager--default-directory-at-point ()
  "Return a sensible directory to start the file prompt from."
  (let ((id (tabulated-list-get-id)))
    (if (and id (not (agent-session-manager--group-p id)))
        (or (alist-get :root id) (alist-get :cwd id) default-directory)
      default-directory)))

(defun agent-session-manager--new (file ask)
  "Start a session on FILE, asking which agent to use when ASK is non-nil.

The shell opens at FILE's project root when projectile knows one, so the
session lands in that project; a file outside any project opens in its own
directory and lands under `agent-session-manager-free-group-name'.
Either way FILE itself is handed to the agent as the opening context.

Left alone, agent-shell picks the agent named by
`agent-shell-preferred-agent-config' and only puts up its picker when that
is unset.  ASK takes the preference out of the way so the picker always
appears."
  (let* ((file (expand-file-name file))
         (directory (if (file-directory-p file)
                        file
                      (file-name-directory file)))
         (root (agent-session-manager--project-root directory))
         (manager (current-buffer))
         (before (agent-shell-buffers)))
    (unless (file-exists-p file)
      (user-error "No such file: %s" file))
    (let ((default-directory (file-name-as-directory (or root directory)))
          ;; `agent-shell' with a prefix argument is DWIM: before starting
          ;; the shell it also collects context from the current buffer.
          ;; Here that buffer is the session list, so its line at point
          ;; would be inserted next to the @FILE we add below.  We want no
          ;; context from the list; FILE is the only context.
          (agent-shell-context-sources nil))
      (if ask
          ;; The only lever agent-shell gives us over its picker.  It breaks
          ;; loudly if upstream renames it.
          (cl-letf (((symbol-function 'agent-shell--auto-preferred-config)
                     (lambda (&rest _) nil)))
            (agent-shell '(4)))
        (agent-shell '(4))))
    (when-let* ((shell (seq-find (lambda (buffer) (not (memq buffer before)))
                                 (agent-shell-buffers))))
      ;; The only way to build the reference the agent expects.  It breaks
      ;; loudly if upstream changes it.
      (agent-shell-insert :text (agent-shell--get-files-context :files (list file))
                          :shell-buffer shell))
    (when (buffer-live-p manager)
      (with-current-buffer manager
        (agent-session-manager-refresh)))))

(defun agent-session-manager-new (file)
  "Start a session on FILE with your preferred agent."
  (interactive
   (list (read-file-name
          "New session on file: "
          (agent-session-manager--default-directory-at-point)
          nil t)))
  (agent-session-manager--new file nil))

(defun agent-session-manager-new-with-agent (file)
  "Start a session on FILE, picking the agent yourself."
  (interactive
   (list (read-file-name
          "New session on file (pick agent): "
          (agent-session-manager--default-directory-at-point)
          nil t)))
  (agent-session-manager--new file t))

(defun agent-session-manager-rename (name)
  "Give the session on this line the NAME you type."
  (interactive
   (list (read-string "Session name: "
                      (alist-get :name (agent-session-manager--session-at-point)))))
  (let* ((session (agent-session-manager--session-at-point))
         (id (alist-get :id session))
         (buffer (alist-get :buffer session))
         (name (string-trim name)))
    (when (string-empty-p name)
      (user-error "Name cannot be empty"))
    (when (buffer-live-p buffer)
      (shell-maker-set-buffer-name buffer name))
    (cond
     (id
      (agent-session-manager--save-name id name))
     ((buffer-live-p buffer)
      (puthash buffer name agent-session-manager--pending-names)
      (message "Named the buffer; saving to disk once the session gets an id. Close it first and the name is lost")))
    (agent-session-manager-refresh)))

(defun agent-session-manager--flush-pending-names ()
  "Save names given to sessions that have since received an id."
  (maphash
   (lambda (buffer name)
     (if (not (buffer-live-p buffer))
         (remhash buffer agent-session-manager--pending-names)
       (when-let* ((id (agent-shell-session-id :shell-buffer buffer)))
         (agent-session-manager--save-name id name)
         (remhash buffer agent-session-manager--pending-names))))
   agent-session-manager--pending-names))

(defun agent-session-manager-copy-id ()
  "Copy the ACP id of the session on this line."
  (interactive)
  (let ((id (alist-get :id (agent-session-manager--session-at-point))))
    (unless id
      (user-error "Session has no id yet"))
    (kill-new id)
    (message "Copied %s" id)))

(defun agent-session-manager-kill-buffer ()
  "Kill the buffer of the session on this line, leaving disk alone."
  (interactive)
  (let ((buffer (alist-get :buffer (agent-session-manager--session-at-point))))
    (unless (buffer-live-p buffer)
      (user-error "Session has no buffer"))
    (kill-buffer buffer)
    (agent-session-manager-refresh)))

(defun agent-session-manager-mark-delete ()
  "Mark the session on this line for deletion."
  (interactive)
  (agent-session-manager--session-at-point)
  (tabulated-list-put-tag "D" t))

(defun agent-session-manager-unmark ()
  "Remove the mark from this line."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun agent-session-manager--marked ()
  "Return the sessions currently marked for deletion."
  (let ((marked nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (let ((id (tabulated-list-get-id)))
            (when (and id (not (agent-session-manager--group-p id)))
              (push id marked))))
        (forward-line 1)))
    (nreverse marked)))

(defun agent-session-manager-execute ()
  "Delete every marked session, after one confirmation."
  (interactive)
  (let ((marked (agent-session-manager--marked)))
    (unless marked
      (user-error "Nothing marked"))
    (when (seq-find (lambda (s) (null (alist-get :id s))) marked)
      (user-error "A marked session has no id yet; unmark it with u"))
    (unless (yes-or-no-p
             (format "Delete %d session(s)? %s "
                     (length marked)
                     (mapconcat (lambda (s) (alist-get :name s)) marked ", ")))
      (user-error "Nothing deleted"))
    (dolist (session marked)
      (let ((buffer (alist-get :buffer session))
            (backend (agent-session-manager--backend
                      (alist-get :agent session))))
        (unless backend
          (error "No backend for agent %s" (alist-get :agent session)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (funcall (plist-get backend :delete) session)
        (agent-session-manager--save-name (alist-get :id session) nil)))
    (message "Deleted %d session(s)" (length marked))
    (agent-session-manager-refresh)))


;;;; Mode

(defvar-keymap agent-session-manager-mode-map
  :doc "Keymap for `agent-session-manager-mode'."
  "RET" #'agent-session-manager-visit
  "M-RET" #'agent-session-manager-visit-with-agent
  "TAB" #'agent-session-manager-toggle-group
  "n" #'agent-session-manager-new
  "M-n" #'agent-session-manager-new-with-agent
  "r" #'agent-session-manager-rename
  "d" #'agent-session-manager-mark-delete
  "u" #'agent-session-manager-unmark
  "x" #'agent-session-manager-execute
  "k" #'agent-session-manager-kill-buffer
  "w" #'agent-session-manager-copy-id
  "g" #'agent-session-manager-refresh
  "q" #'quit-window)

(define-derived-mode agent-session-manager-mode tabulated-list-mode
  "Agent Sessions"
  "Mode for the agent session list."
  ;; Columns are not sortable: sorting would scatter the project headings,
  ;; which are entries like any other.
  (setq tabulated-list-format
        [("Status" 10 nil)
         ("Name" 50 nil)
         ("Agent" 10 nil)
         ("Created" 17 nil)
         ("Updated" 16 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local tabulated-list-printer #'agent-session-manager--print-entry)
  (tabulated-list-init-header))

;;;###autoload
(defun agent-session-manager ()
  "Show the agent session list."
  (interactive)
  (let ((buffer (get-buffer-create "*agent sessions*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-session-manager-mode)
        (agent-session-manager-mode))
      (agent-session-manager--flush-pending-names)
      (agent-session-manager-refresh))
    (pop-to-buffer buffer)))

(provide 'agent-session-manager)

;;; agent-session-manager.el ends here
