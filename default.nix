{ lib, trivialBuild, agent-shell }:

trivialBuild {
  pname = "agent-session-manager";
  version = "0.1.0";

  src = ./agent-session-manager.el;

  packageRequires = [ agent-shell ];

  meta = {
    description = "ibuffer-style list of agent-shell sessions, grouped by project";
    license = lib.licenses.bsd3;
  };
}
