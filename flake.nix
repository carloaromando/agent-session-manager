{
  description = "agent-session-manager";

  outputs = { self }: {
    overlays.default = final: prev: {
      agent-session-manager = final.callPackage ./default.nix { };
    };
  };
}
