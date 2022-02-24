{
  description = "A Nix implementation in Lean";

  inputs = {
    lean = {
      url = "github:leanprover/lean4";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # A lean dependency
    lean-ipld = {
      url = "github:yatima-inc/lean-ipld";
      inputs.lean.follows = "lean";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, lean, flake-utils, nixpkgs, lean-ipld }:
    let
      supportedSystems = [
        # "aarch64-linux"
        # "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems
    #    (system:
    #   {}
    # )
    ;
}
