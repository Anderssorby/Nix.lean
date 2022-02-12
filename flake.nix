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
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        leanPkgs = lean.packages.${system};
        pkgs = nixpkgs.legacyPackages.${system};
        name = "Nix";  # must match the name of the top-level .lean file
        project = leanPkgs.buildLeanPackage {
          inherit name;
          # deps = with leanPkgs; [ Init Lean ];#lean-ipld.project.${system} ];
          # Where the lean files are located
          src = ./src;
        };
        cli = leanPkgs.buildLeanPackage {
          name = "Nix.Cli";
          deps = [ project ];
          src = ./src;
        };
        test = leanPkgs.buildLeanPackage {
          name = "Tests";
          deps = [ project ];
          # Where the lean files are located
          src = ./test;
        };
        joinDepsDerivations = getSubDrv:
          pkgs.lib.concatStringsSep ":" (map (d: (builtins.tryEval "${getSubDrv d}").value) (project.allExternalDeps));
      in
      {
        inherit project test;
        packages = {
          ${name} = project.sharedLib;
          inherit (project) lean-package print-paths;
          inherit (leanPkgs) lean;
          cli = cli.executable;
          test = test.executable;
        };

        checks.test = test.executable;

        defaultPackage = self.packages.${system}.cli;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            leanPkgs.lean-dev
          ];
          LEAN_PATH = "./src:./test";
          LEAN_SRC_PATH = "./src:./test";
        };
      });
}
