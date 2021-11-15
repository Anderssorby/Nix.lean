{
  description = "A Nix implementation in Lean";

  inputs = {
    lean = {
      url = github:yatima-inc/lean4/acs/add-nix-ability-for-native-libs;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # A lean dependency
    lean-ipld.url = github:yatima-inc/lean-ipld;
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
          deps = with leanPkgs; [ Init Lean ];#lean-ipld.project.${system} ];
          # Where the lean files are located
          src = ./src;
        };
        test = leanPkgs.buildLeanPackage {
          name = "Tests";
          deps = [ project ];
          # Where the lean files are located
          src = ./test;
        };
      in
      {
        inherit project test;
        packages = {
          ${name} = project.executable;
        };

        checks.test = test.executable;

        defaultPackage = self.packages.${system}.${name};
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            leanPkgs.lean
          ];
          LEAN_PATH = "${leanPkgs.Lean.modRoot}";
        };
      });
}
