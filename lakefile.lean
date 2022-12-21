import Lake
open Lake DSL

package Nix

@[default_target]
lean_lib Nix

require Megaparsec from git
  "https://github.com/yatima-inc/Megaparsec.lean.git" @ "main"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "f905b68f529de2af44cf6ea63489b7e3cd090050"


@[default_target]
lean_exe nix {
  root := "Nix.Cli"
}

-- lean_exe Tests.Parsec
