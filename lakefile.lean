import Lake
open Lake DSL

package Nix {
  srcDir := "src"
}

@[default_target]
lean_lib Nix {
}

-- require LSpec from git
--   "https://github.com/yatima-inc/LSpec" @ "88f7d23e56a061d32c7173cea5befa4b2c248b41"

require std from git
  "https://github.com/leanprover/std4.git" @ "main"

require Megaparsec from git
  "https://github.com/yatima-inc/Megaparsec.lean.git" @ "main"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "f905b68f529de2af44cf6ea63489b7e3cd090050"


@[default_target]
lean_exe nix {
  root := "Nix.Cli"
}

-- lean_exe Tests.Parsec
