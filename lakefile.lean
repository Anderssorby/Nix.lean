import Lake
open Lake DSL

package Nix {
  srcDir := "src"
}

@[default_target]
lean_lib Nix {
}

-- require LSpec from git
--   "https://github.com/yatima-inc/LSpec" @ "main"

require Cli from git
  "https://github.com/yatima-inc/Cli.lean" @ "main"

require std from git
  "https://github.com/leanprover/std4.git" @ "main"

require Megaparsec from git
  "https://github.com/anderssorby/Megaparsec.lean.git" @ "main"

require YatimaStdLib from git
  "https://github.com/anderssorby/YatimaStdLib.lean" @ "main"


@[default_target]
lean_exe nix {
  root := `Nix.Cli
}

lean_exe test.Tests
