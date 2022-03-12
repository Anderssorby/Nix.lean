import Nix
import Nix.Utils

open Nix Nix.Derivation Parser System
namespace Tests.Derivation

#print "Testing Derivation.Parser"

#eval (listOf stringLitteral).parse "[\"a\"]"
#eval derivation.parse "Derive([(\"out\", \"\", \"\", \"\")], [(\"input\", [\"\"])],[\"srcs\"], \"x86_64-linux\", \"builder\", [\"args\"],[(\"ENV\",\"val\")])"

def tests : IO Unit := do
  let files ← (FilePath.mk "test/drv").findAllWithExt "drv"
  let srcs ← files.mapM (λ fp => do pure (fp, ←IO.FS.readFile fp))
  for (fp, src) in srcs do
    println! "Parsing {fp}"
    match Expression.parse src with
    | Except.ok expr => IO.println <| ToString.toString expr
    | Except.error e => IO.eprintln s!"Error: {e}"
end Tests.Derivation
