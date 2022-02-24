import Nix
import Nix.Utils

open Nix
open System

open Nix.Expression.Parser
#print "Testing Expression.Parser"
#eval letStatement.parse "let\n a = \"\";\n in a"

#eval lambda.parse "arg: expr"
#eval lambda.parse "{a ? 1}: a"
#eval lambda.parse "{a ? 1, b, ...}: a"

#eval stringInterpolation.parse "${test}"
#eval expression.parse "left.right {a}: a.b"
#eval expression.parse "a.${b} c"
#eval expression.parse "flake.utils b c (d) (a: a + b)"
#eval file.parse "flake-utils.lib.eachSystem supportedSystems (system: system)"
 
def main (args : List String) : IO UInt32 := do
  try
    let files ← (FilePath.mk "test/nix").findAllWithExt "nix"
    let srcs ← files.mapM (λ fp => do pure (fp, ←IO.FS.readFile fp))
    for (fp, src) in srcs do
      println! "Parsing {fp}"
      match Expression.parse src with
      | Except.ok expr => IO.println <| ToString.toString expr
      | Except.error e => IO.eprintln s!"Error: {e}"
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e
    pure 1
