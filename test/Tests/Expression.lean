import Nix
import Nix.Utils

open Nix Nix.Expression Nix.Expression.Parser System

namespace Tests.Expression

#print "Testing Expression.Parser"
#eval letStatement.parse "let\n a = \"\";\n in a"
#eval num.parse "1"

#eval comments.parse " #Test\n #test2  t"
#eval expression.parse " #Test\n #test2  t\ni+#a\n2 * a" 

#eval lambda.parse "arg: expr"
#eval lambda.parse "{a ? 1}: a"
#eval lambda.parse "{a ? 1, b, ...}: a"

#eval stringInterpolation.parse "${test}"
#eval expression.parse "left.right {a}: a.b"
#eval expression.parse "a.${b} c"
#eval expression.parse "flake.utils b c (d) (a: a + b)"
#eval file.parse "flake-utils.lib.eachSystem supportedSystems (system: system)"

def tests : IO Unit := do
  let files ← (FilePath.mk "test/nix").findAllWithExt "nix"
  let srcs ← files.mapM (λ fp => do pure (fp, ←IO.FS.readFile fp))
  for (fp, src) in srcs do
    println! "Parsing {fp}"
    match Expression.parse src with
    | Except.ok expr => IO.println <| ToString.toString expr
    | Except.error e => IO.eprintln s!"Error: {e}"

end Tests.Expression
