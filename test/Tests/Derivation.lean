import Nix
import Nix.Utils
import Std

open Nix Nix.Derivation Parser System
namespace Tests.Derivation

#print "Testing Derivation.Parser"

#eval (listOf stringLitteral).parse "[\"a\"]"
#eval derivation.parse "Derive([(\"out\", \"\", \"\", \"\")], [(\"input\", [\"\"])],[\"srcs\"], \"x86_64-linux\", \"builder\", [\"args\"],[(\"ENV\",\"val\")])"

def drv_base : Derivation :=
  { outputs := HashMap.fromArray #[("out", { hash := "hashqweqewrq" })]
  , inputDrvs := HashMap.fromArray #[({ hash := "hashqweqewrq" }, HashSet.fromArray #["out"])]
  , inputSrcs := HashSet.fromArray #[{ hash := "hashqweqewrq" }]
  , system := "x86_64-linux"
  , builder := "/nix/store/l0wlqpbsvh1pgvhcdhw7qkka3d31si7k-bash-5.1-p8/bin/bash"
  , args := #["test"]
  , env := HashMap.fromArray #[("system", "x86_64-linux")]
  : Derivation }

#eval drv_base
#eval (Derivation.parse drv_base.toString).map (λ d => d == drv_base)

def tests : IO Unit := do
  let files ← (FilePath.mk "test/drv").findAllWithExt "drv"
  let srcs ← files.mapM (λ fp => do pure (fp, ←IO.FS.readFile fp))
  for (fp, src) in srcs do
    println! "Parsing {fp}"
    match Expression.parse src with
    | Except.ok expr => IO.println <| ToString.toString expr
    | Except.error e => IO.eprintln s!"Error: {e}"

end Tests.Derivation
