/- import Ipld -/
import Nix.Parser
import Nix.Expression

open Nix

def main (args : List String) : IO UInt32 := do
  try
    (match parse "{\"hello\" = \"world\"; \"test\"={\"a\" = 1;};}" with
    | Except.ok expr => IO.println <| ToString.toString expr
    | Except.error e => println! "Error: {e}"
    )
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

