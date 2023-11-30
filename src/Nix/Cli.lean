import Nix
import Cli

open Nix
open System Cli

def run (p : Parsed) : IO UInt32 := do
  try
    let 
    let file := FilePath.mk <| p.positionalArg! "file" |>.as! String
    let src ← IO.FS.readFile file | "Failed to read file"
    -- if args.contains "--drv" || args.contains "-d" then
    --   match Derivation.parse src with
    --   | Except.ok drv => IO.println <| ToString.toString drv
    --   | Except.error e => IO.eprintln s!"Error: {e}"
    -- else
    --   match Expression.parse src with
    --   | Except.ok expr => IO.println <| ToString.toString expr
    --   | Except.error e => IO.eprintln s!"Error: {e}"
    pure 0
  catch e =>
    IO.eprintln <| "Error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

def cmd : Cmd := `[Cmd|
  nix-lean VIA run; "Nix lean implementation"
  FLAGS:
    drv, d; "Derivations"
  ARGUMENTS:
    file; "The file to process"
]

def main (args : List String) : IO UInt32 := do
  cmd.verify args

