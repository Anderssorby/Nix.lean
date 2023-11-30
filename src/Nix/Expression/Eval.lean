import Nix.Expression.Types
import Nix.HList

namespace Nix.Expression

namespace Checker

/-- Primitive types
-/
inductive Ty where
  | num
  | string
  | bool
  | lambda : Ty -> Ty -> Ty
  -- Optimized primitives
  | list : Ty -> Ty
  | attrset : (fields : Array (Name × Ty)) → Ty
  deriving DecidableEq

inductive HasType : Expr → Ty → Prop
  | nat : HasType (.num v) .num
  | bool : HasType (.bool v) .bool
  | string : HasType (.str v) .string
  | lambda : HasType (.lam binding body) .lambda
  | app : HasType (.lambda input output)  -> HasType (.)

end Checker

end Nix.Expression
