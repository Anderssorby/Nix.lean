import Std

/-
Nix expression AST
-/
namespace Nix
universe u

open Std

def Name := String
deriving instance BEq for Name
deriving instance ToString for Name

deriving instance Hashable for Name
deriving instance Ord for Name

-- mantissa * 10^-exponent
structure Number where
  mantissa : Int
  exponent : Nat
  deriving DecidableEq

namespace Number

protected def fromNat (n : Nat) : Number := ⟨n, 0⟩
protected def fromInt (n : Int) : Number := ⟨n, 0⟩

instance : Coe Nat Number := ⟨Number.fromNat⟩
instance : Coe Int Number := ⟨Number.fromInt⟩
private partial def countDigits (n : Nat) : Nat :=
  let rec loop (n digits : Nat) : Nat :=
    if n ≤ 9 then
      digits
    else
      loop (n/10) (digits+1)
  loop n 1

-- convert mantissa * 10^-exponent to 0.mantissa * 10^exponent
protected def normalize : Number → Int × Nat × Int
  | ⟨m, e⟩ => Id.run <| do
    if m = 0 then (0, 0, 0)
    else
      let sign : Int := if m > 0 then 1 else -1
      let mut mAbs := m.natAbs
      let nDigits := countDigits mAbs
      -- eliminate trailing zeros
      for _ in [0:nDigits] do
        if mAbs % 10 = 0 then
          mAbs := mAbs / 10
        else
          break
      (sign, mAbs, -(e : Int) + nDigits)

-- todo (Dany): We should have an Ordering version of this.
def lt (a b : Number) : Bool :=
  let (as, am, ae) := a.normalize
  let (bs, bm, be) := b.normalize
  match (as, bs) with
  | (-1, 1) => true
  | (1, -1) => false
  | _ =>
    let ((am, ae), (bm, be)) :=
      if as = -1 && bs = -1 then
        ((bm, be), (am, ae))
      else
        ((am, ae), (bm, be))
    let amDigits := countDigits am
    let bmDigits := countDigits bm
    -- align the mantissas
    let (am, bm) :=
      if amDigits < bmDigits then
        (am * 10^(bmDigits - amDigits), bm)
      else
        (am, bm * 10^(amDigits - bmDigits))
    if ae < be then true
    else if ae > be then false
    else am < bm

def ltProp : LT Number :=
  ⟨fun a b => lt a b = true⟩

instance : LT Number :=
  ltProp

instance (a b : Number) : Decidable (a < b) :=
  inferInstanceAs (Decidable (lt a b = true))

instance : Ord Number where
  compare x y :=
    if x < y then Ordering.lt
    else if x > y then Ordering.gt
    else Ordering.eq

protected def toString : Number → String
  | ⟨m, 0⟩ => m.repr
  | ⟨m, e⟩ =>
    let sign := if m ≥ 0 then "" else "-"
    let m := m.natAbs
    -- if there are too many zeroes after the decimal, we
    -- use exponents to compress the representation.
    -- this is mostly done for memory usage reasons:
    -- the size of the representation would otherwise
    -- grow exponentially in the value of exponent.
    let exp : Int := 9 + countDigits m - (e : Int)
    let exp := if exp < 0 then exp else 0
    let e' := (10 : Int) ^ (e - exp.natAbs)
    let left := (m / e').repr
    let right := e' + ↑m % e'
      |>.repr.toSubstring.drop 1
      |>.dropRightWhile (fun c => c = '0')
      |>.toString
    let exp := if exp = 0 then "" else "e" ++ exp.repr
    s!"{sign}{left}.{right}{exp}"

-- shift a Number by a specified amount of places to the left
protected def shiftl : Number → Nat → Number
  -- if s ≤ e, then 10 ^ (s - e) = 1, and hence the mantissa remains unchanged.
  -- otherwise, the expression pads the mantissa with zeroes
  -- to accomodate for the remaining places to shift.
  | ⟨m, e⟩, s => ⟨m * (10 ^ (s - e) : Nat), e - s⟩

-- shift a Number by a specified amount of places to the right
protected def shiftr : Number → Nat → Number
  | ⟨m, e⟩, s => ⟨m, e + s⟩

instance : ToString Number := ⟨Number.toString⟩

instance : Repr Number where
  reprPrec | ⟨m, e⟩, _ => Std.Format.bracket "⟨" (repr m ++ "," ++ repr e) "⟩"

end Number
/-
Bound variable
-/
inductive BVar where
  | var (name: Name) -- $Name: $Expr
  | destruct (d: List Name) -- { $Name,* }:
  deriving BEq

/-
Builtin operators
-/
inductive Operator where
  | dot -- .
  | merge -- //
  | or_ -- or
  | and_ -- and
  | add -- +
  | append -- ++
  | minus -- -
  | mul -- *
  deriving Ord, BEq

open Operator in
instance : ToString Operator := ⟨λ op : Operator =>
  match op with
  | dot => "."
  | merge => "//"
  | or_ => "or"
  | and_ => "and"
  | add => "+"
  | append => "++"
  | minus => "-"
  | mul => "*"
  ⟩

structure Position where
  start : (Nat × Nat)
  end_ : (Nat × Nat)
  deriving BEq

mutual

inductive AttrSetKey where
  | name (name : Expr)
  | expr (arr : Array Expr)
  deriving BEq

/--
Binding of variables in a lambda.
-/
inductive LBinding where
  | var (name : Name)
  | destructure (name : Option Name) (vars : Array (Name × Option Expr)) (catchAll : Bool)

/--
Nix Expressions not desugared.
-/
inductive Expr where
  | lam (binding : LBinding) (body : Expr) -- $BVar: $Expr
  | ifStatement (e : Expr) (trueCase : Expr) (falseCase : Expr)
  | withStatement (with_ : Expr) (expr : Expr)
  | letExpr (vars : Array (Prod Name Expr)) (inExpr : Expr)
  | app (f : Expr) (arg : Expr)
  | attrset (rec : Bool) (kvPairs : List (AttrSetKey × Expr))
  | fvar (name : Name)
  | list (l : Array Expr)
  | str (s : String)
  | opr (left : Expr) (op : Operator) (right : Expr)
  -- Constants
  | num (n : Number)
  -- Meta info
  | meta (pos : Position) (docs : Option String) (e : Expr)
  deriving BEq

end

mutual

private partial def AttrSetKey_toString (a : AttrSetKey) : String :=
  match a with
  | AttrSetKey.name n => Expr_toString n
  | AttrSetKey.expr arr => ".".intercalate <| arr.toList.map Expr_toString

private partial def LBinding_toString (b : LBinding) : String :=
  match b with
  | LBinding.var n => ToString.toString n
  | LBinding.destructure optName vars catchAll =>
    (if let some d := optName then s!"{d}@" else "") ++
    "{ " ++
    (", ".intercalate <| vars.toList.map (fun (n, defVal) =>
      ToString.toString n ++
      (if let some d := defVal then " ? " ++ Expr_toString d else "")
    )) ++
    (if catchAll then ", ..." else "") ++
    " }"

private partial def Expr_toString (e : Expr) : String :=
  match e with
  | Expr.str s => s!"\"{s}\""
  | Expr.attrset rec kvPairs => (if rec then "rec " else "")
    ++ "{\n" ++
    (List.foldl (fun s (k, v) =>
      s ++ s!"{AttrSetKey_toString k} = {Expr_toString v};\n"
    ) "" kvPairs)
    ++ "}"
  | Expr.list l => "[ " ++
    (Array.foldl (fun s v =>
      s ++ s!"{Expr_toString v} "
    ) "" l)
    ++ "]"
  | Expr.lam n b => s!"{LBinding_toString n}: {Expr_toString b}"
  | Expr.withStatement w e => s!"with {Expr_toString w}; {Expr_toString e}"
  | Expr.ifStatement e t f => s!"if {Expr_toString e} then\n {Expr_toString e}\nelse\n{Expr_toString f}"
  | Expr.letExpr vars inExpr => "let\n" ++
    (Array.foldl (fun s (n, v) =>
      s ++ s!"  {n} = {Expr_toString v};\n"
    ) "" vars)
    ++ s!"in\n  {Expr_toString inExpr}\n"
  | Expr.num n => ToString.toString n
  | Expr.fvar n => ToString.toString n
  | Expr.meta pos doc e => Expr_toString e
  | Expr.opr l op r => s!"{Expr_toString l} {op} {Expr_toString r}"
  | Expr.app f a => s!"{Expr_toString f} {Expr_toString a}"
  -- | _ => "TODO print"
  -- termination_by measure e
end

instance : ToString AttrSetKey := ⟨AttrSetKey_toString⟩
instance : ToString LBinding := ⟨LBinding_toString⟩
instance : ToString Expr := ⟨Expr_toString⟩

end Nix
