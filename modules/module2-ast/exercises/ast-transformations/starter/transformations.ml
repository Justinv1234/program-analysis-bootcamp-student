(* transformations.ml - AST transformation passes.

   Each transformation is a pure function: it takes an AST (or part of one)
   and returns a *new* AST with the transformation applied.  The original
   tree is never mutated.

   Implement the three transformations below.  Each one exercises a
   different kind of recursive tree rewriting. *)

open Shared_ast.Ast_types

(* --------------------------------------------------------------------------
   1. Constant folding
   -------------------------------------------------------------------------- *)

let rec constant_fold (expr : expr) : expr =
  match expr with
  | IntLit _ | BoolLit _ | Var _ -> expr
  | Call (name, args) -> Call (name, List.map constant_fold args)
  | UnaryOp (op, inner) ->
    let inner' = constant_fold inner in
    (match op, inner' with
     | Neg, IntLit n -> IntLit (-n)
     | Not, BoolLit b -> BoolLit (not b)
     | _ -> UnaryOp (op, inner'))
  | BinOp (op, left, right) ->
    let left' = constant_fold left in
    let right' = constant_fold right in
    (match op, left', right' with
     (* arithmetic on two ints *)
     | Add, IntLit a, IntLit b -> IntLit (a + b)
     | Sub, IntLit a, IntLit b -> IntLit (a - b)
     | Mul, IntLit a, IntLit b -> IntLit (a * b)
     | Div, IntLit a, IntLit b -> if b <> 0 then IntLit (a / b) else BinOp (op, left', right')
     (* comparison on two ints *)
     | Eq,  IntLit a, IntLit b -> BoolLit (a = b)
     | Neq, IntLit a, IntLit b -> BoolLit (a <> b)
     | Lt,  IntLit a, IntLit b -> BoolLit (a < b)
     | Gt,  IntLit a, IntLit b -> BoolLit (a > b)
     | Le,  IntLit a, IntLit b -> BoolLit (a <= b)
     | Ge,  IntLit a, IntLit b -> BoolLit (a >= b)
     (* logical on two bools *)
     | And, BoolLit a, BoolLit b -> BoolLit (a && b)
     | Or,  BoolLit a, BoolLit b -> BoolLit (a || b)
     (* can't fold anything else *)
     | _ -> BinOp (op, left', right'))

(* --------------------------------------------------------------------------
   2. Variable renaming
   -------------------------------------------------------------------------- *)

let rec rename_expr old_name new_name = function
  | Var s when s = old_name -> Var new_name
  | Var _ | IntLit _ | BoolLit _ as e -> e
  | BinOp (op, e1, e2) ->
    BinOp (op, rename_expr old_name new_name e1, rename_expr old_name new_name e2)
  | UnaryOp (op, e) ->
    UnaryOp (op, rename_expr old_name new_name e)
  | Call (fname, args) ->
    Call (fname, List.map (rename_expr old_name new_name) args)

and rename_stmt old_name new_name = function
  | Assign (v, e) ->
    let v' = if v = old_name then new_name else v in
    Assign (v', rename_expr old_name new_name e)
  | If (cond, tb, eb) ->
    If (rename_expr old_name new_name cond,
        List.map (rename_stmt old_name new_name) tb,
        List.map (rename_stmt old_name new_name) eb)
  | While (cond, body) ->
    While (rename_expr old_name new_name cond,
           List.map (rename_stmt old_name new_name) body)
  | Return None -> Return None
  | Return (Some e) -> Return (Some (rename_expr old_name new_name e))
  | Print exprs -> Print (List.map (rename_expr old_name new_name) exprs)
  | Block stmts -> Block (List.map (rename_stmt old_name new_name) stmts)

let rename_variable (old_name : string) (new_name : string)
    (stmts : stmt list) : stmt list =
  List.map (rename_stmt old_name new_name) stmts

(* --------------------------------------------------------------------------
   3. Dead-code elimination
   -------------------------------------------------------------------------- *)

(* process a list of stmts: keep everything up to and including the first Return,
   drop the rest. also recurse into nested structures. *)
let rec dce_stmts (stmts : stmt list) : stmt list =
  match stmts with
  | [] -> []
  | s :: rest ->
    let s' = dce_stmt s in
    (match s' with
     | Return _ -> [s']  (* everything after return is dead *)
     | _ -> s' :: dce_stmts rest)

and dce_stmt = function
  | If (BoolLit true, tb, _) ->
    Block (dce_stmts tb)
  | If (BoolLit false, _, eb) ->
    Block (dce_stmts eb)
  | If (cond, tb, eb) ->
    If (cond, dce_stmts tb, dce_stmts eb)
  | While (cond, body) ->
    While (cond, dce_stmts body)
  | Block stmts ->
    Block (dce_stmts stmts)
  | other -> other

let eliminate_dead_code (stmts : stmt list) : stmt list =
  dce_stmts stmts
