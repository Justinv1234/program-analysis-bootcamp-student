(* visitor.ml - AST visitor pattern exercises.
   Implement two common visitor-style operations that walk the AST
   and accumulate information. *)

open Shared_ast.Ast_types

(** Count the number of each node type in a statement list.
    Returns an association list like:
      [("Assign", 3); ("IntLit", 5); ("BinOp", 2); ...]
    Keys are constructor names WITHOUT parameters. *)

let bump key counts =
  if List.mem_assoc key counts then
    List.map (fun (k, v) -> if k = key then (k, v + 1) else (k, v)) counts
  else
    counts @ [(key, 1)]

let rec count_expr acc = function
  | IntLit _ -> bump "IntLit" acc
  | BoolLit _ -> bump "BoolLit" acc
  | Var _ -> bump "Var" acc
  | BinOp (_, e1, e2) ->
    let acc = bump "BinOp" acc in
    count_expr (count_expr acc e1) e2
  | UnaryOp (_, e1) ->
    count_expr (bump "UnaryOp" acc) e1
  | Call (_, args) ->
    List.fold_left count_expr (bump "Call" acc) args

and count_stmt acc = function
  | Assign (_, e) -> count_expr (bump "Assign" acc) e
  | If (cond, tb, eb) ->
    let acc = bump "If" acc in
    let acc = count_expr acc cond in
    let acc = List.fold_left count_stmt acc tb in
    List.fold_left count_stmt acc eb
  | While (cond, body) ->
    let acc = bump "While" acc in
    let acc = count_expr acc cond in
    List.fold_left count_stmt acc body
  | Return None -> bump "Return" acc
  | Return (Some e) -> count_expr (bump "Return" acc) e
  | Print exprs -> List.fold_left count_expr (bump "Print" acc) exprs
  | Block stmts -> List.fold_left count_stmt (bump "Block" acc) stmts

let count_nodes (stmts : stmt list) : (string * int) list =
  List.fold_left count_stmt [] stmts

(** Evaluate a constant expression, returning Some int if the
    expression contains only integer literals and arithmetic operators,
    or None if it contains variables, booleans, calls, or comparison ops. *)
let rec evaluate (e : expr) : int option =
  match e with
  | IntLit n -> Some n
  | BoolLit _ -> None
  | Var _ -> None
  | Call _ -> None
  | UnaryOp (Neg, inner) ->
    (match evaluate inner with
     | Some v -> Some (-v)
     | None -> None)
  | UnaryOp (Not, _) -> None
  | BinOp (op, e1, e2) ->
    (match op with
     (* only support arithmetic ops *)
     | Add | Sub | Mul | Div ->
       (match evaluate e1, evaluate e2 with
        | Some a, Some b ->
          (match op with
           | Add -> Some (a + b)
           | Sub -> Some (a - b)
           | Mul -> Some (a * b)
           | Div -> if b = 0 then None else Some (a / b)
           | _ -> None)
        | _ -> None)
     | _ -> None)
