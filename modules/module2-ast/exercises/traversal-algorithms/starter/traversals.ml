(* traversals.ml - AST traversal algorithms exercise.
   Implement three classic tree traversal strategies on the AST:
   pre-order (depth-first), post-order (depth-first), and
   breadth-first (level-order).

   Each function walks a list of statements and collects a string label
   for every node visited. Labels should look like:
     Statements: "Assign", "If", "While", "Return", "Print", "Block"
     Expressions: "IntLit(3)", "BoolLit(true)", "Var(x)", "BinOp(+)",
                  "UnaryOp(-)", "Call(f)"
*)

open Shared_ast.Ast_types

let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">"
  | Le -> "<=" | Ge -> ">=" | And -> "&&" | Or -> "||"

let string_of_uop = function
  | Neg -> "-" | Not -> "!"

(** Helper: produce a string label for a single expression node. *)
let label_of_expr (e : expr) : string =
  match e with
  | IntLit n -> "IntLit(" ^ string_of_int n ^ ")"
  | BoolLit b -> "BoolLit(" ^ string_of_bool b ^ ")"
  | Var s -> "Var(" ^ s ^ ")"
  | BinOp (op, _, _) -> "BinOp(" ^ string_of_op op ^ ")"
  | UnaryOp (op, _) -> "UnaryOp(" ^ string_of_uop op ^ ")"
  | Call (name, _) -> "Call(" ^ name ^ ")"

(** Helper: produce a string label for a single statement node. *)
let label_of_stmt (s : stmt) : string =
  match s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(* pre-order helpers: visit node first, then recurse into children *)
let rec pre_expr (e : expr) : string list =
  match e with
  | IntLit _ | BoolLit _ | Var _ -> [label_of_expr e]
  | BinOp (_, e1, e2) ->
    label_of_expr e :: pre_expr e1 @ pre_expr e2
  | UnaryOp (_, e1) ->
    label_of_expr e :: pre_expr e1
  | Call (_, args) ->
    label_of_expr e :: List.concat_map pre_expr args

and pre_stmt (s : stmt) : string list =
  match s with
  | Assign (_, e) ->
    "Assign" :: pre_expr e
  | If (cond, tb, eb) ->
    "If" :: pre_expr cond @ pre_stmts tb @ pre_stmts eb
  | While (cond, body) ->
    "While" :: pre_expr cond @ pre_stmts body
  | Return None -> ["Return"]
  | Return (Some e) -> "Return" :: pre_expr e
  | Print exprs -> "Print" :: List.concat_map pre_expr exprs
  | Block stmts -> "Block" :: pre_stmts stmts

and pre_stmts (stmts : stmt list) : string list =
  List.concat_map pre_stmt stmts

let pre_order (stmts : stmt list) : string list =
  pre_stmts stmts

(* post-order helpers: recurse into children first, then visit node *)
let rec post_expr (e : expr) : string list =
  match e with
  | IntLit _ | BoolLit _ | Var _ -> [label_of_expr e]
  | BinOp (_, e1, e2) ->
    post_expr e1 @ post_expr e2 @ [label_of_expr e]
  | UnaryOp (_, e1) ->
    post_expr e1 @ [label_of_expr e]
  | Call (_, args) ->
    List.concat_map post_expr args @ [label_of_expr e]

and post_stmt (s : stmt) : string list =
  match s with
  | Assign (_, e) ->
    post_expr e @ ["Assign"]
  | If (cond, tb, eb) ->
    post_expr cond @ post_stmts tb @ post_stmts eb @ ["If"]
  | While (cond, body) ->
    post_expr cond @ post_stmts body @ ["While"]
  | Return None -> ["Return"]
  | Return (Some e) -> post_expr e @ ["Return"]
  | Print exprs -> List.concat_map post_expr exprs @ ["Print"]
  | Block stmts -> post_stmts stmts @ ["Block"]

and post_stmts (stmts : stmt list) : string list =
  List.concat_map post_stmt stmts

let post_order (stmts : stmt list) : string list =
  post_stmts stmts

(* BFS: use a queue, need a sum type to unify stmts and exprs *)
type node =
  | Stmt_node of stmt
  | Expr_node of expr

let children_of_node = function
  | Stmt_node s -> (
    match s with
    | Assign (_, e) -> [Expr_node e]
    | If (cond, tb, eb) ->
      [Expr_node cond]
      @ List.map (fun s -> Stmt_node s) tb
      @ List.map (fun s -> Stmt_node s) eb
    | While (cond, body) ->
      [Expr_node cond] @ List.map (fun s -> Stmt_node s) body
    | Return None -> []
    | Return (Some e) -> [Expr_node e]
    | Print exprs -> List.map (fun e -> Expr_node e) exprs
    | Block stmts -> List.map (fun s -> Stmt_node s) stmts
  )
  | Expr_node e -> (
    match e with
    | IntLit _ | BoolLit _ | Var _ -> []
    | BinOp (_, e1, e2) -> [Expr_node e1; Expr_node e2]
    | UnaryOp (_, e1) -> [Expr_node e1]
    | Call (_, args) -> List.map (fun a -> Expr_node a) args
  )

let label_of_node = function
  | Stmt_node s -> label_of_stmt s
  | Expr_node e -> label_of_expr e

let bfs (stmts : stmt list) : string list =
  let q = Queue.create () in
  List.iter (fun s -> Queue.push (Stmt_node s) q) stmts;
  let result = ref [] in
  while not (Queue.is_empty q) do
    let node = Queue.pop q in
    result := label_of_node node :: !result;
    List.iter (fun c -> Queue.push c q) (children_of_node node)
  done;
  List.rev !result
