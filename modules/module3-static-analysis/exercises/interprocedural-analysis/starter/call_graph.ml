(** Interprocedural Analysis: Call Graph Construction

    Build a call graph from a program and use it to answer questions
    about function relationships like reachability and recursion. *)

open Shared_ast.Ast_types

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type call_graph = {
  nodes : StringSet.t;               (** All function names in the program *)
  edges : StringSet.t StringMap.t;   (** caller -> set of callees *)
}

(** Extract function names called in an expression.

    Walk the expression tree and collect every function name that
    appears in a [Call(name, args)] node. Don't forget to also
    recurse into the argument expressions -- a call like
    [f(g(x))] should return both "f" and "g".

    Examples:
    - [IntLit 5] -> []
    - [Call("f", [Var "x"])] -> ["f"]
    - [Call("f", [Call("g", [IntLit 1])])] -> ["f"; "g"]
    - [BinOp(Add, Call("a", []), Call("b", []))] -> ["a"; "b"] *)
(* Hint: You will need [let rec] when you implement this. *)
let rec calls_in_expr (expr : expr) : string list =
  match expr with
  | IntLit _ | BoolLit _ | Var _ -> []
  | BinOp (_, e1, e2) -> calls_in_expr e1 @ calls_in_expr e2
  | UnaryOp (_, e) -> calls_in_expr e
  | Call (name, args) ->
    [name] @ List.concat_map calls_in_expr args

(** Extract all function names called in a list of statements.

    Walk every statement recursively:
    - [Assign(_, e)] -> calls in e
    - [If(cond, then_branch, else_branch)] -> calls in cond + both branches
    - [While(cond, body)] -> calls in cond + body
    - [Return(Some e)] -> calls in e
    - [Return(None)] -> []
    - [Print(exprs)] -> calls in each expr
    - [Block(stmts)] -> recurse into stmts *)
(* Hint: You will need [let rec ... and ...] for mutual recursion
   between calls_in_stmts and a per-statement helper. *)
let rec calls_in_stmts (stmts : stmt list) : string list =
  List.concat_map calls_in_stmt stmts
and calls_in_stmt (stmt : stmt) : string list =
  match stmt with
  | Assign (_, e) -> calls_in_expr e
  | If (cond, then_branch, else_branch) ->
    calls_in_expr cond @ calls_in_stmts then_branch @ calls_in_stmts else_branch
  | While (cond, body) ->
    calls_in_expr cond @ calls_in_stmts body
  | Return (Some e) -> calls_in_expr e
  | Return None -> []
  | Print exprs -> List.concat_map calls_in_expr exprs
  | Block stmts -> calls_in_stmts stmts

(** Build a call graph from a program.

    For each function definition in the program:
    1. Add it as a node in the graph
    2. Find all function calls in its body using [calls_in_stmts]
    3. Record these as edges: caller -> {callees}

    The result should have:
    - [nodes]: the set of all function names
    - [edges]: a map from each function name to the set of functions it calls *)
let build_call_graph (program : program) : call_graph =
  let nodes = List.fold_left (fun acc fd ->
    StringSet.add fd.name acc
  ) StringSet.empty program in
  let edges = List.fold_left (fun acc fd ->
    let callees = calls_in_stmts fd.body
      |> List.sort_uniq String.compare
      |> StringSet.of_list in
    StringMap.add fd.name callees acc
  ) StringMap.empty program in
  { nodes; edges }

(** Find all functions reachable from a given starting function.

    Perform a BFS or DFS traversal following the call graph edges.
    Return the set of all functions that can be reached (directly
    or transitively), NOT including the starting function itself
    (unless it calls itself recursively).

    Example for: main -> process_data -> helper
    - [reachable_from graph "main"] = {"process_data", "helper"}
    - [reachable_from graph "process_data"] = {"helper"}
    - [reachable_from graph "helper"] = {} *)
let reachable_from (cg : call_graph) (start : string) : StringSet.t =
  let rec dfs visited node =
    if StringSet.mem node visited then visited
    else
      let visited = StringSet.add node visited in
      let callees = match StringMap.find_opt node cg.edges with
        | Some s -> s
        | None -> StringSet.empty in
      StringSet.fold (fun callee acc -> dfs acc callee) callees visited
  in
  let callees = match StringMap.find_opt start cg.edges with
    | Some s -> s
    | None -> StringSet.empty in
  let reachable = StringSet.fold (fun callee acc ->
    dfs acc callee
  ) callees StringSet.empty in
  reachable

(** Detect recursive functions in the call graph.

    A function is recursive if it appears in a cycle in the call graph.
    This includes:
    - Direct recursion: f calls f
    - Mutual recursion: f calls g, g calls f

    Hint: A function f is recursive if f is in [reachable_from graph f].

    Return a sorted list of all recursive function names. *)
let find_recursive (cg : call_graph) : string list =
  StringSet.fold (fun node acc ->
    let reachable = reachable_from cg node in
    if StringSet.mem node reachable then node :: acc
    else acc
  ) cg.nodes []
  |> List.sort String.compare
