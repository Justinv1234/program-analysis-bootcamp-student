(** CFG Construction Exercises.

    Each function below takes a list of AST statements and returns a CFG
    whose shape matches a specific control-flow pattern.

    Students: implement the functions marked with TODO.

    General approach for each exercise:
    1. Create the basic blocks with [Cfg.create_block].
    2. Put them into a [Cfg.StringMap] keyed by label.
    3. Build the initial [Cfg.cfg] record with entry, exit_label, and blocks.
    4. Use [Cfg.add_edge] to wire up the control flow edges.

    The ENTRY and EXIT blocks are always empty (no statements). *)

open Shared_ast.Ast_types

(** Build a CFG for straight-line (sequential) code.

    Expected shape:

      ENTRY --> B1 --> EXIT

    All statements go into a single block B1.

    Example input:
      [ Assign ("x", IntLit 1);
        Assign ("y", IntLit 2);
        Assign ("z", BinOp (Add, Var "x", Var "y")) ]

    @param stmts  A flat list of statements with no branches or loops. *)
let build_cfg_sequential (stmts : stmt list) : Cfg.cfg =
  let entry = Cfg.create_block "ENTRY" [] in
  let b1 = Cfg.create_block "B1" stmts in
  let exit_b = Cfg.create_block "EXIT" [] in
  let blocks = Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry
    |> Cfg.StringMap.add "B1" b1
    |> Cfg.StringMap.add "EXIT" exit_b
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  let cfg = Cfg.add_edge cfg "ENTRY" "B1" in
  Cfg.add_edge cfg "B1" "EXIT"

(** Build a CFG for an if-else branch.

    Expected shape (diamond):

           ENTRY
             |
           B_cond
           /    \
       B_then  B_else
           \    /
           B_join
             |
            EXIT

    The input should contain statements before the if, the if-else
    itself, and statements after the if.

    The condition block B_cond holds any statements that precede the
    If, plus the If statement acts as the branch (but is not placed
    in a block -- only its children are).

    For simplicity, this exercise expects the input to be:
      [ ...pre-if stmts...;
        If (cond, then_stmts, else_stmts);
        ...post-if stmts... ]

    Map them to blocks:
    - B_cond : statements before the If
    - B_then : then_stmts
    - B_else : else_stmts
    - B_join : statements after the If

    @param stmts  Statement list containing exactly one If statement. *)
let build_cfg_ifelse (stmts : stmt list) : Cfg.cfg =
  let rec split_if before = function
    | [] -> failwith "no If statement found"
    | If (_, then_stmts, else_stmts) :: after ->
      (List.rev before, then_stmts, else_stmts, after)
    | s :: rest -> split_if (s :: before) rest
  in
  let (before, then_stmts, else_stmts, after) = split_if [] stmts in
  let blocks = Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" (Cfg.create_block "ENTRY" [])
    |> Cfg.StringMap.add "B_cond" (Cfg.create_block "B_cond" before)
    |> Cfg.StringMap.add "B_then" (Cfg.create_block "B_then" then_stmts)
    |> Cfg.StringMap.add "B_else" (Cfg.create_block "B_else" else_stmts)
    |> Cfg.StringMap.add "B_join" (Cfg.create_block "B_join" after)
    |> Cfg.StringMap.add "EXIT" (Cfg.create_block "EXIT" [])
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  cfg
  |> fun c -> Cfg.add_edge c "ENTRY" "B_cond"
  |> fun c -> Cfg.add_edge c "B_cond" "B_then"
  |> fun c -> Cfg.add_edge c "B_cond" "B_else"
  |> fun c -> Cfg.add_edge c "B_then" "B_join"
  |> fun c -> Cfg.add_edge c "B_else" "B_join"
  |> fun c -> Cfg.add_edge c "B_join" "EXIT"

(** Build a CFG for a while loop.

    Expected shape:

       ENTRY
         |
       B_pre       (statements before the while)
         |
       B_cond  <---+
       /    \      |
    B_body   \     |
      |       \    |
      +--------+   |
               |
            B_post  (statements after the while)
               |
             EXIT

    More precisely:
      ENTRY -> B_pre -> B_cond -> B_body -> B_cond  (back edge!)
                                  B_cond -> B_post -> EXIT

    @param stmts  Statement list containing exactly one While statement. *)
let build_cfg_while (stmts : stmt list) : Cfg.cfg =
  let rec split_while before = function
    | [] -> failwith "no While statement found"
    | While (_, body) :: after ->
      (List.rev before, body, after)
    | s :: rest -> split_while (s :: before) rest
  in
  let (before, body, after) = split_while [] stmts in
  let blocks = Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" (Cfg.create_block "ENTRY" [])
    |> Cfg.StringMap.add "B_pre" (Cfg.create_block "B_pre" before)
    |> Cfg.StringMap.add "B_cond" (Cfg.create_block "B_cond" [])
    |> Cfg.StringMap.add "B_body" (Cfg.create_block "B_body" body)
    |> Cfg.StringMap.add "B_post" (Cfg.create_block "B_post" after)
    |> Cfg.StringMap.add "EXIT" (Cfg.create_block "EXIT" [])
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  cfg
  |> fun c -> Cfg.add_edge c "ENTRY" "B_pre"
  |> fun c -> Cfg.add_edge c "B_pre" "B_cond"
  |> fun c -> Cfg.add_edge c "B_cond" "B_body"
  |> fun c -> Cfg.add_edge c "B_cond" "B_post"
  |> fun c -> Cfg.add_edge c "B_body" "B_cond"
  |> fun c -> Cfg.add_edge c "B_post" "EXIT"
