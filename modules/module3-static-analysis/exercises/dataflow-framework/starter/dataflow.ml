(** Generic iterative dataflow analysis solver.

    This module implements the classic worklist-based fixpoint algorithm
    that underlies most dataflow analyses (reaching definitions, live
    variables, available expressions, etc.).

    The solver is parameterized by:
    - [direction]: whether information flows forward or backward
    - [init]: initial lattice value for every block
    - [merge]: how to combine values from multiple predecessors/successors
    - [transfer]: how a single basic block transforms a lattice value
    - [equal]: when to stop iterating (fixpoint test)

    The CFG is given as a list of
      (block_label, predecessor_labels, successor_labels)
    triples. The solver returns (block_label, in_value, out_value)
    for every block once a fixpoint is reached.
*)

type direction = Forward | Backward

type 'a analysis = {
  direction : direction;
  init : 'a;
  merge : 'a -> 'a -> 'a;
  transfer : string -> 'a -> 'a;
  equal : 'a -> 'a -> bool;
}

module StringMap = Map.Make (String)

(** [solve analysis cfg] runs the iterative fixpoint algorithm.

    @param analysis  the analysis configuration (direction, transfer, etc.)
    @param cfg       list of (block_label, predecessors, successors)
    @return          list of (block_label, in_value, out_value) at fixpoint

    Algorithm sketch (forward case):
    {v
      1. Initialize IN[B] = OUT[B] = analysis.init for every block B.
      2. Repeat until nothing changes:
         For each block B:
           a. IN[B]  = merge over all predecessors P of B: OUT[P]
           b. OUT[B] = transfer(B, IN[B])
      3. Return the final IN/OUT for each block.
    v}

    For the backward case, swap the roles of IN/OUT and
    predecessors/successors.

    TODO: Implement this function. It currently raises [Failure "TODO"].
*)
let solve (analysis : 'a analysis)
    (cfg : (string * string list * string list) list)
    : (string * 'a * 'a) list =
  let in_map = ref StringMap.empty in
  let out_map = ref StringMap.empty in
  List.iter (fun (label, _, _) ->
    in_map := StringMap.add label analysis.init !in_map;
    out_map := StringMap.add label analysis.init !out_map
  ) cfg;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (label, preds, succs) ->
      match analysis.direction with
      | Forward ->
        let new_in = List.fold_left (fun acc p ->
          analysis.merge acc (StringMap.find p !out_map)
        ) analysis.init preds in
        let new_out = analysis.transfer label new_in in
        if not (analysis.equal new_out (StringMap.find label !out_map)) then begin
          changed := true;
          in_map := StringMap.add label new_in !in_map;
          out_map := StringMap.add label new_out !out_map
        end else
          in_map := StringMap.add label new_in !in_map
      | Backward ->
        let new_out = List.fold_left (fun acc s ->
          analysis.merge acc (StringMap.find s !in_map)
        ) analysis.init succs in
        let new_in = analysis.transfer label new_out in
        if not (analysis.equal new_in (StringMap.find label !in_map)) then begin
          changed := true;
          in_map := StringMap.add label new_in !in_map;
          out_map := StringMap.add label new_out !out_map
        end else
          out_map := StringMap.add label new_out !out_map
    ) cfg
  done;
  List.map (fun (label, _, _) ->
    (label, StringMap.find label !in_map, StringMap.find label !out_map)
  ) cfg
