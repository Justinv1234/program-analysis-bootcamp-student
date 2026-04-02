(* Live Variables Analysis - Backward May-Analysis
 *
 * A variable is "live" at a program point if there exists some path from
 * that point to a use of the variable that does not pass through a
 * redefinition. This is a BACKWARD analysis because information flows
 * from uses (later in the program) back to earlier points.
 *
 * Transfer function (applied backward):
 *   IN[B] = use[B] U (OUT[B] - def[B])
 *
 * Merge (at control-flow joins in backward direction):
 *   OUT[B] = U IN[S] for all successors S of B
 *
 * This is a "may" analysis with union merge because a variable is live
 * if it MAY be used along ANY path from the current point.
 *
 * Block representation:
 *   (label, defined_vars, used_vars, successor_labels)
 *
 * - label:            unique string identifying the block
 * - defined_vars:     variables assigned/written in this block
 * - used_vars:        variables read in this block (before any local def)
 * - successor_labels: labels of blocks that follow this one in the CFG
 *)

module StringSet = Set.Make(String)

(* compute_use: Extract the use set for a block.
 *
 * The "use" set contains variables that are read in this block before
 * being defined. These variables must be live on entry to the block
 * regardless of what happens after the block.
 *
 * Block format: (label, defined_vars, used_vars)
 *)
let compute_use ((_label, _defs, uses) : string * string list * string list) : StringSet.t =
  StringSet.of_list uses

(* compute_def: Extract the def set for a block.
 *
 * The "def" set contains variables that are assigned/written in this
 * block. A definition "kills" liveness -- if a variable is defined here,
 * it does not need to be live on entry (unless it is also used before
 * the definition, which is captured by the use set).
 *
 * Block format: (label, defined_vars, used_vars)
 *)
let compute_def ((_label, defs, _uses) : string * string list * string list) : StringSet.t =
  StringSet.of_list defs

(* analyze: Run the live variables backward iterative analysis.
 *
 * Given a list of blocks (label, defined_vars, used_vars, successor_labels),
 * compute the fixed-point solution for IN[B] at each block.
 *
 * Algorithm:
 *   1. Initialize IN[B] = {} for all blocks
 *   2. Repeat until no IN set changes:
 *      a. For each block B:
 *         - OUT[B] = union of IN[S] for all successors S of B
 *         - IN[B]  = use[B] U (OUT[B] - def[B])
 *   3. Return (label, IN[B], OUT[B]) for each block
 *
 * Returns: list of (label, in_set, out_set) triples
 *)
let analyze
    (blocks : (string * string list * string list * string list) list)
    : (string * StringSet.t * StringSet.t) list =
  let module SMap = Map.Make(String) in
  let in_map = ref (List.fold_left (fun acc (label, _, _, _) ->
    SMap.add label StringSet.empty acc
  ) SMap.empty blocks) in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (label, defs, uses, succs) ->
      let out_b = List.fold_left (fun acc s ->
        StringSet.union acc (SMap.find s !in_map)
      ) StringSet.empty succs in
      let use_set = compute_use (label, defs, uses) in
      let def_set = compute_def (label, defs, uses) in
      let in_b = StringSet.union use_set (StringSet.diff out_b def_set) in
      if not (StringSet.equal in_b (SMap.find label !in_map)) then begin
        changed := true;
        in_map := SMap.add label in_b !in_map
      end
    ) blocks
  done;
  List.map (fun (label, _, _, succs) ->
    let out_b = List.fold_left (fun acc s ->
      StringSet.union acc (SMap.find s !in_map)
    ) StringSet.empty succs in
    (label, SMap.find label !in_map, out_b)
  ) blocks
