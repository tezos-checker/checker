(* avl_types *)

type ptr = Ptr of int

let int_of_ptr (p: ptr) =
  match p with Ptr i -> i
let ptr_eq (p1: ptr) (p2: ptr) =
  int_of_ptr p1 = int_of_ptr p2

type avl_ptr = AvlPtr of ptr
type leaf_ptr = LeafPtr of ptr

type branch = {
  left: ptr;
  left_height: int;
  left_tez: tez;
  right_tez: tez;
  right_height: int;
  right: ptr;
  parent: ptr;
}

type leaf = {
  value: int;
  tez: tez;
  parent: ptr;
}

type node =
  | Leaf of leaf
  | Branch of branch
  | Root of (ptr option * int)

(* mem *)

type mem = {
  mem: (ptr, node) map;
  max_id: int;
}

let mem_new (mem: mem) (value: node): mem * ptr =
  let new_id = mem.max_id + 1 in
  let new_mem = {
        mem = Big_map.update (Ptr new_id) (Some value) mem.mem;
        max_id = new_id;
      } in
  (new_mem, Ptr new_id)

let mem_get (mem: mem) (ptr: ptr): node =
  Big_map.find ptr mem.mem

let mem_set (mem: mem) (ptr: ptr) (value: node): mem =
  let ign = assert (int_of_ptr ptr <= mem.max_id) in
  { mem = Big_map.update ptr (Some value) mem.mem;
    max_id = mem.max_id
  }

let mem_update (m: mem) (k: ptr) (f: node -> node) : mem =
  mem_set m k (f (mem_get m k))

(* avl implementation *)

let node_tez (n: node) : tez =
  match n with
  | Leaf leaf -> leaf.tez
  | Branch branch -> branch.left_tez + branch.right_tez
  | Root d ->
    (failwith "node_tez found Root": tez)

let node_parent (n: node) : ptr =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent
  | Root ign -> (failwith "node_parent found Root": ptr)

let node_set_parent (p: ptr) (n: node) : node =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }
  | Root ign -> (failwith "node_set_parent found Root": node)

let node_branch (n: node) : branch =
  match n with
  | Branch branch -> branch
  | Leaf leaf -> (failwith "node_branch found Leaf": branch)
  | Root ign -> (failwith "node_branch found Root": branch)

let max (i: int) (j: int) : int = if i>j then i else j

let node_height (n: node) : int =
  match n with
  | Leaf ign -> 1
  | Branch branch -> max branch.left_height branch.right_height + 1
  | Root ign -> (failwith "node_height found Root" : int)

let mk_empty (mem: mem) (root_data: int): mem * avl_ptr =
  let mem = {
    mem = (Map.empty: (ptr, node) map);
    max_id = 0;
  } in
  let (mem, ptr) = mem_new mem (Root ((None: ptr option), root_data)) in
  (mem, AvlPtr ptr)

let update_matching_child
    (mem: mem) (ptr: ptr) (from_ptr: ptr) (to_ptr: ptr) : mem =
  match mem_get mem ptr with
  | Root m ->
    let (b, r) = m in
    (* let ign = assert (b = Some from_ptr) in *)
    mem_set mem ptr (Root ((Some to_ptr), r))
  | Leaf ign ->
    (failwith "update_matching_child: got a leaf": mem
)  | Branch old_branch ->
    let to_ = mem_get mem to_ptr in
    let new_branch =
      if ptr_eq old_branch.left from_ptr
      then Branch {
          old_branch with
          left = to_ptr;
          left_tez = node_tez to_;
          left_height = node_height to_;
        }
      else (
        let ign = assert (ptr_eq old_branch.right from_ptr) in
        Branch {
          old_branch with
          right = to_ptr;
          right_tez = node_tez to_;
          right_height = node_height to_;
        }) in
    mem_set mem ptr new_branch

let ref_rotate_left (mem: mem) (curr_ptr: ptr) : mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root ign -> (failwith "rotate_left: curr_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_left: curr_ptr is Leaf": branch)
    | Branch curr -> curr in

  let right_ptr = curr.right in
  let right =
    match mem_get mem right_ptr with
    | Root ign -> (failwith "rotate_left: right_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_left: right_ptr is Leaf": branch)
    | Branch right -> right in

  let right_left_ptr = right.left in

  (* move right_left under curr *)
  let mem = mem_update mem right_left_ptr (node_set_parent curr_ptr) in
  let mem = update_matching_child mem curr_ptr right_ptr right_left_ptr in

  (* move curr under right *)
  let mem = mem_update mem right_ptr (node_set_parent curr.parent) in
  let mem = mem_update mem curr_ptr (node_set_parent right_ptr) in
  let mem = update_matching_child mem right_ptr right_left_ptr curr_ptr in

  (mem, right_ptr)

let ref_rotate_right (mem: mem) (curr_ptr: ptr) : mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root ign -> (failwith "rotate_right: curr_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_right: curr_ptr is Leaf": branch)
    | Branch curr -> curr in

  let left_ptr = curr.left in
  let left =
    match mem_get mem left_ptr with
    | Root ign -> (failwith "rotate_right: left_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_right: curr_ptr is Leaf": branch)
    | Branch left -> left in

  let left_right_ptr = left.right in

  (* move left_right under curr *)
  let mem = mem_update mem left_right_ptr (node_set_parent curr_ptr) in
  let mem = update_matching_child mem curr_ptr left_ptr left_right_ptr in

  (* move curr under left *)
  let mem = mem_update mem left_ptr (node_set_parent curr.parent) in
  let mem = mem_update mem curr_ptr (node_set_parent left_ptr) in
  let mem = update_matching_child mem left_ptr left_right_ptr curr_ptr in

  (mem, left_ptr)

let ref_balance (mem: mem) (curr_ptr: ptr) : mem * ptr =
  match mem_get mem curr_ptr with
  | Branch branch ->
    if abs (branch.left_height - branch.right_height) > 1n then
      let balance_ = branch.right_height - branch.left_height in
      let ign = assert (abs balance_ = 2n) in

      let heavy_child_ptr =
        if balance_ < 0 then branch.left else branch.right in
      let heavy_child = match mem_get mem heavy_child_ptr with
        | Branch b -> b
        | Leaf ign -> (failwith "invariant violation: heavy_child should be a branch": branch)
        | Root ign -> (failwith "invariant violation: heavy_child should be a branch": branch) in
      let heavy_child_balance =
        heavy_child.right_height - heavy_child.left_height in

      if balance_ < 0 && heavy_child_balance <= 0 then
        (* Left, Left *)
        ref_rotate_right mem curr_ptr
      else if balance_ < 0 && heavy_child_balance > 0 then
        (* Left, Right *)
        let (mem, new_) = ref_rotate_left mem heavy_child_ptr in
        let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
        ref_rotate_right mem curr_ptr
      else if balance_ > 0 && heavy_child_balance >= 0 then
        (* Right, Right*)
        ref_rotate_left mem curr_ptr
      else if balance_ > 0 && heavy_child_balance < 0 then
        (* Right, Left *)
        let (mem, new_) = ref_rotate_right mem heavy_child_ptr in
        let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
        ref_rotate_left mem curr_ptr
      else
        (failwith "invariant violation: balance_ predicates partial": mem * ptr)
    else (mem, curr_ptr)
  | Root ign -> (mem, curr_ptr)
  | Leaf ign -> (mem, curr_ptr)

type join_direction =
  | Left
  | Right

let rec ref_join
  (params: mem * join_direction * ptr * ptr * (mem -> ptr -> (mem * ptr)))
  : mem * ptr =

  let (mem, direction, left_ptr, right_ptr, callback) = params in

  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

  let focused = match direction with | Left -> left | Right -> right in
  let parent_ptr = node_parent focused in

  if abs (node_height left - node_height right) < 2n then
    let new_branch = Branch {
        left = left_ptr;
        left_height = node_height left;
        left_tez = node_tez left;
        right_tez = node_tez right;
        right_height = node_height right;
        right = right_ptr;
        parent = parent_ptr;
      } in

    let (mem, ptr) = mem_new mem new_branch in
    let mem = mem_update mem left_ptr (node_set_parent ptr) in
    let mem = mem_update mem right_ptr (node_set_parent ptr) in
    (mem, ptr)
  else if node_height left > node_height right then
    let left = node_branch left in
    ref_join
      ( mem
      , (Left)
      , left.right
      , right_ptr
      , (fun (mem: mem) (new_: ptr) ->
          let mem = update_matching_child mem left_ptr left.right new_ in
          let (mem, new_) = ref_balance mem left_ptr in
          (mem, new_))
      )
  else (* node_height left < node_height right *)
    let right = node_branch right in
    ref_join
      ( mem
      , (Right)
      , left_ptr
      , right.left
      , (fun (mem: mem) (new_: ptr) ->
          let mem = update_matching_child mem right_ptr right.left new_ in
          let (mem, new_) = ref_balance mem right_ptr in
          let mem = mem_update mem new_ (node_set_parent parent_ptr) in
          (mem, new_))
      )

let avl_push_back (mem: mem) (root_ptr: avl_ptr) (value: int) (tez: tez): mem * leaf_ptr =
  let root_ptr = match root_ptr with AvlPtr p -> p in
  let node = Leaf { value=value; tez=tez; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new mem node in
  match mem_get mem root_ptr with
  | Root r ->
    let (ptr, r) = r in
    (match ptr with
    | None ->
      let mem = mem_set mem root_ptr (Root (Some leaf_ptr, r)) in
      (mem, LeafPtr leaf_ptr)
    | Some ptr ->
      let (mem, ret) = ref_join
             (mem
             , (Left)
             , ptr
             , leaf_ptr
             , (fun (m: mem) (r: ptr) -> (m, r))
             ) in
      let mem = mem_set mem root_ptr (Root (Some ret, r)) in
      (mem, LeafPtr leaf_ptr))
  | Leaf ign ->
    (failwith "push_back is passed a non-root pointer.": mem * leaf_ptr)
  | Branch ign ->
    (failwith "push_back is passed a non-root pointer.": mem * leaf_ptr)
