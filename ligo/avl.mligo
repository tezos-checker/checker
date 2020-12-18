(* avl_types *)

type ptr = Ptr of int
let null_ptr = Ptr 0

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
  mem: (ptr, node) big_map;
  max_id: int;
}

let mem_new (value: node) (mem: mem) : mem * ptr =
  let new_id = mem.max_id + 1 in
  let new_mem = {
        mem = Big_map.update (Ptr new_id) (Some value) mem.mem;
        max_id = new_id;
      } in
  (new_mem, Ptr new_id)

let mem_get (ptr: ptr) (mem: mem) : node =
  Big_map.find ptr mem.mem

let mem_del (ptr: ptr) (mem: mem) : mem =
  { mem = Big_map.update ptr (None: node option) mem.mem;
    max_id = mem.max_id
  }

let mem_set (ptr: ptr) (value: node) (mem: mem) : mem =
  let ign = assert (int_of_ptr ptr <= mem.max_id) in
  { mem = Big_map.update ptr (Some value) mem.mem;
    max_id = mem.max_id
  }

let mem_update (k: ptr) (f: node -> node) (m: mem) : mem =
  mem_set k (f (mem_get k m)) m

(* avl implementation *)

let node_tez (n: node) : tez =
  match n with
  | Leaf leaf -> leaf.tez
  | Branch branch -> branch.left_tez + branch.right_tez
  | Root d -> (failwith "node_tez found Root": tez)

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

let avl_mk_empty (root_data: int) (mem: mem) : mem * avl_ptr =
  let mem = {
    mem = (Big_map.empty: (ptr, node) big_map);
    max_id = 0;
  } in
  let (mem, ptr) = mem_new (Root ((None: ptr option), root_data)) mem in
  (mem, AvlPtr ptr)

let update_matching_child
    (ptr: ptr) (from_ptr: ptr) (to_ptr: ptr) (mem: mem): mem =
  match mem_get ptr mem with
  | Root m ->
    let (b, r) = m in
    (* let ign = assert (b = Some from_ptr) in *)
    mem_set ptr (Root ((Some to_ptr), r)) mem
  | Leaf ign ->
    (failwith "update_matching_child: got a leaf": mem
)  | Branch old_branch ->
    let to_ = mem_get to_ptr mem in
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
    mem_set ptr new_branch mem

let ref_rotate_left (curr_ptr: ptr) (mem: mem): mem * ptr =
  let curr =
    match mem_get curr_ptr mem with
    | Root ign -> (failwith "rotate_left: curr_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_left: curr_ptr is Leaf": branch)
    | Branch curr -> curr in

  let right_ptr = curr.right in
  let right =
    match mem_get right_ptr mem with
    | Root ign -> (failwith "rotate_left: right_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_left: right_ptr is Leaf": branch)
    | Branch right -> right in

  let right_left_ptr = right.left in

  (* move right_left under curr *)
  let mem = mem_update right_left_ptr (node_set_parent curr_ptr) mem in
  let mem = update_matching_child curr_ptr right_ptr right_left_ptr mem in

  (* move curr under right *)
  let mem = mem_update right_ptr (node_set_parent curr.parent) mem in
  let mem = mem_update curr_ptr (node_set_parent right_ptr) mem in
  let mem = update_matching_child right_ptr right_left_ptr curr_ptr mem in

  (mem, right_ptr)

let ref_rotate_right (curr_ptr: ptr) (mem: mem): mem * ptr =
  let curr =
    match mem_get curr_ptr mem with
    | Root ign -> (failwith "rotate_right: curr_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_right: curr_ptr is Leaf": branch)
    | Branch curr -> curr in

  let left_ptr = curr.left in
  let left =
    match mem_get left_ptr mem with
    | Root ign -> (failwith "rotate_right: left_ptr is Root": branch)
    | Leaf ign -> (failwith "rotate_right: curr_ptr is Leaf": branch)
    | Branch left -> left in

  let left_right_ptr = left.right in

  (* move left_right under curr *)
  let mem = mem_update left_right_ptr (node_set_parent curr_ptr) mem in
  let mem = update_matching_child curr_ptr left_ptr left_right_ptr mem in

  (* move curr under left *)
  let mem = mem_update left_ptr (node_set_parent curr.parent) mem in
  let mem = mem_update curr_ptr (node_set_parent left_ptr) mem in
  let mem = update_matching_child left_ptr left_right_ptr curr_ptr mem in

  (mem, left_ptr)

let ref_balance (curr_ptr: ptr) (mem: mem): mem * ptr =
  match mem_get curr_ptr mem with
  | Branch branch ->
    if abs (branch.left_height - branch.right_height) > 1n then
      let balance_ = branch.right_height - branch.left_height in
      let ign = assert (abs balance_ = 2n) in

      let heavy_child_ptr =
        if balance_ < 0 then branch.left else branch.right in
      let heavy_child = match mem_get heavy_child_ptr mem with
        | Branch b -> b
        | Leaf ign -> (failwith "invariant violation: heavy_child should be a branch": branch)
        | Root ign -> (failwith "invariant violation: heavy_child should be a branch": branch) in
      let heavy_child_balance =
        heavy_child.right_height - heavy_child.left_height in

      if balance_ < 0 && heavy_child_balance <= 0 then
        (* Left, Left *)
        ref_rotate_right curr_ptr mem
      else if balance_ < 0 && heavy_child_balance > 0 then
        (* Left, Right *)
        let (mem, new_) = ref_rotate_left heavy_child_ptr mem in
        let mem = update_matching_child curr_ptr heavy_child_ptr new_ mem in
        ref_rotate_right curr_ptr mem
      else if balance_ > 0 && heavy_child_balance >= 0 then
        (* Right, Right*)
        ref_rotate_left curr_ptr mem
      else if balance_ > 0 && heavy_child_balance < 0 then
        (* Right, Left *)
        let (mem, new_) = ref_rotate_right heavy_child_ptr mem in
        let mem = update_matching_child curr_ptr heavy_child_ptr new_ mem in
        ref_rotate_left curr_ptr mem
      else
        (failwith "invariant violation: balance_ predicates partial": mem * ptr)
    else (mem, curr_ptr)
  | Root ign -> (mem, curr_ptr)
  | Leaf ign -> (mem, curr_ptr)

type join_direction =
  | Left
  | Right

let rec ref_join
  (params: mem * join_direction * ptr * ptr * (ptr -> mem -> (mem * ptr)))
  : mem * ptr =

  let (mem, direction, left_ptr, right_ptr, callback) = params in

  let left = mem_get left_ptr mem in
  let right = mem_get right_ptr mem in

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

    let (mem, ptr) = mem_new new_branch mem in
    let mem = mem_update left_ptr (node_set_parent ptr) mem in
    let mem = mem_update right_ptr (node_set_parent ptr) mem in
    (mem, ptr)
  else if node_height left > node_height right then
    let left = node_branch left in
    ref_join
      ( mem
      , (Left)
      , left.right
      , right_ptr
      , (fun (new_: ptr) (mem: mem) ->
          let mem = update_matching_child left_ptr left.right new_ mem in
          let (mem, new_) = ref_balance left_ptr mem in
          (mem, new_))
      )
  else (* node_height left < node_height right *)
    let right = node_branch right in
    ref_join
      ( mem
      , (Right)
      , left_ptr
      , right.left
      , (fun (new_: ptr) (mem: mem) ->
          let mem = update_matching_child right_ptr right.left new_ mem in
          let (mem, new_) = ref_balance right_ptr mem in
          let mem = mem_update new_ (node_set_parent parent_ptr) mem in
          (mem, new_))
      )

let avl_push_back (root_ptr: avl_ptr) (value: int) (tez: tez) (mem: mem) : mem * leaf_ptr =
  let root_ptr = match root_ptr with AvlPtr p -> p in
  let node = Leaf { value=value; tez=tez; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new node mem in
  match mem_get root_ptr mem with
  | Root r ->
    let (ptr, r) = r in
    (match ptr with
    | None ->
      let mem = mem_set root_ptr (Root (Some leaf_ptr, r)) mem in
      (mem, LeafPtr leaf_ptr)
    | Some ptr ->
      let (mem, ret) = ref_join
             (mem
             , (Left)
             , ptr
             , leaf_ptr
             , (fun (r: ptr) (m: mem) -> (m, r))
             ) in
      let mem = mem_set root_ptr (Root (Some ret, r)) mem in
      (mem, LeafPtr leaf_ptr))
  | Leaf ign ->
    (failwith "push_back is passed a non-root pointer.": mem * leaf_ptr)
  | Branch ign ->
    (failwith "push_back is passed a non-root pointer.": mem * leaf_ptr)

let rec ref_split
  (p: mem * ptr * tez * (ptr option -> ptr option -> mem -> (mem * ptr option * ptr option)))
  : mem * ptr option * ptr option =
  let (mem, curr_ptr, limit, callback) = p in
  match mem_get curr_ptr mem with
  | Root ign -> (failwith "ref_split found Root": mem * ptr option * ptr option)
  | Leaf leaf ->
    if leaf.tez <= limit
    then
      let mem = mem_update curr_ptr (node_set_parent null_ptr) mem in
      (mem, Some curr_ptr, (None: ptr option))
    else
      (mem, (None: ptr option), Some curr_ptr)
  | Branch branch ->

    if branch.left_tez + branch.right_tez <= limit
    then (* total_tez <= limit *)
      let mem = mem_update curr_ptr (node_set_parent null_ptr) mem in
      (mem, Some curr_ptr, (None: ptr option))
    else
      let mem = mem_del curr_ptr mem in
      let mem = mem_update branch.right (node_set_parent branch.parent) mem in

      if branch.left_tez = limit
      then (* left_tez = limit *)
        (mem, Some branch.left, Some branch.right)
      else if limit < branch.left_tez
      then (* limit < left_tez < total_tez *)
        ref_split
          ( mem
          , branch.left
          , limit
          , (fun (left: ptr option) (rightM: ptr option) (mem: mem) ->
              let right = (match rightM with | None -> (failwith "impossible": ptr) | Some r -> r) in
              let (mem, r) = ref_join
                ( mem
                , (Right)
                , right
                , branch.right
                , (fun (r: ptr) (m: mem) -> (mem, r))
                ) in
              (mem, left, Some r)
            )
          )
      else (* left_tez < limit < total_tez *) (
        let left = mem_get branch.left mem in
        ref_split
          ( mem
          , branch.right
          , limit - node_tez left
          , (fun (leftM: ptr option) (right: ptr option) (mem: mem) ->
              match leftM with
               | Some left ->
                   let (mem, joined) = ref_join
                     ( mem
                     , (Left)
                     , branch.left
                     , left
                     , (fun (r: ptr) (m: mem) -> (mem, r))
                     ) in
                   let mem =
                     match right with
                     | None -> mem
                     | Some r -> mem_update r (node_set_parent branch.parent) mem in
                   (mem, Some joined, right)
               | None ->
                   let mem = mem_update branch.left (node_set_parent null_ptr) mem in
                   (mem, Some branch.left, right)
            )
          )
      )

let avl_take (root_ptr: avl_ptr) (limit: tez) (root_data: int) (mem: mem)
  : mem * avl_ptr =
  let root_ptr = match root_ptr with AvlPtr p -> p in
  match mem_get root_ptr mem with
  | Root p -> (
    let (p, data) = p in
    match p with
    | Some r ->
        let (mem, l, r) = ref_split (mem, r, limit
                                    , (fun (a: ptr option) (b: ptr option) (c: mem) -> (c, a, b))) in
        let (mem, new_root) = mem_new (Root (l, root_data)) mem in
        let mem = match l with
          | Some l -> mem_update l (node_set_parent new_root) mem
          | None -> mem in
        let mem = mem_set root_ptr (Root (r, data)) mem in
        (mem, AvlPtr new_root)
    | None ->
        let (mem, new_root) = mem_new (Root ((None: ptr option), root_data)) mem in
        (mem, AvlPtr new_root)
    )
  | Leaf ign -> (failwith "invariant violation: avl_ptr does not point to a Root": mem * avl_ptr)
  | Branch ign -> (failwith "invariant violation: avl_ptr does not point to a Root": mem * avl_ptr)
