module Deque : sig

  (* <debug>, todo remove *)
  type 'a item  = {mutable prev : 'a item option ; value: 'a ; mutable next : 'a item option}
  type 'a deque = {mutable front : 'a item option ; mutable back : 'a item option }
  (* </debug> *)
  (*  type 'a deque *)
  val empty      : unit -> 'a deque
  val is_empty   : 'a deque -> bool
  val push_front : 'a -> 'a deque -> unit
  val peek_front : 'a deque -> 'a
  val pop_front  : 'a deque -> 'a
  val push_back  : 'a -> 'a deque -> unit
  val pop_back   : 'a deque -> 'a
  val to_list    : 'a deque -> 'a list
  val pp         : ('a -> string) -> 'a deque -> string
end = struct  
  type 'a item  = {mutable prev : 'a item option ; value: 'a ; mutable next : 'a item option}
  type 'a deque = {mutable front : 'a item option ; mutable back : 'a item option }
                  
  let empty () = {front = None ; back = None}

  
  let is_empty dq = (dq.front = None)
                    
  let push_front value dq =
    match dq.front with
    | None -> 
      let item = {prev = None ; value ; next = None} in
      dq.front <- Some item ; dq.back <- Some item
    | Some old_front ->                                       
      let front = Some {prev = None ; value ; next = dq.front} in
      old_front.prev <- front ; dq.front <- front

  let push_back value dq =
    match dq.back with
    | None ->      
      let item = {prev = None ; value ; next = None} in
      dq.front <- Some item ; dq.back <- Some item
    | Some old_back ->                                                
      let back = Some {prev = dq.back ; value ; next = None} in
      old_back.next <- back ; dq.back <-back     
  
  let pop_front dq =
    match dq.front with
    | None -> failwith "empty deque"
    | Some front -> (match front.next with
        | None -> dq.front <- None ; dq.back <- None ; front.value
        | Some next -> dq.front <- front.next ; next.prev <- None ; front.value)

  let peek_front dq =
    match dq.front with
    | None -> failwith "empty deque"
    | Some front -> front.value
          
  let pop_back dq =
    match dq.back with
    | None -> failwith "empty deque"
    | Some back -> (match back.prev with
        | None -> dq.front <- None ; dq.back <- None ; back.value
        | Some prev -> dq.back <- back.prev ; prev.next <- None ; back.value)
      
  let to_list dq = 
    let result = ref []
    and iter = ref dq.back in
    while !iter != None do
      let Some item = !iter in
      result := item.value :: (!result) ;
      iter := item.prev
    done ; !result

  let pp f dq =
    let result = ref ""
    and iter = ref dq.back in
    while !iter != None do
      let Some item = !iter in
      result := (f item.value) ^ "\n" ^ (!result) ;
      iter := item.prev
    done ; !result
    
end

module FQ : sig

  (* <debug>, todo remove *)
  type 'a tree = {height : int ; root : 'a root}
  and  'a root = {node : 'a node ; weight : int}
  and  'a node = Leaf of 'a | Node of {left : 'a root ; right : 'a root}
  type 'a forest = ('a tree) Deque.deque
  (* </debug> *)
  (* type 'a forest *)
  val empty : unit -> 'a forest
  val push  : 'a -> int -> 'a forest -> unit
  val take  : int -> 'a forest -> 'a forest

  val pp :  ('a -> string) -> 'a forest -> string

end = struct

  type 'a tree = {height : int ; root : 'a root}
  and  'a root = {node : 'a node ; weight : int}
  and  'a node = Leaf of 'a | Node of {left : 'a root ; right : 'a root}

  type 'a forest = ('a tree) Deque.deque

  let merge tree_a tree_b =
    assert (tree_a.height == tree_b.height) ;
    {height = tree_a.height + 1 ;
     root = {
       weight = tree_a.root.weight + tree_b.root.weight ;
       node = Node {left = tree_a.root ; right = tree_b.root}}}
    
  let empty () =
    Deque.empty ()

  let push_tree tree forest =
    let finished = ref false
    and insert = ref tree in    
    while not !finished do
      (* see note [1] *)      
      if (not (Deque.is_empty forest)) && (Deque.peek_front forest).height = (!insert).height then        
        let front = Deque.pop_front forest in
        insert := merge !insert front
      else
        (Deque.push_front !insert forest ; finished := true)
    done

  let push value weight forest =
    push_tree {height = 0 ; root = {node = Leaf value ; weight }} forest

  let take weight forest =
    let grabbed = empty ()
    and finished = ref false
    and weight_rem = ref weight in
    while not !finished do      
      if Deque.is_empty forest || !weight_rem = 0 then
        finished := true
      else
        let back = Deque.pop_back forest in
        if back.root.weight <= !weight_rem then
          (weight_rem := !weight_rem - back.root.weight ;
           push_tree back grabbed)
        else
          (match back.root.node with
           | Leaf _ ->
             if back.root.weight = 1 then
               (push_tree back grabbed ; finished := true)
             else
               let gone = {back with root = {back.root with weight = !weight_rem}}
               and stay = {back with root = {back.root with weight = back.root.weight- !weight_rem}} in
               (push_tree gone grabbed ; Deque.push_back stay forest ; finished := true)
           | Node node ->             
             (Deque.push_back {height = back.height - 1 ; root = node.left} forest ;
              Deque.push_back {height = back.height - 1 ; root = node.right} forest))
    done ; grabbed              

  let rec print_node f = function
    | Leaf x -> f x
    | Node node -> "(" ^ (print_node f node.left.node) ^ "," ^ (print_node f node.right.node) ^ ")"

  let print_tree f tree = print_node f tree.root.node

  let pp f forest =
    Deque.pp (print_tree f) forest
        
                        
end


(* [1]
   This code might seem fine... we're looking at the forest to see if it's empty,
   if not we look at the height of the first tree, then if it's the same height
   we remove it and work on placing back the newly merged tree. This looks 
   fine for most programs but written like this, we are going to be reading
   the front pointers of the queue 3 or 4 times over when this could be
   done very cheaply, in place. On a normal program, the CPU cache takes care
   of this, but not in a smart-contract where every read inside a bigmap
   is as costly in terms of gas as a read on disk because the gas model does
   not assume a cache. So the abstraction of a deque must eventually be
   broken and the thing implemented directly, in place. *)
