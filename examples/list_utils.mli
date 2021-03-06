(** Utility functions for lists. *)

(* [sample l size] sample [size] element from [l] with replacement. *)
val sample : 'a list -> size:int -> 'a list

(* [group_by l ~f] groups [l] into lists corresponding to the key generated by
   [f]. *)
val group_by : 'a list -> f:('a -> 'key) -> 'a list list

(* [scan l ~init ~f] returns a list of reduced values of [l] from the left
   starting with [init]. *)
val scan : 'a list -> init:'b -> f:('b -> 'a -> 'b) -> 'b list
