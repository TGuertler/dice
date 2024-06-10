(* open Core *)

let eps = 0.00001

let assert_feq f1 f2 =
  OUnit2.assert_equal ~cmp:(fun x y ->
      (Float.compare (Float.abs (x -. y)) eps) < 0) f1 f2
    ~printer:string_of_float

let within_epsilon x y =
  (Float.compare (Float.abs (x -. y)) 0.000001) < 0
  (* Float.abs  < 0.000001 *)

let log2 a = log a /. (log 2.0)

let bit_length x = let open Core in Int.floor_log2 x + 1

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f::fs ->
      (match Sys.is_directory f with
       | true ->
        (* Changed these in order to figure out what was wrong, code does the same as code below
        let file_array = Sys.readdir nf in
        let file_list = Array.to_list file_array in
        let add_prefix = List.map (Filename.concat nf) file_list in
        let added_list = List.append fs add_prefix in 
        loop result added_list*)
        (* switched from Core.Sys to normal Sys, because Core.sys was depricated since [since 2021-04] *)
         Sys.readdir f
         |> Array.to_list
         |> List.map (Filename.concat f)
         |> List.append fs
         |> loop result
       | false -> loop (f::result) fs)
    | []    -> result
  in loop [] [dir]
