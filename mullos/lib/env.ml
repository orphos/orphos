(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t list ref

let enter env = env := Hashtbl.create 10 :: !env

let leave env = env := List.tl !env

let put env key value = Hashtbl.add (List.hd !env) key value

let rec lookup env key =
  match !env with
  | h :: t -> ( match Hashtbl.find_opt h key with Some v -> Some v | None -> lookup (ref t) key )
  | [] -> None

let create () =
  let ret = ref [] in
  enter ret;
  ret
