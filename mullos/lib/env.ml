(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t list

let enter env = Hashtbl.create 10 :: env

let leave env = List.tl env

let put env key value = List.hd env |> Hashtbl.add key value

let rec lookup env key =
  match env with
  | h :: t -> ( match Hashtbl.find_opt h key with Some v -> Some v | None -> lookup t key )
  | [] -> None
