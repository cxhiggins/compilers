(* lab1/memory.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

let mem = Hashtbl.create 100;;

let store x v = Hashtbl.add mem x v;;

let recall x = Hashtbl.find mem x;;
