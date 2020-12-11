(* lab2/kgen.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Dict 
open Tree 
open Keiko 
open Print

let optflag = ref false

(* |line_number| -- find line number of variable reference *)
let rec line_number e =
  match e.e_guts with
      Variable x -> x.x_line
    | Sub (a, e) -> line_number a
    | _ -> 999

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr e =
  match e.e_guts with
      (* Part 3 — Uses the appropriate LOAD instruction based on the typesize of e *)
      Variable _ | Sub _ -> 
        if typesize e.e_type = 4 then SEQ [gen_addr e; LOADW] else SEQ [gen_addr e; LOADC]
    | Constant (n, t) ->
        CONST n
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]

(* |gen_addr| -- generate code to push address of a variable *)
and gen_addr v =
  match v.e_guts with
      Variable x ->
        let d = get_def x in
        SEQ [LINE x.x_line; GLOBAL d.d_lab]
    | Sub (x, e) ->
        (* Part 3 — Calculates the size of the array's basetype, 
           multiplies by the subscript,
           checks it against the full array size,
           and offsets the address of x by this amount if possible *)
        let size = typesize (basetype x.e_type) and a_size = typesize x.e_type and line = 999 in (match e.e_guts with
            Constant (n, t) -> 
              SEQ [gen_addr x; CONST (n*size); CONST a_size; BOUND line; OFFSET]
          | _ ->
            if size = 1 then 
              SEQ [gen_addr x; gen_expr e; CONST a_size; BOUND line; OFFSET]
            else 
              SEQ [gen_addr x; gen_expr e; CONST size; BINOP Times; CONST a_size; BOUND line; OFFSET]
        )
    | _ ->
        failwith "gen_addr"

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond e tlab flab =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e.e_guts with
      Constant (x, t) ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2;
          JUMPC (w, tlab); JUMP flab]
    | Monop (Not, e1) ->
        gen_cond e1 flab tlab
    | Binop (And, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond e1 lab1 flab; LABEL lab1; gen_cond e2 tlab flab]
    | Binop (Or, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond e1 tlab lab1; LABEL lab1; gen_cond e2 tlab flab]
    | _ ->
        SEQ [gen_expr e; CONST 0; JUMPC (Neq, tlab); JUMP flab]

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt =
  function
      Skip -> NOP
    | Seq stmts -> SEQ (List.map gen_stmt stmts)
    | Assign (v, e) -> 
        (* Part 3 — Uses the appropriate STORE instruction based on the typesize of e *)
        if typesize e.e_type = 4 then 
          SEQ [LINE (line_number v); gen_expr e; gen_addr v; STOREW] 
        else 
          SEQ [LINE (line_number v); gen_expr e; gen_addr v; STOREC]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "lib.print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "lib.newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond test lab1 lab2; 
          LABEL lab1; gen_stmt thenpt; JUMP lab3;
          LABEL lab2; gen_stmt elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [JUMP lab2; LABEL lab1; gen_stmt body; 
          LABEL lab2; gen_cond test lab1 lab3; LABEL lab3]

let gen_decl (Decl (xs, t)) =
  List.iter (fun x ->
      let d = get_def x in
      let s = typesize t in   (* Part 3 — Allocates space based on type *)
      printf "GLOVAR $ $\n" [fStr d.d_lab; fNum s]) xs

(* |translate| -- generate code for the whole program *)
let translate (Program (ds, ss)) = 
  let code = gen_stmt ss in
  printf "PROC MAIN 0 0 0\n" [];
  Keiko.output (if !optflag then Peepopt.optimise code else code);
  printf "RETURN\n" [];
  printf "END\n\n" [];
  List.iter gen_decl ds