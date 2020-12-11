(* lab2/array1.p *)

(* Test that arrayOutOfBounds error raised *)

var a: array 10 of integer;
var i: integer;

begin
  print a[11]; newline;
end.

(*<<
Runtime error: array bound error on line 999 in module Main
In procedure MAIN
>>*)
