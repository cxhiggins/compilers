(* lab3/l2r.p *)

(* Check that the arguments' order is preserved *)

proc f(a, b, c);
begin
  return (a * b) + c
end;

begin
  print f(1,2,3); newline;
end.

(*<<
 5
>>*)
