(* lab2/arraydim.p *)

var a, b: array 4 of integer;
var i, j: integer;

begin
  i := 0;
  while i < 4 do
    b[i] := i;
    i := i + 1
  end;

  i := 0;
  while i < 4 do
    a[b[i]] := b[i]+1;
    print a[b[i]]; newline;
    i := i + 1
  end;
end.

(*<<
 1
 2
 3
 4
>>*)
