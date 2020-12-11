var k, j: integer; 
var d, e: array 6 of boolean;

begin
  k := 0;
  while k < 6 do
    d[k] := (k mod 2) <> 0;
    e[k] := true;
    k := k+1
  end;

  (* If the values for d are stored using STOREW, it will overwrite e[0] with zeroes,
     resulting in a count of 5, rather than 6 when summing the elements of e *)

  k := 0; j := 0;
  while k < 6 do
    if e[k] then j := j+1 end;
    k := k+1
  end;

  print j; newline;
end.

(*<<
 6
>>*)
