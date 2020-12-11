(* lab1/repeat2.p *)

(* TEST 2: extra semicolon at body end *)

begin
  i := 0;
  repeat
    i := i + 1;
    print i; newline;
  until i = 5
end.

(*<<
 1
 2
 3
 4
 5
>>*)