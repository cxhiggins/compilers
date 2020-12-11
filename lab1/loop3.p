(* lab1/loop3.p *)

(* TEST 3: exit within a loop *)

begin
  x := 0;
  loop
    x := x + 1;
    if x >= 10 then exit end;
    print x; newline; 
  end
end.

(*<<
 1
 2
 3
 4
 5
 6
 7
 8
 9
>>*)


