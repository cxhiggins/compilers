(* lab1/loop2.p *)

(* TEST 2: exit within while and if constructs *)

begin
  x := 0;
  while true do
    print x; newline;
    x := x + 1;
    if x > 0 then exit end;
  end;
  print x; newline;
end.

(*<<
 0
>>*)



