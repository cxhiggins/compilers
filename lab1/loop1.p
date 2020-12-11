(* lab1/loop2.p *)

(* TEST 1: exit within while construct *)

begin
  x := 3 * 37; y := 5 * 37;
  while x <> y do
    exit;
    if x > y then
      x := x - y
    else
      y := y - x
    end
  end;
  print x; newline
end.

(*<<
>>*)


