(* lab1/loop4.p *)

(* TEST 4: exit within nested loop statements *)

begin
  x := 0; 
  loop
    loop
      x := x+1;
      if x > 4 then exit end;
      print x; newline;
    end;
    exit;
  end
end.

(*<<
 1
 2
 3
 4
>>*)

