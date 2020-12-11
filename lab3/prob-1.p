(* /sheet-3/prob-1.p *)

proc double(x: integer): integer;
begin
    return x + x
end;

proc apply3(proc f(x:integer): integer): integer;
begin
    return f(3)
end;

begin
    print_num(apply3(double));
    newline()
end.