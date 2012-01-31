library PasLib;

procedure SayHello(); cdecl; export;
begin
  WriteLn('Hello!');
end;

exports
  SayHello;
end.