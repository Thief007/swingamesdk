program testsubs ;

procedure SayHello(); cdecl ; external 'HelloWorldLib' ;
function getX(): Integer; cdecl; external 'HelloWorldLib' ;
	
begin
	WriteLn(getX);
	SayHello();
	WriteLn(getX);
	ReadLn();
end.