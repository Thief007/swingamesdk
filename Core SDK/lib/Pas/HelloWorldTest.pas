program testsubs ;

procedure SayHello(); cdecl ; external 'HelloWorldLib' ;


var
	test : Vector;
	
begin
	SayHello();
	ReadLn();
end.