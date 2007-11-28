program testsubs ;

procedure SayHello(); external 'HelloWorldLib' ;
function getX(): Integer; external 'HelloWorldLib' ;
procedure setX(xy : String); external 'HelloWorldLib' ;
	
begin
	SayHello();
	setX('haha');
	SayHello();
	
	ReadLn();
end.