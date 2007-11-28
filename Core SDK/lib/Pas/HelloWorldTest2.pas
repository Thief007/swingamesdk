{$linklib libHelloWorldLib2.dylib}

program testsubs ;

procedure SayHello(); external 'libHelloWorldLib2.dylib';
function getX(): Integer; external 'libHelloWorldLib2.dylib';
	
begin
	WriteLn(getX);
	SayHello();
	WriteLn(getX);
	ReadLn();
end.