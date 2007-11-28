library HelloWorldLib2;

	var	x : Integer;

	procedure SayHello(); cdecl; export;
	begin
		WriteLn('Hello World!');
		x := 1000;
	end ;
	
	function getX() : Integer; cdecl; export;
	begin
		result := x;
	end;
	
	
	
exports
	SayHello,
	getX;
end.