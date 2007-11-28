library HelloWorldLib;

	var	x : Integer;

	procedure SayHello(); cdecl;
	begin
		WriteLn('Hello World!');
		x := 1000;
	end ;
	
	function getX() : Integer; cdecl;
	begin
		result := x;
	end;
	
	
	
exports
	SayHello,
	getX;
end.