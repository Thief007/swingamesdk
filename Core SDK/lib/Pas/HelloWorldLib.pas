library HelloWorldLib;

	type
		msg = string;

	var	x : msg;

	procedure SayHello();
	begin
		WriteLn(x);
	end ;
	
	function getX() : String;
	begin
		result := x;
	end;
	
	procedure setX(xy : String);
	begin
		x := xy;
	end;
	
	
	
exports
	SayHello,
	getX,
	setX;
end.