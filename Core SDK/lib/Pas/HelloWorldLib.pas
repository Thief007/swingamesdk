library HelloWorldLib;

	var	x : Integer; cvar;

	procedure SayHello(); cdecl ;
	
	begin
		WriteLn('Hello World!');
	end ;
	
	exports
		SayHello;
end.