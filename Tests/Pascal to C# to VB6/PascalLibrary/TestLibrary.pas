library TestLibrary;
{$H+}
uses TestUnit in 'TestUnit.pas';

	function SendInt():integer; cdecl; export;
	begin
		result := TestUnit.SendInt();
	end;
	
	function SendString():PChar; cdecl; export;
	begin
		result := TestUnit.SendString();
	end;
	
	function ReciveInt(number:integer):integer; cdecl; export;
	begin
		result := TestUnit.ReciveInt(number);
	end;
	
	function ReciveString(word:PChar):PChar; cdecl; export;
	begin
		result := TestUnit.ReciveString(word);
	end;
	
exports
	ReciveString,
	ReciveInt,
	SendString,
	SendInt;	
	
end.