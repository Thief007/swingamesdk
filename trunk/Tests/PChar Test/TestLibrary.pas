library PChar_Test;
{$H+}
uses TestUnit in 'TestUnit.pas';

	procedure WriteString(); cdecl; export;
	begin
		TestUnit.WriteString();
	end;
	
	function GetString() : String; cdecl; export;
	begin
		result := TestUnit.GetString();
	end;
	
	procedure SetString(word : PChar); cdecl; export;
	begin
		TestUnit.SetString(word);
	end;
	
exports
	WriteString,
	GetString,
	SetString;	
	
end.