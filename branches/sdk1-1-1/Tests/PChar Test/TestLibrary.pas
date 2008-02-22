library PChar_Test;
{$H+}
uses TestUnit in 'TestUnit.pas';

type
	IntArray = Array of Integer;

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

	procedure PrintArray(length : Integer; arr : IntArray); cdecl; export;
	var
		i : Integer;
		arr2 : IntArray;
	begin
		arr2 := arr;
		WriteLn(length);
		SetLength(arr2, length);
		for i:= Low(arr2) to High(arr2) do
		begin
			WriteLn(arr2[i]);
		end;
		
	end;
	
exports
	WriteString,
	GetString,
	SetString,
	PrintArray;	
	
end.