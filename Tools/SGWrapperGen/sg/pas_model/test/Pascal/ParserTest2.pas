program ParserTest2;

var
	variableName : String;
	secondVariable : Single;

var secondDecvar : LongInt;
begin
	while myFunc(variableName) = 5 do
	begin
		myFunc(secondVariable);
		secondVariable := secondVariable + 1;
		secondVariable := secondVariable + 1;
	end;
end.