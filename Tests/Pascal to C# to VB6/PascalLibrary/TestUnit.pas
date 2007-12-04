unit TestUnit;

interface
	
	function SendInt():integer;
	
	function SendString():PChar;
	
	function ReciveInt(number:integer):integer;
	
	function ReciveString(word:PChar):PChar;
	
implementation
	
	function SendInt():integer;
	begin
		result := 123;
	end;
	
	function SendString():PChar;
	begin
		result := 'from pascal';
	end;
	
	function ReciveInt(number:integer):integer;
	begin
		result := number;
	end;
	
	function ReciveString(word:PChar):PChar;
	begin
		result := word;
	end;

end.