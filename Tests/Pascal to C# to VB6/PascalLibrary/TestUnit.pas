unit TestUnit;

interface
	
	Type
		words = record
			hi:PChar;
		end;
		
	function ReciveWords(word:words):PChar;
	
	function SendInt():integer;
	
	function SendString():PChar;
	
	function ReciveInt(number:integer):integer;
	
	function ReciveString(word:PChar):PChar;
	
implementation
	
	function ReciveWords(word:words):PChar;
	begin
		result := word.hi;
	end;

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