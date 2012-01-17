program ParserTest;

var
	maximum, minimum, current : LongInt;

function Increase(toIncrease : LongInt) : LongInt;
var
	increaseInteger : LongInt;
begin
	increaseInteger := 1;
	if (toIncrease < maximum) then
	begin
		toIncrease += increaseInteger;
	end;
	result := toIncrease;
end;

begin
	maximum := 10;
	minimum := 0;
	current := minimum;
	while (current < maximum) do
	begin
		current := Increase(current);
		WriteLn(current);
	end;
end.