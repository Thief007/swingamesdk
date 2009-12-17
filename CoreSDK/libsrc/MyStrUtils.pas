unit MyStrUtils;

interface
uses sgTypes;

  {$ifndef FPC} // Delphi land
  function ExtractDelimited(index: integer; value: string; delim: TSysCharSet): string;
  {$endif}

  function CountDelimiter(value: String; delim: Char): LongInt;
  function CountDelimiterWithRanges(value: String; delim: Char): LongInt;
  
  function ExtractDelimitedWithRanges(index: LongInt; value: String): String;
  function ProcessRange(value: String): LongIntArray;

implementation
  uses 
    SysUtils, Math, Classes, StrUtils,
    sgShared;

  {$ifndef FPC} 
  // Delphi land
  function ExtractDelimited(index: integer; value: string; delim: TSysCharSet): string;
  var
    strs: TStrings;
  begin
    // Assumes that delim is [','] and uses simple commatext mode - better check
    if delim <> [','] then
      raise Exception.create('Internal SG bug using ExtractDelimited');
    // okay - let a stringlist do the work
    strs := TStringList.Create();
    strs.CommaText := value;
    if (index >= 0) and (index < strs.Count) then
      result := strs.Strings[index - 1]
    else
      result := '';
    // cleanup
    strs.Free();
  end;
  {$else}
  // proper ExtractDelimited provided by StrUtils
  {$endif}

  function ExtractDelimitedWithRanges(index: LongInt; value: String): String;
  var
    i, count, start: LongInt;
    inRange: Boolean;
  begin
    SetLength(result, 0);
    inRange := false;
    result := '';
    count := 1; //1 is the first index... not 0
  
    // Find the start of this delimited range
    for i := Low(value) to Length(value) do
    begin
      if count = index then break;
    
      if (not inRange) and (value[i] = ',') then
        count += 1
      else if (inRange) and (value[i] = ']') then
        inRange := false
      else if (not inRange) and (value[i] = '[') then
        inRange := true;
    end;
  
    if count <> index then exit;
    inRange := false;
    start := i;
  
    for i := start to Length(value) do
    begin
      if (not inRange) and (value[i] = ',') then
        break
      else if (inRange) and (value[i] = ']') then
        inRange := false
      else if (not inRange) and (value[i] = '[') then
        inRange := true;
    
      result += value[i];
    end;
  end;

  function CountDelimiter(value: String; delim: Char): LongInt;
  var
    i: Integer;
  begin
    result := 0;
    for i := Low(value) to Length(value) do
    begin
      if value[i] = delim then 
        result := result + 1;
    end;
  end;
  
  function CountDelimiterWithRanges(value: String; delim: Char): LongInt;
  var
    i: Integer;
    inRange: Boolean;
  begin
    inRange := false;
    result := 0;
    for i := Low(value) to Length(value) do
    begin
      if (not inRange) and (value[i] = delim) then 
        result := result + 1
      else if (value[i] = '[') then
        inRange := true
      else if (value[i] = ']') then
        inRange := false;
    end;
  end;
  
  function ProcessRange(value: String): LongIntArray;
  var
    i, j, count, temp, lowPart, highPart, dashCount: LongInt;
    part: String;
  
    procedure _AddToResult(val: LongInt);
    begin
      SetLength(result, Length(result) + 1);
      result[High(result)] := val;
    end;
  
    function MyStrToInt(str: String): LongInt;
    begin
      if Length(str) = 0 then result := 0
      else result := StrToInt(Trim(str));
    end;
  begin
    value := Trim(value);
    SetLength(result, 0);
  
    if (value[1] <> '[') or (value[Length(value)] <> ']') then
      exit; //not a range
  
    value := MidStr(value, 2, Length(value) - 2);
  
    i := 0;
    count := CountDelimiter(value, ',');
  
    while i <= count do
    begin
      part := Trim(ExtractDelimited(i + 1, value, [',']));
    
      if TryStrToInt(part, temp) then
      begin
        //just "a" so...
        _AddToResult(temp);
      end
      else //Should be range
      begin
        dashCount := CountDelimiter(part, '-');
      
        if (dashCount = 1) or ((dashCount = 2) and (part[1] <> '-')) then //a-b or a--b
          lowPart := MyStrToInt(ExtractDelimited(1, part, ['-']))
        else //assume -a...
          lowPart := -MyStrToInt(ExtractDelimited(2, part, ['-']));
      
        if (dashCount = 1) then //a-b
          highPart := MyStrToInt(ExtractDelimited(2, part, ['-']))
        else if (dashCount = 2) and (part[1] = '-') then //-a-b
          highPart := MyStrToInt(ExtractDelimited(3, part, ['-']))
        else if dashCount = 3 then //assume -a--b
          highPart := -MyStrToInt(ExtractDelimited(4, part, ['-'])) //read last string
        else if dashCount = 2 then //assume a--b
          highPart := -MyStrToInt(ExtractDelimited(3, part, ['-']))
        else
        begin
          RaiseException('Error in range.');
          SetLength(result, 0);
          exit;
        end;
      
        for j := 0 to abs(highPart - lowPart) do
        begin
          //lowPart + j * (-1 or +1)
          _AddToResult(lowPart + (j * sign(highPart - lowPart)));
        end;
      end;
    
      i := i + 1;
    end;
  end;

end.
