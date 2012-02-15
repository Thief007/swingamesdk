unit GLDriverUtils;

interface
  uses sgTypes;
  
  procedure FloatColors(c: Color; var r, g, b, a: Single);

implementation

  procedure FloatColors(c: Color; var r, g, b, a: Single);
  begin
    // writeln(c, ' = ', IntToHex(c, 8));
    // writeln(IntToHex(c and $FF000000, 8), ' -> ', IntToHex((c and $FF000000) shr 24, 8));
    a := (c and $FF000000 shr 24) / 255;
    r := (c and $00FF0000 shr 16) / 255;
    g := (c and $0000FF00 shr 8) / 255;
    b := (c and $000000FF) / 255;
  end;

end.