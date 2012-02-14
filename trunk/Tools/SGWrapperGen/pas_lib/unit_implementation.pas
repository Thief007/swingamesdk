Procedure LoadDefaultColors();

var
  ColorWhite, ColorGreen, ColorBlue, ColorBlack, ColorRed, 
  ColorYellow, ColorTurquoise, ColorGrey, ColorMagenta, 
  ColorTransparent, ColorLightGrey : Color;

implementation

  procedure LoadDefaultColors();
  begin
      ColorWhite :=         $FFFFFFFF;
      ColorGreen :=         $FF00FF00;
      ColorBlue :=          $FF0000FF;
      ColorBlack :=         $FF000000;
      ColorRed :=           $FFFF0000;
      ColorYellow :=        $FFFFFF00;
      ColorPink :=          $FFFF1493;
      ColorTurquoise :=     $FF00CED1;
      ColorGrey :=          $FF808080;
      ColorMagenta :=       $FF00FFFF;
      ColorTransparent :=   $00000000;
      ColorLightGrey :=     $FFC8C8C8;
  end;
    