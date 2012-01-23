  var
    //Preset colours, do not change these values.
    ColorBlue, ColorGreen, ColorRed, ColorWhite, ColorBlack, ColorYellow,
    ColorPink, ColorTurquoise, ColorGrey, ColorMagenta, ColorTransparent,
    ColorLightGrey: Color;
    
  procedure LoadDefaultColors();
  
implementation

  procedure LoadDefaultColors();
  begin
      ColorWhite :=         sgGraphics.ColorWhite;
      ColorGreen :=         sgGraphics.ColorGreen;
      ColorBlue :=          sgGraphics.ColorBlue ;
      ColorBlack :=         sgGraphics.ColorBlack;
      ColorRed :=           sgGraphics.ColorRed  ;
      ColorYellow :=        sgGraphics.ColorYellow;
      ColorPink :=          sgGraphics.ColorPink;
      ColorTurquoise :=     sgGraphics.ColorTurquoise;
      ColorGrey :=          sgGraphics.ColorGrey;
      ColorMagenta :=       sgGraphics.ColorMagenta;
      ColorTransparent :=   sgGraphics.ColorTransparent;
      ColorLightGrey :=     sgGraphics.ColorLightGrey;
  end;
    