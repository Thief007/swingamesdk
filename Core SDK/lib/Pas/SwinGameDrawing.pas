unit SwinGameDrawing;

interface
uses SwinGameAPI, SDL;
  procedure DrawPixel(surface: PSDL_Surface; x, y: Integer; color: Color);
  function  DrawHorizontalLine(surface: PSDL_Surface;y, x1, x2: Integer; color: Color): Boolean;
  function  DrawVerticalLine(surface: PSDL_Surface; x, y1, y2: Integer; color: Color): Boolean;
  function  DrawLine(surface: PSDL_Surface; x1, y1, x2, y2: Integer; color: Color): Boolean;
  function  DrawCircle(surface: PSDL_Surface; xc, yc, radius: Integer; color: Color): Boolean;
  procedure FillCircle(surface: PSDL_Surface; xc, yc, radius: Integer; color: Color);
  procedure DrawEllipse(surface: PSDL_Surface; x0, y0, xRadius, yRadius: Integer; color: Color);
  procedure FillEllipse(surface: PSDL_Surface; x0, y0, xRadius, yRadius: Integer; color: Color);
  
implementation
	//uses SDL_gfx;

//  function ToSDLGFXColor(color: UInt32): UInt32;
//  var
//	temp: TSDL_Color;
//  begin
//	// from ARGB to RGBA
//
//    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN } //4321 = ARGB
//      temp.r := (color and $00FF0000) shr 16;
//      temp.g := (color and $0000FF00) shr 8;
//      temp.b := (color and $000000FF);
//	  temp.unused := (color and $FF000000) shr 24;
//    {$ELSE} //1234 = BGRA
//      temp.r := (color and $0000FF00) shr 8;
//      temp.g := (color and $00FF0000) shr 16;
//      temp.b := (color and $FF000000) shr 24;
//    {$IFEND}
//
//	result := UInt32((temp.r shl 24) or (temp.g shl 16) or (temp.b shl 8) or temp.unused);
//  end;

procedure PutPixel(surface: PSDL_Surface; x, y: Integer; color: Color);
var
  bufP: PUInt32;
  pixels: PUint32;
  addr: UInt32;
begin
  if (x < 0) or (x >= surface.w) or (y < 0) or (y >= surface.h) then exit;

  pixels := surface.pixels;
  //addr := Integer(pixels) + (( y * surface.w ) + x) * surface.format.BytesPerPixel;
  addr := Integer(pixels) + (x * surface.format.BytesPerPixel) + (y * surface.pitch) ;
  bufp := PUint32(addr);
  bufp^ := color;
  //PixelColor(surface, x, y, ToSDLGFXColor(color));
end;

procedure DrawPixel(surface: PSDL_Surface; x, y: Integer; color: Color);
begin
  if (x < 0) or (x >= surface.w) or (y < 0) or (y >= surface.h) then exit;

  if SDL_MUSTLOCK(surface) then SDL_LockSurface(surface);

  PutPixel(surface, x, y, color);

  if SDL_MUSTLOCK(surface) then SDL_UnlockSurface(surface);
end;

//Fast horizontal line from (x1,y) to (x2,y), with rgb color
function DrawHorizontalLine(surface: PSDL_Surface;y, x1, x2: Integer; color: Color): Boolean;
var
  w, h, x, addr: Integer;
  bufP: PUInt32;
  pixels: PUint32;
begin
  w := surface.w;
  h := surface.h;

  if x2 < x1 then //swap x1 and x2, x1 must be the leftmost endpoint
  begin
    x1 := x1 + x2;
    x2 := x1 - x2;
    x1 := x1 - x2;
  end;

  if (x2 < 0) or (x1 > w - 1) or (y < 0) or (y > h - 1) then
  begin
   result := false;
   exit; //no single point of the line is on screen
  end;

  if x1 < 0 then x1 := 0;
  if x2 >= w then x2 := w - 1;

  //hlineColor(surface, x1, x2, y, ToSDLGFXColor(color));
  //result := true;

  pixels := surface.pixels;
  //addr := Integer(pixels) + (( y * surface.w ) + x1 - 1) * surface.format.BytesPerPixel;
  addr := Integer(pixels) + ((x1 - 1) * surface.format.BytesPerPixel) + (y * surface.pitch);
  bufp := PUint32(addr);

  if SDL_MUSTLOCK(surface) then SDL_LockSurface(surface);

  for x := x1 to x2 do
  begin
    Inc(bufp);
    bufp^ := color;
  end;
  result := true;

  if SDL_MUSTLOCK(surface) then SDL_UnlockSurface(surface);
end;

//Fast vertical line from (x,y1) to (x,y2), with rgb color

function DrawVerticalLine(surface: PSDL_Surface; x, y1, y2: Integer; color: Color): Boolean;
var
  w, h, y, addr: Integer;
  bufP: PUInt32;
  pixels: PUint32;
begin
  w := surface.w;
  h := surface.h;

  if y2 < y1 then  //swap y1 and y2
  begin
    y1 := y1 + y2;
    y2 := y1 - y2;
    y1 := y1 - y2;
  end;

  if (x < 0) or (x > w - 1) or (y2 < 0) or (y1 > h - 1) then
  begin
    result := false;
    exit;
  end;

  if y1 < 0 then y1 := 0;
  if y2 >= h then y2 := h - 1;
  
  //vlinecolor(surface, x, y1, y2, ToSDLGFXColor(color));

  pixels := surface.pixels;
  //addr := Integer(pixels) + (( y1 * surface.w ) + x) * surface.format.BytesPerPixel;
  addr := Integer(pixels) + (x * surface.format.BytesPerPixel) + (y1 * surface.Pitch);
  bufp := PUint32(addr);

  if SDL_MUSTLOCK(surface) then SDL_LockSurface(surface);

  for y := y1 to y2 - 1 do
  begin
    bufp := PUInt32(Integer(bufp) + (surface.pitch));
    bufp^ := color;
  end;

  if SDL_MUSTLOCK(surface) then SDL_UnlockSurface(surface);

  result := true;
end;

//Bresenham line from (x1,y1) to (x2,y2) with rgb color

function DrawLine(surface: PSDL_Surface; x1, y1, x2, y2: Integer; color: Color): Boolean;
var
  x, y: Integer;
  deltaX, deltaY: Integer;
  xinc1, xinc2, yinc1, yinc2, den, num, numadd, numpixels, curpixel: Integer;
begin
  {w := surface.w;
  h := surface.h;

  if (x1 < 0) or (x1 > w - 1) or (x2 < 0) or
     (x2 > w - 1) or (y1 < 0) or (y1 > h - 1) or (y2 < 0) or (y2 > h - 1) then
  begin
    result := false;
    exit;
  end;}

  deltax := abs(x2 - x1);        // The difference between the x's
  deltay := abs(y2 - y1);        // The difference between the y's

  x := x1;                        // Start x off at the first pixel
  y := y1;                        // Start y off at the first pixel

  if x2 >= x1 then                // The x-values are increasing
  begin
    xinc1 := 1;
    xinc2 := 1;
  end
  else                            // The x-values are decreasing
  begin
    xinc1 := -1;
    xinc2 := -1;
  end;

  if y2 >= y1 then                // The y-values are increasing
  begin
    yinc1 := 1;
    yinc2 := 1;
  end
  else                            // The y-values are decreasing
  begin
    yinc1 := -1;
    yinc2 := -1;
  end;

  if deltax >= deltay then         // There is at least one x-value for every y-value
  begin
    xinc1 := 0;                  // Don't change the x when numerator >= denominator
    yinc2 := 0;                  // Don't change the y for every iteration

    den := deltax;

    num := deltax div 2;

    numadd := deltay;

    numpixels := deltax;         // There are more x-values than y-values
  end
  else                          // There is at least one y-value for every x-value
  begin
    xinc2 := 0;                  // Don't change the x for every iteration
    yinc1 := 0;                  // Don't change the y when numerator >= denominator
    den := deltay;
    num := deltay div 2;

    numadd := deltax;

    numpixels := deltay;         // There are more y-values than x-values
  end;

  for curpixel := 0 to numpixels do
  begin
    PutPixel(surface, x, y, color);  // Draw the current pixel

    num := num + numadd;              // Increase the numerator by the top of the fraction

    if num >= den then           // Check if numerator >= denominator
    begin
      num := num - den;             // Calculate the new numerator value
      x := x + xinc1;             // Change the x as appropriate
      y := y + yinc1;             // Change the y as appropriate
    end;

    x := x + xinc2;                 // Change the x as appropriate
    y := y + yinc2;                 // Change the y as appropriate
  end;

  result := true;
end;

//Bresenham circle with center at (xc,yc) with radius and red green blue color

function DrawCircle(surface: PSDL_Surface; xc, yc, radius: Integer; color: Color): Boolean;
var
  x, y, p: Integer;
  a, b, c, d, e, f, g, h: Integer;
begin
  {w := surface.w;
  h1 := surface.h;

  if (xc - radius < 0) or (xc + radius >= w) or (yc - radius < 0) or (yc + radius >= h1) then
  begin
    result := false;
    exit;
  end;}

  x := 0;
  y := radius;
  p := 3 - (radius shl 1);

  while x <= y do
  begin
    a := xc + x; //8 pixels can be calculated at once thanks to the symmetry
    b := yc + y;
    c := xc - x;
    d := yc - y;
    e := xc + y;
    f := yc + x;
    g := xc - y;
    h := yc - x;

    PutPixel(surface, a, b, color);
    PutPixel(surface, c, d, color);
    PutPixel(surface, e, f, color);
    PutPixel(surface, g, f, color);

    if x > 0 then //avoid drawing pixels at same position as the other ones
    begin
      PutPixel(surface, a, d, color);
      PutPixel(surface, c, b, color);
      PutPixel(surface, e, h, color);
      PutPixel(surface, g, h, color);
    end;

    if p < 0 then
    begin
      p := p + (x shl 2) + 6;
      x := x + 1;
    end
    else
    begin
      p := p + ((x - y) shl 2) + 10;
      x := x + 1;
      y := y - 1;
    end;
  end;

  result := true;
end;

//Filled bresenham circle with center at (xc,yc) with radius and red green blue color

procedure FillCircle(surface: PSDL_Surface; xc, yc, radius: Integer; color: Color);
var
  x, y, p: Integer;
  a, b, c, d, e, f, g, h: Integer;
  pb, pd: Integer; //previous values: to avoid drawing horizontal lines multiple times
begin
  {w := surface.w;
  h1 := surface.h;

  if (xc + radius < 0) or (xc - radius >= w) or (yc + radius < 0) or (yc + radius >= h1) then
  begin
    result := false;
    exit;
  end;}

  x := 0;
  y := radius;
  p := 3 - (radius shl 1);

  pb := -1;
  pd := -1;
  
  while x <= y do
  begin
    // write data

    a := xc + x;
    b := yc + y;
    c := xc - x;
    d := yc - y;
    e := xc + y;
    f := yc + x;
    g := xc - y;
    h := yc - x;

    if b <> pb then DrawHorizontalLine(surface, b, a, c, color);
    if d <> pd then DrawHorizontalLine(surface, d, a, c, color);
    if f <> b  then DrawHorizontalLine(surface, f, e, g, color);
    if (h <> d) and (h <> f) then DrawHorizontalLine(surface, h, e, g, color);

    pb := b;
    pd := d;

    if p < 0 then
    begin
      p := p + (x shl 2) + 6;
      x := x + 1;
    end
    else
    begin
      p := p + ((x - y) shl 2) + 10;
      x := x + 1;
      y := y - 1;
    end;
  end;
end;

procedure DrawEllipse(surface: PSDL_Surface; x0, y0, xRadius, yRadius: Integer; color: Color);
var
  x, y: Integer;
  xChange, yChange: Integer;
  ellipseError: Integer;
  twoASquare, twoBSquare: Integer;
  stoppingX, stoppingY: Integer;
begin
  twoASquare := 2 * Xradius * Xradius;
  twoBSquare := 2 * Yradius * Yradius;

  // 1st set of points
  x := Xradius - 1;  // radius zero == draw nothing
  y := 0;

  xChange := yRadius * yRadius * (1 - 2 * xRadius);
  yChange := xRadius * xRadius;

  ellipseError := 0;

  stoppingX := twoBSquare * xRadius;
  stoppingY := 0;

  //Lock surface
  if SDL_MUSTLOCK(surface) then
  begin
      if SDL_LockSurface(surface) < 0 then exit;
  end;

  // Plot four ellipse points by iteration
  while stoppingX > stoppingY do
  begin
    PutPixel(surface, x0 + x, y0 + y, color);
    PutPixel(surface, x0 - x, y0 + y, color);
    PutPixel(surface, x0 + x, y0 - y, color);
    PutPixel(surface, x0 - x, y0 - y, color);

    y := y + 1;
    stoppingY := stoppingY + twoASquare;
    ellipseError := ellipseError + Ychange;
    yChange := yChange + twoASquare;

    if (2 * ellipseError + xChange) > 0 then
    begin
      x := x - 1;
      stoppingX := stoppingX - twoBSquare;
      ellipseError := ellipseError + xChange;
      xChange := xChange + twoBSquare;
    end;
  end;

  // 2nd set of points
  x := 0;
  y := yRadius - 1;  //radius zero == draw nothing
  xChange := yRadius * yRadius;
  yChange := xRadius * xRadius * (1 - 2 * yRadius);
  ellipseError := 0;
  stoppingX := 0;
  stoppingY := twoASquare * yRadius;

  //Plot four ellipse points by iteration
  while stoppingX < stoppingY do
  begin
    PutPixel(surface, x0 + x, y0 + y, color);
    PutPixel(surface, x0 - x, y0 + y, color);
    PutPixel(surface, x0 + x, y0 - y, color);
    PutPixel(surface, x0 - x, y0 - y, color);

    x := x + 1;
    stoppingX := stoppingX + twoBSquare;
    ellipseError := ellipseError + xChange;
    xChange := xChange + twoBSquare;

    if (2 * ellipseError + yChange) > 0 then
    begin
      y := y - 1;
      stoppingY := stoppingY - TwoASquare;
      ellipseError := ellipseError + yChange;
      yChange := yChange + twoASquare;
    end;
  end;

  // Unlock surface
  if SDL_MUSTLOCK(surface) then  SDL_UnlockSurface(surface);
end;

procedure FillEllipse(surface: PSDL_Surface; x0, y0, xRadius, yRadius: Integer; color: Color);
var
  x, y: Integer;
  xChange, yChange: Integer;
  ellipseError: Integer;
  twoASquare, twoBSquare: Integer;
  stoppingX, stoppingY: Integer;
begin
  twoASquare := 2 * Xradius * Xradius;
  twoBSquare := 2 * Yradius * Yradius;

  // 1st set of points
  x := Xradius - 1;  // radius zero == draw nothing
  y := 0;

  xChange := yRadius * yRadius * (1 - 2 * xRadius);
  yChange := xRadius * xRadius;

  ellipseError := 0;

  stoppingX := twoBSquare * xRadius;
  stoppingY := 0;

  //Lock surface
  if SDL_MUSTLOCK(surface) then
  begin
      if SDL_LockSurface(surface) < 0 then exit;
  end;

  // Plot four ellipse points by iteration
  while stoppingX > stoppingY do
  begin
    DrawHorizontalLine(surface, y0 + y, x0 - x, x0 + x, color);
    DrawHorizontalLine(surface, y0 - y, x0 - x, x0 + x, color);

    y := y + 1;
    stoppingY := stoppingY + twoASquare;
    ellipseError := ellipseError + Ychange;
    yChange := yChange + twoASquare;

    if (2 * ellipseError + xChange) > 0 then
    begin
      x := x - 1;
      stoppingX := stoppingX - twoBSquare;
      ellipseError := ellipseError + xChange;
      xChange := xChange + twoBSquare;
    end;
  end;

  // 2nd set of points
  x := 0;
  y := yRadius - 1;  //radius zero == draw nothing
  xChange := yRadius * yRadius;
  yChange := xRadius * xRadius * (1 - 2 * yRadius);
  ellipseError := 0;
  stoppingX := 0;
  stoppingY := twoASquare * yRadius;

  //Plot four ellipse points by iteration
  while stoppingX < stoppingY do
  begin
    DrawHorizontalLine(surface, y0 + y, x0 - x, x0 + x, color);
    DrawHorizontalLine(surface, y0 - y, x0 - x, x0 + x, color);

    x := x + 1;
    stoppingX := stoppingX + twoBSquare;
    ellipseError := ellipseError + xChange;
    xChange := xChange + twoBSquare;

    if (2 * ellipseError + yChange) > 0 then
    begin
      y := y - 1;
      stoppingY := stoppingY - TwoASquare;
      ellipseError := ellipseError + yChange;
      yChange := yChange + twoASquare;
    end;
  end;

  // Unlock surface
  if SDL_MUSTLOCK(surface) then  SDL_UnlockSurface(surface);
end;

end.
