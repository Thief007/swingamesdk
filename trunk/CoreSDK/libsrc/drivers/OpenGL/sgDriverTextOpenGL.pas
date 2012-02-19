unit sgDriverTextOpenGL;

//=============================================================================
// sgDriverTextSDL.pas
//=============================================================================
//
// This file contains SDL 1.2 specific implementation of text functions that is called by the text driver.
//
// Change History:
//  2012-01-05: Aaron : Created File.
//  
// Notes:
//    -
// TODO:
//    - 
//=============================================================================
interface
  uses 
    gl, glext, glu, FreeType;

{$ifndef IOS}

  type 
    // SwinGame type to store glyph (char) data for a font with OpenGL
    GLGlyphData = record
      texture: GLUint;                  // The texture in OpenGL
      texPropX, texPropY:   Single;     // The texure proportions
      bmpWidth, bmpHeight:  Single;     // The bitmap size
      adv:                  Integer;    // Pixels to advance after this glyph
      left, top:            Single;
    end;

    // SwinGame type to store font data for OpenGL
    GLFontData = record
      face:         FT_Face;                  // Stored to retrieve font related data upon draw
      glyphs:       array of GLGlyphData ;    // Cached character data for the font
      textures:     array of GLUint;
      displaylist:  GLuint ;                    
      baseOffset:   Single;                   // Offset to correct base during draw operations
      glyphHeight:  Single;                   // Max height of a glyph
      lineSize:     Single;                   // Height of the shape (includes line gap)
      lineGap:      Single;                   // Gap between lines
    end;
    GLFont = ^GLFontData;

  procedure LoadOpenGLTextDriver();
  function LoadFont(filepath: String; height: Integer) : GLFont;
  procedure PrintFont(font: GLFont; text: String; x, y: Single);
    
//=============================================================================   
implementation
  uses 
    SysUtils, math, 
    sdl13,  
    sgTypes, sgShared, sgDriverText, GLDriverUtils, sgDriverGraphics;

  var _lastError: String = '';

  var _freeTypeLib: FT_Library = nil;

  const EOL = LineEnding; // from sgShared

function SetupGlyphCache(font: GLFont): Boolean;
var
    numGlyphs: Integer;
    
    face:       FT_Face = nil;
    glyph:      FT_Glyph = nil;
    bmpGlyph:   FT_BitmapGlyph = nil;
    bitglyph:   PFT_BitmapGlyph = nil;
    bitmap:     PFT_Bitmap = nil;
    error:      FT_Error;

    i, idx, w, h, xpos, ypos:   LongInt;

    buffer:       array of GLUByte;

    pixel_size, pixel_coord: Single;
    texpropx, texpropy: Single;
begin
  result := false;
  if not assigned(font) then exit;
  face := font^.face;
  if not assigned(face) then exit;

  numGlyphs := face^.num_glyphs;
  //WriteLn('Loading ', numGlyphs, ' glyphs');

  // Allocate space for the font's bitmaps
  SetLength(font^.glyphs, numGlyphs);
  SetLength(font^.textures, numGlyphs);

  // Generate textures for each glyph
  glGenTextures(numGlyphs, @font^.textures[0]);
  font^.displaylist := glGenLists(numGlyphs);

  // For all the font glyphs
  for i := 0 to High(font^.textures) do
  begin
    // WriteLn('Setting up glyph ', i);
    font^.glyphs[i].texture := font^.textures[i];

    // get bitmap
    // error := FT_Load_Glyph(face, FT_Get_Char_Index(face, i), FT_LOAD_DEFAULT);
    error := FT_Load_Glyph(face, i, FT_LOAD_DEFAULT);
    if error <> 0 then
    begin
      RaiseWarning('Error creating glyph ' + IntToStr(i) + ': ' + IntToStr(error));
      continue;
    end;

    error := FT_Get_Glyph(face^.glyph, @glyph);
    if error <> 0 then
    begin
      RaiseWarning('Error getting glyph ' + IntToStr(i) + ': ' + IntToStr(error));
      continue;
    end;

    // Get the glyph's bitmap, deleting the old bmp
    error := FT_Glyph_To_Bitmap(@glyph, FT_RENDER_MODE_NORMAL, nil, FT_TRUE);
    if error <> 0 then
    begin
      RaiseWarning('Error converting glyph to bitmap ' + IntToStr(i) + ': ' + IntToStr(error));
      continue;
    end;

    bmpGlyph := FT_BitmapGlyph(glyph);
    bitglyph := @bmpGlyph;
    bitmap := @(bmpGlyph^.bitmap);

    // WriteLn(i, ' Advance ', face^.glyph^.advance.x, ':', face^.glyph^.advance.y);
    // WriteLn('advance - glyph ', DWord(@(face^.glyph^.advance)) - dword(face^.glyph));
    // WriteLn('glyph - face ', DWord(@(face^.glyph)) - dword(face));

    // Get texture width and height (power of 2)
    w := Pow2GEQ(bitmap^.width);
    h := Pow2GEQ(bitmap^.rows);
    // WriteLn('w:', w, ' h:', h, ' ', 2*w*h);

    // make bitmap
    SetLength(buffer, 2 * w * h);
    
    for ypos := 0 to bitmap^.rows -1 do
    begin
      for xpos := 0 to bitmap^.width - 1 do
      begin
        // if FT_Get_Char_Index(face, LongInt('U')) = i then
        //   Write(HexStr(bitmap^.buffer[ xpos + ypos * bitmap^.width], 2), ' ');

        buffer[2 * (xpos + ypos * w)] :=        $FF; //bitmap^.buffer[ xpos + ypos * bitmap^.pitch];
        buffer[2 * (xpos + ypos * w) + 1] :=    bitmap^.buffer[ xpos + ypos * bitmap^.width];
        // buffer[xpos + ypos * w] :=    bitmap^.buffer[ xpos + ypos * bitmap^.width];

        //WriteLn(xpos + ypos * w);
      end;
      
      for idx := xpos + 1 to w - 1 do
      begin
        // if FT_Get_Char_Index(face, LongInt('U')) = i then
        //   Write('EE ');
        buffer[2 * (idx + ypos * w)] := $00;
        buffer[2 * (idx + ypos * w) + 1] := $00;
      end;

      // if FT_Get_Char_Index(face, LongInt('U')) = i then
      //   WriteLn();
    end;

    // if FT_Get_Char_Index(face, LongInt('U')) = i then
    //   WriteLn((xpos + ypos * w), ' to ', Length(buffer) - 1);

    for idx := 2 * (xpos + ypos * w) + 2 to Length(buffer) - 1 do
    begin
      buffer[idx] := $00;
    end;

    // if FT_Get_Char_Index(face, LongInt('U')) = i then
    // begin
    //   // WriteLn();

    //   for ypos := 0 to h -1 do
    //   begin
    //     for xpos := 0 to w - 1 do
    //     begin
    //       // Write(HexStr(buffer[ 2 * (xpos + ypos * w) ], 2), ' ');
    //       Write(HexStr(buffer[ 2 * (xpos + ypos * w) + 1], 2), ' ');
    //     end;
    //     // WriteLn();
    //   end;
    // end;

    // Save char data for drawing...
    font^.glyphs[i].texPropX  := bitmap^.width / w;
    font^.glyphs[i].texPropY  := bitmap^.rows / h;
    font^.glyphs[i].bmpWidth  := bitmap^.width;
    font^.glyphs[i].bmpHeight := bitmap^.rows;
    font^.glyphs[i].adv       := face^.glyph^.advance.x shr 6;
    font^.glyphs[i].left      := (bitglyph^)^.left;
    font^.glyphs[i].top       := (bitglyph^)^.top;

    // Bind to the texture for this glyph
    glBindTexture( GL_TEXTURE_2D, font^.textures[i] );
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // Get 2D bitmap data for texture
    // WriteLn('If not working check... GL_LUMINANCE_ALPHA');
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @buffer[0] );
    glBindTexture( GL_TEXTURE_2D, 0 );
  end;

  // Clear the buffer
  SetLength(buffer, 0);
end;

function LoadFont(filepath: String; height: Integer) : GLFont;
var
  temp:     GLFont;
  //face:     FT_Face = nil;
  glyph:    FT_Glyph = nil;
  bmpGlyph: FT_BitmapGlyph = nil;
  bitmap:   PFT_Bitmap = nil;
  bitglyph:   PFT_BitmapGlyph = nil;
  error:    FT_Error;

  w, h, xpos, ypos:   LongInt;
  i:          LongInt;
  buffer:       array of GLUByte;
  texpropx, texpropy: Single;
  pixelSize, pixel_coord: Single;
begin
  result := nil;
  New(temp);
  // WriteLn('Loading ', filepath);

  error := FT_New_Face(_freeTypeLib, PChar(filepath), 0, @temp^.face);
  if error <> 0 then
  begin
    _lastError := 'Error loading font: ' + IntToStr(error);
    Dispose(temp);
    exit;
  end;

  error := FT_Set_Char_Size(temp^.face, height shl 6, height shl 6, 96, 96);
  if error <> 0 then
  begin
    _lastError := 'Error setting font size: ' + IntToStr(error);
    Dispose(temp);
    FT_Done_Face(temp^.face);
    exit;
  end;

  // Load font related data
  pixelSize := height * 96 / 72 / temp^.face^.units_per_EM; // pixel scale factor
  temp^.baseOffset  := (temp^.face^.ascender + temp^.face^.descender) * pixelSize;
  temp^.lineSize    := (temp^.face^.height) * pixelSize;
  temp^.lineGap     := temp^.lineSize - temp^.baseOffset;

  SetupGlyphCache(temp);

  result := temp;
end;

// Draw the text starting at x, y
// assumes single line
procedure PrintFont(font: GLFont; text: String; x, y: Single);
var
  px, py:   Integer;  // current positions
  i:        Integer;  // index of char in text
  glyphIdx: Integer;  // index of the glyph
  ch:       Char;
  texpropx, texpropy: Single; // Texture proportions
  bmpW, bmpH: Single;         // Bitmap size
  useKerning: Boolean;
  previous:   Integer;        // Glyph index of previous character
  delta:      FT_Vector;
  face:       FT_Face;
  adv:        Integer;        // Distance to advance between characters
  destRect:   GLRectangle;
  left:       Single;
begin
    if not assigned(font) then exit;
    face := font^.face;
    if not assigned(face) then exit;

    glPushMatrix();
    glTranslatef(x, y, 0);

    useKerning := FT_HAS_KERNING( face );
    previous := 0;

    destRect.x := 0;
    destRect.y := 0;

    glScalef(1, -1, 1);

    for i := 1 to Length(text) do
    begin
      glPushMatrix();

      ch := text[i];
      glyphIdx := FT_Get_Char_Index( face, LongWord(ch) );
      // WriteLn('ch ', ch, ' ', LongWord(ch), ' ', glyphIdx, ' tex: ', font^.textures[glyphIdx]);

      glBindTexture(GL_TEXTURE_2D, font^.textures[glyphIdx]);

      adv   := font^.glyphs[glyphIdx].adv;
      left  := font^.glyphs[glyphIdx].left;

      // // Position text with kerning
      // if useKerning and (previous > 0) and (glyphIdx > 0) then
      // begin
      //   FT_Get_Kerning( face, previous, glyphIdx, FT_KERNING_DEFAULT, @delta );
      //   if delta.x shr 6 < adv then
      //   begin
      //     adv += delta.x shr 6;
      //     left += delta.x shr 6;
      //     // WriteLn(' delta ', delta.x shr 6);
      //   end;
      // end;
      // Move to next character
      // previous := glyphIdx;

      // proportions of the texture that are the font (not padding)
      texpropx  := font^.glyphs[glyphIdx].texPropX;
      texpropy  := font^.glyphs[glyphIdx].texPropY;
      bmpW      := font^.glyphs[glyphIdx].bmpWidth;
      bmpH      := font^.glyphs[glyphIdx].bmpHeight;

      glTranslatef( left, font^.glyphs[glyphIdx].top - bmpH - font^.baseOffset, 0);

      destRect.width  := bmpW;
      destRect.height := bmpH;

      // WriteLn('pf ch: ', ch, ' ', glyphIdx, ' tex: ', font^.textures[glyphIdx], ' tx: ', texpropx:4:2, ' ty: ', texpropy:4:2, ' bmpW: ', bmpW:4:2, ' bmpH: ', bmpH:4:2, ' adv: ', adv);

      RenderTexture(font^.textures[glyphIdx], 0, texpropy, texpropx, 0, destRect); 
      glPopMatrix();

      glTranslatef(adv, 0, 0);
    end;

    glPopMatrix();
end;

procedure FreeFont(var temp: GLFont);
begin
  if not assigned(temp) then exit;

  glDeleteTextures(Length(temp^.textures), @temp^.textures[0]);
  SetLength(temp^.textures, 0);
  // glDeleteLists(temp^.displaylist, 128);
  Dispose(temp);
  temp := nil;
end;

{$endif}


// ====================
// = Helper Functions =
// ====================

  function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
  begin
    result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
  end;


// ========================
// = Driver Functions.... =
// ========================


  function InitProcedure() : Integer;
  begin
    result := FT_Init_FreeType(@_freeTypeLib);
  end;

  procedure QuitProcedure();
  begin
    if assigned(_freeTypeLib) then FT_Done_FreeType(_freeTypeLib);
    _freeTypeLib := nil;
  end;


  
  function LoadFontProcedure(fontName, filename: String; size: Longint): Font;
  begin
    {$ifndef IOS}
    New(result);
    if result = nil then 
    begin
        RaiseException('LoadFont failed to allocate space.');
        exit;
    end;

    result^.fptr := LoadFont(filename, size);
    if result^.fptr = nil then
    begin
      Dispose(result);
      result := nil;
      RaiseWarning('LoadFont failed: ' + _lastError);
      _lastError := '';
      exit;
    end;

    result^.name := fontName;
    {$else}
    result := nil;
    {$endif}
  end;
  
  procedure CloseFontProcedure(fontToClose : font);
  begin
    if assigned(fontToClose) then
      FreeFont(fontToClose^.fptr);
  end;
  


  procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags: FontAlignment);
  var
    r, g, b, a: Single;
  begin
    // FloatColors(clrBg, r, g, b, a);
    // WriteLn(clrBg, ' r:', r, ' g:', g, ' b:', b, ' a:', a);
    // glColor4f( r, g, b, a );

    GraphicsDriver.FillRectangle(dest, rc, clrBg);
    FloatColors(clrFg, r, g, b, a);
    // WriteLn(clrFg, ' r:', r, ' g:', g, ' b:', b, ' a:', a);

    glColor4f( r, g, b, a );
    PrintFont(font^.fptr, str, rc.x, rc.y);

    // WriteLn(rc.x:4:2, ':', rc.y:4:2, ' ', str);
  end;
  
  
  procedure PrintWideStringsProcedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
    begin
    exit;
  end;
  
  procedure SetFontStyleProcedure(fontToSet : Font; value : FontStyle);
  begin
    RaiseWarning('Font styles not supported, to use bold and/or italic font load a matching true type font.');
    exit;
  end;
  
  function GetFontStyleProcedure(font : Font) : FontStyle;
  begin
    result := NormalFont;
  end;
  
  function SizeOfTextProcedure(font : Font; theText : String; var w : Longint ; var h : LongInt) : Integer;
  var
    i:      Integer;
    ch:     Char = ' ';
    lineWidth: Single = 0;
    width:  Single = 0;
    height: Single = 0;
    innerFont: GLFont = nil;
    face:   FT_Face = nil;
    glyphIdx: Integer = 0;
  begin
    w := 0;
    h := 0;
    result := 0;

    if Length(theText) <= 0 then exit;
    if not assigned(font) then exit;
    innerFont := font^.fptr;
    if not assigned(innerFont) then exit;
    face := innerFont^.face;
    if not assigned(face) then exit;

    height := innerFont^.lineSize - innerFont^.lineGap;

    for i := 1 to Length(theText) do
    begin
      ch := theText[i];

      // Skip or replace CR with LF
      if (ch = #13) then
      begin
        if i = Length(theText) then break;      // finished anyway
        if theText[i + 1] = #10 then continue;  // Detected CR LF = Windows new line
        ch := #10;                              // Otherwise pretend its a newline
      end;

      if (ch = #10) then // its the End of a Line
      begin
        height += innerFont^.lineSize;
        lineWidth := 0;
        continue;
      end;

      glyphIdx := FT_Get_Char_Index( face, LongWord(ch) );
      lineWidth += innerFont^.glyphs[glyphIdx].adv;

      if lineWidth > width then width := lineWidth;
    end;

    h := Round(height);
    w := Round(width);
  end;
  
  function SizeOfUnicodeProcedure(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
  begin
    result := 0;
  end;
  
  function GetErrorProcedure() : string;
  begin
    result := _lastError;
  end;
  
    
  procedure StringColorProcedure(dest : Bitmap; x,y : single; theText : String; theColor:Color); 
  begin
    exit;
  end;
  
  procedure LoadOpenGLTextDriver();
  begin
    TextDriver.LoadFont := @LoadFontProcedure;
    TextDriver.CloseFont := @CloseFontProcedure;
    TextDriver.PrintStrings := @PrintStringsProcedure;
    TextDriver.PrintWideStrings := @PrintWideStringsProcedure;
    TextDriver.SetFontStyle := @SetFontStyleProcedure;
    TextDriver.GetFontStyle := @GetFontStyleProcedure;
    TextDriver.SizeOfText := @SizeOfTextProcedure;
    TextDriver.SizeOfUnicode := @SizeOfUnicodeProcedure;
    TextDriver.Quit := @QuitProcedure;
    TextDriver.GetError := @GetErrorProcedure;
    TextDriver.Init := @InitProcedure;
    TextDriver.StringColor  := @StringColorProcedure;
  end;
  

end.