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
  procedure LoadOpenGLTextDriver();
    
//=============================================================================   
implementation
  uses sdl13, sgTypes, sgShared, sgDriverText, math, {$IFNDEF IOS}gl, glext, glu{$ELSE}gles11{$ENDIF}, FreeType, GLDriverUtils, 
    sgDriverGraphics;
{$ifndef IOS}
  type GLFontData = record
      textures:     array [0..127] of GLuint ;
      displaylist:  GLuint ;
      h:        Single;
    end;
    GLFont = ^GLFontData;

  var _lastError: String = '';

  const EOL = LineEnding; // from sgShared


function LoadFont(filepath: String; height: Integer) : GLFont;
var
    temp:     GLFont;
    lib:    FT_Library = nil;
    face:     FT_Face = nil;
    glyph:    FT_Glyph = nil;
    bmpGlyph: FT_BitmapGlyph = nil;
    bitmap:   PFT_Bitmap = nil;
    bitglyph:   PFT_BitmapGlyph = nil;

    w, h, xpos, ypos:   LongInt;
    i:          LongInt;
    buffer:       array of GLUByte;
    texpropx, texpropy: Single;
    pixel_size, pixel_coord: Single;
begin
    New(temp);

    glGenTextures(128, temp^.textures);
    temp^.displaylist := glGenLists(128);
    FT_Init_FreeType(@lib);
    FT_New_Face(lib, PChar(filepath), 0, @face);
    // WriteLn(HexStr(face));
    FT_Set_Char_Size(face, height shl 6, height shl 6, 96, 96);

    for i := 0 to High(temp^.textures) do
    begin
      // get bitmap
      FT_Load_Glyph(face, FT_Get_Char_Index(face, i), FT_LOAD_DEFAULT);
      FT_Get_Glyph(face^.glyph, @glyph);
      FT_Glyph_To_Bitmap(@glyph, FT_RENDER_MODE_NORMAL, nil, FT_TRUE);
      bmpGlyph := FT_BitmapGlyph(glyph);
      bitglyph := @bmpGlyph;
      bitmap := @(bmpGlyph^.bitmap);

        // WriteLn(i, ' Advance ', face^.glyph^.advance.x, ':', face^.glyph^.advance.y);
        // WriteLn('advance - glyph ', DWord(@(face^.glyph^.advance)) - dword(face^.glyph));
        // WriteLn('glyph - face ', DWord(@(face^.glyph)) - dword(face));

      w := 1;
      while w < bitmap^.width do
      begin
        w *= 2;
      end;

      h := 1;
      while h < bitmap^.rows do
      begin
        h *= 2;
      end;

        // make bitmap
        // WriteLn('w:', w, ' h:', h, ' ', 2*w*h);
        SetLength(buffer, 2*w*h); //buffer = (PGLubyte*)calloc(sizeof(GLubyte)*2*w*h, 1);
        
        for ypos := 0 to bitmap^.rows -1 do
        begin
            for xpos := 0 to bitmap^.width - 1 do
            begin
              //Write(HexStr(bitmap^.buffer[ xpos + ypos * bitmap^.pitch], 2), ' ');

                buffer[2 * (xpos + ypos * w)] :=        $FF; //bitmap^.buffer[ xpos + ypos * bitmap^.pitch];
                buffer[2 * (xpos + ypos * w) + 1] :=    bitmap^.buffer[ xpos + ypos * bitmap^.pitch];
            end;
            // WriteLn();
        end;

        // WriteLn();
        // WriteLn((bitglyph^)^.top, ', ', face^.glyph^.advance.x, ':', face^.glyph^.advance.y);

        glBindTexture( GL_TEXTURE_2D, temp^.textures[i] );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @buffer[0]);

        // make display list (for nicely formatted text)
        glPushMatrix();

        glNewList(temp^.displaylist+i, GL_COMPILE);
            glBindTexture(GL_TEXTURE_2D, temp^.textures[i]);
            // glTranslatef((bitglyph^)^.left, (bitglyph^)^.top - bitmap^.rows, 0);
            //(face^.height - face^.glyph^.metrics.horiBearingY) shr 6
            glPushMatrix();
            glScalef(1, -1, 1);

            // WriteLn(Char(i));
            // WriteLn('bearing: ', face^.glyph^.metrics.horiBearingY);
            // WriteLn('ascender: ', face^.ascender);
            // WriteLn('descender: ', face^.descender);
            // WriteLn('advance: ', face^.glyph^.advance.x);
            // WriteLn('height: ', face^.height);
            // WriteLn('underline_position: ', face^.underline_position);
            // WriteLn('bmp: ', bitmap^.rows);
            // WriteLn('top: ', (bitglyph^)^.top);
            
            pixel_size := height * 96 / 72;
            pixel_coord := (face^.ascender) * pixel_size / face^.units_per_EM;
            // WriteLn('pixel: ', pixel_coord:4:2);
            // scale := face^.height / bitmap^.rows;
            
            glTranslatef((bitglyph^)^.left, (bitglyph^)^.top - bitmap^.rows - pixel_coord, 0);
            // proportions of the texture that are the font (not padding)
            texpropx := bitmap^.width / w;
            texpropy := bitmap^.rows / h;
            
            glBegin(GL_QUADS);
             glTexCoord2f(0, 0);                glVertex3f(0, bitmap^.rows, 0);
             glTexCoord2f(0, texpropy);         glVertex3f(0, 0, 0);
             glTexCoord2f(texpropx, texpropy);  glVertex3f(bitmap^.width, 0, 0);
             glTexCoord2f(texpropx, 0);         glVertex3f(bitmap^.width, bitmap^.rows, 0);
            glEnd();
            
            glPopMatrix();
            
            glTranslatef(face^.glyph^.advance.x shr 6, face^.glyph^.advance.y shr 6, 0);
        glEndList();
        
        glPopMatrix();
        
        SetLength(buffer, 0);
    end;

    FT_Done_Face(face);
    FT_Done_FreeType(lib);

    result := temp;
end;

procedure PrintFont(temp: GLFont; text: String; x, y: Single);
begin
    glPushMatrix();
    glTranslatef(x, y, 0);

    glListBase(temp^.displaylist);
    glCallLists(Length(text), GL_UNSIGNED_BYTE, @text[1]);

    glPopMatrix();
end;

procedure FreeFont(var temp: GLFont);
begin
  if not assigned(temp) then exit;

    glDeleteTextures(128, temp^.textures);
    glDeleteLists(temp^.displaylist, 128);
    Dispose(temp);
    temp := nil;
end;

{$endif}
  
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
    //if assigned(fontToClose) then
    //  FreeFont(fontToClose^.fptr);
  end;
  

  function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
  begin
    result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
  end;

  procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags: FontAlignment);
  var
    r, g, b, a: Single;
  begin
    GraphicsDriver.DrawRectangle(dest, rc, $ffff00ff);
    FloatColors(clrFg, r, g, b, a);
    glColor4f( r, g, b, a );
    //PrintFont(font^.fptr, str, rc.x, rc.y);



    //WriteLn(rc.x:4:2, ':', rc.y:4:2);
  end;
  
  
  procedure PrintWideStringsProcedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
    begin
    exit;
  end;
  
  procedure SetFontStyleProcedure(fontToSet : Font; value : FontStyle);
  begin
    exit;
  end;
  
  function GetFontStyleProcedure(font : Font) : FontStyle;
  begin
    result := NormalFont;
  end;
  
  function SizeOfTextProcedure(font : Font; theText : String; var w : Longint ; var h : LongInt) : Integer;
  begin
    result := 0;
  end;
  
  function SizeOfUnicodeProcedure(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
  begin
    result := 0;
  end;
  
  procedure QuitProcedure();
  begin
    // nothing to undo
  end;
  
  function GetErrorProcedure() : string;
  begin
    result := ''; //_lastError;
  end;
  
  function InitProcedure() : Integer;
  begin
    result := 0;
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