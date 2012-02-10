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
  uses sdl13, sgtypes, sgDriverText;
  
  const EOL = LineEnding; // from sgShared

  
  function LoadFontProcedure(fontName, filename: String; size: Longint): Font;
  begin
    result := nil;
  end;
  


  
  function ToSDLColor(color: Longword): SDL_Color;
  begin
    begin
      result.r := 0;

      result.g := 0;
      result.b := 0;
      exit;
    end;

  end;
  
  procedure CloseFontProcedure(fontToClose : font);
  begin
    exit
  end;
  
  function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
    begin
      result := false
    end;

  procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
  begin
    exit;
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
  
  Procedure QuitProcedure();
  begin
    exit;
  end;
  
  function GetErrorProcedure() : string;
  begin
    result := ''
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