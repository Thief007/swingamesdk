unit sgDriverText;
//=============================================================================
// sgDriverText.pas
//=============================================================================
//
// The TextDriver is responsible for providing an interface between SwinGame
// code and the drivers. It can interface between any Text driver provided
//
// Change History:
//	2012-01-05: Aaron : Created File. 
//
// Notes:
//		-
// TODO:
// 		- 
//=============================================================================

interface
	uses sgDriverTextSDL, sgTypes;
	
	type
		// These function and procedure pointers are required by the TextDriverRecord
		LoadFontProcedure = function(fontName, fileName : String; size : Longint) : font;
		CloseFontProcedure = procedure(fontToClose : font) ;
		PrintStringsProcedure = procedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
		PrintWideStringsProcedure = procedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
		SetFontStyleProcedure = procedure(fontToSet : Font; value: Fontstyle);
		GetFontStyleProcedure = function(font : Font) : FontStyle;	
		SizeOfTextProcedure = function(font : Font ;  theText : String ; var w : Longint; var h : Longint) : Integer;
		SizeOfUnicodeProcedure = function(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
		QuitProcedure = procedure();
		GetErrorProcedure = function() : string;
		InitProcedure = function() : integer;
		
		
		TextDriverRecord = record
			LoadFont : LoadFontProcedure;
			CloseFont : CloseFontProcedure;
			PrintStrings : PrintStringsProcedure;
			PrintWideStrings : PrintWideStringsProcedure;
			SetFontStyle : SetFontStyleProcedure;
			GetFontStyle : GetFontStyleProcedure;
			SizeOfText : SizeOfTextProcedure;
			SizeOfUnicode : SizeOfUnicodeProcedure;
			Quit : QuitProcedure;
			GetError : GetErrorProcedure;
			Init : InitProcedure;
		end;
		
		
	var
	// Global variable used to allow SwinGame to access the functions and procedures
	// of the audio driver.
		TextDriver : TextDriverRecord;
		
//=============================================================================
		
implementation
	procedure LoadDefaultTextDriver();
	begin
		LoadSDLTextDriver();
	end;
	
	function DefaultLoadFontProcedure(fontName, fileName : String; size : Longint) : font;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.LoadFont(fontName, fileName, size);
	end;
	
	procedure DefaultCloseFontProcedure(fontToClose : font);
	begin
		LoadDefaultTextDriver();
		TextDriver.CloseFont(fontToClose);		
	end;
	
	procedure DefaultPrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	begin
		LoadDefaultTextDriver();
		TextDriver.PrintStrings(dest,font,str,rc,clrFg,clrBg,flags)
	end;
	
	procedure DefaultPrintWideStringsProcedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	begin
		LoadDefaultTextDriver();
		TextDriver.PrintWideStrings(dest,font,str,rc,clrFg,clrBg,flags)
	end;
	
	procedure DefaultSetFontStyleProcedure(fontToSet : font; value : FontStyle);
	begin
		LoadDefaultTextDriver();
		TextDriver.SetFontStyle(fontToSet , value);
	end;
	
	function DefaultGetFontStyleProcedure(font : Font) : FontStyle;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.GetFontStyle(font);
	end;
	
	function DefaultSizeOfTextProcedure(font : Font; theText : String; var w : Longint ; var h : Longint) : Integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.SizeOfText(font, theText, w, h);
	end;
	
	function DefaultSizeOfUnicodeProcedure(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.SizeOfUnicode(font, theText, w, h);
	end;
	
	procedure DefaultQuitProcedure();
	begin
		LoadDefaultTextDriver();
		TextDriver.Quit();
	end;
	
	function DefaultGetError() : String;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.GetError();
	end;
	
	function DefaultInit(): integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.Init();
	end;
	
//=============================================================================
	
	
	
	
	initialization
	begin
		TextDriver.LoadFont := @DefaultLoadFontProcedure;
		TextDriver.CloseFont := @DefaultCloseFontProcedure;
		TextDriver.PrintStrings := @DefaultPrintStringsProcedure;
		TextDriver.PrintWideStrings := @DefaultPrintWideStringsProcedure;
		TextDriver.SetFontStyle := @DefaultSetFontStyleProcedure;
		TextDriver.GetFontStyle := @DefaultGetFontStyleProcedure;
		TextDriver.SizeOfText := @DefaultSizeOfTextProcedure;
		TextDriver.SizeOfUnicode := @DefaultSizeOfUnicodeProcedure;
		TextDriver.Quit := @DefaultQuitProcedure;
		TextDriver.GetError := @DefaultGetError;
		TextDriver.Init := @DefaultInit;
	end;

end.