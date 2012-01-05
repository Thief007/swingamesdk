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
		
		
		TextDriverRecord = record
			LoadFont : LoadFontProcedure;
			CloseFont : CloseFontProcedure;
			PrintStrings : PrintStringsProcedure;
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
	
//=============================================================================
	
	
	
	
	initialization
	begin
		TextDriver.LoadFont := @DefaultLoadFontProcedure;
		TextDriver.CloseFont := @DefaultCloseFontProcedure;
		TextDriver.PrintStrings := @DefaultPrintStringsProcedure;
	end;

end.