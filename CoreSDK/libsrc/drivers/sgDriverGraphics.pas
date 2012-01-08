unit sgDriverGraphics;
//=============================================================================
// sgDriverGraphics.pas
//=============================================================================
//
// The Graphics driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the graphics driver to access the 
// current active driver. 
//
// Changing this driver will probably cause graphics drivers to break.
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
	uses sgTypes, sgDriverGraphicsSDL;
	
	type
		FillRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
    DrawRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
		DrawLineProcedure = procedure (dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
		SetPixelColorProcedure = procedure (dest : Bitmap; x, y : Integer; clr : Color);
    SetClipRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle);
    InitializeGraphicsWindowProcedure = procedure(caption : String; screenWidth, screenHeight : LongInt);
    ResizeGraphicsWindowProcedure = procedure(newWidth, newHeight : LongInt);
    SaveImageProcedure = function(bmpToSave : Bitmap; path : String) : Boolean;

	GraphicsDriverRecord = record
	  FillRectangle : FillRectangleProcedure;
		DrawLine : DrawLineProcedure;
		SetPixelColor : SetPixelColorProcedure;
    DrawRectangle : DrawRectangleProcedure;
    SetClipRectangle : SetClipRectangleProcedure;
    InitializeGraphicsWindow : InitializeGraphicsWindowProcedure;
    ResizeGraphicsWindow : ResizeGraphicsWindowProcedure;
    SaveImage : SaveImageProcedure;
	end;
	
	var
		GraphicsDriver : GraphicsDriverRecord;
		
implementation
	procedure LoadDefaultGraphicsDriver();
	begin
		LoadSDLGraphicsDriver();
	end;
	
	procedure DefaultFillRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.FillRectangle(dest, rect, clr);
	end;
	
	procedure DefaultDrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawLine(dest, x1, y1, x2, y2, clr);
	end;
	
	procedure DefaultSetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.SetPixelColor(dest, x, y, clr);
	end;
  
  procedure DefaultDrawRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawRectangle(dest, rect, clr);
	end;
  
  procedure DefaultSetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetClipRectangle(dest, rect);
  end;
	
  procedure DefaultInitializeGraphicsWindowProcedure(caption : String; screenWidth, screenHeight : LongInt);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.InitializeGraphicsWindow(caption, screenWidth, screenHeight);
  end;
  
  procedure DefaultResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.ResizeGraphicsWindow(newWidth, newHeight);
  end;
  
  function DefaultSaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.SaveImage(bmptoSave, path);
  end;

	initialization
	begin
		GraphicsDriver.FillRectangle := @DefaultFillRectangleProcedure;
		GraphicsDriver.DrawLine := @DefaultDrawLineProcedure;
		GraphicsDriver.SetPixelColor := @DefaultSetPixelColorProcedure;
    GraphicsDriver.DrawRectangle := @DefaultDrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle := @DefaultSetClipRectangleProcedure;
    GraphicsDriver.InitializeGraphicsWindow := @DefaultInitializeGraphicsWindowProcedure;
    GraphicsDriver.ResizeGraphicsWindow := @DefaultResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage := @DefaultSaveImageProcedure;
	end;
end.
	
	