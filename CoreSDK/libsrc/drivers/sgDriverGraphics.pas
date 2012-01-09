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
	  GetPixel32Procedure = function (bmp: Bitmap; x, y: Longint) : Color;
    PutPixelProcedure = procedure (bmp: Bitmap; clr: Color; x, y: Longint);      
    FillTriangleProcedure = procedure (dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);  
    DrawTriangleProcedure = procedure (dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);      
    FillCircleProcedure = procedure (dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
    DrawCircleProcedure = procedure (dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);      
	  FillEllipseProcedure = procedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	  DrawEllipseProcedure = procedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
		FillRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
    DrawRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
		DrawLineProcedure = procedure (dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
		SetPixelColorProcedure = procedure (dest : Bitmap; x, y : Integer; clr : Color);
    SetClipRectangleProcedure = procedure (dest : Bitmap; rect : Rectangle);
      
    ResetClipProcedure = procedure (bmp: Bitmap);  
    SetVideoModeFullScreenProcedure   = procedure ();
    SetVideoModeNoFrameProcedure      = procedure ();
      
    InitializeGraphicsWindowProcedure = procedure(caption : String; screenWidth, screenHeight : LongInt);
    ResizeGraphicsWindowProcedure = procedure(newWidth, newHeight : LongInt);
    SaveImageProcedure = function(bmpToSave : Bitmap; path : String) : Boolean;

	GraphicsDriverRecord = record
	  GetPixel32                : GetPixel32Procedure;
	  PutPixel                  : PutPixelProcedure;	  
	  FillTriangle              : FillTriangleProcedure;
	  DrawTriangle              : DrawTriangleProcedure;	  
	  FillCircle                : FillCircleProcedure;
	  DrawCircle                : DrawCircleProcedure;	  
    FillEllipse               : FillEllipseProcedure;
	  DrawEllipse               : DrawEllipseProcedure;
	  FillRectangle             : FillRectangleProcedure;
		DrawLine                  : DrawLineProcedure;
		SetPixelColor             : SetPixelColorProcedure;
    DrawRectangle             : DrawRectangleProcedure;
    SetClipRectangle          : SetClipRectangleProcedure;
    ResetClip                 : ResetClipProcedure;
    SetVideoModeFullScreen    : SetVideoModeFullScreenProcedure;
    SetVideoModeNoFrame       : SetVideoModeNoFrameProcedure;
    InitializeGraphicsWindow  : InitializeGraphicsWindowProcedure;
    ResizeGraphicsWindow      : ResizeGraphicsWindowProcedure;
    SaveImage                 : SaveImageProcedure;
	end;
	
	var
		GraphicsDriver : GraphicsDriverRecord;
		
implementation
	procedure LoadDefaultGraphicsDriver();
	begin
		LoadSDLGraphicsDriver();
	end;
	
	function DefaultGetPixel32Procedure (bmp: Bitmap; x, y: Longint) : Color;
	begin
		LoadDefaultGraphicsDriver();
		result := GraphicsDriver.GetPixel32(bmp, x, y);
	end;
	
	procedure DefaultPutPixelProcedure (bmp: Bitmap; clr: Color; x, y: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.PutPixel(bmp, clr, x, y);
	end;

  procedure DefaultFillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.FillTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;

  procedure DefaultDrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.DrawTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;
  
  procedure DefaultFillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.FillCircle(dest, clr, xc, yc, radius);
  end;

  procedure DefaultDrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.DrawCircle(dest, clr, xc, yc, radius);
  end;
	
	procedure DefaultFillEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.FillEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
	end;
	
	procedure DefaultDrawEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
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
  
  procedure DefaultResetClipProcedure(dest : Bitmap);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.ResetClip(dest);
  end;

  procedure DefaultSetVideoModeFullScreenProcedure();
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetVideoModeFullScreen();
  end;

  procedure DefaultSetVideoModeNoFrameProcedure();
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetVideoModeNoFrame();
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
		GraphicsDriver.GetPixel32               := @DefaultGetPixel32Procedure;
  	GraphicsDriver.PutPixel                 := @DefaultPutPixelProcedure;		
		GraphicsDriver.FillTriangle             := @DefaultFillTriangleProcedure;
		GraphicsDriver.DrawTriangle             := @DefaultDrawTriangleProcedure;		
		GraphicsDriver.FillCircle               := @DefaultFillCircleProcedure;
		GraphicsDriver.DrawCircle               := @DefaultDrawCircleProcedure;				
		GraphicsDriver.FillEllipse              := @DefaultFillEllipseProcedure;
		GraphicsDriver.DrawEllipse              := @DefaultDrawEllipseProcedure;
		GraphicsDriver.FillRectangle            := @DefaultFillRectangleProcedure;
		GraphicsDriver.DrawLine                 := @DefaultDrawLineProcedure;
		GraphicsDriver.SetPixelColor            := @DefaultSetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DefaultDrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @DefaultSetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @DefaultResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @DefaultSetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @DefaultSetVideoModeNoFrameProcedure;
    GraphicsDriver.InitializeGraphicsWindow := @DefaultInitializeGraphicsWindowProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @DefaultResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @DefaultSaveImageProcedure;
	end;
end.
	
	