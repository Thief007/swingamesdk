unit sgDriverGraphicsOpenGL;
//=============================================================================
// sgDriverGraphicsSDL.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//    - Pascal PChar is equivalent to a C-type string
//    - Pascal Word is equivalent to a Uint16
//    - Pascal LongWord is equivalent to a Uint32
//    - Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
uses sgTypes, SDL13, sgNetworking, SysUtils,gl;

  type
    OpenGLWindow = record
        window  : PSDL_window;
        context : SDL_GLContext;  
    end;
  
    POPenGLWindow = ^OpenGLWindow;


  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  procedure LoadOpenGLGraphicsDriver();
  function ToGfxColorProcedure(val : Color): Color;
  procedure DrawSurfaceToRenderer();
  procedure DrawDirtyScreen();
  
implementation
  uses sgDriverGraphics, sgShared, Math, sgDriverImages;  

  procedure DrawDirtyScreen();
  begin  

      exit

  end;

  procedure DisposeSurface(surface : Pointer);
  begin
    exit;
  end;
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color; 
  begin
    result := 0;
  end;

  function CreateSurface(width, height : LongInt) : PSDL_Surface;  
  begin
    result := nil;
  end;  
  
  // Used to draw Primitive Shapes to a Surface -> Bitmap - > Renderer.
  procedure DrawSurfaceToRenderer();
  begin  
    exit;
  end;
  
  function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
  begin
    result := 0;
  end;
    
  procedure PutPixelProcedure(bmp: Bitmap; clr: Color; x, y: Longint);
  begin
    exit;
  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin
    // writeln(c, ' = ', IntToHex(c, 8));
    // writeln(IntToHex(c and $FF000000, 8), ' -> ', IntToHex((c and $FF000000) shr 24, 8));
    a := c and $FF000000 shr 24;
    r := c and $00FF0000 shr 16;
    g := c and $0000FF00 shr 8;
    b := c and $000000FF;
  end;
  
  function ToGfxColorProcedure(val : Color): Color;
  begin
    RaiseException('Do not use SDL_gfx');
    result := 0;
  end;
  
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  begin
    result.x := 0;
    result.y := 0;
    result.w := Word(0);
    result.h := Word(0);
  end;  
  
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  begin
      result := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  procedure SetRenderDrawColor(c : Color);
  begin
    exit;
  end;
  
  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  var
    r,g,b,a : Byte;

  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    
    GraphicsDriver.ColorComponents(clr,r,g,b,a);


    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_LINE_STRIP);
    glVertex2f(x1, y1);
    glVertex2f(x2, y2);
    glVertex2f(x3, y3);
    glVertex2f(x1, y1);
    glEnd();
  end;
  
  procedure GetLengthFromPoints(pos : array of Single; var lnth : LongInt; var lowest : Single );
  begin
    exit;
  end;




  procedure PresentAndFreeShapes(surface : PSDL_Surface; srcRect, destRect : SDL_Rect);
  begin
    exit;
  end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  var
    r,g,b,a : Byte;

  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    
    GraphicsDriver.ColorComponents(clr,r,g,b,a);


    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_TRIANGLES);
    glVertex2f(x1, y1);
    glVertex2f(x2, y2);
    glVertex2f(x3, y3);
    glEnd();
  end;
  
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);

  var
    rad : Single;
    r,g,b,a : Byte;
    angle : LongInt;
  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_LINE_LOOP);
      for angle := 0 to 360 do
      begin
        rad := DegToRad(angle);
        glVertex2f(xc + sin(rad) * radius, yc + cos(rad) * radius);
      end;
    glEnd();
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);

  var
    rad : Single;
    r,g,b,a : Byte;
    angle : LongInt;
  begin
    if(dest <> screen) then
      RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_TRIANGLE_FAN);
      glVertex2f(xc, yc);
      for angle := 0 to 360 do
      begin
        rad := DegToRad(angle);
        glVertex2f(xc + sin(rad) * radius, yc + cos(rad) * radius);
      end;
    glEnd();
  end;
  
  // This procedure draws an Ellipse to a bitmap
  procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);

  var
    
    rad : Single;
    r,g,b,a : Byte;
    angle : LongInt;
  begin
    if(dest <> screen) then
      RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    GraphicsDriver.ColorComponents(clr,r,g,b,a);

    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_TRIANGLE_FAN);
    for angle := 0 to 360 do
      begin
        rad := DegToRad(angle);
        glVertex2f(xPos + cos(rad)*halfWidth*2, yPos + sin(rad)*halfHeight*2);
      end;
    glEnd();

  end;
  
  // This procedure draws an Ellipse to a bitmap
  procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);

  var
    rad : Single;
    r,g,b,a : Byte;
    angle : LongInt;
  begin
    
    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_LINE_LOOP);
      for angle := 0 to 360 do
      begin
        rad := DegToRad(angle);
        glVertex2f(xPos + cos(rad)*halfWidth*2, yPos + sin(rad)*halfHeight*2);
      end;
    glEnd();

  end;
  
  // This procedure draws a filled rectangle to a bitmap
  procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
  var
    
    x1,x2,y1,y2 : LongInt;
    r,g,b,a : Byte;

  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    x1 := Round(rect.x);
    x2 := x1 + rect.width;
    y1 := Round(rect.y);
    y2 := y1 + rect.height;

    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_TRIANGLE_STRIP);
    glVertex2f(x1, y1);
    glVertex2f(x1, y2);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1,y2);

    glEnd();
  end;
  
  procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
  var
    x1,x2,y1,y2 : LongInt;
    r,g,b,a : Byte;

  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;
    x1 := Round(rect.x);
    x2 := x1 + rect.width;
    y1 := Round(rect.y);
    y2 := y1 + rect.height;

    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_LINE_LOOP);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
    glEnd();
  end;
  
  // This procedure draws a line on a bitmap between two points
  procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
  var
    r,g,b,a : Byte;

  begin
  if(dest <> screen) then
    RaiseWarning("Drawing primitives to bitmap is currently not supported with the OpenGL driver");exit;

    GraphicsDriver.ColorComponents(clr,r,g,b,a);



    glColor4f(r/255, g/255, b/255, a/255);
    glBegin(GL_LINES);
    glVertex2f(x1, y1);
    glVertex2f(x2, y2);
    glEnd();
  end;
  
  // This procedure sets the color of a pixel on a bitmap
  procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
  begin
    exit;
  end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
    exit;
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    exit;
  end;

  procedure SetVideoModeFullScreenProcedure();
  begin
    exit;
  end;

  procedure SetVideoModeNoFrameProcedure();
  begin    
    // TODO: need to recreate window to switch to borderless... check further if needed
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen(screenWidth, screenHeight: Longint);
  begin
  end;
  
  function AvailableResolutions() : ResolutionArray;
  var
    resolutions : ResolutionArray;
  begin
    setLength(resolutions,0);

    result := resolutions;
    
  end;
    procedure RefreshScreenProcedure(screen : Bitmap);
  begin
    SDL_GL_SwapWindow(POpenGLWindow(_screen)^.window);
  end;
  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  // var
  //     sdlScreen : PSDL13Screen;
  begin
    // Initialize SDL.
    if (SDL_Init(SDL_INIT_VIDEO) < 0) then exit;
    
    New(POpenGLWindow(_screen));
    
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    
    // Create the window where we will draw.
    {$IFDEF IOS}
      POpenGLWindow(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) or Uint32(SDL_WINDOW_BORDERLESS));
    {$ELSE}
      POpenGLWindow(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) );
    {$ENDIF}

      POpenGLWindow(_screen)^.context := SDL_GL_CreateContext(POpenGLWindow(_screen)^.window);

    //VSYNC
    SDL_GL_SetSwapInterval(1);


    if screen = nil then 
      New(screen);
    if (screen^.surface = nil) then 
      screen^.surface := POpenGLWindow(_screen)^.window;

    ImagesDriver.ClearSurface(screen,0);
    RefreshScreenProcedure(nil);

    glEnable( GL_TEXTURE_2D );
    glEnable (GL_BLEND);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    glOrtho (0, screenWidth, screenHeight, 0, 0, 1);
    glDisable(GL_DEPTH_TEST);
    _SetupScreen(screenWidth, screenHeight);
  end;
  
  // This resizes the graphics window used by SwinGame
  procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin
    exit;
  end;
  
  // This resizes the graphics window used by SwinGame
  procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    exit;
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    result := false;
  end;
  

  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
    result := 0;
  end;

  function GetSurfaceWidthProcedure(src : Bitmap) : LongInt;
  begin
    result := 0;
  end;
  
  function GetSurfaceHeightProcedure(src : Bitmap) : LongInt;
  begin
    result := 0;
  end;
  
  function SurfaceFormatAssignedProcedure(src: Bitmap) : Boolean;
  begin
    result := false;
  end;
  
  procedure GetRGBProcedure(pixel : Byte; r,g,b : Byte); 
    begin
    exit;
  end;

  function GetScreenWidthProcedure(): LongInt; 
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(POpenGLWindow(_screen)^.window,@w, @h);
    result := w;
  end;
  
  function GetScreenHeightProcedure(): LongInt;
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(POpenGLWindow(_screen)^.window,@w, @h);
    result := h;
  end;
  

  procedure LoadOpenGLGraphicsDriver();
  begin
    GraphicsDriver.GetPixel32               := @GetPixel32Procedure;
    GraphicsDriver.PutPixel                 := @PutPixelProcedure;    
    GraphicsDriver.FillTriangle             := @FillTriangleProcedure;
    GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;    
    GraphicsDriver.FillCircle               := @FillCircleProcedure;
    GraphicsDriver.DrawCircle               := @DrawCircleProcedure;    
    GraphicsDriver.FillEllipse              := @FillEllipseProcedure;
    GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;   
    GraphicsDriver.FillRectangle            := @FillRectangleProcedure;
    GraphicsDriver.DrawLine                 := @DrawLineProcedure;
    GraphicsDriver.SetPixelColor            := @SetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @ResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;    
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @SaveImageProcedure;
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;
    GraphicsDriver.GetSurfaceWidth          := @GetSurfaceWidthProcedure;
    GraphicsDriver.GetSurfaceHeight         := @GetSurfaceHeightProcedure;
    GraphicsDriver.GetRGB                   := @GetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;
    GraphicsDriver.AvailableResolutions := @AvailableResolutions;
  end;
end.