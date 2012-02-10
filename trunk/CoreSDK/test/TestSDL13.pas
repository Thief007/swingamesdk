program TestSDL13;
uses SwinGame, sdl13_image, sgInput, glu, glut, glext;

{procedure LoadBitmap();
var
  lLoadedImg : PSDL_Surface;
  offset : Rectangle;
  lTexture : GLuint;
  lFormat : GLenum;
  lNoOfColors : GLint;
begin
  lLoadedImg := IMG_Load(PChar('/Users/admin/Documents/SwingameSDK/CoreSDK/bin/Test.app/Contents/Resources/images/frog.png'));

  WriteLn('Status:  Loading image ');
  if (Assigned(lLoadedImg) ) then
  begin
   
    // Check that the image's width is a power of 2
    if ( lLoadedImg^.w  = 0 ) or ( lLoadedImg^.h = 0 ) then
      WriteLn('BadStuff');

    // get the number of channels in the SDL surface
    lNoOfColors := lLoadedImg^.format^.BytesPerPixel;
    if (lNoOfColors = 4) then    // contains an alpha channel
    begin
      if (lLoadedImg^.format^.Rmask = $000000ff) then
        lFormat := GL_RGBA
      else
        lFormat := GL_BGRA
    end else if (lNoOfColors = 3) then     // no alpha channel
    begin
      if (lLoadedImg^.format^.Rmask = $000000ff) then
        lFormat := GL_RGB
      else
        lFormat := GL_BGR;
    end else 
      WriteLn('warning: the image is not truecolor..  this will probably break\n');
            // this error should not go unhandled      
 
    // Have OpenGL generate a texture object handle for us
    glGenTextures( 1, @lTexture );
   
    // Bind the texture object
    glBindTexture( GL_TEXTURE_2D, lTexture );
   
    // Set the texture's stretching properties
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
   
    // Edit the texture object's image data using the information SDL_Surface gives us
    glTexImage2D( GL_TEXTURE_2D, 0, lNoOfColors, lLoadedImg^.w, lLoadedImg^.h, 0,
                        lFormat, GL_UNSIGNED_BYTE, lLoadedImg^.pixels );
    end else begin
    //  RaiseWarning('SDL could not load image.bmp: ', SDL_GetError());
      SDL_Quit();
    end;
// Free the SDL_Surface only if it was successfully created
  if ( Assigned(lLoadedImg) ) then
    SDL_FreeSurface( lLoadedImg );


end;}

procedure Main();
var
  bmp, bmp2 : Bitmap;
begin      
  OpenGraphicsWindow('Herp', 1000, 1000);
  ClearScreen();
  bmp := LoadBitmap('/Users/admin/Documents/SwingameSDK/CoreSDK/bin/Test.app/Contents/Resources/images/frog.png');
  bmp2 := LoadBitmap('/Users/admin/Documents/SwingameSDK/CoreSDK/bin/Test.app/Contents/Resources/images/swinburne.jpg');

  repeat 
    ProcessEvents();
    DrawBitmap(bmp, 0, 0);
    DrawBitmap(bmp2, 0, 0);
    RefreshScreen();
  until WindowCloseRequested();
end;

begin
	main();
end.