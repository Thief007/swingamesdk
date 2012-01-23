program HowToDrawFramesPerSecond;
uses 
    sgGraphics, sgInput, sgText, sgUtils, sgResources;

procedure Main();
begin
    OpenGraphicsWindow('How To Draw Framerate To The Screen', 400, 300);

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawFramerate(10,8);
        RefreshScreen();
    until WindowCloseRequested();
  
    ReleaseAllResources();
end;

begin
    Main();
end.