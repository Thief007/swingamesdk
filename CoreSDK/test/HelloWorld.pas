program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTileMap, sgTimers;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 640, 480);
  
  //Test releasing of resources - not assigned
  WriteLn('BMP: ', HexStr(FetchBitmap('fred')));
  ReleaseBitmap('fred');
  WriteLn('FNT: ', HexStr(FetchFont('fred')));
  ReleaseFont('fred');
  WriteLn('SND: ', HexStr(FetchSoundEffect('fred')));
  ReleaseSoundEffect('fred');
  WriteLn('MUS: ', HexStr(FetchMusic('fred')));
  ReleaseMusic('fred');
  WriteLn('MAP: ', HexStr(FetchTileMap('fred')));
  ReleaseTileMap('fred');
  ReleaseResourceBundle('fred');

  
  repeat // The game loop...
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    ClearScreen(ColorBlack);
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
