program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
begin
  //OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  //Test releasing of resources - not assigned
  WriteLn('BMP: ', HexStr(BitmapNamed('fred')));
  ReleaseBitmap('fred');
  WriteLn('FNT: ', HexStr(FontNamed('fred')));
  ReleaseFont('fred');
  WriteLn('SND: ', HexStr(SoundEffectNamed('fred')));
  ReleaseSoundEffect('fred');
  WriteLn('MUS: ', HexStr(MusicNamed('fred')));
  ReleaseMusic('fred');
  WriteLn('MAP: ', HexStr(TileMapNamed('fred')));
  ReleaseTileMap('fred');
  ReleaseResourceBundle('fred');

  
  repeat // The game loop...
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  //CloseAudio();
end;

begin
  Main();
end.
