program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTimers;

procedure Main();
var
  img: Bitmap;
begin
  OpenAudio();
  
  LoadResourceBundle('splash.txt');
  LoadResourceBundle('bundle.txt');
  
  OpenGraphicsWindow('Hello World', 640, 480);
  
  //Test releasing of resources - not assigned
  WriteLn('BMP: ', HexStr(BitmapNamed('fred')));
  ReleaseBitmap('fred');
  WriteLn('FNT: ', HexStr(FontNamed('fred')));
  ReleaseFont('fred');
  WriteLn('SND: ', HexStr(SoundEffectNamed('fred')));
  ReleaseSoundEffect('fred');
  WriteLn('MUS: ', HexStr(MusicNamed('fred')));
  ReleaseMusic('fred');
  // WriteLn('MAP: ', HexStr(TileMapNamed('fred')));
  // ReleaseTileMap('fred');
  ReleaseResourceBundle('fred');
  
  StartTimer(TimerNamed('TestTimer'));
  
  repeat // The game loop...
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    ClearScreen(ColorBlack);
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    DrawFramerate(0,0);
    DrawBitmap(img, 50, 50);
    RefreshScreen();
  until WindowCloseRequested();
  
  WriteLn('Time: ', TimerTicks(TimerNamed('TestTimer')));
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
