program HowToDrawABitmap;
uses 
    sgGraphics, sgTypes, sgImages, sgUtils, sgAudio, sgResources;

procedure Main();
begin    
    OpenGraphicsWindow('How to draw a bitmap', 800, 600);
    
    ClearScreen(ColorWhite);

    LoadBitmapNamed('rocket image', 'rocket_large.png');
    DrawBitmap ('rocket image', 111, 4);
    RefreshScreen();
    Delay(5000);

    ReleaseAllResources();
end;

begin
    Main();
end.