program HowToDrawABitmap;
uses 
    sgGraphics, sgTypes, sgImages, sgUtils, sgAudio, sgResources;

procedure main();
begin    
    OpenGraphicsWindow('Import Bitmap', 800, 600);
    
    ClearScreen(ColorWhite);

    LoadBitmapNamed('image', 'SwinGame.jpg');
    DrawBitmap ('image', 171, 158);

    RefreshScreen();
    
    delay(5000);

    ReleaseAllResources();
end;

begin
    main();
end.