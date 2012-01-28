program HowToDrawAnAnimation;
uses 
    sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources;

procedure Main();
var
    explosion: Animation;
begin
    OpenAudio();
    OpenGraphicsWindow('Create Animation', 200, 200);
	
	LoadResourceBundle('explosion_bundle.txt');
	
    explosion := CreateAnimation('explosion_loop', AnimationScriptNamed('explosionScrpt'));
	
    repeat
	    ProcessEvents();
        ClearScreen(ColorWhite);
        DrawAnimation(explosion, BitmapNamed('explosionBmp'), 64, 64);
        RefreshScreen();
    until WindowCloseRequested();

    Delay(800);

    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.