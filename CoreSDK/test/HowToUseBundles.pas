program HowToUseBundles;
uses 
    sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources, sgText;
procedure Main();
var
    gameTime: Timer;
    explosion: sprite;
begin    
    OpenAudio();
    OpenGraphicsWindow('Using Bundles', 300, 300);

    LoadResourceBundle('how_to_bundles.txt');

    BitmapSetCellDetails(BitmapNamed('fireBmp'), 38, 38, 8, 2, 16);
    explosion := CreateSprite(BitmapNamed('fireBmp'), AnimationScriptNamed('fire'));

    SpriteSetX(explosion, 131);
    SpriteSetY(explosion, 131);

    Repeat
		ClearScreen(ColorWhite);
		DrawText('[A]nimation', ColorBlack, 0, 0);
		DrawText('[T]ext', ColorBlack, 0, 10);
		DrawText('[S]ound Effect', ColorBlack, 0, 20);
		DrawText('[M]usic', ColorBlack, 0, 30);
		DrawText('[G]ame Time', ColorBlack, 0, 40);
		DrawSprite(explosion);

		UpdateSprite(explosion);

		ProcessEvents();
		if KeyTyped(VK_A) then SpriteStartAnimation(explosion, 'FireExplosion')
		else if KeyDown(VK_T) then DrawText('Hi!!!', ColorRed, FontNamed('harabaraText'), 0, 200)
		else if KeyTyped(VK_S) then PlaySoundEffect(SoundEffectNamed('danceBeat'))
		else if KeyTyped(VK_M) then PlayMusic(MusicNamed('danceMusic'), 1);
		
		//timer;
	    RefreshScreen(60);
    until WindowCloseRequested();

    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.