unit GameResources;

interface
	uses SysUtils, sgText,sgAudio, sgGraphics, sgInput, sgPhysics, sgImages, sgResources, sgTypes;

	procedure LoadResources();
	procedure FreeResources();
 
implementation

	procedure LoadFonts();
	begin
		LoadFontNamed('ArialLarge', 'arial.ttf', 80);
		LoadFontNamed('Courier', 'cour.ttf', 15);
		LoadFontNamed('CourierLarge', 'cour.ttf', 28);
	end;

	procedure LoadImages();
	var
		i: Integer;
		bmp: Bitmap;
	begin
		LoadBitmapNamed('SmallBall', 'ball_small.png');
		LoadBitmapNamed('BallImage1', 'ball.png');
		LoadBitmapNamed('BallImage2', 'ball2.png');
		LoadBitmapNamed('SmallBall', 'ball_small.png');
		LoadBitmapNamed('Running', 'running.png');
		LoadBitmapNamed('Explosion', 'Explosion.png');
		bmp := LoadBitmapNamed('Ship', 'ship.png');
		BitmapSetCellDetails(bmp, 40, 43, 2, 1, 2);
		LoadBitmapNamed('Sea', 'sea.png');
		LoadBitmapNamed('BGA', 'BackgroundDrawArea.png');
		LoadBitmapNamed('BG', 'BackgroundMain.png');
		LoadBitmapNamed('Frame1', 'F01.png');
		LoadBitmapNamed('Frame2', 'F02.png');
		LoadBitmapNamed('Frame3', 'F03.png');
		LoadBitmapNamed('Frame4', 'F04.png');
		LoadBitmapNamed('Frame5', 'F05.png');
		LoadBitmapNamed('Frame6', 'F06.png');
		LoadBitmapNamed('enShip', 'enShip.png');
		bmp := LoadTransparentBitmapNamed('BlueExplosion', 'explosion_pro.png', ColorBlack);
		BitmapSetCellDetails(bmp, 180, 180, 6, 7, 40);
		for i := 0 to 39 do
		begin
			LoadTransparentBitmapNamed('Explode_' + IntToStr(i), 'explode_' + IntToStr(i) + '.jpg', ColorBlack);
		end;
		//LoadBitmapNamed('NoImages', 'Ufo.png');
	end;

	procedure LoadSounds();
	begin
		LoadSoundEffectNamed('Shock', 'shock.wav');
	end;
	
  // procedure LoadMaps();
  // begin
  //  NewMap('test');
  //  NewMap('test3');
  // end;
	
	procedure LoadMusics();
	begin
		LoadMusicNamed('Fast', 'Fast.mp3');
	end;

	procedure LoadResources();
	begin
		//Remove sleeps once "real" game resources are being loaded
		LoadFonts();
		LoadImages();
		LoadSounds();
		LoadMusics();
    // LoadMaps();
	end;
	
	procedure FreeResources();
	begin
	 ReleaseAllResources();
	end;
end.