unit SGSDK_Graphics;

interface
	uses	SGSDK_Core;
	
	type
		/// Record: Sprite
		///
		///	NOTE: Do not use SpriteData directly. Use Sprite.
		///
		///	- bitmaps: The array of bitmaps related to the Sprite
		///	- xPod, yPos: The sprites location within the game world
		///	- currentFrame: The current animation frame for the Sprite
		///	- usePixelCollision: A flag indicating if pixel collision sould be
		///											 used, if false bounding collision is used.
		SpriteData = record
			bitmaps : Array of Bitmap;
			xPos : Single;
			yPos : Single;
			currentFrame : Integer;
			usePixelCollision: Boolean;
		end;
		
		/// Type: Sprite
		///
		///	Sprites are used to represent Sprites drawn to the screen. Create a
		///	sprite using the CreateSprite function, and free it when complete with
		///	the FreeSprite function. The sprite contain a number of bitmaps used to
		///	store animations, or the like. Sprite drawing operations will draw the
		///	Sprite's current frame.
		Sprite = ^SpriteData;
		
	//*****
	//
	// Resource loading and freeing routines
	//
	//*****
	//
	// These routines are used to load resources, and to free them.
	//
	
	function CreateBitmap(width, height: Integer): Bitmap;
	
	procedure OptimiseBitmap(surface: Bitmap);
	
	function LoadBitmap(pathToBitmap: String; transparent: Boolean;
								transparentColor: Colour): Bitmap; overload;
	function LoadBitmap(pathToBitmap : String): Bitmap; overload;
	
	function LoadTransparentBitmap(pathToBitmap : String;
								transparentColor : Colour): Bitmap; overload;
	
	procedure FreeBitmap(var bitmapToFree : Bitmap);
	
	//*****
	//
	// Bitmap drawing routines
	//
	//*****
	//
	// These routines are used to draw to a bitmap.
	//

	procedure ClearSurface(dest: Bitmap; toColour: Colour); overload;
	procedure ClearSurface(dest: Bitmap); overload;
	
	procedure DrawBitmap(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
		overload;
	
	procedure DrawBitmapPart(dest: Bitmap; bitmapToDraw: Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); overload;
	
	procedure DrawPixel(dest: Bitmap; theColour: Colour; x, y: Integer);
		overload;

	procedure DrawRectangle(dest: Bitmap; theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); overload;

	procedure DrawRectangle(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure FillRectangle(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure DrawLine(dest: Bitmap; theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); overload;

	procedure DrawHorizontalLine(dest: Bitmap; theColor: Color;
								 y, x1, x2: Integer); overload;

	procedure DrawVerticalLine(dest: Bitmap; theColor: Color;
							 x, y1, y2: Integer); overload;

	procedure DrawCircle(dest: Bitmap; theColour: Colour; filled: Boolean;
							 xc, yc, radius: Integer); overload;

	procedure DrawCircle(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); overload;

	procedure FillCircle(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); overload;

	procedure DrawEllipse(dest: Bitmap; theColour: Colour; filled: Boolean;
							xPos, yPos, width, height: Integer); overload;

	procedure DrawEllipse(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); overload;
	
	procedure FillEllipse(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); overload;

	//*****
	//
	// Screen drawing routines
	//
	//*****
	//
	// These routines are used to draw directly to the screen.
	//

	procedure ClearScreen(toColour : Colour); overload;

	procedure ClearScreen(); overload;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Integer); overload;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); overload;
	
	procedure DrawPixel(theColour: Colour; x, y: Integer); overload;

	procedure DrawRectangle(theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); overload;

	procedure DrawRectangle(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure FillRectangle(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure DrawLine(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); overload;

	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Integer); overload;

	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Integer); overload;

	procedure DrawCircle(theColour: Colour; filled: Boolean;
						 xc, yc, radius: Integer); overload;

	procedure DrawCircle(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure FillCircle(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure DrawEllipse(theColour: Colour; filled: Boolean;
						xPos, yPos, width, height: Integer); overload;

	procedure DrawEllipse(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;

	procedure FillEllipse(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;

	//*****
	//
	// Sprite routines
	//
	//*****
	//
	// These routines are used to work with Sprites within your game.
	//

	function	CreateSprite(startBitmap : Bitmap): Sprite;

	procedure FreeSprite(var spriteToFree : Sprite);

	function AddBitmapToSprite(spriteToAddTo : Sprite;
														 bitmapToAdd : Bitmap): Integer;

	function CurrentHeight(sprite: Sprite): Integer;

	function CurrentWidth(sprite: Sprite): Integer;

	procedure DrawSprite(spriteToDraw : Sprite); overload;

	procedure DrawSprite(spriteToDraw : Sprite; vwPrtX, vwPrtY, vwPrtWidth,
											 vwPrtHeight : Integer); overload;

	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector);

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer);

	function IsSpriteOffscreen(theSprite : Sprite): Boolean; overload;

	function	IsSpriteOffscreen(theSprite : Sprite; vwPrtX, vwPrtY,
															vwPrtWidth, vwPrtHeight : Integer) : Boolean; overload;

implementation
	function NewSDLRect(x, y, w, h: Integer): SDL_Rect;
	begin
		if w < 0 then
		begin
			result.x := x + w;
			w := -w;
		end
		else result.x := x;

		if h < 0 then
		begin
			result.y := y + h;
			h := -h;
		end
		else result.y := y;

		result.w := Word(w);
		result.h := Word(h);
	end;
	
	function GetPixel32(surface: PSDL_Surface; x, y: Integer): Colour;
	var
		pixels, pixel: PUint32;
		offset, pixelAddress: Uint32;
	begin
		//Convert the pixels to 32 bit
		pixels := surface.pixels;

		//Get the requested pixel
		offset := (( y * surface.w ) + x) * surface.format.BytesPerPixel;
		pixelAddress := uint32(pixels) + offset;

		{$IFDEF FPC}
			pixel := PUint32(pixelAddress);
		{$ELSE}
			pixel := Ptr(pixelAddress);
		{$ENDIF}

		{$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
		case surface.format.BytesPerPixel of
			1: result := pixel^ and $000000ff;
			2: result := pixel^ and $0000ffff;
			3: result := pixel^ and $00ffffff;
			4: result := pixel^;
		else
			raise Exception.Create('Unsuported bit format...');
		end;
		{$ELSE}
		case surface.format.BytesPerPixel of
			1: result := pixel^ and $ff000000;
			2: result := pixel^ and $ffff0000;
			3: result := pixel^ and $ffffff00;
			4: result := pixel^;
		else
			raise Exception.Create('Unsuporte bit format...');
		end;
		{$IFEND}
	end;

	// Sets the non-transparent pixels in a Bitmap. This is then used for
	// collision detection, allowing the original surface to be optimised.
	//
	// @param toSet	 A pointer to the Bitmap being set
	// @param surface The surface with pixel data for this Bitmap
	procedure SetNonTransparentPixels(toSet: Bitmap; surface: PSDL_Surface;
																		transparentColor: Color);
	var
		r, c: Integer;
	begin
		SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);

		for c := 0 to toSet.width - 1 do
		begin
			for r := 0 to toSet.height - 1 do
			begin
				toSet.nonTransparentPixels[c, r] :=
					(GetPixel32(surface, c, r) <> transparentColor);
			end;
		end;
	end;

	procedure SetNonAlphaPixels(toSet: Bitmap; surface: PSDL_Surface);
	var
		r, c: Integer;
		hasAlpha: Boolean;
	begin
		SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);
		hasAlpha := surface.format.BytesPerPixel = 4;

		for c := 0 to toSet.width - 1 do
		begin
			for r := 0 to toSet.height - 1 do
			begin
				toSet.nonTransparentPixels[c, r] := hasAlpha and
					((GetPixel32(surface, c, r) and SDL_Swap32($000000FF)) > 0);
			end;
		end;
	end;

	/// Loads a bitmap from a given path, with the indicated transparent color.
	/// This loads both transparent and non-transparent bitmaps.
	///
	///	@param pathToBitmap:		 the path to the bitmap to be loaded
	/// @param transparent:      Indicates if transparency should be set
	///	@param transparentColor: the color that will be transparent
	///	@returns: A bitmap from the loaded file.
	function	LoadBitmap(pathToBitmap: String; transparent: Boolean;
											 transparentColor: Colour): Bitmap; overload;
	var
		loadedImage: PSDL_Surface;
		correctedTransColor: Colour;
	begin
		loadedImage := IMG_Load(pchar(pathToBitmap));

		if loadedImage <> nil then
		begin
			new(result);
			if not transparent then
				result.surface := SDL_DisplayFormatAlpha(loadedImage)
			else
				result.surface := SDL_DisplayFormat(loadedImage);

			result.width := result.surface.w;
			result.height := result.surface.h;

			if transparent then
			begin
				correctedTransColor := GetColour(result, transparentColor);
				SDL_SetColorKey(result.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, 
                        correctedTransColor);
				SetNonTransparentPixels(result, loadedImage, correctedTransColor);
			end
			else
			begin
				SetNonAlphaPixels(result, loadedImage);
			end;

			SDL_FreeSurface(loadedImage);
		end
		else
		begin
			raise Exception.Create('Error loading image: ' + 
                             pathToBitmap + ': ' + SDL_GetError());
		end;
	end;

	/// Loads a bitmap from file into a Bitmap variable. This can then be drawn to
	///	the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
	///	contain alpha values, which will be drawn correctly by the API. All
	///	bitmaps must be freed using the FreeBitmap once you are finished with
	///	them.
	///
	///	@param pathToBitmap:	 The path to the bitmap file to open.
	///	@returns: A bitmap from the loaded file
	function LoadBitmap(pathToBitmap : String): Bitmap; overload;
	begin
		result := LoadBitmap(pathToBitmap, false, ColorBlack);
	end;

 	/// Loads a bitmap with a transparent color key. The transparent color is then
	///	setup as the color key to ensure the image is drawn correctly. Alpha
	///	values of Images loaded in this way will be ignored. All bitmaps must be
	///	freed using the FreeBitmap once you are finished with them.
	///
	///	@param pathToBitmap:		 the path to the bitmap to be loaded
	///	@param transparentColor: the color that will be transparent
	///	@returns: A bitmap from the loaded file.
	function LoadTransparentBitmap(pathToBitmap : String;
                                 transparentColor : Colour): Bitmap; overload;
	begin
		result := LoadBitmap(pathToBitmap, true, transparentColor);
	end;

	/// Frees a loaded bitmap. Use this when you will no longer be drawing the
	///	bitmap, and when the program exits.
	///
	/// Side Effects:
	///	- the bitmap is freeed and can no longer be drawn
	procedure FreeBitmap(var bitmapToFree : Bitmap);
	begin
		if bitmapToFree <> nil then
		begin
			if bitmapToFree.surface <> nil then
			begin
				SDL_FreeSurface(bitmapToFree.surface);
			end;
			bitmapToFree.surface := nil;

			Dispose(bitmapToFree);
			bitmapToFree := nil;
		end;
	end;

	/// Creates a sprites, and sets its firat bitmap.
	///
	///	@param startBitmap:		The sprites first bitmap (index 0)
	///	@returns:							A new sprite with this bitmap as its first bitmap
	function CreateSprite(startBitmap : Bitmap): Sprite;
	begin
		New(result);
		SetLength(result.bitmaps, 1);

		result.bitmaps[0]						:= startBitmap;
		result.xPos									:= 0;
		result.yPos									:= 0;
		result.currentFrame					:= 0;
		result.usePixelCollision		 := false;
	end;

	/// Frees a sprite, this does not free the sprite's bitmaps, which allows
	///	bitmaps to be shared between sprites. All created sprites need to be
	///	freed.
	///
	///	@param spriteToFree:		 the sprite to free
	///
	/// Side Effects:
	///	- The sprites details are cleaned up.
	procedure FreeSprite(var spriteToFree : Sprite);
	begin
		//for index := 0 to High(spriteToFree.bitmaps) do
		//begin
		//	FreeBitmap(spriteToFree.bitmaps[index]);
		//end;

		SetLength(spriteToFree.bitmaps, 0);
		Dispose(spriteToFree);
		spriteToFree := nil;
	end;

	/// Sprites may contain multiple images. These images can be used for things
	///	line animation, facing, etc. This routine adds a bitmap to a sprite,
	///	returning the index of the added bitmap.
	///
	///	@param spriteToAddTo:		the sprite to add the bitmap to
	///	@param bitmapToAdd:			the bitmap to add to the sprite
	///	@returns :							 the index of the added bitmap
	///
	/// Side Effects:
	///	- The bitmaps is added to the bitmaps within the sprite.
	function AddBitmapToSprite(spriteToAddTo : Sprite;
														 bitmapToAdd : Bitmap): Integer;
	begin
		//Resize the array
		SetLength(spriteToAddTo.bitmaps, Length(spriteToAddTo.bitmaps) + 1);

		//Add the values to the array
		spriteToAddTo.bitmaps[High(spriteToAddTo.bitmaps)] := bitmapToAdd;

		result := High(spriteToAddTo.bitmaps);
	end;

	/// Returns the current width of the sprite.
	///
	///	@param sprite:		 The sprite to get the width of
	///	@returns					 The width of the sprite's current frame
	function CurrentWidth(sprite: Sprite): Integer;
	begin
		result := sprite.bitmaps[sprite.currentFrame].width;
	end;
  
	/// Returns the current height of the sprite.
	///
	///	@param sprite:		 The sprite to get the height of
	///	@returns					 The height of the sprite's current frame
	function CurrentHeight(sprite: Sprite): Integer;
	begin
		result := sprite.bitmaps[sprite.currentFrame].height;
	end;

 	/// Draws one bitmap (bitmapToDraw) onto another bitmap (dest).
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param bitmapToDraw: The bitmap to be drawn onto the destination
	///	@param x,y:					The x,y location to draw the bitmap to
	///
	/// Side Effects:
	///	- Draws the bitmapToDraw at the x,y location in the destination.
	procedure DrawBitmap(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
    overload;
	var
		offset: SDL_Rect;
	begin
		offset := NewSDLRect(x, y, 0, 0);
		SDL_BlitSurface(bitmapToDraw.surface, nil, dest.surface, @offset);
	end;

	/// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest).
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param bitmapToDraw: The bitmap to be drawn onto the destination
	///	@param srcX, srcY:	 The x,y offset to the area to copy in bitmapToDraw
	///	@param srcW, srcH:	 The width and height of the area to copy
	///	@param x,y:					The x,y location to draw the bitmap part to
	///
	/// Side Effects:
	///	- Draws part of the bitmapToDraw at the x,y location in the destination.
	procedure DrawBitmapPart(dest: Bitmap; bitmapToDraw: Bitmap;
													 srcX, srcY, srcW, srcH, x, y : Integer); overload;
	var
		offset, source: SDL_Rect;
	begin
		offset := NewSDLRect(x, y, 0, 0);
		source := NewSDLRect(srcX, srcY, srcW, srcH);

		SDL_BlitSurface(bitmapToDraw.surface, @source, dest.surface, @offset);
	end;

	/// Draws part of a bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw: The bitmap to be drawn onto the screen
	///	@param srcX, srcY:	 The x,y offset to the area to copy in bitmapToDraw
	///	@param srcW, srcH:	 The width and height of the area to copy
	///	@param x,y:					The x,y location to draw the bitmap part to
	///
	/// Side Effects:
	///	- Draws part of the bitmapToDraw at the x,y location on the screen.
	procedure DrawBitmapPart(bitmapToDraw : Bitmap; 
               srcX, srcY, srcW, srcH, x, y : Integer); overload;
	begin
		DrawBitmapPart(scr, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
	end;

	/// Draws one bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw: The bitmap to be drawn onto the screen
	///	@param x,y:					The x,y location to draw the bitmap to
	///
	/// Side Effects:
	///	- Draws the bitmapToDraw at the x,y location on the screen.
	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Integer); overload;
	begin
		DrawBitmap(scr, bitmapToDraw, x, y);
	end;

	/// Draws the sprite to the screen within a given view port.
	///
	///	@param spriteToDraw:		 The sprite to be drawn
	///	@param vwPrtX, vwPrty: The x, y of the current view port (i.e. screen)
	///	@param vwPrtWidth, vwPrtHeight:		The height and width of the view port
	///
	/// Side Effects:
	///	- The sprite is drawn to the screen, if within view port
	procedure DrawSprite(spriteToDraw : Sprite; 
               vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight : Integer); overload;
	begin
		//Don't draw if its offscreen
		if IsSpriteOffscreen(spriteToDraw, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight)
			 and (vwPrtWidth <> 0)
			 and (vwPrtHeight <> 0) then exit;

		DrawBitmap(spriteToDraw.bitmaps[spriteToDraw.currentFrame],
							 Trunc(spriteToDraw.xPos) - vwPrtX,
							 Trunc(spriteToDraw.yPos) - vwPrtY);
	end;

	/// Draws a sprite to the screen, without using a view port.
	///
	///	@param spriteToDraw:		 The sprite to be drawn
	///
	/// Side Effects:
	///	- The sprite is drawn to the screen, if within screen area
	procedure DrawSprite(spriteToDraw : Sprite); overload;
	begin
		DrawSprite(spriteToDraw, 0, 0, 0, 0);
	end;

	/// Determines if a sprite is off the screen. The view port of the screen
	///	is defined in the vwPrt... parameters.
	///
	///	@param theSprite:			The sprite to check the position of
	///	@param vwPrtX, vwPrty: The x, y of the current view port (i.e. screen)
	///	@param vwPrtWidth, vwPrtHeight:		The height and width of the view port
	///	@returns							 True if the sprite is off the screen
	function IsSpriteOffscreen(theSprite : Sprite; 
              vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight : Integer): Boolean;
	var
		bmp: Bitmap;
	begin
		bmp := theSprite.bitmaps[theSprite.currentFrame];
		if theSprite.xPos > vwPrtX + vwPrtWidth then result := true
		else if theSprite.xPos + bmp.width < vwPrtX then result := true
		else if theSprite.yPos > vwPrtY + vwPrtHeight then result := true
		else if theSprite.yPos + theSprite.bitmaps[theSprite.currentFrame].height 
               < vwPrtY then result := true
		else result := false;
	end;

  	/// Determines if a sprite is off the screen.
	///
	///	@param theSprite:			The sprite to check the position of
	///	@returns							 True if the sprite is off the screen
	function IsSpriteOffscreen(theSprite : Sprite): Boolean;
	begin
		result := IsSpriteOffscreen(theSprite, 0, 0, scr.width, scr.height);
	end;

	/// Moves a sprite based on information in a movement vector.
	///
	///	@param spriteToMove:		 The sprite to move
	///	@param movementVector:	 The vector containing the movement details
	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector);
	begin
		spriteToMove.xPos := spriteToMove.xPos + movementVector.x;
		spriteToMove.yPos := spriteToMove.yPos + movementVector.y;
	end;

	/// Moves a sprite to a given x,y location.
	///
	///	@param spriteToMove:		 the sprite being moved
	///	@param x, y:						 the new location of the sprite
	///
	/// Side Effects:
	///	- Moves the sprite, changing its x and y
	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer);
	begin
		spriteToMove.xPos := x;
		spriteToMove.yPos := y;
	end;
	
	/// Creates a bitmap in memory that can be drawn onto. The bitmap is initially
	///	transparent and can be used as the target for various drawing operations.
	///	Once you have drawn the desired image onto the bitmap you can call
	///	OptimiseBitmap to optimise the surface.
	///
	///  @param width, height:  The width and height of the surface
	///  @returns:              A new bitmap
	function	CreateBitmap(width, height: Integer): Bitmap;
	begin
		New(result);

		with baseSurface.format^ do
		begin
			result.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32,
											 RMask, GMask, BMask, AMask);
		end;
		result.width := width;
		result.height := height;
		SDL_SetAlpha(result.surface, SDL_SRCALPHA, SDL_ALPHA_OPAQUE);
		SDL_FillRect(result.surface, nil, ColorTransparent);
	end;

	/// Created bitmaps can be optimised for faster drawing to the screen. This
	///	optimisation should be called only once after all drawing to the bitmap
	///	is complete. Optimisation should not be used if the bitmap is to be
	///	drawn to in the future. All loaded bitmaps are optimised during loading.
	///
	///	@param surface:	The bitmap to be optimised
	///
	/// Side Effects:
	///	- Bitmap is optimised, and should not be used to draw onto in the future
	procedure OptimiseBitmap(surface: Bitmap);
	var
		oldSurface: PSDL_Surface;
	begin
		oldSurface := surface.surface;
		SetNonAlphaPixels(surface, oldSurface);
		surface.surface := SDL_DisplayFormatAlpha(oldSurface);
		SDL_FreeSurface(oldSurface);
	end;
	
	procedure PutPixel(surface: PSDL_Surface; x, y: Integer; color: Color);
	var
	  bufP: PUInt32;
	  pixels: PUint32;
	  addr: UInt32;
	begin
	  if (x < 0) or (x >= surface.w) or (y < 0) or (y >= surface.h) then exit;
	
	  pixels := surface.pixels;
	  //addr := Integer(pixels) + (( y * surface.w ) + x) * surface.format.BytesPerPixel;
	  addr := Integer(pixels) + (x * surface.format.BytesPerPixel) + (y * surface.pitch) ;
	  bufp := PUint32(addr);
	  bufp^ := color;
	  //PixelColor(surface, x, y, ToSDLGFXColor(color));
	end;
	
	/// Draws a pixel onto the screen.
	///
	///	@param theColor:		 The color to draw the pixel
	///	@param x,y:					The x,y location to draw the pixel at
	///
	/// Side Effects:
	///	- Sets one pixel on the screen
	procedure DrawPixel(theColour: Colour; x, y: Integer); overload;
	begin
		DrawPixel(scr, theColour, x, y);
	end;

	/// Draws a rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param filled:			 True to draw a filled rectangle, false for outline
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure DrawRectangle(theColour : Colour; filled : Boolean;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, filled, xPos, yPos, width, height);
	end;

	/// Draws the outline of a rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure DrawRectangle(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a filled rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure FillRectangle(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		FillRectangle(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param xPosStart,yPosStart: The x,y location to start the line at
	///	@param xPosEnd, yPosEnd:		The x,y location to end the line at
	///
	/// Side Effects:
	///	- Draws a line in the screen
	procedure DrawLine(theColour: Colour; 
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Integer); overload;
	begin
		DrawLine(scr, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
	end;

	/// Draws a horizontal line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param y:						The y location of the line
	///	@param x1, x2:			 The starting and ending x value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Integer); overload;
	begin
		DrawHorizontalLine(scr, theColor, y, x1, x2);
	end;

	/// Draws a vertical line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param x:						The x location of the line
	///	@param y1, y2:			 The starting and ending y value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Integer); overload;
	begin
		DrawVerticalLine(scr, theColor, x, y1, y2);
	end;

	/// Draws a circle centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param filled:			 True to draw a filled circle, false for outline
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure DrawCircle(theColour: Colour; filled: Boolean;
                       xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, filled, xc, yc, radius);
	end;

	/// Draws a circle outline centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure DrawCircle(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, xc, yc, radius);
	end;

	/// Draws a filled circle centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure FillCircle(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		FillCircle(scr, theColour, xc, yc, radius);
	end;

	/// Draws a ellipse within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param filled:			 True to draw a filled ellipse, false for outline
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse on the screen
	procedure DrawEllipse(theColour: Colour; filled: Boolean;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, filled, xPos, yPos, width, height);
	end;

	/// Draws a ellipse outline within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse on the screen
	procedure DrawEllipse(theColour: Colour;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a filled ellipse within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the screen
	 procedure FillEllipse(theColour: Colour; 
                         xPos, yPos, width, height: Integer); overload;
	begin
		FillEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param filled:			 True to draw a filled rectangle, false for outline
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure DrawRectangle(dest: Bitmap; theColour : Colour; filled : Boolean;
                          xPos, yPos, width, height : Integer); overload;
	begin
		if filled then
		begin
			FillRectangle(dest, theColour, xPos, yPos, width, height);
		end
		else
		begin
			DrawRectangle(dest, theColour, xPos, yPos, width, height);
		end;
	end;

	/// Draws the outline of a rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	 procedure DrawRectangle(dest: Bitmap; theColour : Colour;
                           xPos, yPos, width, height : Integer); overload;
	begin
		DrawHorizontalLine(dest, theColour, yPos, xPos, xPos + width);
		DrawHorizontalLine(dest, theColour, yPos + height, xPos, xPos + width);
		DrawVerticalLine(dest, theColour, xPos, yPos, yPos + height);
		DrawVerticalLine(dest, theColour, xPos + width, yPos, yPos + height);
	end;

	/// Draws a filled rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure FillRectangle(dest: Bitmap; theColour : Colour; 
                          xPos, yPos, width, height : Integer);
	var
		rect: SDL_Rect;
	begin
		if width < 0 then
		begin
			rect.x := xPos + width; //move back by width
			width := -width;
		end
		else rect.x := xPos;

		if height < 0 then
		begin
			rect.y := yPos + height; //move up by height
			height := -height;
		end
		else rect.y := yPos;

		rect.w := width;
		rect.h := height;

		SDL_FillRect(dest.surface, @rect, theColour);
	end;

	/// Draws a ellipse within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param filled:			 True to draw a filled ellipse, false for outline
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	procedure DrawEllipse(dest: Bitmap; theColour: Colour; filled: Boolean;
                        xPos, yPos, width, height: Integer); overload;
	begin
		if filled then
		begin
			FillEllipse(dest, theColour, xPos, yPos, width, height);
		end
		else
		begin
			DrawEllipse(dest, theColour, xPos, yPos, width, height);
		end;
	end;

	/// Draws a ellipse outline within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	procedure DrawEllipse(dest: Bitmap; theColour: Colour; 
                 xPos, yPos, width, height: Integer); overload;
	var
		xRadius, yRadius: Integer;
		x, y: Integer;
		xChange, yChange: Integer;
		ellipseError: Integer;
		twoASquare, twoBSquare: Integer;
		stoppingX, stoppingY: Integer;
	begin
		xRadius := width div 2;
		yRadius := height div 2;

		twoASquare := 2 * Xradius * Xradius;
		twoBSquare := 2 * Yradius * Yradius;
		
		// 1st set of points
		x := Xradius - 1;  // radius zero == draw nothing
		y := 0;
		
		xChange := yRadius * yRadius * (1 - 2 * xRadius);
		yChange := xRadius * xRadius;
		
		ellipseError := 0;
		
		stoppingX := twoBSquare * xRadius;
		stoppingY := 0;
		
		//Lock dest.surface
		if SDL_MUSTLOCK(dest.surface) then
		begin
			if SDL_Lockdest.surface(dest.surface) < 0 then exit;
		end;
		
		// Plot four ellipse points by iteration
		while stoppingX > stoppingY do
		begin
			PutPixel(dest.surface, xPos + xRadius + x, yPos + yRadius + y, theColour);
			PutPixel(dest.surface, xPos + xRadius - x, yPos + yRadius + y, theColour);
			PutPixel(dest.surface, xPos + xRadius + x, yPos + yRadius - y, theColour);
			PutPixel(dest.surface, xPos + xRadius - x, yPos + yRadius - y, theColour);
			
			y := y + 1;
			stoppingY := stoppingY + twoASquare;
			ellipseError := ellipseError + Ychange;
			yChange := yChange + twoASquare;
			
			if (2 * ellipseError + xChange) > 0 then
			begin
				x := x - 1;
				stoppingX := stoppingX - twoBSquare;
				ellipseError := ellipseError + xChange;
				xChange := xChange + twoBSquare;
			end;
		end;
		
		// 2nd set of points
		x := 0;
		y := yRadius - 1;  //radius zero == draw nothing
		xChange := yRadius * yRadius;
		yChange := xRadius * xRadius * (1 - 2 * yRadius);
		ellipseError := 0;
		stoppingX := 0;
		stoppingY := twoASquare * yRadius;
		
		//Plot four ellipse points by iteration
		while stoppingX < stoppingY do
		begin
			PutPixel(dest.surface, xPos + xRadius + x, yPos + yRadius + y, theColour);
			PutPixel(dest.surface, xPos + xRadius - x, yPos + yRadius + y, theColour);
			PutPixel(dest.surface, xPos + xRadius + x, yPos + yRadius - y, theColour);
			PutPixel(dest.surface, xPos + xRadius - x, yPos + yRadius - y, theColour);
			
			x := x + 1;
			stoppingX := stoppingX + twoBSquare;
			ellipseError := ellipseError + xChange;
			xChange := xChange + twoBSquare;
			
			if (2 * ellipseError + yChange) > 0 then
			begin
				y := y - 1;
				stoppingY := stoppingY - TwoASquare;
				ellipseError := ellipseError + yChange;
				yChange := yChange + twoASquare;
			end;
		end;
		
		// Unlock dest.surface
		if SDL_MUSTLOCK(dest.surface) then  SDL_Unlockdest.surface(dest.surface);
	end;

	/// Draws a filled ellipse within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	procedure FillEllipse(dest: Bitmap; theColour: Colour;
                         xPos, yPos, width, height: Integer);
	var
		xRadius, yRadius: Integer;
		x, y: Integer;
		xChange, yChange: Integer;
		ellipseError: Integer;
		twoASquare, twoBSquare: Integer;
		stoppingX, stoppingY: Integer;
	begin
		xRadius := width div 2;
		yRadius := height div 2;

		twoASquare := 2 * Xradius * Xradius;
		twoBSquare := 2 * Yradius * Yradius;
		
		// 1st set of points
		x := Xradius - 1;  // radius zero == draw nothing
		y := 0;
		
		xChange := yRadius * yRadius * (1 - 2 * xRadius);
		yChange := xRadius * xRadius;
		
		ellipseError := 0;
		
		stoppingX := twoBSquare * xRadius;
		stoppingY := 0;
		
		//Lock dest.surface
		if SDL_MUSTLOCK(dest.surface) then
		begin
			if SDL_Lockdest.surface(dest.surface) < 0 then exit;
		end;
		
		// Plot four ellipse points by iteration
		while stoppingX > stoppingY do
		begin
			DrawHorizontalLine(dest.surface, yPos + yRadius + y, xPos + xRadius - x, xPos + xRadius + x, theColour);
			DrawHorizontalLine(dest.surface, yPos + yRadius - y, xPos + xRadius - x, xPos + xRadius + x, theColour);
			
			y := y + 1;
			stoppingY := stoppingY + twoASquare;
			ellipseError := ellipseError + Ychange;
			yChange := yChange + twoASquare;
			
			if (2 * ellipseError + xChange) > 0 then
			begin
				x := x - 1;
				stoppingX := stoppingX - twoBSquare;
				ellipseError := ellipseError + xChange;
				xChange := xChange + twoBSquare;
			end;
		end;
		
		// 2nd set of points
		x := 0;
		y := yRadius - 1;  //radius zero == draw nothing
		xChange := yRadius * yRadius;
		yChange := xRadius * xRadius * (1 - 2 * yRadius);
		ellipseError := 0;
		stoppingX := 0;
		stoppingY := twoASquare * yRadius;
		
		//Plot four ellipse points by iteration
		while stoppingX < stoppingY do
		begin
			DrawHorizontalLine(dest.surface, yPos + yRadius + y, xPos + xRadius - x, xPos + xRadius + x, theColour);
			DrawHorizontalLine(dest.surface, yPos + yRadius - y, xPos + xRadius - x, xPos + xRadius + x, theColour);
			
			x := x + 1;
			stoppingX := stoppingX + twoBSquare;
			ellipseError := ellipseError + xChange;
			xChange := xChange + twoBSquare;
			
			if (2 * ellipseError + yChange) > 0 then
			begin
				y := y - 1;
				stoppingY := stoppingY - TwoASquare;
				ellipseError := ellipseError + yChange;
				yChange := yChange + twoASquare;
			end;
		end;
		
		// Unlock dest.surface
		if SDL_MUSTLOCK(dest.surface) then  SDL_Unlockdest.surface(dest.surface);
	end;

	/// Draws a vertical line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param x:						The x location of the line
	///	@param y1, y2:			 The starting and ending y value of the line
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawVerticalLine(dest: Bitmap; theColor: Color; x, y1, y2: Integer);
	var
		w, h, y, addr: Integer;
		bufP: PUInt32;
		pixels: PUint32;
	begin
		w := dest.surface.w;
		h := dest.surface.h;
		
		if y2 < y1 then  //swap y1 and y2
		begin
			y1 := y1 + y2;
			y2 := y1 - y2;
			y1 := y1 - y2;
		end;
		
		if (x < 0) or (x > w - 1) or (y2 < 0) or (y1 > h - 1) then
		begin
			result := false;
			exit;
		end;
		
		if y1 < 0 then y1 := 0;
		if y2 >= h then y2 := h - 1;
		
		//vlinetheColour(dest.surface, x, y1, y2, ToSDLGFXtheColour(theColour));
		
		pixels := dest.surface.pixels;
		//addr := Integer(pixels) + (( y1 * dest.surface.w ) + x) * dest.surface.format.BytesPerPixel;
		addr := Integer(pixels) + (x * dest.surface.format.BytesPerPixel) + (y1 * dest.surface.Pitch);
		bufp := PUint32(addr);
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Lockdest.surface(dest.surface);
		
		for y := y1 to y2 - 1 do
		begin
			bufp := PUInt32(Integer(bufp) + (dest.surface.pitch));
			bufp^ := theColour;
		end;
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Unlockdest.surface(dest.surface);
		
		result := true;
	end;

	/// Draws a horizontal line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param y:						The y location of the line
	///	@param x1, x2:			 The starting and ending x value of the line
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawHorizontalLine(dest: Bitmap; theColor: Color; y,x1,x2: Integer);
	var
		w, h, x, addr: Integer;
		bufP: PUInt32;
		pixels: PUint32;
	begin
		w := dest.surface.w;
		h := dest.surface.h;
		
		if x2 < x1 then //swap x1 and x2, x1 must be the leftmost endpoint
		begin
			x1 := x1 + x2;
			x2 := x1 - x2;
			x1 := x1 - x2;
		end;
		
		if (x2 < 0) or (x1 > w - 1) or (y < 0) or (y > h - 1) then
		begin
			result := false;
			exit; //no single point of the line is on screen
		end;
		
		if x1 < 0 then x1 := 0;
		if x2 >= w then x2 := w - 1;
		
		//hlinetheColour(dest.surface, x1, x2, y, ToSDLGFXtheColour(theColour));
		//result := true;
		
		pixels := dest.surface.pixels;
		//addr := Integer(pixels) + (( y * dest.surface.w ) + x1 - 1) * dest.surface.format.BytesPerPixel;
		addr := Integer(pixels) + ((x1 - 1) * dest.surface.format.BytesPerPixel) + (y * dest.surface.pitch);
		bufp := PUint32(addr);
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Lockdest.surface(dest.surface);
		
		for x := x1 to x2 do
		begin
			Inc(bufp);
			bufp^ := theColour;
		end;
		result := true;
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Unlockdest.surface(dest.surface);
	end;

	/// Draws a line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param xPosStart,yPosStart: The x,y location to start the line at
	///	@param xPosEnd, yPosEnd:		The x,y location to end the line at
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawLine(dest: Bitmap; theColour: Colour;
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Integer);
	var
		x, y: Integer;
		deltaX, deltaY: Integer;
		xinc1, xinc2, yinc1, yinc2, den, num, numadd, numpixels, curpixel: Integer;
	begin
		if xPosStart = xPosEnd then
			DrawVerticalLine(dest, theColour, xPosStart, yPosStart, yPosEnd)
		else if yPosStart = yPosEnd then
			DrawHorizontalLine(dest, theColour, yPosStart, xPosStart, xPosEnd)
		else
		begin
		  	deltax := abs(xPosEnd - xPosStart);        // The difference between the x's
			deltay := abs(yPosEnd - yPosStart);        // The difference between the y's
			
			x := xPosStart;                        // Start x off at the first pixel
			y := yPosStart;                        // Start y off at the first pixel
			
			if xPosEnd >= xPosStart then                // The x-values are increasing
			begin
				xinc1 := 1;
				xinc2 := 1;
			end
			else                            // The x-values are decreasing
			begin
				xinc1 := -1;
				xinc2 := -1;
			end;
			
			if yPosEnd >= yPosStart then                // The y-values are increasing
			begin
				yinc1 := 1;
				yinc2 := 1;
			end
			else                            // The y-values are decreasing
			begin
				yinc1 := -1;
				yinc2 := -1;
			end;
			
			if deltax >= deltay then         // There is at least one x-value for every y-value
			begin
				xinc1 := 0;                  // Don't change the x when numerator >= denominator
				yinc2 := 0;                  // Don't change the y for every iteration
				
				den := deltax;
				
				num := deltax div 2;
				
				numadd := deltay;
				
				numpixels := deltax;         // There are more x-values than y-values
			end
			else                          // There is at least one y-value for every x-value
			begin
				xinc2 := 0;                  // Don't change the x for every iteration
				yinc1 := 0;                  // Don't change the y when numerator >= denominator
				den := deltay;
				num := deltay div 2;
				
				numadd := deltax;
				
				numpixels := deltay;         // There are more y-values than x-values
			end;
			
			for curpixel := 0 to numpixels do
			begin
				PutPixel(dest.surface, x, y, theColour);  // Draw the current pixel
				
				num := num + numadd;              // Increase the numerator by the top of the fraction
				
				if num >= den then           // Check if numerator >= denominator
				begin
					num := num - den;             // Calculate the new numerator value
					x := x + xinc1;             // Change the x as appropriate
					y := y + yinc1;             // Change the y as appropriate
				end;
				
				x := x + xinc2;                 // Change the x as appropriate
				y := y + yinc2;                 // Change the y as appropriate
			end;
			
			result := true;
		end;
	end;

 	/// Draws a pixel onto the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the pixel
	///	@param x,y:					The x,y location to draw the pixel at
	///
	/// Side Effects:
	///	- Sets one pixel on the destination bitmap
	procedure DrawPixel(dest: Bitmap; theColour: Colour; x, y: Integer); overload;
	begin
		if (x < 0) or (x >= dest.surface.w) or (y < 0) or (y >= dest.surface.h) then exit;
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Lockdest.surface(dest.surface);
		
		PutPixel(dest.surface, x, y, theColor);
		
		if SDL_MUSTLOCK(dest.surface) then SDL_Unlockdest.surface(dest.surface);
	end;
	
	/// Draws a circle centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param filled:			 True to draw a filled circle, false for outline
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure DrawCircle(dest: Bitmap; theColour: Colour; filled: Boolean;
                       xc, yc, radius: Integer); overload;
	begin
		if filled then
			DrawCircle(dest, theColour, xc, yc, radius)
		else
			FillCircle(dest, theColour, xc, yc, radius);
	end;

	/// Draws a circle outline centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure DrawCircle(dest: Bitmap; theColour: Colour; xc, yc, radius: Integer);
   overload;
	var
		x, y, p: Integer;
		a, b, c, d, e, f, g, h: Integer;
	begin
	  	x := 0;
		y := radius;
		p := 3 - (radius shl 1);
		
		while x <= y do
		begin
			a := xc + x; //8 pixels can be calculated at once thanks to the symmetry
			b := yc + y;
			c := xc - x;
			d := yc - y;
			e := xc + y;
			f := yc + x;
			g := xc - y;
			h := yc - x;
			
			PutPixel(dest.surface, a, b, theColour);
			PutPixel(dest.surface, c, d, theColour);
			PutPixel(dest.surface, e, f, theColour);
			PutPixel(dest.surface, g, f, theColour);
			
			if x > 0 then //avoid drawing pixels at same position as the other ones
			begin
				PutPixel(dest.surface, a, d, theColour);
				PutPixel(dest.surface, c, b, theColour);
				PutPixel(dest.surface, e, h, theColour);
				PutPixel(dest.surface, g, h, theColour);
			end;
			
			if p < 0 then
			begin
				p := p + (x shl 2) + 6;
				x := x + 1;
			end
			else
			begin
				p := p + ((x - y) shl 2) + 10;
				x := x + 1;
				y := y - 1;
			end;
		end;
		
		result := true;
	end;

	/// Draws a filled circle centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure FillCircle(dest: Bitmap; theColour: Colour;
                       xc, yc, radius: Integer);
	var
		x, y, p: Integer;
		a, b, c, d, e, f, g, h: Integer;
		pb, pd: Integer; //previous values: to avoid drawing horizontal lines multiple times
	begin
	 	x := 0;
		y := radius;
		p := 3 - (radius shl 1);
		
		pb := -1;
		pd := -1;
		
		while x <= y do
		begin
			// write data
			a := xc + x;
			b := yc + y;
			c := xc - x;
			d := yc - y;
			e := xc + y;
			f := yc + x;
			g := xc - y;
			h := yc - x;
			
			if b <> pb then DrawHorizontalLine(dest.surface, b, a, c, theColour);
			if d <> pd then DrawHorizontalLine(dest.surface, d, a, c, theColour);
			if f <> b  then DrawHorizontalLine(dest.surface, f, e, g, theColour);
			if (h <> d) and (h <> f) then DrawHorizontalLine(dest.surface, h, e, g, theColour);
			
			pb := b;
			pd := d;
			
			if p < 0 then
			begin
				p := p + (x shl 2) + 6;
				x := x + 1;
			end
			else
			begin
				p := p + ((x - y) shl 2) + 10;
				x := x + 1;
				y := y - 1;
			end;
		end;
	end;
  
  	/// Returns the average framerate for the last 10 frames as an integer.
	///
	///	@returns		 The current average framerate
	function GetFramerate(): Integer;
	begin
		if renderFPSInfo.average = 0 then
			result := 9999
		else
			result := Round(1000 / renderFPSInfo.average);
	end;
end.