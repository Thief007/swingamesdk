unit SGSDK_Graphics;

interface
	uses	SDL, SGSDK_Core, Classes, SysUtils, SDL_image,
			SDL_Mixer, SDL_TTF, SDLEventProcessing, SGSDK_Camera;
		
		
	//*****
	//
	// Resource loading and freeing routines
	//
	//*****
	//
	// These routines are used to load resources, and to free them.
	//
	
	function NewSDLRect(x, y, w, h: Integer): SDL_Rect;
	
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
							width, height: Integer); overload;

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
	// These routines are used to to the View Area on the screen.
	//

	procedure ClearScreen(); overload;
	procedure ClearScreen(toColour : Colour); overload;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Single); overload;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH: Integer;
							x, y : Single); overload;
	
	procedure DrawPixel(theColour: Colour; x, y: Single); overload;

	procedure DrawRectangle(theColour : Colour; filled : Boolean;
							xPos, yPos: Single;
							width, height : Integer); overload;

	procedure DrawRectangle(theColour : Colour; 
							xPos, yPos : Single;
							width, height : Integer); overload;

	procedure FillRectangle(theColour : Colour; 
							xPos, yPos: Single;
							width, height : Integer); overload;

	procedure DrawLine(theColour: Colour; xPosStart, yPosStart,
					 		xPosEnd, yPosEnd: Single); overload;

	procedure DrawHorizontalLine(theColor: Color; 
							y, x1, x2: Single); overload;

	procedure DrawVerticalLine(theColor: Color; 
							x, y1, y2: Single); overload;

	procedure DrawCircle(theColour: Colour; filled: Boolean;
						 xc, yc: Single; radius: Integer); overload;

	procedure DrawCircle(theColour: Colour; xc, yc: Single; radius: Integer); overload;

	procedure FillCircle(theColour: Colour; xc, yc: Single; radius: Integer); overload;

	procedure DrawEllipse(theColour: Colour; filled: Boolean;
						xPos, yPos: Single; width, height: Integer); overload;

	procedure DrawEllipse(theColour: Colour;
						xPos, yPos: Single; width, height: Integer); overload;

	procedure FillEllipse(theColour: Colour;
						xPos, yPos: Single; width, height: Integer); overload;

	//*****
	//
	// Sprite routines
	//
	//*****
	//
	// These routines are used to work with Sprites within your game.
	//

	function CreateSprite(image : Bitmap; isMulti : Boolean; framesPerCell : Array of Integer; 
		endingAction : SpriteEndingAction; width, height : Integer): Sprite; overload;
	
	function CreateSprite(image : Bitmap; isMulti : Boolean; framesPerCell : Array of Integer; 
		width, height : Integer): Sprite; overload;
	
	function CreateSprite(image: Bitmap; framesPerCell, frames, width, height: Integer): Sprite; overload;
	
	function CreateSprite(image : Bitmap): Sprite; overload;
	
	function CreateSprite(bitmaps : Array of Bitmap; framesPerCell : Array of Integer; endingAction : SpriteEndingAction): Sprite; overload;
	
	function CreateSprite(bitmaps : Array of Bitmap; framesPerCell : Array of Integer): Sprite; overload;
	
	function CreateSprite(bitmaps: Array of Bitmap; framesPerCell, frames: Integer): Sprite; overload;

	procedure FreeSprite(var spriteToFree : Sprite);

	function AddBitmapToSprite(spriteToAddTo : Sprite; bitmapToAdd : Bitmap): Integer;

	function CurrentHeight(sprite: Sprite): Integer; inline;

	function CurrentWidth(sprite: Sprite): Integer; inline;
	
	procedure ReplayAnimation(theSprite : Sprite);
	
	procedure UpdateSprite(spriteToDraw : Sprite);
	procedure UpdateSpriteAnimation(spriteToDraw : Sprite);

	procedure DrawSprite(spriteToDraw : Sprite; xOffset, yOffset: Integer); overload;
	
	procedure DrawSprite(spriteToDraw : Sprite); overload;
		
	procedure MoveSprite(spriteToMove: Sprite); overload;
	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector); overload;

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer);
		
	function IsSpriteOffscreen(theSprite : Sprite): Boolean; overload;

	//*****
	//
	// Draws elements directly onto the screen, ignoring the visible window
	// setting.
	//
	//*****
	//
	// These routines are used to move the visual window.
	//
	procedure DrawBitmapPartOnScreen(bitmapToDraw : Bitmap; srcX, srcY, srcW, srcH, x, y : Integer);
	procedure DrawBitmapOnScreen(bitmapToDraw : Bitmap; x, y : Integer);

	procedure DrawPixelOnScreen(theColour: Colour; x, y: Integer); overload;

	procedure DrawRectangleOnScreen(theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); overload;

	procedure DrawRectangleOnScreen(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure FillRectangleOnScreen(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure DrawLineOnScreen(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); overload;

	procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: Integer); overload;

	procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: Integer); overload;

	procedure DrawCircleOnScreen(theColour: Colour; filled: Boolean;
						 xc, yc, radius: Integer); overload;

	procedure DrawCircleOnScreen(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure FillCircleOnScreen(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure DrawEllipseOnScreen(theColour: Colour; filled: Boolean;
						xPos, yPos, width, height: Integer); overload;

	procedure DrawEllipseOnScreen(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;

	procedure FillEllipseOnScreen(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;


	
implementation

	/// Clears the surface of the bitmap to the passed in color.
	///
	///	@param dest:		 The bitmap to clear
	///	@param toColour: The colour to clear the bitmap to
	///
	/// Side Effects:
	///	- dest's surface is set to the toColor
	procedure ClearSurface(dest: Bitmap; toColour: Colour); overload;
	begin
		try
			SDL_FillRect(dest.surface, @dest.surface.clip_rect, toColour);
		except
			RaiseSGSDKException('Error occured while trying to clear the bitmap you have specified');
		end;
	end;

 	/// Clears the surface of the bitmap to Black.
	///
	///	@param dest:		 The bitmap to clear
	///
	/// Side Effects:
	///	- dest's surface is set to black
	procedure ClearSurface(dest: Bitmap); overload;
	begin
		ClearSurface(dest, ColorBlack);
	end;

	/// Clears the surface of the screen to the passed in color.
	///
	///	@param toColour: The colour to clear the bitmap to
	///
	/// Side Effects:
	///	- Screen's surface is set to the toColor
	procedure ClearScreen(toColour : Colour); overload;
	begin
		ClearSurface(scr, toColour);
	end;

	/// Clears the screen to Black.
	///
	/// Side Effects:
	///	- screen's surface is set to black
	procedure ClearScreen(); overload;
	begin
		ClearScreen(ColorBlack);
	end;


	function NewSDLRect(x, y, w, h: Integer): SDL_Rect;
	begin
		if (w < 0) or (h < 0) then
			RaiseSGSDKException('Width and height of a rectangle must be larger than 0');
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
		try
			result.w := Word(w);
			result.h := Word(h);
		except
			RaiseSGSDKException('Error occured while trying to create a SDL rectangle');
		end;
	end;
	
	function GetPixel32(surface: PSDL_Surface; x, y: Integer): Colour;
	var
		pixels, pixel: PUint32;
		offset, pixelAddress: Uint32;
	begin
    result := 0;
    
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
			RaiseSGSDKException('Unsuported bit format...');
		end;
		{$ELSE}
		case surface.format.BytesPerPixel of
			1: result := pixel^ and $ff000000;
			2: result := pixel^ and $ffff0000;
			3: result := pixel^ and $ffffff00;
			4: result := pixel^;
		else
			RaiseSGSDKException('Unsuporte bit format...')
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
		try
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
		except
			RaiseSGSDKException('Failed to set non alpha pixels');
		end;
	end;

	/// Loads a bitmap from a given path, with the indicated transparent color.
	/// This loads both transparent and non-transparent bitmaps.
	///
	///	@param pathToBitmap:		 the path to the bitmap to be loaded
	/// @param transparent:      Indicates if transparency should be set
	///	@param transparentColor: the color that will be transparent
	///	@returns: A bitmap from the loaded file.
	function LoadBitmap(pathToBitmap: String; transparent: Boolean;
											 transparentColor: Colour): Bitmap; overload;
	var
		loadedImage: PSDL_Surface;
		correctedTransColor: Colour;
	begin
    result := nil;
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
				try
					SDL_SetColorKey(result.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, 
	                        correctedTransColor);
				except
					RaiseSGSDKException('Failed to set the colour key');
				end;
				SetNonTransparentPixels(result, loadedImage, correctedTransColor);
			end
			else
			begin
				SetNonAlphaPixels(result, loadedImage);
			end;
			try
				SDL_FreeSurface(loadedImage);
			except
				RaiseSGSDKException('Failed to free the specified SDL surface');
			end;
		end
		else
		begin
			RaiseSGSDKException('Error loading image: ' + 
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
				//WriteLn('Freeing SDL Surface');
				try
					SDL_FreeSurface(bitmapToFree.surface);
				except
					RaiseSGSDKException('Failed to free the specified SDL surface');
				end;
			end;
			//WriteLn('Nilling bitmap surface');
			bitmapToFree.surface := nil;

			//WriteLn('Diposing Bitmap');
			Dispose(bitmapToFree);
			bitmapToFree := nil;
		end;
	end;
	
	/// Creates a sprites, and sets its first bitmap.
	///
	///	@param startBitmap:		The sprites first bitmap (index 0)
	/// @param isMulti:			True if the bitmap specified is a multi bitmap
	/// @param framesPerCell:	Array of Integer that defines the frames per cell
	/// @param endingAction:	This sprite's ending action (Loop, ReverseLoop or Stop)
	/// @param width, height:	Width and height of this sprite
	///	@returns:				A new sprite with this bitmap as its first bitmap
	function CreateSprite(image : Bitmap; isMulti : Boolean; framesPerCell : Array of Integer; 
		endingAction : SpriteEndingAction; width, height : Integer): Sprite; overload;
	var
		i : Integer;
	begin
		if image = nil then begin
			RaiseSGSDKException('No image specified to create a sprite');
		end;
		
		if isMulti and (Length(framesPerCell) = 0) then begin
			RaiseSGSDKException('No frames per cell defined');
		end;
		
		if (width < 1) or (height < 1) then begin
			RaiseSGSDKException('Sprite Width and Height must be greater then 0');
		end;
		
		New(result);
		SetLength(result.bitmaps, 1);
		
		if isMulti then
		begin
			result.spriteKind := AnimMultiSprite;
			
			result.cols := image.width div width;
			result.row := image.height div height;
			
			SetLength(result.framesPerCell, Length(framesPerCell));
			for i := 0 to High(framesPerCell) do
			begin
				result.framesPerCell[i] := framesPerCell[i];
			end;
		end
		else
		begin
			result.spriteKind := StaticSprite;
		end;

		result.xPos					:= 0;
		result.yPos					:= 0;
		result.currentFrame			:= 0;
		result.usePixelCollision	:= true;
		result.hasEnded				:= false;
		result.bitmaps[0]			:= image;
		result.frameCount			:= 0;
		result.endingAction			:= endingAction;
		result.width				:= width;
		result.height				:= height;
		result.reverse				:= false;
	end;
	
	/// Creates a sprites, and sets its first bitmap.
	///
	///	@param image:			The sprites first bitmap (index 0)
	/// @param isMulti:			True if the bitmap specified is a multi bitmap
	/// @param framesPerCell:	Array of Integer that defines the frames per cell
	/// @param width, height:	Width and height of this sprite
	///	@returns:				A new sprite
	function CreateSprite(image : Bitmap; isMulti : Boolean; framesPerCell : Array of Integer; 
		width, height : Integer): Sprite; overload;
	begin
		result := CreateSprite(image, isMulti, framesPerCell, Loop, width, height);
	end;
	
	/// Creates a sprites, and sets its first bitmap.
	///
	///	@param image:		The sprites first bitmap (index 0)
	/// @param framesPerCell:	Number of frames per cell
	/// @param frames:			Number of frames this sprite contains
	/// @param width, height:	Width and height of this sprite
	///	@returns:				A new sprite
	function CreateSprite(image: Bitmap; framesPerCell, frames, width, height: Integer): Sprite; overload;
	var
		tempIntegers: Array of Integer;
		i: Integer;
	begin
		SetLength(tempIntegers, frames);
		for i := 0 to High(tempIntegers) do
		begin
			tempIntegers[i] := framesPerCell;
		end;
		result := CreateSprite(image, true, tempIntegers, width, height);
	end;

	/// Creates a sprites, and sets its first bitmap.
	///
	///	@param image:			The sprites first bitmap (index 0)
	///	@returns:				A new sprite with this bitmap as its first bitmap
	function CreateSprite(image : Bitmap): Sprite; overload;
	var
		empty : Array of Integer;
	begin
		SetLength(empty, 0);
		result := CreateSprite(image, false, empty, image.width, image.height);
	end;
	
	/// Creates a sprites ans set bitmaps.
	///
	///	@param bitmaps:			The array of bitmaps
	/// @param framesPerCell:	Array of Integer that defines the frames per cell
	/// @param endingAction:	Ending action of this sprite when it finishes animating
	///	@returns:				A new sprite with this bitmap as its first bitmap
	function CreateSprite(bitmaps : Array of Bitmap; framesPerCell : Array of Integer; endingAction : SpriteEndingAction): Sprite; overload;
	var
		i : Integer;
	begin
		if Length(bitmaps) = 0 then begin
			RaiseSGSDKException('No images specified to create a sprite');
		end;
		
		if Length(framesPerCell) = 0 then begin
			RaiseSGSDKException('No frames per cell defined');
		end;
		
		New(result);
		result.xPos					:= 0;
		result.yPos					:= 0;
		result.currentFrame			:= 0;
		result.usePixelCollision	:= true;
		result.hasEnded				:= false;
		SetLength(result.bitmaps, Length(bitmaps));
		for i := 0 to High(bitmaps) do
		begin
			result.bitmaps[i] := bitmaps[i];
		end;
		result.spriteKind			:= AnimArraySprite;
		SetLength(result.framesPerCell, Length(framesPerCell));
		for i := 0 to High(framesPerCell) do
		begin
			result.framesPerCell[i] := framesPerCell[i];
		end;
		result.endingAction			:= endingAction;
		result.width				:= bitmaps[0].width;
		result.height				:= bitmaps[0].height;
		result.reverse				:= false;
	end;
	
	/// Creates a sprites ans set bitmaps.
	///
	///	@param bitmaps:			The array of bitmaps
	/// @param framesPerCell:	Array of Integer that defines the frames per cell
	///	@returns:				A new sprite with this bitmap as its first bitmap
	function CreateSprite(bitmaps : Array of Bitmap; framesPerCell : Array of Integer): Sprite; overload;
	begin
		result := CreateSprite(bitmaps, framesPerCell, Loop);
	end;
	
	/// Creates a sprites, and sets its first bitmap.
	///
	///	@param bitmaps:			The array of bitmaps
	/// @param framesPerCell:	Number of frames per cell
	/// @param frames:			Number of frames this sprite contains
	///	@returns:				A new sprite
	function CreateSprite(bitmaps: Array of Bitmap; framesPerCell, frames: Integer): Sprite; overload;
	var
		tempIntegers: Array of Integer;
		i: Integer;
	begin
		if framesPerCell <= 0 then
			RaiseSGSDKException('Frames per cell must be larger than 0');
		SetLength(tempIntegers, frames);
		for i := 0 to High(tempIntegers) do
		begin
			tempIntegers[i] := framesPerCell;
		end;
		result := CreateSprite(bitmaps, tempIntegers);
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

		if spriteToFree <> nil then
		begin
			SetLength(spriteToFree.bitmaps, 0);
			Dispose(spriteToFree);
			spriteToFree := nil;
		end;
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
	function AddBitmapToSprite(spriteToAddTo : Sprite; bitmapToAdd : Bitmap): Integer;
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
	function CurrentWidth(sprite: Sprite): Integer; inline;
	begin
		result := sprite.width;
	end;
  
	/// Returns the current height of the sprite.
	///
	///	@param sprite:		 The sprite to get the height of
	///	@returns					 The height of the sprite's current frame
	function CurrentHeight(sprite: Sprite): Integer; inline;
	begin
		result := sprite.height;
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
		try
			SDL_BlitSurface(bitmapToDraw.surface, nil, dest.surface, @offset);
		except
			RaiseSGSDKException('Failed to draw the specified bitmap');
		end;
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
		try
			SDL_BlitSurface(bitmapToDraw.surface, @source, dest.surface, @offset);
		except
			RaiseSGSDKException('Failed to draw the specified part of the specified bitmap');
		end;
	end;

	/// Draws part of a bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw: The bitmap to be drawn onto the screen
	///	@param srcX, srcY:	The x,y offset to the area to copy in bitmapToDraw
	///	@param srcW, srcH:	The width and height of the area to copy
	///	@param x,y:				The x,y location to draw the bitmap part to
	///
	/// Side Effects:
	///	- Draws part of the bitmapToDraw at the x,y location on the screen.
	///	- Effected by visible window
	procedure DrawBitmapPartOnScreen(bitmapToDraw : Bitmap; 
               srcX, srcY, srcW, srcH, x, y : Integer); overload;
	begin
		DrawBitmapPart(scr, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
	end;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap; 
               srcX, srcY, srcW, srcH: Integer; x, y : Single); overload;
	begin
		DrawBitmapPart(scr, bitmapToDraw, srcX, srcY, srcW, srcH, SGSDK_Camera.ScreenX(x), SGSDK_Camera.ScreenY(y));
	end;


	/// Draws one bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw:	The bitmap to be drawn onto the screen
	///	@param x,y:				The x,y location to draw the bitmap to
	///
	/// Side Effects:
	///	- Draws the bitmapToDraw at the x,y location on the screen.
	procedure DrawBitmapOnScreen(bitmapToDraw : Bitmap; x, y : Integer); overload;
	begin
		DrawBitmap(scr, bitmapToDraw, x, y);
	end;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Single); overload;
	begin
		DrawBitmap(scr, bitmapToDraw, SGSDK_Camera.ScreenX(x), SGSDK_Camera.ScreenY(y));
	end;
	
	procedure ReplayAnimation(theSprite : Sprite);
	begin
		theSprite.currentFrame := 0;
		theSprite.hasEnded := false;
	end;
	
	/// Update the frame position
	///
	/// @param spriteToDraw:	The sprite to be processed
	///
	/// Side Effects:
	/// - Process the frame position depending on the sprite's setting
	procedure UpdateSpriteAnimation(spriteToDraw : Sprite);
	var
		i : Integer;
		notAllZero : Boolean;
	begin
		if spriteToDraw.hasEnded then exit;
		
		if spriteToDraw.spriteKind <> StaticSprite then
		begin
			notAllZero := true;
			
			for i := Low(spriteToDraw.framesPerCell) to High(spriteToDraw.framesPerCell) do begin
				if spriteToDraw.framesPerCell[i] < 0 then
					RaiseSGSDKException('Frames per cell must be 0 or positive');
				if spriteToDraw.framesPerCell[i] > 0 then
					notAllZero := false;
			end;

			if notAllZero then
				RaiseSGSDKException('Frames per cell cannot be all the zero');
			
			spriteToDraw.frameCount := spriteToDraw.frameCount + 1;
			
			if spriteToDraw.frameCount >= spriteToDraw.framesPerCell[spriteToDraw.currentFrame] then
			begin
				spriteToDraw.frameCount := 0;
				if spriteToDraw.reverse then
				begin
					spriteToDraw.currentFrame := spriteToDraw.currentFrame - 1;
					
					if (spriteToDraw.currentFrame < Low(spriteToDraw.framesPerCell)) then
					begin
						if spriteToDraw.endingAction = ReverseOnce then
						begin
							spriteToDraw.currentFrame := 0;
							spriteToDraw.hasEnded := true;
						end
						else
						begin
							spriteToDraw.currentFrame := 1;
							spriteToDraw.reverse := false;
						end;
					end;
				end
				else //going forward
				begin
					spriteToDraw.currentFrame := spriteToDraw.currentFrame + 1;
					
					if (spriteToDraw.currentFrame > High(spriteToDraw.framesPerCell)) then
					begin
						if (spriteToDraw.endingAction = ReverseLoop) or (spriteToDraw.endingAction = ReverseOnce) then
						begin
							spriteToDraw.reverse := true;
							spriteToDraw.currentFrame := SpriteToDraw.currentFrame - 1;
						end;
						if spriteToDraw.endingAction = Loop then
							spriteToDraw.currentFrame := 0;
						if spriteToDraw.endingAction = Stop then
						begin
							spriteToDraw.currentFrame := High(SpriteToDraw.framesPerCell);
							spriteToDraw.hasEnded := true;
						end;
					end;
				end;
			end;
			if SpriteToDraw.spriteKind = AnimArraySprite then
			begin
				spriteToDraw.width := spriteToDraw.bitmaps[spriteToDraw.currentFrame].width;
				spriteToDraw.height := spriteToDraw.bitmaps[spriteToDraw.currentFrame].height;
			end;
			if spriteToDraw.framesPerCell[spriteToDraw.currentFrame] = 0 then
				UpdateSpriteAnimation(spriteToDraw);

			if spriteToDraw.framesPerCell[spriteToDraw.currentFrame] = 0 then
				UpdateSpriteAnimation(spriteToDraw);
		end;
		
		//Skip Fix
		//WriteLn('fpc: ', Length(spriteToDraw.framesPerCell), ' cc: ', spritetoDraw.currentFrame);
	end;
	
	procedure UpdateSprite(spriteToDraw: Sprite);
	begin
		MoveSprite(spriteToDraw);
		UpdateSpriteAnimation(spriteToDraw);
	end;
	
	/// Draws a sprite to the screen, without using a view port.
	///
	///	@param spriteToDraw:		 The sprite to be drawn
	///
	/// Side Effects:
	///	- The sprite is drawn to the screen, if within screen area
	procedure DrawSprite(spriteToDraw: Sprite); overload;
	var
		srcX, srcY: Integer; // the source x, y : i.e. the location of the current part for multi animations
	begin
		if IsSpriteOffscreen(spriteToDraw) then exit;
		
		if spriteToDraw.spriteKind <> AnimMultiSprite then
		begin
			DrawBitmap(spriteToDraw.bitmaps[spriteToDraw.currentFrame], spriteToDraw.xPos, spriteToDraw.yPos);
		end
		else
		begin
			with spriteToDraw^ do
			begin
				srcX := (currentFrame mod cols) * width;
				srcY := (currentFrame - (currentFrame mod cols)) div cols * height;
			end;
			
			DrawBitmapPart(spriteToDraw.bitmaps[0], srcX, srcY, 
								spriteToDraw.width, spriteToDraw.height,
								spriteToDraw.xPos, spriteToDraw.yPos);
		end;
	end;
	
	procedure DrawSprite(spriteToDraw : Sprite; xOffset, yOffset: Integer); overload;
	var
		tempWidth, tempHeight: Integer;
	begin
		if spriteToDraw.spriteKind <> AnimMultiSprite then
		begin
			DrawBitmap(spriteToDraw.bitmaps[spriteToDraw.currentFrame], 
				spriteToDraw.xPos + xOffset, spriteToDraw.yPos + yOffset);
		end
		else
		begin
			tempWidth := spriteToDraw.currentFrame div spriteToDraw.cols * spriteToDraw.width;
			tempHeight := spriteToDraw.currentFrame mod spriteToDraw.cols * spriteToDraw.height;
			
			DrawBitmapPartOnScreen(spriteToDraw.bitmaps[0], tempWidth, tempHeight, 
								spriteToDraw.width, spriteToDraw.height,
								Round(spriteToDraw.xPos + xOffset), Round(spriteToDraw.yPos + yOffset));
		end;		
	end;

  	/// Determines if a sprite is off the screen.
	///
	///	@param theSprite:			The sprite to check the position of
	///	@returns					True if the sprite is off the screen
	function IsSpriteOffscreen(theSprite : Sprite): Boolean;
	begin
		//WriteLn(theSprite.xPos, ' -> ', SGSDK_Camera.SGSDK_Camera.ScreenX(theSprite.xPos));
		
		if SGSDK_Camera.ScreenX(theSprite.xPos) >= ScreenWidth() then result := true
		else if SGSDK_Camera.ScreenX(theSprite.xPos) + CurrentWidth(theSprite) < 0 then result := true
		else if SGSDK_Camera.ScreenY(theSprite.yPos) >= ScreenHeight() then result := true
		else if SGSDK_Camera.ScreenY(theSprite.yPos) + CurrentHeight(theSprite) < 0 then result := true
		else result := false;
	end;

	/// Moves a sprite based on information in a movement vector.
	///
	///	@param spriteToMove:		 The sprite to move
	///	@param movementVector:	 The vector containing the movement details
	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector); overload;
	begin
		try
			spriteToMove.xPos := spriteToMove.xPos + movementVector.x;
			spriteToMove.yPos := spriteToMove.yPos + movementVector.y;
		except
			RaiseSGSDKException('Failed to move the specified sprite using the specified vector');
		end;
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
		try
			spriteToMove.xPos := x;
			spriteToMove.yPos := y;
		except
			RaiseSGSDKException('Failed to move the specified sprite to the spceified coordinate');
		end;
	end;
	
	procedure MoveSprite(spriteToMove: Sprite); overload;
	begin
		try
			spriteToMove.xPos := spriteToMove.xPos + spriteToMove.movement.x;
			spriteToMove.yPos := spriteToMove.yPos + spriteToMove.movement.y;
		except
			RaiseSGSDKException('Failed to move the specified sprite with the attached vector');
		end;
	end;
	
	/// Creates a bitmap in memory that can be drawn onto. The bitmap is initially
	///	transparent and can be used as the target for various drawing operations.
	///	Once you have drawn the desired image onto the bitmap you can call
	///	OptimiseBitmap to optimise the surface.
	///
	///  @param width, height:  The width and height of the surface
	///  @returns:              A new bitmap
	function CreateBitmap(width, height: Integer): Bitmap;
	begin
    result := nil;

		if (width < 1) or (height < 1) then
			RaiseSGSDKException('Bitmap width and height must be greater then 0');
    if (baseSurface = nil) or (baseSurface.format = nil) then
      RaiseSGSDKException('Unable to CreateBitmap as the window is not open');

		try
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
		except
      Dispose(result);
			RaiseSGSDKException('Failed to create a bitmap');
		end;
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
		try
			oldSurface := surface.surface;
			SetNonAlphaPixels(surface, oldSurface);
			surface.surface := SDL_DisplayFormatAlpha(oldSurface);
			SDL_FreeSurface(oldSurface);
		except
			RaiseSGSDKException('Failed to optimise the specified bitmap')
		end;
	end;
	
	procedure PutPixel(surface: PSDL_Surface; x, y: Integer; color: Color);
	var
	  bufP: PUInt32;
	  pixels: PUint32;
	  addr: UInt32;
	begin
		try
			if (x < 0) or (x >= surface.w) or (y < 0) or (y >= surface.h) then exit;

			pixels := surface.pixels;
			//addr := Integer(pixels) + (( y * surface.w ) + x) * surface.format.BytesPerPixel;
			addr := UInt32(pixels) + (UInt32(x) * surface.format.BytesPerPixel) + (Uint32(y) * surface.pitch) ;
			bufp := PUint32(addr);
			bufp^ := color;
			//PixelColor(surface, x, y, ToSDLGFXColor(color));
		except
			RaiseSGSDKException('Failed to put pixel to the specified coordinate');
		end;
	end;
	
	/// Draws a pixel onto the screen.
	///
	///	@param theColor:		 The color to draw the pixel
	///	@param x,y:					The x,y location to draw the pixel at
	///
	/// Side Effects:
	///	- Sets one pixel on the screen
	procedure DrawPixelOnScreen(theColour: Colour; x, y: Integer);
	begin
		DrawPixel(scr, theColour, x, y);
	end;

	procedure DrawPixel(theColour: Colour; x, y: Single); overload;
	begin
		DrawPixelOnScreen(theColour, SGSDK_Camera.ScreenX(x), SGSDK_Camera.ScreenY(y));
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
	procedure DrawRectangleOnScreen(theColour : Colour; filled : Boolean;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, filled, xPos, yPos, width, height);
	end;

	procedure DrawRectangle(theColour : Colour; filled : Boolean;
                          xPos, yPos: Single; width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, filled, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
	end;

	/// Draws the outline of a rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure DrawRectangleOnScreen(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, xPos, yPos, width, height);
	end;
	
	procedure DrawRectangle(theColour: Colour;
                          xPos, yPos: Single; width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
	end;

	/// Draws a filled rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure FillRectangleOnScreen(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		FillRectangle(scr, theColour, xPos, yPos, width, height);
	end;

	procedure FillRectangle(theColour : Colour;
                          xPos, yPos: Single; width, height : Integer); overload;
	begin
		FillRectangle(scr, theColour, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
	end;

	/// Draws a line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param xPosStart,yPosStart: The x,y location to start the line at
	///	@param xPosEnd, yPosEnd:		The x,y location to end the line at
	///
	/// Side Effects:
	///	- Draws a line in the screen
	procedure DrawLineOnScreen(theColour: Colour; 
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Integer); overload;
	begin
		DrawLine(scr, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
	end;
	
	procedure DrawLine(theColour: Colour; 
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
	begin
		DrawLine(scr, theColour, SGSDK_Camera.ScreenX(xPosStart), SGSDK_Camera.ScreenY(yPosStart), SGSDK_Camera.ScreenX(xPosEnd), SGSDK_Camera.ScreenY(yPosEnd));
	end;

	/// Draws a horizontal line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param y:						The y location of the line
	///	@param x1, x2:			 The starting and ending x value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: Integer); overload;
	begin
		DrawHorizontalLine(scr, theColor, y, x1, x2);
	end;

	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Single); overload;
	begin
		DrawHorizontalLine(scr, theColor, SGSDK_Camera.ScreenY(y), SGSDK_Camera.ScreenX(x1), SGSDK_Camera.ScreenX(x2));
	end;

	/// Draws a vertical line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param x:						The x location of the line
	///	@param y1, y2:			 The starting and ending y value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: Integer); overload;
	begin
		DrawVerticalLine(scr, theColor, x, y1, y2);
	end;

	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Single); overload;
	begin
		DrawVerticalLine(scr, theColor, SGSDK_Camera.ScreenX(x), SGSDK_Camera.ScreenY(y1), SGSDK_Camera.ScreenY(y2));
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
	procedure DrawCircleOnScreen(theColour: Colour; filled: Boolean;
                       xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, filled, xc, yc, radius);
	end;

	procedure DrawCircle(theColour: Colour; filled: Boolean;
                       xc, yc: Single; radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, filled, SGSDK_Camera.ScreenX(xc), SGSDK_Camera.ScreenY(yc), radius);
	end;


	/// Draws a circle outline centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure DrawCircleOnScreen(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, xc, yc, radius);
	end;

	procedure DrawCircle(theColour: Colour; xc, yc: Single; radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, SGSDK_Camera.ScreenX(xc), SGSDK_Camera.ScreenY(yc), radius);
	end;

	/// Draws a filled circle centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure FillCircleOnScreen(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		FillCircle(scr, theColour, xc, yc, radius);
	end;

	procedure FillCircle(theColour: Colour; xc, yc: Single; radius: Integer); overload;
	begin
		FillCircle(scr, theColour, SGSDK_Camera.ScreenX(xc), SGSDK_Camera.ScreenY(yc), radius);
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
	procedure DrawEllipseOnScreen(theColour: Colour; filled: Boolean;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, filled, xPos, yPos, width, height);
	end;

	procedure DrawEllipse(theColour: Colour; filled: Boolean;
                        xPos, yPos: Single; width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, filled, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
	end;

	/// Draws a ellipse outline within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse on the screen
	procedure DrawEllipseOnScreen(theColour: Colour;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	procedure DrawEllipse(theColour: Colour;
                        xPos, yPos: Single; width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
	end;


	/// Draws a filled ellipse within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the screen
	 procedure FillEllipseOnScreen(theColour: Colour; 
                         xPos, yPos, width, height: Integer); overload;
	begin
		FillEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	procedure FillEllipse(theColour: Colour; 
                         xPos, yPos: Single; width, height: Integer); overload;
	begin
		FillEllipse(scr, theColour, SGSDK_Camera.ScreenX(xPos), SGSDK_Camera.ScreenY(yPos), width, height);
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
	procedure DrawRectangle(dest: Bitmap; theColour: Colour; filled : Boolean;
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
		end else rect.x := xPos;
		
		if height < 0 then
		begin
			rect.y := yPos + height; //move up by height
			height := -height;
		end else rect.y := yPos;
		
		rect.w := width;
		rect.h := height;
		
		try
			SDL_FillRect(dest.surface, @rect, theColour);
		except
			RaiseSGSDKException('Failed to fill rectangle');
		end;
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
		x, y: Integer;
		xRadius, yRadius: Integer;
		xChange, yChange: Integer;
		ellipseError: Integer;
		twoASquare, twoBSquare: Integer;
		stoppingX, stoppingY: Integer;
	begin
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw an ellipse is nil');
		end;
		
		if (width < 1) or (height < 1) then begin
			RaiseSGSDKException('Ellipse width and height must be greater then 0');
		end;
		
		xRadius := width div 2;
		yRadius := height div 2;

		xPos := xPos + xRadius;
    yPos := yPos + yRadius;

		twoASquare := 2 * (width shr 1) * (width shr 1);
		twoBSquare := 2 * (height shr 1) * (height shr 1);

		// 1st set of points
		x := (width shr 1) - 1;  // radius zero == draw nothing
		y := 0;

		xChange := (height shr 1) * (height shr 1) * (1 - 2 * (width shr 1));
		yChange := (width shr 1) * (width shr 1);

		ellipseError := 0;

		stoppingX := twoBSquare * (width shr 1);
		stoppingY := 0;

		//Lock dest
		if SDL_MUSTLOCK(dest.surface) then
		begin
			if SDL_LockSurface(dest.surface) < 0 then exit;
		end;

		// Plot four ellipse points by iteration
		while stoppingX > stoppingY do
		begin
			PutPixel(dest.surface, xPos + x, yPos + y, theColour);
			PutPixel(dest.surface, xPos - x, yPos + y, theColour);
			PutPixel(dest.surface, xPos + x, yPos - y, theColour);
			PutPixel(dest.surface, xPos - x, yPos - y, theColour);

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
		y := (height shr 1) - 1;  //radius zero == draw nothing
		xChange := (height shr 1) * (height shr 1);
		yChange := (width shr 1) * (width shr 1) * (1 - 2 * (height shr 1));
		ellipseError := 0;
		stoppingX := 0;
		stoppingY := twoASquare * (height shr 1);

		//Plot four ellipse points by iteration
		while stoppingX < stoppingY do
		begin
			PutPixel(dest.surface, xPos + x, yPos + y, theColour);
			PutPixel(dest.surface, xPos - x, yPos + y, theColour);
			PutPixel(dest.surface, xPos + x, yPos - y, theColour);
			PutPixel(dest.surface, xPos - x, yPos - y, theColour);

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
		
		try
			// Unlock dest
			if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to unlock the bitmap');
		end;
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
		x, y: Integer;
		xChange, yChange: Integer;
		ellipseError: Integer;
		twoASquare, twoBSquare: Integer;
		stoppingX, stoppingY: Integer;
		xRadius, yRadius: Integer;
	begin
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw an ellipse is nil');
		end;
		
		if (width < 1) or (height < 1) then begin
			RaiseSGSDKException('Ellipse width and height must be greater then 0');
		end;
		
		xRadius := width div 2;
		yRadius := height div 2;
		
		xPos := xPos + xRadius;
		yPos := yPos + yRadius;
		
		twoASquare := 2 * (width shr 1) * (width shr 1);
		twoBSquare := 2 * (height shr 1) * (height shr 1);
		
		// 1st set of points
		x := (width shr 1) - 1;  // radius zero == draw nothing
		y := 0;
		
		xChange := (height shr 1) * (height shr 1) * (1 - 2 * (width shr 1));
		yChange := (width shr 1) * (width shr 1);
		
		ellipseError := 0;
		
		stoppingX := twoBSquare * (width shr 1);
		stoppingY := 0;
		
		//Lock dest
		if SDL_MUSTLOCK(dest.surface) then
		begin
			if SDL_LockSurface(dest.surface) < 0 then exit;
		end;
		
		// Plot four ellipse points by iteration
		while stoppingX > stoppingY do
		begin
			DrawHorizontalLine(dest, theColour, yPos + y, xPos - x, xPos + x);
			DrawHorizontalLine(dest, theColour, yPos - y, xPos - x, xPos + x);
			
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
		y := (height shr 1) - 1;  //radius zero == draw nothing
		xChange := (height shr 1) * (height shr 1);
		yChange := (width shr 1) * (width shr 1) * (1 - 2 * (height shr 1));
		ellipseError := 0;
		stoppingX := 0;
		stoppingY := twoASquare * (height shr 1);
		
		//Plot four ellipse points by iteration
		while stoppingX < stoppingY do
		begin
			DrawHorizontalLine(dest, theColour, yPos + y, xPos - x, xPos + x);
			DrawHorizontalLine(dest, theColour, yPos - y, xPos - x, xPos + x);
			
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
		
		try
			// Unlock dest
			if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to unlock the destination bitmap');
		end;
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a vertical line is nil');
		end;
		
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
			exit;
		end;
		
		if y1 < 0 then y1 := 0;
		if y2 >= h then y2 := h - 1;
		
		pixels := dest.surface.pixels;
		addr := UInt32(pixels) + (UInt32(x) * dest.surface.format.BytesPerPixel) + (UInt32(y1) * dest.surface.Pitch);
		bufp := PUint32(addr);
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_LockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to lock the destination bitmap')
		end;
		
		for y := y1 to y2 - 1 do
		begin
			bufp := PUInt32(UInt32(bufp) + (dest.surface.pitch));
			bufp^ := theColor;
		end;
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to unlock the destination bitmap');
		end;
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a horizontal line is nil');
		end;
		
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
			exit; //no single point of the line is on screen
		end;
		
		if x1 < 0 then x1 := 0;
		if x2 >= w then x2 := w - 1;
		
		pixels := dest.surface.pixels;
		addr := UInt32(pixels) + ((UInt32(x1 - 1)) * dest.surface.format.BytesPerPixel) + (UInt32(y) * dest.surface.pitch);
		bufp := PUint32(addr);
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_LockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to lock the destination bitmap')
		end;
		
		for x := x1 to x2 do
		begin
			Inc(bufp);
			bufp^ := theColor;
		end;
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to unlock the destination bitmap');
		end;
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a line is nil');
		end;
		
		if xPosStart = xPosEnd then
			DrawVerticalLine(dest, theColour, xPosStart, yPosStart, yPosEnd)
		else if yPosStart = yPosEnd then
			DrawHorizontalLine(dest, theColour, yPosStart, xPosStart, xPosEnd)
		else
		begin
		  	deltax := abs(xPosEnd - xPosStart);			// The difference between the x's
			deltay := abs(yPosEnd - yPosStart);			// The difference between the y's
			
			x := xPosStart;								// Start x off at the first pixel
			y := yPosStart;								// Start y off at the first pixel
			
			if xPosEnd >= xPosStart then				// The x-values are increasing
			begin
				xinc1 := 1;
				xinc2 := 1;
			end
			else										// The x-values are decreasing
			begin
				xinc1 := -1;
				xinc2 := -1;
			end;
			
			if yPosEnd >= yPosStart then				// The y-values are increasing
			begin
				yinc1 := 1;
				yinc2 := 1;
			end
			else										// The y-values are decreasing
			begin
				yinc1 := -1;
				yinc2 := -1;
			end;
			
			if deltax >= deltay then 					// There is at least one x-value for every y-value
			begin
				xinc1 := 0;								// Don't change the x when numerator >= denominator
				yinc2 := 0;								// Don't change the y for every iteration
				
				den := deltax;
				
				num := deltax div 2;
				
				numadd := deltay;
				
				numpixels := deltax;					// There are more x-values than y-values
			end
			else										// There is at least one y-value for every x-value
			begin
				xinc2 := 0;								// Don't change the x for every iteration
				yinc1 := 0;								// Don't change the y when numerator >= denominator
				den := deltay;
				num := deltay div 2;
				
				numadd := deltax;
				
				numpixels := deltay;					// There are more y-values than x-values
			end;
			
			for curpixel := 0 to numpixels do
			begin
				PutPixel(dest.surface, x, y, theColour);// Draw the current pixel
				
				num := num + numadd;					// Increase the numerator by the top of the fraction
				
				if num >= den then						// Check if numerator >= denominator
				begin
					num := num - den;					// Calculate the new numerator value
					x := x + xinc1;						// Change the x as appropriate
					y := y + yinc1;						// Change the y as appropriate
				end;
				
				x := x + xinc2;							// Change the x as appropriate
				y := y + yinc2;							// Change the y as appropriate
			end;
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a pixel is nil');
		end;
		
		if (x < 0) or (x >= dest.surface.w) or (y < 0) or (y >= dest.surface.h) then exit;
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_LockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to lock the destination bitmap')
		end;
		
		PutPixel(dest.surface, x, y, theColour);
		
		try
			if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
		except
			RaiseSGSDKException('Failed to unlock the destination bitmap');
		end;
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
			FillCircle(dest, theColour, xc, yc, radius)
		else
			DrawCircle(dest, theColour, xc, yc, radius);
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a circle is nil');
		end;
		
		if radius < 1 then begin
			RaiseSGSDKException('Radius for a circle must be greater then 0');
		end;
		
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
		if dest = nil then begin
			RaiseSGSDKException('The destination bitmap to draw a circle is nil');
		end;
		
		if radius < 1 then begin
			RaiseSGSDKException('Radius for a circle must be greater then 0');
		end;
		
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
			
			if b <> pb then DrawHorizontalLine(dest, theColour, b, a, c);
			if d <> pd then DrawHorizontalLine(dest, theColour, d, a, c);
			if f <> b  then DrawHorizontalLine(dest, theColour, f, e, g);
			if (h <> d) and (h <> f) then DrawHorizontalLine(dest, theColour, h, e, g);
			
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