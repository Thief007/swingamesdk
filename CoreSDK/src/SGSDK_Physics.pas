﻿unit SGSDK_Physics;

interface
	uses
		SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing, SGSDK_Core, SGSDK_Graphics, SGSDK_Camera;

	type
	    /// type: Matrix2d
	    ///
	    ///  This record is used to represent transformations that can be
	    ///  used to apply these changes to vectors.
		Matrix2D = Array [0..2,0..2] of Single;

		/// Enumeration: CollisionDetectionRanges
		///	This is used to indicate the kind of collision being checked with the
		///	Sprite collision routines. 
		CollisionDetectionRange = (
				CollisionRangeEquals			= 0,
				CollisionRangeGreaterThan = 1,
				CollisionRangeLessThan		= 2
			);
			
	//*****
	//
	// Collision detection routines
	//
	//*****
	//
	// These routines are used to detect collisions between sprites or bitmaps.
	//

	function HasSpriteCollidedX(theSprite : Sprite; x : Single;
										 range : CollisionDetectionRange): Boolean;

	function HasSpriteCollidedY(theSprite : Sprite; y : Single;
										 range : CollisionDetectionRange): Boolean;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Boolean; overload;

	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;

	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
													 x, y: Single; bounded: Boolean)
													: Boolean; overload;

	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: Integer): Boolean; overload;
	function IsSpriteOnScreenAt(theSprite: Sprite; v: Vector): Boolean; overload;
	
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single): Boolean; overload;


	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2: Bitmap; x2, y2: Integer): Boolean; overload;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Boolean; overload;

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
		: Boolean; overload;

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Boolean; overload;

	//*****
	//
	// Vector routines
	//
	//*****
	//
	// These routines are used to manipulate vectors in the API.
	//

	function CreateVector(x,y : Single; invertY : boolean): Vector; overload;

	function CreateVector(x,y : Single): Vector; overload;

	function AddVectors(v1, v2 : Vector): Vector;

	function SubtractVectors(v1, v2 : Vector): Vector;

	function InvertVector(v : Vector): Vector;

	function ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer): Vector;

	function LimitVector(theVector: Vector; maxMagnitude: Single): Vector;

	function GetUnitVector(theVector : Vector): Vector;

	function IsZeroVector(theVector : Vector): Boolean;

	function Magnitude(theVector : Vector): Single;


	function DotProduct(v1, v2: Vector): Single;
	function MultiplyVector(v1: Vector; s1: Single): Vector;

 	function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
	function CalculateAngle(sprite1, sprite2: Sprite): Single; overload;

	function TranslationMatric(dx, dy: Single): Matrix2D;
	function ScaleMatrix(scale: Single): Matrix2D;
	function RotationMatrix(deg: Single): Matrix2D;
	function Multiply(const m1, m2: Matrix2D): Matrix2D; overload;
	function Multiply(const m: Matrix2D; const v: Vector): Vector; overload;
	procedure VectorCollision(p1, p2: Sprite);
	
	function CalculateVectorFromTo(obj, dest: Sprite): Vector;

	function GetVectorFromAngle(angle, magnitude: Single): Vector;

	function VectorWithinRect(const v: Vector; x, y, w, h: Single): Boolean;
		
implementation
	uses
		SysUtils, Math, Classes;

	/// Creates a new vector with values x and y, possibly with an inverted y. The
	///	inversion of the y value provides a convienient option for handling
	///	screen related vectors.
	///
	///	@param x, y			Initial values for the vector
	///	@param invertY	 Indicates if the y value should be inverted.
	///	@returns				 A new vector with values x and y
	function CreateVector(x,y : Single; invertY : boolean): Vector; overload;
	begin
		if invertY then y := y * -1;

		result.x := x;
		result.y := y;
		result.w := 1;
	end;

	/// Creates a new vector with values x and y.
	///
	///	@param x, y			Initial values for the vector
	///	@returns				 A new vector with values x and y
	function CreateVector(x,y : Single): Vector; overload;
	begin
		result := CreateVector(x,y,false);
	end;


	/// Adds v1 and v2.
	///
	///	@param v1, v2			The vectors to work with
	///	@returns				v1 + v2
	function AddVectors(v1, v2 : Vector): Vector;
	begin
		result.x := v1.x + v2.x;
		result.y := v1.y + v2.y;
	end;

	/// Subtracts v2 from v1 (i.e. v1 - v2).
	///
	///	@param v1, v2			The vectors to work with
	///	@returns				v1 - v2
	function SubtractVectors(v1, v2 : Vector): Vector;
	begin
		result.x := v1.x - v2.x;
		result.y := v1.y - v2.y;
	end;

	/// Inverts the vector v. Changes the direction of the vector's x and y.
	///
	///	@param v		 The vector to invert
	///	@returns		 A new inverted vector
	function InvertVector(v : Vector): Vector;
	begin
		result.x := v.x * -1;
		result.y := v.y * -1;
	end;

	/// Limits the vector within the range min-max of X-Y. AVOID use... use
	 	///  LimitMagnitude
	///
	///	@param theVector		 The vector to limit
	///	@param minX, maxX		The limited range of the vector's x
	///	@param minY, maxY		The limited range of the vector's y
	///	@returns						 A new vector limited within that range
	function ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer)
	   : Vector;
	begin
		if theVector.x > maxX then theVector.x := maxX;
		if theVector.x < minX then theVector.x := minX;
		if theVector.y > maxY then theVector.y := maxY;
		if theVector.y < minY then theVector.y := minY;

		result := theVector;
	end;

	/// Limit the magnitude of a vector.
	///
	///	@param theVector:		The vector to limit
	///	@param maxMagnitude:	The maximum magnitude of the vector.
	///	@returns				A new vector with the same direction as theVector,
	///							with a maximum magnitude of maxMagnitude
	function LimitVector(theVector: Vector; maxMagnitude: Single): Vector;
	var
		mag: Single;
	begin
		mag := Magnitude(theVector);

		if mag > maxMagnitude then
			result := Multiply(ScaleMatrix(maxMagnitude), GetUnitVector(theVector))
		else
			result := theVector;
	end;

	/// Gets the unit vector of the passed in vector. The unit vector has a
	///	magnitude of 1, resulting in a vector that indicates the direction of
	///	the original vector.
	///
	///	@param theVector		 The vector to get the unit vector of
	///	@returns						 The unit vector from the passed in vector
	function GetUnitVector(theVector : Vector): Vector;
	var
		temp : Double;
		vectorMagnitude : Single;
	begin
		vectorMagnitude := Magnitude(theVector);

		if vectorMagnitude = 0 then
			temp := 0
		else
			temp := 1 / Magnitude(theVector);

		result.x := temp * theVector.x;
		result.y := temp * theVector.y;
	end;

	/// Indicates if the magnitude of the vector is 0.
	///
	///	@param theVector			The vector to check
	///	@returns					True if the vector has values 0, 0
	function IsZeroVector(theVector : Vector): Boolean;
	begin
		result := (theVector.x = 0) and (theVector.y = 0);
	end;

	/// Returns the magnitude of a vector. The magnitude represents the length of
	///	the vector.
	///
	///	@param theVector			The vector to get the magnitude of
	///	@returns					The magnitude of the vector
	function Magnitude(theVector : Vector): Single;
	begin
		result := Sqrt((theVector.x * theVector.x) + (theVector.y * theVector.y));
	end;

	function DotProduct(v1, v2: Vector): Single;
	begin
		result := (v1.x * v2.x) + (v1.y * v2.y);
	end;

	function MultiplyVector(v1: Vector; s1: Single): Vector;
	begin
		result.x := v1.x * s1;
		result.y := v1.y * s1;
	end;

	/// Determines if a sprite has collided with a given x position.
	///
	///	@param theSprite:		The sprite to check
	///	@param x:				The x location to check collision with
	///	@param range:			The kind of check to perform less, larger or equal.
	///
	///	@returns				True if the sprite is within the range requested
	function HasSpriteCollidedX(theSprite: Sprite; x: Single; 
	             range: CollisionDetectionRange): Boolean;
	begin
    result := false;
    
		if range = CollisionRangeEquals then
			result := (x >= theSprite.xPos) and 
	        (x <= theSprite.xPos + theSprite.width)
		else if range = CollisionRangeGreaterThan then
			result := x <= theSprite.xPos + 
	                    theSprite.width
		else if range = CollisionRangeLessThan then
			result := x >= theSprite.xPos
		else
			RaiseSGSDKException('Invalid Collision Range');
	end;

	/// Determines if a sprite has collided with a given y position. The x and y
	///	values are in "world" coordinates.
	///
	///	@param theSprite:		The sprite to check
	///	@param y:				The y location to check collision with
	///	@param range:			The kind of check to perform less, larger or equal.
	///
	///	@returns				True if the sprite is within the range requested
	function HasSpriteCollidedY(theSprite : Sprite; y : Single; 
								range : CollisionDetectionRange): Boolean;
	begin
    result := false;
    
		if range = CollisionRangeEquals then
			result := (y >= theSprite.yPos) and 
	       (y <= theSprite.yPos + theSprite.height)
		else if range = CollisionRangeGreaterThan then
			result := y <= theSprite.yPos + 
	                    theSprite.height
		else if range = CollisionRangeLessThan then
			result := y >= theSprite.yPos
		else
			RaiseSGSDKException('Invalid Collision Range');
	end;

	function HasBitmapCollidedWithRect(const image: Bitmap; 
									   x, y, rectX, rectY, rectWidth, rectHeight: Integer): Boolean;
	begin
		if y + image.height <= rectY then result := false
		else if y >= rectY + rectheight then result := false
		else if x + image.width <= rectX then result := false
		else if x >= rectX + rectWidth then result := false
		else result := true;
	end;

	/// Determined if a sprite has collided with a given rectangle. The rectangles
	///	coordinates are expressed in "world" coordinates.
	///
	///	@param theSprite:			The sprite to check
	///	@param x, y :					The x,y location of the rectangle
	///	@param width, height:	The width and height of the rectangle
	///
	///	@returns							 True if the sprite collides with the rectangle
	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
									   width, height : Integer): Boolean; overload;
	begin
		if theSprite = nil then RaiseSGSDKException('The specified sprite is nil');
		if (width < 1) or (height < 1) then 
			RaiseSGSDKException('Rectangle width and height must be greater then 0');
		if theSprite.yPos + CurrentHeight(theSprite) <= y then result := false
		else if theSprite.yPos >= y + height then result := false
		else if theSprite.xPos + CurrentWidth(theSprite) <= x then result := false
		else if theSprite.xPos >= x + width then result := false
		else result := true;
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: Integer): Boolean; overload;
	begin
		result := HasSpriteCollidedWithRect(theSprite, GameX(x), GameY(y), 1, 1);
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; v: Vector): Boolean; overload;
	var
		gameVector: Vector;
	begin
		gameVector := ToGameCoordinates(v);
		result := HasSpriteCollidedWithRect(theSprite, gameVector.x, gameVector.y, 1, 1);		
	end;
	
	function IsPixelDrawnAtPoint(image: Bitmap; x, y: Integer) : Boolean;
	begin
		result := (Length(image.nonTransparentPixels) = image.width)
							and ((x >= 0) and (x < image.width))
							and ((y >= 0) and (y < image.height))
							and image.nonTransparentPixels[x, y];
	end;

	/// Performs a collision detection within two bitmaps at the given x, y
	///	locations using per pixel collision detection. This checks to see if
	///	two non-transparent pixels collide.
	///
	///	@param image1, image2:	The bitmap images to check for collision
	///	@param x1, y1:			The x,y location of image 1
	///	@param x2, y2:			The x,y location of image 2
	///
	///	@returns				True if the bitmaps collide.
	///
	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
	       : Boolean; overload;
	begin
		result := CollisionWithinBitmapImages(image1, x1, y1, false, 
	                 image2, x2, y2, false);
	end;

	/// Performs a collision detection within two bitmaps at the given x, y
	///	locations. The bounded values indicate if each bitmap should use per
	///	pixel collision detection or a bounded collision detection. This version
	///	uses pixel based checking at all times.
	///
	///	When both bitmaps are using bounded collision the routine checks to see
	///	if the bitmap rectangles intersect. If one is bounded and the other is
	///	pixel based the routine checks to see if a non-transparent pixel in the
	///	pixel based image intersects with the bounds of the bounded image. If
	///	both are pixel based, the routine checks to see if two non-transparent
	///	pixels collide.
	///
	///	Note: Bitmaps do not need to actually be drawn on the screen.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				The x,y location of image 1
	///	@param bounded1:			Indicates if image1 should use bounded collision
	///	@param x2, y2:				The x,y location of image 2
	///	@param bounded2:			Indicates if image2 should use bounded collision
	///
	///	@returns					True if the bitmaps collide.
	///	
	function CollisionWithinBitmapImages(
	           image1: Bitmap; x1, y1, w1, h1, offsetX1, offsetY1: Integer; bounded1: Boolean;
			   image2: Bitmap; x2, y2, w2, h2, offsetX2, offsetY2: Integer; bounded2: Boolean
			) : Boolean; overload;
	var
		left1, left2, overLeft: Integer;
		right1, right2, overRight: Integer;
		top1, top2, overTop: Integer;
		bottom1, bottom2, overBottom: Integer;
		i, j, xPixel1, yPixel1, xPixel2, yPixel2: Integer;
	begin
		if (image1 = nil) or (image2 = nil) then
			RaiseSGSDKException('One or both of the spceified bitmaps are nil');
		
		if (w1 < 1) or (h1 < 1) or (w2 < 1) or (h2 < 1) then
			RaiseSGSDKException('Bitmap width and height must be greater then 0');
		
		result := false;

		left1 := x1;
		right1 := x1 + w1 - 1;
		top1 := y1;
		bottom1 := y1 + h1 - 1;

		left2 := x2;
		right2 := x2 + w2 - 1;
		top2 := y2;
		bottom2 := y2 + h2 - 1;

		if bottom1 > bottom2 then overBottom := bottom2
		else overBottom := bottom1;

		if top1 < top2 then overTop := top2
		else overTop := top1;

		if right1 > right2 then overRight := right2
		else overRight := right1;

		if left1 < left2 then overLeft := left2
		else overLeft := left1;

		for i := overTop to overBottom do
		begin
			yPixel1 := i - top1 + offsetY1;
			yPixel2 := i - top2 + offsetY2;

			for j := overLeft to overRight do
			begin
				xPixel1 := j - left1 + offsetX1;
				xPixel2 := j - left2 + offsetX2;

				if (bounded1 or IsPixelDrawnAtPoint(image1, xPixel1, yPixel1))
					 AND (bounded2 or IsPixelDrawnAtPoint(image2, xPixel2, yPixel2)) then
				begin
					result := true;
					exit;
				end;
			end;
		end;
	end;

	function CollisionWithinBitmapImages(
				image1: Bitmap; x1, y1: Integer; bounded1: Boolean;
				image2: Bitmap; x2, y2: Integer; bounded2: Boolean)
				: Boolean; overload;
	begin
		result := CollisionWithinBitmapImages(image1, x1, y1, image1.width, image1.height, 0, 0, bounded1, 
											  image2, x2, y2, image2.width, image2.height, 0, 0, bounded2);
	end;

	function CollisionWithinSpriteImages(sprite1, sprite2: Sprite): Boolean;
	var
		bmp1, bmp2: Bitmap;
		offX1, offY1, offX2, offY2: Integer;
	begin
		if (sprite1 = nil) or (sprite2 = nil) then
			RaiseSGSDKException('One of the sprites specified is nil');
		
		if sprite1.spriteKind = AnimMultiSprite then
		begin
			offX1 := (sprite1.currentFrame mod sprite1.cols) * sprite1.width;
			offY1 := (sprite1.currentFrame - (sprite1.currentFrame mod sprite1.cols)) div sprite1.cols * sprite1.height;
			bmp1 := sprite1.bitmaps[0];
		end
		else
		begin
			bmp1 := sprite1.bitmaps[sprite1.currentFrame];
			offX1 := 0;
			offY1 := 0;
		end;
		
		if sprite2.spriteKind = AnimMultiSprite then
		begin
			offX2 := (sprite2.currentFrame mod sprite2.cols) * sprite2.width;
			offY2 := (sprite2.currentFrame - (sprite2.currentFrame mod sprite2.cols)) div sprite2.cols * sprite2.height;
			bmp2 := sprite2.bitmaps[0];
		end
		else
		begin
			bmp2 := sprite2.bitmaps[sprite2.currentFrame];
			offX2 := 0;
			offY2 := 0;
		end;
		
		result := CollisionWithinBitmapImages(bmp1, Round(sprite1.xPos), Round(sprite1.yPos), CurrentWidth(sprite1), CurrentHeight(sprite1), offX1, offY1, not sprite1.usePixelCollision, 
											  bmp2, Round(sprite2.xPos), Round(sprite2.yPos), CurrentWidth(sprite2), CurrentHeight(sprite2), offX2, offY2, not sprite2.usePixelCollision);
	end;

	/// Checks to see if two bitmaps have collided, this performs a bounded check
	///	then, if required, it performs a per pixel check on the colliding region.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				The x,y location of image 1
	///	@param bounded1:			Indicates if image1 should use bounded collision
	///	@param x2, y2:				The x,y location of image 2
	///	@param bounded2:			Indicates if image2 should use bounded collision
	///
	///	@returns					True if the bitmaps collide.
	///
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
	                          image2 : Bitmap; x2, y2: Integer): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, x1, y1, false, image2, x2, y2, false);
	end;

	/// Checks to see if two bitmaps have collided, this performs a bounded check
	///	then, if required, it performs a per pixel check on the colliding region.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				The x,y location of image 1
	///	@param bounded1:			Indicates if image1 should use bounded collision
	///	@param x2, y2:				The x,y location of image 2
	///	@param bounded2:			Indicates if image2 should use bounded collision
	///
	///	@returns					True if the bitmaps collide.
	///
	function HaveBitmapsCollided(image1: Bitmap; x1,y1: Integer;bounded1: Boolean;
								 image2: Bitmap; x2, y2: Integer; bounded2: Boolean)
								 : Boolean; overload;
	begin
		if not HasBitmapCollidedWithRect(image1, x1, y1, x2, y2, 
	                                    image2.width, image2.height) then
		begin
			result := false;
			exit;
		end;

		result := CollisionWithinBitmapImages(image1, x1, y1, bounded1, 
											  image2, x2, y2, bounded2);
	end;

	/// Determines if two sprites have collided.
	///
	///	@param sprite1, sprite2:	 The two sprites to check.
	///
	///	@returns									 True if the sprites have collided.
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;
	begin
		if not HasSpriteCollidedWithRect(sprite1, sprite2.xPos, sprite2.yPos, 
	            CurrentWidth(sprite2), CurrentHeight(sprite2)) then
		begin
			result := false;
			exit;
		end;

		if sprite1.usePixelCollision or sprite2.usePixelCollision then
		begin
			result := CollisionWithinSpriteImages(sprite1, sprite2);
		end
		else
		begin
			result := true;
		end;
	end;

	/// Determines if a sprite has collided with a bitmap. The x and y values
	///	are expressed in "world" coordinates.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:				The x,y location of the bitmap
	///	@param bounded:				Indicates if bitmap should use bounded collision
	///
	///	@returns					True if the bitmap has collided with the sprite.
	///
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
										 x, y: Single): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, true);
	end;
	
	/// Determines if a sprite has collided with a bitmap using pixel level
	///	collision detection with the bitmap.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:					 The x,y location of the bitmap
	/// @param bounded				Indicates if theBitmap should use bounded collision
	///
	///	@returns							 True if the bitmap has collided with the sprite.
	///
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
										 x, y: Single; bounded: Boolean): Boolean; overload;
	var
		bmp: Bitmap;
		offX, offY: Integer;
	begin
		if theSprite.spriteKind = AnimMultiSprite then
		begin
			offX := (theSprite.currentFrame mod theSprite.cols) * theSprite.width;
			offY := (theSprite.currentFrame - (theSprite.currentFrame mod theSprite.cols)) div theSprite.cols * theSprite.height;
			bmp := theSprite.bitmaps[0];
		end
		else
		begin
			bmp := theSprite.bitmaps[theSprite.currentFrame];
			offX := 0;
			offY := 0;
		end;
		result := CollisionWithinBitmapImages(bmp, Round(theSprite.xPos), Round(theSprite.yPos),
											  theSprite.width, theSprite.height,
											  offX, offY,
											  not theSprite.usePixelCollision, theBitmap,
											  Round(x), Round(y), theBitmap.width, theBitmap.height, 0, 0, bounded);
	end;
	
	/// Multiply two matrix2d. Use this to combine the effects to two
	///  transformations.
	///
	/// @param m1, m2:   the two matrix to multiply
	/// @returns:        the result of multiplying these matrix
	function Multiply(const m1, m2: Matrix2D): Matrix2D; overload;
	begin
		  //unwound for performance optimisation
	  	result[0, 0] := m1[0, 0] * m2[0, 0] +
		  				m1[0, 1] * m2[1, 0] +
	  					m1[0, 2] * m2[2, 0];
		result[0, 1] := m1[0, 0] * m2[0, 1] +
						m1[0, 1] * m2[1, 1] +
						m1[0, 2] * m2[2, 1];
		result[0, 2] := m1[0, 0] * m2[0, 2] +
						m1[0, 1] * m2[1, 2] +
						m1[0, 2] * m2[2, 2];

		result[1, 0] := m1[1, 0] * m2[0, 0] +
						m1[1, 1] * m2[1, 0] +
						m1[1, 2] * m2[2, 0];
		result[1, 1] := m1[1, 0] * m2[0, 1] +
						m1[1, 1] * m2[1, 1] +
						m1[1, 2] * m2[2, 1];
		result[1, 2] := m1[1, 0] * m2[0, 2] +
						m1[1, 1] * m2[1, 2] +
						m1[1, 2] * m2[2, 2];

		result[2, 0] := m1[2, 0] * m2[0, 0] +
						m1[2, 1] * m2[1, 0] +
						m1[2, 2] * m2[2, 0];
		result[2, 1] := m1[2, 0] * m2[0, 1] +
						m1[2, 1] * m2[1, 1] +
						m1[2, 2] * m2[2, 1];
		result[2, 2] := m1[2, 0] * m2[0, 2] +
						m1[2, 1] * m2[1, 2] +
						m1[2, 2] * m2[2, 2];
	end;

	function Multiply(const m: Matrix2D; const v: Vector): Vector; overload;
	begin
		result.x := v.x * m[0, 0] + v.y * m[0,1] + v.w * m[0,2];
		result.y := v.x * m[1, 0] + v.y * m[1,1] + v.w * m[1,2];
		result.w := 1;
	end;

	function RotationMatrix(deg: Single): Matrix2D;
	var
		rads: Double;
	begin
		rads := deg * DEG_TO_RAD;
		
		result[0, 0] := System.Cos(rads);
		result[0, 1] := System.Sin(rads);
		result[0, 2] := 0;
		
		result[1, 0] := -System.Sin(rads);
		result[1, 1] := System.Cos(rads);
		result[1, 2] := 0;
		
		result[2, 0] := 0;
		result[2, 1] := 0;
		result[2, 2] := 1;
	end;

	function ScaleMatrix(scale: Single): Matrix2D;
	begin
		result[0, 0] := scale;
		result[0, 1] := 0;
		result[0, 2] := 0;

		result[1, 0] := 0;
		result[1, 1] := scale;
		result[1, 2] := 0;

		result[2, 0] := 0;
		result[2, 1] := 0;
		result[2, 2] := 1;
	end;

	function TranslationMatric(dx, dy: Single): Matrix2D;
	begin
		result := ScaleMatrix(1);

		result[0, 2] := dx;
		result[1, 2] := dy;
	end;

	function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
	var
		o, a, oa, rads: Single;
	begin
		if (x1 = x2) and (y2 < y1) then result := -90
		else if (x1 = x2) and (y2 >= y1) then result := 90
		else if (y1 = y2) and (x2 < x1) then result := 180
		else if (y1 = y2) and (x2 >= x1) then result := 0
		else
		begin
			o := (y2 - y1);
			a := (x2 - x1);
			oa := o / a;
			rads := arctan(oa);
			result := RadToDeg(rads);

			if x2 < x1 then
			begin
				if (y2 < y1) then result := result - 180
				else result := result + 180;
			end;
		end;
	end;

	function CalculateAngle(sprite1, sprite2: Sprite): Single; overload;
	var
		cx1, cy1, cx2, cy2: Single;
	begin
		cx1 := sprite1.XPos + CurrentWidth(sprite1) / 2;
		cy1 := sprite1.YPos + CurrentHeight(sprite1) / 2;
		cx2 := sprite2.XPos + CurrentWidth(sprite2) / 2;
		cy2 := sprite2.YPos + CurrentHeight(sprite2) / 2;
	
		result := CalculateAngle(cx1, cy1, cx2, cy2);
	end;
	
	procedure VectorCollision(p1, p2: Sprite);
	var
		colNormalAngle, a1, a2, optP: Single;
		n: Vector;
	begin

		if (p1.mass <= 0) or (p2.mass <= 0) then
		begin
			RaiseSGSDKException('Collision with 0 or negative mass... ensure that mass is greater than 0');
		end;
			
		colNormalAngle := CalculateAngle(p1, p2);
		//COLLISION RESPONSE
		// n = vector connecting the centers of the balls.
		// we are finding the components of the normalised vector n
		n := CreateVector(Cos(colNormalAngle), Sin(colNormalAngle));
		// now find the length of the components of each movement vectors
		// along n, by using dot product.
		a1 := DotProduct(p1.Movement, n);
		//Local a1# = c.dx*nX  +  c.dy*nY
		a2 := DotProduct(p2.Movement, n);
		//Local a2# = c2.dx*nX +  c2.dy*nY
		// optimisedP = 2(a1 - a2)
		// ----------
		// m1 + m2
		optP := (2.0 * (a1-a2)) / (p1.mass + p2.mass);
		// now find out the resultant vectors
		// Local r1% = c1.v - optimisedP * mass2 * n
		p1.movement.x := p1.movement.x - (optP * p2.mass * n.x);
		p1.movement.y := p1.movement.y - (optP * p2.mass * n.y);
		
		// Local r2% = c2.v - optimisedP * mass1 * n
		p2.movement.x := p2.movement.x + (optP * p1.mass * n.x);
		p2.movement.y := p2.movement.y + (optP * p1.mass * n.y);
	end;
	
	function CalculateVectorFromTo(obj, dest: Sprite): Vector;
	var
		destWdiv2, destHdiv2: Integer;
		objWdiv2, objHdiv2: Integer;
		v, pc, wc: Vector;
	begin
		objWdiv2 := CurrentWidth(obj) div 2;
		objHdiv2 := CurrentHeight(obj) div 2;
											
		destWdiv2 := CurrentWidth(dest) div 2;
		destHdiv2 := CurrentHeight(dest) div 2;
		
		pc := CreateVector(obj.xPos + objWdiv2, obj.yPos + objHdiv2);
		wc := CreateVector(dest.xPos + destWdiv2, dest.yPos + destHdiv2);
		v := SubtractVectors(wc, pc);
		
		{WriteLn('xy: ', x:4:2, ',', y:4:2);
		WriteLn('pc: ', pc.x:4:2, ',', pc.y:4:2, ' - ', objWdiv2, ',', objHdiv2);
		WriteLn('wc: ', wc.x:4:2, ',', wc.y:4:2, ' - ', destWdiv2, ',', destHdiv2);	
		WriteLn('v : ', v.x:4:2, ',', v.y:4:2);}
		
		result := v;		
	end;
	
	/// Create a vector from the angle and the magnitude.
	///
	/// @param angle:		Target angle
	/// @param magnitude:	Magnitude of the result vector
	function GetVectorFromAngle(angle, magnitude: Single): Vector;
	begin
		result := CreateVector(magnitude * SGSDK_Core.Cos(angle), magnitude * SGSDK_Core.Sin(angle));
	end;
	
	/// Does the vector end within the rectangle
	function VectorWithinRect(const v: Vector; x, y, w, h: Single): Boolean;
	begin
		if v.x < x then result := false
		else if v.x > x + w then result := false
		else if v.y < y then result := false
		else if v.y > y + h then result := false
		else result := true;
	end;
	
end.