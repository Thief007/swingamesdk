unit SGSDK_Physics;

interface
	uses
		SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing, SGSDK_Core, 
		SGSDK_Graphics, SGSDK_Camera, SGSDK_Shapes;

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
			
		CollisionSide = (
			Top,
			Bottom,
			Left,
			Right,
			TopLeft,
			TopRight,
			BottomLeft,
			BottomRight,
			LeftRight,
			TopBottom,
			None
		);

			
	//*****
	//
	// Collision detection routines
	//
	//*****
	//
	// These routines are used to detect collisions between sprites or bitmaps.
	//

	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;

	function HasSpriteCollidedX(theSprite: Sprite; x: Single; range: CollisionDetectionRange): Boolean;
	function HasSpriteCollidedY(theSprite: Sprite; y: Single; range: CollisionDetectionRange): Boolean;

	function HasSpriteCollidedWithRect(theSprite: Sprite; x, y: Single; width, height: Integer): Boolean; overload;
	function HasSpriteCollidedWithRect(theSprite: Sprite; const rect: Rectangle): Boolean; overload;

	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single; bounded: Boolean): Boolean; overload;
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; const pt: Point2D; bounded: Boolean): Boolean; overload;
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single): Boolean; overload;
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; const pt: Point2D; const src: Rectangle; bounded: Boolean): Boolean; overload;

	function HasBitmapCollidedWithRect(image: Bitmap; x, y: Integer; const rect: Rectangle): Boolean;
	function HasBitmapCollidedWithRect(image: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: Integer): Boolean;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2: Bitmap; x2, y2: Integer): Boolean; overload;
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; image2: Bitmap; const pt2: Point2D): Boolean; overload;
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; bounded1: Boolean; image2: Bitmap; x2, y2: Integer; bounded2: Boolean): Boolean; overload;
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; bounded1: Boolean; image2: Bitmap; const pt2: Point2D; bounded2: Boolean): Boolean; overload;
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; const src1: Rectangle; image2: Bitmap; const pt2: Point2D; const src2: Rectangle): Boolean; overload;
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; const src1: Rectangle; bounded1: Boolean; image2: Bitmap; const pt2: Point2D; const src2: Rectangle; bounded2: Boolean): Boolean; overload;

	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: Integer): Boolean; overload;
	function IsSpriteOnScreenAt(theSprite: Sprite; const pt: Point2D): Boolean; overload;
	function IsSpriteOnScreenAt(theSprite: Sprite; const v: Vector): Boolean; overload;

	function CircleHasCollidedWithLine(p1: Sprite; const line: LineSegment): Boolean;
	function RectangleHasCollidedWithLine(p1: Sprite; const line: LineSegment): Boolean; overload;
	function RectangleHasCollidedWithLine(const rect: Rectangle; const line: LineSegment): Boolean; overload;

	//*****
	//
	// Vector routines
	//
	//*****
	//
	// These routines are used to manipulate vectors in the API.
	//

	function CreateVector(x,y : Single): Vector; overload;
	function CreateVector(x,y : Single; invertY : boolean): Vector; overload;
	
	function VectorFromPoints(const p1, p2: Point2D): Vector;
	function VectorFromCenterSpriteToPoint(fromSprt: Sprite; const pnt: Point2D): Vector;
	
	function LineAsVector(const line: lineSegment): Vector;

	function GetVectorFromAngle(angle, magnitude: Single): Vector;

	function VectorFromPointToRectangle(x, y, rectX, rectY: Single; rectWidth, rectHeight: Integer) : Vector; overload;
	function VectorFromPointToRectangle(x, y: Single; const rect: Rectangle): Vector; overload;
	function VectorFromPointToRectangle(const pt: Point2D; const rect: Rectangle): Vector; overload;

	function VectorOutOfRectFromPoint(const pnt: Point2D; const rect: Rectangle; const movement: Vector): Vector;	
	function VectorOutOfRectFromRect(const srcRect, targetRect: Rectangle; const movement: Vector) : Vector;	
	
	function AddVectors(const v1, v2 : Vector): Vector;
	function SubtractVectors(const v1, v2 : Vector): Vector;
	function MultiplyVector(const v1: Vector; s1: Single): Vector;

	function DotProduct(const v1, v2: Vector): Single;

	function VectorNormal(const vect: Vector): Vector;
	function LineNormal(const line: LineSegment): Vector;

	function InvertVector(const v : Vector): Vector;

	function LimitMagnitude(const theVector: Vector; maxMagnitude: Single): Vector;

	function GetUnitVector(const theVector : Vector): Vector;
	function IsZeroVector(const theVector : Vector): Boolean;

	function Magnitude(const theVector : Vector): Single;

 	function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
	function CalculateAngle(sprite1, sprite2: Sprite): Single; overload;

	function CalculateAngleBetween(const v1, v2: Vector): Single; overload;

	function TranslationMatrix(dx, dy: Single): Matrix2D;
	function ScaleMatrix(scale: Single): Matrix2D;
	function RotationMatrix(deg: Single): Matrix2D;

	function Multiply(const m1, m2: Matrix2D): Matrix2D; overload;
	function Multiply(const m: Matrix2D; const v: Vector): Vector; overload;
	
	function CalculateVectorFromTo(obj, dest: Sprite): Vector;

	function VectorWithinRect(const v: Vector; x, y, w, h: Single): Boolean;
	
	function GetLineIntersectionPoint(const line1, line2: LineSegment; out pnt: Point2D) : boolean;
	function LineIntersectsWithLines(const target: LineSegment; const lines: LinesArray): boolean;
	function LineIntersectsWithRect(const line: LineSegment; const rect: Rectangle): boolean;
	
	function GetSideForCollisionTest(const movement: Vector): CollisionSide;
	
	procedure VectorCollision(p1, p2: Sprite);
	procedure CircleCollisionWithLine(p1: Sprite; const line: LineSegment);
	procedure CircularCollision(p1, p2: Sprite);

			
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
		result := CreateVector(x, y, false);
	end;

	function VectorFromPoints(const p1, p2: Point2D): Vector;
	begin
		result := CreateVector(p2.x - p1.x, p2.y - p1.y, false);
	end;
	
	function VectorFromCenterSpriteToPoint(fromSprt: Sprite; const pnt: Point2D): Vector;
	var
		cx, cy: Single;
	begin
		cx := fromSprt.x + CurrentWidth(fromSprt) / 2;
		cy := fromSprt.y + CurrentHeight(fromSprt) / 2;
		
		result := CreateVector(pnt.x - cx, pnt.y - cy, false);		
	end;

	/// Adds v1 and v2.
	///
	///	@param v1, v2			The vectors to work with
	///	@returns				v1 + v2
	function AddVectors(const v1, v2 : Vector): Vector;
	begin
		result.x := v1.x + v2.x;
		result.y := v1.y + v2.y;
	end;

	/// Subtracts v2 from v1 (i.e. v1 - v2).
	///
	///	@param v1, v2			The vectors to work with
	///	@returns				v1 - v2
	function SubtractVectors(const v1, v2 : Vector): Vector;
	begin
		result.x := v1.x - v2.x;
		result.y := v1.y - v2.y;
	end;

	/// Inverts the vector v. Changes the direction of the vector's x and y.
	///
	///	@param v		 The vector to invert
	///	@returns		 A new inverted vector
	function InvertVector(const v : Vector): Vector;
	begin
		result.x := v.x * -1;
		result.y := v.y * -1;
	end;

	/// Limit the magnitude of a vector.
	///
	///	@param theVector:		The vector to limit
	///	@param maxMagnitude:	The maximum magnitude of the vector.
	///	@returns				A new vector with the same direction as theVector,
	///							with a maximum magnitude of maxMagnitude
	function LimitMagnitude(const theVector: Vector; maxMagnitude: Single): Vector;
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
	function GetUnitVector(const theVector : Vector): Vector;
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
	function IsZeroVector(const theVector : Vector): Boolean;
	begin
		result := (theVector.x = 0) and (theVector.y = 0);
	end;

	/// Returns the magnitude of a vector. The magnitude represents the length of
	///	the vector.
	///
	///	@param theVector			The vector to get the magnitude of
	///	@returns					The magnitude of the vector
	function Magnitude(const theVector : Vector): Single;
	begin
		result := Sqrt((theVector.x * theVector.x) + (theVector.y * theVector.y));
	end;

	function DotProduct(const v1, v2: Vector): Single;
	begin
		result := (v1.x * v2.x) + (v1.y * v2.y);
	end;

	function MultiplyVector(const v1: Vector; s1: Single): Vector;
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
	function HasSpriteCollidedX(theSprite: Sprite; x: Single; range: CollisionDetectionRange): Boolean;
	begin
    	result := false;
    
		if range = CollisionRangeEquals then
			result := (x >= theSprite.x) and 
	        (x <= theSprite.x + theSprite.width)
		else if range = CollisionRangeGreaterThan then
			result := x <= theSprite.x + 
	                    theSprite.width
		else if range = CollisionRangeLessThan then
			result := x >= theSprite.x
		else
			raise Exception.Create('Invalid Collision Range');
	end;

	/// Determines if a sprite has collided with a given y position. The x and y
	///	values are in "world" coordinates.
	///
	///	@param theSprite:		The sprite to check
	///	@param y:				The y location to check collision with
	///	@param range:			The kind of check to perform less, larger or equal.
	///
	///	@returns				True if the sprite is within the range requested
	function HasSpriteCollidedY(theSprite: Sprite; y : Single; range : CollisionDetectionRange): Boolean;
	begin
    	result := false;
    
		if range = CollisionRangeEquals then
			result := (y >= theSprite.y) and 
	       (y <= theSprite.y + theSprite.height)
		else if range = CollisionRangeGreaterThan then
			result := y <= theSprite.y + 
	                    theSprite.height
		else if range = CollisionRangeLessThan then
			result := y >= theSprite.y
		else
			raise Exception.Create('Invalid Collision Range');
	end;

	function HasBitmapCollidedWithRect(image: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: Integer): Boolean; overload;
	begin
		result := RectanglesIntersect(CreateRectangle(x, y, image), CreateRectangle(rectX, rectY, rectWidth, rectHeight));
	end;

	function HasBitmapCollidedWithRect(image: Bitmap; x, y: Integer; const rect: Rectangle): Boolean; overload;
	begin
		result := RectanglesIntersect(CreateRectangle(x, y, image), rect);
	end;

	/// Determined if a sprite has collided with a given rectangle. The rectangles
	///	coordinates are expressed in "world" coordinates.
	///
	///	@param theSprite:			The sprite to check
	///	@param x, y :					The x,y location of the rectangle
	///	@param width, height:	The width and height of the rectangle
	///
	///	@returns							 True if the sprite collides with the rectangle
	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single; width, height : Integer): Boolean; overload;
	begin
		if theSprite = nil then raise Exception.Create('The specified sprite is nil');
		if (width < 1) or (height < 1) then 
			raise Exception.Create('Rectangle width and height must be greater then 0');
		if theSprite.y + CurrentHeight(theSprite) <= y then result := false
		else if theSprite.y >= y + height then result := false
		else if theSprite.x + CurrentWidth(theSprite) <= x then result := false
		else if theSprite.x >= x + width then result := false
		else result := true;
	end;

	function HasSpriteCollidedWithRect(theSprite: Sprite; const rect: Rectangle): Boolean; overload;
	begin
		result := HasSpriteCollidedWithRect(theSprite, rect.x, rect.y, rect.width, rect.height);
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: Integer): Boolean; overload;
	begin
		result := HasSpriteCollidedWithRect(theSprite, GameX(x), GameY(y), 1, 1);
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; const pt: Point2D): Boolean; overload;
	begin
		result := HasSpriteCollidedWithRect(theSprite, GameX(Round(pt.x)), GameY(Round(pt.y)), 1, 1);
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; const v: Vector): Boolean; overload;
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
			raise Exception.Create('One or both of the spceified bitmaps are nil');
		
		if (w1 < 1) or (h1 < 1) or (w2 < 1) or (h2 < 1) then
			raise Exception.Create('Bitmap width and height must be greater then 0');
		
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
	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer; image2: Bitmap; x2, y2: Integer): Boolean; overload;
	begin
		result := CollisionWithinBitmapImages(image1, x1, y1, false, image2, x2, y2, false);
	end;

	function CollisionWithinSpriteImages(sprite1, sprite2: Sprite): Boolean;
	var
		bmp1, bmp2: Bitmap;
		offX1, offY1, offX2, offY2: Integer;
	begin
		if (sprite1 = nil) or (sprite2 = nil) then
			raise Exception.Create('One of the sprites specified is nil');
		
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
		
		result := CollisionWithinBitmapImages(bmp1, Round(sprite1.x), Round(sprite1.y), CurrentWidth(sprite1), CurrentHeight(sprite1), offX1, offY1, not sprite1.usePixelCollision, 
											  bmp2, Round(sprite2.x), Round(sprite2.y), CurrentWidth(sprite2), CurrentHeight(sprite2), offX2, offY2, not sprite2.usePixelCollision);
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
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2 : Bitmap; x2, y2: Integer): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, x1, y1, false, image2, x2, y2, false);
	end;

	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; image2: Bitmap; const pt2: Point2D): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, Round(pt1.x), Round(pt1.y), false, image2, Round(pt2.x), Round(pt2.y), false);
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
	function HaveBitmapsCollided(image1: Bitmap; x1,y1: Integer;bounded1: Boolean; image2: Bitmap; x2, y2: Integer; bounded2: Boolean): Boolean; overload;
	begin
		if not HasBitmapCollidedWithRect(image1, x1, y1, x2, y2, image2.width, image2.height) then
		begin
			result := false;
			exit;
		end;

		result := CollisionWithinBitmapImages(image1, x1, y1, bounded1, image2, x2, y2, bounded2);
	end;
	
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; const src1: Rectangle; image2: Bitmap; const pt2: Point2D; const src2: Rectangle): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, pt1, src1, true, image2, pt2, src2, true);	
	end;
	
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; bounded1: Boolean; image2: Bitmap; const pt2: Point2D; bounded2: Boolean): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, Round(pt1.x), Round(pt1.y), bounded1, image2, Round(pt2.x), Round(pt2.y), bounded2);
	end;
	
	function HaveBitmapsCollided(image1: Bitmap; const pt1: Point2D; const src1: Rectangle; bounded1: Boolean; image2: Bitmap; const pt2: Point2D; const src2: Rectangle; bounded2: Boolean): Boolean; overload;
	begin
		if not HasBitmapCollidedWithRect(image1, Round(pt1.x), Round(pt1.y), Round(pt2.x), Round(pt2.y), src2.width, src2.height) then
		begin
			result := false;
			exit;
		end
		else if bounded1 and bounded2 then
		begin
			result := true;
			exit;
		end;

		result := CollisionWithinBitmapImages(image1, Round(pt1.x), Round(pt1.y), src1.width, src1.height, Round(src1.x), Round(src1.y), bounded1, 
											  image2, Round(pt2.x), Round(pt2.y), src2.width, src2.height, Round(src2.x), Round(src2.y), bounded2);
	end;

	/// Determines if two sprites have collided.
	///
	///	@param sprite1, sprite2:	 The two sprites to check.
	///
	///	@returns									 True if the sprites have collided.
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;
	begin
		if not HasSpriteCollidedWithRect(sprite1, sprite2.x, sprite2.y, sprite2.width, sprite2.height) then
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
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, true);
	end;

	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; const pt: Point2D): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, pt.x, pt.y, true);
	end;
	
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; const pt: Point2D; const src: Rectangle; bounded: Boolean): Boolean; overload;
	var
		bmp: Bitmap;
		offX, offY: Integer;
	begin
		if not HasSpriteCollidedWithRect(theSprite, pt.x, pt.y, src.width, src.height) then
		begin
			result := false;
			exit;
		end
		else if bounded then 
		begin
			result := true;
			exit;
		end;

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
		
		result := CollisionWithinBitmapImages(bmp, Round(theSprite.x), Round(theSprite.y),
											  theSprite.width, theSprite.height,
											  offX, offY,
											  not theSprite.usePixelCollision, theBitmap,
											  Round(pt.x), Round(pt.y), src.width, src.height, Round(src.x), Round(src.y), bounded);
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
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single; bounded: Boolean): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, CreatePoint(x, y), CreateRectangle(theBitmap), bounded);
	end;

	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; const pt: Point2D; bounded: Boolean): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, pt, CreateRectangle(theBitmap), bounded);
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
		rads := -deg * DEG_TO_RAD;
		
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

	function TranslationMatrix(dx, dy: Single): Matrix2D;
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
		cx1 := sprite1.x + CurrentWidth(sprite1) / 2;
		cy1 := sprite1.y + CurrentHeight(sprite1) / 2;
		cx2 := sprite2.x + CurrentWidth(sprite2) / 2;
		cy2 := sprite2.y + CurrentHeight(sprite2) / 2;
	
		result := CalculateAngle(cx1, cy1, cx2, cy2);
	end;
	
	function CalculateAngleBetween(const v1, v2: Vector): Single; overload;
	var
		t1, t2: Single;
	begin
		t1 := CalculateAngle(0, 0, v1.x, v1.y);
		t2 := CalculateAngle(0, 0, v2.x, v2.y);
		
		result := t2 - t1;
		
		if result > 180 then result := result - 360
		else if result <= -180 then result := result + 360
	end;
	
	
	procedure VectorCollision(p1, p2: Sprite);
	begin
		CircularCollision(p1, p2);
	end;

	procedure CircularCollision(p1, p2: Sprite);
	var
		colNormalAngle, a1, a2, optP: Single;
		n: Vector;
	begin

		if (p1.mass <= 0) or (p2.mass <= 0) then
		begin
			raise Exception.Create('Collision with 0 or negative mass... ensure that mass is greater than 0');
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
	
	function CircleHasCollidedWithLine(p1: Sprite; const line: LineSegment): Boolean;
	var
		r: Single;
		dist: Single;
	begin
		if CurrentWidth(p1) > CurrentHeight(p1) then
			r := CurrentWidth(p1) div 2
		else
			r := CurrentHeight(p1) div 2;
			
		dist := DistancePointToLine(p1.x + r, p1.y + r, line);
		result := dist < r;
	end;
	
	function RectangleHasCollidedWithLine(const rect: Rectangle; const line: LineSegment): Boolean; overload;
	begin
		result := LineIntersectsWithLines(line, LinesFromRect(rect));
	end;
		
	function RectangleHasCollidedWithLine(p1: Sprite; const line: LineSegment): Boolean; overload;
	begin
		result := RectangleHasCollidedWithLine(CreateRectangle(p1), line);
	end;
	
	procedure CircleCollisionWithLine(p1: Sprite; const line: LineSegment);
	var
		npx, npy, dotPod: Single;
		toLine: Vector;
		intersect: Point2D;
	begin
		intersect := ClosestPointOnLine(CenterPoint(p1), line);
		
		toLine := GetUnitVector(VectorFromCenterSpriteToPoint(p1, intersect));
		
		dotPod := - DotProduct(toLine, p1.movement);
		
		npx := dotPod * toLine.x;
		npy := dotPod * toLine.y;
		
		p1.movement.x := p1.movement.x + 2 * npx;
		p1.movement.y := p1.movement.y + 2 * npy;
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
		
		pc := CreateVector(obj.x + objWdiv2, obj.y + objHdiv2);
		wc := CreateVector(dest.x + destWdiv2, dest.y + destHdiv2);
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
	
	//You need to test for collisions on the ...
	function GetSideForCollisionTest (const movement: Vector): CollisionSide;
	begin
		if movement.x < 0 then //Going Left...
		begin
			if movement.y < 0 then result := BottomRight
			else if movement.y > 0 then result := TopRight
			else result := Right;
		end
		else if movement.x > 0 then //Going Right
		begin
			if movement.y < 0 then result := BottomLeft
			else if movement.y > 0 then result := TopLeft
			else result := Left;			
		end
		else // Going Up or Down
		begin
			if movement.y < 0 then result := Bottom
			else if movement.y > 0 then result := Top
			else result := None;
		end;
	end;
	
	function VectorFromPointToRectangle(x, y, rectX, rectY: Single; rectWidth, rectHeight: Integer) : Vector; overload;
	var
		px, py: Single;
	begin
		if x < rectX then px := rectX
		else if x > (rectX + rectWidth) then px := rectX + rectWidth
		else px := x;
			
		if y < rectY then py := rectY
		else if y > (rectY + rectHeight) then py := rectY + rectHeight
		else py := y;
			
		result := CreateVector(px - x, py - y);
	end;

	function VectorFromPointToRectangle(x, y: Single; const rect: Rectangle) : Vector; overload;
	begin
		result := VectorFromPointToRectangle(x, y, rect.x, rect.y, rect.width, rect.height);
	end;
	
	function VectorFromPointToRectangle(const pt: Point2D; const rect: Rectangle) : Vector; overload;
	begin
		result := VectorFromPointToRectangle(pt.x, pt.y, rect.x, rect.y, rect.width, rect.height);
	end;
	
	function VectorOut(const pnt: Point2D; const rect: Rectangle; const movement: Vector; rectCollisionSide: CollisionSide) : Vector;	
		function GetVectorDiagonal(edgeX, edgeY: Single): Vector;
		var
			toEdge: Vector;
			angle: Single;
			mvOut: Vector;
			xMag, yMag, outMag: Single;
		begin
			mvOut := GetUnitVector(InvertVector(movement));
			
			//Do X
			toEdge := CreateVector(edgeX - pnt.x, 0);
			angle := CalculateAngleBetween(mvOut, toEdge);			
			xMag := Magnitude(toEdge) * 1 / SGSDK_Core.Cos(angle);

			//Do Y
			toEdge := CreateVector(0, edgeY - pnt.y);
			angle := CalculateAngleBetween(mvOut, toEdge);			
			yMag := Magnitude(toEdge) * 1 / SGSDK_Core.Cos(angle);
					
			if (yMag < 0) or (xMag < yMag) then outMag := xMag
			else outMag := yMag;
			
			if outMag < 0 then outMag := 0;
			
			result := MultiplyVector(mvOut, outMag + 1);
		end;		
	begin		
		case rectCollisionSide of
			TopLeft: begin
					if (pnt.x < rect.x) or (pnt.y < rect.y) then result := CreateVector(0,0)
					else result := GetVectorDiagonal(rect.x, rect.y);
				end;
			TopRight: begin
					if (pnt.x > rect.x + rect.width) or (pnt.y < rect.y) then result := CreateVector(0,0)
					else result := GetVectorDiagonal(rect.x + rect.width, rect.y);
				end;
			BottomLeft:	begin
					if (pnt.x < rect.x) or (pnt.y > rect.y + rect.height) then result := CreateVector(0,0)
					else result := GetVectorDiagonal(rect.x, rect.y + rect.height);
				end;
			BottomRight: begin
					if (pnt.x > rect.x + rect.width) or (pnt.y > rect.y + rect.height) then result := CreateVector(0,0)
					else result := GetVectorDiagonal(rect.x + rect.width, rect.y + rect.height);
				end;
			Left: begin
					if (pnt.x < rect.x) or (pnt.y < rect.y) or (pnt.y > rect.y + rect.height) then result := CreateVector(0, 0)
					else result := CreateVector(rect.x - pnt.x - 1, 0);
					exit;
				end;
			Right: begin
					if (pnt.x > rect.x + rect.width) or (pnt.y < rect.y) or (pnt.y > rect.y + rect.height) then result := CreateVector(0, 0)
					else result := CreateVector(rect.x + rect.width - pnt.x + 1, 0);
					exit;
				end;
			Top: begin
					if (pnt.y < rect.y) or (pnt.x < rect.x) or (pnt.x > rect.x + rect.width) then result := CreateVector(0, 0)
					else result := CreateVector(0, rect.y - pnt.y - 1);
					exit;
				end;
			Bottom: begin
					if (pnt.y > rect.y + rect.height) or (pnt.x < rect.x) or (pnt.x > rect.x + rect.width) then result := CreateVector(0, 0)
					else result := CreateVector(0, rect.y + rect.height + 1 - pnt.y);
					exit;
				end;
			else //Not moving... i.e. None
				begin
					result := CreateVector(0, 0);
				end;
		end;
		
		//WriteLn('VectorOut: ', result.x:4:2, ',', result.y:4:2); //, '  angle: ', angle:4:2, ' mag: ', outMag:4:2, ' xmag: ', xMag:4:2, ' ymag: ', yMag:4:2);		
	end;

	//Vector needed to move from x, y out of rectangle assuming movement specified
	function VectorOutOfRectFromPoint(const pnt: Point2D; const rect: Rectangle; const movement: Vector): Vector;	
	begin
		result := VectorOut(pnt, rect, movement, GetSideForCollisionTest(movement));
		
		if (result.x = 0) and (result.y = 0) then exit;	

//		if not LineIntersectsWithRect(x, y, x + result.x, y + result.y, rectX, rectY, rectWidth, rectHeight) then
		if not LineIntersectsWithRect(LineFromVector(pnt, movement), rect) then
			result := CreateVector(0, 0);
	end;

	function VectorOutOfRectFromRect(const srcRect, targetRect: Rectangle; const movement: Vector) : Vector;	
	var
		rectCollisionSide: CollisionSide;
		p: Point2D; // p is the most distant point from the collision on srcRect
		destRect: Rectangle;
	begin
		//Which side of the rectangle did we collide with.
		rectCollisionSide := GetSideForCollisionTest(movement);
		
		p.x := srcRect.x; p.y := srcRect.y;
		
		case rectCollisionSide of
			//Hit top or left of wall... bottom right in
			TopLeft: 		begin p.x := p.x + srcRect.width; p.y := p.y + srcRect.height;  end;
			//Hit top or right of wall... bottom left in
			TopRight:		p.y := p.y + srcRect.height;
			//Hit bottom or left of wall... top right in
			BottomLeft:		p.x := p.x + srcRect.width;
			//Hit bottom or right of wall... top left is in
			BottomRight: 	;
			//Hit left of wall... right in
			Left: 			p.x := p.x + srcRect.width;
			Right: 			;                                   
			//Hit top of wall... bottom in
			Top: 			p.y := p.y + srcRect.height;
			Bottom: 		;
			None: begin 
					result := CreateVector(0, 0);
					exit; 
				end;
		end;
		
		result := VectorOut(p, targetRect, movement, rectCollisionSide);
		
		if (result.x = 0) and (result.y = 0) then exit;	

	 	destRect := RectangleAfterMove(srcRect, result);
		
		//Check diagonal miss...
		case rectCollisionSide of
			//Hit top left, check bottom and right;
			TopLeft: if (RectangleTop(destRect) > RectangleBottom(targetRect)) 	or 
							(RectangleLeft(destRect) > RectangleRight(targetRect)) 	then result := CreateVector(0,0);
			//Hit top right, check bottom and left
			TopRight: if 	(RectangleTop(destRect) > RectangleBottom(targetRect))	or 
								(RectangleRight(destRect) < RectangleLeft(targetRect))	then result := CreateVector(0,0);
			//Hit bottom left, check top and right
			BottomLeft: if (RectangleBottom(destRect) < RectangleTop(targetRect)) 	or 
								(RectangleLeft(destRect) > RectangleRight(targetRect)) 	then result := CreateVector(0,0);
			//Hit bottom right, check top and left
			BottomRight: if 	(RectangleBottom(destRect) < RectangleTop(targetRect)) 	or 
									(RectangleRight(destRect) < RectangleLeft(targetRect))	then result := CreateVector(0,0);
		end		
	end;

	function GetLineIntersectionPoint(const line1, line2: LineSegment; out pnt: Point2D) : boolean;
	var
		// convert lines to the eqn
		// c = ax + by
		a1, b1, c1: Single;
		a2, b2, c2: Single;
		det: Single;
	begin
		pnt.x := 0; pnt.y := 0;
		
		//Convert lines to eqn c = ax + by
		a1 := line1.endPoint.y - line1.startPoint.y; //y12 - y11;
		b1 := line1.startPoint.x - line1.endPoint.x; //x11 - x12;
		c1 := a1 * line1.startPoint.x + b1 * line1.startPoint.y; //a1 * x11 + b1 * y11;

		a2 := line2.endPoint.y - line2.startPoint.y; //y22 - y21;
		b2 := line2.startPoint.x - line2.endPoint.x; //x21 - x22;
		c2 := a2 * line2.startPoint.x + b2 * line2.startPoint.y; //a2 * x21 + b2 * y21;
		
		det := (a1 * b2) - (a2 * b1);
		
		if det = 0 then result := false
		else
		begin
			pnt.x := (b2*c1 - b1*c2) / det;
	        pnt.y := (a1*c2 - a2*c1) / det;
			result := true;
		end;		
	end;

	function LineIntersectsWithLines(const target: LineSegment; const lines: LinesArray): boolean;
	var
		i: Integer;
		pnt: Point2D;
	begin
		for i := 0 to High(lines) do
		begin			
			if GetLineIntersectionPoint(target, lines[i], pnt) and IsPointOnLine(pnt, lines[i]) then
			begin
				result := true;
				exit;
			end;
		end;
		
		result := false;
	end;
	
	function LineIntersectsWithRect(const line: LineSegment; const rect: Rectangle): boolean;
	var
		lines: LinesArray;
	begin
		lines := LinesFromRect(rect);
		result := LineIntersectsWithLines(line, lines);
	end;  	
	
	function LineAsVector(const line: lineSegment): Vector;
	begin
		result.x := line.endPoint.x - line.startPoint.x;
		result.y := line.endPoint.y - line.startPoint.y;
	end;
	
	function VectorNormal(const vect: Vector): Vector;
	var		
		sqrY, sqrX: Single;
	begin
		sqrX := vect.x * vect.x;
		sqrY := vect.y * vect.y;
		
	   result.x := -vect.y / Sqrt(sqrY + sqrX);  // -S2y / ::sqrt(S2y*S2y + S2x*S2x);
	   result.y := vect.x / Sqrt(sqrY + sqrX); // S2x / ::sqrt(S2y*S2y + S2x*S2x);
	end;
	
	function LineNormal(const line: LineSegment): Vector;
	begin
		result := VectorNormal(LineAsVector(line));
	end;
end.

