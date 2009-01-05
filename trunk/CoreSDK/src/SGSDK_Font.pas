///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SGSDK_Font.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Font unit relates to writing text to the screen,
// and to loading and styling the associated fonts.
//
// Change History:
//
// Version 2.0:
// - 2009-01-05: Andrew: Added Unicode rendering
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-12: Andrew: Added simple string printing
// - 2008-12-10: Andrew: Fixed printing of string
//
// Version 1.1:
// - 2008-03-09: Andrew: Added extra exception handling
// - 2008-01-30: Andrew: Fixed Print strings for EOL as last char
// - 2008-01-25: Andrew: Fixed compiler hints
// - 2008-01-21: Andrew: Added Point/Rectangle overloads
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

unit SGSDK_Font;

interface
	uses
		SDL, SDL_TTF, SGSDK_Core, SGSDK_Shapes;
	
	type
		/// Type: Font
		///
		/// Fonts are used to render text to bitmaps and to the screen.
		/// Fonts must be loaded using the CreateFont routine. Also see the
		///	DrawText and DrawTextLines routines.
		Font = PTTF_Font;

		 /// Enumeration: FontStyles
		///
		/// Use font styles to set the style of a font. Setting the style is time
		///	consuming, so create alternative font variables for each different
		///	style you want to work with. Note that these values can be logical
		///	ORed together to combine styles, e.g. BoldFont or ItalicFont = both
		///	bold and italic.
		FontStyle = (
				NormalFont		= TTF_STYLE_NORMAL,
				BoldFont		= TTF_STYLE_BOLD,
				ItalicFont		= TTF_STYLE_ITALIC,
				UnderlineFont 	= TTF_STYLE_UNDERLINE
			);

		/// Enumeration: FontAlignment
		///
		///	Use font alignment for certain drawing operations. With these
		///	operations you specify the area to draw in as well as the alignment
		///	within that area. See DrawTextLines.
		FontAlignment = (
				AlignLeft 	=	1,
				AlignCenter =	2,
				AlignRight 	=	4
			);

	//*****
	//
	// Resource loading and freeing routines
	//
	//*****
	//
	// These routines are used to load resources, and to free them.
	//

	function LoadFont(fontName: String; size: LongInt): Font;
	procedure SetFontStyle(font: Font; style: FontStyle);
	procedure FreeFont(var fontToFree: Font);

	//*****
	//
	// Screen drawing routines
	//
	//*****
	//
	// These routines are used to draw directly to the screen.
	//

	procedure DrawTextOnScreen(theText: String; textColor: Colour; theFont: Font; x, y: LongInt); overload;
	procedure DrawTextOnScreen(theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload; {1.1}
  procedure DrawText(theText: String; textColor: Colour; theFont: Font; x, y: Single); overload;
  procedure DrawText(theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload; {1.1}

	procedure DrawUnicodeOnScreen(theText: WideString; textColor: Colour; theFont: Font; x, y: LongInt); overload;
	procedure DrawUnicodeOnScreen(theText: WideString; textColor: Colour; theFont: Font; const pt: Point2D); overload; {1.1}
  procedure DrawUnicode(theText: WideString; textColor: Colour; theFont: Font; x, y: Single); overload;
  procedure DrawUnicode(theText: WideString; textColor: Colour; theFont: Font; const pt: Point2D); overload; {1.1}

  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y, w, h: LongInt); overload;
	procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload; {1.1}
  procedure DrawTextLines(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y: Single; w, h: LongInt); overload;
  procedure DrawTextLines(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload; {1.1}


  
	//*****
	//
	// Simple text drawing routines
	//
	//*****
	//
	// These routines are used to draw text without a TTF.
	//
  procedure DrawText(theText: String; textColor: Color; x, y: Single); overload;
  procedure DrawTextOnScreen(theText: String; textColor: Color; x, y: Single); overload;
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x, y: Single); overload;
    
	//*****
	//
	// Bitmap drawing routines
	//
	//*****
	//
	// These routines are used to draw to a bitmap.
	//

	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour; theFont: Font; x, y: LongInt); overload;
	procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y, w, h: LongInt); overload;
	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
		
	//*****
	//
	// General routines
	//
	//*****
	//
	// These routines are used for general purposes.
	//

	function TextWidth(theText: String; theFont: Font): LongInt; overload;
	function TextHeight(theText: String; theFont: Font): LongInt; overload;

	function TextWidth(theText: WideString; theFont: Font): LongInt; overload;
	function TextHeight(theText: WideString; theFont: Font): LongInt; overload;
	
	procedure DrawFramerate(x, y: LongInt; font: Font);
	
implementation
	uses SysUtils, Classes, SGSDK_Graphics, SGSDK_Camera, SDL_gfx;
	
	/// Loads a font from file with the specified side. Fonts must be freed using
	///	the FreeFont routine once finished with. Once the font is loaded you
	///	can set its style using SetFontStyle. Fonts are then used to draw and
	///	measure text in your programs.
	///
	///	@param fontName: The name of the font file to load from the file system
	///	@param size:		 The point size of the font
	///	@returns:				The font loaded
	function LoadFont(fontName: String; size: LongInt): Font;
	begin
		result := TTF_OpenFont(PChar(fontName), size);

		if result = nil then
		begin
			raise Exception.Create('LoadFont failed: ' + TTF_GetError());
		end;
	end;

	/// Sets the style of the passed in font. This is time consuming, so load
	///	fonts multiple times and set the style for each if needed.
	///
	///	@param font:	 The font to set the style of
	///	@param style:	 The new style for the font, values can be read together
	///
	/// Side Effects:
	///	- The font's style is changed
	procedure SetFontStyle(font: Font; style: FontStyle);
	begin
	  if not Assigned(font) then raise Exception.Create('No font supplied');
		TTF_SetFontStyle(font, LongInt(style));
	end;

	/// Free a loaded font.
	///
	/// Side Effects:
	///	- The font is freed and cannot be used any more
	procedure FreeFont(var fontToFree: Font);
	begin
		try
			TTF_CloseFont(fontToFree);
			fontToFree := nil;
		except
			raise Exception.Create('Unable to free the specified font');
		end;
	end;

	function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
	begin
		result := (LongInt(toCheck) and LongInt(checkFor)) = LongInt(checkFor);
	end;
	
	/// This function prints "str" with font "font" and color "clrFg"
 	///  * onto a rectangle of color "clrBg".
 	///  * It does not pad the text.
	procedure PrintStrings(dest: PSDL_Surface; font: Font; str: String; rc: PSDL_Rect; clrFg, clrBg:Color; flags:FontAlignment);
	var
		sText: Bitmap;
		temp: PSDL_Surface;
		lineSkip, width, height: LongInt;
		lines: Array of String;
		subStr: String;
		n, w, h, i: LongInt;
		rect: TSDL_Rect;
		colorFG: TSDL_Color;
		bgTransparent: Boolean;
	begin
		if dest = nil then
		begin
			raise Exception.Create('Error Printing Strings: There was no surface.');
		end;

		colorFG := ToSDLColor(clrFg);
		bgTransparent := GetTransparency(clrBg) < 255;
		
		// If there's nothing to draw, return NULL
		if (Length(str) = 0) or (font = nil) then exit;

		// This is the surface that everything is printed to.
		lineSkip	:= TTF_FontLineSkip( font );
		width		 := rc.w;
		height		:= 10;
		SetLength(lines, 1);

		// Break the String into its lines:
		n := -1; i := 0;
		while n <> 0 do
		begin
			// Get until either "\n" or "\0":
			n := Pos(eol, str);

			//Copy all except EOL
			if n = 0 then subStr := str
			else if n = 1 then subStr := ' '
			else subStr := Copy(str, 1, n - 1);

			if Length(subStr) < 1 then subStr := ' ';

			//Remove the line from the original string
			if n <> 0 then
			begin
				str := Copy( str, n + Length(eol), Length(str)); //excess not copied...
			end;

			i := i + 1;
			SetLength(lines, i);
			lines[i - 1] := subStr;

			w := 0;
			// Get the size of the rendered text.
			if Length(subStr) > 0 then TTF_SizeText(font, PChar(subStr), w, height);
			
			if w > width then width := w;
		end;

		// Length(lines) = Number of Lines.
		// we assume that height is the same for all lines.
		height := (Length(lines) - 1) * lineSkip + height;

		sText := CreateBitmap(width, height);
		ClearSurface(sText, clrBg);

		// Actually render the text:
		for i := 0 to High(lines) do
		begin
			// The rendered text:
			temp := TTF_RenderText_Blended(font, PChar(lines[i]), colorFG);
      //temp := TTF_RenderUNICODE_Blended(font, PUint16(lines[i]), colorFG);
      
			// Put it on the surface:
			if IsSet(flags, AlignLeft) or
				 (not (IsSet(flags, AlignCenter) or
							 IsSet(flags, AlignRight))) then
			begin
				// If it's specifically LEFT or none of the others:
				rect := NewSDLRect(0,i*lineSkip,0,0);
			end
			else if IsSet(flags, AlignCenter) then
			begin
				w := 0;
				h := 0;

				TTF_SizeText(font, PChar(lines[i]), w, h);
				rect := NewSDLRect(width div 2 - w div 2, i * lineSkip, 0, 0)
			end
			else if IsSet(flags, AlignRight) then
			begin
				w := 0;
				h := 0;

				TTF_SizeText(font, PChar(lines[i]), w, h);
				rect := NewSDLRect(width - w, i * lineSkip, 0, 0);
			end
			else
			begin
				raise Exception.Create('Invalid font alignment');
			end;

			// Render the current line. Ignore alpha in this draw
			if bgTransparent then SDL_SetAlpha(temp, 0, SDL_ALPHA_TRANSPARENT);
			SDL_BlitSurface(temp, nil, sText.surface, @rect);

			// Clean up:
			SDL_FreeSurface(temp);
		end;

		// Draw the text on top of that:
		rect.x := 0; rect.y := 0; rect.w := rc.w; rect.h := rc.h;
		if (not bgTransparent) then SDL_SetAlpha(sText.surface, 0, SDL_ALPHA_TRANSPARENT);	
		SDL_BlitSurface(sText.surface, @rect, dest, rc );
		
		FreeBitmap(sText);
	end;

	/// This function prints "str" with font "font" and color "clrFg"
 	///  * onto a rectangle of color "clrBg".
 	///  * It does not pad the text.
	procedure PrintWideStrings(dest: PSDL_Surface; font: Font; str: WideString; rc: PSDL_Rect; clrFg, clrBg:Color; flags:FontAlignment);
	var
		sText: Bitmap;
		temp: PSDL_Surface;
		lineSkip, width, height: LongInt;
		lines: Array of String;
		subStr: String;
		n, w, h, i: LongInt;
		rect: TSDL_Rect;
		colorFG: TSDL_Color;
		bgTransparent: Boolean;
	begin
		if dest = nil then
		begin
			raise Exception.Create('Error Printing Strings: There was no surface.');
		end;

		colorFG := ToSDLColor(clrFg);
		bgTransparent := GetTransparency(clrBg) < 255;
		
		// If there's nothing to draw, return NULL
		if (Length(str) = 0) or (font = nil) then exit;

		// This is the surface that everything is printed to.
		lineSkip	:= TTF_FontLineSkip( font );
		width		 := rc.w;
		height		:= 10;
		SetLength(lines, 1);

		// Break the String into its lines:
		n := -1; i := 0;
		while n <> 0 do
		begin
			// Get until either "\n" or "\0":
			n := Pos(eol, str);

			//Copy all except EOL
			if n = 0 then subStr := str
			else if n = 1 then subStr := ' '
			else subStr := Copy(str, 1, n - 1);

			if Length(subStr) < 1 then subStr := ' ';

			//Remove the line from the original string
			if n <> 0 then
			begin
				str := Copy( str, n + Length(eol), Length(str)); //excess not copied...
			end;

			i := i + 1;
			SetLength(lines, i);
			lines[i - 1] := subStr;

			w := 0;
			
			// Get the size of the rendered text.
			if Length(subStr) > 0 then TTF_SizeUNICODE(font, PUint16(subStr), w, height);
			  
			//Keep widest rendered text size
			if w > width then width := w;
		end;

		// Length(lines) = Number of Lines.
		// we assume that height is the same for all lines.
		height := (Length(lines) - 1) * lineSkip + height;

		sText := CreateBitmap(width, height);
		ClearSurface(sText, clrBg);

		// Actually render the text:
		for i := 0 to High(lines) do
		begin
			// The rendered text:
			//temp := TTF_RenderText_Blended(font, PChar(lines[i]), colorFG);
      temp := TTF_RenderUNICODE_Blended(font, PUint16(lines[i]), colorFG);
      
			// Put it on the surface:
			if IsSet(flags, AlignLeft) or
				 (not (IsSet(flags, AlignCenter) or
							 IsSet(flags, AlignRight))) then
			begin
				// If it's specifically LEFT or none of the others:
				rect := NewSDLRect(0,i*lineSkip,0,0);
			end
			else if IsSet(flags, AlignCenter) then
			begin
				w := 0;
				h := 0;

				TTF_SizeUNICODE(font, PUInt16(lines[i]), w, h);
				rect := NewSDLRect(width div 2 - w div 2, i * lineSkip, 0, 0)
			end
			else if IsSet(flags, AlignRight) then
			begin
				w := 0;
				h := 0;

				TTF_SizeUNICODE(font, PUInt16(lines[i]), w, h);
				rect := NewSDLRect(width - w, i * lineSkip, 0, 0);
			end
			else
			begin
				raise Exception.Create('Invalid font alignment');
			end;

			// Render the current line. Ignore alpha in this draw
			if bgTransparent then SDL_SetAlpha(temp, 0, SDL_ALPHA_TRANSPARENT);
			SDL_BlitSurface(temp, nil, sText.surface, @rect);

			// Clean up:
			SDL_FreeSurface(temp);
		end;

		// Draw the text on top of that:
		rect.x := 0; rect.y := 0; rect.w := rc.w; rect.h := rc.h;
		if (not bgTransparent) then SDL_SetAlpha(sText.surface, 0, SDL_ALPHA_TRANSPARENT);	
		SDL_BlitSurface(sText.surface, @rect, dest, rc );
		
		FreeBitmap(sText);
	end;

	/// Draws texts to the destination bitmap. Drawing text is a slow operation,
	///	and drawing it to a bitmap, then drawing the bitmap to screen is a
	///	good idea. Do not use this technique if the text changes frequently.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param theFont:			The font used to draw the text
	///	@param x,y:					The x,y location to draw the text at (top left)
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		in the destination bitmap.
	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour; theFont: Font; x, y: LongInt); overload;
	var
		rect: TSDL_Rect;
	begin
		if theFont = nil then raise Exception.Create('The specified font is nil');
		if dest = nil then raise Exception.Create('Cannot draw text, as no destination was supplied');
			
		rect := NewSDLRect(x, y, TextWidth(theText, theFont), TextHeight(theText, theFont));		
		PrintStrings(dest.surface, theFont, theText, @rect, textColor, ColorTransparent, AlignLeft);
	end;

	procedure DrawUnicode(dest: Bitmap; theText: WideString; textColor: Colour; theFont: Font; x, y: LongInt); overload;
	var
		rect: TSDL_Rect;
	begin
		if theFont = nil then raise Exception.Create('The specified font is nil');
		if dest = nil then raise Exception.Create('Cannot draw text, as no destination was supplied');
			
		rect := NewSDLRect(x, y, TextWidth(theText, theFont), TextHeight(theText, theFont));		
		PrintWideStrings(dest.surface, theFont, theText, @rect, textColor, ColorTransparent, AlignLeft);
	end;
	
	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawText(dest, theText, textColor, theFont, Round(pt.x), Round(pt.y));
	end;

	procedure DrawUnicode(dest: Bitmap; theText: WideString; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawUnicode(dest, theText, textColor, theFont, Round(pt.x), Round(pt.y));
	end;


	/// Draws multiple lines of text to the destination bitmap. This is a very
	///	slow operation, so if the text is not frequently changing save it to a
	///	bitmap and draw that bitmap to screen instead.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param backColor:		The color to draw behind the text
	///	@param theFont:			The font used to draw the text
	///	@param align:				The alignment for the text in the region
	///	@param x,y:					The x,y location to draw the text at (top left)
	///	@param w, h:				 The width and height of the region to draw inside
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		in the destination bitmap.
	procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y, w, h: LongInt); overload;
	var
		rect: TSDL_Rect;
	begin
		rect := NewSDLRect(x, y, w, h);
		PrintStrings(dest.surface, theFont, theText, @rect, textColor, backColor, align);
	end;

	procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
	begin
		DrawTextLines(dest, theText, textColor, backColor, theFont, align, Round(withinRect.x), Round(withinRect.y), withinRect.width, withinRect.height);
	end;


	/// Draws multiple lines of text to the screen. This is a very
	///	slow operation, so if the text is not frequently changing save it to a
	///	bitmap and draw that bitmap to screen instead.
	///
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param backColor:		The color to draw behind the text
	///	@param theFont:			The font used to draw the text
	///	@param align:				The alignment for the text in the region
	///	@param x,y:					The x,y location to draw the text at (top left)
	///	@param w, h:				 The width and height of the region to draw inside
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		on the screen.
	procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y, w, h: LongInt); overload;
	begin
		DrawTextLines(scr, theText, textColor, backColor, theFont, align, x, y, w, h);
	end;
	
	procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
	begin
		DrawTextLines(scr, theText, textColor, backColor, theFont, align, Round(withinRect.x), Round(withinRect.y), withinRect.width, withinRect.height);
	end;

	procedure DrawTextLines(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; x, y:Single; w, h: LongInt); overload;
	begin
		DrawTextLines(scr, theText, textColor, backColor, theFont, align, ScreenX(x), ScreenY(y), w, h);
	end;

	procedure DrawTextLines(theText: String; textColor, backColor: Colour; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
	begin
		DrawTextLines(scr, theText, textColor, backColor, theFont, align, ScreenX(withinRect.x), ScreenY(withinRect.y), withinRect.width, withinRect.height);		
	end;



	/// Draws texts to the screen. Drawing text is a slow operation,
	///	and drawing it to a bitmap, then drawing the bitmap to screen is a
	///	good idea. Do not use this technique if the text changes frequently.
	///
	///	@param theText:			The text to be drawn onto the screen
	///	@param textColor:		The color to draw the text
	///	@param theFont:			The font used to draw the text
	///	@param x,y:					The x,y location to draw the text at (top left)
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		on the screen.
	procedure DrawTextOnScreen(theText: String; textColor: Colour; theFont: Font; x, y: LongInt);
	begin
		DrawText(scr, theText, textColor, theFont, x, y);
	end;

	procedure DrawTextOnScreen(theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawText(scr, theText, textColor, theFont, Round(pt.x), Round(pt.y));
	end;

	procedure DrawUnicodeOnScreen(theText: WideString; textColor: Colour; theFont: Font; x, y: LongInt);
	begin
		DrawUnicode(scr, theText, textColor, theFont, x, y);
	end;

	procedure DrawUnicodeOnScreen(theText: WideString; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawUnicode(scr, theText, textColor, theFont, Round(pt.x), Round(pt.y));
	end;


	procedure DrawText(theText: String; textColor: Colour; theFont: Font; x, y: Single);
	begin
		DrawText(scr, theText, textColor, theFont, ScreenX(x), ScreenY(y));
	end;
	
	procedure DrawText(theText: String; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawText(scr, theText, textColor, theFont, ScreenX(pt.x), ScreenY(pt.y));		
	end;

	procedure DrawUnicode(theText: WideString; textColor: Colour; theFont: Font; x, y: Single);
	begin
		DrawUnicode(scr, theText, textColor, theFont, ScreenX(x), ScreenY(y));
	end;
	
	procedure DrawUnicode(theText: WideString; textColor: Colour; theFont: Font; const pt: Point2D); overload;
	begin
		DrawUnicode(scr, theText, textColor, theFont, ScreenX(pt.x), ScreenY(pt.y));		
	end;

	
	/// Calculates the width of a string when drawn with a given font.
	///
	///	@param theText:		The text to measure
	///	@param theFont:		The font used to draw the text
	///	@returns					 The width of the drawing in pixels
	function TextWidth(theText: String; theFont: Font): LongInt; overload;
	var
		y: LongInt; //SizeText returns both... store and ignore y
	begin
	  if not Assigned(theFont) then raise Exception.Create('No font supplied');
		try
			y := 0; result := 0;
			TTF_SizeText(theFont, PChar(theText), result, y);
		except
			raise Exception.Create('Unable to get the text width');
		end;
	end;

	function TextWidth(theText: WideString; theFont: Font): LongInt; overload;
	var
		y: LongInt; //SizeText returns both... store and ignore y
	begin
	  if not Assigned(theFont) then raise Exception.Create('No font supplied');
		try
			y := 0; result := 0;
			TTF_SizeUNICODE(theFont, PUInt16(theText), result, y);
		except
			raise Exception.Create('Unable to get the text width');
		end;
	end;


	/// Calculates the height of a string when drawn with a given font.
	///
	///	@param theText:		The text to measure
	///	@param theFont:		The font used to draw the text
	///	@returns					 The height of the drawing in pixels
	function TextHeight(theText: String; theFont: Font): LongInt; overload;
	var
		w: LongInt; //SizeText returns both... store and ignore w
	begin
	  if not Assigned(theFont) then raise Exception.Create('No font supplied');
		try
			w := 0; result :=  0;
			TTF_SizeText(theFont, PChar(theText), w, result);
		except
			raise Exception.Create('Unable to get the text height');
		end;
	end;

	function TextHeight(theText: WideString; theFont: Font): LongInt; overload;
	var
		w: LongInt; //SizeText returns both... store and ignore w
	begin
	  if not Assigned(theFont) then raise Exception.Create('No font supplied');
		try
			w := 0; result :=  0;
			TTF_SizeUNICODE(theFont, PUInt16(theText), w, result);
		except
			raise Exception.Create('Unable to get the text height');
		end;
	end;
	
	/// Draws the frame rate using the specified font at the indicated x, y.
	///	Draws the FPS (min, max) current average
	///
	///	@param x,y:			The x, y location to draw to
	///	@param font:		 The font used to draw the framerate
	///
	/// Side Effects:
	///	- Framerate is drawn to the screen
	procedure DrawFramerate(x, y: LongInt; font: Font);
	var
		temp, temp2, temp3 : String;
		textColour : Colour;
		average, highest, lowest : Single;
	begin
		//Draw framerates

		if renderFPSInfo.average = 0 then
			average := 9999
		else
			average := (1000 / renderFPSInfo.average);
		
		lowest	:= (1000 / renderFPSInfo.high);
		highest := (1000 / renderFPSInfo.low);

		if average < 30 then
			textColour := ColourRed
		else if average < 50 then
			textColour := ColourYellow
		else
			textColour := ColourGreen;

		Str(average:4:1, temp);
		Str(highest:4:1, temp2);
		Str(lowest:4:1, temp3);

    if not Assigned(font) then
      DrawTextOnScreen('FPS: (' + temp3 + ', ' + temp2 + ') ' + temp, textColour, x + 2, y + 2)
    else
		  DrawTextOnScreen('FPS: (' + temp3 + ', ' + temp2 + ') ' + temp, textColour, font, x + 2, y + 2);
	end;
	
  procedure DrawText(theText: String; textColor: Color; x, y: Single); overload;
  begin
    DrawText(scr, theText, textColor, ScreenX(x), ScreenY(y));
  end;
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; x, y: Single); overload;
  begin
    DrawText(scr, theText, textColor, x, y);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x, y: Single); overload;
  begin
    stringColor(dest.surface, Round(x), Round(y), PChar(theText), ToGFXColor(textColor));
  end;
	
initialization
begin
	if TTF_Init() = -1 then
	begin
		raise Exception.Create('Error openning font library. ' + string(TTF_GetError));
	end;
end;

finalization
begin
	TTF_Quit();
end;

end.
