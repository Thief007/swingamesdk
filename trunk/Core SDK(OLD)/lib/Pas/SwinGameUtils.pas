unit SwinGameUtils;

interface
	uses SwinGameAPI, SDL;

	function GetRGBFloatColor(r,g,b: Single): Color;
	function GetHSBColor(hue, saturation, brightness: Single): Color;

implementation

	function GetRGBFloatColor(r,g,b: Single): Color;
	begin
		result := GetColour(Round(r * 255), Round(g * 255), Round(b * 255));
	end;
	/// Returs a color from the HSB input.
	///
	/// @param hue, saturation, brightness: Values between 0 and 1
	/// @returns The matching color
	function GetHSBColor(hue, saturation, brightness: Single): Color;
	var
		domainOffset: Single;
		red, green, blue: Single;
	begin
		if brightness = 0 then
		begin
			result := ColorBlack;
			exit;
		end;
		
		if saturation = 0 then
		begin
			result := GetRGBFloatColor(brightness, brightness, brightness);
			exit;
		end;
		
		if hue < 1.0 / 6 then
		begin
			//Red domain... green ascends
			domainOffset := hue;
			red 	:= brightness;
			blue  	:= brightness * (1.0 - saturation);
        	green 	:= blue + (brightness - blue) * domainOffset * 6;
		end
		else if hue < 2.0 / 6 then
		begin
			// yellow domain; red descends
        	domainOffset := hue - 1.0 / 6;
	        green := brightness;
    	    blue  := brightness * (1.0 - saturation);
        	red   := green - (brightness - blue) * domainOffset * 6;
		end
		else if hue < 3.0 / 6 then
      	begin
      		// green domain; blue ascends
        	domainOffset := hue - 2.0 / 6;
        	green := brightness;
        	red   := brightness * (1.0 - saturation);
        	blue  := red + (brightness - red) * domainOffset * 6;
      	end
	    else if hue < 4.0 / 6 then
      	begin
      		// cyan domain; green descends
        	domainOffset := hue - 3.0 / 6;
	       blue  := brightness;
    	    red   := brightness * (1.0 - saturation);
        	green := blue - (brightness - red) * domainOffset * 6;
	    end
      	else if hue < 5.0 / 6 then
       begin
       		// blue domain; red ascends
        	domainOffset := hue - 4.0 / 6;
        	blue  := brightness;
        	green := brightness * (1.0 - saturation);
        	red   := green + (brightness - green) * domainOffset * 6;
      	end
      else
      begin
      	 // magenta domain; blue descends
        domainOffset := hue - 5.0 / 6;
        red   := brightness;
        green := brightness * (1.0 - saturation);
        blue  := red - (brightness - green) * domainOffset * 6;
      end;
      
      result := GetRGBFloatColor(red, green, blue);
	end;

end.