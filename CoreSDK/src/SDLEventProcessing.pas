unit SDLEventProcessing;

interface
uses Classes, SDL, SDL_Mixer, SysUtils, SDL_TTF;

type EventProcessPtr = procedure(event: PSDL_Event);
type EventStartProcessPtr = procedure();

type TSDLManager = class (TObject)
	private
		_quit: Boolean;
		_readingString: Boolean;
		_tempString: String;
		_textSurface: PSDL_Surface;
		_maxStringLen: Integer;
		_font: PTTF_Font;
		_foreColor: TSDL_Color;
		_x, _y: Integer;
		_KeyTyped: Array of Integer;
		_EventProcessors: Array of EventProcessPtr;
		_EventStartProcessors: Array of EventStartProcessPtr;
	public
		procedure ProcessEvents();

		constructor Create(); //caption: String; width, height: Integer);
		destructor Destroy(); override;

		function HasQuit(): Boolean;

		property MaxStringLength: Integer read _maxStringLen write _maxStringLen;
		property EnteredString: String read _tempString write _tempString;
		property IsReading: Boolean read _readingString;

		procedure DrawCollectedText(dest: PSDL_Surface);
		procedure StartReadingText(textColor: TSDL_Color; maxLength: Integer; theFont: PTTF_Font; x, y: Integer);

		function WasKeyTyped(keyCode: Integer): Boolean;

		procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);

	private
		procedure DoQuit;
		procedure HandleEvent(event: PSDL_Event);
		procedure HandleKeydownEvent(event: PSDL_Event);
end;

implementation
	procedure TSDLManager.ProcessEvents();
	var
		event: TSDL_EVENT;
		i: Integer;
	begin
		SetLength(_KeyTyped, 0);

		for i := 0 to High(_EventProcessors) do
		begin
			_EventStartProcessors[i]();
		end;	
	
		SDL_PumpEvents();

		while SDL_PollEvent(@event) > 0 do
		begin
			HandleEvent(@event);
		end;
	end;
	
	procedure TSDLManager.HandleEvent(event: PSDL_Event);
	var i : Integer;
	begin
		case event.type_ of
			SDL_QUITEV: DoQuit();
			SDL_KEYDOWN: HandleKeydownEvent(event);
		end;
	
		for i := 0 to High(_EventProcessors) do
		begin
			_EventProcessors[i](event);
		end;
	end;
	
	procedure TSDLManager.HandleKeydownEvent(event: PSDL_Event);
	var
		oldStr: String;
	begin
		SetLength(_KeyTyped, Length(_KeyTyped) + 1);
		_KeyTyped[High(_KeyTyped)] := event.key.keysym.sym;

		if _readingString then
		begin
			oldStr := _tempString;

			//If the key is not a control character
			if (event.key.keysym.sym = SDLK_BACKSPACE) and (Length(_tempString) > 0)then
			begin
				 _tempString := Copy(_tempString, 1, Length(_tempString) - 1);
			end
			else if event.key.keysym.sym = SDLK_RETURN then
			begin
				_readingString := false;
			end
			else if event.key.keysym.sym = SDLK_ESCAPE then
			begin
				_tempString := '';
				_readingString := false;
			end
			else if Length(_tempString) < _maxStringLen then
			begin
				case event.key.keysym.unicode of
					//Skip non printable characters
					0: ;
					128..159: ;
					7..10, 13: ;
					27: ;
					else //Append the character
						_tempString := _tempString + Char(event.key.keysym.unicode);
				end;
			end;

			//If the string was change
			if oldStr <> _tempString then
			begin
				 //Free the old surface
				 SDL_FreeSurface( _textSurface );

				 //Render a new text surface
				 if Length(_tempString) > 0 then
					 _textSurface := TTF_RenderText_Blended(_font, PChar(_tempString + '|'), _foreColor)
				 else
					_textSurface := nil;
			end;
		end;
	end;
	
	procedure TSDLManager.DrawCollectedText(dest: PSDL_Surface);
	var
		drect: SDL_Rect;
	begin
		if _readingString and (_textSurface <> nil) then
		begin
			drect.x := _x;
			drect.y := _y;
		
			SDL_BlitSurface(_textSurface, nil, dest, @drect);
		end;
	end;
	
	procedure TSDLManager.StartReadingText(textColor: TSDL_Color; maxLength: Integer; theFont: PTTF_Font; x: Integer; y: Integer);
	begin
		if _textSurface <> nil then
		begin
		 	//Free the old surface
		 	SDL_FreeSurface( _textSurface );
			_textSurface := nil;
		end;
	
		_readingString := true;
		_tempString := '';
		_maxStringLen := maxLength;
		_foreColor := textColor;
		_font := theFont;
		_x := x;
		_y := y;
	end;
	
	constructor TSDLManager.Create();
	begin
		_quit := false;
		_textSurface := nil;
		_readingString := false;
	end;
	
	destructor TSDLManager.Destroy();
	begin
		SDL_FreeSurface(_textSurface);
		inherited Destroy();
	end;
	
	procedure TSDLManager.DoQuit();
	begin
		_quit := true;
	end;
	
	function TSDLManager.HasQuit(): Boolean;
	begin
		result := _quit;
	end;
	
	function TSDLManager.WasKeyTyped(keyCode: Integer): Boolean;
	var i : Integer;
	begin
		result := false;
	
		for i := 0 to High(_KeyTyped) do
		begin
			if _KeyTyped[i] = keyCode then
			begin
				result := true;
				exit;
			end;
		end;
	end;
	
	procedure TSDLManager.RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
	begin
		//if handle^ <> nil then
		begin
			SetLength(_EventProcessors, Length(_EventProcessors) + 1);
			_EventProcessors[High(_EventProcessors)] := handle;
		end;
	
		//if handle2^ <> nil then
		begin
			SetLength(_EventStartProcessors, Length(_EventStartProcessors) + 1);
			_EventStartProcessors[High(_EventStartProcessors)] := handle2;
		end;	
	end;
end.