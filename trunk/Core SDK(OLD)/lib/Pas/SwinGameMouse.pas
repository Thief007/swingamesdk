unit SwinGameMouse;

interface
	uses SwinGameAPI, SDL;
	
	type MouseButton = (
			LeftButton = SDL_BUTTON_LEFT,
			MiddleButton = SDL_BUTTON_MIDDLE,
			RightButton = SDL_BUTTON_RIGHT
		);
	
	function GetMousePosition(): Vector;
	function GetMouseMovement(): Vector;
	function IsMouseDown(button: MouseButton): Boolean;
	function IsMouseUp(button: MouseButton): Boolean;
	function MouseWasClicked(button: MouseButton): Boolean;

implementation
	
	var _ButtonsClicked: Array [MouseButton] of Boolean;
	
	function GetMousePosition(): Vector;
	var
		x, y: Integer;
	begin
		SDL_GetMouseState(x, y);
		result := CreateVector(x, y);
	end;

	function GetMouseMovement(): Vector;
	var
		x, y: Integer;
	begin
		SDL_GetRelativeMouseState(x, y);
		result := CreateVector(x, y);
	end;
	
	function IsMouseDown(button: MouseButton): Boolean;
	var
		x, y: Integer;
	begin
		result := (SDL_GetMouseState(x, y) and 
		  SDL_BUTTON(Integer(button))) > 0;
	end;
	
	function IsMouseUp(button: MouseButton): Boolean;
	begin
		result := not IsMouseDown(button);
	end;
	
	function MouseWasClicked(button: MouseButton): Boolean;
	begin
		result := _ButtonsClicked[button];
	end;
	
	procedure ProcessEvent(event: PSDL_Event; first: Boolean);
	begin
		if event^.type_ = SDL_MOUSEBUTTONUP then
		begin
			_ButtonsClicked[MouseButton(event^.button.button)] := true;
		end;
	end;
	
	procedure StartProcessEvents();
	begin
		_ButtonsClicked[LeftButton] := false;
		_ButtonsClicked[RightButton] := false;
	end;
	
initialization
begin
	RegisterEventProcessor(@ProcessEvent, @StartProcessEvents);
end;

end.