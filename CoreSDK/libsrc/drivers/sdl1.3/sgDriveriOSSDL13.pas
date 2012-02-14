unit sgDriveriOSSDL13;
//=============================================================================
// sgDriveriOSSDL.pas
//=============================================================================
//
// The iOS driver is responsible for controling iOS functions 
// between SDL 1.3 and SwinGame.
// Notes:

//=============================================================================

interface 
	uses SDL13, sgTypes, sgShared;
	procedure LoadSDL13iOSDriver();
	
	//iphone max g force.
	const MAX_G  = 32767;	
	
implementation
	uses sgDriveriOS;

  {$IFDEF IOS}
  var
  	accelerometer :PSDL_Joystick;
  {$ENDIF}
  
	procedure InitProcedure();
	begin
		{$IFDEF IOS}
			if (SDL_InitSubSystem(SDL_INIT_JOYSTICK) = -1) then 
			begin
				RaiseWarning('Error Initialising Joystick. '+ SDL_GetError());	exit;
			end;

			if (SDL_NumJoySticks() > 0) then 
			begin
					// assume you are using iOS and as of 2012 it only has 1.
					accelerometer := SDL_JoystickOpen(0);
					if (accelerometer = nil) then 
					begin
						RaiseWarning('Could not open accelerometer'+ SDL_GetError()); exit;
					end;
			end

			else
				RaiseWarning('No accelerometer detected');	
		{$ENDIF}
		exit;	
	end;

	function HandlAxisMotionEventProcedure(): AccelerometerMotion; 
	var
		accel : AccelerometerMotion;
	begin
		accel.xAxis := 0;
		accel.yAxis := 0;
		accel.zAxis := 0;
		{$IFDEF IOS}
			if (accelerometer = nil) then exit;
			accel.xAxis := SDL_JoystickGetAxis(accelerometer,0);
			accel.yAxis := SDL_JoystickGetAxis(accelerometer,1);
			accel.zAxis := SDL_JoystickGetAxis(accelerometer,2);
		{$ENDIF}
		result := accel;
	end;

	function AxisToGProcedure(value : LongInt) : Single; 
	begin
		result := value / MAX_G;
	end;

	function SDLFingerToFinger(sdlFinger : PSDL_Finger ): Finger;
	begin
		if not assigned(sdlFinger) then exit;
		
		result.id               := sdlFinger^.id;
	  result.position.x       := sdlFinger^.x;
	  result.position.y       := sdlFinger^.y;
	  result.positionDelta.x  := sdlFinger^.xdelta;
	  result.positionDelta.y  := sdlFinger^.ydelta;
	  result.lastPosition.x		:= sdlFinger^.last_x;
	  result.lastPosition.y 	:= sdlFinger^.last_y;
	  result.pressure 				:= sdlFinger^.pressure;
	  result.lastPressure 		:= sdlFinger^.last_pressure;
	  result.down 						:= (sdlFinger^.down = SDL_TRUE);
	end;

	function ProcessTouchEventProcedure(touchID : int64): FingerArray; 
	var
	  touch : PSDL_Touch;
	  numberOfFingers, count : LongInt;
	  sdlFingerArray : PPSDL_Finger;
	begin
		touch := SDL_GetTouch(touchID);
		if (touch = nil) then exit;
		sdlFingerArray := touch^.fingers;
		numberOfFingers := touch^.num_fingers;

		SetLength(result, numberOfFingers);

		for count := 0 to numberOfFingers - 1 do
		begin
			result[count] := SDLFingerToFinger((sdlFingerArray + count)^);
		end;
	end;
	
	procedure LoadSDL13iOSDriver();
	begin
		iOSDriver.Init 									:= @InitProcedure;
		iOSDriver.ProcessAxisMotionEvent := @HandlAxisMotionEventProcedure;
		iOSDriver.ProcessTouchEvent			:= @ProcessTouchEventProcedure;
		iOSDriver.AxisToG								:= @AxisToGProcedure;
	end;
end.