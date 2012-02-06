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

	var
		accelerometer : PSDL_Joystick;  

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

	
	procedure LoadSDL13iOSDriver();
	begin
		iOSDriver.Init 									:= @InitProcedure;
		iOSDriver.HandleAxisMotionEvent := @HandlAxisMotionEventProcedure;
		iOSDriver.AxisToG								:= @AxisToGProcedure;
	end;
end.