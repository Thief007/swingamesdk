unit sgDriveriOS;
//=============================================================================
// sgiOSDriver.pas
//=============================================================================
//
//
//=============================================================================

interface
	uses sgTypes, {$IFDEF SWINGAME_SDL13}sgDriveriOSSDL13{$ELSE}sgDriveriOSSDL{$ENDIF};
	
	type
		ShowKeyboardProcedure = procedure();
		InitProcedure = procedure();
		HandleAxisMotionEventProcedure = function() : AccelerometerMotion;
		AxisToGProcedure = function( value : LongInt ) : Single;
	
	iOSDriverRecord = record
		ShowKeyboard     			: ShowKeyboardProcedure;
		Init	         				: InitProcedure;
		HandleAxisMotionEvent : HandleAxisMotionEventProcedure;
		AxisToG 							: AxisToGProcedure;
	end;  
	
	
	
	var
		iOSDriver : iOSDriverRecord;
		
implementation
	procedure LoadDefaultiOSDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
		  LoadSDL13iOSDriver();
		{$ELSE}
		  LoadSDLiOSDriver();
		{$ENDIF}
	end;


	procedure DefaultShowKeyboardProcedure();
	begin
		LoadDefaultiOSDriver();
		iOSDriver.ShowKeyboard();
	end;

	function DefaultHandleAxisMotionEventProcedure() : AccelerometerMotion; 
	begin
		LoadDefaultiOSDriver();
		result := iOSDriver.HandleAxisMotionEvent();
	end;

	function DefaultAxisToGProcedure(value : LongInt): Single; 
	begin
		LoadDefaultiOSDriver();
		result :=  iOSDriver.AxisToG(value);
	end;

	procedure DefaultInitProcedure(); 
	begin
    LoadDefaultiOSDriver();
    iOSDriver.Init();
	end;


	
	initialization
	begin
		iOSDriver.ShowKeyboard 					:= @DefaultShowKeyboardProcedure;
    iOSDriver.Init         					:= @DefaultInitProcedure;
    iOSDriver.HandleAxisMotionEvent := @DefaultHandleAxisMotionEventProcedure;
    iOSDriver.AxisToG 							:= @DefaultAxisToGProcedure;
	end;
end.
	
