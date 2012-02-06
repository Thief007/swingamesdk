unit sgDriveriOS;
//=============================================================================
// sgiOSDriver.pas
//=============================================================================
//
//
//=============================================================================

interface
	uses sgTypes {$IFDEF SWINGAME_SDL13},sgDriveriOSSDL13{$ENDIF};
	
	type
		ShowKeyboardProcedure = procedure();
		InitProcedure = procedure();
		ProcessAxisMotionEventProcedure = function() : AccelerometerMotion;
		AxisToGProcedure = function( value : LongInt ) : Single;
		ProcessTouchEventProcedure = function (touchID : Int64) : FingerArray;

	iOSDriverRecord = record
		ShowKeyboard     			: ShowKeyboardProcedure;
		Init	         				: InitProcedure;
		ProcessAxisMotionEvent : ProcessAxisMotionEventProcedure;
		AxisToG 							: AxisToGProcedure;
		ProcessTouchEvent 			: ProcessTouchEventProcedure;
	end;



	var
		iOSDriver : iOSDriverRecord;

implementation
	procedure LoadDefaultiOSDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
		  LoadSDL13iOSDriver();
		{$ELSE}
		  //LoadSDLiOSDriver();
		{$ENDIF}
	end;


	procedure DefaultShowKeyboardProcedure();
	begin
		LoadDefaultiOSDriver();
		exit;
		//TO DO
		// iOSDriver.ShowKeyboard();
	end;

	function DefaultProcessAxisMotionEventProcedure() : AccelerometerMotion;
	begin
		LoadDefaultiOSDriver();
		result := iOSDriver.ProcessAxisMotionEvent();
	end;

	function DefaultProcessTouchEventProcedure(touchID : Int64): FingerArray;
	begin
		LoadDefaultiOSDriver();
		result := iOSDriver.ProcessTouchEvent(touchID);
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
    iOSDriver.ProcessAxisMotionEvent := @DefaultProcessAxisMotionEventProcedure;
    iOSDriver.AxisToG 							:= @DefaultAxisToGProcedure;
    iOSDriver.ProcessTouchEvent 			:= @DefaultProcessTouchEventProcedure;
	end;
end.

