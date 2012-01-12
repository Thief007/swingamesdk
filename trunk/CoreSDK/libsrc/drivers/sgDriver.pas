unit sgDriver;
//=============================================================================
// sgDriver.pas
//=============================================================================
//
// The Driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the Driver to access the 
// current active driver. 
//
// Changing this driver will probably cause graphics drivers to break.
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface	
  type
    GetErrorProcedure = function () : PChar;
    QuitProcedure = procedure();
    InitProcedure = procedure();
    
  	DriverRecord = record
  	  GetError                : GetErrorProcedure;
  	  Quit                    : QuitProcedure;
  	  Init                    : InitProcedure

	  end;  
	
	
	var
		Driver : DriverRecord;

		
implementation
  	uses sgTypes, sgDriverSDL, sgDriverTimer;

	procedure LoadDefaultDriver();
	begin
		LoadSDLDriver();
	end;

	procedure DefaultInitProcedure();
	begin
	 LoadDefaultDriver();
	 Driver.Init();
	end;

	function DefaultGetErrorProcedure () : PChar;
	begin
		LoadDefaultDriver();
		result := Driver.GetError();
	end;

	procedure DefaultQuitProcedure();
	begin
	 LoadDefaultDriver();
	 Driver.Quit();
	end;

initialization
	begin
		Driver.GetError               := @DefaultGetErrorProcedure;
		Driver.Quit                   := @DefaultQuitProcedure;
		Driver.Init                   := @DefaultInitProcedure;
	end;
end.
	
