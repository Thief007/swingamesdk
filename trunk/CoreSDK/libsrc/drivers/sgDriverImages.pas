unit sgDriverImages;
//=============================================================================
// sgDriverImages.pas
//=============================================================================
//
// The Graphics driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the images driver to access the 
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
	uses sgTypes, sgDriverImagesSDL;
	
	type
	  GetErrorProcedure = function () : PChar;

	ImagesDriverRecord = record
	  GetError                : GetErrorProcedure;
	end;
	
	var
		Driver : DriverRecord;
		
implementation
	procedure LoadDefaultDriver();
	begin
		LoadSDLImagesDriver();
	end;
	
	function DefaultGetErrorProcedure () : PChar;
	begin
		LoadDefaultDriver();
		result := ImagesDriver.GetError();
	end;

	initialization
	begin
		ImagesDriver.GetError               := @DefaultGetErrorProcedure;
	end;
end.
	
