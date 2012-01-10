unit sgDriverImagesSDL;
//=============================================================================
// sgDriverGraphicsSDL.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
	
	procedure LoadSDLImagesDriver();
		
implementation
	uses SDL, sgDriverImages, sgGraphicsDriver;
	
	function GetErrorProcedure() : PChar;
	begin
		result := SDL_GetError();
	end;
  
	procedure LoadSDLImagesDriver();
	begin
		Write('Loading SDL 1.2 Driver...');
		Driver.GetError := @GetErrorProcedure;
		WriteLn('Finished.');
	end;
end.