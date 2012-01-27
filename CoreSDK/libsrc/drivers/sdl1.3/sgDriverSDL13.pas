unit sgDriverSDL13;
//=============================================================================
// sgDriverSDL13.pas
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
uses
  sgTypes, SDL;
  
  type	  
    SDL13Surface = record
      surface : PSDL_Surface;
      texture : PSDL_Texture;
      optimised  : Boolean;
    end;

    
    SDL13Screen = record
      window : PSDL_Window;
      renderer : PSDL_Renderer;
    end;

    PSDL13Screen = ^SDL13Screen;
    PSDL13Surface = ^SDL13Surface;
	
	procedure LoadSDL13Driver();
	function GetSurface(bmp : Bitmap) : PSDL_Surface;

var
  _ScreenDirty : Boolean = False;
		
implementation
	uses sgDriver, sgShared;
	
	var
	  _Initialised : Boolean = False;
	
	function GetSurface(bmp : Bitmap) : PSDL_Surface;
  begin
    if not Assigned(bmp) or not Assigned(bmp^.surface) then
      result := nil
    else 
      result := PSDL13Surface(bmp^.surface)^.surface;
  end;
	
	function GetErrorProcedure() : PChar;
	begin
		result := SDL_GetError();
	end;
  
  procedure QuitProcedure(); 
  begin
    SDL_Quit();
  end;
  
  procedure InitProcedure(); 
  begin
    if _Initialised then exit;
    _Initialised := true;
    
    if (SDL_INIT(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1) then
    begin
      {$IFDEF Trace}
      TraceIf(tlError, 'sgShared', 'ERROR', 'InitialiseSwinGame', GetErrorProcedure());
      {$ENDIF}
      RaiseException('Error loading sdl... ' + GetErrorProcedure());
      exit;
    end;

    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'About to enable unicode');
    {$ENDIF}

    //Unicode required by input manager.
    SDL_EnableUNICODE(SDL_ENABLE);
  end;
  
  
	procedure LoadSDL13Driver();
	begin
		Driver.GetError           := @GetErrorProcedure;
		Driver.Quit               := @QuitProcedure;
		Driver.Init               := @InitProcedure;
	end;
end.