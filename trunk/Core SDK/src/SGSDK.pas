library SGSDK;

uses SGSDK_Core, SGSDK_Input;

	//CORE

	procedure ProcessEvents(); cdecl; export;
	begin
		SGSDK_Input.ProcessEvents();
	end;

	procedure OpenGraphicsWindow(caption : String; width : Integer; height : Integer); cdecl; export;
	begin
		SGSDK_Core.OpenGraphicsWindow(caption, width, height);
	end;
	
	function WindowCloseRequested(): Integer; cdecl; export;
	begin
		if SGSDK_Core.WindowCloseRequested() then
		begin
			//WriteLn('Close...');
			result:= -1
		end
		else
		begin
			//WriteLn('Not Close...');
			result:= 0
		end
	end;
	
	procedure SetIcon(iconFilename: String); cdecl; export;
	begin
		SGSDK_Core.SetIcon(iconFilename);
	end;
	
	//INPUT
exports
	//CORE
	OpenGraphicsWindow,
	WindowCloseRequested,
	ProcessEvents;
	
	//INPUT
	;
end.