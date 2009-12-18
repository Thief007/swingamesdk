//=============================================================================
// sgAnimation.pas
//=============================================================================
//
// Version 0.1 - Resposible for constructing User Interfaces in 
// SwinGame projects.. eventually.
//
// Change History:
//
// Version 3:
// - 2009-12-18: Cooldave : Ability to create panels...  no regions, just panels. 
//							They can  be drawn as rectangles and read from file.
//=============================================================================

{$I sgTrace.inc}

unit sgUserInterface;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

type
	GUILabelData = record
           contentString: string;
           textFont: Font;
    end;
	
	GUILabel = ^GUILabelData;
	
	RegionData = record
           kind: integer;
           elementIndex: integer;
           area: Rectangle;
           active: boolean;
           parent: ^Panel;
    end;

	Region = ^RegionData;
	
    PanelData = record
          stringID: string;
          position: Point2D;
		  width: integer;
		  height: integer;
          visible: boolean;
          active: boolean;
          panelBitmap: Bitmap;
          regions: Array of Region;
          RegionName: Array of String;
          //Hash table of Region strings -> ints
          labels: Array of GUILabel;
    end;
	
	Panel = ^PanelData;
	
	GUIController = record
          panels: Array of Panel;
          visiblePanels: Array of Panel;
          globalGUIFont: Font;
    end;

function LoadPanel(filename: string): Panel;
procedure ShowPanel(p: Panel);
procedure AddPanelToGUI(p: Panel);
procedure DrawPanelsAsRectangles();


//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection,   // libsrc
    sgShared, sgResources, sgTrace, sgImages, sgGraphics, sgCore;
//=============================================================================

var
  _Panels: TStringHash;
  GUIC: GUIController;
  
procedure DrawPanels(gC: GUIController);
var
   i: integer;
begin
    for i := Low(gC.panels) to High(gC.panels) do
	begin
		if (gC.panels[i]^.visible) then
		begin
			DrawBitmapOnScreen(gC.panels[i]^.panelBitmap, gC.panels[i]^.position);
		end;
    end;
end;

procedure DrawPanelsAsRectangles();
var
   i: integer;
begin
    for i := Low(GUIC.panels) to High(GUIC.panels) do
	begin
		if (GUIC.panels[i]^.visible) then
		begin
			DrawRectangleOnScreen(RGBColor(0,255,0), Trunc(GUIC.panels[i]^.position.x), Trunc(GUIC.panels[i]^.position.y), GUIC.panels[i]^.width, GUIC.panels[i]^.height);
		end;
    end;
end;


procedure ShowPanel(p: Panel);
begin
	p^.visible := true;
end;

procedure HidePanel(p: Panel);
begin
	p^.visible := false;
end;

function regionClicked(): Region;
var
	res: Region;
begin
	result := res;
end;

procedure setLabelText(var l: GUILabel; s: string);
begin
	l^.contentString := s;
end;

procedure setLabelFont(var l: GUILabel; f: Font);
begin
	l^.textFont := f;
end;

procedure AddRegionToPanel(r: Region; p: Panel);
var
	i: integer;
begin
	SetLength(p^.Regions, Length(p^.Regions) + 1);
	p^.Regions[Length(p^.Regions) - 1] := r;
	r^.parent := @p;
end;

procedure AddPanelToGUI(p: Panel);
begin
	WriteLn('In add panel to GUI');
	//WriteLn(HexStr(g));
	SetLength(GUIC.panels, Length(GUIC.panels) + 1);
	WriteLn('Length set :', Length(GUIC.panels));
	GUIC.panels[Length(GUIC.panels) - 1] := p;
	WriteLn('Panel added');
end;

function CreateButton(area: Rectangle; active: boolean): Region;
begin
	New(result);
	result^.kind := 0; // Sets as button
	result^.area := area; // 
	result^.active := active; //Sets whether the element can be clicked or not.
    result^.parent := nil; //Set when region is added to panel.
	// Panel is set when region is added to panel.
end;

function CreateGUILabel(area: Rectangle; active: boolean): Region;
begin
	New(result);
	result^.kind := 1; // Sets as GUILabel
	result^.area := area;
	
end;

function LoadPanel(filename: string): Panel;
var
	pathToFile, line, id, data: string;
	panelFile: text;
	lineNo: integer;
	
	function MyStrToInt(str: String; allowEmpty: Boolean): LongInt;
	begin
		if not allowEmpty and (Length(str) = 0) then
		begin
			result := -1;
		end
		else if not TryStrToInt(str, result) then
		begin
			result := 0;
			RaiseException('Error at line ' + IntToStr(lineNo) + ' in Panel: ' + filename + '. Value ' + str + ' is not an integer.');
		end
		else
		begin
			result := StrToInt(str);
		end;
	end;
	
	procedure ProcessLine();
	begin
		// Split line into id and data around :
		id := ExtractDelimited(1, line, [':']); // where id comes before...
		data := ExtractDelimited(2, line, [':']); // ...and data comes after.
	
		// Verify that id is a single char
		if Length(id) <> 1 then
		begin
			RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be a single character.');
			exit; // Fail
		end;
		// Process based on id
		case LowerCase(id)[1] of // in all cases the data variable is read
			'x': result^.position.x := MyStrToInt(data, false);
			'y': result^.position.y := MyStrToInt(data, false);
			'w': result^.width := MyStrToInt(data, false);
			'h': result^.height := MyStrToInt(data, false);
		else
			begin
				RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be one of the characters defined in the template.');
				exit;
			end;
		end;
	end;
	
begin
	{$ifdef Windows}
	pathToFile := PathToResourceWithBase(applicationPath, 'panels\' + filename);
	{$else}
	pathToFile := PathToResourceWithBase(applicationPath, 'panels/' + filename);
	{$endif}
	
	New(result);
	
	Assign(panelFile, pathToFile);
	Reset(panelFile);
	
	lineNo := -1;
	
	while not EOF(panelFile) do
    begin
		lineNo := lineNo + 1;
		
		ReadLn(panelFile, line);
		line := Trim(line);
		if Length(line) = 0 then continue;  //skip empty lines
		if MidStr(line,1,2) = '//' then continue; //skip lines starting with // - the first character at index 0 is the length of the string.
		
		ProcessLine();
    end;
end;

//=============================================================================
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _Panels := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'Initialise');
    {$ENDIF}
  end;

end.
