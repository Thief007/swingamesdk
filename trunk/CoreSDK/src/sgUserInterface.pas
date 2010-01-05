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
    fontID:       String;
  end;
  
	GUILabel = ^GUILabelData;
	
	RegionData = record
    stringID:     String;
    kind:         LongInt;
    regionID:     LongInt;
    elementIndex: LongInt;
    area:         Rectangle;
    active:       boolean;
    parent:       ^Panel;
  end;

	Region = ^RegionData;
	
  PanelData = record
    stringID:     String;
    panelID:      LongInt;
    position:     Point2D;
    width:        LongInt;
    height:       LongInt;
    visible:      boolean;
    active:       boolean;
    panelBitmap:  Bitmap;
    regions:      Array of Region;
    regionIds:    NamedIndexCollection;
    labels: Array of GUILabel;
  end;
	
	Panel = ^PanelData;
	
	GUIController = record
    panels:         Array of Panel;
    panelIds:       NamedIndexCollection;
    visiblePanels:  Array of Panel;
    globalGUIFont:  Font;
    globalGUIVectorColor: Color;
    VectorDrawing:  Boolean;
  end;

function LoadPanel(filename: string): Panel;
procedure ShowPanel(p: Panel);
procedure AddPanelToGUI(p: Panel);
procedure DrawPanels();
procedure SetGUIColorForVectors(c:Color);
procedure DrawGUIAsVectors(b : boolean);
procedure HidePanel(p: Panel);
function PanelClicked(): Panel;
function RegionClicked(): Region;
function RegionClicked(pnl: Panel): Region; Overload;
function RegionStringID(r: Region): string;
procedure ToggleShowPanel(p: Panel);
procedure ActivatePanel(p: Panel);
procedure DeactivatePanel(p: Panel);
procedure ToggleActivatePanel(p: Panel);

//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection,   // libsrc
    sgShared, sgResources, sgTrace, sgImages, sgGraphics, sgCore, sgGeometry, sgText, sgInput;
//=============================================================================

var
  _Panels: TStringHash;
  GUIC: GUIController;
  
procedure DrawAsVectors();
var
  i, j: integer;
begin
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if (GUIC.panels[i]^.visible) then
    begin
      DrawRectangleOnScreen(GUIC.globalGUIVectorColor, Trunc(GUIC.panels[i]^.position.x), Trunc(GUIC.panels[i]^.position.y), GUIC.panels[i]^.width, GUIC.panels[i]^.height);
      for j := Low(GUIC.panels[i]^.Regions) to High(GUIC.panels[i]^.Regions) do
      begin
        if GUIC.panels[i]^.Regions[j]^.kind = 0 then
          DrawRectangleOnScreen(GUIC.globalGUIVectorColor, GUIC.panels[i]^.Regions[j]^.area)
        else if GUIC.panels[i]^.Regions[j]^.kind = 1 then
        begin
          DrawTextOnScreen(GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j]^.elementIndex]^.contentString, GUIC.GlobalGUIVectorColor, FetchFont(GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j]^.elementIndex]^.fontID), Trunc(GUIC.panels[i]^.Regions[j]^.area.x), Trunc(GUIC.panels[i]^.Regions[j]^.area.y));
        end;
      end;
    end;
  end;
end;

procedure DrawAsBitmaps();
var
  i: integer;
begin
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if (GUIC.panels[i]^.visible and not(GUIC.panels[i]^.panelBitmap = nil)) then
      DrawBitmapOnScreen(GUIC.panels[i]^.panelBitmap, GUIC.panels[i]^.position) //Look up syntax...
    else
      RaiseException('Bitmap for panel ' + IntToStr(i) + ' not set. Please check your input file or enable vector drawing.');
  end;
end;
  
procedure DrawPanels();
begin
  if GUIC.VectorDrawing then
    DrawAsVectors()
  else
    DrawAsBitmaps();
end;

function RegionStringID(r: Region): string;
begin
  if assigned(r) then
    result := r^.stringID
  else
    result := '';
end;

procedure ActivatePanel(p: Panel);
begin
  if assigned(p) then p^.active := true;
end;

procedure DeactivatePanel(p: Panel);
begin
  if assigned(p) then p^.active := false;
end;

procedure ToggleActivatePanel(p: Panel);
begin
  if assigned(p) then p^.active := not p^.active;
end;

procedure ShowPanel(p: Panel);
begin
  if assigned(p) then p^.visible := true;
end;

procedure ToggleShowPanel(p: Panel);
begin
  if assigned(p) then p^.visible := not p^.visible;
end;

procedure HidePanel(p: Panel);
begin
	if assigned(p) then p^.visible := false;
end;

function PanelClicked(): Panel;
var
  i: integer;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  for i:= Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if (GUIC.panels[i]^.active) then
    begin
      if PointInRect(MousePosition(), GUIC.panels[i]^.position.x,GUIC.panels[i]^.position.y,GUIC.panels[i]^.width,GUIC.panels[i]^.height) then
      result := GUIC.panels[i];
      exit;
    end;
  end;
end;

function RegionClicked(pnl: Panel): Region; Overload;
var
  i, j: integer;
begin
result := nil;
  if not MouseClicked(Leftbutton) then exit;
  if pnl = nil then exit;
  if (pnl^.active) then
  begin
    for j := Low(pnl^.Regions) to High(pnl^.Regions) do
    begin
      if PointInRect(MousePosition(), pnl^.Regions[j]^.area) then
      begin
        result := pnl^.Regions[j];
        exit;
      end;
    end;
  end;
end;

function RegionClicked(): Region;
var
  i: integer;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    Result := RegionClicked(GUIC.panels[i]);
    if not(result = nil) then exit;
  end;
end;

procedure setLabelText(var l: GUILabel; s: string);
begin
	l^.contentString := s;
end;

procedure setLabelFont(var l: GUILabel; s: String);
begin
	l^.fontID := s;
end;

procedure AddPanelToGUI(p: Panel);
begin
	SetLength(GUIC.panels, Length(GUIC.panels) + 1);
	GUIC.panels[High(GUIC.panels)] := p;
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
  
  procedure CreateLabel(forRegion: Region; d: string);
  var
    newLbl: GUILabel;
  begin
    New(newLbl);
    newLbl^.contentString := ExtractDelimited(7, d, [',']);
    newLbl^.fontID := Trim(ExtractDelimited(8, d, [',']));
    
    SetLength(result^.Labels, Length(result^.Labels) + 1);
    result^.labels[High(result^.labels)] := newLbl;
    forRegion^.elementIndex := High(result^.labels);  // The label index for the region -> so it knows which label
  end;
  
  procedure AddRegionToPanelWithString(d: string; p: panel);
  var
    regID: string;
    regX, regY, regW, regH, regKind, addedIdx: integer;
    r: region;
  begin
    regX := StrToInt(Trim(ExtractDelimited(1, d, [','])));
    regY := StrToInt(Trim(ExtractDelimited(2, d, [','])));
    regW := StrToInt(Trim(ExtractDelimited(3, d, [','])));
    regH := StrToInt(Trim(ExtractDelimited(4, d, [','])));
    regKind := StrToInt(Trim(ExtractDelimited(5, d, [','])));
    
    regID := Trim(ExtractDelimited(6, d, [',']));
    
    regX += Trunc(p^.position.x);
    regY += Trunc(p^.position.y);
    
    new(r);
    r^.area := RectangleFrom(regX, regY, regW, regH);
    r^.kind := regKind;
    r^.active := true;
    r^.StringID := regID;
    
    addedIdx := AddName(result^.regionIds, regID);   //Allocate the index
    SetLength(p^.Regions, Length(p^.Regions) + 1);
    r^.RegionID := High(p^.Regions);
    if High(p^.Regions) <> addedIdx then begin RaiseException('Error creating panel - added index is invalid.'); exit; end;
    
    p^.Regions[addedIdx] := r;
    r^.parent := @p;
    
    case r^.kind of
      0: ;
      1: CreateLabel(r,d);
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
      'r': AddRegionToPanelWithString(data, result);
		else
			begin
				RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be one of the characters defined in the template.');
				exit;
			end;
		end;
	end;
  
  procedure InitPanel();
  var
    P2D: Point2D;
  begin
    P2D.x := 0;
    P2D.y := 0;
  	New(result);
    result^.stringID := '';
    result^.panelBitmap := nil;
    result^.panelID := -1;
    result^.position := P2D;
    result^.width := -1;
    result^.height := -1;
    result^.visible := false;
    result^.active := false;
    InitNamedIndexCollection(result^.regionIds);   //Setup the name <-> id mappings
  end;
	
begin
	{$ifdef Windows}
	pathToFile := PathToResourceWithBase(applicationPath, 'panels\' + filename);
	{$else}
	pathToFile := PathToResourceWithBase(applicationPath, 'panels/' + filename);
	{$endif}
	
  // Initialise the resulting panel
  InitPanel();
	
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

procedure SetGUIColorForVectors(c:Color);
begin
  GUIC.globalGUIVectorColor := c;
end;

procedure DrawGUIAsVectors(b : boolean);
begin
  GUIC.VectorDrawing := b;
end;

//=============================================================================
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    GUIC.VectorDrawing := False;
    
    _Panels := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'Initialise');
    {$ENDIF}
  end;

end.
