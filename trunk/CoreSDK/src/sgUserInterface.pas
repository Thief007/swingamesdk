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
//              They can  be drawn as rectangles and read from file.
// 
// - 2010-01-19: Cooldave : Textboxes, Buttons, Checkboxes, Radiobuttons, Labels all fully functional.
//							Lists are almost completed. Can be created and used, and items can be added at runtime.
//							Need to add the ability to remove items from lists.
//=============================================================================

{$I sgTrace.inc}

unit sgUserInterface;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

type
  Panel = ^PanelData;
  Region = ^RegionData;
  GUILabel = ^GUILabelData;
  GUICheckbox = ^GUICheckboxData;
  GUIRadioGroup = ^GUIRadioGroupData;
  GUITextBox = ^GUITextBoxData;
  GUIList = ^GUIListData;
  GUIListItem = ^GUIListItemData;
  
  GUITextBoxData = record
    contentString:  string;
    fontID:         string;
    lengthLimit:    LongInt;
  end;
  
  GUILabelData = record
    contentString:  string;
    fontID:         String;
  end;
  
  GUIListData = record
    columns:      LongInt;
    items:        Array of GUIListItem;
    activeItem:   LongInt;
    rowHeight:    LongInt;
    colWidth:     LongInt;
    fontID:				String;
  end;
  
  GUIListItemData = record
    nameString: string;
    imageID:     string;
    parentList:  string;
    area:        Rectangle;
  end;
  
  GUIRadioGroupData = record
    groupID:      string;
    buttons:      Array of Region;
    activeButton: LongInt;
  end;
  
  GUICheckboxData = record
    state:        boolean;
  end;
  
  RegionData = record
    stringID:       string;
    kind:         	 LongInt;
    regionID:  	   LongInt;
    elementIndex: LongInt;
    area:        	 Rectangle;
    active:    	   boolean;
    parent:      	 Panel;
  end;

  PanelData = record
    stringID:     string;
    panelID:      LongInt;
    position:     Point2D;
    width:        LongInt;
    height:       LongInt;
    visible:      boolean;
    active:       boolean;
    panelBitmap:  Bitmap;
    regions:      Array of Region;
    regionIds:    NamedIndexCollection;
    labels:       Array of GUILabel;
    checkBoxes:   Array of GUICheckbox;
    radioGroups:  Array of GUIRadioGroup;
    textBoxes:    Array of GUITextBox;
    lists:        Array of GUIList;
  end;
  
  
  GUIController = record
    panels:         Array of Panel;
    panelIds:       NamedIndexCollection;
    visiblePanels:  Array of Panel;
    globalGUIFont:  Font;
    globalGUIVectorColor: Color;
    VectorDrawing:  Boolean;
    lastClicked:    Region;
    activeTextBox:  Region;
  end;

function LoadPanel(filename: string): Panel;
function ActiveRadioButton(rGID: string): integer;
procedure ShowPanel(p: Panel);
procedure AddPanelToGUI(p: Panel);
procedure DrawPanels();
procedure SetGUIColorForVectors(c:Color);
procedure DrawGUIAsVectors(b : boolean);
procedure HidePanel(p: Panel);
function PanelClicked(): Panel;
function RegionClicked(): String;
function RegionClicked(pnl: Panel): Region; Overload;
function RegionStringID(r: Region): string;
procedure ToggleShowPanel(p: Panel);
procedure ActivatePanel(p: Panel);
procedure DeactivatePanel(p: Panel);
procedure ToggleActivatePanel(p: Panel);
procedure UpdateInterface();

function NewListItem(listID, itemID, bitmapID, nString: string): GUIListItem;
procedure AddItemToList(list: Region; item: GUIListItem);

function GetRegionByID(pnl: Panel; ID: String): Region; overload;
function GetRegionByID(ID: String): Region; overload;

function CheckboxState(ID: String): Boolean;
function TextBoxFromRegion(r: Region): GUITextBox;
function TextBoxFromRegion(regID: string): GUITextBox; Overload;
function LabelFromRegion(r: Region): GUILabel;
function LabelFromRegion(regID: string): GUILabel; Overload;
function LabelString(r: Region): string;
function LabelString(regID: string): string; Overload;
function LabelString(lb: GUILabel): string; Overload;
procedure SetLabelString(lb, newString: String);
function TextboxString(tbID: String): string; 
function TextboxString(tb: GUITextBox): string; Overload;

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
  
procedure DrawVectorCheckbox(forRegion: Region);
begin 
  if forRegion^.parent^.Checkboxes[forRegion^.elementIndex]^.state then 
    FillRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area) 
  else
    DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
end;

procedure DrawVectorRadioButton(forRegion: Region);
begin
  DrawEllipseOnScreen(GUIC.globalGUIVectorColor, Trunc(forRegion^.area.x), trunc(forRegion^.area.y), Trunc(forRegion^.area.width), Trunc(forRegion^.area.height));
  if (forRegion^.parent^.radioGroups[forRegion^.elementIndex]^.buttons[forRegion^.parent^.radioGroups[forRegion^.elementIndex]^.activeButton]^.StringID) = (forRegion^.StringID) then
    FillEllipseOnScreen(GUIC.globalGUIVectorColor, Trunc(forRegion^.area.x), trunc(forRegion^.area.y), Trunc(forRegion^.area.width), Trunc(forRegion^.area.height));
end;

procedure DrawVectorTextbox(forRegion: Region);
begin
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
  if assigned(GUIC.activeTextBox) AND NOT(GUIC.activeTextBox^.StringID = forRegion^.StringID) then
    DrawTextOnScreen(forRegion^.parent^.textBoxes[forRegion^.ElementIndex]^.contentString, GUIC.globalGUIVectorColor, FontNamed(forRegion^.parent^.textBoxes[forRegion^.ElementIndex]^.fontID), Trunc(forRegion^.area.x), Trunc(forRegion^.area.y))
  else if NOT(assigned(GUIC.activeTextBox)) then
    DrawTextOnScreen(forRegion^.parent^.textBoxes[forRegion^.ElementIndex]^.contentString, GUIC.globalGUIVectorColor, FontNamed(forRegion^.parent^.textBoxes[forRegion^.ElementIndex]^.fontID), Trunc(forRegion^.area.x), Trunc(forRegion^.area.y)); 
end;
  
procedure DrawVectorList(forRegion: Region);
var
	tempList: GUIList;
	i: integer;
begin
	tempList := forRegion^.parent^.lists[forRegion^.elementIndex];
	DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
	FillRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.items[tempList^.activeItem]^.area);
	
	for i := Low(tempList^.items) to High(tempList^.items) do
	begin
		DrawRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.items[i]^.area);
		
		if i <> tempList^.activeItem then
		begin
			DrawTextOnscreen(tempList^.items[i]^.nameString, GUIC.globalGUIVectorColor, FontNamed(tempList^.fontID), Trunc(tempList^.items[i]^.area.x), Trunc(tempList^.items[i]^.area.y));
		end
		else
		begin
			DrawTextOnscreen(tempList^.items[i]^.nameString, ColorBlack, FontNamed(tempList^.fontID), Trunc(tempList^.items[i]^.area.x), Trunc(tempList^.items[i]^.area.y));
		end;
	end;
end;

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
        case GUIC.panels[i]^.Regions[j]^.kind of
        0: DrawRectangleOnScreen(GUIC.globalGUIVectorColor, GUIC.panels[i]^.Regions[j]^.area);
        1: DrawTextOnScreen(GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j]^.elementIndex]^.contentString, GUIC.GlobalGUIVectorColor, FontNamed(GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j]^.elementIndex]^.fontID), Trunc(GUIC.panels[i]^.Regions[j]^.area.x), Trunc(GUIC.panels[i]^.Regions[j]^.area.y));
        2: DrawVectorCheckbox(GUIC.panels[i]^.Regions[j]);
        3: DrawVectorRadioButton(GUIC.panels[i]^.Regions[j]);
        4: DrawVectorTextbox(GUIC.panels[i]^.Regions[j]);
        5: DrawVectorList(GUIC.panels[i]^.Regions[j]);
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

function GetRegionByID(pnl: Panel; ID: String): Region; overload;
var
	idx: Integer;
begin
	result := nil;
	idx := IndexOf(pnl^.regionIds, ID);
	if idx >= 0 then
	begin
    result := pnl^.regions[idx];
	end;	
end;

function GetRegionByID(ID: String): Region; overload;
var
  i: integer;
begin
	result := nil;
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
  	result := GetRegionById(GUIC.panels[i], ID);
  	if assigned(result) then exit;
    //for j := Low(GUIC.panels[i]^.regions) to High(GUIC.panels[i]^.regions) do
    //begin
    //  if GUIC.panels[i]^.regions[j]^.StringID = ID then
    //  begin
    //    result := GUIC.panels[i]^.regions[j];
    //    exit;
    //  end;
    //end;
  end;
end;

function ActiveRadioButton(rGID: string): integer;
var
  i,j:integer;
begin
  for i:= Low(GUIC.Panels) to High(GUIC.Panels) do
    for j:= Low(GUIC.Panels[i]^.radioGroups) to High(GUIC.Panels[i]^.radioGroups) do
      if GUIC.Panels[i]^.radioGroups[j]^.groupID = rGID then
      begin
        result := GUIC.Panels[i]^.radioGroups[j]^.activeButton;
        exit;
      end;
end;

function TextBoxFromRegion(regID: string): GUITextBox; Overload;
begin
  if assigned(GetRegionByID(regID)^.parent^.TextBoxes[GetRegionByID(regID)^.elementIndex]) then
    result := GetRegionByID(regID)^.parent^.TextBoxes[GetRegionByID(regID)^.elementIndex]
  else
    result := nil;
end;

function TextBoxFromRegion(r: Region): GUITextBox;
begin
  if assigned(r^.parent^.TextBoxes[r^.elementIndex]) then
    result := r^.parent^.TextBoxes[r^.elementIndex]
  else
    result := nil;
end;

function LabelFromRegion(regID: string): GUILabel; Overload;
begin
  if assigned(GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex]) then
    result := GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex]
  else
    result := nil;
end;

function LabelFromRegion(r: Region): GUILabel;
begin
  if assigned(r^.parent^.labels[r^.elementIndex]) then
    result := r^.parent^.labels[r^.elementIndex]
  else
    result := nil;
end;

function ListFromRegion(reg: Region): GUIList;
begin
	if assigned(reg^.parent^.lists[reg^.elementIndex]) then
		result := reg^.parent^.lists[reg^.elementIndex]
	else
		result := nil;
end;

function ListFromID(idString: String): GUIList;
begin
	if assigned(GetRegionByID(idString)^.parent^.lists[GetRegionByID(idString)^.elementIndex]) then
		result := GetRegionByID(idString)^.parent^.lists[GetRegionByID(idString)^.elementIndex]
	else
		result := nil;
end;

function LabelString(r: Region): string;
begin
  if assigned(r^.parent^.labels[r^.elementIndex]) then
    result := r^.parent^.labels[r^.elementIndex]^.contentString
  else
    result := '';
end;

function LabelString(regID: string): string; Overload;
begin
  if assigned(GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex]) then
    result := GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex]^.contentString
  else
    result := '';
end;

function LabelString(lb: GUILabel): string; Overload;
begin
  if assigned(lb) then
    result := lb^.contentString;
end;

function TextboxString(tb: GUITextBox): string; Overload;
begin
  if assigned(tb) then
    result := tb^.contentString;
end;

function TextboxString(tbID: String): string;
begin
  if assigned(TextBoxFromRegion(tbID)) then
    result := TextboxString(TextBoxFromRegion(tbID));
end;

procedure SetLabelString(lb, newString: String);
begin
  if assigned(LabelFromRegion(lb)) then
    LabelFromRegion(lb)^.contentString := newString;
end;

function RegionStringID(r: Region): string;
begin
  if assigned(r) then
    result := r^.stringID
  else
    result := '';
end;

procedure RecalibrateList(listReg: Region);
var
    workingCol, workingRow: LongInt;
		k: LongInt;
		theList: GUIList;
begin
  workingCol := 0;
  workingRow := 0;
  
  theList := listReg^.parent^.lists[listReg^.elementIndex];
  
  for k := Low(theList^.items) to High(thelist^.items) do
  begin
  	WriteLn('Checking list elements ', k);
    theList^.items[k]^.area.x := (workingCol * theList^.colWidth) + listReg^.parent^.position.x + listReg^.area.x;
    theList^.items[k]^.area.y := (workingRow * theList^.rowHeight) + listReg^.parent^.position.y + listReg^.area.y;
    theList^.items[k]^.area.width := (theList^.colWidth);
    theList^.items[k]^.area.height := (theList^.rowHeight);
    
    WriteLn('Item ', k, ' details: x = ',  theList^.items[k]^.area.x, ' y = ',  theList^.items[k]^.area.y, ' width = ',  theList^.items[k]^.area.width, ' height = ',  theList^.items[k]^.area.height, '.');
    
    workingCol += 1;
    
    if workingCol >= theList^.columns then
    begin
      workingCol := 0;
      workingRow += 1;
  	end;
  	
	end;            
end;


function NewListItem(listID, itemID, bitmapID, nString: string): GUIListItem;
var
	tempReg: Region;
begin
	new(Result);
	result^.nameString := nString;
	WriteLn(nString);
	WriteLn(result^.nameString);
	result^.parentList := listID;
	result^.imageID := bitmapID;
	
	new(tempReg);
	tempReg^.stringID := itemID;
	tempReg^.kind := 6;
	tempReg^.area.x := 0; tempReg^.area.y := 0; tempReg^.area.width := 0; tempReg^.area.height := 0;
	tempReg^.active := True;
	
	SetLength(GetRegionByID(listID)^.parent^.regions, Length(GetRegionByID(listID)^.parent^.regions) + 1);
	GetRegionByID(listID)^.parent^.regions[High(GetRegionByID(listID)^.parent^.regions)] := tempReg;
	tempReg^.elementIndex := High(GetRegionByID(listID)^.parent^.regions);
	tempReg^.parent := GetRegionByID(listID)^.parent;
end; 

procedure AddItemToList(list: Region; item: GUIListItem);
var
	theList: GUIList;
begin
	  if not(assigned(list)) OR not(assigned(item)) then exit;
	  
	  theList := ListFromRegion(list);
	  
	  SetLength(theList^.items, Length(theList^.items) + 1);
	  theList^.items[High(theList^.items)] := item;
	
		RecalibrateList(list);
end;

function CheckboxState(ID: String): Boolean;
var
  reg: Region;
begin
  reg := GetRegionByID(ID);
  result := reg^.parent^.checkboxes[reg^.elementIndex]^.state;
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
  
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if (GUIC.panels[i]^.active) then
    begin
      if PointInRect(MousePosition(), GUIC.panels[i]^.position.x,GUIC.panels[i]^.position.y,GUIC.panels[i]^.width,GUIC.panels[i]^.height) then
      begin
        result := GUIC.panels[i];
        exit;
      end;
    end;
  end;
end;

procedure ToggleCheckboxState(c: GUICheckbox);
begin
  c^.state := not c^.state;
end;

procedure SelectRadioButton(r: Region);
var
  rGroup: GUIRadioGroup;
  i: integer;
begin
  rGroup := r^.parent^.radioGroups[r^.elementIndex];
  
  for i := Low(rGroup^.buttons) to High(rGroup^.buttons) do
  begin
    if rGroup^.buttons[i]^.stringID = r^.StringID then
      rGroup^.activeButton := i;
  end;
end;

procedure FinishReadingText();
begin
  if not assigned(GUIC.activeTextBox) then exit;
  
  //Write(' Storing last read into the contentString... ');
  GUIC.activeTextBox^.parent^.textBoxes[GUIC.activeTextBox^.elementIndex]^.contentString := EndReadingText();
  //WriteLn('Success');
  GUIC.activeTextBox := nil;
end;

procedure SetAsActiveTextbox(r: Region);
var
  textBox: GUITextbox;
begin
  //Write(' Attempting to store GUITextbox of passed region... ');
  textBox := r^.parent^.textBoxes[r^.elementIndex];
  //WriteLn('Stored successfully in textBox');
  
  //Write(' Setting new activeTextBox... ');
  GUIC.activeTextBox := r;
  //WriteLn('Success');
  //Write(' Beginning to Read text... ');
  
  StartReadingTextWithText(textBox^.contentString, GUIC.globalGUIVectorColor, textBox^.lengthLimit, FontNamed(textBox^.FontID), Trunc(r^.area.x), Trunc(r^.area.y));
  WriteLn('Started with: ', textBox^.contentString);
  //WriteLn('Success');
end;

procedure SetActiveListItem(forRegion: region; pointClicked: Point2D);
var
	theList: GUIList;
	i: LongInt;
begin
	theList := ListFromRegion(forRegion);
	for i := Low(theList^.items) to High(theList^.items) do
	begin
		if PointInRect(pointClicked, theList^.items[i]^.area) then
		begin
			theList^.activeItem := i;
		end;
	end;
end;

function RegionClicked(pnl: Panel): Region; Overload;
var
	j: LongInt;
	pointClicked: Point2D;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  FinishReadingText();
  
  pointClicked := MousePosition();
  
  if pnl = nil then exit;
  if not pnl^.active then exit;
  
  for j := Low(pnl^.Regions) to High(pnl^.Regions) do
  begin
    if PointInRect(pointClicked, pnl^.Regions[j]^.area) then
    begin
      case pnl^.Regions[j]^.kind of
        2: ToggleCheckboxState(pnl^.checkboxes[pnl^.Regions[j]^.elementindex]);
        3: SelectRadioButton(pnl^.Regions[j]);
        4: SetAsActiveTextbox(pnl^.Regions[j]);
        5: SetActiveListItem(pnl^.Regions[j], pointClicked);
      end;
      result := pnl^.Regions[j];
      exit;
    end;
  end;
end;

function RegionClicked(): String;
begin
  result := RegionStringID(GUIC.lastClicked);
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
  
  procedure AddRegionToGroup(regToAdd: Region; groupToRecieve: GUIRadioGroup);
  begin
    SetLength(groupToRecieve^.buttons, Length(groupToRecieve^.buttons) + 1);
    groupToRecieve^.buttons[High(groupToRecieve^.buttons)] := regToAdd;
  end;
  
  procedure CreateRadioButton(forRegion: Region; data: String);
  var
    newRadioGroup: GUIRadioGroup;
    i: Integer;
    radioGroupID: string;
  begin
    radioGroupID := Trim(ExtractDelimited(7,data,[',']));
    
    for i := Low(result^.radioGroups) to High(result^.radioGroups) do
    begin
      if (radioGroupID = result^.radioGroups[i]^.GroupID) then
      begin
        AddRegionToGroup(forRegion, result^.radioGroups[i]);
        forRegion^.elementIndex := i;
        exit;
      end;
    end;
    New(newRadioGroup);
    SetLength(newRadioGroup^.buttons, 0);
    newRadioGroup^.GroupID := radioGroupID;
    AddRegionToGroup(forRegion, newRadioGroup);
    if assigned(newRadioGroup^.buttons[0]) then
      newRadioGroup^.activeButton := 0; 
    // add to panel, record element index.
    SetLength(result^.radioGroups, Length(result^.radioGroups) + 1);
    result^.radioGroups[High(result^.radioGroups)] := newRadioGroup;
    forRegion^.elementIndex := High(result^.radioGroups);
  end;
  
  procedure CreateCheckbox(forRegion: Region; data: string);
  var
    newChkbox: GUICheckbox;
  begin
    new(newChkbox);
    newChkbox^.state := LowerCase(ExtractDelimited(7, data, [','])) = 'true';
    
    SetLength(result^.Checkboxes, Length(result^.Checkboxes) + 1);
    result^.Checkboxes[High(result^.Checkboxes)] := newChkbox;
    forRegion^.elementIndex := High(result^.labels);
  end;
  
  procedure CreateTextbox(r: region; data: string);
  var
    newTextbox: GUITextbox;
  begin
    new(newTextbox);
    newTextbox^.fontID := Trim(ExtractDelimited(7, data, [',']));
    newTextbox^.lengthLimit := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newTextBox^.contentString := Trim(ExtractDelimited(9, data, [',']));
    
    SetLength(result^.textBoxes, Length(result^.textBoxes) + 1);
    result^.textBoxes[High(result^.textBoxes)] := newTextbox;
    r^.ElementIndex := High(result^.textBoxes);
  end;
  
  procedure CreateList(r: Region; data: string);
  var
    newList: GUIList;
  begin
    new(newList);
    newList^.columns := StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList^.activeItem := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList^.colWidth := StrToInt(Trim(ExtractDelimited(9, data, [','])));
    newList^.rowHeight := StrToInt(Trim(ExtractDelimited(10, data, [','])));
    newList^.fontID := Trim(ExtractDelimited(11, data, [',']));
    SetLength(newList^.items, 0);
    
    SetLength(result^.lists, Length(result^.lists) + 1);
    result^.lists[High(result^.lists)] := newList;
    r^.elementIndex := High(result^.lists);
    WriteLn('List ''', r^.stringID, ''' added.');
  end;
  
  procedure CreateListItem(r: region; data: string);
  var
    newListItem: GUIListItem;
    reg: Region;
    pList: GUIList;
  begin
    new(newListItem);
    newListItem^.parentList := Trim(ExtractDelimited(7, data, [',']));
    newListItem^.nameString := Trim(ExtractDelimited(8, data, [',']));
    newListItem^.imageID := Trim(ExtractDelimited(9, data, [',']));
    
    WriteLn('trying ListFromID with: ', newListItem^.parentList);
    reg := GetRegionByID(result, newListItem^.parentList);
    pList := ListFromRegion(reg);
    
    Write('Setting length + 1... to ', HexStr(pList));
    SetLength(pList^.items, Length(pList^.items) + 1);
    WriteLn('Success');
    pList^.items[High(pList^.items)] := newListItem;
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
    r^.parent := p;
    
    case r^.kind of
      0: ;
      1: CreateLabel(r,d);
      2: CreateCheckbox(r,d);
      3: CreateRadioButton(r,d);
      4: CreateTextbox(r,d);
      5: CreateList(r, d);
      6: CreateListItem(r, d);
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
    SetLength(result^.radioGroups, 0);
    SetLength(result^.lists, 0);
    InitNamedIndexCollection(result^.regionIds);   //Setup the name <-> id mappings
  end;
  
  procedure InitializeLists();
  var
    tempListPtr: GUIList;
    workingCol, workingRow: LongInt;
    j, k: LongInt;
  begin
    workingCol := 0;
    workingRow := 0;
		for j := Low(result^.Regions) to High(result^.Regions) do
		begin
			WriteLn('Got regions');
		  if result^.Regions[j]^.kind = 5 then
		  begin
		  	WriteLn('Found a list..');
		    tempListPtr := result^.Regions[j]^.parent^.Lists[result^.Regions[j]^.elementIndex];
		    for k := Low(tempListPtr^.items) to High(tempListPtr^.items) do
		    begin
		    	WriteLn('Checking list elements ', k);
		      tempListPtr^.items[k]^.area.x := (workingCol * tempListPtr^.colWidth) + result^.position.x + result^.Regions[j]^.area.x;
		      tempListPtr^.items[k]^.area.y := (workingRow * tempListPtr^.rowHeight) + result^.position.y + result^.Regions[j]^.area.y;
		      tempListPtr^.items[k]^.area.width := (tempListPtr^.colWidth);
		      tempListPtr^.items[k]^.area.height := (tempListPtr^.rowHeight);
		      
		      workingCol += 1;
		      
		      if workingCol >= tempListPtr^.columns then
		      begin
		        workingCol := 0;
		        workingRow += 1;
		    end;
		  end;            
		end;
	end;
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
  
  InitializeLists();
end;

procedure SetGUIColorForVectors(c:Color);
begin
  GUIC.globalGUIVectorColor := c;
end;

procedure DrawGUIAsVectors(b : boolean);
begin
  GUIC.VectorDrawing := b;
end;

procedure UpdateInterface();
begin
  GUIC.lastClicked := RegionClicked(PanelClicked());
  if assigned(GUIC.activeTextbox) and not ReadingText() then
    FinishReadingText();
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