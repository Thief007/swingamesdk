//=============================================================================
// sgUserInterface.pas
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
  
  GuiElementKind = ( gkLabel, gkButton, gkCheckBox, gkRadioGroup, gkTextBox, gkList );
  
  GUITextBoxData = record
    contentString:  string;
    font:         	Font;
    lengthLimit:    LongInt;
  end;
  
  GUILabelData = record
    contentString:  string;
    font:         Font;
  end;
  
  /// Each list item has text and an image
  GUIListItem = record
    text: 		String;
    image:		Bitmap;
    parent:		GUIList;
  end;

  
  GUIListData = record
  	verticalScroll: Boolean;
  	scrollNeg:		Rectangle;
  	scrollPos:		Rectangle;
  	
    columns:      LongInt;
    rows:					LongInt;
    
    rowHeight:   	LongInt;
    colWidth:     LongInt;
    scrollSize:		LongInt;
    placeholder: 	Array of Rectangle;
    
    activeItem:   LongInt;
    startingAt:		LongInt;

    font:				Font;
        
    items:        Array of GUIListItem;
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
    stringID:       String;
    kind:         	GUIElementKind;
    regionID:  	   	LongInt;
    elementIndex: 	LongInt;
    area:        	 	Rectangle;
    active:    	   	Boolean;
    
    parent:      	 	Panel;
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
    regions:      Array of RegionData;
    regionIds:    NamedIndexCollection;
    labels:       Array of GUILabelData;
    checkBoxes:   Array of GUICheckboxData;
    radioGroups:  Array of GUIRadioGroupData;
    textBoxes:    Array of GUITextBoxData;
    lists:        Array of GUIListData;
  end;
  
  
  GUIController = record
    panels:         		Array of Panel;
    panelIds:       		NamedIndexCollection;
    visiblePanels:  		Array of Panel;
    globalGUIFont:  		Font;
    globalGUIVectorColor: Color;
    VectorDrawing:  		Boolean;
    lastClicked:    		Region;
    activeTextBox: 			Region;
    lastActiveTextBox:	Region;
    doneReading: 				Boolean;
    lastTextRead:				String; //The text that was in the most recently changed textbox before it was changed.
  end;

function LoadPanel(filename: string): Panel;
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
procedure FinishReadingText();
procedure ToggleCheckboxState(c: GUICheckbox);
procedure SelectRadioButton(r: Region);
procedure SetAsActiveTextbox(r: Region);
procedure SetActiveListItem(forRegion: region; pointClicked: Point2D);

procedure ToggleShowPanel(p: Panel);
procedure ActivatePanel(p: Panel);
procedure DeactivatePanel(p: Panel);
procedure ToggleActivatePanel(p: Panel);
procedure UpdateInterface();

procedure ListRemoveItem(lst: GUIList; idx: LongInt);

procedure ListAddItem(lst: GUIList; text: String);
procedure ListAddItem(lst: GUIList; img:Bitmap);
procedure ListAddItem(lst: GUIList; img:Bitmap; text: String);

function ListBitmapIndex(lst: GUIList; img: Bitmap): LongInt;
function ListTextIndex(lst: GUIList; value: String): LongInt;

//function ListTextAt(lst: GUIList; idx: LongInt): String;
//function ListBitmapAt(lst: GUIList; idx: LongInt): Bitmap;

//function ListActiveIndex(lst: GUIList): LongInt;
//function ListActiveText(lst: GUIList): String;
//function ListActiveBitmap(lst: GUIList): Bitmap;

function RadioGroupFromRegion(forRegion: Region): GUIRadioGroup;
function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;

function ListFromRegion(reg: Region): GUIList; overload;

function GetRegionByID(pnl: Panel; ID: String): Region; overload;
function GetRegionByID(ID: String): Region; overload;

function CheckboxState(ID: String): Boolean;

function TextBoxFromRegion(r: Region): GUITextBox;
function TextBoxText(tb: GUITextBox): string;

function ActiveTextBoxParent() : Panel;
procedure TextboxSetString(tb: GUITextBox; s: string);
function LabelFromRegion(r: Region): GUILabel;
function LabelString(lb: GUILabel): string; Overload;
procedure setLabelText(lb: GUILabel; newString: String);

function TextboxString(tb: GUITextBox): string; Overload;

procedure DeactivateTextBox();
function ActiveTextIndex(): Integer;
function GUITextEntryComplete(): boolean;
function RegionParent(r: Region): Panel;
function GUITextBoxOfTextEntered(): GUITextbox;
function RegionOfLastUpdatedTextBox(): Region;
function PanelShown(p: Panel): boolean;

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

function GUITextEntryComplete(): boolean;
begin
	result := GUIC.doneReading;
end;

function GUITextBoxOfTextEntered(): GUITextbox;
begin
	result := @GUIC.lastActiveTextBox^.parent^.textBoxes[GUIC.lastActiveTextBox^.elementIndex];
end;

function RegionOfLastUpdatedTextBox(): Region;
begin
	result := GUIC.lastActiveTextBox;
end;

function ActiveTextIndex(): Integer;
begin
	result := GUIC.activeTextBox^.elementIndex;
end;

procedure DeactivateTextBox();
begin	
	GUIC.activeTextBox := nil;
end;

procedure TextboxSetString(tb: GUITextBox; s: string);
begin
  if assigned(tb) then
    tb^.contentString := s;
end;

function ActiveTextBoxParent() : Panel;
begin
	result := GUIC.activeTextBox^.parent;
end;
  
//---------------------------------------------------------------------------------------
// Drawing Loops
//---------------------------------------------------------------------------------------  
  
procedure DrawVectorCheckbox(forRegion: Region);
begin 
  if forRegion^.parent^.Checkboxes[forRegion^.elementIndex].state then 
    FillRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area) 
  else
    DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
end;

procedure DrawVectorRadioButton(forRegion: Region);
begin
  DrawEllipseOnScreen(GUIC.globalGUIVectorColor, Round(forRegion^.area.x), Round(forRegion^.area.y), Round(forRegion^.area.width), Round(forRegion^.area.height));
  
  if (forRegion^.parent^.radioGroups[forRegion^.elementIndex].buttons[forRegion^.parent^.radioGroups[forRegion^.elementIndex].activeButton]^.StringID) = (forRegion^.StringID) then
  begin
    FillEllipseOnScreen(GUIC.globalGUIVectorColor, Round(forRegion^.area.x), Round(forRegion^.area.y), Round(forRegion^.area.width), Round(forRegion^.area.height));
  end;
end;

procedure DrawVectorTextbox(forRegion: Region);
begin
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
  if assigned(GUIC.activeTextBox) AND NOT(GUIC.activeTextBox^.StringID = forRegion^.StringID) then
    DrawTextOnScreen(TextBoxText(TextBoxFromRegion(forRegion)), GUIC.globalGUIVectorColor, TextBoxFromRegion(forRegion)^.font, Round(forRegion^.area.x), Round(forRegion^.area.y))
  else if NOT(assigned(GUIC.activeTextBox)) then
    DrawTextOnScreen(forRegion^.parent^.textBoxes[forRegion^.ElementIndex].contentString, GUIC.globalGUIVectorColor, forRegion^.parent^.textBoxes[forRegion^.ElementIndex].font, Round(forRegion^.area.x), Round(forRegion^.area.y)); 
end;
  
procedure DrawVectorList(forRegion: Region);
var
	tempList: 		 GUIList;
	i, itemIdx: 	 LongInt;
	barHeight: 		 LongInt;
	scrollButtonY: LongInt;
begin
	tempList := ListFromRegion(forRegion);	
	if not assigned(tempList) then exit;

	PushClip(Round(forRegion^.area.x), Round(forRegion^.area.y), forRegion^.area.width + 1, forRegion^.area.Height + 1);
	DrawRectangleOnScreen(GUIC.globalGUIVectorColor, forRegion^.area);
	//FillRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[tempList^.activeItem - tempList^.startingAt]);
	
	//Scrollbuttons
	barHeight := forRegion^.area.height - (tempList^.scrollneg.height *  2);
	scrollButtonY := Round((tempList^.scrollNeg.y + tempList^.scrollNeg.height) + (((tempList^.startingAt) / (Length(tempList^.items) - (tempList^.columns * tempList^.rows)) * barHeight)));
	DrawRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.scrollNeg);
	DrawRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.scrollPos);
	
	if scrollButtonY <= tempList^.scrollPos.y - tempList^.scrollPos.Height then
		FillRectangleOnScreen(GUIC.globalGUIVectorColor, Round(tempList^.scrollNeg.x), scrollButtonY, Round(tempList^.scrollNeg.width), Round(tempList^.scrollNeg.height))
	else
		FillRectangleOnScreen(GUIC.globalGUIVectorColor, Round(tempList^.scrollNeg.x), Round(tempList^.scrollPos.y - tempList^.scrollPos.Height), Round(tempList^.scrollNeg.width), Round(tempList^.scrollNeg.height));
	
	
	for i := Low(tempList^.placeHolder) to High(tempList^.placeHolder) do
	begin
		DrawRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[i]);
			
		itemIdx := i + tempList^.startingAt;
		
		if (itemIdx < 0) OR (itemIdx > High(tempList^.items)) then continue;
		
		if itemIdx <> tempList^.activeItem then
			DrawTextOnscreen(tempList^.items[itemIdx].text, GUIC.globalGUIVectorColor, tempList^.font, round(tempList^.placeHolder[i].x), round(tempList^.placeHolder[i].y))
		else
		begin
			FillRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[i]);
			DrawTextOnscreen(tempList^.items[itemIdx].text, ColorBlack, tempList^.font, Round(tempList^.placeHolder[i].x), Round(tempList^.placeHolder[i].y));
		end;
	end;
	
	PopClip();
end;

procedure DrawAsVectors();
var
  i, j: integer;
begin
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if (GUIC.panels[i]^.visible) then
    begin
    	DrawRectangleOnScreen(GUIC.globalGUIVectorColor, Round(GUIC.panels[i]^.position.x), Round(GUIC.panels[i]^.position.y), GUIC.panels[i]^.width, GUIC.panels[i]^.height);
      
    	for j := Low(GUIC.panels[i]^.Regions) to High(GUIC.panels[i]^.Regions) do
      begin
        case GUIC.panels[i]^.Regions[j].kind of
        gkButton: 		DrawRectangleOnScreen(GUIC.globalGUIVectorColor, GUIC.panels[i]^.Regions[j].area);
        gkLabel: 			DrawTextOnScreen(GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j].elementIndex].contentString, GUIC.GlobalGUIVectorColor, GUIC.panels[i]^.Labels[GUIC.panels[i]^.Regions[j].elementIndex].font, Round(GUIC.panels[i]^.Regions[j].area.x), Round(GUIC.panels[i]^.Regions[j].area.y));
        gkCheckbox: 	DrawVectorCheckbox(@GUIC.panels[i]^.Regions[j]);
        gkRadioGroup: DrawVectorRadioButton(@GUIC.panels[i]^.Regions[j]);
        gkTextbox: 		DrawVectorTextbox(@GUIC.panels[i]^.Regions[j]);
        gkList: 			DrawVectorList(@GUIC.panels[i]^.Regions[j]);
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
    if GUIC.panels[i]^.visible then
      DrawBitmapOnScreen(GUIC.panels[i]^.panelBitmap, GUIC.panels[i]^.position);
  end;
end;
  
procedure DrawPanels();
begin
  if GUIC.VectorDrawing then
    DrawAsVectors()
  else
    DrawAsBitmaps();
end;

//---------------------------------------------------------------------------------------
// Region Code
//---------------------------------------------------------------------------------------

function GetRegionByID(pnl: Panel; ID: String): Region; overload;
var
	idx: Integer;
begin
	result := nil;
	if not assigned(pnl) then exit;
	
	idx := IndexOf(pnl^.regionIds, ID);
	if idx >= 0 then
	begin
    result := @pnl^.regions[idx];
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
  end;
end;

function RegionStringID(r: Region): string;
begin
  if assigned(r) then
    result := r^.stringID
  else
    result := '';
end;

function RegionClicked(pnl: Panel): Region; Overload;
var
	j: LongInt;
	pointClicked: Point2D;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  FinishReadingText();
  GUIC.doneReading := true;
  
  pointClicked := MousePosition();
  
  if pnl = nil then exit;
  if not pnl^.active then exit;
  
  for j := Low(pnl^.Regions) to High(pnl^.Regions) do
  begin
    if PointInRect(pointClicked, pnl^.Regions[j].area) then
    begin
      case pnl^.Regions[j].kind of
        gkCheckBox: 		ToggleCheckboxState(@pnl^.checkboxes[pnl^.Regions[j].elementindex]);
        gkRadioGroup: 	SelectRadioButton(@pnl^.Regions[j]);
        gkTextBox: 			SetAsActiveTextbox(@pnl^.Regions[j]);
        gkList: 				SetActiveListItem(@pnl^.Regions[j], pointClicked);
      end;
      result := @pnl^.Regions[j];
      exit;
    end;
  end;
end;

function RegionClicked(): String;
begin
  result := RegionStringID(GUIC.lastClicked);
end;

function RegionParent(r: Region): Panel;
begin
	if not assigned(r) then exit;
	
	result := r^.parent;
end;

//---------------------------------------------------------------------------------------
// RadioGroup Code
//---------------------------------------------------------------------------------------

function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;
begin
	if not(assigned(RadioGroup)) then exit;
	result := RadioGroup^.activeButton;
end;

function RadioGroupFromRegion(forRegion: Region): GUIRadioGroup;
begin
	if not assigned(forRegion) then exit;
	result := @forRegion^.parent^.radioGroups[forRegion^.elementIndex];
end;

procedure SelectRadioButton(r: Region);
var
  rGroup: GUIRadioGroup;
  i: integer;
begin
	if not assigned(r) then exit;

  rGroup := @r^.parent^.radioGroups[r^.elementIndex];
  
  for i := Low(rGroup^.buttons) to High(rGroup^.buttons) do
  begin
    if rGroup^.buttons[i]^.stringID = r^.StringID then
      rGroup^.activeButton := i;
  end;
end;

//---------------------------------------------------------------------------------------
// Label Code
//---------------------------------------------------------------------------------------

procedure setLabelText(lb: GUILabel; newString: String);
begin
	if not assigned(lb) then exit;	

  lb^.contentString := newString;
end;

procedure setLabelFont(var l: GUILabel; s: String);
begin
	if not assigned(l) then exit;
  l^.font := FontNamed(s);
end;

function LabelFromRegion(r: Region): GUILabel; overload;
begin
	result := nil;
	
	if not assigned(r) then exit;
	if not assigned(r^.parent) then exit;
	if not (r^.kind = gkLabel) then exit;
	if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.labels)) then exit;
  if not (High(r^.parent^.labels) >= r^.elementIndex) then exit;
  
  //Return a pointer to the label in the panel
  result := @r^.parent^.labels[r^.elementIndex]
end;

function LabelString(regID: string): string; Overload;
begin
  if High(GetRegionByID(regID)^.parent^.labels) >= GetRegionByID(regID)^.elementIndex then
    result := GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex].contentString
  else
    result := '';
end;

function LabelString(lb: GUILabel): string; Overload;
begin
  if assigned(lb) then
    result := lb^.contentString;
end;

//---------------------------------------------------------------------------------------
// Textbox Code
//---------------------------------------------------------------------------------------

function TextBoxText(tb: GUITextBox): String;
begin
	if not assigned(tb) then exit;
	result := tb^.contentString;
end;

function LastTextRead(): string;
begin
	result := GUIC.lastTextRead;
end;

procedure FinishReadingText();
begin
  if not assigned(GUIC.activeTextBox) then exit;
  
  GUIC.lastTextRead := GUIC.activeTextBox^.parent^.textBoxes[GUIC.activeTextBox^.elementIndex].contentString;
  GUIC.activeTextBox^.parent^.textBoxes[GUIC.activeTextBox^.elementIndex].contentString := EndReadingText();
  
  GUIC.lastActiveTextBox := GUIC.activeTextBox;
  GUIC.activeTextBox := nil;
end;

function TextboxString(tb: GUITextBox): string; Overload;
begin
  if assigned(tb) then
    result := tb^.contentString;
end;

function TextBoxFromRegion(r: Region): GUITextBox;
begin
	result := nil;
	if not assigned(r) then exit;
	if not assigned(r^.parent) then exit;
	if not (r^.kind = gkTextbox) then exit;
	if (r^.elementIndex < Low(r^.parent^.TextBoxes)) OR (r^.elementIndex > High(r^.parent^.TextBoxes)) then exit;
	
  if High(r^.parent^.TextBoxes) >= r^.elementIndex then
    result := @r^.parent^.TextBoxes[r^.elementIndex]
  else
    result := nil;
end;

procedure SetAsActiveTextbox(r: Region);
var
  textBox: GUITextbox;
begin
  textBox := @r^.parent^.textBoxes[r^.elementIndex];
  
  GUIC.activeTextBox := r;
  
  StartReadingTextWithText(textBox^.contentString, GUIC.globalGUIVectorColor, textBox^.lengthLimit, textBox^.Font, Round(r^.area.x), Round(r^.area.y));
end;

//---------------------------------------------------------------------------------------
// List Code
//---------------------------------------------------------------------------------------

function ListFromRegion(reg: Region): GUIList; overload;
begin
	if High(reg^.parent^.lists) >= reg^.elementIndex then
		result := @reg^.parent^.lists[reg^.elementIndex]
	else
		result := nil;
end;

function ListFromID(idString: String): GUIList;
var
	tempReg: Region;
begin
	tempReg := GetRegionByID(idString);
	if assigned(tempReg) AND (tempReg^.elementIndex < Length(tempReg^.parent^.lists)) AND (tempReg^.elementIndex >= 0) then
		result := @tempReg^.parent^.lists[GetRegionByID(idString)^.elementIndex]
	else
		result := nil;
end;

procedure ListAddItem(lst: GUIList; text: String);
begin
	ListAddItem(lst, nil, text);
end;

procedure ListAddItem(lst: GUIList; img:Bitmap);
begin
	ListAddItem(lst, img, '');
end;

procedure ListAddItem(lst: GUIList; img:Bitmap; text: String); overload;
begin
	  if not assigned(lst) then exit;
	  
	  SetLength(lst^.items, Length(lst^.items) + 1);
	  lst^.items[High(lst^.items)].text 		:= text;	//Assign the text to the item
	  lst^.items[High(lst^.items)].image := img; //Assign the image to the item
end;

procedure ListRemoveItem(lst: GUIList; idx: LongInt);
var
	i: LongInt;
begin
	  if not assigned(lst) then exit;
	  if (idx < 0) or (idx > High(lst^.items)) then exit;
	  
	  for i := idx to High(lst^.items) - 1 do
	  begin
	  	lst^.items[i] := lst^.items[i + 1];
		end;
		
	  SetLength(lst^.items, Length(lst^.items) - 1);
end;

function ListTextIndex(lst: GUIList; value: String): LongInt;
var
	i: LongInt;
begin
	result := -1;
	if not assigned(lst) then exit;
	
	for i := Low(lst^.items) to High(lst^.items) do
	begin
		//find the text... then exit
		if lst^.items[i].text = value then
		begin
			result := i;
			exit;
		end;
	end;
end;

function ListBitmapIndex(lst: GUIList; img: Bitmap): LongInt;
var
	i: LongInt;
begin
	result := -1;
	if not assigned(lst) then exit;
	
	for i := Low(lst^.items) to High(lst^.items) do
	begin
		//find the text... then exit
		if lst^.items[i].image = img then
		begin
			result := i;
			exit;
		end;
	end;
end;

procedure SetActiveListItem(forRegion: region; pointClicked: Point2D);
var
	theList: GUIList;
	i: LongInt;
begin
	theList := ListFromRegion(forRegion);
	
	if PointInRect(pointClicked, theList^.scrollNeg) then
	begin
		if theList^.startingAt >= 1 then
			theList^.startingAt := theList^.startingAt - theList^.columns;
		exit;
	end;
	
	if PointInRect(pointClicked, theList^.scrollPos) then
	begin
		if theList^.startingAt + Length(theList^.placeHolder) <= Length(theList^.items) then
			theList^.startingAt := theList^.startingAt + theList^.columns;
		exit;
	end;
	
	for i := Low(theList^.placeHolder) to High(theList^.placeHolder) do
	begin
		if PointInRect(pointClicked, theList^.placeHolder[i]) then
		begin
			theList^.activeItem := theList^.startingAt + i;
		end;
	end;
end;

function ListActiveItemIndex(lst: GUIList): LongInt;
begin
	result := -1;
	if not assigned(lst) then exit;
	result := lst^.activeItem;
end;

//---------------------------------------------------------------------------------------
// Checkbox Code Code
//---------------------------------------------------------------------------------------

function CheckboxState(ID: String): Boolean;
var
  reg: Region;
begin
  reg := GetRegionByID(ID);
  result := reg^.parent^.checkboxes[reg^.elementIndex].state;
end;

procedure ToggleCheckboxState(c: GUICheckbox);
begin
  c^.state := not c^.state;
end;

//---------------------------------------------------------------------------------------
// Panel Code
//---------------------------------------------------------------------------------------

function PanelShown(p: Panel): boolean;
begin
	result := p^.visible;
end;

procedure AddPanelToGUI(p: Panel);
begin
  SetLength(GUIC.panels, Length(GUIC.panels) + 1);
  GUIC.panels[High(GUIC.panels)] := p;
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
      if PointInRect(MousePosition(), GUIC.panels[i]^.position.x, GUIC.panels[i]^.position.y, GUIC.panels[i]^.width, GUIC.panels[i]^.height) then
      begin
        result := GUIC.panels[i];
        exit;
      end;
    end;
  end;
end;

//=============================================================================
// Create Panels/GUI Elements/Regions etc.
//=============================================================================

function LoadPanel(filename: string): Panel;
var
  pathToFile, line, id, data: string;
  panelFile: text;
  lineNo: integer;
  regionDataArr: Array of String;
   
  procedure CreateLabel(forRegion: Region; d: string);
  var
    newLbl: GUILabelData;
  begin
    newLbl.contentString := ExtractDelimited(7, d, [',']);
    newLbl.font := FontNamed(Trim(ExtractDelimited(8, d, [','])));
    
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
    newRadioGroup: GUIRadioGroupData;
    i: Integer;
    radioGroupID: string;
  begin
    radioGroupID := Trim(ExtractDelimited(7,data,[',']));
    
    for i := Low(result^.radioGroups) to High(result^.radioGroups) do
    begin
      if (radioGroupID = result^.radioGroups[i].GroupID) then
      begin
        AddRegionToGroup(forRegion, @result^.radioGroups[i]);
        forRegion^.elementIndex := i;
        exit;
      end;
    end;
    
    SetLength(newRadioGroup.buttons, 0);
    newRadioGroup.GroupID := radioGroupID;
    AddRegionToGroup(forRegion, @newRadioGroup);
    
    newRadioGroup.activeButton := 0;
    // add to panel, record element index.
    SetLength(result^.radioGroups, Length(result^.radioGroups) + 1);
    result^.radioGroups[High(result^.radioGroups)] := newRadioGroup;
    forRegion^.elementIndex := High(result^.radioGroups);
  end;
  
  procedure CreateCheckbox(forRegion: Region; data: string);
  var
    newChkbox: GUICheckboxData;
  begin
    newChkbox.state := LowerCase(ExtractDelimited(7, data, [','])) = 'true';
    
    SetLength(result^.Checkboxes, Length(result^.Checkboxes) + 1);
    result^.Checkboxes[High(result^.Checkboxes)] := newChkbox;
    forRegion^.elementIndex := High(result^.labels);
  end;
  
  procedure CreateTextbox(r: region; data: string);
  var
    newTextbox: GUITextboxData;
  begin
    newTextbox.font := FontNamed(Trim(ExtractDelimited(7, data, [','])));
    newTextbox.lengthLimit := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newTextBox.contentString := Trim(ExtractDelimited(9, data, [',']));
    
    SetLength(result^.textBoxes, Length(result^.textBoxes) + 1);
    result^.textBoxes[High(result^.textBoxes)] := newTextbox;
    r^.ElementIndex := High(result^.textBoxes);
  end;
  
  procedure CreateList(r: Region; data: string);
  var
    newList: GUIListData;
  begin
    newList.columns 				:= StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList.rows 						:= StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList.activeItem 			:= StrToInt(Trim(ExtractDelimited(9, data, [','])));
    newList.scrollSize  		:= StrToInt(Trim(ExtractDelimited(13, data, [','])));;
    newList.verticalScroll 	:= True;
    newList.font 						:= FontNamed(Trim(ExtractDelimited(12, data, [','])));
    
    newList.startingAt  := 0;
    
    // Calculate col and row sizes
    newList.colWidth 		:= StrToInt(Trim(ExtractDelimited(10, data, [','])));
    newList.rowHeight 	:= StrToInt(Trim(ExtractDelimited(11, data, [','])));
    
    r^.area.width := (newList.columns * newList.colWidth) + newList.scrollSize;
    r^.area.height := (newList.rows * newList.rowHeight);
    
    
    // Set up scroll buttons
    newList.scrollNeg.x := r^.area.x + r^.area.width - newList.scrollSize;
    newList.scrollNeg.y := r^.area.y;
    newList.scrollNeg.width := newList.scrollSize;
    newList.scrollNeg.height := newList.scrollSize;
    
    newList.scrollPos.x := r^.area.x + r^.area.width - newList.scrollSize;
    newList.scrollPos.y := r^.area.y + r^.area.height - newList.scrollSize;
    newList.scrollPos.width := newList.scrollSize;
    newList.scrollPos.height := newList.scrollSize;
    
    SetLength(newList.placeHolder, (newList.columns * newList.rows));
    
    SetLength(newList.items, 0);
    
    SetLength(result^.lists, Length(result^.lists) + 1);
    result^.lists[High(result^.lists)] := newList;
    r^.elementIndex := High(result^.lists);
  end;
  
  procedure CreateListItem(r: region; data: string);
  var
    newListItem: GUIListItem;
    reg: Region;
    pList: GUIList;
    bitmap: String;
  begin
  	newListItem.parent := ListFromRegion(GetRegionByID(Trim(ExtractDelimited(8, data, [',']))));
  
    newListItem.text := Trim(ExtractDelimited(8, data, [',']));
    
    //Load the bitmap or nil if bitmap text is 'n'
    bitmap := Trim(ExtractDelimited(9, data, [',']));
    if bitmap <> 'n' then
    	newListItem.image := BitmapNamed(bitmap)
    else
    	newListItem.image := nil;
    
    reg := GetRegionByID(result, Trim(ExtractDelimited(8, data, [','])));
    pList := ListFromRegion(reg);
    
    SetLength(pList^.items, Length(pList^.items) + 1);
    pList^.items[High(pList^.items)] := newListItem;
  end;
  
  procedure AddRegionToPanelWithString(d: string; p: panel);
  var
    regID: string;
    regX, regY, regW, regH, regKind, addedIdx: integer;
    r: RegionData;
  begin
    regX := StrToInt(Trim(ExtractDelimited(1, d, [','])));
    regY := StrToInt(Trim(ExtractDelimited(2, d, [','])));
    regW := StrToInt(Trim(ExtractDelimited(3, d, [','])));
    regH := StrToInt(Trim(ExtractDelimited(4, d, [','])));
    regKind := StrToInt(Trim(ExtractDelimited(5, d, [','])));
    
    regID := Trim(ExtractDelimited(6, d, [',']));
    
    regX += Round(p^.position.x);
    regY += Round(p^.position.y);
    
    addedIdx := AddName(result^.regionIds, regID);   //Allocate the index
    if High(p^.Regions) < addedIdx then begin RaiseException('Error creating panel - added index is invalid.'); exit; end;

    case regKind of
    	0: r.kind := gkButton;
    	1: r.kind := gkLabel;
    	2: r.kind := gkCheckbox;
    	3: r.kind := gkRadioGroup;
    	4: r.kind := gkTextbox;
    	5: r.kind := gkList;
    end;
    
    r.RegionID 			:= High(p^.Regions);
    r.area 					:= RectangleFrom(regX, regY, regW, regH);
    r.active 				:= true;
    r.stringID 			:= regID;
    r.elementIndex	:= -1;
    r.parent 				:= p;
    
    p^.Regions[addedIdx] := r;
    
    
    case r.kind of
      gkButton: ;
      gkLabel: 			CreateLabel(@p^.Regions[addedIdx],d);
      gkCheckbox: 	CreateCheckbox(@p^.Regions[addedIdx],d);
      gkRadioGroup: CreateRadioButton(@p^.Regions[addedIdx],d);
      gkTextbox: 		CreateTextbox(@p^.Regions[addedIdx],d);
      gkList: 			CreateList(@p^.Regions[addedIdx], d);
    end;    
  end;
  
  procedure StoreRegionData(data: String);
  begin
  	SetLength(regionDataArr, Length(regionDataArr) + 1);
  	regionDataArr[High(regionDataArr)] := data;
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
      'b': begin 
      		   LoadBitmap(Trim(data)); 
      		   result^.panelBitmap := BitmapNamed(Trim(data));
      		 end;
      'r': StoreRegionData(data);
    else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be one of the characters defined in the template.');
        exit;
      end;
    end;
  end;
  
  procedure CreateRegions();
  var
  	i: LongInt;
  begin
  	SetLength(result^.regions, Length(regionDataArr));
  	
  	for i := Low(regionDataArr) to High(regionDataArr) do
  	begin
  		AddRegionToPanelWithString(regionDataArr[i], result);
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
		  if result^.Regions[j].kind = gkList then
		  begin
		  	
		    tempListPtr := @result^.Lists[result^.Regions[j].elementIndex];
		    for k := Low(tempListPtr^.placeHolder) to High(tempListPtr^.placeHolder) do
		    begin
		      tempListPtr^.placeHolder[k].x := (workingCol * tempListPtr^.colWidth) + result^.Regions[j].area.x;
		      tempListPtr^.placeHolder[k].y := (workingRow * tempListPtr^.rowHeight) + result^.Regions[j].area.y;
		      tempListPtr^.placeHolder[k].width := (tempListPtr^.colWidth);
		      tempListPtr^.placeHolder[k].height := (tempListPtr^.rowHeight);
		      		      
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
  
  SetLength(regionDataArr, 0);
  
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
  
  CreateRegions();
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
	GUIC.doneReading := false;
	
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