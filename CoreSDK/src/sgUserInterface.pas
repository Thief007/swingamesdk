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
//              Lists are almost completed. Can be created and used, and items can be added at runtime.
//              Need to add the ability to remove items from lists.
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
    font:           Font;
    lengthLimit:    LongInt;
  end;
  
  GUILabelData = record
    contentString:  string;
    font:         Font;
  end;
  
  /// Each list item has text and an image
  GUIListItem = record
    text:     String;
    image:    Bitmap;
    parent:   GUIList;
  end;

  
  GUIListData = record
    verticalScroll: Boolean;
    scrollUp:    Rectangle;
    scrollDown:    Rectangle;
    
    columns:      LongInt;
    rows:         LongInt;
    
    rowHeight:    LongInt;
    colWidth:     LongInt;
    scrollSize:   LongInt;
    placeholder:  Array of Rectangle;
    
    activeItem:   LongInt;
    startingAt:   LongInt;

    font:       Font;
        
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
    kind:           GUIElementKind;
    regionID:       LongInt;
    elementIndex:   LongInt;
    area:           Rectangle;
    active:         Boolean;
    
    parent:         Panel;
  end;
  
  PanelData = record
    stringID:     string;
    panelID:      LongInt;
    area:         Rectangle;
    //position:     Point2D;
    //width:        LongInt;
    //height:       LongInt;
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

function RadioGroupFromRegion(r: Region): GUIRadioGroup;
function ListFromRegion(r: Region): GUIList; overload;
function TextBoxFromRegion(r: Region): GUITextBox;
function LabelFromRegion(r: Region): GUILabel;
function CheckboxFromRegion(r: Region): GUICheckbox;

function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;
procedure SelectRadioButton(r: Region);
function ActionRadioButton(grp: GUIRadioGroup): Region; overload;
function ActionRadioButton(r: Region): Region; overload;

function GetRegionByID(pnl: Panel; ID: String): Region; overload;
function GetRegionByID(ID: String): Region; overload;

function CheckboxState(chk: GUICheckbox): Boolean; overload;
function CheckboxState(r: Region): Boolean; overload;
procedure CheckboxSetState(chk: GUICheckbox; val: Boolean);

procedure ToggleCheckboxState(c: GUICheckbox);


function TextBoxFont(tb: GUITextBox): Font; overload;
function TextBoxFont(r: Region): Font; overload;

function TextBoxText(r: Region): String; overload;
function TextBoxText(tb: GUITextBox): String; overload;

procedure TextboxSetText(r: Region; s: string); overload;
procedure TextboxSetText(tb: GUITextBox; s: string); overload;


function ActiveTextBoxParent() : Panel;



function LabelFont(l: GUILabel): Font; overload;
function LabelFont(r: Region): Font; overload;
procedure LabelSetFont(l: GUILabel; s: String);

function LabelText(lb: GUILabel): string; overload;
function LabelText(r: Region): string; overload;
procedure LabelSetText(lb: GUILabel; newString: String);

procedure DeactivateTextBox();
function ActiveTextIndex(): Integer;
function GUITextEntryComplete(): boolean;
function RegionParent(r: Region): Panel;
function GUITextBoxOfTextEntered(): GUITextbox;
function RegionOfLastUpdatedTextBox(): Region;
function PanelShown(p: Panel): boolean;
function IndexOfLastUpdatedTextBox(): Integer;

//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection,   // libsrc
    sgShared, sgResources, sgTrace, sgImages, sgGraphics, sgCore, sgGeometry, sgText, sgInput;
//=============================================================================

type
  GUIController = record
    panels:             Array of Panel;         // The panels that are loaded into the GUI
    panelIds:           NamedIndexCollection;
    visiblePanels:      Array of Panel;         // The panels that are currently visible (in reverse z order - back to front)
    globalGUIFont:      Font;
    globalGUIVectorColor: Color;
    VectorDrawing:      Boolean;
    lastClicked:        Region;
    activeTextBox:      Region;
    lastActiveTextBox:  Region;
    doneReading:        Boolean;
    lastTextRead:       String;                 // The text that was in the most recently changed textbox before it was changed.
  end;
  
var
  // _Panels: TStringHash;
  GUIC: GUIController;

function GUITextEntryComplete(): boolean;
begin
  result := GUIC.doneReading;
end;

function GUITextBoxOfTextEntered(): GUITextbox;
begin
  result := nil;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := @GUIC.lastActiveTextBox^.parent^.textBoxes[GUIC.lastActiveTextBox^.elementIndex];
end;

function RegionOfLastUpdatedTextBox(): Region;
begin
  result := nil;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := GUIC.lastActiveTextBox;
end;

function IndexOfLastUpdatedTextBox(): LongInt;
begin
  result := -1;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := RegionOfLastUpdatedTextBox()^.elementIndex;
end;

function ActiveTextIndex(): LongInt;
begin
  result := -1;
  if not assigned(GUIC.activeTextBox) then exit;
  result := GUIC.activeTextBox^.elementIndex;
end;

procedure DeactivateTextBox();
begin 
  GUIC.activeTextBox := nil;
end;

function ActiveTextBoxParent() : Panel;
begin
  result := GUIC.activeTextBox^.parent;
end;

function RegionRectangle(r: Region): Rectangle;
begin
  if not assigned(r) then begin result := RectangleFrom(0,0,0,0); exit; end;
  
  result := RectangleOffset(r^.area, RectangleTopLeft(r^.parent^.area));
end;

//---------------------------------------------------------------------------------------
// Drawing Loops
//---------------------------------------------------------------------------------------  
  
procedure DrawVectorCheckbox(forRegion: Region; const area: Rectangle);
begin
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, area);
  
  if CheckboxState(forRegion) then 
  begin
    FillRectangleOnScreen(GUIC.globalGUIVectorColor, InsetRectangle(area, 2));
  end;
end;

procedure DrawVectorRadioButton(forRegion: Region; const area: Rectangle);
begin
  DrawEllipseOnScreen(GUIC.globalGUIVectorColor, area);
  
  if forRegion = ActionRadioButton(forRegion) then
  begin
    FillEllipseOnScreen(GUIC.globalGUIVectorColor, InsetRectangle(area, 2));
  end;
end;

procedure DrawVectorTextbox(forRegion: Region; const area: Rectangle);
begin
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, area);
  
  if GUIC.activeTextBox <> forRegion then
    DrawTextOnScreen( TextBoxText(forRegion), 
                      GUIC.globalGUIVectorColor, 
                      TextBoxFont(forRegion), 
                      RectangleTopLeft(area));
end;
  
procedure DrawVectorList(forRegion: Region; const area: Rectangle);
var
  tempList:      GUIList;
  i, itemIdx:    LongInt;
  barHeight:     LongInt;
  scrollButtonY: LongInt;
  areaPt:       Point2D;
begin
  tempList := ListFromRegion(forRegion);
  if not assigned(tempList) then exit;
  
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, area);
  PushClip(area);
  
  areaPt := InvertVector(RectangleTopLeft(area));
  
  //FillRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[tempList^.activeItem - tempList^.startingAt]);
  
  // //Scrollbuttons
  // barHeight := area.height - (tempList^.scrollUp.height *  2);
  // scrollButtonY := Round((tempList^.scrollUp.y + tempList^.scrollUp.height) + (((tempList^.startingAt) / (Length(tempList^.items) - (tempList^.columns * tempList^.rows)) * barHeight)));
  
  // Draw the up and down buttons
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, RectangleOffset(tempList^.scrollUp, areaPt));
  DrawRectangleOnScreen(GUIC.globalGUIVectorColor, RectangleOffset(tempList^.scrollDown, areaPt));
  
  // if scrollButtonY <= tempList^.scrollDown.y - tempList^.scrollDown.Height then
  //   FillRectangleOnScreen(GUIC.globalGUIVectorColor, Round(tempList^.scrollUp.x), scrollButtonY, Round(tempList^.scrollUp.width), Round(tempList^.scrollUp.height))
  // else
  //   FillRectangleOnScreen(GUIC.globalGUIVectorColor, Round(tempList^.scrollUp.x), Round(tempList^.scrollDown.y - tempList^.scrollDown.Height), Round(tempList^.scrollUp.width), Round(tempList^.scrollUp.height));
  // 
  // for i := Low(tempList^.placeHolder) to High(tempList^.placeHolder) do
  // begin
  //   DrawRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[i]);
  //     
  //   itemIdx := i + tempList^.startingAt;
  //   
  //   if (itemIdx < 0) OR (itemIdx > High(tempList^.items)) then continue;
  //   
  //   if itemIdx <> tempList^.activeItem then
  //     DrawTextOnscreen(tempList^.items[itemIdx].text, GUIC.globalGUIVectorColor, tempList^.font, round(tempList^.placeHolder[i].x), round(tempList^.placeHolder[i].y))
  //   else
  //   begin
  //     FillRectangleOnScreen(GUIC.globalGUIVectorColor, tempList^.placeHolder[i]);
  //     DrawTextOnscreen(tempList^.items[itemIdx].text, ColorBlack, tempList^.font, Round(tempList^.placeHolder[i].x), Round(tempList^.placeHolder[i].y));
  //   end;
  // end;
  
  PopClip();
end;

procedure DrawAsVectors();
var
  i, j: integer;
  current: Panel;
  currentReg: Region;
begin
  for i := Low(GUIC.visiblePanels) to High(GUIC.visiblePanels) do
  begin
    current := GUIC.visiblePanels[i];
    
    DrawRectangleOnScreen(GUIC.globalGUIVectorColor, current^.area);
    PushClip(current^.area);
    
    for j := High(current^.Regions) downto Low(current^.Regions) do
    begin
      currentReg := @GUIC.panels[i]^.Regions[j];
      case currentReg^.kind of
        gkButton:     DrawRectangleOnScreen(GUIC.globalGUIVectorColor, RegionRectangle(currentReg));
        gkLabel:      DrawTextOnScreen( LabelText(currentReg), 
                                        GUIC.GlobalGUIVectorColor, 
                                        LabelFont(currentReg), 
                                        RectangleTopLeft(RegionRectangle(currentReg)));
        gkCheckbox:   DrawVectorCheckbox(currentReg, RegionRectangle(currentReg));
        gkRadioGroup: DrawVectorRadioButton(currentReg, RegionRectangle(currentReg));
        gkTextbox:    DrawVectorTextbox(currentReg, RegionRectangle(currentReg));
        gkList:       DrawVectorList(currentReg, RegionRectangle(currentReg));
      end;
    end;
    
    PopClip();
  end;
end;

procedure DrawAsBitmaps();
var
  i, j: integer;
begin
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    if GUIC.panels[i]^.visible then
      DrawBitmapOnScreen(GUIC.panels[i]^.panelBitmap, RectangleTopLeft(GUIC.panels[i]^.area));
      
    for j := Low(GUIC.panels[i]^.Regions) to High(GUIC.panels[i]^.Regions) do
    begin
      case GUIC.panels[i]^.Regions[j].kind of
        gkLabel: ;//      DrawTextOnScreen(LabelText(currentReg), GUIC.GlobalGUIVectorColor, LabelFont(currentReg), RectangleTopLeft(currentReg.area));
      end;
    end;
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

// Check which of the regions was clicked in this panel
function RegionClicked(pnl: Panel): Region; overload;
var
  j: LongInt;
  pointClicked: Point2D;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  
  if ReadingText() then
    FinishReadingText();
  
  //Adjust the mouse point into this panels area
  pointClicked := PointAdd(MousePosition(), InvertVector(RectangleTopLeft(pnl^.area)));
  
  if pnl = nil then exit;
  if not pnl^.active then exit;
  
  for j := Low(pnl^.Regions) to High(pnl^.Regions) do
  begin
    if PointInRect(pointClicked, pnl^.Regions[j].area) then
    begin
      case pnl^.Regions[j].kind of
        gkCheckBox:     ToggleCheckboxState(@pnl^.checkboxes[pnl^.Regions[j].elementindex]);
        gkRadioGroup:   SelectRadioButton(@pnl^.Regions[j]);
        gkTextBox:      SetAsActiveTextbox(@pnl^.Regions[j]);
        gkList:         SetActiveListItem(@pnl^.Regions[j], pointClicked);
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
// Get element from region
//---------------------------------------------------------------------------------------

function RadioGroupFromRegion(r: Region): GUIRadioGroup;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkRadioGroup) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.radioGroups)) then exit;

  result := @r^.parent^.radioGroups[r^.elementIndex];
end;

function LabelFromRegion(r: Region): GUILabel; overload;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkLabel) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.labels)) then exit;
  
  //Return a pointer to the label in the panel
  result := @r^.parent^.labels[r^.elementIndex]
end;

function TextBoxFromRegion(r: Region): GUITextBox;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkTextbox) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.textBoxes)) then exit;
  
  result := @r^.parent^.textBoxes[r^.elementIndex]
end;

function ListFromRegion(r: Region): GUIList; overload;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkList) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.lists)) then exit;
  
  result := @r^.parent^.lists[r^.elementIndex]
end;

function CheckboxFromRegion(r: Region): GUICheckbox;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkCheckBox) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.checkboxes)) then exit;
  
  result := @r^.parent^.checkboxes[r^.elementIndex];
end;


//---------------------------------------------------------------------------------------
// RadioGroup Code
//---------------------------------------------------------------------------------------

function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;
begin
  result := -1;
  if not(assigned(RadioGroup)) then exit;
  result := RadioGroup^.activeButton;
end;

function ActionRadioButton(r: Region): Region;
begin
  result := ActionRadioButton(RadioGroupFromRegion(r));
end;

function ActionRadioButton(grp: GUIRadioGroup): Region;
begin
  result := nil;
  if not assigned(grp) then exit;
  if (grp^.activeButton < 0) or (grp^.activeButton > High(grp^.buttons)) then exit;
  
  result := grp^.buttons[grp^.activeButton];
end;

procedure SelectRadioButton(r: Region);
var
  rGroup: GUIRadioGroup;
  i: integer;
begin
  rGroup := RadioGroupFromRegion(r);
  if not assigned(rGroup) then exit;
  
  rGroup^.activeButton := -1;
  
  for i := Low(rGroup^.buttons) to High(rGroup^.buttons) do
  begin
    if rGroup^.buttons[i] = r then
    begin
      rGroup^.activeButton := i;
      exit;
    end;
  end;
end;

//---------------------------------------------------------------------------------------
// Label Code
//---------------------------------------------------------------------------------------

procedure LabelSetText(lb: GUILabel; newString: String);
begin
  if not assigned(lb) then exit;  
  
  lb^.contentString := newString;
end;

procedure LabelSetFont(l: GUILabel; s: String);
begin
  if not assigned(l) then exit;
  l^.font := FontNamed(s);
end;

function LabelFont(r: Region): Font; overload;
begin
  result := LabelFont(LabelFromRegion(r));
end;

function LabelFont(l: GUILabel): Font;
begin
  result := nil;
  if not assigned(l) then exit;
  
  result := l^.font;
end;

// function LabelText(regID: string): string; overload;
// begin
//   if High(GetRegionByID(regID)^.parent^.labels) >= GetRegionByID(regID)^.elementIndex then
//     result := GetRegionByID(regID)^.parent^.labels[GetRegionByID(regID)^.elementIndex].contentString
//   else
//     result := '';
// end;
// 

function LabelText(lb: GUILabel): String; overload;
begin
  if assigned(lb) then
    result := lb^.contentString;
end;

function LabelText(r: Region): String; overload;
begin
  result := LabelText(LabelFromRegion(r));
end;

//---------------------------------------------------------------------------------------
// Textbox Code
//---------------------------------------------------------------------------------------


function TextBoxFont(r: Region): Font; overload;
begin
  result := TextBoxFont(TextBoxFromRegion(r));
end;

function TextBoxFont(tb: GUITextBox): Font; overload;
begin
  result := nil;
  if not assigned(tb) then exit;
  
  result := tb^.font;
end;

procedure TextboxSetText(r: Region; s: string); overload;
begin
  TextboxSetText(TextBoxFromRegion(r), s);
end;

procedure TextboxSetText(tb: GUITextBox; s: string); overload;
begin
  if assigned(tb) then
    tb^.contentString := s;
end;

function TextBoxText(r: Region): String; overload;
begin
  result := TextBoxText(TextBoxFromRegion(r));
end;

function TextBoxText(tb: GUITextBox): String; overload;
begin
  result := '';
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
  
  GUIC.lastTextRead := TextBoxText(GUIC.activeTextBox);
  TextboxSetText(GUIC.activeTextBox, EndReadingText());
  
  GUIC.lastActiveTextBox := GUIC.activeTextBox;
  GUIC.activeTextBox := nil;
  GUIC.doneReading := true;
end;

procedure SetAsActiveTextbox(r: Region);
var
  textBox: GUITextbox;
begin
  textBox := TextBoxFromRegion(r);
  if not assigned(textBox) then exit;
  
  GUIC.activeTextBox := r;
  
  StartReadingTextWithText( textBox^.contentString, 
                            GUIC.globalGUIVectorColor, 
                            textBox^.lengthLimit, 
                            textBox^.Font, 
                            RectangleTopLeft(RegionRectangle(r)));
end;

//---------------------------------------------------------------------------------------
// List Code
//---------------------------------------------------------------------------------------

// function ListFromID(idString: String): GUIList;
// var
//   tempReg: Region;
// begin
//   tempReg := GetRegionByID(idString);
//   if assigned(tempReg) AND (tempReg^.elementIndex < Length(tempReg^.parent^.lists)) AND (tempReg^.elementIndex >= 0) then
//     result := @tempReg^.parent^.lists[GetRegionByID(idString)^.elementIndex]
//   else
//     result := nil;
// end;

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
    lst^.items[High(lst^.items)].text     := text;  //Assign the text to the item
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
  
  if PointInRect(pointClicked, theList^.scrollUp) then
  begin
    if theList^.startingAt >= 1 then
      theList^.startingAt := theList^.startingAt - theList^.columns;
    exit;
  end;
  
  if PointInRect(pointClicked, theList^.scrollDown) then
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

function CheckboxState(chk: GUICheckbox): Boolean; overload;
begin
  if not assigned(chk) then begin result := false; exit; end;
  
  result := chk^.state;
end;

function CheckboxState(r: Region): Boolean; overload;
begin
  result := CheckboxState(CheckboxFromRegion(r));
end;

procedure CheckboxSetState(chk: GUICheckbox; val: Boolean);
begin
  if not assigned(chk) then exit;
  
  chk^.state := val;
end;


// function CheckboxState(ID: String): Boolean;
// var
//   reg: Region;
// begin
//   reg := GetRegionByID(ID);
//   result := reg^.parent^.checkboxes[reg^.elementIndex].state;
// end;

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
  if assigned(p) and (not p^.visible) then ToggleShowPanel(p);
end;

procedure HidePanel(p: Panel);
begin
  if assigned(p) and (p^.visible) then ToggleShowPanel(p);
end;

procedure ToggleShowPanel(p: Panel);
var
  i: Integer;
  found: Boolean;
begin
  if assigned(p) then
  begin
    p^.visible := not p^.visible;
    
    if p^.visible then
    begin
      // Now visible so add to the visible panels
      SetLength(GUIC.visiblePanels, Length(GUIC.visiblePanels) + 1);
      GUIC.visiblePanels[High(GUIC.visiblePanels)] := p;
    end
    else
    begin
      // No longer visible - remove from visible panels
      found := false;
      
      // Search for panel and remove from visible panels
      for i := 0 to High(GUIC.visiblePanels) do
      begin
        if (not found) then
        begin
          if (p = GUIC.visiblePanels[i]) then found := true;
        end
        else // found - so copy over...
          GUIC.visiblePanels[i - 1] := GUIC.visiblePanels[i]
      end;
      
      SetLength(GUIC.visiblePanels, Length(GUIC.visiblePanels) - 1);
    end;
  end;
end;


function PanelClicked(): Panel;
var
  i: integer;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  
  for i := Low(GUIC.visiblePanels) to High(GUIC.visiblePanels) do
  begin
    if (GUIC.visiblePanels[i]^.active) then
    begin
      if PointInRect(MousePosition(), GUIC.panels[i]^.area) then
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
    newList.columns         := StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList.rows            := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList.activeItem      := StrToInt(Trim(ExtractDelimited(9, data, [','])));
    newList.scrollSize      := StrToInt(Trim(ExtractDelimited(13, data, [','])));;
    newList.verticalScroll  := True;
    newList.font            := FontNamed(Trim(ExtractDelimited(12, data, [','])));
    
    newList.startingAt  := 0;
    
    // Calculate col and row sizes
    newList.colWidth    := StrToInt(Trim(ExtractDelimited(10, data, [','])));
    newList.rowHeight   := StrToInt(Trim(ExtractDelimited(11, data, [','])));
    
    r^.area.width := (newList.columns * newList.colWidth) + newList.scrollSize;
    r^.area.height := (newList.rows * newList.rowHeight);
    
    
    // Set up scroll buttons
    newList.scrollUp.x := r^.area.x + r^.area.width - newList.scrollSize;
    newList.scrollUp.y := r^.area.y;
    newList.scrollUp.width := newList.scrollSize;
    newList.scrollUp.height := newList.scrollSize;
    
    newList.scrollDown.x := r^.area.x + r^.area.width - newList.scrollSize;
    newList.scrollDown.y := r^.area.y + r^.area.height - newList.scrollSize;
    newList.scrollDown.width := newList.scrollSize;
    newList.scrollDown.height := newList.scrollSize;
    
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
    regX, regY: Single;
    regW, regH, regKind, addedIdx: integer;
    r: RegionData;
  begin
    regX := StrToInt(Trim(ExtractDelimited(1, d, [','])));
    regY := StrToInt(Trim(ExtractDelimited(2, d, [','])));
    regW := StrToInt(Trim(ExtractDelimited(3, d, [','])));
    regH := StrToInt(Trim(ExtractDelimited(4, d, [','])));
    regKind := StrToInt(Trim(ExtractDelimited(5, d, [','])));
    
    regID := Trim(ExtractDelimited(6, d, [',']));
    
    // this should be done when used... so that moving the panel effects this
    //
    // regX += p^.area.x;
    // regY += p^.area.y;
    
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
    
    r.RegionID      := High(p^.Regions);
    r.area          := RectangleFrom(regX, regY, regW, regH);
    r.active        := true;
    r.stringID      := regID;
    r.elementIndex  := -1;
    r.parent        := p;
    
    p^.Regions[addedIdx] := r;
    
    
    case r.kind of
      gkButton: ;
      gkLabel:      CreateLabel(@p^.Regions[addedIdx],d);
      gkCheckbox:   CreateCheckbox(@p^.Regions[addedIdx],d);
      gkRadioGroup: CreateRadioButton(@p^.Regions[addedIdx],d);
      gkTextbox:    CreateTextbox(@p^.Regions[addedIdx],d);
      gkList:       CreateList(@p^.Regions[addedIdx], d);
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
      'x': result^.area.x       := MyStrToInt(data, false);
      'y': result^.area.y       := MyStrToInt(data, false);
      'w': result^.area.width   := MyStrToInt(data, false);
      'h': result^.area.height  := MyStrToInt(data, false);
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
  begin
    New(result);
    result^.stringID := '';
    result^.panelBitmap := nil;
    result^.panelID := -1;
    result^.area := RectangleFrom(0,0,0,0);
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
    GUIC.lastActiveTextBox := nil;
    
    // _Panels := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'Initialise');
    {$ENDIF}
  end;

end.