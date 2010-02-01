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
    contentString:  String;
    font:           Font;
    lengthLimit:    LongInt;
    region:         Region;
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
    
    //The areas for the up/left down/right scrolling buttons
    scrollUp:     Rectangle;
    scrollDown:   Rectangle;
    scrollArea:   Rectangle;
    
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

//---------------------------------------------------------------------------
// Alter GUI global values
//---------------------------------------------------------------------------
procedure GUISetForegroundColor(c:Color);
procedure GUISetBackgroundColor(c:Color);

// Panels
function LoadPanel(filename: string): Panel;
procedure ShowPanel(p: Panel);
procedure AddPanelToGUI(p: Panel);
procedure HidePanel(p: Panel);
procedure DrawPanels();
procedure ToggleShowPanel(p: Panel);
procedure ActivatePanel(p: Panel);
procedure DeactivatePanel(p: Panel);
procedure ToggleActivatePanel(p: Panel);
function PanelShown(p: Panel): boolean;
function PanelClicked(): Panel;

// Regions
function RegionClickedID(): String;
function RegionClicked(): Region;
function RegionID(r: Region): string;
function RegionWithID(pnl: Panel; ID: String): Region; overload;
function RegionWithID(ID: String): Region; overload;
function RegionPanel(r: Region): Panel;


function CheckboxState(s: String): Boolean; overload;
procedure UpdateInterface();
procedure DrawGUIAsVectors(b : boolean);
procedure FinishReadingText();
procedure SetAsActiveTextbox(r: Region); overload;
procedure SetAsActiveTextbox(t: GUITextBox); overload;
  
procedure SetActiveListItem(forRegion: region; pointClicked: Point2D);
procedure SetActiveListItem(lst: GUIList; pointClicked: Point2D);

function RadioGroupFromRegion(r: Region): GUIRadioGroup;
function ListFromRegion(r: Region): GUIList; overload;
function TextBoxFromRegion(r: Region): GUITextBox;
function LabelFromRegion(r: Region): GUILabel;
function CheckboxFromRegion(r: Region): GUICheckbox;

function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;
function ActionRadioButton(grp: GUIRadioGroup): Region; overload;
function ActionRadioButton(r: Region): Region; overload;
procedure SelectRadioButton(r: Region); overload;
procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;


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

function ListFont(r: Region): Font; overload;
function ListFont(lst: GUIList): Font; overload;
function ListItemText(r: Region; idx: LongInt): String; overload;
function ListItemText(lst: GUIList; idx: LongInt): String; overload;

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

function ListActiveItemIndex(lst: GUIList): LongInt;
procedure DeactivateTextBox();
function ActiveTextIndex(): Integer;
function GUITextEntryComplete(): boolean;

function GUITextBoxOfTextEntered(): GUITextbox;
function RegionOfLastUpdatedTextBox(): Region;
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
    foregroundClr:      Color;                  // The color of the foreground
    backgroundClr:      Color;                  // The color of the background
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

function UpdateRegionClicked(pnl: Panel): Region; overload; forward;

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
  DrawRectangleOnScreen(GUIC.foregroundClr, area);
  
  if CheckboxState(forRegion) then 
  begin
    FillRectangleOnScreen(GUIC.foregroundClr, InsetRectangle(area, 2));
  end;
end;

procedure DrawVectorRadioButton(forRegion: Region; const area: Rectangle);
begin
  DrawEllipseOnScreen(GUIC.foregroundClr, area);
  
  if forRegion = ActionRadioButton(forRegion) then
  begin
    FillEllipseOnScreen(GUIC.foregroundClr, InsetRectangle(area, 2));
  end;
end;

procedure DrawVectorTextbox(forRegion: Region; const area: Rectangle);
begin
  DrawRectangleOnScreen(GUIC.foregroundClr, area);
  
  if GUIC.activeTextBox <> forRegion then
    DrawTextOnScreen( TextBoxText(forRegion), 
                      GUIC.foregroundClr, 
                      TextBoxFont(forRegion), 
                      RectangleTopLeft(area));
end;
  
procedure DrawVectorList(forRegion: Region; const area: Rectangle);
var
  tempList:       GUIList;
  i, itemIdx:     LongInt;
  // barHeight:      LongInt;
  // scrollButtonY:  LongInt;
  areaPt:         Point2D;
  itemArea, scrollArea:       Rectangle;
  pct:            Single;
  
  procedure _DrawUpDownArrow(const rect: Rectangle; up: Boolean);
  var
    tri: Triangle;
    innerRect, arrowArea: Rectangle;
  begin
    arrowArea := RectangleOffset(rect, areaPt);
    FillRectangleOnScreen(GUIC.foregroundClr, arrowArea);
    innerRect := InsetRectangle(arrowArea, 2);
    
    if up then
    begin
      tri[0] := RectangleBottomLeft(innerRect);
      tri[1] := RectangleBottomRight(innerRect);
      tri[2] := RectangleCenterTop(innerRect);
    end
    else
    begin
      tri[0] := RectangleTopLeft(innerRect);
      tri[1] := RectangleTopRight(innerRect);
      tri[2] := RectangleCenterBottom(innerRect);
    end;
    
    FillTriangleOnScreen(GUIC.backgroundClr, tri);
  end;
  procedure _DrawLeftRightArrow(const rect: Rectangle; left: Boolean);
  var
    tri: Triangle;
    innerRect, arrowArea: Rectangle;
  begin
    arrowArea := RectangleOffset(rect, areaPt);
    FillRectangleOnScreen(GUIC.foregroundClr, arrowArea);
    innerRect := InsetRectangle(arrowArea, 2);
    
    if left then
    begin
      tri[0] := RectangleCenterLeft(innerRect);
      tri[1] := RectangleBottomRight(innerRect);
      tri[2] := RectangleTopRight(innerRect);
    end
    else
    begin
      tri[0] := RectangleCenterRight(innerRect);
      tri[1] := RectangleTopLeft(innerRect);
      tri[2] := RectangleBottomLeft(innerRect);
    end;
    
    FillTriangleOnScreen(GUIC.backgroundClr, tri);
  end;
// Start DrawVectorList
begin
  tempList := ListFromRegion(forRegion);
  if not assigned(tempList) then exit;
  
  DrawRectangleOnScreen(GUIC.foregroundClr, area);
  
  PushClip(area);
  areaPt := RectangleTopLeft(area);
  
  // Draw the up and down buttons
  if tempList^.verticalScroll then
  begin
    _DrawUpDownArrow(tempList^.scrollUp, true);
    _DrawUpDownArrow(tempList^.scrollDown, false);
  end
  else
  begin
    _DrawLeftRightArrow(tempList^.scrollUp, true);
    _DrawLeftRightArrow(tempList^.scrollDown, false);    
  end;
  
  // Draw the scroll area
  scrollArea := RectangleOffset(tempList^.scrollArea, areaPt);
  
  DrawRectangleOnScreen(GUIC.backgroundClr, scrollArea);
  DrawRectangleOnScreen(GUIC.foregroundClr, scrollArea);
  
  PushClip(scrollArea);
  
  // Draw the scroll position indicator
  
  // if the number of items is >= the number shown then pct := 0
  if tempList^.rows * tempList^.columns >= Length(tempList^.items) then 
    pct := 0
  else 
  begin
    if tempList^.verticalScroll then
      pct := tempList^.startingAt / (Length(tempList^.items) - (tempList^.rows * tempList^.columns) + tempList^.columns)
    else 
      pct := tempList^.startingAt / (Length(tempList^.items) - (tempList^.rows * tempList^.columns) + tempList^.rows);
  end;
  
  if tempList^.verticalScroll then
    FillRectangleOnScreen(GUIC.foregroundClr, 
                          Round(scrollArea.x),
                          Round(scrollArea.y + pct * (scrollArea.Height - tempList^.scrollSize)),
                          tempList^.scrollSize,
                          tempList^.scrollSize
                          )
  else
    FillRectangleOnScreen(GUIC.foregroundClr, 
                          Round(scrollArea.x + pct * (scrollArea.Width - tempList^.scrollSize)),
                          Round(scrollArea.y),
                          tempList^.scrollSize,
                          tempList^.scrollSize
                          );
  PopClip();
  
  // WriteLn('-------');
  //Draw all of the placeholders
  for i := Low(tempList^.placeHolder) to High(tempList^.placeHolder) do
  begin
    itemArea := RectangleOffset(tempList^.placeHolder[i], areaPt);
    
    // Outline the item's area
    DrawRectangleOnScreen(GUIC.foregroundClr, itemArea);
    
    // Find the index of the first item to be shown in the list
    // NOTE: place holders in col then row order 0, 1 -> 2, 3 -> 4, 5 (for 2 cols x 3 rows)
    
    if tempList^.verticalScroll then
      itemIdx := i + tempList^.startingAt
    else
      // 0, 1 => 0, 3
      // 2, 3 => 1, 4
      // 4, 5 => 2, 5
      itemIdx := tempList^.startingAt + ((i mod tempList^.columns) * tempList^.rows) + (i div tempList^.columns);
      
    // WriteLn(' Drawing ', itemIdx);
    
    // Dont draw item details if out of range, but continue to draw outlines
    if (itemIdx < 0) OR (itemIdx > High(tempList^.items)) then continue;
    
    PushClip(itemArea);
    
    if itemIdx <> tempList^.activeItem then
      DrawTextOnScreen(ListItemText(tempList, itemIdx), GUIC.foregroundClr, ListFont(tempList), RectangleTopLeft(itemArea))
    else
    begin
      FillRectangleOnScreen(GUIC.foregroundClr, itemArea);
      DrawTextOnScreen(ListItemText(tempList, itemIdx), GUIC.backgroundClr, ListFont(tempList), RectangleTopLeft(itemArea))
    end;
    
    PopClip();
  end;
  
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
    
    FillRectangleOnScreen(GUIC.backgroundClr, current^.area);
    DrawRectangleOnScreen(GUIC.foregroundClr, current^.area);
    PushClip(current^.area);
    
    for j := High(current^.Regions) downto Low(current^.Regions) do
    begin
      currentReg := @GUIC.panels[i]^.Regions[j];
      case currentReg^.kind of
        gkButton:     DrawRectangleOnScreen(GUIC.foregroundClr, RegionRectangle(currentReg));
        gkLabel:      DrawTextOnScreen( LabelText(currentReg), 
                                        GUIC.foregroundClr, 
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
        gkLabel: ;//      DrawTextOnScreen(LabelText(currentReg), GUIC.foregroundClr, LabelFont(currentReg), RectangleTopLeft(currentReg.area));
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

function RegionWithID(pnl: Panel; ID: String): Region; overload;
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

function RegionWithID(ID: String): Region; overload;
var
  i: integer;
begin
  result := nil;
  
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    result := RegionWithID(GUIC.panels[i], ID);
    if assigned(result) then exit;
  end;
end;

function RegionID(r: Region): string;
begin
  if assigned(r) then
    result := r^.stringID
  else
    result := '';
end;

// Check which of the regions was clicked in this panel
function UpdateRegionClicked(pnl: Panel): Region; overload;
var
  j: LongInt;
  pointClickedInPnl, pointClickedInRegion: Point2D;
  current: Region;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  
  if ReadingText() then
    FinishReadingText();
  
  if pnl = nil then exit;
  if not pnl^.active then exit;
  
  // Adjust the mouse point into this panels area (offset from top left of pnl)
  pointClickedInPnl := PointAdd(MousePosition(), InvertVector(RectangleTopLeft(pnl^.area)));
  
  for j := Low(pnl^.Regions) to High(pnl^.Regions) do
  begin
    // Get the region at the j index
    current := @pnl^.Regions[j];
    
    //Check if it has been clicked
    if PointInRect(pointClickedInPnl, current^.area) then
    begin
      // Adjust the mouse point into the region's coordinates (offset from top left of region)
      pointClickedInRegion := PointAdd(pointClickedInPnl, InvertVector(RectangleTopLeft(current^.area)));
      
      // Perform kind based updating
      case pnl^.Regions[j].kind of
        gkCheckBox:     ToggleCheckboxState ( CheckboxFromRegion(current)   );
        gkRadioGroup:   SelectRadioButton   ( RadioGroupFromRegion(current), current );
        gkTextBox:      SetAsActiveTextbox  ( TextBoxFromRegion(current)    );
        gkList:         SetActiveListItem   ( ListFromRegion(current), pointClickedInRegion);
      end;
      
      // Return the clicked region
      result := current;
      exit;
    end;
  end;
end;

function RegionClicked(): region;
begin
     result := GUIC.lastClicked;
end;

function RegionClickedID(): String;
begin
  result := RegionID(GUIC.lastClicked);
end;

function RegionPanel(r: Region): Panel;
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

procedure SelectRadioButton(r: Region); overload;
begin
  SelectRadioButton(RadioGroupFromRegion(r), r);
end;

procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;
var
  i: integer;
begin
  if not assigned(rGroup) then exit;
  if RadioGroupFromRegion(r) <> rGroup then exit;
    
  // Remove the selection ...
  rGroup^.activeButton := -1;
  
  // Find this button in the group and select it
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
begin
  SetAsActiveTextbox(TextBoxFromRegion(r));
end;

procedure SetAsActiveTextbox(t: GUITextbox);
begin
  if not assigned(t) then exit;
  
  GUIC.activeTextBox := t^.region;
  
  if ReadingText() then FinishReadingText();
  
  StartReadingTextWithText( t^.contentString, 
                            GUIC.foregroundClr, 
                            t^.lengthLimit, 
                            t^.Font, 
                            RectangleTopLeft(RegionRectangle(t^.region)));
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

function ListFont(r: Region): Font; overload;
begin
  result := ListFont(ListFromRegion(r));
end;

function ListFont(lst: GUIList): Font; overload;
begin
  result := nil;
  if not assigned(lst) then exit;
  
  result := lst^.font;
end;

function ListItemText(r: Region; idx: LongInt): String; overload;
begin
  result := ListItemText(ListFromRegion(r), idx);
end;

function ListItemText(lst: GUIList; idx: LongInt): String; overload;
begin
  result := '';
  if not assigned(lst) then exit;
  if (idx < 0) or (idx > High(lst^.items)) then exit;
  
  result := lst^.items[idx].text;
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
begin
  SetActiveListItem(ListFromRegion(forRegion), pointClicked);
end;

procedure SetActiveListItem(lst: GUIList; pointClicked: Point2D);
var
  i, inc: LongInt;
begin
  if not assigned(lst) then exit;
  
  // WriteLn(PointToString(pointClicked));
  
  if lst^.verticalScroll then
    inc := lst^.columns
  else
    inc := lst^.rows;
  
  // Write(lst^.startingAt);
  
  if PointInRect(pointClicked, lst^.scrollUp) then
  begin
    if lst^.startingAt >= inc then 
      lst^.startingAt := lst^.startingAt - inc;
    // WriteLn(' -> ', lst^.startingAt);
    exit;
  end;
  
  if PointInRect(pointClicked, lst^.scrollDown) then
  begin
    if lst^.startingAt + inc <= Length(lst^.items) then
      lst^.startingAt := lst^.startingAt + inc;
    // WriteLn(' -> ', lst^.startingAt);
    exit;
  end;
  
  // WriteLn(' -> ', lst^.startingAt);
  
  for i := Low(lst^.placeHolder) to High(lst^.placeHolder) do
  begin
    if PointInRect(pointClicked, lst^.placeHolder[i]) then
    begin
      lst^.activeItem := lst^.startingAt + i;
      exit;
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

function CheckboxState(s: String): Boolean; overload;
begin
  result := CheckboxState(CheckboxFromRegion(RegionWithID(s)));
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
  
  for i := High(GUIC.visiblePanels) downto Low(GUIC.visiblePanels) do
  begin
    if PointInRect(MousePosition(), GUIC.panels[i]^.area) then
    begin
      result := GUIC.panels[i];
      exit;
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
    //Format is
    // 1, 2, 3, 4, 5, 6,      7,    8,      9, 
    // x, y, w, h, 5, ListID, font, maxLen, text
    newTextbox.font           := FontNamed(Trim(ExtractDelimited(7, data, [','])));
    newTextbox.lengthLimit    := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newTextBox.contentString  := Trim(ExtractDelimited(9, data, [',']));
    newTextBox.region         := r;
    
    SetLength(result^.textBoxes, Length(result^.textBoxes) + 1);
    result^.textBoxes[High(result^.textBoxes)] := newTextbox;
    r^.ElementIndex := High(result^.textBoxes);
  end;
  
  procedure CreateList(r: Region; data: string);
  var
    newList: GUIListData;
    scrollSz, rhs, btm, height, width: LongInt;
  begin
    //Format is
    // 1, 2, 3, 4, 5, 6,      7,       8,    9,          10,     11,         12
    // x, y, w, h, 5, ListID, Columns, Rows, ActiveItem, fontID, scrollSize, scrollKind
    
    newList.columns         := StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList.rows            := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList.activeItem      := StrToInt(Trim(ExtractDelimited(9, data, [','])));
    newList.scrollSize      := StrToInt(Trim(ExtractDelimited(11, data, [','])));;
    newList.verticalScroll  := LowerCase(Trim(ExtractDelimited(12, data, [',']))) = 'v';
    newList.font            := FontNamed(Trim(ExtractDelimited(10, data, [','])));
    
    scrollSz := newList.scrollSize;
    
    // Start at the fist item in the list, or the activeItem
    if newList.activeItem = -1 then
      newList.startingAt    := 0
    else
      newList.startingAt    := newList.activeItem;
    
    rhs     := r^.area.width - scrollSz;
    btm     := r^.area.height - scrollSz;
    height  := r^.area.height;
    width   := r^.area.width;
    
    // Calculate col and row sizes
    if newList.verticalScroll then
    begin
      newList.colWidth    := rhs    div newList.columns;
      newList.rowHeight   := height div newList.rows;
      
      // Set scroll buttons
      newList.scrollUp    := RectangleFrom( rhs, 0, scrollSz, scrollSz);
      newList.scrollDown  := RectangleFrom( rhs, btm, scrollSz, scrollSz);
      
      newList.scrollArea  := RectangleFrom( rhs, scrollSz, scrollSz, height - (2 * scrollSz));
    end
    else
    begin
      newList.colWidth    := r^.area.width div newList.columns;
      newList.rowHeight   := btm div newList.rows;
      
      // Set scroll buttons
      newList.scrollUp    := RectangleFrom( 0, btm, scrollSz, scrollSz);
      newList.scrollDown  := RectangleFrom( rhs, btm, scrollSz, scrollSz);
      
      newList.scrollArea  := RectangleFrom( scrollSz, btm, width - (2 * scrollSz), scrollSz);
    end;
    
    
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
    newListItem.parent := ListFromRegion(RegionWithID(Trim(ExtractDelimited(8, data, [',']))));
  
    newListItem.text := Trim(ExtractDelimited(8, data, [',']));
    
    //Load the bitmap or nil if bitmap text is 'n'
    bitmap := Trim(ExtractDelimited(9, data, [',']));
    if bitmap <> 'n' then
      newListItem.image := BitmapNamed(bitmap)
    else
      newListItem.image := nil;
    
    reg := RegionWithID(result, Trim(ExtractDelimited(8, data, [','])));
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
    // Format is 
    // x, y, w, h, kind, id
    regX := StrToFloat(Trim(ExtractDelimited(1, d, [','])));
    regY := StrToFloat(Trim(ExtractDelimited(2, d, [','])));
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
  
  procedure SetupListPlaceholders();
  var
    tempListPtr: GUIList;
    workingCol, workingRow: LongInt;
    j, k: LongInt;
    
  begin
    workingCol := 0;
    workingRow := 0;
    
    for j := Low(result^.Regions) to High(result^.Regions) do
    begin
      //Get the region as a list (will be nil if not list...)
      tempListPtr := ListFromRegion(@result^.regions[j]);
      
      if assigned(tempListPtr) then
      begin
        for k := Low(tempListPtr^.placeHolder) to High(tempListPtr^.placeHolder) do
        begin
          tempListPtr^.placeHolder[k].x := (workingCol * tempListPtr^.colWidth);
          tempListPtr^.placeHolder[k].y := (workingRow * tempListPtr^.rowHeight);
          
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
  SetupListPlaceholders();
end;

procedure GUISetForegroundColor(c:Color);
begin
  GUIC.foregroundClr := c;
end;

procedure GUISetBackgroundColor(c:Color);
begin
  GUIC.backgroundClr := c;
end;

procedure DrawGUIAsVectors(b : boolean);
begin
  GUIC.VectorDrawing := b;
end;

function PanelActive(pnl: Panel): Boolean;
begin
  if assigned(pnl) then result := pnl^.active
  else result := false;
end;

procedure UpdateInterface();
var
  pnl: Panel;
begin
  GUIC.doneReading := false;
  
  pnl := PanelClicked();
  if PanelActive(pnl) then
    GUIC.lastClicked := UpdateRegionClicked(pnl)
  else
    GUIC.lastClicked := nil;
    
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
