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
    alignment:      FontAlignment;
  end;
  
  GUILabelData = record
    contentString:  String;
    font:           Font;
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
    font:         Font;
    items:        Array of GUIListItem;
    scrollButton: Bitmap;
    alignment:    FontAlignment;
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
    stringID:             string;
    panelID:              LongInt;
    area:                 Rectangle;
    visible:              boolean;
    active:               boolean;
    panelBitmapInactive:  Bitmap;
    panelBitmapActive:    Bitmap;
    regions:              Array of RegionData;
    regionIds:            NamedIndexCollection;
    labels:               Array of GUILabelData;
    checkBoxes:           Array of GUICheckboxData;
    radioGroups:          Array of GUIRadioGroupData;
    textBoxes:            Array of GUITextBoxData;
    lists:                Array of GUIListData;
    draggable:            Boolean;
  end;

//---------------------------------------------------------------------------
// Alter GUI global values
//---------------------------------------------------------------------------
procedure GUISetForegroundColor(c:Color);
procedure GUISetBackgroundColor(c:Color);

// Panels
function LoadPanel(filename: string): Panel;
procedure AddPanelToGUI(p: Panel);
procedure ShowPanel(p: Panel);
procedure HidePanel(p: Panel);
procedure ToggleShowPanel(p: Panel);
procedure DrawPanels();
procedure ActivatePanel(p: Panel);
procedure DeactivatePanel(p: Panel);
procedure ToggleActivatePanel(p: Panel);
function PanelVisible(p: Panel): boolean;
function PanelClicked(): Panel;
procedure PanelSetDraggable(p: panel; b:boolean);
function PanelDraggable(p: panel): boolean;
procedure MovePanel(p: Panel; mvmt: Vector);
function PanelAtPoint(pt: Point2D): Panel;
function PointInRegion(pt: Point2D; p: Panel): Boolean;

function IsDragging(): Boolean;

function PanelY(p: Panel): Single;
function PanelX(p: Panel): Single;
function PanelHeight(p: Panel): LongInt;
function PanelWidth(p: Panel): LongInt;

// Regions
function RegionClickedID(): String;
function RegionClicked(): Region;
function RegionID(r: Region): string;
function RegionWithID(pnl: Panel; ID: String): Region; overload;
function RegionWithID(ID: String): Region; overload;
function RegionPanel(r: Region): Panel;
procedure ToggleRegionActive(forRegion: Region);
procedure SetRegionActive(forRegion: Region; b: boolean);

function RegionY(r: Region): Single;
function RegionX(r: Region): Single;
function RegionHeight(r: Region): LongInt;
function RegionWidth(r: Region): LongInt;


//Checkbox
function CheckboxState(s: String): Boolean; overload;
function CheckboxState(chk: GUICheckbox): Boolean; overload;
function CheckboxState(r: Region): Boolean; overload;
procedure CheckboxSetState(chk: GUICheckbox; val: Boolean);
procedure ToggleCheckboxState(c: GUICheckbox);
function CheckboxFromRegion(r: Region): GUICheckbox;

//RadioGroup
function RadioGroupFromId(pnl: Panel; id: String): GUIRadioGroup;
function RadioGroupFromRegion(r: Region): GUIRadioGroup;
function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): integer;
function ActionRadioButton(grp: GUIRadioGroup): Region; overload;
function ActionRadioButton(r: Region): Region; overload;
procedure SelectRadioButton(r: Region); overload;
procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;


//TextBox
function TextBoxFont(tb: GUITextBox): Font; overload;
function TextBoxFont(r: Region): Font; overload;
procedure TextboxSetFont(Tb: GUITextbox; f: font);
function TextBoxText(r: Region): String; overload;
function TextBoxText(tb: GUITextBox): String; overload;
procedure TextboxSetText(r: Region; s: string); overload;
procedure TextboxSetText(tb: GUITextBox; s: string); overload;
function TextBoxFromRegion(r: Region): GUITextBox;
procedure SetAsActiveTextbox(r: Region); overload;
procedure SetAsActiveTextbox(t: GUITextBox); overload;
procedure DeactivateTextBox();
function ActiveTextIndex(): Integer;
function GUITextEntryComplete(): boolean;
function GUITextBoxOfTextEntered(): GUITextbox;
function RegionOfLastUpdatedTextBox(): Region;
function IndexOfLastUpdatedTextBox(): Integer;
procedure UpdateInterface();
procedure DrawGUIAsVectors(b : boolean);
procedure FinishReadingText();
function ActiveTextBoxParent() : Panel;

function TextboxAlignment(r: Region): FontAlignment;
function TextboxAlignment(tb: GUITextbox): FontAlignment;
procedure TextboxSetAlignment(tb: GUITextbox; align: FontAlignment);
procedure TextboxSetAlignment(r: Region; align: FontAlignment);


//Lists
procedure ListSetActiveItemIndex(lst: GUIList; idx: LongInt);
procedure SetActiveListItem(forRegion: region; pointClicked: Point2D);
procedure SetActiveListItem(lst: GUIList; pointClicked: Point2D);

function ListFromRegion(r: Region): GUIList; overload;

function ListFont(r: Region): Font; overload;
function ListFont(lst: GUIList): Font; overload;
procedure ListSetFont(lst: GUITextbox; f: font);
function ListItemText(r: Region; idx: LongInt): String; overload;
function ListItemText(lst: GUIList; idx: LongInt): String; overload;
function ListItemCount(r: Region): LongInt; overload;
function ListItemCount(lst:GUIList): LongInt; overload;
function ListActiveItemIndex(r: Region): LongInt; overload;
function ListActiveItemIndex(lst: GUIList): LongInt;
procedure ListRemoveItem(lst: GUIList; idx: LongInt);
procedure ListClearItems(lst: GUIList);
procedure ListAddItem(lst: GUIList; text: String);
procedure ListAddItem(lst: GUIList; img:Bitmap);
procedure ListAddItem(lst: GUIList; img:Bitmap; text: String);
function ListBitmapIndex(lst: GUIList; img: Bitmap): LongInt;
function ListTextIndex(lst: GUIList; value: String): LongInt;

function ListStartAt(lst: GUIList): LongInt;
function ListStartAt(r: Region): LongInt;

procedure ListSetStartAt(lst: GUIList; idx: LongInt);
procedure ListSetStartAt(r: Region; idx: LongInt);

function ListFontAlignment(r: Region): FontAlignment;
function ListFontAlignment(lst: GUIList): FontAlignment;
procedure ListSetFontAlignment(r: Region; align: FontAlignment);
procedure ListSetFontAlignment(lst: GUIList; align: FontAlignment);

//Label
function  LabelFont(l: GUILabel): Font; overload;
function  LabelFont(r: Region): Font; overload;
procedure LabelSetFont(l: GUILabel; s: String);
  
function  LabelText(lb: GUILabel): string; overload;
function  LabelText(r: Region): string; overload;
procedure LabelSetText(lb: GUILabel; newString: String); overload;
procedure LabelSetText(r: Region; newString: String); overload;
procedure LabelSetText(pnl: Panel; id, newString: String); overload;

function  LabelFromRegion(r: Region): GUILabel;

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
    
    // Variables for dragging panels
    downRegistered:     Boolean;
    panelDragging:      Panel;
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

function RegionRectangleOnscreen(r: Region): Rectangle;
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

procedure DrawTextbox(forRegion: Region; const area: Rectangle);
begin
  if GUIC.VectorDrawing then DrawRectangleOnScreen(GUIC.foregroundClr, area);
  
  if GUIC.activeTextBox <> forRegion then
    DrawTextLinesOnScreen(TextboxText(forRegion), 
                          GUIC.foregroundClr,
                          GUIC.backgroundClr, 
                          TextboxFont(forRegion), 
                          TextboxAlignment(forRegion),
                          InsetRectangle(area, 1));
end;
  
procedure DrawList(forRegion: Region; const area: Rectangle);
var
  tempList:                 GUIList;
  i, itemIdx:               LongInt;
  areaPt, Imagept:          Point2D;
  itemTextArea, itemArea, 
  scrollArea:    Rectangle;
  pct:                      Single;
  placeHolderScreenRect:    Rectangle;

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
  
  procedure ResizeItemArea(var area: Rectangle; aligned: FontAlignment; bmp: bitmap);
  begin
    
    case aligned of
      AlignLeft:    begin
                      area.Width := area.Width - BitmapWidth(bmp);
                      area.x     := area.x + Bitmapwidth(bmp);
                    end;
      AlignRight:   begin
                    end;
      AlignCenter:  begin
                    end;
    end;
    
  end;
  
  procedure _DrawScrollPosition();
  begin
    if tempList^.verticalScroll then
    begin
      if GUIC.VectorDrawing then
        FillRectangleOnScreen(GUIC.foregroundClr, 
                              Round(scrollArea.x),
                              Round(scrollArea.y + pct * (scrollArea.Height - tempList^.scrollSize)),
                              tempList^.scrollSize,
                              tempList^.scrollSize
                              )
      else
        DrawBitmapOnScreen(tempList^.ScrollButton,
                            Round(scrollArea.x),
                            Round(scrollArea.y + pct * (scrollArea.Height - tempList^.scrollSize)));
    end
    else
    begin
      if GUIC.VectorDrawing then
        FillRectangleOnScreen(GUIC.foregroundClr, 
                              Round(scrollArea.x + pct * (scrollArea.Width - tempList^.scrollSize)),
                              Round(scrollArea.y),
                              tempList^.scrollSize,
                              tempList^.scrollSize
                              )
      else
        DrawBitmapOnScreen(tempList^.ScrollButton,
                            Round(scrollArea.x + pct * (scrollArea.Width - tempList^.scrollSize)),
                            Round(scrollArea.y));
    end;
  end;
begin
  tempList := ListFromRegion(forRegion);
  if not assigned(tempList) then exit;
  
  DrawRectangleOnScreen(GUIC.foregroundClr, area);
  
  PushClip(area);
  areaPt := RectangleTopLeft(area);
  
  // Draw the up and down buttons
  if GUIC.VectorDrawing then
  begin
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
  end;
  
  // Draw the scroll area
  scrollArea := RectangleOffset(tempList^.scrollArea, areaPt);
  
  if GUIC.VectorDrawing then
  begin
    DrawRectangleOnScreen(GUIC.backgroundClr, scrollArea);
    DrawRectangleOnScreen(GUIC.foregroundClr, scrollArea);
  end;
  
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
  
  _DrawScrollPosition();
  
  PopClip(); // pop the scroll area
  
  // WriteLn('-------');
  //Draw all of the placeholders
  for i := Low(tempList^.placeHolder) to High(tempList^.placeHolder) do
  begin
    itemTextArea := RectangleOffset(tempList^.placeHolder[i], areaPt);
    itemArea := RectangleOffset(tempList^.placeHolder[i], areaPt);
    placeHolderScreenRect := RectangleOffset(tempList^.placeHolder[i], RectangleTopLeft(forRegion^.area));
    
    // Outline the item's area
    if GUIC.VectorDrawing then
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
    
    imagePt   := RectangleTopLeft(itemArea);    
    imagept.y := Imagept.y + (itemArea.height - BitmapHeight(tempList^.items[itemidx].image)) / 2;
    
    if not (tempList^.items[itemIdx].image = nil) then
      ResizeItemArea(itemTextArea, ListFontAlignment(tempList), tempList^.items[itemIdx].image);

    
    if (itemIdx <> tempList^.activeItem) then
    begin
      DrawTextLinesOnScreen(ListItemText(tempList, itemIdx), 
                            GUIC.foregroundClr, GUIC.backgroundClr, 
                            ListFont(tempList), ListFontAlignment(tempList), 
                            itemTextArea);
      if not (tempList^.items[itemIdx].image = nil) then
        DrawBitmapOnScreen(tempList^.items[itemIdx].image, imagePt);
    end
    else
    begin
      if GUIC.VectorDrawing then
      begin
        FillRectangleOnScreen(GUIC.foregroundClr, itemArea);
        DrawTextOnScreen(ListItemText(tempList, itemIdx), GUIC.backgroundClr, ListFont(tempList), RectangleTopLeft(itemTextArea));
      end
      else
      begin
        DrawBitmapPart(forRegion^.parent^.panelBitmapActive,
                       placeHolderScreenRect,
                       RectangleTopLeft(itemArea));
        DrawTextLinesOnScreen(ListItemText(tempList, itemIdx), 
                     GUIC.foregroundClr, GUIC.backgroundClr, 
                     ListFont(tempList), ListFontAlignment(tempList),
                     itemTextArea);
      end;
      
      if  assigned(tempList^.items[itemIdx].image) then
        DrawBitmapOnScreen(tempList^.items[itemIdx].image, imagePt);
    end;
    
    PopClip(); // item area
  end;
  
  PopClip(); // region
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
      currentReg := @GUIC.visiblePanels[i]^.Regions[j];
      case currentReg^.kind of
        gkButton:     DrawRectangleOnScreen(GUIC.foregroundClr, RegionRectangleOnscreen(currentReg));
        gkLabel:      DrawTextOnScreen( LabelText(currentReg), 
                                        GUIC.foregroundClr, 
                                        LabelFont(currentReg), 
                                        RectangleTopLeft(RegionRectangleOnScreen(currentReg)));
        gkCheckbox:   DrawVectorCheckbox(currentReg, RegionRectangleOnScreen(currentReg));
        gkRadioGroup: DrawVectorRadioButton(currentReg, RegionRectangleOnScreen(currentReg));
        gkTextbox:    DrawTextbox(currentReg, RegionRectangleOnScreen(currentReg));
        gkList:       DrawList(currentReg, RegionRectangleOnScreen(currentReg));
      end;
    end;
    
    PopClip();
  end;
end;

procedure DrawBitmapCheckbox(forRegion: Region; const area: Rectangle);
begin
  if CheckboxState(forRegion) then
  begin
    DrawBitmapPart(forRegion^.parent^.panelBitmapActive, forRegion^.area, RectangleTopLeft(area));
  end;
end;

procedure DrawBitmapRadioGroup(forRegion: Region; const area: Rectangle);
begin
  if forRegion = ActionRadioButton(forRegion) then
  begin
    DrawBitmapPart(forRegion^.parent^.panelBitmapActive, forRegion^.area, RectangleTopLeft(area));
  end;
end;

procedure DrawAsBitmaps();
var
  i, j: integer;
  currentReg: Region;
begin
  for i := Low(GUIC.visiblePanels) to High(GUIC.visiblePanels) do
  begin
    if GUIC.visiblePanels[i]^.visible then
      DrawBitmapOnScreen(GUIC.visiblePanels[i]^.panelBitmapInactive, RectangleTopLeft(GUIC.visiblePanels[i]^.area));
      
    for j := Low(GUIC.visiblePanels[i]^.Regions) to High(GUIC.visiblePanels[i]^.Regions) do
    begin
      currentReg := @GUIC.visiblePanels[i]^.Regions[j];
      case GUIC.visiblePanels[i]^.Regions[j].kind of
        gkLabel: DrawTextOnScreen(LabelText(currentReg), GUIC.foregroundClr, LabelFont(currentReg), RectangleTopLeft(RegionRectangleOnScreen(currentReg)));//      DrawTextOnScreen(LabelText(currentReg), GUIC.foregroundClr, LabelFont(currentReg), RectangleTopLeft(currentReg.area));
        gkTextbox: DrawTextbox(currentReg, RegionRectangleOnScreen(currentReg));
        gkCheckbox: DrawBitmapCheckbox(currentReg, RegionRectangleOnScreen(currentReg));
        gkRadioGroup: DrawBitmapRadioGroup(currentReg, RegionRectangleOnScreen(currentReg));
        gkList: DrawList(currentReg, RegionRectangleOnScreen(currentReg));
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

function RegionWidth(r: Region): LongInt;
begin
  result := -1;
  if not(assigned(r)) then exit;
  
  result := r^.area.width;
end;

function RegionHeight(r: Region): LongInt;
begin
  result := -1;
  if not(assigned(r)) then exit;
  
  result := r^.area.height;
end;

function RegionX(r: Region): Single;
begin
  result := -1;
  
  if not(assigned(r)) then exit;
  
  result := r^.area.x;
end;

function RegionY(r: Region): Single;
begin
  result := -1;
  if not(assigned(r)) then exit;
  
  result := r^.area.y;
end;

procedure SetRegionActive(forRegion: Region; b: boolean);
begin
  if not assigned(forRegion) then exit;

  forRegion^.active := b;
end;

procedure ToggleRegionActive(forRegion: Region);
begin
  if not assigned(forRegion) then exit;
  
  forRegion^.active := not forRegion^.active;
end;

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
        gkCheckBox:     ToggleCheckboxState ( CheckboxFromRegion(current) );
        gkRadioGroup:   SelectRadioButton   ( RadioGroupFromRegion(current), current );
        gkTextBox:      SetAsActiveTextbox  ( TextBoxFromRegion(current) );
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
  result := nil;  
  if not assigned(GUIC.lastClicked) then exit;
  
  result := GUIC.lastClicked;
end;

function RegionClickedID(): String;
begin
  result := '';
  if not assigned(GUIC.lastClicked) then exit;
  
  result := RegionID(GUIC.lastClicked);
end;

function RegionPanel(r: Region): Panel;
begin
  result := nil;
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

function RadioGroupFromId(pnl: Panel; id: String): GUIRadioGroup;
var
  i: Integer;
begin
  result := nil;
  if not assigned(pnl) then exit;
  
  id := LowerCase(id);
  
  for i := 0 to High(pnl^.radioGroups) do
  begin
    if LowerCase(pnl^.radioGroups[i].groupID) = id then
    begin
      result := @pnl^.radioGroups[i];
      exit;
    end;
  end;
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

procedure LabelSetText(pnl: Panel; id, newString: String); overload;
begin
  LabelSetText(LabelFromRegion(RegionWithID(pnl, id)), newString);
end;

procedure LabelSetText(r: Region; newString: String); overload;
begin
  LabelSetText(LabelFromRegion(r), newString);
end;

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
    result := lb^.contentString
  else
    result := '';
end;

function LabelText(r: Region): String; overload;
begin
  result := LabelText(LabelFromRegion(r));
end;

//---------------------------------------------------------------------------------------
// Textbox Code
//---------------------------------------------------------------------------------------

procedure TextboxSetFont(Tb: GUITextbox; f: font);
begin
  if not(assigned(tb)) OR not(assigned(f)) then exit;

  Tb^.font := f;
end;

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
                            RegionRectangleOnScreen(t^.region));
end;

function TextboxAlignment(r: Region): FontAlignment;
begin
  result := TextboxAlignment(TextBoxFromRegion(r));
end;

function TextboxAlignment(tb: GUITextbox): FontAlignment;
begin
  result := AlignLeft;
  if not assigned(tb) then exit;
  
  result := tb^.alignment;
end;

procedure TextboxSetAlignment(tb: GUITextbox; align: FontAlignment);
begin
  if not assigned(tb) then exit;
  
  tb^.alignment := align;
end;

procedure TextboxSetAlignment(r: Region; align: FontAlignment);
begin
  TextboxSetAlignment(TextBoxFromRegion(r), align);
end;

//---------------------------------------------------------------------------------------
// List Code
//---------------------------------------------------------------------------------------

function ListActiVeItemText(pnl: Panel; ID: String): String;
var
  r: Region;
begin
  result := '';
  if not assigned(pnl) then exit;
  r := RegionWithID(ID);

  Result := ListItemText(r, ListActiveItemIndex(r));
end;

procedure ListSetActiveItemIndex(lst: GUIList; idx: LongInt);
begin
  if not assigned(lst) then exit;
  lst^.activeItem := idx;
end;

procedure ListSetFont(lst: GUITextbox; f: font);
begin
  if not(assigned(lst)) OR not(assigned(f)) then exit;

  lst^.font := f;
end;


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

function ListItemCount(r: Region): LongInt; overload;
begin
  result := ListItemCount(ListFromRegion(r));
end;

function ListItemCount(lst:GUIList): LongInt; overload;
begin
  result := 0;
  if not assigned(lst) then exit;
  
  result := Length(lst^.items);
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

procedure ListClearItems(lst: GUIList);
begin
  if not assigned(lst) then exit;
  
  SetLength(lst^.items, 0);
  lst^.activeItem := -1;
  lst^.startingAt := 0;
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
    
    if lst^.startingAt >= idx then lst^.startingAt := lst^.startingAt - 1;
    if lst^.activeItem >= idx then lst^.activeItem := lst^.activeItem - 1;
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

function ListScrollIncrement(lst: GUIList): LongInt;
begin
  result := 1;
  if not assigned(lst) then exit;
  
  if lst^.verticalScroll then
    result := lst^.columns
  else
    result := lst^.rows;
    
  if result <= 0 then result := 1;
end;

procedure SetActiveListItem(forRegion: Region; pointClicked: Point2D);
begin
  SetActiveListItem(ListFromRegion(forRegion), pointClicked);
end;

procedure SetActiveListItem(lst: GUIList; pointClicked: Point2D);
var
  i, inc: LongInt;
begin
  if not assigned(lst) then exit;
  
  // WriteLn(PointToString(pointClicked));
  inc := ListScrollIncrement(lst);
  
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
    if lst^.startingAt + inc + (lst^.columns * lst^.rows) <= Length(lst^.items) then
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

function ListActiveItemIndex(r: Region): LongInt; overload;
begin
  result := ListActiveItemIndex(ListFromRegion(r));
end;

function ListActiveItemIndex(lst: GUIList): LongInt; overload;
begin
  result := -1;
  if not assigned(lst) then exit;
  result := lst^.activeItem;
end;

function ListStartAt(lst: GUIList): LongInt;
begin
  result := 0;
  if not assigned(lst) then exit;
  
  result := lst^.startingAt;
end;

function ListStartAt(r: Region): LongInt;
begin
  result := ListStartAt(ListFromRegion(r));
end;

procedure ListSetStartAt(lst: GUIList; idx: LongInt);
begin
  if not assigned(lst) then exit;
  if (idx < 0) then idx := 0
  else if (idx > High(lst^.items)) then idx := (High(lst^.items) div ListScrollIncrement(lst)) * ListScrollIncrement(lst);
  
  lst^.startingAt := idx;
end;

procedure ListSetStartAt(r: Region; idx: LongInt);
begin
  ListSetStartAt(ListFromRegion(r), idx);
end;

function ListFontAlignment(r: Region): FontAlignment;
begin
  result := ListFontAlignment(ListFromRegion(r));
end;

function ListFontAlignment(lst: GUIList): FontAlignment;
begin
  result := AlignLeft;
  if not assigned(lst) then exit;
  
  result := lst^.alignment;
end;

procedure ListSetFontAlignment(r: Region; align: FontAlignment);
begin
  ListSetFontAlignment(ListFromRegion(r), align);
end;

procedure ListSetFontAlignment(lst: GUIList; align: FontAlignment);
begin
  if not assigned(lst) then exit;
  
  lst^.alignment := align;
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
  if not assigned(c) then exit;

  c^.state := not c^.state;
end;

//---------------------------------------------------------------------------------------
// Panel Code
//---------------------------------------------------------------------------------------

function PanelDraggable(p: panel): boolean;
begin
  if not assigned(p) then exit;
  
  Result := p^.draggable;
end;

procedure PanelSetDraggable(p: panel; b:boolean);
begin
  if not assigned(p) then exit;
  
   p^.draggable := b;
end;

procedure PanelSetX(p: Panel; nX: LongInt);
begin
  if not(assigned(p)) then exit;
  
  p^.area.X := nX;
end;

procedure PanelSetY(p: Panel; nY: LongInt);
begin
  if not(assigned(p)) then exit;
  
  p^.area.Y := nY;
end;

function PanelWidth(p: Panel): LongInt;
begin
  result := -1;
  if not(assigned(p)) then exit;
  
  result := p^.area.width;
end;

function PanelHeight(p: Panel): LongInt;
begin
  result := -1;
  if not(assigned(p)) then exit;
  
  result := p^.area.height;
end;

function PanelX(p: Panel): Single;
begin
  result := -1;
  if not(assigned(p)) then exit;
  
  result := p^.area.x;
end;

function PanelY(p: Panel): Single;
begin
  result := -1;
  if not(assigned(p)) then exit;
  
  result := p^.area.y;
end;

function PanelVisible(p: Panel): boolean;
begin
  result := p^.visible;
end;

procedure AddPanelToGUI(p: Panel);
begin
  if assigned(p) then
  begin
    SetLength(GUIC.panels, Length(GUIC.panels) + 1);
    GUIC.panels[High(GUIC.panels)] := p;
  end;
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

function MousePointInPanel(pnl: Panel): Point2D;
begin
  result.x := -1; result.y := -1;
  if not assigned(pnl) then exit;
  
  result := PointAdd(MousePosition(), InvertVector(RectangleTopLeft(pnl^.area)));
end;

function PanelClicked(): Panel;
var
  i: integer;
begin
  result := nil;
  if not MouseClicked(Leftbutton) then exit;
  
  for i := High(GUIC.visiblePanels) downto Low(GUIC.visiblePanels) do
  begin
    if PointInRect(MousePosition(), GUIC.visiblePanels[i]^.area) then
    begin
      result := GUIC.visiblePanels[i];
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
    forRegion^.elementIndex := High(result^.Checkboxes);
  end;
  
  procedure CreateTextbox(r: region; data: string);
  var
    newTextbox: GUITextboxData;
  begin
    //Format is
    // 1, 2, 3, 4, 5, 6,   7,    8,      9,     10, 
    // x, y, w, h, 5, id , font, maxLen, align, text
    
    if CountDelimiter(data, ',') <> 9 then
    begin
      RaiseException('Error with textbox (' + data + ') should have 10 values (x, y, w, h, 5, id, font, maxLen, align, text)');
      exit;
    end;
    
    newTextbox.font           := FontNamed(Trim(ExtractDelimited(7, data, [','])));
    newTextbox.lengthLimit    := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newTextBox.alignment      := TextAlignmentFrom(Trim(ExtractDelimited(9, data, [','])));
    newTextBox.contentString  := Trim(ExtractDelimited(10, data, [',']));
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
    // 1, 2, 3, 4, 5, 6,      7,       8,    9,          10,     11,        12,         13          14                  
    // x, y, w, h, 5, ListID, Columns, Rows, ActiveItem, fontID, alignment, scrollSize, scrollKind, scrollbutton bitmap
    
    newList.columns         := StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList.rows            := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList.activeItem      := StrToInt(Trim(ExtractDelimited(9, data, [','])));
    newList.font            := FontNamed(Trim(ExtractDelimited(10, data, [','])));
    newList.alignment       := TextAlignmentFrom(Trim(ExtractDelimited(11, data, [','])));
    newList.scrollSize      := StrToInt(Trim(ExtractDelimited(12, data, [','])));;
    newList.verticalScroll  := LowerCase(Trim(ExtractDelimited(13, data, [',']))) = 'v';
    
    if Trim(ExtractDelimited(14, data, [','])) <> 'n' then
    begin
      //LoadBitmap(Trim(ExtractDelimited(13, data, [','])));
      newList.scrollButton    := LoadBitmap(Trim(ExtractDelimited(14, data, [',']))); //BitmapNamed(Trim(ExtractDelimited(13, data, [','])));
    end;
    
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
      gkButton:     ;
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
      'i': begin
             LoadBitmap(Trim(data)); 
             result^.panelBitmapInactive := BitmapNamed(Trim(data));
           end;
      'a': begin
             LoadBitmap(Trim(data));
             result^.panelBitmapActive := BitmapNamed(Trim(data));
           end;
      'r': StoreRegionData(data);
      'd': result^.draggable    := (data = 'true');
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
    result^.stringID  := '';
    result^.panelID   := -1;
    result^.area      := RectangleFrom(0,0,0,0);
    result^.visible   := false;
    result^.active    := false;
    result^.draggable := false;
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
    for j := Low(result^.Regions) to High(result^.Regions) do
    begin
      //Get the region as a list (will be nil if not list...)
      tempListPtr := ListFromRegion(@result^.regions[j]);
      
      if assigned(tempListPtr) then
      begin
        workingCol := 0;
        workingRow := 0;
        
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

function PointInRegion(pt: Point2D; p: Panel): Boolean;
var
	i: LongInt;
begin
	result := false;
  if not assigned(p) then exit;
	
	for i := Low(p^.Regions) to High(p^.Regions) do
	begin
		if PointInRect(MousePointinPanel(p), p^.Regions[i].area) then 
		begin	    
			result := true;
			exit;
		end;
	end;
end;

function PanelAtPoint(pt: Point2D): Panel;
var
	i: LongInt;
	curPanel: Panel;
begin
	result := nil;
	
	for i := Low(GUIC.Panels) to High(GUIC.Panels) do
	begin
		curPanel := GUIC.Panels[i];
		
		if PointInRect(pt, GUIC.Panels[i]^.area) then
		begin
			result := curPanel;
			exit;
		end;
	end;
end;

procedure DragPanel();
var
	mp: Point2D;
	curPanel: Panel;
begin
  if not IsDragging() then exit;
  
  mp := MousePosition();
  curPanel := PanelAtPoint(mp);
  if curPanel^.draggable then
	  MovePanel(curPanel, MouseMovement());
end;

function IsDragging(): Boolean;
begin
  result := assigned(GUIC.panelDragging);
end;

procedure StartDragging();
var
  mp: Point2D;
  curPanel: Panel;
begin
  mp := MousePosition();
  curPanel := PanelAtPoint(mp);
  
  if not PointInRegion(mp, curPanel) then
  begin
    GUIC.panelDragging := curPanel;
  end
  else
  begin
    GUIC.panelDragging := nil;
  end;
end;

procedure StopDragging();
begin
  GUIC.panelDragging  := nil;
  GUIC.downRegistered := false;
end;

procedure MovePanel(p: Panel; mvmt: Vector);
begin
  if not assigned(p) then exit;
  
  p^.area := RectangleOffset(p^.area, mvmt);
end;

procedure UpdateDrag();
begin
  // if mouse is down and this is the first time we see it go down
  if MouseDown(LeftButton) and not GUIC.downRegistered then
  begin
    GUIC.downRegistered := true;
    
    if ReadingText() then
      FinishReadingText();
    
    StartDragging();
  end
  // if mouse is now up... and it was down before...
  else if MouseUp(LeftButton) and GUIC.downRegistered then
  begin
    StopDragging();
  end;
  
  // If it is dragging then...
  if IsDragging() then
    DragPanel();
end;

procedure UpdateInterface();
var
  pnl: Panel;
begin
  GUIC.doneReading := false;
  
  UpdateDrag();
  
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
