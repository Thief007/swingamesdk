unit BitmapEditor;

//=============================================================================
interface
	uses sgTypes, sgCore, sgGraphics,
  sgGeometry, sgImages, sgInput, SysUtils, 
	sgUserInterface, sgShared, EditorShared;
	
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues; var bmpArray: LoadedBitmaps);
	procedure UpdateBitmapEditor(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
	
const
//Bitmap Panel Array Indexes
	BitmapDetails = 0;
	CellDetails = 1;
	CellBitmapNames = 2;
	BitmapButtons = 3;	
	
implementation
	
	procedure MyTextBoxSetText(id, value : string);
	begin
		TextBoxSetText(TextBoxFromRegion(RegionWithID(id)), value);
	end;
  
  function MyTextBoxText(id: string) : string;
	begin
		result := TextBoxText(TextBoxFromRegion(RegionWithID(id)));
	end;
	
	procedure MyLabelSetText(id, value : string);
	begin
		LabelSetText(LabelFromRegion(RegionWithID(id)), value); 
	end;
	
	procedure MyListSetActiveItemIndex(id: string; value: Integer);
	begin
		ListSetActiveItemIndex(ListFromRegion(RegionWithID(id)), value);
	end;
	
	function MyLabelText(id: string): string;
	begin
		result := LabelText(LabelFromRegion(RegionWithID(id)));
	end;
	
	function MyListActiveItemIndex(id: string): Integer;
	begin
		result := ListActiveItemIndex(ListFromRegion(RegionWithID(id)));
	end;
			
	procedure ShowSelectedDetails(cellGrp : CellGroupData; var pnl: PanelArray);
	var
		i: integer;
	begin
		with cellGrp do
    begin
      if (Length(selectedOrder) = 1) AND (MyTextBoxText('CellIn') <> IntToStr(cells[selectedOrder[0]].cellIdx)) then
      begin
        MyTextBoxSetText('CellIn', IntToStr(cells[selectedOrder[0]].cellIdx));
        if (cells[selectedOrder[0]].bmpPtr <> nil) then 
          MyLabelSetText('CurrentBitmapNameLbl', cells[selectedOrder[0]].bmpPtr^.src[cellGrp.GridType]^.name)
        else
          MyLabelSetText('CurrentBitmapNameLbl', 'None'); 
      end else 		
      if (Length(cellGrp.selectedOrder) > 1) AND (MyTextBoxText('CellIn') <> 'Multiple') then
      begin
        MyTextBoxSetText('CellIn','Multiple');
        MyLabelSetText('CurrentBitmapNameLbl', 'Multiple'); 
        MyListSetActiveItemIndex('BMPList',  0);
      end;
      if (Length(cellGrp.selectedOrder) > 0)  AND (not PanelVisible(pnl[CellDetails])) then ShowPanel(pnl[CellDetails]);
    end;
  end;
	
	procedure HideSelectedDetails(cellGrp : CellGroupData; var pnl: PanelArray; bmpArray: LoadedBitmaps);
	var
		i: integer; 
	begin
    if PanelVisible(pnl[CellDetails]) AND (Length(cellGrp.selectedOrder) = 0) then
    begin
      HidePanel(pnl[CellBitmapNames]);
      HidePanel(pnl[CellDetails]);
      exit;
    end;
    if (RegionClickedID <> 'BMPList') then exit;
    with cellGrp do
    begin    
      if (Length(selectedOrder) = 1) then
      begin
        cells[selectedOrder[0]].bmpPtr := SetLoadedBitmapPtr(MyListActiveItemIndex('BMPList') - 1, bmpArray);
      end else
      if (Length(selectedOrder) > 1) then
      begin
        for i := Low(selectedOrder) to High(selectedOrder) do
        begin
          cells[selectedOrder[i]].bmpPtr := SetLoadedBitmapPtr(MyListActiveItemIndex('BMPList') - 1, bmpArray);
        end;
      end;
      MyLabelSetText('CurrentBitmapNameLbl', ListActiveItemText(pnl[CellBitmapNames], 'BMPList'));
      HidePanel(pnl[CellBitmapNames]);
    end;
	end;
	
	procedure ScaleBitmaps(var cellGrp: CellGroupData; var bmpArray: LoadedBitmaps; newScale : integer; var bmpScale : integer);
	var
		initHeight, initWidth: integer;
	begin
		with cellGrp do
		begin
			initWidth   := cellW DIV bmpScale;
			initHeight  := cellH DIV bmpScale;			
			cellW 	    := initWidth	* newScale;
			cellH	      := initHeight * newScale;
			bmpScale		:= newScale;
      
      WriteLn(bmpScale);
      WriteLn(cellGrp.cellW);
      
      InitializeBitmapDetails(GridType, bmpArray, newScale, cols, rows);
		end;
	end;
  
  function ValidateInput(var target: Integer): Integer;
  begin
    result := -1;
    if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), result) AND (result > -1) then
		begin	
      target := result;
    end else
      TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(target));
  end;
  
  procedure ValidateInputForScale(var cellGrp: CellGroupData; var target: Integer; bmpArray: LoadedBitmaps);
  var
    newVal : Integer;
  begin
    if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
      ScaleBitmaps(cellGrp, bmpArray, newVal, target);
  end;
  
  procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroupData; var  bmpArray: LoadedBitmaps; pnl : PanelArray;var bmpScale: integer);
	begin
		with cellGrp do
		begin	
      if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[BitmapDetails]) then
      begin			
        case IndexOfLastUpdatedTextBox of 
          0: ValidateInput(cellCount);
          1: ValidateInput(cols);
          2: ValidateInput(cellW);
          3: ValidateInput(cellH);
          4: ValidateInputForScale(cellGrp, bmpScale, bmpArray);
        end;
        InitializeCellArea(cellGrp, nil, CellGap, true)	;   
      end else 
      if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[CellDetails]) AND (IndexOfLastUpdatedTextBox = 0) then
      begin
        ValidateInput(cells[selectedOrder[0]].cellIdx);
      end;   
		end;
	end;
  
  procedure UpdateGUI(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
  begin
    with BitmapMode do
    begin
    if CheckboxState(RegionWithID('Anchor')) then DragCellGroup(cellGrp, sharedVals);
    if GUITextEntryComplete then UpdateCellDetailsFromTextInput(cellGrp, sharedVals.BMPArray, panels, scale);
    if DialogComplete AND (sharedVals.OpenSave = SaveBMP) then ExportBitmap(destbmp, cellGrp, sharedVals.bmpArray);
    if (RegionClickedID() = 'ExportBitmap') then DoSaveDialog(sharedVals, saveBMP);			
    if (RegionClickedID() = 'CurrentBitmapNameLbl') then ToggleShowPanel(panels[CellBitmapNames]);
    if (RegionClickedID() = 'ResetPosition') then
    begin
      cellGrp.grpArea.X	:= ClipX;
      cellGrp.grpArea.Y	:= ClipY;
      UpdatePosition(cellGrp);
    end;
    end;
  end;
  
	procedure UpdateBitmapEditor(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
	var
		i: integer;
	begin
		with BitmapMode do
		begin
      UpdateGUI(BitmapMode, sharedVals);
			ShowSelectedDetails(cellGrp, panels);
			HideSelectedDetails(cellGrp, panels, sharedVals.BMPArray);
				
			PushClip(ClipX, ClipY, ClipW, ClipH);
      MyDrawEmptyCells(cellGrp, sharedVals);      
      MyDrawSplitBitmapCells(cellGrp, sharedVals);
      if CheckboxState(RegionWithID('Drag')) then FillRectangle(RGBAColor(255,255,0,180), cellGrp.grpArea);
 			PopClip();
      				
			if (not CheckboxState(RegionWithID('Anchor'))) then
			begin
				if MouseClicked(LeftButton) then HandleSelection(cellGrp, sharedVals.dragCell);
			end;
			
      if KeyTyped(vk_DELETE) then DeleteSelected(cellGrp, CellGap);
      if MouseClicked(RightButton) then DragAndDrop(cellGrp, sharedVals, CellGap);
        			
			if KeyTyped(vk_ESCAPE) then 
			begin
				for i := Low(cellGrp.SelectedOrder) to High(cellGrp.SelectedOrder) do 
          cellGrp.cells[cellGrp.selectedOrder[i]].isSelected := false;
				SetLength(cellGrp.SelectedOrder, 0);				
			end;
      
		end;
	end;
	//---------------------------------------------------------------------------
  // Initialize Bitmap Editor
  //--------------------------------------------------------------------------- 

	function InitializePanels(): PanelArray;
	var
		i: LongInt;
	begin
		SetLength(result, 4);
		result[BitmapDetails]		:= LoadPanel('BitmapDetails.txt');
		result[CellDetails]			:= LoadPanel('CellDetails.txt');
		result[CellBitmapNames]	:= LoadPanel('CellBMPNames.txt');
		result[BitmapButtons]		:= LoadPanel('BitmapButtons.txt');
		for i := Low(result) to High(result) do
		begin
		  ShowPanel(result[i]);
		end;
		HidePanel(result[CellBitmapNames]);
	end;
	
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues; var bmpArray: LoadedBitmaps);
	begin	
		with BitmapMode do
		begin
			panels	:= InitializePanels();
			cellGrp := InitializeCellGroup(12, 3, 4, 24, 32, ClipX, ClipY, CellGap, BitmapGroup);
			InitializeCellArea(cellGrp, nil, CellGap, true);
			ListAddItem(ListFromRegion(RegionWithID('BMPList')), 'None');
			ListSetActiveItemIndex(ListFromRegion(RegionWithID('BMPList')), 0);
      InitializeBitmapDetails(cellGrp.GridType, bmpArray, 1, cellGrp.cols, cellGrp.rows);
			scale		 		:= 1;
      destBMP := nil;
			bg := LoadBitmap('BMPEDITOR.png');
		end;
	end;
end.