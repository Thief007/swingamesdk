unit BitmapEditor;

//=============================================================================
interface
	uses EditorShared;	
	
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues);
	procedure UpdateBitmapEditor(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
	
const
//Bitmap Panel Array Indexes
	BitmapDetails = 0;
	CellDetails = 1;
	BitmapButtons = 2;
//Clip Area
	ClipX										= 220;
	ClipY										= 85;
	ClipW										= 565;
	ClipH										= 490;	
	
implementation
	uses sgTypes, sgCore, sgGraphics, sgGeometry, sgImages, sgInput, SysUtils, sgUserInterface, sgShared;		
	
	procedure ShowSelectedDetails(cellGrp : CellGroupData; var pnl: PanelArray);
	var
		i: integer;
	begin
		with cellGrp do
    begin
      if (Length(selectedOrder) = 1) AND (TextBoxText(RegionWithID('CellIn')) <> IntToStr(cells[selectedOrder[0]].Idx)) then
      begin
        TextBoxSetText(RegionWithID('CellIn'), IntToStr(cells[selectedOrder[0]].cellIdx));
        LabelSetText(RegionWithID('CellIdx'), IntToStr(cells[selectedOrder[0]].Idx));
        if (cells[selectedOrder[0]].bmpPtr <> nil) then 
          LabelSetText(RegionWithID('CurrentBitmapNameLbl'), cells[selectedOrder[0]].bmpPtr^.scaled[cellGrp.GridType]^.name)
        else
          LabelSetText(RegionWithID('CurrentBitmapNameLbl'), 'None'); 
      end else 		
      if (Length(cellGrp.selectedOrder) > 1) AND (TextBoxText(RegionWithID('CellIn')) <> 'Multiple') then
      begin
        TextBoxSetText(RegionWithID('CellIn'),'Multiple');
        LabelSetText(RegionWithID('CellIdx'),'Multiple');
        LabelSetText(RegionWithID('CurrentBitmapNameLbl'), 'Multiple'); 
      end;
      if (Length(cellGrp.selectedOrder) > 0)  AND (not PanelVisible(pnl[CellDetails])) then ShowPanel(pnl[CellDetails]);
      if (Length(cellGrp.selectedOrder) = 0)  AND PanelVisible(pnl[CellDetails]) then HidePanel(pnl[CellDetails]);
    end;
  end;
		
	procedure ScaleBitmaps(var cellGrp: CellGroupData; var bmpArray : CharBodyTypes; newScale : integer; var bmpScale : integer);
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
      UpdateGroupSize(cellGrp, CellGap);
      
      InitializeBitmapDetails(GridType, bmpArray, newScale, cols, rows);
		end;
	end;
    
  procedure ValidateInputForScale(var cellGrp: CellGroupData; var target: Integer; bmpArray: CharBodyTypes);
  var
    newVal : Integer;
  begin
    if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
      ScaleBitmaps(cellGrp, bmpArray, newVal, target);
  end;
  
  procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroupData; var  bmpArray: CharBodyTypes; pnl : PanelArray;var bmpScale: integer);
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
        if cellCount = 0 then SetLength(cells, 0);
        InitializeCellArea(cellGrp, nil, CellGap, true)	;  
        DeselectAll(cellGrp);
      end else 
      if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[CellDetails]) AND (IndexOfLastUpdatedTextBox = 0) then
      begin
        ValidateInput(cells[selectedOrder[0]].cellIdx);
      end;   
		end;
	end;
  
  procedure ChangeDrawnBitmap(cellGrp: CellGroupData; bmpPtr: LoadedBitmapPtr; pnl : Panel);
  var
    i : Integer;
  begin
    with cellGrp do
    begin
      if Length(selectedOrder) = 0 then exit;
    
      for i := Low(selectedOrder) to High(selectedOrder) do
      begin
        cells[selectedOrder[i]].bmpPtr := bmpPtr;
      end;     
      LabelSetText(RegionWithID('CurrentBitmapNameLbl'), bmpPtr^.scaled[Original]^.name);
      HidePanel(pnl);
      DeselectAll(cellGrp);
    end;
  end;
  
  procedure UpdateGUI(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
  begin
    with BitmapMode do
    begin
      if CheckboxState(RegionWithID('Anchor')) then DragCellGroup(cellGrp, sharedVals);
      if GUITextEntryComplete then UpdateCellDetailsFromTextInput(cellGrp, sharedVals.browser, panels, scale);
      if DialogComplete AND (sharedVals.OpenSave = SaveBMP) then ExportBitmap(destbmp, cellGrp);
      if (RegionClickedID() = 'ExportBitmap') then DoSaveDialog(sharedVals, saveBMP);		
      if (RegionClickedID() = 'ChangeButton') then ShowPanel(sharedVals.panels[BrowserPanel]);
      if (RegionClickedID() = 'ResetPosition') then
      begin
        MoveGroup(cellGrp, ClipX, ClipY);
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
  
      if sharedVals.BitmapPtr <> nil then ChangeDrawnBitmap(cellGrp, sharedVals.BitmapPtr, panels[CellDetails]);
				
			PushClip(ClipX, ClipY, ClipW, ClipH);
      MyDrawEmptyCells(cellGrp, sharedVals);      
      MyDrawSplitBitmapCells(cellGrp, sharedVals);
      if CheckboxState(RegionWithID('Anchor')) then FillRectangle(RGBAColor(255,255,0,180), cellGrp.grpArea);
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
		SetLength(result, 3);
		result[BitmapDetails]		:= LoadPanel('BitmapDetails.txt');
		result[CellDetails]			:= LoadPanel('BMPCellDetails.txt');
		result[BitmapButtons]		:= LoadPanel('BitmapButtons.txt');
		for i := Low(result) to High(result) do
		begin
		  ShowPanel(result[i]);
		end;
		HidePanel(result[CellDetails]);
	end;
	
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues);
	begin	
		with BitmapMode do
		begin
			panels	:= InitializePanels();
			cellGrp := InitializeCellGroup(12, 3, 4, 24, 32, ClipX, ClipY, CellGap, BitmapGroup);
			InitializeCellArea(cellGrp, nil, CellGap, true);
			scale		:= 1;
      destBMP := nil;
			bg := LoadBitmap('BMPEDITOR.png');
		end;
	end;
end.