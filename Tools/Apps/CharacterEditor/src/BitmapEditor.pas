unit BitmapEditor;

//=============================================================================
interface
	uses sgTypes, sgCore, sgGraphics,
  sgGeometry, sgImages, sgInput, SysUtils, 
	sgUserInterface, sgShared, EditorShared;
	
	procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroupData; var  bmpArray: LoadedBitmaps; pnl : PanelArray;var bmpScale: Integer);
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
			
	procedure DrawSelectedDetails(cellGrp : CellGroupData; var pnl: PanelArray; bmpArray: LoadedBitmaps);
	var
		i: integer;
	begin
		if MyListActiveItemIndex('BMPList') > Length(bmpArray) then exit;
		with cellGrp do
			begin
			if (Length(selectedOrder) = 1) AND (not PanelVisible(pnl[CellDetails])) then
			begin
				MyTextBoxSetText('CellIn', IntToStr(cells[selectedOrder[0]].cellIdx));
				if (cells[selectedOrder[0]].bmpPtr <> nil) then 
					MyLabelSetText('CurrentBitmapNameLbl', cells[selectedOrder[0]].bmpPtr^.src[cellGrp.GridType]^.name)
				else
					MyLabelSetText('CurrentBitmapNameLbl', 'None'); 
				ShowPanel(pnl[CellDetails]);
			end else 		
			if (Length(cellGrp.selectedOrder) > 1) AND (TextBoxText(RegionWithID('CellIn')) <> 'Multiple') then
			begin
				MyTextBoxSetText('CellIn','Multiple');
				MyLabelSetText('CurrentBitmapNameLbl', 'Multiple'); 
				MyListSetActiveItemIndex('BMPList',  0);
				ShowPanel(pnl[CellDetails])
			end;
				
			if PanelVisible(pnl[CellBitmapNames]) AND (ListActiveItemText(pnl[CellBitmapNames], 'BMPList') <> MyLabelText('CurrentBitmapNameLbl')) then
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
			
			if PanelVisible(pnl[CellDetails]) AND (Length(selectedOrder) = 0) then
			begin
				HidePanel(pnl[CellBitmapNames]);
				HidePanel(pnl[CellDetails]);
			end;
		end;
	end;
	
	procedure ScaleBitmaps(var cellGrp: CellGroupData;var pnl: Panel; var bmpArray: LoadedBitmaps; newScale : integer; var bmpScale : integer);
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
      
      InitializeBitmapDetails(GridType, bmpArray, newScale, cols, rows);
		end;
	end;
	
	procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroupData; var  bmpArray: LoadedBitmaps; pnl : PanelArray;var bmpScale: integer);
	var
		newVal: LongInt;
	begin
		with cellGrp do
		begin
			if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
			begin			
				if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[BitmapDetails]) then
				begin			
					case IndexOfLastUpdatedTextBox of 
						0: cellCount 			:= newVal;
						1: cols						:= newVal;
						2: cellW 	:= newVal;
						3: cellH := newVal;
						4: ScaleBitmaps(cellGrp,pnl[BitmapDetails],  bmpArray, newVal, bmpScale);
					end;
				end else if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[CellDetails]) AND (IndexOfLastUpdatedTextBox = 0) then
				begin
					cells[selectedOrder[0]].cellIdx := newVal;
				end;
				InitializeCellArea(cellGrp, nil, CellGap)	;
			end else 
			begin
				case IndexOfLastUpdatedTextBox of 
					0: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellCount));
					1: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cols));
					2: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellW));
					3: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellH));
					4: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(bmpScale));
				end;	
			end;
		end;
	end;
  
	procedure UpdateBitmapEditor(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
	var
		i: integer;
	begin
		with BitmapMode do
		begin
			DrawSelectedDetails(cellGrp, panels, sharedVals.BMPArray);
			
			
			
			PushClip(ClipX, ClipY, ClipW, ClipH);
      MyDrawEmptyCells(cellGrp, sharedVals);      
      MyDrawSplitBitmapCells(cellGrp, sharedVals);
      
      if GUITextEntryComplete then UpdateCellDetailsFromTextInput(cellGrp, sharedVals.BMPArray, panels, scale);
			if CheckboxState(RegionWithID('Anchor')) then DragCellGroup(cellGrp, sharedVals);
			PopClip();
      
      if (RegionClickedID() = 'ExportBitmap') then ShowSaveDialog();
      
      if DialogComplete then ExportBitmap(destbmp, cellGrp, sharedVals.bmpArray);
				
			if (not CheckboxState(RegionWithID('Anchor'))) then
			begin
				if MouseClicked(LeftButton) then
				begin
					HandleSelection(cellGrp, sharedVals.dragCell);
				end;
			end;
			
			if (RegionClickedID() = 'CurrentBitmapNameLbl') then ToggleShowPanel(panels[2]);
			if (RegionClickedID() = 'ResetPosition') then
			begin
				cellGrp.grpArea.X	:= ClipX;
				cellGrp.grpArea.Y	:= ClipY;
				UpdatePosition(cellGrp);
			end;

      if KeyTyped(vk_9) then DeleteSelected(cellGrp, CellGap);

      if MouseClicked(RightButton) then
      begin
        DragAndDrop(cellGrp, sharedVals, CellGap);
      end;
        
			
			if KeyTyped(vk_ESCAPE) then 
			begin
				for i := Low(cellGrp.SelectedOrder) to High(cellGrp.SelectedOrder) do cellGrp.cells[cellGrp.selectedOrder[i]].isSelected := false;
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
			InitializeCellArea(cellGrp, nil, CellGap);
			ListAddItem(ListFromRegion(RegionWithID('BMPList')), 'None');
			ListSetActiveItemIndex(ListFromRegion(RegionWithID('BMPList')), 0);
      InitializeBitmapDetails(cellGrp.GridType, bmpArray, 1, cellGrp.cols, cellGrp.rows);
			scale		 		:= 1;
      destBMP := nil;
			bg := LoadBitmap('BMPEDITOR.png');
		end;
	end;
end.