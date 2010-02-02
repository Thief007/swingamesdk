unit BitmapEditor;

//=============================================================================
interface
	uses sgTypes, sgCore, sgGraphics,
  sgGeometry, sgImages, sgInput, SysUtils, 
	sgUserInterface, sgShared, EditorShared;
	
	procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroup; var  bmpArray: LoadedBitmaps; pnl : PanelArray;var bmpScale: Integer);
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues; var bmpArray: LoadedBitmaps);
	procedure InitializeCellArea(var cellGrp : CellGroup; bmpArray: LoadedBitmaps);
	procedure MoveCellPosition(var cellGrp : CellGroup);
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
			
	procedure DrawSelectedDetails(cellGrp : CellGroup; var pnl: PanelArray; bmpArray: LoadedBitmaps);
	var
		i: integer;
	begin
		if MyListActiveItemIndex('BMPList') > Length(bmpArray) then exit;
		with cellGrp do
			begin
			if (Length(selectedOrder) = 1) AND (not PanelShown(pnl[CellDetails])) then
			begin
				MyTextBoxSetText('CellIn', IntToStr(cells[selectedOrder[0]].cellIdx));
				if (cells[selectedOrder[0]].bmpIdx <> -1) then 
					MyLabelSetText('CurrentBitmapNameLbl', bmpArray[cells[selectedOrder[0]].bmpIdx].src^.name)
				else
					MyLabelSetText('CurrentBitmapNameLbl', 'None'); 
				MyListSetActiveItemIndex('BMPList', cells[cellGrp.selectedOrder[0]].bmpIdx + 1);
				ShowPanel(pnl[CellDetails]);
			end else 		
			if (Length(cellGrp.selectedOrder) > 1) AND (TextBoxText(RegionWithID('CellIn')) <> 'Multiple') then
			begin
				MyTextBoxSetText('CellIn','Multiple');
				MyLabelSetText('CurrentBitmapNameLbl', 'None'); 
				MyListSetActiveItemIndex('BMPList',  0);
				ShowPanel(pnl[CellDetails])
			end;
				
			if PanelShown(pnl[CellBitmapNames]) AND (ListActiveItemText('BMPList') <> MyLabelText('CurrentBitmapNameLbl')) then
			begin
				if (Length(selectedOrder) = 1) then
				begin
					cells[selectedOrder[0]].bmpIdx := MyListActiveItemIndex('BMPList') - 1;
				end else
				if (Length(selectedOrder) > 1) then
				begin
					for i := Low(selectedOrder) to High(selectedOrder) do
					begin
						cells[selectedOrder[i]].bmpIdx := MyListActiveItemIndex('BMPList') - 1;
					end;
				end;
				MyLabelSetText('CurrentBitmapNameLbl', ListActiveItemText('BMPList'));
				HidePanel(pnl[CellBitmapNames]);
			end;
			
			if PanelShown(pnl[CellDetails]) AND (Length(selectedOrder) = 0) then
			begin
				HidePanel(pnl[CellBitmapNames]);
				HidePanel(pnl[CellDetails]);
			end;
		end;
	end;
	
	procedure ScaleBitmaps(var cellGrp: CellGroup;var pnl: Panel; var bmpArray: LoadedBitmaps; newScale : integer; var bmpScale : integer);
	var
		i, initHeight, initWidth: integer;
	begin
		with cellGrp do
		begin
			initWidth := grpData.Width DIV bmpScale;
			initHeight := grpData.Height DIV bmpScale;
			
			grpData.Width 	:= initWidth	* newScale;
			grpData.Height	:= initHeight * newScale;
			bmpScale				:= newScale;
		
			for i:= Low(bmpArray) to High(bmpArray) do bmpArray[i].src := RotateScaleBitmap(bmpArray[i].original, 0, bmpScale);		
		end;
	end;
	
	procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroup; var  bmpArray: LoadedBitmaps; pnl : PanelArray;var bmpScale: integer);
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
						2: grpData.Width 	:= newVal;
						3: grpData.Height := newVal;
						4: ScaleBitmaps(cellGrp,pnl[BitmapDetails],  bmpArray, newVal, bmpScale);
					end;
				end else if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[CellDetails]) AND (IndexOfLastUpdatedTextBox = 0) then
				begin
					cells[selectedOrder[0]].cellIdx := newVal;
				end;
				InitializeCellArea(cellGrp, bmpArray)	;
			end else 
			begin
				case IndexOfLastUpdatedTextBox of 
					0: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellCount));
					1: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cols));
					2: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(grpData.Width));
					3: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(grpData.Height));
					4: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(bmpScale));
				end;	
			end;
		end;
	end;
	
	procedure MoveCellGroup(var cellGrp: CellGroup; var mouseOffSet: Point2D; var dragGroup: boolean);
	var
		height : LongInt;
		rect: Rectangle;
	begin
		with cellGrp do
		begin
			if (cols = 0) OR (cellCount = 0) then exit;
			height := Ceiling(cellCount / cols);
			rect := RectangleFrom(cells[Low(cells)].Area.X, cells[Low(cells)].Area.Y, 
														grpData.Width * cols + (CellGap * cols - CellGap),
														grpData.Height * height + (CellGap * height - CellGap));
			FillRectangle(RGBAColor(255,255,0,180), rect);
			if PointInRect(MousePosition, rect) and MouseDown(LeftButton) AND not dragGroup then
			begin
				dragGroup := true;
				mouseOffset.x := Trunc(MousePosition().x) - grpData.X;
				mouseOffset.y := Trunc(MousePosition().y) - grpData.Y;
			end;
			if dragGroup and MouseDown(LeftButton) then
			begin		
				grpData.X := Trunc(MousePosition().x -  mouseOffset.x);
				grpData.Y := Trunc(MousePosition().y -	mouseOffset.y);
				MoveCellPosition(cellGrp);
			end;
			
			if not MouseDown(LeftButton) and dragGroup then dragGroup := false; 
		end;
	end;
	
//	procedure UpdateBitmapEditor(var cellGrp: CellGroup; var bmpArray: LoadedBitmaps; var destbmp: Bitmap; var mouseOffSet: Point2D; var dragGroup: boolean; dCell: DraggedCell; panelArray: PanelArray);

	procedure UpdateBitmapEditor(var BitmapMode: BitmapEditorValues; var sharedVals: EditorValues);
	var
		i: integer;
	begin
		with BitmapMode do
		begin
			DrawBitmap(bg, 0, 0);
			DrawSelectedDetails(cellGrp, panels, sharedVals.BMPArray);
			
			if GUITextEntryComplete then UpdateCellDetailsFromTextInput(cellGrp, sharedVals.BMPArray, panels, scale);
			
			PushClip(ClipX, ClipY, ClipW, ClipH);
			DrawSplitBitmapCells(destbmp, sharedVals.bmpArray, cellGrp, false, sharedVals.mouseOffSet, dCell);
			if CheckboxState(RegionWithID('Anchor')) then MoveCellGroup(cellGrp, sharedVals.mouseOffset, dragGroup);
			PopClip();
				
			if (not CheckboxState(RegionWithID('Anchor'))) then
			begin
				if MouseClicked(LeftButton) then
				begin
					HandleSelection(cellGrp, sharedVals.mouseOffSet, dCell);
					HandleCellDragging(cellGrp, dCell, sharedVals.mouseOffSet);
				end;
			end;
			
			if (RegionClickedID() = 'CurrentBitmapNameLbl') then ToggleShowPanel(panels[2]);
			if (RegionClickedID() = 'ResetPosition') then
			begin
				cellGrp.grpData.X	:= ClipX;
				cellGrp.grpData.Y	:= ClipY;
				MoveCellPosition(cellGrp);
			end;
			
			if KeyTyped(vk_ESCAPE) then
			begin
				for i := Low(cellGrp.SelectedOrder) to High(cellGrp.SelectedOrder) do cellGrp.cells[cellGrp.selectedOrder[i]].isSelected := false;
				SetLength(cellGrp.SelectedOrder, 0);
				dCell.index := -1;
			end;
		end;
	end;
	//---------------------------------------------------------------------------
  // Initialize Bitmap Editor
  //--------------------------------------------------------------------------- 
	function InitializeCellGroup() : CellGroup;
	begin
		result.CellCount 			:= 12;
		result.Cols 					:= 3;
		result.Rows 					:= 4;
		result.grpData.Width 	:= 24;
		result.grpData.Height := 32;
		result.grpData.X 			:= ClipX;
		result.grpData.Y 			:= ClipY;
		
		SetLength(result.selectedOrder, 0);
	end;
	
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
			ActivatePanel(result[i]); 
			AddPanelToGUI(result[i]); 
		end;
		HidePanel(result[CellBitmapNames]);
	end;
	
	procedure MoveCellPosition(var cellGrp : CellGroup);
	var
		r, c, i: LongInt; //Rows and Column count in loop
	begin
		c := 0;
		r := 0;
		
		With cellGrp do 
		begin
			for  i := Low(cells) to High(cells) do
			begin				
				cells[i].Area.X := grpData.Width  * c + grpData.X + cells[i].xGap;
				cells[i].Area.Y := grpData.Height * r + grpData.Y + cells[i].yGap;
				
				c += 1;				
				if c = Cols then
				begin		
					c := 0;
					r += 1;
				end;
			end;
		end;
	end;

	procedure InitializeCellArea(var cellGrp : CellGroup; bmpArray: LoadedBitmaps);
	var
		xGap, yGap, oldLength, i, columns: LongInt;
	// Gaps are inbetween cells when drawn on screen
	begin
		if (cellGrp.cellCount = 0) OR (cellGrp.cols = 0) then exit;
		xGap 			:= 0;
		yGap 			:= 0;	
		columns		:= 0;
		With cellGrp do
		begin		
			rows := Ceiling(cellCount / cols);	
			for i:= Low(bmpArray) to High(bmpArray) do
			begin	
				SetBitmapCellDetails(bmpArray[i].src, grpData.Width, grpData.Height, cols, rows, cellCount);
			end;
			
			if Length(cells) < CellCount then oldLength := Length(cells)-1 else oldLength := cellCount;			
			SetLength(cells, cellCount);
			
			for  i := Low(cells) to High(cells) do
			begin							
				cells[i].XGap	:= xGap;
				cells[i].YGap	:= yGap;		
				
				if i > oldLength then
				begin
					cells[i].isSelected	:= false;	
					cells[i].bmpIdx			:= -1;
					cells[i].cellIdx		:= i;				
				end;		
				
				xGap 		+= CellGap;		
				columns += 1;
				
				if columns = cols then
				begin		
					columns := 0;
					xGap 		:= 0;
					yGap 		+= CellGap;
				end;
			end;
		end;
		MoveCellPosition(cellGrp);
	end;
	
	procedure InitializeBitmapEditor(out BitmapMode: BitmapEditorValues; var bmpArray: LoadedBitmaps);
	begin	
		with BitmapMode do
		begin
			panels	:= InitializePanels();
			cellGrp := InitializeCellGroup();
			InitializeCellArea(cellGrp, bmpArray);
			ListAddItem(ListFromRegion(RegionWithID('BMPList')), 'None');
			ListSetActiveItemIndex(ListFromRegion(RegionWithID('BMPList')), 0);	
			dCell.Index := -1;
			dragGroup 	:= false;
			scale		 		:= 1;
			bg := LoadBitmap('BMPEDITOR.png');
		end;
	end;
end.