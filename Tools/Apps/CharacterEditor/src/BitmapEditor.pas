unit BitmapEditor;

//=============================================================================
interface
	uses EditorTypes, sgUserInterface;
	
	procedure InitializeCellArea(var cellGrp : CellGroup; bmpArray: arrayofLoadedBMP);
	procedure DrawSelectedDetails(cellGrp : CellGroup; var pnl: editorPanels; bmpArray: arrayofLoadedBMP);
	procedure MoveCellPosition(var cellGrp: CellGroup);
	procedure UpdateCellDetailsFromTextInput(var cellGrp : CellGroup; var  bmpArray: arrayofLoadedBMP; pnl : editorPanels);
	procedure InitializeCellGroup(out cellGrp : CellGroup);
	procedure InitializePanels(var 	panelArray : editorPanels);
	
implementation
uses sgInput, SysUtils, sgShared, sgImages;

	procedure InitializeCellGroup(out cellGrp : CellGroup);
	var
		i : LongInt;
	begin
		SetLength(cellGrp.grpData, LengthgrpData); 
		cellGrp.grpData[cgCellCount]:= 12;
		cellGrp.grpData[cgColumns]	:= 3;
		cellGrp.grpData[cgRows] 		:= 4;
		cellGrp.grpData[cgWidth]		:= 24;
		cellGrp.grpData[cgHeight]		:= 32;
		cellGrp.grpData[cgScale]		:= 1;
		cellGrp.grpData[cgDrag]			:= -1;
		cellGrp.grpData[cgSelected]	:= -1;
		cellGrp.isAniGroup 					:= false;

		cellGrp.grpData[cgXpos]	:=  ClipX;
		cellGrp.grpData[cgYpos]	:= 	ClipY;
	end;
	
	procedure InitializePanels(var 	panelArray : editorPanels);
	var
		i: LongInt;
	begin
		panelArray[0]:= LoadPanel('BitmapDetails.txt');
		panelArray[1]:= LoadPanel('CellDetails.txt');
		panelArray[2]:= LoadPanel('CellBMPNames.txt');
		panelArray[3]:= LoadPanel('BitmapButtons.txt');
		
		for i := Low(panelArray) to High(panelArray) do
		begin
		//	ShowPanel(panelArray[i]);
			ActivatePanel(panelArray[i]); 
			AddPanelToGUI(panelArray[i]); 
			WriteLn(i);
		end;
		HidePanel(panelArray[2]);
		ShowPanel(panelArray[3]);
	end;
	
	procedure MoveCellPosition(var cellGrp: CellGroup);
	var
		rows, columns, i: LongInt;
	begin
		columns := 0;
		rows := 0;
		
		for  i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
		begin				
			cellGrp.cellArea[i].data[caXpos] 	:= cellGrp.grpData[cgWidth]  * columns + cellGrp.grpData[cgXpos] + cellGrp.cellArea[i].data[caXGap];
			cellGrp.cellArea[i].data[caYpos]	:= cellGrp.grpData[cgHeight] * rows		 + cellGrp.grpData[cgYpos] + cellGrp.cellArea[i].data[caYGap];	
			
			columns += 1;
			
			if columns = cellGrp.grpData[cgColumns] then
			begin		
				columns := 0;
				rows +=1;
			end;
		end;
	end;

	procedure InitializeCellArea(var cellGrp : CellGroup; bmpArray: arrayofLoadedBMP);
	var
		xGap, yGap, oldLength, i, columns: LongInt;
	// Gaps are shown inbetween cells when drawn on screen
	begin
		xGap 			:= 0;
		yGap 			:= 0;	
		columns	:= 0;
				
		if (cellGrp.grpData[cgCellCount] <> 0) AND (cellGrp.grpData[cgColumns] <> 0) then 
		begin
			cellGrp.grpData[cgRows] := Ceiling(cellGrp.grpData[cgCellCount] / cellGrp.grpData[cgColumns]);	
			for i:= Low(bmpArray) to High(bmpArray) do
			begin	
				SetBitmapCellDetails(bmpArray[i].src, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight], cellGrp.grpData[cgColumns], cellGrp.grpData[cgRows], cellGrp.grpData[cgCellCount]);
			end;
		end;
		
		if Length(cellGrp.cellArea) < cellGrp.grpData[cgCellCount] then oldLength := Length(cellGrp.cellArea)-1 else oldLength := cellGrp.grpData[cgCellCount];
		
		SetLength(cellGrp.cellArea, cellGrp.grpData[cgCellCount]);
		
		for  i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
		begin							
			cellGrp.cellArea[i].data[caXGap]	:= xGap;
			cellGrp.cellArea[i].data[caYGap]	:= yGap;		
			
			if i > oldLength then
			begin
				cellGrp.cellArea[i].isSelected			:= false;	
				cellGrp.cellArea[i].data[caBitmap]	:= -1;
				cellGrp.cellArea[i].data[caCell]		:= i;				
			end;		
			
			xGap 			+= CellGap;		
			columns += 1;
			
			if columns = cellGrp.grpData[cgColumns] then
			begin		
				columns := 0;
				xGap := 0;
				yGap += CellGap;
			end;
		end;
		MoveCellPosition(cellGrp);
	end;
		
	procedure DrawSelectedDetails(cellGrp : CellGroup; var pnl: editorPanels; bmpArray: arrayofLoadedBMP);
	begin
		if (Length(cellGrp.selectedOrder) = 1) AND (not PanelShown(pnl[1])) then
		begin
			TextBoxSetText(TextBoxFromRegion(RegionWithID(pnl[1], 'CellIn')), IntToStr(cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caCell]));
			if (cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caBitmap] <> -1) then 
				LabelSetText(LabelFromRegion(RegionWithID(pnl[1], 'CurrentBitmapNameLbl')), bmpArray[cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caBitmap]].src^.name)
			else
				LabelSetText(LabelFromRegion(RegionWithID(pnl[1], 'CurrentBitmapNameLbl')), 'None');
			ShowPanel(pnl[1])
		end
		else if PanelShown(pnl[1]) AND (Length(cellGrp.selectedOrder) <> 1) then
		begin
			HidePanel(pnl[1]);
			HidePanel(pnl[2]);
		end;
		
		if PanelShown(pnl[2]) AND (ListActiveItemIndex(ListFromRegion(RegionWithID(pnl[2],'BMPList'))) <> cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caBitmap]) then
		begin
			if (ListActiveItemIndex(ListFromRegion(RegionWithID(pnl[2],'BMPList'))) <= High(bmpArray)) then 
			begin
				cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caBitmap] := ListActiveItemIndex(ListFromRegion(RegionWithID(pnl[2],'BMPList')));
				LabelSetText(LabelFromRegion(RegionWithID(pnl[1], 'CurrentBitmapNameLbl')), bmpArray[cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caBitmap]].src^.name);
			end;
			HidePanel(pnl[2]);
		end;
	end;
	
	procedure ScaleBitmaps(var cellGrp: CellGroup;var pnl: Panel; var bmpArray: arrayOfLoadedBMP; newScale : integer);
	var
		i, initHeight, initWidth: integer;
	begin
		initWidth := cellGrp.grpData[cgWidth] DIV cellGrp.grpData[cgScale];
		initHeight := cellGrp.grpData[cgHeight] DIV cellGrp.grpData[cgScale];
		
		cellGrp.grpData[cgWidth] 	:= initWidth	* newScale;// + (cellGrp.grpData[cgWidth]	DIV (cellGrp.grpData[cgScale] - multiplier) * multiplier);
		cellGrp.grpData[cgHeight]	:= initHeight * newScale;
		cellGrp.grpData[cgScale]	:= newScale;
		
	//	TextBoxSetString(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgWidth]));
	//	TextBoxSetString(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgHeight]));
	
		for i:= Low(bmpArray) to High(bmpArray) do bmpArray[i].src := RotateScaleBitmap(bmpArray[i].original, 0, cellGrp.grpData[cgScale]);		
	end;
	
	procedure UpdateCellDetailsFromTextInput(var cellGrp: CellGroup; var  bmpArray: arrayofLoadedBMP; pnl : editorPanels);
	var
		newVal, i : LongInt;
	begin
		if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
		begin
			if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[0]) then
			begin			
				case IndexOfLastUpdatedTextBox of 
					0: cellGrp.grpData[cgCellCount] := newVal;
					1: cellGrp.grpData[cgColumns] 	:= newVal;
					2: cellGrp.grpData[cgWidth] 		:= newVal;
					3: cellGrp.grpData[cgHeight] 		:= newVal;
					4: ScaleBitmaps(cellGrp,pnl[0],  bmpArray, newVal);
				end;
			end else if (RegionPanel(RegionOfLastUpdatedTextBox) = pnl[1]) AND (IndexOfLastUpdatedTextBox = 0) then
			begin
				cellGrp.cellArea[cellGrp.selectedOrder[0]].data[caCell] := newVal;
			end;
			InitializeCellArea(cellGrp, bmpArray);	
		end else 
		begin
			case IndexOfLastUpdatedTextBox of 
				0: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgCellCount]));
				1: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgColumns]));
				2: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgWidth]));
				3: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgHeight]));
				4: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.grpData[cgScale]));
			end;	
		end;
		WriteLn(cellGrp.grpData[cgCellCount]);
	end;


end.