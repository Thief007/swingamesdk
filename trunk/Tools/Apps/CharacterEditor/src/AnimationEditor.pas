unit AnimationEditor;

//=============================================================================
interface
		uses sgTypes, sgCore, sgGraphics,
  sgGeometry, sgImages, sgInput, SysUtils, 
	sgUserInterface, sgShared, EditorShared, sgText;
	
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues; SharedVals : EditorValues);
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	
const
	BMPWindowWidth = 320;
	BMPWindowHeight = 190;
	BMPWindowX = 57;
	BMPWindowY = 85;
	AniBitmapDetails = 0;
	AniDetailTabs = 1;
	AniCellBMPNames = 2;
	CellGapSmall = 2;
	
implementation

	//---------------------------------------------------------------------------
  // Draw Animation Editor
  //--------------------------------------------------------------------------- 
	
	procedure DrawSourceCells(cellGrp: CellGroup; srcBMP: Bitmap);
	var
		i: integer;
	begin
		if not assigned(srcBMP) then exit;
		
		with cellGrp do
		begin
			for i := Low(cells) to High(cells) do
			begin		
				DrawCell(srcBMP, i, cells[i].area.x, cells[i].area.y);
				DrawRectangle(ColorWhite, cells[i].area.x, cells[i].area.y, grpData.Width, grpData.Height);
			end;
		end;
	end;
	
	//---------------------------------------------------------------------------
  // Update Animation Editor
  //--------------------------------------------------------------------------- 
	procedure MoveAniBitmapCellArea(var cellGrp : CellGroup);
	var
		xGap, yGap, i, c, r: LongInt;
	// Gaps are inbetween cells when drawn on screen
	begin
		if (cellGrp.cellCount = 0) OR (cellGrp.cols = 0) then exit;
		xGap := 0;
		yGap := 0;	
		c		 := 0;
		r		 := 0;
		With cellGrp do
		begin						
			for  i := Low(cells) to High(cells) do
			begin							
				cells[i].Area.X 		:= grpData.Width  * c + grpData.X + xGap;//cells[i].xGap;
				cells[i].Area.Y 		:= grpData.Height * r + grpData.Y + yGap;//cells[i].yGap;
				cells[i].isSelected	:= false;	
				cells[i].cellIdx		:= i;			
		//		cells[i].XGap	:= xGap;
		//		cells[i].YGap	:= yGap;		
							
				xGap 		+= CellGapSmall;		
				c += 1;
				
				if c = cols then
				begin		
					c		 := 0;
					r 	 += 1;
					xGap := 0;
					yGap += CellGapSmall;
				end;
			end;
		end;
	end;
	
	procedure UpdateSourceCells(var cellGrp: CellGroup; srcBMP : Bitmap; var workingBMP: Bitmap);
	var
		i: integer;
		scale : single;
	begin
		with cellGrp do
		begin
			if (cols = 0) OR (rows = 0) then exit;
			if assigned(workingBMP) then FreeBitmap(workingBMP);
			grpData.Width := srcBMP^.Width DIV cols;
			grpData.Height := srcBMP^.Height DIV rows;
			
			SetLength(cells, rows * cols);
			for i := Low(cells) to High(cells) do
			begin
				cells[i].idx := i;
			end;
			
			if (srcBMP^.Width > srcBMP^.Height) OR (srcBMP^.Width = srcBMP^.Height) then 
			begin
				scale := BMPWindowWidth / (srcBMP^.Width + (cols-1)*CellGapSmall);
				grpData.x := BMPWindowX;
				grpData.y := BMPWindowY + (BMPWindowHeight DIV 2) - ((srcBMP^.Height  + (cols-1)*CellGapSmall) DIV 2);
			end else begin
				scale := BMPWindowHeight / (srcBMP^.Height + (rows-1)*CellGapSmall);
				grpData.x := BMPWindowX + (BMPWindowWidth DIV 2) - ((srcBMP^.Width  + (rows-1)*CellGapSmall) DIV 2);
				grpData.y := BMPWindowY;
			end;
			
			grpData.Width := Trunc(grpData.Width * scale);
			grpData.Height := Trunc(grpData.Height * scale);
			
			MoveAniBitmapCellArea(cellGrp);
					
			workingBMP := RotateScaleBitmap(srcBMP, 0, scale);		
			SetBitmapCellDetails(workingBMP, grpData.Width, grpData.Height, cols, rows, Length(cells));
		end;
	end;
		
	procedure UpdateAniCellDetailsFromTextInput(var AniMode: AnimationEditorValues; BMPArray: LoadedBitmaps);
	var
		newVal : LongInt;
	begin
		with AniMode do
		begin
			if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
			begin
				if (RegionPanel(RegionOfLastUpdatedTextBox) = panels[AniBitmapDetails]) then
				begin			
					case IndexOfLastUpdatedTextBox of 
						0: cellGrp.cols	:= newVal;
						1: cellGrp.rows := newVal;
					end;
				end;
				if (bmpIdx <> -1) then UpdateSourceCells(CellGrp, BMPArray[bmpIdx].src, workingBMP);
			end else begin
				case IndexOfLastUpdatedTextBox of 
					0: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.cols));
					1: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.rows));
				end;	
			end;
		end;
	end;
	
	procedure ChangeActiveBitmap(var AniMode: AnimationEditorValues; BMPArray: LoadedBitmaps);
	begin
		with AniMode do
		begin
			if PanelShown(panels[AniCellBMPNames]) AND (ListActiveItemText('AniBMPList') <> LabelText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')))) then
			begin
				ToggleShowPanel(panels[AniCellBMPNames]);
				bmpIdx := ListActiveItemIndex(ListFromRegion(RegionWithID('AniBMPList'))) - 1;	
				UpdateSourceCells(AniMode.CellGrp, BMPArray[bmpIdx].src, AniMode.workingBMP);
				LabelSetText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')), ListActiveItemText('AniBMPList'));
			end;
		end;
	end;
	
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues; SharedVals : EditorValues);
	begin
		with AniMode do
		begin
			DrawBitmap(bg, 0, 0);
			if (RegionClickedID() = 'AniBmpActiveLbl') then ToggleShowPanel(panels[AniCellBMPNames]);
			if GUITextEntryComplete  then UpdateAniCellDetailsFromTextInput(AniMode, SharedVals.BMPArray);
			ChangeActiveBitmap(AniMode,SharedVals.BMPArray);
			
			PushClip(BMPWindowX, BMPWindowY, BMPWindowWidth, BMPWindowHeight);	
			DrawSourceCells(cellGrp, workingBMP);
			PopClip();
		end;
	end;
{
	procedure InitializeAniCell(var aniCellGrp: cellGroup; i, ID, bitmap, cell:LongInt; var xGap: LongInt);
	begin
		aniCellGrp.cellArea[i].data[caXpos] 	:= aniCellGrp.grpData[cgWidth]  * i + aniCellGrp.grpData[aeXpos] + xGap;
		aniCellGrp.cellArea[i].data[caYpos]		:= aniCellGrp.grpData[aeYpos];	
		aniCellGrp.cellArea[i].data[caXGap]		:= xGap;
		aniCellGrp.cellArea[i].data[caID]			:= ID;
		aniCellGrp.cellArea[i].data[caBitmap]	:= bitmap;
		aniCellGrp.cellArea[i].data[caCell]		:= cell;
		
		xGap 			+= CellGap;
		
		if i = High(aniCellGrp.cellArea) then aniCellGrp.cellArea[i].data[caXpos] += ArrowWidth + CellGap;
	end;	
		
	procedure InitializeAniCellArea(var aniCellGrp, cellGrp : cellGroup);
	var
		xGap, i, oldLength: LongInt;
	begin
		xGap 			:= 0;

		aniCellGrp.grpData[aeWidth]		:= cellGrp.grpData[cgWidth];
		aniCellGrp.grpData[aeHeight]	:= cellGrp.grpData[cgHeight];
		
		aniCellGrp.grpData[aeXpos]		:= edgeDistance;
		aniCellGrp.grpData[aeYpos]		:= ScreenHeight - aniCellGrp.grpData[aeHeight]  - edgeDistance;
		
		for  i := Low(aniCellGrp.cellArea) to High(aniCellGrp.cellArea) do
		begin
			aniCellGrp.cellArea[i].data[caXpos] 	:= cellGrp.grpData[cgWidth]  * i + aniCellGrp.grpData[aeXpos] + xGap;
			aniCellGrp.cellArea[i].data[caYpos]		:= aniCellGrp.grpData[aeYpos];	
			aniCellGrp.cellArea[i].data[caXGap]		:= xGap;
			aniCellGrp.cellArea[i].data[caID]			:= -1;
			aniCellGrp.cellArea[i].data[caBitmap]	:= -1;
			aniCellGrp.cellArea[i].data[caCell]		:= -1;
			
			xGap 			+= CellGap; 

			InitializeAniCell(aniCellGrp, i, -1, -1, -1, xGap);
		end;	
	end;

	procedure InitializeAniCellGroup(var aniCellGrp, cellGrp : cellGroup);
	begin
		SetLength(aniCellGrp.grpData, LengthAniEditing);
		aniCellGrp.grpData[aeDrag]			:= -1;
		aniCellGrp.isAniGroup 					:= true;
		SetLength(aniCellGrp.cellArea, 2);
		
		InitializeAniCellArea(aniCellGrp, cellgrp);
	end;
	}
		
	//---------------------------------------------------------------------------
  // Initialize Animation Editor
  //--------------------------------------------------------------------------- 
	
	function InitializeAnimationPanels(): PanelArray;
	var
		i: LongInt;
	begin
		SetLength(result, 3);
		result[AniBitmapDetails]:= LoadPanel('AniBitmapDetails.txt');
		result[AniDetailTabs]		:= LoadPanel('AniDetailTabs.txt');
		result[AniCellBMPNames]	:= LoadPanel('AniCellBMPNames.txt');
		for i := Low(result) to High(result) do
		begin
			ActivatePanel(result[i]); 
			AddPanelToGUI(result[i]); 
		end;
		ListAddItem(ListFromRegion(RegionWithID('AniBMPList')), 'None');
		ListSetActiveItemIndex(ListFromRegion(RegionWithID('AniBMPList')), 0);	
		LabelSetText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')), ListActiveItemText('AniBMPList'));
	end;
	
	function InitializeSourceCells(): CellGroup;
	begin
		result.grpData.Width  := 0;
		result.grpData.Height := 0;
		result.cols := 0;
		result.rows := 0;
	end;
	
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	begin
		with AniMode do
		begin
			cellGrp 	 := InitializeSourceCells();
			panels 		 := InitializeAnimationPanels();		
			bg 				 := LoadBitmap('ANIEDITOR.png');
			workingBMP := nil;
			bmpidx		 := -1;
		end;
	end;

	{
	
	procedure DrawAnimationGroup(aniCellGrp, cellGrp : CellGroup; dCell: DraggedCell; bmpArray: arrayOfLoadedBMP; mouseOffSet: Point2D);
	var
		i: LongInt;
		dragPos : Point2D;
	begin
		for i := Low(aniCellGrp.cellArea) to High(aniCellGrp.cellArea) do
		begin
			DrawText(IntToStr(i), ColorBlack, aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos] - 10);
			DrawText(IntToStr(aniCellGrp.cellArea[i].data[caID]), ColorBlack, aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos] + 10 + aniCellGrp.grpData[cgHeight]);
			DrawCellGroup(aniCellGrp, i, aniCellGrp.grpData[cgWidth], aniCellGrp.grpData[cgHeight]);
			if (aniCellGrp.cellArea[i].data[caBitmap] <> -1) AND (i <> dCell.Index) then
				DrawCell(bmpArray[aniCellGrp.cellArea[i].data[caBitmap]].src, aniCellGrp.cellArea[i].data[caCell], aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos]);
		end;
		if ((dCell.Index <> -1)) then if (dCell.isAniGroup) then 
		begin
			WriteLn('WHy');
			DrawOnMouse(aniCellGrp, bmpArray, dCell, mouseOffSet);
		end;
	end; }

end.