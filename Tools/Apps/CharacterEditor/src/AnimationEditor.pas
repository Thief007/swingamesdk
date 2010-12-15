unit AnimationEditor;

//=============================================================================
interface
  uses EditorShared;
	
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues;var SharedVals : EditorValues);
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	
implementation
	uses sgTypes, sgGraphics, sgResources, StrUtils,
  sgGeometry, sgImages, sgInput, SysUtils, sgSprites, sgAnimations, 
	sgUserInterface, sgShared, sgNamedIndexCollection, sgText;
const
//Panel Indexs
	AniBitmapDetails  = 0;
	AniLists       		= 1;
	AniMenuPanel 			= 2;
	AniStats 			    = 3;
	AniScroll			    = 4;
	AniPreview		    = 5;


	//---------------------------------------------------------------------------
  // Initialize Animation Editor
  //--------------------------------------------------------------------------- 
 
	function InitializeAnimationPanels(): PanelArray;
	begin
		SetLength(result, 6);
		result[AniBitmapDetails]:= LoadPanel('AniBitmapDetails.txt');
		result[AniLists]	      := LoadPanel('AniLists.txt');
		result[AniMenuPanel]	  := LoadPanel('AniMenuPanel.txt');
		result[AniStats]	      := LoadPanel('AniStats.txt');
		result[AniScroll]	      := LoadPanel('AniScroll.txt');
		result[AniPreview]	    := LoadPanel('AniPreview.txt');
	end;
	
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	begin
		with AniMode do
		begin
			cellGrp 	    := InitializeCellGroup(0,0,0,0,0,0,0,0,SourceGroup);
			panels 		    := InitializeAnimationPanels();		
			bg 				    := LoadBitmap('ANIEDITOR.png');
			filmStrip	    := LoadBitmap('filmstrip.png');
			pointArrow    := LoadBitmap('arrow.png');
																	// x y w h
			startArrow    := RectangleFrom(0,0, 15,71);
			finishArrow   := RectangleFrom(56,0,15,71);
			middle 		    := RectangleFrom(18,0,39,71);
      selectedIdx   := -2;
      scrollOffSet  := 0;
      originalSize  := 0;
      radio1        := 0;
      radio2        := 0;
      stripCount    := -1;
      previewSprite := nil;
      aniTemp       := nil;
		end;
	end;
    
	//---------------------------------------------------------------------------
  // Draw Animation Editor
  //--------------------------------------------------------------------------- 
  
	procedure DrawSourceCells(cellGrp: CellGroupData; sharedVals : EditorValues);
	begin
		PushClip(BMPWindowX, BMPWindowY, BMPWindowWidth, BMPWindowHeight);	
    
    MyDrawSplitBitmapCells(cellGrp, sharedVals);
    if CheckboxState(RegionWithID('Drag')) then FillRectangle(RGBAColor(255,255,0,180), cellGrp.grpArea);
		PopClip();
	end;
  
  procedure DrawFilmStrip(AniMode: AnimationEditorValues; sharedVals: EditorValues; aniIndex : integer);
  var
    i: integer;
  const
    w = 702;    // DIfferent push clips for so that the bitmap parts arent drawn on the side scrolling buttons or visible behind them
    h = 267;
    initialX = 47;
    YoffSet = 19;
    outerX = 30;
    maxHCells = 16;
  begin
    with AniMode.aniStrips[aniIndex].cellGrp do
    begin
      PushClip(outerX, InitialAnimationPosY-YoffSet, 20, h);
      DrawBitmapPart(AniMode.filmStrip, AniMode.startArrow, FilmStripStartX, cells[Low(cells)].area.y- FilmStripYOffSet);	
      PopClip();
      PushClip(initialX, InitialAnimationPosY-YoffSet, w, h);
      for  i := Low(cells) to High(cells) do
      begin			
        DrawBitmapPart(AniMode.filmStrip, AniMode.middle, cells[i].area.x + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
        DrawText(IntToStr(cells[i].idx) +','+ IntToStr(cells[i].cellIdx), ColorGreen, cells[i].area.x - 5, cells[i].area.y -7);  
      end;
      DrawBitmap(AniMode.pointArrow, cells[i].area.x + AniMode.middle.width + FilmOffSet,  cells[i].area.y - FilmStripYOffSet);
      DrawBitmapPart(AniMode.filmStrip, AniMode.middle, cells[i].area.x + (AniMode.middle.width)*2  + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
      PopClip();
      PushClip(outerX, InitialAnimationPosY-YoffSet, 752, h);
      if Length(cells) >= maxHCells then
        DrawBitmapPart(AniMode.filmStrip, AniMode.finishArrow, 743, cells[i].area.y - FilmStripYOffSet)
      else
        DrawBitmapPart(AniMode.filmStrip, AniMode.finishArrow, cells[i].area.x + (AniMode.middle.width)*3  + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
      PopClip();
    end;
  end;
  
  procedure DrawAnimationEditor(AniMode: AnimationEditorValues; sharedVals: EditorValues);
  var
    i: Integer;
  const
    w = 702;
    h = 267;
    initialX = 47;
    YoffSet = 19;
  begin
    with AniMode do
    begin
      DrawSourceCells(cellGrp, sharedVals);
      for i := 0 to AniMode.stripCount do
			begin       
        DrawFilmStrip(AniMode, sharedVals, i);
        PushClip(initialX, InitialAnimationPosY-YoffSet, w, h);
        MyDrawSplitBitmapCells(aniStrips[i].cellGrp, sharedVals);
        MyDrawSplitBitmapCells(aniStrips[i].LastCell, sharedVals);
        if aniStrips[i].lastCellParentID <> -1 then
        begin
          DrawText((IntToStr(aniStrips[i].lastCellParentID)+ ','+IntToStr(aniStrips[i].lastCell.cells[0].idx)), ColorRed,
                    aniStrips[i].lastCell.cells[0].area.x -5, aniStrips[i].lastCell .cells[0].area.y + 33);
        end;
        PopClip();
      end;
    end;
  end;
  
 	//---------------------------------------------------------------------------
  // Scroll Handling
  //---------------------------------------------------------------------------  
  
  procedure MoveAniStrip(var AniStrip: AnimationStrip; x, y: Integer);
  begin
    with AniStrip do
    begin
      MoveGroup(cellGrp, x, y);
      UpdatePosition(cellGrp);
      MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(cellGrp.grpArea.y));
      UpdatePosition(LastCell); 
    end;
  end;
 
  procedure MoveStripsArray(var AniMode: AnimationEditorValues);
  var
    i: Integer;
  begin  
    for i := 0 to AniMode.stripCount do
    begin
      with AniMode.aniStrips[i] do
      begin
        MoveAniStrip(AniMode.aniStrips[i], Trunc(cellGrp.grpArea.x), Trunc(InitialAnimationPosY + (AnimationIncrPosY * i) - (AnimationIncrPosY*AniMode.scrollOffset)));
      end;
    end;
  end;
  
  procedure ScrollAnimationsVertical(var AniMode: AnimationEditorValues);
  begin
    if regionclicked = nil then exit;
    with AniMode do 
    begin
      if (RegionClickedID = 'ScrollUp') AND (scrollOffset > 0) then
      begin
        AniMode.scrollOffset -= 1;
        MoveStripsArray(AniMode);
        PositionAniHorizontalScrollButtons(AniMode);
      end else
      if (RegionClickedID = 'ScrollDown') AND (Length(aniStrips) > 3) AND (scrollOffset < Length(aniStrips) - 3) then
      begin
        AniMode.scrollOffset += 1;  
        MoveStripsArray(AniMode);
        PositionAniHorizontalScrollButtons(AniMode);
      end;  
    end;
  end;
  
  procedure ScrollAnimationsHorizontal(var AniMode: AnimationEditorValues);
  var
    i: Integer;
  begin    
    if regionclicked = nil then exit;
    for i:= 0 to 2 do
    begin
      with AniMode.aniStrips[i + AniMode.scrolloffSet] do
      begin
        if (RegionClickedID = ('Left'+IntToStr(i+1))) AND (scrollOffset < 0) then
        begin      
          scrollOffset += 1;
          MoveAniStrip(AniMode.aniStrips[i + AniMode.scrolloffSet], Trunc(InitialAnimationPosX + ((cellGrp.cellW+CellGapLarge)*scrollOffSet)), Trunc(cellGrp.grpArea.y));
        end else
        if (RegionClickedID = ('Right'+IntToStr(i+1))) AND (Length(cellGrp.Cells) >= 16) AND (scrollOffset > 16 - Length(cellGrp.cells)) then
        begin
          WriteLn(scrollOffset);
          scrollOffset -= 1;
          MoveAniStrip(AniMode.aniStrips[i + AniMode.scrolloffSet], Trunc(InitialAnimationPosX + ((cellGrp.cellW+CellGapLarge)*scrollOffSet)), Trunc(cellGrp.grpArea.y));
        end;
      end;
    end;
  end;

	
	//---------------------------------------------------------------------------
  // Animation IDs
  //--------------------------------------------------------------------------- 
   
  // Calculates the index of the animation strip based on the cell's position
  function CalculateParentStripID(cell: CellArea; AniMode: AnimationEditorValues): Integer;
  begin
    result := Trunc((cell.area.y - InitialAnimationPosY + (AniMode.scrollOffset*AnimationIncrPosY)) / AnimationIncrPosY);
  end;
  
  function SelectedIdx(aniStrip: AniStripArray; stripCount: Integer) : Integer;
  var
    i, count: Integer;
  begin
    count := 0;
    result := -2;
    for i := 0 to stripCount do
    begin
      with aniStrip[i] do
      begin
        if (Length(cellGrp.selectedOrder) <> 0) then count := count + Length(cellGrp.selectedOrder);
        if (count = 1) AND (Length(cellGrp.selectedOrder) = 1) then
        begin
          result := i;
        end;
        if count > 1 then
        begin
          result := -1;
          exit;
        end;
      end;
    end;
  end;
  
  procedure RefreshIDList(var AniMode: AnimationEditorValues);
  var
    i, j: Integer;
  begin
    ListClearItems(RegionWithID('IDList'));
    ListClearItems(RegionWithID('SoundList'));
    for i := 0 to AniMode.stripCount do
    begin
      with AniMode.aniStrips[i].cellGrp do
      begin
        for j:= Low(cells) to High(cells) do
        begin
          if Trim(cells[j].identifier) <> '' then 
            ListAddItem(RegionWithID('IDList'), IntToStr(CalculateParentStripID(cells[j], AniMode)) + ',' + IntToStr(cells[j].idx) + ',' + cells[j].identifier);
          if Trim(cells[j].sound) <> '' then 
            ListAddItem(RegionWithID('SoundList'), IntToStr(CalculateParentStripID(cells[j], AniMode)) + ',' + IntToStr(cells[j].idx) + ',' + ExtractFileName(cells[j].sound));
        end;
      end;
    end;
  end;
  
  procedure RemoveIDFromList(var AniMode: AnimationEditorValues;const id: string; const idType: Char);
  var
    lst: GUIList;
    txt: string;
    aniID, cellID : Integer;
  begin    
    if ListActiveItemIndex(RegionWithID(id)) = -1 then exit;
    
    txt     := ListActiveItemText(RegionWithID(id));
    aniID   := StrToInt(ExtractDelimited(1, txt, [',']));
    cellID  := StrToInt(ExtractDelimited(2, txt, [',']));
    
    case idType of 
      'i' : AniMode.aniStrips[aniID].cellgrp.cells[cellID].identifier := '';
      's' : AniMode.aniStrips[aniID].cellgrp.cells[cellID].sound      := '';
    end;
    
    ListRemoveItem(ListFromRegion(RegionWithID(id)), ListActiveItemIndex(RegionWithID(id)));
  end;
  
  //---------------------------------------------------------------------------
  // Update Animation Editor
  //--------------------------------------------------------------------------- 
  
  procedure ShowCellDetails(idx, cellidx, groupID, stringId, soundID, timing : string);
  var
    ID, groupIndex: GUILabel;
    aniIdtxt, soundtxt,  cellIndex, timingTxt : GUITextBox;
  begin
    ID           := LabelFromRegion(RegionWithID('AniIDDisp'));
    cellIndex    := TextBoxFromRegion(RegionWithID('AniCellIndexDisp'));
    GroupIndex   := LabelFromRegion(RegionWithID('AniGroupIDDisp'));
    aniIdtxt     := TextBoxFromRegion(RegionWithID('AnimationIDDisp'));
    soundtxt     := TextBoxFromRegion(RegionWithID('SoundIDDisp'));
    timingTxt    := TextBoxFromRegion(RegionWithID('AniTiming'));
      
    LabelSetText(ID, idx);
    LabelSetText(groupIndex, groupID);
    TextBoxSetText(cellIndex, cellidx);
    TextBoxSetText(aniIdtxt, stringId);
    TextBoxSetText(soundtxt, soundId);
    TextBoxSetText(timingTxt, timing);
  end;
  
  procedure DisplaySelectedCellStats(var AniMode : AnimationEditorValues);
  var
    selected, index: Integer;
  const
    None = 'None';
    Multiple = 'Multiple';
  begin
    selected:= SelectedIdx(AniMode.aniStrips, aniMode.stripCount);
    
    if (selected = AniMode.selectedIdx) then 
    begin
      if selected < 0 then exit;
      index := AniMode.aniStrips[selected].cellGrp.cells[AniMode.aniStrips[selected].cellGrp.selectedOrder[0]].idx;
      if (IntToStr(Index) = LabelText(LabelFromRegion(RegionWithID('AniIDDisp')))) then exit;
    end;
    AniMode.selectedIdx := selected;
    
    case selected of 
      -1 : ShowCellDetails('*', Multiple, Multiple, Multiple, Multiple, '*');
      -2 : ShowCellDetails(None, None, None, None, None, '-');
    else
      with AniMode.aniStrips[selected] do
      begin
        index := cellGrp.selectedOrder[0];
        ShowCellDetails(IntToStr(cellGrp.cells[index].Idx), 
                        IntToStr(cellGrp.cells[index].cellIdx), 
                        IntToStr(selected), 
                        cellGrp.cells[index].identifier, 
                        cellGrp.cells[index].sound,
                        IntToStr(timing));                                                    
      end;
    end;   
  end;
	
  procedure ResetSourceCells(var cellGrp: CellGroupData);
  var
    i: Integer;
  begin
    with cellGrp do
    begin
      for  i := Low(cells) to High(cells) do
      begin
          cells[i].bmpPtr := nil;
          cells[i].parent := nil;
      end;
      SetLength(cells, 0);
      cellGrp 	 := InitializeCellGroup(cellCount,cols,rows,0,0,0,0,0,GridType);
    end;    
  end;
  
  procedure UpdateAllAniBitmaps(targetBMP: LoadedBitmapPtr; var  AniMode: AnimationEditorValues);
	var
		ratio, tempW, tempH, i: integer;
		scale : single;
	begin
		with AniMode.cellGrp do
		begin
			if (targetBMP = nil) then
      begin
        LabelSetText(RegionWithID('AniBmpActiveLbl'), 'None');
        UpdateBitmapPointers(AniMode, nil);
        ResetSourceCells(AniMode.cellGrp);
        exit;
      end;

      cellW     := targetBMP^.scaled[GridType]^.cellW;
      cellH     := targetBMP^.scaled[GridType]^.cellH;
      cols      := targetBMP^.scaled[GridType]^.cellCols;
      rows      := targetBMP^.scaled[GridType]^.cellRows;
      cellCount := targetBMP^.scaled[GridType]^.cellCount;
      
      if targetBMP^.scaled[GridType]^.Width > targetBMP^.scaled[GridType]^.Height then
      begin
        grpArea.x := BMPWindowX;
        grpArea.y := BMPWindowY + (BMPWindowHeight DIV 2) - ((targetBMP^.scaled[GridType]^.Height  + (rows-1)*CellGapSmall) DIV 2);
      end else begin
        grpArea.x := BMPWindowX + (BMPWindowWidth DIV 2) - ((targetBMP^.scaled[GridType]^.Width  + (cols-1)*CellGapSmall) DIV 2);
        grpArea.y := BMPWindowY;
      end;
      
      InitializeCellArea(AniMode.cellGrp, targetBMP, CellGapSmall, true);   
      for  i := Low(cells) to High(cells) do cells[i].bmpPtr := targetBMP;     
      LabelSetText(RegionWithID('AniBmpActiveLbl'), cells[0].bmpPtr^.scaled[Original]^.name);
      UpdateGroupSize(AniMode.cellGrp, CellGapSmall);  
      UpdateBitmapPointers(AniMode, targetBMP); 
		end;
	end;
  
  procedure ChangeSourceBMPType(newGridType: BMPType;var AniMode: AnimationEditorValues);
  begin
    with AniMode.cellGrp do
    begin
      GridType := newGridType;
      
      if Length(cells) = 0 then exit;
      
      UpdateAllAniBitmaps(cells[0].bmpPtr, AniMode);
    end;
  end;
		  
  procedure UpdateAniCellStripFromTextBox(var AniMode: AnimationEditorValues);
  begin
    with AniMode do
    begin
      if (RegionPanel(RegionOfLastUpdatedTextBox) = panels[AniStats]) AND (selectedIdx >= 0) then
      begin	
      	case IndexOfLastUpdatedTextBox of 
          0: ValidateInput(aniStrips[selectedIdx].timing); // := StrToInt(TextBoxText(GUITextBoxOfTextEntered));
          1: ValidateInput(aniStrips[selectedIdx].cellGrp.cells[aniStrips[selectedIdx].cellGrp.selectedOrder[0]].cellIdx); // := StrToInt(TextBoxText(GUITextBoxOfTextEntered));
          2: aniStrips[selectedIdx].cellGrp.cells[aniStrips[selectedIdx].cellGrp.selectedOrder[0]].identifier := TextBoxText(GUITextBoxOfTextEntered);
          3: aniStrips[selectedIdx].cellGrp.cells[aniStrips[selectedIdx].cellGrp.selectedOrder[0]].sound := TextBoxText(GUITextBoxOfTextEntered);
        end;       
        RefreshIDList(AniMode);
      end;
    end;
  end;
	
  //---------------------------------------------------------------------------
  // Cell Dragging and Deleting
  //--------------------------------------------------------------------------- 
    
  procedure SourceCellDragging(cellGrp: CellGroupData; var sharedVals: EditorValues);
  begin
    if sharedVals.dragCell <> nil then exit;
    
    sharedVals.dragCell := CellAreaAt(cellGrp, MousePosition, CellGapSmall);
    PickUpCell(sharedVals);
  end;
  
  procedure LastCellDragging(var sharedVals: EditorValues;var AniMode: AnimationEditorValues; index: Integer);
  var
    target: CellAreaPtr;
    dragIdx: Integer;
  begin
    with AniMode.aniStrips[index] do
    begin
      if (sharedVals.dragCell <> nil) AND (sharedVals.dragCell^.parent^.GridType = AnimationGroup) then
      begin
        target := CellAreaAt(lastCell, MousePosition, CellGapLarge);
        WriteLn('lastCell = ' , target = nil);
        if target = nil then exit;
        dragIdx := sharedVals.dragCell^.idx;
        AniMode.aniStrips[CalculateParentStripID(target^, AniMode)].lastCellParentID := CalculateParentStripID(sharedVals.dragCell^, AniMode);
        WriteLn(AniMode.aniStrips[CalculateParentStripID(target^, AniMode)].lastCellParentID);
        DropData(sharedVals.dragCell, target);
        cellGrp.Cells[0].idx := dragIdx;
      end;
    end;
  end;
  
  procedure ShiftDataInAnimationUp(var cellGrp: CellGroupData; const index: Integer);
  var
    i: integer;
  begin
    with cellGrp do
    begin
      for i := High(cells) - 1 downto index do
      begin
        if i = Low(cells) then exit;
        InitialCellAreaValues(cells[i], i, cells[i-1].cellIdx, cells[i-1].bmpPtr, @cellGrp, cells[i-1].identifier, cells[i-1].sound );
      end;
    end;
  end;
  
  procedure IncreaseAniStripLength(var AniMode: AnimationEditorValues; const count, index : Integer);
  begin
    with AniMode.aniStrips[index] do
    begin
      cellGrp.CellCount += count;
      cellGrp.cols += count;
      InitializeCellArea(cellGrp, nil, CellGapLarge, false);
      UpdateGroupSize(cellGrp, CellGapLarge);
      MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(LastCell.grpArea.y));
      UpdatePosition(LastCell);
    end;
    PositionAniHorizontalScrollButtons(AniMode);
  end;
  
  procedure AniCellDragging(var sharedVals: EditorValues; var AniMode: AnimationEditorValues; index: Integer);
  var
    target: CellAreaPtr;
    i, idx : integer;
  begin
    with AniMode.aniStrips[index] do
    begin
      if (sharedVals.dragCell <> nil) then 
      begin     
        target := CellAreaAt(cellGrp, MousePosition, CellGapLarge); 
        if target = nil then exit;
        if(sharedVals.dragCell^.parent^.GridType = AnimationGroup) AND (CalculateParentStripID(sharedVals.dragCell^, AniMode) = CalculateParentStripID(target^, AniMode)) 
          AND (target^.idx <> High(cellGrp.cells)) then
        begin
          WriteLn('Swapping');
          SwapData(sharedVals.dragCell, target);
        end else if(sharedVals.dragCell^.parent^.GridType = SourceGroup) then
        begin
          if not PointInRect(MousePosition, target^.area.x, target^.area.y, cellGrp.cellW, cellGrp.cellH) then
          begin
            idx := target^.idx + 1;          
            IncreaseAniStripLength(AniMode, 1, index);
            ShiftDataInAnimationUp(cellGrp, idx);
            target := @cellGrp.cells[idx];
            DropData(sharedVals.dragCell, target);      
          end else begin
            DropData(sharedVals.dragCell, target);      
            if (cellGrp.cells[High(cellGrp.cells)].bmpPtr <> nil) then
            begin         
              IncreaseAniStripLength(AniMode, 1, index);
            end;
          end;
        end;
      end else begin 
        WriteLn('Pickup');
        sharedVals.dragCell := CellAreaAt(cellGrp, MousePosition, CellGapLarge);
        PickUpCell(sharedVals);
      end;
    end;
  end;
  {
  procedure AniCellDragging(var sharedVals: EditorValues; var AniMode: AnimationEditorValues; index: Integer);
  var
    target: CellAreaPtr;
    i, idx : integer;
  begin
    with AniMode.aniStrips[index] do
    begin
      if (sharedVals.dragCell <> nil) AND (sharedVals.dragCell^.parent^.GridType = SourceGroup) then
      begin
        target := CellAreaAt(cellGrp, MousePosition, CellGapLarge);
        if target = nil then exit;
        if not PointInRect(MousePosition, target^.area.x, target^.area.y, cellGrp.cellW, cellGrp.cellH) then
        begin
          idx := target^.idx + 1;          
          IncreaseAniStripLength(AniMode, 1, index);
          ShiftDataInAnimationUp(cellGrp, idx);
          target := @cellGrp.cells[idx];
          DropData(sharedVals.dragCell, target);      
        end else begin
          DropData(sharedVals.dragCell, target);      
          if (cellGrp.cells[High(cellGrp.cells)].bmpPtr <> nil) then
          begin         
            IncreaseAniStripLength(AniMode, 1, index);
          end;
        end;
      end else begin 
        DragAndDrop(cellGrp, sharedVals, CellGapLarge);
      end;
    end;
  end;
  }
  procedure AddMultiple(var AniMode: AnimationEditorValues);
  var
    dest, count: GUITextBox;
    aniID, countVal : Integer;
  begin
    dest := TextBoxFromRegion(RegionWithID('AniAddMultiDest'));
    count := TextBoxFromRegion(RegionWithID('AniAddMultiCount'));
    
    if (not TryStrToInt(TextBoxText(dest), aniID)) OR (not TryStrToInt(TextBoxText(count), countVal)) OR (aniID > AniMode.stripCount) then exit;
    with AniMode.aniStrips[aniID] do
    begin
      IncreaseAniStripLength(AniMode, countVal, aniID);
    end;
    
    TextBoxSetText(dest, '-');
    TextBoxSetText(count, '-');
  end;
  
  procedure AddSelected(var AniMode: AnimationEditorValues);
  var
    i, j, oldLength, aniID: Integer;
    dest: GUITextBox;
  begin
    dest  := TextBoxFromRegion(RegionWithID('AniAddSelDest'));
    
    if (not TryStrToInt(TextBoxText(dest), aniID)) OR (aniID > AniMode.stripCount) OR (Length(AniMode.cellGrp.selectedOrder) = 0) then exit;
    
    with AniMode.aniStrips[aniID] do
    begin
      oldLength := AniMode.aniStrips[aniID].cellGrp.cellCount -1;
      IncreaseAniStripLength(AniMode, Length(AniMode.cellGrp.selectedOrder), aniID);

      for i := 0 to High(AniMode.cellGrp.selectedOrder) do
      begin
        cellGrp.cells[oldLength+i].bmpPtr := AniMode.cellGrp.cells[AniMode.cellGrp.selectedOrder[i]].bmpPtr;
        cellGrp.cells[oldLength+i].cellIdx := AniMode.cellGrp.cells[AniMode.cellGrp.selectedOrder[i]].cellIdx;
      end;
    end;
    TextBoxSetText(dest, '-');
  end;
 
  procedure DeleteAnimationStrip(var AniMode: AnimationEditorValues; index: Integer);
  var
    i, j, tempX, tempY: Integer;
  begin 
    with AniMode do
    begin
      if (Length(aniStrips[index].cellGrp.cells) <> 0) then exit;
      for j := index to AniMode.stripCount do
      begin
        if j <> AniMode.stripCount then
        begin
          tempX := Trunc(aniStrips[j].cellGrp.grpArea.x);
          tempY := Trunc(aniStrips[j].cellGrp.grpArea.y);
          aniStrips[j] := aniStrips[j+1];
          aniStrips[j].idx := index;
          MoveAniStrip(aniStrips[j], tempX, tempY);
          UpdateCellPointers(aniStrips[j].lastCell.cells[0], aniStrips[j+1].lastCell.cells[0].bmpPtr, @aniStrips[j].lastCell);
          for i := Low(aniStrips[j].cellGrp.cells) to High(aniStrips[j].cellGrp.cells) do
          begin
            UpdateCellPointers(aniStrips[j].cellGrp.cells[i],aniStrips[j+1].cellGrp.cells[i].bmpPtr, @aniStrips[j].cellGrp);
            aniStrips[j].cellGrp.cells[i].isSelected := false;
          end;
        end;
      end;
      for i := Low(aniStrips[High(aniStrips)].cellGrp.cells) to High(aniStrips[High(aniStrips)].cellGrp.cells) do
      begin
        UpdateCellPointers(aniStrips[High(aniStrips)].cellGrp.cells[i],nil, nil);
      end;
      UpdateCellPointers(aniStrips[High(aniStrips)].lastCell.cells[0],nil, nil);
      AniMode.stripCount -= 1;
    end;
  end; 

  procedure DeleteAniStrip(var AniMode: AnimationEditorValues; index: Integer);
  begin
    with AniMode.aniStrips[index] do
    begin
      if (Length(AniMode.aniStrips[index].lastCell.selectedOrder) <> 0) then
      begin
        lastCell.cells[0].bmpPtr := nil;
        lastCellParentID := -1;
      end;
      DeleteSelected(AniMode.aniStrips[index].cellGrp, CellGapLarge);
      cellGrp.cols := cellGrp.cellCount;
      UpdateGroupSize(cellGrp, CellGapLarge);
      if Length(cellGrp.cells) <> 0 then MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(cellGrp.grpArea.y));
      UpdatePosition(LastCell); 
      DeleteAnimationStrip(AniMode, index);
      RefreshIDList(AniMode);
      PositionAniHorizontalScrollButtons(AniMode);
    end;
  end;  
   
  //---------------------------------------------------------------------------
  // Preview Animation
  //---------------------------------------------------------------------------  
 
  procedure RefreshPreview(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  var
    i: integer;
  const
    xPos = 483; // Position of preview Sprite
    yPos = 122; // Position of preview Sprite
  begin
    with AniMode do
    begin
      if (cellGrp.cellCount = 0) then exit;
      if previewSprite <> nil then FreeSprite(previewSprite);
      ExportAnimation(aniStrips, PathToResource('/animations/testsave.txt'));
      if aniTemp <> nil then FreeAnimationTemplate(aniTemp);
      aniTemp := LoadAnimationTemplate('testsave.txt');
      previewSprite := CreateSprite(cellGrp.cells[0].bmpPtr^.scaled[PreviewGroup], aniTemp);
      ListClearItems(RegionWithID('AniList'));
      for i := 0 to NameCount(aniTemp^.animationIDs)-1 do
      begin
        ListAddItem(RegionWithID('AniList'), NameAt(aniTemp^.animationIDs, i));
      end;
      previewSprite^.position.x := xPos;
      previewSPrite^.position.y := yPos;
    end;
  end;
 
  //---------------------------------------------------------------------------
  // Update Animation Editor
  //--------------------------------------------------------------------------- 
   
  procedure HandleSourceCells(var AniMode: AnimationEditorValues;var sharedVals: EditorValues);
  begin   
    with AniMode do
    begin   
      if (not PointInRect(MousePosition, BMPWindowX, BMPWindowY, BMPWindowWidth, BMPWindowHeight)) then exit;
      if CheckboxState(RegionWithID('Drag')) then DragCellGroup(cellGrp, sharedVals);     
      HandleSelection(cellGrp, sharedVals.dragCell);
      if MouseClicked(RightButton) then SourceCellDragging(cellGrp, sharedVals);
    end;
  end;
  
  procedure HandlePreview(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  begin
    with AniMode do
    begin
      if previewSprite <> nil then DrawSprite(previewSprite);

      if (ListActiveItemIndex(RegionWithID('AniList')) <> -1) AND (ListActiveItemText(RegionWithID('AniList')) <> '') AND (RegionClickedID = 'AniList') then
      begin
          SpriteStartAnimation(previewSprite, ListActiveItemText(RegionWithID('AniList')));
      end;
      if (ListActiveItemIndex(RegionWithID('AniList')) <> -1) then UpdateSpriteAnimation(previewSprite);
    end;    
  end;
  
  procedure UpdateFromGUI(var AniMode: AnimationEditorValues;var sharedVals: EditorValues);
  begin
    with AniMode do
    begin      
      if (RegionClickedID() = 'NewStrip') then
      begin      
        CreateAnimationStrip(1, AniMode);
        PositionAniHorizontalScrollButtons(AniMode);
        selectedIdx := -2;
      end;
      
      if sharedVals.BitmapPtr <> nil then UpdateAllAniBitmaps(sharedVals.BitmapPtr,  AniMode);
      
      if (RegionClickedID() = 'ExportAnimationStrip') then DoSaveDialog(sharedVals, SaveAni);
      if (RegionClickedID() = 'LoadAniIntoEditor') then DoOpenDialog(sharedVals, LoadAniEdit);
      if (RegionClickedID() = 'AniChangeBitmap') then ShowPanel(sharedVals.panels[BrowserPanel]);
      if (RegionClickedID() = 'BrowseSound') then DoOpenDialog(sharedVals, SetSoundPath);
      if (RegionClickedID() = 'AniAddMultiple') then AddMultiple(AniMode);
      if (RegionClickedID() = 'AniAddSelection') then AddSelected(AniMode);
      if (RegionClickedID() = 'RefreshPreview') then RefreshPreview(AniMode, sharedVals);
      if (RegionClickedID() = 'AniClearBitmap') then UpdateAllAniBitmaps(nil, AniMode);
      if (RegionClickedID() = 'RemoveID') then RemoveIDFromList(AniMode, 'IDList', 'i');
      if (RegionClickedID() = 'RemoveSound') then RemoveIDFromList(AniMode, 'SoundList', 's');
      if (RegionClickedID() = 'Fit') then ChangeSourceBMPType(SourceGroup, AniMode);
      if (RegionClickedID() = 'Full') then ChangeSourceBMPType(Original, AniMode);
      
      if DialogComplete AND (sharedVals.OpenSave = SaveAni) then ExportAnimation(aniStrips, dialogPath);
      if DialogComplete AND (sharedVals.OpenSave = SetSoundPath) then
      begin
        aniStrips[selectedIdx].cellGrp.cells[aniStrips[selectedIdx].cellGrp.selectedOrder[0]].sound := dialogPath;
        RefreshIDList(AniMode);
      end;
      if DialogComplete AND (sharedVals.OpenSave = LoadAniEdit) then
      begin
        LoadAnimation(AniMode);
        RefreshIDList(AniMode);
      end;
      
      if GUITextEntryComplete  then UpdateAniCellStripFromTextBox(AniMode);
                   
      DisplaySelectedCellStats(AniMode); 
      ScrollAnimationsVertical(AniMode);
      ScrollAnimationsHorizontal(AniMode);      
    end;
  end;
  
  procedure HandleAnimationStrips(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  var
		i: integer;
  begin
    with AniMode do
    begin
      for i := 0 to AniMode.stripCount do
      begin
      	HandleSelection(aniStrips[i].cellGrp, sharedVals.dragCell);
				HandleSelection(aniStrips[i].lastCell, sharedVals.dragCell);
        if KeyTyped(vk_DELETE) then
        begin
          if i = Length(aniStrips) then break;
          DeleteAniStrip(AniMode, i);
        end;
        if MouseClicked(RightButton) then
        begin       
          AniCellDragging(sharedVals, AniMode, i);
          LastCellDragging(sharedVals, AniMode, i);           
        end;       
        if KeyTyped(vk_ESCAPE) then DeselectAll(aniStrips[i].cellGrp);
        FillRectangle(RGBAColor(255,0,0,100), aniStrips[i].cellGrp.grpArea.x, aniStrips[i].cellGrp.grpArea.y, aniStrips[i].cellGrp.grpArea.width, aniStrips[i].cellGrp.grpArea.height);
        end;
    end;
  end;
 
    
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
	begin    
    with AniMode do
    begin
      DrawAnimationEditor(AniMode, sharedVals);	
			UpdateFromGUI(AniMode, sharedVals);
           
      HandleSourceCells(AniMode, sharedVals);
      HandlePreview(AniMode, sharedVals);
      
      if regionclicked <> nil then exit;
      
      HandleAnimationStrips(AniMode, sharedVals);
 		end;
	end;
end.