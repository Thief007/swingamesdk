unit AnimationEditor;

//=============================================================================
interface
		uses sgTypes, sgCore, sgGraphics, sgResources,
  sgGeometry, sgImages, sgInput, SysUtils, sgSprites, sgAnimations, 
	sgUserInterface, sgShared, EditorShared, sgNamedIndexCollection, sgText;
	
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues;var SharedVals : EditorValues);
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	
implementation

const
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
	var
		i: LongInt;
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
      if PanelVisible(panels[AniBitmapDetails]) then DrawSourceCells(cellGrp, sharedVals);
      for i := Low(aniStrips) to High(aniStrips) do
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
    for i := Low(AniMode.anistrips) to High(AniMode.anistrips) do
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
          WriteLn('Left');          
          scrollOffset += 1;
          MoveAniStrip(AniMode.aniStrips[i + AniMode.scrolloffSet], Trunc(InitialAnimationPosX + ((cellGrp.cellW+CellGapLarge)*scrollOffSet)), Trunc(cellGrp.grpArea.y));
        end else
        if (RegionClickedID = ('Right'+IntToStr(i+1))) AND (Length(cellGrp.Cells) >= 16) AND (scrollOffset > 16 - Length(cellGrp.cells)) then
        begin
          WriteLn('Right');
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
  
  function SelectedIdx(aniStrip: AniStripArray) : Integer;
  var
    i, count: Integer;
  begin
    count := 0;
    result := -2;
    for i := Low(aniStrip) to High(aniStrip) do
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
    for i := Low(AniMode.aniStrips) to High(AniMode.aniStrips) do
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
  
  //---------------------------------------------------------------------------
  // Update Animation Editor
  //--------------------------------------------------------------------------- 
  
  procedure ShowCellDetails(idx, cellidx, groupID, stringId, soundID : string);
  var
    ID, cellIndex, groupIndex: GUILabel;
    aniIdtxt, soundtxt : GUITextBox;
  begin
    ID           := LabelFromRegion(RegionWithID('AniIDDisp'));
    cellIndex    := LabelFromRegion(RegionWithID('AniCellIndexDisp'));
    GroupIndex   := LabelFromRegion(RegionWithID('AniGroupIDDisp'));
    aniIdtxt     := TextBoxFromRegion(RegionWithID('AnimationIDDisp'));
    soundtxt     := TextBoxFromRegion(RegionWithID('SoundIDDisp'));
      
    LabelSetText(ID, idx);
    LabelSetText(cellIndex, cellidx);
    LabelSetText(groupIndex, groupID);
    TextBoxSetText(aniIdtxt, stringId);
    TextBoxSetText(soundtxt, soundId);
  end;
  
  procedure DisplaySelectedCellStats(var AniMode : AnimationEditorValues);
  var
    selected, index: Integer;
  const
    None = 'None';
    Multiple = 'Multiple';
  begin
    selected:= SelectedIdx(AniMode.aniStrips);
    
    if (selected = AniMode.selectedIdx) then 
    begin
      if selected < 0 then exit;
      index := AniMode.aniStrips[selected].cellGrp.cells[AniMode.aniStrips[selected].cellGrp.selectedOrder[0]].idx;
      if (IntToStr(Index) = LabelText(LabelFromRegion(RegionWithID('AniIDDisp')))) then exit;
    end;
    AniMode.selectedIdx := selected;
    
    case selected of 
      -1 : ShowCellDetails('*', Multiple, Multiple, Multiple, Multiple);
      -2 : ShowCellDetails(None, None, None, None, None);
    else
      with AniMode.aniStrips[selected] do
      begin
        index := cellGrp.selectedOrder[0];
        ShowCellDetails(IntToStr(cellGrp.cells[index].Idx), 
                        IntToStr(cellGrp.cells[index].cellIdx), 
                        IntToStr(selected), 
                        cellGrp.cells[index].identifier, 
                        cellGrp.cells[index].sound);                                                    
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
      cellGrp 	 := InitializeCellGroup(cellCount,cols,rows,0,0,0,0,0,SourceGroup);
    end;    
  end;
  
  procedure UpdateAllAniBitmaps(var cellGrp: CellGroupData; targetBMP: LoadedBitmapPtr; var aniStrips: AniStripArray);
	var
		ratio, tempW, tempH, i: integer;
		scale : single;
	begin
		with cellGrp do
		begin
			if (targetBMP = nil) then
      begin
        ResetSourceCells(cellGrp);
        exit;
      end;

      cellW     := targetBMP^.scaled[SourceGroup]^.cellW;
      cellH     := targetBMP^.scaled[SourceGroup]^.cellH;
      cols      := targetBMP^.scaled[SourceGroup]^.cellCols;
      rows      := targetBMP^.scaled[SourceGroup]^.cellRows;
      cellCount := targetBMP^.scaled[SourceGroup]^.cellCount;
      
      if targetBMP^.scaled[SourceGroup]^.Width > targetBMP^.scaled[SourceGroup]^.Height then
      begin
        grpArea.x := BMPWindowX;
        grpArea.y := BMPWindowY + (BMPWindowHeight DIV 2) - ((targetBMP^.scaled[SourceGroup]^.Height  + (rows-1)*CellGapSmall) DIV 2);
      end else begin
        grpArea.x := BMPWindowX + (BMPWindowWidth DIV 2) - ((targetBMP^.scaled[SourceGroup]^.Width  + (cols-1)*CellGapSmall) DIV 2);
        grpArea.y := BMPWindowY;
      end;
      
      for  i := Low(cells) to High(cells) do
      begin
          cells[i].bmpPtr := targetBMP;
      end;
      
      InitializeCellArea(cellGrp, targetBMP, CellGapSmall, true);   
      UpdateGroupSize(cellGrp, CellGapSmall);  
      UpdateBitmapPointers(cellGrp, aniStrips); 
		end;
	end;
		  
  procedure UpdateAniCellStripFromTextBox(var AniMode: AnimationEditorValues);
  begin
    with AniMode do
    begin
      if (RegionPanel(RegionOfLastUpdatedTextBox) = panels[AniStats]) AND (selectedIdx >= 0) then
      begin	
      	case IndexOfLastUpdatedTextBox of 
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
  
  procedure LastCellDragging(cellGrp: CellGroupData; var sharedVals: EditorValues;var AniMode: AnimationEditorValues);
  var
    target: CellAreaPtr;
    dragIdx: Integer;
  begin
    with sharedVals do
    begin
      if (dragCell <> nil) AND (dragCell^.parent^.GridType = AnimationGroup) then
      begin
        target := CellAreaAt(cellGrp, MousePosition, CellGapLarge);
        if target = nil then exit;
        dragIdx := dragCell^.idx;
        AniMode.aniStrips[CalculateParentStripID(target^, AniMode)].lastCellParentID := CalculateParentStripID(dragCell^, AniMode);
        WriteLn(AniMode.aniStrips[CalculateParentStripID(target^, AniMode)].lastCellParentID);
        DropData(dragCell, target);
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
  
  procedure IncreaseAniStripLength(var cellGrp, lastCell: CellGroupData; AniMode: AnimationEditorValues; const count : Integer);
  begin
    cellGrp.CellCount += count;
    cellGrp.cols += count;
    InitializeCellArea(cellGrp, nil, CellGapLarge, false);
    UpdateGroupSize(cellGrp, CellGapLarge);
    MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(LastCell.grpArea.y));
    UpdatePosition(LastCell);
    PositionAniHorizontalScrollButtons(AniMode);
  end;
  
  procedure AniCellDragging(var cellGrp, lastCell: CellGroupData; var sharedVals: EditorValues; AniMode: AnimationEditorValues);
  var
    target: CellAreaPtr;
    i, idx : integer;
  begin
    with sharedVals do
    begin
      if (dragCell <> nil) AND (dragCell^.parent^.GridType = SourceGroup) then
      begin
        target := CellAreaAt(cellGrp, MousePosition, CellGapLarge);
        if target = nil then exit;
        if not PointInRect(MousePosition, target^.area.x, target^.area.y, cellGrp.cellW, cellGrp.cellH) then
        begin
          idx := target^.idx + 1;          
          IncreaseAniStripLength(cellGrp, lastCell, AniMode, 1);
          ShiftDataInAnimationUp(cellGrp, idx);
          target := @cellGrp.cells[idx];
          DropData(dragCell, target);      
        end else begin
          DropData(dragCell, target);      
          if (cellGrp.cells[High(cellGrp.cells)].bmpPtr <> nil) then
          begin         
            IncreaseAniStripLength(cellGrp, lastCell, AniMode, 1);
          end;
        end;
      end else begin 
        DragAndDrop(cellGrp, sharedVals, CellGapLarge);
      end;
    end;
  end;
  
  procedure AddMultiple();
  begin
    IncreaseAniStripLength(cellGrp, lastCell, AniMode, 1);
  end;
 
  procedure DeleteAnimationStrip(var AniMode: AnimationEditorValues; index: Integer);
  var
    i, j, tempX, tempY: Integer;
  begin 
    with AniMode do
    begin
      if (Length(aniStrips[index].cellGrp.cells) <> 0) then exit;
      for j := index to High(anistrips) do
      begin
        if j <> High(aniStrips) then
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
      SetLength(aniStrips, Length(aniStrips) -1);
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
      if Length(cellGrp.cells) <> 0 then MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(cellGrp.grpArea.y));
      UpdatePosition(LastCell); 
      DeleteAnimationStrip(AniMode, index);
      RefreshIDList(AniMode);
      PositionAniHorizontalScrollButtons(AniMode);
    end;
  end;  
 
  //---------------------------------------------------------------------------
  // Update Animation Editor
  //--------------------------------------------------------------------------- 
 
  procedure ChangePanels(pnl1, pnl2 : Panel; var oldIdx: Integer; newIdx : Integer);
  begin
    ToggleShowPanel(pnl1);
    ToggleShowPanel(pnl2);
    oldIdx := newIdx;
  end;
  
  procedure HandleSourceCells(var AniMode: AnimationEditorValues;var sharedVals: EditorValues);
  begin   
    with AniMode do
    begin
      
      if CheckboxState(RegionWithID('Drag')) then DragCellGroup(cellGrp, sharedVals);
      
      HandleSelection(cellGrp, sharedVals.dragCell);
      if MouseClicked(RightButton) then SourceCellDragging(cellGrp, sharedVals);
    end;
  end;
  
  procedure UpdateFromGUI(var AniMode: AnimationEditorValues;var sharedVals: EditorValues);
  begin
    with AniMode do
    begin      
      if (RegionClickedID() = 'NewStrip') then
      begin      
        CreateAnimationStrip(AniMode.aniStrips, 1);
        PositionAniHorizontalScrollButtons(AniMode);
        selectedIdx := -2;
      end;
      
      if sharedVals.BitmapPtr <> nil then UpdateAllAniBitmaps(cellGrp, sharedVals.BitmapPtr, aniStrips);
      if (RegionClickedID() = 'ExportAnimationStrip') then DoSaveDialog(sharedVals, SaveAni);
      if (RegionClickedID() = 'LoadAniIntoEditor') then DoOpenDialog(sharedVals, LoadAniEdit);
      if (RegionClickedID() = 'AniChangeBitmap') then ShowPanel(sharedVals.panels[BrowserPanel]);
      if (RegionClickedID() = 'BrowseSound') then DoOpenDialog(sharedVals, SetSoundPath);
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
      for i := Low(aniStrips) to High(aniStrips) do
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
          AniCellDragging(aniStrips[i].cellGrp, aniStrips[i].LastCell, sharedVals, AniMode);
          LastCellDragging(aniStrips[i].LastCell, sharedVals, AniMode);           
        end;       
        if KeyTyped(vk_ESCAPE) then DeselectAll(aniStrips[i].cellGrp);
      end;
    end;
  end;
  
  procedure RefreshPreview(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  var
    i: integer;
  const
    xPos = 400; // Middle of panel based on scale
    yPos = 60; // Middle of panel based on scale
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
  
  procedure HandlePreview(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  begin
    with AniMode do
    begin
      if previewSprite <> nil then DrawSprite(previewSprite);
      if RegionClickedID = 'RefreshPreview' then RefreshPreview(AniMode, sharedVals);

      if (ListActiveItemIndex(RegionWithID('AniList')) <> -1) AND (ListActiveItemText(RegionWithID('AniList')) <> '') AND (RegionClickedID = 'AniList') then
      begin
          SpriteStartAnimation(previewSprite, ListActiveItemText(RegionWithID('AniList')));
      end;
      if (ListActiveItemIndex(RegionWithID('AniList')) <> -1) then UpdateSpriteAnimation(previewSprite);
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