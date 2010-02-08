unit AnimationEditor;

//=============================================================================
interface
		uses sgTypes, sgCore, sgGraphics,
  sgGeometry, sgImages, sgInput, SysUtils, 
	sgUserInterface, sgShared, EditorShared, sgText;
	
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues;var SharedVals : EditorValues);
	procedure InitializeAnimationEditor(out AniMode: AnimationEditorValues);
	
const
	BMPWindowWidth 		= 320;
	BMPWindowHeight 	= 190;
	BMPWindowX 				= 57;
	BMPWindowY 				= 85;
	AniBitmapDetails  = 0;
	AniDetailTabs 		= 1;
	AniCellBMPNames 	= 2;
	AniMenuPanel 			= 3;
	AniStats 			    = 4;
	AniScroll			    = 5;
	Preview1 			    = 6;
	CellGapSmall 			= 2;
	CellGapLarge 			= 15;
	
//FilmStrip
	FilmStripStartX			= 30;
	FilmStripY					= 340;
	FilmStripSEWidth 		= 10;
	FilmStripSEHeight		= 71;
	
	InitialAnimationPosX	= 50;
	FilmStripStartPos	=12;
	FilmStripYOffSet	=18;
	InitialAnimationPosY	= 340;
	AnimationIncrPosY	= 97;
  FilmOffSet = -7;
  
  AniCellWidth = 24;
  AniCellHeight = 32;

implementation

	//---------------------------------------------------------------------------
  // Initialize Animation Editor
  //--------------------------------------------------------------------------- 
 
	function InitializeAnimationPanels(): PanelArray;
	var
		i: LongInt;
	begin
		SetLength(result, 7);
		result[AniBitmapDetails]:= LoadPanel('AniBitmapDetails.txt');
		result[AniDetailTabs]		:= LoadPanel('AniDetailTabs.txt');
		result[AniCellBMPNames]	:= LoadPanel('AniCellBMPNames.txt');
		result[AniMenuPanel]	:= LoadPanel('AniMenuPanel.txt');
		result[AniStats]	:= LoadPanel('AniStats.txt');
		result[AniScroll]	:= LoadPanel('AniScroll.txt');
		result[Preview1]	:= LoadPanel('PreviewPanel1.txt');
		for i := Low(result) to High(result) do
		begin
			ActivatePanel(result[i]); 
		end;
		ListAddItem(ListFromRegion(RegionWithID('AniBMPList')), 'None');
		ListSetActiveItemIndex(ListFromRegion(RegionWithID('AniBMPList')), 0);	
		LabelSetText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')), ListActiveItemText(result[AniCellBMPNames], 'AniBMPList'));
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
		end;
	end;
    
  procedure PositionAniHorizontalScrollButtons(AniMode: AnimationEditorValues);
  var
    buttonArray: Array [0..2] of Region;
    i: Integer;
  begin
    buttonArray[0] := RegionWithID('Right1');
    buttonArray[1] := RegionWithID('Right2');
    buttonArray[2] := RegionWithID('Right3');
    
    with AniMode do
    begin
      for i:= Low(buttonArray) to High(buttonArray) do
      begin
        if Length(aniStrips) >= 1+i then
        begin
          if Length(aniStrips[scrollOffset+i].cellGrp.cells) >= 16 then
            buttonArray[0+i]^.area.x := 713
          else
            buttonArray[0+i]^.area.x := aniStrips[scrollOffset+i].cellGrp.cells[High(aniStrips[scrollOffset+i].cellGrp.cells)].area.x + (AniMode.middle.width)*2
        end else
          buttonArray[0+i]^.area.x := -50;
        buttonArray[0+i] := nil;
      end;
    end;         
  end;
  
	procedure CreateAnimationStrip(var AniMode: AnimationEditorValues);
  var
    i: Integer;
	begin
		with AniMode do
		begin
			SetLength(aniStrips, Length(aniStrips) + 1);
      
      aniStrips[High(aniStrips)].idx 	:= 	High(aniStrips); 
      
      aniStrips[High(aniStrips)].cellGrp  := InitializeCellGroup(1, 1, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * High(aniStrips)), CellGapLarge, AnimationGroup);
      aniStrips[High(aniStrips)].LastCell := InitializeCellGroup(1, 1, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * High(aniStrips)), CellGapLarge, AnimationGroup);
      
      InitializeCellArea(aniStrips[High(aniStrips)].cellGrp, nil, CellGapLarge);
      InitializeCellArea(aniStrips[High(aniStrips)].LastCell, nil, CellGapLarge);
      
      selectedIdx := -2;
      aniStrips[High(aniStrips)].scrollOffSet := 0;
      aniStrips[High(aniStrips)].lastCellParentID := -1;
      
      for i := Low(aniStrips) to High(aniStrips) do DeselectAll(aniStrips[i].cellGrp);
      
      PositionAniHorizontalScrollButtons(AniMode);
     end;

    with AniMode.AniStrips[High(AniMode.AniStrips)] do
    begin
      MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(LastCell.grpArea.y));
      UpdatePosition(LastCell);
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
  begin
    with AniMode.aniStrips[aniIndex].cellGrp do
    begin
      PushClip(30, InitialAnimationPosY-19, 20, 267);
      DrawBitmapPart(AniMode.filmStrip, AniMode.startArrow, FilmStripStartX, cells[Low(cells)].area.y- FilmStripYOffSet);	
      PopClip();
      PushClip(47, InitialAnimationPosY-19, 702, 267);
      for  i := Low(cells) to High(cells) do
      begin			
        DrawBitmapPart(AniMode.filmStrip, AniMode.middle, cells[i].area.x + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
        DrawText(IntToStr(cells[i].idx) +','+ IntToStr(cells[i].cellIdx), ColorGreen, cells[i].area.x - 5, cells[i].area.y -7);  
      end;
      DrawBitmap(AniMode.pointArrow, cells[i].area.x + AniMode.middle.width + FilmOffSet,  cells[i].area.y - FilmStripYOffSet);
      DrawBitmapPart(AniMode.filmStrip, AniMode.middle, cells[i].area.x + (AniMode.middle.width)*2  + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
      PopClip();
      PushClip(30, InitialAnimationPosY-19, 752, 267);
      if Length(cells) >= 16 then
        DrawBitmapPart(AniMode.filmStrip, AniMode.finishArrow, 743, cells[i].area.y - FilmStripYOffSet)
      else
        DrawBitmapPart(AniMode.filmStrip, AniMode.finishArrow, cells[i].area.x + (AniMode.middle.width)*3  + FilmOffSet, cells[i].area.y - FilmStripYOffSet);
      PopClip();
    end;
  end;
  
  procedure DrawAnimationEditor(AniMode: AnimationEditorValues; sharedVals: EditorValues);
  var
    i: Integer;
  begin
    with AniMode do
    begin
      if PanelVisible(panels[AniBitmapDetails]) then DrawSourceCells(cellGrp, sharedVals);
      for i := Low(aniStrips) to High(aniStrips) do
			begin       
        DrawFilmStrip(AniMode, sharedVals, i);
        PushClip(47, InitialAnimationPosY-19, 702, 267);
        MyDrawSplitBitmapCells(aniStrips[i].cellGrp, sharedVals);
        MyDrawSplitBitmapCells(aniStrips[i].LastCell, sharedVals);
        if aniStrips[i].lastCellParentID <> -1 then
        begin
          DrawText((IntToStr(aniStrips[i].lastCellParentID)+ ','+IntToStr(aniStrips[i].lastCell.cells[0].idx)), ColorRed,
                    aniStrips[i].lastCell.cells[0].area.x -5, aniStrips[i].lastCell .cells[0].area.y + 33);
        end;
        if PanelVisible(panels[Preview1]) then Draw
        PopClip();
      end;
    end;
  end;
  
  procedure ScaleAniBitmap(group: BMPType; bmpArray: LoadedBitmaps; cols, rows, w, h, maxw, maxh, ratio: Integer);
  var
    scale: single;
  begin
    if ratio <> 0 then
      scale := maxw / w 
    else
      scale := maxh / h;
    
    InitializeBitmapDetails(group, bmpArray, scale, cols, rows); 
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
  
  function SelectedIdx(aniStrip: Array of AnimationStrip) : Integer;
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
  
  function GetAniIDByCellIdx(aniId: AniIDArray; idx: Integer): String;
  var
    i: Integer;
  begin
    result := '';
    for i := Low(aniId) to High(aniId) do
    begin
      if aniId[i].idx = idx then result := aniId[i].id;
    end;
  end;
  
  function GetAniIdxByCellIdx(aniId: AniIDArray; idx: Integer): Integer;
  var
    i: Integer;
  begin
    result := -1;
    for i := Low(aniId) to High(aniId) do
    begin
      if aniId[i].idx = idx then result := i;
    end;
  end;
  
  procedure AddAniID(var aniId: AniIDArray; id: string; index : Integer);
  var
    stringIdx: Integer;
  begin
      stringIdx := GetAniIdxByCellIdx(aniId, index);
      if stringIdx <> -1 then 
      begin
        aniId[stringIdx].id := id;
        exit;
      end;
      SetLength(aniId, Length(aniId) + 1);
      
      aniId[High(aniId)].id := id;
      aniId[High(aniId)].idx := index;
  end;
  
  function PurgeInvalidIds(aniId: AniIDArray; var aniStrip: AnimationStrip): AniIDArray;
  var
    i, j: Integer;
    stringId : AniIDArray;
  begin
    SetLength(result, 0);
    for i := Low(aniId) to High(aniId) do
    begin
      if not ((Trim(aniId[i].id) = '') OR (aniId[i].idx > High(aniStrip.cellGrp.cells)-1)) then
      begin
        SetLength(result, Length(result)+ 1);
        result[High(stringId)].id := aniId[i].id;
        result[High(stringId)].idx := aniId[i].idx;
      end;
    end;
  end;
  
  procedure PurgeAllInvalidIDs(var aniStrip: AnimationStrip);
  begin
    aniStrip.stringIDs  := PurgeInvalidIDs(aniStrip.stringIDs, aniStrip);
    aniStrip.soundIDs   := PurgeInvalidIDs(aniStrip.soundIDs, aniStrip);
  end;
  
  procedure RefreshIDList(var aniStrips: Array of AnimationStrip);
  var
    i, j: Integer;
  begin
    ListClearItems(RegionWithID('IDList'));
    ListClearItems(RegionWithID('SoundList'));
    for i := Low(aniStrips) to High(aniStrips) do
    begin
      with aniStrips[i] do
      begin
        for j:= Low(stringIDs) to High(stringIDs) do
        begin
          if Trim(stringIDs[j].id) <> '' then ListAddItem(RegionWithID('IDList'), stringIDs[j].id);
        end;
        for j:= Low(soundIDs) to High(soundIDs) do
        begin
          if Trim(soundIDs[j].id) <> '' then ListAddItem(RegionWithID('SoundList'), soundIDs[j].id);
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
      -1 : ShowCellDetails(Multiple, Multiple, Multiple, Multiple, Multiple);
      -2 : ShowCellDetails(None, None, None, None, None);
    else
      with AniMode.aniStrips[selected] do
      begin
        index := cellGrp.selectedOrder[0];
        ShowCellDetails(IntToStr(cellGrp.cells[index].Idx), 
                        IntToStr(cellGrp.cells[index].cellIdx), 
                        IntToStr(selected), 
                        GetAniIDByCellIdx(stringIDs, index), 
                        GetAniIDByCellIdx(soundIDs, index));                                                    
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
  
  procedure CalculateScale(var rect: rectangle;out scale: single; cols, rows, ratio: Integer; targetBMP: Bitmap);
  begin
    if ratio <> 0 then
    begin
      scale  := BMPWindowWidth / (targetBMP^.Width + (cols-1)*CellGapSmall);
      rect.x := BMPWindowX;
      rect.y := BMPWindowY + (BMPWindowHeight DIV 2) - ((targetBMP^.Height  + (rows-1)*CellGapSmall) DIV 2);
    end else begin
      scale  := BMPWindowHeight / (targetBMP^.Height + (rows-1)*CellGapSmall);
      rect.x := BMPWindowX + (BMPWindowWidth DIV 2) - ((targetBMP^.Width  + (cols-1)*CellGapSmall) DIV 2);
      rect.y := BMPWindowY;
    end;
  end;  
  
	procedure UpdateAllAniBitmaps(var cellGrp: CellGroupData; bmpArray: LoadedBitmaps; listIdx: Integer; original: integer);
	var
		ratio, tempW, tempH, i: integer;
		scale : single;
    targetBMP : LoadedBitmapPtr;
	begin
		with cellGrp do
		begin
			if (cols = 0) OR (rows = 0) then exit;
      targetBMP := SetLoadedBitmapPtr(listIdx -1, bmpArray);
			if (targetBMP = nil) then
      begin
        ResetSourceCells(cellGrp);
        exit;
      end;
      
      tempW := targetBMP^.original^.Width DIV cols;
      temph := targetBMP^.original^.Height DIV rows;
      cellCount := cols*rows;
			
      ratio := targetBMP^.original^.width DIV targetBMP^.original^.height;
      
      CalculateScale(grpArea, scale, cols, rows, ratio, targetBMP^.original);
      
      if original = 1 then scale := 1;
			
			cellW := Trunc(tempW * scale);
			cellH := Trunc(tempH * scale);
      
      for  i := Low(cells) to High(cells) do
      begin
          cells[i].bmpPtr := targetBMP;
      end;
      
      InitializeCellArea(cellGrp, targetBMP, CellGapSmall);
      UpdateGroupSize(cellGrp, CellGapSmall);
      InitializeBitmapDetails(SourceGroup, bmpArray, scale, cols, rows);  
      ScaleAniBitmap(AnimationGroup, bmpArray, cols, rows, tempW, tempH, aniCellWidth, aniCellHeight, ratio);  
      ScaleAniBitmap(PreviewGroup, bmpArray, cols, rows, tempW, tempH, aniCellWidth*2, aniCellHeight*2, ratio);     
      targetBMP := nil;
		end;
	end;
  
  procedure GetUpdateAllAniBitmapsIndexs(out radioIndex, listIndex : Integer);
  begin
    radioIndex := ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Fit')));
    listIndex  := ListActiveItemIndex(ListFromRegion(RegionWithID('AniBMPList')));
  end;
		
	procedure UpdateAniCellDetailsFromTextInput(var AniMode: AnimationEditorValues; BMPArray: LoadedBitmaps);
	var
		newVal, radioIndex, listIndex : LongInt;
	begin
		with AniMode do
		begin
      GetUpdateAllAniBitmapsIndexs(radioIndex, listIndex);
      
      if (RegionPanel(RegionOfLastUpdatedTextBox) = panels[AniBitmapDetails]) then
      begin	
        if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), newVal) AND (newVal > -1) then
        begin		
					case IndexOfLastUpdatedTextBox of 
						0: cellGrp.cols	:= newVal;
						1: cellGrp.rows  := newVal;
					end;
        UpdateAllAniBitmaps(cellGrp, bmpArray, listIndex, radioIndex);
        end else begin
          case IndexOfLastUpdatedTextBox of 
            0: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.cols));
            1: TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(cellGrp.rows));
          end;	
        end;
			end;
		end;
	end;
 
  
  procedure UpdateAniCellStripFromTextBox(var AniMode: AnimationEditorValues);
  begin
    with AniMode do
    begin
      if (RegionPanel(RegionOfLastUpdatedTextBox) = panels[AniStats]) AND (selectedIdx >= 0) then
      begin	
      	case IndexOfLastUpdatedTextBox of 
          1: AddAniID(aniStrips[selectedIdx].stringIDs, TextBoxText(GUITextBoxOfTextEntered), aniStrips[selectedIdx].cellGrp.selectedOrder[0]);
          2: AddAniID(aniStrips[selectedIdx].soundIDs, TextBoxText(GUITextBoxOfTextEntered), aniStrips[selectedIdx].cellGrp.selectedOrder[0]);
        end;
        RefreshIDList(aniStrips);
      end;
    end;
  end;
  
	
	procedure ChangeActiveBitmap(var AniMode: AnimationEditorValues; BMPArray: LoadedBitmaps);
  var
    listIndex, radioIndex: Integer;
	begin
		with AniMode do
		begin
			if PanelVisible(panels[AniCellBMPNames]) AND (ListActiveItemText(panels[AniCellBMPNames], 'AniBMPList') <> LabelText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')))) then
			begin
				ToggleShowPanel(panels[AniCellBMPNames]);
        GetUpdateAllAniBitmapsIndexs(radioIndex, listIndex);
        UpdateAllAniBitmaps(cellGrp, bmpArray, listIndex, radioIndex);        
				LabelSetText(LabelFromRegion(RegionWithID('AniBmpActiveLbl')), ListActiveItemText(panels[AniCellBMPNames], 'AniBMPList'));
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
  
  // Calculates the index of the animation strip based on the cell's position
  function CalculateParentStripID(cell: CellArea; AniMode: AnimationEditorValues): Integer;
  begin
    result := Trunc((cell.area.y - InitialAnimationPosY + (AniMode.scrollOffset*AnimationIncrPosY)) / AnimationIncrPosY);
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
        InitialCellAreaValues(cells[i], i, cells[i-1].cellIdx, cells[i-1].bmpPtr, @cellGrp);
      end;
    end;
  end;
  
  procedure IncreaseAniStripLength(var cellGrp, lastCell: CellGroupData; AniMode: AnimationEditorValues);
  begin
    cellGrp.CellCount += 1;
    cellGrp.cols += 1;
    InitializeCellArea(cellGrp, nil, CellGapLarge);
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
          IncreaseAniStripLength(cellGrp, lastCell, AniMode);
          ShiftDataInAnimationUp(cellGrp, idx);
          target := @cellGrp.cells[idx];
          DropData(dragCell, target);      
        end else begin
          DropData(dragCell, target);      
          if (cellGrp.cells[High(cellGrp.cells)].bmpPtr <> nil) then
          begin         
            IncreaseAniStripLength(cellGrp, lastCell, AniMode);
          end;
        end;
      end else begin 
        DragAndDrop(cellGrp, sharedVals, CellGapLarge);
      end;
    end;
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
      PurgeAllInvalidIds(AniMode.aniStrips[index]);
      RefreshIDList(AniMode.aniStrips);
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
  var
    listIndex, radioIndex: Integer;
  begin   
    with AniMode do
    begin
      if (not PanelVisible(panels[AniBitmapDetails])) then
      begin
        if PanelVisible(panels[AniCellBMPNames]) then HidePanel(panels[AniCellBMPNames]);
        exit;
      end;
      
      if CheckboxState(RegionWithID('Drag')) then DragCellGroup(cellGrp, sharedVals);
      if (RegionClickedID() = 'AniBmpActiveLbl') then ShowPanel(panels[AniCellBMPNames]);
      if originalSize <> ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Fit'))) then
      begin
        GetUpdateAllAniBitmapsIndexs(radioIndex, listIndex);
        UpdateAllAniBitmaps(cellGrp, sharedVals.bmpArray, listIndex, radioIndex);
        originalSize := radioIndex;
      end;
      ChangeActiveBitmap(AniMode,SharedVals.BMPArray);

      HandleSelection(cellGrp, sharedVals.dragCell);
      if MouseClicked(RightButton) then SourceCellDragging(cellGrp, sharedVals);
    end;
  end;
  
  procedure UpdateFromGUI(var AniMode: AnimationEditorValues;var sharedVals: EditorValues);
  begin
    with AniMode do
    begin      
      if (RegionClickedID() = 'NewStrip') then CreateAnimationStrip(AniMode);
      
      if GUITextEntryComplete  then 
      begin
        UpdateAniCellDetailsFromTextInput(AniMode, SharedVals.BMPArray);
        UpdateAniCellStripFromTextBox(AniMode);
      end;
      
      if (radio1 <>  ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('PreviewTab')))) then
        ChangePanels(panels[AniBitmapDetails], panels[Preview1], radio1, ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('PreviewTab'))));
             
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
    aniTemplate : AnimationTemplate;
  begin
    with AniMode do
    begin
      if (ListActiveItemIndex(ListFromRegion(RegionWithID('AniBMPList'))) = 0) OR
          cellGrp.cellCount = 0 then exit;
      if previewSprite <> nil then FreeSprite(previewSprite);
      ExportAnimation(aniStrips,'testsave');
      aniTemplate := LoadAnimationTemplate('testsave.txt');
      Erase(PathToResources('Animations\testsave.txt'));
      previewSprite := CreateSprite(cellGrp.cells[0].bmpPtr^.src[PreviewGroup], 
    end;
  end;
  
  procedure ChangeLoadedAnimation(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  begin
    
    
  end;
  
  procedure HandlePreview(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
  begin
    with AniMode do
    begin
      if (not PanelVisible(panels[Preview1])) then
      begin
      //  if PanelVisible(panels[AniCellBMPNames]) then HidePanel(panels[AniCellBMPNames]);
        exit;
      end;
      
      if aniTemp = nil then exit;
    end;
    
  end;
    
	procedure UpdateAnimationEditor(var AniMode: AnimationEditorValues; var SharedVals : EditorValues);
	begin    
    with AniMode do
    begin
			UpdateFromGUI(AniMode, sharedVals);
      HandleSourceCells(AniMode, sharedVals);
      
      DrawAnimationEditor(AniMode, sharedVals);	
      HandleAnimationStrips(AniMode, sharedVals);
      
      if KeyTyped(vk_0) then ExportAnimation(aniStrips,'testsave');
 		end;
	end;
end.