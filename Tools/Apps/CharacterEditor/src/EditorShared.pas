unit EditorShared;

//=============================================================================
interface
uses sgTypes, sgUserInterface;

type
  BMPType  = (BitmapGroup, SourceGroup, AnimationGroup, PreviewGroup); // The type of grids used for the various scales of the bitmaps draw
	
  LoadedBitmapPtr = ^LoadedBitmap;               // Pointer to a loaded bitmap for the type of bitmaps used in the editor 
  CellGroup = ^CellGroupData;                    // Pointer to a CellGroup used by the cells to their parents
  CellAreaPtr = ^CellArea;                       // Pointer to a cell Area. Mainly used for drag and drop

	LoadedBitmap = record
		original  : Bitmap;                          // The original bitmap - ensures all scales occur from this
    src       : Array [BMPType] of Bitmap;       // The scaled version of the bitmaps for the different groups
	end;
  
	CellArea = record                              // Individual cells in a cell Group
		area: Rectangle;                             // X Y width height. The width and height are mainly accessed from the cellGroup instead of the cell
		xGap, yGap, idx, cellIdx: Integer;           // Gaps are shown inbetween cells. Idx = index of cell. CellIdx = index of cell drawn in bitmap
		isSelected: Boolean;                         // Is this cell selected?
    bmpPtr : LoadedBitmapPtr;                    // Pointer to the loaded bitmap
    parent : CellGroup;                          // pointer to the cellgroup which the cell belongs to
	end; 
  
	CellGroupData = record                         //Cell Group which has a group of cells
		grpArea             : Rectangle;             // x y w h of cellgroup
		cells               : Array of CellArea;     // The Array of cell areas within the group
		rows, cols, cellCount, 
    cellW, cellH        : Integer;               // Rows, columns, cell count, cell width, cell height
		selectedOrder       : Array of LongInt;      // the order which the cells were selected (for shift click select functionality)
		GridType            : BMPType                // The type of grid used. This is used for the Loaded bitmap record for the type of scale to draw
	end;
	
  AniIDs = record                                // The animation ID
    ID: string;                                  // The String Value
    idx : Integer;                               // The index of the ID
  end;
  
  AniIDArray = Array of AniIDs;
  
  AnimationStrip = record                        // The Animation Strip has two cellgroups. One for the cells of the animation, and one for the 'goto' cell
    cellGrp             : CellGroupData;         // The Animation cells 
    LastCell            : CellGroupData;         // The GOTO cell
    stringIDs,soundIDs  : AniIDArray;            // The array of string IDs and soundIDs
    idx, scrollOffSet,                           // idx = the index of the animation strip. scrolloffset is used as a mutliplier to position the strip with scrolling
    lastCellParentID    : Integer;               // LastCellParentID is the id of the parent for the last cell for saving purposes
  end;
  
  PanelArray = Array of Panel;                   // PanelArray data type used for array of panels
		
	BitmapEditorValues = record                    // The main record used by the bitmap editor
		cellGrp         : CellGroupData;             // The only cellgroup in the editor
		scale           : Integer;                   // the scale used for the bitmap
		panels          : PanelArray;                // the editor's panel array
		destBMP         : Bitmap;                    // The saved Bitmap
		bg              : Bitmap;                    // THe background of the mode
	end;

	AnimationEditorValues = record
		cellGrp         : CellGroupData;            // The group of cells in the top source cells
		AniStrips       : Array of AnimationStrip;  // Array of AnimationStrips in the editor
		panels          : PanelArray;               // The panels of the Editor
		bg,                                         // bg = background of the editor
    filmStrip,                                  // THe bitmap used for the filmstrip background
    pointArrow      : Bitmap;                   // pointarrow = big arrow drawn pointing to the last cell       
		startArrow,                                 // Area of the first arrow in the bitmap
    finishArrow,                                // Area of the end arrow in the bitmap
    middle          : Rectangle;                // Area of the cell background
    selectedIdx,                                // What animationstrip is selected if any. -2 = none, -1 = multiple
    scrollOffSet,                               // Vertical animationstrip scrolling offset
    originalSize,                               // Use original size or fit to window. Stores value of radio group to check if changed
    radio1,
    radio2          : Integer;
    previewSprite   : Sprite
	end;
    
	LoadedBitmaps = array of LoadedBitmap;        // The array that the bitmaps are stored in with the original and different scales
	
	EditorValues = record                         // The Editor values used by all modes
		BMPArray        : LoadedBitmaps;            // The array of loaded bitmaps
	//	SoundArray      : Array of SoundEffect;     // THe array of sound effects
		mouseOffSet     : Point2D;	                // The mouse offset used when dragging
    dragCell        : CellAreaPtr;              // The cell area pointer of the cell currently on the mouse
    dragGroup       : Boolean;                  // When true allows the user to drag the entire cellgroup
	end;
  
	//---------------------------------------------------------------------------
  // Locate CellGroup and CellArea
  //---------------------------------------------------------------------------  
  
  // Returns the cell at the mouse position    
  function CellAreaAt(cellGrp: CellGroupData; const pt: Point2D; const gap: Integer): CellAreaPtr;
 
  //---------------------------------------------------------------------------
  // Initializing
  //--------------------------------------------------------------------------- 
 
  //Initializes the cell group with the data passed through the parameters
  function InitializeCellGroup(cellCount, cols, rows, cellw, cellh, x, y, gap: Integer; GridType : BMPType): CellGroupData;
  //Moves the cellgroup to the x y passed through parameters
  procedure MoveGroup(var cellGrp: CellGroupData; x, y: Integer);
  //Changes the width and height of the cellgroup by calculating the rows and columsn of the group
  procedure UpdateGroupSize(var cellGrp: CellGroupData; const gap: Integer);  
  //Initializes individual cells in the cellgroup
  procedure InitialCellAreaValues(var cell: CellArea; idx, cellIdx: Integer; bmpPtr: LoadedBitmapPtr; cellGrpPtr: CellGroup);
  //Updates the pointers of cells within the cellgroup
  procedure UpdateCellPointers(var cell: CellArea; bmpPtr: LoadedBitmapPtr; parent: CellGroup);
  //Initializes the cell area's x y width height and calls initialize cell area values
  procedure InitializeCellArea(var cellGrp : CellGroupData; bmpPtr: LoadedBitmapPtr; const cellGapSize: Integer);
  //Updates the position of all cells in the cellgroup to by offset by the x y of the cell group
  procedure UpdatePosition(var cellGrp: CellGroupData);

	//---------------------------------------------------------------------------
  // Bitmaps
  //--------------------------------------------------------------------------- 
  
  function SetLoadedBitmapPtr(idx : Integer; bmpArray: LoadedBitmaps): LoadedBitmapPtr;
  procedure AddBitmapToList(bmp : Bitmap; lst1, lst2: GUIList);
  procedure InitializeBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; scale: single; cols, rows: Integer);
  procedure SetBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; cols, rows: Integer);	
  procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string);

	//---------------------------------------------------------------------------
  // Dragging
  //--------------------------------------------------------------------------- 
	
  procedure PickupCell(var sharedVals: EditorValues);
  procedure SwapData(var dragCell, target: CellAreaPtr);
  procedure DropData(var dragCell, target: CellAreaPtr);
  procedure DragCellGroup(var cellGrp: CellGroupData; var sharedVals: EditorValues);
  procedure DragAndDrop(cellGrp: CellGroupData; var sharedVals: EditorValues; const gap: Integer);
  
	//---------------------------------------------------------------------------
  // Selecting
  //--------------------------------------------------------------------------- 	
  
	procedure AddSelection(var cellGrp : CellGroupData; i : LongInt);
	procedure RemoveSelection(var cellGrp : CellGroupData; i : LongInt);
	procedure DeselectAll(var cellGrp : CellGroupData);
	procedure SelectMultiple(var cellGrp : CellGroupData; i : LongInt);
	procedure HandleSelection(var cellGrp : CellGroupData; dCell: CellAreaPtr);
  procedure DeleteSelected(var cellGrp: CellGroupData; const gap : Integer);
 
	//---------------------------------------------------------------------------
  // Drawing
  //--------------------------------------------------------------------------- 
 
  procedure MyDrawSplitBitmapCells(cellGrp: CellGroupData; sharedVals: EditorValues);
  procedure MyDrawEmptyCells(cellGrp: CellGroupData; sharedVals: EditorValues);
  procedure DrawOnMouse(sharedVals: EditorValues);
	procedure DrawCellGroup(cellGrp : CellGroupData; i: LongInt);

  //---------------------------------------------------------------------------
  // Saving
  //--------------------------------------------------------------------------- 
 	procedure ExportBitmap(destbmp: Bitmap; cellGrp: CellGroupData; bmpArray : LoadedBitmaps); 
  procedure ExportAnimation(aniStrips: Array of AnimationStrip; path: string);
	
const
	CellGap 								= 5;
	ScreenWidth 						= 800;
	ScreenHeight						= 600;
	LengthCellData 					= 6;
	LengthgrpArea 					= 10;
	LengthMode							= 2;
	LengthAniEditing				= 6;

//Editors
	BitmapEditorID					= 0;
	AnimationEditorID				= 1;
	CharacterEditorID				= 2;

//Clip Area
	ClipX										= 195;
	ClipY										= 65;
	ClipW										= 570;
	ClipH										= 480;
		
//Colors
	Red											= 255;
	Green										= 255;
	Blue										= 255;
//Other
	edgeDistance						= 50;
	ArrowWidth							= 39;
	ArrowHeight							= 26;
	
implementation
	uses sgImages, sgCore, sgInput, sgText, sgGeometry, sgGraphics, SysUtils, sgResources, sgShared;

	//---------------------------------------------------------------------------
  // Locate CellGroup and CellArea
  //---------------------------------------------------------------------------   
  
  function CellAreaAt(cellGrp: CellGroupData; const pt: Point2D; const gap: Integer): CellAreaPtr;
  var
    pointCol, pointRow, index: Integer;
    pos: Point2D;
  begin
    result := nil;
    
    with cellGrp do
    begin         
      if not PointInRect(pt, grpArea) then exit;{(pt.x < grpArea.x) OR 
         (pt.y < grpArea.y) OR 
         (pt.x > grpArea.x + grpArea.width) OR 
         (pt.y > grpArea.y + grpArea.Height) then exit;
         }
         Write('pt.x ');WriteLn(pt.x);
         Write('pt.y ');WriteLn(pt.y);
         Write('grpArea.x ');WriteLn(grpArea.x);
         Write('grpArea.y ');WriteLn(grpArea.y);
         Write('grpArea.x + grpArea.width ');WriteLn(grpArea.x + grpArea.Width);
         Write('grpArea.y + grpArea.Height');WriteLn(grpArea.y + grpArea.Height);
         Write('grpArea.Height');WriteLn(grpArea.Height);
         Write('grpArea.width');WriteLn(grpArea.width);
      
      pos.x := pt.x - grpArea.x;
      pos.y := pt.y - grpArea.y;
            
      pointCol := Trunc(pos.x / (cellW + gap));
      pointRow := Trunc(pos.y / (cellH + gap));
      
      Write('pointCol');WriteLn( pointCol);
      Write('pointRow');WriteLn( pointRow);
      
      index := cols*pointRow + pointCol;
      
      result := @cells[index];
      
      Write('cell idx '); WriteLn(index);
    end;
  end;
  
  //---------------------------------------------------------------------------
  // Initializing
  //--------------------------------------------------------------------------- 
     
  procedure UpdateCellPointers(var cell: CellArea; bmpPtr: LoadedBitmapPtr; parent: CellGroup);
  begin
    cell.bmpPtr := bmpPtr;
    cell.parent := parent;
  end;
  
  procedure InitialCellAreaValues(var cell: CellArea; idx, cellIdx: Integer; bmpPtr: LoadedBitmapPtr; cellGrpPtr: CellGroup);
  begin
    cell.isSelected := false;
    cell.Idx        := idx;
    cell.cellIdx    := cellIdx;
    UpdateCellPointers(cell, bmpPtr, cellGrpPtr);
  end;
  
  procedure UpdateGroupSize(var cellGrp: CellGroupData; const gap: Integer);
  begin
    with cellGrp do
    begin
      cellGrp.grpArea.width  := cellW * cols + (gap * cols - gap);
      cellGrp.grpArea.height := cellH * rows + (gap * rows - gap);
    end;
  end;
  
  procedure MoveGroup(var cellGrp: CellGroupData; x, y: Integer);
  begin 
    cellGrp.grpArea.x    := x;
    cellGrp.grpArea.y    := y; 
  end;
  
  procedure UpdatePosition(var cellGrp: CellGroupData);
  var
		r, c, i: LongInt; //Rows and Column count in loop
	begin
		c := 0;
		r := 0;		
		with cellGrp do 
		begin
			for  i := Low(cells) to High(cells) do
			begin				
				cells[i].Area.X := cellW  * c + grpArea.X + cells[i].xGap;
				cells[i].Area.Y := cellH  * r + grpArea.Y + cells[i].yGap;
				
				c += 1;				
				if c = Cols then
				begin		
					c := 0;
					r += 1;
				end;
			end;
		end;
	end;
  
  procedure InitializeCellArea(var cellGrp : CellGroupData; bmpPtr: LoadedBitmapPtr; const cellGapSize: Integer);
	var
		xGap, yGap, oldLength, i, columns: LongInt;
	begin
		if (cellGrp.cellCount = 0) OR (cellGrp.cols = 0) then exit;
		xGap 			:= 0;
		yGap 			:= 0;	
		columns		:= 0;
		with cellGrp do
		begin		
			if Length(cells) < CellCount then oldLength := Length(cells)-1 else oldLength := cellCount;			
			SetLength(cells, cellCount);
			
			for  i := Low(cells) to High(cells) do
			begin							
				cells[i].XGap	:= xGap;
				cells[i].YGap	:= yGap;		
				
				if i > oldLength then
				begin
          InitialCellAreaValues(cells[i], i, i, bmpPtr, @cellGrp);
				end;
				
				xGap 		+= cellGapSize;		
				columns += 1;
				
				if columns = cols then
				begin		
					columns := 0;
					xGap 		:= 0;
					yGap 		+= cellGapSize;
				end;
			end;
		end;
		UpdatePosition(cellGrp);
	end;
  
  function InitializeCellGroup(cellCount, cols, rows, cellw, cellh, x, y, gap: Integer; GridType : BMPType): CellGroupData;
  begin
    result.CellCount    := cellCount;
    result.Cols         := cols;
    result.Rows         := rows;
    result.cellW        := cellw;
    result.cellH        := cellH;
    result.GridType     := GridType;
    MoveGroup(result, x, y);
  
    UpdateGroupSize(result, gap);
    SetLength(result.selectedOrder, 0);
  end;
  
	//---------------------------------------------------------------------------
  // Bitmaps
  //--------------------------------------------------------------------------- 
	
	procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string);
	begin
		SetLength(bmpArray, Length(bmpArray)+1);
		
		bmpArray[High(bmpArray)].original := MapBitmap(id, fileName);	
		SetTransparentColor(bmpArray[High(bmpArray)].original, RGBColor(Red,Green,Blue));
		
		bmpArray[High(bmpArray)].src[BitmapGroup]    := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].src[SourceGroup]    := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].src[AnimationGroup] := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].src[PreviewGroup] := bmpArray[High(bmpArray)].original;
	end;
  
  procedure AddBitmapToList(bmp : Bitmap; lst1, lst2: GUIList);
  begin
  	ListAddItem(lst1, bmp^.name);
		ListAddItem(lst2, bmp^.name);
  end;
  
  procedure SetBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; cols, rows: Integer);
  var
    cellW, cellH, i: Integer;
  begin    
    for i := Low(bmpArray) to High(bmpArray) do
    begin
      cellW := Trunc(bmpArray[i].src[GridType]^.width  / cols);
      cellH := Trunc(bmpArray[i].src[GridType]^.height / rows);
      SetBitmapCellDetails(bmpArray[i].src[GridType], cellW, cellH, cols, rows, cols*rows);
    end;
  end;
  
  procedure ScaleAllBitmaps(GridType: BMPType; var bmpArray: LoadedBitmaps; scale: single);
  var
    i: integer;
  begin
    for i := Low(bmpArray) to High(bmpArray) do
    begin
      bmpArray[i].src[GridType] := RotateScaleBitmap(bmpArray[i].original, 0, scale);	
    end;
  end;
  
  procedure InitializeBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; scale: single; cols, rows: Integer);
  begin	 
    ScaleAllBitmaps(GridType, bmpArray, scale);    
    SetBitmapDetails(GridType, bmpArray, cols, rows);
  end;
    
  function SetLoadedBitmapPtr(idx : Integer; bmpArray: LoadedBitmaps): LoadedBitmapPtr;
  begin
    if idx = -1 then result := nil   
    else result := @bmpArray[idx];
  end;

	//---------------------------------------------------------------------------
  // Selecting
  //--------------------------------------------------------------------------- 	
	
	procedure AddSelection(var cellGrp : CellGroupData; i : LongInt);
	begin
		SetLength(cellGrp.selectedOrder, Length(cellGrp.selectedOrder) + 1);
		cellGrp.selectedOrder[High(cellGrp.selectedOrder)] := i;
		cellGrp.cells[i].isSelected := true;
	end;

	procedure RemoveSelection(var cellGrp : CellGroupData; i : LongInt);
	var
		j: LongInt;
	begin
		for j := Low(cellGrp.selectedOrder) to High(cellGrp.selectedOrder) do
		begin
			cellGrp.cells[i].isSelected := false;
			
			if cellGrp.selectedOrder[j] = i then
			begin
				if High(cellGrp.selectedOrder) <> j then cellGrp.selectedOrder[j] := cellGrp.selectedOrder[j+1];
			end;
		end;
		
		if (Length(cellGrp.selectedOrder) >  0) then SetLength(cellGrp.selectedOrder, Length(cellGrp.selectedOrder)-1);
	end;
  
  procedure ShiftCellsDown(var cellGrp: CellGroupData; currentIdx : Integer; var lastNotSelected: Integer);
  var
    j, startIdx: Integer;
  begin
    with cellGrp do
    begin
      if currentIdx > lastNotSelected then startIdx := currentIdx + 1
      else startIdx := lastNotSelected +1;
      
      for j:= startidx to High(cells) do
      begin
        WriteLn('j', ' ', j);
        if not cells[j].isSelected then
        begin
          lastNotSelected := j;
          InitialCellAreaValues(cells[currentIdx], cells[currentIdx].idx, cells[j].cellIdx, cells[j].bmpPtr, cells[currentIdx].parent);
          cells[j].isSelected := true;
          WriteLn(cells[currentIdx].isSelected);
          break;
        end;
      end;  
    end;
  end;
    
  procedure DeleteSelected(var cellGrp: CellGroupData; const gap : Integer);
  var
    i, lastNotSelected : Integer;
  begin   
    with cellGrp do
    begin
      if Length(selectedOrder) = 0 then exit;
      if Length(cells) <> Length(selectedOrder) then
      begin
        lastNotSelected := 0;
        for i:= Low(cells) to High(cells) do
        begin
         // if i <= lastNotSelected then continue; 
          if cells[i].isSelected  then
          begin
            ShiftCellsDown(cellGrp, i, lastNotSelected);
          end;        
          if lastNotSelected = High(cells) then break;
        end;
      end;
      for i:= High(cells) downto High(cells) - High(selectedOrder) do
      begin
        UpdateCellPointers(cells[i], nil, nil);
      end;
      cellCount := Length(cells) - Length(selectedOrder);
      SetLength(cells, cellCount);
      SetLength(selectedOrder, 0);
      UpdateGroupSize(cellGrp, gap);
    end;
  end;
  
	procedure DeselectAll(var cellGrp : CellGroupData);
	var 
		i: LongInt;
	begin
		SetLength(cellGrp.selectedOrder, 0);
		
		for i := Low(cellGrp.cells) to High(cellGrp.cells) do cellGrp.cells[i].isSelected := false;
	end;

	procedure SelectMultiple(var cellGrp : CellGroupData; i : LongInt);
	var
		previousSelect, j, count, startVal : LongInt;
	begin
		if Length(cellGrp.selectedOrder) = 0 then exit;
		previousSelect := cellGrp.selectedOrder[High(cellGrp.selectedOrder)];
		
		DeselectAll(cellGrp);
			
		if i > previousSelect then
		begin
			count 		:= i - previousSelect;
			startVal	:= previousSelect;
			SetLength(cellGrp.selectedOrder, i - previousSelect +1);
		end else begin
			count		:= previousSelect - i;
			startVal 	:= i;
			SetLength(cellGrp.selectedOrder, previousSelect - i +1);
		end;
					
		for j := 0 to count do
		begin
			cellGrp.selectedOrder[j] 						 	:= startVal;
			cellGrp.cells[startval].isSelected := true;
			startVal														 	+= 1;
		end;
	end;
  
  procedure HandleSelection(var cellGrp : CellGroupData; dCell: CellAreaPtr);
	var
		i: LongInt;
	begin
		if (dCell <> nil) OR (not MouseClicked(LeftButton)) then exit;
		with cellGrp do
		begin
			for i := Low(cells) to High(cells) do
			begin
				if PointInRect(MousePosition(), cells[i].Area.X, cells[i].Area.Y, cellW, cellH) then
				begin
					if cells[i].isSelected then RemoveSelection(cellGrp, i)
          else if KeyDown(VK_LShift) then SelectMultiple(cellGrp, i)
					else begin
            DeselectAll(cellGrp);
            AddSelection(cellGrp, i);			
          end;
				end;
			end;
		end;
	end;
  
	//---------------------------------------------------------------------------
  // Dragging
  //--------------------------------------------------------------------------- 

 procedure PickupCell(var sharedVals: EditorValues);
 begin;      
    with sharedVals do
    begin
      if dragCell = nil then exit;      
      if dragCell^.bmpPtr = nil then dragCell := nil;
      if dragCell = nil then exit;
      sharedVals.mouseOffSet.x 	:= MousePosition().x - dragCell^.Area.X;
      sharedVals.mouseOffSet.y 	:= MousePosition().y - dragCell^.Area.Y;
    end;
  end; 
  
  procedure SwapData(var dragCell, target: CellAreaPtr);
  var
    temp: CellArea;
  begin
    if (dragCell = nil) OR (target = nil) then exit;
    temp := target^;
    target^.cellIdx := dragCell^.cellIdx;
    target^.bmpPtr := dragCell^.bmpPtr;
    dragCell^.cellIdx := temp.cellIdx;
    dragCell^.bmpPtr := temp.bmpPtr;
    dragCell := nil;
    target   := nil;
  end;
  
  procedure DropData(var dragCell, target: CellAreaPtr);
  begin
    if (dragCell = nil) OR (target = nil) then exit;
    InitialCellAreaValues(target^, target^.idx, dragCell^.cellIdx, dragCell^.bmpPtr, target^.parent);
    dragCell := nil;
    target   := nil;
  end;
  
  procedure DragAndDrop(cellGrp: CellGroupData; var sharedVals: EditorValues; const gap: Integer);
  var
    target : CellAreaPtr;
  begin
    if sharedVals.dragCell = nil then
    begin
      sharedVals.dragCell := CellAreaAt(cellGrp, MousePosition, gap);
      PickUpCell(sharedVals);
    end else begin
      target := CellAreaAt(cellGrp, MousePosition, gap);
      SwapData(sharedVals.dragCell, target);
    end;
  end;
  
  procedure DragCellGroup(var cellGrp: CellGroupData; var sharedVals: EditorValues);
	begin
		with cellGrp do
		begin
			if (cols = 0) OR (cellCount = 0) then exit;
			if PointInRect(MousePosition, grpArea) and MouseDown(LeftButton) AND not sharedVals.dragGroup then
			begin
				sharedVals.dragGroup := true;
				sharedVals.mouseOffset.x := Trunc(MousePosition().x) - grpArea.X;
				sharedVals.mouseOffset.y := Trunc(MousePosition().y) - grpArea.Y;
			end;
			if sharedVals.dragGroup and MouseDown(LeftButton) then
			begin		
				grpArea.X := Trunc(MousePosition().x -  sharedVals.mouseOffset.x);
				grpArea.Y := Trunc(MousePosition().y -	sharedVals.mouseOffset.y);
				UpdatePosition(cellGrp);
			end;
			
			if not MouseDown(LeftButton) and sharedVals.dragGroup then sharedVals.dragGroup := false; 
		end;
	end;
  
	//---------------------------------------------------------------------------
  // Saving
  //--------------------------------------------------------------------------- 

	procedure SaveBitmapGrid(destbmp: bitmap; cellGrp : CellGroupData; idx, xPos, yPos : LongInt);
	begin
		with cellGrp do
		begin
			DrawRectangle(destbmp, ColorWhite, xPos, yPos, cellW -1, cellH -1);
			FillRectangle(destbmp, ColorBlack, xPos, yPos, cellW -1, cellH -1);
			DrawText(destbmp, IntToStr(idx), ColorYellow, xPos, yPos);		
		end;
	end;

	procedure ExportBitmap(destbmp: Bitmap; cellGrp: CellGroupData; bmpArray : LoadedBitmaps);
	var
		xPos, yPos, i: Integer;
	begin
		with cellGrp do
		begin	
      destbmp := CreateBitmap(cellGrp.grpArea.width - (cellGap*(cols-1)), cellGrp.grpArea.height - (cellGap*(rows-1)));
      for i := Low(bmpArray) to High(bmpArray) do MakeOpaque(bmpArray[i].src[GridType]);		
			for i := Low(cells) to High(cells) do
			begin
        xPos := Trunc(cells[i].Area.X - cells[i].xGap - grpArea.X);
        yPos := Trunc(cells[i].Area.Y - cells[i].yGap - grpArea.Y);
				if cells[i].bmpPtr = nil then
          SaveBitmapGrid(destBMP, cellGrp, i, xPos, yPos)
        else
          DrawCell(destbmp, cells[i].bmpPtr^.src[cells[i].parent^.GridType], cells[i].cellIdx, xPos, yPos);
			end;
      for i := Low(bmpArray) to High(bmpArray) do MakeTransparent(bmpArray[i].src[GridType]);
      SaveToPNG(destbmp, dialogpath);
      FreeBitmap(destBmp);
		end;
	end; 
  
  procedure LoadAnimation();
  type
    AniData = record
      minID, maxID, speed, next : Integer;
      range : Array of Integer;
    end;
    AniDataArray = Array of AniDAta;
  begin
  end;
  
  procedure ExportAnimation(aniStrips: Array of AnimationStrip; path: string);
  type
    last = record
      cellIdx, aniIdx, parentIdx : Integer;
    end;
    FrameSet = record
      id : Integer;
      aniType : Char;
      values : string;
      lastCellDetails: last;
    end;   
  var
    i, Id : LongInt;
    txt: Text;
    arrayofSounds, arrayofIDs : StringArray;
    FramesArray : Array of FrameSet;
    
    function _lastCellCheck(aniStrip: AnimationStrip): last;
    begin
      with aniStrip do
      begin
        if (lastCell.cells[0].bmpPtr <> nil) then
        begin
          result.cellIdx := lastCell.cells[0].idx;
          result.parentIdx := idx;
          result.aniIdx := lastCellParentID;
        end else
          result.cellIdx := -1;
      end;
    end;
    
    function _OutputSingleFrameAnimation(aniStrip: AnimationStrip): FrameSet;
    begin
      with aniStrip do
      begin
        result.id :=  id;
        result.aniType := 'f';
        result.values := IntToStr(id) + ',' + IntToStr(cellGrp.cells[0].cellidx) + ',5,';
        result.lastCellDetails := _lastCellCheck(aniStrip);
        id += 1;
      end;
    end;

    function _OutputMultiFrameAnimation(aniStrip: AnimationStrip): FrameSet;
    var
      i, lastVal : LongInt;
    begin
      with aniStrip do
      begin
        result.id := id;
        result.aniType := 'm';
        result.values := '[' + IntToStr(Low(cellGrp.cells) + id) + '-' + IntToStr(High(cellGrp.cells) + id - 1) + '],[' + IntToStr(cellGrp.cells[Low(cellGrp.cells)].cellIdx);
        lastVal := cellGrp.cells[Low(cellGrp.cells)].cellIdx;
        for i := Low(cellGrp.cells) + 1 to High(cellGrp.cells) - 1 do
        begin
          if (cellGrp.cells[i - 1].cellIdx  <> cellGrp.cells[i].cellIdx -1) then
          begin
            if lastVal <> cellGrp.cells[i - 1].cellidx then 
              result.values += '-' + IntToStr(cellGrp.cells[i - 1].cellidx) + ',' + IntToStr(cellGrp.cells[i].cellidx)
          else
            result.values += ',' + IntToStr(cellGrp.cells[i].cellidx);
            lastVal := cellGrp.cells[i].cellidx;
          end;
        end;
        if lastVal <> cellGrp.cells[High(cellGrp.cells) - 1].cellIdx then 
        begin
          result.values += '-' + IntToStr(cellGrp.cells[High(cellGrp.cells)-1].cellidx);
        end;
        result.values += ']' + ',' + '5' + ',';
        result.lastCellDetails := _lastCellCheck(aniStrip);
        id := id + High(cellGrp.cells);
      end;
    end;
    
    procedure _ProcessAnimationStrip(aniStrip: AnimationStrip);
    var
      i, oldID: Integer;
    begin
      with aniStrip do
      begin
        oldID := id;
        SetLength(FramesArray, Length(FramesArray) + 1);
        
        if Length(aniStrip.cellGrp.cells) = 2 then
        begin
          FramesArray[High(FramesArray)] := _OutputSingleFrameAnimation(aniStrip);
        end else if Length(aniStrip.cellGrp.cells) > 2 then
        begin
          FramesArray[High(FramesArray)] := _OutputMultiFrameAnimation(aniStrip);
        end;
        if Length(aniStrip.stringIDs) <> 0 then
        begin
          for i:= Low(aniStrip.stringIDs) to High(aniStrip.stringIDs) do
          begin
            SetLength(arrayofIDs, Length(arrayofIDs) + 1);
            arrayofIDs[High(arrayofIDs)] := 'i:' + aniStrip.stringIDs[i].id + ',' + IntToStr(aniStrip.stringIDs[i].idx + oldID);       
          end;
        end;
        if Length(aniStrip.soundIDs) <> 0 then
        begin
          for i:= Low(aniStrip.soundIDs) to High(aniStrip.soundIDs) do
          begin
            SetLength(arrayofSounds, Length(arrayofSounds) + 1);
            WriteLn(aniStrip.soundIDs[i].id);
            arrayofsounds[High(arrayofsounds)] := 's:' + aniStrip.soundIDs[i].id + ',' + IntToStr(aniStrip.soundIDs[i].idx + oldID);       
          end;
        end;
      end;
    end; 
  begin
    id := 0;
    Assign(txt, PathToResource('Animations\' + path + '.txt'));
    ReWrite(txt);
    WriteLn(txt,'SwinGame Animation #v1');
    WriteLn(txt,'');
    WriteLn(txt,'//Frames are declared with an f: and contain');
    WriteLn(txt,'//the following comma separated values');
    WriteLn(txt,'//ID,CELL,DUR,NEXT');
    
    for i := Low(aniStrips) to High(aniStrips) do
    begin
      _ProcessAnimationStrip(aniStrips[i]);
    end;
    
    for i := Low(FramesArray) to High(FramesArray) do
    begin
      with FramesArray[i] do
      begin
        if (lastCellDetails.cellIdx > -1) AND (lastCellDetails.aniIdx < Length(aniStrips)) then
        begin
          lastCellDetails.cellIdx := FramesArray[lastCellDetails.aniIdx].id + lastCellDetails.cellIdx;
          values += IntToStr(lastCellDetails.cellIdx);
        end;
        if aniType = 'f' then WriteLn(txt, aniType + ':' + values);
      end;
    end;
    
    WriteLn(txt,'');
    WriteLn(txt, '//Multi-frame: ranges are in[]');
    WriteLn(txt, '//[a-b] = numbers from a to b inclusive');
    WriteLn(txt, '//[a,b,c] = explicit values');
    WriteLn(txt, '//[a-b,c] = combination');
    
    for i := Low(FramesArray) to High(FramesArray) do
    begin
      if FramesArray[i].aniType = 'm' then WriteLn(txt, FramesArray[i].aniType + ':' + FramesArray[i].values);
    end;
    
    WriteLn(txt, '//Sounds');
    
    for i := Low(arrayofSounds) to High(arrayofSounds) do 
    begin
      WriteLn(txt, arrayofSounds[i]);
    end;
    
    WriteLn(txt, '//Identifiers');
    
    for i := Low(arrayofIDs) to High(arrayofIDs) do 
    begin
      WriteLn(txt, arrayofIDs[i]);
    end;
    
    Close(txt);
  end;
      
	//---------------------------------------------------------------------------
  // Drawing
  //--------------------------------------------------------------------------- 

	procedure DrawCellGroup(cellGrp : CellGroupData; i: LongInt);
	begin
		with cellGrp do
		begin
			if cells[i].isSelected then
				FillRectangleOnScreen(RGBAColor(255,0,0,150), Trunc(cells[i].area.X), Trunc(cells[i].area.Y), cellW, cellH)
			else
				DrawRectangleOnScreen(ColorWhite, Trunc(cells[i].area.X), Trunc(cells[i].area.Y), cellW, cellH)
		end;
	end; 
  
  procedure DrawOnMouse(sharedVals: EditorValues);
	var
		dragPos: Point2D;
	begin
    with sharedVals do
    begin
      if dragCell^.bmpPtr = nil then exit;
      dragPos.x := MousePosition().x - mouseOffSet.x;
      dragPos.y := MousePosition().y - mouseOffSet.y;
      DrawCell(dragCell^.BMPPtr^.src[dragCell^.parent^.GridType], dragCell^.cellIdx, dragPos);
    end;
	end;
  
  procedure DrawCellArea(cell: CellArea);
  begin
    DrawCell(cell.bmpPTR^.src[cell.parent^.GridType], cell.cellIdx, Trunc(cell.area.X), Trunc(cell.area.Y));
  end;
  
  procedure MyDrawSplitBitmapCells(cellGrp: CellGroupData; sharedVals: EditorValues);
  var
    i: Integer;
  begin
    with cellGrp do
    begin
      for i := Low(cells) to High(cells) do
      begin
        if (cells[i].bmpPtr  <> nil) AND (sharedVals.dragCell <> @cells[i]) then
        begin
          DrawCellArea(cells[i]);
        end;       
        DrawCellGroup(cellGrp, i);
      end;
    end;
  end;
  
  procedure MyDrawEmptyCells(cellGrp: CellGroupData; sharedVals: EditorValues);
	 var
    i: Integer;
  begin
    with cellGrp do
    begin
      for i := Low(cells) to High(cells) do
      begin
        if cells[i].bmpPtr = nil then
        begin
          FillRectangleOnscreen(ColorWhite, Trunc(cells[i].area.X), Trunc(cells[i].area.Y), cellW, cellH);
          DrawText(IntToStr(i), ColorBlack, Trunc(cells[i].area.X), Trunc(cells[i].area.Y));
        end;
      end;
    end;
  end;
end.