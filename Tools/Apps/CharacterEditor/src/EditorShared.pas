unit EditorShared;

//=============================================================================
interface
uses sgTypes, sgUserInterface, sgCharacters;

type
  BMPType  = (BitmapGroup, SourceGroup, AnimationGroup, PreviewGroup); // The type of grids used for the various scales of the bitmaps draw
  DialogType = (None, SaveBMP, SaveAni, SaveChar, LoadBMP, LoadAni, LoadChar, LoadAniEdit);
	
  LoadedBitmapPtr = ^LoadedBitmap;               // Pointer to a loaded bitmap for the type of bitmaps used in the editor 
  CellGroup = ^CellGroupData;                    // Pointer to a CellGroup used by the cells to their parents
  CellAreaPtr = ^CellArea;                       // Pointer to a cell Area. Mainly used for drag and drop
    
	CellArea = record                              // Individual cells in a cell Group
		area: Rectangle;                             // X Y width height. The width and height are mainly accessed from the cellGroup instead of the cell
		xGap, yGap, idx, cellIdx: Integer;           // Gaps are shown inbetween cells. Idx = index of cell. CellIdx = index of cell drawn in bitmap
		isSelected: Boolean;                         // Is this cell selected?
    bmpPtr : LoadedBitmapPtr;                    // Pointer to the loaded bitmap
    parent : CellGroup;                          // pointer to the cellgroup which the cell belongs to
    identifier, sound : string;
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
    
  LoadedBitmap = record 
    original : Bitmap;                           // The original bitmap - ensures all scales occur from this
    Scaled : Array [BMPType] of Bitmap;          // The scaled version of the bitmaps for the different groups
  end;
  
  LoadedBitmaps = Array of LoadedBitmap;
  
  BitmapCollection = record
    ids: NamedIndexCollection;
    bmps : LoadedBitmaps;
  end;

  Parts = record
    ids: NamedIndexCollection;
    parts: Array of BitmapCollection;
  end;

  CharBodyTypes = record
    ids: NamedIndexCollection;
    bodyType: Array of Parts;
  end;
   
  ItemCache = record
    listID, body, part, bmp : Integer;
    bmpPtr : LoadedBitmapPtr;
  end;
  ItemCacheArray = Array of Array of Array of ItemCache;
  
  CharEditorValues = record
    panels : PanelArray;
    bg: Bitmap;
  //  BrowserData : BodyTypes;
    MainChar : Character;
    BaseLayer : Array of ItemCache;
    Cache : ItemCacheArray;
  end;

  AniStripArray = Array of AnimationStrip;
  
	AnimationEditorValues = record
		cellGrp         : CellGroupData;            // The group of cells in the top source cells
		AniStrips       : AniStripArray;            // AniStripArray in the editor
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
    previewSprite   : Sprite;
    aniTemp         : AnimationTemplate;
	end;
    
	//LoadedBitmaps = LoadedBitmaps;        // The array that the bitmaps are stored in with the original and different scales
	
	EditorValues = record                         // The Editor values used by all modes
		Browser        : CharBodyTypes;            // The array of loaded bitmaps
		mouseOffSet     : Point2D;	                // The mouse offset used when dragging
    dragCell        : CellAreaPtr;              // The cell area pointer of the cell currently on the mouse
    dragGroup       : Boolean;                  // When true allows the user to drag the entire cellgroup
    OpenSave        : DialogType;
    BitmapPtr       : LoadedBitmapPtr;
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
  procedure InitialCellAreaValues(var cell: CellArea; idx, cellIdx: Integer; bmpPtr: LoadedBitmapPtr; cellGrpPtr: CellGroup; id, s: string);
  //Updates the pointers of cells within the cellgroup
  procedure UpdateCellPointers(var cell: CellArea; bmpPtr: LoadedBitmapPtr; parent: CellGroup);
  //Initializes the cell area's x y width height and calls initialize cell area values
  procedure InitializeCellArea(var cellGrp : CellGroupData; bmpPtr: LoadedBitmapPtr; const cellGapSize: Integer; drawindex : boolean);
  //Updates the position of all cells in the cellgroup to by offset by the x y of the cell group
  procedure UpdatePosition(var cellGrp: CellGroupData);
  // Creates an Animation Strip
  procedure CreateAnimationStrip(var aniStrips: AniStripArray; cellCount : Integer);
  procedure UpdateBitmapPointers(var sourceGrp: CellGroupData; var aniStrips: AniStripArray);
  procedure PositionAniHorizontalScrollButtons(AniMode: AnimationEditorValues);

	//---------------------------------------------------------------------------
  // Bitmaps
  //--------------------------------------------------------------------------- 
  
  function SetLoadedBitmapPtr(idx : Integer; bmpArray: LoadedBitmaps): LoadedBitmapPtr;
  procedure AddBitmapToList(bmp : Bitmap; lst1, lst2: GUIList);
  procedure InitializeBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; scale: single; cols, rows: Integer);
  procedure SetBitmapDetails(GridType: BMPType; var bmpArray: LoadedBitmaps; cols, rows: Integer);	
  procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string);
  procedure LoadBitmapsFromTextFile(var charBmps: CharBodyTypes; fileName : string; lst1, lst2: GUIList);

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
  procedure ExportAnimation(aniStrips: AniStripArray);
  procedure LoadAnimation(var AniMode: AnimationEditorValues);
  procedure DoOpenDialog(var sharedVals : EditorValues; dt : DialogType);
  procedure DoSaveDialog(var sharedVals : EditorValues; dt : DialogType);
	
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
  
//Animation Editor
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
	AniNames 			    = 7;
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
  
  MainCharasFolders = 'chara_chubby,Chara-dwarf,Chara-Isometric,Chara-little,Chara-medium,Chara-monsters,Chara-monsters_f,Chara-Overweight Torso,Chara-soldiers,Chara-tall,Custom';
	
implementation
	uses sgImages, sgCore, sgInput, sgText, sgGeometry, sgGraphics, SysUtils, sgResources, sgShared, StrUtils, sgUtils, sgNamedIndexCollection;

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
      if not PointInRect(pt, grpArea) then exit;
      
      pos.x := pt.x - grpArea.x;
      pos.y := pt.y - grpArea.y;
            
      pointCol := Trunc(pos.x / (cellW + gap));
      pointRow := Trunc(pos.y / (cellH + gap));
      
      index := cols*pointRow + pointCol;
      
      result := @cells[index];
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
  
  procedure InitialCellAreaValues(var cell: CellArea; idx, cellIdx: Integer; bmpPtr: LoadedBitmapPtr; cellGrpPtr: CellGroup; id, s: string);
  begin
    cell.isSelected := false;
    cell.Idx        := idx;
    cell.cellIdx    := cellIdx;
    cell.identifier  := id;
    cell.sound      := s;
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
  
  procedure InitializeCellArea(var cellGrp : CellGroupData; bmpPtr: LoadedBitmapPtr; const cellGapSize: Integer; drawindex : boolean);
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
          if drawIndex then
            InitialCellAreaValues(cells[i], i, i, bmpPtr, @cellGrp, '', '')
          else
            InitialCellAreaValues(cells[i], i, -1, bmpPtr, @cellGrp, '', '');
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
    
  procedure CreateAnimationStrip(var aniStrips: AniStripArray; cellCount : Integer);
  var
    i: Integer;
	begin
    SetLength(aniStrips, Length(aniStrips) + 1);
    
    aniStrips[High(aniStrips)].idx 	:= 	High(aniStrips); 
    
    aniStrips[High(aniStrips)].cellGrp  := InitializeCellGroup(cellCount, cellCount, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * High(aniStrips)), CellGapLarge, AnimationGroup);
    aniStrips[High(aniStrips)].LastCell := InitializeCellGroup(1, 1, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * High(aniStrips)), CellGapLarge, AnimationGroup);
    
    InitializeCellArea(aniStrips[High(aniStrips)].cellGrp, nil, CellGapLarge, false);
    InitializeCellArea(aniStrips[High(aniStrips)].LastCell, nil, CellGapLarge, false);
    
    aniStrips[High(aniStrips)].scrollOffSet := 0;
    aniStrips[High(aniStrips)].lastCellParentID := -1;
    
    for i := Low(aniStrips) to High(aniStrips) do DeselectAll(aniStrips[i].cellGrp);
    
    with AniStrips[High(aniStrips)] do
    begin
      MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(LastCell.grpArea.y));
      UpdatePosition(LastCell);
    end;   
	end;
  
  procedure UpdateBitmapPointers(var sourceGrp: CellGroupData; var aniStrips: AniStripArray);
  var
    i, j: Integer;
  begin
    if (Length(sourceGrp.cells) = 0) then exit;
    
    for i := Low(aniStrips) to High(aniStrips) do
    begin
      for j := Low(aniStrips[i].cellGrp.cells) to High(aniStrips[i].cellGrp.cells) do
      begin
        if aniStrips[i].cellGrp.cells[j].cellIdx <> -1 then
          aniStrips[i].cellGrp.cells[j].bmpPtr := sourceGrp.cells[0].bmpPtr;
      end;
      if aniStrips[i].lastCell.cells[0].cellIdx <> -1 then
        aniStrips[i].lastCell.cells[0].bmpPtr := sourceGrp.cells[0].bmpPtr;
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
    
	//---------------------------------------------------------------------------
  // Bitmaps
  //--------------------------------------------------------------------------- 
	function CalculateScale(maxWidth, maxHeight, totalWidth, totalHeight: Integer): Single;
  begin
    if totalWidth > totalHeight then
      result := maxWidth / totalWidth
    else
      result := maxHeight / totalHeight;  
  end;
  
  procedure LoadBitmapsFromTextFile(var charBmps: CharBodyTypes; fileName : string; lst1, lst2: GUIList);
  var
    line : string;
    txt : text;
    lineNo, j : Integer;
    
    procedure AddToCollection(out bodyID, partID: Integer);
    var
      body, part, name : string;
    begin
      body := ExtractDelimited(3, line, ['/']);
      part := ExtractDelimited(4, line, ['/']);
      name := ExtractFileName(ExtractDelimited(1, line, [',']));
      
      if IndexOf(charBmps.ids, body) = -1 then
      begin
        body := 'Custom';
        Part := 'Custom';   
      end;
      
      bodyID := IndexOf(charBmps.ids, body);
      
      if IndexOf(charBmps.BodyType[bodyID].ids, part) = -1 then
      begin
        AddName(charBmps.BodyType[bodyID].ids, part);
        SetLength(charBmps.BodyType[bodyID].parts, Length(charBmps.BodyType[bodyID].parts) + 1);
        InitNamedIndexCollection(charBmps.BodyType[bodyID].parts[IndexOf(charBmps.BodyType[bodyID].ids, part)].ids);
      end;     
      
      partID := IndexOf(charBmps.BodyType[bodyID].ids, part);    
      
      if IndexOf(charBmps.BodyType[bodyID].parts[partID].ids, part) = -1 then
      begin
        with charBmps.BodyType[bodyID].parts[partID] do
        begin
          AddName(ids, name);
          SetLength(bmps, Length(bmps) + 1);
          bmps[High(bmps)].original  := MapBitmap(ExtractFileName(ExtractDelimited(1, line, [','])), ExtractDelimited(1, line, [',']));	
        end;
      end;
    end;
    
    procedure ProcessLine();
    var
      scale: Array [BMPType] of Single;
      width, height, cols, rows, count, totalW, totalH, bodyID, partsID: Integer;
      i : BMPType;
    begin      
      AddToCollection(bodyID, partsID);
      
      with charBmps.BodyType[bodyID].parts[partsID] do
      begin
        SetTransparentColor(bmps[High(bmps)].original, RGBColor(StrToInt(ExtractDelimited(2, line, [','])),
                                                                StrToInt(ExtractDelimited(3, line, [','])),
                                                                StrToInt(ExtractDelimited(4, line, [','])))); 
               
        width   := StrToInt(ExtractDelimited(7, line, [',']));
        height  := StrToInt(ExtractDelimited(8, line, [',']));
        cols    := StrToInt(ExtractDelimited(5, line, [',']));
        rows    := StrToInt(ExtractDelimited(6, line, [',']));
        count   := StrToInt(ExtractDelimited(9, line, [',']));
        totalW  := bmps[High(bmps)].original^.Width;
        totalH  := bmps[High(bmps)].original^.Height;
              
        scale[BitmapGroup]    := 1;
        scale[SourceGroup]    := CalculateScale(BMPWindowWidth, BMPWindowHeight, totalW + (cols-1)*CellGapSmall, totalH + (rows-1)*CellGapSmall);                                
        scale[AnimationGroup] := CalculateScale(AniCellWidth, AniCellHeight, width, height); 
        scale[PreviewGroup]   := CalculateScale(AniCellWidth*2, AniCellHeight*2, width, height);
        
        for i := Low(BMPType) to High(BMPType) do
        begin
          if scale[i] = 1 then
            bmps[High(bmps)].Scaled[i] := bmps[High(bmps)].original
          else
            bmps[High(bmps)].Scaled[i] := RotateScaleBitmap(bmps[High(bmps)].original, 0, scale[i]);	
          BitmapSetCellDetails(bmps[High(bmps)].Scaled[i],  Trunc(width*scale[i]), Trunc(height*scale[i]), cols, rows, count);
        end;
      end;
      ListAddItem(lst1, ExtractFileName(ExtractDelimited(1, line, [','])));
      ListAddItem(lst2, ExtractFileName(ExtractDelimited(1, line, [','])));
    end;
  begin
    Assign(txt, fileName);
    Reset(txt);
    lineNo :=0;
    line := '';
    
    InitNamedIndexCollection(charBmps.ids);
    AddNamesToCollection(charBmps.ids,MainCharasFolders);
    SetLength(charBmps.BodyType, NameCount(charBmps.ids));
    
    for j := Low(charBmps.BodyType) to High(charBmps.BodyType) do
    begin
      ListAddItem(RegionWithID('BodyList'), NameAt(charBmps.ids, j));
      InitNamedIndexCollection(charBmps.BodyType[j].ids);
    end;
    
   try
    while not EOF(txt) do
    begin 
      lineNo := lineNo + 1;
      ReadLn(txt, line);
      line := Trim(line);
      if Length(line) = 0 then continue;  //skip empty lines
      if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
      ProcessLine();
    end;   
    finally
      Close(txt);
    end;    
    
  end;
  
	procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string);
	begin
		SetLength(bmpArray, Length(bmpArray)+1);
		
		bmpArray[High(bmpArray)].original := MapBitmap(id, fileName);	
		SetTransparentColor(bmpArray[High(bmpArray)].original, RGBColor(Red,Green,Blue));
		
		bmpArray[High(bmpArray)].scaled[BitmapGroup]    := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].scaled[SourceGroup]    := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].scaled[AnimationGroup] := bmpArray[High(bmpArray)].original;
		bmpArray[High(bmpArray)].scaled[PreviewGroup]   := bmpArray[High(bmpArray)].original;
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
      cellW := Trunc(bmpArray[i].scaled[GridType]^.width  / cols);
      cellH := Trunc(bmpArray[i].scaled[GridType]^.height / rows);
      BitmapSetCellDetails(bmpArray[i].scaled[GridType], cellW, cellH, cols, rows, cols*rows);
    end;
  end;
  
  procedure ScaleAllBitmaps(GridType: BMPType; var bmpArray: LoadedBitmaps; scale: single);
  var
    i: integer;
  begin
    for i := Low(bmpArray) to High(bmpArray) do
    begin
      bmpArray[i].scaled[GridType] := RotateScaleBitmap(bmpArray[i].original, 0, scale);	
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
          InitialCellAreaValues(cells[currentIdx], cells[currentIdx].idx, cells[j].cellIdx, cells[j].bmpPtr, cells[currentIdx].parent, cells[j].identifier, cells[j].sound);
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
    InitialCellAreaValues(target^, target^.idx, dragCell^.cellIdx, dragCell^.bmpPtr, target^.parent, dragCell^.identifier, dragCell^.sound);
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

  procedure DoOpenDialog(var sharedVals : EditorValues; dt : DialogType);
  begin
    ShowOpenDialog();
    sharedVals.OpenSave := dt;
  end;
  
  procedure DoSaveDialog(var sharedVals : EditorValues; dt : DialogType);
  begin
    ShowSaveDialog();
    sharedVals.OpenSave := dt;
  end;
  
	procedure SaveBitmapGrid(destbmp: bitmap; cellGrp : CellGroupData; idx, xPos, yPos : LongInt);
	begin
		with cellGrp do
		begin
			DrawRectangle(destbmp, ColorBlack, xPos, yPos, cellW -1, cellH -1);
	//		FillRectangle(destbmp, ColorBlack, xPos, yPos, cellW -1, cellH -1);
			DrawText(destbmp, IntToStr(idx), ColorRed, xPos, yPos);		
		end;
	end;

	procedure ExportBitmap(destbmp: Bitmap; cellGrp: CellGroupData; bmpArray : LoadedBitmaps);
	var
		xPos, yPos, i: Integer;
	begin
		with cellGrp do
		begin	
      destbmp := CreateBitmap(cellGrp.grpArea.width - (cellGap*(cols-1)), cellGrp.grpArea.height - (cellGap*(rows-1)));
      for i := Low(bmpArray) to High(bmpArray) do MakeOpaque(bmpArray[i].scaled[GridType]);		
			for i := Low(cells) to High(cells) do
			begin
        xPos := Trunc(cells[i].Area.X - cells[i].xGap - grpArea.X);
        yPos := Trunc(cells[i].Area.Y - cells[i].yGap - grpArea.Y);
				if cells[i].bmpPtr = nil then
          SaveBitmapGrid(destBMP, cellGrp, i, xPos, yPos)
        else
          DrawCell(destbmp, cells[i].bmpPtr^.scaled[cells[i].parent^.GridType], cells[i].cellIdx, xPos, yPos);
			end;
      for i := Low(bmpArray) to High(bmpArray) do MakeTransparent(bmpArray[i].scaled[GridType]);
      SaveToPNG(destbmp, dialogpath);
      FreeBitmap(destBmp);
		end;
	end; 
  
  procedure LoadAnimation(var AniMode: AnimationEditorValues);
  type
    LoadedData = record
      speed, next : Integer;
      range, idArray : Array of Integer;
    end;
    identifiers = record
      value       : string;
      targetCell  : Integer;
    end;
    
    AniDataArray  = Array of LoadedData;
    AniIDArray    = Array of Identifiers;
    
    var
      aniData : AniDataArray;
      aniString : AniIDArray;
      aniSounds : AniIDArray;
      tempAni : AniStripArray;
      data, line, id, path: string;
      i, j, lineNo: Integer;
      input: text;
      
      procedure _ProcessSingleFrame();
      begin
        SetLength(aniData, Length(aniData) + 1);
        SetLength(aniData[High(aniData)].range, 1);
        SetLength(aniData[High(aniData)].idArray, 1);
        WriteLn('Adding SingleFrame');
        aniData[High(aniData)].idArray[0]    := StrToInt(ExtractDelimited(1, data, [',']));
        aniData[High(aniData)].range[0] := StrToInt(ExtractDelimited(2, data, [',']));
        aniData[High(aniData)].speed    := StrToInt(ExtractDelimited(3, data, [',']));
        if ExtractDelimitedWithRanges(4, data) <> '' then 
          aniData[High(aniData)].next     := StrToInt(ExtractDelimited(4, data, [',']))
        else
          aniData[High(aniData)].next     := -1;          
      end;
      
      procedure _ProcessMultiFrame();
      begin
        SetLength(aniData, Length(aniData) + 1);
        WriteLn('Adding MultiFrame');
        WriteLn('Doing IDs');
        aniData[High(aniData)].idArray  := ProcessRange(ExtractDelimitedWithRanges(1, data));
        WriteLn('Doing Cells');
        aniData[High(aniData)].range    := ProcessRange(ExtractDelimitedWithRanges(2, data));
        WriteLn('Finished Cells');
        aniData[High(aniData)].speed    := StrToInt(ExtractDelimitedWithRanges(3, data));
        if ExtractDelimitedWithRanges(4, data) <> '' then 
          aniData[High(aniData)].next     := StrToInt(ExtractDelimitedWithRanges(4, data))
        else
          aniData[High(aniData)].next     := -1;
        WriteLn('Finished MultiFrame');
      end;
      
      procedure _ProcessIDs();
      begin
        WriteLn('Start IDs');
        SetLength(aniString, Length(aniString) + 1);
        aniString[High(aniString)].value      := ExtractDelimited(1, data, [',']);

        aniString[High(aniString)].targetCell := StrToInt(ExtractDelimited(2, data, [',']));
        WriteLn('End IDs');
      end;
      
      procedure _ProcessSounds();
      begin
        WriteLn('Adding Sound');
        SetLength(aniSounds, Length(aniSounds) + 1);
        WriteLn(data);
        WriteLn(ExtractDelimited(1, data, [',']));
        aniSounds[High(aniSounds)].value      := ExtractDelimited(1, data, [',']);
        WriteLn(ExtractDelimited(2, data, [',']));
        WriteLn('Write Done');
        aniSounds[High(aniSounds)].targetCell := StrToInt(ExtractDelimited(2, data, [',']));
        WriteLn('ENdSOund');
      end;
      
      procedure _CalculateTargetStrip();
      var
        k,n : Integer;
      begin         
        for k := Low(aniData) to High(aniData) do
        begin
            WriteLn('parent idx k ', k);
            WriteLn('parent low k ', aniData[k].IDArray[Low(aniData[k].IDArray)]);
            WriteLn('parent low k ', aniData[k].IDArray[High(aniData[k].IDArray)]);
            WriteLn('next ' , aniData[i].next);
          if (aniData[i].next >= aniData[k].IDArray[Low(aniData[k].IDArray)]) AND
             (aniData[i].next <= aniData[k].IDArray[High(aniData[k].IDArray)]) then
          begin
            
            tempAni[High(tempAni)].lastCellParentID := k;
            tempAni[High(tempAni)].lastCell.cells[0].Idx     := aniData[i].next - aniData[k].idArray[0];
            tempAni[High(tempAni)].lastCell.cells[0].cellIdx := aniData[k].range[tempAni[High(tempAni)].lastCell.cells[0].Idx];
            WriteLn('idx ' ,tempAni[High(tempAni)].lastCell.cells[0].Idx);
          end;
       
        end;
      end;
      
      procedure _CalculateTargetID();
      var
        k,n : Integer;
      begin
        for k := Low(aniData) to High(aniData) do
        begin          
          for n := Low(aniString) to High(aniString) do
          begin
            if (aniString[n].targetCell >= aniData[k].IDArray[Low(aniData[k].IDArray)]) AND
            (aniString[n].targetCell <= aniData[k].IDArray[High(aniData[k].IDArray)]) then
              tempAni[k].cellGrp.cells[aniString[n].targetCell - aniData[k].idArray[0]].identifier := aniString[n].value;
          end;   
          for n := Low(aniSounds) to High(aniSounds) do
          begin
            if (aniSounds[n].targetCell >= aniData[k].IDArray[Low(aniData[k].IDArray)]) AND
            (aniSounds[n].targetCell <= aniData[k].IDArray[High(aniData[k].IDArray)]) then
              tempAni[k].cellGrp.cells[aniSounds[n].targetCell - aniData[k].idArray[0]].sound := aniSounds[n].value;
          end;            
        end;
      end;
      
      procedure _VerifyVersion();
      begin
        if EOF(input) then exit;
        line := '';
        
        while (Length(line) = 0) or (MidStr(line,1,2) = '//') do
        begin
          ReadLn(input, line);
          line := Trim(line);
        end;
        
        //Verify that the line has the right version
        if line <> 'SwinGame Animation #v1' then 
          RaiseException('Error in animation ' + dialogpath + '. Animation files must start with "SwinGame Animation #v1"');
        
      end;
      
      procedure ProcessLine();
      begin
        // Split line into id and data
        WriteLn(line);
        id := ExtractDelimited(1, line, [':']);
        data := ExtractDelimited(2, line, [':']);
        case LowerCase(id)[1] of // in all cases the data variable is read
          'f': _ProcessSingleFrame();
          'm': _ProcessMultiFrame();
          'i': _ProcessIds();
          's': _ProcessSounds();
        end;
      end;
      
  begin
    lineNo := 0;
    
    Assign(input, dialogpath);
    Reset(input);
    
     _VerifyVersion();
    try
      while not EOF(input) do
      begin
        lineNo := lineNo + 1;
      
        ReadLn(input, line);
        line := Trim(line);
        if Length(line) = 0 then continue;  //skip empty lines
        if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
      
        ProcessLine();
      end;
    finally
      Close(input);
    end;
      
    for i := Low(aniData) to High(aniData) do
    begin
      CreateAnimationStrip(tempAni, Length(aniData[i].range) + 1);
      with tempAni[High(tempAni)] do
      begin
        
        for j := Low(cellGrp.cells) to High(cellGrp.cells) - 1 do
        begin
          cellGrp.cells[j].cellIdx := aniData[i].range[j];
        end;         
        
        if aniData[i].next <> -1 then _CalculateTargetStrip();        
      end;
    end;  
    _CalculateTargetID();
    AniMode.aniStrips := tempAni;
    WriteLn(AniMode.aniStrips[0].lastCell.cells[0].cellIdx);
    UpdateBitmapPointers(AniMode.cellGrp, AniMode.aniStrips);
    PositionAniHorizontalScrollButtons(AniMode);
  end;
  
  procedure ExportAnimation(aniStrips: AniStripArray);
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
    
    procedure AddID(var idArray : StringArray; value : string; idlocation: integer);
    begin
      if value <> '' then
      begin
        SetLength(idArray, Length(idArray)+1);
        idArray[High(idArray)] := value + ',' + IntToStr(idLocation);
      end;
    end;
    
    function _lastCellCheck(aniStrip: AnimationStrip): last;
    begin
      with aniStrip do
      begin
        if (lastCell.cells[0].cellIdx <> -1) then
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
        AddID(arrayofIDs, cellGrp.cells[0].identifier, id);
        AddID(arrayofSounds, cellGrp.cells[0].sound, id);
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
        
        for i := Low(cellGrp.cells) to High(cellGrp.cells) do
        begin
          AddID(arrayofIDs, cellGrp.cells[i].identifier, id+i);
          AddID(arrayofSounds, cellGrp.cells[i].sound, id+i);
        end;
        
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
      end;
    end; 
  begin
    id := 0;
    Assign(txt, dialogpath);
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
      WriteLn(txt, 's:' + arrayofSounds[i]);
    end;
    
    WriteLn(txt, '//Identifiers');
    
    for i := Low(arrayofIDs) to High(arrayofIDs) do 
    begin
      WriteLn(txt, 'i:' + arrayofIDs[i]);
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
      DrawCell(dragCell^.BMPPtr^.scaled[dragCell^.parent^.GridType], dragCell^.cellIdx, dragPos);
    end;
	end;
  
  procedure DrawCellArea(cell: CellArea);
  begin
    DrawCell(cell.bmpPTR^.scaled[cell.parent^.GridType], cell.cellIdx, Trunc(cell.area.X), Trunc(cell.area.Y));
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