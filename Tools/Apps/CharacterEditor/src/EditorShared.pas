unit EditorShared;

//=============================================================================
interface
uses sgTypes, sgUserInterface, sgCharacters;

const
	CellGap 								= 5;
	CellGapSmall 			      = 2;
	CellGapLarge 			      = 15;
	ScreenWidth 						= 800;
	ScreenHeight						= 600;

//Editors
	BitmapEditorID					= 0;
	AnimationEditorID				= 1;
	CharacterEditorID				= 2;
		
//Other
	edgeDistance						= 50;
	ArrowWidth							= 39;
	ArrowHeight							= 26;
  
//Animation Editor
	BMPWindowWidth 		= 147;
	BMPWindowHeight 	= 170;
	BMPWindowX 				= 140;
	BMPWindowY 				= 135;
	
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
  MaxAniStrips  = 40;
  
 // Editor Shared Panels
  ToolBarMenu = 0; // Top menu of the editor
  FileMenu     = 1; // Drop down menu from the file button
  BrowserPanel   = 2;
  
  MainCharasFolders = 'chara_chubby,Chara-dwarf,Chara-Isometric,Chara-little,Chara-medium,Chara-monsters,Chara-monsters_f,Chara-Overweight Torso,Chara-soldiers,Chara-tall,Custom';
	
type
  BMPType  = (Original, BitmapGroup, SourceGroup, AnimationGroup, PreviewGroup); // The type of grids used for the various scales of the bitmaps draw
  DialogType = (None, SaveBMP, SaveAni, SaveChar, LoadBMP, LoadAni, LoadChar, LoadAniEdit, SetSoundPath);
	
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
    lastCellParentID,
    timing              : Integer;               // LastCellParentID is the id of the parent for the last cell for saving purposes
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
    Scaled : Array [BMPType] of Bitmap;          // The scaled version of the bitmaps for the different groups
    red, green, blue, cols, rows, 
    width, height, count : Integer;
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
    
  CharEditorValues = record
    panels : PanelArray;
    bg: Bitmap;
    MainChar : Character;
  end;

  AniStripArray = Array [0..MaxAniStrips] of AnimationStrip;
  
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
    radio2,
    stripCount      : Integer;
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
    panels          : PanelArray;
	end;
  
	//---------------------------------------------------------------------------
  // Locate CellGroup and CellArea
  //---------------------------------------------------------------------------  
  
  // Returns the cell at the mouse position    
  function CellAreaAt(cellGrp: CellGroupData; const pt: Point2D; const gap: Integer): CellAreaPtr;
  function ValidateInput(var target: Integer): Integer;
 
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
  procedure CreateAnimationStrip(cellCount : Integer; var AniMode: AnimationEditorValues);
  procedure UpdateBitmapPointers(var AniMode: AnimationEditorValues; bmpPtr: LoadedBitmapPtr);
  procedure PositionAniHorizontalScrollButtons(AniMode: AnimationEditorValues);

	//---------------------------------------------------------------------------
  // Bitmaps
  //--------------------------------------------------------------------------- 
  
  function BitmapAt(sharedVals: EditorValues; name : String): LoadedBitmap;
  function SetLoadedBitmapPtr(idx : Integer; bmpArray: LoadedBitmaps): LoadedBitmapPtr;
  procedure InitializeBitmapDetails(GridType: BMPType; var bmpArray: CharBodyTypes; scale: single; cols, rows: Integer);
  procedure SetBitmapDetails(GridType: BMPType; var bmp: LoadedBitmap; cols, rows: Integer);	
  procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string);
  procedure LoadBitmapsFromTextFile(var charBmps: CharBodyTypes; fileName : string);

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
 	procedure ExportBitmap(destbmp: Bitmap; cellGrp: CellGroupData); 
  procedure ExportAnimation(aniStrips: AniStripArray; path: string);
  procedure LoadAnimation(var AniMode: AnimationEditorValues);
  procedure ExportCharacter(CharMode: CharEditorValues; sharedVals: EditorValues; path: String);
  procedure LoadCharacterToEditor(var CharMode: CharEditorValues; path:string);
  procedure DoOpenDialog(var sharedVals : EditorValues; dt : DialogType);
  procedure DoSaveDialog(var sharedVals : EditorValues; dt : DialogType);
	
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
  
  function ValidateInput(var target: Integer): Integer;
  begin
    result := -1;
    if TryStrToInt(TextBoxText(GUITextBoxOfTextEntered), result) AND (result > -1) then
		begin	
      target := result;
    end else
      TextBoxSetText(GUITextBoxOfTextEntered, IntToStr(target));
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
    
  procedure CreateAnimationStrip(cellCount : Integer;var AniMode: AnimationEditorValues);
  var
    i: Integer;
	begin
    with AniMode do
    begin
    if stripCount <> MaxAniStrips then stripCount += 1;
    
    aniStrips[stripCount].idx 	:= 	stripCount; 
    
    aniStrips[stripCount].cellGrp  := InitializeCellGroup(cellCount, cellCount, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * stripCount), CellGapLarge, AnimationGroup);
    aniStrips[stripCount].LastCell := InitializeCellGroup(1, 1, 1, 24, 32, InitialAnimationPosX, InitialAnimationPosY + (AnimationIncrPosY * stripCount), CellGapLarge, AnimationGroup);
    
    InitializeCellArea(aniStrips[stripCount].cellGrp, nil, CellGapLarge, false);
    InitializeCellArea(aniStrips[stripCount].LastCell, nil, CellGapLarge, false);
 
    aniStrips[stripCount].scrollOffSet := 0;
    aniStrips[stripCount].lastCellParentID := -1;
    aniStrips[stripCount].timing := 1;
    
    for i := 0 to stripCount do DeselectAll(aniStrips[i].cellGrp);
    
    with aniStrips[stripCount] do
    begin
      MoveGroup(LastCell, Trunc(cellGrp.cells[High(cellGrp.cells)].area.x + (LastCell.grpArea.width + CellGapLarge)*2), Trunc(LastCell.grpArea.y));
      UpdatePosition(LastCell);
    end;   
    end;
	end;
  
  procedure UpdateBitmapPointers(var AniMode: AnimationEditorValues; bmpPtr: LoadedBitmapPtr);
  var
    i, j: Integer;
  begin
    if (Length(AniMode.cellGrp.cells) = 0) then exit;
    
    for i:= 0 to AniMode.stripCount do
    begin
      with AniMode.aniStrips[i] do
      begin       
        for j := Low(cellGrp.cells) to High(cellGrp.cells) do
        begin
          if cellGrp.cells[j].cellIdx <> -1 then
            cellGrp.cells[j].bmpPtr := bmpPtr;
        end;
        if lastCell.cells[0].cellIdx <> -1 then
          lastCell.cells[0].bmpPtr := bmpPtr;
      end;
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
      for i:= 0 to 2 do
      begin
        if aniMode.stripCount >= i then
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
   
  function BitmapAt(sharedVals: EditorValues; name : String): LoadedBitmap;
  var
    body, part, bmp: integer;
  begin  
    with sharedVals do
    begin
      for body := Low(Browser.bodyType) to High(Browser.bodyType) do
      begin
        for part := Low(Browser.bodyType[body].parts) to High(Browser.bodyType[body].parts) do
        begin
          for bmp := Low(Browser.bodyType[body].parts[part].bmps) to High(Browser.bodyType[body].parts[part].bmps) do
          begin
            if Browser.bodyType[body].parts[part].bmps[bmp].scaled[Original]^.name = name then
            begin
              result := Browser.bodyType[body].parts[part].bmps[bmp];
              exit;
            end;
          end;
        end;
      end;
    end;
  end;
  
	function CalculateScale(maxWidth, maxHeight, totalWidth, totalHeight: Integer): Single;
  begin
    if totalWidth > totalHeight then
      result := maxWidth / totalWidth
    else
      result := maxHeight / totalHeight;  
  end;
  
  procedure LoadBitmapsFromTextFile(var charBmps: CharBodyTypes; fileName : string);
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
          bmps[High(bmps)].scaled[Original]  := MapBitmap(ExtractFileName(ExtractDelimited(1, line, [','])), ExtractDelimited(1, line, [',']));	
        end;
      end;
    end;
    
    procedure ProcessLine();
    var
      scale: Array [BMPType] of Single;
      totalW, totalH, bodyID, partsID: Integer;
      i : BMPType;
    begin      
      AddToCollection(bodyID, partsID);
      
      with charBmps.BodyType[bodyID].parts[partsID] do
      begin
        bmps[High(bmps)].red     := StrToInt(ExtractDelimited(2, line, [',']));
        bmps[High(bmps)].green   := StrToInt(ExtractDelimited(3, line, [',']));
        bmps[High(bmps)].blue    := StrToInt(ExtractDelimited(4, line, [',']));
        bmps[High(bmps)].cols    := StrToInt(ExtractDelimited(5, line, [',']));
        bmps[High(bmps)].rows    := StrToInt(ExtractDelimited(6, line, [',']));
        bmps[High(bmps)].width   := StrToInt(ExtractDelimited(7, line, [',']));
        bmps[High(bmps)].height  := StrToInt(ExtractDelimited(8, line, [',']));
        bmps[High(bmps)].count   := StrToInt(ExtractDelimited(9, line, [',']));
        totalW       := bmps[High(bmps)].scaled[Original]^.Width;
        totalH       := bmps[High(bmps)].scaled[Original]^.Height;
                
        SetTransparentColor(bmps[High(bmps)].scaled[Original], RGBColor(bmps[High(bmps)].red, bmps[High(bmps)].green, bmps[High(bmps)].blue)); 
              
        scale[BitmapGroup]    := 1;
        scale[SourceGroup]    := CalculateScale(BMPWindowWidth, BMPWindowHeight, totalW + (bmps[High(bmps)].cols-1)*CellGapSmall, totalH + (bmps[High(bmps)].rows-1)*CellGapSmall);                                
        scale[AnimationGroup] := CalculateScale(AniCellWidth, AniCellHeight, bmps[High(bmps)].width, bmps[High(bmps)].height); 
        scale[PreviewGroup]   := CalculateScale(AniCellWidth*2, AniCellHeight*2, bmps[High(bmps)].width, bmps[High(bmps)].height);
        
        for i := BitmapGroup to High(BMPType) do
        begin
          if scale[i] = 1 then
            bmps[High(bmps)].Scaled[i] := bmps[High(bmps)].scaled[Original]
          else
            bmps[High(bmps)].Scaled[i] := RotateScaleBitmap(bmps[High(bmps)].scaled[Original], 0, scale[i]);	
          BitmapSetCellDetails(bmps[High(bmps)].Scaled[i],  Trunc(bmps[High(bmps)].width*scale[i]), Trunc(bmps[High(bmps)].height*scale[i]), bmps[High(bmps)].cols, bmps[High(bmps)].rows, bmps[High(bmps)].count);
        end;
      end;
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
		
		bmpArray[High(bmpArray)].scaled[Original] := MapBitmap(id, fileName);	
	//	SetTransparentColor(bmpArray[High(bmpArray)].scaled[Original], RGBColor(Red,Green,Blue));
		
		bmpArray[High(bmpArray)].scaled[BitmapGroup]    := bmpArray[High(bmpArray)].scaled[Original];
		bmpArray[High(bmpArray)].scaled[SourceGroup]    := bmpArray[High(bmpArray)].scaled[Original];
		bmpArray[High(bmpArray)].scaled[AnimationGroup] := bmpArray[High(bmpArray)].scaled[Original];
		bmpArray[High(bmpArray)].scaled[PreviewGroup]   := bmpArray[High(bmpArray)].scaled[Original];
	end;
    
  procedure SetBitmapDetails(GridType: BMPType; var bmp: LoadedBitmap; cols, rows: Integer);
  var
    cellW, cellH: Integer;
  begin    
    cellW := Trunc(bmp.scaled[GridType]^.width  / cols);
    cellH := Trunc(bmp.scaled[GridType]^.height / rows);
    BitmapSetCellDetails(bmp.scaled[GridType], cellW, cellH, cols, rows, cols*rows);
  end;
  
  procedure InitializeBitmapDetails(GridType: BMPType; var bmpArray: CharBodyTypes; scale: single; cols, rows: Integer);
  var
    body, part, img : Integer;
  begin	 
    for body := Low(bmpArray.bodyType) to High(bmpArray.bodyType) do
    begin
      for part := Low(bmpArray.bodyType[body].parts) to High(bmpArray.bodyType[body].parts) do
      begin
        for img := Low(bmpArray.bodyType[body].parts[part].bmps) to High(bmpArray.bodyType[body].parts[part].bmps) do
        begin
          bmpArray.bodyType[body].parts[part].bmps[img].scaled[GridType] := RotateScaleBitmap(bmpArray.bodyType[body].parts[part].bmps[img].scaled[Original], 0, scale);	
          SetBitmapDetails(GridType, bmpArray.bodyType[body].parts[part].bmps[img], cols, rows);
        end;
      end;
    end;
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
          else if KeyDown(VK_LCTRL) then AddSelection(cellGrp, i)
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
			DrawRectangle(destbmp, ColorBlack, xPos, yPos, cellW, cellH);
	//		FillRectangle(destbmp, ColorBlack, xPos, yPos, cellW -1, cellH -1);
			DrawText(destbmp, IntToStr(idx), ColorRed, xPos, yPos);		
		end;
	end;

	procedure ExportBitmap(destbmp: Bitmap; cellGrp: CellGroupData);
	var
		xPos, yPos, i: Integer;
	begin
		with cellGrp do
		begin	
      destbmp := CreateBitmap(cellGrp.grpArea.width - (cellGap*(cols-1)), cellGrp.grpArea.height - (cellGap*(rows-1)));
			for i := Low(cells) to High(cells) do
			begin
        xPos := Trunc(cells[i].Area.X - cells[i].xGap - grpArea.X);
        yPos := Trunc(cells[i].Area.Y - cells[i].yGap - grpArea.Y);
				if cells[i].bmpPtr = nil then
          SaveBitmapGrid(destBMP, cellGrp, i, xPos, yPos)
        else begin
          MakeOpaque(cells[i].bmpPtr^.scaled[cells[i].parent^.GridType]);		
          DrawCell(destbmp, cells[i].bmpPtr^.scaled[cells[i].parent^.GridType], cells[i].cellIdx, xPos, yPos);
          MakeTransparent(cells[i].bmpPtr^.scaled[cells[i].parent^.GridType]);	
        end;
			end;
      SaveToPNG(destbmp, dialogpath);
      FreeBitmap(destBmp);
		end;
	end; 
  
  procedure LoadAnimation(var AniMode: AnimationEditorValues);
  type
    LoadedData = record
      timing, next : Integer;
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
        aniData[High(aniData)].idArray[0]     := StrToInt(ExtractDelimited(1, data, [',']));
        aniData[High(aniData)].range[0]       := StrToInt(ExtractDelimited(2, data, [',']));
        aniData[High(aniData)].timing         := StrToInt(ExtractDelimited(3, data, [',']));
        if ExtractDelimitedWithRanges(4, data) <> '' then 
          aniData[High(aniData)].next         := StrToInt(ExtractDelimited(4, data, [',']))
        else
          aniData[High(aniData)].next         := -1;          
      end;
      
      procedure _ProcessMultiFrame();
      begin
        SetLength(aniData, Length(aniData) + 1);
        aniData[High(aniData)].idArray        := ProcessRange(ExtractDelimitedWithRanges(1, data));
        aniData[High(aniData)].range          := ProcessRange(ExtractDelimitedWithRanges(2, data));
        aniData[High(aniData)].timing         := StrToInt(ExtractDelimitedWithRanges(3, data));
        if ExtractDelimitedWithRanges(4, data) <> '' then 
          aniData[High(aniData)].next         := StrToInt(ExtractDelimitedWithRanges(4, data))
        else
          aniData[High(aniData)].next         := -1;
      end;
      
      procedure _ProcessIDs();
      begin
        SetLength(aniString, Length(aniString) + 1);
        aniString[High(aniString)].value      := ExtractDelimited(1, data, [',']);

        aniString[High(aniString)].targetCell := StrToInt(ExtractDelimited(2, data, [',']));
      end;
      
      procedure _ProcessSounds();
      begin
        SetLength(aniSounds, Length(aniSounds) + 1);
        aniSounds[High(aniSounds)].value      := ExtractDelimited(3, data, [',']);
        aniSounds[High(aniSounds)].targetCell := StrToInt(ExtractDelimited(1, data, [',']));
      end;
      
      procedure _CalculateTargetStrip();
      var
        k,n : Integer;
      begin         
        for k := Low(aniData) to High(aniData) do
        begin
          if (aniData[i].next >= aniData[k].IDArray[Low(aniData[k].IDArray)]) AND
             (aniData[i].next <= aniData[k].IDArray[High(aniData[k].IDArray)]) then
          begin
            AniMode.aniStrips[i].lastCellParentID := k;
            AniMode.aniStrips[i].lastCell.cells[0].Idx     := aniData[i].next - aniData[k].idArray[0];
            AniMode.aniStrips[i].lastCell.cells[0].cellIdx := aniData[k].range[AniMode.aniStrips[i].lastCell.cells[0].Idx];
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
              AniMode.aniStrips[k].cellGrp.cells[aniString[n].targetCell - aniData[k].idArray[0]].identifier := aniString[n].value;
          end;   
          for n := Low(aniSounds) to High(aniSounds) do
          begin
            if (aniSounds[n].targetCell >= aniData[k].IDArray[Low(aniData[k].IDArray)]) AND
            (aniSounds[n].targetCell <= aniData[k].IDArray[High(aniData[k].IDArray)]) then
              AniMode.aniStrips[k].cellGrp.cells[aniSounds[n].targetCell - aniData[k].idArray[0]].sound := aniSounds[n].value;
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
       // WriteLn(line);
        id := ExtractDelimited(1, line, [':']);
        data := ExtractAfterFirstDelim(1, line,':');//ExtractDelimited(2, line, [':']);
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
    
    AniMode.stripCount := -1;
    AniMode.aniStrips := tempAni;
    
    for i := Low(aniData) to High(aniData) do
    begin
      CreateAnimationStrip(Length(aniData[i].range) + 1, AniMode);
      with AniMode.aniStrips[i] do
      begin
        timing := aniData[i].timing;
        for j := Low(cellGrp.cells) to High(cellGrp.cells) - 1 do
        begin
          cellGrp.cells[j].cellIdx := aniData[i].range[j];
        end;         
        
        if aniData[i].next <> -1 then _CalculateTargetStrip();        
      end;
    end;  
    _CalculateTargetID();
    
    if AniMode.cellGrp.cellCount = 0 then
      UpdateBitmapPointers(AniMode, nil)
    else
      UpdateBitmapPointers(AniMode, AniMode.cellGrp.cells[0].bmpPtr);
    PositionAniHorizontalScrollButtons(AniMode);
  end;
  
  procedure ExportAnimation(aniStrips: AniStripArray; path: string);
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
        result.values := IntToStr(id) + ',' + IntToStr(cellGrp.cells[0].cellidx) + ',' + IntToStr(aniStrip.timing) + ',';
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
        result.values += ']' + ',' + IntToStr(aniStrip.timing) + ',';
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
    Assign(txt, path);
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
      WriteLn(txt, 's:' + ExtractDelimited(2, arrayofSounds[i], [',']) + ',' + ExtractFileName(ExtractDelimited(1, arrayofSounds[i], [','])) + ',' + ExtractDelimited(1, arrayofSounds[i], [',']));
    end;
    
    WriteLn(txt, '//Identifiers');
    
    for i := Low(arrayofIDs) to High(arrayofIDs) do 
    begin
      WriteLn(txt, 'i:' + arrayofIDs[i]);
    end;
    
    Close(txt);
  end;
  
  procedure ExportCharacter(CharMode: CharEditorValues; sharedVals: EditorValues; path: String);
  var
    i, j, k: Integer;
    txt: Text;
    bmp : LoadedBitmap;
    lst: GUIList;
    filename : string;
  begin
    Assign(txt, path);
    ReWrite(txt);
    WriteLn(txt, 'SwinGame Character #v1');
    WriteLn(txt, '//Character Template');
    WriteLn(txt, '');
    
    if  CharMode.MainChar^.CharName <> '' then
    begin
      WriteLn(txt, '//Name: [STRING]');
      WriteLn(txt, 'n: ', CharMode.MainChar^.CharName);
      WriteLn(txt, '');
    end;
    
    if  CharMode.MainChar^.CharName <> '' then
    begin
      WriteLn(txt, '//Type: [STRING]');
      WriteLn(txt, 't: ',CharMode.MainChar^.CharType);
      WriteLn(txt, '');
    end;    
    
    WriteLn(txt, '//Bitmaps: [NAME||FILE||WIDTH||HEIGHT||COLS||ROWS||CELLCOUNT||R||G||B||CollisionLayer]');
       
    lst := ListFromRegion(RegionWithID('LayerList'));
    
    for i := 0 to ListItemCount(RegionWithID('LayerList')) -1 do
    begin
      fileName := lst^.items[i].image.bmp^.name;
      bmp := BitmapAt(sharedVals, filename);
      Write(txt, 'b: ',fileName,',');
      Write(txt, bmp.scaled[Original]^.filename,',',bmp.scaled[Original]^.Width,',',bmp.scaled[Original]^.Height,',');
      Write(txt, bmp.cols , ',',bmp.rows , ',',bmp.count, ',',bmp.red, ',',bmp.green ,',',bmp.blue);
      if i = 0 then WriteLn(txt,', t') else WriteLn(txt,'');
    end;
    
    WriteLn(txt, '');
    
    with CharMode.MainChar^ do
    begin    
      if CharSprite^.animationTemplate <> nil then
      begin
        WriteLn(txt, '//Animation Template');
        WriteLn(txt, 'a: ' + CharSprite^.animationTemplate^.filename);
        WriteLn(txt, '');
      end;
      
      if NameCount(Directions) <> 0 then
      begin
        WriteLn(txt, '//Directions [Initial|Count|Names]');
        Write(txt, 'd: 0,',NameCount(Directions),',');
        for i := 0 to NameCount(Directions) -1 do
        begin
          Write(txt, NameAt(Directions, i));
          if i <> NameCount(Directions) -1 then Write(txt,',')
          else WriteLn(txt, ''); 
        end;    
        WriteLn(txt, '');
      end;
      
      if NameCount(States) <> 0 then
      begin
        WriteLn(txt, '//States [Initial|Count|Names]');
        Write(txt, 's: 0,',NameCount(States),',');
        for i := 0 to NameCount(States) -1 do
        begin
          Write(txt, NameAt(States, i));
          if i <> NameCount(States) -1 then Write(txt,',')
          else WriteLn(txt, ''); 
        end;
        WriteLn(txt, '');
      end;
      
      
      if Length(ShownLayers) <> 0 then
      begin
        WriteLn(txt, '//ShownLayers [t = true(show)| f = false(hide)]');
        Write(txt, 'l: ');
        for i := Low(ShownLayers) to High(ShownLayers) do 
        begin
          if ShownLayers[i] then Write(txt, 't') else Write(txt, 'f');
          if i <> High(ShownLayers) then Write(txt, ',')
          else WriteLn(txt, ''); 
        end;
        WriteLn(txt, '');
      end;
      
      if (NameCount(States) <> 0) AND (NameCount(Directions) <> 0) then
      begin
        WriteLn(txt, '');
        for i := 0 to NameCount(States) -1 do
        begin
          for j := 0 to NameCount(Directions) -1 do
          begin
            Write(txt, 'sd: ', NameAt(States, i), ',', NameAt(Directions, j),',', NameAt(CharSprite^.AnimationTemplate^.animationids, ShownLayersByDirState[i,j].Anim));
            if Length(ShownLayersByDirState[i,j].LayerOrder) <> 0 then
            begin
              Write(txt, ',[');
              for k := Low(ShownLayersByDirState[i,j].LayerOrder) to High(ShownLayersByDirState[i,j].LayerOrder) do
              begin
                Write(txt, ShownLayersByDirState[i,j].LayerOrder[k]);
                if k <> High(ShownLayersByDirState[i,j].LayerOrder) then Write(txt,',')
                else WriteLn(txt, ']'); 
              end;
            end;
          end;
        end;
        WriteLn(txt, '');
      end;
      
      
      if ListItemCount(RegionWithID('ValueList'))  <> 0 then
      begin
        WriteLn(txt, '//Values: [Name|Single]');
        for i := 0 to ListItemCount(RegionWithID('ValueList')) -1 do
        begin
          WriteLn(txt, 'v: ', Trim(ListItemText(RegionWithID('ValueList'), i)));
        end;
      end;
      WriteLn(txt, '');
    end;
    
    if ListItemCount(RegionWithID('AngleList'))  <> 0 then
    begin
      WriteLn(txt, '//Angles: [Direction|Min|Max]');
      for i := 0 to ListItemCount(RegionWithID('AngleList')) -1 do
      begin
        WriteLn(txt, 'p: ', Trim(ListItemText(RegionWithID('AngleList'), i)));
      end;
      WriteLn(txt, '');
    end;
    Close(txt);
  end;
  
  procedure LoadCharacterToEditor(var CharMode: CharEditorValues; path:string);
  var
    i, j, k: Integer;
    cell : BitmapCell;
    lst: GUIList;
  begin    
    FreeCharacter(CharMode.MainChar);
    CharMode.MainChar := LoadCharacter(path);
    
    with CharMode.MainChar^ do
    begin   
      TextBoxSetText(RegionWithID('CharName'), CharName);    
      TextBoxSetText(RegionWithID('CharType'), CharType);
       
      for i := 0 to NameCount(Directions) -1 do
      begin
        ListAddItem(ListFromRegion(RegionWithID('DirList')), NameAt(Directions, i));
        ListAddItem(ListFromRegion(RegionWithID('DirAngleList')), NameAt(Directions, i));
        ListAddItem(ListFromRegion(RegionWithID('DirLayerList')), NameAt(Directions, i));
      end;
      
      for i := 0 to NameCount(States) -1 do
      begin
        ListAddItem(ListFromRegion(RegionWithID('StateList')), NameAt(States, i));
        ListAddItem(ListFromRegion(RegionWithID('StateAngleList')), NameAt(States, i));
        ListAddItem(ListFromRegion(RegionWithID('StateLayerList')), NameAt(States, i));
      end;
      
      for i := 2 to NameCount(CharSprite^.ValueIds) -1 do
      begin
        ListAddItem(ListFromRegion(RegionWithID('ValueList')), NameAt(CharSprite^.ValueIds, i) + ',' + FloatToStr(CharSprite^.values[i]));
      end;
      
      for i := Low(DirectionParameters) to High(DirectionParameters) do
      begin
        ListAddItem(ListFromRegion(RegionWithID('AngleList')), IntToStr(DirectionParameters[i].min) + ',' + IntToStr(DirectionParameters[i].max) + ',' + NameAt(Directions, i));
      end;
      
      for i := Low(CharSprite^.layers) to High(CharSprite^.layers) do
      begin
        name := ExtractFileName(CharSprite^.layers[i]^.filename);
        cell := BitmapCellOf(BitmapNamed(name), 0);
        ListAddItem(ListFromRegion(RegionWithID('LayerList')), cell, name); 
      end;
      
      ListClearItems(RegionWithID('AniLayerList'));
      for i := 0 to NameCount(CharSprite^.animationTemplate^.animationIDs)-1 do
      begin 
        ListAddItem(RegionWithID('AniLayerList'), NameAt(CharSprite^.animationTemplate^.animationIDs, i));
      end;
    end;
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