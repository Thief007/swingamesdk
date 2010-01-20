program BitmapAnimationEditor;

uses
  crt, sgTypes, sgCore, sgAudio, sgAnimations, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, sgNamedIndexCollection,
  sgSprites, sgTimers, SysUtils;

const
	CellGap 								= 5;
	ScreenWidth 						= 1280;
	ScreenHeight						= 720;
	LengthCellData 					= 6;
	LengthgrpData 					= 10;
	LengthMode							= 2;
	LengthAniEditing				= 6;
	
//Cell Group Indexes
	cgXpos									= 0;
	cgYpos									= 1;
	cgDrag									= 2;
	cgCellCount							= 3;
	cgWidth									= 4;
	cgHeight								= 5;
	cgColumns								= 6;
	cgRows									= 7;
	cgScale									= 8;
	cgSelected							= 9;
//Animation Editing Area
	aeXpos									= 0;
	aeYpos									= 1;
	aeDrag									= 2;
	aeCellCount							= 3;
	aeWidth									= 4;
	aeHeight								= 5;

//Cell Area Indexes
	caXpos									= 0;
	caYpos									= 1;
	caCell									= 2;
	caXGap									= 3;
	caYGap									= 4;
	caBitmap								= 5;
	caID										= 4;
		
// Modes
	blankCells							= 0;
	splitImage							= 1;
//Colors
	Red											= 255;
	Green										= 255;
	Blue										= 255;
//Multipliers
	Neg											= -1;
	Pos											= 1;
//Other
	edgeDistance						= 50;
	ArrowWidth							= 39;
	ArrowHeight							= 26;
	
type
	EditorValues = record
		name : string;
		val : integer;
	end;
	
	LoadedBitmap = record
		original, src : Bitmap;
	end;
	
	CellArea = record
		data : Array [0..LengthCellData] of LongInt;
		isSelected : Boolean;
	end;
	
	CellGroup = record
		grpData : Array of LongInt; 
		cellArea : Array of CellArea;
		selectedOrder : Array of LongInt; 
		isAniGroup : boolean;
	end;
		
	Frame = record
		ids, cells, duration, next : Array of Integer;
	end;
	
	InitialData = record
		name : string;
		startCell : integer;
	end;	
	
	DraggedCell = record
		index, bmp, cell:Integer;
		isAniGroup: Boolean
	end;
		
	MultiFrame = Array of Frame;
	CellGroups 		= array of CellGroup;
	arrayofString = array [cgCellCount..cgScale] of String;
	arrayOfLoadedBMP = array of LoadedBitmap;

procedure InitializeAniCell(var aniCellGrp: cellGroup; i, ID, bitmap, cell:integer; var xGap: integer);
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
	xGap, i, oldLength: integer;
begin
	xGap 			:= 0;

	aniCellGrp.grpData[aeWidth]		:= cellGrp.grpData[cgWidth];
	aniCellGrp.grpData[aeHeight]	:= cellGrp.grpData[cgHeight];
	
	aniCellGrp.grpData[aeXpos]		:= edgeDistance;
	aniCellGrp.grpData[aeYpos]		:= ScreenHeight - aniCellGrp.grpData[aeHeight]  - edgeDistance;
	
	for  i := Low(aniCellGrp.cellArea) to High(aniCellGrp.cellArea) do
	begin
	{	aniCellGrp.cellArea[i].data[caXpos] 	:= cellGrp.grpData[cgWidth]  * i + aniCellGrp.grpData[aeXpos] + xGap;
		aniCellGrp.cellArea[i].data[caYpos]		:= aniCellGrp.grpData[aeYpos];	
		aniCellGrp.cellArea[i].data[caXGap]		:= xGap;
		aniCellGrp.cellArea[i].data[caID]			:= -1;
		aniCellGrp.cellArea[i].data[caBitmap]	:= -1;
		aniCellGrp.cellArea[i].data[caCell]		:= -1;
		
		xGap 			+= CellGap; }

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

procedure InitializeCellArea(var cellGrp : CellGroup);
var
	rows, columns, cellIndex, xGap, yGap, oldLength: LongInt;
// Gaps are shown inbetween cells when drawn on screen
begin
	xGap 			:= 0;
	yGap 			:= 0;	
	cellIndex := 0;
	
	if Length(cellGrp.cellArea) < cellGrp.grpData[cgCellCount] then oldLength := Length(cellGrp.cellArea)-1 else oldLength := cellGrp.grpData[cgCellCount];
	
	SetLength(cellGrp.cellArea, cellGrp.grpData[cgCellCount]);
	
	cellGrp.grpData[cgXpos]	:= (ScreenWidth DIV 2) 	- (cellGrp.grpData[cgColumns] * (cellGrp.grpData[cgWidth]  + CellGap)) DIV 2;
	cellGrp.grpData[cgYpos]	:= (ScreenHeight DIV 2) - (cellGrp.grpData[cgRows] 		* (cellGrp.grpData[cgHeight] + CellGap)) DIV 2;
	
	for  rows := 0 to cellGrp.grpData[cgRows] - 1 do
	begin
		for  columns := 0 to cellGrp.grpData[cgColumns] - 1 do
		begin
			cellGrp.cellArea[cellIndex].data[caXpos] 	:= cellGrp.grpData[cgWidth]  * columns + cellGrp.grpData[cgXpos] + xGap;
			cellGrp.cellArea[cellIndex].data[caYpos]	:= cellGrp.grpData[cgHeight] * rows		 + cellGrp.grpData[cgYpos] + yGap;	
			cellGrp.cellArea[cellIndex].data[caXGap]	:= xGap;
			cellGrp.cellArea[cellIndex].data[caYGap]	:= yGap;
			
			if cellIndex > oldLength then
			begin
				cellGrp.cellArea[cellIndex].isSelected			:= false;	
				cellGrp.cellArea[cellIndex].data[caBitmap]	:= -1;
				cellGrp.cellArea[cellIndex].data[caCell]		:= cellIndex;				
			end;
			
			if cellIndex = cellGrp.grpData[cgCellCount] - 1 then exit;
			
			xGap 			+= CellGap;
			cellIndex	+= 1;
		end;
		xGap := 0;
		yGap += CellGap;
	end;
end;

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

	InitializeCellArea(cellGrp);
end;

procedure UpdateAllCellAreas(var aniCellGrp, cellGrp: cellGroup);
begin
	InitializeCellArea(cellGrp);
	InitializeAniCellArea(aniCellGrp,cellGrp);	
end;

procedure AddCellToAnimation(var aniCellGrp, cellGrp : cellGroup);
var
	i, oldLength, xGap, selectIncr: integer;
begin
	oldLength := Length(aniCellGrp.cellArea);
	selectIncr := 0;
	xGap := aniCellGrp.cellArea[High(aniCellGrp.cellArea) - 1].data[caXGap] + CellGap;
	SetLength(aniCellGrp.cellArea, Length(cellGrp.selectedOrder) + oldLength);
	aniCellGrp.cellArea[High(aniCellGrp.cellArea)] := aniCellGrp.cellArea[oldLength -1];
	
	for i := oldLength -2 to High(aniCellGrp.cellArea) -2 do
	begin	
		InitializeAniCell(aniCellGrp, i, cellGrp.selectedOrder[selectIncr], cellGrp.cellArea[cellGrp.selectedOrder[selectIncr]].data[caBitmap], cellGrp.cellArea[cellGrp.selectedOrder[selectIncr]].data[caCell], xGap);
		selectIncr += 1;
	end;
	InitializeAniCell(aniCellGrp, High(aniCellGrp.cellArea) -1 , -1 , -1 , -1, xGap);
	InitializeAniCell(aniCellGrp, High(aniCellGrp.cellArea), aniCellGrp.cellArea[High(aniCellGrp.cellArea)].data[caID], aniCellGrp.cellArea[High(aniCellGrp.cellArea)].data[caBitmap],
										aniCellGrp.cellArea[High(aniCellGrp.cellArea)].data[caCell], xGap);	
end;

procedure AddCellToAnimation(var aniCellGrp: cellGroup; dCell: DraggedCell; i: integer); overload;
var
	oldLength, xGap, selectIncr: integer;
begin	
	InitializeAniCell(aniCellGrp, i, dCell.Index, dCell.Bmp, dCell.Cell, aniCellGrp.cellArea[i].data[caXGap]);
	
	if (i = High(aniCellGrp.cellArea) -1) then
	begin
		SetLength(aniCellGrp.cellArea, Length(aniCellGrp.cellArea) + 1);
		xGap := aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caXGap] + CellGap;
		InitializeAniCell(aniCellGrp, High(aniCellGrp.cellArea), aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caID], aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caBitmap],
									aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caCell], xGap);
		InitializeAniCell(aniCellGrp, High(aniCellGrp.cellArea) -1 , -1 , -1 , -1, aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caXGap]);
	end;
end;


procedure AddBitmapToArray(var bmpArray : arrayOfLoadedBMP; id, fileName : string; cellGrp : CellGroup);
begin
	SetLength(bmpArray, Length(bmpArray)+1);
	
	bmpArray[High(bmpArray)].original := MapBitmap(id, fileName);	
	SetTransparentColor(bmpArray[High(bmpArray)].original, RGBColor(Red,Green,Blue));
	
	bmpArray[High(bmpArray)].src := bmpArray[High(bmpArray)].original;
	SetBitmapCellDetails(bmpArray[High(bmpArray)].src, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight], cellGrp.grpData[cgColumns], cellGrp.grpData[cgRows], cellGrp.grpData[cgCellCount]);
end;

procedure InitializeLabels(out labels : arrayofString);
begin
	labels[cgCellCount] := 'Cell Count';
	labels[cgWidth] 		:= 'Width';
	labels[cgHeight] 		:= 'Height';
	labels[cgRows] 			:= 'Rows';
	labels[cgColumns] 	:= 'Columns';
	labels[cgScale] 		:= 'Scale';
end;

procedure ModifyCellBitmap(var cellGrp : cellGroup; value, comparedValue : integer; valid : boolean);
var
	i : integer;
begin
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		if ((cellGrp.cellArea[i].isSelected) AND ((cellGrp.cellArea[i].data[caBitmap] + value <= comparedValue) = valid)) then cellGrp.cellArea[i].data[caBitmap] += value;
	end;
end;

procedure ModifyCellIndex(var cellGrp : cellGroup; value, comparedValue : integer; valid : boolean);
var
	i : integer;
begin
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		if ((cellGrp.cellArea[i].isSelected) AND ((cellGrp.cellArea[i].data[caCell] + value <= comparedValue) = valid)) then cellGrp.cellArea[i].data[caCell] += value;
	end;
end;

procedure AddSelection(var cellGrp : cellGroup; i : integer);
begin
	SetLength(cellGrp.selectedOrder, Length(cellGrp.selectedOrder) + 1);
	
	cellGrp.selectedOrder[High(cellGrp.selectedOrder)] := i;
	cellGrp.cellArea[i].isSelected := true;
end;

procedure RemoveSelection(var cellGrp : cellGroup; i : integer);
var
	j: integer;
begin

	for j := Low(cellGrp.selectedOrder) to High(cellGrp.selectedOrder) do
	begin
		cellGrp.cellArea[i].isSelected := false;
		
		if cellGrp.selectedOrder[j] = i then
		begin
			if High(cellGrp.selectedOrder) <> j then cellGrp.selectedOrder[j] := cellGrp.selectedOrder[j+1];
		end;
	end;
	
	if (Length(cellGrp.selectedOrder) >  0) then SetLength(cellGrp.selectedOrder, Length(cellGrp.selectedOrder)-1);
end;

procedure DeselectAll(var cellGrp : cellGroup);
var 
	i: integer;
begin
	SetLength(cellGrp.selectedOrder, 0);
	
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do cellGrp.cellArea[i].isSelected := false;
end;

procedure SelectMultiple(var cellGrp : cellGroup; i : integer);
var
	previousSelect, newLength, j, count, startVal : integer;
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
		cellGrp.cellArea[startval].isSelected := true;
		startVal														 	+= 1;
	end;
end;

procedure DrawCellGroup(cellGrp : CellGroup; i, width, height: integer);
begin
	if cellGrp.cellArea[i].isSelected then
		DrawRectangleOnScreen(ColorBlack, cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], width, height)
	else
		DrawRectangleOnScreen(ColorWhite, cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], width, height);
end; 

procedure DrawOnMouse(cellGrp: cellGroup; bmpArray: arrayOfLoadedBMP; dCell: DraggedCell; mouseOffSet : Point2D);
var
	dragPos: Point2D;
begin
	dragPos.x := MousePosition().x - mouseOffSet.x;
	dragPos.y := MousePosition().y - mouseOffSet.y;
	if (dCell.bmp <> -1) then DrawCell(bmpArray[cellGrp.cellArea[dCell.Index].data[caBitmap]].src, cellGrp.cellArea[dCell.Index].data[caCell], dragPos);
end;

procedure DrawAnimationGroup(aniCellGrp, cellGrp : CellGroup; dCell: DraggedCell; bmpArray: arrayOfLoadedBMP; mouseOffSet: Point2D);
var
	i: integer;
	dragPos : Point2D;
begin
	for i := Low(aniCellGrp.cellArea) to High(aniCellGrp.cellArea) do
	begin
		DrawText(IntToStr(i), ColorBlack, aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos] - 10);
		DrawText(IntToStr(aniCellGrp.cellArea[i].data[caID]), ColorBlack, aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos] + 10 + aniCellGrp.grpData[cgHeight]);
		DrawCellGroup(aniCellGrp, i, aniCellGrp.grpData[cgWidth], aniCellGrp.grpData[cgHeight]);
		if (aniCellGrp.cellArea[i].data[caBitmap] <> -1) AND (i <> dCell.Index) then
			DrawCell(bmpArray[aniCellGrp.cellArea[i].data[caBitmap]].src, aniCellGrp.cellArea[i].data[caCell], aniCellGrp.cellArea[i].data[caXpos], aniCellGrp.cellArea[i].data[caYpos]);
		WriteLn(i);
	end;
	if ((dCell.Index <> -1)) then if (dCell.isAniGroup) then 
	begin
		WriteLn('WHy');
		DrawOnMouse(aniCellGrp, bmpArray, dCell, mouseOffSet);
	end;
	WriteLn('fail');
end; 

procedure DrawBitmapGrid(destbmp: bitmap; save: boolean; cellGrp : cellGroup; i : integer);
var
  xPos, yPos : integer;
begin
	FillRectangleOnscreen(ColorWhite, cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight]);
	DrawText(IntToStr(i), ColorBlack, cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos]);
	if save then 
	begin
		xPos := cellGrp.cellArea[i].data[caXpos] - cellGrp.grpData[cgXpos] - cellGrp.cellArea[i].data[caXGap];
		yPos := cellGrp.cellArea[i].data[caYpos] - cellGrp.grpData[cgYpos] - cellGrp.cellArea[i].data[caYGap];
		
		DrawRectangle(destbmp, ColorWhite, xPos, yPos, cellGrp.grpData[cgWidth] -1, cellGrp.grpData[cgHeight] -1);
		DrawText(destbmp, IntToStr(i), ColorYellow, xPos, yPos);		
	end; 	
end;

procedure DrawSplitBitmapCells(destbmp: Bitmap; bmpArray: arrayOfLoadedBMP; cellGrp : CellGroup; save: boolean; mouseOffSet: Point2D; dCell: DraggedCell);
var
	i: integer;
	dragPos : Point2D;
begin
	if save then 	for i := Low(bmpArray) to High(bmpArray) do MakeOpaque(bmpArray[i].src);
	
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		DrawCellGroup(cellGrp, i, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight]);
		if cellGrp.cellArea[i].data[caBitmap] = -1 then 
		begin
			DrawBitmapGrid(destbmp,save,cellGrp,i)
		end else begin
			if i <> dCell.index then DrawCell(bmpArray[cellGrp.cellArea[i].data[caBitmap]].src, cellGrp.cellArea[i].data[caCell], cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos]);
			
			if save then DrawCell(destbmp, bmpArray[cellGrp.cellArea[i].data[caBitmap]].src, cellGrp.cellArea[i].data[caCell], 
				cellGrp.cellArea[i].data[caXpos] - cellGrp.cellArea[i].data[caXGap] - cellGrp.grpData[cgXpos], cellGrp.cellArea[i].data[caYpos] - cellGrp.cellArea[i].data[caYGap] - cellGrp.grpData[cgYpos]);
		end;	
	end;
	
	if (dCell.Index <> -1) AND (not dCell.isAniGroup) then DrawOnMouse(cellGrp, bmpArray, dCell, mouseOffSet);

	if save then for i := Low(bmpArray) to High(bmpArray) do MakeTransparent(bmpArray[i].src);
end;

procedure DrawTestValues(labels : arrayofString; cellGrp : CellGroup; editVal: integer);
var 
	i, xPos, yPos: integer;
begin
	xPos := 10;
	yPos := 10;
	for i:= Low(labels) to High(labels) do
	begin
		DrawText(labels[i] + ' : ' + IntToStr(cellGrp.grpData[i]), ColorWhite, xPos, yPos);
		yPos += xPos;
	end;
	DrawText('Currently Editing:', ColorWhite, xPos, yPos + 30);
	DrawText(labels[editVal] + ' : ' + IntToStr(cellGrp.grpData[editVal]), ColorWhite, xPos, yPos + 40);	
	DrawText('X : ' + IntToStr(cellGrp.grpData[cgXpos]), ColorWhite, xPos, yPos + 70);
	DrawText('Y : ' + IntToStr(cellGrp.grpData[cgYpos]), ColorWhite, xPos, yPos + 80);
	DrawText('[TAB]: Change Currently Editing Index', ColorWhite, xPos, yPos + 100);
	DrawText('CapsLock: Change Mode', ColorWhite, xPos, yPos + 110);
	DrawText('[+]: Increment', ColorWhite, xPos, yPos + 120);
	DrawText('[-]: Decrement', ColorWhite, xPos, yPos + 130);
end;

procedure DrawCellTestValues(cellGrp : CellGroup);
var
	i, startPosY : integer;
begin
	startPosY := 250;
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		if cellGrp.cellArea[i].isSelected then
		begin
				DrawText('Cell: ' + IntToStr(cellGrp.cellArea[i].data[caCell]), ColorWhite, 10, startPosY + i * 20);
				DrawText('Bitmap: ' + IntToStr(cellGrp.cellArea[i].data[caBitmap]), ColorWhite, 10, startPosY + 10 + i * 20);
		end;
	end;
end;

procedure AlterValues(var cellGrp, aniCell: CellGroup; var bmpArray: arrayOfLoadedBMP; editVal, multiplier : integer);
var
	i: LongInt;
begin
	if (editVal = cgColumns) AND (((cellGrp.grpData[cgColumns] + multiplier) * cellGrp.grpData[cgRows])  > (cellGrp.grpData[cgCellCount] + cellGrp.grpData[cgRows])) 	  OR
		 (editVal = cgRows)		 AND (((cellGrp.grpData[cgRows] + multiplier) * cellGrp.grpData[cgColumns])  > (cellGrp.grpData[cgCellCount] + cellGrp.grpData[cgColumns])) OR
		 (editVal = cgScale)   AND (cellGrp.grpData[cgScale] + multiplier = 0) then exit;

	cellGrp.grpData[editVal] += multiplier;
	
	if editVal = cgScale then
	begin
		cellGrp.grpData[cgWidth] := cellGrp.grpData[cgWidth]	 + (cellGrp.grpData[cgWidth]	DIV (cellGrp.grpData[cgScale] - multiplier) * multiplier);
		cellGrp.grpData[cgHeight]:= cellGrp.grpData[cgHeight] + (cellGrp.grpData[cgHeight] DIV (cellGrp.grpData[cgScale] - multiplier) * multiplier);
	end;
	
	UpdateAllCellAreas(aniCell, cellGrp);
	
	for i:= Low(bmpArray) to High(bmpArray) do
	begin
		if editVal = cgScale then bmpArray[i].src := RotateScaleBitmap(bmpArray[i].original, 0, cellGrp.grpData[cgScale]);		
		SetBitmapCellDetails(bmpArray[i].src, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight], cellGrp.grpData[cgColumns], cellGrp.grpData[cgRows], cellGrp.grpData[cgCellCount]);
	end;
end;

procedure HandleSelection(var cellGrp : CellGroup; out mouseOffSet : Point2D);
var
	i, tempCell, tempBMP : integer;
begin
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		if KeyDown(VK_LCTRL) AND PointInRect(MousePosition(), cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgHeight], cellGrp.grpData[cgWidth]) AND cellGrp.cellArea[i].isSelected then
		begin
			RemoveSelection(cellGrp, i);
		end else
		if KeyDown(VK_LCTRL) AND PointInRect(MousePosition(), cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgHeight], cellGrp.grpData[cgWidth]) then
		begin
			AddSelection(cellGrp, i);
		end else
		if KeyDown(VK_LShift) AND PointInRect(MousePosition(), cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgHeight], cellGrp.grpData[cgWidth]) then
		begin
			SelectMultiple(cellGrp, i);
		end;
	end;
end;

procedure HandleCellDragging(var cellGrp : CellGroup;var dCell: DraggedCell; out mouseOffSet : Point2D);
var
	i, temp : integer;
	tempCell : DraggedCell;
begin
	for i := Low(cellGrp.cellArea) to High(cellGrp.cellArea) do
	begin
		if KeyDown(VK_LALT) AND PointInRect(MousePosition(), cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgHeight], cellGrp.grpData[cgWidth]) AND 
			(dCell.index = -1) AND (cellGrp.cellArea[i].data[caBitmap] <> -1) then
		begin
			dCell.index  		:= i;
			dCell.cell 	 		:= cellGrp.cellArea[i].data[caCell];
			dCell.Bmp 			:= cellGrp.cellArea[i].data[caBitmap];
			dCell.isAniGroup:= cellGrp.isAniGroup;
			
			mouseOffSet.x 	:= MousePosition().x - cellGrp.cellArea[i].data[caXpos];
			mouseOffSet.y 	:= MousePosition().y - cellGrp.cellArea[i].data[caYpos];
		end else
		if PointInRect(MousePosition(), cellGrp.cellArea[i].data[caXpos], cellGrp.cellArea[i].data[caYpos], cellGrp.grpData[cgHeight], cellGrp.grpData[cgWidth]) AND (dCell.index <> -1) then
		begin
			if (not cellGrp.isAniGroup AND dCell.isAniGroup) then exit
			else if (not cellGrp.isAniGroup AND not dCell.isAniGroup) OR (cellGrp.isAniGroup AND dCell.isAniGroup) then
			begin
				cellGrp.cellArea[dCell.index].data[caCell] 	 := cellGrp.cellArea[i].data[caCell];
				cellGrp.cellArea[dCell.index].data[caBitmap] := cellGrp.cellArea[i].data[caBitmap];
			end;
			if (cellGrp.isAniGroup AND (not dCell.isAniGroup)) then
			begin
				AddCellToAnimation(cellGrp, dCell, i);
			end; //then	cellGrp.cellArea[i].data[caID] := dCell.Index;
			if (cellGrp.isAniGroup) AND (dCell.isAniGroup) then
			begin
				temp := cellGrp.cellArea[dCell.Index].data[caID];
				cellGrp.cellArea[dCell.Index].data[caID] := cellGrp.cellArea[i].data[caID];
				cellGrp.cellArea[i].data[caID] := temp;
			end;
			
			cellGrp.cellArea[i].data[caCell] 						 := dCell.Cell;
			cellGrp.cellArea[i].data[caBitmap] 					 := dCell.Bmp;
			dCell.index  																 	 := -1;
		end;
	end;
end;

procedure ProcessInput(var aniCell, cellGrp : cellGroup; labels : arrayofString; var destBMP: Bitmap; var bmpArray: arrayofLoadedBMP; var editVal, mode: integer;out  mouseOffSet: Point2D;out save: boolean);
var
	w, h, i: integer;
begin
	if KeyTyped(vk_TAB) 				AND (editVal < High(labels)) then editVal += 1
	else if KeyTyped(vk_TAB) 		AND (editVal = High(labels)) then editVal := Low(labels)
	
	else if (KeyTyped(vk_MINUS) AND (cellGrp.grpData[editVal] > 1)) then AlterValues(cellGrp, aniCell, bmpArray, editVal, - 1)			
	else if (KeyTyped(vk_EQUALS)) then AlterValues(cellGrp, aniCell, bmpArray, editVal, 1)
	
	else if (KeyTyped(vk_1)) then
	begin
		FreeBitmap(destBMP);
		w 			:= cellGrp.grpData[cgColumns] * cellGrp.grpData[cgWidth];
		h 			:= cellGrp.grpData[cgRows] 		* cellGrp.grpData[cgHeight];
		destbmp := CreateBitmap(w,h );
		save 		:= true;
	end		
	
	else if (KeyTyped(vk_2) AND (Length(cellGrp.selectedOrder) > 0)) then 
	begin
		AddCellToAnimation(aniCell, cellGrp);
		exit;
	end
	
	else if KeyTyped(vk_CAPSLOCK) AND (mode < LengthMode -1) then mode += 1
	else if KeyTyped(vk_CAPSLOCK) AND (mode = LengthMode -1) then mode := 0
	
	else if KeyTyped(vk_PAGEUP) 	then ModifyCellBitmap(cellGrp, 1,	high(bmpArray), true)
	else if KeyTyped(vk_PAGEDOWN) then ModifyCellBitmap(cellGrp, -1, -2, false)
	else if KeyTyped(vk_0) 				then ModifyCellIndex(cellGrp, 1, cellGrp.grpData[cgCellCount] -1, true)
	else if KeyTyped(vk_9) 				then ModifyCellIndex(cellGrp, -1, 0, false)
	else if KeyTyped(vk_ESCAPE) 	then DeselectAll(cellGrp)
	else exit;
	
	for i:= Low(bmpArray) to High(bmpArray) do
	begin	
		SetBitmapCellDetails(bmpArray[i].src, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight], cellGrp.grpData[cgColumns], cellGrp.grpData[cgRows], cellGrp.grpData[cgCellCount]);
	end;
end;

function OutputSingleFrameAnimation(aniCellGrp: cellGroup): string;
begin
	result := 'f:'  + '0,' + IntToStr(aniCellGrp.cellArea[Low(aniCellGrp.cellArea)].data[caID]) + ',5,';
	if (aniCellGrp.cellArea[High(aniCellGrp.cellArea)].data[caID] <> -1) then result += IntToStr(aniCellGrp.cellArea[Low(aniCellGrp.cellArea)].data[caID])
end;

function OutputMultiFrameAnimation(aniCellGrp: cellGroup): string;
begin
end;

procedure ExportAnimationFile(aniCellGrp : cellGroup; name : string);
var
	i : integer;
	txt: Text;
	arrayOfSingleFrame, arrayofMultiFrame : array of string;
begin
	Assign(txt, PathToResource('Animations\' + name + '.txt'));
	ReWrite(txt);
	WriteLn(txt,'SwinGame Animation #v1');
	WriteLn(txt,'');
	WriteLn(txt,'//Frames are declared with an f: and contain');
	WriteLn(txt,'//the following comma separated values');
	WriteLn(txt,'//ID,CELL,DUR,NEXT');
	if Length(aniCellGrp.cellArea) = 3 then
	begin
		SetLength(arrayOfSingleFrame, Length(arrayOfSingleFrame) + 1);
		arrayOfSingleFrame[High(arrayOfSingleFrame)] := OutputSingleFrameAnimation(aniCellGrp));
	end else if Length(aniCellGrp.cellArea) > 3 then
	begin
		SetLength(arrayOfMultiFrame, Length(arrayOfMultiFrame) + 1);
		arrayOfMultiFrame[High(arrayOfMultiFrame)] := OutputSingleFrameAnimation(aniCellGrp));
	end;
	for i := Low(arrayOfSingleFrame) to High(arrayOfSingleFrame) do WriteLn(txt, arrayOfSingleFrame[i]);
	
	WriteLn(txt, '//Multi-frame: ranges are in[]');
	WriteLn(txt, '//[a-b] = numbers from a to b inclusive');
	WriteLn(txt, '//[a,b,c] = explicit values');
	WriteLn(txt, '//[a-b,c] = combination');

	for i := Low(arrayOfMultiFrame) to High(arrayOfMultiFrame) do WriteLn(txt, arrayOfMuliFrame[i]);
	
	
	
	
	Close(txt);
end;

procedure Main();
var
	cellGrp, aniCellGrp : CellGroup;
	dCell: DraggedCell;
	mode, i, H, W, editVal: integer; 
	save: boolean;
	destBMP, src, original, arrow : Bitmap;
	bmpArray : arrayofLoadedBMP;
	mouseOffset : Point2D; 
	labels : arrayofString;
	ani : Animation;
	aniTemp : AnimationTemplate;
	aniData : AnimationData;
	s : string;
begin
  OpenAudio();
  OpenGraphicsWindow('Hello World', ScreenWidth, ScreenHeight);
	
	InitializeCellGroup(cellGrp);
	InitializeAniCellGroup(aniCellGrp, cellGrp);
	InitializeLabels(labels);
	
	save 		:= false;
	mode 		:= 0;
	editVal := Low(labels);	
	destBMP := CreateBitmap(1, 1);
	arrow 	:= LoadBitmap('arrow.png');
	dCell.Index 	:= -1;
	
	AddBitmapToArray(bmpArray, 'Corpo', 'corpolupo1.png', cellGrp);
	AddBitmapToArray(bmpArray, 'Black', 'blackbody.png', cellGrp);

	for i := 0 to 4 do AddBitmapToArray(bmpArray, IntToStr(i), IntToStr(i) + '.gif', cellGrp);
	
	aniTemp:=LoadAnimationTemplate('Test.txt');
  ani := CreateAnimation('Up',aniTemp,FALSE);
	
  repeat // The game loop...
    ProcessEvents();
    ClearScreen(ColorRed);
		
		if AnyKeyPressed 						then ProcessInput(aniCellGrp, cellGrp, labels, destBMP, bmpArray, editVal, mode, mouseOffSet, save);
		if MouseClicked(LeftButton) then
		begin
			HandleSelection(cellGrp, mouseOffSet);
			HandleSelection(aniCellGrp, mouseOffSet);
			HandleCellDragging(cellGrp, dCell, mouseOffSet);
			HandleCellDragging(aniCellGrp, dCell, mouseOffSet);
		end;
		
		DrawTestValues(labels,cellGrp, editVal);
		DrawCellTestValues(cellGrp);
		DrawSplitBitmapCells(destbmp, bmpArray, cellGrp, save, mouseOffSet, dCell);	
		UpdateAnimation(ani);
		DrawAnimation(ani, destBMP, ScreenWidth - 50 - cellGrp.grpData[cgWidth], (ScreenHeight DIV 2) - (cellGrp.grpData[cgHeight] DIV 2));
		DrawAnimationGroup(aniCellGrp, cellGrp, dCell, bmpArray, mouseOffSet);
		DrawBitmap(arrow, aniCellGrp.cellArea[High(aniCellGrp.cellArea)-1].data[caXPos] + CellGap + aniCellGrp.grpData[aeWidth], aniCellGrp.grpData[aeYPos] - (ArrowHeight DIV 2) + aniCellGrp.grpData[aeHeight] DIV 2);
		
		if save then
		begin
			SaveBitmap(destbmp,'D:\destbmp.bmp');
			SetBitmapCellDetails(destBMP, cellGrp.grpData[cgWidth], cellGrp.grpData[cgHeight], cellGrp.grpData[cgColumns], cellGrp.grpData[cgRows], cellGrp.grpData[cgCellCount]);
			save := false;
		end;
		
		if KeyTyped(vk_3) then ExportAnimationFile(aniCellGrp,'testAni');
		
		RefreshScreen(60);   
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio(); 
end;

begin
	Main();
end.