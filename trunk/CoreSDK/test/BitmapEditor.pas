program BitmapAnimationEditor;

uses
  crt, sgTypes, sgCore, sgAudio, sgAnimations, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTimers, SysUtils;

type
	EditorValues = record
		name : string;
		val : integer;
	end;
	
	LoadedBitmap = record
		original, src : Bitmap;
	end;
	
	
	
	layout = array of Integer;			
	eVals = array of array of EditorValues;
	rectangleArray = Array of Rectangle;

const
	CellGap			= 5;
	CellCount 	= 0;
	CellWidth 	= 1;
	CellHeight 	= 2;
	Columns			= 5;
	Rows 				= 6;
	xPos 				= 3;
	yPos 				= 4;
	Scale 			= 7;	
	ScreenWidth = 1280;
	ScreenHeight= 720;

procedure IncrementValues(out intArray : array of Integer);
begin
	intArray[CellCount] 	:= 1;
	intArray[CellWidth] 	:= 1;
	intArray[CellHeight] 	:= 1;
	intArray[Columns] 		:= 1;
	intArray[Rows] 				:= 1;
	intArray[xPos] 				:= 50;
	intArray[yPos] 				:= 50;
	intArray[Scale] 			:= 1;
end;
	
procedure InitializeTestValues(var editVals : eVals);
var
	i: integer;
begin
	for i := 0 to high(EditVals) do
	begin
		editVals[i,CellCount].Name 	:= 'Cell Count';
		editVals[i,CellWidth].Name 	:= 'Cell Width';
		editVals[i,CellHeight].Name := 'Cell Height';
		editVals[i,Columns].Name 		:= 'Columns';
		editVals[i,Rows].Name 			:= 'Rows';
		editVals[i,xPos].Name 			:= 'xPos';
		editVals[i,yPos].Name 			:= 'yPos';
		editVals[i,Scale].Name 			:= 'Scale';
		
		// initial values
		editVals[i,CellCount].val 	:= 12;
		editVals[i,CellWidth].val 	:= 24;
		editVals[i,CellHeight].val 	:= 32;
		editVals[i,Columns].val 		:= 3;
		editVals[i,Rows].val 				:= 4;
		editVals[i,xPos].val 				:= 600;
		editVals[i,yPos].val 				:= 300;
		editVals[i,Scale].val 			:= 1;
	end;
end;

procedure InitializePositions(out position : layout; rectArray : rectangleArray);
var
	i: integer;
begin
	SetLength(position, Length(rectArray));
	for i := 0 to High(rectArray) do
	begin
		position[i] := i;
	end;
end;

procedure ResetRectangles(out rectArray : rectangleArray; editVals : eVals; bmpType : integer);
var
  i,j, xGap, yGap, count: LongInt;
begin
  xGap := 0;
  yGap := 0; 
	count := 0;
	
	SetLength(rectArray, editVals[bmpType,CellCount].val);
	
	editVals[bmpType,xPos].val 				:= (ScreenWidth DIV 2) - (editVals[bmpType,Columns].val * (editVals[bmpType,CellWidth].val + CellGap)) DIV 2;
	editVals[bmpType,yPos].val 				:= (ScreenHeight DIV 2) - (editVals[bmpType,Rows].val * (editVals[bmpType,CellHeight].val + CellGap)) DIV 2;
	
	for i := 0 to editVals[bmpType,Rows].val - 1 do
	begin
		for j := 0 to editVals[bmpType,Columns].val - 1 do
		begin
			rectArray[count].x 				:= editVals[bmpType,CellWidth].val * j + editVals[bmpType,xPos].val + xGap;
			rectArray[count].y 				:= editVals[bmpType,CellHeight].val * i + editVals[bmpType,yPos].val + yGap;
			rectArray[count].Width 		:= editVals[bmpType,CellWidth].val;
			rectArray[count].Height 	:= editVals[bmpType,CellHeight].val;
			count += 1;
			xGap += CellGap;
			if count = editVals[bmpType,CellCount].val then exit;
		end;
		xGap:= 0;
		yGap += CellGap;
	end;
end;

procedure DrawRectangleArray(rectArray : rectangleArray);
var
	i : integer;
begin
	for i := 0 to High(rectArray) do
	begin
		DrawRectangleOnScreen(ColorWhite, rectArray[i]);
	end; 
end; 

procedure DrawTestValues(editVals : eVals; currentStat, bmpType : integer);
var 
	i, xPos, yPos, increment: integer;
begin
	xPos := 10;
	yPos := 10;
	increment := 10;
	for i:= 0 to High(editVals[bmpType]) do
	begin
		DrawText(editVals[bmpType,i].Name + ' : ' + IntToStr(editVals[bmpType,i].val), ColorWhite, xPos, yPos);
		yPos += increment;
	end;
	DrawText('Editing Type:' + IntToStr(bmpType), ColorWhite, xPos, yPos + 20);
	DrawText('Currently Editing:', ColorWhite, xPos, yPos + 30);
	DrawText(editVals[bmpType,currentStat].Name + ' : ' + IntToStr(editVals[bmpType,currentStat].val), ColorWhite, xPos, yPos + 40);
end;

procedure DrawMultipleBitmapFiles(destbmp : bitmap; src : array of Bitmap; editVals : array of EditorValues; save: boolean);
var
  i,j, xGap, yGap, count: integer;
begin
  xGap := 0;
  yGap := 0; 
	count := 0;
		
  for i := 0 to editVals[Rows].val-1 do
  begin
    for j := 0 to editVals[Columns].val-1 do
		begin			
			DrawBitmap(src[count], src[count]^.Width * j + editVals[xPos].val + xGap, src[count]^.Height * i + editVals[yPos].val + yGap);
			if save then 
			begin
				MakeOpaque(src[i]);
				DrawBitmap(destbmp, src[count], src[count]^.Width * j, src[count]^.Height * i);
				MakeTransparent(src[i]);
			end;
			count += 1;
			xGap += CellGap;
			if count = Length(src) then exit;
    end;
		xGap:= 0;
		yGap += CellGap;
  end; 		
end; 

procedure DrawSplitBitmapCells(destbmp, src : Bitmap; editVals : array of EditorValues; save: boolean; selectedCell : integer; offSet : Point2D; positions : layout; rectArray : rectangleArray);
var
  i,j, xGap, yGap, count: LongInt;
	pos : Point2D;
begin
  xGap := 0;
  yGap := 0;
  count := 0;
  for i := 0 to editVals[Rows].val-1 do
  begin
    for j := 0 to editVals[Columns].val-1 do
		begin
			if count <> selectedCell then
			begin
				pos.x := rectArray[positions[count]].x;
				pos.y := rectArray[positions[count]].y;
				DrawCell(src, count, pos)
			end else begin
				pos.X := MousePosition().x - offSet.x;
				pos.Y := MousePosition().y - offSet.y;
				DrawCell(src, count, pos);
			end;
			if save then 
			begin
				pos.x := rectArray[positions[count]].x - editVals[xPos].val - xGap;
				pos.y := rectArray[positions[count]].y - editVals[yPos].val - yGap;
				DrawCell(destbmp, src, count, pos);
				Write(pos.x:3:4); Write(' , '); Write(pos.y:3:4); Write(' , '); WriteLn(rectArray[positions[count]].y :3:4); 
				
			end;
			count += 1;
			xGap += CellGap;
    end;
	xGap:= 0;
	yGap += CellGap;
  end; 
end; 

procedure SaveGridToBitmap(destbmp: Bitmap; editVals : array of EditorValues; xGap, yGap, i, j, count: integer);
begin
	if (j = 0) then xGap := 0
	else xGap -= 5;
	if (i = 0) then yGap := 0
	else yGap -= 5;
	
	FillRectangle(destbmp, ColorWhite,(j * editVals[CellWidth].val),(i * editVals[CellHeight].val), editVals[CellWidth].val, editVals[CellHeight].val);
	if (i <> 0) then DrawLine(destbmp,ColorBlack,(j * editVals[CellWidth].val),(i * editVals[CellHeight].val), (j * editVals[CellWidth].val) + editVals[CellWidth].val, (i * editVals[CellHeight].val));
	if (j <> 0) then DrawLine(destbmp,ColorBlack,(j * editVals[CellWidth].val),(i * editVals[CellHeight].val), (j * editVals[CellWidth].val), (i * editVals[CellHeight].val)+ editVals[CellHeight].val);
	DrawText(destbmp, IntToStr(count), ColorBlack,(j * editVals[CellWidth].val)+2,(i * editVals[CellHeight].val)+2);
end;

procedure DrawBitmapGrid(destbmp: bitmap; editVals : array of EditorValues; save: boolean);
var
  i,j, xGap, yGap, count : integer;
begin
  xGap := 0;
  yGap := 0;
  count := 0;
	
  for i := 0 to editVals[Rows].val-1 do
  begin
    for j := 0 to editVals[Columns].val-1 do
		begin
			FillRectangleOnscreen(ColorWhite, editVals[xPos].val + xGap + (j * editVals[CellWidth].val),editVals[yPos].val + yGap + (i * editVals[CellHeight].val), editVals[CellWidth].val, editVals[CellHeight].val);
			DrawText(IntToStr(count), ColorBlack, editVals[xPos].val + xGap + (j * editVals[CellWidth].val),editVals[yPos].val + yGap + (i * editVals[CellHeight].val));
			if save then SaveGridToBitmap(destbmp, editVals, xGap, yGap, i, j, count);
			count+=1;
			xGap += CellGap;
			if count = editVals[CellCount].val then	exit;
    end;
		xGap:= 0;
		yGap += CellGap;
  end;
end;

procedure AlterValues(out rectArray : rectangleArray; var editVals : eVals; intArray : array of integer; var src, original : Bitmap; currentStat, bmpType, multiplier : integer);
begin
	if (currentStat < xPos) OR
		 ((currentStat = Columns) AND (((editVals[bmpType,Columns].val + multiplier) * editVals[bmpType,Rows].val)  < (editVals[bmpType,CellCount].val + editVals[bmpType,Rows].val))) OR
		 ((currentStat = Rows) AND (((editVals[bmpType,Rows].val + multiplier) * editVals[bmpType,Columns].val)  < (editVals[bmpType,CellCount].val + editVals[bmpType,Columns].val))) then
	begin
		editVals[bmpType,currentStat].val += intArray[currentStat] * multiplier;
	end else if ((currentStat = Scale) AND (editVals[bmpType,Scale].val + multiplier <> 0)) then
	begin
		editVals[bmpType,currentStat].val += intArray[currentStat] * multiplier;
		if bmpType = 1 then 
		begin
			src := RotateScaleBitmap(original, 0, editVals[bmpType,currentStat].val);
			editVals[bmpType,CellWidth].val := editVals[bmpType,CellWidth].val + (editVals[bmpType,CellWidth].val DIV (editVals[bmpType,Scale].val - multiplier) * multiplier);
			editVals[bmpType,CellHeight].val := editVals[bmpType,CellHeight].val + (editVals[bmpType,CellHeight].val DIV (editVals[bmpType,Scale].val - multiplier) * multiplier);
		end;
	end;
	if (currentStat <> xPos) OR (currentStat <> yPos) then
		BitmapSetCellDetails(src, editVals[bmpType,CellWidth].val, editVals[bmpType,CellHeight].val, editVals[bmpType,Columns].val, editVals[bmpType,Rows].val, editVals[bmpType,CellCount].val);
	ResetRectangles(rectArray, editVals, bmpType);
end;

procedure DragRectangle(rectArray : rectangleArray;var selectedCell: integer; var offSet : Point2D;var pos : layout);
var
	i, j, tempPos : integer;
begin
	for i := 0 to High(rectArray) do
	begin
		if PointInRect(MousePosition(),rectArray[i]) AND (selectedCell > High(rectArray)) then
		begin
			for j := 0 to High(pos) do
			begin
				if (i = pos[j]) then selectedCell := j;
			end;
			offSet.x := MousePosition().x -rectArray[i].x;
			offSet.y := MousePosition().y -rectArray[i].y;
		end else
		if PointInRect(MousePosition(),rectArray[i]) AND (selectedCell < Length(rectArray)) then
		begin
			tempPos := pos[selectedCell];
			
			for j := 0 to High(pos) do
			begin
				if (i = pos[j]) then pos[j] := tempPos;
			end;
			pos[selectedCell] := i;
			selectedCell := Length(rectArray);
		end;
	end;
end;

procedure Main();
var
	editVals : eVals;
	currentStat, bmpType, i, H, W, selectedCell: integer;
	save: boolean;
	positions : layout;
	destbmp, src, original : Bitmap;
	bmpArray : Array of Bitmap;
	intArray : Array of Integer;
	rectArray : rectangleArray;
	offSet : Point2D;
begin
  OpenAudio();
  OpenGraphicsWindow('Hello World', ScreenWidth, ScreenHeight);
	
	currentStat := 0;
	bmpType := 0;
	save := false;
	original := LoadBitmap('corpolupo1.png');
	
	SetLength(editVals,3, 8);	
	SetLength(bmpArray,4);
	SetLength(intArray,Length(editVals[0]));
	
	InitializeTestValues(editVals);
	IncrementValues(intArray);
	ResetRectangles(rectArray, editVals, bmpType);
	InitializePositions(positions, rectArray);
	SetTransparentColor(original, RGBColor(255,255,255));
	
	src := original;
	selectedCell := editVals[bmpType,CellCount].val;
	
	for i := 0 to High(bmpArray) do
	begin
		bmpArray[i] := LoadBitmap(IntToStr(i) + '.gif');
	end;
	
  repeat // The game loop...
    ProcessEvents();
    ClearScreen(ColorRed);
		
		if (KeyTyped(vk_1)) then
		begin
			if (bmpType <> 2) then
			begin
				w:=editVals[bmpType,Columns].val * editVals[bmpType,CellWidth].val;
				h:=editVals[bmpType,Rows].val * editVals[bmpType,CellHeight].val;
			end else begin
				w := bmpArray[CellCount]^.Width * editVals[bmpType,Columns].val;
				h := bmpArray[CellCount]^.Height * editVals[bmpType,Rows].val;
			end;
			destbmp := CreateBitmap(w,h );
			save := true;
		end;
		
		if (KeyTyped(vk_TAB) AND (currentStat < High(editVals[bmpType]))) then currentStat += 1
		else if (KeyTyped(vk_TAB) AND (currentStat = High(editVals[bmpType]))) then currentStat := 0;
		
		if (KeyTyped(vk_CAPSLOCK) AND (bmpType < High(editVals))) then bmpType += 1
		else if (KeyTyped(vk_CAPSLOCK) AND (bmpType = High(editVals))) then bmpType := 0;
		
		if (KeyTyped(vk_MINUS) AND (editVals[bmpType,currentStat].val > 0)) then AlterValues(rectArray, editVals, intArray, src, original, currentStat, bmpType, - 1);
				
		if (KeyTyped(vk_EQUALS)) then AlterValues(rectArray, editVals, intArray, src, original, currentStat, bmpType, 1);
		
		if KeyTyped(vk_ESCAPE) then selectedCell := editVals[bmpType,CellCount].val;
		
		
		DrawTestValues(editVals, currentStat,bmpType);
		
		case bmpType of
			0: DrawBitmapGrid(destbmp,editVals[CellCount],save);
			1: begin
					 if save then MakeOpaque(src);
					 DrawSplitBitmapCells(destbmp, src, editVals[CellWidth], save, selectedCell, offset, positions, rectArray);
					 if save then MakeTransparent(src);
				 end;
			2: DrawMultipleBitmapFiles(destbmp,bmpArray,editVals[CellHeight],save);
		end;
		
		if save then
		begin
			SaveBitmap(destbmp,'D:\destbmp.bmp');
			FreeBitmap(destbmp);
			save := false;
		end;
		
		DrawRectangleArray(rectArray);
		if MouseClicked(LeftButton) then DragRectangle(rectArray, selectedCell, offSet, positions);
		
		RefreshScreen(60);
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
	Main();
end.