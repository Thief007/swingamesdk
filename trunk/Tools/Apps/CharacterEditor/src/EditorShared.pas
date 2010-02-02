unit EditorShared;

//=============================================================================
interface
uses sgTypes, sgUserInterface;

type
	GridType = (BitmapGrid, AnimationGrid);

	LoadedBitmap = record
		original, src : Bitmap;
	end;
	
	DraggedCell = record
		index, bmp, cell :LongInt;
		GroupType 			 :GridType;
	end;	
	
	LoadedBitmaps = array of LoadedBitmap;
	PanelArray = Array of Panel;
	
	AreaValues = record
		x, y, width, height : LongInt;
	end;
	
	CellArea = record
		area: AreaValues;
		xGap, yGap, idx, bmpIdx, cellIdx : Integer;
		isSelected: Boolean;
	end; 
	
	CellGroup = record
		grpData: AreaValues;
		cells : Array of CellArea;
		rows, cols, cellCount: Integer;
		selectedOrder : Array of LongInt;
		GroupType : GridType;
	end;
		
	BitmapEditorValues = record
		cellGrp : CellGroup;
		scale : Integer;
		dCell : DraggedCell;
		panels : PanelArray;
		destBMP : Bitmap;
		dragGroup : Boolean;
		bg : Bitmap;
	end;

	AnimationEditorValues = record
		cellGrp : CellGroup;
		draggedCell : Integer;
		panels : PanelArray;
		workingBMP : Bitmap;
		bmpidx : Integer;
		bg : Bitmap;
	end;
	
	EditorValues = record
		BMPArray : LoadedBitmaps;
		mouseOffSet : Point2D;		
	end;
	
	procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string; cellGrp : CellGroup; lst1, lst2: GUIList);
	procedure AddSelection(var cellGrp : cellGroup; i : LongInt);
	procedure RemoveSelection(var cellGrp : cellGroup; i : LongInt);
	procedure DeselectAll(var cellGrp : cellGroup);
	procedure SelectMultiple(var cellGrp : cellGroup; i : LongInt);
	procedure DrawCellGroup(cellGrp : CellGroup; i: LongInt);
	procedure DrawOnMouse(cellGrp: cellGroup; bmpArray: LoadedBitmaps; dCell: DraggedCell; mouseOffSet : Point2D);
	procedure SaveBitmap(destbmp: Bitmap; cellGrp: CellGroup; bmpArray : LoadedBitmaps);
	procedure DrawSplitBitmapCells(destbmp: Bitmap; bmpArray: LoadedBitmaps; cellGrp : CellGroup; save: boolean; mouseOffSet: Point2D; dCell: DraggedCell);
	procedure HandleSelection(var cellGrp : CellGroup; out mouseOffSet : Point2D; dCell: DraggedCell);
	procedure HandleCellDragging(var cellGrp : CellGroup;var dCell: DraggedCell; out mouseOffSet : Point2D);
	
const
	CellGap 								= 5;
	ScreenWidth 						= 800;
	ScreenHeight						= 600;
	LengthCellData 					= 6;
	LengthgrpData 					= 10;
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
	uses sgImages, sgCore, sgInput, sgText, sgGeometry, sgGraphics, SysUtils;

	//---------------------------------------------------------------------------
  // Bitmap Loading
  //--------------------------------------------------------------------------- 
	
	procedure AddBitmapToArray(var bmpArray : LoadedBitmaps; id, fileName : string; cellGrp : CellGroup; lst1, lst2: GUIList);
	begin
		SetLength(bmpArray, Length(bmpArray)+1);
		
		bmpArray[High(bmpArray)].original := MapBitmap(id, fileName);	
		SetTransparentColor(bmpArray[High(bmpArray)].original, RGBColor(Red,Green,Blue));
		
		bmpArray[High(bmpArray)].src := bmpArray[High(bmpArray)].original;
		with cellGrp do
			SetBitmapCellDetails(bmpArray[High(bmpArray)].src, grpData.Width, grpData.Height, cols, rows, cellCount);  
		ListAddItem(lst1, bmpArray[High(bmpArray)].src^.name);
		ListAddItem(lst2, bmpArray[High(bmpArray)].src^.name);
	end;

	//---------------------------------------------------------------------------
  // Selecting
  //--------------------------------------------------------------------------- 	
	
	procedure AddSelection(var cellGrp : cellGroup; i : LongInt);
	begin
		SetLength(cellGrp.selectedOrder, Length(cellGrp.selectedOrder) + 1);
		
		cellGrp.selectedOrder[High(cellGrp.selectedOrder)] := i;
		cellGrp.cells[i].isSelected := true;
	end;

	procedure RemoveSelection(var cellGrp : cellGroup; i : LongInt);
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

	procedure DeselectAll(var cellGrp : cellGroup);
	var 
		i: LongInt;
	begin
		SetLength(cellGrp.selectedOrder, 0);
		
		for i := Low(cellGrp.cells) to High(cellGrp.cells) do cellGrp.cells[i].isSelected := false;
	end;

	procedure SelectMultiple(var cellGrp : cellGroup; i : LongInt);
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
	
	//---------------------------------------------------------------------------
  // Drawing
  //--------------------------------------------------------------------------- 

	procedure DrawCellGroup(cellGrp : CellGroup; i: LongInt);
	begin
		with cellGrp do
		begin
			if cells[i].isSelected then
				FillRectangleOnScreen(RGBAColor(255,0,0,150), cells[i].area.X, cells[i].area.Y, grpData.Width, grpData.Height)
			else
				DrawRectangleOnScreen(ColorWhite, cells[i].area.X, cells[i].area.Y, grpData.Width, grpData.Height)
		end;
	end; 

	procedure DrawOnMouse(cellGrp: cellGroup; bmpArray: LoadedBitmaps; dCell: DraggedCell; mouseOffSet : Point2D);
	var
		dragPos: Point2D;
	begin
		dragPos.x := MousePosition().x - mouseOffSet.x;
		dragPos.y := MousePosition().y - mouseOffSet.y;
		if (dCell.bmp <> -1) then DrawCell(bmpArray[cellGrp.cells[dCell.Index].bmpIdx].src, cellGrp.cells[dCell.Index].cellIdx, dragPos);
	end;

	procedure DrawBitmapGrid(destbmp: bitmap; save: boolean; cellGrp : cellGroup; i : LongInt);
	var
		xPos, yPos : LongInt;
	begin
		with cellGrp do
		begin
			FillRectangleOnscreen(ColorWhite, cells[i].Area.X, cells[i].Area.Y, grpData.Width, grpData.Height);
			DrawText(IntToStr(i), ColorBlack, cells[i].Area.X, cells[i].Area.Y);
			if save then 
			begin
				xPos := cells[i].Area.X - grpData.X - cells[i].xGap;
				yPos := cells[i].Area.Y - grpData.Y - cells[i].yGap;
				
				DrawRectangle(destbmp, ColorWhite, xPos, yPos, grpData.Width -1, grpData.Height -1);
				DrawText(destbmp, IntToStr(i), ColorYellow, xPos, yPos);		
			end; 	
		end;
	end;

	procedure SaveBitmap(destbmp: Bitmap; cellGrp: CellGroup; bmpArray : LoadedBitmaps);
	var
		xPos, yPos, i: Integer;
	begin
		for i := Low(bmpArray) to High(bmpArray) do MakeOpaque(bmpArray[i].src);
		
		with cellGrp do
		begin		
			for i := Low(cells) to High(cells) do
			begin
				xPos := cells[i].Area.X - cells[i].xGap - grpData.X;
				yPos := cells[i].Area.Y - cells[i].yGap - grpData.Y;
				DrawCell(destbmp, bmpArray[cells[i].bmpIdx].src, cells[i].cellIdx, xPos, yPos);
			end;
		end;
								
		for i := Low(bmpArray) to High(bmpArray) do MakeTransparent(bmpArray[i].src);
	end;
	
	procedure DrawSplitBitmapCells(destbmp: Bitmap; bmpArray: LoadedBitmaps; cellGrp : CellGroup; save: boolean; mouseOffSet: Point2D; dCell: DraggedCell);
	var
		i: LongInt;
	begin
		with cellGrp do
		begin
			if (cols = 0) OR (cellCount = 0) then exit;
			
			for i := Low(cellGrp.cells) to High(cellGrp.cells) do
			begin
				if cells[i].bmpIdx = -1 then 
				begin
					DrawBitmapGrid(destbmp,save,cellGrp,i)
				end else begin
					if i <> dCell.index then begin
						DrawCell(bmpArray[cells[i].bmpIdx].src, cells[i].cellIdx, cells[i].Area.X, cells[i].Area.Y);
					end;
				end;				
				DrawCellGroup(cellGrp, i);
			end;
			
			if (dCell.Index <> -1) AND (dCell.GroupType <> BitmapGrid) then DrawOnMouse(cellGrp, bmpArray, dCell, mouseOffSet);
		end;
	end;

	procedure HandleSelection(var cellGrp : CellGroup; out mouseOffSet : Point2D; dCell: DraggedCell);
	var
		i: LongInt;
	begin
		if dCell.index <> -1 then exit;
		with cellGrp do
		begin
			for i := Low(cells) to High(cells) do
			begin
				if PointInRect(MousePosition(), cells[i].Area.X, cells[i].Area.Y, grpData.Width, grpData.Height) then
				begin
					if KeyDown(VK_LCTRL) AND cells[i].isSelected then RemoveSelection(cellGrp, i)
					else if KeyDown(VK_LCTRL) then AddSelection(cellGrp, i)
					else if KeyDown(VK_LShift) then SelectMultiple(cellGrp, i);
				end;
			end;
		end;
	end;
	
	procedure HandleCellDragging(var cellGrp : CellGroup;var dCell: DraggedCell; out mouseOffSet : Point2D);
	var
		i, temp : LongInt;
		tempCell : DraggedCell;
	begin
		if Length(cellGrp.SelectedOrder) > 0 then exit;
		with cellGrp do
		begin
			for i := Low(cells) to High(cells) do
			begin
				if KeyDown(VK_LALT) AND PointInRect(MousePosition(), cells[i].Area.X, cells[i].Area.Y, grpData.Height, grpData.Width) AND 
					(dCell.index = -1) AND (cells[i].bmpIdx <> -1) then
				begin
					dCell.index  		:= i;
					dCell.cell 	 		:= cells[i].cellIdx;
					dCell.Bmp 			:= cells[i].bmpIdx;
					
					mouseOffSet.x 	:= MousePosition().x - cells[i].Area.X;
					mouseOffSet.y 	:= MousePosition().y - cells[i].Area.Y;
				end else
				if PointInRect(MousePosition(), cells[i].Area.X, cells[i].Area.Y, grpData.Height, grpData.Width) AND (dCell.index <> -1) then
				begin
					temp												 := cells[dCell.Index].idx;
					cells[dCell.index].cellIdx 	 := cells[i].cellIdx;
					cells[dCell.index].bmpIdx 	 := cells[i].bmpIdx;					
					cells[dCell.Index].idx 			 := cells[i].idx;
					cells[i].idx    						 := temp;
					cells[i].cellIdx 						 := dCell.Cell;
					cells[i].bmpIdx 						 := dCell.Bmp;
					dCell.index  								 := -1;
				end; 
			end;
		end;
	end;
end.