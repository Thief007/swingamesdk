program BitmapAnimationEditor;

uses
  crt, sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgGeometry, sgImages, sgInput, 
  SysUtils, sgUserInterface, sgAnimations, 
	EditorShared, BitmapEditor, AnimationEditor;
	
{procedure AddCellToAnimation(var aniCellGrp, cellGrp : cellGroup);
var
	i, oldLength, xGap, selectIncr: LongInt;
begin
	oldLength := Length(aniCellGrp.cells);
	selectIncr := 0;
	xGap := aniCellGrp.cells[High(aniCellGrp.cells) - 1].xGap + CellGap;
	SetLength(aniCellGrp.cells, Length(cellGrp.selectedOrder) + oldLength);
	aniCellGrp.cells[High(aniCellGrp.cells)] := aniCellGrp.cells[oldLength -1];
	
	for i := oldLength -2 to High(aniCellGrp.cells) -2 do
	begin	
		InitializeAniCell(aniCellGrp, i, cellGrp.selectedOrder[selectIncr], cellGrp.cells[cellGrp.selectedOrder[selectIncr]].bmpIdx, cellGrp.cells[cellGrp.selectedOrder[selectIncr]].cellIdx, xGap);
		selectIncr += 1;
	end;
	InitializeAniCell(aniCellGrp, High(aniCellGrp.cells) -1 , -1 , -1 , -1, xGap);
	InitializeAniCell(aniCellGrp, High(aniCellGrp.cells), aniCellGrp.cells[High(aniCellGrp.cells)].idx, aniCellGrp.cells[High(aniCellGrp.cells)].bmpIdx,
										aniCellGrp.cells[High(aniCellGrp.cells)].cellIdx, xGap);	
end;

procedure AddCellToAnimation(var aniCellGrp: cellGroup; dCell: DraggedCell; i: LongInt); overload;
var
	oldLength, xGap, selectIncr: LongInt;
begin	
	InitializeAniCell(aniCellGrp, i, dCell.Index, dCell.Bmp, dCell.Cell, aniCellGrp.cells[i].xGap);
	
	if (i = High(aniCellGrp.cells) -1) then
	begin
		SetLength(aniCellGrp.cells, Length(aniCellGrp.cells) + 1);
		xGap := aniCellGrp.cells[High(aniCellGrp.cells)-1].xGap + CellGap;
		InitializeAniCell(aniCellGrp, High(aniCellGrp.cells), aniCellGrp.cells[High(aniCellGrp.cells)-1].idx, aniCellGrp.cells[High(aniCellGrp.cells)-1].bmpIdx,
									aniCellGrp.cells[High(aniCellGrp.cells)-1].cellIdx, xGap);
		InitializeAniCell(aniCellGrp, High(aniCellGrp.cells) -1 , -1 , -1 , -1, aniCellGrp.cells[High(aniCellGrp.cells)-1].xGap);
	end;
end;

}

{
function OutputSingleFrameAnimation(aniCellGrp: cellGroup): string;
begin
	result := 'f:'  + '0,' + IntToStr(aniCellGrp.cells[Low(aniCellGrp.cells)].idx) + ',5,';
	if (aniCellGrp.cells[High(aniCellGrp.cells)].idx <> -1) then result += IntToStr(aniCellGrp.cells[Low(aniCellGrp.cells)].idx)
end;

function OutputMultiFrameAnimation(aniCellGrp: cellGroup): string;
var
	i, lastVal : LongInt;
begin
	result := 'm:' + '[' + IntToStr(Low(aniCellGrp.cells)) + '-' + IntToStr(High(aniCellGrp.cells)- 2) + '],[' + IntToStr(aniCellGrp.cells[Low(aniCellGrp.cells)].idx);
	lastVal := Low(aniCellGrp.cells);
	for i := Low(aniCellGrp.cells) + 1 to High(aniCellGrp.cells) - 2 do
	begin
		if (aniCellGrp.cells[i - 1].idx  <> aniCellGrp.cells[i].idx -1) then
		begin
			if lastVal <> aniCellGrp.cells[i - 1].idx then result += '-' + IntToStr(aniCellGrp.cells[i - 1].idx) + ',' + IntToStr(aniCellGrp.cells[i].idx)
			else
				result += ',' + IntToStr(aniCellGrp.cells[i].idx);
			lastVal := aniCellGrp.cells[i].idx;
		end;
	end;
	if lastVal <> aniCellGrp.cells[High(aniCellGrp.cells) - 2].idx then result += '-' + IntToStr(aniCellGrp.cells[High(aniCellGrp.cells)-2].idx);
	result += ']' + ',' + '5' + ',';
	if aniCellGrp.cells[High(aniCellGrp.cells)].idx <> -1 then result +=  IntToStr(aniCellGrp.cells[High(aniCellGrp.cells)].idx);
end;

procedure ExportAnimationFile(aniCellGrp : cellGroup; name : string);
var
	i : LongInt;
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
	if Length(aniCellGrp.cells) = 3 then
	begin
		SetLength(arrayOfSingleFrame, Length(arrayOfSingleFrame) + 1);
		arrayOfSingleFrame[High(arrayOfSingleFrame)] := OutputSingleFrameAnimation(aniCellGrp);
	end else if Length(aniCellGrp.cells) > 3 then
	begin
		SetLength(arrayOfMultiFrame, Length(arrayOfMultiFrame) + 1);
		arrayOfMultiFrame[High(arrayOfMultiFrame)] := OutputMultiFrameAnimation(aniCellGrp);
	end;
	for i := Low(arrayOfSingleFrame) to High(arrayOfSingleFrame) do WriteLn(txt, arrayOfSingleFrame[i]);
	WriteLn(txt,'');
	WriteLn(txt, '//Multi-frame: ranges are in[]');
	WriteLn(txt, '//[a-b] = numbers from a to b inclusive');
	WriteLn(txt, '//[a,b,c] = explicit values');
	WriteLn(txt, '//[a-b,c] = combination');

	for i := Low(arrayOfMultiFrame) to High(arrayOfMultiFrame) do WriteLn(txt, arrayOfMultiFrame[i]);

	Close(txt);
end; 
}
procedure InitializeFilePanel(out p: Panel);
begin
	p := LoadPanel('ToolBarPanel.txt');
	
	ShowPanel(p);
	ActivatePanel(p); 
	AddPanelToGUI(p);
	
	DrawGUIAsVectors(true);	
	GUISetBackGroundColor(RGBAColor(0,0,0,0));
	GUISetForeGRoundColor(ColorWhite);
end;

procedure ShowBitmapPanels(panels: PanelArray);
var
	i: integer;
begin
	for i := Low(panels) to High(panels) do
	begin
		ShowPanel(panels[i]);
	end;
	HidePanel(panels[CellBitmapNames]);
end;

procedure ShowAnimationPanels(panels: PanelArray);
var
	i: integer;
begin	
	for i := Low(panels) to High(panels) do
	begin
		ShowPanel(panels[i]);
	end;
	HidePanel(panels[AniCellBMPNames]);
end;

procedure HidePanels(panels: PanelArray);
var
	i: integer;
begin
	for i := Low(panels) to High(panels) do
	begin
		HidePanel(panels[i]);
	end;
end;

procedure ChangePanelMode(BitmapPanels, AnimationPanels: PanelArray; var prevMode :integer; currentMode: Integer);
var
	i: integer;
begin
	case currentMode of
		BitmapEditorID 		: ShowBitmapPanels(BitmapPanels);
		AnimationEditorID : ShowAnimationPanels(AnimationPanels);
		//	CharacterEditorID : ShowBitmapPanels(panels[i]);
	end;
	
	case prevMode of
		BitmapEditorID 		:  HidePanels(BitmapPanels);
		AnimationEditorID :  HidePanels(AnimationPanels);
		//	CharacterEditorID : ShowBitmapPanels(panels[i]);
	end;
	prevMode := currentMode;
end;

procedure Main();
var
	prevMode: LongInt; 
	p : Panel;
	SharedData : EditorValues;
	BitmapMode : BitmapEditorValues;
	AniMode		 : AnimationEditorValues;
//	CharMode	 : CharacterEditorValues;
begin
  OpenAudio();
  OpenGraphicsWindow('Bitmap | Animation | Character Editor', ScreenWidth, ScreenHeight);
  LoadResourceBundle('MainMenu.txt');			
	
	InitializeFilePanel(p);
	InitializeBitmapEditor(BitmapMode, SharedData.BMPArray);
	InitializeAnimationEditor(AniMode);
	
	AddBitmapToArray(SharedData.BMPArray, 'Corpo', 'corpolupo1.png', BitmapMode.cellGrp, ListFromRegion(RegionWithID('BMPList')), ListFromRegion(RegionWithID('AniBMPList')));
	AddBitmapToArray(SharedData.BMPArray, 'Black', 'blackbody.png', BitmapMode.cellGrp, ListFromRegion(RegionWithID('BMPList')), ListFromRegion(RegionWithID('AniBMPList')));
	
	prevMode := 0;
	
//	InitializeAniCellGroup(aniCellGrp, cellGrp);
//	save 		:= false;
//	destBMP := CreateBitmap(1, 1);
//	arrow 	:= LoadBitmap('arrow.png');
//	aniTemp := LoadAnimationTemplate('Test.txt');
//  ani 		:= CreateAnimation('Up',aniTemp,FALSE);

  repeat // The game loop...
    ProcessEvents();

		case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))) of 
			0: UpdateBitmapEditor(BitmapMode, SharedData);//(cellGrp, bmpArray, destbmp, mouseOffSet, dragGroup, dCell, panelArray[BitmapEditorID]);
			1: UpdateAnimationEditor(AniMode, SharedData);//(sc, bmpArray, workingBMP, panelArray[AnimationEditorID]);
		end;
		
		DrawPanels();
		
		if (prevMode <> ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap')))) then
		begin
			ChangePanelMode(BitmapMode.panels, AniMode.panels, prevMode, ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))));
		end;
	  
		
		//		HandleSelection(aniCellGrp, mouseOffSet);
	//			HandleCellDragging(aniCellGrp, dCell, mouseOffSet);
{
		DrawSplitBitmapCells(destbmp, bmpArray, cellGrp, save, mouseOffSet, dCell);	
		UpdateAnimation(ani);
		DrawAnimation(ani, destBMP, ScreenWidth - 50 - cellGrp.grpData.Width, (ScreenHeight DIV 2) - (cellGrp.grpData.Height DIV 2));
		DrawAnimationGroup(aniCellGrp, cellGrp, dCell, bmpArray, mouseOffSet);
		DrawBitmap(arrow, aniCellGrp.cells[High(aniCellGrp.cells)-1].Area.X + CellGap + aniCellGrp.grpData[aeWidth], aniCellGrp.grpData[aeYPos] - (ArrowHeight DIV 2) + aniCellGrp.grpData[aeHeight] DIV 2);
		
		if save then
		begin
			SaveBitmap(destbmp,'D:\destbmp.bmp');
			SetBitmapCellDetails(destBMP, cellGrp.grpData.Width, cellGrp.grpData.Height, cellGrp.Columns, cellGrp.Rows, cellGrp.CellCount);
			save := false;
		end;
		
		if KeyTyped(vk_3) then ExportAnimationFile(aniCellGrp,'testAni');}
		UpdateInterface();
		RefreshScreen(60);   
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio(); 
end;

begin
	Main();
end.