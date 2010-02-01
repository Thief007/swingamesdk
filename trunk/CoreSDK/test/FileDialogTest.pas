program FileDialogTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils;


type
  
  FileDialogData = record
    dialogPanel:  Panel;      // The panel used to show the dialog
    currentPath:  String;
    cancelled:    Boolean;
    lastSelectedFile, lastSelectedPath: LongInt; // These represent the index if the selected file/path to enable double-like clicking
  end;
  
var
  dialog: FileDialogData;

procedure SetFileDialogPath(path: String); forward;

procedure InitialiseFileDialog();
begin
  with dialog do
  begin
    lastSelectedFile := -1;
    lastSelectedPath := -1;
    
    cancelled := false;
    if length(currentPath) = 0 then 
      SetFileDialogPath(GetUserDir());
  end;
end;

procedure ShowOpenDialog();
begin
  InitialiseFileDialog();
  
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OpenLabel', 'Open');
    
    ShowPanel(dialogPanel);
  end;
end;

procedure ShowSaveDialog();
begin
  InitialiseFileDialog();
  
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OpenLabel', 'Save');
    
    ShowPanel(dialogPanel);
  end;
end;

procedure SetFileDialogPath(path: String);
var
  tmpPath: String;
  pathList, filesList: GUIList;
  paths: Array [0..255] of PChar;
  i, len: Integer;
  info : TSearchRec;
begin
  if not (FileExists(path) or DirectoryExists(path)) then exit;
  
  // expand the relative path to absolute
  path := ExpandFileName(path);
  
  with dialog do
  begin
    currentPath := IncludeTrailingPathDelimiter(path);
    lastSelectedPath := -1;
    lastSelectedFile := -1;
  end;
  
  pathList  := ListFromRegion(RegionWithId(dialog.dialogPanel, 'PathList'));
  filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
  
  // Set the details in the path list
  tmpPath := ExcludeTrailingPathDelimiter(path);
  len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
  
  // Clear the list of all of its items
  ListClearItems(pathList);
  
  // Loop through the directories adding them to the list
  for i := 0 to len - 1 do
  begin
    WriteLn(paths[i]);
    ListAddItem(pathList, paths[i]);
  end;
  
  // Clear the files in the filesList
  ListClearItems(filesList);
  
  //Loop through the directories
  WriteLn(dialog.currentPath + '*');
  if FindFirst (dialog.currentPath + '*', faAnyFile and faDirectory, info)=0 then
  begin
    repeat
      with Info do
      begin
        // if (attr and faDirectory) = faDirectory then
        //   Write('Dir : ');
        // Writeln (Name:40,Size:15);
        ListAddItem(filesList, name);
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(info);
end;

procedure UpdateFileDialog();
var
  clicked: Region;
  selectedText, selectedPath: String;
  selectedIdx: Integer;
begin
  if PanelClicked() = dialog.dialogPanel then
  begin
    clicked := RegionClicked();
    
    if clicked = RegionWithID(dialog.dialogPanel, 'FilesList') then
    begin
      selectedIdx := ListActiveItemIndex(clicked);
      
      if (dialog.lastSelectedFile = selectedIdx) and (selectedIdx > 0) and (selectedIdx < ListItemCount(clicked)) then
      begin
        selectedText := ListItemText(clicked, selectedIdx);
        selectedPath := IncludeTrailingPathDelimiter(dialog.currentPath) + selectedText;
        
        if DirectoryExists(selectedPath) then
          SetFileDialogPath(selectedPath);
      end;
      
      dialog.lastSelectedFile := selectedIdx;
    end;
  end;
end;

procedure InitInterface();
begin
  GUISetForegroundColor(ColorWhite);
  GUISetBackgroundColor(ColorGrey);
  
  LoadResourceBundle('FileDialog.txt');
  
  with dialog do
  begin
    dialogPanel := LoadPanel('FileDialog.txt');
    cancelled   := false;
    currentPath := '';
    
    AddPanelToGUI(dialogPanel);
    HidePanel(dialogPanel);
  end;
  
  DrawGUIAsVectors(true);
end;



// procedure UpdateGUI(var pnla, pnlb: panel; lst: GUilist);
// var
//   reg: Region;
//   parpnl: Panel;
//   radGroup: GUIRadioGroup;
// begin
//   if (RegionClickedID() = 'Button1') And (CheckboxState(RegionWithID(pnla, 'Checkbox2'))) then
//   begin
//     ToggleShowPanel(pnlb);
//     Toggleactivatepanel(pnlb);
//   end;
//   
//   LabelSetText(LabelFromRegion(RegionWithID('Label1')), TextboxText(TextBoxFromRegion(RegionWithID('TextBox1'))));
//   
//    case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('radButton1'))) of
//      0: GUISetForegroundColor(ColorGreen);
//      1: GUISetForegroundColor(ColorRed);
//      2: GUISetForegroundColor(ColorBlue);
//    end;  
// end;



procedure Main();
var
  lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('File Dialog Test', 800, 600);
  
  InitInterface();
  
  // lst := ListFromRegion(regionWithID('List1'));
  // 
  // ListAddItem(lst, 'Hat');
  // ListAddItem(lst, 'Sword');
  // ListAddItem(lst, 'Cape');
  // ListAddItem(lst, 'Cheese');
  // ListAddItem(lst, 'Cake');
  // ListAddItem(lst, 'Mouse');
  // ListAddItem(lst, 'Dog');
  // ListAddItem(lst, 'Axe');
  // ListAddItem(lst, 'Mace');
  // ListAddItem(lst, 'Chainmail');
  // ListAddItem(lst, 'Ninja');
  // ListAddItem(lst, 'Newspaper');
  // ListAddItem(lst, 'Car');  
  // ListAddItem(lst, 'Hat2');
  // ListAddItem(lst, 'Sword2');
  // ListAddItem(lst, 'Cape2');
  // ListAddItem(lst, 'Cheese2');
  // ListAddItem(lst, 'Cake2');
  // ListAddItem(lst, 'Mouse2');
  // ListAddItem(lst, 'Dog2');
  // ListAddItem(lst, 'Axe2');
  // ListAddItem(lst, 'Mace2');
  // ListAddItem(lst, 'Chainmail2');
  // ListAddItem(lst, 'Ninja2');
  // ListAddItem(lst, 'Newspaper2');
  // ListAddItem(lst, 'Car2');
  
  
  ShowSaveDialog();
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    // UpdateGUI(pnla, pnlb, lst);
    UpdateInterface();
    UpdateFileDialog();
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
