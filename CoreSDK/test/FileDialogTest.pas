program FileDialogTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  Math, SysUtils,
  MyStrUtils,
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes;


type
  
  FileDialogData = record
    dialogPanel:          Panel;      // The panel used to show the dialog
    currentPath:          String;     // The path to the current directory being shown
    currentSelectedPath:  String;     // The path to the file selected by the user
    cancelled:            Boolean;
    allowNew:             Boolean;
    lastSelectedFile, lastSelectedPath: LongInt; // These represent the index if the selected file/path to enable double-like clicking
  end;
  
var
  dialog: FileDialogData;

procedure SetFileDialogPath(fullname: String); forward;

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
    LabelSetText(dialogPanel, 'OkLabel', 'Open');
    allowNew := false;
    ShowPanel(dialogPanel);
  end;
end;

procedure ShowSaveDialog();
begin
  InitialiseFileDialog();
  
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OkLabel', 'Save');
    allowNew := true;
    ShowPanel(dialogPanel);
  end;
end;

function DialogComplete(): Boolean;
begin
  result := not (PanelVisible(dialog.dialogPanel) or dialog.cancelled);
end;

function DialogCancelled(): Boolean;
begin
  result := dialog.cancelled;
end;

function DialogPath(): String;
begin
  if dialog.cancelled then result := ''
  else result := dialog.currentSelectedPath;
end;

procedure UpdateDialogPathText();
var
  selectedPath: String;
  pathTxt: Region;
begin
  selectedPath  := dialog.currentSelectedPath;
  
  pathTxt := RegionWithId(dialog.dialogPanel,'PathTextbox');
  
  // WriteLn('tw: ', TextWidth(TextboxFont(pathTxt), selectedPath));
  // WriteLn('rw: ', RegionWidth(pathTxt));
  if TextWidth(TextboxFont(pathTxt), selectedPath) > RegionWidth(pathTxt) then TextboxSetAlignment(pathTxt, AlignRight)
  else TextboxSetAlignment(pathTxt, AlignLeft);
  
  TextboxSetText(pathTxt, selectedPath);
  LabelSetText(RegionWithId(dialog.dialogPanel, 'PathLabel'), selectedPath);
end;

procedure SetFileDialogPath(fullname: String);
var
  tmpPath, path, filename: String;
  pathList, filesList: GUIList;
  paths: Array [0..255] of PChar;
  i, len: Integer;
  info : TSearchRec;
  
  procedure _SelectFileInList();
  var
    fileIdx: Integer;
  begin
    if Length(filename) > 0 then
    begin
      fileIdx := ListTextIndex(filesList, filename);
      ListSetActiveItemIndex(filesList, fileIdx);
      ListSetStartAt(filesList, fileIdx);
    end;
  end;
begin
  // expand the relative path to absolute
  if not ExtractFileAndPath(fullname, path, filename, dialog.allowNew) then exit;
  
  // WriteLn('path is ', path);
  
  // Get the path without the ending delimiter (if one exists...)
  tmpPath := ExcludeTrailingPathDelimiter(path);
  
  with dialog do
  begin
    currentPath         := IncludeTrailingPathDelimiter(tmpPath);
    currentSelectedPath := fullname;
    lastSelectedPath    := -1;
    lastSelectedFile    := -1;
  end;
  
  UpdateDialogPathText();
  
  pathList  := ListFromRegion(RegionWithId(dialog.dialogPanel, 'PathList'));
  filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
  
  // Set the details in the path list
  len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
  
  // Clear the list of all of its items
  ListClearItems(pathList);
  
  // Add the drive letter at the start
  if Length(ExtractFileDrive(path)) = 0 then
    ListAddItem(pathList, PathDelim)
  else
    ListAddItem(pathList, ExtractFileDrive(path));
  
  // Loop through the directories adding them to the list
  for i := 0 to len - 1 do
  begin
    ListAddItem(pathList, paths[i]);
  end;
  
  // Ensure that the last path is visible in the list
  ListSetStartAt(pathList, len - 1);
  
  // Clear the files in the filesList
  ListClearItems(filesList);
  
  //Loop through the directories
  if FindFirst (dialog.currentPath + '*', faAnyFile and faDirectory, info) = 0 then
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
  
  _SelectFileInList();
end;

procedure UpdateFileDialog();
var
  clicked: Region;
  
  procedure _PerformFileListClick();
  var
    selectedText, selectedPath: String;
    selectedIdx: Integer;
  begin
    // Get the idx of the item selected in the files list
    selectedIdx := ListActiveItemIndex(clicked);
    
    if (selectedIdx >= 0) and (selectedIdx < ListItemCount(clicked)) then
    begin
      selectedText := ListItemText(clicked, selectedIdx);
      selectedPath := dialog.currentPath + selectedText;
    end
    else exit;
    
    // if it is double click...
    if (dialog.lastSelectedFile = selectedIdx) then
    begin
      if DirectoryExists(selectedPath) then
        SetFileDialogPath(selectedPath)
      else
        HidePanel(dialog.dialogPanel);
    end
    else
    begin
      // Update the selected file for double click
      dialog.lastSelectedFile     := selectedIdx;
      // Update the selected path and its label
      dialog.currentSelectedPath  := selectedPath;
      UpdateDialogPathText();
    end;
  end;
  
  procedure _PerformPathListClick();
  var
    tmpPath, newPath: String;
    selectedIdx, i, len: Integer;
    paths: Array [0..256] of PChar;
  begin
    // Get the idx of the item selected in the paths
    selectedIdx := ListActiveItemIndex(clicked);
    
    // if it is double click...
    if (dialog.lastSelectedPath = selectedIdx) and (selectedIdx >= 0) and (selectedIdx < ListItemCount(clicked)) then
    begin
      // Get the parts of the current path, and reconstruct up to (and including) selectedIdx
      tmpPath := dialog.currentPath;
      len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
      
      newPath := ExtractFileDrive(dialog.currentPath) + PathDelim;
      for i := 0 to Min(selectedIdx - 1, len) do
      begin
        //Need to exclude trailing path delimiter as some paths will include this at the end...
        newPath := newPath + ExcludeTrailingPathDelimiter(paths[i]) + PathDelim;
      end;
      
      SetFileDialogPath(newPath);
    end
    else
      dialog.lastSelectedPath := selectedIdx;
  end;
  
  procedure _PerformCancelClick();
  begin
    HidePanel(dialog.dialogPanel);
    dialog.cancelled := true;
  end;
  
  procedure _PerformOkClick();
  begin
    HidePanel(dialog.dialogPanel);
  end;
  
  procedure _PerformChangePath();
  var
    newPath: String;
  begin
    newPath := TextboxText(RegionWithID(dialog.dialogPanel, 'PathTextbox'));
    
    SetFileDialogPath(newPath);
  end;
begin
  if PanelClicked() = dialog.dialogPanel then
  begin
    clicked := RegionClicked();
    
    if clicked = RegionWithID(dialog.dialogPanel, 'FilesList') then
      _PerformFileListClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'PathList') then
      _PerformPathListClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'CancelButton') then
      _PerformCancelClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'OkButton') then
      _PerformOkClick();
  end;
  
  if GUITextEntryComplete() and (RegionOfLastUpdatedTextBox() = RegionWithID(dialog.dialogPanel, 'PathTextbox')) then
  begin
    _PerformChangePath();
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
  until WindowCloseRequested() or DialogComplete() or DialogCancelled();
  
  WriteLn(DialogPath());
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
