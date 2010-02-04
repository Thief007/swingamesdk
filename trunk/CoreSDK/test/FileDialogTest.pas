program FileDialogTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  Math, SysUtils,
  MyStrUtils,
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes;


type
  
  FileDialogSelectType = ( fdFiles = 1, fdDirectories = 2, fdFilesAndDirectories = 3 );
  
  FileDialogData = record
    dialogPanel:          Panel;      // The panel used to show the dialog
    currentPath:          String;     // The path to the current directory being shown
    currentSelectedPath:  String;     // The path to the file selected by the user
    cancelled:            Boolean;
    allowNew:             Boolean;
    selectType:           FileDialogSelectType;
    onlyFiles:            Boolean;
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
    
    cancelled     := false;
    
    if length(currentPath) = 0 then 
      SetFileDialogPath(GetUserDir());
  end;
end;

procedure ShowOpenDialog(select: FileDialogSelectType); overload;
begin
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OkLabel', 'Open');
    LabelSetText(dialogPanel, 'FileEntryLabel', 'Open');
    
    allowNew      := false;
    selectType    := select;
    ShowPanel(dialogPanel);
  end;
  
  InitialiseFileDialog();
end;

procedure ShowOpenDialog(); overload;
begin
  ShowOpenDialog(fdFilesAndDirectories);
end;

procedure ShowSaveDialog(select: FileDialogSelectType); overload;
begin
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OkLabel', 'Save');
    LabelSetText(dialogPanel, 'FileEntryLabel', 'Save');
    
    allowNew      := true;
    selectType    := select;
    ShowPanel(dialogPanel);
  end;
  
  InitialiseFileDialog();
end;

procedure ShowSaveDialog(); overload;
begin
  ShowSaveDialog(fdFilesAndDirectories);
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
  else result := ExpandFileName(dialog.currentSelectedPath);
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
  if TextWidth(TextboxFont(pathTxt), selectedPath) > RegionWidth(pathTxt) then
    TextboxSetAlignment(pathTxt, AlignRight)
  else TextboxSetAlignment(pathTxt, AlignLeft);
  
  TextboxSetText(pathTxt, selectedPath);
end;

function IsSet(toCheck, checkFor: FileDialogSelectType): Boolean; overload;
begin
  result := (LongInt(toCheck) and LongInt(checkFor)) = LongInt(checkFor);
end;

procedure ShowErrorMessage(msg: String);
begin
  LabelSetText(dialog.dialogPanel, 'ErrorLabel', msg);
  PlaySoundEffect(SoundEffectNamed('DialogError'));
end;

procedure SetFileDialogPath(fullname: String);
var
  tmpPath, path, filename: String;
  paths: Array [0..255] of PChar;
  
  procedure _PopulatePathList();
  var
    pathList: GUIList;
    i, len: Integer;
  begin
    pathList  := ListFromRegion(RegionWithId(dialog.dialogPanel, 'PathList'));
    
    // Clear the list of all of its items
    ListClearItems(pathList);
    
    // Add the drive letter at the start
    if Length(ExtractFileDrive(path)) = 0 then
      ListAddItem(pathList, PathDelim)
    else
      ListAddItem(pathList, ExtractFileDrive(path));
    
    // Set the details in the path list
    len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
    
    // Loop through the directories adding them to the list
    for i := 0 to len - 1 do
    begin
      ListAddItem(pathList, paths[i]);
    end;
    
    // Ensure that the last path is visible in the list
    ListSetStartAt(pathList, ListLargestStartIndex(pathList));
  end;
  
  procedure _PopulateFileList();
  var
    filesList: GUIList;
    info : TSearchRec;
  begin
    filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
    
    // Clear the files in the filesList
    ListClearItems(filesList);
    
    //Loop through the directories
    if FindFirst (dialog.currentPath + '*', faAnyFile and faDirectory, info) = 0 then
    begin
      repeat
        with Info do
        begin
          if (attr and faDirectory) = faDirectory then 
          begin
            // its a directory... always add it
            ListAddItem(filesList, name);
          end
          else
          begin
            // Its a file ... add if not dir only
            if IsSet(dialog.selectType, fdFiles) then
            begin
              ListAddItem(filesList, name);
            end;
          end;
        end;
      until FindNext(info) <> 0;
    end;
    FindClose(info);
  end;
  
  procedure _SelectFileInList();
  var
    filesList: GUIList;
    fileIdx: Integer;
  begin
    filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
    if Length(filename) > 0 then
    begin
      fileIdx := ListTextIndex(filesList, filename);
      ListSetActiveItemIndex(filesList, fileIdx);
      ListSetStartAt(filesList, fileIdx);
    end;
  end;
begin
  // expand the relative path to absolute
  if not ExtractFileAndPath(fullname, path, filename, dialog.allowNew) then 
  begin
    ShowErrorMessage('Unable to find file or path.');
    exit;
  end;
  
  // Get the path without the ending delimiter (if one exists...)
  tmpPath := ExcludeTrailingPathDelimiter(path);
  
  with dialog do
  begin
    currentPath         := IncludeTrailingPathDelimiter(tmpPath);
    currentSelectedPath := path + filename;
    lastSelectedPath    := -1;
    lastSelectedFile    := -1;
  end;
  
  UpdateDialogPathText();
  
  _PopulatePathList();
  _PopulateFileList();
  _SelectFileInList();
end;

procedure DialogCheckComplete();
var
  path: String;
begin
  path := DialogPath();
  
  if (not dialog.allowNew) and (not FileExists(path)) then
  begin
    ShowErrorMessage('Select an existing file or path.');
    exit;
  end;
  
  // The file exists, but is not a directory and files cannot be selected
  if (not IsSet(dialog.selectType, fdFiles)) and FileExists(path) and (not DirectoryExists(path)) then
  begin
    ShowErrorMessage('Please select a directory.');
    exit;
  end;
  
  if (not IsSet(dialog.selectType, fdDirectories)) and DirectoryExists(path) then
  begin
    ShowErrorMessage('Please select a file.');
    exit;
  end;
  
  HidePanel(dialog.dialogPanel);
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
        DialogCheckComplete();
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
    DialogCheckComplete();
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

procedure Main();
var
  lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('File Dialog Test', 800, 600);
  
  InitInterface();
  ShowOpenDialog(fdFiles);
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
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
