///
/// GameLogic.pas
///
/// --------------------------------------
///	 This file contains the logic for your
///  game. The Main procedure is called from
///  the game launcher. 
/// --------------------------------------
unit GameLogic;

interface
  procedure Main();

implementation
uses
  GameResources,
  SysUtils,
  SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
  SGSDK_KeyCodes;

  type 
    IntArr = Array of Integer;
    
  const n = 1000000; //number of queens
  
  var
    //Arrays to store diagonal 
    dp, dn: IntArr;
    currentConflicts: Integer;
    atQueen, steps: Integer;
    //Queen locations
    queenLoc: IntArr;
    startTime: TDateTime;
  
  function Conflicts(queenCol, queenRow: Integer): Integer;
  begin
    //The number of queens on each diagonal - this queen from both diagonals (-2)
    result := dp[n + queenCol - queenRow - 1] + dn[queenCol + queenRow] - 2;
    //WriteLn(queenCol, ',', queenRow, ': ', dp[n + queenCol - queenRow - 1], ' + ', dn[queenCol + queenRow], ' = ', result);    
  end;
  
  procedure UpdateDiagonals(queenCol, queenRow, change: Integer);
  begin
    dp[n + queenCol - queenRow - 1] += change;
    dn[queenCol + queenRow] += change;
  end;
  
  function NextConflictingQueen(): Integer;
  begin    
    atQueen := (atQueen + 1) mod n;
    
    //Console.WriteLine("Conflicts = {0} == {1}", CurrentConflicts, TotalConflicts);
    while Conflicts(atQueen, queenLoc[atQueen]) = 0 do
    begin
        atQueen := (atQueen + 1) mod n;
    end;
    
    steps += 1;
    if steps mod 100 = 0 then Write('.');
    result := atQueen;
  end;
  
  procedure Swap(idx1, idx2: Integer);
  var
    tmp: Integer;
  begin
    //Remove from current locations
    UpdateDiagonals(idx1, queenLoc[idx1], -1);
    UpdateDiagonals(idx2, queenLoc[idx2], -1);
    
    //Swap rows...
    tmp := queenLoc[idx1];
    queenLoc[idx1] := queenLoc[idx2];
    queenLoc[idx2] := tmp;
    
    //Add to new locations
    UpdateDiagonals(idx1, queenLoc[idx1], +1);
    UpdateDiagonals(idx2, queenLoc[idx2], +1);    
  end;
  
  procedure MaxFromExchange(currentQueen, otherQueen, start: Integer; var max, best: Integer);
  var
    otherStart, now, otherNow: Integer;
  begin
      otherStart := Conflicts(otherQueen, queenLoc[otherQueen]);
      
      Swap(currentQueen, otherQueen);
      
      now := Conflicts(currentQueen, queenLoc[currentQueen]);
      otherNow := Conflicts(otherQueen, queenLoc[otherQueen]);
      
      if (start - now) + (otherStart - otherNow) > max then
      begin
          best := otherQueen;
          max := (start - now) + (otherStart - otherNow);
          //WriteLn('New best');
      end;
      
      Swap(currentQueen, otherQueen);
  end;
  
  procedure SwapLeftUpdate(idx1, idx2: Integer; isFirst: Boolean);
  var
    tmp: Integer;
  begin
    //Remove from current locations
    if not isFirst then UpdateDiagonals(idx1, queenLoc[idx1], -1);
    //UpdateDiagonals(idx2, queenLoc[idx2], -1);
    
    //Swap rows...
    tmp := queenLoc[idx1];
    queenLoc[idx1] := queenLoc[idx2];
    queenLoc[idx2] := tmp;
    
    //Add to new locations
    if isFirst then UpdateDiagonals(idx1, queenLoc[idx1], +1);
    //UpdateDiagonals(idx2, queenLoc[idx2], +1);    
  end;
  
  procedure InitialSearch();
  var
    i, j, m: Integer;
  begin
    for i := 0 to n - 1 do queenLoc[i] := i;
      
    j := 0;
    //Place queens without collisions
    
    for i := 0 to Round(3.08 * n) do
    begin
      m := j + Random(n - j);
      
      SwapLeftUpdate(j, m, true);
      if Conflicts(j, queenLoc[j]) = 0 then j += 1
      else SwapLeftUpdate(j, m, false);
    end;
    
    for i := j to n - 1 do
    begin
      m := i + Random(n - i);
      SwapLeftUpdate(i, m, true);
    end;
    
    currentConflicts := n - j + 2; //check...
  end;
  
  procedure FinalSearch();
  var
    i, j, limit: Integer;
    b: Boolean;
  begin
    for i := n - currentConflicts to n - 1 do
    begin
      if Conflicts(i, queenLoc[i]) > 0 then
      begin
        limit := 0;
        
        repeat
          limit += 1;
          j := Random(n);
          if i <> j then
          begin
            Swap(i, j);
            b := (Conflicts(i, queenLoc[i]) > 0) or (Conflicts(j, queenLoc[j]) > 0);
            if b then Swap(i, j);
          end
          else
            b := true;
        until (not b) or (limit >= 7000);
        
        if b then
        begin
          InitialSearch();
          exit;
        end;
      end;
    end;
    
    WriteLn('At solution in ', TimeToStr(Now - startTime));
  end;
  
  procedure StepToSolutionSwapHeuristic();
  var
    qIdx, max, start: Integer;
    best: Integer; //, queenRow
    idx: Integer;
  begin
    if currentConflicts = 0 then exit;

    //Get the next queen's index
    qIdx := NextConflictingQueen();
    
    //Do Best Swap
    best := -1; //best is now know...
    //queenRow := queenLoc[qIdx]; //this queen's row;
    
    max := 0;
    start := Conflicts(qIdx, queenLoc[qIdx]); //the starting conflicts
    
    for idx := qIdx + 1 to n - 1 do
    begin
        MaxFromExchange(qIdx, idx, start, max, best);
    end;
    
    if max = 0 then
    begin
        for idx := 0 to idx - 1 do
        begin
            MaxFromExchange(qIdx, idx, start, max, best);
        end;                
    end;
    
    if max > 0 then
    begin
        Swap(qIdx, best);
        currentConflicts -= max * 2;
    end;
    
    if currentConflicts = 0 then
    begin
      WriteLn('At solution in ', TimeToStr(Now - startTime));
      //Halt();
    end;
  end;
  
  procedure Draw();
  var
    i, confl, x, y: Integer;
  begin
    ClearScreen();

    for i := 0 to n - 1 do
    begin
      confl := Conflicts(i, queenLoc[i]);
      
      x := Round((i / n) * 800);
      y := Round((queenLoc[i] / n) * 800);
                      
      case confl of
          0: DrawPixel(ColorWhite, x, y);
          1: DrawPixel(ColorYellow, x, y);
          else DrawPixel(ColorRed, x, y);
      end;
    end;
  end;
  
  procedure PrintStatus();
  var
    i: Integer;
  begin
    for i := 0 to n - 1 do
    begin
      WriteLn(i, ':', queenLoc[i], ' -> ', Conflicts(i, queenLoc[i]));
    end;
  end;
  
  //The main procedure that controlls the game logic.
  //
  // SIDE EFFECTS:
  // - Creates the screen, and displays a message
  procedure Main();
  var
    i, j, tmpInt: Integer;
    tmp: IntArr;
  begin
    Randomize();
    OpenGraphicsWindow(IntToStr(n) + ' Queens', 800, 800);
    
    LoadResources();
    
    DrawText('Loading Queens...', ColorWhite, GameFont('ArialLarge'), 50, 50);
    
    startTime := Now();
    
    SetLength(queenLoc, n);
    //SetLength(tmp, n);
    SetLength(dp, 2 * n - 1); //positive diagonal
    SetLength(dn, 2 * n - 1); //negative diagonal
    
    for i := 0 to 2 * n - 1 - 1 do
    begin
      dp[i] := 0;
      dn[i] := 0;
    end;
    
    {
    //Populate index array....
    for i := 0 to n - 1 do
    begin
      if i mod 2 = 0 then  
        tmp[i] := i //populate with index values
      else
        tmp[i] := n - i;
    end;
    
    //Randomise the locations...
    for i := 0 to n - 1 do
    begin
      j := Random(n);
      tmpInt := tmp[i];
      tmp[i] := tmp[j];
      tmp[j] := tmpInt;
    end;
    
    currentConflicts := 0;
    
    //Grenerate initial random locations
    for i := 0 to n - 1 do
    begin
      queenLoc[i] := tmp[i];
      //Update the queens on the diagonals
      UpdateDiagonals(i, queenLoc[i], +1);
      
      if (i mod 10000 = 0) or (i = n) then
      begin
          ClearScreen();
          
          DrawText(IntToStr(Round(i / n * 100)) + '% Loaded', ColorWhite, GameFont('Courier'), 10, 400);
          
          RefreshScreen();
          ProcessEvents();
          
          if WindowCloseRequested() then exit;
      end;
    end;
    
    SetLength(tmp, 0);
  
    //Setup initial conflicts
    for i := 0 to n - 1 do
    begin
      currentConflicts += Conflicts(i, queenLoc[i]);
    end;
  }
    InitialSearch();
    //PrintStatus();
    WriteLn('Start with - ', currentConflicts, ' conflicts!');
    FinalSearch();
    //PrintStatus();
    
    i := 0;
    
    repeat
      ProcessEvents();
      
      //if i mod 100 = 0 then
      begin      
        Draw();
        DrawFrameRate(0, 0, GameFont('Courier'));
        RefreshScreen();
      end;
    
      //StepToSolution();
      i += 1;
    until WindowCloseRequested();
    
    FreeResources();
  end;

end.
