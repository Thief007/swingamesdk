Attribute VB_Name = "VectorAngleExample"
Public Sub VectorAngleExample()
    Dim pd As sprite
    Dim mousePos As Vector
    Set pd = New sprite
    Set mousePos = New Vector
    Set pd = Graphics.CreateSprite(GameImage("BallImage1"))
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call mousePos.setVector(MouseAndKey.GetMousePosition)
        Call pd.SetMovementVector(Physics.GetVectorFromAngle(Physics.CalculateAngle_Number(pd.GetX + Graphics.CurrentWidth(pd) / 2, pd.GetY + Graphics.CurrentHeight(pd) / 2, mousePos.GetX, mousePos.GetY), 2))
            '        pd.movement := GetVectorFromAngle      (CalculateAngle         (pd.xPos + CurrentWidth(pd)         / 2, pd.yPos + CurrentHeight(pd)          / 2, mousePos.X,      mousePos.Y), 2);

        Call Graphics.MoveSprite(pd, pd.GetMovementVector)
        Call Graphics.ClearScreen_ToColour(black)
        Call Graphics.DrawSprite(pd)
        Call TitleDisplay.DrawOverlay("Get Vector From Angle Example (The ball will follow your cursor)")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        Call SwinGame.Graphics.ClearScreen
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
    '        mousePos := GetMousePosition();
    '        pd.movement := GetVectorFromAngle(CalculateAngle(pd.xPos + CurrentWidth(pd) / 2, pd.yPos + CurrentHeight(pd) / 2, mousePos.X, mousePos.Y), 2);
    '        MoveSprite(pd, pd.movement);
    '        ClearScreen(ColorBlack);
    '        DrawSprite(pd);
    '        DrawOverlay('Get Vector From Angle Example (The ball will follow your cursor)');
    '        RefreshScreen();
    '        if WindowCloseRequested() then exit;
    '    until IsKeyPressed(VK_N);
    '    Sleep(500);
    '    ProcessEvents();
    'end;
