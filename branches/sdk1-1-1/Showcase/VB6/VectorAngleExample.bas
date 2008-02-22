Attribute VB_Name = "VectorAngleExample"
Public Sub VectorAngleExample()
    Dim pd As sprite
    Dim mousePos As Vector
    Set pd = New sprite
    Set mousePos = New Vector
    Set pd = Graphics.CreateSprite(GameImage("BallImage1"))
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call mousePos.setVector(MouseAndKey.GetMousePosition)
        Call pd.SetMovementVector(Physics.GetVectorFromAngle(Physics.CalculateAngle_Number(pd.getX + Graphics.CurrentWidth(pd) / 2, pd.getY + Graphics.CurrentHeight(pd) / 2, mousePos.getX, mousePos.getY), 2))
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
