Attribute VB_Name = "MouseCursor"
Public Sub MouseCursor()
    Dim ball As sprite
    Dim position As Vector
    Set ball = New sprite
    Set position = New Vector
    Set ball = Graphics.CreateSprite(GameImage("BallImage1"))
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Set position = MouseAndKey.GetMousePosition
        Call Graphics.DrawHorizontalLine(white, position.GetY, 0, 800)
        Call Graphics.DrawVerticalLine(white, position.GetX, 0, 600)
        If MouseAndKey.MouseWasClicked(MouseButton_LeftButton) Then
            Call ball.SetX(position.GetX - Graphics.CurrentWidth(ball) / 2)
            Call ball.SetY(position.GetY - Graphics.CurrentHeight(ball) / 2)
            Call Graphics.DrawSprite(ball)
        End If
        Call TitleDisplay.DrawOverlay("Mouse Cursor Example")
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
