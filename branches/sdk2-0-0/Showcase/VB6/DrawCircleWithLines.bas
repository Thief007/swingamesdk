Attribute VB_Name = "DrawCircleWithLines"
Public Sub DrawCircleWithLines()
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        If MouseAndKey.IsKeyPressed(Keys_VK_RIGHT) Then
            Call Camera.MoveVisualArea(4, 0)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_DOWN) Then
            Call Camera.MoveVisualArea(0, 4)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_UP) Then
            Call Camera.MoveVisualArea(0, -4)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_LEFT) Then
            Call Camera.MoveVisualArea(-4, 0)
        End If
        Call Graphics.FillCircle(green, Core.ScreenWidth / 2, Core.ScreenHeight / 2, 200)
        Call Graphics.DrawCircle_NoFill(white, Core.ScreenWidth / 2, Core.ScreenHeight / 2, 200)
        Call Graphics.FillCircleOnScreen(red, Core.ScreenWidth / 2, Core.ScreenHeight / 2, 100)
        Call Graphics.DrawCircleOnScreen(white, Core.ScreenWidth / 2, Core.ScreenHeight / 2, 100)
        Call Graphics.DrawHorizontalLine(white, Core.ScreenHeight / 2, Core.ScreenWidth, 0)
        Call Graphics.DrawVerticalLine(white, Core.ScreenWidth / 2, Core.ScreenHeight, 0)
        Call TitleDisplay.DrawOverlay("Camera Example")
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
