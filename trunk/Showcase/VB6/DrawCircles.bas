Attribute VB_Name = "DrawCircles"
Public Sub DrawCircles()
    Call SwinGame.Graphics.ClearScreen
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        Call SwinGame.Graphics.DrawCircleOnScreen(Colour.GetRandomColour, (Rnd * 800), (Rnd * 800), (Rnd * 800))
        Call SwinGame.Graphics.FillCircleOnScreen(Colour.GetRandomColour, (Rnd * 800), (Rnd * 800), (Rnd * 800))
        Call TitleDisplay.DrawOverlay("Drawing Circles Example")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
