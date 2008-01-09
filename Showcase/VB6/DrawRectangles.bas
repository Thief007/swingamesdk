Attribute VB_Name = "DrawRectangles"
Public Sub DrawRectangles()
    Call SwinGame.Graphics.ClearScreen
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        Call SwinGame.Graphics.DrawRectangleOnScreen(Colour.GetRandomColour, (Rnd * 800 + 1), (Rnd * 800 + 1), (Rnd * 800 + 1), (Rnd * 800 + 1))
        Call SwinGame.Graphics.FillRectangleOnScreen(Colour.GetRandomColour, (Rnd * 800 + 1), (Rnd * 800 + 1), (Rnd * 800 + 1), (Rnd * 800 + 1))
        Call TitleDisplay.DrawOverlay("Drawing Rectangles Example")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
