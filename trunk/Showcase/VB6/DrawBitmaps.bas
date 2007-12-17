Attribute VB_Name = "DrawBitmaps"
Public Sub DrawBitmaps()
    Call SwinGame.Graphics.ClearScreen
    Dim i As Long
    i = 0
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        Call SwinGame.Graphics.DrawBitmap(GameResources.GameImage("BallImage1"), Round(SwinGame.Core.Sin(i) * 100) + 250, Round(SwinGame.Core.Cos(i) * 100) + 200)
        Call SwinGame.Graphics.DrawBitmap(GameResources.GameImage("BallImage2"), Round(SwinGame.Core.Cos(i) * 100) + 250, Round(SwinGame.Core.Sin(i) * 100) + 200)
        Call TitleDisplay.DrawOverlay("Drawing Bitmap Example")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        Call SwinGame.Graphics.ClearScreen
        i = i + 1
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
    
    
    
