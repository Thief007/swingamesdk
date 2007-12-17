Attribute VB_Name = "FollowSpriteExample"
Public Sub FollowSpriteExample()
    Dim sea As Bitmap
    Dim ship As sprite
    Set sea = New Bitmap
    Set ship = New sprite
    Set sea = GameImage("Sea")
    Set ship = Graphics.CreateSprite_MultiFPC(GameImage("Ship"), 3, 2, 40, 43)
    Call ship.SetX(0)
    Call ship.SetY(0)
    
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        If MouseAndKey.IsKeyPressed(Keys_VK_RIGHT) Then
            Call ship.SetX(ship.GetX + 4)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_DOWN) Then
            Call ship.SetY(ship.GetY + 4)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_UP) Then
            Call ship.SetY(ship.GetY - 4)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_LEFT) Then
            Call ship.SetX(ship.GetX - 4)
        End If
        Call Camera.FollowSprite(ship, 0, -150)
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Graphics.ClearScreen
        Call Graphics.DrawBitmap(sea, 0, 0)
        Call Graphics.DrawSprite(ship)
        Call Graphics.UpdateSprite(ship)
        Call TitleDisplay.DrawOverlay("Follow Sprite Example")
        
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
    Call Camera.SetScreenOffset(0, 0)
End Sub
