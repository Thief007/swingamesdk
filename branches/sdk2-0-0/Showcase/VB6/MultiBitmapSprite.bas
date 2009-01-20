Attribute VB_Name = "MultiBitmapSprite"
Public Sub MultiBitmapSprite()
    Dim sp1 As sprite
    Dim sp2 As sprite
    Set sp1 = New sprite
    Set sp2 = New sprite
    Set sp1 = Graphics.CreateSprite_MultiFPC(GameImage("Explosion"), 5, 15, 38, 38)
    Set sp2 = Graphics.CreateSprite_MultiFPC(GameImage("Ship"), 3, 2, 40, 43)
    Call sp1.SetX(70)
    Call sp1.SetY(100)
    Call sp2.SetX(80)
    Call sp2.SetY(110)
    
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Graphics.DrawSprite(sp1)
        Call Graphics.DrawSprite(sp2)
        Call Graphics.UpdateSprite(sp1)
        Call Graphics.UpdateSprite(sp2)
        Call Graphics.DrawBitmap(GameImage("Explosion"), 70, 170)
        Call Text.DrawText("Explosion Bitmap", white, GameFont("Courier"), 90 + GameImage("Explosion").Width, 190)
        Call Graphics.DrawBitmap(GameImage("Ship"), 70, 250)
        Call Text.DrawText("Ship Bitmap", white, GameFont("Courier"), 90 + GameImage("Ship").Width, 260)
        If Physics.HaveSpritesCollided(sp1, sp2) Then
            Call Text.DrawText("Collided...", white, GameFont("Courier"), 125, 120)
        End If
        Call TitleDisplay.DrawOverlay("Multi-bitmap Collision Detection")
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
            
