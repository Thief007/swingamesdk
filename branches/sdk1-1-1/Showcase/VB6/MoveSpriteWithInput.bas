Attribute VB_Name = "MoveSpriteWithInput"
Public Sub MoveSpriteWithInput()
    Dim ball As sprite
    Dim xSpeed As Long
    Dim ySpeed As Long
    Set ball = New sprite
    Set ball = Graphics.CreateSprite(GameImage("BallImage1"))
    Call ball.SetX(400)
    Call ball.SetY(300)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        xSpeed = 0
        ySpeed = 0
        If MouseAndKey.IsKeyPressed(Keys_VK_UP) = True Then
            ySpeed = -2
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_DOWN) = True Then
            ySpeed = 2
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_LEFT) = True Then
            xSpeed = -2
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_RIGHT) = True Then
            xSpeed = 2
        End If
        Call SwinGame.Graphics.DrawSprite(ball)
        Call ball.SetX(ball.GetX + xSpeed)
        Call ball.SetY(ball.GetY + ySpeed)
        Call TitleDisplay.DrawOverlay("Move Sprite with Arrow Keys Example")
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
