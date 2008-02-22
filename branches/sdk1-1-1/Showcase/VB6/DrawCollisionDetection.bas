Attribute VB_Name = "DrawCollisionDetection"

Public Sub DrawCollisionDetection()
    Dim ball1 As SGSDKVB6.sprite
    Dim ball2 As SGSDKVB6.sprite
    Dim xSpeed1 As Long
    Dim xSpeed2 As Long
    Dim ySpeed1 As Long
    Dim ySpeed2 As Long
    Call SwinGame.Graphics.ClearScreen
    xSpeed1 = 3
    xSpeed2 = 3
    ySpeed1 = 3
    ySpeed2 = 3
    Set ball1 = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BallImage1"))
    Set ball2 = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BallImage2"))
    Call ball1.SetX(0)
    Call ball1.SetY(0)
    Call ball2.SetX(SwinGame.Core.ScreenWidth() - SwinGame.Graphics.CurrentWidth(ball2))
    Call ball2.SetY(SwinGame.Core.ScreenHeight() - SwinGame.Graphics.CurrentHeight(ball2))
    Call ball1.SetUsePixelCollision(True)
    Call ball2.SetUsePixelCollision(True)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call SwinGame.Graphics.DrawSprite(ball1)
        Call SwinGame.Graphics.DrawSprite(ball2)
        If SwinGame.Physics.HaveSpritesCollided(ball1, ball2) Then
            Call SwinGame.Text.DrawText("Collided!", white, GameResources.GameFont("Courier"), SwinGame.Core.ScreenWidth - 90, SwinGame.Core.ScreenHeight - 20)
        End If
        Call MoveBall(ball1, xSpeed1, ySpeed1)
        Call MoveBall(ball2, xSpeed2, ySpeed2)
        Call SwinGame.Physics.HaveSpritesCollided(ball1, ball2)
        Call TitleDisplay.DrawOverlay("Collision Detection Example")
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
    
Private Sub MoveBall(ball As SGSDKVB6.sprite, xSpeed As Long, ySpeed As Long)
    ball.SetX (ball.GetX + xSpeed)
    ball.SetY (ball.GetY + ySpeed)
    If ball.GetX > (SwinGame.Core.ScreenWidth - SwinGame.Graphics.CurrentWidth(ball)) Then
        ball.SetX (SwinGame.Core.ScreenWidth - SwinGame.Graphics.CurrentWidth(ball))
        xSpeed = -1 * xSpeed
    End If
    If ball.GetY > (SwinGame.Core.ScreenHeight - SwinGame.Graphics.CurrentHeight(ball)) Then
        ball.SetY (SwinGame.Core.ScreenHeight - SwinGame.Graphics.CurrentHeight(ball))
        ySpeed = -1 * ySpeed
    End If
    If ball.GetX < 0 Then
        ball.SetX (0)
        xSpeed = -1 * xSpeed
    End If
    If ball.GetY < 0 Then
        ball.SetY (0)
        ySpeed = -1 * ySpeed
    End If
End Sub


