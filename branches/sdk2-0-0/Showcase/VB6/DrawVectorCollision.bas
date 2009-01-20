Attribute VB_Name = "DrawVectorCollision"
Private Sub MoveBallUsingVecotor(ball As SGSDKVB6.sprite)
    Call SwinGame.Graphics.MoveSprite(ball, ball.GetMovementVector)
    If ball.GetX > SwinGame.Core.ScreenWidth - SwinGame.Graphics.CurrentWidth(ball) Then
        Call ball.SetMovementX(ball.GetMovementX * -1)
        Call ball.SetX(SwinGame.Core.ScreenWidth - SwinGame.Graphics.CurrentWidth(ball))
    End If
    If ball.GetY > SwinGame.Core.ScreenHeight - SwinGame.Graphics.CurrentHeight(ball) Then
        Call ball.SetMovementY(ball.GetMovementY * -1)
        Call ball.SetY(SwinGame.Core.ScreenHeight - SwinGame.Graphics.CurrentHeight(ball))
    End If
    If ball.GetX < 0 Then
        Call ball.SetMovementX(ball.GetMovementX * -1)
        Call ball.SetX(0)
    End If
    If ball.GetY < 0 Then
        Call ball.SetMovementY(ball.GetMovementY * -1)
        Call ball.SetY(0)
    End If
End Sub

Public Sub DrawVectorCollision()
    Dim ball1 As SGSDKVB6.sprite
    Dim ball2 As SGSDKVB6.sprite
    Dim i As Long
    i = 0
    Set ball1 = New SGSDKVB6.sprite
    Set ball2 = New SGSDKVB6.sprite
    Call SwinGame.Graphics.ClearScreen
    Set ball1 = SwinGame.Graphics.CreateSprite(GameImage("BallImage1"))
    Set ball2 = SwinGame.Graphics.CreateSprite(GameImage("BallImage2"))
    Call ball1.SetMovementVector(SwinGame.Physics.CreateVector(3, 3, True))
    Call ball2.SetMovementVector(SwinGame.Physics.CreateVector(3, 3, True))
    Call ball1.SetMass(1)
    Call ball2.SetMass(5)
    Call ball1.SetX(0)
    Call ball1.SetY(0)
    Call ball2.SetX(SwinGame.Core.ScreenWidth - SwinGame.Graphics.CurrentWidth(ball2))
    Call ball2.SetY(SwinGame.Core.ScreenHeight - SwinGame.Graphics.CurrentHeight(ball2))
    Call ball1.SetUsePixelCollision(True)
    Call ball2.SetUsePixelCollision(True)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        Call SwinGame.Graphics.DrawSprite(ball1)
        Call SwinGame.Graphics.DrawSprite(ball2)
        If SwinGame.Physics.HaveSpritesCollided(ball1, ball2) Then
            Call SwinGame.Physics.VectorCollision(ball1, ball2)
        End If
        
        Call MoveBallUsingVecotor(ball1)
        Call MoveBallUsingVecotor(ball2)
        Call TitleDisplay.DrawOverlay("Vector Collision Example")
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
