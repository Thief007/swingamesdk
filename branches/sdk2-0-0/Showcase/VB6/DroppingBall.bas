Attribute VB_Name = "DroppingBall"
Public Sub DroppingBall()
    Dim ball As sprite
    Dim GravityConst As Vector
    Set ball = New sprite
    Set GravityConst = New Vector
    Call GravityConst.setVector(Physics.CreateVector_NoInvert(0, 0.5))
    Set ball = Graphics.CreateSprite(GameImage("SmallBall"))
    
    Call ball.SetMovementVector(Physics.CreateVector_NoInvert(5, 0))
    Call ball.SetMass(1)
    ball.SetX (110)
    ball.SetY (110)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call ball.SetMovementVector(Physics.AddVectors(ball.GetMovementVector, GravityConst.getVector))
        Call ball.SetMovementVector(Physics.MultiplyVector(ball.GetMovementVector, 0.995))
        Call Graphics.MoveSprite(ball, ball.GetMovementVector)
        If ball.GetX > (Core.ScreenWidth - Graphics.CurrentWidth(ball)) Then
            Call ball.SetMovementX(ball.GetMovementX * -1)
            Call ball.SetX(Core.ScreenWidth - Graphics.CurrentWidth(ball))
        End If
        If ball.GetY > (Core.ScreenHeight - Graphics.CurrentHeight(ball)) Then
            If ball.GetMovementY < 1 Then
                Call ball.SetMovementY(0)
            End If
            Call ball.SetMovementY(ball.GetMovementY * -1)
            Call ball.SetY(Core.ScreenHeight - Graphics.CurrentHeight(ball))
        End If
        If ball.GetX < 0 Then
            Call ball.SetMovementX(ball.GetMovementX * -1)
            Call ball.SetX(0)
        End If
        If ball.GetY < 0 Then
            Call ball.SetMovementY(ball.GetMovementY * -1)
            Call ball.SetY(0)
        End If
        Call Graphics.DrawSprite(ball)
        Call TitleDisplay.DrawOverlay("Dropping Ball Example")
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
