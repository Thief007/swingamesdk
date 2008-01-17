Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    Call StartUp
    Dim Game As AirHockeyGame
    Game = CreateGame
    Call StartBall(Game)
    'Call Core.ToggleFullScreen
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call Text.DrawText(("About to enter MoveBall"), red, GameFont("Courier"), 0, 20)
        Call Core.RefreshScreen_WithFrame(6000)
        'Call Core.TakeScreenshot("moveball")
        Call Core.ProcessEvents
        
        Call MoveBall(Game)
        
        
        Call Text.DrawText(("Left MoveBall and entering MoveBat"), red, GameFont("Courier"), 0, 40)
        Call Core.RefreshScreen_WithFrame(6000)
        Call Core.TakeScreenshot("\movebat.bmp")
        Call Core.ProcessEvents

        Call MoveBat(Game)
        
        
         Call Text.DrawText(("Left MoveBall"), red, GameFont("Courier"), 0, 40)
         Call Core.RefreshScreen_WithFrame(6000)
         Call Core.TakeScreenshot("done")
        Call Core.ProcessEvents
       

        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
        Call Draw(Game)
    Loop
    Call ShutDown
End Sub
Private Sub MoveBat(Game As AirHockeyGame)
    Call MovePlayer(Game.Players(0).Bat)
    Call MoveAI(Game.Players(1).Bat, Game.ball)
End Sub

Private Sub MoveAI(Bat As Sprite, ball As Sprite)
    Call Bat.SetMovementVector(Physics.LimitVector(Bat.GetMovementVector, 20))
    Call Bat.SetMovementVector(Physics.Multiply_Vector(Physics.ScaleMatrix(0.95), Bat.GetMovementVector))
    Call Graphics.MoveSprite(Bat, Bat.GetMovementVector)
    

    
    If ball.getY > 300 Then
        'move back to base
        If (Bat.getX + Graphics.CurrentWidth(Bat)) > (ball.getX + Graphics.CurrentWidth(ball)) Then
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(-0.1, 0)))
        End If
        If (Bat.getX + Graphics.CurrentWidth(Bat)) < (ball.getX + Graphics.CurrentWidth(ball)) Then
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0.1, 0)))
        End If
        If Bat.getY > 100 Then
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0, -0.2)))
        Else
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0, 0.05)))
        End If
    Else
       Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.GetVectorFromAngle(Physics.CalculateAngle_Number(Bat.getX + Graphics.CurrentWidth(Bat) / 2, Bat.getY + Graphics.CurrentHeight(Bat) / 2, ball.getX + Graphics.CurrentWidth(ball) / 2, ball.getY + Graphics.CurrentHeight(ball) / 2), 0.3)))

    End If
    
    If Bat.getX < 216 Then
        Call Bat.setX(216)
        Call Bat.SetMovementX(Bat.GetMovementX * -1)
    End If
    If Bat.getX > 584 - Graphics.CurrentWidth(Bat) Then
        Call Bat.setX(584 - Graphics.CurrentWidth(Bat))
        Call Bat.SetMovementX(Bat.GetMovementX * -1)
    End If
    If Bat.getY < 70 Then
        Call Bat.setY(70)
        Call Bat.SetMovementY(Bat.GetMovementY * -1)
    End If
    If Bat.getY > 300 - Graphics.CurrentHeight(Bat) Then
        Call Bat.setY(300 - Graphics.CurrentHeight(Bat))
        Call Bat.SetMovementY(0) '(Bat.GetMovementY * -1)
    End If
End Sub
Private Sub MovePlayer(Bat As Sprite)       'add friction to the bat
    'Call Bat.setX(MouseAndKey.GetMousePosition.getX - (Graphics.CurrentWidth(Bat) / 2))
    'Call Bat.setY(MouseAndKey.GetMousePosition.getY - (Graphics.CurrentHeight(Bat) / 2))
    Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.MultiplyVector(MouseAndKey.GetMouseMovement, 0.015)))
    Call Bat.SetMovementVector(Physics.LimitVector(Bat.GetMovementVector, 20))
    Call Bat.SetMovementVector(Physics.Multiply_Vector(Physics.ScaleMatrix(0.95), Bat.GetMovementVector))
    Call Graphics.MoveSprite(Bat, Bat.GetMovementVector)
    Call MouseAndKey.HideMouse
    
    Call MouseAndKey.MoveMouse(400, 300) 'Core.ScreenWidth / 2, Core.ScreenHeight / 2)
    Call Core.ProcessEvents
    If Bat.getX < 216 Then
        Call Bat.setX(216)
        Call Bat.SetMovementX(Bat.GetMovementX * -1)
    End If
    If Bat.getX > 584 - Graphics.CurrentWidth(Bat) Then
        Call Bat.setX(584 - Graphics.CurrentWidth(Bat))
        Call Bat.SetMovementX(Bat.GetMovementX * -1)
    End If
    If Bat.getY < 300 Then
        Call Bat.setY(300)
        Call Bat.SetMovementY(0) '(Bat.GetMovementY * -1)
    End If
    If Bat.getY > 530 - Graphics.CurrentHeight(Bat) Then
        Call Bat.setY(530 - Graphics.CurrentHeight(Bat))
        Call Bat.SetMovementY(Bat.GetMovementY * -1)
    End If
End Sub
Private Sub Draw(Game As AirHockeyGame)
    Call Graphics.ClearScreen_ToColour(Core.GetColor(159, 207, 241))
    Call Graphics.DrawBitmap(Game.TablePics.BackGround, 0, 0)
    Call Graphics.DrawSprite(Game.ball)
    Call Graphics.DrawSprite(Game.Players(0).Bat)
    Call Graphics.DrawSprite(Game.Players(1).Bat)
    
    
    'Call Text.DrawText(Math.Round(Game.Ball.GetMovementVector.getX), red, GameFont("Courier"), 0, 0)
    'Call Text.DrawText(Math.Round(Game.Ball.GetMovementY), red, GameFont("Courier"), 0, 20)
    'Call Text.DrawText((MouseAndKey.GetMouseMovement().getX & ""), red, GameFont("Courier"), 0, 40)
    'Call Text.DrawText((MouseAndKey.GetMouseMovement().getY & ""), red, GameFont("Courier"), 0, 60)
    Call Text.DrawFramerate(0, 0, GameFont("Courier"))
    
    Call MouseAndKey.GetMouseMovement
End Sub
Private Sub StartBall(Game As AirHockeyGame)
    Dim x As Long
    Dim y As Long
    Call Game.ball.setX(400 - (Graphics.CurrentWidth(Game.ball) / 2))
    Call Game.ball.setY(300 - (Graphics.CurrentHeight(Game.ball) / 2))
    Call Randomize
    x = Int(Rnd * 7) - 3
    y = Int(Rnd * 7) - 3
    Call Game.ball.SetMovementVector(Physics.CreateVector_NoInvert(x, y))
End Sub
Private Function CreateGame() As AirHockeyGame
    CreateGame.TablePics = CreateTable
    Set CreateGame.ball = New Sprite
    Set CreateGame.ball = Graphics.CreateSprite(GameImage("ball"))
    Call CreateGame.ball.SetUsePixelCollision(True)
    
    Call CreateGame.ball.SetMass(1)
    
    ReDim CreateGame.Players(1)
    CreateGame.Players(0) = CreatePlayer(0)
    CreateGame.Players(1) = CreatePlayer(1)
End Function
Private Function CreatePlayer(playernumber As Long) As Player
    Set CreatePlayer.Bat = New Sprite
    Set CreatePlayer.Bat = Graphics.CreateSprite(GameImage(playernumber & "bat"))  'image in here (playernumber & "bat")
    Call CreatePlayer.Bat.SetUsePixelCollision(True)
    Call CreatePlayer.Bat.setX(400 - Graphics.CurrentWidth(CreatePlayer.Bat) / 2)
    Call CreatePlayer.Bat.setY(100 - 360 * (playernumber - 1))
    
    Set CreatePlayer.Goal = New Sprite
    Set CreatePlayer.Goal = Graphics.CreateSprite(GameImage(playernumber & "goal"))
    Call CreatePlayer.Goal.SetUsePixelCollision(True)
    CreatePlayer.Score = 0
    
    Call CreatePlayer.Bat.SetMass(5)
    
End Function
Private Function CreateTable() As Table
    Set CreateTable.BackGround = New Bitmap
    Set CreateTable.BackGround = GameImage("Table")
    Set CreateTable.TableHorizontal = New Sprite
    Set CreateTable.TableHorizontal = Graphics.CreateSprite(GameImage("TableHorizontal"))
    Set CreateTable.TableVertical = New Sprite
    Set CreateTable.TableVertical = Graphics.CreateSprite(GameImage("TableVertical"))
    Call CreateTable.TableVertical.SetUsePixelCollision(True)
    Call CreateTable.TableHorizontal.SetUsePixelCollision(True)
End Function
Private Sub MoveBall(Game As AirHockeyGame)
    Call KeepBallOnTable(Game)
    Call Game.ball.SetMovementVector(Physics.LimitVector(Game.ball.GetMovementVector, 20))
    Call Game.ball.SetMovementVector(Physics.Multiply_Vector(Physics.ScaleMatrix(0.995), Game.ball.GetMovementVector))
    Call Graphics.MoveSprite(Game.ball, Game.ball.GetMovementVector)
    Dim i As Long
    For i = 0 To 1
        If Physics.HaveSpritesCollided(Game.ball, Game.Players(i).Bat) Then
            Call Physics.VectorCollision(Game.ball, Game.Players(i).Bat)
        End If
    Next i
End Sub


Private Sub KeepBallOnTable(Game As AirHockeyGame)
    If Physics.HaveSpritesCollided(Game.ball, Game.TablePics.TableHorizontal) Then
        Call RescueTheBall(Game)
        Call Game.ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.ball.GetMovementX, Game.ball.GetMovementY * -1))
    End If
    
    If Physics.HaveSpritesCollided(Game.ball, Game.TablePics.TableVertical) Then
        Call RescueTheBall(Game)
        Call Game.ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.ball.GetMovementX * -1, Game.ball.GetMovementY))
    End If
End Sub

Private Sub RescueTheBall(Game As AirHockeyGame)
    If Game.ball.getX < 216 Then
        Call Game.ball.setX(217)
    End If
    If Game.ball.getX > 584 Then
        Call Game.ball.setX(583)
    End If
    If Game.ball.getY < 70 Then
        Call Game.ball.setY(71)
    End If
    If Game.ball.getY > 530 Then
        Call Game.ball.setY(529)
    End If
End Sub

Private Sub StartUp()
    Call Audio.OpenAudio
    Call Core.OpenGraphicsWindow("Air Hockey", 800, 600)
    Call MouseAndKey.MoveMouse(400, 300)
    Call LoadResources
End Sub

Private Sub ShutDown()
    Call FreeResources
    Call Audio.CloseAudio
End Sub
