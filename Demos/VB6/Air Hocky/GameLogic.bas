Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    Call StartUp
    Dim Game As AirHockeyGame
    Game = CreateGame
    Call StartBall(Game)
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call MoveBall(Game)
        Call MoveBat(Game)
        Call Draw(Game)

        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
    Loop
    Call ShutDown
End Sub
Private Sub MoveBat(Game As AirHockeyGame)
    Call MovePlayer(Game.Players(0).Bat)
End Sub
Private Sub MovePlayer(Bat As Sprite)
    Call Bat.setX(MouseAndKey.GetMousePosition.getX - (Graphics.CurrentWidth(Bat) / 2))
    Call Bat.setY(MouseAndKey.GetMousePosition.getY - (Graphics.CurrentHeight(Bat) / 2))
    Call Bat.SetMovementVector(MouseAndKey.GetMouseMovement)
    MouseAndKey.GetMouseMovement
    If Bat.getX < 216 Then
        Call Bat.setX(216)
        Call Bat.SetMovementX(0)
    End If
    If Bat.getX > 584 - Graphics.CurrentWidth(Bat) Then
        Call Bat.setX(584 - Graphics.CurrentWidth(Bat))
        Call Bat.SetMovementX(0)
    End If
    If Bat.getY < 300 Then
        Call Bat.setY(300)
        Call Bat.SetMovementY(0)
    End If
    If Bat.getY > 530 - Graphics.CurrentHeight(Bat) Then
        Call Bat.setY(530 - Graphics.CurrentHeight(Bat))
        Call Bat.SetMovementY(0)
    End If
End Sub
Private Sub Draw(Game As AirHockeyGame)
    Call Graphics.ClearScreen_ToColour(Core.GetColor(159, 207, 241))
    Call Graphics.DrawBitmap(Game.TablePics.BackGround, 0, 0)
    Call Graphics.DrawSprite(Game.Ball)
    Call Graphics.DrawSprite(Game.Players(0).Bat)
    
    
    'Call Text.DrawText(Math.Round(Game.Ball.GetMovementVector.getX), red, GameFont("Courier"), 0, 0)
    'Call Text.DrawText(Math.Round(Game.Ball.GetMovementY), red, GameFont("Courier"), 0, 20)
    'Call Text.DrawText((MouseAndKey.GetMouseMovement().getX & ""), red, GameFont("Courier"), 0, 40)
    'Call Text.DrawText((MouseAndKey.GetMouseMovement().getY & ""), red, GameFont("Courier"), 0, 60)
    
    Call MouseAndKey.GetMouseMovement
End Sub
Private Sub StartBall(Game As AirHockeyGame)
    Dim x As Long
    Dim y As Long
    Call Game.Ball.setX(400 - (Graphics.CurrentWidth(Game.Ball) / 2))
    Call Game.Ball.setY(300 - (Graphics.CurrentHeight(Game.Ball) / 2))
    Call Randomize
    x = Int(Rnd * 7) - 3
    y = Int(Rnd * 7) - 3
    Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(x, y))
End Sub
Private Function CreateGame() As AirHockeyGame
    CreateGame.TablePics = CreateTable
    Set CreateGame.Ball = New Sprite
    Set CreateGame.Ball = Graphics.CreateSprite(GameImage("ball"))
    Call CreateGame.Ball.SetUsePixelCollision(True)
    
    Call CreateGame.Ball.SetMass(1)
    
    ReDim CreateGame.Players(1)
    CreateGame.Players(0) = CreatePlayer(0)
    'CreateGame.Players(1) = CreatePlayer(1)
End Function
Private Function CreatePlayer(playernumber As Long) As Player
    Set CreatePlayer.Bat = New Sprite
    Set CreatePlayer.Bat = Graphics.CreateSprite(GameImage(playernumber & "bat"))  'image in here (playernumber & "bat")
    Call CreatePlayer.Bat.SetUsePixelCollision(True)
    Set CreatePlayer.Goal = New Sprite
    Set CreatePlayer.Goal = Graphics.CreateSprite(GameImage(playernumber & "goal"))
    Call CreatePlayer.Goal.SetUsePixelCollision(True)
    CreatePlayer.Score = 0
    
    Call CreatePlayer.Bat.SetMass(2)
    
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
    Call Game.Ball.SetMovementVector(Physics.LimitVector(Game.Ball.GetMovementVector, 15))
    Call Game.Ball.SetMovementVector(Physics.Multiply_Vector(Physics.ScaleMatrix(0.995), Game.Ball.GetMovementVector))
    Call Graphics.MoveSprite(Game.Ball, Game.Ball.GetMovementVector)
    If Physics.HaveSpritesCollided(Game.Ball, Game.Players(0).Bat) Then
        Call Physics.VectorCollision(Game.Ball, Game.Players(0).Bat)
    End If
End Sub
Private Sub KeepBallOnTable(Game As AirHockeyGame)
    If Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableHorizontal) Then
        Call RescueTheBall(Game)
        Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.Ball.GetMovementX, Game.Ball.GetMovementY * -1))
    End If
    
    If Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableVertical) Then
        Call RescueTheBall(Game)
        Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.Ball.GetMovementX * -1, Game.Ball.GetMovementY))
    End If
End Sub

Private Sub RescueTheBall(Game As AirHockeyGame)
    If Game.Ball.getX < 216 Then
        Call Game.Ball.setX(216)
    End If
    If Game.Ball.getX > 584 Then
        Call Game.Ball.setX(584)
    End If
    If Game.Ball.getY < 70 Then
        Call Game.Ball.setY(70)
    End If
    If Game.Ball.getY > 530 Then
        Call Game.Ball.setY(530)
    End If
End Sub

Private Sub StartUp()
    Call Audio.OpenAudio
    Call Core.OpenGraphicsWindow("Air Hockey", 800, 600)
    Call LoadResources
End Sub

Private Sub ShutDown()
    Call FreeResources
    Call Audio.CloseAudio
End Sub
