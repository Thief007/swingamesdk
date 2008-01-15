Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    Call StartUp
    Dim Game As AirHockeyGame
    Game = CreateGame
    Call StartBall(Game)
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call MoveBall(Game)
        Call Draw(Game)
        Call Core.ProcessEvents
        Call Core.RefreshScreen_WithFrame(60)
    Loop
    Call ShutDown
End Sub
Private Sub Draw(Game As AirHockeyGame)
    Call Graphics.ClearScreen_ToColour(Core.GetColor(159, 207, 241))
    Call Graphics.DrawBitmap(Game.TablePics.BackGround, 0, 0)
    Call Graphics.DrawSprite(Game.Ball)
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
    ReDim CreateGame.Players(1)
    CreateGame.Players(0) = CreatePlayer(0)
    CreateGame.Players(1) = CreatePlayer(1)
End Function
Private Function CreatePlayer(playernumber As Long) As Player
    Set CreatePlayer.Bat = New Sprite
    'Set CreatePlayer.Bat = Graphics.CreateSprite(GameImage("")) 'image in here (playernumber & "bat")
    'Call CreatePlayer.Bat.SetUsePixelCollision(True)
    Set CreatePlayer.Goal = New Sprite
    Set CreatePlayer.Goal = Graphics.CreateSprite(GameImage(playernumber & "goal"))
    Call CreatePlayer.Goal.SetUsePixelCollision(True)
    CreatePlayer.Score = 0
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
    If Game.Ball.getX > 583 Then
        Call Game.Ball.setX(583)
    End If
    If Game.Ball.getY < 70 Then
        Call Game.Ball.setY(70)
    End If
    If Game.Ball.getY > 529 Then
        Call Game.Ball.setY(529)
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
