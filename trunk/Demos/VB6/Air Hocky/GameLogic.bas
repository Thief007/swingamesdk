Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    Call StartUp
    Dim Game As AirHockeyGame
    Game = CreateGame()
    
    
    
    'Do Until game.state = Quit
    '   Select Case game.state
    '        Case AtMenu
    '            Call Menu(game)
    '        Case StartingGame
    '        Case PlayingGame
    '        Case FinshedGame
    '    End Select
        
    'Loop
    
    Call Game.Ball.SetMass(1)
    Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(1.5, 2)) '(0, 4)) '(1.5, 2)) '(4, -2)) '(6, -1))
    Call Game.Ball.setX(250) '150)
    Call Game.Ball.setY(450)
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call Graphics.ClearScreen_ToColour(Core.GetColor(159, 207, 241))
        Call Graphics.DrawBitmap(Game.TablePics.BackGround, 0, 0)
        Call Graphics.MoveSprite(Game.Ball, Game.Ball.GetMovementVector)
        Call KeepBallOnTable(Game)
        Call Graphics.DrawSprite(Game.Ball)
        Call Core.ProcessEvents
        Call Core.RefreshScreen_WithFrame(60)
    Loop
    Call ShutDown
End Sub
Private Sub KeepBallOnTable(Game As AirHockeyGame)
    'If Physics.HaveSpritesCollided(Game.Ball, Game.AIGoal) Then
    '    MsgBox ("Player socres!!")
    'End If
    'If Physics.HaveSpritesCollided(Game.Ball, Game.playerGoal) Then
    '    MsgBox ("Computer socres!!")
    'End If
    If Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableHorizontal) Then
        Call RescueTheBall(Game)
        Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.Ball.GetMovementX, Game.Ball.GetMovementY * -1))
    End If
    
    If Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableVertical) Then
        Call RescueTheBall(Game)
        Call Game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(Game.Ball.GetMovementX * -1, Game.Ball.GetMovementY))
    End If
End Sub


Private Function CreateGame() As AirHockeyGame
    CreateGame.TablePics = CreateTable

    Set CreateGame.Ball = New Sprite
    Set CreateGame.Ball = Graphics.CreateSprite(GameImage("ball"))
    Call CreateGame.Ball.SetUsePixelCollision(True)
End Function
Private Function CreatePlayer() As Player
    'set createplayer(0).Bat
    
    'Set CreateGame.AIGoal = New Sprite
    'Set CreateGame.AIGoal = Graphics.CreateSprite(GameImage("AIGoal"))
    'Set CreateGame.playerGoal = New Sprite
    'Set CreateGame.playerGoal = Graphics.CreateSprite(GameImage("PlayerGoal"))
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


Private Sub StartUp()
    Call Audio.OpenAudio
    Call Core.OpenGraphicsWindow("Air Hockey", 800, 600)
    Call LoadResources
End Sub

Private Sub ShutDown()
    Call FreeResources
    Call Audio.CloseAudio
End Sub

Private Sub RescueTheBall(Game As AirHockeyGame)
    Call Graphics.MoveSprite(Game.Ball, Physics.InvertVector(Game.Ball.GetMovementVector))
    Dim tempVector As Vector
    Set tempVector = New Vector
    Set tempVector = Game.Ball.GetMovementVector
    Do Until Physics.Magnitude(tempVector) < 0.001
        Call Graphics.MoveSprite(Game.Ball, tempVector)
        If Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableHorizontal) Or Physics.HaveSpritesCollided(Game.Ball, Game.TablePics.TableVertical) Then
            Call Graphics.MoveSprite(Game.Ball, Physics.InvertVector(tempVector))
            
        End If
        Set tempVector = Physics.MultiplyVector(tempVector, 0.5)
    Loop
    'Call Graphics.MoveSprite(Game.ball, Physics.InvertVector(tempVector))
End Sub



