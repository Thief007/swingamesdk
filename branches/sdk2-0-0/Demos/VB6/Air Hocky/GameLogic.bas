Attribute VB_Name = "GameLogic"
Option Explicit
Dim scaleBall As Matrix2D
Dim scaleBat As Matrix2D
Public Const TopOfTable As Long = 68
Public Const BottomOfTable As Long = 530
Public Const RightOfTable As Long = 685
Public Const LeftOfTable As Long = 315
Public Const MiddleOfTable As Long = 300 '300 'accors
Public Const CenterOfTable As Long = 500 '400 ' up and down

Dim assa As Boolean

Sub Run()
    Set scaleBall = New Matrix2D
    Set scaleBat = New Matrix2D
    Set scaleBat = Physics.ScaleMatrix(0.9)
    Set scaleBall = Physics.ScaleMatrix(0.98)
    Call StartUp
    Dim game As AirHockeyGame
    game = CreateGame
    Call SplashScreen
    Call StartBall(game)
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call MoveBall(game)
        Call MoveBat(game)
        Call Draw(game)
        If MouseAndKey.IsKeyPressed(Keys_VK_P) Then
            Call MouseAndKey.ShowHideMouse(True)
            Call Pause(game)
            Call MouseAndKey.ShowHideMouse(False)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_R) Then
            game.Resetting = True
            Call Reset(game)
        End If
        If game.Resetting Then
            Call Reset(game)
        End If
        
        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
    Loop
    Call ShutDown
End Sub
Private Sub Pause(game As AirHockeyGame)
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_RETURN) Or MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested() Or MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Core.ProcessEvents
        Call Graphics.DrawBitmap(GameImage("Menu"), 0, 0)
        Call Text.DrawText("Paused, press 'Enter'", white, GameFont("Comic"), 265, 460)
        Call Text.DrawText("For a new game press 'N'", white, GameFont("Comic"), 240, 490)
        Call Core.RefreshScreen_WithFrame(60)
        
        If MouseAndKey.IsKeyPressed(Keys_VK_N) Then
            game.Reset = 200
            game.Players(0).Score = 0
            game.Players(1).Score = 0
            game.Resetting = True
            Call Reset(game)
        End If
    Loop
End Sub


Private Sub SplashScreen()
    Dim i As Long
    i = 0
    Dim sec As Long
    sec = 5
    Do Until MouseAndKey.IsKeyPressed(Keys_VK_RETURN) Or MouseAndKey.IsKeyPressed(Keys_VK_ESCAPE) Or Core.WindowCloseRequested()
        Call Graphics.DrawBitmap(GameImage("Menu"), 0, 0)
        If i < 60 * sec Then
            Call Text.DrawText("To control Air Hockey", white, GameFont("Comic"), 260, 460)
            Call Text.DrawText("you move the mouse to control the bat", white, GameFont("Comic"), 150, 490)
        Else
            If i < 120 * sec Then
                Call Text.DrawText("To pause the game press 'P'", white, GameFont("Comic"), 225, 460)
            Else
                If i < 180 * sec Then
                    Call Text.DrawText("To reset the ball if it gets stuck press 'R'", white, GameFont("Comic"), 125, 460)
                Else
                    If i < 240 * sec Then
                        Call Text.DrawText("To exit the game press 'Esc'", white, GameFont("Comic"), 215, 460)
                    Else
                        Call Text.DrawText("Press Enter", white, GameFont("Comic"), 330, 460)
                    End If
                End If
            End If
        End If
        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
        i = i + 1
    Loop
End Sub

Private Sub Reset(game As AirHockeyGame)
    Dim i As Long
    game.Reset = game.Reset + 1
    If game.Reset < 60 Then
        Call Text.DrawText("3", yellow, GameFont("ArialBig"), 390, 80)
    Else
        If game.Reset < 120 Then
            Call Text.DrawText("2", yellow, GameFont("ArialBig"), 390, 80)
        Else
            If game.Reset < 180 Then
                Call Text.DrawText("1", yellow, GameFont("ArialBig"), 390, 80)
            Else
                game.Reset = 0
                game.Resetting = False
                Call StartBall(game)
                For i = 0 To 1
                    Call game.Players(i).Bat.setX(CenterOfTable - Graphics.CurrentWidth(game.Players(i).Bat) / 2)
                    Call game.Players(i).Bat.setY(100 - 360 * (i - 1))
                    Call game.Players(i).Bat.SetMovementVector(Physics.CreateVector_NoInvert(0, 0))
                Next i
            End If
        End If
    End If
End Sub
Private Sub MoveBat(game As AirHockeyGame)
    Call MovePlayer(game.Players(0).Bat)
    Call MoveAI(game.Players(1).Bat, game.Ball)
    Dim i As Long
    For i = 0 To 1
        Call game.Players(i).Bat.SetMovementVector(Physics.Multiply_Vector(scaleBat, game.Players(i).Bat.GetMovementVector))
        Call Graphics.MoveSprite(game.Players(i).Bat, game.Players(i).Bat.GetMovementVector)
        If Physics.HaveSpritesCollided(game.Players(i).Bat, game.TablePics.TableHorizontal) Or Physics.HaveSpritesCollided(game.Players(i).Bat, game.TablePics.TableVertical) Then
            Call Audio.PlaySoundEffect(GameSound("Bat hit side"))
        End If
        'side
        If game.Players(i).Bat.getX < LeftOfTable Then
            Call game.Players(i).Bat.setX(LeftOfTable + 1)
            Call game.Players(i).Bat.SetMovementX(game.Players(i).Bat.GetMovementX * -1)
        End If
        If game.Players(i).Bat.getX > RightOfTable - Graphics.CurrentWidth(game.Players(i).Bat) Then
            Call game.Players(i).Bat.setX(RightOfTable - Graphics.CurrentWidth(game.Players(i).Bat) - 1)
            Call game.Players(i).Bat.SetMovementX(game.Players(i).Bat.GetMovementX * -1)
        End If
    Next i
    'top and bottom sides
    Call BottomHalf(game.Players(0).Bat)
    Call TopHalf(game.Players(1).Bat)


End Sub
Private Sub TopHalf(Bat As Sprite)
    If Bat.getY < TopOfTable Then
        Call Bat.setY(TopOfTable - 1)
        Call Bat.SetMovementY(Bat.GetMovementY * -1)
    End If
    If Bat.getY > MiddleOfTable - Graphics.CurrentHeight(Bat) Then
        Call Bat.setY(MiddleOfTable - Graphics.CurrentHeight(Bat))
        Call Bat.SetMovementY(0)
    End If
End Sub

Private Sub BottomHalf(Bat As Sprite)
    If Bat.getY < MiddleOfTable Then
        Call Bat.setY(MiddleOfTable)
        Call Bat.SetMovementY(0)
    End If
    If Bat.getY > BottomOfTable - Graphics.CurrentHeight(Bat) Then
        Call Bat.setY(BottomOfTable - Graphics.CurrentHeight(Bat) - 1)
        Call Bat.SetMovementY(Bat.GetMovementY * -1)
    End If
End Sub

Private Sub MoveAI(Bat As Sprite, Ball As Sprite)
    If Physics.Magnitude(Ball.GetMovementVector) < 1 And Ball.getY < MiddleOfTable Then
        Call AIOffence(Bat, Ball)
    Else
        If Bat.getX < (LeftOfTable + Graphics.CurrentWidth(Bat)) Then
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(1, 0)))
        Else
            If Bat.getX > (RightOfTable - (Graphics.CurrentWidth(Bat) * 2)) Then
                Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(-1, 0)))
            Else
                If Bat.getY < (TopOfTable + Graphics.CurrentHeight(Bat)) Then
                    Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0, 1)))
                Else
                    If Ball.getY > MiddleOfTable Then
                        'defence mode
                        Call AIDefence(Bat, Ball)
                    Else
                        'offence mode
                        Call AIOffence(Bat, Ball)
                    End If
                End If
            End If
        End If
    End If
End Sub

Private Sub AIOffence(Bat As Sprite, Ball As Sprite)
    If (Bat.getY + (Graphics.CurrentHeight(Bat))) < Ball.getY Then
        Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.GetVectorFromAngle(Physics.CalculateAngle_Number(Bat.getX + Graphics.CurrentWidth(Bat) / 2, Bat.getY, Ball.getX + Graphics.CurrentWidth(Ball) / 2, Ball.getY + Graphics.CurrentHeight(Ball) / 2), 1.5)))
    Else
        If Physics.Magnitude(Ball.GetMovementVector) < 1 Then
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.GetVectorFromAngle(Physics.CalculateAngle_Number(Bat.getX + Graphics.CurrentWidth(Bat) / 2, Bat.getY, Ball.getX + Graphics.CurrentWidth(Ball) / 2, Ball.getY + Graphics.CurrentHeight(Ball) / 2), 1.5)))
        Else
            Call AIDefence(Bat, Ball)
        End If
    End If
End Sub

Private Sub AIDefence(Bat As Sprite, Ball As Sprite)
    If Bat.getX > (RightOfTable - 114 - Graphics.CurrentWidth(Bat)) Then 'bat on right
        Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(-0.5, 0)))
    Else
        If Bat.getX < (LeftOfTable + 114) Then  'bat on left
            Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0.5, 0)))
        Else    'bat in middle
        
            If (Bat.getX + Graphics.CurrentWidth(Bat)) > (Ball.getX + Graphics.CurrentWidth(Ball)) Then
                Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(-0.5, 0)))
            End If
            If (Bat.getX + Graphics.CurrentWidth(Bat)) < (Ball.getX + Graphics.CurrentWidth(Ball)) Then
                Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0.5, 0)))
            End If
        End If
    End If
    If Bat.getY > (TopOfTable + 20) Then 'In front section
        Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0, -0.5)))
    Else    'in back section
        Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.CreateVector_NoInvert(0, 0)))
    End If
End Sub
Private Sub MovePlayer(Bat As Sprite)
    Call Bat.SetMovementVector(Physics.AddVectors(Bat.GetMovementVector, Physics.MultiplyVector(MouseAndKey.GetMouseMovement, 0.03)))
    Call MouseAndKey.HideMouse
    Call MouseAndKey.MoveMouse(Core.ScreenWidth / 2, Core.ScreenHeight / 2)

    Call Core.ProcessEvents
End Sub
Private Sub Draw(game As AirHockeyGame)
    Call Graphics.ClearScreen_ToColour(Core.GetColor(159, 207, 241))
    Call Graphics.DrawBitmap(game.TablePics.BackGround, 0, 0)
    Call Graphics.DrawSprite(game.Ball)
    Call Graphics.DrawSprite(game.Players(0).Bat)
    Call Graphics.DrawSprite(game.Players(1).Bat)
    Call Text.DrawText("Red", black, GameFont("CourierBigger"), 120, 165)
    Call Text.DrawText("Blue", black, GameFont("CourierBigger"), 120, 345)
    Call Text.DrawText(game.Players(1).Score, black, GameFont("CourierBigger"), 135, 210)
    Call Text.DrawText(game.Players(0).Score, black, GameFont("CourierBigger"), 135, 390)
End Sub
Private Sub StartBall(game As AirHockeyGame)
    Dim x As Long
    Dim y As Long
    Call game.Ball.setX(CenterOfTable - (Graphics.CurrentWidth(game.Ball) / 2))
    Call game.Ball.setY(MiddleOfTable - (Graphics.CurrentHeight(game.Ball) / 2))
    Call Randomize
    x = Int(Rnd * 7) - 3
    y = Int(Rnd * 7) - 3
    Call game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(x, y))
End Sub
Private Function CreateGame() As AirHockeyGame
    CreateGame.TablePics = CreateTable
    Set CreateGame.Ball = New Sprite
    Set CreateGame.Ball = Graphics.CreateSprite(GameImage("ball"))
    Call CreateGame.Ball.SetUsePixelCollision(True)
    
    Call CreateGame.Ball.SetMass(1)
    
    ReDim CreateGame.Players(1)
    CreateGame.Players(0) = CreatePlayer(0)
    CreateGame.Players(1) = CreatePlayer(1)
    
    CreateGame.Reset = 0
    CreateGame.Resetting = False
End Function
Private Function CreatePlayer(playernumber As Long) As Player
    Set CreatePlayer.Bat = New Sprite
    Set CreatePlayer.Bat = Graphics.CreateSprite(GameImage(playernumber & "bat"))
    Call CreatePlayer.Bat.SetUsePixelCollision(True)
    Call CreatePlayer.Bat.setX(CenterOfTable - Graphics.CurrentWidth(CreatePlayer.Bat) / 2)
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
Private Sub MoveBall(game As AirHockeyGame)
    Call KeepBallOnTable(game)
    Call game.Ball.SetMovementVector(Physics.LimitMagnitude(game.Ball.GetMovementVector, 15)) '25
    Call game.Ball.SetMovementVector(Physics.Multiply_Vector(scaleBall, game.Ball.GetMovementVector))
    Call Graphics.MoveSprite(game.Ball, game.Ball.GetMovementVector)
    Dim i As Long
    For i = 0 To 1
        If Physics.HaveSpritesCollided(game.Ball, game.Players(i).Bat) Then
            Call Physics.VectorCollision(game.Ball, game.Players(i).Bat)
            Call Audio.PlaySoundEffect(GameSound("Ball hit bat"))
        End If
    Next i
    If Physics.HaveSpritesCollided(game.Ball, game.Players(0).Goal) Then
        game.Players(0).Score = game.Players(0).Score + 1
        game.Resetting = True
        Call Audio.PlaySoundEffect(GameSound("Ball in goal"))
        Call game.Ball.setX(2000)
        Call game.Ball.setY(2000)
        Call game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(0, 0))
    End If
    If Physics.HaveSpritesCollided(game.Ball, game.Players(1).Goal) Then
        game.Players(1).Score = game.Players(1).Score + 1
        game.Resetting = True
        Call Audio.PlaySoundEffect(GameSound("Ball in goal"))
        Call game.Ball.setX(2000)
        Call game.Ball.setY(2000)
        Call game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(0, 0))
    End If
End Sub


Private Sub KeepBallOnTable(game As AirHockeyGame)
    If Physics.HaveSpritesCollided(game.Ball, game.TablePics.TableHorizontal) Then
        Call RescueTheBall(game)
        Call game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(game.Ball.GetMovementX, game.Ball.GetMovementY * -1))
        Call Audio.PlaySoundEffect(GameSound("Ball hit side"))
    End If
    
    If Physics.HaveSpritesCollided(game.Ball, game.TablePics.TableVertical) Then
        Call RescueTheBall(game)
        Call game.Ball.SetMovementVector(Physics.CreateVector_NoInvert(game.Ball.GetMovementX * -1, game.Ball.GetMovementY))
        Call Audio.PlaySoundEffect(GameSound("Ball hit side"))
    End If
End Sub

Private Sub RescueTheBall(game As AirHockeyGame)
    If game.Ball.getX < LeftOfTable Then
        Call game.Ball.setX(LeftOfTable + 1)
    End If
    If (game.Ball.getX + Graphics.CurrentWidth(game.Ball)) > RightOfTable Then
        Call game.Ball.setX(RightOfTable - Graphics.CurrentWidth(game.Ball) - 1)
    End If
    If game.Ball.getY < TopOfTable Then
        Call game.Ball.setY(TopOfTable - 1)
    End If
    If (game.Ball.getY + Graphics.CurrentHeight(game.Ball)) > BottomOfTable Then
        Call game.Ball.setY(BottomOfTable - Graphics.CurrentHeight(game.Ball) - 1)
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
