Imports SwinGame

Module GameLogic
    Dim scaleBall As Matrix2D
    Dim scaleBat As Matrix2D

    Public Const TopOfTable As Integer = 68
    Public Const BottomOfTable As Integer = 530
    Public Const RightOfTable As Integer = 685
    Public Const LeftOfTable As Integer = 315
    Public Const MiddleOfTable As Integer = 300 'accors
    Public Const CenterOfTable As Integer = 500 ' up and down

    Public Sub Main()
        scaleBat = Physics.ScaleMatrix(0.9)
        scaleBall = Physics.ScaleMatrix(0.98)

        StartUp()

        Dim game As AirHockeyGame

        game = CreateGame()

        SplashScreen()

        StartBall(game)
        Do
            MoveBall(game)
            MoveBat(game)
            Draw(game)
            If Input.IsKeyPressed(SwinGame.Keys.VK_P) Then
                Input.ShowMouse(True)
                Pause(game)
                Input.ShowMouse(False)
            End If
            If Input.IsKeyPressed(SwinGame.Keys.VK_R) Then
                game.Resetting = True
                Reset(game)
            End If
            If game.Resetting = True Then
                Reset(game)
            End If
            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen(60)
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True Or Input.IsKeyPressed(SwinGame.Keys.VK_ESCAPE)
        ShutDown()

    End Sub

    Private Sub StartUp()
        Audio.OpenAudio()
        Core.OpenGraphicsWindow("Air Hockey", 800, 600)
        Input.MoveMouse(400, 300)
        LoadResources()
    End Sub

    Private Sub SplashScreen()
        Dim i as Integer
        i = 0
        Dim sec As Integer
        sec = 5
        Do Until Input.IsKeyPressed(SwinGame.Keys.VK_RETURN) Or Input.IsKeyPressed(SwinGame.Keys.VK_ESCAPE) Or Core.WindowCloseRequested()
            Graphics.DrawBitmap(GameImage("Menu"), 0, 0)
            If i < 60 * sec Then
                Text.DrawText("To control Air Hockey", Color.White, GameFont("Comic"), 260, 460)
                Text.DrawText("you move the mouse to control the bat", Color.White, GameFont("Comic"), 150, 490)
            Else
                If i < 120 * sec Then
                    Text.DrawText("To pause the game press 'P'", Color.White, GameFont("Comic"), 225, 460)
                Else
                    If i < 180 * sec Then
                        Text.DrawText("To reset the ball if it gets stuck press 'R'", Color.White, GameFont("Comic"), 125, 460)
                    Else
                        If i < 240 * sec Then
                            Text.DrawText("To exit the game press 'Esc'", Color.White, GameFont("Comic"), 215, 460)
                        Else
                            Text.DrawText("Press Enter", Color.White, GameFont("Comic"), 330, 460)
                        End If
                    End If
                End If
            End If
            Core.RefreshScreen(60)
            Core.ProcessEvents()
            i = i + 1
        Loop
    End Sub

    Private Sub ShutDown()
        FreeResources()
        Audio.CloseAudio()
    End Sub

    Private Function CreateGame() As AirHockeyGame
		Dim result As new AirHockeyGame
        result.TablePics = CreateTable()
        result.Ball = New Sprite
        result.Ball = Graphics.CreateSprite(GameImage("ball"))
        result.Ball.UsePixelCollision = True

        result.Ball.Mass = 1

        ReDim result.Players(1)
        result.Players(0) = CreatePlayer(0)
        result.Players(1) = CreatePlayer(1)

        result.Reset = 0
        result.Resetting = False

		return result
    End Function

    Private Function CreatePlayer( playernumber As Integer) As Player
		Dim result as new Player
		
        result.Bat = New Sprite
        result.Bat = Graphics.CreateSprite(GameImage(playernumber & "bat"))
        result.Bat.UsePixelCollision = True
        result.Bat.X = CenterOfTable - Graphics.CurrentWidth(result.Bat) / 2
        result.Bat.Y = 100 - 360 * (playernumber - 1)

        result.Goal = New Sprite
        result.Goal = Graphics.CreateSprite(GameImage(playernumber & "goal"))
        result.Goal.UsePixelCollision = True
        result.Score = 0

        result.Bat.Mass = 5

		return result
    End Function

    Private Function CreateTable() As Table
		Dim result as new Table
		
        result.BackGround = New Bitmap
        result.BackGround = GameImage("Table")
        result.TableHorizontal = New Sprite
        result.TableHorizontal = Graphics.CreateSprite(GameImage("TableHorizontal"))
        result.TableVertical = New Sprite
        result.TableVertical = Graphics.CreateSprite(GameImage("TableVertical"))
        result.TableVertical.UsePixelCollision = True
        result.TableHorizontal.UsePixelCollision = True

		return result
    End Function

    Private Sub StartBall( game As AirHockeyGame)
        Dim x As Integer
        Dim y As Integer
        game.Ball.X = CenterOfTable - (Graphics.CurrentWidth(game.Ball) / 2)
        game.Ball.Y = MiddleOfTable - (Graphics.CurrentHeight(game.Ball) / 2)
        Randomize()
        x = Int(Rnd() * 7) - 3
        y = Int(Rnd() * 7) - 3
        game.Ball.Movement.SetTo(Physics.CreateVector(x, y))
    End Sub

    Private Sub MoveBall( game As AirHockeyGame)
		Dim v as Vector
		
        KeepBallOnTable(game)

		v = game.Ball.Movement.AsVector()
        v = Physics.LimitMagnitude(v, 15)

        game.Ball.Movement.SetTo(Physics.Multiply(scaleBall, v))
        Graphics.MoveSprite(game.Ball, game.Ball.Movement.AsVector())
		
        Dim i As Integer
        For i = 0 To 1
            If Physics.HaveSpritesCollided(game.Ball, game.Players(i).Bat) Then
                Physics.VectorCollision(game.Ball, game.Players(i).Bat)
                Audio.PlaySoundEffect(GameSound("Ball hit bat"))
            End If
        Next
		
        If Physics.HaveSpritesCollided(game.Ball, game.Players(0).Goal) Then
            game.Players(0).Score = game.Players(0).Score + 1
            game.Resetting = True
            Audio.PlaySoundEffect(GameSound("Ball in goal"))
            game.Ball.X = 2000
            game.Ball.Y = 2000
            game.Ball.Movement.SetTo(Physics.CreateVector(0, 0))
        End If
        If Physics.HaveSpritesCollided(game.Ball, game.Players(1).Goal) Then
            game.Players(1).Score = game.Players(1).Score + 1
            game.Resetting = True
            Audio.PlaySoundEffect(GameSound("Ball in goal"))
            game.Ball.X = (2000)
            game.Ball.Y = (2000)
            game.Ball.Movement.SetTo(Physics.CreateVector(0, 0))
        End If
    End Sub

    Private Sub KeepBallOnTable( game As AirHockeyGame)
        If Physics.HaveSpritesCollided(game.Ball, game.TablePics.TableHorizontal) Then
            RescueTheBall(game)
            game.Ball.Movement.SetTo(Physics.CreateVector(game.Ball.Movement.X, game.Ball.Movement.Y * -1))
            Audio.PlaySoundEffect(GameSound("Ball hit side"))
        End If

        If Physics.HaveSpritesCollided(game.Ball, game.TablePics.TableVertical) Then
            RescueTheBall(game)
            game.Ball.Movement.SetTo(Physics.CreateVector(game.Ball.Movement.X * -1, game.Ball.Movement.Y))
            Audio.PlaySoundEffect(GameSound("Ball hit side"))
        End If
    End Sub

    Private Sub RescueTheBall( game As AirHockeyGame)
        If game.Ball.X < LeftOfTable Then
            game.Ball.X = LeftOfTable + 1
        End If
        If (game.Ball.X + Graphics.CurrentWidth(game.Ball)) > RightOfTable Then
            game.Ball.X = (RightOfTable - Graphics.CurrentWidth(game.Ball) - 1)
        End If
        If game.Ball.Y < TopOfTable Then
            game.Ball.Y = (TopOfTable - 1)
        End If
        If (game.Ball.Y + Graphics.CurrentHeight(game.Ball)) > BottomOfTable Then
            game.Ball.Y = (BottomOfTable - Graphics.CurrentHeight(game.Ball) - 1)
        End If
    End Sub
 
   Private Sub Draw( game As AirHockeyGame)
        Graphics.ClearScreen(Core.GetColor(159, 207, 241))
        Graphics.DrawBitmap(game.TablePics.BackGround, 0, 0)
        Graphics.DrawSprite(game.Ball)
        Graphics.DrawSprite(game.Players(0).Bat)
        Graphics.DrawSprite(game.Players(1).Bat)
        Text.DrawText("Red", Color.Black, GameFont("CourierBigger"), 125, 165)
        Text.DrawText("Blue", Color.Black, GameFont("CourierBigger"), 120, 345)
        
		Text.DrawText("" + Str(game.Players(1).Score), Color.White, GameFont("CourierBigger"), 140, 210)
        Text.DrawText("" + Str(game.Players(0).Score), Color.White, GameFont("CourierBigger"), 140, 390)
    End Sub

    Private Sub TopHalf( Bat As Sprite)
        If Bat.Y < TopOfTable Then
            Bat.Y = (TopOfTable - 1)
            Bat.Movement.Y = (Bat.Movement.Y * -1)
        End If
        If Bat.Y > MiddleOfTable - Graphics.CurrentHeight(Bat) Then
            Bat.Y = (MiddleOfTable - Graphics.CurrentHeight(Bat))
            Bat.Movement.Y = 0
        End If
    End Sub
    Private Sub BottomHalf( Bat As Sprite)
        If Bat.Y < MiddleOfTable Then
            Bat.Y = (MiddleOfTable)
            Bat.Movement.Y = (0)
        End If
        If Bat.Y > BottomOfTable - Graphics.CurrentHeight(Bat) Then
            Bat.Y = (BottomOfTable - Graphics.CurrentHeight(Bat) - 1)
            Bat.Movement.Y = (Bat.Movement.Y * -1)
        End If
    End Sub

    Private Sub MoveAI( Bat As Sprite,  Ball As Sprite)
        If Physics.Magnitude(Ball.Movement.AsVector()) < 1 And Ball.Y < MiddleOfTable Then
            AIOffence(Bat, Ball)
        Else
            If Bat.X < (LeftOfTable + Graphics.CurrentWidth(Bat)) Then
                Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(1, 0)))
            Else
                If Bat.X > (RightOfTable - (Graphics.CurrentWidth(Bat) * 2)) Then
                    Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(-1, 0)))
                Else
                    If Bat.Y < (TopOfTable + Graphics.CurrentHeight(Bat)) Then
                        Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(0, 1)))
                    Else
                        If Ball.Y > MiddleOfTable Then
                            'defence mode
                            AIDefence(Bat, Ball)
                        Else
                            'offence mode
                            AIOffence(Bat, Ball)
                        End If
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub AIOffence( Bat As Sprite,  Ball As Sprite)
        If (Bat.Y + (Graphics.CurrentHeight(Bat))) < Ball.Y Then
            Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.GetVectorFromAngle(Physics.CalculateAngle(Bat.X + Graphics.CurrentWidth(Bat) / 2, Bat.Y, Ball.X + Graphics.CurrentWidth(Ball) / 2, Ball.Y + Graphics.CurrentHeight(Ball) / 2), 1.5)))
        Else
            If Physics.Magnitude(Ball.Movement.AsVector()) < 1 Then
                Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.GetVectorFromAngle(Physics.CalculateAngle(Bat.X + Graphics.CurrentWidth(Bat) / 2, Bat.Y, Ball.X + Graphics.CurrentWidth(Ball) / 2, Ball.Y + Graphics.CurrentHeight(Ball) / 2), 1.5)))
            Else
                AIDefence(Bat, Ball)
            End If
        End If
    End Sub
    Private Sub AIDefence( Bat As Sprite,  Ball As Sprite)
        If Bat.X > (RightOfTable - 114 - Graphics.CurrentWidth(Bat)) Then 'bat on right
            Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(-0.5, 0)))
        Else
            If Bat.X < (LeftOfTable + 114) Then  'bat on left
                Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(0.5, 0)))
            Else    'bat in middle

                If (Bat.X + Graphics.CurrentWidth(Bat)) > (Ball.X + Graphics.CurrentWidth(Ball)) Then
                    Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(-0.5, 0)))
                End If
                If (Bat.X + Graphics.CurrentWidth(Bat)) < (Ball.X + Graphics.CurrentWidth(Ball)) Then
                    Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(0.5, 0)))
                End If
            End If
        End If
        If Bat.Y > (TopOfTable + 20) Then 'In front section
            Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(0, -0.5)))
        Else    'in back section
            Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.CreateVector(0, 0)))
        End If
    End Sub

    Private Sub MovePlayer( Bat As Sprite)

        Core.ProcessEvents()
        Bat.Movement.SetTo(Physics.AddVectors(Bat.Movement.AsVector(), Physics.MultiplyVector(Input.GetMouseMovement(), 0.05)))
        Input.HideMouse()
        Input.MoveMouse(Core.ScreenWidth / 2, Core.ScreenHeight / 2)

        Core.ProcessEvents()
        Input.GetMouseMovement()
    End Sub

    Private Sub Pause( game As AirHockeyGame)
        Do Until Input.IsKeyPressed(SwinGame.Keys.VK_RETURN) Or Input.IsKeyPressed(SwinGame.Keys.VK_ESCAPE) Or Core.WindowCloseRequested() Or Input.IsKeyPressed(SwinGame.Keys.VK_N)
            Core.ProcessEvents()
            Graphics.DrawBitmap(GameImage("Menu"), 0, 0)
            Text.DrawText("Paused, press 'Enter'", Color.White, GameFont("Comic"), 265, 460)
            Text.DrawText("For a new game press 'N'", Color.White, GameFont("Comic"), 240, 490)
            Core.RefreshScreen(60)

            If Input.IsKeyPressed(SwinGame.Keys.VK_N) Then
                game.Reset = 200
                game.Players(0).Score = 0
                game.Players(1).Score = 0
                game.Resetting = True
                Reset(game)
            End If
        Loop
    End Sub

    Private Sub Reset( game As AirHockeyGame)
        Dim i as Integer
        game.Reset = game.Reset + 1
        If game.Reset < 60 Then
            Text.DrawText("3", Color.Yellow, GameFont("ArialBig"), 390, 80)
        Else
            If game.Reset < 120 Then
                Text.DrawText("2", Color.Yellow, GameFont("ArialBig"), 390, 80)
            Else
                If game.Reset < 180 Then
                    Text.DrawText("1", Color.Yellow, GameFont("ArialBig"), 390, 80)
                Else
                    game.Reset = 0
                    game.Resetting = False
                    StartBall(game)
                    For i = 0 To 1
                        game.Players(i).Bat.X = (CenterOfTable - Graphics.CurrentWidth(game.Players(i).Bat) / 2)
                        game.Players(i).Bat.Y = (100 - 360 * (i - 1))
                        game.Players(i).Bat.Movement.SetTo(Physics.CreateVector(0, 0))
                    Next
                End If
            End If
        End If
    End Sub

    Private Sub MoveBat(game As AirHockeyGame)
		Dim v as Vector

        MovePlayer(game.Players(0).Bat)
        MoveAI(game.Players(1).Bat, game.Ball)
		
        Dim i as Integer
        For i = 0 To 1
			v = game.Players(i).Bat.Movement.AsVector()
            game.Players(i).Bat.Movement.SetTo(Physics.Multiply(scaleBat, v))
            Graphics.MoveSprite(game.Players(i).Bat, game.Players(i).Bat.Movement.AsVector)
            If Physics.HaveSpritesCollided(game.Players(i).Bat, game.TablePics.TableHorizontal) Or Physics.HaveSpritesCollided(game.Players(i).Bat, game.TablePics.TableVertical) Then
                Audio.PlaySoundEffect(GameSound("Bat hit side"))
            End If
            'side
            If game.Players(i).Bat.X < LeftOfTable Then
                game.Players(i).Bat.X = (LeftOfTable + 1)
                game.Players(i).Bat.Movement.X = (game.Players(i).Bat.Movement.X * -1)
            End If
            If game.Players(i).Bat.X > RightOfTable - Graphics.CurrentWidth(game.Players(i).Bat) Then
                game.Players(i).Bat.X = (RightOfTable - Graphics.CurrentWidth(game.Players(i).Bat) - 1)
                game.Players(i).Bat.Movement.X = (game.Players(i).Bat.Movement.X * -1)
            End If
        Next
        'top and bottom sides
        BottomHalf(game.Players(0).Bat)
        TopHalf(game.Players(1).Bat)
    End Sub
 
End Module
