Module GameLogic

    Private listBugs As List(Of Bug)
    Private level As Integer
    Private gameTimer As Timer
    Public score As Integer
    Private endLevelAt As Integer

    Public Sub DrawLevelEnd()
        Do
            Graphics.ClearScreen(Color.White)
            Text.DrawText("You Loose!", Color.Green, GameFont("ArialLarge"), 200, 200)
            Text.DrawText("Level " & level, Color.Green, GameFont("Arial"), 300, 300)
            Text.DrawText("Press ENTER to start", Color.Green, GameFont("Arial"), 180, 500)
            Core.RefreshScreen(30)
            Core.ProcessEvents()
        Loop Until Input.WasKeyTyped(Keys.VK_RETURN) Or SwinGame.Core.WindowCloseRequested() = True
        score = 0
    End Sub

    Public Sub DrawLevelStart()
        For i = 1 To 22
            Graphics.ClearScreen(Color.White)
            Text.DrawText("Level " & level, Color.Green, GameFont("ArialLarge"), 300, 200)
            Text.DrawText("Score: " & score, Color.Green, GameFont("Arial"), 320, 300)
            Core.RefreshScreen(30)
            Core.ProcessEvents()
        Next
    End Sub

    Public Sub LevelSetUp()
        Core.StopTimer(gameTimer)

        DrawLevelStart()

        For Each Bug In listBugs
            Bug.CleanUp()
        Next

        listBugs.Clear()

        For i = 1 To level * 3
            listBugs.Add(New Bug)
        Next

        endLevelAt = 10000 - 500 * (level - 1)
        If endLevelAt < 500 Then endLevelAt = 500

        Core.StartTimer(gameTimer)
    End Sub

    Public Function EndOfLevel() As Boolean
        For Each myBug As Bug In listBugs
            If myBug.IsAlive Then
                Return False
            End If
        Next
        Return True
    End Function

    Public Sub ControlMusic()
        'if "m" was typed - stop or start music
        If Input.WasKeyTyped(SwinGame.Keys.VK_M) Then
            Select Case Audio.IsMusicPlaying()
                Case True
                    Audio.StopMusic()
                Case False
                    Audio.PlayMusic(GameMusic("lion"), -1)
            End Select
        End If
    End Sub

    Public Sub ChangeVolume()
        If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
            Audio.SetMusicVolume(Audio.MusicVolume + 0.01F)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
            Audio.SetMusicVolume(Audio.MusicVolume - 0.01F)
        End If
    End Sub

    Public Sub DrawMouse()
        Dim mousePoint As Point2D

        mousePoint = Input.GetMousePosition() 'gets mouse position at a time
        Graphics.DrawBitmapOnScreen(GameImage("target"), mousePoint.X - 20, mousePoint.Y - 20) 'draws the new mouse pointer
    End Sub

    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()
        Input.ShowMouse(False) 'hides the original ouse pointer
        Randomize()

        listBugs = New List(Of Bug)

        gameTimer = Core.CreateTimer()
        Dim time As Integer

        level = 1
        LevelSetUp()
        score = 0

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            time = (endLevelAt - Core.GetTimerTicks(gameTimer)) / 100

            Text.DrawText("Bugs killed: " & score, Color.Green, GameFont("Courier"), 2, 2)
            Text.DrawText("Time: " & time, Color.Green, GameFont("Courier"), 700, 2)

            For Each Bug In listBugs
                Bug.Draw()
                Bug.Update()
            Next

            If EndOfLevel() Then
                level = level + 1
                LevelSetUp()
            End If

            If time < 0 Then
                score = score - 1
                Core.StopTimer(gameTimer)
                Core.StartTimer(gameTimer)

                If score < 0 Then
                    DrawLevelEnd()
                End If
            End If

            'draws the mouse pointer on the screen.
            DrawMouse()
            ControlMusic()
            ChangeVolume()

            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
