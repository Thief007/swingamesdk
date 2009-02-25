Module GameLogic
    Private listBugs As List(Of Bug)
    Private scorePenalty As Integer
    Private level As Integer
    Private gameTimer As Timer
    Private endLevelAt As Integer
    Public bugsKilled As Integer
    Public score As Integer
    Public time As Integer


    Public Sub ControlMusic()
        'if "m" was typed - stop or start music
        If Input.WasKeyTyped(SwinGame.Keys.VK_M) Then
            If Audio.IsMusicPlaying() = True Then
                Audio.StopMusic()
            Else
                Audio.PlayMusic(GameMusic("lion"), -1)
            End If
        End If
    End Sub

    Public Sub ChangeVolume()
        'If Up is pressed put the music up if down, put the music down
        If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
            Audio.SetMusicVolume(Audio.MusicVolume + 0.01F)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
            Audio.SetMusicVolume(Audio.MusicVolume - 0.01F)
        End If
    End Sub

    Public Sub DrawMouse()
        'Replace the mouse pointer with the target picture
        Dim mousePoint As Point2D
        mousePoint = Input.GetMousePosition() 'gets mouse position at a particular time
        Graphics.DrawBitmapOnScreen(GameImage("target"), mousePoint.X - 20, mousePoint.Y - 20) 'draws the new mouse pointer
    End Sub

    Public Sub LevelSetUp()
        'At the start of each new level stop the timer and clear all the bugs out of the memory
        Core.StopTimer(gameTimer)
        For Each Bug As Bug In listBugs
            Bug.CleanUp()
        Next
        listBugs.Clear()

        'Times whatever level it is by 2 and add that many bugs to the screen
        For i As Integer = 1 To level * 2
            listBugs.Add(New Bug)
        Next

        '1000 is around 1 second, multiply the level by 500 and minus number from 10,000 therefore - 
        '- each level is half a second shorter than the last
        endLevelAt = 10000 - 500 * (level - 1)

        'if we go far enough in the game with the shorter and shorter levels, then the shortes the level -
        'can ever be is 500 otherwise the game is unplayable
        If endLevelAt < 500 Then
            endLevelAt = 500
        End If

        'Start the game timer
        Core.StartTimer(gameTimer)

        'Draw the level splash screen "level 1...3 etc"
        DrawLevelIntro()
    End Sub

    Public Function EndOfLevel() As Boolean
        'For each bug in the list, test to see if it is alive, this means the level will end if they-
        'are all dead
        For Each myBug As Bug In listBugs
            If myBug.IsAlive Then
                Return False
            End If
        Next
        Return True
    End Function

    Public Sub DrawLevelIntro()
        'This is the screen that comes up at the start of each level
        For i As Integer = 1 To 50
            Graphics.ClearScreen(Color.Black)
            Text.DrawText("Level " & level, Color.Green, GameFont("bear"), 280, 200)
            Text.DrawText("Score: " & score, Color.Green, GameFont("cat_scratch"), 320, 300)
            Core.RefreshScreen(25)
            Core.ProcessEvents()
        Next
    End Sub
    Public Sub Penalty()
        'If the timer runs out this is used to draw a red reactangel say too slow and how much score they lose
        For i As Integer = 1 To 25
            Graphics.FillRectangle(Color.FromArgb(5, 255, 0, 0), 0, 0, 800, 600)
            Text.DrawText("Too Slow!! ", Color.Black, GameFont("bear"), 190, 200)
            Text.DrawText("- " & scorePenalty, Color.Black, GameFont("cat_scratch"), 310, 300)
            Core.RefreshScreen(25)
            Core.ProcessEvents()
        Next
    End Sub

    Public Sub DrawLoser()
        'This is the screen that comes up when you lose
        Do
            Graphics.ClearScreen(Color.White)
            Text.DrawText("YOU ", Color.Green, GameFont("bear"), 80, 150)
            Text.DrawText("LOOOOSE!", Color.Red, GameFont("bear_huge"), 275, 125)
            Text.DrawText("You Reached Level " & level, Color.Green, GameFont("cat_scratch"), 200, 340)
            Text.DrawText("And Killed " & bugsKilled & " Bugs", Color.Green, GameFont("cat_scratch"), 240, 400)
            Text.DrawText("Press SPACE to play again", Color.Green, GameFont("cat_scratch"), 120, 480)
            Core.RefreshScreen(30)
            Core.ProcessEvents()
        Loop Until Input.WasKeyTyped(Keys.VK_SPACE) Or SwinGame.Core.WindowCloseRequested() = True

        If Input.WasKeyTyped(Keys.VK_SPACE) Then
            score = 0
            level = 1
            LevelSetUp()
        End If

    End Sub

    Public Sub Main()

        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Hide the mouse pointer
        Input.ShowMouse(False)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        'Initiate a random number sequence based on the computer clock that will generate a list of -
        'random numbers to be used to randomise the location, direction and speed of the bugs
        Randomize()

        'create a new list objet of bugs
        listBugs = New List(Of Bug)

        'Initiate timer
        gameTimer = Core.CreateTimer()


        'Initiate level and score
        level = 1
        LevelSetUp()
        score = 0

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Game Loop

        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            'set the time to display
            time = (endLevelAt - Core.GetTimerTicks(gameTimer)) / 100

            'Draw the time, level and number of bugs killed to the screen
            Text.DrawText("Level: " & level, Color.Green, GameFont("cat_scratch"), 320, 2)
            Text.DrawText("Time: " & time, Color.Red, GameFont("comic"), 2, 2)
            Text.DrawText("Bugs killed: " & bugsKilled, Color.Green, GameFont("comic"), 2, 60)
            Text.DrawText("Score: " & score, Color.Green, GameFont("comic"), 2, 30)


            'Draw and update each bug on the list
            For Each Bug As Bug In listBugs
                Bug.Draw()
                Bug.Update()
            Next

            'If the time runs out subtract one from the score, stop the timer, draw the start of the level and start the timer
            If time < 0 Then
                scorePenalty = 100 * level
                score = score - scorePenalty
                Core.StopTimer(gameTimer)
                Core.StartTimer(gameTimer)
                Penalty()
            End If

            'If they run out of score then
            If score < 0 Then
                DrawLoser()
            End If

            'If the player gets to the end of the level go to thenext level.
            If EndOfLevel() = True Then
                level = level + 1
                LevelSetUp()
            End If

            'Calls the DrawMouse procedure that replaces the cursor with the target
            DrawMouse()

            'Calls the ControlMusic sub that enables the user to turn the music off and on
            ControlMusic()

            'Calls the ChangeVolume sub which enables the user to put the music volume up and down
            ChangeVolume()

            'Refreshes the Screen and Processes Input Events such as mouse movement
            Core.RefreshScreen()
            Core.ProcessEvents()

            'keep looping untill the window is closed
        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        Music.Stop()
        FreeResources()
        Audio.CloseAudio()
    End Sub

End Module
