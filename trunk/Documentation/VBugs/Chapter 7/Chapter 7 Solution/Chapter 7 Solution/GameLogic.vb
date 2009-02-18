Module GameLogic
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
        If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
            Audio.SetMusicVolume(Audio.MusicVolume + 0.01F)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
            Audio.SetMusicVolume(Audio.MusicVolume - 0.01F)
        End If
    End Sub

    Public Sub DrawMouse()
        Dim mousePoint As Point2D
        mousePoint = Input.GetMousePosition() 'gets mouse position at a particular time
        Graphics.DrawBitmapOnScreen(GameImage("target"), mousePoint.X - 20, mousePoint.Y - 20) 'draws the new mouse pointer
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

        Randomize()

        'declare the myBug as variable type Bug
        Dim myBug As Bug

        'assiciate the myBug variable with the class Bug
        myBug = New Bug

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Game Loop

        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            myBug.Draw()
            myBug.Update()

            DrawMouse()
            ControlMusic()
            ChangeVolume()

            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        Music.Stop()
        FreeResources()
        Audio.CloseAudio()


    End Sub

End Module
