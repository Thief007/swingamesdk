Module GameLogic

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

        Dim myBug As Bug
        myBug = New Bug

        Dim myBug1 As Bug
        myBug1 = New Bug

        Dim myBug2 As Bug
        myBug2 = New Bug

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            myBug.Draw()
            myBug.Update()

            myBug1.Draw()
            myBug1.Update()

            myBug2.Draw()
            myBug2.Update()

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
