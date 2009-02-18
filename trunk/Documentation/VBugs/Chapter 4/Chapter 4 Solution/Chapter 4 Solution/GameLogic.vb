Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        'BUG 1 INFORMATION!!!!
        'bug 1 variable
        Dim bug As Sprite
        bug = Graphics.CreateSprite(GameImage("sprite"))

        'bug 1 movement 
        bug.Movement.X = 0.5
        bug.Movement.Y = 0.5

        'BUG 2 INFORMATION!!!!!
        'bug 2 variable
        Dim bug1 As Sprite
        bug1 = Graphics.CreateSprite(GameImage("sprite"))

        'bug 2 positioning in the middle of the screen
        bug1.X = 400 - bug1.Width / 2
        bug1.Y = 300 - bug1.Height / 2

        'bug 2 movement
        bug1.Movement.X = -0.7
        bug1.Movement.Y = 0.7

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Point2D variable declaration - for mouse position

        Audio.PlaySoundEffect(GameSound("hit"), 3)
        Audio.PlaySoundEffect(GameSound("hit"), 10, 5)

        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)


            'BUG 1 MOVEMENT!
            'Draws and Updates he bug
            Graphics.DrawSprite(bug)
            Graphics.UpdateSprite(bug)

            'collision with the right edge of the screen
            If bug.X + bug.Width >= Core.ScreenWidth Then
                bug.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the left edge of the screen
            If bug.X <= 0 Then
                bug.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the bottom edge of the screen
            If bug.Y + bug.Height >= Core.ScreenHeight Then
                bug.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the top edge of the screen
            If bug.Y <= 0 Then
                bug.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            'BUG 2 MOVEMENT!!
            'Draws and updates the bug
            Graphics.DrawSprite(bug1)
            Graphics.UpdateSprite(bug1)

            'collision with the right edge of the screen
            If bug1.X + bug1.Width >= Core.ScreenWidth Then
                bug1.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the left edge of the screen
            If bug1.X <= 0 Then
                bug1.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the bottom edge of the screen
            If bug1.Y + bug1.Height >= Core.ScreenHeight Then
                bug1.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the top edge of the screen
            If bug1.Y <= 0 Then
                bug1.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            'if "m" was typed - stop or start music
            If Input.WasKeyTyped(SwinGame.Keys.VK_M) Then
                If Audio.IsMusicPlaying() = True Then
                    Audio.StopMusic()
                Else
                    Audio.PlayMusic(GameMusic("lion"), -1)
                End If
            End If

            If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
                Audio.SetMusicVolume(Audio.MusicVolume + 0.01F)
            End If

            If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
                Audio.SetMusicVolume(Audio.MusicVolume - 0.01F)
            End If


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        Audio.CloseAudio()
        Music.Stop()
        FreeResources()




    End Sub

End Module
