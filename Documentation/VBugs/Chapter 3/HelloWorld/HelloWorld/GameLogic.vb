Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        Dim bug As Sprite
        bug = Graphics.CreateSprite(GameImage("sprite"))

        Dim bug1 As Sprite
        bug1 = Graphics.CreateSprite(GameImage("sprite"))

        bug.X = 400 - bug.Width / 2
        bug.Y = 300 - bug.Height / 2

        bug1.X = 400 - bug1.Width / 2
        bug1.Y = 300 - bug1.Height / 2


        bug.Movement.X = 0.5
        bug.Movement.Y = 0.5

        bug1.Movement.X = -0.7
        bug1.Movement.Y = 0.7

        'Game Loop
        Do
            'Clears the Screen to Black
            SwinGame.Graphics.ClearScreen(Color.White)

            Graphics.DrawSprite(bug)

            Graphics.UpdateSprite(bug)

            Graphics.DrawSprite(bug1)

            Graphics.UpdateSprite(bug1)

            If bug.X + bug.Width >= Core.ScreenWidth Then
                bug.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            If bug.X <= 0 Then
                bug.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            If bug.Y + bug.Height >= Core.ScreenHeight Then
                bug.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            If bug.Y <= 0 Then
                bug.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            If bug1.X + bug1.Width >= Core.ScreenWidth Then
                bug1.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit1"))
            End If

            If bug1.X <= 0 Then
                bug1.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit1"))
            End If

            If bug1.Y + bug1.Height >= Core.ScreenHeight Then
                bug1.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit1"))
            End If

            If bug1.Y <= 0 Then
                bug1.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit1"))
            End If


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        Graphics.FreeSprite(bug)
        Graphics.FreeSprite(bug1)
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
