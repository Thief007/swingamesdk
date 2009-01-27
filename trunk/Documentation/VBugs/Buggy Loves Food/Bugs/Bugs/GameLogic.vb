Module GameLogic

    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Buggy Loves Food!", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        Dim level1 As New Level("Level 1!")

        Dim myBug As New Bug()

        Dim gameTimer As Timer
        gameTimer = Core.CreateTimer()
        Core.StartTimer(gameTimer)

        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            level1.Draw()
            level1.Update(Core.GetTimerTicks(gameTimer))

            myBug.Draw()
            myBug.Update()
            myBug.MoveBug()

            level1.CheckCollisions(myBug)

            'level1.calculateScore()


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
