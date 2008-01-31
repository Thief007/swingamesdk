Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        'Game Loop
        Do
            'Clears the Screen to Black
            SwinGame.Graphics.ClearScreen()

            Graphics.FillRectangle(Color.Red, 20, 200, 200, 100)
            Graphics.FillRectangle(Color.Green, 220, 200, 200, 100)
            Graphics.FillRectangle(Color.Blue, 420, 200, 200, 100)

            Text.DrawText("Hello World", Color.Red, GameFont("Courier"), 20, 310)
            Text.DrawText("Hello World", Color.Green, GameFont("Courier"), 220, 310)
            Text.DrawText("Hello World", Color.Blue, GameFont("Courier"), 420, 310)

            Text.DrawFramerate(0, 0, GameFont("Courier"))
            Text.DrawText("Hello World", Color.White, GameFont("ArialLarge"), 50, 50)

            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
