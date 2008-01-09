Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        SwinGame.Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        SwinGame.Audio.OpenAudio()

        'Load Resources
        Dim Resources As GameResources
        Resources = New GameResources
        Resources.LoadResources()

        'Game Loop
        Do
            'Clears the Screen to Black
            SwinGame.Graphics.ClearScreen()

            'Hello World
            SwinGame.Text.DrawText("Hello World", Color.White, Resources.GameFont("Courier"), 10, 10)

            'Refreshes the Screen and Processes Input Events
            SwinGame.Core.RefreshScreen()
            SwinGame.Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        Resources.FreeResources()
        SwinGame.Audio.CloseAudio()

    End Sub

End Module
