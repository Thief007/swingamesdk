Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Battle Ships", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        Audio.PlayMusic(GameMusic("Background"))

        'Game Loop
        Do
            HandleUserInput()
            DrawScreen()
        Loop Until SwinGame.Core.WindowCloseRequested() = True Or CurrentState = GameState.Quitting

        'Show Credits

        Audio.StopMusic()
        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
