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

            'Draws the background image
            Graphics.DrawBitmap(GameImage("back"), 0, 0)

            'draws the photo image
            Graphics.DrawBitmap(GameImage("photo"), 100, 30)

            'Draws the text to the screen
            Text.DrawText("I'm a cool bug!", Color.Black, GameFont("BeanTown"), 450, 10)

            'Draws the 3 rectangles
            Graphics.FillRectangle(Color.FromArgb(255, 255, 0, 0), 700, 500, 100, 100)
            Graphics.FillRectangle(Color.FromArgb(30, 0, 255, 0), 600, 500, 100, 100)
            Graphics.FillRectangle(Color.FromArgb(0, 0, 0, 255), 500, 500, 100, 100)

            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
