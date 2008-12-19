
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
            'Solution for "Change Color of the background" excercize: put color declaration inside the brackets - Color.LightSkyBlue
            SwinGame.Graphics.ClearScreen(Color.White)

            'Solution for "Change location of the rectangle" excercize: 20, 150 changed to 100, 250
            'Solution for "Change size" excercize: 500, 50 changed to 630, 20
            'Solution for "Change Color of the rectangle": Color.Red ahsnged to Color.Blue
            Graphics.FillRectangle(Color.Blue, 100, 250, 630, 20)

            'Solution for "Change the text" excercize: "Hello World!" text changed to "Hello Your Name!"
            'Solution for "Change Location of the text" excercize: 50, 50 at the and of sub call changed to 100, 150 
            'Solution for "Change Color of the text": Color.Aqua ahsnged to Color.GreenYellow
            Text.DrawText("Hello Your Name!", Color.GreenYellow, GameFont("ArialLarge"), 100, 150)

            'Solution for Extra Exercise
            'Draw yellow circle on the screen – “face”. 
            Graphics.FillEllipseOnScreen(Color.Yellow, 100, 250, 100, 100)

            'Draw eyes – two black circles inside the yellow circle
            Graphics.FillEllipseOnScreen(Color.Black, 125, 280, 15, 15)
            Graphics.FillEllipseOnScreen(Color.Black, 160, 280, 15, 15)

            'Draw “smile” on the screen. 
            Graphics.DrawLineOnScreen(Color.Black, 130, 310, 150, 330)
            Graphics.DrawLineOnScreen(Color.Black, 170, 310, 150, 330)



            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()
        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
