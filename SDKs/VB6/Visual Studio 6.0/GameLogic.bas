Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    'Your Code goes in here
    Call Core.OpenGraphicsWindow("VB6 Start Pack", 800, 600)
    Call Audio.OpenAudio
    Call LoadResources
    Do Until Core.WindowCloseRequested
        Call Graphics.ClearScreen_ToColour(black)
        Call Graphics.FillRectangle(red, 20, 200, 200, 100)
        Call Graphics.FillRectangle(green, 220, 200, 200, 100)
        Call Graphics.FillRectangle(blue, 420, 200, 200, 100)
        Call Text.DrawText("Hello World", red, GameFont("Courier"), 20, 310)
        Call Text.DrawText("Hello World", green, GameFont("Courier"), 220, 310)
        Call Text.DrawText("Hello World", blue, GameFont("Courier"), 420, 310)
        Call Text.DrawFramerate(0, 0, GameFont("Courier"))
        Call Text.DrawText("Hello World", white, GameFont("ArialLarge"), 50, 50)
        Call Core.ProcessEvents
        Call Core.RefreshScreen_WithFrame(60)
    Loop
    Call Audio.CloseAudio
End Sub



