Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    'Your Code goes in here
    
    
    
    
    Call Core.OpenGraphicsWindow("VB6 Start Pack", 800, 600)
    Call Audio.OpenAudio
    Call LoadResources
    Do Until Core.WindowCloseRequested
        Call Text.DrawText("Hello World", blue, GameFont("Courier"), 10, 10)
        Call Core.ProcessEvents
        Call Core.RefreshScreen_WithFrame(60)
    Loop
    Call Audio.CloseAudio
End Sub



