Attribute VB_Name = "GameLogic"
Option Explicit


Sub Run()
    'Your Code goes in here
    
    
    Call LoadResources
    Call Core.OpenGraphicsWindow("VB6 Start Pack", 800, 600)
    
    Call Text.DrawText("Hello World", blue, GameFont("Courier"), 10, 10)
End Sub



