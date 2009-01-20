Attribute VB_Name = "TextReadExample"
Public Sub TextReadExample()
    Call MouseAndKey.StartReadingText(green, 50, GameFont("Courier"), 0, 65)
    Do Until MouseAndKey.IsReadingText = False
        Call Graphics.ClearScreen
        Call Text.DrawText("Please enter a message:", green, GameFont("Courier"), 0, 50)
        Call DrawOverlay("Test Reading Example")
        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
        If Core.WindowCloseRequested Then
            Exit Sub
        End If
    Loop
    Call Text.DrawText("You have entered " + MouseAndKey.TextReadAsASCII, green, GameFont("Courier"), 0, 80)
    Call Core.RefreshScreen_WithFrame(60)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Core.Sleep(20)
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
