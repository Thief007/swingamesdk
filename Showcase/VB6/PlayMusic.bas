Attribute VB_Name = "PlayMusic"
Public Sub PlayMusic()
    Dim musicSource As SGSDKVB6.music
    Set musicSource = New SGSDKVB6.music
    Call SwinGame.Graphics.ClearScreen
    Set musicSource = GameResources.GameMusic("Fast")
    Call SwinGame.Audio.PlayMusic(musicSource)
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call TitleDisplay.DrawOverlay("Music Playback Example")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Audio.StopMusic
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
