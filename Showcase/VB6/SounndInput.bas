Attribute VB_Name = "SounndInput"
Public Sub SounndInput()
    Dim sound As SoundEffect
    Dim Font1 As Fonts
    Set sound = New SoundEffect
    Set Font1 = New Fonts
    Set sound = GameSound("Shock")
    Set Font1 = GameFont("Courier")
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        If (MouseAndKey.IsKeyPressed(Keys_VK_SPACE)) And (Not Audio.IsSoundEffectPlaying(sound)) Then
            Call Audio.PlaySoundEffect(sound)
        End If
        Call Text.DrawText("Press Space to play a Sound Effect", white, Font1, 210, 300)
        Call TitleDisplay.DrawOverlay("Play Sound Effect when hitting a key Example")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        Call SwinGame.Graphics.ClearScreen
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
