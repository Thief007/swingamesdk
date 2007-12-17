Attribute VB_Name = "DrawRandomText"
Private Function GetRandomFontStyle() As SGSDKVB6.FontStyle
    Dim Random As Long
    Call Randomize
    Random = Int(Rnd * 4)
    Select Case Random
    Case 0
        GetRandomFontStyle = FontStyle_NormalFont
    Case 1
        GetRandomFontStyle = FontStyle_BoldFont
    Case 2
        GetRandomFontStyle = FontStyle_ItalicFont
    Case 3
        GetRandomFontStyle = FontStyle_UnderlineFont
    End Select

End Function

Public Sub DrawRandomText()
    Dim random1 As Single
    Dim random2 As Single
    Dim random3 As Single
    Dim random4 As Single
    Call SwinGame.Graphics.ClearScreen
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        random1 = (Rnd * SwinGame.Core.ScreenWidth)
        random2 = (Rnd * SwinGame.Core.ScreenHeight)
        random3 = (Rnd * SwinGame.Core.ScreenWidth)
        random4 = (Rnd * SwinGame.Core.ScreenHeight)
        Call SwinGame.Text.SetFontStyle(GameResources.GameFont("Courier"), GetRandomFontStyle)
        Call SwinGame.Text.DrawText("SwinGameSDK!", Colour.GetRandomColour, GameResources.GameFont("Courier"), random1, random2)
        Call SwinGame.Text.SetFontStyle(GameResources.GameFont("Courier"), GetRandomFontStyle)
        Call SwinGame.Text.DrawText("SwinGameSDK!", Colour.GetRandomColour, GameResources.GameFont("Courier"), random3, random4)
        Call SwinGame.Text.SetFontStyle(GameResources.GameFont("Courier"), FontStyle_NormalFont)
        Call TitleDisplay.DrawOverlay("Drawing Random Texts")
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
End Sub
