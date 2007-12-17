Attribute VB_Name = "TitleDisplay"
Public Sub DrawOverlay(title As String)
    Call SwinGame.Graphics.FillRectangleOnScreen(black, 0, 0, 800, 50)
    Call SwinGame.Text.DrawTextOnScreen(title, white, GameResources.GameFont("Courier"), Round((SwinGame.Core.ScreenWidth() / 2) - ((Len(title) / 2) * 10)), 20)
End Sub

