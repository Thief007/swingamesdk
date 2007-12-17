Attribute VB_Name = "DrawSprites"
Private Sub DrawSpriteCaption(ByRef sprite As SGSDKVB6.sprite, caption As String)
    Call SwinGame.Text.DrawText(caption, white, GameResources.GameFont("Courier"), Round((sprite.GetX + SwinGame.Graphics.CurrentWidth(sprite) / 2) - ((Len(caption) / 2) * 10)), Round(sprite.GetY + SwinGame.Graphics.CurrentHeight(sprite)))
End Sub

Public Sub DrawSprites()
    Dim loopSprite As SGSDKVB6.sprite ', reverseSprite, stopSprite, reverseOnceSprite As SGSDKVB6.sprite
    Dim reverseSprite As SGSDKVB6.sprite
    Dim stopSprite As SGSDKVB6.sprite
    Dim reverseOnceSprite As SGSDKVB6.sprite
    Dim frameArray(14) As Long
    Dim i As Long
    For i = 0 To 14
        frameArray(i) = 5
    Next i
    Call SwinGame.Graphics.ClearScreen
    Set loopSprite = New SGSDKVB6.sprite
    Set reverseSprite = New SGSDKVB6.sprite
    Set stopSprite = New SGSDKVB6.sprite
    Set reverseOnceSprite = New SGSDKVB6.sprite
    Set loopSprite = SwinGame.Graphics.CreateSprite_MultiEnding(GameResources.GameImage("Running"), True, frameArray, SpriteEndingAction_Loop, 80, 94)
    Set reverseSprite = SwinGame.Graphics.CreateSprite_MultiEnding(GameResources.GameImage("Running"), True, frameArray, SpriteEndingAction_ReverseLoop, 80, 94)
    Set stopSprite = SwinGame.Graphics.CreateSprite_MultiEnding(GameResources.GameImage("Running"), True, frameArray, SpriteEndingAction_ReverseOnce, 80, 94)
    Set reverseOnceSprite = SwinGame.Graphics.CreateSprite_MultiEnding(GameResources.GameImage("Running"), True, frameArray, SpriteEndingAction_Stop, 80, 94)
    Call loopSprite.SetX(50)
    Call loopSprite.SetY(200)
    Call reverseSprite.SetX(150)
    Call reverseSprite.SetY(200)
    Call stopSprite.SetX(350)
    Call stopSprite.SetY(200)
    Call reverseOnceSprite.SetX(450)
    Call reverseOnceSprite.SetY(200)
    
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        Call Randomize
        Call SwinGame.Graphics.ClearScreen
        
        Call SwinGame.Graphics.DrawSprite(loopSprite)
        Call SwinGame.Graphics.DrawSprite(reverseSprite)
        Call SwinGame.Graphics.DrawSprite(stopSprite)
        Call SwinGame.Graphics.DrawSprite(reverseOnceSprite)
        Call DrawSpriteCaption(loopSprite, "Loop")
        Call DrawSpriteCaption(reverseSprite, "ReverseLoop")
        Call DrawSpriteCaption(stopSprite, "Stop")
        Call DrawSpriteCaption(reverseOnceSprite, "ReverseOnce")
        Call SwinGame.Graphics.UpdateSprite(loopSprite)
        Call SwinGame.Graphics.UpdateSprite(reverseSprite)
        Call SwinGame.Graphics.UpdateSprite(stopSprite)
        Call SwinGame.Graphics.UpdateSprite(reverseOnceSprite)
        
        Call TitleDisplay.DrawOverlay("Drawing Bitmap Example")
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
