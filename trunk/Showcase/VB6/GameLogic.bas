Attribute VB_Name = "GameLogic"
Option Explicit
Sub Run()
    Call SwinGame.Load
    Call SwinGame.Audio.OpenAudio
    Call SwinGame.Core.OpenGraphicsWindow("window", 800, 600)
    Call GameResources.LoadResources
    Call DrawLines.DrawLines
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawRectangles.DrawRectangles
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawCircles.DrawCircles
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawEllipses.DrawEllipses
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawBitmaps.DrawBitmaps
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawSprites.DrawSprites
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawCollisionDetection.DrawCollisionDetection
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call PlayMusic.PlayMusic
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawRandomText.DrawRandomText
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawVectorCollision.DrawVectorCollision
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call MoveSpriteWithInput.MoveSpriteWithInput
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call MouseCursor.MouseCursor
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call TextReadExample.TextReadExample
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call SounndInput.SounndInput
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DroppingBall.DroppingBall
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    
    Call VectorAngleExample.VectorAngleExample
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call FollowSpriteExample.FollowSpriteExample
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call MultiBitmapSprite.MultiBitmapSprite
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call DrawCircleWithLines.DrawCircleWithLines
    If SwinGame.Core.WindowCloseRequested() = True Then
        Call ShutDown
        Exit Sub
    End If
    Call MapExample.MapExample
    
    Call ShutDown

End Sub

Private Sub ShutDown()
    Call FreeResources
    Call Audio.CloseAudio
End Sub
