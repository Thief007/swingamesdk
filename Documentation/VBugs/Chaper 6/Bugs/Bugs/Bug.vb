﻿Public Class Bug
    Private Alive As Boolean
    Private AliveSprite As Sprite
    Private DeadSprite As Sprite

    Public Property IsAlive() As Boolean
        Get
            Return Alive
        End Get
        Set(ByVal value As Boolean)
            Alive = value
        End Set
    End Property

    Public Sub New()
        Alive = True

        If IsAlive Then
            AliveSprite = Graphics.CreateSprite(GameImage("sprite"))
            AliveSprite.X = Rnd() * (800 - AliveSprite.Width)
            AliveSprite.Y = Rnd() * (600 - AliveSprite.Height)
            AliveSprite.Movement.X = Rnd() * 2 - 1
            AliveSprite.Movement.Y = Rnd() * 2 - 1
            'Else
            '    DeadSprite = Graphics.CreateSprite(GameImage("deadBug"), 20, 10, 57, 43)
            '    DeadSprite.EndingAction = SpriteEndingAction.Stop
        End If
    End Sub

    Public Sub Draw()
        If IsAlive Then
            Graphics.DrawSprite(AliveSprite)
        Else
            Graphics.DrawSprite(DeadSprite)
        End If
    End Sub

    Public Sub Update()
        If IsAlive Then
            Graphics.UpdateSprite(AliveSprite)
            CheckCollisions()
            CheckIfClicked()
        Else
            Graphics.UpdateSprite(DeadSprite)
        End If
    End Sub

    Private Sub CheckCollisions()
        If AliveSprite.X + AliveSprite.Width >= Core.ScreenWidth Or AliveSprite.X <= 0 Then
            AliveSprite.Movement.X = -AliveSprite.Movement.X
            Audio.PlaySoundEffect(GameSound("hit"))
        End If
        
        If AliveSprite.Y + AliveSprite.Height >= Core.ScreenHeight Or AliveSprite.Y <= 0 Then
            AliveSprite.Movement.Y = -AliveSprite.Movement.Y
            Audio.PlaySoundEffect(GameSound("hit"))
        End If
    End Sub

    Private Sub CheckIfClicked()
        Dim mousePoint As Point2D
        mousePoint = Input.GetMousePosition()

        'plays squash when mouse was clicked on the bug2
        If IsAlive And Physics.IsSpriteOnScreenAt(AliveSprite, mousePoint.X, mousePoint.Y) Then
            If Input.MouseWasClicked(MouseButton.LeftButton) Then
                Audio.PlaySoundEffect(GameSound("hit1"))
                Alive = False

                DeadSprite = Graphics.CreateSprite(GameImage("deadBug"), 20, 10, 57, 43)
                DeadSprite.EndingAction = SpriteEndingAction.Stop

                DeadSprite.X = AliveSprite.X
                DeadSprite.Y = AliveSprite.Y
            End If
        End If
    End Sub
End Class