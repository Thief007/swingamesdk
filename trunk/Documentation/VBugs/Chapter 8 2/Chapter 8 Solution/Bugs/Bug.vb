Public Class Bug
    Private AliveSprite As Sprite
    Private DeadSprite As Sprite
    Private Alive As Boolean
    Public Property IsAlive() As Boolean
        Get
            Return Alive 'allows the reading of the value
        End Get
        Set(ByVal value As Boolean)
            Alive = value 'allows the assigning of a value
        End Set
    End Property
    Public Sub New()
        'When a new bug is created then set it with the following properties, this is a "Constructor"
        Alive = True
        AliveSprite = Graphics.CreateSprite(GameImage("sprite"))
        AliveSprite.X = Rnd() * (800 - AliveSprite.Width)
        AliveSprite.Y = Rnd() * (600 - AliveSprite.Height)
        AliveSprite.Movement.X = Rnd() * 2 - 1
        AliveSprite.Movement.Y = Rnd() * 2 - 1

        DeadSprite = Graphics.CreateSprite(GameImage("deadBug"), 20, 10, 57, 43)
        DeadSprite.EndingAction = SpriteEndingAction.Stop
    End Sub
    Public Sub Draw()
        'Draw the alive sprite if it is alive and the dead if it's dead
        If IsAlive Then
            Graphics.DrawSprite(AliveSprite)
        Else
            Graphics.DrawSprite(DeadSprite)
        End If
    End Sub
    Public Sub Update()
        'When updating check to see if the sprite is alive, has colided with a wall or has been clicked
        If IsAlive Then
            Graphics.UpdateSprite(AliveSprite)
            CheckCollisions()
            CheckIfClicked()
        Else
            Graphics.UpdateSprite(DeadSprite)
        End If
    End Sub

    Private Sub CheckCollisions()
        'If a sprite collides with a wall, reverse its direction and play the hit sound
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
        'If the bug is clicked then set it to not alive, play the splat noise and play the dead animation
        Dim mousePoint As Point2D
        mousePoint = Input.GetMousePosition()

        If IsAlive And Physics.IsSpriteOnScreenAt(AliveSprite, mousePoint.X, mousePoint.Y) Then
            If Input.MouseWasClicked(MouseButton.LeftButton) Then
                Audio.PlaySoundEffect(GameSound("splat"))
                Alive = False

                'Play the dead sprite animation in place of the bug once
                DeadSprite = Graphics.CreateSprite(GameImage("deadBug"), 40, 10, 57, 43)
                DeadSprite.X = AliveSprite.X
                DeadSprite.Y = AliveSprite.Y
                DeadSprite.EndingAction = SpriteEndingAction.Stop

                'Add to the score and number of bugsKilled
                bugsKilled = bugsKilled + 1
                score = score + time
            End If
        End If
    End Sub

    Public Sub CleanUp()
        'Get rid of unused bugs out of the memory
        DeadSprite.Dispose()
        AliveSprite.Dispose()
    End Sub

End Class
