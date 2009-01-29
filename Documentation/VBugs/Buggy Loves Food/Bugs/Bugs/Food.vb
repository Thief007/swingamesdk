Public Class Food

    Public foodSprite As Sprite
    Public eatenImg As Bitmap
    Private timeCreated As Integer
    Private timeToDie As Integer
    Private isDying As Boolean
    Public isDead As Boolean
    Public isEaten As Boolean

    Public Sub New(ByVal time As Integer, ByVal img As Bitmap, ByVal toEat As Bitmap)
        timeCreated = time
        timeToDie = timeCreated + (Rnd() * 10000)

        isDying = False
        isDead = False
        isEaten = False
        eatenImg = toEat

        foodSprite = Graphics.CreateSprite(img, 20, 6, 57, 43)
        foodSprite.EndingAction = SpriteEndingAction.Stop
        foodSprite.X = Rnd() * (Core.ScreenWidth() - foodSprite.Width)
        foodSprite.Y = Rnd() * (Core.ScreenHeight() - foodSprite.Height)
    End Sub

    Public Sub Draw()
        If Not isDead Then
            Graphics.DrawSprite(foodSprite)
        End If

    End Sub

    Public Sub Update(ByVal time As Integer)
        Graphics.UpdateSprite(foodSprite)

        If isEaten Or isDying Then
            If foodSprite.hasEnded Then
                isDead = True
                foodSprite.Dispose()
            End If
        ElseIf time > timeToDie Then
            isDying = True
            'Reverse the animation once
            foodSprite.EndingAction = SpriteEndingAction.ReverseOnce
            'Replaying the animation
            foodSprite.ReplayAnimation()
            'Starting from the 5th frame
            foodSprite.CurrentFrame = 5
        End If


    End Sub

    Public Sub WasEaten()
        If Not isEaten Then
            isEaten = True
            Dim x, y As Integer
            x = foodSprite.X
            y = foodSprite.Y
            foodSprite.Dispose()

            foodSprite = Graphics.CreateSprite(eatenImg, 20, 6, 57, 43)
            foodSprite.EndingAction = SpriteEndingAction.Stop
            foodSprite.X = x
            foodSprite.Y = y

            AddScore()
        End If
    End Sub

    Public Overridable Sub AddScore()
        score = score + 1
    End Sub

End Class
