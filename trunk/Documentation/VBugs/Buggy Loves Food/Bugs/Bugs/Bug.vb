Public Class Bug

    Public bug As Sprite
    Public score As Integer

    Public Sub New()
        bug = Graphics.CreateSprite(GameImage("sprite"))
        bug.X = Core.ScreenWidth() / 2 - bug.Width / 2
        bug.Y = Core.ScreenHeight() / 2 - bug.Height / 2
    End Sub

    Public Sub Draw()
        Graphics.DrawSprite(bug)
    End Sub

    Public Sub Update()
        Graphics.UpdateSprite(bug)
    End Sub

    Public Sub MoveBug()
        If Input.IsKeyPressed(SwinGame.Keys.VK_W) Then
            bug.Y = bug.Y - 1
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_S) Then
            bug.Y = bug.Y + 1
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_D) Then
            bug.X = bug.X + 1
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_A) Then
            bug.X = bug.X - 1
        End If

    End Sub

    Public Sub CheckAndEat(ByVal myFood As Food)
        If Physics.HaveSpritesCollided(bug, myFood.foodSprite) Then
            myFood.WasEaten()

        End If
    End Sub

End Class
