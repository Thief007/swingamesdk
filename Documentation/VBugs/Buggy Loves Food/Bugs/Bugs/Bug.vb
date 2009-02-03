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

    Public Sub MoveBug(ByVal level As Integer)
        If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
            bug.Y = bug.Y - (level * 0.5 + 1)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
            bug.Y = bug.Y + (level * 0.5 + 1)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT) Then
            bug.X = bug.X + (level * 0.5 + 1)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_LEFT) Then
            bug.X = bug.X - (level * 0.5 + 1)
        End If
    End Sub

    Public Sub CheckAndEat(ByVal myFood As Food)
        If Physics.HaveSpritesCollided(bug, myFood.foodSprite) Then
            myFood.WasEaten()
        End If
    End Sub

End Class
