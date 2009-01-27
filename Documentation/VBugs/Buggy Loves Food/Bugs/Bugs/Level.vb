Public Class Level
    Public background As Bitmap
    Public levelName As String
    Public levelFood As List(Of Food)
    Public createTime As Integer

    Public Sub New(ByVal levelName As String)
        Me.levelName = levelName
        Me.background = GameImage(levelName)
        levelFood = New List(Of Food)
        createTime = 0
    End Sub

    Public Sub Draw()
        Graphics.DrawBitmap(background, 0, 0)
        Text.DrawText(levelName, Color.Black, GameFont("Courier"), 0, 0)

        For Each myFood In levelFood
            myFood.Draw()
        Next
    End Sub

    Public Sub Update(ByVal time As Integer)
        Dim toRemove As New List(Of Food)

        For Each myFood In levelFood
            myFood.Update(time)
            If myFood.IsDead Then
                toRemove.Add(myFood)
            End If
        Next

        'Remove all the food that is dead
        For Each myFood In toRemove
            levelFood.Remove(myFood)
        Next

        If createTime < time Then
            levelFood.Add(New Food(time))
            createTime = time + 1000
        End If
    End Sub

    Public Sub CheckCollisions(ByVal myBug As Bug)
        For Each myFood In levelFood
            If Not myFood.isDead And Not myFood.isEaten Then
                myBug.CheckAndEat(myFood)
            End If
        Next
    End Sub

    'Public Function calculateScore(ByVal myBug As Sprite, ByVal apple As Sprite) As Integer
    '    If Physics.HaveSpritesCollided(myBug, apple) Then
    '        score = score + 1
    '        apple.eatApple()
    '        Return score
    '    End If

    'End Function

    'Public Sub displayScore()
    '    Text.DrawText("Score:", Color.Black, GameFont("Courier"), 20, 50)
    '    Text.DrawText(score, Color.Black, GameFont("Courier"), 150, 50)
    'End Sub


End Class
