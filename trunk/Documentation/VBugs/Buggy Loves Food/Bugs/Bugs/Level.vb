Public Class Level
    Public background As Bitmap
    Public levelName As String
    Public levelFood As List(Of Food)
    Public createTime As Integer
    Public percentBad As Double
    Public timeToCompleteLevel As Integer
    Public scoreToCompleteLevel As Integer

    Public Sub New(ByVal levelName As String, ByVal background As String)
        Me.levelName = levelName
        Me.background = GameImage(background)
        levelFood = New List(Of Food)
        createTime = 0
        percentBad = 0.1
    End Sub

    Public Sub Draw()
        Graphics.DrawBitmap(background, 0, 0)
        Text.DrawText("Level Name: " & levelName, Color.Black, GameFont("Courier"), 0, 0)
        Text.DrawText("Score:  " & score, Color.Black, GameFont("Courier"), 550, 0)

        For Each myFood In levelFood
            myFood.Draw()
        Next

    End Sub

    Public Sub Update(ByVal time As Integer)
        Dim toRemove As New List(Of Food)
        Dim toRemoveBad As New List(Of Food)

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
            AddFood(time)
            createTime = time + 1000
        End If


    End Sub

    Public Sub AddFood(ByVal time As Integer)
        If Rnd() < percentBad Then
            levelFood.Add(New BadFood(time))
        Else
            levelFood.Add(New Apple(time))
        End If
    End Sub

    Public Sub CheckCollisions(ByVal myBug As Bug)
        For Each myFood In levelFood
            If Not myFood.isDead And Not myFood.isEaten Then
                myBug.CheckAndEat(myFood)
            End If
        Next

    End Sub

    Function CheckEndLevel(ByVal gameTimer As Timer) As Boolean
        If Core.GetTimerTicks(gameTimer) > timeToCompleteLevel Or score >= scoreToCompleteLevel Then
            Core.StopTimer(gameTimer)
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub endLevel()
        Do
            Graphics.ClearScreen(Color.Black)
            Text.DrawText("Press Enter to Start Again...", Color.White, GameFont("Courier"), 200, 200)
            Core.RefreshScreen(30)
            Core.ProcessEvents()
        Loop Until Input.WasKeyTyped(Keys.VK_RETURN) Or SwinGame.Core.WindowCloseRequested() = True
        score = 0
    End Sub

End Class
