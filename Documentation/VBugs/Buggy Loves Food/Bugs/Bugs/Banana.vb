Public Class Banana : Inherits Food

    Public Sub New(ByVal time As Integer)
        MyBase.New(time, GameImage("bananaap"), GameImage("bananaan"))
    End Sub

    Public Overrides Sub AddScore()
        score = score + 3
    End Sub

End Class
