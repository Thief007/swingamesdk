Public Class Apple : Inherits Food

    Public Sub New(ByVal time As Integer)
        MyBase.New(time, GameImage("appleap"), GameImage("applean"))
    End Sub

    Public Overrides Sub AddScore()
        score = score + 1
        Audio.PlaySoundEffect(GameSound("crunch"))
    End Sub

End Class
