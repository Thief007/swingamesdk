Public Class Apple : Inherits Food

    Public Sub New(ByVal time As Integer)
        MyBase.New(time, GameImage("appleap"), GameImage("applean"))
    End Sub

End Class
