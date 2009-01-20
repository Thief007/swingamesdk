Imports SwinGame

Public Class Player
    Public Bat As Sprite
    Public Score As Integer
    Public Goal As Sprite
End Class

Public Class Table
    Public BackGround As Bitmap
    Public TableHorizontal As Sprite
    Public TableVertical As Sprite
End Class

Public Class AirHockeyGame
    Public Players() As Player
    Public Ball As Sprite
    Public TablePics As Table
    Public Reset As Integer
    Public Resetting As Boolean
End Class