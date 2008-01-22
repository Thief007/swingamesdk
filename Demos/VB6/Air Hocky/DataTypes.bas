Attribute VB_Name = "DataTypes"

Public Type Player
    Bat As Sprite
    Score As Long
    Goal As Sprite
End Type

Public Type Table
    BackGround As Bitmap
    TableHorizontal As Sprite
    TableVertical As Sprite
End Type

Public Type AirHockeyGame
    Players() As Player
    Ball As Sprite
    TablePics As Table
    Reset As Long
    Resetting As Boolean
End Type


