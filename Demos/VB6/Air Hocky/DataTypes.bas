Attribute VB_Name = "DataTypes"

Public Type Player
    Bat As Sprite
    Score As Long
    Goal As Sprite
End Type

Public Enum GameState
    AtMenu
    StartingGame
    PlayingGame
    FinshedGame
    Quit
End Enum

Public Type Table
    BackGround As Bitmap
    TableHorizontal As Sprite
    TableVertical As Sprite
End Type

Public Type AirHockeyGame
    Players(1) As Player
    Ball As Sprite
    TablePics As Table
    'state As GameState
End Type


