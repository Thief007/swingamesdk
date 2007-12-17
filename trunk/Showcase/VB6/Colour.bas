Attribute VB_Name = "Colour"
Option Explicit

Public Const black As Long = -16777216
Public Const blue As Long = -16776961
Public Const green As Long = -16744448
Public Const red As Long = -65536
Public Const yellow As Long = -256
Public Const pink As Long = -16181
Public Const turquoise As Long = -12525360
Public Const grey As Long = -8355712
Public Const megenta As Long = -65281
Public Const transparent As Long = 16777215
Public Const white As Long = -1

Public Function GetRandomColour() As Long
    Dim Random As Long
    Call Randomize
    Random = Int(Rnd * 10)
    Select Case Random
    Case 0
        GetRandomColour = black
    Case 1
        GetRandomColour = blue
    Case 2
        GetRandomColour = green
    Case 3
        GetRandomColour = red
    Case 4
        GetRandomColour = yellow
    Case 5
        GetRandomColour = pink
    Case 6
        GetRandomColour = turquoise
    Case 7
        GetRandomColour = grey
    Case 8
        GetRandomColour = megenta
    Case 9
        GetRandomColour = white
    End Select

End Function

