Attribute VB_Name = "GameLogic"
Option Explicit


Public Const CHANGE_COUNT As Integer = 120 ' how many cycles before it changes direction (auto)
Public Const SCREEN_WIDTH As Integer = 800
Public Const SCREEN_HEIGHT As Integer = 600

Sub Run()
    'Your Code goes in here
    Call Core.OpenGraphicsWindow("Rotation", 800, 600)
    Call Audio.OpenAudio
    Call LoadResources
    Call Initialise
    Call RunGame
    Call Audio.CloseAudio
End Sub

Public Sub Initialise()
    Call InitAircraft
End Sub

Public Sub Draw()
    'Draw screen
    Call Graphics.ClearScreen_ToColour(Colour.black)
    Call Graphics.DrawBitmap(GameResources.GameImage("Background"), 0, 0)

    Call Aircraft.Draw

    Call Core.RefreshScreen_WithFrame(65)
End Sub

Public Sub HandleInput()
    Dim angle As Integer

    If (Manual) Then
      If (MouseAndKey.WasKeyTyped(Keys_VK_LEFT)) Then
        angle = -30
      End If
      If (MouseAndKey.WasKeyTyped(Keys_VK_RIGHT)) Then
        angle = angle + 30
      End If
    
      If (angle <> 0) Then
        Call Aircraft.ChangeDirection(angle)
      End If
    End If

    If (MouseAndKey.WasKeyTyped(Keys_VK_T)) Then
      Aircraft.Manual = Not Aircraft.Manual
    End If
End Sub

Public Sub Update()
    Call Aircraft.Update
End Sub

Public Sub RunGame()
    'Game Loop
    Do
        Call Core.ProcessEvents
        Call HandleInput
        Call Update
        Call Draw

        Call Core.RefreshScreen
    Loop Until Core.WindowCloseRequested()

    'Free Resources and Close Audio, to end the program.
    Call FreeResources
    Call Audio.CloseAudio
End Sub


