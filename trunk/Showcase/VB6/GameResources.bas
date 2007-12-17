Attribute VB_Name = "GameResources"
Private Images() As SGSDKVB6.Bitmap
Private Font1() As SGSDKVB6.Fonts
Private Sounds() As SGSDKVB6.SoundEffect
Private Musics() As SGSDKVB6.music
Private Maps() As SGSDKVB6.Map

Private ImagesStr() As String
Private FontsStr() As String
Private SoundsStr() As String
Private MusicsStr() As String
Private MapsStr() As String


Private Background As SGSDKVB6.Bitmap
Private Animation As SGSDKVB6.sprite
Private Loadingfont As SGSDKVB6.Fonts
Private StartSound As SGSDKVB6.SoundEffect


Option Explicit

Private Sub LoadFonts()
    Call NewFont("ArialLarge", "arial.ttf", 80)
    Call NewFont("Courier", "cour.ttf", 16)
End Sub

Private Sub LoadImages()
    Call NewImage("BallImage1", "ball.png")
    Call NewImage("BallImage2", "ball2.png")
    Call NewImage("SmallBall", "ball_small.png")
    Call NewImage("Running", "running.png")
    Call NewImage("Explosion", "Explosion.png")
    Call NewImage("Sea", "sea.png")
    Call NewImage("Ship", "ship.png")
End Sub

Private Sub LoadSounds()
    Call NewSound("Shock", "shock.wav")
End Sub

Private Sub LoadMusics()
    
    Call NewMusic("Fast", "Fast.mp3")
End Sub

Private Sub LoadMaps()
    NewMap ("test")
End Sub



Sub LoadResources()
    ReDim Images(0)
    ReDim Font1(0)
    ReDim Sounds(0)
    ReDim Musics(0)
    ReDim Maps(0)
    
    ReDim ImagesStr(0)
    ReDim FontsStr(0)
    ReDim SoundsStr(0)
    ReDim MusicsStr(0)
    ReDim MapsStr(0)

    Dim oldW, oldH As Long
    oldW = SwinGame.Core.ScreenHeight
    oldH = SwinGame.Core.ScreenWidth
    Call SwinGame.Core.ChangeScreenSize(800, 600)
    Call ShowLoadingScreen
    Call ShowMessage("Loading fonts...", 0)
    Call LoadFonts
    Call SwinGame.Core.Sleep(50)

    Call ShowMessage("Loading images...", 1)
    Call LoadImages
    Call SwinGame.Core.Sleep(50)

    Call ShowMessage("Loading sounds...", 2)
    Call LoadSounds
    Call SwinGame.Core.Sleep(50)

    Call ShowMessage("Loading music...", 3)
    Call LoadMusics
    Call SwinGame.Core.Sleep(50)
   
    Call ShowMessage("Loading maps...", 3)
    Call LoadMaps
    Call SwinGame.Core.Sleep(50)
    
    
    'Add game level loading here...
    Call SwinGame.Core.Sleep(50)
    Call ShowMessage("Game loaded...", 4)
    Call SwinGame.Core.Sleep(50)
    Call EndLoadingScreen

    Call SwinGame.Core.ChangeScreenSize(oldH, oldW)
End Sub

Private Sub ShowLoadingScreen()
    Set Background = SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource("SplashBack.png", ResourceKind_ImageResource))
    Call SwinGame.Graphics.DrawBitmap(Background, 0, 0)
    Call SwinGame.Core.RefreshScreen_WithFrame(60)
    Call SwinGame.Core.ProcessEvents
    Set Animation = New SGSDKVB6.sprite
    Set StartSound = New SGSDKVB6.SoundEffect
    Set Animation = SwinGame.Graphics.CreateSprite_MultiFPC(SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource("SwinGameAni.png", ResourceKind_ImageResource)), 4, 14, 712, 184)
    Call Animation.SetX(41)
    Call Animation.SetY(242)
    Set Loadingfont = SwinGame.Text.LoadFont(SwinGame.Core.GetPathToResource("cour.ttf", ResourceKind_FontResource), 18)
    Set StartSound = SwinGame.Audio.LoadSoundEffect(SwinGame.Core.GetPathToResource("SwinGameStart.ogg", ResourceKind_SoundResource))
    Call PlaySwinGameIntro
End Sub

Private Sub PlaySwinGameIntro()
    Dim i As Long
    Call SwinGame.Audio.PlaySoundEffect(StartSound)
     i = 0
    Do Until i = 55
        i = i + 1
        Call SwinGame.Graphics.DrawBitmap(Background, 0, 0)
        Call SwinGame.Graphics.DrawSprite(Animation)
        Call SwinGame.Graphics.UpdateSprite(Animation)
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        Call SwinGame.Core.ProcessEvents
    Loop
    Call SwinGame.Core.Sleep(1500)
End Sub

Private Sub ShowMessage(message As String, number As Long)
    Call SwinGame.Text.DrawText(message, red, Loadingfont, 240, 20 + (25 * number))
    Call SwinGame.Core.RefreshScreen_WithFrame(60)
    Call SwinGame.Core.ProcessEvents
End Sub


Private Sub EndLoadingScreen()
    Call SwinGame.Graphics.ClearScreen
    Call SwinGame.Core.RefreshScreen_WithFrame(60)
    Call SwinGame.Text.FreeFont(Loadingfont)
    Call SwinGame.Graphics.FreeBitmap(Background)
    Call SwinGame.Graphics.FreeSprite(Animation)
    Call SwinGame.Audio.FreeSoundEffect(StartSound)
End Sub


Private Sub NewFont(fontName As String, fileName As String, size As Long)
    If UBound(Font1, 1) = Not 0 Then
    
        ReDim Preserve Font1(UBound(Font1, 1) + 1)
        ReDim Preserve FontsStr(UBound(FontsStr, 1) + 1)
    End If
    Set Font1(UBound(Font1, 1)) = New SGSDKVB6.Fonts
    Set Font1(UBound(Font1, 1)) = SwinGame.Text.LoadFont(SwinGame.Core.GetPathToResource(fileName, ResourceKind_FontResource), size)
    FontsStr(UBound(FontsStr, 1)) = fontName
End Sub

'This is not working right!!!
Private Sub NewImage(imageName As String, fileName As String)
    'If UBound(ImagesStr, 1) = Not 0 Then
        ReDim Preserve Images(UBound(Images, 1) + 1)
        ReDim Preserve ImagesStr(UBound(ImagesStr, 1) + 1)
    'End If
    Set Images(UBound(ImagesStr, 1)) = New SGSDKVB6.Bitmap
    Set Images(UBound(ImagesStr, 1)) = SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource(fileName, ResourceKind_ImageResource))
    ImagesStr(UBound(ImagesStr, 1)) = imageName
End Sub
    
Private Sub NewSound(soundName As String, fileName As String)
    If UBound(Sounds, 1) = Not 0 Then
        ReDim Preserve Sounds(UBound(Sounds, 1) + 1)
        ReDim Preserve SoundsStr(UBound(SoundsStr, 1) + 1)
    End If
    Set Sounds(UBound(Sounds, 1)) = New SGSDKVB6.SoundEffect
    Set Sounds(UBound(Sounds, 1)) = SwinGame.Audio.LoadSoundEffect(SwinGame.Core.GetPathToResource(fileName, ResourceKind_SoundResource))
    SoundsStr(UBound(SoundsStr, 1)) = soundName
End Sub

Private Sub NewMusic(musicName As String, fileName As String)
    If UBound(Musics, 1) = Not 0 Then
        ReDim Preserve Musics(UBound(Musics, 1) + 1)
        ReDim Preserve MusicsStr(UBound(MusicsStr, 1) + 1)
    End If
    Set Musics(UBound(Musics, 1)) = New SGSDKVB6.music
    Set Musics(UBound(Musics, 1)) = SwinGame.Audio.LoadMusic(SwinGame.Core.GetPathToResource(fileName, ResourceKind_SoundResource))
    MusicsStr(UBound(MusicsStr, 1)) = musicName
End Sub

Private Sub NewMap(mapName As String)
    If UBound(Maps, 1) = Not 0 Then
        ReDim Preserve Maps(UBound(Maps, 1) + 1)
        ReDim Preserve MapsStr(UBound(MapsStr, 1) + 1)
    End If
    Set Maps(UBound(Maps, 1)) = New Map
    Set Maps(UBound(Maps, 1)) = MappyLoader.Loadmap(mapName)
    MapsStr(UBound(Maps, 1)) = mapName
    '_Maps[High(_Maps)] := LoadMap(mapName);
    '_MapsStr[High(_MapsStr)] := mapName;
End Sub

Public Sub FreeResources()
    Call FreeFonts
    Call FreeImages
    Call FreeSounds
    Call FreeMusics
End Sub


Public Sub FreeFonts()
    Dim i As Long
    For i = (LBound(Font1, 1)) To (UBound(Font1, 1))
        Call SwinGame.Text.FreeFont(Font1(i))
    Next i
End Sub



Public Sub FreeImages()
    Dim i As Long
    For i = (LBound(Images, 1)) + 1 To (UBound(Images, 1))
        Call SwinGame.Graphics.FreeBitmap(Images(i))
    Next i
End Sub



Public Sub FreeSounds()
    Dim i As Long
    For i = (LBound(Sounds, 1)) To (UBound(Sounds, 1))
        Call SwinGame.Audio.FreeSoundEffect(Sounds(i))
    Next i
End Sub



Public Sub FreeMusics()
    Dim i As Long
    For i = (LBound(Musics, 1)) To (UBound(Musics, 1))
        Call SwinGame.Audio.FreeMusic(Musics(i))
    Next i
End Sub


'procedure FreeMaps()
'Var
'i: Integer
'begin
'for i := Low(_Maps) to High(_Maps) do
'begin
'    FreeMap(_Maps[i])
'End
'End
        

    
Public Function GameFont(font2 As String) As SGSDKVB6.Fonts
Dim i As Long
    
    For i = (LBound(FontsStr, 1)) To (UBound(FontsStr, 1))
        If FontsStr(i) = font2 Then
            Set GameFont = Font1(i)
            Exit Function
        End If
    Next i
End Function
    
Public Function GameImage(image As String) As SGSDKVB6.Bitmap
Dim i As Long
    For i = (LBound(ImagesStr)) To (UBound(ImagesStr))
        If ImagesStr(i) = image Then
            Set GameImage = Images(i)
            Exit Function
        End If
    Next i
End Function

Public Function GameSound(sound As String) As SGSDKVB6.SoundEffect
Dim i As Long
    For i = (LBound(SoundsStr, 1)) To (UBound(SoundsStr, 1))
        If SoundsStr(i) = sound Then
            Set GameSound = Sounds(i)
            Exit Function
        End If
    Next i
End Function
Public Function GameMusic(music As String) As SGSDKVB6.music
Dim i As Long
    For i = (LBound(MusicsStr, 1)) To (UBound(MusicsStr, 1))
        If MusicsStr(i) = music Then
            Set GameMusic = Musics(i)
            Exit Function
        End If
    Next i
End Function

Public Function GameMap(mapName As String) As SGSDKVB6.Map

Dim i As Long
    For i = (LBound(MapsStr, 1)) To (UBound(MapsStr, 1))
        If MapsStr(i) = mapName Then
            Set GameMap = Maps(i)
            Exit Function
        End If
    Next i
End Function
    

   
