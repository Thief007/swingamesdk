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


Private BackGround As SGSDKVB6.Bitmap
Private Animation As SGSDKVB6.Bitmap
Private Loadingfont As SGSDKVB6.Fonts
Private StartSound As SGSDKVB6.SoundEffect


Option Explicit

Private Sub LoadFonts()
    Call NewFont("Courier", "cour.ttf", 16)
    Call NewFont("CourierBigger", "cour.ttf", 25)
    Call NewFont("ArialBig", "arial.ttf", 400)
    Call NewFont("Comic", "comicbd.ttf", 25)
End Sub

Private Sub LoadImages()
    Call NewImage("ball", "ball.png")
    Call NewImage("Table", "air_hocky_table.png")
    Call NewImage("TableHorizontal", "air_hocky_table_collision_horizontal.png")
    Call NewImage("TableVertical", "air_hocky_table_collision_vertical.png")
    Call NewImage("0goal", "air_hocky_table_collision_AI_goal.png")
    Call NewImage("1goal", "air_hocky_table_collision_player_goal.png")
    Call NewImage("0bat", "player_bat.png")
    Call NewImage("1bat", "AI_bat.png")
    Call NewImage("Menu", "air_hockey_logo.png")
End Sub

Private Sub LoadSounds()
    Call NewSound("Ball hit bat", "bosu06.wav")
    Call NewSound("Ball hit side", "hit15.wav")
    Call NewSound("Ball in goal", "koro00.wav")
    Call NewSound("Bat hit side", "bosu07_b.wav")
End Sub

Private Sub LoadMusics()

End Sub

Private Sub LoadMaps()

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
    oldW = Core.ScreenHeight
    oldH = Core.ScreenWidth
    Call Core.ChangeScreenSize(800, 600)
    Call ShowLoadingScreen
    Call ShowMessage("Loading fonts...", 0)
    Call LoadFonts
    Call Core.Sleep(50)

    Call ShowMessage("Loading images...", 1)
    Call LoadImages
    Call Core.Sleep(50)

    Call ShowMessage("Loading sounds...", 2)
    Call LoadSounds
    Call Core.Sleep(50)

    Call ShowMessage("Loading music...", 3)
    Call LoadMusics
    Call Core.Sleep(50)
   
    Call ShowMessage("Loading maps...", 4)
    Call LoadMaps
    Call Core.Sleep(50)
    
    'Add game level loading here...
    Call Core.Sleep(50)
    Call ShowMessage("Game loaded...", 5)
    Call Core.Sleep(50)
    Call EndLoadingScreen

    Call Core.ChangeScreenSize(oldH, oldW)
End Sub

Private Sub ShowLoadingScreen()
    Set BackGround = Graphics.LoadBitmap(Core.GetPathToResource("SplashBack.png", ResourceKind_ImageResource))
    Call Graphics.DrawBitmap(BackGround, 0, 0)
    Call Core.RefreshScreen_WithFrame(60)
    Call Core.ProcessEvents
    Set StartSound = New SGSDKVB6.SoundEffect
    Set Animation = Graphics.LoadBitmap(Core.GetPathToResource("SwinGameAni.png", ResourceKind_ImageResource)
    Set Loadingfont = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind_FontResource), 18)
    Set StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind_SoundResource))
    Call PlaySwinGameIntro
End Sub

Private Sub PlaySwinGameIntro()
    Dim i As Long
    Call Audio.PlaySoundEffect(StartSound)
     i = 0
    Do Until i = 14
        Call Graphics.DrawBitmap(BackGround, 0, 0)

        Call Graphics.DrawBitmapPart(Animation, (i \ 7) * 712, (i mod 7) * 184, 712, 184, 41, 242);

        Call Core.RefreshScreen_WithFrame(60)
        Call Core.ProcessEvents
		Call Core.Sleep(67)
        i = i + 1
    Loop
    Call Core.Sleep(1500)
End Sub

Private Sub ShowMessage(message As String, number As Long)
    Call Text.DrawText(message, red, Loadingfont, 240, 20 + (25 * number))
    Call Core.RefreshScreen_WithFrame(60)
    Call Core.ProcessEvents
End Sub


Private Sub EndLoadingScreen()
	Call Core.ProcessEvents()
	Call Core.Sleep(500)

    Call Graphics.ClearScreen
    Call Core.RefreshScreen_WithFrame(60)
    Call Text.FreeFont(Loadingfont)
    Call Graphics.FreeBitmap(BackGround)
    Call Graphics.FreeBitmap(Animation)
    Call Audio.FreeSoundEffect(StartSound)
End Sub


Private Sub NewFont(fontName As String, fileName As String, size As Long)
        ReDim Preserve Font1(UBound(Font1, 1) + 1)
        ReDim Preserve FontsStr(UBound(FontsStr, 1) + 1)
    Set Font1(UBound(Font1, 1)) = New SGSDKVB6.Fonts
    Set Font1(UBound(Font1, 1)) = Text.LoadFont(Core.GetPathToResource(fileName, ResourceKind_FontResource), size)
    FontsStr(UBound(FontsStr, 1)) = fontName
End Sub

Private Sub NewImage(imageName As String, fileName As String)
        ReDim Preserve Images(UBound(Images, 1) + 1)
        ReDim Preserve ImagesStr(UBound(ImagesStr, 1) + 1)
    Set Images(UBound(ImagesStr, 1)) = New SGSDKVB6.Bitmap
    Set Images(UBound(ImagesStr, 1)) = Graphics.LoadBitmap(Core.GetPathToResource(fileName, ResourceKind_ImageResource))
    ImagesStr(UBound(ImagesStr, 1)) = imageName
End Sub
    
Private Sub NewSound(soundName As String, fileName As String)
        ReDim Preserve Sounds(UBound(Sounds, 1) + 1)
        ReDim Preserve SoundsStr(UBound(SoundsStr, 1) + 1)
    Set Sounds(UBound(Sounds, 1)) = New SGSDKVB6.SoundEffect
    Set Sounds(UBound(Sounds, 1)) = Audio.LoadSoundEffect(Core.GetPathToResource(fileName, ResourceKind_SoundResource))
    SoundsStr(UBound(SoundsStr, 1)) = soundName
End Sub

Private Sub NewMusic(musicName As String, fileName As String)
        ReDim Preserve Musics(UBound(Musics, 1) + 1)
        ReDim Preserve MusicsStr(UBound(MusicsStr, 1) + 1)
    Set Musics(UBound(Musics, 1)) = New SGSDKVB6.music
    Set Musics(UBound(Musics, 1)) = Audio.LoadMusic(Core.GetPathToResource(fileName, ResourceKind_SoundResource))
    MusicsStr(UBound(MusicsStr, 1)) = musicName
End Sub

Private Sub NewMap(mapName As String)
    ReDim Preserve Maps(UBound(Maps, 1) + 1)
    ReDim Preserve MapsStr(UBound(MapsStr, 1) + 1)
    Set Maps(UBound(Maps, 1)) = New Map
    Set Maps(UBound(Maps, 1)) = MappyLoader.LoadMap(mapName)
    MapsStr(UBound(Maps, 1)) = mapName
End Sub

Public Sub FreeResources()
    Call FreeFonts
    Call FreeImages
    Call FreeSounds
    Call FreeMusics
    'Call FreeMaps
End Sub


Public Sub FreeFonts()
    Dim i As Long
    For i = (LBound(Font1, 1)) + 1 To (UBound(Font1, 1))
        Call Text.FreeFont(Font1(i))
    Next i
End Sub



Public Sub FreeImages()
    Dim i As Long
    For i = (LBound(Images, 1)) + 1 To (UBound(Images, 1))
        Call Graphics.FreeBitmap(Images(i))
    Next i
End Sub



Public Sub FreeSounds()
    Dim i As Long
    For i = (LBound(Sounds, 1)) + 1 To (UBound(Sounds, 1))
        Call Audio.FreeSoundEffect(Sounds(i))
    Next i
End Sub



Public Sub FreeMusics()
    Dim i As Long
    For i = (LBound(Musics, 1)) + 1 To (UBound(Musics, 1))
        Call Audio.FreeMusic(Musics(i))
    Next i
End Sub

Public Sub FreeMaps()
    Dim i As Long
    For i = (LBound(Maps, 1)) + 1 To (UBound(Maps, 1))
        Call MappyLoader.FreeMap(Maps(i))
    Next i
End Sub
        

    
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
    

   
