Public Class GameResources

    Dim _Images(-1) As SwinGame.Bitmap
    Dim _Fonts(-1) As SwinGame.Font
    Dim _Sounds(-1) As SwinGame.SoundEffect
    Dim _Music(-1) As SwinGame.Music
    Dim _Maps(-1) As SwinGame.Map

    Dim _ImagesStr(-1) As String
    Dim _FontsStr(-1) As String
    Dim _SoundsStr(-1) As String
    Dim _MusicStr(-1) As String
    Dim _MapsStr(-1) As String

    Dim _Background As SwinGame.Bitmap
    Dim _Animation As SwinGame.Bitmap
    Dim _LoadingFont As SwinGame.Font
    Dim _StartSound As SwinGame.SoundEffect

    Public Sub LoadResources()
        Dim width, height As Integer

        width = SwinGame.Core.ScreenWidth()
        height = SwinGame.Core.ScreenHeight()

        SwinGame.Core.ChangeScreenSize(800, 600)

        ShowLoadingScreen()

        ShowMessage("Loading fonts...", 0)
        LoadFonts()
        SwinGame.Core.Sleep(100)

        ShowMessage("Loading images...", 1)
        LoadImages()
        SwinGame.Core.Sleep(100)

        ShowMessage("Loading sounds...", 2)
        LoadSounds()
        SwinGame.Core.Sleep(100)

        ShowMessage("Loading music...", 3)
        LoadMusic()
        SwinGame.Core.Sleep(100)

        ShowMessage("Loading maps...", 4)
        LoadMaps()
        SwinGame.Core.Sleep(100)

        SwinGame.Core.Sleep(100)
        ShowMessage("Game loaded...", 5)
        SwinGame.Core.Sleep(100)
        EndLoadingScreen(width, height)
    End Sub

    Public Sub ShowLoadingScreen()
        _Background = SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource("SplashBack.png", SwinGame.ResourceKind.ImageResource))
        SwinGame.Graphics.DrawBitmap(_Background, 0, 0)
        SwinGame.Core.RefreshScreen()
        SwinGame.Core.ProcessEvents()

        _Animation = SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource("SwinGameAni.png", SwinGame.ResourceKind.ImageResource))
        _LoadingFont = SwinGame.Text.LoadFont(SwinGame.Core.GetPathToResource("cour.ttf", SwinGame.ResourceKind.FontResource), 18)
        _StartSound = SwinGame.Audio.LoadSoundEffect(SwinGame.Core.GetPathToResource("SwinGameStart.ogg", SwinGame.ResourceKind.SoundResource))

        PlaySwinGameIntro()
    End Sub

    Public Sub PlaySwinGameIntro()
        SwinGame.Core.Sleep(400)
        SwinGame.Audio.PlaySoundEffect(_StartSound)

        Dim i As Integer
        For i = 0 To 13
            SwinGame.Graphics.DrawBitmap(_Background, 0, 0)
            SwinGame.Graphics.DrawBitmapPart(_Animation, 0, i * 184, 712, 184, 41, 242)
            SwinGame.Core.Sleep(67)
            SwinGame.Core.RefreshScreen()
            SwinGame.Core.ProcessEvents()
        Next i

        SwinGame.Core.Sleep(400)

    End Sub

    Private Sub ShowMessage(ByVal message As String, ByVal number As Integer)
        SwinGame.Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number))
        SwinGame.Core.RefreshScreen()
        SwinGame.Core.ProcessEvents()
    End Sub

    Private Sub EndLoadingScreen(ByVal width As Integer, ByVal height As Integer)
        SwinGame.Graphics.ClearScreen()
        SwinGame.Core.RefreshScreen()
        SwinGame.Text.FreeFont(_LoadingFont)
        SwinGame.Graphics.FreeBitmap(_Background)
        SwinGame.Graphics.FreeBitmap(_Animation)
        SwinGame.Audio.FreeSoundEffect(_StartSound)
        SwinGame.Core.ChangeScreenSize(width, height)
    End Sub

    Private Sub NewMap(ByVal mapName As String)
        System.Array.Resize(_Maps, _Maps.Length + 1)
        System.Array.Resize(_MapsStr, _MapsStr.Length + 1)
        'ReDim Preserve _Maps(_Maps.Length + 1)
        'ReDim Preserve _MapsStr(_Maps.Length + 1)
        _Maps(_Maps.Length - 1) = SwinGame.MappyLoader.LoadMap(mapName)
        _MapsStr(_Maps.Length - 1) = mapName
    End Sub

    Private Sub NewFont(ByVal fontName As String, ByVal filename As String, ByVal size As Integer)
        System.Array.Resize(_Fonts, _Fonts.Length + 1)
        System.Array.Resize(_FontsStr, _FontsStr.Length + 1)
        'ReDim Preserve _Fonts(_Fonts.Length + 1)
        'ReDim Preserve _FontsStr(_Fonts.Length + 1)
        _Fonts(_Fonts.Length - 1) = SwinGame.Text.LoadFont(SwinGame.Core.GetPathToResource(filename, ResourceKind.FontResource), size)
        _FontsStr(_Fonts.Length - 1) = fontName
    End Sub

    Private Sub NewImage(ByVal imageName As String, ByVal filename As String)
        System.Array.Resize(_Images, _Images.Length + 1)
        System.Array.Resize(_ImagesStr, _ImagesStr.Length + 1)
        'ReDim Preserve _Images(_Images.Length + 1)
        'ReDim Preserve _ImagesStr(_Images.Length + 1)
        _Images(_Images.Length - 1) = SwinGame.Graphics.LoadBitmap(SwinGame.Core.GetPathToResource(filename, ResourceKind.ImageResource))
        _ImagesStr(_ImagesStr.Length - 1) = imageName
    End Sub

    Private Sub NewSound(ByVal soundName As String, ByVal filename As String)
        System.Array.Resize(_Sounds, _Sounds.Length + 1)
        System.Array.Resize(_SoundsStr, _SoundsStr.Length + 1)
        'ReDim Preserve _Sounds(_Sounds.Length + 1)
        'ReDim Preserve _SoundsStr(_Sounds.Length + 1)
        _Sounds(_Sounds.Length - 1) = SwinGame.Audio.LoadSoundEffect(SwinGame.Core.GetPathToResource(filename, ResourceKind.SoundResource))
        _SoundsStr(_SoundsStr.Length - 1) = soundName
    End Sub

    Private Sub NewMusic(ByVal musicName As String, ByVal filename As String)
        System.Array.Resize(_Music, _Music.Length + 1)
        System.Array.Resize(_MusicStr, _MusicStr.Length + 1)
        'ReDim Preserve _Music(_Music.Length + 1)
        'ReDim Preserve _MusicStr(_Music.Length + 1)
        _Music(_Music.Length - 1) = SwinGame.Audio.LoadMusic(SwinGame.Core.GetPathToResource(filename, ResourceKind.SoundResource))
        _MusicStr(_MusicStr.Length - 1) = musicName
    End Sub

    Private Sub FreeFonts()
        Dim i As Integer
        For i = 0 To _Fonts.Length - 1
            SwinGame.Text.FreeFont(_Fonts(i))
            _FontsStr(i) = String.Empty
        Next
    End Sub

    Private Sub FreeImages()
        Dim i As Integer
        For i = 0 To _Images.Length - 1
            SwinGame.Graphics.FreeBitmap(_Images(i))
            _ImagesStr(i) = String.Empty
        Next
    End Sub

    Private Sub FreeSounds()
        Dim i As Integer
        For i = 0 To _Sounds.Length - 1
            SwinGame.Audio.FreeSoundEffect(_Sounds(i))
            _SoundsStr(i) = String.Empty
        Next
    End Sub

    Private Sub FreeMusic()
        Dim i As Integer
        For i = 0 To _Music.Length - 1
            SwinGame.Audio.FreeMusic(_Music(i))
            _MusicStr(i) = String.Empty
        Next
    End Sub

    Private Sub FreeMaps()
        Dim i As Integer
        For i = 0 To _Maps.Length - 1
            SwinGame.MappyLoader.FreeMap(_Maps(i))
            _MapsStr(i) = String.Empty
        Next
    End Sub

    Public Sub FreeResources()
        FreeFonts()
        FreeImages()
        FreeMusic()
        FreeSounds()
        FreeMaps()
    End Sub

    Public Function GameFont(ByVal font As String) As SwinGame.Font
        Dim i As Integer
        For i = 0 To _FontsStr.Length - 1
            If _FontsStr(i).Equals(font) Then
                Return _Fonts(i)
            End If
        Next
        Throw New SwinGame.SwinGameException("Could not find Font " + font)
    End Function

    Public Function GameImage(ByVal image As String) As SwinGame.Bitmap
        Dim i As Integer
        For i = 0 To _ImagesStr.Length - 1
            If _ImagesStr(i).Equals(image) Then
                Return _Images(i)
            End If
        Next
        Throw New SwinGame.SwinGameException("Could not find Image " + image)
    End Function

    Public Function GameSound(ByVal sound As String) As SwinGame.SoundEffect
        Dim i As Integer
        For i = 0 To _SoundsStr.Length - 1
            If _SoundsStr(i).Equals(sound) Then
                Return _Sounds(i)
            End If
        Next
        Throw New SwinGame.SwinGameException("Could not find sound effect " + sound)
    End Function

    Public Function GameMusic(ByVal music As String) As SwinGame.Music
        Dim i As Integer
        For i = 0 To _MusicStr.Length - 1
            If _MusicStr(i).Equals(music) Then
                Return _Music(i)
            End If
        Next
        Throw New SwinGame.SwinGameException("Could not find music " + music)
    End Function

    Public Function GameMap(ByVal map As String) As SwinGame.Map
        Dim i As Integer
        For i = 0 To _MapsStr.Length - 1
            If _MapsStr(i).Equals(map) Then
                Return _Maps(i)
            End If
        Next
        Throw New SwinGame.SwinGameException("Could not find map " + map)
    End Function

    Private Sub LoadFonts()
        NewFont("ArialLarge", "arial.ttf", 80)
        NewFont("Courier", "cour.ttf", 16)
    End Sub

    Private Sub LoadImages()

    End Sub

    Private Sub LoadSounds()

    End Sub

    Private Sub LoadMusic()

    End Sub

    Private Sub LoadMaps()

    End Sub

End Class
