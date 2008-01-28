Imports SwinGame
Imports System.Collections.Generic

Public Class GameResources

    Private _Images As New Dictionary (Of String, Bitmap)
    Private _Fonts As New Dictionary (Of String, Font)
    Private _Sounds As New Dictionary (Of String, SoundEffect)
    Private _Music As New Dictionary (Of String, Music)
    Private _Maps As New Dictionary (Of String, Map)

    Dim _Background As Bitmap
    Dim _Animation As Bitmap
    Dim _LoadingFont As Font
    Dim _StartSound As SoundEffect

    Public Sub LoadResources()
        Dim width, height As Integer

        width = Core.ScreenWidth()
        height = Core.ScreenHeight()

        Core.ChangeScreenSize(800, 600)

        ShowLoadingScreen()

        ShowMessage("Loading fonts...", 0)
        LoadFonts()
        Core.Sleep(100)

        ShowMessage("Loading images...", 1)
        LoadImages()
        Core.Sleep(100)

        ShowMessage("Loading sounds...", 2)
        LoadSounds()
        Core.Sleep(100)

        ShowMessage("Loading music...", 3)
        LoadMusic()
        Core.Sleep(100)

        ShowMessage("Loading maps...", 4)
        LoadMaps()
        Core.Sleep(100)

        Core.Sleep(100)
        ShowMessage("Game loaded...", 5)
        Core.Sleep(100)
        EndLoadingScreen(width, height)
    End Sub

    Public Sub ShowLoadingScreen()
        _Background = Graphics.LoadBitmap(Core.GetPathToResource("SplashBack.png", ResourceKind.ImageResource))
        Graphics.DrawBitmap(_Background, 0, 0)
        Core.RefreshScreen()
        Core.ProcessEvents()

        _Animation = Graphics.LoadBitmap(Core.GetPathToResource("SwinGameAni.png", ResourceKind.ImageResource))
        _LoadingFont = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18)
        _StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource))

        PlaySwinGameIntro()
    End Sub

    Public Sub PlaySwinGameIntro()
        Core.Sleep(400)
        Audio.PlaySoundEffect(_StartSound)

        Dim i As Integer
        For i = 0 To 13
            Graphics.DrawBitmap(_Background, 0, 0)
            Graphics.DrawBitmapPart(_Animation, 0, i * 184, 712, 184, 41, 242)
            Core.Sleep(67)
            Core.RefreshScreen()
            Core.ProcessEvents()
        Next i

        Core.Sleep(400)

    End Sub

    Private Sub ShowMessage(ByVal message As String, ByVal number As Integer)
        Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number))
        Core.RefreshScreen()
        Core.ProcessEvents()
    End Sub

    Private Sub EndLoadingScreen(ByVal width As Integer, ByVal height As Integer)
        Graphics.ClearScreen()
        Core.RefreshScreen()
        Text.FreeFont(_LoadingFont)
        Graphics.FreeBitmap(_Background)
        Graphics.FreeBitmap(_Animation)
        Audio.FreeSoundEffect(_StartSound)
        Core.ChangeScreenSize(width, height)
    End Sub

    Private Sub NewMap(ByVal mapName As String)
        _Maps.Add(mapName, MappyLoader.LoadMap(mapName))
    End Sub

    Private Sub NewFont(ByVal fontName As String, ByVal filename As String, ByVal size As Integer)
        _Fonts.Add(fontName, Text.LoadFont(Core.GetPathToResource(filename, ResourceKind.FontResource), size))
    End Sub

    Private Sub NewImage(ByVal imageName As String, ByVal filename As String)
		_Images.Add(imageName, Graphics.LoadBitmap(Core.GetPathToResource(filename, ResourceKind.ImageResource)))

    End Sub

    Private Sub NewSound(ByVal soundName As String, ByVal filename As String)
        _Sounds.Add(soundName, Audio.LoadSoundEffect(Core.GetPathToResource(filename, ResourceKind.SoundResource)))
    End Sub

    Private Sub NewMusic(ByVal musicName As String, ByVal filename As String)
        _Music.Add(musicName, Audio.LoadMusic(Core.GetPathToResource(filename, ResourceKind.SoundResource)))
    End Sub

    Private Sub FreeFonts()
		Dim obj as Font
		For Each obj in _Fonts.Values 
			Text.FreeFont(obj)
		Next
    End Sub

    Private Sub FreeImages()
		Dim obj as Image
		For Each obj in _Images.Values
            Graphics.FreeBitmap(obj)
		Next
    End Sub

    Private Sub FreeSounds()
		Dim obj as SoundEffect
		For Each obj in _Sounds.Values
            Audio.FreeSoundEffect(obj)
		Next
    End Sub

    Private Sub FreeMusic()
		Dim obj as Music
		For Each obj in _Music.Values
            Audio.FreeMusic(obj)
        Next
    End Sub

    Private Sub FreeMaps()
		Dim obj as Map
		For Each obj in _Maps.Values
            MappyLoader.FreeMap(obj)
        Next
    End Sub

    Public Sub FreeResources()
        FreeFonts()
        FreeImages()
        FreeMusic()
        FreeSounds()
        FreeMaps()
    End Sub

    Public Function GameFont(ByVal font As String) As Font
		Return _Fonts(font)
    End Function

    Public Function GameImage(ByVal image As String) As Bitmap
		Return _Images(image)
    End Function

    Public Function GameSound(ByVal sound As String) As SoundEffect
		Return _Sounds(sound)
    End Function

    Public Function GameMusic(ByVal music As String) As Music
		Return _Music(music)
    End Function

    Public Function GameMap(ByVal map As String) As Map
		Return _Maps(map)
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
