Imports SwinGame
Imports System.Collections.Generic

Public Module GameResources

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

    ''' <summary>
    ''' Gets a Font Loaded in the Resources
    ''' </summary>
    ''' <param name="font">Name of Font</param>
    ''' <returns>The Font Loaded with this Name</returns>
    ''' <remarks></remarks>
    Public Function GameFont(ByVal font As String) As Font
        Return _Fonts(font)
    End Function

    ''' <summary>
    ''' Gets an Image loaded in the Resources
    ''' </summary>
    ''' <param name="image">Name of image</param>
    ''' <returns>The image loaded with this name</returns>
    ''' <remarks></remarks>
    Public Function GameImage(ByVal image As String) As Bitmap
        Return _Images(image)
    End Function

    ''' <summary>
    ''' Gets an sound loaded in the Resources
    ''' </summary>
    ''' <param name="sound">Name of sound</param>
    ''' <returns>The sound with this name</returns>
    ''' <remarks></remarks>
    Public Function GameSound(ByVal sound As String) As SoundEffect
        Return _Sounds(sound)
    End Function

    ''' <summary>
    ''' Gets the music loaded in the Resources
    ''' </summary>
    ''' <param name="music">Name of music</param>
    ''' <returns>The music with this name</returns>
    ''' <remarks></remarks>
    Public Function GameMusic(ByVal music As String) As Music
        Return _Music(music)
    End Function

    ''' <summary>
    ''' Gets a map loaded in the Resources
    ''' </summary>
    ''' <param name="map">Name of map</param>
    ''' <returns>The map with this name</returns>
    ''' <remarks></remarks>
    Public Function GameMap(ByVal map As String) As Map
        Return _Maps(map)
    End Function

    Private _Images As New Dictionary(Of String, Bitmap)
    Private _Fonts As New Dictionary(Of String, Font)
    Private _Sounds As New Dictionary(Of String, SoundEffect)
    Private _Music As New Dictionary(Of String, Music)
    Private _Maps As New Dictionary(Of String, Map)

    Dim _Background As Bitmap
    Dim _Animation As Bitmap
    Dim _LoadingFont As Font
    Dim _StartSound As SoundEffect

    ''' <summary>
    ''' The Resources Class stores all of the Games Media Resources, such as Images, Fonts
    ''' Sounds, Music, and Maps.
    ''' </summary>
    ''' <remarks></remarks>
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

    Private Sub ShowLoadingScreen()
        _Background = Graphics.LoadBitmap(Core.GetPathToResource("SplashBack.png", ResourceKind.ImageResource))
        Graphics.DrawBitmap(_Background, 0, 0)
        Core.RefreshScreen()
        Core.ProcessEvents()

        _Animation = Graphics.LoadBitmap(Core.GetPathToResource("SwinGameAni.png", ResourceKind.ImageResource))
        _LoadingFont = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18)
        _StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource))

        PlaySwinGameIntro()
    End Sub

    Private Sub PlaySwinGameIntro()
        Core.Sleep(400)
        Audio.PlaySoundEffect(_StartSound)

        Dim i As Integer
        For i = 0 To 13
            Graphics.DrawBitmap(_Background, 0, 0)
            Graphics.DrawBitmapPart(_Animation, (i \ 7) * 712, (i Mod 7) * 184, 712, 184, 41, 242)
            Core.Sleep(67)
            Core.RefreshScreen()
            Core.ProcessEvents()
        Next i

        Core.Sleep(1500)

    End Sub

    Private Sub ShowMessage(ByVal message As String, ByVal number As Integer)
        Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number))
        Core.RefreshScreen()
        Core.ProcessEvents()
    End Sub

    Private Sub EndLoadingScreen(ByVal width As Integer, ByVal height As Integer)
        Core.ProcessEvents()
        Core.Sleep(500)
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

    Private Sub NewTransparentColorImage(ByVal imageName As String, ByVal fileName As String, ByVal transColor As Color)
        _Images.Add(imageName, Graphics.LoadBitmap(Core.GetPathToResource(fileName, ResourceKind.ImageResource), True, transColor))
    End Sub

    Private Sub NewTransparentColourImage(ByVal imageName As String, ByVal fileName As String, ByVal transColor As Color)
        NewTransparentColorImage(imageName, fileName, transColor)
    End Sub

    Private Sub NewSound(ByVal soundName As String, ByVal filename As String)
        _Sounds.Add(soundName, Audio.LoadSoundEffect(Core.GetPathToResource(filename, ResourceKind.SoundResource)))
    End Sub

    Private Sub NewMusic(ByVal musicName As String, ByVal filename As String)
        _Music.Add(musicName, Audio.LoadMusic(Core.GetPathToResource(filename, ResourceKind.SoundResource)))
    End Sub

    Private Sub FreeFonts()
        Dim obj As Font
        For Each obj In _Fonts.Values
            Text.FreeFont(obj)
        Next
    End Sub

    Private Sub FreeImages()
        Dim obj As Bitmap
        For Each obj In _Images.Values
            Graphics.FreeBitmap(obj)
        Next
    End Sub

    Private Sub FreeSounds()
        Dim obj As SoundEffect
        For Each obj In _Sounds.Values
            Audio.FreeSoundEffect(obj)
        Next
    End Sub

    Private Sub FreeMusic()
        Dim obj As Music
        For Each obj In _Music.Values
            Audio.FreeMusic(obj)
        Next
    End Sub

    Private Sub FreeMaps()
        Dim obj As Map
        For Each obj In _Maps.Values
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
End Module
