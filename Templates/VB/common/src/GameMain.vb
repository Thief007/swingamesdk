Public Module GameMain
    Public Sub Main()
        ' Set the path to the application so the resource manager can
        ' find the files it needs to load.
        Resources.SetAppPath(Assembly.GetExecutingAssembly().Location, True)
        
        'Start the audio system.
        Audio.OpenAudio()
        
        Core.OpenGraphicsWindow("GameMain", 800, 600)
        
        'Run the game loop
        Do While Not Core.WindowCloseRequested()
            'Fetch the next batch of UI interaction
            Core.ProcessEvents()
            
            Graphics.ClearScreen()
            Text.DrawFramerate(0,0) 'Draw framerate top left
            
            'Draw onto the screen
            Core.RefreshScreen()
        Loop
        
        'End the audio
        Audio.CloseAudio()
        Resources.ReleaseAllResources()
    End Sub
End Module