Public Module GameMain
    Public Sub Main()
        'Start the audio system.
        Audio.OpenAudio()
        
        'Open the game window
        Core.OpenGraphicsWindow("GameMain", 800, 600)
        
        'Run the game loop
        Do While Not Core.WindowCloseRequested()
            'Fetch the next batch of UI interaction
            Core.ProcessEvents()
            
            'Clear the screen and draw the framerate'
            Graphics.ClearScreen()
            Text.DrawFramerate(0,0)
            
            'Draw onto the screen
            Core.RefreshScreen()
        Loop
        
        'End the audio
        Audio.CloseAudio()
        
        'Close any resources we were using
        Resources.ReleaseAllResources()
    End Sub
End Module