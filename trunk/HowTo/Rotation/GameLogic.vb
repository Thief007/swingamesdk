Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()
		Initialise()
		
        RunGame()

        'Free Resources and Close Audio, to end the program.
        FreeResources()

        Audio.CloseAudio()

    End Sub

    public const CHANGE_COUNT As Integer = 120 ' how many cycles before it changes direction (auto)
    public const SCREEN_WIDTH As Integer = 800
    public const SCREEN_HEIGHT As Integer = 600

    private _Aircraft As Aircraft

    public Sub Initialise()
        _Aircraft = new Aircraft()
    End sub

    public Sub Draw()
        'Draw screen
        Graphics.ClearScreen(Color.Black)
        Graphics.DrawBitmap(GameResources.GameImage("Background"), 0, 0)

        _Aircraft.Draw()

        Core.RefreshScreen(65)
    End Sub
    
    public sub HandleInput()
        Dim angle As Integer

        if (_Aircraft.Manual) then
          if (Input.WasKeyTyped(Keys.VK_LEFT)) then
            angle = -30
		  end if
          if (Input.WasKeyTyped(Keys.VK_RIGHT)) then
            angle = angle + 30
		  end if
		
          if (angle <> 0) then
            _Aircraft.ChangeDirection(angle)
		  end if
        end if

        if (Input.WasKeyTyped(Keys.VK_T)) then
          _Aircraft.Manual =  not _Aircraft.Manual
		end if
    end sub
    
    public sub Update()
        _Aircraft.Update()
    end sub
  
    public sub RunGame()
        'Game Loop
        do
    		Core.ProcessEvents()
            HandleInput()
            Update()
            Draw()

    		Core.RefreshScreen()
        loop until Core.WindowCloseRequested()

        'Free Resources and Close Audio, to end the program.
        GameResources.FreeResources()
        Audio.CloseAudio()
    end sub


End Module
