Module GameLogic

	'Numbers represents the position of the numbers on the wheel (00 is -1)
	Private _Numbers As Integer() = { _
		0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15, _
		3, 24, 36, 13, 1, -1, 27, 10, 25, 29, 12, 8, 19, 31, _
		18, 6, 21, 33, 16, 4, 23, 35, 14, 2 _
		}


	Private Function GetWheelPosition(ByVal startAt As Integer, ByVal distance As Integer) As Integer
		return startAt + distance mod 38
	End Function

	Private Function GetColorForPosition(ByVal wheelPosition As Integer) As Color
		if wheelPosition = 0 or wheelPosition = -1 then
			return Color.Green
		elseif wheelPosition mod 2 = 1 then
			return Color.Black
		else
			return Color.Red
		end if
	End Function

	Private Function GetNumberSpun(ByVal position As Integer) As Integer
		Dim index as Integer
		index = position mod 38
		
		return _Numbers(index)
	End Function


	Private Sub ShowNumber(ByVal wheelPosition As Integer)
		Dim textColor as Color
		Dim numberShown as Integer
		
		textColor = GetColorForPosition(wheelPosition)
		numberShown = GetNumberSpun(wheelPosition)
		
		DrawBackground(true)
		Text.DrawText(Str(numberShown), textColor, GameFont("ArialLarge"), 50, 50)
		Core.RefreshScreen()
	End Sub

	Private Sub SpinWheel()
		Dim startAt As Integer
		Dim distanceSpun As Integer
		Dim isAt As Integer, i As Integer
		Dim factor As Double
		Dim sleepFor As Integer
		
		startAt = Rnd() * 38
		
		'Up to 5 times around the wheel
		distanceSpun = Rnd() * 38 * Rnd() * 5
		
		factor = 1000 / (distanceSpun * distanceSpun)
		
		For i = 0 to distanceSpun
			
			sleepFor = i * i * factor
		
			Core.Sleep(sleepFor)
			ShowNumber(GetWheelPosition(startAt, i))
		Next
		
	
	End Sub


	Private Sub DrawBackground(spinWheel As Boolean)
		Graphics.DrawBitmap(GameImage("Background"), 0, 0)
	End Sub
	

    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        'Game Loop
        Do
			DrawBackground(true)
			
			SpinWheel()

            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
