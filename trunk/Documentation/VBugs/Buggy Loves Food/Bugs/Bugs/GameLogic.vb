Module GameLogic

    Public score As Integer
    Private Levels As List(Of Level)

    Public Sub LoadLevels()
        Levels = New List(Of Level)

        Dim reader As StreamReader
        reader = File.OpenText(Core.GetPathToResource("Level.txt", ResourceKind.None))

        Dim line As String
        Dim parts As String()

        While Not reader.EndOfStream
            line = reader.ReadLine()
            parts = line.Split(",")

            Dim newLevel As Level
            newLevel = New Level(parts(0), parts(1).Trim())

            newLevel.createTime = Int(parts(2))
            newLevel.timeToCompleteLevel = Int(parts(3))
            newLevel.scoreToCompleteLevel = Int(parts(4))
            newLevel.percentBad = Val(parts(5))

            Levels.Add(newLevel)
        End While
    End Sub


    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Buggy Loves Food!", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        LoadLevels()

        Dim level As Integer
        level = 0

        Dim currentLevel As Level
        currentLevel = Levels(level)

        Dim myBug As New Bug()

        Dim gameTimer As Timer
        gameTimer = Core.CreateTimer()
        Core.StartTimer(gameTimer)

        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            currentLevel.Draw()
            myBug.Draw()
            Text.DrawText("Time: " & Core.GetTimerTicks(gameTimer) / 1000, Color.Black, GameFont("Courier"), 680, 0)

            currentLevel.Update(Core.GetTimerTicks(gameTimer))
            myBug.Update()
            myBug.MoveBug()
            currentLevel.CheckCollisions(myBug)

            If currentLevel.CheckEndLevel(gameTimer) Then
                level = level + 1
                currentLevel = Levels(level)
                Core.StartTimer(gameTimer)
            End If

            'If score >= 5 Then
            '    Core.StopTimer(gameTimer)
            '    Core.StartTimer(gameTimer)
            '    level2.Draw()
            '    level2.Update(Core.GetTimerTicks(gameTimer))
            '    myBug.Draw()
            '    myBug.Update()
            '    myBug.MoveBug()
            '    level2.CheckCollisions(myBug)
            'End If


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()
        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
