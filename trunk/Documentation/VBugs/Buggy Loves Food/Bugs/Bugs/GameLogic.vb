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

    Public Sub NextLevel()
        For i = 1 To 40
            Graphics.ClearScreen()
            Graphics.DrawBitmap(GameImage("nextlevel"), 0, 0)
            Text.DrawText("Next Level!", Color.White, GameFont("canker"), 300, 250)
            Core.RefreshScreen(60)
        Next
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
                If score < currentLevel.scoreToCompleteLevel Then
                    Do
                        Graphics.ClearScreen()
                        Graphics.DrawBitmap(GameImage("looser"), 0, 0)

                        Text.DrawText("You LOSE!", Color.White, GameFont("GR"), 250, 100)
                        Text.DrawText("Score  " & score, Color.White, GameFont("GRlittle"), 340, 250)
                        Core.RefreshScreen(30)
                        Core.ProcessEvents()
                    Loop Until Input.WasKeyTyped(Keys.VK_RETURN) Or SwinGame.Core.WindowCloseRequested() = True
                Else
                    NextLevel()
                    level = level + 1
                End If
                If currentLevel.levelName = "Beware the Evil Apples!" And score >= currentLevel.scoreToCompleteLevel Then
                    Do
                        Graphics.ClearScreen()
                        Graphics.DrawBitmap(GameImage("winner"), 0, 0)
                        Text.DrawText("You WON!!!!", Color.White, GameFont("cankerbig"), 100, 100)
                        Text.DrawText("Score: " & score, Color.White, GameFont("Courier"), 200, 300)
                        Core.RefreshScreen(30)
                        Core.ProcessEvents()
                    Loop Until Input.WasKeyTyped(Keys.VK_RETURN) Or SwinGame.Core.WindowCloseRequested() = True
                Else
                    currentLevel = Levels(level)
                    Core.StartTimer(gameTimer)
                End If
            End If


                'Refreshes the Screen and Processes Input Events
                Core.RefreshScreen()
                Core.ProcessEvents()
        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
