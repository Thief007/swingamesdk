Module GameLogic
    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()

        'declare our new bug and  make it equal to the sprite image
        Dim bug As Sprite
        bug = Graphics.CreateSprite(GameImage("sprite"))

        'bug1 is the second bug on the screen
        Dim bug1 As Sprite
        bug1 = Graphics.CreateSprite(GameImage("sprite"))

        'set the bug's X position to the middle of the screen (taking into account the bug's width
        bug.X = 400 - bug.Width / 2
        bug.Y = 300 - bug.Height / 2

        bug1.X = 400 - bug1.Width / 2
        bug1.Y = 300 - bug1.Height / 2

        'set the speed of the bug
        bug.Movement.X = 0.5
        bug.Movement.Y = 0.5

        bug1.Movement.X = -0.7
        bug1.Movement.Y = 0.7

        'Game Loop
        Do
            'Clears the Screen to Black
            SwinGame.Graphics.ClearScreen(Color.White)

            'draw and update our bug
            Graphics.DrawSprite(bug)
            Graphics.UpdateSprite(bug)

            Graphics.DrawSprite(bug1)
            Graphics.UpdateSprite(bug1)

            'reverse the bug's X momement if it hit the right wall (taking into account it's width)
            If bug.X + bug.Width >= Core.ScreenWidth Then
                bug.Movement.X = -0.5
            End If

            'reverse the bug's X momement if it hit the left wall (not taking into account it's width as position is measured from the top left)
            If bug.X <= 0 Then
                bug.Movement.X = 0.5
            End If

            'reverse the bug's Y momement if it hit the bottom wall (taking into account its height)
            If bug.Y + bug.Height >= Core.ScreenHeight Then
                bug.Movement.Y = -0.5
            End If

            'reverse the bug's Y momement if it hit the top wall (not taking into account its height as position is measured from the top left)
            If bug.Y <= 0 Then
                bug.Movement.Y = 0.5
            End If

            If bug1.X + bug1.Width >= Core.ScreenWidth Then
                bug1.Movement.X = -0.5
            End If

            If bug1.X <= 0 Then
                bug1.Movement.X = 0.5
            End If

            If bug1.Y + bug1.Height >= Core.ScreenHeight Then
                bug1.Movement.Y = -0.5
            End If

            If bug1.Y <= 0 Then
                bug1.Movement.Y = 0.5
            End If


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

            'repeat until the user hits the close button
        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.

        'Close and free up the resources
        Graphics.FreeSprite(bug)
        Graphics.FreeSprite(bug1)
        FreeResources()
        Audio.CloseAudio()

    End Sub

End Module
