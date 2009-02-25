Module GameLogic
    Public Sub ContolMusic()
        'if "m" was typed - stop or start music
        If Input.WasKeyTyped(SwinGame.Keys.VK_M) Then
            If Audio.IsMusicPlaying() = True Then
                Audio.StopMusic()
            Else
                Audio.PlayMusic(GameMusic("lion"), -1)
            End If
        End If
    End Sub
    Public Sub ChangeVolume()
        'turns the volume up and down if the arrow keys up and down are hit
        If Input.IsKeyPressed(SwinGame.Keys.VK_UP) Then
            Audio.SetMusicVolume(Audio.MusicVolume + 0.01F)
        End If

        If Input.IsKeyPressed(SwinGame.Keys.VK_DOWN) Then
            Audio.SetMusicVolume(Audio.MusicVolume - 0.01F)
        End If
    End Sub
    Public Sub DrawMouse()
        'replaces the pointer with the target
        Dim mousePoint As Point2D
        mousePoint = Input.GetMousePosition() 'gets mouse position at a particular time
        Graphics.DrawBitmapOnScreen(GameImage("target"), mousePoint.X - 20, mousePoint.Y - 20) 'draws the new mouse pointer
    End Sub



    Public Sub Main()
        'Opens a new Graphics Window
        Core.OpenGraphicsWindow("Game", 800, 600)

        'Open Audio Device
        Audio.OpenAudio()

        'Load Resources
        LoadResources()
        Input.ShowMouse(False) 'hides the original ouse pointer

        'BUG 1 INFORMATION!!!!
        'bug 1 variable
        Dim bug As Sprite
        bug = Graphics.CreateSprite(GameImage("sprite"))

        'bug1 positioning in the middle of the screen
        bug.X = 400 - bug.Width / 2
        bug.Y = 300 - bug.Height / 2

        'bug 1 movement 
        bug.Movement.X = 0.5
        bug.Movement.Y = 0.5

        'BUG 2 INFORMATION!!!!!
        'bug 2 variable
        Dim bug1 As Sprite
        bug1 = Graphics.CreateSprite(GameImage("sprite"))

        'bug 2 positioning in the middle of the screen
        bug1.X = 400 - bug1.Width / 2
        bug1.Y = 300 - bug1.Height / 2

        'bug 2 movement
        bug1.Movement.X = -0.7
        bug1.Movement.Y = 0.7

        'Playing music in the loop of infinity
        Audio.PlayMusic(GameMusic("lion"), -1)

        'Point2D variable declaration - for mouse position
        Dim mousePoint As Point2D

        'plays the hit sound
        Audio.PlaySoundEffect(GameSound("hit"), 3)
        Audio.PlaySoundEffect(GameSound("hit"), 10, 5)

        'declares our dead bug
        Dim deadBug As Sprite
        deadBug = Graphics.CreateSprite(GameImage("deadBug"), 40, 10, 57, 43)

        Dim deadBug1 As Sprite
        deadBug1 = Graphics.CreateSprite(GameImage("deadBug"), 40, 10, 57, 43)


        'Game Loop
        Do
            'Clears the Screen to White (customized color)
            SwinGame.Graphics.ClearScreen(Color.White)

            'BUG 1 MOVEMENT!
            'Draws and Updates he bug
            Graphics.DrawSprite(bug)
            Graphics.UpdateSprite(bug)

            'collision with the right edge of the screen
            If bug.X + bug.Width >= Core.ScreenWidth Then
                bug.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the left edge of the screen
            If bug.X <= 0 Then
                bug.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the bottom edge of the screen
            If bug.Y + bug.Height >= Core.ScreenHeight Then
                bug.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the top edge of the screen
            If bug.Y <= 0 Then
                bug.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If

            'BUG 2 MOVEMENT!!
            'Draws and updates the bug
            Graphics.DrawSprite(bug1)
            Graphics.UpdateSprite(bug1)

            'collision with the right edge of the screen
            If bug1.X + bug1.Width >= Core.ScreenWidth Then
                bug1.Movement.X = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the left edge of the screen
            If bug1.X <= 0 Then
                bug1.Movement.X = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the bottom edge of the screen
            If bug1.Y + bug1.Height >= Core.ScreenHeight Then
                bug1.Movement.Y = -0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If
            'collision with the top edge of the screen
            If bug1.Y <= 0 Then
                bug1.Movement.Y = 0.5
                Audio.PlaySoundEffect(GameSound("hit"))
            End If


            'draws the mouse pointer on the screen.

            mousePoint = Input.GetMousePosition() 'gets mouse position at a particular time

            ''plays squash when mouse was clicked on the bug1
            If Physics.IsSpriteOnScreenAt(bug, mousePoint.X, mousePoint.Y) Then
                If Input.MouseWasClicked(MouseButton.LeftButton) Then
                    Audio.PlaySoundEffect(GameSound("splat"))
                End If
            End If

            'plays squash when mouse was clicked on the bug2
            If Physics.IsSpriteOnScreenAt(bug1, mousePoint.X, mousePoint.Y) Then
                If Input.MouseWasClicked(MouseButton.LeftButton) Then
                    Audio.PlaySoundEffect(GameSound("splat"))

                    bug1.Movement.X = 0
                    bug1.Movement.Y = 0
                    deadBug.X = bug1.X
                    deadBug.Y = bug1.Y
                    bug1 = deadBug
                    Graphics.DrawSprite(deadBug)
                    Graphics.UpdateSpriteAnimation(deadBug)
                    deadBug.EndingAction = SpriteEndingAction.Stop
                End If
            End If

            'plays squash when mouse was clicked on the bug2
            If Physics.IsSpriteOnScreenAt(bug, mousePoint.X, mousePoint.Y) Then
                If Input.MouseWasClicked(MouseButton.LeftButton) Then
                    Audio.PlaySoundEffect(GameSound("splat"))

                    bug.Movement.X = 0
                    bug.Movement.Y = 0
                    deadBug1.X = bug.X
                    deadBug1.Y = bug.Y
                    bug = deadBug1
                    Graphics.DrawSprite(deadBug1)
                    Graphics.UpdateSpriteAnimation(deadBug1)
                    deadBug1.EndingAction = SpriteEndingAction.Stop
                End If
            End If
            'Call the sub ChangeVolume() and DrawMouse
            ChangeVolume()
            Drawmouse()


            'Refreshes the Screen and Processes Input Events
            Core.RefreshScreen()
            Core.ProcessEvents()

        Loop Until SwinGame.Core.WindowCloseRequested() = True

        'Free Resources and Close Audio, to end the program.

        FreeResources()
        Audio.CloseAudio()
        Music.Stop()
    End Sub

End Module
