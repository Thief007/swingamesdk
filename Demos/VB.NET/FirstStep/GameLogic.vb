Module GameLogic
	Sub PerformCollision(ball As Sprite, paddle as Sprite)
		Dim hitLine, lines() as LineSegment
		Dim vectTemp as Vector
		Dim shortest as Double
		Dim temp, ballCenter as Point2D		
		Dim pdlRect as Rectangle = Shapes.CreateRectangle(paddle)
		Dim ballRect as Rectangle = Shapes.CreateRectangle(ball)
		
		lines = Shapes.LinesFromRect(pdlRect)
		
		'Get the vector to move the ball sprite out of the paddle sprite
		Dim mv As Vector = Physics.VectorOutOfRectFromRect(ballRect, pdlRect, ball.Movement.AsVector())

		'Move the ball out of the paddle
		Graphics.MoveSprite(ball, mv)

		ballCenter = Shapes.CenterPoint(ball)
		
		shortest = ball.width 'it must be < than this...
		
		'find the closest point
		for i as Integer = 0 to 3
			temp = Shapes.ClosestPointOnLine(ballCenter, lines(i))
			vectTemp = Physics.VectorFromPoints(ballCenter, temp)

			if Physics.Magnitude(vectTemp) < shortest then
				shortest = Physics.Magnitude(vectTemp)
				hitLine = lines(i)
			end if
		next
		
		Physics.CircleCollisionWithLine(ball, hitLine)
	end sub

    Public Sub Main()
      'Opens a new Graphics Window
      Core.OpenGraphicsWindow("Game", 800, 600)

      'Open Audio Device
      Audio.OpenAudio()

      'Load Resources
      LoadResources()
      'Create Sprites
      Dim ball As Sprite
      ball = Graphics.CreateSprite(GameImage("Ball"))
      'Position
      ball.X = 400
      ball.Y = 300
      'Movement
      ball.Movement.X = 1
      ball.Movement.Y = 1

      Dim Paddle As Sprite
      Paddle = Graphics.CreateSprite(GameImage("Paddle"))
      'Position
      Paddle.X = 400
      Paddle.Y = 550

      Do
          'Clears the Screen to Black
          SwinGame.Graphics.ClearScreen()

          'Updating the game
          Graphics.MoveSprite(ball)

          If ball.X > 800 - ball.Width Then
              ball.Movement.X = -ball.Movement.X
              'Audio.PlaySoundEffect(GameSound("Ding"))
          End If

          If ball.Y > 600 - ball.Height Then
              ball.Movement.Y = -ball.Movement.Y
              'Audio.PlaySoundEffect(GameSound("Ding"))
          End If

          If Input.IsKeyPressed(SwinGame.Keys.VK_LEFT) Then
              Paddle.X = Paddle.X - 5
          End If
          If Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT) Then
              Paddle.X = Paddle.X + 5
          End If

          If Physics.HaveSpritesCollided(ball, Paddle) Then
			PerformCollision(ball, paddle)
          End If

          'Drawing the game
          'Graphics.DrawBitmap(GameImage("BG"), 0, 0)
          Graphics.DrawSprite(ball)
          Graphics.DrawSprite(Paddle)
          Text.DrawFramerate(0, 0, GameFont("Courier"))

          'Refreshes the Screen and Processes Input Events
          Core.RefreshScreen(65)
          Core.ProcessEvents()

      Loop Until SwinGame.Core.WindowCloseRequested() = True

      'Free Resources and Close Audio, to end the program.
      FreeResources()
      Audio.CloseAudio()

  End Sub

End Module
