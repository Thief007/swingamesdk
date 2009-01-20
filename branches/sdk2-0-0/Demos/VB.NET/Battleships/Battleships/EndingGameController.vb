Imports SwinGame

''' <summary>
''' The EndingGameController is responsible for managing the interactions at the end
''' of a game.
''' </summary>

Module EndingGameController

    ''' <summary>
    ''' Draw the end of the game screen, shows the win/lose state
    ''' </summary>
    Public Sub DrawEndOfGame()
        DrawField(ComputerPlayer.PlayerGrid, ComputerPlayer, True)
        DrawSmallField(HumanPlayer.PlayerGrid, HumanPlayer)

        If HumanPlayer.IsDestroyed Then
            Text.DrawTextLines("YOU LOSE!", Color.White, Color.Transparent, GameFont("ArialLarge"), FontAlignment.AlignCenter, 0, 0, Core.ScreenWidth(), Core.ScreenHeight())
        Else
            Text.DrawTextLines("-- WINNER --", Color.White, Color.Transparent, GameFont("ArialLarge"), FontAlignment.AlignCenter, 0, 0, Core.ScreenWidth(), Core.ScreenHeight())
        End If
    End Sub

    ''' <summary>
    ''' Handle the input during the end of the game. Any interaction
    ''' will result in it reading in the highscore.
    ''' </summary>
    Public Sub HandleEndOfGameInput()
        If Input.MouseWasClicked(MouseButton.LeftButton) _
            OrElse Input.WasKeyTyped(Keys.VK_RETURN) _
            OrElse Input.WasKeyTyped(Keys.VK_ESCAPE) Then
            ReadHighScore(HumanPlayer.Score)
            EndCurrentState()
        End If
    End Sub

End Module
