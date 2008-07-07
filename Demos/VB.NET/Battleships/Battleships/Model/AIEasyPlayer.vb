''' <summary>
''' EasyAIPlayer is a type of AIPlayer. All shot that are taken are random
''' shot generated witht the GenerateCoords
''' </summary>
Public Class AIEasyPlayer : Inherits AIPlayer

    Public Sub New(ByVal controller As BattleShipsGame)
        MyBase.New(controller)
    End Sub

    Private _nextLocation As Location

    ''' <summary>
    ''' GenerateCoords generates shots at random location.
    ''' </summary>
    ''' <param name="row">the row it generates</param>
    ''' <param name="column">the column it generates</param>
    Protected Overrides Sub GenerateCoords(ByRef row As Integer, ByRef column As Integer)
        Do
            If _nextLocation IsNot Nothing Then
                row = _nextLocation.Row
                column = _nextLocation.Column
                _nextLocation = Nothing
            Else
                row = Player._Random.Next(0, EnemyGrid.Height)
                column = _Random.Next(0, EnemyGrid.Width)
            End If
        Loop While EnemyGrid.Item(row, column) <> TileView.Sea
    End Sub

    ''' <summary>
    ''' ProcessShot does nothing with the easy AIPlayer because this player
    ''' doesn't know anything beside generating random coordinates
    ''' </summary>
    ''' <param name="row">row it shot at</param>
    ''' <param name="col">the col it shot at</param>
    ''' <param name="result">the result from the last shot</param>
    Protected Overrides Sub ProcessShot(ByVal row As Integer, ByVal col As Integer, ByVal result As AttackResult)
        If result.Value = ResultOfAttack.Hit Then
            Select Case _Random.Next(4)
                Case 0 : _nextLocation = New Location(row + 1, col)
                Case 1 : _nextLocation = New Location(row - 1, col)
                Case 2 : _nextLocation = New Location(row, col + 1)
                Case 3 : _nextLocation = New Location(row, col - 1)
            End Select
        End If
    End Sub

End Class
