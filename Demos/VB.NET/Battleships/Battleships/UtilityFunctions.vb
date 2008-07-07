''' <summary>
''' This includes a number of utility methods for
''' drawing and interacting with the Mouse.
''' </summary>
Module UtilityFunctions
    Public Const FIELD_TOP As Integer = 122
    Public Const FIELD_LEFT As Integer = 349
    Public Const FIELD_WIDTH As Integer = 418
    Public Const FIELD_HEIGHT As Integer = 418

    Public Const CELL_WIDTH As Integer = 40
    Public Const CELL_HEIGHT As Integer = 40
    Public Const CELL_GAP As Integer = 2

    Public Const SHIP_GAP As Integer = 2

    Private ReadOnly SMALL_SEA As Color = Color.FromArgb(255, 6, 60, 94)
    Private ReadOnly SMALL_SHIP As Color = Color.Gray
    Private ReadOnly SMALL_MISS As Color = Color.FromArgb(255, 1, 147, 220)
    Private ReadOnly SMALL_HIT As Color = Color.FromArgb(255, 169, 24, 37)

    Private ReadOnly LARGE_SEA As Color = Color.FromArgb(255, 6, 60, 94)
    Private ReadOnly LARGE_SHIP As Color = Color.Gray
    Private ReadOnly LARGE_MISS As Color = Color.FromArgb(255, 1, 147, 220)
    Private ReadOnly LARGE_HIT As Color = Color.FromArgb(255, 252, 2, 3)

    Private ReadOnly OUTLINE_COLOR As Color = Color.FromArgb(255, 5, 55, 88)
    Private ReadOnly SHIP_FILL_COLOR As Color = Color.Gray
    Private ReadOnly SHIP_OUTLINE_COLOR As Color = Color.White

    ''' <summary>
    ''' Determines if the mouse is in a given rectangle.
    ''' </summary>
    ''' <param name="x">the x location to check</param>
    ''' <param name="y">the y location to check</param>
    ''' <param name="w">the width to check</param>
    ''' <param name="h">the height to check</param>
    ''' <returns>true if the mouse is in the area checked</returns>
    Public Function IsMouseInRectangle(ByVal x As Integer, ByVal y As Integer, ByVal w As Integer, ByVal h As Integer) As Boolean
        Dim mouse As Point2D
        Dim result As Boolean = False

        mouse = Input.GetMousePosition()

        'if the mouse is inline with the button horizontally
        If mouse.X >= x And mouse.X <= x + w Then
            'Check vertical position
            If mouse.Y >= y And mouse.Y <= y + h Then
                result = True
            End If
        End If

        Return result
    End Function

    ''' <summary>
    ''' Draws a large field using the grid and the indicated player's ships.
    ''' </summary>
    ''' <param name="grid">the grid to draw</param>
    ''' <param name="thePlayer">the players ships to show</param>
    ''' <param name="showShips">indicates if the ships should be shown</param>
    Public Sub DrawField(ByVal grid As ISeaGrid, ByVal thePlayer As Player, ByVal showShips As Boolean)
        DrawCustomField(grid, thePlayer, False, showShips, FIELD_LEFT, FIELD_TOP, FIELD_WIDTH, FIELD_HEIGHT, CELL_WIDTH, CELL_HEIGHT, CELL_GAP)
    End Sub

    ''' <summary>
    ''' Draws a small field, showing the attacks made and the locations of the player's ships
    ''' </summary>
    ''' <param name="grid">the grid to show</param>
    ''' <param name="thePlayer">the player to show the ships of</param>
    Public Sub DrawSmallField(ByVal grid As ISeaGrid, ByVal thePlayer As Player)
        Const SMALL_FIELD_LEFT As Integer = 39, SMALL_FIELD_TOP As Integer = 373
        Const SMALL_FIELD_WIDTH As Integer = 166, SMALL_FIELD_HEIGHT As Integer = 166
        Const SMALL_FIELD_CELL_WIDTH As Integer = 13, SMALL_FIELD_CELL_HEIGHT As Integer = 13
        Const SMALL_FIELD_CELL_GAP As Integer = 4

        DrawCustomField(grid, thePlayer, True, True, SMALL_FIELD_LEFT, SMALL_FIELD_TOP, SMALL_FIELD_WIDTH, SMALL_FIELD_HEIGHT, SMALL_FIELD_CELL_WIDTH, SMALL_FIELD_CELL_HEIGHT, SMALL_FIELD_CELL_GAP)
    End Sub

    ''' <summary>
    ''' Draws the player's grid and ships.
    ''' </summary>
    ''' <param name="grid">the grid to show</param>
    ''' <param name="thePlayer">the player to show the ships of</param>
    ''' <param name="small">true if the small grid is shown</param>
    ''' <param name="showShips">true if ships are to be shown</param>
    ''' <param name="left">the left side of the grid</param>
    ''' <param name="top">the top of the grid</param>
    ''' <param name="width">the width of the grid</param>
    ''' <param name="height">the height of the grid</param>
    ''' <param name="cellWidth">the width of each cell</param>
    ''' <param name="cellHeight">the height of each cell</param>
    ''' <param name="cellGap">the gap between the cells</param>
    Private Sub DrawCustomField(ByVal grid As ISeaGrid, ByVal thePlayer As Player, ByVal small As Boolean, ByVal showShips As Boolean, ByVal left As Integer, ByVal top As Integer, ByVal width As Integer, ByVal height As Integer, ByVal cellWidth As Integer, ByVal cellHeight As Integer, ByVal cellGap As Integer)
        'Graphics.FillRectangle(Color.Blue, left, top, width, height)

        Dim rowTop As Integer
        Dim colLeft As Integer

        'Draw the grid
        For row As Integer = 0 To 9
            rowTop = top + (cellGap + cellHeight) * row

            For col As Integer = 0 To 9
                colLeft = left + (cellGap + cellWidth) * col

                Dim fillColor As Color
                Dim draw As Boolean

                draw = True

                Select Case grid.Item(row, col)
                    Case TileView.Ship
                        draw = False
                        'If small Then fillColor = _SMALL_SHIP Else fillColor = _LARGE_SHIP
                    Case TileView.Miss
                        If small Then fillColor = SMALL_MISS Else fillColor = LARGE_MISS
                    Case TileView.Hit
                        If small Then fillColor = SMALL_HIT Else fillColor = LARGE_HIT
                    Case TileView.Sea, TileView.Ship
                        If small Then fillColor = SMALL_SEA Else draw = False
                End Select

                If draw Then
                    Graphics.FillRectangle(fillColor, colLeft, rowTop, cellWidth, cellHeight)
                    If Not small Then
                        Graphics.DrawRectangle(OUTLINE_COLOR, colLeft, rowTop, cellWidth, cellHeight)
                    End If
                End If
            Next
        Next

        If Not showShips Then
            Exit Sub
        End If

        Dim shipHeight, shipWidth As Integer

        'Draw the ships
        For Each s As Ship In thePlayer
            If s Is Nothing OrElse Not s.IsDeployed Then Continue For
            rowTop = top + (cellGap + cellHeight) * s.Row + SHIP_GAP
            colLeft = left + (cellGap + cellWidth) * s.Column + SHIP_GAP

            If s.Direction = Direction.LeftRight Then
                shipHeight = cellHeight - (SHIP_GAP * 2)
                shipWidth = (cellWidth + cellGap) * s.Size - (SHIP_GAP * 2) - cellGap
            Else
                'Up down
                shipHeight = (cellHeight + cellGap) * s.Size - (SHIP_GAP * 2) - cellGap
                shipWidth = cellWidth - (SHIP_GAP * 2)
            End If

            Graphics.FillRectangle(SHIP_FILL_COLOR, colLeft, rowTop, shipWidth, shipHeight)
            Graphics.DrawRectangle(SHIP_OUTLINE_COLOR, colLeft, rowTop, shipWidth, shipHeight)
        Next
    End Sub

    Private _message As String

    ''' <summary>
    ''' The message to display
    ''' </summary>
    ''' <value>The message to display</value>
    ''' <returns>The message to display</returns>
    Public Property Message() As String
        Get
            Return _message
        End Get
        Set(ByVal value As String)
            _message = value
        End Set
    End Property

    'Draws the message to the screen
    Public Sub DrawMessage()
        Text.DrawText(Message, Color.White, GameFont("Menu"), FIELD_LEFT, FIELD_TOP - 20)
    End Sub

    ''' <summary>
    ''' Draws the background for the current state of the game
    ''' </summary>
    Public Sub DrawBackground()
        Select Case CurrentState
            Case GameState.ViewingMainMenu, GameState.ViewingGameMenu, GameState.AlteringSettings, GameState.ViewingHighScores
                Graphics.DrawBitmap(GameImage("Menu"), 0, 0)
            Case GameState.Discovering, GameState.EndingGame
                Graphics.DrawBitmap(GameImage("Discovery"), 0, 0)
            Case GameState.Deploying
                Graphics.DrawBitmap(GameImage("Deploy"), 0, 0)
            Case Else
                Graphics.ClearScreen()
        End Select

        Text.DrawFramerate(675, 585, GameFont("CourierSmall"))
    End Sub
End Module
