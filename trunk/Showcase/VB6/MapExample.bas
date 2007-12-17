Attribute VB_Name = "MapExample"


Public Sub MapExample()
    Dim m As Map
    Dim balls() As sprite
    Dim tempString As CollisionSide
    Dim i As Long
    Dim j As Long
    Dim temp1 As Long
    Dim temp2 As Long
    Dim gravity As Vector
    Dim tempVector1 As Vector
    Dim tempVector2 As Vector
    Dim hit As Boolean
    Set m = New Map
    Set gravity = New Vector
    Set tempVector1 = New Vector
    Set tempVector2 = New Vector
    Call Randomize
    Set m = GameMap("test")
    ReDim balls(1)
    For i = 0 To UBound(balls, 1)
        Set balls(i) = New sprite
        Set balls(i) = Graphics.CreateSprite(GameImage("SmallBall"))
        
        Call balls(i).SetMovementVector(Physics.CreateVector_NoInvert(Rnd * 15, 0))
        Call balls(i).SetX(MappyLoader.EventPositionX(m, Event_Event1, i))
        Call balls(i).SetY(MappyLoader.EventPositionY(m, Event_Event1, i))
        'Call balls(i).SetMass(1)
    Next i
    Call gravity.setVector(Physics.CreateVector_NoInvert(0, 0.7)) '0.7
    Do Until SwinGame.MouseAndKey.IsKeyPressed(Keys_VK_N)
        If MouseAndKey.IsKeyPressed(Keys_VK_RIGHT) Then
            Call Camera.MoveVisualArea(2, 0)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_DOWN) Then
            Call Camera.MoveVisualArea(0, 2)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_UP) Then
            Call Camera.MoveVisualArea(0, -2)
        End If
        If MouseAndKey.IsKeyPressed(Keys_VK_LEFT) Then
            Call Camera.MoveVisualArea(-2, 0)
        End If
        Call MappyLoader.Drawmap(m)
        Call Text.DrawText(balls(0).GetY, red, GameFont("Courier"), 10, 100)
        Call Text.DrawText(balls(0).GetX, red, GameFont("Courier"), 10, 120)
        Call Text.DrawText(balls(1).GetY, red, GameFont("Courier"), 10, 140)
        Call Text.DrawText(balls(1).GetX, red, GameFont("Courier"), 10, 160)
        hit = False
        For i = 0 To UBound(balls, 1) - 1
            For j = i + 1 To UBound(balls, 1)
                If i <> j Then
                    If Physics.HaveSpritesCollided(balls(i), balls(j)) Then
                        hit = True
                        Call Graphics.MoveSprite(balls(i), Physics.InvertVector(balls(i).GetMovementVector))
                        Call Graphics.MoveSprite(balls(j), Physics.InvertVector(balls(j).GetMovementVector))
                        Call tempVector1.setVector(Physics.MultiplyVector(balls(i).GetMovementVector, 0.5))
                        Call tempVector2.setVector(Physics.MultiplyVector(balls(j).GetMovementVector, 0.5))
                        Do
                            Call Graphics.MoveSprite(balls(i), tempVector1.getVector)
                            Call Graphics.MoveSprite(balls(j), tempVector2.getVector)
                            If Physics.HaveSpritesCollided(balls(i), balls(j)) Then
                                Call Graphics.MoveSprite(balls(i), Physics.InvertVector(tempVector1))
                                Call Graphics.MoveSprite(balls(j), Physics.InvertVector(tempVector2))
                                Call tempVector1.setVector(Physics.MultiplyVector(tempVector1, 0.5))
                                Call tempVector2.setVector(Physics.MultiplyVector(tempVector2, 0.5))
                            End If
                        Loop While (Physics.Magnitude(tempVector1.getVector) < 0.05) And (Physics.Magnitude(tempVector2.getVector) < 0.5)
                        Call Physics.VectorCollision(balls(i), balls(j))
                    End If
                End If
            Next j
        Next i
        For i = 0 To UBound(balls)
            Call balls(i).SetMovementVector(Physics.AddVectors(balls(i).GetMovementVector, gravity.getVector))
            Call balls(i).SetMovementVector(Physics.MultiplyVector(balls(i).GetMovementVector, 0.995))
            Call Graphics.MoveSprite(balls(i), balls(i).GetMovementVector)
        Next i
        If hit = False Then
            For i = 0 To UBound(balls)
                tempString = MappyLoader.CollisionWithMap(m, balls(i))
                If (tempString = CollisionSide_Right) Or (tempString = CollisionSide_Left) Or (tempString = CollisionSide_TopLeft) Or (tempString = CollisionSide_TopRight) Or (tempString = CollisionSide_BottomLeft) Or (tempString = CollisionSide_BottomRight) Then
                    Call balls(i).SetMovementX(balls(i).GetMovementX * -1)
                    
                End If
                If (tempString = CollisionSide_Top) Or (tempString = CollisionSide_Bottom) Or (tempString = CollisionSide_TopLeft) Or (tempString = CollisionSide_TopRight) Or (tempString = CollisionSide_BottomLeft) Or (tempString = CollisionSide_BottomRight) Then
                    Call balls(i).SetMovementY(balls(i).GetMovementY * -1)
                End If
            Next i
        End If
        For i = 0 To UBound(balls)
            Call Graphics.DrawSprite(balls(i))
        Next i
        Call TitleDisplay.DrawOverlay("MappyLoader Example")
        Call Text.DrawFramerate(0, 0, GameFont("Courier"))
        Call SwinGame.Core.ProcessEvents
        Call SwinGame.Core.RefreshScreen_WithFrame(60)
        Call SwinGame.Graphics.ClearScreen
        If SwinGame.Core.WindowCloseRequested() = True Then
            Exit Sub
        End If
    Loop
    Call SwinGame.Core.Sleep(500)
    Call SwinGame.Core.ProcessEvents
    
End Sub
