Attribute VB_Name = "Aircraft"
Dim Aircraft As Sprite     ' The aircraft moving
Dim Shadow As Sprite       ' The aircraft's shadow
Dim Count As Integer
Public Manual As Boolean

Sub InitAircraft()
    Set Aircraft = Graphics.CreateSprite_MultiFPC(GameResources.GameImage("Aircraft"), 120, 12, 80, 80)
    Set Shadow = Graphics.CreateSprite_MultiFPC(GameResources.GameImage("AircraftShadow"), 120, 12, 40, 40)
    
    Call Aircraft.setX(610)
    Call Aircraft.setY(300)
    Call Aircraft.SetMovementVector(Physics.CreateVector(0, -1, False))

    Count = 0
    Manual = False
End Sub

'' Align the aircraft's shadow with the aircraft sprite
Public Sub AlignShadow()
    Call Shadow.setX(Aircraft.getX() + 30)
    Call Shadow.setY(Aircraft.getY() + 80)
    Call Shadow.SetCurrentFrame(Aircraft.GetCurrentFrame())
End Sub

' Change the direction of the aircraft - select the correct frame
' and alter the direction of its movement
Public Sub ChangeDirection(angle As Integer)
    If angle <> 0 Then
        Dim m As Matrix2D
        Dim n As Single
        n = angle
        Set m = Physics.RotationMatrix(n)
        
        Dim v As Vector
        Set v = Physics.Multiply_Vector(m, Aircraft.GetMovementVector())
        Call Aircraft.SetMovementVector(v)

        If angle = -30 Then
          Call Aircraft.SetCurrentFrame((Aircraft.GetCurrentFrame() + 1) Mod 12)
        Else
          Call Aircraft.SetCurrentFrame(Aircraft.GetCurrentFrame - 1)
          If Aircraft.GetCurrentFrame < 0 Then
            Call Aircraft.SetCurrentFrame(11)
          End If
        End If
    End If
End Sub

Public Sub Wrap()
    ' Wrap the sprite when offscreen
    If Aircraft.getX() < -Aircraft.GetWidth() Then
        Call Aircraft.setX(Aircraft.getX + GameLogic.SCREEN_WIDTH)
    End If
    If Aircraft.getX > GameLogic.SCREEN_WIDTH + Aircraft.GetWidth Then
        Call Aircraft.setX(Aircraft.getX - GameLogic.SCREEN_WIDTH)
    End If
        
    If Aircraft.getY < -Aircraft.GetHeight Then
        Call Aircraft.setY(Aircraft.getY + GameLogic.SCREEN_HEIGHT)
    End If
    
    If (Aircraft.getY > GameLogic.SCREEN_HEIGHT + Aircraft.GetHeight) Then
        Call Aircraft.setY(Aircraft.getY - GameLogic.SCREEN_HEIGHT)
    End If
End Sub

Public Sub Draw()
    Dim offsetX, offsetY As Integer

    If (Aircraft.getX < 0) Then
      offsetX = GameLogic.SCREEN_WIDTH
    ElseIf (Aircraft.getX + Aircraft.GetWidth > GameLogic.SCREEN_WIDTH) Then
      offsetX = -GameLogic.SCREEN_WIDTH
    End If
    
    If (Aircraft.getY < 0) Then
      offsetY = GameLogic.SCREEN_HEIGHT
    ElseIf (Aircraft.getY + Aircraft.GetHeight > GameLogic.SCREEN_HEIGHT) Then
      offsetY = -GameLogic.SCREEN_HEIGHT
    End If

    'Draw the sprite in its current position.
    Call Graphics.DrawSprite(Shadow)
    Call Graphics.DrawSprite(Aircraft)

    If ((offsetX <> 0) Or (offsetY <> 0)) Then
        'Draw it offset
        Call Graphics.DrawSprite_WithOffset(Aircraft, offsetX, offsetY)
        Call Graphics.DrawSprite_WithOffset(Shadow, offsetX, offsetY)
    End If
End Sub

Public Sub Update()
    If False = Manual Then
        Count = Count + 1
        If Count Mod 120 = 0 Then
            ChangeDirection (-30)
            Count = 0
        End If
    End If
    Call Graphics.MoveSprite_NoVector(Aircraft)
    Call AlignShadow

    Call Wrap
End Sub
