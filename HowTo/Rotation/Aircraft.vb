public class Aircraft

    private _Aircraft As Sprite   	' The aircraft moving
    private _Shadow As Sprite     	' The aircraft's shadow
    private _Count As Integer
    private _Manual As Boolean
    
    Public Sub New()
        _Aircraft = Graphics.CreateSprite(GameResources.GameImage("Aircraft"), 120, 12, 80, 80)
        _Shadow = Graphics.CreateSprite(GameResources.GameImage("AircraftShadow"), 120, 12, 40, 40)
        _Aircraft.X = 610
        _Aircraft.Y = 300
        _Aircraft.Movement.SetTo(Physics.CreateVector(0, -1))

        _Count = 0
        _Manual = false
    End Sub
    
    Public Property Manual As Boolean
        Get
			return _Manual
		End Get
        Set (value as Boolean)
			_Manual = value
		End Set
    End Property

    '' Align the aircraft's shadow with the aircraft sprite
    Public Sub AlignShadow()
        _Shadow.X = _Aircraft.X + 30
        _Shadow.Y = _Aircraft.Y + 80
        _Shadow.CurrentFrame = _Aircraft.CurrentFrame
    End Sub

    ' Change the direction of the aircraft - select the correct frame
    ' and alter the direction of its movement
    Public Sub ChangeDirection(angle As Integer)
        if angle <> 0 then
            Dim m as Matrix2D
 			m = Physics.RotationMatrix(angle)
            
			Dim v as Vector
			v = Physics.Multiply(m, _Aircraft.Movement.AsVector())
            _Aircraft.Movement.SetTo(v)

            if angle = -30 then
              _Aircraft.CurrentFrame = (_Aircraft.CurrentFrame + 1) mod 12
            else 
              _Aircraft.CurrentFrame = _Aircraft.CurrentFrame - 1
              If _Aircraft.CurrentFrame < 0 Then
 				_Aircraft.CurrentFrame = 11
			  End If
            End If
        End If    
    End Sub
    
    Public Sub Wrap()
        // Wrap the sprite when offscreen
        If _Aircraft.X < -_Aircraft.Width then
            _Aircraft.X += GameLogic.SCREEN_WIDTH
		End If
        if _Aircraft.X > GameLogic.SCREEN_WIDTH + _Aircraft.Width then
            _Aircraft.X -= GameLogic.SCREEN_WIDTH
		End if
            
        if _Aircraft.Y < -_Aircraft.Height then
            _Aircraft.Y += GameLogic.SCREEN_HEIGHT	
		end if
		
        if (_Aircraft.Y > GameLogic.SCREEN_HEIGHT + _Aircraft.Height) then
            _Aircraft.Y -= GameLogic.SCREEN_HEIGHT
		end if
    End Sub
    
    public sub Draw()
        Dim offsetX, offsetY As Integer

        if (_Aircraft.X < 0) then
          offsetX = GameLogic.SCREEN_WIDTH
        elseif (_Aircraft.X + _Aircraft.Width > GameLogic.SCREEN_WIDTH) then
          offsetX = -GameLogic.SCREEN_WIDTH
		end if
		
        if (_Aircraft.Y < 0)
          offsetY = GameLogic.SCREEN_HEIGHT
        else if (_Aircraft.Y + _Aircraft.Height > GameLogic.SCREEN_HEIGHT)
          offsetY = -GameLogic.SCREEN_HEIGHT
		end if

        'Draw the sprite in its current position.
        Graphics.DrawSprite(_Shadow)
        Graphics.DrawSprite(_Aircraft)

        if ((offsetX <> 0) or (offsetY <> 0)) then
            'Draw it offset
            Graphics.DrawSprite(_Aircraft, offsetX, offsetY)
            Graphics.DrawSprite(_Shadow, offsetX, offsetY)
        end if
    end sub
    
    public sub Update()
        if false = Manual then
            _Count = _Count + 1
            if _Count mod 120 = 0 then
                ChangeDirection(-30)
                _Count = 0
            end if
        end if
        Graphics.MoveSprite(_Aircraft)
        AlignShadow()

        Wrap()
    end sub
End Class