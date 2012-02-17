program PongCentipede;
uses
	SwinGame, sgTypes;

const
	PADDLE_HEIGHT_RATIO = 6;
	PADDLE_WIDTH_RATIO	= 40;
	MOVE_SPEED					= 5;

type
	Side	=	( Left, Right, Middle);

	Ball = record
		circ : Circle;
		v 	: Vector;
	end;


procedure Listen();
begin
	
end;

procedure Connect();
begin	
end;

procedure InitPlayer(var aPlayer : Rectangle; aSide : Side);
begin	
	aPlayer.height := ScreenHeight div PADDLE_HEIGHT_RATIO;
	aPlayer.width  := ScreenWidth  div PADDLE_WIDTH_RATIO;
	aPlayer.y 	   := Round((ScreenHeight div 2) - (aPlayer.height div 2)); 

	case aSide of
		aSide.Left  : aPlayer.x := Round(aPlayer.width * 2); 
		aSide.Right : aPlayer.x := Round(ScreenWidth - aPlayer.width * 2); 
		aSide.Middle: aPlayer.x := Round((ScreenWidth div 2) - (aPlayer.width div 2)); 
	end;
end;

procedure Init(aIsHost : Boolean; aConnectedPlayers : Integer; var aPlayer1 : Rectangle; var aBall : Circle);
begin
	if (aIsHost) then
	begin
		InitPlayer(aPlayer1, Side.Left);
		aBall.center.x := Round(ScreenWidth div 2);
		aBall.center.y := Round(ScreenHeight div 2);
		aBall.radius	 := (ScreenWidth  div PADDLE_WIDTH_RATIO) div 2;
	end;
end;

procedure UpdateBall();
begin
end;

procedure MovePlayer(var aPlayer : Rectangle);
begin
	if KeyDown(vk_UP) then 
		aPlayer.y -= MOVE_SPEED
	else if KeyDown(VK_DOWN) then aPlayer.y += MOVE_SPEED;
end;

procedure Draw(aPlayer1 : Rectangle; aBall : Circle);
begin
	FillRectangle(ColorWhite, aPlayer1);
	FillCircle(ColorWhite, aBall);
end;

procedure Update();
begin
end;

procedure Main();
var
	lBall : Circle;
	lPlayer : Rectangle;
	lConnections : Integer;
	lIsHost			: Boolean = True;
begin
	OpenGraphicsWindow('Pong Centipede', 640, 480);
	LoadDefaultColors();
	Init(lisHost, 0, lPlayer, lBall);
	repeat
		ProcessEvents();
		ClearScreen();
		Update();
		MovePlayer(lPlayer);
		Draw(lPlayer, lBall);
		DrawFrameRate(0,0);
		RefreshScreen(60);
	until WindowCloseRequested();
end;

begin
	Main();
end.