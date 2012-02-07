program TestSDL_NetClient;
uses SDL13, SDL_Net, SysUtils, sgDriverNetworkingSDL, sgShared, sgUtils, sgSharedUtils, SwinGame;

var
  _Ports : Array of Integer;
  _Quit : Boolean;

procedure ConnectAndListen();
var
  lPeers, i : Integer;
  lConnectedToHost : Integer = 0;
  lConnectedToClient : Integer = 0;
  lConnectedToClientArray : Array of Boolean;
  lHostSuccess : Boolean;
begin
  lPeers := StrToInt(ParamStr(1));
  
  SetLength(_Ports, lPeers);
  SetLength(lConnectedToClientArray, lPeers);
  
  OpenUDPListenerSocket(StrToInt(ParamStr(2)));
  OpenUDPSendSocket();
  for i := Low(_Ports) to High(_Ports) do
    _Ports[i] := StrToInt(ParamStr(i + 3)); 
  WriteLn('Connections: ', lConnectedToClient);
end;

function InitialOptionsQuery() : Integer;
var
  lInput : String = '';
begin
  while (lInput <> '1') and (lInput <> '2') and (lInput <> '3') and (lInput <> '4') and (lInput <> '5') do
  begin
    WriteLn('What would you like to do?');
    WriteLn('3: Send Message To');
    WriteLn('4: BroadcastMessage');
    WriteLn('5: Receive and AutoReply Broadcast');
    ReadLn(lInput)
  end;
  result := StrToInt(lInput);
end;

function ReceiveMessage() : Boolean;
var
  lReceivedData : MessageDataArray;
  i             : Integer;
begin  
  result := False;
  if (not ReceiveUDPMessage()) then exit;
  lReceivedData := PopAllMessages();
  result := True;
  for i:= Low(lReceivedData) to High(lReceivedData) do
  begin
    WriteLn('Received Message: ', lReceivedData[i].msg, ' From ', lReceivedData[i].ip );
    if lReceivedData[i].msg = 'quit' then
      _Quit := True;
  end;
end;


procedure Broadcast();
var
  lInput : String = '';
  i      : Integer;
begin
  ReceiveMessage();
  Write('Send Message To All: ');
  ReadLn(lInput);
  for i := Low(_Ports) to High(_Ports) do
  begin
    SendUDPMessage(lInput, '127.0.0.1', _Ports[i]);
  end;
  ReceiveMessage();
  if lInput = 'quit' then
    _Quit := True;
end;

procedure ReceiveAndAutoReplyAll();
var
  lReceivedData : MessageData;
begin  
  if not ReceiveMessage() then exit;
  WriteLn('Sending AutoReply...');
  SendUDPMessage('(Automated Reply: Herp Derp)', '127.0.0.1', 2001);
end;

procedure Main();
var
  lState : Integer = 0;
  lQuit   : Boolean = False;
begin  
  ConnectAndListen();
  repeat
    case lState of
      0 : lState := InitialOptionsQuery();
      4 : Broadcast();
      5 : ReceiveAndAutoReplyAll();
      6 : lQuit := True;
    end;
     
  until _Quit;
end;

begin
	main();
end.