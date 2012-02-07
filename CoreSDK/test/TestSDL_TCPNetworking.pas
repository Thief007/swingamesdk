program TestSDL_NetServer;
uses SDL13, SDL_Net, SysUtils, sgDriverNetworkingSDL, sgShared, sgUtils, sgSharedUtils, SwinGame;

var
  lPorts : Array of Integer;
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
  
  SetLength(lPorts, lPeers);
  SetLength(lConnectedToClientArray, lPeers);
  
  for i := Low(lPorts) to High(lPorts) do
    lPorts[i] := StrToInt(ParamStr(i + 3));
  
  lHostSuccess := CreateHostProcedure(StrToInt(ParamStr(2)));
  
  if not lHostSuccess then
    exit;
  
  for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do lConnectedToClientArray[i] := False;
  
  while (lPeers <> lConnectedToHost) or (lPeers <> lConnectedToClient) do
  begin
    if ServerAcceptConnection() then
      lConnectedToHost += 1;
      
    lConnectedToClient := 0;
    for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do
    begin  
      if not lConnectedToClientArray[i] then    
        if OpenConnectionToHostProcedure('127.0.0.1', lPorts[i]) then
          lConnectedToClientArray[i] := True;
    end;
    for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do
      if lConnectedToClientArray[i] then
        lConnectedToClient += 1;
  end;      
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

function Connect() : Integer;
var
  lInput : String = '';
  lCount : Integer = 0;
  lMax   : Integer = 0;
  lPort  : Integer = 0;
begin    
  Write('Count of Clients to Connect: ');
  ReadLn(lInput);
  
  lMax := StrToInt(lInput);
  
  repeat
    Write('Client Port: ');
    ReadLn(lInput);

    lPort := StrToInt(lInput);
    OpenConnectionToHostProcedure('127.0.0.1', lPort);
    
    lCount += 1;
  until lCount = lMax;
  WriteLn('Connected to ', lCount, ' clients.');
  result := 0;
end;

function ListenForConnections() : Integer;
var
  lInput : String = '';
  lCount : Integer = 0;
  lMax   : Integer = 0;
begin  
  Write('Enter your port number: ');
  ReadLn(lInput);
  CreateHostProcedure(StrToInt(lInput));
  Write('Count of Clients to Connect: ');
  ReadLn(lInput);
  
  lMax := StrToInt(lInput);
  
  WriteLn('Listening for Connections.....');
  repeat
    if ServerAcceptConnection() then
      lCount += 1;
  until lCount = lMax;
  WriteLn('Connected to ', lCount, ' clients.');
  result := 0;
end;

function ReceiveMessage() : Boolean;
var
  lReceivedData : MessageDataArray;
  i             : Integer;
begin  
  result := False;
  if (not DataReceived()) then exit;
  lReceivedData := PopAllMessages();
  result := True;
  for i:= Low(lReceivedData) to High(lReceivedData) do
  begin
    WriteLn('Received Message: ', lReceivedData[i].msg, ' From ', lReceivedData[i].ip );
    if lReceivedData[i].msg = 'quit' then
      _Quit := True;
  end;
end;

procedure SendMessageToPeer();
var
  lInput : String = '';
  lPort  : Integer = 0;
begin
  Write('Enter Port: ');
  ReceiveMessage();
  ReadLn(lInput);
  lPort := StrToInt(lInput);
  Write('Message: ');
  ReadLn(lInput);
  SendMessageTo(lInput, '127.0.0.1', lPort);
  ReceiveMessage();
  if lInput = 'quit' then
    _Quit := True;
end;

procedure Broadcast();
var
  lInput : String = '';
begin
  ReceiveMessage();
  Write('Send Message To All: ');
  ReadLn(lInput);
  BroadcastMessageProcedure(lInput);
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
  SendMessageTo('(Automated Reply: Herp Derp)', '127.0.0.1', 2002);
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
      3 : SendMessageToPeer();
      4 : Broadcast();
      5 : ReceiveAndAutoReplyAll();
      6 : lQuit := True;
    end;
     
  until _Quit;
end;

begin
	main();
end.