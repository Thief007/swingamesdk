program TestSDL_NetServer;
uses SDL13, SDL_Net, SysUtils, sgNetworking, sgShared, sgUtils, sgSharedUtils, SwinGame, sgTypes;

type 
  MessageData = packed record
    msg       : String;
    IP        : String;
    port      : LongInt;
  end;
  MessageDataArray = Array of MessageData;

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
  
  lHostSuccess := CreateTCPHost(StrToInt(ParamStr(2)));
  
  if not lHostSuccess then
    exit;
  
  for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do lConnectedToClientArray[i] := False;
  
  while (lPeers <> lConnectedToHost) or (lPeers <> lConnectedToClient) do
  begin
    if ServerAcceptTCPConnection() then
      lConnectedToHost += 1;
      
    lConnectedToClient := 0;
    for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do
    begin  
      if not lConnectedToClientArray[i] then    
        if OpenTCPConnectionToHost('127.0.0.1', lPorts[i]) then
          lConnectedToClientArray[i] := True;
    end;
    for i := Low(lConnectedToClientArray) to High(lConnectedToClientArray) do
      if lConnectedToClientArray[i] then
        lConnectedToClient += 1;
  end;      
  WriteLn('Connections: ', lConnectedToClient);
end;

function DequeueMessage() : MessageData;
begin    
  result.msg := GetMessage();
  result.ip  := GetIPFromMessage();
  result.port  := GetPortFromMessage();
  DequeueTopMessage();
end;

function PopAllMessages() : MessageDataArray;
var
  i : LongInt;
begin
  SetLength(result, GetMessageCount());
  for i := Low(result) to High(result) do
  begin
    result[i] := DequeueMessage();
  end;
end; 

function ReceiveMessage() : Boolean;
var
  lReceivedData : MessageDataArray;
  i             : Integer;
begin  
  result := False;
  if (not TCPMessageReceived()) then exit;
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
  close : String;
begin
  ReceiveMessage();
  Write('Send Message: ');
  if lInput = 'quit' then
    _Quit := True;
  ReadLn(lInput);
  close := SendTCPMessageTo(lInput, '127.0.0.1', 2000);

  if (close <> '') then
  begin
    WriteLn('CLosing: ', close);
    CloseTCPSenderSocket('127.0.0.1', 2000);
    WriteLn('Close Success');
  end;


  ReceiveMessage();
end;

procedure Broadcast();
var
  lInput : String = '';
begin
  ReceiveMessage();
  Write('Send Message To All: ');
  ReadLn(lInput);
  BroadcastTCPMessage(lInput);
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
  SendTCPMessageTo('Disconnecting from you', '127.0.0.1', 2001);
  WriteLn('Closing Socket at 2000...');
  WriteLn('Success? : ', CloseTCPReceiverSocket('127.0.0.1', 2000));
  WriteLn('Closing Host Socket at 2000...');
  WriteLn('Success? : ', CloseTCPHostSocket(2000));
  _Quit := true;
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