program HowToCreateTCPProgram;
uses
	SwinGame;

const
  HOST_SOCKET = 2000;

function SelectPeerType() : Boolean
var 
  lInput : String = '';
begin
  Write('Are You Hosting? [y/n]: ');
  while (lInput <> 'y') and (lInput <> 'n') do
  begin
    ReadLn(lInput);
    if (lInput = 'y') then 
    begin
      CreateTCPHost(HOST_SOCKET); 
      result := True;    
    end else if (lInput = 'n') then
      result := False;   
  end;
end;

function WaitForConnections() : Connection;
begin  
  WriteLn('Waiting for Connections....');
  result := nil
  while (result = nil)
  begin
    ServerAcceptTCPConnection();
    result := FetchConnection();
  end;
  WriteLn('Connection Established.');
end;

function WaitToConnectToHost() : Connection;
begin
  WriteLn('Connecting to Host....');
  result := nil
  while (result = nil)
    result := OpenTCPConnectionToHost('127.0.0.1', HOST_SOCKET);
  WriteLn('Connected to Host');
end;

procedure HandleMessages(aHost, aClient : Connection);
var
  lQuit : Boolean = False;
  lInput : String = '';
  lCounter : Integer = 0;
const 
  AUTO_MESSAGE_COUNT = 10;
  SEND_DELAY         = 1000;
begin
  while lCounter <> AUTO_MESSAGE_COUNT do
  begin
    if Assigned(aHost) then
    begin
      WriteLn('Client Received Message: ', ReadMessage(aHost));
    end else if Assigned(aClient) then
    begin
      WriteLn('Client Received Message: ', ReadMessage(lalient));
    end;
    Delay(SEND_DELAY);
    lCounter += 1;
  end;
end;

procedure Main();
var
  lConA, lConB, lTmp, lTmpA : Connection;
  lMsgReceived : Boolean = False;
  lInput : String = '';
  lIsHost : Boolean = False;
  lQuit : Boolean = False;
  lHost : Connection = nil;
  lClient : Connection = nil;
begin
  lIsHost := SelectPeerType();

  if (lIsHost) then
    lHost := WaitForConnections();
  else
    lClient := WaitToConnectToHost();
  
  HandleMessages(lHost, lClient);

  CloseAllConnections();
end;

begin
  main();
end.