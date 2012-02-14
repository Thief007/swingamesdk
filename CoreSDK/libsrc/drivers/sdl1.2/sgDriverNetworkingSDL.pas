unit sgDriverNetworkingSDL;

interface
uses sgShared, SDL_Net, sgNamedIndexCollection, sgTypes;

   //TCP
  function CreateTCPHostProcedure              (aPort : LongInt; var aConSocketIndexes : NamedIndexCollection) : Boolean;
  function OpenTCPConnectionToHostProcedure    (aIP : String; aPort : LongInt; var aSendSocketIndexes : NamedIndexCollection) : Boolean;
  function ServerAcceptTCPConnectionProcedure  (var aReceiveSocketIndexes, aConSocketIndexes : NamedIndexCollection) : String;
  function TCPMessageReceivedProcedure         (aReceiveSocketIndexes : NamedIndexCollection) : Boolean;    
  function BroadcastTCPMessageProcedure        (aMsg : String; aSendSocketIndexes : NamedIndexCollection) : StringArray;
  function SendTCPMessageToProcedure           (aMsg, aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : String;
  //UDP
  function OpenUDPListenerSocketProcedure      (aPort : LongInt; aUDPSocketIndexes : NamedIndexCollection) : Boolean;
  function OpenUDPSendSocketProcedure          () : Boolean;
  function UDPMessageReceivedProcedure         () : Boolean;
  function SendUDPMessageProcedure             (aMsg, aIP : String; aPort : LongInt) : Boolean;

  procedure LoadSDLNetworkingDriver            (); 

  function CloseTCPHostSocketProcedure        (var aCollection : NamedIndexCollection; aPort: LongInt) : Boolean;  
  function CloseTCPReceiverSocketProcedure    (var aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  function CloseTCPSenderSocketProcedure      (var aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  function CloseUDPListenSocketProcedure      (var aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;
  function CloseUDPSendSocketProcedure        () : Boolean;

  function GetMyIPProcedure                   () : String;
    
type
  
  TCPSocketArray = Array of PTCPSocket;
  UDPSocketArray = Array of PUDPSocket;
      
implementation
uses SysUtils, sgUtils, sgNetworking, sgDriverNetworking, StrUtils;
var
  _ConSockets             : TCPSocketArray;
  _ReceiveSockets         : TCPSocketArray;
  _SendSockets            : TCPSocketArray;
  _Socketset              : PSDLNet_SocketSet;
  _UDPSendSocket          : PUDPSocket = nil;
  _UDPListenSockets       : UDPSocketArray;
  _UDPSendPacket          : PUDPPacket = nil;
  _UDPReceivePacket       : PUDPPacket = nil;
  _Buffer                 : Array [0..512] of Char;
  _BufferPtr              : ^Char;
    
// -- Start Connection Code

  function AddUDPSocket(aPort : String; aSockets : UDPSocketArray; aNewSocket : PUDPSocket; var aCollection : NamedIndexCollection) : UDPSocketArray;
  var
    lTmpSockets : UDPSocketArray;
    i         : LongInt;
  begin
    if HasName(aCollection, aPort) then begin result := aSockets; exit; end;
    SetLength(lTmpSockets, Length(aSockets) + 1);
    for i := Low(aSockets) to High(aSockets) do
      lTmpSockets[i] := aSockets[i];
    lTmpSockets[High(lTmpSockets)] := aNewSocket;
    AddName(aCollection, aPort);
    result := lTmpSockets;    
  end;
  
  function AddSocket(aIP : String; aSockets : TCPSocketArray; aNewSocket : PTCPSocket; var aCollection : NamedIndexCollection) : TCPSocketArray;
  var
    lTmpSockets : Array of PTCPSocket;
    i         : LongInt;
  begin
    if HasName(aCollection, aIP) then begin result := aSockets; exit; end;
    SetLength(lTmpSockets, Length(aSockets) + 1);
    for i := Low(aSockets) to High(aSockets) do
      lTmpSockets[i] := aSockets[i];
    lTmpSockets[High(lTmpSockets)] := aNewSocket;
    AddName(aCollection, aIP);
    result := lTmpSockets;    
  end;

  function GetTCPPort(aNewSocket : PTCPSocket) : String;
  var
    lRemoteIP : PIPAddress;
  begin
    lRemoteIP := SDLNet_TCP_GetPeerAddress(aNewSocket);
    result := IntToStr(SDLNet_Read16(@lRemoteIP^.port));
  end;

  function GetTCPIP(aNewSocket : PTCPSocket) : String;
  var
    lRemoteIP : PIPAddress;
  begin
    lRemoteIP := SDLNet_TCP_GetPeerAddress(aNewSocket);
    result := HexStrToIPv4(DecToHex(SDLNet_Read32(@lRemoteIP^.host)));
  end;
  
  function AddSocket(aSockets : TCPSocketArray; aNewSocket : PTCPSocket; aCollection : NamedIndexCollection) : TCPSocketArray;
  var
    lRemoteIP : PIPAddress;
    lIP       : String;
  begin
    lRemoteIP := SDLNet_TCP_GetPeerAddress(aNewSocket);
    lIP := HexStrToIPv4(DecToHex(SDLNet_Read32(@lRemoteIP^.host))) +':'+ IntToStr(SDLNet_Read16(@lRemoteIP^.port));
    result := AddSocket(lIP, aSockets, aNewSocket, aCollection);
  end;
  
  function OpenTCPConnection(aIP : PChar; aPort : LongInt) : PTCPSocket;
  var
    lIPAddress : TIPAddress;  
  begin
    result := nil;
    if (SDLNet_ResolveHost(lIPAddress, aIP, aPort) < 0) then
  		exit;//RaiseWarning('SDLNet_ResolveHost: ' + SDLNet_GetError());
    
  	result := SDLNet_TCP_Open(lIPAddress); 

  //	if (not Assigned(result)) then
  //		RaiseWarning('SDLNet_TCP_Open:' + SDLNet_GetError() + ' ' + aIP);
  end;
  
  function CreateTCPHostProcedure(aPort : LongInt; var aConSocketIndexes : NamedIndexCollection) : Boolean;
  var
    lTempSocket  : PTCPSocket = nil;
    lIdentifier : String;   
  begin
    lTempSocket := OpenTCPConnection(nil, aPort);
    if Assigned(lTempSocket) then
    begin
      lIdentifier := IntToStr(aPort);
      if HasName(aConSocketIndexes, lIdentifier) then begin result := False; exit; end;
      _ConSockets := AddSocket(lIdentifier, _ConSockets, lTempSocket, aConSocketIndexes);
    end;
    result := Assigned(lTempSocket);    
  end;
  
  function OpenTCPConnectionToHostProcedure(aIP : String; aPort : LongInt;var aSendSocketIndexes : NamedIndexCollection) : Boolean;
  var
    lTempSocket : PTCPSocket = nil; 
    lIdentifier : String;   
  begin    
    lTempSocket := OpenTCPConnection(PChar(aIP), aPort);
    if Assigned(lTempSocket) then
    begin
      lIdentifier := GetTCPIP(lTempSocket) + ':' + IntToStr(aPort);
      if HasName(aSendSocketIndexes, lIdentifier) then begin result := False; exit; end;
      _SendSockets := AddSocket(GetTCPIP(lTempSocket) + ':' + IntToStr(aPort), _SendSockets, lTempSocket, aSendSocketIndexes);
    end;
    result := Assigned(lTempSocket);
  end;   
  
  function ServerAcceptTCPConnectionProcedure(var aReceiveSocketIndexes, aConSocketIndexes : NamedIndexCollection) : String;
  var
    lTempSocket : PTCPSocket = nil;
    i : LongInt;
    lIdentifier : String;
  begin  
    result := '';
    for i := Low(_ConSockets) to High(_ConSockets) do
    begin
      lTempSocket := SDLNet_TCP_Accept(_ConSockets[i]);
      if Assigned(lTempSocket) then
      begin
        lIdentifier := GetTCPIP(lTempSocket) + ':' + NameAt(aConSocketIndexes, i);
        if HasName(aReceiveSocketIndexes, lIdentifier) then exit;
        _ReceiveSockets := AddSocket(lIdentifier, _ReceiveSockets, lTempSocket, aReceiveSocketIndexes);
        result := lIdentifier;
        SDLNet_AddSocket(_SocketSet, PSDLNet_GenericSocket(lTempSocket));
      end;
    end;
  end;

// -- End Connection Code
// -- Start Message Code
  
  procedure EnqueueMessageProcedure(aMsg : String; aSocket : PTCPSocket);
  var
    lRemoteIP  : PIPAddress;
    lIP        : String;
    lPort      : LongInt;
  begin
    lRemoteIP := SDLNet_TCP_GetPeerAddress(aSocket);
    lIP := HexStrToIPv4(DecToHex(SDLNet_Read32(@lRemoteIP^.host)));
    lPort := LongInt(SDLNet_Read16(@lRemoteIP^.port));
    EnqueueMessage(aMsg, lIP, lPort);
  end;
  
  procedure ExtractData(socket : PTCPSocket; aReceivedCount : LongInt; aID : String);
  var
    lData : String = '';
    i      : LongInt;
    lSize  : Byte = 0;
    lSizeIdx : LongInt = 0;
    lComplete : Boolean = False;
  begin
    while not lComplete do
    begin      
      lSize := Byte(_Buffer[lSizeIdx]);
    
  	  for i := lSizeIdx + 1 to lSize do
  	  begin
  	    lData += _Buffer[i];
  	  end;
      EnqueueMessage(lData, ExtractDelimited(1, aID, [':']), StrToInt(ExtractDelimited(2, aID, [':'])));
      lSizeIdx := lSizeIdx + lSize + 1;
      if lSizeIdx = aReceivedCount then lComplete := True;
    end;
  end;
  
  function TCPMessageReceivedProcedure(aReceiveSocketIndexes : NamedIndexCollection) : Boolean;
  var
    i, lReceived : LongInt;
  begin
    result := False;
    if SDLNet_CheckSockets(_SocketSet, 0) < 1 then exit;
    for i := Low(_ReceiveSockets) to High(_ReceiveSockets) do
    begin
      if SDLNET_SocketReady(PSDLNet_GenericSocket(_ReceiveSockets[i])) then
      begin
        lReceived := SDLNet_TCP_Recv(_ReceiveSockets[i], _BufferPtr, 512);
    	  if (lReceived > 0) then
    	  begin
    	    ExtractData(_ReceiveSockets[i], lReceived, NameAt(aReceiveSocketIndexes, i));
    	    result := True;
    	  end;
    	end;
		end;
  end;
  
  function BroadcastTCPMessageProcedure(aMsg : String; aSendSocketIndexes : NamedIndexCollection) : StringArray;
  var
    lLen, i : LongInt;
  begin
    SetLength(result, 0);    
    for i := 0 to Length(aMsg) + 1 do
	  begin
	    if i = 0 then
	      _Buffer[i] := Char(Length(aMsg))
	    else if  i < Length(aMsg) + 1 then
	      _Buffer[i] := aMsg[i];
	  end;
	  
	  lLen := Length(aMsg) + 1;
	  for i := Low(_SendSockets) to High(_SendSockets) do
	  begin
  	  if (SDLNet_TCP_Send(_SendSockets[i], _BufferPtr, lLen) < lLen) then
  		begin
  			RaiseWarning('SDLNet_TCP_Send: ' + SDLNet_GetError());
  			SetLength(result, Length(result) + 1);
        result[High(result)] := NameAt(aSendSocketIndexes, i)
  		end;
		end;
  end;
  
  function SendTCPMessageToProcedure(aMsg, aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : String;
  var
    lLen, i, lSocketIdx : LongInt;
    lIdentifier : String;
  begin
    result := '';
    
    for i := 0 to Length(aMsg) + 1 do
	  begin
	    if i = 0 then
	      _Buffer[i] := Char(Length(aMsg))
	    else if  i < Length(aMsg) + 1 then
	      _Buffer[i] := aMsg[i];
	  end;
	  
	  lLen := Length(aMsg) + 1;
	  	  
    lIdentifier:= aIP + ':' + IntToStr(aPort);
	  lSocketIdx := IndexOf(aSendSocketIndexes, lIdentifier);
	  
	  if (lSocketIdx = -1) then begin RaiseWarning('Socket not found for: ' + lIdentifier); exit; end;
	  
    if (SDLNet_TCP_Send(_SendSockets[lSocketIdx], _BufferPtr, lLen) < lLen) then
		begin
      result := lIdentifier;
			RaiseWarning('SDLNet_TCP_Send: ' + SDLNet_GetError());
		end;
  end;
// -- End Message Code  
// -- Start UDP Section
  function OpenUDPListenerSocketProcedure(aPort : LongInt; aUDPSocketIndexes : NamedIndexCollection) : Boolean;
  var
    lTempSocket  : PUDPSocket = nil;    
  begin    
    lTempSocket := SDLNet_UDP_Open(aPort);
    if Assigned(lTempSocket) then
      _UDPListenSockets := AddUDPSocket(IntToStr(aPort), _UDPListenSockets, lTempSocket, aUDPSocketIndexes);

    if _UDPReceivePacket = nil then
      _UDPReceivePacket := SDLNet_AllocPacket(512);
    result := Assigned(lTempSocket);
    
    if not result then
  		RaiseWarning('OpenUDPListenerPort: ' + SDLNET_GetError());
  end;
  
  function OpenUDPSendSocketProcedure() : Boolean;
  var
    lTempSocket  : PUDPSocket = nil;    
  begin    
    lTempSocket := SDLNet_UDP_Open(0);
    if Assigned(lTempSocket) then
      _UDPSendSocket := lTempSocket;

    result := Assigned(lTempSocket);

    if _UDPSendPacket = nil then
      _UDPSendPacket := SDLNet_AllocPacket(512);
    
    if not result then
  		RaiseWarning('OpenUDPSendPort: ' + SDLNET_GetError());
  end;

  function UDPMessageReceivedProcedure() : Boolean;
  var
    i, j : LongInt;    
    msg  : String = '';
  begin
    result := False;
    for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
    begin
      if SDLNet_UDP_Recv(_UDPListenSockets[i], _UDPReceivePacket) > 0 then
      begin
        for j := 0 to _UDPReceivePacket^.len - 1 do
          msg += Char((_UDPReceivePacket^.data)[j]);
          
        EnqueueMessage(msg, HexStrToIPv4(DecToHex(SDLNet_Read32(@_UDPReceivePacket^.address.host))), _UDPReceivePacket^.address.port);        
        result := True;
      end;  
    end;
  end;
  
  function SendUDPMessageProcedure(aMsg, aIP : String; aPort : LongInt) : Boolean;
  var
    lIPAddress : TIPaddress;
  begin
    SDLNet_ResolveHost(lIPAddress, PChar(aIP), aPort);
    _UDPSendPacket^.address.host   := lIPAddress.host;
    _UDPSendPacket^.address.port   := lIPAddress.port;
    _UDPSendPacket^.len  := Length(aMsg);
    _UDPSendPacket^.data  := @(aMsg[1]);
    SDLNet_UDP_Send(_UDPSendSocket, -1, _UDPSendPacket);
    result := True; 
  end;

// -- End UDP Section  
// -- Start Clean Section
  procedure Close();
  var
    i : LongInt;
  begin
    for i := Low(_ReceiveSockets) to High(_ReceiveSockets) do
      SDLNet_TCP_Close(_ReceiveSockets[i]);
    for i := Low(_SendSockets) to High(_SendSockets) do
      SDLNet_TCP_Close(_SendSockets[i]);
    for i := Low(_ConSockets) to High(_ConSockets) do
      SDLNet_TCP_Close(_ConSockets[i]);
      
    if Assigned(_UDPReceivePacket) then
      SDLNet_FreePacket(_UDPReceivePacket);
 //   _UDPSendPacket is not Allocated
 //   if Assigned(_UDPSendPacket) then
 //     SDLNet_FreePacket(_UDPSendPacket);    
      
    for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
      SDLNet_UDP_Close(_UDPListenSockets[i]);
      
    if Assigned(_SocketSet) then
      SDLNet_FreeSocketSet(_SocketSet);
    ClearMessageQueue();
    SDLNet_Quit();
  end;

  function RemoveTCPSocketAt(aSockets : TCPSocketArray; aSkippedIdx : LongInt) : TCPSocketArray;
  var
    lTmpSockets : Array of PTCPSocket;
    i, lOffset  : LongInt;
  begin
    lOffset := 0;
    SDLNet_TCP_Close(aSockets[aSkippedIdx]);
        WriteLn('Closing Socket..');
    SetLength(lTmpSockets, Length(aSockets) - 1);
    for i := Low(lTmpSockets) to High(lTmpSockets) do
    begin
      WriteLn('i: ', i, ' Skipped Idx: ' , aSkippedIdx);
      if i = aSkippedIdx then 
      begin
        lOffset := 1
      end else
        lTmpSockets[i] := aSockets[i + lOffset];
    end;
    result := lTmpSockets;   
  end;

  function RemoveUDPSocketAt(aSockets : UDPSocketArray; aSkippedIdx : LongInt) : UDPSocketArray;
  var
    lTmpSockets : Array of PUDPSocket;
    i, lOffset  : LongInt;
  begin
    lOffset := 0;
    SetLength(lTmpSockets, Length(aSockets) - 1);
    for i := Low(lTmpSockets) to High(lTmpSockets) do
    begin
      if i = aSkippedIdx then 
      begin
        lOffset := 1;
        SDLNet_UDP_Close(aSockets[i])
      end else
        lTmpSockets[i] := aSockets[i + lOffset];
    end;
    result := lTmpSockets;   
  end;

  function RemoveSocketAt(var aCollection : NamedIndexCollection; aName : String) : LongInt;
  begin
    result := IndexOf(aCollection, aName);
    if (result < 0) then exit;

    RemoveName(aCollection, result);
  end;

  function CloseTCPHostSocketProcedure(var aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;
  var
    lIdx : LongInt;
  begin
    result := False;
    lIdx := RemoveSocketAt(aCollection, IntToStr(aPort));
    if lIdx = -1 then exit;
    _ConSockets := RemoveTCPSocketAt(_ConSockets, lIdx);
    result := True;
  end;

  function CloseTCPReceiverSocketProcedure(var aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  var
    lIdx : LongInt;
  begin
    result := False;
    lIdx := RemoveSocketAt(aCollection, aIP + ':' + IntToStr(aPort));
    if lIdx = -1 then exit;
    _ReceiveSockets := RemoveTCPSocketAt(_ReceiveSockets, lIdx);
    result := True;
  end;

  function CloseTCPSenderSocketProcedure(var aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  var
    lIdx : LongInt;
  begin
    result := False;
    lIdx := RemoveSocketAt(aCollection,  aIP + ':' + IntToStr(aPort));
    if lIdx = -1 then exit;
    _SendSockets := RemoveTCPSocketAt(_SendSockets, lIdx);
    result := True;
  end;

  function CloseUDPListenSocketProcedure(var aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;
  var
    lIdx : LongInt;
  begin
    result := False;
    lIdx := RemoveSocketAt(aCollection, IntToStr(aPort));
    if lIdx = -1 then exit;
    _UDPListenSockets := RemoveUDPSocketAt(_UDPListenSockets, lIdx);
    result := True;
  end;

  function CloseUDPSendSocketProcedure() : Boolean;
  begin
    result := False;
    if Assigned(_UDPSendSocket) then
    begin
      SDLNet_UDP_Close(_UDPSendSocket);
      _UDPSendSocket := nil;
      result := True;
    end;
  end;

  function GetMyIPProcedure() : String;
  begin
    result := '127.0.0.1';
  end;

  // -- End Close Sockets
  
  procedure LoadSDLNetworkingDriver(); 
  begin
    NetworkingDriver.CreateTCPHost               := @CreateTCPHostProcedure;
    NetworkingDriver.OpenTCPConnectionToHost     := @OpenTCPConnectionToHostProcedure;
    NetworkingDriver.ServerAcceptTCPConnection   := @ServerAcceptTCPConnectionProcedure;
    NetworkingDriver.TCPMessageReceived          := @TCPMessageReceivedProcedure;
    NetworkingDriver.BroadcastTCPMessage         := @BroadcastTCPMessageProcedure;
    NetworkingDriver.SendTCPMessageTo            := @SendTCPMessageToProcedure;

    NetworkingDriver.OpenUDPListenerSocket       := @OpenUDPListenerSocketProcedure;
    NetworkingDriver.OpenUDPSendSocket           := @OpenUDPSendSocketProcedure;
    NetworkingDriver.UDPMessageReceived          := @UDPMessageReceivedProcedure;
    NetworkingDriver.SendUDPMessage              := @SendUDPMessageProcedure;

    NetworkingDriver.CloseTCPHostSocket          := @CloseTCPHostSocketProcedure;
    NetworkingDriver.CloseTCPReceiverSocket      := @CloseTCPReceiverSocketProcedure;
    NetworkingDriver.CloseTCPSenderSocket        := @CloseTCPSenderSocketProcedure;
    NetworkingDriver.CloseUDPListenSocket        := @CloseUDPListenSocketProcedure;
    NetworkingDriver.CloseUDPSendSocket          := @CloseUDPSendSocketProcedure;

    NetworkingDriver.GetMyIP                     := @GetMyIPProcedure;
  end;
  
  initialization 
  begin
    if (SDLNet_Init() < 0) then
  		RaiseWarning('SDLNet_Init: ' + SDLNet_GetError())
  	else
  	  _BufferPtr := @_Buffer;
    _SocketSet := SDLNET_AllocSocketSet(16);
  end;

  finalization
  begin
    Close();
  end;
end.