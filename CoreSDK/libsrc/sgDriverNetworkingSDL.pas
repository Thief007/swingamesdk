unit sgDriverNetworkingSDL;

interface
uses sgShared, sgTypes, sgNamedIndexCollection, SDL_Net;

type
  MessageData = packed record
    msg       : String;
    IP        : String;
    port      : LongInt;
  end;
  
  MessageDataArray = Array of MessageData;

  function CreateHostProcedure(port : LongInt) : Boolean;
  function OpenConnectionToHostProcedure(ip : String; port : LongInt) : Boolean;
  function ServerAcceptConnection() : Boolean;  
  function DataReceived() : Boolean;    
  function PopAllMessages() : MessageDataArray;
  function BroadcastMessageProcedure(msg : String) : Boolean;
  function DequeueMessage() : MessageData;
  function SendMessageTo(msg, ip : String; port : LongInt) : Boolean;
  function OpenUDPListenerSocket(port : LongInt) : Boolean;
  function OpenUDPSendSocket() : Boolean;
  function ReceiveUDPMessage() : Boolean;
  function SendUDPMessage(msg, ip : String; port : LongInt) : Boolean;
    
type
  
  MessagePtr = ^MessageLink;
  MessageLink = packed record
    data  : MessageData;
    next  : MessagePtr;
  end;
  
  TCPSocketArray = Array of PTCPSocket;
  UDPSocketArray = Array of PUDPSocket;
      
implementation
uses SysUtils, sgUtils;
var
  _ConSocketIndexes       : NamedIndexCollection;
  _ReceiveSocketIndexes   : NamedIndexCollection;
  _SendSocketIndexes      : NamedIndexCollection;
  _UDPSocketIndexes       : NamedIndexCollection;
  _ConSockets             : TCPSocketArray;
  _ReceiveSockets         : TCPSocketArray;
  _SendSockets            : TCPSocketArray;
  _Buffer                 : Array [0..512] of Char;
  _BufferPtr              : ^Char;
  _Socketset              : PSDLNet_SocketSet;
  _LastMessage            : MessagePtr = nil;
  _FirstMessage           : MessagePtr = nil;
  _MessageCount           : LongInt = 0;
  _UDPSendSocket          : PUDPSocket = nil;
  _UDPListenSockets       : UDPSocketArray;
  _UDPSendPacket          : PUDPPacket = nil;
  _UDPReceivePacket       : PUDPPacket = nil;
  
// -- Start Hex Code
    
  function DecToHex(dec : LongInt) : String;
  var
    LRemainder : LongInt;
    lHexAlpha : String = '0123456789ABCDEF';
  begin
    lRemainder := (dec mod 16);
    if dec - lRemainder = 0 then
      result := lHexAlpha[lRemainder + 1]
    else 
      result := DecToHex( (dec - lRemainder) div 16 ) + lHexAlpha[lRemainder + 1]
  end;
      
  function HexToDecString(hex : String) : String;
  var
    i    : LongInt;
    val  : LongInt = 0;
    expo : Double;
  begin
    for i := 1 to Length(hex) do
    begin      
      expo := Exp((Length(hex) - i)*Ln(16));
      case hex[i] of
        '0' : val += Round(0  * expo);
        '1' : val += Round(1  * expo);
        '2' : val += Round(2  * expo);
        '3' : val += Round(3  * expo);
        '4' : val += Round(4  * expo);
        '5' : val += Round(5  * expo);
        '6' : val += Round(6  * expo);
        '7' : val += Round(7  * expo);
        '8' : val += Round(8  * expo); 
        '9' : val += Round(9  * expo);
        'A' : val += Round(10 * expo);
        'B' : val += Round(11 * expo);
        'C' : val += Round(12 * expo);
        'D' : val += Round(13 * expo);
        'E' : val += Round(14 * expo);
        'F' : val += Round(15 * expo);
      end;
    end;   
    result := IntToStr(val); 
  end;
  
  function HexStrToIPv4(hex : String) : String;
  begin
    result := HexToDecString(hex[1] + hex[2]);
    result += '.' + HexToDecString(hex[3] + hex[4]);
    result += '.' + HexToDecString(hex[5] + hex[6]);
    result += '.' + HexToDecString(hex[7] + hex[8]);
  end;
  
// -- End Hex Code  
// -- Start Connection Code

  function AddUDPSocket(port : String; var sockets : UDPSocketArray; newSocket : PUDPSocket; var collection : NamedIndexCollection) : UDPSocketArray;
  var
    tmpSockets : UDPSocketArray;
    i         : LongInt;
  begin
    if HasName(collection, port) then begin result := sockets; exit; end;
    SetLength(tmpSockets, Length(sockets) + 1);
    for i := Low(sockets) to High(sockets) do
      tmpSockets[i] := sockets[i];
    tmpSockets[High(tmpSockets)] := newSocket;
    AddName(collection, port);
    result := tmpSockets;    
  end;
  
  function AddSocket(ip : String; var sockets : TCPSocketArray; newSocket : PTCPSocket; var collection : NamedIndexCollection) : TCPSocketArray;
  var
    tmpSockets : Array of PTCPSocket;
    i         : LongInt;
  begin
    if HasName(collection, ip) then begin result := sockets; exit; end;
    SetLength(tmpSockets, Length(sockets) + 1);
    for i := Low(sockets) to High(sockets) do
      tmpSockets[i] := sockets[i];
    tmpSockets[High(tmpSockets)] := newSocket;
    AddName(collection, ip);
    result := tmpSockets;    
  end;
  
  function AddSocket(var sockets : TCPSocketArray; newSocket : PTCPSocket; var collection : NamedIndexCollection) : TCPSocketArray;
  var
    remoteIP : PIPAddress;
    ip       : String;
    i        : LongInt;
  begin
    remoteIP := SDLNet_TCP_GetPeerAddress(newSocket);
    ip := HexStrToIPv4(DecToHex(SDLNet_Read32(@remoteIP^.host))) +':'+ IntToStr(SDLNet_Read16(@remoteIP^.port));
    result := AddSocket(ip, sockets, newSocket, collection);
  end;
  
  function OpenTCPConnection(ip : PChar; port : LongInt) : PTCPSocket;
  var
    lIPAddress : TIPAddress;  
  begin
    result := nil;
    if (SDLNet_ResolveHost(lIPAddress, ip, port) < 0) then
  		RaiseWarning('SDLNet_ResolveHost: ' + SDLNet_GetError());
    
  	result := SDLNet_TCP_Open(lIPAddress); 

  	if (not Assigned(result)) then
  		RaiseWarning('SDLNet_TCP_Open:' + SDLNet_GetError());
  end;
  
  function CreateHostProcedure(port : LongInt) : Boolean;
  var
    lTempSocket  : PTCPSocket = nil;
  begin
    lTempSocket := OpenTCPConnection(nil, port);
    if Assigned(lTempSocket) then
      _ConSockets := AddSocket(IntToStr(port), _ConSockets, lTempSocket, _ConSocketIndexes);
    result := Assigned(lTempSocket);    
  end;
  
  function OpenConnectionToHostProcedure(ip : String; port : LongInt) : Boolean;
  var
    lTempSocket  : PTCPSocket = nil;    
  begin    
    lTempSocket := OpenTCPConnection(PChar(ip), port);
    if Assigned(lTempSocket) then
      _SendSockets := AddSocket(_SendSockets, lTempSocket, _SendSocketIndexes);
    result := Assigned(lTempSocket);
  end;   
  
  function ServerAcceptConnection() : Boolean;
  var
    lTempSocket : PTCPSocket = nil;
    i           : LongInt;
  begin  
    result := False;
    for i := Low(_ConSockets) to High(_ConSockets) do
    begin
      lTempSocket := SDLNet_TCP_Accept(_ConSockets[i]);
      if Assigned(lTempSocket) then
      begin
        _ReceiveSockets := AddSocket(_ReceiveSockets, lTempSocket, _ReceiveSocketIndexes);
        result := True;
        SDLNet_AddSocket(_SocketSet, PSDLNet_GenericSocket(lTempSocket));
      end;
    end;
  end;

// -- End Connection Code
// -- Start Message Code
  
  procedure EnqueueMessage(msg : String; ip : String; port : LongInt);
  var
    MsgData   : MessagePtr;
  begin
    New(MsgData);  
    
    MsgData^.data.msg := msg;
    MsgData^.data.IP := ip;
    MsgData^.data.port := port;
    MsgData^.next  := nil;
    if _FirstMessage = nil then
    begin
      _FirstMessage := MsgData;
      _LastMessage  := MsgData;
    end else begin
      _LastMessage^.next := MsgData;
      _LastMessage       := MsgData;
    end;  
    _MessageCount += 1;    
  end;
  
  procedure EnqueueMessage(msg : String; socket : PTCPSocket);
  var
    lRemoteIP  : PIPAddress;
    ip        : String;
    port      : LongInt;
  begin
    lRemoteIP := SDLNet_TCP_GetPeerAddress(socket);
    ip := HexStrToIPv4(DecToHex(SDLNet_Read32(@lRemoteIP^.host)));
    port := LongInt(SDLNet_Read16(@lRemoteIP^.port));
    EnqueueMessage(msg, ip, port);
  end;
  
  function DequeueMessage() : MessageData;
  var
    lTmp : MessagePtr;
  begin  
    if _FirstMessage = nil then begin _LastMessage := nil; exit; end; 
    
    result.msg := _FirstMessage^.data.msg;
    result.ip  := _FirstMessage^.data.ip;
    result.port  := _FirstMessage^.data.port;
    lTmp := @_FirstMessage^.next^;
    Dispose(_FirstMessage);
    _FirstMessage := lTmp;
    _MessageCount -= 1;
  end;
  
  function PopAllMessages() : MessageDataArray;
  var
    i : LongInt;
  begin
    SetLength(result, _MessageCount);
    for i := Low(result) to High(result) do
    begin
      result[i] := DequeueMessage();
    end;
  end;

  procedure ExtractData(socket : PTCPSocket; aReceivedCount : LongInt);
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
      EnqueueMessage(lData, socket);
      lSizeIdx := lSizeIdx + lSize + 1;
      if lSizeIdx = aReceivedCount then lComplete := True;
    end;
  end;
  
  function DataReceived() : Boolean;
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
    	    ExtractData(_ReceiveSockets[i], lReceived);
    	    result := True;
    	  end;
    	end;
		end;
  end;
  
  function BroadcastMessageProcedure(msg : String) : Boolean;
  var
    len, i : LongInt;
  begin
    result := True;
    
    for i := 0 to Length(msg) + 1 do
	  begin
	    if i = 0 then
	      _Buffer[i] := Char(Length(msg))
	    else if  i < Length(msg) + 1 then
	      _Buffer[i] := msg[i];
	  end;
	  
	  len := Length(msg) + 1;
	  for i := Low(_SendSockets) to High(_SendSockets) do
	  begin
  	  if (SDLNet_TCP_Send(_SendSockets[i], _BufferPtr, len) < len) then
  		begin
  			RaiseWarning('SDLNet_TCP_Send: ' + SDLNet_GetError());
  			result := False;
  		end;
		end;
  end;
  
  function SendMessageTo(msg, ip : String; port : LongInt) : Boolean;
  var
    len, i, socketIdx : LongInt;
  begin
    result := True;
    
    for i := 0 to Length(msg) + 1 do
	  begin
	    if i = 0 then
	      _Buffer[i] := Char(Length(msg))
	    else if  i < Length(msg) + 1 then
	      _Buffer[i] := msg[i];
	  end;
	  
	  len := Length(msg) + 1;
	  	  
	  socketIdx := IndexOf(_SendSocketIndexes, ip + ':' + IntToStr(port));
	  
	  if (socketIdx = -1) then
	  begin
	    RaiseWarning('Socket not found for: ' + ip + IntToStr(port));
			result := False;
	  end;
	  
	  if (SDLNet_TCP_Send(_SendSockets[socketIdx], _BufferPtr, len) < len) then
		begin
			RaiseWarning('SDLNet_TCP_Send: ' + SDLNet_GetError());
			result := False;
		end;
  end;
// -- End Message Code  
// -- Start UDP Section
  function OpenUDPListenerSocket(port : LongInt) : Boolean;
  var
    lTempSocket  : PUDPSocket = nil;    
  begin    
    lTempSocket := SDLNet_UDP_Open(port);
    if Assigned(lTempSocket) then
      _UDPListenSockets := AddUDPSocket(IntToStr(port), _UDPListenSockets, lTempSocket, _UDPSocketIndexes);

    if _UDPReceivePacket = nil then
      _UDPReceivePacket := SDLNet_AllocPacket(512);
    result := Assigned(lTempSocket);
    
    if not result then
  		RaiseWarning('OpenUDPListenerPort: ' + SDLNET_GetError());
  end;
  
  function OpenUDPSendSocket() : Boolean;
  var
    lTempSocket  : PUDPSocket = nil;    
  begin    
    lTempSocket := SDLNet_UDP_Open(0);
    if Assigned(lTempSocket) then
      _UDPSendSocket := lTempSocket;

    if _UDPSendPacket = nil then
      _UDPSendPacket := SDLNet_AllocPacket(512);
    result := Assigned(lTempSocket);
    
    if not result then
  		RaiseWarning('OpenUDPSendPort: ' + SDLNET_GetError());
  end;

  function ReceiveUDPMessage() : Boolean;
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
  
  function SendUDPMessage(msg, ip : String; port : LongInt) : Boolean;
  var
    lIPAddress : TIPaddress;
  begin
    SDLNet_ResolveHost(lIPAddress, PChar(ip), port);
    _UDPSendPacket^.address.host   := lIPAddress.host;
    _UDPSendPacket^.address.port   := lIPAddress.port;
    _UDPSendPacket^.len  := Length(msg);
    _UDPSendPacket^.data  := @(msg[1]);
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
    WriteLn('Removing UDP Sender Packet...');
    if Assigned(_UDPSendPacket) then
      SDLNet_FreePacket(_UDPSendPacket);    
      
    WriteLn('Removing UDP Listener Sockets...');
    for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
      SDLNet_UDP_Close(_UDPListenSockets[i]);
      
    WriteLn('Removing SocketSet...');
    if Assigned(_SocketSet) then
      SDLNet_FreeSocketSet(_SocketSet);
    WriteLn('Removing Messages...');
    PopAllMessages();
  end;

  // -- End Close Sockets
  
  
  initialization 
  begin
    if (SDLNet_Init() < 0) then
  		RaiseWarning('SDLNet_Init: ' + SDLNet_GetError())
  	else
  	  _BufferPtr := @_Buffer;
    _SocketSet := SDLNET_AllocSocketSet(16);
    
    InitNamedIndexCollection(_ReceiveSocketIndexes);
    InitNamedIndexCollection(_SendSocketIndexes);
    InitNamedIndexCollection(_ConSocketIndexes);    
    InitNamedIndexCollection(_UDPSocketIndexes);
  end;

  finalization
  begin
    FreeNamedIndexCollection(_ReceiveSocketIndexes);
    FreeNamedIndexCollection(_SendSocketIndexes);
    FreeNamedIndexCollection(_ConSocketIndexes);
    FreeNamedIndexCollection(_UDPSocketIndexes);
    Close();
    SDLNet_Quit();
  end;
end.