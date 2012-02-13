//=============================================================================
// sgNetworking.pas
//=============================================================================
//
//
//Messages : IP - Source IP, Incoming (Local) Port
//Host Socket ID : Listening Port
//Receiving SocketID : IP - Source IP, Incoming (Local) Port
//Send Socket ID : IP - Destination, Port - Destination
//
// Version 1.0:
//
//=============================================================================
/// The networking code of SwinGame is used for TCP and UDP connections to
/// and from multiple clients.
/// 
/// @module Networking
/// @static
unit sgNetworking;
//=============================================================================
interface
  uses sgShared, sgTypes, sgNamedIndexCollection;
//=============================================================================

//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns true if success or false
  /// if the binding failed. Uses TCP.
  ///
  /// @param port The port to bind the socket to.
  ///
  /// @lib
  /// @uname CreateTCPHost
  /// @sn createTCPHost:%s port:%s
  function CreateTCPHost              (port : LongInt) : Boolean;

  /// Opens a connection to a peer using the IP and port
  /// Creates a Socket for the purpose of sending messages
  /// to the connected peer. Returns True if the connection
  /// was a success or false if the connection fails.
  ///
  /// @param ip The IP Address of the host
  /// @param port The port the host is listening to connections on
  ///
  /// @lib
  /// @uname OpenTCPConnectionToHost
  /// @sn openTCPConnectionToHost:%s ip:%s port:%s
  function OpenTCPConnectionToHost    (ip : String; port : LongInt) : Boolean;

  /// Accepts an incomming connection from another client.
  /// Returns true if another client has connected or false
  /// if nothing has connected.
  ///
  /// @lib
  /// @uname ServerAcceptTCPConnection
  function ServerAcceptTCPConnection  () : Boolean; 
   
  /// Checks if a message has been received. If a message has been received,
  /// It will automatically add it to the message queue, with the message,
  /// source's IP and the port it received the message on.
  ///
  /// @lib
  /// @uname TCPMessageReceived
  function TCPMessageReceived         () : Boolean;    

  /// TODO
  ///
  /// @param msg The message to be sent
  ///
  /// @lib
  /// @uname BroadcastTCPMessage
  function BroadcastTCPMessage        (msg : String) : Boolean;

  /// Sends the message to the specified client, dictated the by ip and port
  /// passed in as an arguement. This will either return an empty string if 
  /// the message succeeded in being sent, or a string with the destination ip
  /// and destination port if the message failed to be sent. this string can 
  /// then be used to close the socket that failed to send the message if desired.
  ///
  /// @param msg The message to be sent
  /// @param ip The ip to send the message to
  /// @param port The port to send the message to for the peer.
  ///
  /// @lib
  /// @uname SendTCPMessageTo
  /// @sn sendTCPMessageTo:%s msg:%s ip:%s port:%s
  function SendTCPMessageTo           (msg, ip : String; port : LongInt) : String;

//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns true if success or false
  /// if the binding failed. Uses UDP
  ///
  /// @param port The port to bind the socket to.
  ///
  /// @lib
  /// @uname openUDPListenerSocket
  /// @sn openUDPListenerSocket:%s port:%s
  function OpenUDPListenerSocket      (port : LongInt) : Boolean;

  /// Creates a socket to send UDP Packets to another client. This 
  /// uses a random port for the socket.
  ///
  /// @lib
  /// @uname OpenUDPListenerSocket
  function OpenUDPSendSocket          () : Boolean;

  /// Checks all UDP listening sockets to see if a packet has been received.
  /// If a packet has been received, it will Enqueue the message into the message
  /// queue. This will set the message, sender's address and sender's port. it
  /// will return true if a message has been received or false if there has been
  /// no message.
  ///
  /// @lib
  /// @uname UDPMessageReceived 
  function UDPMessageReceived         () : Boolean;

  /// Sends a UDP packet to the specified IP and Port. the packet is populated
  /// with the message.
  ///
  /// @param msg The message to be sent
  /// @param ip The destination IP
  /// @param port The destination port
  ///
  /// @lib
  /// @uname SendUDPMessage 
  /// @sn sendUDPMessage:%s msg:%s ip:%s port:%s
  function SendUDPMessage             (msg, ip : String; port : LongInt) : Boolean;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------

  /// Dequeues the top message and returns it as a MessageData.
  ///
  /// @lib
  /// @uname DequeueMessage 
  function DequeueMessage             () : MessageData;

  /// Dequeues ALL messages and returns it as a MessageDataArray
  ///
  /// @lib
  /// @uname PopAllMessages 
  function PopAllMessages             () : MessageDataArray;

  /// Queues a message to the end of the Message Queue
  ///
  /// @param msg The message Sent
  /// @param ip 
  /// @param port 
  ///
  /// @lib
  /// @uname EnqueueMessage 
  /// @sn enqueueMessage:%s msg:%s ip:%s port:%s
  procedure EnqueueMessage            (msg : String; ip : String; port : LongInt);

//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------

  /// Converts an Integer to a Hex value and returns it as a string.
  ///
  /// @param dec The Integer
  ///
  /// @lib
  /// @uname DecToHex 
  /// @sn decToHex:%s dec:%s
  function DecToHex                   (dec : LongInt) : String;

  /// Converts a Hex String to a Decimal Value as a String.
  ///
  /// @param hex The Hex String
  ///
  /// @lib
  /// @uname HexToDecString 
  /// @sn hexToDecString:%s hex:%s
  function HexToDecString             (hex : String) : String;

  /// Converts a Hex String to an IPV4 Address (0.0.0.0)
  ///
  /// @param hex The Hex String
  ///
  /// @lib
  /// @uname HexStrToIPv4 
  /// @sn hexStrToIPv4:%s hex:%s
  function HexStrToIPv4               (hex : String) : String;

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------
  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Host Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  /// @uname CloseTCPHostSocket
  /// @sn closeTCPHostSocket:%s aPort:%s
  function CloseTCPHostSocket        (aPort: LongInt) : Boolean;

  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Receiver Sockets
  ///
  /// @param aIP  The IP that makes up the identifier for the Receiver Socket
  /// @param aPort The Port that makes up the identifier for the Receiver Socket
  ///
  /// @lib
  /// @uname CloseTCPReceiverSocket
  /// @sn closeTCPReceiverSocket:%s aIP:%s aPort:%s
  function CloseTCPReceiverSocket    (aIP : String; aPort : LongInt) : Boolean;

  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Sender Sockets
  ///
  /// @param aIP  The IP that makes up the identifier for the Sender Socket
  /// @param aPort The Port that makes up the identifier for the Sender Socket
  ///
  /// @lib
  /// @uname CloseTCPSenderSocket
  /// @sn closeTCPSenderSocket:%s aIP:%s aPort:%s
  function CloseTCPSenderSocket      (aIP : String; aPort : LongInt) : Boolean;

  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to UDP Listener Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  /// @uname CloseUDPListenSocket
  /// @sn CloseUDPListenSocket:%s aPort:%s
  function CloseUDPListenerSocket      (aPort : LongInt) : Boolean;

  /// Closes the UDP Sender Socket.
  ///
  /// @lib
  /// @uname CloseUDPSenderSocket
  function CloseUDPSenderSocket      () : Boolean;
          
//=============================================================================
implementation
  uses SysUtils, sgUtils, sgDriverNetworking;
//=============================================================================

type
  MessagePtr = ^MessageLink;
  MessageLink = packed record
    data  : MessageData;
    next  : MessagePtr;
  end;

var  
  _ConSocketIndexes       : NamedIndexCollection;
  _ReceiveSocketIndexes   : NamedIndexCollection;
  _SendSocketIndexes      : NamedIndexCollection;
  _UDPSocketIndexes       : NamedIndexCollection;
  _LastMessage            : MessagePtr = nil;
  _FirstMessage           : MessagePtr = nil;
  _MessageCount           : LongInt = 0;
  
//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------
    
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


//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------
    

  function CreateTCPHost(port : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CreateTCPHost(port, _ConSocketIndexes);
  end;
  

  function OpenTCPConnectionToHost(ip : String; port : LongInt) : Boolean;
  begin
    result := NetworkingDriver.OpenTCPConnectionToHost(ip, port, _SendSocketIndexes);
  end;   
  
  function ServerAcceptTCPConnection() : Boolean;
  begin
    result := NetworkingDriver.ServerAcceptTCPConnection(_ReceiveSocketIndexes, _ConSocketIndexes);
  end;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------
  
  procedure EnqueueMessage(msg : String; ip : String; port : LongInt);
  var
    MsgData   : MessagePtr;
  begin
    New(MsgData);  
    
    WriteLn('Message From: ', ip, ' : ', port);
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

  function TCPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.TCPMessageReceived(_ReceiveSocketIndexes);
  end;
  
  function BroadcastTCPMessage(msg : String) : Boolean;
  begin
    result := NetworkingDriver.BroadcastTCPMessage(msg);
  end;
  
  function SendTCPMessageTo(msg, ip : String; port : LongInt) : String;
  begin
    result := NetworkingDriver.SendTCPMessageTo(msg, ip, port, _SendSocketIndexes);
  end;

//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  function OpenUDPListenerSocket(port : LongInt) : Boolean;
  begin
    result := NetworkingDriver.OpenUDPListenerSocket(port, _UDPSocketIndexes);
  end;
  
  function OpenUDPSendSocket() : Boolean;
  begin
    result := NetworkingDriver.OpenUDPSendSocket();
  end;

  function UDPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function SendUDPMessage(msg, ip : String; port : LongInt) : Boolean;
  begin
    result := NetworkingDriver.SendUDPMessage(msg, ip, port);
  end;

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------

  function CloseTCPHostSocket(aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseTCPHostSocket(_ConSocketIndexes, aPort);    
  end;

  function CloseTCPReceiverSocket(aIP : String; aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseTCPReceiverSocket(_ReceiveSocketIndexes, aIP, aPort);    
  end;

  function CloseTCPSenderSocket(aIP : String; aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseTCPSenderSocket(_SendSocketIndexes, aIP, aPort);    
  end;

  function CloseUDPListenerSocket(aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseUDPSocket(_UDPSocketIndexes, aPort);    
  end;

  function CloseUDPSenderSocket() : Boolean;
  begin
    result := False;//NetworkingDriver.CloseUDPSenderSocket();    
  end;

//=============================================================================

  initialization 
  begin    
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
  end;
end.