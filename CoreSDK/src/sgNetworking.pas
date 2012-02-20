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
uses
  sgTypes;
//=============================================================================

//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns true if success or false
  /// if the binding failed. Uses TCP.
  ///
  /// @param aPort The port to bind the socket to.
  ///
  /// @lib
  /// @uname CreateTCPHost
  /// @sn createTCPHost:%s
  function CreateTCPHost              (aPort : LongInt) : Boolean;

  /// Opens a connection to a peer using the IP and port
  /// Creates a Socket for the purpose of two way messages. 
  /// Returns a new connection if successful or nil if failed.
  ///
  /// @param aIP The IP Address of the host
  /// @param aPort The port the host is listening to connections on
  ///
  /// @lib
  /// @uname OpenTCPConnectionToHost
  /// @sn openTCPConnectionToHostIP:%s port:%s
  function OpenTCPConnectionToHost    (aIP : String;  aPort : LongInt) : Connection;

  /// Accepts an incomming connection from another client.
  /// Returns the amount of new connections that have been
  /// accepted.
  ///
  /// @lib
  function ServerAcceptTCPConnection  () : LongInt; 
   
  /// Checks if a message has been received. If a message has been received,
  /// It will automatically add it to the message queue, with the message,
  /// source's IP and the port it received the message on. Returns true if
  /// a new message has been received.
  ///
  /// @lib
  function TCPMessageReceived         () : Boolean;    

  /// Broadcasts a message through all open connections.
  ///
  /// @param aMsg The message to be sent
  ///
  /// @lib
  /// @sn broadcastTCPMessage:%s
  function BroadcastTCPMessage        ( aMsg : String) : Boolean;

  /// Sends the message to the specified client, attached to the socket
  /// Retuns the connection if the message fails to
  /// send so that it may be closed. Returns nil if the message has been sent
  /// successfully.
  ///
  /// @param aMsg The message to be sent
  /// @param aConnection Send the message through this connection's socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendTCPMessage
  /// @self 2
  /// @sn sendTCPMessage:%s toConnection:%s
  function SendTCPMessageTo           ( aMsg : String; aConnection : Connection) : Connection;

  /// Adds a connection to the list of new connections. This is called by the 
  /// Accept connection in TCP and Receive message in UDP (if the message has
  /// been sent by a new connection). This is used in conjunction with Fetch
  /// connection, that will pop the new connection out of the list.
  ///
  /// @param aConnection The new connection to add to the list
  ///
  /// @lib
  /// @class Connection
  /// @method EnqueueNewConnection
  /// @sn enqueueNewConnection:%s
  procedure EnqueueNewConnection(aConnection : Connection);

  /// Removes the top connection from the New connection queue and
  /// returns it.
  ///
  /// @lib
  function FetchConnection() : Connection;

  /// Returns the size of the New Connection List
  ///
  /// @lib
  function ConnectionQueueSize() : LongInt;

//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns the index of the Socket in the
  /// socket array.
  ///
  /// @param aPort The port to bind the socket to.
  ///
  /// @lib
  /// @sn createUDPSocket:%s
  function CreateUDPSocket            ( aPort : LongInt) : LongInt;

  /// Creates the connection and sets the ip and port values. Creates a
  /// socket if there is no socket attached to the specified port. this
  /// socket can be used to send and receive messages. Returns the connection
  /// if this has been successful, or will return nil on failure.
  ///
  /// @param aDestIP The destination IP
  /// @param aDestPort The Destination Port
  /// @param aInPort The port to receive messages
  ///
  /// @lib
  /// @sn createUDPConnectionIP:%s port:%s inPort:%s
  function CreateUDPConnection        (aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 

  /// Checks all UDP listening sockets to see if a packet has been received.
  /// If a packet has been received, it will Enqueue the message into the message
  /// queue. This will set the message, sender's address and sender's port. it
  /// will return true if a message has been received or false if there has been
  /// no message.
  ///
  /// @lib
  function UDPMessageReceived         () : Boolean;

  /// Sends a UDP packet to the port and ip specified in the connection
  /// with the message.
  ///
  /// @param aMsg The message to be sent
  /// @param aConnection Send the Message through this connection's Socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendUDPMessage
  /// @self 2
  /// @sn sendUDPMessage:%s toConnection:%s
  function SendUDPMessage             ( aMsg : String; aConnection : Connection) : Boolean;

//----------------------------------------------------------------------------
// Messages and Connection Data Access
//----------------------------------------------------------------------------

  /// Gets the Decimal IP of the destination for the connection
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ConnectionIP
  /// @sn connectionIP:%s
  function  ConnectionIP(aConnection : Connection) : LongInt;

  /// Gets the Port of the destination for the connectiom
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ConnectionPort
  /// @sn connectionPort:%s
  function  ConnectionPort(aConnection : Connection) : LongInt;

  /// Dequeues the Top Message
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ReadMessage
  /// @sn readMessage:%s
  function ReadMessage            (aConnection : Connection) : String ;

  /// Clears the Message Queue
  ///
  /// @lib
  /// @sn clearMessageQueue:%s
  ///
  /// @class Connection
  /// @method ClearMessageQueue
  procedure ClearMessageQueue          (aConnection : Connection) ;

  /// Gets the Size of the Message Queue
  ///
  /// @lib
  /// @sn messageCountOnConnection:%s
  ///
  /// @class Connection
  /// @method MessageCount
  function  MessageCount            (aConnection : Connection) : LongInt;

  /// Queues a message to the end of the Message Queue
  ///
  /// @param aMsg The message Sent
  /// @param aConnection The connection to enqueue the message into
  ///
  /// @lib
  /// @sn enqueueMessage:%s toConnection:%s
  ///
  /// @class Connection
  /// @method EnqueueMessage
  /// @self 2
  procedure EnqueueMessage            ( aMsg : String; aConnection : Connection);


//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------

  /// Converts an Integer to a Hex value and returns it as a string.
  ///
  /// @param aDec The Integer
  ///
  /// @lib
  /// @uname DecToHex 
  /// @sn decToHex:%s
  function DecToHex                   (aDec : LongInt) : String;

  /// Converts a Hex String to a Decimal Value as a String.
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexToDecString 
  /// @sn hexToDecString:%s
  function HexToDecString             (aHex : String) : String;

  /// Converts a Hex String to an IPV4 Address (0.0.0.0)
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexStrToIPv4 
  /// @sn hexStrToIPv4:%s
  function HexStrToIPv4               (aHex : String) : String;

  /// Converts an IP to a decimal value
  ///
  /// @param aIP The IP
  ///
  /// @lib
  /// @sn iPv4ToDec:%s
  function IPv4ToDec(aIP : String) : LongInt; 

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
  /// @sn closeTCPHostSocketPort:%s
  function CloseTCPHostSocket        ( aPort: LongInt) : Boolean;

  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Receiver Sockets
  ///
  /// @param aConnection  The Connection to close
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method Close
  function CloseConnection            (var aConnection : Connection) : Boolean;

  /// An internal function used to close the specified Socket. 
  /// Call ``CloseConnection`` instead.
  ///
  /// @param aConnection  The Connection to close
  ///
  /// @lib
  /// 
  /// @class Connection
  /// @dispose
  procedure FreeConnection(var aConnection : Connection);
  
  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to UDP Listener Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  function CloseUDPListenSocket      ( aPort : LongInt) : Boolean;

  /// Closes All TCP Host Sockets
  ///
  /// @lib
  procedure CloseAllTCPHostSocket     ();

  /// Closes All TCP Receiver Sockets
  ///
  /// @lib
  procedure CloseAllConnections ();

  /// Closes All UDP Listener Sockets
  ///
  /// @lib
  procedure CloseAllUDPListenSocket   ();

  /// Closes all sockets that have been created.
  ///
  /// @lib
  procedure CloseAllSockets ();

  /// Releases All resources used by the Networking code.
  ///
  /// @lib
  procedure ReleaseAllConnections();

//----------------------------------------------------------------------------
// Other
//----------------------------------------------------------------------------
  /// Returns the caller's IP.
  ///
  /// @lib
  function MyIP                  () : String;
          
//=============================================================================
implementation
  uses SysUtils, sgUtils, sgDriverNetworking, sgNamedIndexCollection, sgShared, StrUtils;
//=============================================================================

type
  MessageData = packed record
    msg       : String;
    IP        : String;
    port      : LongInt;
  end;
  MessageDataArray = Array of MessageData;

  NewConnectionPtr = ^NewConnection;
  NewConnection = record
    con : Connection;
    nextCon : NewConnectionPtr;
  end;

var  
  _NewConnectionQueue     : NewConnectionPtr = nil;
  _NewConnectionCount     : LongInt = 0;
  
//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------
    
  function DecToHex(aDec : LongInt) : String;
  var
    LRemainder : LongInt;
    lHexAlpha : String = '0123456789ABCDEF';
  begin
    lRemainder := (aDec mod 16);
    if aDec - lRemainder = 0 then
      result := lHexAlpha[lRemainder + 1]
    else 
      result := DecToHex( (aDec - lRemainder) div 16 ) + lHexAlpha[lRemainder + 1]
  end;
      
  function HexToDecString(aHex : String) : String;
  var
    i    : LongInt;
    lVal  : LongInt = 0;
    lExpo : Double;
  begin
    for i := 1 to Length(aHex) do
    begin      
      lExpo := Exp((Length(aHex) - i)*Ln(16));
      case aHex[i] of
        '0' : lVal += Round(0  * lExpo);
        '1' : lVal += Round(1  * lExpo);
        '2' : lVal += Round(2  * lExpo);
        '3' : lVal += Round(3  * lExpo);
        '4' : lVal += Round(4  * lExpo);
        '5' : lVal += Round(5  * lExpo);
        '6' : lVal += Round(6  * lExpo);
        '7' : lVal += Round(7  * lExpo);
        '8' : lVal += Round(8  * lExpo); 
        '9' : lVal += Round(9  * lExpo);
        'A' : lVal += Round(10 * lExpo);
        'B' : lVal += Round(11 * lExpo);
        'C' : lVal += Round(12 * lExpo);
        'D' : lVal += Round(13 * lExpo);
        'E' : lVal += Round(14 * lExpo);
        'F' : lVal += Round(15 * lExpo);
      end;
    end;   
    result := IntToStr(lVal); 
  end;
  
  function HexStrToIPv4(aHex : String) : String;
  begin
    result :=       HexToDecString(aHex[1] + aHex[2]);
    result += '.' + HexToDecString(aHex[3] + aHex[4]);
    result += '.' + HexToDecString(aHex[5] + aHex[6]);
    result += '.' + HexToDecString(aHex[7] + aHex[8]);
  end;

  function IPv4ToDec(aIP : String) : LongInt;
  var
    w, x, y, z : LongInt;
  begin
    w := StrToInt(ExtractDelimited(1, aIP, ['.']));
    x := StrToInt(ExtractDelimited(2, aIP, ['.']));
    y := StrToInt(ExtractDelimited(3, aIP, ['.']));
    z := StrToInt(ExtractDelimited(4, aIP, ['.']));
    result := 16777216 * w + 65536 * x + 256 * y + z;
    WriteLn('Result: ', result);
  end;
  
// -- End Hex Code  


//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------
    

  function CreateTCPHost( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CreateTCPHost(aPort);
  end;
  
  function OpenTCPConnectionToHost( aIP : String;  aPort : LongInt) : Connection;
  begin
    result := NetworkingDriver.OpenTCPConnectionToHost(aIP, aPort);
  end;   
  
  function ServerAcceptTCPConnection() : LongInt;
  begin
    result := NetworkingDriver.ServerAcceptTCPConnection();
  end;

  procedure EnqueueNewConnection(aConnection : Connection);
  var
    lNewConLink : NewConnectionPtr;
    lLastLink : NewConnectionPtr = nil;
  begin
    New(lNewConLink);  
    lNewConLink^.con        := aConnection;
    lNewConLink^.nextCon    := nil;

    if _NewConnectionQueue = nil then
      _NewConnectionQueue   := lNewConLink
    else begin
      lLastLink := _NewConnectionQueue;

      while lLastLink^.nextCon <> nil do
        lLastLink := lLastLink^.nextCon;

      lLastLink^.nextCon := lNewConLink;
    end;  
    _NewConnectionCount += 1; 
  end;

  function FetchConnection() : Connection;
  var
    lTmp : NewConnectionPtr;
  begin  
    result := nil;
    if _NewConnectionQueue = nil then exit;

    result := _NewConnectionQueue^.con;

    lTmp := _NewConnectionQueue^.nextCon;
    Dispose(_NewConnectionQueue);
    _NewConnectionQueue := lTmp;
    _NewConnectionCount -= 1; 
  end;

  function ConnectionQueueSize() : LongInt;
  begin
    result := _NewConnectionCount;
  end;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------
  
  procedure EnqueueMessage( aMsg : String; aConnection : Connection);
  var
    MsgData   : MessagePtr;
  begin
    New(MsgData);  
    MsgData^.data := aMsg;
    MsgData^.next  := nil;
    if aConnection^.firstMsg = nil then
    begin
      aConnection^.firstMsg := MsgData;
      aConnection^.lastMsg  := MsgData;
    end else begin
      aConnection^.lastMsg^.next := MsgData;
      aConnection^.lastMsg       := MsgData;
    end;  
    aConnection^.MsgCount += 1;    
  end;
   
  function ReadMessage(aConnection : Connection) : String;
  var
    lTmp : MessagePtr;
  begin      
    if not Assigned(aConnection) or (aConnection^.msgCount = 0) then exit;
    if not Assigned(aConnection^.firstMsg) then begin aConnection^.lastMsg := nil; exit; end; 
    result := aConnection^.firstMsg^.data;
    lTmp := aConnection^.firstMsg^.next;
    Dispose(aConnection^.firstMsg);
    aConnection^.firstMsg := lTmp;
    aConnection^.msgCount -= 1;
  end;
  
  procedure ClearMessageQueue(aConnection : Connection);
  var
    i : LongInt;
  begin
    for i := 0 to aConnection^.msgCount do
    begin
      ReadMessage(aConnection);
    end;
  end;  

  function ConnectionIP(aConnection : Connection) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.ip;
  end;

  function ConnectionPort(aConnection : Connection) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.port;
  end;

  function MessageCount(aConnection : Connection) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.msgCount;
  end;

  function TCPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.TCPMessageReceived();
  end;
  
  function BroadcastTCPMessage( aMsg : String) : Boolean;
  begin
    result := NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;
  
  function SendTCPMessageTo( aMsg : String; aConnection : Connection) : Connection;
  begin
    result := NetworkingDriver.SendTCPMessageTo(aMsg, aConnection);
  end;

//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  function CreateUDPSocket( aPort : LongInt) : LongInt;
  begin
    result := NetworkingDriver.CreateUDPSocket(aPort);
  end;
  
  function CreateUDPConnection(aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 
  begin
    result := NetworkingDriver.CreateUDPConnection(aDestIP, aDestPort, aInPort);
  end;

  function UDPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function SendUDPMessage( aMsg : String; aConnection : Connection) : Boolean;
  begin
    result := NetworkingDriver.SendUDPMessage(aMsg, aConnection);
  end;

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------

  function CloseTCPHostSocket( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseTCPHostSocket(aPort);    
  end;

  function CloseConnection(var aConnection : Connection) : Boolean;
  begin
    result := NetworkingDriver.CloseConnection(aConnection);    
  end;

  function CloseUDPListenSocket( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseUDPListenSocket(aPort);    
  end;

  procedure CloseAllTCPHostSocket();
  begin     
    NetworkingDriver.CloseAllTCPHostSocket();    
  end;

  procedure CloseAllConnections();
  begin        
    NetworkingDriver.CloseAllConnections();    
  end;

  procedure CloseAllUDPListenSocket();
  begin        
    NetworkingDriver.CloseAllUDPListenSocket();    
  end;

  procedure CloseAllSockets();
  begin
    CloseAllTCPHostSocket();
    CloseAllConnections();
    CloseAllUDPListenSocket();
  end;

  procedure FreeConnection(var aConnection : Connection);
  begin
    CallFreeNotifier(aConnection);
    Dispose(aConnection);
    aConnection := nil;
  end;

  procedure ReleaseAllConnections();
  var
    i : LongInt;
  begin
    NetworkingDriver.FreeAllNetworkingResources();

    for i := 0 to _NewConnectionCount - 1 do
      FetchConnection();
    _NewConnectionCount := 0;
  end;

//----------------------------------------------------------------------------
// Other
//----------------------------------------------------------------------------

  function MyIP() : String;
  begin
    result := NetworkingDriver.MyIP();
  end;

//=============================================================================

end.