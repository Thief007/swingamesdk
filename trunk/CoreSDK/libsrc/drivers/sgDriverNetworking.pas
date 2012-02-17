unit sgDriverNetworking;

interface
uses sgShared, sgTypes, sgNamedIndexCollection;

type

    //TCP Connection
    CreateTCPHostProcedure             = function (const aPort : LongInt) : Boolean;
    OpenTCPConnectionToHostProcedure   = function (const aDestIP : String; const aDestPort : LongInt) : Connection;
    ServerAcceptTCPConnectionProcedure = function () : LongInt;

    //TCP Message
    BroadcastTCPMessageProcedure       = function (const aMsg : String) : Boolean;
    SendTCPMessageToProcedure          = function (const aMsg : String; const aConnection : Connection) : Connection;
    TCPMessageReceivedProcedure        = function () : Boolean;

    //UDP
    CreateUDPSocketProcedure           = function (const aPort : LongInt) : LongInt;
    CreateUDPConnectionProcedure       = function (aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 
    UDPMessageReceivedProcedure        = function () : Boolean;
    SendUDPMessageProcedure            = function (const aMsg : String; const aConnection : Connection) : Boolean;

    //Close Sockets
    CloseTCPHostSocketProcedure        = function (const aPort: LongInt) : Boolean;  
    CloseConnectionProcedure           = function (var aConnection : Connection) : Boolean;
    CloseUDPListenSocketProcedure      = function (const aPort : LongInt) : Boolean;

    CloseAllTCPHostSocketProcedure    = procedure();
    CloseAllConnectionsProcedure      = procedure();
    CloseAllUDPListenSocketProcedure  = procedure();

    FreeAllNetworkingResourcesProcedure = procedure();

    MyIPProcedure                     = function () : String;

  NetworkingDriverRecord = record
    CreateTCPHost               : CreateTCPHostProcedure;
    OpenTCPConnectionToHost     : OpenTCPConnectionToHostProcedure;
    ServerAcceptTCPConnection   : ServerAcceptTCPConnectionProcedure;
    TCPMessageReceived          : TCPMessageReceivedProcedure;
    BroadcastTCPMessage         : BroadcastTCPMessageProcedure;
    SendTCPMessageTo            : SendTCPMessageToProcedure;

    CreateUDPSocket             : CreateUDPSocketProcedure;
    CreateUDPConnection         : CreateUDPConnectionProcedure;
    UDPMessageReceived          : UDPMessageReceivedProcedure;
    SendUDPMessage              : SendUDPMessageProcedure;

    CloseTCPHostSocket          : CloseTCPHostSocketProcedure;  
    CloseConnection             : CloseConnectionProcedure;         
    CloseUDPListenSocket        : CloseUDPListenSocketProcedure;     

    MyIP                        : MyIPProcedure; 

    CloseAllTCPHostSocket       : CloseAllTCPHostSocketProcedure;     
    CloseAllConnections         : CloseAllConnectionsProcedure; 
    CloseAllUDPListenSocket     : CloseAllUDPListenSocketProcedure; 

    FreeAllNetworkingResources  : FreeAllNetworkingResourcesProcedure;
  end;
      
var
  NetworkingDriver : NetworkingDriverRecord;

implementation
uses
  {$IFDEF SWINGAME_SDL13}sgDriverNetworkingSDL{$ELSE}sgDriverNetworkingSDL{$ENDIF};

  procedure LoadDefaultNetworkingDriver(); 
  begin
    {$IFDEF SWINGAME_SDL13}
      LoadSDLNetworkingDriver();
    {$ELSE}
      LoadSDLNetworkingDriver();
    {$ENDIF}
  end;

  function DefaultMyIPProcedure() : String;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.MyIP();
  end;

//----------------------------------------------------------------------------
// TCP Connection Handling
//----------------------------------------------------------------------------

  function DefaultCreateTCPHostProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateTCPHost(aPort);
  end;

  function DefaultOpenTCPConnectionToHostProcedure(const aDestIP : String; const aDestPort : LongInt) : Connection;
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.OpenTCPConnectionToHost(aDestIP, aDestPort);
  end;  

  function DefaultServerAcceptTCPConnectionProcedure() : LongInt;
  begin  
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.ServerAcceptTCPConnection();
  end;

//----------------------------------------------------------------------------
// TCP Message Handling
//----------------------------------------------------------------------------

  function DefaultTCPMessageReceivedProcedure() : Boolean;
   begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.TCPMessageReceived();
   end;

  function DefaultSendTCPMessageToProcedure(const aMsg : String; const aConnection : Connection) : Connection;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.SendTCPMessageTo(aMsg, aConnection);
  end;

  function DefaultBroadcastTCPMessageProcedure(const aMsg : String) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;

//----------------------------------------------------------------------------
// UDP Connections
//----------------------------------------------------------------------------

  function DefaultCreateUDPSocketProcedure(const aPort : LongInt) : LongInt;
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateUDPSocket(aPort);
  end;
  
  function DefaultCreateUDPConnectionProcedure(aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateUDPConnection(aDestIP, aDestPort, aInPort);
  end;

//----------------------------------------------------------------------------
// UDP Message
//----------------------------------------------------------------------------

  function DefaultUDPMessageReceivedProcedure() : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function DefaultSendUDPMessageProcedure(const aMsg : String; const aConnection : Connection) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.SendUDPMessage(aMsg, aConnection);
  end;

//----------------------------------------------------------------------------
// Close Single
//----------------------------------------------------------------------------

  function DefaultCloseTCPHostSocketProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseTCPHostSocket(aPort);
  end;

  function DefaultCloseConnectionProcedure(var aConnection : Connection) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseConnection(aConnection);
  end;

  function DefaultCloseUDPListenSocketProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseUDPListenSocket(aPort);
  end;

//----------------------------------------------------------------------------
// Close All
//----------------------------------------------------------------------------

  procedure DefaultCloseAllTCPHostSocketProcedure();
  begin    
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllTCPHostSocket();
  end;

  procedure DefaultCloseAllConnectionsProcedure();
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllConnections();
  end;

  procedure DefaultCloseAllUDPListenSocketProcedure();
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllUDPListenSocket();
  end;  

  procedure DefaultFreeAllNetworkingResourcesProcedure();
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.FreeAllNetworkingResources();
  end;

  initialization 
  begin
    NetworkingDriver.CreateTCPHost               := @DefaultCreateTCPHostProcedure;
    NetworkingDriver.OpenTCPConnectionToHost     := @DefaultOpenTCPConnectionToHostProcedure;
    NetworkingDriver.ServerAcceptTCPConnection   := @DefaultServerAcceptTCPConnectionProcedure;
    NetworkingDriver.TCPMessageReceived          := @DefaultTCPMessageReceivedProcedure;
    NetworkingDriver.BroadcastTCPMessage         := @DefaultBroadcastTCPMessageProcedure;
    NetworkingDriver.SendTCPMessageTo            := @DefaultSendTCPMessageToProcedure;

    NetworkingDriver.CreateUDPSocket             := @DefaultCreateUDPSocketProcedure;
    NetworkingDriver.CreateUDPConnection         := @DefaultCreateUDPConnectionProcedure;
    NetworkingDriver.UDPMessageReceived          := @DefaultUDPMessageReceivedProcedure;
    NetworkingDriver.SendUDPMessage              := @DefaultSendUDPMessageProcedure;

    NetworkingDriver.CloseTCPHostSocket          := @DefaultCloseTCPHostSocketProcedure;
    NetworkingDriver.CloseConnection             := @DefaultCloseConnectionProcedure;
    NetworkingDriver.CloseUDPListenSocket        := @DefaultCloseUDPListenSocketProcedure;

    NetworkingDriver.MyIP                        := @DefaultMyIPProcedure;

    NetworkingDriver.CloseAllTCPHostSocket       :=@DefaultCloseAllTCPHostSocketProcedure;     
    NetworkingDriver.CloseAllConnections         :=@DefaultCloseAllConnectionsProcedure; 
    NetworkingDriver.CloseAllUDPListenSocket     :=@DefaultCloseAllUDPListenSocketProcedure; 

    NetworkingDriver.FreeAllNetworkingResources  := @DefaultFreeAllNetworkingResourcesProcedure;
  end;
end.