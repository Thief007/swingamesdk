unit sgDriverNetworking;

interface
uses sgShared, sgTypes, sgNamedIndexCollection;

type

  //TCP
  CreateTCPHostProcedure              = function (aPort : LongInt; aConSocketIndexes : NamedIndexCollection) : Boolean;
  OpenTCPConnectionToHostProcedure    = function (aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : Boolean;
  ServerAcceptTCPConnectionProcedure  = function (aReceiveSocketIndexes, aConSocketIndexes : NamedIndexCollection) : Boolean;
  TCPMessageReceivedProcedure         = function (aReceiveSocketIndexes : NamedIndexCollection) : Boolean;    
  BroadcastTCPMessageProcedure        = function (aMsg : String) : Boolean;
  SendTCPMessageToProcedure           = function (aMsg, aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : Boolean;
  //UDP
  OpenUDPListenerSocketProcedure      = function (aPort : LongInt; aUDPSocketIndexes : NamedIndexCollection) : Boolean;
  OpenUDPSendSocketProcedure          = function () : Boolean;
  UDPMessageReceivedProcedure         = function () : Boolean;
  SendUDPMessageProcedure             = function (aMsg, aIP : String; aPort : LongInt) : Boolean;

  CloseTCPHostSocketProcedure         = function (aCollection : NamedIndexCollection; aPort: LongInt) : Boolean;  
  CloseTCPReceiverSocketProcedure     = function (aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  CloseTCPSenderSocketProcedure       = function (aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  CloseUDPSocketProcedure             = function (aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;

  NetworkingDriverRecord = record
    CreateTCPHost               : CreateTCPHostProcedure;
    OpenTCPConnectionToHost     : OpenTCPConnectionToHostProcedure;
    ServerAcceptTCPConnection   : ServerAcceptTCPConnectionProcedure;
    TCPMessageReceived          : TCPMessageReceivedProcedure;
    BroadcastTCPMessage         : BroadcastTCPMessageProcedure;
    SendTCPMessageTo            : SendTCPMessageToProcedure;

    OpenUDPListenerSocket       : OpenUDPListenerSocketProcedure;
    OpenUDPSendSocket           : OpenUDPSendSocketProcedure;
    UDPMessageReceived          : UDPMessageReceivedProcedure;
    SendUDPMessage              : SendUDPMessageProcedure;

    CloseTCPHostSocket          : CloseTCPHostSocketProcedure;  
    CloseTCPReceiverSocket      : CloseTCPReceiverSocketProcedure;  
    CloseTCPSenderSocket        : CloseTCPSenderSocketProcedure;          
    CloseUDPSocket              : CloseUDPSocketProcedure; 
  end;
      
var
  NetworkingDriver : NetworkingDriverRecord;

implementation
uses
  {$IFDEF SWINGAME_SDL13}sgDriverNetworkingSDL{$ELSE}sgDriverNetworkingSDL{$ENDIF};

  procedure LoadDefaultNetworkingDriver; 
  begin
    {$IFDEF SWINGAME_SDL13}
      LoadSDLNetworkingDriver();
    {$ELSE}
      LoadSDLNetworkingDriver();
    {$ENDIF}
  end;
  
  
// -- End Hex Code  
// -- Start Connection Code
  
  function DefaultCreateTCPHostProcedure(aPort : LongInt; aConSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.CreateTCPHost(aPort, aConSocketIndexes);
  end;
  
  function DefaultOpenTCPConnectionToHostProcedure(aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.OpenTCPConnectionToHost(aIP, aPort, aSendSocketIndexes);
  end;
 
  
  function DefaultServerAcceptTCPConnectionProcedure(aReceiveSocketIndexes, aConSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.ServerAcceptTCPConnection(aReceiveSocketIndexes, aConSocketIndexes);
  end;
  
  function DefaultTCPMessageReceivedProcedure(aReceiveSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.TCPMessageReceived(aReceiveSocketIndexes);
  end;
  
  function DefaultBroadcastTCPMessageProcedure(aMsg : String) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;
  
  function DefaultSendTCPMessageToProcedure(aMsg, aIP : String; aPort : LongInt; aSendSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.SendTCPMessageTo(aMsg, aIP, aPort, aSendSocketIndexes);
  end;
// -- End Message Code  
// -- Start UDP Section
  function DefaultOpenUDPListenerSocketProcedure(aPort : LongInt; aUDPSocketIndexes : NamedIndexCollection) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.OpenUDPListenerSocket(aPort, aUDPSocketIndexes);
  end;
  
  function DefaultOpenUDPSendSocketProcedure() : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.OpenUDPSendSocket();
  end;

  function DefaultUDPMessageReceivedProcedure() : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function DefaultSendUDPMessageProcedure(aMsg, aIP : String; aPort : LongInt) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.SendUDPMessage(aMsg, aIP, aPort);
  end;

  function DefaultCloseTCPHostSocketProcedure(aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.CloseTCPHostSocket(aCollection, aPort);    
  end;

  function DefaultCloseTCPReceiverSocketProcedure(aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.CloseTCPReceiverSocket(aCollection, aIP, aPort);    
  end;

  function DefaultCloseTCPSenderSocketProcedure(aCollection : NamedIndexCollection; aIP : String; aPort : LongInt) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.CloseTCPSenderSocket(aCollection, aIP, aPort);    
  end;

  function DefaultCloseUDPSocketProcedure(aCollection : NamedIndexCollection; aPort : LongInt) : Boolean;
  begin
    LoadSDLNetworkingDriver();
    result := NetworkingDriver.CloseUDPSocket(aCollection, aPort);    
  end;

// -- End UDP Section  

  initialization 
  begin
    NetworkingDriver.CreateTCPHost               := @DefaultCreateTCPHostProcedure;
    NetworkingDriver.OpenTCPConnectionToHost     := @DefaultOpenTCPConnectionToHostProcedure;
    NetworkingDriver.ServerAcceptTCPConnection   := @DefaultServerAcceptTCPConnectionProcedure;
    NetworkingDriver.TCPMessageReceived          := @DefaultTCPMessageReceivedProcedure;
    NetworkingDriver.BroadcastTCPMessage         := @DefaultBroadcastTCPMessageProcedure;
    NetworkingDriver.SendTCPMessageTo            := @DefaultSendTCPMessageToProcedure;

    NetworkingDriver.OpenUDPListenerSocket       := @DefaultOpenUDPListenerSocketProcedure;
    NetworkingDriver.OpenUDPSendSocket           := @DefaultOpenUDPSendSocketProcedure;
    NetworkingDriver.UDPMessageReceived          := @DefaultUDPMessageReceivedProcedure;
    NetworkingDriver.SendUDPMessage              := @DefaultSendUDPMessageProcedure;

    NetworkingDriver.CloseTCPHostSocket          := @DefaultCloseTCPHostSocketProcedure;
    NetworkingDriver.CloseTCPReceiverSocket      := @DefaultCloseTCPReceiverSocketProcedure;
    NetworkingDriver.CloseTCPSenderSocket        := @DefaultCloseTCPSenderSocketProcedure;
    NetworkingDriver.CloseUDPSocket              := @DefaultCloseUDPSocketProcedure;
{
    NetworkingDriver.DequeueMessage              : @DefaultDequeueMessageProcedure
    NetworkingDriver.PopAllMessages              : @DefaultPopAllMessagesProcedure}
  end;
end.