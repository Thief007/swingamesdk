unit sgNetworking;

interface
uses sgShared, sgTypes, sgNamedIndexCollection;

  //TCP
  function CreateTCPHost              (port : LongInt) : Boolean;
  function OpenTCPConnectionToHost    (ip : String; port : LongInt) : Boolean;
  function ServerAcceptTCPConnection  () : Boolean;  
  function TCPMessageReceived         () : Boolean;    
  function BroadcastTCPMessage        (msg : String) : Boolean;
  function SendTCPMessageTo           (msg, ip : String; port : LongInt) : Boolean;
  //UDP
  function OpenUDPListenerSocket      (port : LongInt) : Boolean;
  function OpenUDPSendSocket          () : Boolean;
  function UDPMessageReceived         () : Boolean;
  function SendUDPMessage             (msg, ip : String; port : LongInt) : Boolean;
  //Messages
  function DequeueMessage             () : MessageData;
  function PopAllMessages             () : MessageDataArray;
  procedure EnqueueMessage            (msg : String; ip : String; port : LongInt);
  //Hex-Dec Conversion
  function DecToHex                   (dec : LongInt) : String;
  function HexToDecString             (hex : String) : String;
  function HexStrToIPv4               (hex : String) : String;
  //Close
  function CloseTCPHostSocket        (aPort: LongInt) : Boolean;
  function CloseTCPReceiverSocket    (aIP : String; aPort : LongInt) : Boolean;
  function CloseTCPSenderSocket      (aIP : String; aPort : LongInt) : Boolean;
  function CloseUDPSocket            (aPort : LongInt) : Boolean;

          
implementation
uses SysUtils, sgUtils, sgDriverNetworking;

var  
  _ConSocketIndexes       : NamedIndexCollection;
  _ReceiveSocketIndexes   : NamedIndexCollection;
  _SendSocketIndexes      : NamedIndexCollection;
  _UDPSocketIndexes       : NamedIndexCollection;
  _LastMessage            : MessagePtr = nil;
  _FirstMessage           : MessagePtr = nil;
  _MessageCount           : LongInt = 0;
  
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

// -- End Connection Code
// -- Start Message Code
  
  
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
  
  function SendTCPMessageTo(msg, ip : String; port : LongInt) : Boolean;
  begin
    result := NetworkingDriver.SendTCPMessageTo(msg, ip, port, _SendSocketIndexes);
  end;
// -- End Message Code  
// -- Start UDP Section
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

  function CloseUDPSocket(aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseUDPSocket(_UDPSocketIndexes, aPort);    
  end;

// -- End UDP Section  

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