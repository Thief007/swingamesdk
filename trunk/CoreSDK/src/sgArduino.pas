/// SwinGame's Arduino unit is capable of connecting and communicating
/// with an Arduino device using a Serial port.
///
/// @module sgArduino
/// @static
///
/// @doc_types ArduinoDevice
unit sgArduino;

interface
	uses sgTypes;

	/// Creates an Arduino device at the specified port, with
	/// the indicated baud. The name of the device matches its port.
	///
	/// @lib
	/// @sn createArduinoOnPort:%s atBaud:%s
	///
	/// @class ArduinoDevice
	/// @constructor
	/// @csn initOnPort:%s atBaud:%s
	function CreateArduinoDevice(port: String; baud: LongInt) : ArduinoDevice;

	/// Creates an Arduino device with the given name, 
	/// at the specified port, with the indicated baud.
	///
	/// @lib CreateArduinoNamed
	/// @sn createArduinoNamed:%s onPort:%s atBaud:%s 
	///
	/// @class ArduinoDevice
	/// @constructor
	/// @csn initWithName:%s OnPort:%s atBaud:%s
	function CreateArduinoDevice(name, port: String; baud: LongInt) : ArduinoDevice;

	/// Returns the ArduinoDevice with the indicated name.
	///
	/// @lib
	function ArduinoDeviceNamed(name: String): ArduinoDevice;

	/// Does an ArduinoDevice exist with the indicated name?
	///
	/// @lib
	function HasArduinoDevice(name: String): Boolean;

	/// Release the ArduinoDevice with the indicated name.
	///
	/// @lib
	procedure ReleaseArduinoDevice(name: String);

	/// Close the connection to the Arduino Device and dispose
	/// of the resources associated with the Device.
	/// 
	/// @lib
	/// @sn ArduinoCloseConnection:%s
	///
	/// @class ArduinoDevice
	/// @dispose
	procedure FreeArduinoDevice(var dev: ArduinoDevice);

	/// Connect the game to the Arduino device.
	///
	/// @lib
	/// @sn connectToDevice:%s
	/// 
	/// @class ArduinoDevice
	/// @method Connect
	/// @csn connect
	procedure ArduinoConnectToDevice(dev: ArduinoDevice);

	/// Reads a line of text from the ArduinoDevice. This
	/// returns an empty string if nothing is read within a
	/// few milliseconds.
	/// 
	/// @lib
	///
	/// @class ArduinoDevice
	/// @method ReadLine
	function ArduinoReadLine(dev: ArduinoDevice): String;

	/// Reads a line of text from the ArduinoDevice, within a 
	/// specified amount of time.
	///
	/// @lib ArduinoReadLineTimeout
	/// @sn arduinoReadLine:%s timeout:%s
	///
	/// @class ArduinoDevice
	/// @overload ReadLine ReadLineTimeout
	/// @csn readLineWithTimeout:%s
	function ArduinoReadLine(dev: ArduinoDevice; timeout: LongInt): String;

	/// Read an Integer from the ArduinoDevice. Has a short
	/// timeout and returns 0 if no integer is read within the given time.
	/// 
	/// @lib
	///
	/// @class ArduinoDevice
	/// @method ReadInteger
	function ArduinoReadInteger(dev: ArduinoDevice): LongInt;

	/// Reads an integer from the ArduinoDevice, with the given timeout in milliseconds.
	/// Returns 0 if no integer is read within the given time.
	/// 
	/// @lib ArduinoReadIntegerTimeout
	/// @sn arduinoReadInteger:%s timeout:%s
	///
	/// @class ArduinoDevice
	/// @overload ReadInteger ReadIntegerTimeout
	/// @csn readIntegerWithTimeout:%s
	function ArduinoReadInteger(dev: ArduinoDevice; timeout: LongInt): LongInt;

	/// Returns true if there is data waiting to be read from the device.
	///
	/// @lib
	///
	/// @class ArduinoDevice
	/// @getter HasData
	function ArduinoHasData(dev: ArduinoDevice): Boolean;

	/// Release all of the ArduinoDevices
	/// 
	/// @lib
	procedure ReleaseAllArduinoDevices();

implementation
uses
	SysUtils, Classes, Synaser, sgShared, sgUtils, stringhash;

    var _ArduinoDevices: TStringHash;


	function CreateArduinoDevice(port: String; baud: LongInt) : ArduinoDevice;
	begin
		result := CreateArduinoDevice(port, port, baud);
	end;

	function CreateArduinoDevice(name, port: String; baud: LongInt) : ArduinoDevice;
    var
        obj: tResourceContainer;
	begin
        if HasArduinoDevice(name) then
        begin
            result := ArduinoDeviceNamed(name);
            exit;
        end;

		New(result);
		result^.name := name;
		result^.ptr := Pointer(TBlockSerial.Create());
		result^.port := port;
		result^.baud := baud;
		result^.open := false;
		result^.hasError := false;
		result^.errorMessage := 'Working';

		obj := tResourceContainer.Create(result);
        if not _ArduinoDevices.setValue(name, obj) then
        begin
            RaiseWarning('** Leaking: Caused by ArduinoDevice resource loading twice, ' + name);
            result := nil;
            exit;
        end;
	end;

	procedure ArduinoConnectToDevice(dev: ArduinoDevice);
	var
	    ser: TBlockSerial;	
	begin
		if assigned(dev) then
		begin
			ser := TBlockSerial(dev^.ptr);
		    // WriteLn('Connecting...');
		    ser.Connect(dev^.port);	

		    // WriteLn('Configure...');
		    ser.Config(dev^.baud, 8, 'N', SB1, False, False);
		    if ser.LastError <> sOK then
		    begin
    		    RaiseWarning('Error configuring connection to Arduino: ' + IntToStr(ser.LastError) + ' ' + ser.LastErrorDesc);
		    	dev^.hasError := true;
		    	dev^.errorMessage := ser.LastErrorDesc;
		    	exit;
		    end;

    		ArduinoReadLine(dev, 15);
	    end;
	end;

	function ArduinoHasData(dev: ArduinoDevice): Boolean;
	var
	    ser: TBlockSerial;	
	begin
		result := false;
		if assigned(dev) then
		begin
			ser := TBlockSerial(dev^.ptr);
			result := ser.CanRead(0);
		end;
	end;

	function ArduinoReadLine(dev: ArduinoDevice): String;
	begin
		result := ArduinoReadLine(dev, 10);
	end;

	function ArduinoReadLine(dev: ArduinoDevice; timeout: LongInt): String;
	var
	    ser: TBlockSerial;	
	begin
		result := '';
		if assigned(dev) then
		begin
			ser := TBlockSerial(dev^.ptr);
			result := ser.RecvString(timeout)
		end;
	end;

	function ArduinoReadInteger(dev: ArduinoDevice): LongInt;
	begin
		result := ArduinoReadInteger(dev, 10);
	end;

	function ArduinoReadInteger(dev: ArduinoDevice; timeout: LongInt): LongInt;
	var
	    ser: TBlockSerial;	
	begin
		result := 0;
		if assigned(dev) then
		begin
			ser := TBlockSerial(dev^.ptr);
			result := ser.RecvInteger(timeout)
		end;
	end;

	// ========================
	// = Resource Management Routines
	// ========================	


    // private:
    // Called to actually free the resource
    procedure DoFreeArduinoDevice(var dev: ArduinoDevice);
	var
	    ser: TBlockSerial;	
    begin
        if assigned(dev) then
        begin
            CallFreeNotifier(dev);
			ser := TBlockSerial(dev^.ptr);
			ser.Free();
			dev^.ptr := nil;
            Dispose(dev);
        end;

        dev := nil;
    end;
    
    procedure FreeArduinoDevice(var dev: ArduinoDevice);
	begin
		if assigned(dev) then
		begin
			ReleaseArduinoDevice(dev^.name);
		end;

        dev := nil;
    end;
    
    procedure ReleaseArduinoDevice(name: String);
    var
        dev: ArduinoDevice;
    begin
        dev := ArduinoDeviceNamed(name);
        if assigned(dev) then
        begin
            _ArduinoDevices.remove(name).Free();
            DoFreeArduinoDevice(dev);
        end;
    end;
    
    procedure ReleaseAllArduinoDevices();
    begin
        ReleaseAll(_ArduinoDevices, @ReleaseArduinoDevice);
    end;

    function HasArduinoDevice(name: String): Boolean;
    begin
        result := _ArduinoDevices.containsKey(name);
    end;

    function ArduinoDeviceNamed(name: String): ArduinoDevice;
    var
        tmp : TObject;
    begin
        tmp := _ArduinoDevices.values[name];
        if assigned(tmp) then 
        	result := ArduinoDevice(tResourceContainer(tmp).Resource)
        else result := nil;
    end;


initialization
    begin
        InitialiseSwinGame();
        _ArduinoDevices := TStringHash.Create(False, 1024);
    end;
    
    finalization
    begin
        ReleaseAllArduinoDevices();
        FreeAndNil(_ArduinoDevices);
    end;
end.

