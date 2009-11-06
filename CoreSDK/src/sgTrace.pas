//=============================================================================
// sgTrace.pas
//=============================================================================
//
// Support "tracing" and writing of messages to a trace file (Trace.log).
// "Trace" must be defined before these will be included (see sgTrace.inc).
//
// Change History:
//
// Version 3:
// - 2009-11-06: Andrew : Fixed formatting
// - 2009-09-11: Andrew : Fixed io exceptions
// - 2009-06-23: Clinton: Comment formatting/cleanup
//
// Version 2:
// - 2008-12-17: Andrew : Moved all integers to LongInt
//
// Version 1.1.6:
// - 2008-05-09: Andrew : Introduced unit
//=============================================================================

unit sgTrace;

//=============================================================================
interface
//=============================================================================

  {$I sgTrace.inc}

  {$IFDEF Trace}
  type
      TraceLevel = (tlNone, tlError, tlWarning, tlInfo, tlVerbose);

    const TRACE_LEVEL = 4;

    procedure Trace(unitname, action, routine, message: String);
    procedure TraceIf(tl: TraceLevel; unitname, action, routine, message: String);
    procedure TraceEnter(unitName, routine: String); overload;
    procedure TraceEnter(unitName, routine, message: String); overload;
    procedure TraceExit(unitName, routine: String); overload;
    procedure TraceExit(unitName, routine, message: String); overload;
  {$ENDIF}

//=============================================================================
implementation
//=============================================================================

  uses SysUtils
  {$IFDEF UNIX}
    , BaseUnix
  {$ENDIF}
  ;

  {$IFDEF Trace}
  {$Info SwinGame Tracing Enabled}

  var 
    indentLevel: LongInt;
    output: Text;

  procedure Trace(unitname, action, routine, message: String);
  begin
    try 
      WriteLn(output, unitname, ': ':(15 - Length(unitname)), action, ': ':(8 - Length(action)), StringOfChar(' ', indentLevel * 2), routine, ': ', message);
    except
    end;
  end;

  procedure TraceIf(tl: TraceLevel; unitname, action, routine, message: String);
  begin
    if TRACE_LEVEL >= LongInt(tl) then
      Trace(unitname, action, routine, message);
  end;

  procedure TraceEnter(unitName, routine: String); overload;
  begin
    TraceEnter(unitName, routine, '');
  end;

  procedure TraceEnter(unitName, routine, message: String); overload;
  begin
    Trace(unitName, 'Enter', routine, message);
    indentLevel := indentLevel + 1;
  end;

  procedure TraceExit(unitName, routine: String); overload;
  begin
    TraceExit(unitName, routine, '');
  end;
  
  procedure TraceExit(unitName, routine, message: String); overload;
  begin
    indentLevel := indentLevel - 1;
    Trace(unitName, 'Exit', routine, message);
    if indentLevel = 0 then WriteLn(output, StringOfChar('-', 50));
  end;

  //=============================================================================
  
  initialization
  begin
    try
      {$IFDEF UNIX}
      fpChmod ('Trace.log',S_IWUSR or S_IRUSR or S_IWGRP or S_IRGRP or S_IWOTH or S_IROTH);
      {$ENDIF}
      Assign(output, 'Trace.log');
      Rewrite(output);
    except
      WriteLn('ERROR: Unable to write to trace file. Please make Trace.log writable by this program.')
    end;
  end;

//=============================================================================

  finalization
  begin
    try
      Close(output);
    except
    end;
  end;


  {$ENDIF}
end.
