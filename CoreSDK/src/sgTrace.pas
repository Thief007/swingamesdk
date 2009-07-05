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
    procedure TraceExit(unitName, routine: String);
  {$ENDIF}

//=============================================================================
implementation
//=============================================================================

  uses SysUtils;

  {$IFDEF Trace}
  {$Info SwinGame Tracing Enabled}

  var indentLevel: LongInt;

  procedure Trace(unitname, action, routine, message: String);
  var
    output: Text;
  begin
    Assign(output, 'Trace.log');
    if FileExists('Trace.log') then Append(output)
    else Rewrite(output);

    WriteLn(output, StringOfChar(' ', indentLevel * 2), unitname:10, ': ', action:10, ': ', routine:10, ': ', message);
    Close(output);
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

  procedure TraceExit(unitName, routine: String);
  begin
    indentLevel := indentLevel - 1;
    Trace(unitName, 'Exit', routine, '');
  end;

  {$ENDIF}
end.
