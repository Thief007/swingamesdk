///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SwinGameTrace.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// This unit contains the code used to write to the 
// Trace file. TRACE must be defined before these will
// be included. See SwinGame.inc.
//
// Change History:
//  
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
//
//  Version 1.1.6:
//   - 2008-05-09: Andrew: Introduced unit

unit SwinGameTrace;

interface

	{$I SwinGame.inc}

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

implementation
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
