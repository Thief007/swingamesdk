library PascalSDLTypeChecker;
uses SDL, SysUtils;
type IntPtr = ^LongInt;

   procedure PascalCheckEnum_SDL_bool(ptr: IntPtr); cdecl; export;
   var
     _SDL_bool : SDL_bool;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_bool) then WriteLn('Different size for SDL_bool!');
     cptr += 1;
     
     for _SDL_bool := Low(SDL_bool) to High(SDL_bool) do
     begin
        try
            WriteStr(name, _SDL_bool);
            if cptr^ <> LongInt(_SDL_bool) then
                WriteLn('Values differ in SDL_bool for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_bool));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_DUMMY_ENUM(ptr: IntPtr); cdecl; export;
   var
     _SDL_DUMMY_ENUM : SDL_DUMMY_ENUM;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_DUMMY_ENUM) then WriteLn('Different size for SDL_DUMMY_ENUM!');
     cptr += 1;
     
     for _SDL_DUMMY_ENUM := Low(SDL_DUMMY_ENUM) to High(SDL_DUMMY_ENUM) do
     begin
        try
            WriteStr(name, _SDL_DUMMY_ENUM);
            if cptr^ <> LongInt(_SDL_DUMMY_ENUM) then
                WriteLn('Values differ in SDL_DUMMY_ENUM for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_DUMMY_ENUM));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_assert_state(ptr: IntPtr); cdecl; export;
   var
     _SDL_assert_state : SDL_assert_state;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_assert_state) then WriteLn('Different size for SDL_assert_state!');
     cptr += 1;
     
     for _SDL_assert_state := Low(SDL_assert_state) to High(SDL_assert_state) do
     begin
        try
            WriteStr(name, _SDL_assert_state);
            if cptr^ <> LongInt(_SDL_assert_state) then
                WriteLn('Values differ in SDL_assert_state for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_assert_state));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_errorcode(ptr: IntPtr); cdecl; export;
   var
     _SDL_errorcode : SDL_errorcode;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_errorcode) then WriteLn('Different size for SDL_errorcode!');
     cptr += 1;
     
     for _SDL_errorcode := Low(SDL_errorcode) to High(SDL_errorcode) do
     begin
        try
            WriteStr(name, _SDL_errorcode);
            if cptr^ <> LongInt(_SDL_errorcode) then
                WriteLn('Values differ in SDL_errorcode for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_errorcode));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_ThreadPriority(ptr: IntPtr); cdecl; export;
   var
     _SDL_ThreadPriority : SDL_ThreadPriority;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_ThreadPriority) then WriteLn('Different size for SDL_ThreadPriority!');
     cptr += 1;
     
     for _SDL_ThreadPriority := Low(SDL_ThreadPriority) to High(SDL_ThreadPriority) do
     begin
        try
            WriteStr(name, _SDL_ThreadPriority);
            if cptr^ <> LongInt(_SDL_ThreadPriority) then
                WriteLn('Values differ in SDL_ThreadPriority for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_ThreadPriority));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_AudioStatus(ptr: IntPtr); cdecl; export;
   var
     _SDL_AudioStatus : SDL_AudioStatus;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_AudioStatus) then WriteLn('Different size for SDL_AudioStatus!');
     cptr += 1;
     
     for _SDL_AudioStatus := Low(SDL_AudioStatus) to High(SDL_AudioStatus) do
     begin
        try
            WriteStr(name, _SDL_AudioStatus);
            if cptr^ <> LongInt(_SDL_AudioStatus) then
                WriteLn('Values differ in SDL_AudioStatus for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_AudioStatus));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_BlendMode(ptr: IntPtr); cdecl; export;
   var
     _SDL_BlendMode : SDL_BlendMode;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_BlendMode) then WriteLn('Different size for SDL_BlendMode!');
     cptr += 1;
     
     for _SDL_BlendMode := Low(SDL_BlendMode) to High(SDL_BlendMode) do
     begin
        try
            WriteStr(name, _SDL_BlendMode);
            if cptr^ <> LongInt(_SDL_BlendMode) then
                WriteLn('Values differ in SDL_BlendMode for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_BlendMode));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_WindowFlags(ptr: IntPtr); cdecl; export;
   var
     _SDL_WindowFlags : SDL_WindowFlags;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_WindowFlags) then WriteLn('Different size for SDL_WindowFlags!');
     cptr += 1;
     
     for _SDL_WindowFlags := Low(SDL_WindowFlags) to High(SDL_WindowFlags) do
     begin
        try
            WriteStr(name, _SDL_WindowFlags);
            if cptr^ <> LongInt(_SDL_WindowFlags) then
                WriteLn('Values differ in SDL_WindowFlags for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_WindowFlags));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_WindowEventID(ptr: IntPtr); cdecl; export;
   var
     _SDL_WindowEventID : SDL_WindowEventID;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_WindowEventID) then WriteLn('Different size for SDL_WindowEventID!');
     cptr += 1;
     
     for _SDL_WindowEventID := Low(SDL_WindowEventID) to High(SDL_WindowEventID) do
     begin
        try
            WriteStr(name, _SDL_WindowEventID);
            if cptr^ <> LongInt(_SDL_WindowEventID) then
                WriteLn('Values differ in SDL_WindowEventID for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_WindowEventID));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_GLattr(ptr: IntPtr); cdecl; export;
   var
     _SDL_GLattr : SDL_GLattr;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_GLattr) then WriteLn('Different size for SDL_GLattr!');
     cptr += 1;
     
     for _SDL_GLattr := Low(SDL_GLattr) to High(SDL_GLattr) do
     begin
        try
            WriteStr(name, _SDL_GLattr);
            if cptr^ <> LongInt(_SDL_GLattr) then
                WriteLn('Values differ in SDL_GLattr for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_GLattr));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_Scancode(ptr: IntPtr); cdecl; export;
   var
     _SDL_Scancode : SDL_Scancode;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_Scancode) then WriteLn('Different size for SDL_Scancode!');
     cptr += 1;
     
     for _SDL_Scancode := Low(SDL_Scancode) to High(SDL_Scancode) do
     begin
        try
            WriteStr(name, _SDL_Scancode);
            if cptr^ <> LongInt(_SDL_Scancode) then
                WriteLn('Values differ in SDL_Scancode for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_Scancode));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_Keymod(ptr: IntPtr); cdecl; export;
   var
     _SDL_Keymod : SDL_Keymod;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_Keymod) then WriteLn('Different size for SDL_Keymod!');
     cptr += 1;
     
     for _SDL_Keymod := Low(SDL_Keymod) to High(SDL_Keymod) do
     begin
        try
            WriteStr(name, _SDL_Keymod);
            if cptr^ <> LongInt(_SDL_Keymod) then
                WriteLn('Values differ in SDL_Keymod for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_Keymod));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_EventType(ptr: IntPtr); cdecl; export;
   var
     _SDL_EventType : SDL_EventType;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_EventType) then WriteLn('Different size for SDL_EventType!');
     cptr += 1;
     
     for _SDL_EventType := Low(SDL_EventType) to High(SDL_EventType) do
     begin
        try
            WriteStr(name, _SDL_EventType);
            if cptr^ <> LongInt(_SDL_EventType) then
                WriteLn('Values differ in SDL_EventType for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_EventType));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_eventaction(ptr: IntPtr); cdecl; export;
   var
     _SDL_eventaction : SDL_eventaction;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_eventaction) then WriteLn('Different size for SDL_eventaction!');
     cptr += 1;
     
     for _SDL_eventaction := Low(SDL_eventaction) to High(SDL_eventaction) do
     begin
        try
            WriteStr(name, _SDL_eventaction);
            if cptr^ <> LongInt(_SDL_eventaction) then
                WriteLn('Values differ in SDL_eventaction for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_eventaction));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_HintPriority(ptr: IntPtr); cdecl; export;
   var
     _SDL_HintPriority : SDL_HintPriority;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_HintPriority) then WriteLn('Different size for SDL_HintPriority!');
     cptr += 1;
     
     for _SDL_HintPriority := Low(SDL_HintPriority) to High(SDL_HintPriority) do
     begin
        try
            WriteStr(name, _SDL_HintPriority);
            if cptr^ <> LongInt(_SDL_HintPriority) then
                WriteLn('Values differ in SDL_HintPriority for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_HintPriority));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_LogPriority(ptr: IntPtr); cdecl; export;
   var
     _SDL_LogPriority : SDL_LogPriority;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_LogPriority) then WriteLn('Different size for SDL_LogPriority!');
     cptr += 1;
     
     for _SDL_LogPriority := Low(SDL_LogPriority) to High(SDL_LogPriority) do
     begin
        try
            WriteStr(name, _SDL_LogPriority);
            if cptr^ <> LongInt(_SDL_LogPriority) then
                WriteLn('Values differ in SDL_LogPriority for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_LogPriority));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_PowerState(ptr: IntPtr); cdecl; export;
   var
     _SDL_PowerState : SDL_PowerState;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_PowerState) then WriteLn('Different size for SDL_PowerState!');
     cptr += 1;
     
     for _SDL_PowerState := Low(SDL_PowerState) to High(SDL_PowerState) do
     begin
        try
            WriteStr(name, _SDL_PowerState);
            if cptr^ <> LongInt(_SDL_PowerState) then
                WriteLn('Values differ in SDL_PowerState for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_PowerState));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_RendererFlags(ptr: IntPtr); cdecl; export;
   var
     _SDL_RendererFlags : SDL_RendererFlags;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_RendererFlags) then WriteLn('Different size for SDL_RendererFlags!');
     cptr += 1;
     
     for _SDL_RendererFlags := Low(SDL_RendererFlags) to High(SDL_RendererFlags) do
     begin
        try
            WriteStr(name, _SDL_RendererFlags);
            if cptr^ <> LongInt(_SDL_RendererFlags) then
                WriteLn('Values differ in SDL_RendererFlags for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_RendererFlags));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_TextureAccess(ptr: IntPtr); cdecl; export;
   var
     _SDL_TextureAccess : SDL_TextureAccess;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_TextureAccess) then WriteLn('Different size for SDL_TextureAccess!');
     cptr += 1;
     
     for _SDL_TextureAccess := Low(SDL_TextureAccess) to High(SDL_TextureAccess) do
     begin
        try
            WriteStr(name, _SDL_TextureAccess);
            if cptr^ <> LongInt(_SDL_TextureAccess) then
                WriteLn('Values differ in SDL_TextureAccess for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_TextureAccess));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_TextureModulate(ptr: IntPtr); cdecl; export;
   var
     _SDL_TextureModulate : SDL_TextureModulate;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_TextureModulate) then WriteLn('Different size for SDL_TextureModulate!');
     cptr += 1;
     
     for _SDL_TextureModulate := Low(SDL_TextureModulate) to High(SDL_TextureModulate) do
     begin
        try
            WriteStr(name, _SDL_TextureModulate);
            if cptr^ <> LongInt(_SDL_TextureModulate) then
                WriteLn('Values differ in SDL_TextureModulate for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_TextureModulate));
            cptr += 1;
        except
        end;
     end;
   end;
    

   procedure PascalCheckEnum_SDL_GrabMode(ptr: IntPtr); cdecl; export;
   var
     _SDL_GrabMode : SDL_GrabMode;
     cptr: IntPtr;
     name: String;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(SDL_GrabMode) then WriteLn('Different size for SDL_GrabMode!');
     cptr += 1;
     
     for _SDL_GrabMode := Low(SDL_GrabMode) to High(SDL_GrabMode) do
     begin
        try
            WriteStr(name, _SDL_GrabMode);
            if cptr^ <> LongInt(_SDL_GrabMode) then
                WriteLn('Values differ in SDL_GrabMode for ', name, ' C Value: ', cptr^, ' Pas Value:', LongInt(_SDL_GrabMode));
            cptr += 1;
        except
        end;
     end;
   end;
    
exports
     PascalCheckEnum_SDL_bool,
     PascalCheckEnum_SDL_DUMMY_ENUM,
     PascalCheckEnum_SDL_assert_state,
     PascalCheckEnum_SDL_errorcode,
     PascalCheckEnum_SDL_ThreadPriority,
     PascalCheckEnum_SDL_AudioStatus,
     PascalCheckEnum_SDL_BlendMode,
     PascalCheckEnum_SDL_WindowFlags,
     PascalCheckEnum_SDL_WindowEventID,
     PascalCheckEnum_SDL_GLattr,
     PascalCheckEnum_SDL_Scancode,
     PascalCheckEnum_SDL_Keymod,
     PascalCheckEnum_SDL_EventType,
     PascalCheckEnum_SDL_eventaction,
     PascalCheckEnum_SDL_HintPriority,
     PascalCheckEnum_SDL_LogPriority,
     PascalCheckEnum_SDL_PowerState,
     PascalCheckEnum_SDL_RendererFlags,
     PascalCheckEnum_SDL_TextureAccess,
     PascalCheckEnum_SDL_TextureModulate,
     PascalCheckEnum_SDL_GrabMode;
end.