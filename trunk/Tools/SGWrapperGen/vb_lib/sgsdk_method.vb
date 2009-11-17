    <DllImport("sgsdk.dll", CallingConvention:=CallingConvention.Cdecl, EntryPoint:="%(name)s", CharSet:=CharSet.Ansi)> _
    Private Shared %(fn_type)sDLL_%(name)s(%(lib_params)s)%(fn_return)s
    End %(fn_type)s
    
    <System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()> _
    Public Shared %(fn_type)s%(name)s(%(params)s)%(fn_return)s
    {
        #If DEBUG Then
        try
        #End If
            %(pre_call)s%(the_call)s%(post_call)s
        #If DEBUG Then
        Catch exc As Exception
            Throw New SwinGameException(exc.Message)
        Finally
          If DLL_sg_Core_ExceptionOccured() <> 0 Then
              Dim sb As New StringBuilder(2048)
              DLL_sg_Core_ExceptionMessage(sb)
              Throw New SwinGameException(sb.ToString())
          End If
        End Try
        #End If
    End %(fn_type)s
    
