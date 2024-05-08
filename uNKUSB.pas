unit uNKUSB;

{ I myglobal.inc }

interface

uses
        Windows,
        Sysutils,
        Classes,
        Messages;

const
        CM_NKUSB = WM_USER + 2;

type
        TPort = string;
        TRxCharEvent = procedure(Sender: TObject; Count: Integer) of object;
        TSyncMethod = (smThreadSync, smWindowSync, smNone);
        THandleMsgEvent = procedure(Sender: TObject; Msg: string);
        // exception class for ComPort Library errors

        TNKUSB = class; // forward declaration

        TNKUSBThread = class(TThread)
                private
                        FComPort: TNKUSB;
                        FStopEvent: THandle;
                protected
                        procedure DispatchComMsg;
                        procedure DoEvents;
                        procedure Execute; override;
                        procedure SendEvents;
                public
                        constructor Create(AComPort: TNKUSB);
                        destructor Destroy; override;
        end;

        TNKUSB = class(TObject)
                private
                        FEventThread: TNKUSBThread;
                        FThreadCreated: Boolean;
                        OvlR: TOverlapped;
                        OvlW: TOverlapped;
                        FPort: TPort;
                        FWindow: THandle;
                        FHandle: THandle;
                        FConnected: Boolean;
                        FMessage: string;
                        FLastCommand: string;
                        FEventThreadPriority: TThreadPriority;
                        FSyncMethod: TSyncMethod;
                        FOnRxChar: TRxCharEvent;
                        FHandleMsg: THandleMsgEvent;
                        procedure WindowMethod(var Message: TMessage);
                        procedure SetPort(const Value: TPort);
                        procedure SetConnected(const Value: Boolean);
                        procedure SetEventThreadPriority(const Value: TThreadPriority);
                        procedure SetSyncMethod(const Value: TSyncMethod);
                public
                        InputCount: Integer;
                        Buffer: AnsiString;
                        FStopEvent: THandle;
                        property Connected: Boolean
                          read FConnected
                          write SetConnected
                          default False;
                        property Port: TPort
                          read FPort
                          write SetPort;
                        property Handle: THandle
                          read FHandle;
                        property EventThreadPriority: TThreadPriority
                          read FEventThreadPriority
                          write SetEventThreadPriority;
                        property SyncMethod: TSyncMethod
                          read FSyncMethod
                          write SetSyncMethod
                          default smThreadSync;
                        property OnRxChar: TRxCharEvent
                          read FOnRxChar
                          write FOnRxChar;
                        property OnHandleMsg: THandleMsgEvent
                          read FHandleMsg
                          write FHandleMsg;
                        constructor Create;
                        destructor Destroy; override;
                        procedure Open;
                        procedure Close;
                        procedure CreateHandle;
                        procedure DestroyHandle;
                        procedure CallRxChar;
                        procedure DoRxChar(Count: Integer);
                        function Read(var Buffer; Count: Integer): Integer;
                        function ReadStr(var Str: AnsiString; Count: Integer): Integer;
                        function Write(const Buffer; Count: Integer): Integer;
                        function WriteStr(const Str: AnsiString): Integer;
                        procedure DoHandleMsg(Msg: string);
        end;

implementation

uses
        uDebug,
        uMain,
        uSerial,
        farlang;

(* ****************************************
  * TNKUSBThread class                      *
  **************************************** *)
// create thread
constructor TNKUSBThread.Create(AComPort: TNKUSB);
begin
        inherited Create(True);
        FComPort := AComPort;
        // set thread priority
        Priority := FComPort.EventThreadPriority;
        // execute thread
        Resume;
end;

// destroy thread
destructor TNKUSBThread.Destroy;
begin
        inherited Destroy;
end;

// thread action
procedure TNKUSBThread.Execute;
begin
        debug('[USB] Thread started', 5);
        repeat
                FComPort.ReadStr(FComPort.Buffer, $FFFF);
                // debug(format('port: 0x%x  sig: 0x%x',[FComPort.Handle, Signaled]),5);
                FComPort.InputCount := length(FComPort.Buffer);
                DispatchComMsg;
        until False;
        debug('[USB] Thread stopped!', 5);
end;

// dispatch events
procedure TNKUSBThread.DispatchComMsg;
begin
        case FComPort.SyncMethod of
                smThreadSync: begin
                        // debug('ThreadSync...',5);
                        Synchronize(DoEvents); // call events in main thread
                end;
                smWindowSync: begin
                        // debug('WindowSync...',5);
                        SendEvents; // call events in thread that opened the port
                end;
                smNone: begin
                        // debug('NoSync...',5);
                        DoEvents; // call events inside monitoring thread
                end;
        end;
end;

// send events to TKNUSB component using window message
procedure TNKUSBThread.SendEvents;
begin
        SendMessage(FComPort.FWindow, CM_NKUSB, EV_RXCHAR, 0);
end;

// call events
procedure TNKUSBThread.DoEvents;
begin
        FComPort.CallRxChar;
end;

(* ****************************************
  * TKNUSB                               *
  **************************************** *)

constructor TNKUSB.Create;
begin
        inherited Create;
        // component cannot reside on inheritable forms
        FEventThreadPriority := tpNormal;
        FPort := '';
        FHandle := INVALID_HANDLE_VALUE;
        FSyncMethod := smNone; // smNone;//smWindowSync;//smThreadSync;
end;

destructor TNKUSB.Destroy;
begin
        Close;
        inherited Destroy;
end;

procedure TNKUSB.SetConnected(const Value: Boolean);
begin
        if Value <> FConnected then
                if Value then
                        Open
                else
                        Close;
end;

procedure TNKUSB.Open;
begin
        // if already connected, do nothing
        if not FConnected then begin
                // open port
                CreateHandle;
                FConnected := True;
                try
                        // initialize port
                        // SetupComPort;
                except
                        // error occured during initialization, destroy handle
                        DestroyHandle;
                        FConnected := False;
                        raise;
                end;
                // create special thread to monitor port
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
                FWindow := AllocateHWnd(WindowMethod);
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
                FEventThread := TNKUSBThread.Create(Self);
                FThreadCreated := True;
        end;
end;

procedure TNKUSB.Close;
begin
        // if already closed, do nothing
        if FConnected then begin
                // stop monitoring for events
                if FThreadCreated then begin
                        TerminateThread(FEventThread.Handle, 0);
                        FEventThread.Free;
                        FThreadCreated := False;
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
                        DeallocateHWnd(FWindow);
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
                end;
                // close port
                DestroyHandle;
                FConnected := False;
        end;
end;

procedure TNKUSB.WindowMethod(var Message: TMessage);
var
        AMsg: string;
begin
        with Message do
                if Msg = CM_NKUSB then
                        try
                                if InSendMessage then
                                        ReplyMessage(0);
                                if FConnected then
                                        case wParam of
                                                EV_RXCHAR: CallRxChar;
                                                EV_HANDLEMSG: begin
                                                        if LParam <> 0 then begin
                                                                AMsg := StrPas(PChar(LParam));
                                                                DoHandleMsg(AMsg);
                                                                StrDispose(PChar(LParam));
                                                        end;
                                                end;
                                        end
                        except
                        end
                else
                        Result := DefWindowProc(FWindow, Msg, wParam, LParam);
end;

// create handle to serial port
procedure TNKUSB.CreateHandle;
begin
        debug('[USB] CreateHandle: ' + FPort, 5);
        FHandle := CreateFile(PChar(FPort), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
          FILE_FLAG_OVERLAPPED, 0);
        if FHandle = INVALID_HANDLE_VALUE then
                raise EComPort.Create(CError_OpenFailed, GetLastError);
end;

// destroy serial port handle
procedure TNKUSB.DestroyHandle;
begin
        if FHandle <> INVALID_HANDLE_VALUE then
                CloseHandle(FHandle);
end;

procedure TNKUSB.CallRxChar;
var
        Count: Integer;
begin
        Count := InputCount;
        debug(format('[USB] OnRxChar [%d]...', [Count]), 5);
        if Count > 0 then
                DoRxChar(Count);
end;

procedure TNKUSB.DoHandleMsg(Msg: string);
begin
        if Assigned(FHandleMsg) then
                FHandleMsg(Self, Msg);
end;

// sets event thread priority
procedure TNKUSB.SetEventThreadPriority(const Value: TThreadPriority);
begin
        if Value <> FEventThreadPriority then begin
                if FConnected then
                        raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
                else
                        FEventThreadPriority := Value;
        end;
end;

// set event synchronization method
procedure TNKUSB.SetSyncMethod(const Value: TSyncMethod);
begin
        if Value <> FSyncMethod then begin
                if FConnected then
                        raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
                else
                        FSyncMethod := Value;
        end;
end;

// set port
procedure TNKUSB.SetPort(const Value: TPort);
begin
        if Value <> FPort then begin
                FPort := Value;
                if FConnected then begin
                        Close;
                        Open;
                end;
        end;
end;

// initialization of PAsync variables used in asynchronous calls
procedure InitAsync(var Overlapped: TOverlapped);
begin
        ZeroMemory(@Overlapped, SizeOf(TOverlapped));
        Overlapped.hEvent := CreateEvent(nil, True, True, nil);
end;

// clean-up of PAsync variable
procedure DoneAsync(Overlapped: TOverlapped);
begin
        CloseHandle(Overlapped.hEvent);
end;

// perform asynchronous read operation
function TNKUSB.Read(var Buffer; Count: Integer): Integer;
var
        Success: Boolean;
        Signaled, BytesTrans: DWORD;
begin
        InitAsync(OvlR);
        try
                Success := ReadFile(FHandle, Buffer, Count, BytesTrans, @OvlR) or (GetLastError = ERROR_IO_PENDING);
                if not Success then
                        raise EComPort.Create(CError_ReadFailed, GetLastError);
                Signaled := WaitForSingleObject(OvlR.hEvent, INFINITE);
                Success := (Signaled = WAIT_OBJECT_0) and (GetOverlappedResult(FHandle, OvlR, BytesTrans, False));
                if not Success then
                        raise EComPort.Create(CError_ReadFailed, GetLastError);
                Result := BytesTrans;
        finally
                DoneAsync(OvlR);
        end;
end;

function TNKUSB.ReadStr(var Str: AnsiString; Count: Integer): Integer;
var
        buf: string;
begin
        SetLength(buf, Count);
        if Count > 0 then begin
                Result := Read(buf[1], Count);
                Str := copy(buf, 1, Result);
        end
        else
                Result := 0;
end;

// perform synchronous write operation
function TNKUSB.Write(const Buffer; Count: Integer): Integer;
var
        Success: Boolean;
        Signaled, BytesTrans: DWORD;
begin
        InitAsync(OvlW);
        try
                Success := WriteFile(FHandle, Buffer, Count, BytesTrans, @OvlW) or (GetLastError = ERROR_IO_PENDING);
                if not Success then
                        raise EComPort.Create(CError_WriteFailed, GetLastError);
                Signaled := WaitForSingleObject(OvlW.hEvent, INFINITE);
                Success := (Signaled = WAIT_OBJECT_0) and (GetOverlappedResult(FHandle, OvlW, BytesTrans, False));
                if not Success then
                        raise EComPort.Create(CError_WriteFailed, GetLastError);
                Result := BytesTrans;
        finally
                DoneAsync(OvlW);
        end;
end;

// perform synchronous write operation
function TNKUSB.WriteStr(const Str: AnsiString): Integer;
begin
        if length(Str) > 0 then
                Result := Write(Str[1], length(Str))
        else
                Result := 0;
end;

procedure TNKUSB.DoRxChar(Count: Integer);
begin
        if Assigned(FOnRxChar) then begin
                FOnRxChar(Self, Count);
        end;
end;

end.
