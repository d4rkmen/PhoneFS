unit uSerial;

{$I myglobal.inc}

interface

uses
        Windows,
        Sysutils,
        Classes,
        Messages,
        farlang;

const
        CM_COMPORT    = WM_USER + 1;
        EV_HANDLEMSG  = $2000;
        WRITE_TIMEOUT = 10000;
        // auxilary constants used not defined in windows.pas
        dcb_Binary           = $00000001;
        dcb_Parity           = $00000002;
        dcb_OutxCTSFlow      = $00000004;
        dcb_OutxDSRFlow      = $00000008;
        dcb_DTRControl       = $00000030;
        dcb_DSRSensivity     = $00000040;
        dcb_TxContinueOnXoff = $00000080;
        dcb_OutX             = $00000100;
        dcb_InX              = $00000200;
        dcb_ErrorChar        = $00000400;
        dcb_Null             = $00000800;
        dcb_RTSControl       = $00003000;
        dcb_AbortOnError     = $00004000;

        // error codes
        {
          CError_OpenFailed      = 1;
          CError_WriteFailed     = 2;
          CError_ReadFailed      = 3;
          CError_InvalidAsync    = 4;
          CError_PurgeFailed     = 5;
          CError_AsyncCheck      = 6;
          CError_SetStateFailed  = 7;
          CError_TimeoutsFailed  = 8;
          CError_SetupComFailed  = 9;
          CError_ClearComFailed  = 10;
          CError_ModemStatFailed = 11;
          CError_EscapeComFailed = 12;
          CError_TransmitFailed  = 13;
          CError_ConnChangeProp  = 14;
          CError_EnumPortsFailed = 15;
          CError_StoreFailed     = 16;
          CError_LoadFailed      = 17;
          CError_RegFailed       = 18;
          CError_LedStateFailed  = 19;
          CError_ThreadCreated   = 20;
          CError_WaitFailed      = 21;
          CError_HasLink         = 22;
          CError_RegError        = 23;
        }
type
        TPort = string;
        TBaudRate = integer;
        TStopBits = (sbOneStopBit, sbOne5StopBits, sbTwoStopBits);
        TDataBits = (dbFive, dbSix, dbSeven, dbEight);
        TParityBits = (prNone, prOdd, prEven, prMark, prSpace);
        TDTRFlowControl = (dtrDisable, dtrEnable, dtrHandshake);
        TRTSFlowControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);
        TFlowControl = (fcHardware, fcSoftware, fcNone, fcCustom);
        TComEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR, evError, evRLSD, evRx80Full);
        TComEvents = set of TComEvent;
        TComError = (ceFrame, ceRxParity, ceOverrun, ceBreak, ceIO, ceMode, ceRxOver, ceTxFull);
        TComErrors = set of TComError;
        TRxCharEvent = procedure(Sender: TObject; Count: integer) of object;
        TComErrorEvent = procedure(Sender: TObject; Errors: TComErrors) of object;
        TSyncMethod = (smThreadSync, smWindowSync, smNone);
        THandleMsgEvent = procedure(Sender: TObject; Msg: string);

        // exception class for ComPort Library errors
        EComPort = class(Exception)
                private
                        FWinCode: integer;
                        FCode: TMessageNumber;
                public
                        constructor Create(ACode: TMessageNumber; AWinCode: integer);
                        constructor CreateNoWinCode(ACode: TMessageNumber);
                        constructor CreateMsg(Msg: String; AWinCode: integer);
                        property WinCode: integer
                          read FWinCode
                          write FWinCode;
                        property Code: TMessageNumber
                          read FCode
                          write FCode;
        end;

        TSerial = class; // forward declaration

        TComThread = class(TThread)
                private
                        FComPort: TSerial;
                        FStopEvent: THandle;
                        FEvents: TComEvents;
                protected
                        procedure DispatchComMsg;
                        procedure DoEvents;
                        procedure Execute; override;
                        procedure SendEvents;
                        procedure Stop;
                public
                        constructor Create(AComPort: TSerial);
                        destructor Destroy; override;
        end;

        TComTimeouts = record
                ReadInterval: integer;
                ReadTotalMultiplier: integer;
                ReadTotalConstant: integer;
                WriteTotalMultiplier: integer;
                WriteTotalConstant: integer;
        end;

        TSerial = class(TObject)
                private
                        FEventThread: TComThread;
                        FThreadCreated: Boolean;
                        OvlR: TOverlapped;
                        OvlW: TOverlapped;
                        FPort: TPort;
                        FWindow: THandle;
                        FHandle: THandle;
                        FConnected: Boolean;
                        FWaitStr: string;
                        FMessage: string;
                        FLastCommand: string;
                        FBaudRate: TBaudRate;
                        FTimeouts: TComTimeouts;
                        FCustomBaudRate: integer;
                        FEventThreadPriority: TThreadPriority;
                        FEvents: TComEvents;
                        FSyncMethod: TSyncMethod;
                        FOnRxChar: TRxCharEvent;
                        FHandleMsg: THandleMsgEvent;
                        FControlDTR: TDTRFlowControl;
                        FControlRTS: TRTSFlowControl;
                        FDSRSensitivity: Boolean;
                        procedure WindowMethod(var Message: TMessage);
                        procedure SetControlDTR(const Value: TDTRFlowControl);
                        procedure SetControlRTS(const Value: TRTSFlowControl);
                        procedure SetPort(const Value: TPort);
                        procedure SetConnected(const Value: Boolean);
                        procedure SetBaudRate(const Value: TBaudRate);
                        procedure SetCustomBaudRate(const Value: integer);
                        procedure SetEventThreadPriority(const Value: TThreadPriority);
                        procedure SetSyncMethod(const Value: TSyncMethod);
                        procedure SetDSRSensitivity(const Value: Boolean);
                public
                        FWaiting: Boolean;
                        RxSize, TxSize: integer;
                        RxBuffer: TStrings;
                        // OBEX:TObject;
                        property Connected: Boolean
                          read FConnected
                          write SetConnected
                          default False;
                        property Port: TPort
                          read FPort
                          write SetPort;
                        property BaudRate: TBaudRate
                          read FBaudRate
                          write SetBaudRate;
                        property CustomBaudRate: integer
                          read FCustomBaudRate
                          write SetCustomBaudRate;
                        property Handle: THandle
                          read FHandle;
                        property EventThreadPriority: TThreadPriority
                          read FEventThreadPriority
                          write SetEventThreadPriority;
                        property Events: TComEvents
                          read FEvents
                          write FEvents;
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
                        property ControlDTR: TDTRFlowControl
                          read FControlDTR
                          write SetControlDTR;
                        property ControlRTS: TRTSFlowControl
                          read FControlRTS
                          write SetControlRTS;
                        property DSRSensitivity: Boolean
                          read FDSRSensitivity
                          write SetDSRSensitivity
                          default False;
                        constructor Create;
                        destructor Destroy; override;
                        procedure Open;
                        procedure Close;
                        procedure CreateHandle;
                        procedure SetupComPort;
                        procedure DestroyHandle;
                        procedure CallRxChar;
                        procedure DoRxChar(Count: integer);
                        function InputCount: integer;
                        function OutputCount: integer;
                        procedure ApplyDCB;
                        procedure ApplyTimeouts;
                        procedure ApplyBuffer;
                        function LastErrors: TComErrors;
                        procedure AbortAllAsync;
                        function Read(var Buffer; Count: integer): integer;
                        function ReadStr(var Str: AnsiString; Count: integer): integer;
                        function Write(const Buffer; Count: integer): integer;
                        function WriteStr(const Str: AnsiString): integer;
                        function TxAndWait(Data, WaitStr: AnsiString; TimeOut: Cardinal): Boolean;
                        procedure DoHandleMsg(Msg: AnsiString);
                        procedure SetDTR(OnOff: Boolean);
                        procedure SetRTS(OnOff: Boolean);
        end;

        //
function EventsToInt(const Events: TComEvents): integer;
function IntToEvents(Mask: integer): TComEvents;
procedure EnumComPorts(Ports: TStrings);

implementation

uses {D6DLLsync,}
        uDebug,
        uMain, {$IFDEF UNICODE}pluginw{$ELSE}plugin{$ENDIF};

(* ****************************************
  * EComPort exception                    *
  **************************************** *)

constructor EComPort.Create(ACode: TMessageNumber; AWinCode: integer);
begin
        FWinCode := AWinCode;
        FCode := ACode;
        inherited CreateFmt(GetMsg(MErrorFormat), [GetMsg(ACode), AWinCode, SysErrorMessage(AWinCode)]);
end;

// create exception with windows error code
constructor EComPort.CreateMsg(Msg: String; AWinCode: integer);
begin
        FWinCode := AWinCode;
        inherited CreateFmt(GetMsg(MErrorFormat), [Msg, AWinCode, SysErrorMessage(AWinCode)]);
end;

// create exception
constructor EComPort.CreateNoWinCode(ACode: TMessageNumber);
begin
        FWinCode := - 1;
        FCode := ACode;
        inherited Create(GetMsg(ACode));
end;

(* ****************************************
  * TComThread class                      *
  **************************************** *)
// create thread
constructor TComThread.Create(AComPort: TSerial);
begin
        inherited Create(True);
        FStopEvent := CreateEvent(nil, True, False, nil);
        FComPort := AComPort;
        // set thread priority
        Priority := FComPort.EventThreadPriority;
        // select which events are monitored
        SetCommMask(FComPort.Handle, EventsToInt(FComPort.Events));
        // execute thread
        Resume;
end;

// destroy thread
destructor TComThread.Destroy;
begin
        Stop;
        inherited Destroy;
end;

// thread action
procedure TComThread.Execute;
var
        EventHandles: array [0 .. 1] of THandle;
        Overlapped: TOverlapped;
        Signaled, BytesTrans, Mask: DWORD;
begin
        debug('[SERIAL] Thread started', 5);
        ZeroMemory(@Overlapped, SizeOf(Overlapped));
        Overlapped.hEvent := CreateEvent(nil, True, True, nil);
        EventHandles[0] := FStopEvent;
        EventHandles[1] := Overlapped.hEvent;
        repeat
                // wait for event to occur on serial port
                WaitCommEvent(FComPort.Handle, Mask, @Overlapped);
                Signaled := WaitForMultipleObjects(2, @EventHandles, False, INFINITE);
                // if event occurs, dispatch it
                if (Signaled = WAIT_OBJECT_0 + 1) and GetOverlappedResult(FComPort.Handle, Overlapped, BytesTrans, False)
                then begin
                        FEvents := IntToEvents(Mask);;
                        DispatchComMsg;
                end;
        until Signaled <> (WAIT_OBJECT_0 + 1);
        // clear buffers
        SetCommMask(FComPort.Handle, 0);
        PurgeComm(FComPort.Handle, PURGE_TXCLEAR or PURGE_RXCLEAR);
        CloseHandle(Overlapped.hEvent);
        CloseHandle(FStopEvent);
        debug('[SERIAL] Thread stopped!', 5);
end;

// stop thread
procedure TComThread.Stop;
begin
        SetEvent(FStopEvent);
        Sleep(0);
end;

// dispatch events
procedure TComThread.DispatchComMsg;
begin
{$IFDEF DEBUG_IO}
        debug('Dispatching...', 5);
{$ENDIF}
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

// send events to TSerial component using window message
procedure TComThread.SendEvents;
begin
        if evRxChar in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_RXCHAR, 0);
{$IFDEF 0}
        if evError in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_ERR, 0);
        if evTxEmpty in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_TXEMPTY, 0);
        if evBreak in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_BREAK, 0);
        if evRing in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_RING, 0);
        if evCTS in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_CTS, 0);
        if evDSR in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_DSR, 0);
        if evRxFlag in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_RXFLAG, 0);
        if evRing in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_RLSD, 0);
        if evRx80Full in FEvents then
                SendMessage(FComPort.FWindow, CM_COMPORT, EV_RX80FULL, 0);
{$ENDIF}
end;

// call events
procedure TComThread.DoEvents;
begin
        if evRxChar in FEvents then
                FComPort.CallRxChar;
{$IFDEF 0}
        if evError in FEvents then
                FComPort.CallError;
        if evTxEmpty in FEvents then
                FComPort.CallTxEmpty;
        if evBreak in FEvents then
                FComPort.CallBreak;
        if evRing in FEvents then
                FComPort.CallRing;
        if evCTS in FEvents then
                FComPort.CallCTSChange;
        if evDSR in FEvents then
                FComPort.CallDSRChange;
        if evRxFlag in FEvents then
                FComPort.CallRxFlag;
        if evRLSD in FEvents then
                FComPort.CallRLSDChange;
        if evRx80Full in FEvents then
                FComPort.CallRx80Full;
{$ENDIF}
end;

(* ****************************************
  * TSerial                               *
  **************************************** *)

constructor TSerial.Create;
begin
        inherited Create;
        // component cannot reside on inheritable forms
        FEventThreadPriority := tpNormal;
        FBaudRate := 9600;
        FCustomBaudRate := 9600;
        FPort := 'COM1';
        // FEvents := [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak,
        // evCTS, evDSR, evError, evRLSD, evRx80Full];
        FEvents := [evRxChar];
        FHandle := INVALID_HANDLE_VALUE;
        FSyncMethod := smNone;
        RxSize := 32768;
        TxSize := 32768;
        FWaiting := False;
        RxBuffer := TStringList.Create;
end;

destructor TSerial.Destroy;
begin
        Close;
        RxBuffer.Free;
        inherited Destroy;
end;

procedure TSerial.SetConnected(const Value: Boolean);
begin
        if Value <> FConnected then
                if Value then
                        Open
                else
                        Close;
end;

procedure TSerial.Open;
begin
        // if already connected, do nothing
        if not FConnected then begin
                // open port
                CreateHandle;
                FConnected := True;
                try
                        // initialize port
                        SetupComPort;
                except
                        // error occured during initialization, destroy handle
                        DestroyHandle;
                        FConnected := False;
                        raise;
                end;
                // create special thread to monitor port
                if (FEvents = []) then
                        FThreadCreated := False
                else begin
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
                        FWindow := AllocateHWnd(WindowMethod);
{$IFDEF DELPHI_6_OR_HIGHER}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
                        FEventThread := TComThread.Create(Self);
                        FThreadCreated := True;
                end;
        end;
end;

procedure TSerial.Close;
begin
        // if already closed, do nothing
        if FConnected then begin
                // abort all pending operations
                try
                        AbortAllAsync;
                except
                        // silent errors
                end;
                // stop monitoring for events
                if FThreadCreated then begin
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

procedure TSerial.WindowMethod(var Message: TMessage);
var
        AMsg: string;
begin
        with Message do
                if Msg = CM_COMPORT then
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
{$IFDEF 0}
                                                EV_TXEMPTY: CallTxEmpty;
                                                EV_BREAK: CallBreak;
                                                EV_RING: CallRing;
                                                EV_CTS: CallCTSChange;
                                                EV_DSR: CallDSRChange;
                                                EV_RXFLAG: CallRxFlag;
                                                EV_RLSD: CallRLSDChange;
                                                EV_ERR: CallError;
                                                EV_RX80FULL: CallRx80Full;
{$ENDIF}
                                        end
                        except
                                // silent errors
                        end
                else
                        Result := DefWindowProc(FWindow, Msg, wParam, LParam);
end;

// get number of bytes in input buffer
function TSerial.InputCount: integer;
var
        Errors: DWORD;
        ComStat: TComStat;
begin
        if not ClearCommError(FHandle, Errors, @ComStat) then
                raise EComPort.Create(CError_ClearComFailed, GetLastError);
        Result := ComStat.cbInQue;
end;

// get number of bytes in output buffer
function TSerial.OutputCount: integer;
var
        Errors: DWORD;
        ComStat: TComStat;
begin
        if not ClearCommError(FHandle, Errors, @ComStat) then
                raise EComPort.Create(CError_ClearComFailed, GetLastError);
        Result := ComStat.cbOutQue;
end;

// create handle to serial port
procedure TSerial.CreateHandle;
begin
        FHandle := CreateFile(PChar('\\.\' + FPort), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
          FILE_FLAG_OVERLAPPED, 0);
        if FHandle = INVALID_HANDLE_VALUE then
                raise EComPort.Create(CError_OpenFailed, GetLastError);
end;

// destroy serial port handle
procedure TSerial.DestroyHandle;
begin
        if FHandle <> INVALID_HANDLE_VALUE then
                CloseHandle(FHandle);
end;

// apply port properties
procedure TSerial.ApplyDCB;
const
        CParityBits: array [TParityBits] of integer = (NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
        CStopBits: array [TStopBits] of integer     = (ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
        // CBaudRate: array[TBaudRate] of Integer =
        // (0, CBR_110, CBR_300, CBR_600, CBR_1200, CBR_2400, CBR_4800, CBR_9600,
        // CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600, CBR_115200,
        // CBR_128000, CBR_256000, 230400, 460800, 921600);
        CDataBits: array [TDataBits] of integer         = (5, 6, 7, 8);
        CControlRTS: array [TRTSFlowControl] of integer = (RTS_CONTROL_DISABLE shl 12, RTS_CONTROL_ENABLE shl 12,
          RTS_CONTROL_HANDSHAKE shl 12, RTS_CONTROL_TOGGLE shl 12);
        CControlDTR: array [TDTRFlowControl] of integer = (DTR_CONTROL_DISABLE shl 4, DTR_CONTROL_ENABLE shl 4,
          DTR_CONTROL_HANDSHAKE shl 4);
var
        DCB: TDCB;

begin
        // if not connected or inside BeginUpdate/EndUpdate block, do nothing
        if FConnected then begin
                DCB.DCBlength := SizeOf(TDCB);
                DCB.XonLim := RxSize div 4;
                DCB.XoffLim := DCB.XonLim;
                DCB.EvtChar := Char(0);

                DCB.Flags := dcb_Binary;
                DCB.Flags := DCB.Flags or CControlDTR[ControlDTR] or CControlRTS[ControlRTS];
                if DSRSensitivity then
                        DCB.Flags := DCB.Flags or dcb_DSRSensivity;
                DCB.Parity := CParityBits[prNone];
                DCB.StopBits := CStopBits[sbOneStopBit];
                DCB.BaudRate := FBaudRate;
                DCB.ByteSize := CDataBits[dbEight];
                // apply settings
                if not SetCommState(FHandle, DCB) then
                        raise EComPort.Create(CError_SetStateFailed, GetLastError);
        end;
end;

// apply timeout properties
procedure TSerial.ApplyTimeouts;
var
        Timeouts: TCommTimeouts;

        function GetTOValue(const Value: integer): DWORD;
        begin
                if Value = - 1 then
                        Result := MAXDWORD
                else
                        Result := Value;
        end;

begin
        // if not connected or inside BeginUpdate/EndUpdate block, do nothing
        if FConnected then begin
                Timeouts.ReadIntervalTimeout := GetTOValue(FTimeouts.ReadInterval);
                Timeouts.ReadTotalTimeoutMultiplier := GetTOValue(FTimeouts.ReadTotalMultiplier);
                Timeouts.ReadTotalTimeoutConstant := GetTOValue(FTimeouts.ReadTotalConstant);
                Timeouts.WriteTotalTimeoutMultiplier := GetTOValue(FTimeouts.WriteTotalMultiplier);
                Timeouts.WriteTotalTimeoutConstant := GetTOValue(FTimeouts.WriteTotalConstant);
                // apply settings
                if not SetCommTimeouts(FHandle, Timeouts) then
                        raise EComPort.Create(CError_TimeoutsFailed, GetLastError);
        end;
end;

// apply buffers
procedure TSerial.ApplyBuffer;
begin
        if FConnected then
                if not SetupComm(FHandle, RxSize, TxSize) then
                        raise EComPort.Create(CError_SetupComFailed, GetLastError);
end;

// initialize port
procedure TSerial.SetupComPort;
begin
        ApplyBuffer;
        ApplyDCB;
        ApplyTimeouts;
end;

procedure TSerial.CallRxChar;
var
        i, Count: integer;
        Buffer: AnsiString;
        c: AnsiChar;
begin
        Count := InputCount;
{$IFDEF DEBUG_IO}
        debug(format('[SERIAL] OnRxChar [%d]...', [Count]), 5);
{$ENDIF}
        if FWaiting then begin
                debug('[SERIAL] Waiting for: ' + FWaitStr, 5);
                ReadStr(Buffer, Count);
{$IFDEF DEBUG_IO}
                debug('>> ' + Buffer, 5);
{$ENDIF}
                for i := 1 to length(Buffer) do begin
                        c := Buffer[i];
                        case c of
                                #00:;
                                #10:;
                                #13: begin
                                        if length(trim(FMessage)) > 0 then begin
                                                RxBuffer.Add(FMessage);
                                                DoHandleMsg(FMessage);
                                        end;
                                        FMessage := '';
                                end;
                                else begin
                                        FMessage := FMessage + c;
                                end;
                        end;
                        if (FMessage = FWaitStr) and (FMessage <> '') then begin
                                debug('[SERIAL] Reached wait string: ' + FWaitStr, 5);
                                FWaiting := False;
                                FWaitStr := '';
                        end;
                end;
        end // FWaiting
        else begin
                if Count > 0 then
                        DoRxChar(Count);
        end;
end;

procedure TSerial.DoHandleMsg(Msg: AnsiString);
begin
        if Assigned(FHandleMsg) then
                FHandleMsg(Self, Msg);
end;

// return last errors on port
function TSerial.LastErrors: TComErrors;
var
        Errors: DWORD;
        ComStat: TComStat;
begin
        if not ClearCommError(FHandle, Errors, @ComStat) then
                raise EComPort.Create(CError_ClearComFailed, GetLastError);
        Result := [];

        if (CE_FRAME and Errors) <> 0 then
                Result := Result + [ceFrame];
        if ((CE_RXPARITY and Errors) <> 0) then
                Result := Result + [ceRxParity];
        if (CE_OVERRUN and Errors) <> 0 then
                Result := Result + [ceOverrun];
        if (CE_RXOVER and Errors) <> 0 then
                Result := Result + [ceRxOver];
        if (CE_TXFULL and Errors) <> 0 then
                Result := Result + [ceTxFull];
        if (CE_BREAK and Errors) <> 0 then
                Result := Result + [ceBreak];
        if (CE_IOE and Errors) <> 0 then
                Result := Result + [ceIO];
        if (CE_MODE and Errors) <> 0 then
                Result := Result + [ceMode];
end;

// sets event thread priority
procedure TSerial.SetEventThreadPriority(const Value: TThreadPriority);
begin
        if Value <> FEventThreadPriority then begin
                if FConnected then
                        raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
                else
                        FEventThreadPriority := Value;
        end;
end;

// set event synchronization method
procedure TSerial.SetSyncMethod(const Value: TSyncMethod);
begin
        if Value <> FSyncMethod then begin
                if FConnected then
                        raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
                else
                        FSyncMethod := Value;
        end;
end;

procedure TSerial.SetBaudRate(const Value: TBaudRate);
begin
        if Value <> FBaudRate then begin
                FBaudRate := Value;
                // if possible, apply settings
                ApplyDCB;
        end;
end;

// set custom baud rate
procedure TSerial.SetCustomBaudRate(const Value: integer);
begin
        if Value <> FCustomBaudRate then begin
                FCustomBaudRate := Value;
                ApplyDCB;
        end;
end;

// set port
procedure TSerial.SetPort(const Value: TPort);
begin
        if Value <> FPort then begin
                FPort := Value;
                if FConnected then begin
                        Close;
                        Open;
                end;
        end;
end;

// set DTR signal
procedure TSerial.SetDTR(OnOff: Boolean);
var
        Act: DWORD;
begin
        if OnOff then
                Act := Windows.SetDTR
        else
                Act := Windows.CLRDTR;

        if not EscapeCommFunction(FHandle, Act) then
                raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

// set RTS signals
procedure TSerial.SetRTS(OnOff: Boolean);
var
        Act: DWORD;
begin
        if OnOff then
                Act := Windows.SetRTS
        else
                Act := Windows.CLRRTS;

        if not EscapeCommFunction(FHandle, Act) then
                raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

// set input flow control for DTR (data-terminal-ready)
procedure TSerial.SetControlDTR(const Value: TDTRFlowControl);
begin
        if Value <> FControlDTR then begin
                FControlDTR := Value;
                ApplyDCB;
        end;
end;

// set input flow control for RTS (request-to-send)
procedure TSerial.SetControlRTS(const Value: TRTSFlowControl);
begin
        if Value <> FControlRTS then begin
                FControlRTS := Value;
                ApplyDCB;
        end;
end;

// set DSR sensitivity
procedure TSerial.SetDSRSensitivity(const Value: Boolean);
begin
        if Value <> FDSRSensitivity then begin
                FDSRSensitivity := Value;
                ApplyDCB;
        end;
end;

// abort all asynchronous operations
procedure TSerial.AbortAllAsync;
begin
        if not PurgeComm(FHandle, PURGE_TXABORT or PURGE_RXABORT) then
                raise EComPort.Create(CError_PurgeFailed, GetLastError);
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
function TSerial.Read(var Buffer; Count: integer): integer;
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

function TSerial.ReadStr(var Str: AnsiString; Count: integer): integer;
begin
        SetLength(Str, Count);
        if Count > 0 then
                Result := Read(Str[1], Count)
        else
                Result := 0;
end;

// perform synchronous write operation
function TSerial.Write(const Buffer; Count: integer): integer;
var
        Success: Boolean;
        Signaled, BytesTrans: DWORD;
begin
        InitAsync(OvlW);
        try
                Success := WriteFile(FHandle, Buffer, Count, BytesTrans, @OvlW) or (GetLastError = ERROR_IO_PENDING);
                if not Success then
                        raise EComPort.Create(CError_WriteFailed, GetLastError);
                Signaled := WaitForSingleObject(OvlW.hEvent, WRITE_TIMEOUT);
                Success := (Signaled = WAIT_OBJECT_0) and (GetOverlappedResult(FHandle, OvlW, BytesTrans, False));
                if not Success then
                        raise EComPort.Create(CError_WriteFailed, GetLastError);
                Result := BytesTrans;
        finally
                DoneAsync(OvlW);
        end;
end;

// perform synchronous write operation
function TSerial.WriteStr(const Str: AnsiString): integer;
begin
        if length(Str) > 0 then begin
                Result := Write(Str[1], length(Str))
        end
        else
                Result := 0;
end;

// converts TComEvents type to Integer
function EventsToInt(const Events: TComEvents): integer;
begin
        Result := 0;
        if evRxChar in Events then
                Result := Result or EV_RXCHAR;
        if evRxFlag in Events then
                Result := Result or EV_RXFLAG;
        if evTxEmpty in Events then
                Result := Result or EV_TXEMPTY;
        if evRing in Events then
                Result := Result or EV_RING;
        if evCTS in Events then
                Result := Result or EV_CTS;
        if evDSR in Events then
                Result := Result or EV_DSR;
        if evRLSD in Events then
                Result := Result or EV_RLSD;
        if evError in Events then
                Result := Result or EV_ERR;
        if evBreak in Events then
                Result := Result or EV_BREAK;
        if evRx80Full in Events then
                Result := Result or EV_RX80FULL;
end;

function IntToEvents(Mask: integer): TComEvents;
begin
        Result := [];
        if (EV_RXCHAR and Mask) <> 0 then
                Result := Result + [evRxChar];
        if (EV_TXEMPTY and Mask) <> 0 then
                Result := Result + [evTxEmpty];
        if (EV_BREAK and Mask) <> 0 then
                Result := Result + [evBreak];
        if (EV_RING and Mask) <> 0 then
                Result := Result + [evRing];
        if (EV_CTS and Mask) <> 0 then
                Result := Result + [evCTS];
        if (EV_DSR and Mask) <> 0 then
                Result := Result + [evDSR];
        if (EV_RXFLAG and Mask) <> 0 then
                Result := Result + [evRxFlag];
        if (EV_RLSD and Mask) <> 0 then
                Result := Result + [evRLSD];
        if (EV_ERR and Mask) <> 0 then
                Result := Result + [evError];
        if (EV_RX80FULL and Mask) <> 0 then
                Result := Result + [evRx80Full];
end;

procedure TSerial.DoRxChar(Count: integer);
begin
        if Assigned(FOnRxChar) then begin
                FOnRxChar(Self, Count);
        end;
end;

function ComparePortNames(List: TStringList; Index1, Index2: integer): integer;
var
        PortNum1, PortNum2: integer;
begin
        // Extract numeric part of port names
        PortNum1 := StrToIntDef(Copy(List[Index1], 4, MaxInt), 0);
        PortNum2 := StrToIntDef(Copy(List[Index2], 4, MaxInt), 0);

        // Compare numeric parts
        Result := PortNum1 - PortNum2;

        // If numeric parts are equal, compare entire strings
        if Result = 0 then
                Result := CompareStr(List[Index1], List[Index2]);
end;

procedure EnumComPorts(Ports: TStrings);
var
        KeyHandle: HKEY;
        ErrCode, Index: integer;
        ValueName: array [0 .. MAX_PATH] of Char;
        Data: array [0 .. MAX_PATH] of Char;
        ValueLen, DataLen, ValueType: Cardinal;
        TmpPorts: TStringList;
begin
        ErrCode := RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'HARDWARE\DEVICEMAP\SERIALCOMM', 0, KEY_READ, KeyHandle);

        if ErrCode <> ERROR_SUCCESS then begin
                raise EComPort.Create(CError_EnumPortsFailed, ErrCode);
                Ports.Clear;
                Exit;
        end;

        TmpPorts := TStringList.Create;
        try
                Index := 0;
                repeat
                        ValueLen := MAX_PATH;
                        DataLen := SizeOf(Data);
                        ErrCode := RegEnumValue(KeyHandle, Index, @ValueName, ValueLen, nil, @ValueType, nil, @DataLen);
                        if ErrCode = ERROR_SUCCESS then begin
                                ValueLen := MAX_PATH;
                                ZeroMemory(@Data, SizeOf(Data));
                                ErrCode := RegEnumValue(KeyHandle, Index, ValueName, ValueLen, nil, @ValueType, @Data,
                                  @DataLen);
                                if ErrCode = ERROR_SUCCESS then begin
                                        TmpPorts.Add(StrPas(PChar(@Data)));
                                        Inc(Index);
                                end;
                        end
                        else if ErrCode <> ERROR_NO_MORE_ITEMS then
                                raise EComPort.Create(CError_EnumPortsFailed, ErrCode);

                until (ErrCode <> ERROR_SUCCESS);

                TmpPorts.CustomSort(ComparePortNames);
                Ports.Assign(TmpPorts);
        finally
                RegCloseKey(KeyHandle);
                TmpPorts.Free;
        end;

end;

function TSerial.TxAndWait(Data, WaitStr: AnsiString; TimeOut: Cardinal): Boolean;
var
        StartTime: DWORD;
begin
{$IFDEF DEBUG_IO}
        debug('<< ' + Data, 5);
{$ENDIF}
        FLastCommand := Data;
        RxBuffer.Clear;
        if WaitStr = '' then begin
                WriteStr(Data + AnsiChar(#13));
                Result := True;
        end
        else begin
                FWaiting := True;
                try
                        FWaitStr := WaitStr;
                        StartTime := GetTickCount;
                        WriteStr(Data + AnsiChar(#13));
                        while FWaiting do begin
                                Sleep(5); // give some CPU time for other threads
                                if (GetTickCount - StartTime) >= TimeOut then
                                        break;
                        end;
                        Result := FWaitStr = '';
                finally
                        FWaiting := False;
                end;
        end;
end;

end.
