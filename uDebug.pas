unit uDebug;

{$I myglobal.inc}

interface

uses
        Windows,
        SysUtils,
        Messages,
        Classes;

const
        CM_DEBUG   = WM_USER + 55;
        CM_QUIT    = WM_USER + 56;
        PhoneFSDir = 'PhoneFS';
        LogExt     = 'log';
        // msg types
        msgtype_connect           = 1;
        msgtype_disconnect        = 2;
        msgtype_details           = 3;
        msgtype_transfercomplete  = 4;
        msgtype_connectcomplete   = 5;
        msgtype_importanterror    = 6;
        msgtype_operationcomplete = 7;

type
        TDebug = class // (TObject)
                private
                        FName: string;
{$IFNDEF POST}
                        procedure WindowMethod(var Message: TMessage);
{$ENDIF}
                        procedure DoHandleMsg(Msg: string; Level: integer);
                public
                        dbgThread: thandle;
                        dbgThreadID: dword;
{$IFNDEF POST}
                        FWindow: thandle;
{$ENDIF}
                        constructor Create(name: string);
                        destructor Destroy; override;
                        procedure Add(Msg: string; Level: integer);
        end;

var
        dbg: TDebug;

procedure dbgMessageLoop(param: pointer); cdecl;

procedure debug(Str: string; Msgtype: integer);

implementation

var
        aName: string;
        f: text;

constructor TDebug.Create(name: string);
{$IFDEF DEBUG}
var
        f: text;
{$ENDIF}
begin
        inherited Create;
        FName := name;
        aName := name;
{$IFDEF POST}
        dbgThread := CreateThread(nil, 8192, @dbgMessageLoop, nil, 0, dbgThreadID);
{$ENDIF}
{$IFDEF DEBUG}
        // ! creating file log
        Assign(f, FName);
        Rewrite(f);
        Writeln(f, format('%s {%.4x} [LOG] debug logging started. ThreadId: 0x%x',
          [DateTimeToStr(Now), GetCurrentThreadId, dbgThreadID]));
        Close(f);
{$ENDIF}
end;

destructor TDebug.Destroy;
begin
{$IFDEF POST}
        PostThreadMessage(dbgThread, WM_QUIT, 0, 0);
{$ELSE}
        TerminateThread(dbgThread, 0);
{$ENDIF}
        inherited;
end;

{$IFNDEF POST}

procedure TDebug.WindowMethod(var Message: TMessage);
var
        AMsg: string;
begin
        with Message do
                if Msg = CM_DEBUG then
                        try
                                if WParam <> 0 then begin
                                        AMsg := StrPas(PChar(WParam));
                                        // StrDispose(PChar(WParam));
                                        DoHandleMsg(AMsg, LParam);
                                end;
                        except
                                // Application.HandleException(Self);
                        end
                else
                        Result := DefWindowProc(FWindow, Msg, WParam, LParam);
end;
{$ENDIF}

procedure TDebug.DoHandleMsg(Msg: string; Level: integer);
{$IFDEF DEBUG}
var
        f: text;
{$ENDIF}
begin
{$IFDEF DEBUG}
        Assign(f, FName);
        Append(f);
        Writeln(f, DateTimeToStr(Now) + ' ' + Msg);
        Close(f);
{$ENDIF}
end;

procedure TDebug.Add(Msg: string; Level: integer);
begin
{$IFDEF POST}
        PostThreadMessage(Self.dbgThreadID, CM_DEBUG, NativeUint(StrNew(PChar(Msg))), Level);
{$ELSE}
        SendMessage(FWindow, CM_DEBUG, integer(PChar(Msg)), Level);
{$ENDIF}
end;

procedure debug(Str: string; Msgtype: integer);
begin
{$IFDEF DEBUG}
        // debugger message
        OutputDebugString(PChar(format('{%.4x} PhoneFS # %s', [GetCurrentThreadId, Str])));
        // file message
        if Assigned(dbg) then
                dbg.Add(format('{%.4x} %s'#00#00, [GetCurrentThreadId, Str]), Msgtype);
{$ENDIF}
end;

procedure dbgMessageLoop(param: pointer); cdecl;
var
        Msg: tagMsg;
{$IFDEF POST}
        AMsg: string;
{$ENDIF}
begin
        if not Assigned(dbg) then
                exit;
{$IFNDEF POST}
        dbg.FWindow := AllocateHWnd(dbg.WindowMethod);
{$ENDIF}
        while true do
                try
                        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
{$IFDEF POST}
                                case Msg.Message of
                                        CM_DEBUG:
                                        if Assigned(dbg) then begin
                                                if Assigned(PChar(Msg.WParam)) then begin
                                                        AMsg := StrPas(PChar(Msg.WParam));
                                                        dbg.DoHandleMsg(AMsg, Msg.LParam);
                                                        StrDispose(PChar(Msg.WParam));
                                                end;
                                        end;
                                        WM_QUIT: break;
                                end;
{$ENDIF}
                        end
                        else
                                WaitMessage;
                except
                        // silent errors
                        dbg.DoHandleMsg('Debug system error', 0);
                end;
end;

function GetEnvironmentString(name: string): string;
var
        size: integer;
begin
        size := MAX_PATH;
        SetLength(Result, size);
        size := ExpandEnvironmentStrings(PChar(format('%%%s%%', [name])), PChar(Result), size);
        if size > 0 then
                SetLength(Result, size)
        else
                Result := '';
        Result := Trim(Result);
end;

{$IFDEF DEBUG}

var
        AppData: string;

initialization

AppData := GetEnvironmentString('APPDATA') + '\' + PhoneFSDir;
if not DirectoryExists(AppData) then
        CreateDir(AppData);
dbg := TDebug.Create(format('%s\%s.%s', [AppData, FormatDateTime('yyyy-mm-dd hh-mm-ss', Now), LogExt]));

finalization

dbg.Free;
if FileExists(aName) then begin
        Assign(f, aName);
        Append(f);
        Writeln(f, format('%s {%.4x} [LOG] debug logging finished', [DateTimeToStr(Now), GetCurrentThreadId]));
        Close(f);
end;
{$ENDIF}

end.
