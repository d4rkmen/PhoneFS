unit uMain;

{$I myglobal.inc}

interface

uses
        Windows,
        SysUtils,
        Classes,
        System.Generics.Collections,
        uSerial,
        uObex,
        PluginW,
        PluginSettings,
        rpVersionInfo,
        Xml.Win.msxmldom,
        uNKUSB;

const
        MAX_JAVA      = 300;
        ATCMD_TIMEOUT = 2000;
        AT_OK         = 'OK';
        //
        SE_OBEX_GUID: TGUID          = '{757BA907-A2B3-48DA-8A95-2D6CED294126}';
        SERIAL_GUID: TGUID           = '{4D36E978-E325-11CE-BFC1-08002BE10318}';
        MODEM_GUID: TGUID            = '{2C7089AA-2E0E-11D1-B114-00C04FC2AAE4}';
        NOKIA_OBEX_GUID: TGUID       = '{4f919100-4adf-11d5-882d-00b0d02fe381}';
        BLUETOOTH_GUID: TGUID        = '{E0CBF06C-CD8B-4647-BB8A-263B43F0F974}';
        BTMODEM_GUID: TGUID          = '{CB3A4004-46F0-11D0-B08F-00609713053F}';
        MAIN_GUID: TGUID             = '{D692CC9D-2327-4778-8539-41248584DBA4}';
        MENU_GUID: TGUID             = '{2AFE63E4-E968-4DA6-B349-BC15C225E4EC}';
        WELCOME_DLG_GUID: TGUID      = '{6561837B-1DDA-4E06-ABFF-2F9C9A5B1160}';
        CONFIGURE_DLG_GUID: TGUID    = '{9728F9D1-69C0-4F18-A2D5-125DC2C7DB62}';
        PROGRESS_MSG_GUID: TGUID     = '{84552AC3-C4E7-4E38-89BC-E49953DB2430}';
        CONFIRM_EXIT_MSG_GUID: TGUID = '{622A16FF-4403-4AF5-A2F8-373464FD95BC}';
        CHECK_ABORT_MSG_GUID: TGUID  = '{15BCFEE3-7633-417E-A54C-37658542A529}';
        GET_FILES_DLG_GUID: TGUID    = '{D8815FAC-04F8-4D0E-B33B-32D423CE0474}';
        PUT_FILES_DLG_GUID: TGUID    = '{D83A258B-FA53-462E-AA64-20E09073D027}';
        MAKE_DIR_DLG_GUID: TGUID     = '{2F0468E6-E551-4541-8DC7-15B2C9AE4D70}';
        DELETE_FILES_DLG_GUID: TGUID = '{4E0D8136-010C-46E1-B9CA-68196D7347D1}';
        CONNECT_MSG_GUID: TGUID      = '{E92E7E8E-7238-4D7F-B7D7-83D55FF15F9C}';
        // device type
        DEVTYPE_USB_SONYERICSSON = 0;
        DEVTYPE_USB_NOKIA        = 1;
        DEVTYPE_SERIAL_PORT      = 2;
        DEVTYPE_MODEM_PORT       = 3;
        DEVTYPE_COM_PORT         = 4;
        //
        PARENT_FOLDER = '..';
        ROOT_FOLDER   = '\';
        // progress bar length
        prLen        = 44;
        URL_HOMEPAGE = 'www.unlock.se';

type
        TPortName = record
                Port: string;
                Name: string;
        end;

        TEJava = record
                Name: string;
                Ver: string;
                AppID: integer;
                ID: integer;
        end;

        TPanelItems = array of TPluginPanelItem;
        PPanelItems = ^TPanelItems;

        // options
        TMainOpt = record
                AddToDisksMenu, AddToPluginsMenu: boolean;
        end;

        PMainOpt = ^TMainOpt;

        TOpt = record
                ComPort: string;
                BaudRate: integer;
                OBEXPS: integer;
                DTR, RTS, Log, UseWMC: integer;
                DevType: integer;
                DevName, DevTitle: string;
                // local context vars
                Ports: TStrings;
                Names: TStrings;
                Port, Name, OldPort: string;
                // ports list
                PortsListItem, PortItem: NativeInt;
                // speed combo
                SpeedCombo: NativeInt;
                // packet size combo
                PacketCombo: NativeInt;
                // flow control
                RTSItem, DTRItem, UseWMCItem: NativeInt;
                // buttons
                vConn, vCancel: NativeInt;
        end;

        POpt = ^TOpt;

        TMyPlugin = class
                hPlugin: THandle;
                Connected: boolean;
                IsAborted: boolean;
                // UTF-8 charset flag
                UseUTF8: boolean;
                // device info data
                DevName, DevIMEI, DevFW: string;
                Opt: TOpt;
                CPrt: TSerial;
                USB: TNKUSB;
                OBEX: TOBEX;
                InfoLines: array [0 .. 7] of TInfoPanelLine;
                PhonePath: string;
                NewFolderName: string;
                FCurDir: string;
                FPanelTitle: string;
                FFormat: string;
                FInfoLine: string;
                // progress dialog data
                Src, Dest: string;
                PrMax, PrPos, fPos, fMax, tPos: Int64;
                tMax: real;
                StartTime, TimeElapsed, TimeLeft: TDateTime;
                StartTicks: dword;
                ProgProcessingFile, ProgTitle: PFarChar;
                Item: integer;
                SScreen: THandle;
                SProgress: THandle;
                function FixString(InString: string): string;
                constructor Create;
                destructor Destroy; override;
                procedure Init;
                procedure GetOpenPanelInfo(var AInfo: TOpenPanelInfo);
                procedure ClosePanel;
                procedure LoadSettings;
                procedure SaveSettings;
                procedure ShowProgressMessage(Text: array of string; isWait: boolean = false);
                procedure ProgressInit(Sender: TObject; Count: integer);
                procedure ProgressStep(Sender: TObject; StepPos, Count: integer);
                procedure ProgressFree(Sender: TObject; Count: integer);
                procedure ShowProgress(Sender: TObject; Check: boolean);
                procedure ProgressProc(iPos, iMax: Int64);
                function IsRoot(): boolean;
                function GetFindData(var PanelItemArray: PPluginPanelItemArray; var ItemsNumber: size_t;
                  const OpMode: TOperationModes): NativeInt;
                procedure FreeFindData(const AInfo: TFreeFindDataInfo);
                function ObexListFolder(Path: string; var PanelItems: TPanelItems): NativeInt;
                procedure GetFolderContent(Path, LocalPath: string);
                // TODO Remove Var
                function GetFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; Move: boolean;
                  var DestPath: PFarChar; OpMode: TOperationModes): NativeInt;
                function PutFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; Move: boolean; SrcPath: PFarChar;
                  OpMode: TOperationModes): NativeInt;
                function DeleteFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; OpMode: TOperationModes)
                  : NativeInt;
                function CheckAbort: boolean;
                function SetDirectory(Dir: PFarChar; OpMode: TOperationModes): NativeInt;
                function MakeDirectory(var Dir: PFarChar; OpMode: TOperationModes): NativeInt;
                function GetFolderSize(Path: string): Int64;
                function GetLocalFolderSize(Path: string): Int64;
                function PutFolderContent(LocalPath, Path, SrcPath: string; Move: boolean): NativeInt;
                procedure CPrtRxChar(Sender: TObject; Count: integer);
        end;

        PMyPlugin = ^TMyPlugin;

var
        // global settings, common for all plugin instances
        MainOpt: TMainOpt;
        // dialog handle dictionary: keep options pointer for every dialog
        Dialogs: TDictionary<THandle, POpt>;
        // load save settings helper class
        Settings: TPluginSettings;
        // runtime version info
        VersionInfo: TrpVersionInfo;

procedure HandleMsg(Sender: TObject; Msg: string);
function ToFileTime(str: string): FILETIME;
{$IFDEF 0}
procedure GetDevInfo(const DevName: string; aGUID: TGUID; var Name: string; var Port: string);
{$ENDIF}
procedure GetDevsByGUID(const aGUID: TGUID; const Ports: TStrings; const Names: TStrings);
procedure GetDevsByClass(const DevClass: string; const Opt: TOpt);
procedure EnumInterfaces(const aGUID: TGUID; const Ports: TStrings; const Names: TStrings);

function FixFilename(Name: string): string;
function FixPath(Path: string): string;
function LPos(substr: WideChar; s: string): integer;
function PWideStr(const str: string): PChar;
function FixPhonePath(Path: string): string;

function FarDI(ItemType: Cardinal; X1, Y1, X2, Y2, Focus, Flags, DefaultButton: NativeInt; Data: string)
  : TFarDialogItem;

function GetDevInfoNew(DevName: string): TPortName;

implementation

uses
        uDebug,
        DateUtils,
        maths,
        Registry,
        WebUtil,
        farlang,
        SetupAPI,
{$IFDEF MSXML}
        XMLdoc,
        XMLIntf,
        ActiveX;
{$ELSE}
uXML;
{$ENDIF}

function FarDI(ItemType: Cardinal; X1, Y1, X2, Y2, Focus, Flags, DefaultButton: NativeInt; Data: string)
  : TFarDialogItem;
begin
        ZeroMemory(@result, SizeOf(result));
        result.ItemType := ItemType;
        result.X1 := X1;
        result.Y1 := Y1;
        result.X2 := X2;
        result.Y2 := Y2;
        result.Flags := Flags;
        if Focus = 1 then
                result.Flags := Flags or DIF_FOCUS;
        if DefaultButton = 1 then
                result.Flags := Flags or DIF_DEFAULTBUTTON;

        result.Data := PWideStr(Data);
end;

function DrawProgress(pos, total: dword): string;
var
        i: integer;
begin
        SetLength(result, prLen);
        for i := 1 to prLen do
                result[i] := WideChar($2591);
        if total = 0 then
                total := pos;
        if pos > total then
                pos := total;
        if total = 0 then
                inc(total);
        for i := 1 to (pos * prLen) div total do
                result[i] := WideChar($2588);
        result := format('%s %3d%%'#00, [result, (pos * 100) div total]);
end;

procedure TMyPlugin.ProgressProc(iPos, iMax: Int64);
var
        lines: array [0 .. 20] of PFarChar;
        Count: integer;
        TicksSpent, Speed: dword;
begin
        Count := 0;
        lines[Count] := ProgTitle;
        inc(Count);
        lines[Count] := ProgProcessingFile;
        inc(Count);
        lines[Count] := PFarChar(FixFilename(Src));
        inc(Count);
        if length(Dest) > 0 then begin
                lines[Count] := GetMsg(MCopyingTo);
                inc(Count);
                lines[Count] := PFarChar(FixFilename(Dest));
                inc(Count);
        end;
        lines[Count] := PFarChar(DrawProgress(iPos, iMax));
        inc(Count);
        lines[Count] := PFarChar(format(#01'%s: %s', [GetMsg(MTotal), format('%.0n', [tMax])]));
        inc(Count);
        if fMax > 1 then begin
                lines[Count] := PFarChar(DrawProgress(tPos + iPos, Trunc(tMax)));
                inc(Count);
                lines[Count] := #01#00;
                inc(Count);
                lines[Count] :=
                  PFarChar(format('%s: %d %s %d', [GetMsg(MFilesProcessed), fPos, GetMsg(MFilesProcessedFrom), fMax]));
                inc(Count);
                lines[Count] := #01#00;
                inc(Count);
        end;
        TimeElapsed := Now - StartTime;
        if (tPos + iPos) = 0 then
                TimeLeft := 0
        else
                TimeLeft := TimeElapsed * tMax / (tPos + iPos);
        if TimeLeft >= TimeElapsed then
                TimeLeft := TimeLeft - TimeElapsed;
        TicksSpent := GetTickCount - StartTicks;
        if TicksSpent = 0 then
                Speed := 0
        else
                Speed := ((iPos div TicksSpent) * 1000) div 1024;
        lines[Count] := PFarChar(format('%s: %s  %s: %s   %4d Kbps', [GetMsg(MTimeSpent), FormatDateTime('hh:nn:ss',
          TimeElapsed), GetMsg(MTimeLeft), FormatDateTime('hh:nn:ss', TimeLeft), Speed]));
        inc(Count);
        with Info do
                Message(MAIN_GUID, PROGRESS_MSG_GUID, FMSG_LEFTALIGN, '', @lines, Count, 0);
end;

procedure TMyPlugin.ShowProgress(Sender: TObject; Check: boolean);
begin
        if Check then
                (Sender as TOBEX).IsAborted := CheckAbort;
        ProgressProc(PrPos, max(1, PrMax));
        if (Sender as TOBEX).IsAborted then begin
                debug('User called abort from dialog', 5);
        end;
end;

procedure TMyPlugin.ProgressInit(Sender: TObject; Count: integer);
begin
        PrPos := 0;
        PrMax := Count;
        // save screen
        SProgress := Info.SaveScreen(0, 0, - 1, - 1);
        ShowProgress(Sender, false);
end;

procedure TMyPlugin.ProgressStep(Sender: TObject; StepPos, Count: integer);
begin
        PrPos := StepPos;
        ShowProgress(Sender, true);
end;

procedure TMyPlugin.ProgressFree(Sender: TObject; Count: integer);
begin
        // restore screen
        Info.RestoreScreen(SProgress);
end;

procedure TMyPlugin.ShowProgressMessage(Text: array of string; isWait: boolean);
const
        SPIN_1    = '░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░';
        SPIN_2    = '▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░';
        SPIN_3    = '▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒';
        SPIN_4    = '░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒▒░░▒';
        MAX_LINES = 20;
const
        spinner: array [0 .. 3] of PChar = (SPIN_1, SPIN_2, SPIN_3, SPIN_4);
var
        lines: array [0 .. MAX_LINES] of PFarChar;
        Count, i: integer;
begin
        Count := 0;
        lines[Count] := GetMsg(MTitle);
        inc(Count);
        for i := 0 to min(High(Text), MAX_LINES) do begin
                lines[Count] := PFarChar(Text[i]);
                inc(Count);
        end;
        if isWait then begin
                // to make message window big enough to remain same height
                for i := Count to 5 do begin
                        lines[Count] := '';
                        inc(Count);
                end;
                inc(Count);
                // to make message window big enough to remain same width
                for i := Low(spinner) to High(spinner) do begin
                        lines[Count - 1] := PFarChar(spinner[i]);
                        Info.Message(MAIN_GUID, CONNECT_MSG_GUID, 0, 'hcConnecting', @lines, Count, 0);
                        Sleep(50);
                end;
        end
        else
                Info.Message(MAIN_GUID, CONNECT_MSG_GUID, 0, 'hcConnecting', @lines, Count, 0);
end;

procedure TMyPlugin.Init;
const
        STATE_OPEN_PORT   = 1;
        STATE_IDENTIFY    = 2;
        STATE_SET_SPEED   = 3;
        STATE_SET_CHARSET = 4;
        STATE_TRY_OBEX    = 5;
        STATE_ERROR       = 10;
var
        state: integer;
        SupportedCharsets: string;
begin
        try
                // saving screen before messages
                SScreen := INVALID_HANDLE_VALUE;
                state := STATE_OPEN_PORT;
                repeat
                        case state of
                                STATE_OPEN_PORT: begin
                                        Connected := false;
                                        OBEX.DevType := Opt.DevType;
                                        OBEX.OnProgressInit := ProgressInit;
                                        OBEX.OnProgressStep := ProgressStep;
                                        OBEX.OnProgressFree := ProgressFree;
                                        if Opt.DevType = DEVTYPE_USB_NOKIA then begin
                                                ShowProgressMessage
                                                  ([PChar(Opt.DevName), GetMsg(MLogOpeningUsbPort)], true);
                                                debug(format('Init() Port: %s', [Opt.ComPort]), msgtype_details);
                                                USB.Port := Opt.ComPort;
                                                USB.Open;
                                                OBEX.UseWMC := (Opt.UseWMC = 1);
                                                OBEX.MaxPacketSize := (512 shl Opt.OBEXPS) - 1;
                                                debug(format('Init() Packet: %d', [512 shl Opt.OBEXPS]),
                                                  msgtype_details);
                                                state := STATE_TRY_OBEX;
                                        end
                                        else begin
                                                CPrt.Port := Opt.ComPort;
                                                CPrt.BaudRate := 115200 shl Opt.BaudRate;
                                                debug(format('Init() Port: %s', [CPrt.Port]), msgtype_details);
                                                debug(format('Init() BaudRate: %d', [CPrt.BaudRate]), msgtype_details);
                                                ShowProgressMessage
                                                  ([PChar(Opt.DevName),
                                                  PChar(format(GetMsg(MLogOpeningSerialPortAt),
                                                  [CPrt.Port, 115200 shl Opt.BaudRate]))], true);
                                                if Opt.DTR = 1 then
                                                        CPrt.ControlDTR := dtrEnable
                                                else
                                                        CPrt.ControlDTR := dtrDisable;
                                                if Opt.RTS = 1 then
                                                        CPrt.ControlRTS := rtsEnable
                                                else
                                                        CPrt.ControlRTS := rtsDisable;
                                                OBEX.MaxPacketSize := (512 shl Opt.OBEXPS) - 1;
                                                OBEX.UseWMC := (Opt.UseWMC = 1);
                                                CPrt.Open;
                                                Sleep(200);
                                                debug(format('Init() %s opened OK', [CPrt.Port]), msgtype_details);
                                                if Opt.UseWMC = 1 then begin
                                                        // in cace WMC Obex interface - cant use AT commands
                                                        UseUTF8 := true;
                                                        state := STATE_TRY_OBEX;
                                                end
                                                else
                                                        state := STATE_IDENTIFY;
                                        end;
                                end;
                                STATE_IDENTIFY: begin
                                        // echo OFF
                                        ShowProgressMessage([PChar(Opt.DevName), GetMsg(MLogEchoOff)], true);
                                        if not CPrt.TxAndWait('ATE0', AT_OK, ATCMD_TIMEOUT) then begin
                                                debug('Init() Failed to turn ECHO OFF', msgtype_importanterror);
                                                state := STATE_ERROR;
                                                continue;
                                        end;
                                        // reading info0 data
                                        ShowProgressMessage([PChar(Opt.DevName), GetMsg(MLogReadingDeviceName)], true);
                                        if CPrt.TxAndWait('ATI', 'OK', ATCMD_TIMEOUT) then begin
                                                if (CPrt.RxBuffer.Count > 1) then begin
                                                        DevName := CPrt.RxBuffer[0];
                                                        debug('Init() Device name: ' + DevName, msgtype_details);
                                                end
                                                else begin
                                                        DevName := 'N/a';
                                                        debug('Init() Failed to read device name', msgtype_details);
                                                end;
                                        end
                                        else begin
                                                debug('Init() Failed to read device name', msgtype_importanterror);
                                                state := STATE_ERROR;
                                                continue
                                        end;
                                        // if got device name, can go on. even if the rest identification fails
                                        state := STATE_SET_SPEED;
                                        ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                          GetMsg(MLogReadingDeviceIMEI)], true);
                                        // reading IMEI
                                        if CPrt.TxAndWait('AT+CGSN', AT_OK, ATCMD_TIMEOUT) then begin
                                                if (CPrt.RxBuffer.Count > 1) then begin
                                                        DevIMEI := CPrt.RxBuffer[0];
                                                        debug('Init() IMEI: ' + DevIMEI, msgtype_details);
                                                end
                                                else begin
                                                        DevIMEI := 'N/a';
                                                        debug('Init() Failed to read device IMEI', msgtype_details);
                                                end;
                                        end
                                        else begin
                                                debug('Init() Failed to read device IMEI', msgtype_importanterror);
                                        end;
                                        // reading fw version
                                        ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                          PChar('IMEI: ' + DevIMEI), GetMsg(MLogReadingFWVersion)], true);
                                        if CPrt.TxAndWait('AT+CGMR', AT_OK, ATCMD_TIMEOUT) then begin
                                                if (CPrt.RxBuffer.Count > 1) then begin
                                                        DevFW := CPrt.RxBuffer[0];
                                                        debug('Init() FW: ' + DevFW, msgtype_details);
                                                end
                                                else begin
                                                        DevFW := 'N/a';
                                                        debug('Init() Failed to read device FW', msgtype_details);
                                                end;
                                        end
                                        else begin
                                                debug('Init() Failed to read fw version', msgtype_importanterror);
                                        end;
                                end;
                                STATE_SET_SPEED: begin
                                        ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                          PChar('IMEI: ' + DevIMEI), PChar(DevFW),
                                          PChar(format(GetMsg(MLogSettingTransferSpeedTo),
                                          [115200 shl Opt.BaudRate]))], true);
                                        if CPrt.TxAndWait(format('AT+IPR=%d', [Opt.BaudRate]), AT_OK, ATCMD_TIMEOUT)
                                        then begin
                                                state := STATE_SET_CHARSET;
                                        end
                                        else begin
                                                if CPrt.RxBuffer.Count = 2 then begin
                                                        if CPrt.RxBuffer[1] = 'ERROR' then begin
                                                                debug(format
                                                                  ('Init() Error: The COM-port does''nt support speed: %d',
                                                                  [Opt.BaudRate]), msgtype_importanterror);
                                                                ShowProgressMessage
                                                                  ([PChar(Opt.DevName), PChar(DevName),
                                                                  PChar('IMEI: ' + DevIMEI), PChar(DevFW),
                                                                  PChar(format(GetMsg(MLogTransferSpeedNotSupported),
                                                                  [115200 shl Opt.BaudRate]))], true);
                                                        end;
                                                end;
                                                // error, but still can work on default speed
                                                state := STATE_SET_CHARSET;
                                        end;

                                end;
                                STATE_SET_CHARSET: begin
                                        // debug('Init() Try switching character set to UTF-8', msgtype_details);
                                        SupportedCharsets := 'N/a';
                                        UseUTF8 := false;
                                        if CPrt.TxAndWait('AT+CSCS=?', AT_OK, ATCMD_TIMEOUT) then begin
                                                if (CPrt.RxBuffer.Count > 1) then begin
                                                        SupportedCharsets := CPrt.RxBuffer[0];
                                                        debug('Init() FW: ' + DevFW, msgtype_details);
                                                end;
                                                ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                                  PChar('IMEI: ' + DevIMEI), PChar(DevFW),
                                                  GetMsg(MLogSwitchingCS)], true);
                                                if pos('UTF-8', SupportedCharsets) <> 0 then begin
                                                        if CPrt.TxAndWait('AT+CSCS="UTF-8"', AT_OK, ATCMD_TIMEOUT) then
                                                        begin
                                                                UseUTF8 := true;
                                                                ShowProgressMessage
                                                                  ([PChar(Opt.DevName), PChar(DevName),
                                                                  PChar('IMEI: ' + DevIMEI), PChar(DevFW),
                                                                  GetMsg(MLogUsingUTF8)], true);
                                                                debug('Init() Using UTF-8', msgtype_details);
                                                        end;
                                                end;
                                        end
                                        else begin
                                                debug('Init() Switching character sets command is not supported',
                                                  msgtype_importanterror);
                                                ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                                  PChar('IMEI: ' + DevIMEI), PChar(DevFW),
                                                  GetMsg(MLogFailedToReadSupportedCS)], true);
                                        end;
                                        state := STATE_TRY_OBEX;
                                end;
                                STATE_TRY_OBEX: begin
                                        ShowProgressMessage([PChar(Opt.DevName), PChar(DevName),
                                          PChar('IMEI: ' + DevIMEI), PChar(DevFW), GetMsg(MLogConnectingOBEX)], true);
                                        PhonePath := '';
                                        OBEX.Connect(ObexFolderBrowserServiceID);
                                        Connected := OBEX.Connected;
                                        break;
                                end;
                                STATE_ERROR: begin
                                        break;
                                end;
                        end;
                until false;
        except
                OBEX.Disconnect;
                CPrt.Close;
                Connected := false;
                raise;
        end;
end;

{ TODO : Remove if unused }
procedure HandleMsg(Sender: TObject; Msg: string);
var
        AMsg: String;
begin
        AMsg := trim(Msg);
{$IFDEF DEBUG_IO}
        debug('HandleMsg [RX] ' + AMsg, msgtype_details);
{$ENDIF}
        try
                if pos('RING', AMsg) = 1 then begin
                        // HandleRinging;
                end
                else if pos('+CSQ', AMsg) = 1 then begin
                        // HandleStatus(AMsg);
                end
                else if pos('+CBC', AMsg) = 1 then begin
                        // HandleStatus(AMsg);
                end
                else if pos('+CMTI', AMsg) = 1 then begin
                        // HandleNEWSMS(AMsg);
                end
                else if pos('+CSCS', AMsg) = 1 then begin
                        // HandleCSCS(AMsg);
                end
                { else if pos('*EJAVA', AMsg) = 1 then begin
                  HandleEJAVA(AMsg);
                  end }
                else begin
                        if pos('ERROR: 515', AMsg) > 0 then begin
                                debug('HandleMsg() Please wait, init in progress...', msgtype_details);
                                (Sender as TSerial).FWaiting := false;
                        end
                        else if pos('ERROR', AMsg) > 0 then begin
                                debug('HandleMsg() Recieved ERROR!', msgtype_details);
                                (Sender as TSerial).FWaiting := false;
                        end;
                end;
        except
                on e: Exception do begin
                        debug('HandleMsg() ' + e.Message, msgtype_importanterror);
                end;
        end;
end;

procedure TMyPlugin.CPrtRxChar(Sender: TObject; Count: integer);
var
        buffer: AnsiString;
begin
        if Opt.DevType = DEVTYPE_USB_NOKIA then
                buffer := USB.buffer
        else
                CPrt.ReadStr(buffer, Count);
{$IFDEF DEBUG_IO}
        debug('[RX]: ' + bytestream2hex(buffer), msgtype_details);
{$ENDIF}
        if Assigned(OBEX) then
                if OBEX.Connected then
                        OBEX.OnRxStr(buffer);
end;

function ToFileTime(str: string): FILETIME;
var
        st: _SYSTEMTIME;
begin
        result.dwLowDateTime := 0;
        result.dwHighDateTime := 0;
        if pos('T', str) > 0 then begin
                with st do begin
                        wYear := StrToIntDef(copy(str, 1, 4), 0);
                        wMonth := StrToIntDef(copy(str, 5, 2), 0);
                        wDay := StrToIntDef(copy(str, 7, 2), 0);
                        wHour := StrToIntDef(copy(str, 10, 2), 0);
                        wMinute := StrToIntDef(copy(str, 12, 2), 0);
                        wSecond := StrToIntDef(copy(str, 14, 2), 0);
                        wMilliSeconds := 0;
                end;
                SystemTimeToFileTime(st, result);
        end;
end;

procedure TMyPlugin.LoadSettings;
begin
        try
                debug('LoadSettings', msgtype_details);
                Opt.ComPort := Settings.Get(0, 'ComPort', 'COM1');
                Opt.BaudRate := Settings.Get(0, 'BaudRate', 115200);
                Opt.RTS := Settings.Get(0, 'RTS', 0);
                Opt.DTR := Settings.Get(0, 'DTR', 0);
                Opt.Log := Settings.Get(0, 'LOG', 0);
                Opt.OBEXPS := Settings.Get(0, 'OBEXPS', 7);
                Opt.DevType := Settings.Get(0, 'DEVTYPE', DEVTYPE_COM_PORT);
                Opt.UseWMC := Settings.Get(0, 'USEWMC', 0);
                // saving port for later reference
                Opt.OldPort := Opt.ComPort;
                debug(format('LoadSettings (port: %s, baud: %d, rts: %d, dtr: %d, packet: %d)',
                  [Opt.ComPort, Opt.BaudRate, Opt.RTS, Opt.DTR, Opt.OBEXPS]), msgtype_details);
        except
                on e: Exception do begin
                        debug('LoadSettings: ' + e.Message, msgtype_importanterror);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorLoadSettings), PFarChar(e.Message),
                          integer(MOkButton)], 1);
                end;
        end;
end;

procedure TMyPlugin.SaveSettings;
begin
        try
                debug(format('SaveSettings (port: %s, baud: %d, rts: %d, dtr: %d, packet: %d)',
                  [Opt.ComPort, Opt.BaudRate, Opt.RTS, Opt.DTR, Opt.OBEXPS]), msgtype_details);
                Settings.SetString(0, 'ComPort', PChar(Opt.ComPort));
                Settings.SetInteger(0, 'BaudRate', Opt.BaudRate);
                Settings.SetInteger(0, 'RTS', Opt.RTS);
                Settings.SetInteger(0, 'DTR', Opt.DTR);
                Settings.SetInteger(0, 'LOG', Opt.Log);
                Settings.SetInteger(0, 'OBEXPS', Opt.OBEXPS);
                Settings.SetInteger(0, 'DEVTYPE', Opt.DevType);
                Settings.SetInteger(0, 'USEWMC', Opt.UseWMC);
        except
                on e: Exception do begin
                        debug('SaveSettings: ' + e.Message, msgtype_importanterror);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorSaveSettings), PFarChar(e.Message),
                          integer(MOkButton)], 1);
                end;
        end;
end;

// Detect USB interface (SE and NOKIA)
procedure EnumInterfaces(const aGUID: TGUID; const Ports: TStrings; const Names: TStrings);
var
        DevPorts, DevNames, tmpP, tmpN: TStrings;
        index, i: integer;
        Name, Port: string;
begin
        DevPorts := TStringList.Create;
        DevNames := TStringList.Create;
        tmpP := TStringList.Create;
        tmpN := TStringList.Create;
        try
                GetDevsByGUID(aGUID, DevPorts, DevNames);
                for i := 0 to DevPorts.Count - 1 do begin
                        // skipping wrong devices
                        name := DevNames[i];
                        Port := DevPorts[i];
                        name := trim(StringReplace(name, 'USB OBEX', '', [rfReplaceAll, rfIgnoreCase]));
                        index := tmpN.IndexOf(name);
                        if index >= 0 then
                                tmpP[index] := tmpP[index] + ',' + Port
                        else begin
                                tmpN.Add(name);
                                tmpP.Add(trim(Port));
                        end;
                end;
                Ports.Assign(tmpP);
                Names.Assign(tmpN);
        finally
                tmpN.Free;
                tmpP.Free;
                DevNames.Free;
                DevPorts.Free;
        end;
end;

procedure GetDevsByGUID(const aGUID: TGUID; const Ports: TStrings; const Names: TStrings);
var
        DevHandle: HDEVINFO;
        MemIndex: dword;
        DevData: TSPDevInfoData;
        DeviceInterfaceData: TSPDeviceInterfaceData;
        RegDataType: dword;
        BytesReturned: dword;
        buffer: array [0 .. MAX_PATH] of CHAR;
        FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
        e: integer;
        tmpP, tmpN: TStrings;
        size: Cardinal;
        DevInstance: PChar;
        Name, Port: string;
        PortName: TPortName;
begin
        DevHandle := SetupDiGetClassDevsW(@aGUID, nil, 0, DIGCF_DEVICEINTERFACE or DIGCF_PRESENT);
        if DevHandle <> Pointer(INVALID_HANDLE_VALUE) then begin
                tmpP := TStringList.Create;
                tmpN := TStringList.Create;
                try
                        DeviceInterfaceData.cbSize := SizeOf(TSPDeviceInterfaceData);
                        DevData.cbSize := SizeOf(DevData);
                        MemIndex := 0;
                        while SetupDiEnumDeviceInterfaces(DevHandle, nil, aGUID, MemIndex, DeviceInterfaceData) do begin
                                BytesReturned := 0;
                                SetupDiGetDeviceInterfaceDetailW(DevHandle, @DeviceInterfaceData, nil, 0, BytesReturned,
                                  @DevData);
                                e := GetLastError;
                                if (BytesReturned <> 0) and (e = ERROR_INSUFFICIENT_BUFFER) then begin
                                        FunctionClassDeviceData := AllocMem(BytesReturned);
                                        try
                                                FunctionClassDeviceData.cbSize := SizeOf(TSPDeviceInterfaceDetailData);
                                                if SetupDiGetDeviceInterfaceDetailW(DevHandle, @DeviceInterfaceData,
                                                  FunctionClassDeviceData, BytesReturned, BytesReturned, @DevData) then
                                                begin
                                                        // save device name here
                                                        SetupDiGetDeviceInstanceIdW(DevHandle, @DevData, nil, 0, size);
                                                        GetMem(DevInstance, size * SizeOf(CHAR));
                                                        if Assigned(DevInstance) then
                                                                try
                                                                        if SetupDiGetDeviceInstanceIdW(DevHandle,
                                                                          @DevData, DevInstance, size, size) then begin
                                                                                ZeroMemory(@buffer, SizeOf(buffer));
                                                                                if SetupDiGetDeviceRegistryPropertyW
                                                                                  (DevHandle, DevData,
                                                                                  SPDRP_LOCATION_INFORMATION,
                                                                                  RegDataType, PByte(@buffer[0]),
                                                                                  SizeOf(buffer), BytesReturned) then
                                                                                begin
                                                                                        PortName :=
                                                                                          GetDevInfoNew(DevInstance);
                                                                                        if PortName.Port = '' then
                                                                                                PortName.Port :=
                                                                                                  String(FunctionClassDeviceData.
                                                                                                  DevicePath);
                                                                                        if IsEqualGUID(aGUID,
                                                                                          SE_OBEX_GUID) and
                                                                                          (BytesReturned <> 0) then
                                                                                                PortName.Name :=
                                                                                                  String(buffer);
                                                                                        tmpP.Add(trim(Port));
                                                                                        tmpN.Add(trim(Name));
                                                                                end;
                                                                        end;
                                                                finally
                                                                        FreeMem(DevInstance);
                                                                end;
                                                end;
                                        finally
                                                FreeMem(FunctionClassDeviceData);
                                        end;
                                end;
                                inc(MemIndex);
                        end;
                        Ports.AddStrings(tmpP);
                        Names.AddStrings(tmpN);
                finally
                        tmpN.Free;
                        tmpP.Free;
                        SetupDiDestroyDeviceInfoList(DevHandle);
                end;
        end;
end;

procedure GetDevsByClass(const DevClass: string; const Opt: TOpt);
var
        DevHandle: HDEVINFO;
        MemIndex: dword;
        DevData: TSPDevInfoData;
        buffer: array [0 .. MAX_PATH] of CHAR;
        rsize, size: dword;
        DevInstance: PChar;
        PortName: TPortName;
begin
        if not SetupDiClassGuidsFromNameW(PChar(DevClass), @buffer, SizeOf(buffer), size) then
                raise Exception.Create(GetMsg(MUnknownError));
        DevHandle := SetupDiGetClassDevsW(@buffer, nil, 0, DIGCF_PRESENT);
        if DevHandle <> Pointer(INVALID_HANDLE_VALUE) then
                try
                        Opt.Ports.Clear;
                        Opt.Names.Clear;
                        ZeroMemory(@DevData, SizeOf(DevData));
                        DevData.cbSize := SizeOf(DevData);
                        MemIndex := 0;
                        while SetupDiEnumDeviceInfo(DevHandle, MemIndex, DevData) do begin
                                inc(MemIndex);
                                // ZeroMemory(buffer, SizeOf(buffer));
                                // SetupDiGetDeviceRegistryPropertyW(DevHandle, DevData, SPDRP_FRIENDLYNAME, RegDataType,
                                // PByte(@buffer[0]), SizeOf(buffer), BytesReturned);
                                // // no friendly name?
                                // if BytesReturned = 0 then
                                // SetupDiGetDeviceRegistryPropertyW(DevHandle, DevData, SPDRP_DEVICEDESC,
                                // RegDataType, PByte(@buffer[0]), SizeOf(buffer), BytesReturned);
                                size := 0;
                                SetupDiGetDeviceInstanceIdW(DevHandle, @DevData, nil, 0, size);
                                // debug(format('GetDevsByClass, need %d bytes', [size]), 5);
                                if (size <> 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then begin
                                        GetMem(DevInstance, size * SizeOf(CHAR));
                                        if Assigned(DevInstance) then
                                                try
                                                        if SetupDiGetDeviceInstanceIdW(DevHandle, @DevData, DevInstance,
                                                          size, rsize) then begin
                                                                PortName := GetDevInfoNew(String(DevInstance));
                                                                if PortName.Port <> '' then begin
                                                                        Opt.Ports.Add(PortName.Port);
                                                                        Opt.Names.Add(PortName.Name);
                                                                end;
                                                        end;
                                                finally
                                                        FreeMem(DevInstance);
                                                end;
                                end

                        end;
                finally
                        SetupDiDestroyDeviceInfoList(DevHandle);
                end;
end;

{$IFDEF 0}

procedure GetDevInfo(const DevName: string; aGUID: TGUID; var Name: string; var Port: string);

var
        reg: TRegIniFile;
        st: TStrings;
        i: integer;
        loc, enum, ParentID, tmp, com, vp, d1, mi: string;
begin
        // Name: \\?\USB#Vid_0fce&Pid_e012#5&42b9eef&0&1#{a5dcbf10-6530-11d2-901f-00c04fb951ed}
        enum := DevName;
        Delete(enum, 1, 4);
        enum := copy(enum, 1, pos('#', enum) - 1) + '\';
        // debug('ENUM: '+enum,5);
        tmp := copy(DevName, pos('#', DevName) + 1, length(DevName));
        vp := copy(tmp, 1, pos('#', tmp) - 1);
        tmp := copy(tmp, pos('#', tmp) + 1, length(tmp));
        d1 := copy(tmp, 1, pos('#', tmp) - 1);
        tmp := vp + '\' + d1;
        com := 'FAIL';
        reg := TRegIniFile.Create;
        try
                reg.RootKey := HKEY_LOCAL_MACHINE;
                if not reg.OpenKeyReadOnly('system\CurrentControlSet\Enum\' + enum + tmp) then
                        exit;
                { case ptype of
                  1,2:ParentID:=reg.ReadString('','ParentIdPrefix','');
                  3:ParentID:=d1;
                  end; }
                ParentID := d1;
                reg.CloseKey;
                { case ptype of
                  1:mi:='&Mi_01';//master
                  2:mi:='&Mi_03';//slave
                  3:mi:='';//normal //debvice menegment
                  end; }
                mi := '';

                tmp := vp + mi;
                if true then begin
                        reg.OpenKeyReadOnly('system\CurrentControlSet\Enum\' + enum + tmp);
                        st := TStringList.Create;
                        try
                                reg.GetKeyNames(st);
                                // GetValueNames(st);
                                for i := 0 to st.Count - 1 do
                                        if pos(ParentID, st[i]) = 1 then begin
                                                Port := reg.ReadString(st[i] + '\Device Parameters', 'PortName', '');
                                                if Port = '' then
                                                        Port := DevName;
                                                name := reg.ReadString(st[i], 'FriendlyName', '');
                                                if name = '' then
                                                        Name := reg.ReadString(st[i], 'DeviceDesc', '');
                                                loc := reg.ReadString(st[i], 'LocationInformation', '');
                                                if (GUIDtoString(aGUID) = GUIDtoString(SE_OBEX_GUID)) and (loc <> '')
                                                then
                                                        Name := loc;
                                                // if loc<>'' then
                                                // name:=loc;
                                                // name:=format('%s > %s',[loc, name]);
                                                debug(format('[CONF] getdevinfo: name: %s / port: %s',
                                                  [Name, Port]), 5);
                                                break;
                                        end;
                        finally
                                st.Free;
                        end;
                end;
                reg.CloseKey;
        finally
                reg.Free;
        end;
end;
{$ENDIF}

function GetDevInfoNew(DevName: string): TPortName;
const
        SEMICOLON = ';';
var
        reg: TRegIniFile;
        tmp: string;
begin
        result.Port := '';
        result.Name := '';
        reg := TRegIniFile.Create;
        try
                reg.RootKey := HKEY_LOCAL_MACHINE;
                if reg.OpenKeyReadOnly(format('system\CurrentControlSet\Enum\%s', [DevName])) then
                        try
                                tmp := reg.ReadString('', 'FriendlyName', 'N/a');
                                if tmp = '' then
                                        tmp := reg.ReadString('', 'DeviceDesc', '');
                                result.Name := tmp;
                                if result.Name.Contains(SEMICOLON) then
                                        result.Name := result.Name.Split([SEMICOLON])[1];
                                result.Port := reg.ReadString('Device Parameters', 'PortName', '');
                        finally
                                reg.CloseKey;
                        end;
                debug(format('name: %s, port: %s', [result.Name, result.Port]), 5);
        finally
                reg.Free;
        end;
end;

function buf2hex(byteStream: PChar; len: dword): String;
var
        i: integer;
begin
        try
                result := '';
                if len = 0 then
                        exit;
                if byteStream = nil then
                        exit;
                for i := 0 to min(len - 1, $1000 - 1) do
                        result := result + IntToHex(byte(byteStream[i]), 2) + ' ';
        except
                // silent errors
        end;
end;

function TMyPlugin.FixString(InString: string): string;
begin
        result := StringReplace(HTMLDecode(InString), #$0D#$0A, #$0A, [rfReplaceAll]);
{$IFDEF MSXML}
{$ELSE}
        if UseUTF8 then begin
                result := UTF8Decode(result);
                // if UTF8.IndexOf(result)<0 then begin
                // UTF8.Add(result);
                // OEM.Append(StringReplace(result,#$0A,' ',[rfReplaceAll]));
                // end;
        end;
{$ENDIF}
        result := StringReplace(result, #$0A, ' ', [rfReplaceAll]);
end;

function TMyPlugin.ObexListFolder(Path: string; var PanelItems: TPanelItems): NativeInt;
var
        StringStream: TStringStream;
        perm: String;
        CurName, objname: string;
        CurSize: string;
{$IFDEF MSXML}
        Xml: IXMLDocument;
        XMLNode: IXMLNode;
{$ELSE}
        Xml: TXML;
        XMLNode: TXMLNode;
{$ENDIF}
        i: integer;
        Encoding: TEncoding;
begin
        debug(format('ObexListFolder(0x%.8x) Accessing: "%s"', [hPlugin, Path]), msgtype_details);
        result := 0;
        try
                StringStream := TStringStream.Create('');
                try
                        Path := FixPhonePath(Path);
                        objname := Path;
                        if OBEX.SetDir(objname) then begin
                                // changing nested dirs at once (Pictures/Camera) doesn't seem to be supported
                                // so we do it in steps (change to '\' -> change to 'Pictures' -> change to 'Camera')
                                OBEX.List(StringStream); // get current dir contents
                        end
                        else
                                exit;
{$IFDEF MSXML}
                        Xml := TXMLDOcument.Create(nil);
{$ELSE}
                        Xml := TXML.Create();
{$ENDIF}
                        try
                                // p1 := pos(' [ <', Dir.Text);
                                // p2 := pos('> ]', Dir.Text);
                                // if (p1 > 0) and (p2 > 0) then
                                // Dir.Text := copy(Dir.Text, 1, p1) + copy(Dir.Text, p2 + 3, length(Dir.Text));
{$IFDEF MSXML}
                                if UseUTF8 then
                                        Encoding := TEncoding.UTF8
                                else
                                        Encoding := nil;
                                Xml.Xml.LoadFromStream(StringStream, Encoding);
                                Xml.Active := true;
{$ELSE}
                                Xml.Xml := Dir.Text;
{$ENDIF}
                                // first check for dirs, they should appear first in the list
                                // debug(format('XML: We have %d childs here',[XML.RootNode.nodes.Count]),msgtype_details);
{$IFDEF MSXML}
                                SetLength(PanelItems, max(3, Xml.DocumentElement.ChildNodes.Count + 3));
                                XMLNode := Xml.DocumentElement.ChildNodes[0];
{$ELSE}
                                SetLength(PanelItems, max(3, Xml.RootNode.nodes.Count + 3));
                                XMLNode := Xml.FirstChild;
{$ENDIF}
                                i := 0;
                                while Assigned(XMLNode) do begin
{$IFDEF MSXML}
                                        if SameText(XMLNode.NodeName, 'folder') then begin
                                                CurName := FixString(XMLNode.GetAttributeNS('name', ''));
{$ELSE}
                                        if SameText(XMLNode.TagName, 'folder') then begin
                                                CurName := FixString(XMLNode.attribute['name']);
{$ENDIF}
                                                PanelItems[i].FileName := StrNew(PChar(CurName));
                                                PanelItems[i].FileSize := 0;
                                                PanelItems[i].FileAttributes := FILE_ATTRIBUTE_DIRECTORY;
                                                if XMLNode.hasAttribute('created') then
                                                        PanelItems[i].CreationTime :=
                                                          ToFileTime(XMLNode.GetAttribute('created'));
                                                if XMLNode.hasAttribute('accessed') then
                                                        PanelItems[i].LastAccessTime :=
                                                          ToFileTime(XMLNode.GetAttribute('accessed'));
                                                if XMLNode.hasAttribute('modified') then
                                                        PanelItems[i].LastWriteTime :=
                                                          ToFileTime(XMLNode.GetAttribute('modified'));
                                                if PanelItems[i].LastAccessTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastAccessTime := PanelItems[i].LastWriteTime;
                                                if PanelItems[i].LastAccessTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastAccessTime := PanelItems[i].CreationTime;
                                                if PanelItems[i].CreationTime.dwHighDateTime = 0 then
                                                        PanelItems[i].CreationTime := PanelItems[i].LastWriteTime;
                                                if PanelItems[i].LastWriteTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastWriteTime := PanelItems[i].CreationTime;
                                                if PanelItems[i].LastWriteTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastWriteTime := PanelItems[i].LastAccessTime;
                                                if XMLNode.hasAttribute('user-perm') then begin
                                                        perm := XMLNode.GetAttribute('user-perm');
                                                        if pos('W', perm) = 0 then
                                                                PanelItems[i].FileAttributes :=
                                                                  PanelItems[i].FileAttributes or
                                                                  FILE_ATTRIBUTE_READONLY;
                                                end;
                                                inc(i);
                                        end;
                                        XMLNode := XMLNode.NextSibling;
                                end;
                                // now check for files in this dir
{$IFDEF MSXML}
                                XMLNode := Xml.DocumentElement.ChildNodes[0];
{$ELSE}
                                XMLNode := Xml.FirstChild;
{$ENDIF}
                                while Assigned(XMLNode) do begin
{$IFDEF MSXML}
                                        if SameText(XMLNode.NodeName, 'file') then begin
                                                CurName := FixString(XMLNode.Attributes['name']);
                                                CurSize := XMLNode.Attributes['size'];
{$ELSE}
                                        if SameText(XMLNode.TagName, 'file') then begin
                                                CurName := FixString(XMLNode.attribute['name']);
                                                CurSize := XMLNode.attribute['size'];
{$ENDIF}
                                                if SameText(CurSize[length(CurSize)], 'D') then
                                                        // check for '12345d' case
                                                        SetLength(CurSize, length(CurSize) - 1);
                                                // cut of 'd' if found
                                                PanelItems[i].FileName := StrNew(PChar(CurName));
                                                PanelItems[i].FileSize := StrToInt64Def(CurSize, 0);
                                                PanelItems[i].FileAttributes := 0;
                                                if XMLNode.hasAttribute('created') then
                                                        PanelItems[i].CreationTime :=
                                                          ToFileTime(XMLNode.GetAttribute('created'));
                                                if XMLNode.hasAttribute('accessed') then
                                                        PanelItems[i].LastAccessTime :=
                                                          ToFileTime(XMLNode.GetAttribute('accessed'));
                                                if XMLNode.hasAttribute('modified') then
                                                        PanelItems[i].LastWriteTime :=
                                                          ToFileTime(XMLNode.GetAttribute('modified'));
                                                if PanelItems[i].LastAccessTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastAccessTime := PanelItems[i].LastWriteTime;
                                                if PanelItems[i].LastAccessTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastAccessTime := PanelItems[i].CreationTime;
                                                if PanelItems[i].CreationTime.dwHighDateTime = 0 then
                                                        PanelItems[i].CreationTime := PanelItems[i].LastWriteTime;
                                                if PanelItems[i].LastWriteTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastWriteTime := PanelItems[i].CreationTime;
                                                if PanelItems[i].LastWriteTime.dwHighDateTime = 0 then
                                                        PanelItems[i].LastWriteTime := PanelItems[i].LastAccessTime;
                                                if XMLNode.hasAttribute('user-perm') then begin
                                                        perm := XMLNode.GetAttribute('user-perm');
                                                        if pos('W', perm) = 0 then
                                                                PanelItems[i].FileAttributes :=
                                                                  PanelItems[i].FileAttributes or
                                                                  FILE_ATTRIBUTE_READONLY;
                                                end;
                                                inc(i);
                                        end;
                                        XMLNode := XMLNode.NextSibling;
                                end;
                                result := i;
                                debug(format('ObexListFolder(0x%.8x) %s: %d items', [hPlugin, Path, i]),
                                  msgtype_operationcomplete);
                        finally
{$IFDEF MSXML}
                                // dont free ActiveX object
{$ELSE}
                                Xml.Free;
{$ENDIF}
                        end;
                finally
                        StringStream.Free;
                end;
        except
                on e: Exception do begin
                        debug('[PLUGIN] ObexListFolder: ' + e.Message, msgtype_importanterror);
                        raise;
                end;
        end;
end;

constructor TMyPlugin.Create;
begin
        inherited;
        ZeroMemory(@Opt, SizeOf(TOpt));
        Opt.Ports := TStringList.Create;
        Opt.Names := TStringList.Create;
        CPrt := TSerial.Create;
        USB := TNKUSB.Create;
        OBEX := TOBEX.Create;
        OBEX.ComPort := CPrt;
        OBEX.USB := USB;
        USB.OnRxChar := CPrtRxChar;
        CPrt.OnRxChar := CPrtRxChar;
        CPrt.OnHandleMsg := HandleMsg;
end;

destructor TMyPlugin.Destroy;
begin
        debug('Destroy', 5);
        if Assigned(OBEX) then begin
                OBEX.ComPort := nil;
                OBEX.USB := nil;
                OBEX.Free;
        end;
        debug('OBEX Destroyed', 5);
        if Assigned(CPrt) then begin
                CPrt.Free;
        end;
        debug('CPrt Destroyed', 5);
        if Assigned(USB) then begin
                USB.Free;
        end;
        debug('USB Destroyed', 5);
        if Assigned(Opt.Ports) then
                Opt.Ports.Free;
        if Assigned(Opt.Names) then
                Opt.Names.Free;
        inherited;
end;

function FixPath(Path: string): string;
begin
        if Path.EndsWith('\') then
                result := Path
        else
                result := Path + '\';
end;

function FixFilename(Name: string): string;
var
        l, s: integer;
begin
        l := length(name);
        if l < (prLen + 4) then begin
                result := name;
                exit;
        end;
        s := l - prLen + 8 - 3 + 1;
        result := format('%s...%s', [copy(name, 1, 7), copy(name, s, l)]);
end;

function LPos(substr: WideChar; s: string): integer;
var
        i: integer;
begin
        for i := length(s) downto 1 do
                if s[i] = substr then begin
                        result := i;
                        exit;
                end;
        result := 0;
end;

function PWideStr(const str: string): PChar;
begin
        if str = '' then
                result := nil
        else
                result := StrNew(PChar(str));
end;

procedure WStrDispose(str: PWideChar);
begin
        if str <> nil then
                StrDispose(str);
end;

function FixPhonePath(Path: string): string;
begin
        result := Path;
        if (length(Path) > 0) and (Path[length(Path)] <> '\') then
                result := Path + '\';
end;

function TMyPlugin.GetFolderSize(Path: string): Int64;
var
        res, i, Count: NativeUInt;
        parr: PPluginPanelItemArray;
begin
        debug(format('GetFolderSize(0x%.8x) dir: "%s"', [hPlugin, Path]), 5);
        result := 0;
        with Info do
                res := GetPluginDirList(MAIN_GUID, hPlugin, PFarChar(Path), parr, Count);
        try
                if res = 0 then begin
                        IsAborted := true;
                        exit;
                end;
                for i := 0 to Count - 1 do begin
                        debug(format('GetFolderSize() found: %s = %d', [parr[i].FileName, parr[i].FileSize]), 5);
                        if (parr[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                                result := result + parr[i].FileSize;
                                inc(fMax);
                        end;
                end;
        finally
                if Assigned(Info.FreePluginDirList) then
                        Info.FreePluginDirList(0, parr, Count)
        end;
end;

procedure TMyPlugin.GetFolderContent(Path, LocalPath: string);
var
        i, Count: size_t;
        parr: TPanelItems;
        LocalName, RemoteName: string;
        Code: Cardinal;
begin
        debug(format('GetFolderContent(0x%.8x) "%s" -> "%s"', [hPlugin, Path, LocalPath]), 5);
        Count := ObexListFolder(Path, parr);
        try
                LocalPath := FixPath(LocalPath);
                if not CreateDirectoryW(PChar(LocalPath), nil) then begin
                        Code := GetLastError;
                        if Code <> 183 then
                                raise EComPort.CreateMsg
                                  (format('%s: %s', [GetMsg(MErrorCreateFolder), LocalPath]), Code);
                end;
                for i := 0 to Count - 1 do begin
                        if (parr[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                GetFolderContent(format('%s\%s', [Path, parr[i].FileName]),
                                  format('%s%s', [LocalPath, parr[i].FileName]));
                                if IsAborted then
                                        break;
                        end
                        else begin
                                StartTicks := GetTickCount;
                                LocalName := (format('%s%s', [LocalPath, parr[i].FileName]));
                                RemoteName := format('%s\%s', [Path, parr[i].FileName]);
                                Dest := LocalName;
                                Src := Opt.DevTitle + ':\' + RemoteName;
                                debug(format('GetFolderContent() GetFile: %s -> %s', [RemoteName, LocalName]), 5);
                                OBEX.GetFile(LocalName, RemoteName);
                                if IsAborted then
                                        break;
                                inc(fPos);
                                tPos := tPos + parr[i].FileSize;
                        end;
                end;
        finally
                parr := nil;
        end;
end;

function TMyPlugin.GetLocalFolderSize(Path: string): Int64;
var
        res, i, Count: NativeUInt;
        parr: PPluginPanelItemArray;
begin
        debug(format('GetLocalFolderSize(0x%.8x) dir: "%s"', [hPlugin, Path]), 5);
        result := 0;
        with Info do
                res := GetDirList(PFarChar(Path), parr, Count);
        try
                if res = 0 then begin
                        IsAborted := true;
                        exit;
                end;
                for i := 0 to Count - 1 do begin
                        debug(format('GetLocalDirList: "%s"', [parr[i].FileName]), 5);
                        if (parr[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                                result := result + parr[i].FileSize;
                                inc(fMax);
                        end;
                end;
        finally
                Info.FreeDirList(parr, Count);
        end;
end;

function TMyPlugin.PutFolderContent(LocalPath, Path, SrcPath: string; Move: boolean): NativeInt;
var
        i, Count: NativeUInt;
        parr: PPluginPanelItemArray;
        LocalName, RemoteName: string;
begin
        debug(format('PutFolderContent(0x%.8x) "%s" -> "%s"', [hPlugin, LocalPath, Path]), 5);
        result := 0;
        with Info do
                GetDirList(PFarChar(LocalPath), parr, Count);
        try
                OBEX.CreateDir(FixPhonePath(Path));
                for i := 0 to Count - 1 do begin
                        if (parr[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                                StartTicks := GetTickCount;
                                LocalName := format('%s', [parr[i].FileName]);
                                RemoteName := Path + '\' + copy(LocalName, length(LocalPath) + 2, length(LocalName));
                                Src := LocalName;
                                Dest := Opt.DevTitle + ':\' + RemoteName;
                                debug(format('PutFolderContent() PutFile: %s -> %s', [LocalName, RemoteName]), 5);
                                OBEX.PutFile(LocalName, RemoteName);
                                if IsAborted then
                                        break;
                                if Move and (not DeleteFileW(PChar(LocalName))) then
                                        raise EComPort.CreateMsg
                                          (format('%s: %s', [GetMsg(MErrorDeleteFile), parr[i].FileName]),
                                          GetLastError);
                                inc(fPos);
                                tPos := tPos + parr[i].FileSize;
                        end
                        else begin
                                LocalName := format('%s', [parr[i].FileName]);
                                RemoteName := FixPhonePath(Path + '\' + copy(LocalName, length(LocalPath) + 2,
                                  length(LocalName)));
                                debug(format('PutFolderContent() NewFolder: %s -> %s', [LocalName, RemoteName]), 5);
                                OBEX.CreateDir(RemoteName);
                        end;
                end;
                if Move and not IsAborted then begin
                        for i := Count - 1 downto 0 do begin
                                if (parr[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                        if not RemoveDir(format('%s', [parr[i].FileName])) then
                                                raise EComPort.CreateMsg
                                                  (format('%s: %s', [GetMsg(MErrorDeleteFolder), parr[i].FileName]),
                                                  GetLastError);
                                end;
                        end;
                end;
        finally
                Info.FreeDirList(parr, Count);
        end;
end;

function TMyPlugin.CheckAbort: boolean;
var
        IR: INPUT_RECORD;
        HP: THandle;
        l, ke: dword;
        lines: array [0 .. 20] of PFarChar;
        res, Count: integer;
begin
        result := false;
        if IsAborted then
                exit;
        HP := GetStdHandle(STD_INPUT_HANDLE);
        GetNumberOfConsoleInputEvents(HP, l);
        while l > 0 do begin
                ke := 1;
                if ReadConsoleInput(HP, IR, ke, ke) then begin
                        if (IR.EventType = KEY_EVENT) then
                                if (IR.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) and (IR.Event.KeyEvent.bKeyDown) then
                                begin
                                        Count := 0;
                                        lines[Count] := GetMsg(MTitle);
                                        inc(Count);
                                        lines[Count] := GetMsg(MTerminate);
                                        inc(Count);
                                        with Info do
                                                res := Message(MAIN_GUID, CHECK_ABORT_MSG_GUID,
                                                  FMSG_WARNING + FMSG_MB_YESNO, 'hcBreak', @lines, Count, 0);
                                        IsAborted := (res = 0);
                                        if res = - 1 then
                                                ReadConsoleInput(HP, IR, ke, ke);
                                        result := IsAborted;
                                        exit;
                                end;
                end;
                dec(l);
        end;
end;

function TMyPlugin.IsRoot(): boolean;
begin
        result := PhonePath = '';
end;

function TMyPlugin.GetFindData(var PanelItemArray: PPluginPanelItemArray; var ItemsNumber: size_t;
  const OpMode: TOperationModes): NativeInt;
var
        PanelItems: TPanelItems;
        i: size_t;
        ShowProgress: boolean;
        SS: THandle;
begin
        debug(format('GetFindData(0x%.8x) dir: "%s" 0x%.4X', [hPlugin, PhonePath, OpMode]), 5);
        try
                if not Connected then begin
                        result := 0;
                end
                else begin
                        ShowProgress := OpMode = OPM_NONE;
                        SS := 0;
                        if ShowProgress then begin
                                SS := Info.SaveScreen(0, 0, - 1, - 1);
                                ShowProgressMessage([PChar(Opt.DevName), GetMsg(MReadingFolder), PChar(PhonePath)]);
                        end;
                        try
                                IsAborted := false;
                                ItemsNumber := ObexListFolder(PhonePath, PanelItems);
                        finally
                                if ShowProgress and (SS <> 0) then
                                        Info.RestoreScreen(SS);
                        end;
                        if ItemsNumber <> 0 then begin
                                GetMem(PanelItemArray, ItemsNumber * SizeOf(TPluginPanelItem));
                                for i := 0 to ItemsNumber - 1 do
                                        PanelItemArray[i] := PanelItems[i];
                                PanelItems := nil;
                        end;

                        result := 1;
                end;
        except
                on e: Exception do begin
                        debug('GetFindData: ' + e.Message, 5);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorListingFolder), PFarChar(PhonePath),
                          PFarChar(e.Message), integer(MOkButton)], 1);
                        OBEX.Connected := false;
                        Connected := false;
                        result := 0;
                end;
        end;
end;

procedure TMyPlugin.FreeFindData(const AInfo: TFreeFindDataInfo);
var
        i: integer;
begin
        debug(format('FreeFindData(0x%.8X) dir: "%s"', [hPlugin, PhonePath]), 5);
        for i := 0 to AInfo.ItemsNumber - 1 do begin
                StrDispose(AInfo.PanelItem[i].FileName);
        end;
        FreeMem(AInfo.PanelItem);
end;

function TMyPlugin.GetFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; Move: boolean; var DestPath: PFarChar;
  OpMode: TOperationModes): NativeInt;
var
        i, Count, res, CopyBtn, PathItem: integer;
        FDI: array [0 .. 14] of TFarDialogItem;
        hDlg: THandle;
        CopyFrom, Path, LocalName, RemoteName: string;
        ToProcess: boolean;
        Title: PFarChar;
begin
        debug(format('GetFiles(0x%.8x) dest: "%s" items: %d [%d]', [hPlugin, DestPath, ItemsNumber, OpMode]), 5);
        i := 0;
        IsAborted := false;
        Path := FixPath(DestPath);
        try
                case OpMode of
                        OPM_NONE: begin
                                if Move then begin
                                        Title := GetMsg(MF6);
                                        ProgTitle := GetMsg(MMoveTitle);
                                        ProgProcessingFile := GetMsg(MMovingFile);
                                end
                                else begin
                                        Title := GetMsg(MF5);
                                        ProgTitle := GetMsg(MCopyTitle);
                                        ProgProcessingFile := GetMsg(MCopyingFile);
                                end;
                                if ItemsNumber > 1 then
                                        CopyFrom :=
                                          format('%s %d %s %s:', [Title, ItemsNumber, GetMsg(MItems), GetMsg(MTo)])
                                else
                                        CopyFrom := format('%s "%s" %s:', [Title, ppi[0].FileName, GetMsg(MTo)]);
                                Count := 0;
                                FDI[Count] := FarDI(DI_DOUBLEBOX, 3, 1, 57, 8, 0, 0, 0, Title);
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 2, 55, 2, 240, DIF_LEFTTEXT, 0, PFarChar(CopyFrom));
                                inc(Count);
                                FDI[Count] := FarDI(DI_EDIT, 5, 3, 55, 3, 0, DIF_LEFTTEXT, 0, Path);
                                PathItem := Count;
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 4, 0, 4, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 5, 55, 5, 0, DIF_CENTERGROUP, 0, URL_HOMEPAGE);
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 6, 0, 6, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 7, 0, 0, 1, DIF_CENTERGROUP, 1, Title);
                                CopyBtn := Count;
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 7, 0, 0, 0, DIF_CENTERGROUP, 0,
                                  GetMsg(MCancelButton));
                                inc(Count);
                                with Info do begin
                                        hDlg := DialogInit(MAIN_GUID, GET_FILES_DLG_GUID, - 1, - 1, 61, 10, 'hcCopy',
                                          @FDI, Count, 0, 0, nil, 0);
                                        res := DialogRun(hDlg);
                                        Path := FixPath
                                          (StrPas(PChar(SendDlgMessage(hDlg, DM_GETCONSTTEXTPTR, PathItem, nil))));
                                        DialogFree(hDlg);
                                end;
                                ToProcess := res = CopyBtn;
                        end;
                        else begin
                                ToProcess := true;
                                ProgTitle := GetMsg(MReadTitle);
                                ProgProcessingFile := GetMsg(MReadingFile);
                        end;
                end;
                if ToProcess then begin
                        fMax := 0;
                        fPos := 0;
                        tMax := 0;
                        tPos := 0;
                        for i := 0 to ItemsNumber - 1 do begin
                                if (ppi[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                        tMax := tMax +
                                          GetFolderSize(format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]));
                                        if IsAborted then
                                                break;
                                end
                                else begin
                                        tMax := tMax + ppi[i].FileSize;
                                        inc(fMax);
                                end;
                        end;

                        StartTime := Now;
                        for i := 0 to ItemsNumber - 1 do begin
                                if IsAborted then
                                        break;
                                if (ppi[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                        GetFolderContent(format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]),
                                          format('%s%s', [Path, ppi[i].FileName]));
                                        if IsAborted then
                                                break;
                                        if Move then
                                                OBEX.PutFile('',
                                                  format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]), true);
                                end
                                else begin
                                        StartTicks := GetTickCount;
                                        LocalName := format('%s%s', [Path, ppi[i].FileName]);
                                        RemoteName := format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]);
                                        Dest := LocalName;
                                        Src := Opt.DevTitle + ':\' + RemoteName;
                                        debug(format('GetFiles: %s -> %s', [RemoteName, LocalName]), 5);
                                        OBEX.GetFile(LocalName, RemoteName);
                                        if IsAborted then
                                                break;
                                        if Move then
                                                OBEX.PutFile('', RemoteName, true);
                                        tPos := tPos + ppi[i].FileSize;
                                        inc(fPos);
                                end;
                                ppi[i].Flags := ppi[i].Flags and (not PPIF_SELECTED);
                        end;

                        if IsAborted then
                                result := - 1
                        else
                                result := 1;
                end
                else begin
                        result := 0;
                end;
        except
                on e: Exception do begin
                        debug('GetFiles: ' + e.Message, 5);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorGetingFile),
                          PFarChar(string(ppi[i].FileName)), PFarChar(e.Message), integer(MOkButton)], 1);
                        if (OBEX.FLastErrorCode and OBEX_RESPONSE_MASK) = OBEX_RESPONSE_SERVICE_UNAVAILABLE then
                                OBEX.Connected := false;
                        Connected := false;
                        result := 0;
                end;
        end;
end;

function TMyPlugin.SetDirectory(Dir: PFarChar; OpMode: TOperationModes): NativeInt;
{$IFDEF 0}
const
        MAX_LINES = 5;
var
        lines: array [0 .. MAX_LINES] of PFarChar;
        Count, i: integer;
{$ENDIF}
var
        SlashIndex: integer;
begin
        debug(format('SetDirectory(0x%.8x) "%s" -> "%s" [%d]', [hPlugin, PhonePath, Dir, OpMode]), 5);
        // fixed in 2454
        if (PhonePath = '') and (pos('C:\', Dir) = 1) then
                Dir := 'C:';
        if (PhonePath = '') and (pos('D:\', Dir) = 1) then
                Dir := 'D:';
        if (PhonePath = '') and (pos('E:\', Dir) = 1) then
                Dir := 'E:';
        // moving up?
        if Dir = PARENT_FOLDER then begin
{$IFDEF 0}
                // ask for exit from root
                if (PhonePath = '') and (OpMode = OPM_NONE) then begin
                        Count := 0;
                        lines[Count] := GetMsg(MTitle);
                        inc(Count);
                        lines[Count] := PChar(Opt.DevName);
                        inc(Count);
                        lines[Count] := GetMsg(MConfirmExit);
                        inc(Count);
                        if Info.Message(MAIN_GUID, CONFIRM_EXIT_MSG_GUID, FMSG_MB_YESNO, 'hcExit', @lines, Count, 0) = 0
                        then begin
                                result := 1;
                                Connected := false;
                                OBEX.Disconnect;
                                CPrt.OnRxChar := nil;
                                CPrt.Close;
                                USB.Close;
                        end
                        else
                                result := 0;
                end;
{$ENDIF}
                // more then 1 path segment?
                if PhonePath.IndexOf('\') > 0 then begin
                        Delete(PhonePath, LPos('\', PhonePath), length(PhonePath));
                end
                else begin
                        // in root
                        PhonePath := '';
                end;
        end
        else begin
                SlashIndex := pos('\', Dir);
                if SlashIndex = 1 then
                        PhonePath := copy(Dir, 2, length(Dir))
                else if SlashIndex > 0 then
                        PhonePath := Dir
                else begin
                        if PhonePath <> '' then
                                PhonePath := FixPhonePath(PhonePath);
                        PhonePath := PhonePath + Dir;
                end;
        end;
        result := 1;
end;

procedure TMyPlugin.GetOpenPanelInfo(var AInfo: TOpenPanelInfo);
var
        i: integer;
begin
        debug(format('GetOpenPanelInfo(0x%.8X) "%s"', [hPlugin, PhonePath]), 5);
        try
                with AInfo do begin
                        StructSize := SizeOf(TOpenPanelInfo);
                        Flags := OPIF_USEATTRHIGHLIGHTING or OPIF_RAWSELECTION or
                          OPIF_SHOWPRESERVECASE { or OPIF_FINDFOLDERS } or OPIF_ADDDOTS;
                        HostFile := nil;
                        // fixed in 2454
                        if (PhonePath = 'C:') or (PhonePath = 'D:') or (PhonePath = 'E:') then
                                FCurDir := PhonePath + '\'
                        else
                                FCurDir := ROOT_FOLDER + PhonePath;
                        case Opt.DevType of
                                DEVTYPE_USB_SONYERICSSON, DEVTYPE_USB_NOKIA:
                                FPanelTitle := String.format('%s:%s'#00, [Opt.DevName, FCurDir]);
                                else FPanelTitle := String.format('%s:%s'#00, [Opt.ComPort, FCurDir]);
                        end;

                        CurDir := PChar(PhonePath);
                        format := PChar(PhonePath);
                        PanelTitle := PChar(FPanelTitle);
                        i := 0;
                        Self.InfoLines[i].Text := GetMsg(MVersion);
                        Self.InfoLines[i].Data := PChar(VersionInfo.FileVersion);
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MAuthor);
                        Self.InfoLines[i].Data := 'darkmen';
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MDeviceInfo);
                        Self.InfoLines[i].Data := nil;
                        Self.InfoLines[i].Flags := IPLFLAGS_SEPARATOR;
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MDeviceInterface);
                        Self.InfoLines[i].Data := PChar(Opt.DevName);
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MDevicePort);
                        Self.InfoLines[i].Data := PChar(Opt.ComPort);
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MDeviceName);
                        Self.InfoLines[i].Data := PChar(DevName);
                        inc(i);
                        Self.InfoLines[i].Text := 'IMEI';
                        Self.InfoLines[i].Data := PChar(DevIMEI);
                        inc(i);
                        Self.InfoLines[i].Text := GetMsg(MDeviceFW);
                        Self.InfoLines[i].Data := PChar(DevFW);

                        InfoLines := @Self.InfoLines;
                        InfoLinesNumber := HIGH(Self.InfoLines) + 1;
                        DescrFiles := nil;
                        DescrFilesNumber := 0;
                        ShortcutData := nil;
                        DescrFiles := nil;
                        DescrFilesNumber := 0;
                        KeyBar := nil;
                end;
        except
                on e: Exception do begin
                        // silent errors
                        // Message(FMSG_WARNING, '', [integer(MTitle), PFarChar(GetMsg(MUnknownError)),
                        // PFarChar(e.Message), integer(MOkButton)], 1);
                end;
        end;
end;

function TMyPlugin.MakeDirectory(var Dir: PFarChar; OpMode: TOperationModes): NativeInt;
var
        Count, res, MakeBtn, DirItem: integer;
        FDI: array [0 .. 14] of TFarDialogItem;
        hDlg: THandle;
        Path: string;
begin
        debug(format('MakeDirectory(0x%.8x) dir: "%s" [%d]', [hPlugin, Dir, OpMode]), 5);
        IsAborted := false;
        Path := Dir;
        try
                case OpMode of
                        OPM_SILENT: begin
                                OBEX.CreateDir(format('%s\%s\', [PhonePath, Path]))
                        end
                        else begin
                                NewFolderName := '';
                                Count := 0;
                                FDI[Count] := FarDI(DI_DOUBLEBOX, 3, 1, 57, 9, 0, 0, 0, GetMsg(MMakeFolderTitle));
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 2, 55, 2, 240, DIF_LEFTTEXT, 0, GetMsg(MMakingFolder));
                                inc(Count);
                                FDI[Count] := FarDI(DI_EDIT, 5, 3, 55, 3, 1, DIF_LEFTTEXT, 0, Path);
                                DirItem := Count;
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 5, 0, 5, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 6, 55, 6, 0, DIF_CENTERGROUP, 0, URL_HOMEPAGE);
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 7, 0, 7, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 8, 0, 0, 0, DIF_CENTERGROUP, 1,
                                  GetMsg(MContinueButton));
                                MakeBtn := Count;
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 8, 0, 0, 0, DIF_CENTERGROUP, 0,
                                  GetMsg(MCancelButton));
                                inc(Count);
                                with Info do begin
                                        hDlg := DialogInit(MAIN_GUID, MAKE_DIR_DLG_GUID, - 1, - 1, 61, 11,
                                          'hcMakeDirectory', @FDI, Count, 0, 0, nil, 0);
                                        res := DialogRun(hDlg);
                                        Path := StrPas(PChar(SendDlgMessage(hDlg, DM_GETCONSTTEXTPTR, DirItem, nil)));
                                        DialogFree(hDlg);
                                        NewFolderName := Path;
                                        Dir := PFarChar(NewFolderName);
                                end;
                                if (res = MakeBtn) and (length(Path) > 0) then begin
                                        OBEX.CreateDir(format('%s\%s\', [PhonePath, Path]))
                                end;
                        end;
                end; // case
                result := 1;
        except
                on e: Exception do begin
                        debug('MakeDirectory: ' + e.Message, 5);
                        Message(FMSG_WARNING, '',
                          [integer(MTitle), PFarChar(format('%s%s', [GetMsg(MErrorCreateFolder), Dir])),
                          PFarChar(e.Message), integer(MOkButton)], 1);
                        result := 0;
                end;
        end;
end;

function TMyPlugin.PutFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; Move: boolean; SrcPath: PFarChar;
  OpMode: TOperationModes): NativeInt;
var
        i: integer;
        LocalName, RemoteName: string;
begin
        debug(format('PutFiles(0x%.8x) %d from "%s" items to: "%s" [%d]', [hPlugin, ItemsNumber, SrcPath, PhonePath,
          OpMode]), 5);
        i := 0;
        try
                IsAborted := false;
                case OpMode of
                        OPM_NONE: begin
                                if Move then begin
                                        ProgTitle := GetMsg(MMoveTitle);
                                        ProgProcessingFile := GetMsg(MMovingFile);
                                end
                                else begin
                                        ProgTitle := GetMsg(MCopyTitle);
                                        ProgProcessingFile := GetMsg(MCopyingFile);
                                end;
                        end
                        else begin
                                ProgTitle := GetMsg(MReadTitle);
                                ProgProcessingFile := GetMsg(MReadingFile);
                        end;
                end;
                fMax := 0;
                fPos := 0;
                tMax := 0;
                tPos := 0;
                for i := 0 to ItemsNumber - 1 do begin
                        if (ppi[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                tMax := tMax + GetLocalFolderSize(format('%s\%s', [SrcPath, ppi[i].FileName]));
                        end
                        else begin
                                tMax := tMax + ppi[i].FileSize;
                                inc(fMax);
                        end;
                end;

                StartTime := Now;
                for i := 0 to ItemsNumber - 1 do begin
                        if IsAborted then
                                break;
                        if (ppi[i].FileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                                PutFolderContent(format('%s\%s', [SrcPath, ppi[i].FileName]),
                                  format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]), SrcPath, Move);
                                if IsAborted then
                                        break;
                                if Move and (not RemoveDir(format('%s', [SrcPath, ppi[i].FileName]))) then
                                        raise EComPort.CreateMsg
                                          (format('%s: %s', [GetMsg(MErrorDeleteFolder), ppi[i].FileName]),
                                          GetLastError);
                        end
                        else begin
                                StartTicks := GetTickCount;
                                LocalName := format('%s\%s', [SrcPath, ppi[i].FileName]);
                                RemoteName := format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]);
                                Src := LocalName;
                                Dest := Opt.DevTitle + ':\' + RemoteName;
                                debug(format('[PLUGIN] PutFiles: %s -> %s', [RemoteName, LocalName]), 5);
                                OBEX.PutFile(LocalName, RemoteName);
                                if IsAborted then
                                        break;
                                if Move and (not DeleteFileW(PChar(LocalName))) then
                                        raise EComPort.CreateMsg
                                          (format('%s: %s', [GetMsg(MErrorDeleteFile), ppi[i].FileName]), GetLastError);
                                tPos := tPos + ppi[i].FileSize;
                                inc(fPos);
                        end;
                        ppi[i].Flags := ppi[i].Flags and (not PPIF_SELECTED);
                end;
                if IsAborted then
                        result := - 1
                else
                        result := 1;
        except
                on e: Exception do begin
                        debug('PutFiles: ' + e.Message, 5);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorPutingFile),
                          PFarChar(string(ppi[i].FileName)), PFarChar(e.Message), integer(MOkButton)], 1);
                        if (OBEX.FLastErrorCode and OBEX_RESPONSE_MASK) = OBEX_RESPONSE_SERVICE_UNAVAILABLE then begin
                                OBEX.Connected := false;
                                Connected := false;
                        end;
                        result := 0;
                end;
        end;
end;

function TMyPlugin.DeleteFiles(ppi: PPluginPanelItemArray; ItemsNumber: NativeInt; OpMode: TOperationModes): NativeInt;
var
        i, Count, res, DelBtn: integer;
        FDI: array [0 .. 14] of TFarDialogItem;
        hDlg: THandle;
        CopyFrom, Path, RemoteName: string;
        ToProcess: boolean;
begin
        debug(format('DeleteFiles(0x%.8x) %d items from: "%s" [%d]', [hPlugin, ItemsNumber, PhonePath, OpMode]), 5);
        i := 0;
        try
                // ToProcess:=false;
                Path := PhonePath;
                IsAborted := false;
                case OpMode of
                        OPM_NONE: begin
                                ProgTitle := GetMsg(MDeleteTitle);
                                ProgProcessingFile := GetMsg(MDeletingFile);
                                if ItemsNumber > 1 then
                                        CopyFrom :=
                                          format('%s %d %s %s:', [ProgTitle, ItemsNumber, GetMsg(MItems),
                                          GetMsg(MFrom)])
                                else
                                        CopyFrom := format('%s "%s" %s:', [ProgTitle, ppi[0].FileName, GetMsg(MFrom)]);
                                Count := 0;
                                FDI[Count] := FarDI(DI_DOUBLEBOX, 3, 1, 57, 8, 0, 0, 0, GetMsg(MF8));
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 2, 55, 2, 240, DIF_LEFTTEXT, 0, PFarChar(CopyFrom));
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 3, 55, 3, 0, DIF_LEFTTEXT, 0, Path);
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 4, 0, 5, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, 5, 5, 55, 6, 0, DIF_CENTERGROUP, 0, URL_HOMEPAGE);
                                inc(Count);
                                FDI[Count] := FarDI(DI_TEXT, - 1, 6, 0, 7, 0, DIF_SEPARATOR, 0, '');
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 7, 0, 0, 1, DIF_CENTERGROUP, 1, ProgTitle);
                                DelBtn := Count;
                                inc(Count);
                                FDI[Count] := FarDI(DI_BUTTON, 0, 7, 0, 0, 0, DIF_CENTERGROUP, 0,
                                  GetMsg(MCancelButton));
                                inc(Count);
                                with Info do begin
                                        hDlg := DialogInit(MAIN_GUID, DELETE_FILES_DLG_GUID, - 1, - 1, 61, 10,
                                          'hcDelete', @FDI, Count, 0, 0, nil, 0);
                                        res := DialogRun(hDlg);
                                        DialogFree(hDlg);
                                end;
                                ToProcess := res = DelBtn;
                        end;
                        else begin
                                ToProcess := true;
                                ProgTitle := GetMsg(MReadTitle);
                                ProgProcessingFile := GetMsg(MReadingFile);
                        end;
                end;
                if ToProcess then begin
                        fMax := 0;
                        fPos := 0;
                        tMax := 0;
                        tPos := 0;
                        for i := 0 to ItemsNumber - 1 do begin
                                inc(fMax);
                                tMax := tMax + ppi[i].FileSize;
                        end;

                        StartTime := Now;
                        for i := 0 to ItemsNumber - 1 do begin
                                StartTicks := GetTickCount;
                                RemoteName := format('%s%s', [FixPhonePath(PhonePath), ppi[i].FileName]);
                                Dest := '';
                                Src := Opt.DevTitle + ':\' + RemoteName;
                                debug(format('DeleteFile: %s', [RemoteName]), 5);
                                OBEX.OnProgressInit(OBEX, ppi[i].FileSize);
                                if IsAborted then
                                        break;
                                OBEX.PutFile('', RemoteName, true);
                                tPos := tPos + ppi[i].FileSize;
                                inc(fPos);
                                OBEX.OnProgressStep(OBEX, ppi[i].FileSize, ppi[i].FileSize);
                                ppi[i].Flags := ppi[i].Flags and (not PPIF_SELECTED);
                        end;
                        if IsAborted then
                                result := - 1
                        else
                                result := 1;
                end
                else begin
                        result := 0;
                end;
        except
                on e: Exception do begin
                        debug('DeleteFiles: ' + e.Message, 5);
                        Message(FMSG_WARNING, '', [integer(MTitle), integer(MErrorDeleteFile),
                          PFarChar(string(ppi[i].FileName)), PFarChar(e.Message), integer(MOkButton)], 1);
                        result := 0;
                end;
        end;
end;

procedure TMyPlugin.ClosePanel;
begin
        debug(format('ClosePlugin(0x%.8x)', [hPlugin]), 5);
        if Connected then begin
                Connected := false;
                OBEX.Disconnect;
                CPrt.OnRxChar := nil;
                CPrt.Close;
                USB.Close;
        end;
        Free;
end;

initialization

{$IFDEF MSXML}
  CoInitialize(nil);
Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', false);
{$ENDIF}
VersionInfo := TrpVersionInfo.Create(nil);
Dialogs := TDictionary<THandle, POpt>.Create;
// filling MainOpt, global settings for all instances

finalization

Settings.Free;
Dialogs.Free;
VersionInfo.Free;
{$IFDEF MSXML}
CoUninitialize();
{$ENDIF}

end.
