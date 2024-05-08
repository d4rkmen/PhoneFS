unit uObex;

{$I myglobal.inc}

interface

uses
        Windows,
        Classes,
        uSerial,
        uNKUSB,
        SyncObjs;

const
        ObexNoSession: Cardinal                   = Cardinal( - 1);
        ObexFolderBrowserServiceID: AnsiString    = #$F9#$EC#$7B#$C4#$95#$3C#$11#$D2#$98#$4E#$52#$54#$00#$DC#$9E#$09;
        ObexFolderBrowserServiceSS: AnsiString    = #$6B#$01#$CB#$31#$41#$06#$11#$D4#$9A#$77#$00#$50#$DA#$3F#$47#$1F;
        ObexFolderListing: AnsiString             = 'x-obex/folder-listing'#00;
        ObexSyncMLDataSyncXML: AnsiString         = 'application/vnd.syncml+xml';
        ObexSyncMLDataSyncWirelessXML: AnsiString = 'application/vnd.syncml+wbxml';
        ObexSyncMLDevManXML: AnsiString           = 'application/vnd.syncml.dm+xml';
        ObexSyncMLDevManWirelessXML: AnsiString   = 'application/vnd.syncml.dm+wbxml';
        PACKET_TIMEOUT                            = 60000;
        ABORT_TIMEOUT                             = 10000;

        OBEX_HT_TEXT   = $00;
        OBEX_HT_BYTES  = $40;
        OBEX_HT_1_BYTE = $80;
        OBEX_HT_4_BYTE = $C0;

        OBEX_HEADER_ID_COUNT            = $C0;
        OBEX_HEADER_ID_NAME             = $01;
        OBEX_HEADER_ID_TYPE             = $42;
        OBEX_HEADER_ID_LENGTH           = $C3;
        OBEX_HEADER_ID_TIME             = $44;
        OBEX_HEADER_ID_TIME_4           = $C4;
        OBEX_HEADER_ID_DESCRIPTION      = $05;
        OBEX_HEADER_ID_TARGET           = $46;
        OBEX_HEADER_ID_HTTP             = $47;
        OBEX_HEADER_ID_BODY             = $48;
        OBEX_HEADER_ID_END_OF_BODY      = $49;
        OBEX_HEADER_ID_WHO              = $4A;
        OBEX_HEADER_ID_CONNECTION_ID    = $CB;
        OBEX_HEADER_ID_APP_PARAM        = $4C;
        OBEX_HEADER_ID_AUTH_CHALLENGE   = $4D;
        OBEX_HEADER_ID_AUTH_RESPONSE    = $4E;
        OBEX_HEADER_ID_CREATOR_ID       = $CF;
        OBEX_HEADER_ID_WAN_UUID         = $50;
        OBEX_HEADER_ID_OBJECT_CLASS     = $51;
        OBEX_HEADER_ID_SESSION_PARAMS   = $52;
        OBEX_HEADER_ID_SESSION_SEQ      = $93;
        OBEX_HEADER_ID_ACTION_ID        = $94;
        OBEX_HEADER_ID_DEST_NAME        = $15;
        OBEX_HEADER_ID_PERMISSIONS      = $D6;
        OBEX_HEADER_ID_SINGLE_RESP_MODE = $97;
        OBEX_HEADER_ID_SRM_PARAMS       = $98;

        OBEX_OPCODE_FINAL      = $80;
        OBEX_OPCODE_CONNECT    = $00 or OBEX_OPCODE_FINAL;
        OBEX_OPCODE_DISCONNECT = $01 or OBEX_OPCODE_FINAL;
        OBEX_OPCODE_PUT        = $02;
        OBEX_OPCODE_GET        = $03;
        OBEX_OPCODE_RESERVED   = $04;
        OBEX_OPCODE_SETPATH    = $05 or OBEX_OPCODE_FINAL;
        OBEX_OPCODE_ACTION     = $06;
        OBEX_OPCODE_SESSION    = $07 or OBEX_OPCODE_FINAL;
        OBEX_OPCODE_ABORT      = $7F or OBEX_OPCODE_FINAL;

        OBEX_RESPONSE_FINAL                         = $80;
        OBEX_RESPONSE_MASK                          = $7F;
        OBEX_RESPONSE_CONTINUE                      = $10;
        OBEX_RESPONSE_SUCCESS                       = $20;
        OBEX_RESPONSE_CREATED                       = $21;
        OBEX_RESPONSE_ACCEPTED                      = $22;
        OBEX_RESPONSE_NON_AUTHORITATIVE             = $23;
        OBEX_RESPONSE_NO_CONTENT                    = $24;
        OBEX_RESPONSE_RESET_CONTENT                 = $25;
        OBEX_RESPONSE_PARTIAL_CONTENT               = $26;
        OBEX_RESPONSE_MULTIPLE_CHOICES              = $30;
        OBEX_RESPONSE_MOVED_PERMANENTLY             = $31;
        OBEX_RESPONSE_MOVED_TEMPORARILY             = $32;
        OBEX_RESPONSE_SEE_OTHER                     = $33;
        OBEX_RESPONSE_NOT_MODIFIED                  = $34;
        OBEX_RESPONSE_USE_PROXY                     = $35;
        OBEX_RESPONSE_BAD_REQUEST                   = $40;
        OBEX_RESPONSE_UNAUTHORIZED                  = $41;
        OBEX_RESPONSE_PAYMENT_REQUIRED              = $42;
        OBEX_RESPONSE_FORBIDDEN                     = $43;
        OBEX_RESPONSE_NOT_FOUND                     = $44;
        OBEX_RESPONSE_METHOD_NOT_ALLOWED            = $45;
        OBEX_RESPONSE_NOT_ACCEPTABLE                = $46;
        OBEX_RESPONSE_PROXY_AUTHENTICATION_REQUIRED = $47;
        OBEX_RESPONSE_REQUEST_TIMEOUT               = $48;
        OBEX_RESPONSE_CONFLICT                      = $49;
        OBEX_RESPONSE_GONE                          = $4A;
        OBEX_RESPONSE_LENGTH_REQUIRED               = $4B;
        OBEX_RESPONSE_PRECONDITION_FAILED           = $4C;
        OBEX_RESPONSE_REQUEST_ENTITY_TOO_LARGE      = $4D;
        OBEX_RESPONSE_REQUEST_URI_TOO_LARGE         = $4E;
        OBEX_RESPONSE_UNSUPPORTED_MEDIA_TYPE        = $4F;
        OBEX_RESPONSE_INTERNAL_SERVER_ERROR         = $50;
        OBEX_RESPONSE_NOT_IMPLEMENTED               = $51;
        OBEX_RESPONSE_BAD_GATEWAY                   = $52;
        OBEX_RESPONSE_SERVICE_UNAVAILABLE           = $53;
        OBEX_RESPONSE_GATEWAY_TIMEOUT               = $54;
        OBEX_RESPONSE_HTTP_VERSION_NOT_SUPPORTED    = $55;
        OBEX_RESPONSE_DATABASE_FULL                 = $60;
        OBEX_RESPONSE_DATABASE_LOCKED               = $61;

type
        TOnProgressInitEvent = procedure(Sender: TObject; Size: Integer) of object;
        TOnProgressStepEvent = procedure(Sender: TObject; StepPos, Count: Integer) of object;
        TOnProgressFreeEvent = procedure(Sender: TObject; Count: Integer) of object;

        TObexItem = class(TObject)
                private
                protected
                        FPacketData: AnsiString;
                        procedure SetRaw(buffer: AnsiString); virtual;
                        function GetRaw: AnsiString; virtual;
                        function GetPacketData: AnsiString; virtual;
                        procedure SetPacketData(const Value: AnsiString); virtual;
                        function GetPacketLen: Integer; virtual;
                public
                        PacketID: Byte;
                        property PacketLen: Integer
                          read GetPacketLen;
                        property PacketData: AnsiString
                          read GetPacketData
                          write SetPacketData;
                        property Raw: AnsiString
                          read GetRaw
                          write SetRaw;
                        constructor Create(HID: Byte = 0; data: AnsiString = '');
        end;

        TObexName = class(TObexItem)
                protected
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                public
                        name: WideString;
                        constructor Create(nameStr: WideString = '');
        end;

        TObexDate = class(TObexItem)
                protected
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                public
                        date: TDateTime;
                        constructor Create(DateTime: TDateTime = 0);
        end;

        TObexDescription = class(TObexName)
                public
                        constructor Create(descrStr: WideString = '');
                        property descr: WideString
                          read name
                          write name;
        end;

        TObexByteSeq = class(TObexItem)
                protected
                        seqbuffer: AnsiString;
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                public
                        constructor Create(HID: Byte; byteseq: AnsiString = '');
        end;

        TObexTarget = class(TObexByteSeq)
                public
                        constructor Create(targetStr: AnsiString = '');
                        property Target: AnsiString
                          read seqbuffer
                          write seqbuffer;
        end;

        TObexWho = class(TObexByteSeq)
                public
                        constructor Create(whoStr: AnsiString = '');
                        property Who: AnsiString
                          read seqbuffer
                          write seqbuffer;
        end;

        TObexType = class(TObexByteSeq)
                public
                        constructor Create(typeStr: AnsiString = '');
                        property MimeType: AnsiString
                          read seqbuffer
                          write seqbuffer;
        end;

        TObexLength = class(TObexItem)
                protected
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                public
                        Size: Cardinal;
                        constructor Create(s: Cardinal = 0);
        end;

        TObexConnection = class(TObexLength)
                public
                        constructor Create(cid: Cardinal = 0);
                        property ConnectionID: Cardinal
                          read Size
                          write Size;
        end;

        TObexItemList = class(TList)
                protected
                        function GetObexItem(Index: Integer): TObexItem;
                        procedure PutObexItem(Index: Integer; ObexItem: TObexItem);
                public
                        property Items[Index: Integer]: TObexItem
                          read GetObexItem
                          write PutObexItem;
                        destructor Destroy; override;
                        procedure FreeAll;
        end;

        TObexPacket = class(TObexItem)
                private
                        function GetBody: TObexItem;
                protected
                        function GetPacketData: AnsiString; override;
                        procedure SetPacketData(const Value: AnsiString); override;
                        function GetConnectionID: Cardinal;
                        function GetLength: Integer;
                        function GetWho: AnsiString;
                public
                        Child: TObexItemList;
                        property Body: TObexItem
                          read GetBody;
                        constructor Create(HID: Byte = 0; data: AnsiString = '');
                        destructor Destroy; override;
        end;

        TObexSetPath = class(TObexPacket)
                protected
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                        function GetPacketLen: Integer; override;
                public
                        Flags, Constants: Byte;
                        constructor Create(path: WideString = ''; GoUpFirst: boolean = False;
                          DontCreateDir: boolean = True);
        end;

        TObexConnectPacket = class(TObexItem)
                protected
                        procedure SetRaw(buffer: AnsiString); override;
                        function GetRaw: AnsiString; override;
                public
                        ObexVersion: Integer;
                        Flag: Byte;
                        MaxPacketLen: Integer;
                        Target: AnsiString;
                        constructor Create(HID: Byte = 0; MaxLen: Integer = 0; targetStr: AnsiString = '');
        end;

        TObexTargetType = (ocOther, ocSyncML, ocIrmcSync, ocFolderBrowseing);

        TObex = class(TObject)
                private
                        cs: TCriticalSection;
                        FAbort: boolean;
                        FLastReceivedPacket: AnsiString;
                        PacketLen: Integer;
                        FRxBuffer: AnsiString;
                        FPacketsize: Integer;
                        FDir: WideString;
                        ConnID: Cardinal;
                        RcPackets: TStringList;
                        FOnProgressInit: TOnProgressInitEvent;
                        FOnProgressStep: TOnProgressStepEvent;
                        FOnProgressFree: TOnProgressFreeEvent;
                        function CheckForPacket: boolean;
                        procedure GetReceivedObject(var obj: TObexPacket);
                        procedure SendObject(ObexItem: TObexItem); overload;
                        procedure SendObject(HID: Byte = 0; data: AnsiString = ''); overload;
                        procedure DoAbort;
                        procedure ClearRxBuffers;
                protected
                        FIsAborted, SendingData: boolean;
                        TargetType: TObexTargetType;
                public
                        FLastErrorCode: Integer;
                        Connected: boolean;
                        UseWMC: boolean;
                        DevType: Integer;
                        MaxPacketSize: Integer;
                        debugobex: boolean;
                        ComPort: TSerial;
                        USB: TNKUSB;
                        constructor Create;
                        destructor Destroy; override;
                        { For incomming data }
                        procedure OnRxChar(c: AnsiChar);
                        procedure OnRxStr(str: AnsiString);
                        { Connection }
                        procedure Connect(Target: AnsiString = '');
                        procedure Disconnect;
                        { Dangerous! Do not you if you don't know what are you doing! }
                        procedure ForceAbort;
                        { Empty dir changes to root folder }
                        function ChangeDir(name: WideString; DontCreateDir: boolean = True): boolean;
                        // function ChangeDir(name: WideString): boolean;
                        function CreateDir(name: WideString): boolean;
                        { Set folder recursive }
                        function SetDir(name: WideString): boolean;
                        { List folder contents }
                        function List(var Xml: TStringStream): Cardinal;
                        { Returns current LUID of the object if any. If the stream parameter
                          is nil, the object will be deleted. }
                        function PutObject(name: WideString; stream: TStream; progress: boolean = False): WideString;
                        { Returns object size in bytes or 0 on failure. }
                        function GetObject(path: WideString; var where: TMemoryStream; progress: boolean = False)
                          : Cardinal;
                        { Direct calss }
                        procedure PutFile(FileName, objname: WideString; Delete: boolean = False);
                        procedure GetFile(FileName: WideString; objname: WideString = ''; Silent: boolean = False);
                        property OnProgressInit: TOnProgressInitEvent
                          read FOnProgressInit
                          write FOnProgressInit;
                        property OnProgressStep: TOnProgressStepEvent
                          read FOnProgressStep
                          write FOnProgressStep;
                        property OnProgressFree: TOnProgressFreeEvent
                          read FOnProgressFree
                          write FOnProgressFree;
                        property isAborted: boolean
                          read FIsAborted
                          write FIsAborted;
                        property LastErrorCode: Integer
                          read FLastErrorCode;
                        procedure DoOnProgressInit(Count: Integer);
                        procedure DoOnProgressStep(StepPos, Count: Integer);
                        procedure DoOnProgressFree(Count: Integer);
        end;

const
        FMaxLuidLen: Cardinal = 12;

function bytestream2hex(byteStream: AnsiString; seperator: AnsiString = ' '): string;

implementation

uses
        SysUtils,
        maths,
        uMain,
        uDebug,
        pluginw,
        FarLang;

function FileCreateW(const FileName: WideString): Integer;
begin
        Result := Integer(CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
          FILE_ATTRIBUTE_NORMAL, 0));
end;

function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
const
        AccessMode: array [0 .. 2] of LongWord = (GENERIC_READ, GENERIC_WRITE, GENERIC_READ or GENERIC_WRITE);
        ShareMode: array [0 .. 4] of LongWord  = (0, 0, FILE_SHARE_READ, FILE_SHARE_WRITE,
          FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
        Result := - 1;
        if ((Mode and 3) <= fmOpenReadWrite) and (((Mode and $F0) shr 4) <= fmShareDenyNone) then
                Result := Integer(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
                  ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
end;

function ExtractFileName(const FileName: WideString): WideString;
var
        I: Integer;
begin
        I := LPos(PathDelim, FileName);
        Result := Copy(FileName, I + 1, MaxInt);
end;

{ TObex }

function TObex.ChangeDir(name: WideString; DontCreateDir: boolean = True): boolean;
var
        thisPacket: TObexSetPath;
        received: TObexPacket;
        wasconn: boolean;
begin
        wasconn := Connected;
        FIsAborted := False;
        if not Connected then
                Connect; // Start OBEX Mode if it's nessesery
        // FAbortDetected:=False;
        try
                Debug('[OBEX] chdir \' + name, msgtype_details);
                thisPacket := TObexSetPath.Create(name, False, DontCreateDir);
                try
                        SendObject(thisPacket);
                finally
                        thisPacket.Destroy;
                end;

                received := TObexPacket.Create;
                try
                        GetReceivedObject(received);

                        if received.PacketID <> (OBEX_RESPONSE_FINAL or OBEX_RESPONSE_SUCCESS) then
                                raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse),
                                  [bytestream2hex(received.Raw)]);
                        Debug('[OBEX] now in: \' + name, msgtype_details);
                        Result := True;
                finally
                        received.Free;
                end;
        finally
                { Stop our connections only }
                if not wasconn then
                        Disconnect;
        end;
end;

function TObex.SetDir(name: WideString): boolean;
var
        I: Integer;
        NewDir, Dir: WideString;
begin
        NewDir := Copy(name, 1, LPos('\', name));
        Debug(format('[OBEX] SetDir: %s -> %s', [FDir, NewDir]), 5);
        if not SameText(NewDir, FDir) then begin
                ChangeDir('');
                repeat
                        I := Pos('\', name);
                        if I = 0 then
                                break;
                        Dir := Copy(name, 1, I - 1);
                        StringReplace(Dir, #13#10, #10, [rfReplaceAll]);
                        System.Delete(name, 1, I);
                        ChangeDir(Dir);
                until False;
                FDir := NewDir;
        end;
        Result := True;
end;

function TObex.CreateDir(name: WideString): boolean;
var
        I: Integer;
        NewDir, Dir: WideString;
begin
        Debug(format('[OBEX] CreateDir: %s', [name]), 5);
        NewDir := Copy(name, 1, LPos('\', name));
        ChangeDir('');
        repeat
                I := Pos('\', name);
                if I = 0 then
                        break;
                Dir := Copy(name, 1, I - 1);
                Delete(name, 1, I);
                if Pos('\', name) = 0 then
                        ChangeDir(Dir, False)
                else
                        ChangeDir(Dir);
        until False;
        FDir := NewDir;
        Result := True;
end;

function TObex.CheckForPacket: boolean;
var
        s: AnsiString;
begin
        cs.Enter;
        try
                if RcPackets.Count <> 0 then begin
                        s := RcPackets[0];
                        RcPackets.Delete(0);
                        FLastReceivedPacket := s;
                        SendingData := False;
                        Result := True;
                end
                else
                        Result := False;
        finally
                cs.Leave;
        end;
end;

procedure TObex.Connect(Target: AnsiString);
var
        recpackt: TObexPacket;
        sent, received: TObexConnectPacket;
        whoreply: AnsiString;
        ErrorCode: Integer;
begin
        if Connected then
                exit;
        if DevType <> DEVTYPE_USB_NOKIA then
                if not UseWMC then begin
                        if not ComPort.TxAndWait('AT*EOBEX', 'CONNECT', ATCMD_TIMEOUT) then
                                if not ComPort.TxAndWait('AT+CPROT=0', 'CONNECT', ATCMD_TIMEOUT) then begin
                                        // if not ComPort.TxAndWait('AT^SQWE=3', AT_OK,ATCMD_TIMEOUT) then begin
                                        // if not ComPort.TxAndWait('AT+SYNCML=OBEXSTART', '',ATCMD_TIMEOUT) then begin
                                        if ComPort.RxBuffer.Count = 1 then begin
                                                if Pos('ERROR', ComPort.RxBuffer[0]) = 1 then begin
                                                        Debug('[OBEX] connect: ' + ComPort.Port +
                                                          ' interface doesn''t support OBEX protocol!',
                                                          msgtype_importanterror);
                                                        raise Exception.Create(GetMsg(MErrorNoOBEX));
                                                end
                                                else if Pos('NO CARRIER', ComPort.RxBuffer[0]) = 1 then begin
                                                        Debug('[OBEX] connect: OBEX already in use!',
                                                          msgtype_importanterror);
                                                        raise Exception.Create(GetMsg(MErrorOBEXInUse));
                                                end;
                                        end
                                        else begin
                                                Debug('[OBEX] connect: Unknown response: ' + ComPort.RxBuffer.Text,
                                                  msgtype_importanterror);
                                                raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse),
                                                  [ComPort.RxBuffer.Text]);
                                        end;
                                end;
                end;
        try
                FIsAborted := False;
                FAbort := False;
                SendingData := False;
                Connected := True;
                if not Connected or FIsAborted then
                        SysUtils.Abort;
                ClearRxBuffers;
                sent := TObexConnectPacket.Create(OBEX_OPCODE_CONNECT, MaxPacketSize, Target);
                try
                        SendObject(sent);
                finally
                        sent.Free;
                end;
                if FLastReceivedPacket = '' then
                        SysUtils.Abort;
                FLastErrorCode := Integer(FLastReceivedPacket[1]);
                ErrorCode := FLastErrorCode and OBEX_RESPONSE_MASK;
                if ErrorCode = OBEX_RESPONSE_FORBIDDEN then
                        raise Exception.Create(GetMsg(MErrorAccessDenied));
                if ErrorCode = OBEX_RESPONSE_NOT_FOUND then
                        raise Exception.Create(GetMsg(MErrorFBNotSupported));
                if ErrorCode <> OBEX_RESPONSE_SUCCESS then
                        raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse), [bytestream2hex(FLastReceivedPacket)]);

                received := TObexConnectPacket.Create;
                try
                        // get packet size
                        received.Raw := FLastReceivedPacket;
                        FPacketsize := min(MaxPacketSize, received.MaxPacketLen);
                finally
                        received.Free;
                end;
                // process optional headers, remove connect data (6 bytes + opcode)
                Delete(FLastReceivedPacket, 1, 7);
                recpackt := TObexPacket.Create;
                try
                        recpackt.PacketData := FLastReceivedPacket;
                        // get who reply (should be target)
                        whoreply := recpackt.GetWho;
                        // get connection id
                        ConnID := recpackt.GetConnectionID;
                finally
                        recpackt.Free;
                end;
                TargetType := ocOther;
                if whoreply <> '' then begin
                        if whoreply <> Target then
                                raise Exception.CreateFmt(GetMsg(OError_WrongWho), [whoreply]);
                        if (AnsiCompareText(whoreply, ObexFolderBrowserServiceID) = 0) or
                          (AnsiCompareText(whoreply, ObexFolderListing) = 0) then begin
                                // if CompareMem(@whoreply[1], @ObexFolderBrowserServiceID[1], length(whoreply)) then begin
                                whoreply := 'Folder Browsing';
                                TargetType := ocFolderBrowseing;
                        end;
                        if AnsiCompareText(whoreply, 'IRMC-SYNC') = 0 then
                                TargetType := ocIrmcSync;
                        if AnsiCompareText(whoreply, 'SYNCML-SYNC') = 0 then
                                TargetType := ocSyncML;
                        Debug('[OBEX] Connect: Obex Negotiated. Application = ' + whoreply, msgtype_details);
                end;
                if ConnID <> ObexNoSession then
                        Debug('[OBEX] Connect: Obex Negotiated. Connection = ' + IntToStr(ConnID),
                          msgtype_connectcomplete);
        except
                Connected := False;
                raise;
        end;
end;

constructor TObex.Create;
begin
        RcPackets := TStringList.Create;
        cs := TCriticalSection.Create;
        MaxPacketSize := 1024;
        Connected := False;
        ConnID := ObexNoSession;
        FDir := '';
end;

destructor TObex.Destroy;
begin
        if Connected then
                Disconnect;
        cs.Free;
        RcPackets.Free;
        inherited;
end;

procedure TObex.Disconnect;
var
        received: TObexPacket;
begin
        if not Connected then
                exit;
        SendObject(OBEX_OPCODE_DISCONNECT); // Disconnect

        received := TObexPacket.Create;
        try
                GetReceivedObject(received);

                if received.PacketID <> (OBEX_RESPONSE_SUCCESS or OBEX_RESPONSE_FINAL) then // expect 'Sucess'
                        raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse), [bytestream2hex(received.Raw)]);

                ConnID := ObexNoSession;
                Connected := False;
                Debug('[OBEX] disconnect: Obex Session Ended', msgtype_details);
        finally
                received.Free;
        end;
end;

procedure TObex.DoAbort;
begin
        if Connected and not FIsAborted then begin
                ForceAbort;
                { Do not cancel connection attemt }
        end;
end;

function TObex.GetObject(path: WideString; var where: TMemoryStream; progress: boolean): Cardinal;
var
        received, thisPacket: TObexPacket;
        rsize, bsize, sofar: Integer;
        buffer: AnsiString;
        wasconn: boolean;
begin
        rsize := - 2; // set here -3 to prevent size check/detection
        FIsAborted := False;
        wasconn := Connected;
        if not Connected then
                Connect; // Start OBEX Mode if it's nessesery
        try

                try
                        thisPacket := TObexPacket.Create(OBEX_OPCODE_GET or OBEX_OPCODE_FINAL);
                        try
                                if TargetType = ocSyncML then
                                        thisPacket.Child.Add(TObexType.Create(ObexSyncMLDataSyncXML));

                                thisPacket.Child.Add(TObexName.Create(path));
                                SendObject(thisPacket);
                        finally
                                thisPacket.Destroy;
                        end;

                        { Show progress window }

                        sofar := 0;
                        received := TObexPacket.Create;
                        try
                                repeat
                                        try
                                                GetReceivedObject(received);
                                                { check for length header on first packet only }
                                                if (rsize = - 2) then begin
                                                        rsize := received.GetLength;
                                                        if rsize > 0 then
                                                                if progress then
                                                                        DoOnProgressInit(rsize);
                                                end;

                                                if Assigned(received.Body) then
                                                        buffer := received.Body.PacketData
                                                else
                                                        buffer := '';
                                                bsize := length(buffer);
                                                sofar := sofar + bsize;
                                                where.WriteBuffer(buffer[1], bsize);
                                                if progress then
                                                        DoOnProgressStep(sofar, rsize);
                                                if FIsAborted then begin
                                                        Debug('[OBEX] getobject: User calls abort!', msgtype_details);
                                                        ForceAbort;
                                                        break;
                                                end;

                                                // if FIsAborted or (received.PacketID <> $90) then break; // expect continue?
                                                if (received.PacketID <> (OBEX_RESPONSE_CONTINUE or OBEX_RESPONSE_FINAL))
                                                then
                                                        break; // expect continue?

                                                thisPacket := TObexPacket.Create(OBEX_OPCODE_GET or OBEX_OPCODE_FINAL);
                                                try
                                                        if TargetType = ocSyncML then
                                                                thisPacket.Child.Add
                                                                  (TObexType.Create(ObexSyncMLDataSyncXML));
                                                        SendObject(thisPacket);
                                                finally
                                                        thisPacket.Destroy;
                                                end;
                                        except
                                                { we should abort miltipart operations }
                                                on E: Exception do begin
                                                        Debug('[OBEX] getobject: ' + E.Message, msgtype_details);
                                                        if FLastErrorCode < $C0 then
                                                                DoAbort;
                                                        raise;
                                                end;
                                        end;
                                until not Connected or FIsAborted;
                        finally
                                received.Free;
                        end;
                        if rsize > 0 then
                                Debug('[OBEX] getobject: Received ' + path + ' (' + IntToStr(rsize) + ' bytes)',
                                  msgtype_details)
                        else
                                Debug('[OBEX] getobject: Received ' + path, msgtype_details);
                        Result := where.Size;
                        where.Seek(0, soFromBeginning);
                except
                        on E: Exception do begin
                                if FIsAborted then begin
                                        raise;
                                end
                                else if FLastErrorCode = (OBEX_RESPONSE_FORBIDDEN or OBEX_RESPONSE_FINAL) then
                                        raise Exception.Create(GetMsg(MErrorFileNotFound))
                                else
                                        raise;
                        end;
                end;
        finally
                { Stop our connections only }
                if not wasconn then
                        Disconnect;
                DoOnProgressFree(rsize);
        end;
end;

procedure TObex.GetReceivedObject(var obj: TObexPacket);
var
        ErrorCode: Integer;
begin
        if not Connected then
                exit;
        obj.Raw := FLastReceivedPacket;
{$IFDEF DEBUG_IO}
        Debug(format('[OBEX] getreceivedobject: PacketID: %.2x', [obj.PacketID]), 5);
{$ENDIF}
        try
                FLastErrorCode := 0; // TODO: obj.PacketID
                ErrorCode := obj.PacketID and OBEX_RESPONSE_MASK;
                case ErrorCode of
                        OBEX_RESPONSE_BAD_REQUEST: raise Exception.Create(GetMsg(OError_BadRequest));
                        OBEX_RESPONSE_UNAUTHORIZED: raise Exception.Create(GetMsg(OError_Unauthorized));
                        OBEX_RESPONSE_PAYMENT_REQUIRED: raise Exception.Create(GetMsg(OError_PaymentRequired));
                        OBEX_RESPONSE_FORBIDDEN: raise Exception.Create(GetMsg(OError_Forbidden));
                        OBEX_RESPONSE_NOT_FOUND: raise Exception.Create(GetMsg(OError_NotFound));
                        OBEX_RESPONSE_METHOD_NOT_ALLOWED: raise Exception.Create(GetMsg(OError_MethodNotAllowed));
                        OBEX_RESPONSE_NOT_ACCEPTABLE: raise Exception.Create(GetMsg(OError_NotAcceptable));
                        OBEX_RESPONSE_PROXY_AUTHENTICATION_REQUIRED:
                        raise Exception.Create(GetMsg(OError_ProxyAuthenticationRequired));
                        OBEX_RESPONSE_REQUEST_TIMEOUT: raise Exception.Create(GetMsg(OError_RequestTimeout));
                        OBEX_RESPONSE_CONFLICT: raise Exception.Create(GetMsg(OError_Conflict));
                        OBEX_RESPONSE_GONE: raise Exception.Create(GetMsg(OError_Gone));
                        OBEX_RESPONSE_LENGTH_REQUIRED: raise Exception.Create(GetMsg(OError_LengthRequired));
                        OBEX_RESPONSE_PRECONDITION_FAILED: raise Exception.Create(GetMsg(OError_PreconditionFailed));
                        OBEX_RESPONSE_REQUEST_ENTITY_TOO_LARGE:
                        raise Exception.Create(GetMsg(OError_RequestedEntityTooLarge));
                        OBEX_RESPONSE_REQUEST_URI_TOO_LARGE:
                        raise Exception.Create(GetMsg(OError_RequestedURLTooLarge));
                        OBEX_RESPONSE_UNSUPPORTED_MEDIA_TYPE:
                        raise Exception.Create(GetMsg(OError_UnsupportedMediaType));
                        OBEX_RESPONSE_INTERNAL_SERVER_ERROR: raise Exception.Create(GetMsg(OError_InternalServerError));
                        OBEX_RESPONSE_NOT_IMPLEMENTED: raise Exception.Create(GetMsg(OError_NotImplemented));
                        OBEX_RESPONSE_BAD_GATEWAY: raise Exception.Create(GetMsg(OError_BadGateway));
                        OBEX_RESPONSE_SERVICE_UNAVAILABLE: raise Exception.Create(GetMsg(OError_ServiceUnavailable));
                        OBEX_RESPONSE_GATEWAY_TIMEOUT: raise Exception.Create(GetMsg(OError_GatewayTimeout));
                        OBEX_RESPONSE_HTTP_VERSION_NOT_SUPPORTED:
                        raise Exception.Create(GetMsg(OError_HTTPVersionNotSupported));
                        OBEX_RESPONSE_DATABASE_FULL: raise Exception.Create(GetMsg(OError_ObjectOverflow));
                        OBEX_RESPONSE_DATABASE_LOCKED: raise Exception.Create(GetMsg(OError_ObjectIsInUse));
                end;
        except
                SendingData := False;
                FLastErrorCode := obj.PacketID;
                // FAbort:=True;
                raise;
        end;
end;

function TObex.List(var Xml: TStringStream): Cardinal;
var
        thisPacket, received: TObexPacket;
        wasconn: boolean;
begin
        Debug('[OBEX] list: Starting...', msgtype_details);
        Xml.Size := 0;
        FIsAborted := False;
        wasconn := Connected;
        if not Connected then
                Connect; // Start OBEX Mode if it's nessesery
        try
                thisPacket := TObexPacket.Create(OBEX_OPCODE_FINAL or OBEX_OPCODE_GET);
                try
                        thisPacket.Child.Add(TObexItem.Create(OBEX_HEADER_ID_NAME, ''));
                        thisPacket.Child.Add(TObexItem.Create(OBEX_HEADER_ID_TYPE, 'x-obex/folder-listing'#00));
                        SendObject(thisPacket);
                finally
                        thisPacket.Destroy;
                end;
                try
                        received := TObexPacket.Create;
                        try
                                repeat
                                        try
                                                GetReceivedObject(received);
                                                try
                                                        Xml.WriteString(received.Body.PacketData);
                                                except
                                                end;
                                                if FIsAborted or
                                                  (received.PacketID <> (OBEX_RESPONSE_FINAL or OBEX_RESPONSE_CONTINUE))
                                                then
                                                        break;
                                                SendObject(OBEX_OPCODE_FINAL or OBEX_OPCODE_GET);
                                        except
                                                { we should abort miltipart operations }
                                                if FLastErrorCode < $C0 then
                                                        DoAbort;
                                                raise;
                                        end;
                                until not Connected or FIsAborted;
                                Debug('[OBEX] list: Complete', msgtype_details);
                        finally
                                received.Free;
                        end;

                        Result := length(Xml.DataString);
                        Xml.Seek(0, soFromBeginning);
                except
                        on E: Exception do begin
                                Debug('[OBEX] list: ' + E.Message, msgtype_importanterror);
                                if FIsAborted then begin
                                        Debug('[OBEX] list: Aborted by user!', msgtype_details);
                                        raise;
                                end
                                else
                                        raise Exception.Create('OBEX ListDir Failed: ' + E.Message);
                        end;
                end;
        finally
                { Stop our connections only }
                if not wasconn then
                        Disconnect;
        end;
end;

procedure TObex.OnRxStr(str: AnsiString);
var
        I: Integer;
begin
        cs.Enter;
        try
                for I := 1 to length(str) do
                        OnRxChar(str[I]);
        finally
                cs.Leave;
        end;
end;

procedure TObex.OnRxChar(c: AnsiChar);
begin
        FRxBuffer := FRxBuffer + c;

        if length(FRxBuffer) < 3 then
                PacketLen := - 1
        else if (PacketLen = - 1) and (length(FRxBuffer) > 2) then
                PacketLen := (Byte(FRxBuffer[2]) shl 8) or Byte(FRxBuffer[3]);

        if length(FRxBuffer) = PacketLen then begin
                RcPackets.Add(FRxBuffer);
                FRxBuffer := '';
                PacketLen := - 1;
        end;
end;

procedure TObex.PutFile(FileName, objname: WideString; Delete: boolean);
var
        stream: TFileStream;
        I: Integer;
begin
        Debug(format('[OBEX] putfile: %s ', [objname]), msgtype_details);
        if Delete then begin
                SetDir(objname);
                repeat
                        I := Pos('\', objname);
                        if I = 0 then
                                break;
                        System.Delete(objname, 1, I);
                until False;
                PutObject(objname, nil);
        end
        else begin
                stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone, 0);
                try
                        SetDir(objname);
                        repeat
                                I := Pos('\', objname);
                                if I = 0 then
                                        break;
                                System.Delete(objname, 1, I);
                        until False;
                        PutObject(objname, stream, True);
                finally
                        stream.Free;
                end;
        end;
end;

procedure TObex.GetFile(FileName, objname: WideString; Silent: boolean);
var
        str: TMemoryStream;
        stream: TFileStream;
        I: Integer;
begin
        if objname = '' then
                objname := ExtractFileName(FileName);
        SetDir(objname);
        repeat
                I := Pos('\', objname);
                if I = 0 then
                        break;
                Delete(objname, 1, I);
        until False;
        str := TMemoryStream.Create;
        try
                Debug('[OBEX] getfile: ' + objname, msgtype_details);
                GetObject(objname, str, not Silent);
                { Create file only on success }
                stream := TFileStream.Create(FileName, fmCreate, 0);
                try
                        stream.CopyFrom(str, str.Size)
                finally
                        stream.Free;
                end;
        finally
                str.Free;
        end;
end;

function TObex.PutObject(name: WideString; stream: TStream; progress: boolean): WideString;
var
        received, thisPacket: TObexPacket;
        emptySlot: Integer;
        buffer: AnsiString;
        wasconn: boolean;
begin
        Result := '';
        FIsAborted := False;
        wasconn := Connected;
        if not Connected then
                Connect; // Start OBEX Mode if it's nessesery
        try
                { nil means put null packet and delete the entries }
                if stream = nil then begin
                        try
                                Debug('[OBEX] putobject: Deleting ' + name, msgtype_details);
                                thisPacket := TObexPacket.Create(OBEX_OPCODE_PUT or OBEX_OPCODE_FINAL);
                                try
                                        thisPacket.Child.Add(TObexName.Create(name));
                                        SendObject(thisPacket);
                                finally
                                        thisPacket.Destroy;
                                end;

                                received := TObexPacket.Create;
                                try
                                        GetReceivedObject(received);

                                        if received.PacketID <> (OBEX_RESPONSE_SUCCESS or OBEX_RESPONSE_FINAL) then
                                                raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse),
                                                  [bytestream2hex(received.Raw)]);

                                        // For Sync i need only the last row (LUID) :)
                                        if received.Body <> nil then
                                                Result := Copy(received.Body.PacketData, 3, FMaxLuidLen);
                                finally
                                        received.Free;
                                end;
                                Debug('[OBEX] putobject: Deleted ' + name, msgtype_details);
                                { Now exit }
                                exit;
                        except
                                on E: Exception do begin
                                        Debug('[OBEX] putobject: Failed to delete: ' + E.Message,
                                          msgtype_importanterror);
                                        raise; // Exception.Create(e.Message);
                                end;
                        end;
                end;

                { Show progress window }
                if progress then
                        DoOnProgressInit(stream.Size);
                Debug('[OBEX] putobject: Sending ' + name + ' (' + IntToStr(stream.Size) + ' bytes)', msgtype_details);
                try
                        stream.Seek(0, soFromBeginning);

                        // put request
                        while Connected and ((stream.Position < stream.Size) or (stream.Size = 0)) and
                          (not FIsAborted) do
                                try
                                        thisPacket := TObexPacket.Create(OBEX_OPCODE_PUT);
                                        try
                                                if TargetType = ocSyncML then
                                                        thisPacket.Child.Add(TObexType.Create(ObexSyncMLDataSyncXML));

                                                // add name for first packet
                                                if stream.Position = 0 then begin
                                                        thisPacket.Child.Add(TObexName.Create(name));
                                                        thisPacket.Child.Add(TObexLength.Create(stream.Size));
                                                        thisPacket.Child.Add(TObexDate.Create(Now));
                                                        thisPacket.Child.Add(TObexWho.Create(GetMsg(MTitle)));
                                                end;
                                                emptySlot := FPacketsize - thisPacket.PacketLen - 3 - 5;

                                                SetLength(buffer, emptySlot);
                                                SetLength(buffer, stream.Read(buffer[1], emptySlot));

                                                thisPacket.Child.Add(TObexItem.Create(OBEX_HEADER_ID_BODY, buffer));
                                                SendObject(thisPacket);
                                        finally
                                                thisPacket.Destroy;
                                        end;
                                        if progress then
                                                DoOnProgressStep(stream.Position, stream.Size);
                                        received := TObexPacket.Create;
                                        try
                                                GetReceivedObject(received);
                                                if FIsAborted then
                                                        break;
                                                if received.PacketID <> (OBEX_RESPONSE_CONTINUE or OBEX_RESPONSE_FINAL)
                                                then // expect 'continue'
                                                        raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse),
                                                          [bytestream2hex(received.Raw)]);
                                        finally
                                                received.Free;
                                        end;
                                        if stream.Size = 0 then
                                                break;
                                except
                                        { we should abort miltipart operations }
                                        on E: Exception do begin
                                                Debug('[OBEX] getobject: ' + E.Message, msgtype_details);
                                                if FLastErrorCode < $C0 then
                                                        DoAbort;
                                                raise;
                                        end;
                                end;

                        { bookmarks and etc. ask user to keep file _after_ file transfer }
                        // Check your phone for instructions...

                        thisPacket := TObexPacket.Create(OBEX_OPCODE_PUT or OBEX_OPCODE_FINAL); // final put
                        try
                                if TargetType = ocSyncML then
                                        thisPacket.Child.Add(TObexType.Create(ObexSyncMLDataSyncXML));

                                thisPacket.Child.Add(TObexItem.Create(OBEX_HEADER_ID_END_OF_BODY));

                                SendObject(thisPacket);
                        finally
                                thisPacket.Destroy;
                        end;

                        received := TObexPacket.Create;
                        try
                                GetReceivedObject(received);

                                if received.PacketID <> (OBEX_RESPONSE_SUCCESS or OBEX_RESPONSE_FINAL) then
                                        // expect 'success'
                                        raise Exception.CreateFmt(GetMsg(MErrorInvalidResponse),
                                          [bytestream2hex(received.Raw)]);
                                // For Sync need only the last row (LUID)
                                if received.Body <> nil then
                                        Result := Copy(received.Body.PacketData, 3, FMaxLuidLen);
                                Debug('[OBEX] putobject: Sent ' + name + ' (' + IntToStr(stream.Size) + ' bytes)',
                                  msgtype_details);
                        finally
                                received.Free;
                        end;
                except
                        on E: Exception do begin
                                Debug('[OBEX] putobject: Error sending ' + name + ': ' + E.Message,
                                  msgtype_importanterror);
                                if FIsAborted then begin
                                        Debug('[OBEX] putobject: Aborted by user', msgtype_details);
                                        raise;
                                end
                                else begin
                                        if (FLastErrorCode and OBEX_RESPONSE_MASK) = OBEX_RESPONSE_DATABASE_FULL then
                                                raise Exception.Create(GetMsg(MErrorNoSpace))
                                        else
                                                raise Exception.CreateFmt(GetMsg(MErrorOBEXPutObject), [E.Message]);
                                end;
                        end;
                end;
        finally
                { Stop our connections only }
                if not wasconn then
                        Disconnect;
                if progress then
                        DoOnProgressFree(stream.Size);
        end;
end;

//
procedure TObex.SendObject(ObexItem: TObexItem);
const
        Aborting: boolean = False;
var
        sid: TObexConnection;
        Raw: AnsiString;
        STime: dword;
begin
        FLastErrorCode := 0;
        FLastReceivedPacket := '';

        if (ConnID <> ObexNoSession) then begin
                sid := TObexConnection.Create(ConnID);
                try
                        // set conn id as a first header
                        ObexItem.PacketData := sid.Raw + ObexItem.PacketData;
                finally
                        sid.Free;
                end;
        end;
        Raw := ObexItem.Raw;

        // TODO: Add semafore use here and WaitForSingleObject....
        while not isAborted and SendingData do begin
                if CheckForPacket then
                        break;
                Sleep(1);
        end;
{$IFDEF DEBUG_IO}
        Debug(format('[TX] %.4x: %s', [length(Raw), bytestream2hex(Raw)]), msgtype_details);
{$ENDIF}
        SendingData := True;

        if DevType = DEVTYPE_USB_NOKIA then
                USB.WriteStr(Raw)
        else
                ComPort.WriteStr(Raw);
        STime := GetTickCount;
        while Connected and (FLastReceivedPacket = '') and (Aborting or not FAbort) do begin
                if CheckForPacket then
                        break;
                Sleep(1);
                if (GetTickCount - STime) > PACKET_TIMEOUT then
                        raise Exception.Create(GetMsg(OError_TimedOut));
                if FAbort then begin
                        Aborting := False;
                        FAbort := True;
                end;
        end;

        if Connected and not Aborting and FAbort then begin
                Aborting := True;
                try
                        FAbort := False;
                        DoAbort;
                finally
                        Aborting := False;
                end;
                Debug('[OBEX] sendobject: Aborted by user', msgtype_details);
                SysUtils.Abort;
        end;
end;

procedure TObex.SendObject(HID: Byte; data: AnsiString);
var
        item: TObexItem;
begin
        item := TObexItem.Create(HID, data);
        try
                SendObject(item);
        finally
                item.Free;
        end;
end;

procedure TObex.ClearRxBuffers;
begin
        FRxBuffer := '';
        RcPackets.Clear;
end;

procedure TObex.ForceAbort;
var
        tm: Cardinal;
        ErrorCode: Integer;
begin
        FIsAborted := True;
        // Should we remove this command complete wait loop.... ?
        tm := GetTickCount;
        while SendingData do begin
                if CheckForPacket then
                        break;
                Sleep(1);
                if Abs(GetTickCount - tm) > ABORT_TIMEOUT then
                        SendingData := False;
        end;
        // Aborting...
        Debug('[OBEX] Aborting...', msgtype_details);
        ClearRxBuffers;
        SendObject(OBEX_OPCODE_ABORT);
        ErrorCode := Integer(FLastReceivedPacket[1]);
        if (FLastReceivedPacket = '') or (ErrorCode <> (OBEX_RESPONSE_SUCCESS or OBEX_RESPONSE_FINAL)) then
                Disconnect;
        // FAbort:=true;
end;

procedure TObex.DoOnProgressInit(Count: Integer);
begin
        if Assigned(FOnProgressInit) then
                FOnProgressInit(self, Count);
end;

procedure TObex.DoOnProgressStep(StepPos, Count: Integer);
begin
        if Assigned(FOnProgressStep) then
                FOnProgressStep(self, StepPos, Count);
end;

procedure TObex.DoOnProgressFree(Count: Integer);
begin
        if Assigned(FOnProgressFree) then
                FOnProgressFree(self, Count);
end;

{ TObexPacket }

constructor TObexItem.Create(HID: Byte; data: AnsiString);
begin
        PacketID := HID;
        PacketData := data;
end;

function TObexItem.GetPacketData: AnsiString;
begin
        Result := FPacketData;
end;

function TObexItem.GetPacketLen: Integer;
begin
        Result := length(PacketData) + 3;
end;

function TObexItem.GetRaw: AnsiString;
var
        lenHigh, lenLow: Byte;
begin
        lenHigh := (PacketLen and $FF00) shr 8;
        lenLow := PacketLen and $00FF;

        Result := AnsiChar(PacketID) + AnsiChar(lenHigh) + AnsiChar(lenLow) + PacketData;
end;

procedure TObexItem.SetPacketData(const Value: AnsiString);
begin
        FPacketData := Value;
end;

procedure TObexItem.SetRaw(buffer: AnsiString);
begin
        try
                PacketID := Byte(buffer[1]);
                PacketData := Copy(buffer, 4, length(buffer) - 3);
        except
                PacketID := 0;
                PacketData := '';
        end;
end;

{ TPutRequestPacket }

constructor TObexConnectPacket.Create(HID: Byte; MaxLen: Integer; targetStr: AnsiString);
begin
        PacketID := HID;
        ObexVersion := $10;
        Flag := $00;
        MaxPacketLen := MaxLen;
        Target := targetStr;
end;

function TObexConnectPacket.GetRaw: AnsiString;
var
        lenHigh, lenLow: Byte;
        obexTarget: TObexTarget;
begin
        lenHigh := (MaxPacketLen and $FF00) shr 8;
        lenLow := MaxPacketLen and $00FF;
        PacketData := AnsiChar(ObexVersion) + AnsiChar(Flag) + AnsiChar(lenHigh) + AnsiChar(lenLow);

        if Target <> '' then begin
                obexTarget := TObexTarget.Create(Target);
                try
                        PacketData := PacketData + obexTarget.Raw;
                finally
                        obexTarget.Free;
                end;
        end;

        Result := Inherited GetRaw;
end;

procedure TObexConnectPacket.SetRaw(buffer: AnsiString);
begin
        Inherited SetRaw(buffer);

        if length(PacketData) >= 4 then begin
                ObexVersion := Byte(PacketData[1]);
                Flag := Byte(PacketData[2]);
                MaxPacketLen := (Byte(PacketData[3]) shl 8) or Byte(PacketData[4]);
        end;
end;

{ TObexPacket }

constructor TObexPacket.Create(HID: Byte; data: AnsiString);
begin
        Child := TObexItemList.Create;
        inherited Create(HID, data);
end;

destructor TObexPacket.Destroy;
begin
        Child.Free;
end;

function TObexPacket.GetBody: TObexItem;
var
        I: Integer;
begin
        Result := nil;

        for I := 0 to Child.Count - 1 do begin
                if (Child.Items[I].PacketID in [OBEX_HEADER_ID_BODY, OBEX_HEADER_ID_END_OF_BODY,
                  OBEX_HEADER_ID_APP_PARAM])
                // OBEX_HEADER_ID_LENGTH
                // OBEX_HEADER_ID_TIME
                then begin
                        Result := Child.Items[I];
                end;
        end;
end;

function TObexPacket.GetPacketData: AnsiString;
var
        I: Integer;
begin
        Result := '';

        for I := 0 to Child.Count - 1 do begin
                Result := Result + Child.Items[I].Raw;
        end;
end;

function TObexPacket.GetConnectionID: Cardinal;
var
        I: Integer;
begin
        Result := ObexNoSession;
        for I := 0 to Child.Count - 1 do
                if Child.Items[I].ClassType = TObexConnection then begin
                        Result := (Child.Items[I] as TObexConnection).ConnectionID;
                        break;
                end;
end;

procedure TObexPacket.SetPacketData(const Value: AnsiString);
var
        buffer: AnsiString;
        itemlen: Integer;
        ObexItem: TObexItem;
        HT, HI: Byte;
begin
        Child.FreeAll;
        buffer := Value;
        while buffer <> '' do begin
                itemlen := 0;
                HI := ord(buffer[1]);
                HT := (HI and $C0); // shr 6;
                case HT of
                        OBEX_HT_TEXT: begin
                                // null terminated Unicode text, length prefixed with 2 byte unsigned integer
                                itemlen := (Byte(buffer[2]) shl 8) or Byte(buffer[3]);
                        end;
                        OBEX_HT_BYTES: begin // byte sequence, length prefixed with 2 byte unsigned integer
                                itemlen := (Byte(buffer[2]) shl 8) or Byte(buffer[3]);
                        end;
                        OBEX_HT_1_BYTE: begin // 1 byte quantity
                                itemlen := 2;
                        end;
                        OBEX_HT_4_BYTE: begin
                                // 4 byte quantity – transmitted in network byte order (high byte first)
                                itemlen := 5;
                        end;
                end;

                // TODO: Add more header ID support here.
                case HI of
                        OBEX_HEADER_ID_NAME: ObexItem := TObexName.Create;
                        OBEX_HEADER_ID_DESCRIPTION: ObexItem := TObexDescription.Create;
                        OBEX_HEADER_ID_TIME: ObexItem := TObexDate.Create;
                        OBEX_HEADER_ID_TARGET: ObexItem := TObexTarget.Create;
                        OBEX_HEADER_ID_WHO: ObexItem := TObexWho.Create;
                        OBEX_HEADER_ID_LENGTH: ObexItem := TObexLength.Create;
                        OBEX_HEADER_ID_CONNECTION_ID: ObexItem := TObexConnection.Create;
                        else ObexItem := TObexItem.Create;
                end;
                ObexItem.Raw := Copy(buffer, 1, itemlen);
                Child.Add(ObexItem);
                Delete(buffer, 1, itemlen);
        end;
end;

function TObexPacket.GetWho: AnsiString;
var
        I: Integer;
begin
        Result := '';
        for I := 0 to Child.Count - 1 do
                if Child.Items[I].ClassType = TObexWho then begin
                        Result := (Child.Items[I] as TObexWho).Who;
                        break;
                end;
end;

function TObexPacket.GetLength: Integer;
var
        I: Integer;
begin
        Result := - 1;
        for I := 0 to Child.Count - 1 do
                if Child.Items[I].ClassType = TObexLength then begin
                        Result := (Child.Items[I] as TObexLength).Size;
                        break;
                end;
end;

constructor TObexDate.Create(DateTime: TDateTime);
begin
        inherited Create(OBEX_HEADER_ID_TIME);
        date := DateTime;
end;

function TObexDate.GetRaw: AnsiString;
var
        tmp: AnsiString;
begin
        PacketData := '';
        tmp := FormatDateTime('yyyymmdd', date) + 'T' + FormatDateTime('hhmmss', date);
        PacketData := PacketData + tmp;
        Result := inherited GetRaw;
end;

procedure TObexDate.SetRaw(buffer: AnsiString);
begin
        inherited SetRaw(buffer);
        date := Now;
        // PacketData
end;

{ TObexName }

constructor TObexName.Create(nameStr: WideString);
begin
        inherited Create(OBEX_HEADER_ID_NAME);
        name := nameStr;
end;

function TObexName.GetRaw: AnsiString;
var
        I, c: Integer;
begin
        PacketData := '';
        for I := 1 to length(name) do begin
                c := ord(name[I]);
                PacketData := PacketData + AnsiChar((c and $FF00) shr 8) + AnsiChar(c and $00FF);
        end;

        // null terminated (if not empty, and if needed)
        if (PacketData <> '') and (Copy(PacketData, length(PacketData) - 1, 2) <> #00#00) then
                PacketData := PacketData + #00#00;

        Result := inherited GetRaw;
end;

procedure TObexName.SetRaw(buffer: AnsiString);
var
        I: Integer;
begin
        inherited SetRaw(buffer);

        name := '';
        for I := 0 to round(PacketLen / 2) - 3 do
                name := name + WideChar((ord(PacketData[(I * 2) + 1]) shl 8) or ord(PacketData[(I * 2) + 2]));
end;

{ TObexLength }

constructor TObexLength.Create(s: Cardinal);
begin
        inherited Create(OBEX_HEADER_ID_LENGTH);
        Size := s;
end;

function TObexLength.GetRaw: AnsiString;
begin
        PacketData := AnsiChar((Size and $FF000000) shr 24) + AnsiChar((Size and $00FF0000) shr 16) +
          AnsiChar((Size and $0000FF00) shr 8) + AnsiChar(Size and $000000FF);

        Result := AnsiChar(PacketID) + PacketData;
end;

procedure TObexLength.SetRaw(buffer: AnsiString);
var
        c: Cardinal;
begin
        PacketID := Byte(buffer[1]);

        c := (Cardinal(buffer[2]) shl 24) or (Cardinal(buffer[3]) shl 16) or (Cardinal(buffer[4]) shl 8) or
          Cardinal(buffer[5]);
        Size := c;
end;

{ Global }

function bytestream2hex(byteStream, seperator: AnsiString): string;
var
        I: Integer;
begin
        Result := '';
        for I := 1 to length(byteStream) do
                Result := Result + IntToHex(Byte(byteStream[I]), 2) + seperator;
end;

{ TObexItemList }

destructor TObexItemList.Destroy;
begin
        FreeAll;
        inherited;
end;

procedure TObexItemList.FreeAll;
var
        Index: Integer;
begin
        for Index := 0 to Count - 1 do
                Items[Index].Free;
        Clear;
end;

function TObexItemList.GetObexItem(Index: Integer): TObexItem;
begin
        Result := TObexItem(inherited Items[Index]);
end;

procedure TObexItemList.PutObexItem(Index: Integer; ObexItem: TObexItem);
begin
        inherited Items[Index] := ObexItem;
end;

{ TObexConnID }

constructor TObexConnection.Create(cid: Cardinal);
begin
        inherited Create(cid);
        PacketID := OBEX_HEADER_ID_CONNECTION_ID;
end;

{ TObexDescription }

constructor TObexDescription.Create(descrStr: WideString);
begin
        inherited Create(descrStr);
        PacketID := OBEX_HEADER_ID_DESCRIPTION;
end;

{ TObexSetPath }

constructor TObexSetPath.Create(path: WideString; GoUpFirst, DontCreateDir: boolean);
begin
        Constants := 0;
        Flags := 0;
        if GoUpFirst then
                Flags := Flags or 1; // set bit 0
        if DontCreateDir then
                Flags := Flags or 2; // set bit 1
        inherited Create(OBEX_OPCODE_SETPATH or OBEX_OPCODE_FINAL);
        Child.Add(TObexName.Create(path));
end;

function TObexSetPath.GetPacketLen: Integer;
begin
        Result := inherited GetPacketLen + 2;
end;

function TObexSetPath.GetRaw: AnsiString;
var
        lenHigh, lenLow: Byte;
begin
        lenHigh := (PacketLen and $FF00) shr 8;
        lenLow := PacketLen and $00FF;
        { Add Flags and Constants }
        Result := AnsiChar(PacketID) + AnsiChar(lenHigh) + AnsiChar(lenLow) + AnsiChar(Flags) + AnsiChar(Constants) +
          PacketData;
end;

procedure TObexSetPath.SetRaw(buffer: AnsiString);
begin
        PacketID := Byte(buffer[1]);
        Flags := Byte(buffer[4]);
        Constants := Byte(buffer[5]);
        PacketData := Copy(buffer, 6, length(buffer) - 5);
end;

{ TObexByteSeq }

constructor TObexByteSeq.Create(HID: Byte; byteseq: AnsiString);
begin
        inherited Create(HID);
        seqbuffer := byteseq;
end;

function TObexByteSeq.GetRaw: AnsiString;
begin
        PacketData := seqbuffer;
        Result := inherited GetRaw;
end;

procedure TObexByteSeq.SetRaw(buffer: AnsiString);
begin
        inherited SetRaw(buffer);
        seqbuffer := PacketData;
end;

{ TObexTarget }

constructor TObexTarget.Create(targetStr: AnsiString);
begin
        inherited Create(OBEX_HEADER_ID_TARGET, targetStr);
end;

{ TObexWho }

constructor TObexWho.Create(whoStr: AnsiString);
begin
        inherited Create(OBEX_HEADER_ID_WHO, whoStr);
end;

{ TObexType }

constructor TObexType.Create(typeStr: AnsiString);
begin
        { Make sure type is null terminated }
        if (typeStr <> '') and (typeStr[length(typeStr)] <> #0) then
                typeStr := typeStr + #0;
        inherited Create(OBEX_HEADER_ID_TYPE, typeStr);
end;

end.
