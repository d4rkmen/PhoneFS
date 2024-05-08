library phonefs;

{$I myglobal.inc}
{$E dll}
{$R *.RES}

uses
        Windows,
        Sysutils,
        Classes,
        FarLang in 'FarLang.pas',
        uObex in 'uObex.pas',
        uSerial in 'uSerial.pas',
        uMain in 'uMain.pas',
        uDebug in 'uDebug.pas',
        pluginw in 'pluginw.pas',
        uNKUSB in 'uNKUSB.pas',
        maths in 'maths.pas',
        setupapi in 'setupapi.pas',
        PluginSettings in 'PluginSettings.pas';

const
        PORTS_SONYERICSSON = 2;
        PORTS_NOKIA        = 3;
        PORTS_SERIAL       = 4;
        PORTS_BLUETOOTH    = 4;
        PORTS_MODEM        = 5;
        PORTS_ALL          = 6;

        SETTINGS_ADD_TO_DISKS   = 'AddToDisksMenu';
        SETTINGS_ADD_TO_PLUGINS = 'AddToPluginsMenu';

var
        PluginStrings: array [0 .. 0] of PFarChar;

procedure GetPluginInfoW(var APluginInfo: TPluginInfo);
stdcall export;
begin
        debug(format('GetPluginInfoW(0x%.8P)', [@APluginInfo]), 5);
        with APluginInfo do begin
                StructSize := SizeOf(TPluginInfo);
                Flags := 0;

                PluginStrings[0] := GetMsg(MTitle);

                debug(format('GetPluginInfoW: %s', [PluginStrings[0]]), 5);
                if MainOpt.AddToDisksMenu then begin
                        DiskMenu.Guids := @MENU_GUID;
                        DiskMenu.Strings := @PluginStrings;
                        DiskMenu.Count := 1;
                end
                else begin
                        DiskMenu.Strings := nil;
                        DiskMenu.Guids := nil;
                        DiskMenu.Count := 0;
                end;

                if MainOpt.AddToPluginsMenu then begin
                        PluginMenu.Guids := @MENU_GUID;
                        PluginMenu.Strings := @PluginStrings;
                        PluginMenu.Count := 1;
                end
                else begin
                        PluginMenu.Guids := nil;
                        PluginMenu.Strings := nil;
                        PluginMenu.Count := 0;
                end;

                PluginConfig.Guids := @MENU_GUID;
                PluginConfig.Strings := @PluginStrings;
                PluginConfig.Count := 1;

                CommandPrefix := GetMsg(MPanelTitle);
        end;
end;

procedure GetOpenPanelInfoW(var AInfo: TOpenPanelInfo);
stdcall export;
begin
        PMyPlugin(AInfo.hPanel).GetOpenPanelInfo(AInfo);
end;

procedure ClearList(hDlg: THandle; ID: NativeInt);
begin
        DlgList_ClearList(Info, hDlg, ID);
end;

procedure FillList(hDlg: THandle; ID: NativeInt; const Ports: TStrings);
var
        i: integer;
begin
        for i := 0 to Ports.Count - 1 do
                DlgList_AddString(Info, hDlg, ID, PFarChar(Ports[i]));
end;

function IndexOf(const Lines: TStrings; const value: string): integer;
var
        i: integer;
begin
        result := - 1;
        for i := 0 to Lines.Count - 1 do
                if value.StartsWith(Lines[i]) then begin
                        result := i;
                        break;
                end;
end;

function ConnDlgProc(hDlg: THandle; Msg: TIntPtr; Param1: TIntPtr; Param2: TIntPtr): TIntPtr; stdcall;
var
        cur: NativeInt;
        Opt: POpt;
begin
        Opt := nil;
        Dialogs.TryGetValue(hDlg, Opt);
        case Msg of
                DN_LISTCHANGE: begin
                        debug(format('[PLUGIN] ConnDlgProc hDlg=0x%x: DN_LISTCHANGE [%d,%d]',
                          [hDlg, Param1, Param2]), 5);
                        if Param1 = Opt.PortsListItem then
                                if Param2 < Opt.Ports.Count then begin
                                        Opt.ComPort := Opt.Ports[Param2];
                                        Opt.DevName := Opt.Names[Param2];
                                        DlgItem_SetText(Info, hDlg, Opt.PortItem, PFarChar(Opt.ComPort))
                                end;
                        if Param1 = Opt.SpeedCombo then
                                Opt.BaudRate := Param2;
                        if Param1 = Opt.PacketCombo then
                                Opt.OBEXPS := Param2;
                end;
                DN_INITDIALOG: begin
                        // getting options context
                        Opt := @PMyPlugin(Param2)^.Opt;
                        if Dialogs.ContainsKey(hDlg) then
                                debug(format('ConnDlgProc dialog already exists 0x%.8x', [hDlg]), 5)
                        else
                                Dialogs.Add(hDlg, Opt);
                        // Opt.OldPort := Opt.ComPort;
                        // debug(format('[PLUGIN] ConnDlgProc hDlg=0x%x: DN_INITDIALOG [%d,%d] port: %s',
                        // [hDlg, Param1, Param2, Opt.ComPort]), 5);
                        // configuring ListBox
                        Info.SendDlgMessage(hDlg, DM_SETMOUSEEVENTNOTIFY, Opt.PortsListItem, nil );
                        // congigure Speed
                        DlgList_AddString(Info, hDlg, Opt.SpeedCombo, '115200');
                        DlgList_AddString(Info, hDlg, Opt.SpeedCombo, '230400');
                        DlgList_AddString(Info, hDlg, Opt.SpeedCombo, '460800');
                        DlgList_AddString(Info, hDlg, Opt.SpeedCombo, '921600');
                        DlgList_SetCurPos(Info, hDlg, Opt.SpeedCombo, Opt.BaudRate);
                        // congigure Packet
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '512');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '1024');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '2048');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '4096');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '8192');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '16384');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '32768');
                        DlgList_AddString(Info, hDlg, Opt.PacketCombo, '65536');
                        DlgList_SetCurPos(Info, hDlg, Opt.PacketCombo, Opt.OBEXPS);
                        // configure DevTypes
                        DlgItem_SetCheck(Info, hDlg, Opt.DevType + 2, 1);
                end;
                DN_BTNCLICK: begin
                        debug(format('[PLUGIN] ConnDlgProc hDlg=0x%x: DN_BTNCLICK [%d,%d]', [hDlg, Param1, Param2]), 5);
                        if Param2 = 1 then begin
                                case Param1 of
                                        PORTS_SONYERICSSON: begin
                                                ClearList(hDlg, Opt.PortsListItem);
                                                DlgItem_Disable(Info, hDlg, Opt.PortsListItem);
                                                Opt.Ports.Clear;
                                                Opt.Names.Clear;
                                                EnumInterfaces(SE_OBEX_GUID, Opt.Ports, Opt.Names);
                                                FillList(hDlg, Opt.PortsListItem, Opt.Names);
                                                DlgItem_Enable(Info, hDlg, Opt.PortsListItem);
                                                Opt.DevType := DEVTYPE_USB_SONYERICSSON;
                                                DlgItem_SetCheck(Info, hDlg, Opt.UseWMCItem, 1);
                                                DlgItem_Disable(Info, hDlg, Opt.UseWMCItem);
                                                Opt.UseWMC := 1;
                                                DlgItem_Disable(Info, hDlg, Opt.RTSItem);
                                                DlgItem_Disable(Info, hDlg, Opt.DTRItem);
                                                DlgItem_Enable(Info, hDlg, Opt.SpeedCombo);
                                        end;
                                        PORTS_NOKIA: begin
                                                ClearList(hDlg, Opt.PortsListItem);
                                                DlgItem_Disable(Info, hDlg, Opt.PortsListItem);
                                                Opt.Ports.Clear;
                                                Opt.Names.Clear;
                                                EnumInterfaces(NOKIA_OBEX_GUID, Opt.Ports, Opt.Names);
                                                FillList(hDlg, Opt.PortsListItem, Opt.Names);
                                                DlgItem_Enable(Info, hDlg, Opt.PortsListItem);
                                                Opt.DevType := DEVTYPE_USB_NOKIA;
                                                DlgItem_SetCheck(Info, hDlg, Opt.UseWMCItem, 1);
                                                DlgItem_Disable(Info, hDlg, Opt.UseWMCItem);
                                                Opt.UseWMC := 1;
                                                DlgItem_Disable(Info, hDlg, Opt.RTSItem);
                                                DlgItem_Disable(Info, hDlg, Opt.DTRItem);
                                                DlgItem_Disable(Info, hDlg, Opt.SpeedCombo);
                                        end;
                                        PORTS_SERIAL: begin
                                                ClearList(hDlg, Opt.PortsListItem);
                                                DlgItem_Disable(Info, hDlg, Opt.PortsListItem);
                                                Opt.Ports.Clear;
                                                Opt.Names.Clear;
                                                GetDevsByClass('Ports', Opt^);
                                                FillList(hDlg, Opt.PortsListItem, Opt.Names);
                                                DlgItem_Enable(Info, hDlg, Opt.PortsListItem);
                                                Opt.DevType := DEVTYPE_SERIAL_PORT;
                                                DlgItem_SetCheck(Info, hDlg, Opt.UseWMCItem, 0);
                                                DlgItem_Enable(Info, hDlg, Opt.UseWMCItem);
                                                Opt.UseWMC := 0;
                                                DlgItem_Enable(Info, hDlg, Opt.RTSItem);
                                                DlgItem_Enable(Info, hDlg, Opt.DTRItem);
                                                DlgItem_Enable(Info, hDlg, Opt.SpeedCombo);
                                        end;
                                        PORTS_MODEM: begin
                                                ClearList(hDlg, Opt.PortsListItem);
                                                DlgItem_Disable(Info, hDlg, Opt.PortsListItem);
                                                Opt.Ports.Clear;
                                                Opt.Names.Clear;
                                                GetDevsByClass('Modem', Opt^);
                                                FillList(hDlg, Opt.PortsListItem, Opt.Names);
                                                DlgItem_Enable(Info, hDlg, Opt.PortsListItem);
                                                Opt.DevType := DEVTYPE_MODEM_PORT;
                                                DlgItem_SetCheck(Info, hDlg, Opt.UseWMCItem, 0);
                                                DlgItem_Enable(Info, hDlg, Opt.UseWMCItem);
                                                Opt.UseWMC := 0;
                                                DlgItem_Enable(Info, hDlg, Opt.RTSItem);
                                                DlgItem_Enable(Info, hDlg, Opt.DTRItem);
                                                DlgItem_Enable(Info, hDlg, Opt.SpeedCombo);
                                        end;
                                        PORTS_ALL: begin
                                                ClearList(hDlg, Opt.PortsListItem);
                                                DlgItem_Disable(Info, hDlg, Opt.PortsListItem);
                                                EnumComPorts(Opt.Ports);
                                                Opt.Names.Assign(Opt.Ports);
                                                FillList(hDlg, Opt.PortsListItem, Opt.Names);
                                                DlgItem_Enable(Info, hDlg, Opt.PortsListItem);
                                                Opt.DevType := DEVTYPE_COM_PORT;
                                                DlgItem_SetCheck(Info, hDlg, Opt.UseWMCItem, 0);
                                                DlgItem_Enable(Info, hDlg, Opt.UseWMCItem);
                                                Opt.UseWMC := 0;
                                                DlgItem_Enable(Info, hDlg, Opt.RTSItem);
                                                DlgItem_Enable(Info, hDlg, Opt.DTRItem);
                                                DlgItem_Enable(Info, hDlg, Opt.SpeedCombo);
                                        end;
                                end;
                                // setting selected port item
                                case Param1 of
                                        PORTS_SONYERICSSON, PORTS_NOKIA, PORTS_SERIAL, PORTS_MODEM, PORTS_ALL: begin
                                                if Opt.Ports.Count > 0 then begin
                                                        Opt.ComPort := Opt.Ports[0];
                                                        Opt.DevName := Opt.Names[0];
                                                end
                                                else begin
                                                        Opt.ComPort := '';
                                                        Opt.DevName := '';
                                                end;
                                                // select last selected port, if there is one
                                                cur := IndexOf(Opt.Ports, Opt.OldPort);
                                                if cur >= 0 then
                                                        DlgList_SetCurPos(Info, hDlg, Opt.PortsListItem, cur);
                                                if Param1 = PORTS_NOKIA then
                                                        DlgItem_SetText(Info, hDlg, Opt.PortItem, 'USB')
                                                else
                                                        DlgItem_SetText(Info, hDlg, Opt.PortItem,
                                                          PFarChar(Opt.ComPort));
                                                if Opt.ComPort = '' then
                                                        DlgItem_Disable(Info, hDlg, Opt.vConn)
                                                else
                                                        DlgItem_Enable(Info, hDlg, Opt.vConn);
                                        end;
                                end;
                        end;
                        if Param1 = Opt.RTSItem then
                                Opt.RTS := Param2;
                        if Param1 = Opt.DTRItem then
                                Opt.DTR := Param2;
                        if Param1 = Opt.UseWMCItem then
                                Opt.UseWMC := Param2;
                end;
        end;
        result := Info.DefDlgProc(hDlg, Msg, Param1, Param2);
end;

function OpenW(var AOpenInfo: TOpenInfo): THandle;
stdcall export;
var
        fdi: array [0 .. 50] of TFarDialogItem;
        hDlg: THandle;
        i, Count: size_t;
        res: TIntPtr;
        Flags, Param, Reserved: NativeInt;
        plugin: PMyPlugin;
        p: TStrings;
begin
        result := 0;
        Count := 0;
        New(plugin);
        plugin^ := TMyPlugin.Create;
        with plugin^ do
                try
                        hPlugin := THandle(plugin);
                        debug(format('OpenW v%s [%d:%d]', [VersionInfo.FileVersion, AOpenInfo.OpenFrom, AOpenInfo.Data]), 5);
                        PhonePath := '';
                        LoadSettings;
                        ZeroMemory(@fdi, SizeOf(fdi));
                        fdi[Count] := FarDI(DI_DOUBLEBOX, 3, 1, 59 + 7, 15, 0, 0, 0,
                          PFarChar(format('%s v%s - %s', [GetMsg(MTitle), VersionInfo.FileVersion, VersionInfo.CompanyName])));
                        inc(Count);
                        fdi[Count] := FarDI(DI_SINGLEBOX, 5, 2, 57 + 7, 8, 0, DIF_LEFTTEXT, 0, GetMsg(MSelectDevice));
                        inc(Count);
                        fdi[Count] := FarDI(DI_RADIOBUTTON, 6, 3, 16 + 7, 3, 0, DIF_LEFTTEXT or DIF_GROUP, 0,
                          GetMsg(MSEOBEX));
                        fdi[Count].Param.Selected := 1;
                        inc(Count);
                        fdi[Count] := FarDI(DI_RADIOBUTTON, 6, 4, 16 + 7, 4, 0, DIF_LEFTTEXT, 0, GetMsg(MNKOBEX));
                        inc(Count);
                        fdi[Count] := FarDI(DI_RADIOBUTTON, 6, 5, 16 + 7, 5, 0, DIF_LEFTTEXT, 0, GetMsg(MSerial));
                        inc(Count);
                        fdi[Count] := FarDI(DI_RADIOBUTTON, 6, 6, 16 + 7, 5, 0, DIF_LEFTTEXT, 0, GetMsg(MModem));
                        inc(Count);
                        fdi[Count] := FarDI(DI_RADIOBUTTON, 6, 7, 16 + 7, 7, 0, DIF_LEFTTEXT, 0, GetMsg(MAll));
                        inc(Count);
                        fdi[Count] := FarDI(DI_LISTBOX, 20 + 7, 3, 56 + 7, 7, 0,
                          DIF_LISTNOBOX or DIF_LISTNOCLOSE, 0, '');
                        Opt.PortsListItem := Count;
                        inc(Count);
                        // device settings
                        fdi[Count] := FarDI(DI_SINGLEBOX, 5, 9, 57 + 7, 12, 0, DIF_LEFTTEXT, 0,
                          GetMsg(MDeviceSettings));
                        inc(Count);
                        fdi[Count] := FarDI(DI_TEXT, 7, 10, 11, 10, 0, DIF_LEFTTEXT, 0, GetMsg(MPort));
                        inc(Count);
                        fdi[Count] := FarDI(DI_TEXT, 13, 10, 19, 10, 0, DIF_LEFTTEXT, 0, PFarChar(Opt.Port));
                        Opt.PortItem := Count;
                        inc(Count);
                        fdi[Count] := FarDI(DI_TEXT, 21, 10, 27, 10, 0, DIF_LEFTTEXT, 0, GetMsg(MSpeed));
                        inc(Count);
                        fdi[Count] := FarDI(DI_COMBOBOX, 28, 10, 35, 10, 0, DIF_DROPDOWNLIST or DIF_CENTERTEXT, 0, '');
                        Opt.SpeedCombo := Count;
                        inc(Count);
                        fdi[Count] := FarDI(DI_CHECKBOX, 40, 10, 45, 10, 0, DIF_NOFOCUS or DIF_LEFTTEXT, 0, 'RTS');
                        Opt.RTSItem := Count;
                        if Opt.RTS = 1 then
                                fdi[Count].Param.Selected := LIF_SELECTED
                        else
                                fdi[Count].Param.Selected := 0;
                        inc(Count);
                        fdi[Count] := FarDI(DI_CHECKBOX, 49, 10, 53, 10, 0, DIF_NOFOCUS or DIF_LEFTTEXT, 0, 'DTR');
                        Opt.DTRItem := Count;
                        if Opt.DTR = 1 then
                                fdi[Count].Param.Selected := LIF_SELECTED
                        else
                                fdi[Count].Param.Selected := 0;
                        inc(Count);
                        fdi[Count] := FarDI(DI_TEXT, 21, 11, 27, 11, 0, DIF_LEFTTEXT, 0, GetMsg(MPacket));
                        inc(Count);
                        fdi[Count] := FarDI(DI_COMBOBOX, 28, 11, 35, 11, 0, DIF_DROPDOWNLIST or DIF_CENTERTEXT, 0, '');
                        Opt.PacketCombo := Count;
                        inc(Count);
                        fdi[Count] := FarDI(DI_CHECKBOX, 40, 11, 54, 11, 0, DIF_NOFOCUS or DIF_LEFTTEXT, 0,
                          GetMsg(MUseOBEX));
                        Opt.UseWMCItem := Count;
                        if Opt.UseWMC = 1 then
                                fdi[Count].Param.Selected := LIF_SELECTED
                        else
                                fdi[Count].Param.Selected := 0;
                        inc(Count);

                        fdi[Count] := FarDI(DI_TEXT, - 1, 13, 0, 0, 0, DIF_SEPARATOR, 0, '');
                        inc(Count);
                        fdi[Count] := FarDI(DI_BUTTON, 0, 14, 0, 0, 1, DIF_CENTERGROUP, 1, GetMsg(MConnectButton));
                        Opt.vConn := Count;
                        inc(Count);
                        fdi[Count] := FarDI(DI_BUTTON, 0, 14, 0, 0, 0, DIF_CENTERGROUP, 0, GetMsg(MExitButton));
                        Opt.vCancel := Count;
                        inc(Count);
                        repeat
                                try
                                        Flags := 0;
                                        // passing plugin handle to dialog
                                        Param := NativeInt(plugin);
                                        Reserved := 0;
                                        with Info do begin
                                                debug('OpenW() Init dialog...', 5);
                                                hDlg := DialogInit(MAIN_GUID, WELCOME_DLG_GUID, - 1, - 1, 63 + 7, 17,
                                                  'Contents', @fdi, Count, Reserved, Flags, ConnDlgProc, Param);
                                                debug('OpenW() Running dialog...', 5);
                                                res := DialogRun(hDlg);
                                                DialogFree(hDlg);
                                                // Removing from dictionary
                                                Dialogs.Remove(hDlg);
                                        end;
                                        debug(format('OpenW() dialog: %d', [res]), 5);
                                        if res = Opt.vConn then begin
                                                case Opt.DevType of
                                                        DEVTYPE_USB_SONYERICSSON, DEVTYPE_USB_NOKIA:
                                                        Opt.DevTitle := Opt.DevName;
                                                        else Opt.DevTitle := Opt.ComPort;
                                                end;
                                                p := TStringList.Create;
                                                try
                                                        p.DelimitedText := Opt.ComPort;
                                                        for i := 0 to p.Count - 1 do begin
                                                                Opt.ComPort := p[i];
                                                                try
                                                                        // open port, estabilish OBEX connection
                                                                        Init;
                                                                        if Connected then
                                                                                break;
                                                                except
                                                                        if i = (p.Count - 1) then
                                                                                raise;
                                                                end;
                                                        end;

                                                finally
                                                        p.Free;
                                                end;
                                                if not Connected then
                                                        raise Exception.Create(GetMsg(MErrorConnectingOBEX))
                                                else begin
                                                        SaveSettings;
                                                        result := NativeUInt(plugin);
                                                        break;
                                                end;
                                        end // Connect
                                        else begin // Cancel selected
                                                Free;
                                                Dispose(plugin);
                                                break;
                                        end;
                                except
                                        on e: Exception do begin
                                                debug('OpenW() ERROR: ' + e.Message, 5);
                                                Message(FMSG_WARNING, '',
                                                  [integer(MTitle), integer(MErrorConnectingDevice),
                                                  PFarChar(Opt.DevName), PFarChar(e.Message), integer(MOkButton)], 1);
                                                OBEX.Connected := false;
                                                Connected := false;
                                        end;
                                end;
                        until false;
                finally
                        // Releasing resources
                        for i := 0 to Count - 1 do
                                StrDispose(fdi[i].Data);
                end;
end;

procedure ClosePanelW(AInfo: TClosePanelInfo);
stdcall export;
begin
        try
                if Assigned(PMyPlugin(AInfo.hPanel)) then begin
                        PMyPlugin(AInfo.hPanel).ClosePanel;
                        Dispose(PMyPlugin(AInfo.hPanel));
                end;
        except
                // silent errors
        end;
end;

function GetFindDataW(var AInfo: TGetFindDataInfo): NativeInt;
stdcall export;
begin
        if AInfo.StructSize >= SizeOf(TGetFindDataInfo) then
                result := PMyPlugin(AInfo.hPanel).GetFindData(AInfo.PanelItem, AInfo.ItemsNumber, AInfo.OpMode)
        else
                result := 0;
end;

procedure FreeFindDataW(const AInfo: TFreeFindDataInfo);
stdcall export;
begin
        if AInfo.StructSize >= SizeOf(TFreeFindDataInfo) then
                PMyPlugin(AInfo.hPanel).FreeFindData(AInfo);
end;

function SetDirectoryW(const AInfo: TSetDirectoryInfo): NativeInt;
stdcall export;
begin
        result := PMyPlugin(AInfo.hPanel).SetDirectory(AInfo.Dir, AInfo.OpMode);
end;

function MakeDirectoryW(hPlugin: THandle; var Dir: PFarChar; OpMode: integer): integer;
stdcall export;
begin
        result := PMyPlugin(hPlugin).MakeDirectory(Dir, OpMode);
end;

function GetFilesW(var AInfo: TGetFilesInfo): NativeInt;
stdcall export;
begin
        result := PMyPlugin(AInfo.hPanel).GetFiles(AInfo.PanelItem, AInfo.ItemsNumber, AInfo.Move, AInfo.DestPath,
          AInfo.OpMode);
end;

function PutFilesW(var AInfo: TPutFilesInfo): NativeInt;
stdcall export;
begin
        result := PMyPlugin(AInfo.hPanel).PutFiles(AInfo.PanelItem, AInfo.ItemsNumber, AInfo.Move, AInfo.SrcPath,
          AInfo.OpMode);
end;

function DeleteFilesW(AInfo: TDeleteFilesInfo): integer stdcall; export;
begin
        result := PMyPlugin(AInfo.hPanel).DeleteFiles(AInfo.PanelItem, AInfo.ItemsNumber, AInfo.OpMode);
end;

procedure SaveMainSettings;
begin
        debug(format('SaveMainSettings (disk: %s, menu: %s)', [BoolToStr(MainOpt.AddToPluginsMenu),
          BoolToStr(MainOpt.AddToDisksMenu)]), msgtype_details);
        Settings.SetBoolean(0, SETTINGS_ADD_TO_DISKS, MainOpt.AddToDisksMenu);
        Settings.SetBoolean(0, SETTINGS_ADD_TO_PLUGINS, MainOpt.AddToPluginsMenu);
end;

procedure LoadMainSettings;
begin
        MainOpt.AddToDisksMenu := Settings.Get(0, SETTINGS_ADD_TO_DISKS, true);
        MainOpt.AddToPluginsMenu := Settings.Get(0, SETTINGS_ADD_TO_PLUGINS, true);
        debug(format('LoadMainSettings (disk: %s, menu: %s)', [BoolToStr(MainOpt.AddToPluginsMenu),
          BoolToStr(MainOpt.AddToDisksMenu)]), msgtype_details);
end;

function ConfigureW(var AConfigureInfo: TConfigureInfo): TIntPtr;
stdcall export;
const
        WIDTH = 41;
var
        res: NativeInt;
        fdi: array [0 .. 20] of TFarDialogItem;
        hDlg: THandle;
        Count, dm, pm: NativeInt;
        vOK, vCancel: NativeInt;
begin
        debug('ConfigureW', 5);
        result := 1;
        Count := 0;
        fdi[Count] := FarDI(DI_DOUBLEBOX, 3, 1, WIDTH, 6, 0, 0, 0, GetMsg(MConfigTitle));
        inc(Count);
        fdi[Count] := FarDI(DI_CHECKBOX, 5, 2, WIDTH - 6, 2, 0, 0, 0, GetMsg(MConfigAddToPluginMenu));
        pm := Count;
        if MainOpt.AddToPluginsMenu then
                fdi[Count].Param.Selected := LIF_SELECTED
        else
                fdi[Count].Param.Selected := 0;
        inc(Count);
        fdi[Count] := FarDI(DI_CHECKBOX, 5, 3, WIDTH - 6, 3, 0, 0, 0, GetMsg(MConfigAddToDisksMenu));
        dm := Count;
        if MainOpt.AddToDisksMenu then
                fdi[Count].Param.Selected := LIF_SELECTED
        else
                fdi[Count].Param.Selected := 0;
        inc(Count);
        fdi[Count] := FarDI(DI_TEXT, - 1, 4, 0, 0, 0, DIF_SEPARATOR, 0, '');
        inc(Count);
        fdi[Count] := FarDI(DI_BUTTON, 0, 5, 0, 0, 1, DIF_CENTERGROUP, 1, GetMsg(MOkButton));
        vOK := Count;
        inc(Count);
        fdi[Count] := FarDI(DI_BUTTON, 0, 5, 0, 0, 0, DIF_CENTERGROUP, 0, GetMsg(MCancelButton));
        vCancel := Count;
        inc(Count);
        with Info do begin
                hDlg := DialogInit(MAIN_GUID, CONFIGURE_DLG_GUID, - 1, - 1, WIDTH + 4, 8, 'hcConfigure', @fdi, Count, 0,
                  0, nil, 0);
                res := DialogRun(hDlg);
                if res = vOK then begin
                        if SendDlgMessage(hDlg, DM_GETCHECK, dm, nil) <> 65536 then
                                MainOpt.AddToDisksMenu := boolean(SendDlgMessage(hDlg, DM_GETCHECK, dm, nil));
                        if SendDlgMessage(hDlg, DM_GETCHECK, pm, nil) <> 65536 then
                                MainOpt.AddToPluginsMenu := boolean(SendDlgMessage(hDlg, DM_GETCHECK, pm, nil));
                end;
                DialogFree(hDlg);
        end;
        if res = vOK then
                SaveMainSettings()
        else
                result := 0;
end;

procedure SetStartupInfoMy;
begin
        debug('SetStartupInfo', 5);
        Settings := TPluginSettings.Create(MAIN_GUID, TFarSettingsControl(Info.SettingsControl));
        LoadMainSettings;
end;

procedure GetGlobalInfoW(var AGlobalInfo: TGlobalInfo); stdcall; export;
var
        VA: TArray<string>;
begin
        debug(format('GetGlobalInfoW(0x%P)', [@AGlobalInfo]), 5);
        AGlobalInfo.StructSize := SizeOf(TGlobalInfo);
        // min Far version required
        AGlobalInfo.MinFarVersion := MAKEFARVERSION(3, 0, 0, 2927, VS_RELEASE);
        // getting plugin version from version info
        VA := VersionInfo.FileVersion.Split(['.']);
        if Length(VA) < 4 then
                // should not happen, but report dummy version if so
                AGlobalInfo.Version := MAKEFARVERSION(3, 0, 0, 1, VS_RC)
        else
                AGlobalInfo.Version := MAKEFARVERSION(StrToIntDef(VA[0], 0), StrToIntDef(VA[1], 0),
                  StrToIntDef(VA[2], 0), StrToIntDef(VA[3], 0), VS_RELEASE);
        AGlobalInfo.Guid := MAIN_GUID;
        AGlobalInfo.Title := 'PhoneFS';
        AGlobalInfo.Description := 'Access mobile phone using OBEX protocol';
        AGlobalInfo.Author := 'darkmen';
end;

exports
        OpenW,
        ClosePanelW,
        GetPluginInfoW,
        GetOpenPanelInfoW,
        GetFindDataW,
        FreeFindDataW,
        GetFilesW,
        PutFilesW,
        DeleteFilesW,
        SetDirectoryW,
        MakeDirectoryW,
        ConfigureW,
        GetGlobalInfoW;

begin
        OnStartupInfoLoad := SetStartupInfoMy;

end.
