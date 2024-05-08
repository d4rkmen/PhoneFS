unit setupapi;

interface

uses
        Windows,
        Classes;

type
        HDEVINFO = Pointer;

        TSPDevInfoData = record
                cbSize: DWord;
                Guid: TGUID;
                DevInst: DWord;
                Reserve: Pointer;
        end;

        PSPDevInfoData = ^TSPDevInfoData;

        TSPDeviceInterfaceDetailData = packed record
                cbSize: DWord;
                DevicePath: array [0 .. 0] of Char;
        end;

        PSPDeviceInterfaceDetailData = ^TSPDeviceInterfaceDetailData;

        TSPDeviceInterfaceData = record
                cbSize: DWord;
                InterfaceClassGuid: TGUID;
                Flags: DWord;
                Reserve: Pointer;
        end;

        PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;

const
        SPDRP_DEVICEDESC           = DWord($00000000);
        DIGCF_DEFAULT              = DWord($00000001);
        DIGCF_PRESENT              = DWord($00000002);
        DIGCF_ALLCLASSES           = DWord($00000004);
        DIGCF_PROFILE              = DWord($00000008);
        DIGCF_DEVICEINTERFACE      = DWord($00000010);
        SPDRP_HARDWAREID           = DWord($00000001);
        SPDRP_FRIENDLYNAME         = DWord($0000000C);
        SPDRP_LOCATION_INFORMATION = DWord($0000000D);

function SetupDiClassGuidsFromNameW(ClassName: PCHAR; ClassGuidList: PGuid; ClassGuidListSize: DWord;
  var RequiredSize: DWord): Bool; stdcall; external 'setupapi.dll';

function SetupDiGetClassDevsW(ClassGuid: PGuid; Enumerator: PCHAR; hwndParent: HWND; Flags: DWord): HDEVINFO; stdcall;
  external 'setupapi.dll';

function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; numDev: DWord; var DeviceInfoData: TSPDevInfoData): Bool;
  stdcall; external 'setupapi.dll';

function SetupDiGetDeviceRegistryPropertyW(DeviceInfoSet: HDEVINFO; const DeviceInfoData: TSPDevInfoData;
  Propertys: DWord; var PropertyRegDataType: DWord; PropertyBuffer: PBYTE; PropertyBufferSize: DWord;
  var RequiredSize: DWord): Bool; stdcall; external 'setupapi.dll';

function SetupDiGetDeviceInterfaceDetailW(DeviceInfoSet: HDEVINFO; DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailData; DeviceInterfaceDetailDataSize: DWord;
  var RequiredSize: DWord; DeviceInfoData: PSPDevInfoData): Bool; stdcall; external 'setupapi.dll';

function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData;
  const InterfaceClassGuid: TGUID; MemberIndex: DWord; var DeviceInterfaceData: TSPDeviceInterfaceData): Bool; stdcall;
  external 'setupapi.dll';

function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): Bool; stdcall; external 'setupapi.dll';

function SetupDiGetDeviceInstanceIdW(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData; DeviceInstanceID: PCHAR;
  DeviceInstanceIDSize: DWord; var RequiredSize: DWORD): Bool; stdcall; external 'setupapi.dll';

implementation

end.
