unit rpVersionInfo; // by Rick Peterson rickpet@airmail.net

interface

uses

        Windows,
        SysUtils,
        Classes,
        TypInfo;

type
{$M+}
        TVersionType = (vtCompanyName, vtFileDescription, vtFileVersion, vtInternalName, vtLegalCopyright,
          vtLegalTradeMark, vtOriginalFileName, vtProductName, vtProductVersion, vtComments);
{$M-}

        TrpVersionInfo = class(TComponent)
                private
                        FVersionInfo: array [0 .. ord(high(TVersionType))] of string;
                protected
                        function GetCompanyName: string;
                        function GetFileDescription: string;
                        function GetFileVersion: string;
                        function GetInternalName: string;
                        function GetLegalCopyright: string;
                        function GetLegalTradeMark: string;
                        function GetOriginalFileName: string;
                        function GetProductName: string;
                        function GetProductVersion: string;
                        function GetComments: string;
                        function GetVersionInfo(VersionType: TVersionType): string; virtual;
                        procedure SetVersionInfo; virtual;
                public
                        constructor Create(AOwner: TComponent); override;
                published
                        property CompanyName: string
                          read GetCompanyName;
                        property FileDescription: string
                          read GetFileDescription;
                        property FileVersion: string
                          read GetFileVersion;
                        property InternalName: string
                          read GetInternalName;
                        property LegalCopyright: string
                          read GetLegalCopyright;
                        property LegalTradeMark: string
                          read GetLegalTradeMark;
                        property OriginalFileName: string
                          read GetOriginalFileName;
                        property ProductName: string
                          read GetProductName;
                        property ProductVersion: string
                          read GetProductVersion;
                        property Comments: string
                          read GetComments;
        end;

procedure Register;

implementation

constructor TrpVersionInfo.Create(AOwner: TComponent);
begin

        inherited Create(AOwner);
        SetVersionInfo;
end;

function TrpVersionInfo.GetCompanyName: string;
begin

        result := GetVersionInfo(vtCompanyName);
end;

function TrpVersionInfo.GetFileDescription: string;
begin

        result := GetVersionInfo(vtFileDescription);
end;

function TrpVersionInfo.GetFileVersion: string;
begin

        result := GetVersionInfo(vtFileVersion);
end;

function TrpVersionInfo.GetInternalName: string;
begin

        result := GetVersionInfo(vtInternalName);
end;

function TrpVersionInfo.GetLegalCopyright: string;
begin

        result := GetVersionInfo(vtLegalCopyright);
end;

function TrpVersionInfo.GetLegalTradeMark: string;
begin

        result := GetVersionInfo(vtLegalTradeMark);
end;

function TrpVersionInfo.GetOriginalFileName: string;
begin
        result := GetVersionInfo(vtOriginalFileName);
end;

function TrpVersionInfo.GetProductName: string;
begin
        result := GetVersionInfo(vtProductName);
end;

function TrpVersionInfo.GetProductVersion: string;
begin
        result := GetVersionInfo(vtProductVersion);
end;

function TrpVersionInfo.GetComments: string;
begin
        result := GetVersionInfo(vtComments);
end;

function TrpVersionInfo.GetVersionInfo(VersionType: TVersionType): string;
begin
        result := FVersionInfo[ord(VersionType)];
end;

procedure TrpVersionInfo.SetVersionInfo;
var
        sAppName, sVersionType: string;
        iAppSize, iLenOfValue, i: Cardinal;
        pcBuf, pcValue: PChar;
begin
        sAppName := GetModuleName(hInstance);
        iAppSize := GetFileVersionInfoSize(PChar(sAppName), iAppSize);
        if iAppSize > 0 then begin
                pcBuf := AllocMem(iAppSize);
                GetFileVersionInfo(PChar(sAppName), 0, iAppSize, pcBuf);
                for i := 0 to ord(High(TVersionType)) do begin
                        sVersionType := GetEnumName(TypeInfo(TVersionType), i);
                        sVersionType := Copy(sVersionType, 3, length(sVersionType));
                        if VerQueryValue(pcBuf, PChar('StringFileInfo\040904E4\' + sVersionType), Pointer(pcValue),
                          iLenOfValue) then
                                FVersionInfo[i] := pcValue;
                end;
                FreeMem(pcBuf, iAppSize);
        end;
end;

procedure Register;
begin
        RegisterComponents('FreeWare', [TrpVersionInfo]);
end;

end.
