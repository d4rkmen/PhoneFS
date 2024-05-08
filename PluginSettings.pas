unit PluginSettings;

interface

uses
        Windows,
        Math,
        PluginW;

type
        TFarSettingsControl = function(Handle: THandle; Command: DWORD; Param1: DWORD_PTR; Param2: Pointer)
          : BOOL; stdcall;

        TPluginSettings = class
                private
                        FHandle: THandle;
                        FSettingsControl: TFarSettingsControl;
                public
                        constructor Create(const AGuid: TGUID; ASettingsControl: TFarSettingsControl);
                        destructor Destroy; override;

                        function CreateSubKey(Root: size_t; const Name: PWideChar): Integer;
                        function OpenSubKey(Root: size_t; const Name: PWideChar): Integer;
                        function DeleteSubKey(Root: size_t): Boolean;
                        function DeleteValue(Root: size_t; const Name: PWideChar): Boolean;
                        function Get(Root: size_t; const Name, Default: PWideChar): PWideChar; overload;
                        procedure Get(Root: size_t; const Name: PWideChar; Value: PWideChar; Size: size_t;
                          const Default: PWideChar); overload;
                        function Get(Root: size_t; const Name: PWideChar; Default: UInt64): UInt64; overload;
                        function Get(Root: size_t; const Name: PWideChar; Default: Int64): Int64; overload;
                        function Get(Root: size_t; const Name: PWideChar; Default: Integer): Integer; overload;
                        function Get(Root: size_t; const Name: PWideChar; Default: Cardinal): Cardinal; overload;
                        function Get(Root: size_t; const Name: PWideChar; Default: Boolean): Boolean; overload;
                        function Get(Root: size_t; const Name: PWideChar; Value: Pointer; Size: size_t)
                          : size_t; overload;
                        function SetString(Root: size_t; const Name, Value: PWideChar): Boolean;
                        function SetUInt64(Root: size_t; const Name: PWideChar; Value: UInt64): Boolean;
                        function SetInt64(Root: size_t; const Name: PWideChar; Value: Int64): Boolean;
                        function SetInteger(Root: size_t; const Name: PWideChar; Value: Integer): Boolean;
                        function SetCardinal(Root: size_t; const Name: PWideChar; Value: Cardinal): Boolean;
                        function SetBoolean(Root: size_t; const Name: PWideChar; Value: Boolean): Boolean;
                        function SetData(Root: size_t; const Name: PWideChar; Value: Pointer; Size: size_t): Boolean;
                        function Enum(Root: size_t; var Fse: TFarSettingsEnum): Boolean;
        end;

implementation

constructor TPluginSettings.Create(const AGuid: TGUID; ASettingsControl: TFarSettingsControl);
var
        Settings: TFarSettingsCreate;
begin
        FSettingsControl := ASettingsControl;
        FHandle := INVALID_HANDLE_VALUE;

        Settings.StructSize := SizeOf(TFarSettingsCreate);
        Settings.Guid := AGuid;
        Settings.Handle := FHandle;

        if FSettingsControl(INVALID_HANDLE_VALUE, SCTL_CREATE, 0, @Settings) then
                FHandle := Settings.Handle;
end;

destructor TPluginSettings.Destroy;
begin
        FSettingsControl(FHandle, SCTL_FREE, 0, nil);
        inherited Destroy;
end;

function TPluginSettings.CreateSubKey(Root: size_t; const Name: PWideChar): Integer;
var
        Value: TFarSettingsValue;
begin
        Value.StructSize := SizeOf(TFarSettingsValue);
        Value.Root := Root;
        Value.Value := Name;
        Result := Integer(FSettingsControl(FHandle, SCTL_CREATESUBKEY, 0, @Value));
end;

function TPluginSettings.OpenSubKey(Root: size_t; const Name: PWideChar): Integer;
var
        Value: TFarSettingsValue;
begin
        Value.StructSize := SizeOf(TFarSettingsValue);
        Value.Root := Root;
        Value.Value := Name;
        Result := Integer(FSettingsControl(FHandle, SCTL_OPENSUBKEY, 0, @Value));
end;

function TPluginSettings.DeleteSubKey(Root: size_t): Boolean;
var
        Value: TFarSettingsValue;
begin
        Value.StructSize := SizeOf(TFarSettingsValue);
        Value.Root := Root;
        Value.Value := nil;
        Result := FSettingsControl(FHandle, SCTL_DELETE, 0, @Value);
end;

function TPluginSettings.DeleteValue(Root: size_t; const Name: PWideChar): Boolean;
var
        Value: TFarSettingsValue;
begin
        Value.StructSize := SizeOf(TFarSettingsValue);
        Value.Root := Root;
        Value.Value := Name;
        Result := FSettingsControl(FHandle, SCTL_DELETE, 0, @Value);
end;

function TPluginSettings.Get(Root: size_t; const Name, Default: PWideChar): PWideChar;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_STRING;

        if FSettingsControl(FHandle, SCTL_GET, 0, @Item) then
                Result := Item.Value.Str
        else
                Result := Default;
end;

procedure TPluginSettings.Get(Root: size_t; const Name: PWideChar; Value: PWideChar; Size: size_t;
  const Default: PWideChar);
begin
        lstrcpynW(Value, Get(Root, Name, Default), Size);
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Default: UInt64): UInt64;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_QWORD;

        if FSettingsControl(FHandle, SCTL_GET, 0, @Item) then
                Result := Item.Value.Number
        else
                Result := Default;
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Default: Int64): Int64;
begin
        Result := Int64(Get(Root, Name, UInt64(Default)));
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Default: Integer): Integer;
begin
        Result := Integer(Get(Root, Name, UInt64(Default)));
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Default: Cardinal): Cardinal;
begin
        Result := Cardinal(Get(Root, Name, UInt64(Default)));
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Default: Boolean): Boolean;
begin
        Result := Get(Root, Name, UInt64(Default)) <> 0;
end;

function TPluginSettings.Get(Root: size_t; const Name: PWideChar; Value: Pointer; Size: size_t): size_t;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_DATA;

        if FSettingsControl(FHandle, SCTL_GET, 0, @Item) then begin
                Result := Min(Item.Value.Data.Size, Size);
                Move(Item.Value.Data.Data^, Value^, Result);
        end
        else
                Result := 0;
end;

function TPluginSettings.SetString(Root: size_t; const Name, Value: PWideChar): Boolean;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_STRING;
        Item.Value.Str := Value;

        Result := FSettingsControl(FHandle, SCTL_SET, 0, @Item);
end;

function TPluginSettings.SetUInt64(Root: size_t; const Name: PWideChar; Value: UInt64): Boolean;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_QWORD;
        Item.Value.Number := Value;

        Result := FSettingsControl(FHandle, SCTL_SET, 0, @Item);
end;

function TPluginSettings.SetInt64(Root: size_t; const Name: PWideChar; Value: Int64): Boolean;
begin
        Result := SetUInt64(Root, Name, UInt64(Value));
end;

function TPluginSettings.SetInteger(Root: size_t; const Name: PWideChar; Value: Integer): Boolean;
begin
        Result := SetUInt64(Root, Name, UInt64(Value));
end;

function TPluginSettings.SetCardinal(Root: size_t; const Name: PWideChar; Value: Cardinal): Boolean;
begin
        Result := SetUInt64(Root, Name, UInt64(Value));
end;

function TPluginSettings.SetBoolean(Root: size_t; const Name: PWideChar; Value: Boolean): Boolean;
begin
        Result := SetUInt64(Root, Name, Ord(Value));
end;

function TPluginSettings.SetData(Root: size_t; const Name: PWideChar; Value: Pointer; Size: size_t): Boolean;
var
        Item: TFarSettingsItem;
begin
        Item.StructSize := SizeOf(TFarSettingsItem);
        Item.Root := Root;
        Item.Name := Name;
        Item.fType := FST_DATA;
        Item.Value.Data.Size := Size;
        Item.Value.Data.Data := Value;

        Result := FSettingsControl(FHandle, SCTL_SET, 0, @Item);
end;

function TPluginSettings.Enum(Root: size_t; var Fse: TFarSettingsEnum): Boolean;
begin
        Fse.Root := Root;
        Fse.StructSize := SizeOf(TFarSettingsEnum);
        Result := FSettingsControl(FHandle, SCTL_ENUM, 0, @Fse);
end;

end.
