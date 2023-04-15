unit Xander.DllCallback;

{$I Default.inc}

interface

uses
  System.SysUtils,

  Winapi.Windows,

  Xander.LdrMonitor;

type
  TDllCallback = procedure(const Name: string; Base: Pointer); register;

procedure Setup(Callback: TDllCallback);

implementation

var
  Callback: TDllCallback = nil;

type
  LDR_MANIFEST_PROBER_ROUTINE = function(DllHandle: Pointer; FullDllName: PWideChar; out ActCtx: Pointer): Cardinal; stdcall;
  TLdrManifestProberRoutine = LDR_MANIFEST_PROBER_ROUTINE;

type
  PLDR_RESOURCE_INFO = ^LDR_RESOURCE_INFO;
  LDR_RESOURCE_INFO = record
    &Type: Cardinal;
    Name: Cardinal;
    Language: Cardinal;
  end;
  PLdrResourceInfo = ^TLdrResourceInfo;
  TLdrResourceInfo = LDR_RESOURCE_INFO;

  PIMAGE_RESOURCE_DATA_ENTRY = ^IMAGE_RESOURCE_DATA_ENTRY;
  IMAGE_RESOURCE_DATA_ENTRY = record
    OffsetToData: Cardinal;
    Size: Cardinal;
    CodePage: Cardinal;
    Reserved: Cardinal;
  end;
  TImageResourceDataEntry = IMAGE_RESOURCE_DATA_ENTRY;
  PImageResourceDataEntry = ^TImageResourceDataEntry;

type
  PACTIVATION_CONTEXT_DATA = ^ACTIVATION_CONTEXT_DATA;
  ACTIVATION_CONTEXT_DATA = record
    Magic: Cardinal;
    HeaderSize: Cardinal;
    FormatVersion: Cardinal;
    TotalSize: Cardinal;
    DefaultTocOffset: Cardinal;
    ExtendedTocOffset: Cardinal;
    AssemblyRosterOffset: Cardinal;
    Flags: Cardinal;
  end;
  TActivationContextData = ACTIVATION_CONTEXT_DATA;
  PActivationContextData = ^TActivationContextData;

const
  STATUS_SUCCESS = $00000000;
  STATUS_INVALID_PARAMETER = $C000000D;
  STATUS_SXS_INVALID_ACTCTXDATA_FORMAT = $C0150003;
  STATUS_RESOURCE_DATA_NOT_FOUND = $C0000089;

(* Windows XP version, which has only one argument. For higher OSes we will use LdrRegisterDllNotification. *)
procedure LdrSetDllManifestProber(Routine: TLdrManifestProberRoutine); stdcall;
  external 'ntdll.dll' name 'LdrSetDllManifestProber' delayed;

function LdrFindResource_U(BaseAddress: Pointer; const ResourceInfo:
  TLdrResourceInfo; Level: Cardinal;
  out ResourceDataEntry: TImageResourceDataEntry): Cardinal; stdcall; external 'ntdll.dll';

function RtlCreateActivationContext(Flags: Cardinal;
  ActivationContextData: Pointer; ExtraBytes: Cardinal;
  NotificationRoutine: Pointer; NotificationContext: Pointer;
  out ActCtx: Pointer): Cardinal; stdcall; external 'ntdll.dll';

function IsWindowsXP: Boolean; inline;
begin
  Result := (Win32MajorVersion = 5) and (Win32MinorVersion = 1);
end;

procedure LdrDllNotification(NotificationReason: TNotificationReason; const NotificationData: TLdrDllNotificationData; Context: Pointer); stdcall;
var
  Name: array[0..4096 - 1] of WideChar;
begin
  if NotificationReason = LDR_DLL_NOTIFICATION_REASON_LOADED then
  begin
    StrLCopy(Name, NotificationData.Loaded.FullDllName.Buffer, NotificationData.Loaded.FullDllName.Length);

    if @Callback <> nil then
      Callback(Name, NotificationData.Loaded.DllBase);
  end;
end;

(* BasepProbeForDllManifest from ReactOS *)
function BasepProbeForDllManifest(DllHandle: Pointer; FullDllName: PWideChar; out ActCtx: Pointer): Cardinal; stdcall;
var
  Info: TLdrResourceInfo;
  Entry: TImageResourceDataEntry;
  Context: ACTCTXW;
  Status: Cardinal;
  Res: Pointer;
begin
  if PPointer(@ActCtx) = nil then
    Exit(STATUS_INVALID_PARAMETER);

  ActCtx := nil;

  Info.&Type := Cardinal(RT_MANIFEST);
  Info.Name := Cardinal(ISOLATIONAWARE_MANIFEST_RESOURCE_ID);
  Info.Language := 0;

  Status := LdrFindResource_U(DllHandle, Info, 3, Entry);
  if Status = STATUS_SUCCESS then
  begin
    Context.cbSize := SizeOf(Context);
    Context.lpSource := FullDllName;
    Context.dwFlags := ACTCTX_FLAG_RESOURCE_NAME_VALID or ACTCTX_FLAG_HMODULE_VALID;
    Context.hModule := Cardinal(DllHandle);
    Context.lpResourceName := ISOLATIONAWARE_MANIFEST_RESOURCE_ID;

    Status := RtlCreateActivationContext(0, @Context, 0, nil, nil, Res);
    if Status = STATUS_SUCCESS then
      ActCtx := Res
    else
    if Status = STATUS_SXS_INVALID_ACTCTXDATA_FORMAT then
    begin
      Status := STATUS_RESOURCE_DATA_NOT_FOUND;
    end;
  end
  else
  begin
    Exit(STATUS_SUCCESS);
  end;

  Exit(Status);
end;

function LdrDllNotificationXP(DllHandle: Pointer; FullDllName: PWideChar; out ActivationContext: Pointer): Cardinal; stdcall;
var
  Name: array[0..4096 - 1] of WideChar;
begin
  GetModuleFileNameW(Cardinal(DllHandle), Name, SizeOf(Name));

  if @Callback <> nil then
    Callback(Name, DllHandle);

  Result := BasepProbeForDllManifest(DllHandle, FullDllName, ActivationContext);
end;

procedure Setup(Callback: TDllCallback);
var
  Cookie: LongWord;
begin
  Xander.DllCallback.Callback := Callback;

  if IsWindowsXP then
    LdrSetDllManifestProber(LdrDllNotificationXP)
  else
    LdrRegisterDllNotification(0, @LdrDllNotification, nil, @Cookie);
end;

end.
