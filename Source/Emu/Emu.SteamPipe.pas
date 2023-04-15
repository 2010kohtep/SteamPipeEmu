unit Emu.SteamPipe;

{$I Default.inc}

{.$DEFINE PIPE_TERMINATE_IF_CMD_NOT_FOUND}

{$A4}

interface

uses
  Winapi.Windows,
  Winapi.WinSock,

  Emu.GCDetour,

  Steam.Callback,

  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Math,

  Xander.Memoria,
  Xander.MasterServer,
  Xander.Buffer,
  Xander.Console;

type
{$REGION 'Steam Types'}
{$A8}
  HTTPRequestCompleted_t = record
    Request: UInt32;
    ContextValue: UInt64;
    RequestSuccessful: Boolean;
    StatusCode: Int32;
    BodySize: UInt32;

    class function Create: HTTPRequestCompleted_t; static;
  end;
  THTTPRequestCompleted = HTTPRequestCompleted_t;
  {$IF SizeOf(THTTPRequestCompleted) <> 32} {$MESSAGE ERROR 'Structure size mismatch @ THTTPRequestCompleted.'} {$IFEND}
{$A4}

const
  SteamMaster_SharedMemFile = 'Steam3Master_SharedMemFile';
  SteamMaster_SteamMemLock = 'Steam3Master_SharedMemLock';

function IsSteamLaunched: Boolean;

type
  ELauncherType =
  (
    k_ELauncherTypeDefault = 0,	// default
    k_ELauncherTypePw_dota2,	  // pw_dota2
    k_ELauncherTypeNexon_dota2,	// nexon_dota2
    k_ELauncherTypeSteamcmd,    // steamcmd
    k_ELauncherTypePw_csgo,     // pw_csgo
    k_ELauncherTypeClientui,    // clientui
    k_ELauncherTypeSteamhdl,    // steamhdl
    k_ELauncherTypeSteamchina,	// steamchina
    k_ELauncherTypeSingleapp    // singleapp
  );

  // Steam account types
  EAccountType =
  (
    k_EAccountTypeInvalid = 0,
    k_EAccountTypeIndividual = 1,		   // single user account
    k_EAccountTypeMultiseat = 2,		   // multiseat (e.g. cybercafe) account
    k_EAccountTypeGameServer = 3,		   // game server account
    k_EAccountTypeAnonGameServer = 4,	 // anonymous game server account
    k_EAccountTypePending = 5,			   // pending
    k_EAccountTypeContentServer = 6,	 // content server
    k_EAccountTypeClan = 7,
    k_EAccountTypeChat = 8,
    k_EAccountTypeConsoleUser = 9,		 // Fake SteamID for local PSN account on PS3 or Live account on 360, etc.
    k_EAccountTypeAnonUser = 10
  );

const
  k_EAccountTypeMax = 11;

type
  EUniverse =
  (
    k_EUniverseInvalid = 0,
    k_EUniversePublic = 1,
    k_EUniverseBeta = 2,
    k_EUniverseInternal = 3,
    k_EUniverseDev = 4
    // k_EUniverseRC = 5,				// no such universe anymore
  );

type
  TSteamID = record
  strict private
    // unique account identifier
    function GetAccountID: LongWord;
    procedure SetAccountID(AValue: LongWord);

    // dynamic instance ID
    function GetAccountInstance: Cardinal;
    procedure SetAccountInstance(AValue: Cardinal);

    // type of account - can't show as EAccountType, due to signed / unsigned difference
    function GetAccountType: Cardinal;
    procedure SetAccountType(AValue: Cardinal);

    // universe this account belongs to
    function GetUniverse: Cardinal;
    procedure SetUniverse(AValue: Cardinal);
  public
    Value: UInt64;

    class operator Implicit(AValue: LongWord): TSteamID;
    class operator Implicit(AValue: UInt64): TSteamID;

    property ID: LongWord read GetAccountID write SetAccountID;
    property Instance: LongWord read GetAccountInstance write SetAccountInstance;
    property &Type: LongWord read GetAccountType write SetAccountType;
    property Universe: LongWord read GetUniverse write SetUniverse;
  end;

type
  EAppOwnershipFlags =
  (
    k_EAppOwnershipFlagsNone              =	0,
    k_EAppOwnershipFlagsOwnsLicense       = 1 shl 0,
    k_EAppOwnershipFlagsFreeLicense       = 1 shl 1,
    k_EAppOwnershipFlagsRegionRestricted  =	1 shl 2,
    k_EAppOwnershipFlagsLowViolence       = 1 shl 3,
    k_EAppOwnershipFlagsInvalidPlatform   = 1 shl 4,
    k_EAppOwnershipFlagsSharedLicense     = 1 shl 5,
    k_EAppOwnershipFlagsFreeWeekend       = 1 shl 6,
    k_EAppOwnershipFlagsLockedLicense     = 1 shl 7,
    k_EAppOwnershipFlagsPending	          = 1 shl 8,
    k_EAppOwnershipFlagsExpired	          = 1 shl 9,
    k_EAppOwnershipFlagsPermanent	        = 1 shl 10,
    k_EAppOwnershipFlagsRecurring     	  = 1 shl 11
  );

  // A structure that describes the level of application proficiency
  //
  // In CS:GO, in the client.dll library, the SteamID value is checked against
  // the value (most likely) obtained from the IClientUser::GetSteamID call, and
  // if it does not match, then the application cannot be purchased.
  TAppStateInfo = packed record
    dword0: Int32;
    Flags: Int32; // EAppOwnershipFlags
    dword8: Int32;
    SteamID: UInt64;
    dword14: Int32;
    dword18: Int32;
    GameVersion: Int32; // from Steam\appcache\appinfo.vdf (CAppInfoCache::ReadFromDisk)
    dword20: Int32;
    dword24: Int32;
  end;
  {$IF SizeOf(TAppStateInfo) <> 40} {$MESSAGE ERROR 'Structure size mismatch @ TAppStateInfo.'} {$IFEND}

type
  JobID_t = UInt64;
  TJobID = JobID_t;

// used for message types in GCSDK where we don't have the actual enum
type
  MsgType_t = Int32;
  TMsgType = MsgType_t;

const
  k_EMsgProtoBufFlag = $80000000;

//-----------------------------------------------------------------------------
// Purpose: Header for messages from a client or gameserver to or from the GC
//-----------------------------------------------------------------------------
type
  // I love the deprecated object type... why? Because binary works in exactly
  // the same way as record, but it allows you to do inheritance.

  GCMsgHdr_t = packed object
    Msg: TMsgType;         // The message type
    SrcGCDirIndex: UInt32; // The GC index that this message was sent from (set to the same as the current GC if not routed through another GC)
    SteamID: UInt64;       // User's SteamID
  end;
  TGCMsgHdr = GCMsgHdr_t;
  {$IF SizeOf(TGCMsgHdr) <> 16} {$MESSAGE ERROR 'Structure size mismatch @ TGCMsgHdr.'} {$IFEND}

  GCMsgHdrEx_t = packed object(GCMsgHdr_t)
    Version: UInt16;
    JobIDTarget: TJobID;
    JobIDSource: TJobID;
  end;
  TGCMsgHdrEx = GCMsgHdrEx_t;

  ProtoBufMsgHeader_t = packed record
    EMsgFlagged: Int32; // High bit should be set to indicate this message header type is in use.  The rest of the bits indicate message type.
    ProtoBufExtHdr: Int32; // Size of the extended header which is a serialized protobuf object.  Indicates where it ends and the serialized body protobuf begins.
  end;
  TProtoBufMsgHeader = ProtoBufMsgHeader_t;

var
  SteamID: TSteamID;
  AppStateInfo: TAppStateInfo;

// SteamPipe package handlers are located in CSteamEngine::RunInterface
//
// The structure of each package is as follows:
// [UInt8] [UInt32] [...]
// Where UInt8 is the interface index and UInt32 is the interface message index
//
// Each interface has its own list of message handlers, for example -
// the interface IClientUtilsDispatchMsg has index 4 and the message GetLauncherType
// has an index of 1606 (0x646).

type
  PSteamSharedMemory = ^TSteamSharedMemory;
  TSteamSharedMemory = record
  strict private
  {$HINTS OFF}
    Unknown: Integer;
  {$HINTS ON}
  public
    // SteamPipe initialization state.
    //   0 - Available for initialization; set by client
    //   1 - Connected to server's pipe and waiting for client-side init; set by server
    //   2 - Client-side performed its initialization; set by client
    //   3 - Server-side performed some handle checks; set by server
    MasterState: Integer;

    SVProcessId: Integer; // Server's process idenfitier
    CLProcessId: Integer; // Client's process identifier

    Input: THandle; // Handle for reading pipe handle
    Output: THandle; // Handle for writing pipe handle

    SyncRead: THandle; // Read sync event
    SyncWrite: THandle; // Write sync event

    Success: Boolean; // Successfully inited all handles
  end;
{$ENDREGION}

  TSteamPipeDispatchMsg = function(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean of object;

  TSteamPipeServer = class;

  TGCInternalMessage = record
    Header: TGCMsgHdr;
    Data: TBytes;
  end;

  TDispatchInterfaceMap = class
  protected
    FDispatch: TDictionary<Integer, TSteamPipeDispatchMsg>;
  public
    constructor Create;
    destructor Destroy; override;

    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  end;

  TIClientUserMap = class(TDispatchInterfaceMap)
  strict private
    function BLoggedOn(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetSteamID(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetUserDataFolder(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetAuthSessionTicket(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function CancelAuthTicket(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function BIsSubscribedApp(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function BSetDurationControlOnlineState(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientGameServerMap = class(TDispatchInterfaceMap)
  strict private
    function BeginAuthSession(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientFriendsMap = class(TDispatchInterfaceMap)
  strict private
    function GetFriendCount(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetFriendByIndex(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetPersonaName(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetSmallFriendAvatar(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetMediumFriendAvatar(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function ActivateGameOverlayToWebPage(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function RequestUserInformation(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SetRichPresence(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetFriendPersonaName_Public(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientUtilsMap = class(TDispatchInterfaceMap)
  strict private
    FCheckFileSignatureIndex: UInt64;
  strict private
    function GetConnectedUniverse(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetIPCountry(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetServerRealTime(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetImageSize(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetCSERIPPort(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SetAppIDForCurrentPipe(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetAppID(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SetAPIDebuggingActive(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function IsAPICallCompleted(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetAPICallResult(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function CheckFileSignature(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetSteamUILanguage(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function InitFilterText(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function IsSteamChinaLauncher(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function RecordSteamInterfaceCreation(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientMatchmakingMap = class(TDispatchInterfaceMap)
  strict private
    FGMSQueryIndex: UInt64;
    FMasterServer: TMasterServer;
  strict private
    function GetFavoriteGameCount(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function BeginGMSQuery(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function ReleaseGMSQuery(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function EnsureFavoriteGameAccountsUpdated(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function PollGMSQuery(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetGMSQueryResults(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientAppsMap = class(TDispatchInterfaceMap)
  strict private
    function GetAppData(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientUserStatsMap = class(TDispatchInterfaceMap)
  strict private
    function RequestCurrentStats(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientNetworkingMap = class(TDispatchInterfaceMap)
  strict private
    function AllowP2PPacketRelay(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientRemoteStorageMap = class(TDispatchInterfaceMap)
  strict private
    function EnumerateUserSubscribedFiles(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetQuota(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function FileExists(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientAppManagerMap = class(TDispatchInterfaceMap)
  strict private
    function GetActiveBeta(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetCurrentLanguage(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetAppStateInfo(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientGameCoordinatorMap = class(TDispatchInterfaceMap)
  strict private
    FParent: TSteamPipeServer;
    FGCMessages: TList<TGCInternalMessage>;
  strict private
    function SendMessage(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function IsMessageAvailable(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function RetrieveMessage(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create; overload;
    constructor Create(AParent: TSteamPipeServer); overload;

    function DispatchGCMessage(AMsg: TMsgType; const AData: IBufferDescription): Integer;
    procedure GC_SendWelcomeToClient;
  end;

  TIClientGameStatsMap = class(TDispatchInterfaceMap)
  strict private
    function GetNewSession(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientConfigStoreMap = class(TDispatchInterfaceMap)
  strict private
    function GetInt(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetString(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientHTTPMap = class(TDispatchInterfaceMap)
  strict private
    FHTTPRequestIndex: Cardinal;
    FSteamAPICallIndex: Cardinal;
  strict private
    function ReleaseHTTPRequest(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function CreateHTTPRequest(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SetHTTPRequestHeaderValue(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SendHTTPRequest(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientControllerSerializedMap = class(TDispatchInterfaceMap)
  strict private
    function GetActionSetHandle(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function HasGameMapping(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientShaderMap = class(TDispatchInterfaceMap)
  strict private
    function ProcessShaderCache(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function SetupShaderCacheEnvironment(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientNetworkingSocketsSerializedMap = class(TDispatchInterfaceMap)
  strict private
    function GetCachedRelayTicketCount(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
    function GetCertAsync(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

  TIClientNetworkingUtilsSerializedMap = class(TDispatchInterfaceMap)
  strict private
    function GetLauncherType(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  public
    constructor Create;
  end;

{$REGION 'IPC Processors'}
  TIPCProcessor = class
  protected
    FParent: TSteamPipeServer;
  public
    constructor Create(AParent: TSteamPipeServer);

    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; virtual; abstract;
  end;

  TIPC_NotImplemented = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_RunInterface = class(TIPCProcessor)
  strict private
    FMaps: TDictionary<Integer, TDispatchInterfaceMap>;

    FGCMessages: TList<TGCInternalMessage>;

    FHTTPRequestIndex: Cardinal;
    FSteamAPICallIndex: Cardinal; // SteamAPICall_t
    FGMSQueryIndex: Cardinal;
    FCheckFileSignatureIndex: Cardinal;
  public
    constructor Create(AParent: TSteamPipeServer);
    destructor Destroy; override;

    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_SerializeCallbacks = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_ConnectUser = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_DisconnectUser = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_TerminatePipe = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_Ping = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;

  TIPC_ConnectPipe = class(TIPCProcessor)
  public
    function Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean; override;
  end;
{$ENDREGION}

  TSteamPipeServer = class
  strict private
    FFileMap: THandle;
    FEvent: THandle;

    FHandlers: array[0..9] of TIPCProcessor;

    function CallSerializedFunction(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
  private
    FSharedMem: PSteamSharedMemory;
    FCallbackCompleted: Boolean;
    FCallbackQueue: TList<TCallbackMsg>;

    function SendBuffer(const ABuffer: IBufferDescription; AClear: Boolean = True): Boolean;

    procedure DispatchCallback(ASteamUser: Cardinal; ACallbackIndex: Integer; AData: Pointer; ASize: Integer); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Frame;
  end;

implementation

class function HTTPRequestCompleted_t.Create: HTTPRequestCompleted_t;
begin
  Result.Request := 0;
  Result.ContextValue := 0;
  Result.RequestSuccessful := False;
  Result.StatusCode := 0;
  Result.BodySize := 0;
end;

{ TSteamPipeServer }

constructor TSteamPipeServer.Create;
var
  I: Integer;
  NI: TIPC_NotImplemented;
begin
  inherited;

  FillChar(FHandlers, SizeOf(FHandlers), 0);

  NI := TIPC_NotImplemented.Create(Self);
  for I := 0 to Length(FHandlers) - 1 do
    FHandlers[I] := NI;

  FHandlers[1] := TIPC_RunInterface.Create(Self);
  FHandlers[2] := TIPC_SerializeCallbacks.Create(Self);
  FHandlers[3] := TIPC_ConnectUser.Create(Self);
  FHandlers[4] := TIPC_DisconnectUser.Create(Self);
  FHandlers[5] := TIPC_TerminatePipe.Create(Self);
  FHandlers[6] := TIPC_Ping.Create(Self);
  FHandlers[9] := TIPC_ConnectPipe.Create(Self);

  FFileMap := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, 1024, SteamMaster_SharedMemFile);
  FEvent := CreateEvent(nil, False, False, SteamMaster_SteamMemLock);

  if FEvent = 0 then
    raise Exception.Create('TSteamPipeServer.Create: FEvent = 0');

  FSharedMem := MapViewOfFile(FFileMap, FILE_MAP_WRITE, 0, 0, 0);

  FSharedMem.MasterState := 0; // k_EMasterStateAvailable
  FSharedMem.SVProcessId := GetCurrentProcessId;
  FSharedMem.CLProcessId := 0;

  FCallbackCompleted := True;
  FCallbackQueue := TList<CallbackMsg_t>.Create;
end;

destructor TSteamPipeServer.Destroy;
begin
  // TODO: Free FFileMap
  // TODO: Free FEvent
  // TODO: Free all IIPCProcessor
  // TODO: Free FCallbackQueue

  inherited;
end;

procedure TSteamPipeServer.Frame;
var
  Data: array[0..1023] of Byte;
  Read, Summary, Waiting: Cardinal;

  TmpBuf: IWriteBuffer;
  InBuf: IReadBuffer;
  OutBuf: IWriteBuffer;
begin
  case FSharedMem.MasterState of
    0:
    begin
      SetEvent(FEvent);
      Sleep(500);
    end;

    1:
    begin
      FSharedMem.MasterState := 2;
    end;

    2:
    begin
      // Do nothing
    end;

    3:
    begin
      if not FSharedMem.Success then
        Exit;

      WaitForSingleObject(FSharedMem.SyncRead, INFINITE);

      if ReadFile(FSharedMem.Input, Waiting, SizeOf(Waiting), Read, nil) then
      begin
        TmpBuf := TReadAndWriteBuffer.CreateWrite;
        OutBuf := TReadAndWriteBuffer.CreateWrite;

        Read := 0;
        Summary := 0;

        while Summary < Waiting do
        begin
          if not ReadFile(FSharedMem.Input, Data[0], Min(SizeOf(Data), Waiting), Read, nil) then
            Break;

          Inc(Summary, Read);
          TmpBuf.WriteData(@Data, Read);
        end;

        InBuf := TReadAndWriteBuffer.CreateRead(TmpBuf.Data, TmpBuf.Size);

        while not InBuf.Eof do
        begin
          if not CallSerializedFunction(InBuf, OutBuf) then
          begin
          {$IFDEF PIPE_TERMINATE_IF_CMD_NOT_FOUND}
            TConsole.Error('[TSteamPipeServer] Could not process command, terminating...');
            Exit;
          {$ELSE}
            Break;
          {$ENDIF}
          end;
        end;

        if OutBuf.Size > 0 then
        begin
          Summary := OutBuf.Size;
          WriteFile(FSharedMem.Output, Summary, SizeOf(Cardinal), Read, nil);
          WriteFile(FSharedMem.Output, OutBuf.Data^, OutBuf.Size, Read, nil);

          SetEvent(FSharedMem.SyncWrite);
        end;

        if FCallbackQueue.Count > 0 then
        begin
          if FCallbackCompleted then
          begin
            FCallbackCompleted := False;

            OutBuf.Clear;
            OutBuf.WriteInt8(7);

            Summary := OutBuf.Size;
            WriteFile(FSharedMem.Output, Summary, SizeOf(Cardinal), Read, nil);
            WriteFile(FSharedMem.Output, OutBuf.Data^, OutBuf.Size, Read, nil);

            SetEvent(FSharedMem.SyncWrite);
          end;
        end;
      end;
    end;

    else
    begin

    end;
  end;
end;

function TSteamPipeServer.SendBuffer(const ABuffer: IBufferDescription; AClear: Boolean): Boolean;
var
  Summary, Read: Cardinal;
begin
  Summary := ABuffer.Size;

  if not WriteFile(FSharedMem.Output, Summary, SizeOf(Cardinal), Read, nil) then
    Exit(False);

  if not WriteFile(FSharedMem.Output, ABuffer.Data^, ABuffer.Size, Read, nil) then
    Exit(False);

  if not SetEvent(FSharedMem.SyncWrite) then
    Exit(False);

  if AClear then
    ABuffer.Clear;

  Exit(True);
end;

procedure TSteamPipeServer.DispatchCallback(ASteamUser: Cardinal; ACallbackIndex: Integer;
  AData: Pointer; ASize: Integer);
begin
  FCallbackQueue.Add(CallbackMsg_t.Create(ASteamUser, ACallbackIndex, AData, ASize));
end;

function TSteamPipeServer.CallSerializedFunction(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  Hdr: Byte;
begin
  Hdr := ABuffer.ReadUInt8; // CIPCServer::CallSerializedFunctionInternal

  if Hdr < Length(FHandlers) then
  begin
    Result := FHandlers[Hdr].Process(ABuffer, AResponse);
    Exit;
  end
  else
  begin
    TConsole.Error('[SteamPipe] IPC command %d is not implemented.', [Hdr]);
    Exit(False);
  end;
end;

{ TIPC_NotImplemented }

function TIPC_NotImplemented.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  TConsole.Error('[SteamPipe] Message with header %d is not implemented.', [ABuffer.Data^]);
  Exit(False);
end;

{ TIPC_RunInterface }

constructor TIPC_RunInterface.Create(AParent: TSteamPipeServer);
begin
  inherited Create(AParent);

  FMaps := TDictionary<Integer, TDispatchInterfaceMap>.Create;

  FGCMessages := TList<TGCInternalMessage>.Create;

  FHTTPRequestIndex := 1;
  FSteamAPICallIndex := 1;
  FGMSQueryIndex := 1;
  FCheckFileSignatureIndex := 1;

  // In general, I think that the ECallbackType type has the most direct value
  // to these indexes and something can be done with them.

  with FMaps do
  begin
    Add( 1, TIClientUserMap.Create);
    Add( 2, TIClientGameServerMap.Create);
    Add( 3, TIClientFriendsMap.Create);
    Add( 4, TIClientUtilsMap.Create);
    Add( 6, TIClientMatchmakingMap.Create);
    Add( 8, TIClientAppsMap.Create);
    Add(11, TIClientUserStatsMap.Create);
    Add(12, TIClientNetworkingMap.Create);
    Add(13, TIClientRemoteStorageMap.Create);
    Add(17, TIClientAppManagerMap.Create);
    Add(19, TIClientGameCoordinatorMap.Create(AParent));
    Add(21, TIClientGameStatsMap.Create);
    Add(18, TIClientConfigStoreMap.Create);
    Add(22, TIClientHTTPMap.Create);
    Add(41, TIClientControllerSerializedMap.Create);
    Add(45, TIClientShaderMap.Create);
    Add(46, TIClientNetworkingSocketsSerializedMap.Create);
    Add(50, TIClientNetworkingUtilsSerializedMap.Create);
  end;
end;

destructor TIPC_RunInterface.Destroy;
begin
  FMaps.Free;

  inherited;
end;

function SteamAPI_GetHSteamUser: LongWord; cdecl; external 'steam_api.dll';

var
  orgPostInternalCallback: procedure(HSteamUser: Cardinal; CallbackID: Integer; Data: PByte; Size: Cardinal; GameServer: Boolean); stdcall;

procedure PostInternalCallback(CallbackID: Integer; Data: PByte; Size: Cardinal; GameServer: Boolean);
begin
  TModule.CreateModule('steamclient.dll').CreatePattern(orgPostInternalCallback).FindPattern([$55, $8B, $EC, $83, $EC, $1C, $53, $56, $57, $6A, $04, $6A, $00, $6A, $04, $6A, $00]);

  if @orgPostInternalCallback <> nil then
  asm
    call SteamAPI_GetHSteamUser
    mov edx, eax

    movzx eax, byte ptr [GameServer]

    push eax
    push [Size]
    push [Data]
    push [CallbackID]
    push edx

    xor ecx, ecx
    call [orgPostInternalCallback]
  end;

  //orgPostInternalCallback(DoubleCall, CallbackID, Data, Size, GameServer);
end;

function TIPC_RunInterface.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  Msg: Cardinal;
  I: TDispatchInterfaceMap;
begin
  Msg := ABuffer.ReadUInt8;
  ABuffer.ReadUInt32; // SteamUser_t (handle)?

  // Absolutely every RunInterface call first reads a stub byte, which can
  // take on any value. Its purpose is to let the pipe server know that we
  // saw the packet and gave a response to it. Usually, methods respond with
  // a byte with a value of 1, and after it they write a full response to
  // the request. However, if for some reason we could not give an answer,
  // then it is enough just to write this byte here, otherwise the process
  // will hang in anticipation.
  AResponse.WriteUInt8(1);

  if not FMaps.TryGetValue(Msg, I) then
  begin
    TConsole.Error('[RunInterface] Interface %d is not implemented.', [Msg]);
    Exit(False);
  end;

  if not Assigned(I) then
  begin
    TConsole.Error('[RunInterface] Interface %d found, but cannot be called.', [Msg]);
    Exit(False);
  end;

  Result := I.Process(ABuffer, AResponse);

  if Result then
  begin
    AResponse.WriteUInt8(1);
    AResponse.WriteUInt32(0);
  end;
end;

{ TIPC_ConnectUser }

function TIPC_ConnectUser.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  ConnectUserCmd: Byte;
  SpecPayload: Boolean;
begin
  AResponse.WriteUInt8(3);

  SpecPayload := False;
  ConnectUserCmd := ABuffer.ReadInt8;

  if ConnectUserCmd = 1 then
  begin
    // CreateGlobalUser
    // Must be called ONCE. If not the first one is called,
    // then ConnectUserCmd should be set to 2

    if ABuffer.ReadInt8 <> Ord(k_EAccountTypeIndividual) then
    begin
      TConsole.Error('Attempting to create a global user of the wrong user type.');
      Exit(False);
    end;

    SpecPayload := True;
    ConnectUserCmd := 2;
  end;

  if ConnectUserCmd = 2 then
  begin
    AResponse.WriteUInt32(SteamID.ID); // SteamUserID?
    Exit(True);
  end;

  if ConnectUserCmd = 3 then
  begin
    if not SpecPayload then
    begin
      // TODO: CSteamEngine::CreateUserInstance

      ABuffer.ReadUInt8;
      AResponse.WriteUInt32(SteamID.ID); // SteamUserID?
    end;
  end;

  Exit(True);
end;

{ TIPC_DisconnectUser }

function TIPC_DisconnectUser.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  Exit(False);
end;

{ TIPC_TerminatePipe }

function TIPC_TerminatePipe.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  TConsole.Error('[SteamPipe] Pipe terminated.');
  Exit(False);
end;

{ TIPC_Ping }

function TIPC_Ping.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  AResponse.WriteUInt8(6);
  Exit(True);
end;

{ TIPC_ConnectPipe }

function TIPC_ConnectPipe.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  if ABuffer.ReadUInt32 = 1 then
  begin
    ABuffer.ReadUInt32; // ProcessID
    ABuffer.ReadUInt32; // ThreadID

    AResponse.WriteUInt32(GetCurrentProcessId);
    AResponse.WriteUInt32(GetCurrentThreadId);
    AResponse.WriteUInt32(31337);
  end
  else
  begin
    AResponse.WriteInt32(-1);
    AResponse.WriteInt32(-1);
    AResponse.WriteInt32(-1);
  end;

  Exit(True);
end;

{ TIPC_SerializeCallbacks }

function TIPC_SerializeCallbacks.Process(const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  Buf: IWriteBuffer;
  CB: TCallbackMsg;
begin
  if FParent.FCallbackQueue.Count = 0 then
    Exit(False);

  Buf := TReadAndWriteBuffer.CreateWrite;

  for CB in FParent.FCallbackQueue do
  begin
    with CB do
    begin
      Buf.WriteInt8(2);
      Buf.WriteInt32(SteamUser);
      Buf.WriteInt32(Callback);
      Buf.WriteInt32(ParamSize);

      if ParamSize > 0 then
        Buf.WriteData(ParamData, ParamSize);
    end;

    FParent.SendBuffer(Buf);
  end;

  FParent.FCallbackQueue.Clear;

  Buf.WriteInt8(2);
  Buf.WriteInt32(0);
  Buf.WriteInt32(0);
  Buf.WriteInt32(0);
  FParent.SendBuffer(Buf);

  FParent.FCallbackCompleted := True;
  Exit(True);
end;

//var
//  Output: IWriteBuffer;
//  Read: Cardinal;
//  B: Byte;
//begin
//{$IFDEF DEBUG}
//  if FParent.FCallbackQueue.Count = 0 then
//  begin
//    TConsole.Error('[SerializeCallbacks] Callback request received, but there is no callbacks in queue.');
//    Exit(False);
//  end;
//{$ENDIF}
//
//  Output := TReadAndWriteBuffer.CreateWrite;
//
//  repeat
//    with FParent.FCallbackQueue.First do
//    begin
//    {$IFDEF DEBUG}
//      TConsole.Success('[SerializeCallbacks] Calling %d', [Callback]);
//    {$ENDIF}
//
//      Output.Clear;
//      Output.WriteInt32(1 + 12 + ParamSize);
//      Output.WriteInt8(2);
//      Output.WriteInt32(SteamUser);
//      Output.WriteInt32(Callback);
//      Output.WriteInt32(ParamSize);
//      Output.WriteData(ParamData, ParamSize);
//
//      if not WriteFile(FParent.FSharedMem.Output, Output.Data^, Output.Size, Read, nil) then
//      begin
//      {$IFDEF DEBUG}
//        TConsole.Error('[SerializeCallbacks] Failed to write callback response.');
//      {$ENDIF}
//        Exit(False);
//      end;
//
//      SetEvent(FParent.FSharedMem.SyncWrite);
//
//      FreeMemory(ParamData);
//    end;
//
//    FParent.FCallbackQueue.Delete(0);
//
//    WaitForSingleObject(FParent.FSharedMem.SyncRead, INFINITE);
//
//    if not ReadFile(FParent.FSharedMem.Input, B, SizeOf(B), Read, nil) then
//    begin
//    {$IFDEF DEBUG}
//      TConsole.Error('[SerializeCallbacks] Failed to read next callback request.');
//    {$ENDIF}
//      Exit(False);
//    end;
//
//    if B <> 2 then
//    begin
//    {$IFDEF DEBUG}
//      TConsole.Error('[SerializeCallbacks] Received %d message, but expected 2.', [B]);
//    {$ENDIF}
//      Exit(False);
//    end;
//
//    if FParent.FCallbackQueue.Count = 0 then
//    begin
//      Output.Clear;
//      Output.WriteInt32(1 + 12);
//      Output.WriteInt8(2);
//      Output.WriteInt32(0);
//      Output.WriteInt32(0);
//      Output.WriteInt32(0);
//
//      WriteFile(FParent.FSharedMem.Output, Output.Data^, Output.Size, Read, nil);
//      SetEvent(FParent.FSharedMem.SyncWrite);
//      Break;
//    end;
//  until False;
//
//  FParent.FCallbackCompleted := True;
//  Exit(True);
//end;

{ TSteamID }

function TSteamID.GetAccountID: LongWord;
begin
  Result := Value and $FFFFFFFF;
end;

function TSteamID.GetAccountInstance: Cardinal;
begin
  Result := (Value shr 32) and $FFFFF;
end;

function TSteamID.GetAccountType: Cardinal;
begin
  Result := (Value shr 52) and $F;
end;

function TSteamID.GetUniverse: Cardinal;
begin
  Result := (Value shr 56) and $FF;
end;

class operator TSteamID.Implicit(AValue: LongWord): TSteamID;
begin
  PUInt64(@Result)^ := AValue;
end;

class operator TSteamID.Implicit(AValue: UInt64): TSteamID;
begin
  PUInt64(@Result)^ := AValue;
end;

procedure TSteamID.SetAccountID(AValue: LongWord);
begin

end;

procedure TSteamID.SetAccountInstance(AValue: Cardinal);
begin

end;

procedure TSteamID.SetAccountType;
begin

end;

procedure TSteamID.SetUniverse;
begin

end;

{ Helpers }

function IsSteamLaunched: Boolean;
var
  H: THandle;
begin
  H := CreateFile(SteamMaster_SharedMemFile, GENERIC_READ or GENERIC_WRITE, 0, nil, 0, FILE_ATTRIBUTE_NORMAL, 0);
  Result := H <> INVALID_HANDLE_VALUE;

  if Result then
    CloseHandle(H);
end;

{ TIPCProcessor }

constructor TIPCProcessor.Create(AParent: TSteamPipeServer);
begin
  FParent := AParent;
end;

{ TSteamPipeMap }

constructor TDispatchInterfaceMap.Create;
begin
  inherited;

  FDispatch := TDictionary<Integer, TSteamPipeDispatchMsg>.Create;
end;

destructor TDispatchInterfaceMap.Destroy;
begin
  FDispatch.Free;

  inherited;
end;

function TDispatchInterfaceMap.Process(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  Msg: Cardinal;

  F: TSteamPipeDispatchMsg;
begin
  Msg := ABuffer.ReadUInt32;

  if FDispatch.TryGetValue(Msg, F) and (@F <> nil) then
    Result := F(ABuffer, AResponse)
  else
  begin
    TConsole.Error('[%s] Message %d is not implemented.', [ClassName, Msg]);
    Exit(False);
  end;
end;

{ TIClientUserMap }

function TIClientUserMap.BIsSubscribedApp(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // Checks if the active user is subscribed to a specified AppId.
  //
  // Only use this if you need to check ownership of another game related to yours, a demo for example.
  //
  // Returns: bool
  // true if the active user is subscribed to the specified App ID, otherwise false.

  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1); // Yeah, I own it!

  Result := True;
end;

function TIClientUserMap.BLoggedOn(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // returns true if the Steam client current has a live connection to the Steam servers.
  // If false, it means there is no active connection due to either a networking issue on the local machine, or the Steam server is down/busy.
  // The Steam client will automatically be trying to recreate the connection as often as possible.
  //
  // NOTE: This method is used by the game coordinator in CGCClientSystem::ThinkConnection.

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);

  Result := True;
end;

function TIClientUserMap.BSetDurationControlOnlineState(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0);

  Result := True;
end;

function TIClientUserMap.CancelAuthTicket(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // Cancel auth ticket from GetAuthSessionTicket, called when no longer playing game with the entity you gave the ticket to

  ABuffer.ReadUInt32; // HAuthTicket
  ABuffer.ReadUInt32; // Fencepost

  Result := True;
end;

constructor TIClientUserMap.Create;
begin
  inherited;

  FDispatch.Add(30, BLoggedOn);
  FDispatch.Add(34, GetSteamID);
  FDispatch.Add(265, GetUserDataFolder);
  FDispatch.Add(391, GetAuthSessionTicket);
  FDispatch.Add(417, CancelAuthTicket);
  FDispatch.Add(621, BIsSubscribedApp);
  FDispatch.Add(771, BSetDurationControlOnlineState);
end;

function TIClientUserMap.GetAuthSessionTicket(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  L: Cardinal;
  P: Pointer;

  Ticket: IWriteBuffer;
begin
  // Retrieve ticket to be sent to the entity who wishes to authenticate you.
  // pcbTicket retrieves the length of the actual ticket.
  //
  // The function should not write the SteamID value to the ticket!

  L := ABuffer.ReadUInt32; // cbMaxTicket
  ABuffer.ReadUInt32; // Fencepost

  if L > 65535 then
  begin
    TConsole.Error('[IClientUser::GetAuthSessionTicket] Suspicious ticket size; Received %d, expected < 65536', [L]);
    Exit(False);
  end;

  P := GetMemory(L);
  if P = nil then
  begin
    TConsole.Error('[IClientUser::GetAuthSessionTicket] Could not allocate memory for ticket.');
    Exit(False);
  end;

  FillChar(P^, L, 0);

  Ticket := TReadAndWriteBuffer.CreateWrite(P, L);
  Ticket.WriteUInt32(20);
  Ticket.WriteUInt32(0);
  Ticket.WriteUInt32(0);
  Ticket.WriteUInt32(SteamID.ID);
  Ticket.WriteUInt32(0);
  Ticket.WriteUInt32(0);
  Ticket.WriteUInt32(0);

//  for I := 0 to L - 1 do
//    PByte(Integer(P) + I)^ := I mod 256;

  AResponse.WriteUInt32(1); // HAuthTicket
  AResponse.WriteData(P, L); // Session ticket data
  AResponse.WriteUInt32(Ticket.Size); // Actual ticket size

  FreeMemory(P);

  Result := True;
end;

function TIClientUserMap.GetSteamID(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(SteamID.Value);

  Result := True;
end;

function TIClientUserMap.GetUserDataFolder(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt64;
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // Length of data folder name?
  //AResponse.WriteData(nil, 0);

  Result := True;
end;

{ TIClientGameServerMap }

function TIClientGameServerMap.BeginAuthSession(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // Authenticate ticket ( from GetAuthSessionTicket ) from entity steamID to be sure it is valid and isnt reused
  // Registers for callbacks if the entity goes offline or cancels the ticket ( see ValidateAuthTicketResponse_t callback and EAuthSessionResponse )
  //
  // And Steam, as usual, is cunning (who would doubt it!). In fact, the interface is
  // not called immediately. The CGameServer::BeginAuthSession method is called first,
  // which wraps the interface call and performs the SteamID and ticket checks. All
  // CGameServer method variant checks must be called and satisfied in order for the
  // original interface to be called.
  //
  // A solution is to intercept CGameServer::BeginAuthSession and implement
  // its own handler, which will either always return k_EBeginAuthSessionResultOK,
  // or which will read pirated emulators and validate them on its own. In both
  // cases, this interface will not be called. Otherwise, the method will always
  // exit with k_EBeginAuthSessionResultInvalidTicket and the player will be
  // kicked with the reason 'Steam validation rejected'.

  ABuffer.ReadUInt32; // pAuthTicket
  ABuffer.ReadUInt32; // cbAuthTicket
  ABuffer.ReadUInt64; // CSteamID

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0); // EBeginAuthSessionResult

  Result := True;
end;

constructor TIClientGameServerMap.Create;
begin
  inherited;

  FDispatch.Add(873, BeginAuthSession);
end;

{ TIClientFriendsMap }

function TIClientFriendsMap.ActivateGameOverlayToWebPage(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  S: AnsiString;
begin
  S := '???';
  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  ABuffer.ReadUInt32;

  ABuffer.ReadUInt32; // Fencepost

  Result := True;
end;

constructor TIClientFriendsMap.Create;
begin
  inherited;

  FDispatch.Add(1029, GetPersonaName);
  FDispatch.Add(1042, GetSmallFriendAvatar);
  FDispatch.Add(1043, GetMediumFriendAvatar);
  FDispatch.Add(1088, GetFriendCount);
  FDispatch.Add(1089, GetFriendByIndex);
  FDispatch.Add(1376, ActivateGameOverlayToWebPage);
  FDispatch.Add(1388, RequestUserInformation);
  FDispatch.Add(1408, SetRichPresence);
  FDispatch.Add(1475, GetFriendPersonaName_Public);
end;

function TIClientFriendsMap.GetFriendByIndex(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // iFriend
  ABuffer.ReadUInt32; // iFriendFlags
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(0);

  Result := True;
end;

function TIClientFriendsMap.GetFriendCount(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // iFriendFlags?
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0);

  Result := True;
end;

function TIClientFriendsMap.GetFriendPersonaName_Public(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt64; // CSteamID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(7);
  AResponse.WriteLStr('noname');

  Result := True;
end;

function TIClientFriendsMap.GetMediumFriendAvatar(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt64; // CSteamID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(1);

  Result := True;
end;

function TIClientFriendsMap.GetPersonaName(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // returns the local players name - guaranteed to not be NULL.
  // this is the same name as on the users community profile page
  // this is stored in UTF-8 format
  // like all the other interface functions that return a char *, it's important that this pointer is not saved
  // off; it will eventually be free'd or re-allocated

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(7);
  AResponse.WriteLStr('Xander');

  Result := True;
end;

function TIClientFriendsMap.GetSmallFriendAvatar(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // gets the small (32x32) avatar of the current user, which is a handle to be used in IClientUtils::GetImageRGBA(), or 0 if none set

  ABuffer.ReadUInt64; // CSteamID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(1);

  Result := True;
end;

function TIClientFriendsMap.RequestUserInformation(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // requests information about a user - persona name & avatar
  // if bRequireNameOnly is set, then the avatar of a user isn't downloaded
  // - it's a lot slower to download avatars and churns the local cache, so if you don't need avatars, don't request them
  // if returns true, it means that data is being requested, and a PersonaStateChanged_t callback will be posted when it's retrieved
  // if returns false, it means that we already have all the details about that user, and functions can be called immediately

  ABuffer.ReadUInt64; // steamIDUser
  ABuffer.ReadUInt8; // bRequireNameOnly
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);

  Result := True;
end;

function TIClientFriendsMap.SetRichPresence(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  S, S2: AnsiString;
begin
  // Rich Presence data is automatically shared between friends who are in the same game
  // Each user has a set of Key/Value pairs
  // Up to 20 different keys can be set
  // There are two magic keys:
  //		"status"  - a UTF-8 string that will show up in the 'view game info' dialog in the Steam friends list
  //		"connect" - a UTF-8 string that contains the command-line for how a friend can connect to a game
  // GetFriendRichPresence() returns an empty string "" if no value is set
  // SetRichPresence() to a NULL or an empty string deletes the key
  // You can iterate the current set of keys for a friend with GetFriendRichPresenceKeyCount()
  // and GetFriendRichPresenceKeyByIndex() (typically only used for debugging)

  ABuffer.ReadUInt32;

  S := '???';
  S2 := '???';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  if ABuffer.ReadUInt8 > 0 then
    S2 := ABuffer.ReadLStr;

  //Log('IClientFriends::SetRichPresence', 'S1: %s; S2: %s', [S, S2]);

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0);

  Result := True;
end;

{ TIClientUtilsMap }

function TIClientUtilsMap.CheckFileSignature(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  S: AnsiString;
begin
  S := '???';
  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  ABuffer.ReadUInt32; // Fencepost

  TConsole.Write('[IClientUtils::CheckFileSignature] %s', [S], YELLOW);

  AResponse.WriteUInt64(FCheckFileSignatureIndex);
  Inc(FCheckFileSignatureIndex);

  Result := True;
end;

constructor TIClientUtilsMap.Create;
begin
  inherited;

  FCheckFileSignatureIndex := 1;

  FDispatch.Add(1557, GetConnectedUniverse);
  FDispatch.Add(1558, GetServerRealTime);
  FDispatch.Add(1559, GetIPCountry);
  FDispatch.Add(1560, GetImageSize);
  FDispatch.Add(1574, GetCSERIPPort);
  FDispatch.Add(1590, SetAppIDForCurrentPipe);
  FDispatch.Add(1591, GetAppID);
  FDispatch.Add(1592, SetAPIDebuggingActive);
  FDispatch.Add(1594, IsAPICallCompleted);
  FDispatch.Add(1603, GetAPICallResult);
  FDispatch.Add(1629, CheckFileSignature);
  FDispatch.Add(1653, GetSteamUILanguage);
  FDispatch.Add(1733, IsSteamChinaLauncher);
  FDispatch.Add(1736, InitFilterText);
  FDispatch.Add(1751, RecordSteamInterfaceCreation);
end;

function TIClientUtilsMap.GetAPICallResult(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt64; // hSteamAPICall
  ABuffer.ReadUInt32; // cubCallback
  ABuffer.ReadUInt32; // iCallbackExpected

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1); // GetAPICallResult result
  AResponse.WriteUInt32(1); // ECheckFileSignature?
//  for I := 0 to CallbackSize - 1 do
//    AResponse.WriteUInt8(0);
  AResponse.WriteUInt8(0); // 1 if failed

  Result := True;
end;

function TIClientUtilsMap.GetAppID(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(730);

  Result := True;
end;

function TIClientUtilsMap.GetConnectedUniverse(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(Ord(k_EUniversePublic)); // EUniverse

  Result := True;
end;

function TIClientUtilsMap.GetCSERIPPort(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // returns the IP of the reporting server for valve - currently only used in Source engine games

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // GetCSERIPPort result
  AResponse.WriteUInt32(0); // IP
  AResponse.WriteUInt16(0); // Port

  Result := True;
end;

function TIClientUtilsMap.GetImageSize(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // returns true if the image exists, and valid sizes were filled out

  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0);
  AResponse.WriteUInt32(0);
  AResponse.WriteUInt32(0);

  Result := True;
end;

function TIClientUtilsMap.GetIPCountry(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteInt8(1);

  AResponse.WriteInt8(3);
  AResponse.WriteLStr('RU');

  Result := True;
end;

function TIClientUtilsMap.GetServerRealTime(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0);

  Result := True;
end;

function TIClientUtilsMap.GetSteamUILanguage(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(7);
  AResponse.WriteLStr('russian');

  Result := True;
end;

function TIClientUtilsMap.InitFilterText(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32;

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);

  Result := True;
end;

function TIClientUtilsMap.IsAPICallCompleted(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  U: UInt64;
begin
  U := ABuffer.ReadUInt64; // SteamAPICall_t

  ABuffer.ReadUInt32; // Fencepost

  TConsole.Write('[IClientUtils::IsAPICallCompleted] SteamAPICall: %d', [U], YELLOW);

  AResponse.WriteUInt8(1); // IsAPICallCompleted result (true if call completed)
  AResponse.WriteUInt8(0); // pbFailed?

  Result := True;
end;

function TIClientUtilsMap.IsSteamChinaLauncher(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // Definitely not china launcher as for me

  Result := True;
end;

function TIClientUtilsMap.RecordSteamInterfaceCreation(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  S, S2: AnsiString;
begin
  S := '???';
  S2 := '???';

  if ABuffer.ReadUInt8 > 0  then
    S := ABuffer.ReadLStr;

  if ABuffer.ReadUInt8 > 0 then
    S2 := ABuffer.ReadLStr;

  //Log('IClientUtils::RecordSteamInterfaceCreation', '%s / %s', [S, S2]);

  ABuffer.ReadUInt32; // Fencepost

  Result := True;
end;

function TIClientUtilsMap.SetAPIDebuggingActive(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt8;
  ABuffer.ReadUInt8;
  ABuffer.ReadUInt32; // Fencepost

  Result := True;
end;

function TIClientUtilsMap.SetAppIDForCurrentPipe(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // AppID
  ABuffer.ReadUInt8;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(1111111111); // CSteamEngine::SetAppIDForCurrentPipe result

  Result := True;
end;

{ TIClientMatchmakingMap }

function TIClientMatchmakingMap.BeginGMSQuery(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  AppID: Cardinal;
  RegionCode: Cardinal;
  FilterText: UTF8String;
begin
  AppID := ABuffer.ReadUInt32; // AppID
  RegionCode := ABuffer.ReadUInt32; // RegionCode

  FilterText := '???';
  if ABuffer.ReadUInt8 > 0 then
    FilterText := ABuffer.ReadLStr; // FilterText

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(FGMSQueryIndex);
  Inc(FGMSQueryIndex);

  TConsole.Write('[IClientMatchmaking::BeginGMSQuery] AppID: %d; RegionCode: %d; FilterText: %s', [AppID, RegionCode, FilterText], DARKMAGENTA);

  Result := True;
end;

constructor TIClientMatchmakingMap.Create;
begin
  inherited;

  FGMSQueryIndex := 1;

  FMasterServer := TMasterServer.Create;
  FMasterServer.LoadFromMaster('csgo.cs-love.club', 27011, True, '\gamedir\csgo\region\255\gametagsnor\valve_ds\gametype\no-steam', '0.0.0.0:0');

  FDispatch.Add(1786, GetFavoriteGameCount);
  FDispatch.Add(1879, BeginGMSQuery);
  FDispatch.Add(1888, ReleaseGMSQuery);
  FDispatch.Add(1894, EnsureFavoriteGameAccountsUpdated);
  FDispatch.Add(1880, PollGMSQuery);
  FDispatch.Add(1881, GetGMSQueryResults);
end;

function TIClientMatchmakingMap.EnsureFavoriteGameAccountsUpdated(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt8; // ?

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(0); // some job index?

  Result := True;
end;

function TIClientMatchmakingMap.GetFavoriteGameCount(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(0);

  Result := True;
end;

function TIClientMatchmakingMap.GetGMSQueryResults(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
type
  TGMSQueryResult = record
    ServerIP: LongWord;
    ServerPort: LongWord;
    AuthPlayers: Longint;
  end;
var
  ResSize: Cardinal;
  ResData: TGMSQueryResult;

  GMSQuery: UInt64;

  A: TArray<string>;
  S: string;

  IP: LongWord;
  Port: Word;
begin
  GMSQuery := ABuffer.ReadUInt64; // ullGMSQuery
  ResSize := ABuffer.ReadUInt32;

  ABuffer.ReadUInt32; // Fencepost

  if ResSize < 12 then
  begin
    TConsole.Error('[IClientMatchmaking::GetGMSQueryResults] GMSQueryResults bad size; Got %d, expected at least 12', [ResSize]);
    Exit(False);
  end;

  AResponse.WriteUInt32(FMasterServer.Servers.Count - 1);

  for S in FMasterServer.Servers do
  begin
    A := S.Split([':']);

    if Length(A) = 1 then
    begin
      IP := inet_addr(PAnsiChar(AnsiString(S)));
      Port := 27015;
    end
    else
    begin
      IP := inet_addr(PAnsiChar(AnsiString(A[0])));
      Port := StrToIntDef(A[1], 27015);
    end;

    ResData.ServerIP := htonl(IP);
    ResData.ServerPort := Port;
    ResData.AuthPlayers := -1;
    AResponse.WriteData(@ResData, SizeOf(ResData));
  end;

  TConsole.Write('[IClientMatchmaking::GetGMSQueryResults] GMSQuery: %d; ResSize: %d', [GMSQuery, ResSize], DARKMAGENTA);

  Result := True;
end;

function TIClientMatchmakingMap.PollGMSQuery(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  GMSQuery: UInt64;
begin
  // This function returns the number of servers, which the GetGMSQueryResults
  // function will return as an array of structures.

  GMSQuery := ABuffer.ReadUInt64; // ullGMSQuery

  ABuffer.ReadUInt32; // Fencepost

  TConsole.Write('[IClientMatchmaking::PollGMSQuery] GMSQuery: %d', [GMSQuery], DARKMAGENTA);

  AResponse.WriteUInt32(FMasterServer.Servers.Count);

  Result := True;
end;

function TIClientMatchmakingMap.ReleaseGMSQuery(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  GMSQuery: UInt64;
begin
  GMSQuery := ABuffer.ReadUInt64; // ullGMSQuery

  TConsole.Write('[IClientMatchmaking::ReleaseGMSQuery] GMSQuery: %d', [GMSQuery], DARKMAGENTA);

  ABuffer.ReadUInt32; // Fencepost

  Result := True;
end;

{ TIClientAppsMap }

constructor TIClientAppsMap.Create;
begin
  inherited;

  FDispatch.Add(1986, GetAppData);
end;

function TIClientAppsMap.GetAppData(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  S: AnsiString;
begin
  ABuffer.ReadUInt32;

  S := '???';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  //ABuffer.Seek(Len, skCur);
  TConsole.Write('[IClientApps::GetAppData] %s', [S]);

  if ABuffer.Capacity - ABuffer.Size < 4 then
  begin
    if IsDebuggerPresent then
      asm int 3 end; // what?
  end
  else
  begin
    ABuffer.ReadUInt32; // numElements for CUtlMemoryBase cstructor?
  end;

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0); // Next data size
  //AResponse.WriteData(nil, 0);

  Result := True;
end;

{ TIClientUserStatsMap }

constructor TIClientUserStatsMap.Create;
begin
  inherited;

  FDispatch.Add(2480, RequestCurrentStats);
end;

function TIClientUserStatsMap.RequestCurrentStats(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt64; // CSteamID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // IClientUserStats::RequestCurrentStats result

  Result := True;
end;

{ TIClientNetworkingDispatchMsg }

function TIClientNetworkingMap.AllowP2PPacketRelay(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt8; // bAllow

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // maybe 1?

  Result := True;
end;

constructor TIClientNetworkingMap.Create;
begin
  inherited;

  FDispatch.Add(2753, AllowP2PPacketRelay);
end;

{ TIClientRemoteStorageMap }

constructor TIClientRemoteStorageMap.Create;
begin
  inherited;

  FDispatch.Add(2867, FileExists);
  FDispatch.Add(2880, GetQuota);
  FDispatch.Add(3034, EnumerateUserSubscribedFiles);
end;

function TIClientRemoteStorageMap.EnumerateUserSubscribedFiles(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt8;
  ABuffer.ReadUInt32; // EPublishedFileInfoMatchingFileType
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(0);

  Result := True;
end;

function TIClientRemoteStorageMap.FileExists(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  L: Cardinal;
  S: AnsiString;
begin
  ABuffer.ReadUInt32;
  L := ABuffer.ReadUInt32; // ERemoteStorageFileRoot

  S := '???';
  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  TConsole.Write('[IClientRemoteStorage::FileExists] %d %s', [L, S]);

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // File always do not exists; TODO: Add real FileExists check

  Result := True;
end;

function TIClientRemoteStorageMap.GetQuota(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // AppID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);
  AResponse.WriteUInt64(1);
  AResponse.WriteUInt64(1);

  Result := True;
end;

{ TIClientAppManagerMap }

constructor TIClientAppManagerMap.Create;
begin
  inherited;

  FDispatch.Add(2178, GetActiveBeta);
  FDispatch.Add(2238, GetAppStateInfo);
  FDispatch.Add(2300, GetCurrentLanguage);
end;

function TIClientAppManagerMap.GetActiveBeta(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // nAppID
  ABuffer.ReadUInt32; // nMaxDest

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(7);
  AResponse.WriteLStr('public');

  Result := True;
end;

function TIClientAppManagerMap.GetAppStateInfo(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // AppID
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1); // GetAppStateInfo result
  AResponse.WriteData(@AppStateInfo, SizeOf(AppStateInfo));

  Result := True;
end;

function TIClientAppManagerMap.GetCurrentLanguage(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);
  AResponse.WriteUInt32(8);
  AResponse.WriteLStr('russian');

  Result := True;
end;

{ TIClientGameCoordinatorMap }

constructor TIClientGameCoordinatorMap.Create;
begin
  inherited;

  FGCMessages := TList<TGCInternalMessage>.Create;

  FDispatch.Add(3293, SendMessage);
  FDispatch.Add(3300, IsMessageAvailable);
  FDispatch.Add(3306, RetrieveMessage);
end;

constructor TIClientGameCoordinatorMap.Create(AParent: TSteamPipeServer);
begin
  inherited Create;
  Create;

  FParent := AParent;
end;

function TIClientGameCoordinatorMap.DispatchGCMessage(AMsg: TMsgType;
  const AData: IBufferDescription): Integer;
var
  Msg: TGCInternalMessage;
begin
  Msg.Header.Msg := AMsg;
  Msg.Header.SrcGCDirIndex := 0;
  Msg.Header.SteamID := SteamID.Value;

  SetLength(Msg.Data, AData.Size);
  Move(AData.Data^, Msg.Data[0], AData.Size);
  FGCMessages.Add(Msg);

  Result := SizeOf(Msg.Header) + Length(Msg.Data);
end;

procedure TIClientGameCoordinatorMap.GC_SendWelcomeToClient;
var
  Buf: IWriteBuffer;
  Data: LongWord;
begin
  Buf := TReadAndWriteBuffer.CreateWrite;
  Buf.WriteUInt8(1);

  Data := DispatchGCMessage(4004, Buf);
  FParent.DispatchCallback(SteamID.ID, 1701, @Data, SizeOf(Data));
end;

function TIClientGameCoordinatorMap.IsMessageAvailable(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // AppID

  ABuffer.ReadUInt32; // Fencepost

  if FGCMessages.Count > 0 then
  begin
    AResponse.WriteUInt8(1);
    AResponse.WriteUInt32(Length(FGCMessages[0].Data));
  end
  else
  begin
    AResponse.WriteUInt8(0);
    AResponse.WriteUInt32(0);
  end;

  Result := True;
end;

function TIClientGameCoordinatorMap.RetrieveMessage(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // fills the provided buffer with the first message in the queue and returns k_EGCResultOK or
  // returns k_EGCResultNoMessage if there is no message waiting. pcubMsgSize is filled with the message size.
  // If the provided buffer is not large enough to fit the entire message, k_EGCResultBufferTooSmall is returned
  // and the message remains at the head of the queue.

  ABuffer.ReadUInt32; // AppID
  ABuffer.ReadUInt32; // cubDest

  ABuffer.ReadUInt32; // Fencepost

  if FGCMessages.Count > 0 then
  begin
    AResponse.WriteUInt32(Ord(k_EGCResultOK));

    with FGCMessages[0] do
    begin
      AResponse.WriteUInt32(Int32(k_EMsgProtoBufFlag) or Header.Msg);
      AResponse.WriteUInt32(SizeOf(Header) + Length(Data));

      AResponse.WriteUInt32(Int32(k_EMsgProtoBufFlag) or Header.Msg);
      AResponse.WriteUInt32(0);
      AResponse.WriteUInt64(Header.SteamID);

      if Length(Data) > 0 then
        AResponse.WriteData(@Data[0], Length(Data));
    end;

    FGCMessages.Delete(0);
  end
  else
  begin
    AResponse.WriteUInt32(Ord(k_EGCResultNoMessage));
    AResponse.WriteUInt32(0);
    AResponse.WriteUInt32(0);
  end;

  Result := True;
end;

function TIClientGameCoordinatorMap.SendMessage(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  MsgType: Cardinal;
  Size: Cardinal;
  Data: Pointer;

  Response: HTTPRequestCompleted_t;
begin
(*
  Okay... the very first message we get is $80000FA6.
  How to read it?
  Open Steamworks states that if the $80000000 bit is set then this message
  is a protobuf message, and this is not very good, because there was a time
  when [s]I had time[/s] this protobuf was not there, and processing such
  messages are easily maintained. The new protobuf format introduces its own complications.
  In general, the quote:

  /**
   * Valve moved a lot of messages to the protobuf format.
   * This means that the structs below should no longer be trusted to be correct.
   * A protobuf message can be detected with:
   *  (uMsgType & 0x80000000) == 0x80000000
   */

  Let's analyze the message $80000FA6. It has a protobuf bit, so it needs to be
  processed. We discard it and get a "clean" message type - $FA6. We convert
  it to the decimal system - we get 4006. A little clever googling and we find
  out the name of the message - k_EMsgGCClientHello. It is sent by the
  GCSDK::CGCClient::SendHello function inside the client.dll library. In this
  case, the sender waits for a response from the server in order to know for
  sure that the coordinator is in order. If it is not given, then every 10
  (or 7?) seconds the client will send a hello message in anticipation of a
  response.

  There are two options for implementing the answer:
    1. By callbacks
    2. By intercepting methods of the ISteamGameCoordinator interface

  The first one is correct and canonical, the original steam client works
  through it. The problem is that it is not yet clear how callbacks work
  and how to call them. In theory, you can call the desired callback through
  CSteamEngine::PostInternalCallback. In practice, the callback is not
  even registered. In general, so far the question of implementation through the
  first method remains open and it is possible that it will be implemented
  in the future.

  The second one is dirty and not entirely correct. Basically, we implement our
  own methods in ISteamGameCoordinator::IsMessageAvailable and
  ISteamGameCoordinator::RetrieveMessage, which will work with their own packet
  queue, and give them on request from outside. It is not quite correct because
  we have to resort to interceptions and patches, and this by default implies
  gross interference with the program, which the emulator tries to avoid.
  However, until the correct way to work through the first method is found,
  we will work through the second.

  UPD 07/15/2020: The second method did not work. Moreover, the interface turned
  out to be corrupted - the first argument of each method is always some kind of
  AppID, the rest are in place. Only now there is no information about this
  anywhere, neither on the partner site, nor in the open steam api. I managed
  to figure out the callbacks and do it through them, because in order for the
  interface methods to start being called, you need to call the callback.

  ------------

  When the game starts rendering (where the feed is shown, character model, etc.),
  a message like 9205 is sent. When the "Play CS:GO" window is opened, 9194 is sent
  every 10 seconds. When connecting to any server, two messages are sent - 9102
  (immediately after processing the connect command) and 9164 (when the loading bar
  appears). Most likely, the lack of a handler for them is the reason that the server
  cannot be entered, as well as the "could not connect to Steam" errors.

  All 9XXX messages are ECsgoGCMsg. Its description can be found in the
  cstrike15_gcmessages.pb.h file. The structure of each package can be
  found in the xxx.proto files.

  9102 - k_EMsgGCCStrike15_v2_MatchmakingStop
  9164 - k_EMsgGCCStrike15_v2_ClientRequestJoinServerData
  9194 - k_EMsgGCCStrike15_v2_ClientGCRankUpdate
  9205 - k_EMsgGCCStrike15_v2_ClientReportValidation
*)

  ABuffer.ReadUInt32; // unAppID
  MsgType := ABuffer.ReadUInt32; // unMsgType
  Size := ABuffer.ReadUInt32; // cubData

  Data := GetMemory(Size);
  if Data = nil then
  begin
    TConsole.Error('[IClientGameCoordinator::SendMessage] Could not allocate %d bytes.', [Size]);
    Exit(False);
  end;

  ABuffer.ReadData(Data, Size);

  ABuffer.ReadUInt32; // Fencepost

  if MsgType and k_EMsgProtoBufFlag = k_EMsgProtoBufFlag then
  begin
    MsgType := MsgType and not k_EMsgProtoBufFlag;

    TConsole.Write('[IClientGameCoordinator::SendMessage] Msg: %d; Size: %d', [MsgType, Size], MAGENTA);

    if MsgType = 9205 then
    begin
      Response.Request := 2;
      Response.ContextValue := 0;
      Response.RequestSuccessful := True;
      Response.StatusCode := 200;
      Response.BodySize := 0;
      FParent.DispatchCallback(SteamID.Value, 2101, @Response, SizeOf(Response));
    end;

    if MsgType = 4006 then
      GC_SendWelcomeToClient;
  end
  else
  begin
    TConsole.Error('[IClientGameCoordinator::SendMessage] Received non-protobuf GC message %.08X.', [MsgType]);
  end;

  AResponse.WriteUInt32(Ord(k_EGCResultOK));

  Result := True;
end;

{ TIClientGameStatsMap }

constructor TIClientGameStatsMap.Create;
begin
  inherited;

  FDispatch.Add(3325, GetNewSession);
end;

function TIClientGameStatsMap.GetNewSession(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt8; // nAccountType
  ABuffer.ReadUInt64; // ulAccountID
  ABuffer.ReadUInt32; // nAppID
  ABuffer.ReadUInt32; // rtTimeStarted

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(1);

  Result := True;
end;

{ TIClientConfigStoreMap }

constructor TIClientConfigStoreMap.Create;
begin
  inherited;

  FDispatch.Add(3104, GetInt);
  FDispatch.Add(3107, GetString);
end;

function TIClientConfigStoreMap.GetInt(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  S: AnsiString;
begin
  ABuffer.ReadUInt32; // EConfigStore

  S := '???';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  TConsole.Write('[ClientConfigStore::GetInt] %s', [S]);

  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0);

  Result := True;
end;

function TIClientConfigStoreMap.GetString(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  S, S2: AnsiString;
begin
  ABuffer.ReadUInt32; // EConfigStore

  S := '???';
  S2 := '???';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  if ABuffer.ReadUInt8 > 0 then
    S2 := ABuffer.ReadLStr;

  TConsole.Write('[ClientConfigStore::GetString] S: %s; S2: %s', [S, S2]);

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);
  AResponse.WriteUInt8(0);

  Result := True;
end;

{ TIClientHTTPMap }

constructor TIClientHTTPMap.Create;
begin
  inherited;

  FHTTPRequestIndex := 1;
  FSteamAPICallIndex := 1;

  FDispatch.Add(3407, ReleaseHTTPRequest);
  FDispatch.Add(3351, CreateHTTPRequest);
  FDispatch.Add(3354, SetHTTPRequestHeaderValue);
  FDispatch.Add(3356, SendHTTPRequest);
end;

function TIClientHTTPMap.CreateHTTPRequest(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  L: Cardinal;
  S: AnsiString;
begin
  L := ABuffer.ReadUInt32; // EHTTPMethod

  S := '';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr; // AbsoluteURL

  ABuffer.ReadUInt32; // Fencepost

  TConsole.Write('[IClientHTTPDispatchMsg::CreateHTTPRequest] Method: %d; URL: %s', [L, S]);

  AResponse.WriteUInt32(FHTTPRequestIndex); // ISteamHTTP::CreateHTTPRequest result (HTTPRequestHandle) (INVALID_HTTPREQUEST_HANDLE = 0)
  Inc(FHTTPRequestIndex);

  Result := True;
end;

function TIClientHTTPMap.ReleaseHTTPRequest(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // HTTPRequestHandle
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1);

  Result := True;
end;

function TIClientHTTPMap.SendHTTPRequest(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
begin
  // Sends the HTTP request, will return false on a bad handle, otherwise use SteamCallHandle to wait on
  // asynchronous response via callback.
  //
  // Note: If the user is in offline mode in Steam, then this will add a only-if-cached cache-control
  // header and only do a local cache lookup rather than sending any actual remote request.

  ABuffer.ReadUInt32; // HTTPRequestHandle?
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1); // IClientHTTP::SendHTTPRequest result
  AResponse.WriteUInt64(FSteamAPICallIndex); // SteamAPICall_t
  Inc(FSteamAPICallIndex);

  Result := True;
end;

function TIClientHTTPMap.SetHTTPRequestHeaderValue(const ABuffer: IReadBuffer;
  const AResponse: IWriteBuffer): Boolean;
var
  L: Cardinal;
  S, S2: AnsiString;
begin
  L := ABuffer.ReadUInt32; // HTTP query handle?

  S := '';
  S2 := '';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  if ABuffer.ReadUInt8 > 0 then
    S2 := ABuffer.ReadLStr;

  TConsole.Write('[IClientHTTPDispatchMsg::SetHTTPRequestHeaderValue] Request: %d; Header: %s; Value: %s', [L, S, S2]);

  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteInt8(1);

  Result := True;
end;

{ TIClientControllerSerializedMap }

constructor TIClientControllerSerializedMap.Create;
begin
  inherited;

  FDispatch.Add(5037, GetActionSetHandle);
  FDispatch.Add(5230, HasGameMapping);
end;

function TIClientControllerSerializedMap.GetActionSetHandle(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
var
  S: AnsiString;
begin
  ABuffer.ReadUInt32;

  S := '';

  if ABuffer.ReadUInt8 > 0 then
    S := ABuffer.ReadLStr;

  ABuffer.ReadUInt32;

  AResponse.WriteUInt64(0);

  Result := True;
end;

function TIClientControllerSerializedMap.HasGameMapping(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(1); // Skip the 'Timed out waiting for game mapping!' message sleeps

  Result := True;
end;

{ TIClientShaderDispatchMsg }

constructor TIClientShaderMap.Create;
begin
  inherited;

  FDispatch.Add(5300, ProcessShaderCache);
  FDispatch.Add(5307, SetupShaderCacheEnvironment);
end;

function TIClientShaderMap.ProcessShaderCache(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32;
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0);

  Result := True;
end;

function TIClientShaderMap.SetupShaderCacheEnvironment(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // CUtlMemory size?
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt8(0); // Next data size? (should be 1)
  //AResponse.WriteData(nil, 0);

  Result := True;
end;

{ TIClientNetworkingSocketsSerializedMap }

constructor TIClientNetworkingSocketsSerializedMap.Create;
begin
  inherited;

  FDispatch.Add(5352, GetCachedRelayTicketCount);
  FDispatch.Add(5346, GetCertAsync);
end;

function TIClientNetworkingSocketsSerializedMap.GetCachedRelayTicketCount(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(0);

  Result := True;
end;

function TIClientNetworkingSocketsSerializedMap.GetCertAsync(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt64(0);

  Result := True;
end;

{ TIClientNetworkingUtilsSerializedMap }

constructor TIClientNetworkingUtilsSerializedMap.Create;
begin
  inherited;

  FDispatch.Add(5388, GetLauncherType);
end;

function TIClientNetworkingUtilsSerializedMap.GetLauncherType(
  const ABuffer: IReadBuffer; const AResponse: IWriteBuffer): Boolean;
begin
  ABuffer.ReadUInt32; // Fencepost

  AResponse.WriteUInt32(Ord(k_ELauncherTypeDefault));

  Result := True;
end;

initialization
  SteamID := $110000100000000 + $00C0FFEE;

  FillChar(AppStateInfo, SizeOf(AppStateInfo), 0);

  AppStateInfo.dword0 := 4;
  AppStateInfo.Flags := $00000803; // k_EAppOwnershipFlagsOwnsLicense or k_EAppOwnershipFlagsFreeLicense or k_EAppOwnershipFlagsRecurring
  AppStateInfo.dword8 := $2004;
  AppStateInfo.SteamID := SteamID.Value;
  AppStateInfo.dword14 := 0;
  AppStateInfo.dword18 := $5F07911B;
  AppStateInfo.GameVersion := $00835619;
  AppStateInfo.dword20 := 0;
  AppStateInfo.dword24 := 0;
end.
