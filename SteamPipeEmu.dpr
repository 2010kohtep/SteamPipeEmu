library SteamPipeEmu;

{$I Default.inc}

uses
  Winapi.Windows,
  Winapi.WinSock,
  Winapi.TlHelp32,
  System.SysUtils,
  System.Math,

  Steam.Callback in 'Source\Public\Steam.Callback.pas',
  Steam.API in 'Source\Public\Steam.API.pas',

  Emu.SteamPipe in 'Source\Emu\Emu.SteamPipe.pas',
  Emu.GCDetour in 'Source\Emu\Emu.GCDetour.pas',

  Xander.Memoria in 'Source\Xander\Xander.Memoria.pas',
  Xander.ThisWrap in 'Source\Xander\Xander.ThisWrap.pas',
  Xander.DllCallback in 'Source\Xander\Xander.DllCallback.pas',
  Xander.Exception in 'Source\Xander\Xander.Exception.pas',
  Xander.LdrMonitor in 'Source\Xander\Xander.LdrMonitor.pas',
  Xander.Helpers in 'Source\Xander\Xander.Helpers.pas',
  Xander.Console in 'Source\Xander\Xander.Console.pas',
  Xander.Buffer in 'Source\Xander\Xander.Buffer.pas',
  Xander.MasterServer in 'Source\Xander\Xander.MasterServer.pas',
  Xander.MiniUDP in 'Source\Xander\Xander.MiniUDP.pas';

{$R *.res}

// (char const *, int, enum  EVProfBugdetGroup, bool, int, void *)
procedure F(_EAX, _EDX: Integer; This: Pointer; P: Pointer; I: Integer; B: Boolean; E: Integer; I2: Integer; S: PAnsiChar); register;
const
  BannedLogList: array of PAnsiChar =
  [
    'IClientUser::BLoggedOn',
    'CSteamMatchMaking::RunFrame',
    'CServerList::RunFrame',
    'CServerSocket::RunFrame',
    'IClientHTTP::CreateHTTPRequest',
    'IClientFriends::GetMediumFriendAvatar',
    'IClientUtils::GetImageSize',
    'IClientFriends::GetSmallFriendAvatar',
    'IClientUtils::GetImageSize',

    'CUtlString::SetValue',
    'IClientFriends::SetRichPresence',

    'IClientUser::GetSteamID'
  ];

  BannedSubStrsList: array of PAnsiChar =
  [
    'UDP',
    'HTTP',
    'CJob',
    'FreeDeletedPackets',
    'CNet::',
    'CWorkThreadPool::',
    'CTCPConnection::',
    'CScheduledFunctionMgr::',
    'GetNextOutgoingPacket',
    'CIPCServer::',
    'RunFrame'
  ];
var
  Str: PAnsiChar;
begin
  if S = nil then
    Exit;

//  if StrPos(S, 'BeginAuthSession') <> nil then
//  begin
//    MessageBox(HWND_DESKTOP, 'BeginAuthSession', '', MB_SYSTEMMODAL);
//    asm int 3 end;
//  end;
//
  for Str in BannedLogList do
  begin
    if StrComp(S, Str) = 0  then
      Exit;
  end;
//
//  for Str in BannedSubStrsList do
//  begin
//    if StrPos(S, Str) <> nil then
//      Exit;
//  end;

  TConsole.Important('[EnterScope] %s', [S]);
end;

procedure EnableDebugOutput;
var
  P: PLongint;
begin
  with TModule.CreateModule('tier0_s.dll') do
  begin
    HookExport('?EnterScope@CVProfile@@QAE_NPBDHW4EVProfBugdetGroup@@_NHPAX@Z', @F);

    CreatePattern(P).GetProcedure('g_VProfProfilesRunningCount');
    if P <> nil then
    begin
      P^ := 1;
      TConsole.Success('VProf enabled');
    end
    else
    begin
      TConsole.Error('VProf failed');
    end;
  end;
end;

(*
void(*Msg)(const char *pMsg, ...);
void(*Warning)(const char *pMsg, ...);
void(*Warning_SpewCallStack)(int iMaxCallStackLength, const char *pMsg, ...);

void(*DevMsg)(int level, const char *pMsg, ...);
void(*DevWarning)(int level, const char *pMsg, ...);

void(*ConColorMsg)(const Color &clr, const char *pMsg, ...);
void(*ConMsg)(const char *pMsg, ...);
*)

//procedure Msg(Fmt: PAnsiChar); cdecl;
//const
//  TriggerString: PAnsiChar = 'Server using ''%s'' lobbies, requiring pw %s, lobby id %llx'#$A;
//asm
//  mov eax, [Fmt]
//  mov edx, [TriggerString]
//  call StrComp
//
//  test al, al
//  jnz @A
//    // Clear 'dc.m_chLobbyType' string
//    mov byte ptr [edi + 1009], 0
//
//    // clear 'char context[ 256 ]' string
//    lea eax, [esp + 156]
//    mov byte ptr [eax], 0
//@A:
//end;

//function sprintf(Buffer: PAnsiChar; Fmt: PAnsiChar): Integer; cdecl varargs; external 'msvcrt' name 'sprintf';
function vsprintf(Buffer: PAnsiChar; Fmt: PAnsiChar; Args: Pointer): Integer; cdecl varargs; external 'msvcrt' name 'vsprintf';

procedure Msg(Fmt: PAnsiChar); cdecl;
var
  Buffer: array[0..2047] of AnsiChar;
  VAList: Pointer;
begin
  TConsole.Write('[Msg] %s', [Fmt], CYAN, [pfNoLineBreak]);

  if StrComp(Fmt, 'Server is running a newer version, client version %d, server version %d'#10) = 0 then
  begin
    VAList := @PPointer(@Fmt)[1];
    vsprintf(Buffer, Fmt, VAList);

    MessageBoxA(HWND_DESKTOP, Buffer, '', MB_ICONWARNING or MB_SYSTEMMODAL);
    Exit;
  end;

  if StrComp(Fmt, 'Server is running an older version, client version %d, server version %d'#10) = 0 then
  begin
    VAList := @PPointer(@Fmt)[1];
    vsprintf(Buffer, Fmt, VAList);

    MessageBoxA(HWND_DESKTOP, Buffer, '', MB_ICONWARNING or MB_SYSTEMMODAL);
    Exit;
  end;
end;

procedure Warning(Fmt: PAnsiChar); cdecl;
begin
  TConsole.Write('[Warning] %s', [Fmt], CYAN, [pfNoLineBreak]);
end;

procedure DevMsg(Level: Integer; Fmt: PAnsiChar); cdecl;
begin
  TConsole.Write('[DevMsg] %s', [Fmt], CYAN, [pfNoLineBreak]);
end;

procedure DevWarning(Level: Integer; Fmt: PAnsiChar); cdecl;
begin
  TConsole.Write('[DevWarning] %s', [Fmt], CYAN, [pfNoLineBreak]);
end;

procedure ConMsg(Fmt: PAnsiChar); cdecl;
begin
  TConsole.Write('[ConMsg] %s', [Fmt], CYAN, [pfNoLineBreak]);
end;

procedure DetourTier0Msgs;
begin
{$IFDEF DEBUG}
  with TModule.CreateModule('tier0.dll') do
  begin
    HookExport('Msg', @Msg);
    //HookExport('Warning', @Warning);
    //HookExport('DevMsg', @DevMsg);
    //HookExport('DevWarning', @DevWarning);
    //HookExport('ConMsg', @ConMsg);
  end;
{$ENDIF}
end;

type
  // results from BeginAuthSession
  EBeginAuthSessionResult =
  (
    k_EBeginAuthSessionResultOK = 0,						        // Ticket is valid for this game and this steamID.
    k_EBeginAuthSessionResultInvalidTicket = 1,				  // Ticket is not valid.
    k_EBeginAuthSessionResultDuplicateRequest = 2,			// A ticket has already been submitted for this steamID
    k_EBeginAuthSessionResultInvalidVersion = 3,			  // Ticket is from an incompatible interface version
    k_EBeginAuthSessionResultGameMismatch = 4,				  // Ticket is not for this game
    k_EBeginAuthSessionResultExpiredTicket = 5				  // Ticket has expired
  );

function CGameServer_BeginAuthSession(Data: Pointer; Size: Integer; SteamID: TSteamID): Integer; stdcall;
begin
  Result := Ord(k_EBeginAuthSessionResultOK);
end;

procedure DetourCGameServer;
var
  P: Pointer;
begin
  with TModule.CreateModule('steamclient.dll') do
  begin
    with CreatePattern(P) do
    begin
      FindPattern([$55, $8B, $EC, $83, $EC, $54, $53, $81, $65, $F0, $FF, $FF, $0F, $FF]);
      FindReference(P);
    end;
  end;

  if P <> nil then
    TMemory.WritePrimitive<Pointer>(P, @CGameServer_BeginAuthSession);
end;

procedure RemoveServerQueryLimit;
var
  P: Pointer;
begin
  with TModule.CreateModule('steamclient.dll') do
  begin
    with CreatePattern(P) do
    begin
      FindPattern([$D9, $86, $90, $02, $00, $00]);
      FindUInt8($77);
    end;

    if P <> nil then
      TMemory.Fill(P, $90, 2)
    {$IFDEF DEBUG}
    else
      TConsole.Error('RemoveServerQueryLimit: Failed to perform patch #1.')
    {$ENDIF}
    ;

    with CreatePattern(P) do
    begin
      FindPattern([$0F, $2F, $86, $AC, $02, $00, $00]);
      FindUInt8($76);
    end;

    if P <> nil then
      TMemory.WritePrimitive<Byte>(P, $EB)
    {$IFDEF DEBUG}
    else
      TConsole.Error('RemoveServerQueryLimit: Failed to perform patch #2.');
    {$ENDIF}
    ;
  end;
end;

var
  orgrecvfrom: function(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;

type
  TServerInfo = record
    GSSecure: Boolean;
    Password: Boolean;
    Friends: Boolean;
    IsValve: Boolean;
    Encrypted: Boolean;

    LobbyType: AnsiString;
    Context: AnsiString;

    Challenge: Integer;
    AuthProtocol: Integer;
    GameProtocol: Integer;
    SteamKeySize: Word;

    GSSteamID: Int64;
    LobbyID: Int64;
  end;

function hkrecvfrom(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
var
  MsgIn: IReadBuffer;
  MsgOut: IWriteBuffer;

  Info: TServerInfo;
begin
  Result := orgrecvfrom(s, Buf, len, flags, from, fromlen);

  if Result <> SOCKET_ERROR then
  begin
    MsgIn := TReadAndWriteBuffer.CreateRead(@Buf, Result);
    MsgOut := TReadAndWriteBuffer.CreateWrite;

    if MsgIn.ReadInt32 <> -1 then
      Exit;

    if AnsiChar(MsgIn.ReadInt8) <> 'A' then // S2C_CHALLENGE
      Exit;

    FillChar(Info, SizeOf(Info), 0);

    Info.Challenge := MsgIn.ReadInt32;
    Info.AuthProtocol := MsgIn.ReadInt32;
    Info.SteamKeySize := MsgIn.ReadUInt16;
    Info.GSSteamID := MsgIn.ReadInt64;
    Info.GSSecure := MsgIn.ReadInt8 <> 0;
    Info.Context := MsgIn.ReadLStr;
    Info.GameProtocol := MsgIn.ReadInt32;
    Info.LobbyType := MsgIn.ReadLStr;
    Info.Password := MsgIn.ReadInt8 <> 0;
    Info.LobbyID := MsgIn.ReadInt64;
    Info.Friends := MsgIn.ReadInt8 <> 0;
    Info.IsValve := MsgIn.ReadInt8 <> 0;
    Info.Encrypted := MsgIn.ReadInt8 <> 0;

    if (Info.AuthProtocol <> 3) or (Info.SteamKeySize <> 0) then
    begin
    {$IFDEF DEBUG}
      TConsole.Error('[RecvFrom] Only Steam connections with empty Steam key allowed, but got protocol %d with key size %d.',
        [Info.AuthProtocol, Info.SteamKeySize]);
    {$ENDIF}

      Exit;
    end;

    if not MsgIn.Eof then
    begin
    {$IFDEF DEBUG}
      TConsole.Error('[RecvFrom] Message should be finished, but got some data instead.');
    {$ENDIF}

      Exit;
    end;

//    if Info.LobbyType <> '' then
//    begin
//    {$IFDEF DEBUG}
//      TConsole.Important('[RecvFrom] Received bad LobbyType (%s), spoofing...', [Info.LobbyType]);
//    {$ENDIF}
//
//      Info.LobbyType := '';
//    end;

// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU
// CSGO_GAME_UI_STATE_MAINMENU


//    if Info.LobbyID <> 0 then
//    begin
//    {$IFDEF DEBUG}
//      TConsole.Important('[WinSock] Received bad LobbyID (%d), spoofing...', [Info.LobbyID]);
//    {$ENDIF}
//
//      Info.LobbyID := 0;
//    end;

    Info.Context := 'connect-granted';

    MsgOut.WriteInt32(-1);
    MsgOut.WriteInt8(Ord('A'));
    MsgOut.WriteUInt32(Info.Challenge);
    MsgOut.WriteUInt32(Info.AuthProtocol);
    MsgOut.WriteUInt16(Info.SteamKeySize);
    MsgOut.WriteUInt64(Info.GSSteamID);
    MsgOut.WriteUInt8(Ord(Info.GSSecure));
    MsgOut.WriteLStr(Info.Context);
    MsgOut.WriteUInt32(Info.GameProtocol);
    MsgOut.WriteLStr(Info.LobbyType);
    MsgOut.WriteUInt8(Ord(Info.Password));
    MsgOut.WriteInt64(Info.LobbyID);
    MsgOut.WriteUInt8(Ord(Info.Friends));
    MsgOut.WriteUInt8(Ord(Info.IsValve));
    MsgOut.WriteUInt8(Ord(Info.Encrypted));

    Move(MsgOut.Data^, Buf, MsgOut.Size);
    Result := MsgOut.Size;
  end;
end;

var
  orgSteamAPI_RegisterCallback: procedure(Callback: Pointer; CallbackIndex: Integer); cdecl;
  orgSteamAPI_RegisterCallResult: procedure(Callback: Pointer; APICall: UInt64); cdecl;
  orgSteam_BGetCallback: function(SteamPipe: HSteamPipe; CallbackMsg: PCallbackMsg): Boolean; cdecl;

procedure SteamAPI_RegisterCallback(Callback: Pointer; CallbackIndex: Integer); cdecl;
begin
  TConsole.Write('[SteamAPI_RegisterCallback] From: %s; Callback: %.08X; CallbackIndex: %d',
    [TMemory.BeautifyPointer(ReturnAddress), Integer(Callback), CallbackIndex], DARKBLUE);

  orgSteamAPI_RegisterCallback(Callback, CallbackIndex);
end;

procedure SteamAPI_RegisterCallResult(Callback: Pointer; APICall: UInt64); cdecl;
begin
  TConsole.Write('[SteamAPI_RegisterCallResult] From: %s; Callback: %.08X; APICall: %d',
    [TMemory.BeautifyPointer(ReturnAddress), Integer(Callback), APICall], DARKBLUE);

  orgSteamAPI_RegisterCallResult(Callback, APICall);
end;

function Steam_BGetCallback(SteamPipe: HSteamPipe; CallbackMsg: PCallbackMsg): Boolean; cdecl;
begin
  Result := orgSteam_BGetCallback(SteamPipe, CallbackMsg);
end;

procedure DetourSteamAPICallbacks;
begin
  with TModule.CreateModule('steam_api.dll') do
  begin
    @orgSteamAPI_RegisterCallback := HookExport('SteamAPI_RegisterCallback', @SteamAPI_RegisterCallback);
    @orgSteamAPI_RegisterCallResult := HookExport('SteamAPI_RegisterCallResult', @SteamAPI_RegisterCallResult);
  end;
end;

procedure DetourChallengeMessages;
begin
  @orgrecvfrom := TModule.CreateModule('wsock32.dll').HookExport('recvfrom', @hkrecvfrom);
end;

procedure DetourSteamClientCallbacks;
begin
  with TModule.CreateModule('steamclient.dll') do
  begin
    @orgSteam_BGetCallback := HookExport('Steam_BGetCallback', @Steam_BGetCallback);
  end;
end;

procedure DllCallback(const Name: string; Base: Pointer); register;
var
  GenName: string;
  Desired: Boolean;
begin
  Desired := False;

  GenName := ExtractFileName(Name);
  GenName := LowerCase(GenName);

  try
    if SameStr('steam_api.dll', GenName) then
    begin
      Desired := True;

      //DetourSteamAPICallbacks;

      Exit;
    end;

    if SameStr('steamclient.dll', GenName) then
    begin
      Desired := True;

      DetourCGameServer;
      DetourSteamClientCallbacks;
      RemoveServerQueryLimit;

      //DetourChallengeMessages;

      DetourTier0Msgs;
      EnableDebugOutput;

      Exit;
    end;
  finally
    if Desired then
      TConsole.Write('[DllCallback] Name: %s; Handle: $%s', [GenName, IntToHex(Integer(Base), 8)], DARKGREEN);
  end;
end;

var
  SteamPipeServer: TSteamPipeServer;

function SteamPipeReadThread(Parameter: Pointer): Integer;
begin
  repeat
    SteamPipeServer.Frame;
  until False;

  Exit(1);
end;

function ProcessExists(const FileName: string; out ProcessId: Integer): Boolean; overload;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;

  UpperName: string;
  UpperProcName: string;
begin
  if PPointer(@ProcessId) <> nil then
    ProcessId := 0;

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if FSnapshotHandle = INVALID_HANDLE_VALUE then
    Exit(False);

  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  try
    if not ContinueLoop then
      Exit(False);

    UpperName := UpperCase(FileName);

    repeat
      UpperProcName := UpperCase(ExtractFileName(FProcessEntry32.szExeFile));
      if Pos(UpperName, UpperProcName) > 0 then
      begin
        if PPointer(@ProcessId) <> nil then
          ProcessId := FProcessEntry32.th32ProcessID;

        Exit(True);
      end;

      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    until not ContinueLoop;
  finally
    CloseHandle(FSnapshotHandle);
  end;

  Exit(False);
end;

procedure Init;
begin
  // TODO: Replace ProcessExists with file mapping check
  if ProcessExists('steam.exe', PInteger(nil)^) then
  begin
    MessageBox(HWND_DESKTOP, 'XanderEmu does not work while Steam is running. Please, terminate Steam and try again.', 'XanderEmu - Error', MB_ICONWARNING or MB_SYSTEMMODAL);
    Halt;
  end;

{$IFDEF DEBUG}
  MessageBox(HWND_DESKTOP, 'Are you ready?', '', MB_SYSTEMMODAL);

  AllocConsole;
  TConsole.Init(True);
{$ENDIF}

  SteamPipeServer := TSteamPipeServer.Create;
  BeginThread(nil, 0, SteamPipeReadThread, nil, 0, PCardinal(nil)^);

  Xander.DllCallback.Setup(DllCallback);
end;

// TODO: Add validation - was the original steamclient.dll loaded
// Current versions of MacOS Steam libraries can be taken from http://osw.didrole.com/src/

begin
  Init;
end.
