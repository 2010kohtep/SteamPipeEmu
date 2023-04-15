(*=========== (C) Copyright 2019, Alexander B. All rights reserved. ===========*)
(*                                                                             *)
(*  Name:                                                                      *)
(*    Steam.API                                                                *)
(*                                                                             *)
(*  Description:                                                               *)
(*=============================================================================*)

unit Steam.API;

{$I Default.inc}

{$Z4}

interface

const
  FavoriteFlagNone      = $00;
  FavoriteFlagFavorite  = $01; // this game favorite entry is for the favorites list
  FavoriteFlagHistory   = $02; // this game favorite entry is for the history list

type
  HServerQuery = Integer;

const
  HSERVERQUERY_INVALID = -1;

type
  HServerListRequest = Pointer;

  TAppId = Cardinal;

  PPMatchMakingKeyValuePair = ^PMatchMakingKeyValuePair;
  PMatchMakingKeyValuePair = ^TMatchMakingKeyValuePair;
  TMatchMakingKeyValuePair = record
    Key, Value: array[0..255] of AnsiChar;
  end;

const
  MaxGameServerGameDir = 32;
  MaxGameServerMapName = 32;
  MaxGameServerGameDescription = 64;
  MaxGameServerName = 64;
  MaxGameServerTags = 128;
  MaxGameServerGameData = 2048;

type
  servernetadr_t = record
    ConnectionPort: Word; // (in HOST byte order)
    QueryPort: Word;
    IP: LongWord;
  end;
  TServerNetAdr = servernetadr_t;

  gameserveritem_t = record
    NetAdr: TServerNetAdr;
    Ping: Integer;
    HadSuccessfullResponse: Boolean;
    DoNotRefresh: Boolean;
    GameDir: array[0..MaxGameServerGameDir - 1] of AnsiChar;
    Map: array[0..MaxGameServerMapName - 1] of AnsiChar;
    GameDescription: array[0..MaxGameServerGameDescription] of AnsiChar;
    AppID: LongWord;
    Players: Integer;
    MaxPlayers: Integer;
    BotPlayers: Integer;
    Password: Boolean;
    Secure: Boolean;
    TimeLastPlayed: LongWord;
    ServerVersion: Integer;
    ServerName: array[0..MaxGameServerName - 1] of AnsiChar;
    GameTags: array[0..MaxGameServerTags - 1] of AnsiChar;
    SteamID: UInt64;
  end;
  TGameServerItem = gameserveritem_t;
  PGameServerItem = ^TGameServerItem;

  PVSteamMatchmakingPingResponse = ^VSteamMatchmakingPingResponse;
  VSteamMatchmakingPingResponse = record
    ServerResponded: procedure(var Server: TGameServerItem); stdcall;
    ServerFailedToRespond: procedure; stdcall;
  end;

  PISteamMatchmakingPingResponse = ^ISteamMatchmakingPingResponse;
  ISteamMatchmakingPingResponse = record
    VTable: PVSteamMatchmakingPingResponse;
  end;

  PVSteamMatchmakingServerListResponse = ^VSteamMatchmakingServerListResponse;
  VSteamMatchmakingServerListResponse = record
    ServerResponded: procedure(Request: HServerListRequest; Server: Integer); stdcall;
    ServerFailedToRespond: procedure(Request: HServerListRequest; Server: Integer); stdcall;
    RefreshComplete: procedure(Request: HServerListRequest; Response: Integer); stdcall;
  end;

  PISteamMatchmakingServerListResponse = ^ISteamMatchmakingServerListResponse;
  ISteamMatchmakingServerListResponse = record
    VTable: PVSteamMatchmakingServerListResponse;
  end;

type
  PISteamMatchmakingServers = ^ISteamMatchmakingServers;
  ISteamMatchmakingServers = record
    (* +00 *) RequestInternetServerList: function(App: TAppId; Filters: PPMatchMakingKeyValuePair; FilterCount: Cardinal; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;
    (* +04 *) RequestLANServerList: function(App: TAppId; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;
    (* +08 *) RequestFriendsServerList: function(App: TAppId; Filters: PPMatchMakingKeyValuePair; FilterCount: Cardinal; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;
    (* +12 *) RequestFavoritesServerList: function(App: TAppId; Filters: PPMatchMakingKeyValuePair; FilterCount: Cardinal; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;
    (* +16 *) RequestHistoryServerList: function(App: TAppId; Filters: PPMatchMakingKeyValuePair; FilterCount: Cardinal; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;
    (* +20 *) RequestSpectatorServerList: function(App: TAppId; Filters: PPMatchMakingKeyValuePair; FilterCount: Cardinal; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; stdcall;

    (* +24 *) ReleaseRequest: procedure(ServerListRequest: HServerListRequest); stdcall;

    (* +28 *) GetServerDetails: function(Request: HServerListRequest; Server: Integer): PGameServerItem; stdcall;

    (* +32 *) CancelQuery: procedure(Request: HServerListRequest); stdcall;

    (* +36 *) RefreshQuery: procedure(Request: HServerListRequest); stdcall;

    (* +40 *) IsRefreshing: procedure(Request: HServerListRequest); stdcall;
    (* +44 *) GetServerCount: procedure(Request: HServerListRequest); stdcall;
    (* +48 *) RefreshServer: procedure(Request: HServerListRequest; Server: Integer); stdcall;

    (* +52 *) PingServer: function(IP: LongWord; Port: Word; RequestServersResponse: PISteamMatchmakingPingResponse): HServerQuery; stdcall;

    (* +56 *) PlayerDetails: function(IP: LongWord; Port: Word; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerQuery; stdcall;

    (* +60 *) ServerRules: function(IP: LongWord; Port: Word; RequestServersResponse: PISteamMatchmakingServerListResponse): HServerQuery; stdcall;

    (* +64 *) CancelServerQuery: procedure(ServerQuery: HServerQuery); stdcall;
  end;

  PSteamMatchmakingServers = ^TSteamMatchmakingServers;
  TSteamMatchmakingServers = record
    VTable: PISteamMatchmakingServers;
    ClientMatchmakingServers: Pointer; // CLIENTMATCHMAKINGSERVERS_INTERFACE_VERSION001 [SteamMatchMakingServers002] (CSteamMatchMaking?)
  end;

type
  HSteamPipe = Longint;
  HSteamUser = Longint;

type
  PISteamClient = ^ISteamClient;
  ISteamClient = record
    (* +000 *) CreateSteamPipe: function: HSteamPipe; stdcall;

    (* +004 *) BReleaseSteamPipe: function(SteamPipe: HSteamPipe): Boolean; stdcall;

    (* +008 *) ConnectToGlobalUser: function(SteamPipe: HSteamPipe): HSteamUser; stdcall;

    (* +012 *) CreateLocalUser: function(var SteamPipe: HSteamPipe; AccountType: Integer): HSteamUser; stdcall;

    (* +016 *) ReleaseUser: procedure(SteamPipe: HSteamPipe; User: HSteamUser); stdcall;

	  (* +020 *) GetISteamUser: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +024 *) GetISteamGameServer: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;

    (* +028 *) SetLocalIPBinding: procedure(IP: LongWord; Port: Word); stdcall;

	  (* +032 *) GetISteamFriends: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +036 *) GetISteamUtils: function(SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +040 *) GetISteamMatchmaking: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +044 *) GetISteamMatchmakingServers: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): PSteamMatchmakingServers; stdcall;
	  (* +048 *) GetISteamGenericInterface: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +052 *) GetISteamUserStats: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +056 *) GetISteamGameServerStats: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +060 *) GetISteamApps: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +064 *) GetISteamNetworking: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +068 *) GetISteamRemoteStorage: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
	  (* +072 *) GetISteamScreenshots: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;

    (* +076 *) RunFrame: procedure; stdcall;

    (* +080 *) GetIPCCallCount: function: LongWord; stdcall;

    (* +084 *) SetWarningMessageHook: procedure(Func: Pointer); stdcall;

    (* +088 *) BShutdownIfAllPipesClosed: function: Boolean; stdcall;

    (* +092 *) GetISteamHTTP: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
    (* +096 *) GetISteamUnifiedMessages: function(SteamUser: HSteamUser; SteamPipe: HSteamPipe; Version: PAnsiChar): Pointer; stdcall;
  end;

type
  PVSteamMatchmaking = ^VSteamMatchmaking;
  VSteamMatchmaking = record
    GetFavoriteGameCount: function: Integer; stdcall;

  	// returns the details of the game server
	  // iGame is of range [0,iGame)
    GetFavoriteGame: function(Game: Integer; out AppID: LongWord; out IP: LongWord; out ConnPort: Word; out QueryPort: Word; out Flags: LongWord; out Time32LastPlayedOnServer: LongWord): Boolean; stdcall;
    // returns the new index of the game
    AddFavoriteGame: function(AppID: LongWord; IP: LongWord; ConnPort: Word; QueryPort: Word; Flags: LongWord; Time32LastPlayedOnServer: LongWord): Integer; stdcall;
	  // removes the game; returns true if one was removed
    RemoveFavoriteGame: function(AppID: LongWord; IP: LongWord; ConnPort: Word; QueryPort: Word; Flags: LongWord): Boolean; stdcall;

    // ...
  end;

  PISteamMatchmaking = ^ISteamMatchmaking;
  ISteamMatchmaking = record
    VTable: PVSteamMatchmaking;
  end;

const
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient012';

type
  PSteamClient = ^TSteamClient;
  TSteamClient = record
    VTable: PISteamClient;
  end;

// steamtypes.h
type
  TSteamAPICall = UInt64;

// steamhttpenums.h

type
  // This enum is used in client API methods, do not re-number existing values.
  EHTTPMethod = (
    EHTTPMethodInvalid = 0,

    EHTTPMethodGET,
    EHTTPMethodHEAD,
    EHTTPMethodPOST,

	  // The remaining HTTP methods are not yet supported, per rfc2616 section 5.1.1 only GET and HEAD are required for
  	// a compliant general purpose server.  We'll likely add more as we find uses for them.

    // EHTTPMethodOPTIONS,
    EHTTPMethodPUT,
    EHTTPMethodDELETE
	  // EHTTPMethodTRACE,
	  // EHTTPMethodCONNECT
  );

// isteamhttp.h

type
  HTTPRequestHandle = LongWord;

const
  INVALID_HTTPREQUEST_HANDLE = 0;

type
  VSteamHTTP = record
    (* +00 *) CreateHTTPRequest: function(HTTPRequestMethod: EHTTPMethod; AbsoluteURL: PAnsiChar): HTTPRequestHandle; stdcall;
    (* +04 *) SetHTTPRequestContextValue: function(Request: HTTPRequestHandle; ContextValue: UInt64): Boolean; stdcall;
    (* +08 *) SetHTTPRequestNetworkActivityTimeout: function(Request: HTTPRequestHandle; TimeoutSeconds: LongWord): Boolean; stdcall;
    (* +12 *) SetHTTPRequestHeaderValue: function(Request: HTTPRequestHandle; HeaderName, HeaderValue: PAnsiChar): Boolean; stdcall;
    (* +16 *) SetHTTPRequestGetOrPostParameter: function(Request: HTTPRequestHandle; ParamName, ParamValue: PAnsiChar): Boolean; stdcall;
    (* +20 *) SendHTTPRequest: function(Request: HTTPRequestHandle; out CallHandle: TSteamAPICall): Boolean; stdcall;
    (* +24 *) SendHTTPRequestAndStreamResponse: function(Request: HTTPRequestHandle; CallHandle: TSteamAPICall): Boolean; stdcall;
    (* +28 *) DeferHTTPRequest: function(Request: HTTPRequestHandle): Boolean; stdcall;
    (* +32 *) PrioritizeHTTPRequest: function(Request: HTTPRequestHandle): Boolean; stdcall;
    (* +36 *) GetHTTPResponseHeaderSize: function(Request: HTTPRequestHandle; HeaderName: PAnsiChar; out ResponseHeaderSize: LongWord): Boolean; stdcall;
    (* +40 *) GetHTTPResponseHeaderValue: function(Request: HTTPRequestHandle; HeaderName: PAnsiCHar; HeaderValueBuffer: PByte; BufferSize: LongWord): Boolean; stdcall;
    (* +44 *) GetHTTPResponseBodySize: function(Request: HTTPRequestHandle; out BodySize: LongWord): Boolean; stdcall;
    (* +48 *) GetHTTPResponseBodyData: function(Request: HTTPRequestHandle; BodyDataBuffer: PByte; BufferSize: LongWord): Boolean; stdcall;
    (* +52 *) GetHTTPStreamingResponseBodyData: function(Request: HTTPRequestHandle; Offset: Longint; BodyDataBuffer: PByte; BufferSize: LongWord): Boolean; stdcall;
    (* +56 *) ReleaseHTTPRequest: function(Request: HTTPRequestHandle): Boolean; stdcall;
    (* +60 *) GetHTTPDownloadProgressPct: function(Request: HTTPRequestHandle; out Percent: Single): Boolean; stdcall;
    (* +64 *) SetHTTPRequestRawPostBody: function(Request: HTTPRequestHandle; ContentType: PAnsiChar; Body: PByte; BodyLen: LongWord): Boolean; stdcall;
  end;

  PISteamHTTP = ^ISteamHTTP;
  ISteamHTTP = record
    VTable: ^VSteamHTTP;
  end;

const
  STEAMHTTP_INTERFACE_VERSION = 'STEAMHTTP_INTERFACE_VERSION002';

type
  THTTPRequestCompleted = record
    Request: HTTPRequestHandle;
    Dummy04: Longint;

    ContextValue: UInt64;

    RequestSuccessful: Boolean;
    Dummy14: array[0..6] of Byte;

    StatusCode: Integer;
    Dummy1C: Longint;
  end;

  THTTPRequestHeadersReceived = record
    Request: HTTPRequestHandle;
    Dummy04: Longint;

    ContextValue: UInt64;
  end;

  THTTPRequestDataReceived = record
    Request: HTTPRequestHandle;
    Dummy04: Longint;

    ContextValue: UInt64;

    Offset: Longint;
    Dummy14: Longint;

    BytesReceived: Integer;
    Dummy1C: Longint;
  end;

function SteamHTTP: PISteamHTTP; external 'steam_api.dll';

implementation

end.
