unit Emu.GCDetour;

interface

procedure Init;
procedure Done;

// list of possible return values from the ISteamGameCoordinator API
type
  EGCResults =
  (
    k_EGCResultOK = 0,
    k_EGCResultNoMessage = 1,			 // There is no message in the queue
    k_EGCResultBufferTooSmall = 2, // The buffer is too small for the requested message
    k_EGCResultNotLoggedOn = 3,		 // The client is not logged onto Steam
    k_EGCResultInvalidMessage = 4	 // Something was wrong with the message being sent with SendMessage
  );

implementation

uses
  Xander.Memoria;

const
  STEAMGAMECOORDINATOR_INTERFACE_VERSION = 'SteamGameCoordinator001';

type
  VISteamGameCoordinator = record
	  // sends a message to the Game Coordinator
	  SendMessage: function(MsgType: UInt32; Data: Pointer; Size: UInt32): EGCResults; stdcall;

    // returns true if there is a message waiting from the game coordinator
    IsMessageAvailable: function(var MsgSize: UInt32): Boolean; stdcall;

    // fills the provided buffer with the first message in the queue and returns k_EGCResultOK or
    // returns k_EGCResultNoMessage if there is no message waiting. pcubMsgSize is filled with the message size.
    // If the provided buffer is not large enough to fit the entire message, k_EGCResultBufferTooSmall is returned
    // and the message remains at the head of the queue.
    RetrieveMessage: function(var MsgType: UInt32; Dest: Pointer; MaxSize: UInt32; var MsgSize: UInt32): EGCResults; stdcall;
  end;

  ISteamGameCoordinator = ^VISteamGameCoordinator;

var
  VSteamGameCoordinator: ^VISteamGameCoordinator;

function IsMessageAvailable(var MsgSize: UInt32): Boolean; stdcall;
begin
  Result := False;
end;

function RetrieveMessage(var MsgType: UInt32; Dest: Pointer; MaxSize: UInt32; var MsgSize: UInt32): EGCResults; stdcall;
begin
  Result := k_EGCResultNoMessage;
end;

procedure Init;
begin
  if VSteamGameCoordinator <> nil then
    Exit;

  //TModule.CreateModule('steamclient.dll').CreatePattern(SteamGameCoordinator).GetInterface(STEAMGAMECOORDINATOR_INTERFACE_VERSION);
  TModule.CreateModule('steamclient.dll').CreatePattern(VSteamGameCoordinator).FindVTable('IClientGameCoordinatorMap');

  if VSteamGameCoordinator <> nil then
  begin
    TMemory.WritePrimitive<Pointer>(@@VSteamGameCoordinator^.IsMessageAvailable, @IsMessageAvailable);
    TMemory.WritePrimitive<Pointer>(@@VSteamGameCoordinator^.RetrieveMessage, @RetrieveMessage);
  end;
end;

procedure Done;
begin

end;

end.
