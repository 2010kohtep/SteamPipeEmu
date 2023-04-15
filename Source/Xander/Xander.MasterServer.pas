unit Xander.MasterServer;

{$I Default.inc}

interface

uses
  Winapi.Windows,
  Winapi.WinSock,

  System.Classes,
  System.SysUtils,

  Xander.Console,
  Xander.MiniUDP,
  Xander.Buffer;

const
  MS_HEADER = '1';
  OOB_HEADER = -1;

  MSMAGIC   = $BFEF2010;
  MSFILE    = 'mscache.dat';
  MSFILETXT = 'mscache.txt';

type
  TMasterServer = class(TMiniUDP)
  strict private
    FServerList: TStringList;
  protected
    procedure OnReadUDP(const ABuffer: IReadBuffer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromMaster(const AIP: AnsiString; APort: Word; AIsASCII: Boolean; const ARequest: AnsiString = ''; const AAddress: AnsiString = '');

    procedure SaveToFile(const AFileName: string; AAppend: Boolean; AAsText: Boolean = False);

    procedure SendServerList(const AIP: string; APort: Word; const AOffset: string);

    procedure PutServer(AAddr: TSockAddrIn);
    function IsExisting(AAddr: TSockAddrIn): Boolean;

    function GetServerIndex(AIP: Cardinal; APort: Word): Integer;
    procedure PrintList;

    property Servers: TStringList read FServerList;
    property List: TStringList read FServerList;
  end;

implementation

procedure SeparateAddress(const Addr: string; out IP: string; out Port: Word); overload;
begin
  if Pos(':', Addr) <> 0 then
  begin
    IP := Addr.Split([':'])[0];
    Port := StrToIntDef(Addr.Split([':'])[1], 27015);
  end
  else
  begin
    IP := Addr;
    Port := 27015;
  end;
end;

procedure SeparateAddress(const Addr: string; out IP: Integer; out Port: Word); overload;
var
  StrIP: string;
begin
  SeparateAddress(Addr, StrIP, Port);
  IP := inet_addr(PAnsiChar(AnsiString(StrIP)));
end;

{ TLocalMaster }

procedure TMasterServer.OnReadUDP(const ABuffer: IReadBuffer);
const
  WSAECONNRESET = 10054;

  MS_HEADER = '1';
  OOB_HEADER = -1;
  SPLIT_HEADER = -2;
var
  Addr: TSockAddrIn;

  IP: Cardinal;
  Port: Word;
begin
{$IFDEF DEBUG}
  TConsole.Write('[MasterServer] OnReadUDP: Size - %d, Header - $%.02X', [ABuffer.Size, ABuffer.Data^], DARKYELLOW);
{$ENDIF}

  if ABuffer.ReadInt32 <> -1 then
    Exit;

  if ABuffer.ReadInt16 = $0A66 then
  begin
  {$IFDEF DEBUG}
    TConsole.Write('[MasterServer] OnReadUDP: Received server list datagram. Processing...', DARKYELLOW);
  {$ENDIF}

    while not ABuffer.Eof do
    begin
      IP := ABuffer.ReadInt32;
      Port := ABuffer.ReadInt16;

      Addr.sin_family := AF_INET;
      Addr.sin_addr.S_addr := IP;
      Addr.sin_port := htons(Port);

      if IsExisting(Addr) then
      begin
      {$IFDEF DEBUG}
        TConsole.Write('[MasterServer] OnReadUDP: Server %s:%d already exists.', [inet_ntoa(TInAddr(IP)), Addr.sin_port], DARKYELLOW);
      {$ENDIF}
      end
      else
        PutServer(Addr);
    end;

  {$IFDEF DEBUG}
    TConsole.Write('[MasterServer] OnReadUDP: Collected %d servers.', [FServerList.Count], DARKYELLOW);
  {$ENDIF}

  {$IFDEF DEBUG}
    SaveToFile(MSFILE, False);
    SaveToFile(MSFILETXT, False, True);
  {$ENDIF}

    Exit;
  end;

  if ABuffer.ReadUInt8 = Ord(MS_HEADER) then
  begin
  {$IFDEF DEBUG}
    TConsole.Write('[MasterServer] OnReadUDP: Requested server list.', DARKYELLOW);
  {$ENDIF}

    ABuffer.ReadInt8; // region

    SendServerList(PeerIPStr, PeerPort, ABuffer.ReadLStr);
  end;
end;


{ TLocalMaster }

constructor TMasterServer.Create;
begin
  inherited Create(0);

  FServerList := TStringList.Create;
end;

destructor TMasterServer.Destroy;
begin
  inherited;
end;

function TMasterServer.GetServerIndex(AIP: Cardinal; APort: Word): Integer;
var
  Addr: string;
  I: Integer;
begin
  Addr := Format('%s:%d', [inet_ntoa(TInAddr(AIP)), htons(APort)]);

  for I := 0 to FServerList.Count - 1 do
  begin
    if FServerList[I] = Addr then
      Exit(I);
  end;

  Exit(-1);
end;

function TMasterServer.IsExisting(AAddr: TSockAddrIn): Boolean;
var
  Address, S: string;
begin
  Address := Format('%s:%d', [inet_ntoa(TInAddr(AAddr.sin_addr.S_addr)), AAddr.sin_port]);

  for S in FServerList do
  begin
    if Address = S then
      Exit(True);
  end;

  Exit(False);
end;

procedure TMasterServer.LoadFromFile(const AFileName: string);
var
  Buf: IReadBuffer;
  I: Cardinal;
  F: File of Byte;
  Data: array[0..65535] of Byte;
  S: string;
begin
  if not FileExists(AFileName) then
    Exit;

  AssignFile(F, AFileName);
  Reset(F);
  I := FileSize(F);

  if I > SizeOf(Data) then
  begin
    CloseFile(F);
    Exit;
  end;

  BlockRead(F, Data[0], I);
  CloseFile(F);

  Buf := TReadAndWriteBuffer.CreateRead(@Data[0], SizeOf(Data));
  if Buf.ReadUInt32 <> MSMAGIC then
    Exit;

  Dec(I, SizeOf(LongInt));
  while Buf.Size < I do
  begin
    Buf.ReadInt32;

    // S := Format('%s:%d', [inet_ntoa(in_addr(0)), htons(Buf.Read<Word>)]);
    FServerList.Append(S);
  end;
end;

procedure TMasterServer.LoadFromMaster(const AIP: AnsiString; APort: Word; AIsASCII: Boolean; const ARequest: AnsiString; const AAddress: AnsiString);
var
  Addr: PHostEnt;
  IP: LongWord;
  Buf: IWriteBuffer;
  Data: array[0..1024] of Byte;
begin
  if (AIP = '') or (APort = 0) then
    Exit;

  FServerList.Clear;

  Buf := TReadAndWriteBuffer.CreateWrite(@Data[0], SizeOf(Data));
  Buf.WriteUInt8(Ord('1'));
  Buf.WriteUInt8($FF);

  if AAddress <> '' then
    Buf.WriteLStr(AAddress)
  else
    Buf.WriteLStr('0.0.0.0:0');

  Buf.WriteLStr(ARequest);

  if AIsASCII then
  begin
    Addr := gethostbyname(PAnsiChar(AIP));

    if Addr <> nil then
      IP := PInteger(PCardinal(in_addr(Addr.h_addr).S_addr)^)^
    else
      IP := INADDR_NONE;
  end
  else
  begin
    IP := inet_addr(PAnsiChar(AIP));
  end;

  if IP = INADDR_NONE then
    Exit;

  Send(AIP, APort, Buf.Data, Buf.Size);
end;

procedure TMasterServer.SaveToFile(const AFileName: string; AAppend: Boolean; AAsText: Boolean);
var
  Buf: IWriteBuffer;
  Data: array[0..65535] of Byte;
  I: Integer;

  F: File of Byte;
  FT: TextFile;

  IP: Integer;
  Port: Word;
begin
  if AAsText then
  begin
    AssignFile(FT, AFileName);

    if AAppend then
      Reset(FT)
    else
      ReWrite(FT);

    WriteLn(FT, FServerList.Text);
    CloseFile(FT);

    Exit;
  end;

  Buf := TReadAndWriteBuffer.CreateWrite(@Data[0], SizeOf(Data));

  Buf.WriteUInt32(MSMAGIC);
  for I := 0 to FServerList.Count - 1 do
  begin
    SeparateAddress(FServerList[I], IP, Port);

    Buf.WriteInt32(IP);
    Buf.WriteInt16(Port);
  end;

  AssignFile(F, AFileName);

  if AAppend then Reset(F) else ReWrite(F);

  BlockWrite(F, Buf.Data^, Buf.Size);
  CloseFile(F);
end;

procedure TMasterServer.SendServerList(const AIP: string; APort: Word; const AOffset: string);
var
  Buf: IWriteBuffer;
  I: Integer;
  Data: array[0..8191] of Byte;

  CurIP: Integer;
  CurPort: Word;
begin
  if AOffset <> '0.0.0.0:0' then
    Exit;

  Buf := TReadAndWriteBuffer.CreateWrite(@Data[0], SizeOf(Data));
  Buf.WriteInt32(-1);
  Buf.WriteInt16($0A66);

  for I := 0 to FServerList.Count - 1 do
  begin
    SeparateAddress(FServerList[I], CurIP, CurPort);
    Buf.WriteInt32(CurIP);
    Buf.WriteInt16(CurPort);
  end;

  Buf.WriteInt32(0);
  Buf.WriteInt16(0);

  Send(AIP, APort, Buf.Data, Buf.Size);
end;

procedure TMasterServer.PrintList;
var
  I: Integer;
begin
  if FServerList.Count = 0 then
    TConsole.Write('[MasterServer] PrintList: List is empty.', DARKYELLOW);

  for I := 0 to FServerList.Count - 1 do
    TConsole.Write('%d. %s', [I, FServerList[I]]);
end;

procedure TMasterServer.PutServer(AAddr: TSockAddrIn);
var
  S: string;
  I: LongInt;
begin
  S := Format('%s:%d', [inet_ntoa(AAddr.sin_addr), AAddr.sin_port]);
  if not FServerList.Find(S, I) then
    FServerList.Append(S);
end;

end.

