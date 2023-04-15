(*=========== (C) Copyright 2019, Alexander B. All rights reserved. ===========*)
(*                                                                             *)
(*  Name:                                                                      *)
(*    Xander.MiniUDP                                                           *)
(*                                                                             *)
(*  Description:                                                               *)
(*=============================================================================*)

unit Xander.MiniUDP;

{$I Default.inc}

{$LEGACYIFEND ON}

{$IF CompilerVersion >= 17}
  {$DEFINE INLINE}
{$IFEND}

interface

uses
  Winapi.WinSock,
  Winapi.Windows,

  System.SysUtils,

  Xander.Buffer,
  Xander.Console;

type
  sockaddr = sockaddr_in;

const
  PORT_ANY = 0;

type
  TOnReadUDP = procedure(Sender: TObject; const Buffer: IReadBuffer) of object;

  TMiniUDP = class
  strict private
    function GetIP: string;
  protected
    class procedure ReadUDPThread(Self: TMiniUDP); static;
    procedure OnReadUDP(const Buffer: IReadBuffer); virtual;
  protected
    FSocket: TSocket;
    FReadThread: THandle;
    FPort: Word;

    FStrPeerIP: string;
    FLongPeerIP: LongWord;
    FPeerPort: Word;
  public
    constructor Create(APort: Word = PORT_ANY);
    destructor Destroy; override;

    procedure Send(const AIP: string; APort: Word; AData: Pointer; ASize: Integer); overload;
    procedure Send(const AIP: string; APort: Word; const AData: AnsiString); overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Send(const AIP: string; APort: Word; AData: TBytes); overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Send(const AIP: string; APort: Word; const AData: array of const); overload;

    property SocketHandle: TSocket read FSocket;
    property ReadThread: THandle read FReadThread;
    property IP: string read GetIP;
    property Port: Word read FPort;

    property PeerIPStr: string read FStrPeerIP;
    property PeerIP: LongWord read FLongPeerIP;
    property PeerPort: Word read FPeerPort;
  end;

implementation

var
  WSA: TWSAData;

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): {$IFDEF MSWINDOWS} Integer {$ELSE} Cardinal {$ENDIF};
begin
  Result := Winapi.WinSock.WSAStartup(wVersionRequired, WSData);
end;

procedure CloseSocket(H: TSocket);
begin
  Winapi.Winsock.closesocket(H);
end;

function SocketLastError: Integer;
begin
  Result := WSAGetLastError;
end;

constructor TMiniUDP.Create(APort: Word = PORT_ANY);
var
  I: Integer;
  Addr: TSockAddr;
begin
  inherited Create;

  FSocket := socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
  if FSocket = INVALID_SOCKET then
    raise Exception.CreateFmt('TMiniUDP.Create: socket() : %d', [SocketLastError]);

  FPort := APort;

  Addr.sin_family := AF_INET;
  Addr.sin_addr.S_addr := INADDR_ANY;
  Addr.sin_port := htons(APort);

  if bind(FSocket, TSockAddr(Addr), SizeOf(Addr)) = SOCKET_ERROR then
    raise Exception.CreateFmt('TMiniUDP.Create: bind() : %d', [SocketLastError]);

  if APort = PORT_ANY then
  begin
    I := SizeOf(Addr);
    if getsockname(FSocket, sockaddr(Addr), I) = 0 then
      FPort := htons(Addr.sin_port)
    else
      raise Exception.CreateFmt('TMiniUDP.Create: getsockname() : %d', [SocketLastError]);
  end;

  FReadThread := System.BeginThread(nil, 0, @TMiniUDP.ReadUDPThread, Self, 0, PCardinal(nil)^);
end;

destructor TMiniUDP.Destroy;
begin
  TerminateThread(FReadThread, 0);

  if FSocket <> 0 then
    closesocket(FSocket);

  inherited;
end;

function TMiniUDP.GetIP: string;
var
  HostEnt: PHostEnt;
  InAddr: in_addr;
  NameBuf: array[0..255] of Byte;
begin
  gethostname(@NameBuf[0], SizeOf(NameBuf));
  HostEnt := gethostbyname(@NameBuf[0]);
  InAddr.S_addr := Cardinal(PCardinal(HostEnt^.h_addr_list^)^);
  Result := string(inet_ntoa(InAddr));
end;

procedure TMiniUDP.OnReadUDP(const Buffer: IReadBuffer);
begin

end;

class procedure TMiniUDP.ReadUDPThread(Self: TMiniUDP);
const
  EightKB = 1024 * 8;
var
  I: Integer;

  Addr: sockaddr_in;

  Data: array[0..EightKB - 1] of Byte;
  Buffer: IReadBuffer;
begin
  repeat
    I := SizeOf(Addr);
    FillChar(Addr, SizeOf(Addr), 0);

    I := recvfrom(Self.FSocket, Data[0], SizeOf(Data), 0, sockaddr(Addr), I);

    if I = INVALID_SOCKET then
    begin
      I := GetLastError;

    {$IFDEF DEBUG}
      if I <> 10054 then // WSAECONNRESET
        TConsole.Error('TMiniUDP.OnReadUDPRaw: %d on %s:%d', [SocketLastError, Self.IP, Self.Port]);
    {$ENDIF}

      Continue;
    end;

    Self.FStrPeerIP := string(inet_ntoa(Addr.sin_addr));
    Self.FLongPeerIP := Addr.sin_addr.S_addr;
    Self.FPeerPort := htons(Addr.sin_port);

    Buffer := TReadAndWriteBuffer.CreateRead(@Data, I); // TODO: Leak?
    Self.OnReadUDP(Buffer);
  until False;
end;

procedure TMiniUDP.Send(const AIP: string; APort: Word; AData: Pointer;
  ASize: Integer);
var
  Addr: sockaddr_in;
  Host: PHostEnt;
begin
  if (AData = nil) or (ASize = 0) then
    Exit;

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);

  Host := gethostbyname(MarshaledAString(RawByteString(AIP)));
  if Host <> nil then
    Addr.sin_addr.S_addr := PInteger(PCardinal(Host.h_addr)^)^
  else
    Addr.sin_addr.S_addr := inet_addr(MarshaledAString(RawByteString(AIP))); // remove?

  sendto(FSocket, AData^, ASize, 0, sockaddr(Addr), SizeOf(Addr));
end;

procedure TMiniUDP.Send(const AIP: string; APort: Word; const AData: AnsiString);
begin
  Send(AIP, APort, PAnsiChar(AData), Length(AData));
end;

procedure TMiniUDP.Send(const AIP: string; APort: Word; AData: TBytes);
begin
  Send(AIP, APort, @AData[0], Length(AData));
end;

procedure TMiniUDP.Send(const AIP: string; APort: Word;
  const AData: array of const);
var
  I: Integer;
  J: Integer;
  Buf: array[0..8191] of Byte;
begin
  J := 0;

  for I := 0 to Length(AData) - 1 do
    case AData[I].VType of
      varBoolean, varByte:
      begin
        Buf[J] := Byte(AData[I].VChar);
        Inc(J, SizeOf(Byte));
      end;

      varWord, varSmallint:
      begin
        PWord(@Buf[J])^ := Word(AData[I].VInteger);
        Inc(J, SizeOf(Word));
      end;

      varInteger, varLongWord:
      begin
        PLongWord(@Buf[J])^ := AData[I].VInteger;
        Inc(J, SizeOf(Integer));
      end;

      varInt64:
      begin
        PInt64(@Buf[J])^ := AData[I].VInt64^;
        Inc(J, SizeOf(Int64));
      end;
    end;

  Send(AIP, APort, @AData[0], J);
end;

initialization
  if WSA.wVersion = 0 then
    if WSAStartup($0002, WSA) = INVALID_SOCKET then
      raise Exception.CreateFmt('MiniUDP.initialization: %s', [SocketLastError]);
end.

