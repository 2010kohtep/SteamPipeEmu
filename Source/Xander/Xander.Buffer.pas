unit Xander.Buffer;

{$I Default.inc}

interface

uses
  System.SysUtils,
  System.Math.Vectors;

type
  TSeekKind = (skSet = 0, skCur = 1, skEnd = 2);

  IReadBuffer = interface;
  IWriteBuffer = interface;

  IBufferDescription = interface
    function GetData: PByte;
    function GetSize: Cardinal;
    function GetCapacity: Cardinal;
    function GetCurrent: PByte;
    function IsOverflowed: Boolean;
    function IsEof: Boolean;

    procedure Clear;
    procedure Seek(AValue: Integer; AKind: TSeekKind);

    function ToReadBuffer: IReadBuffer; deprecated 'Untested method.';
    function ToWriteBuffer: IWriteBuffer; deprecated 'Untested method.';

    // Data from where we will read or where we will write.
    property Data: PByte read GetData;

    // Read: The amount of data read in bytes.
    // Write: The amount of data written in bytes.
    property Size: Cardinal read GetSize;
    // Read: The maximum number of bytes that can be read.
    // Write: The "true" size of allocated memory.
    property Capacity: Cardinal read GetCapacity;
    // Read: The last write attempt may have exceeded the buffer bounds.
    // Write: The last read attempt may have exceeded buffer bounds.
    property Overflowed: Boolean read IsOverflowed;
    // The Size value is equal to the Capacity value.
    property Eof: Boolean read IsEof;

    // Get a pointer to the current byte, which will be overwritten the next
    // time any Write method is called, or which will be read by any Read method.
    //
    // WARNING! When using IReadBuffer, using Current to write a value in the
    // future is highly discouraged due to the high probability of reallocation of
    // the buffer when its size increases.
    property Current: PByte read GetCurrent;
  end;

  IReadBuffer = interface(IBufferDescription)
    function ReadInt8: Int8;
    function ReadInt16: Int16;
    function ReadInt32: Int32;
    function ReadInt64: Int64;

    function ReadUInt8: UInt8;
    function ReadUInt16: UInt16;
    function ReadUInt32: UInt32;
    function ReadUInt64: UInt64;

    function ReadSingle: Single;
    function ReadDouble: Double;

    function ReadData(ADest: Pointer; ASize: Cardinal): Boolean;

    function ReadLStr(ALength: Cardinal = 0): AnsiString;
    function ReadWStr(ALength: Cardinal = 0): WideString;

    function ReadVector: TVector;
  end;

  IWriteBuffer = interface(IBufferDescription)
    procedure WriteInt8(AValue: Int8);
    procedure WriteInt16(AValue: Int16);
    procedure WriteInt32(AValue: Int32);
    procedure WriteInt64(AValue: Int64);

    procedure WriteUInt8(AValue: UInt8);
    procedure WriteUInt16(AValue: UInt16);
    procedure WriteUInt32(AValue: UInt32);
    procedure WriteUInt64(AValue: UInt64);

    procedure WriteSingle(AValue: Single);
    procedure WriteDouble(AValue: Double);

    procedure WriteData(AValue: Pointer; ASize: Cardinal);

    procedure WriteLStr(const AValue: AnsiString);
    procedure WriteWStr(const AValue: WideString);

    procedure WriteVector(const AValue: TVector);
  end;

  TBufferKind =
  (
    bkUnknown = 0, // Invalid buffer
    bkReadBuf,     // Read buffer
    bkWriteBuf,    // Write buffer
    bkWriteBufExt  // Write buffer with external storage (memory will not be freed)
  );

  TReadAndWriteBuffer = class(TInterfacedObject, IReadBuffer, IWriteBuffer)
  strict private
    function IsWideString<T>: Boolean;
    function IsAnsiString<T>: Boolean;
  strict private
    function GetData: PByte;
    function GetSize: Cardinal;
    function GetCapacity: Cardinal;
    function GetCurrent: PByte;
    function IsOverflowed: Boolean;
    function IsEof: Boolean;

    procedure Clear;
    procedure Seek(AValue: Integer; AKind: TSeekKind);

    function ToReadBuffer: IReadBuffer;
    function ToWriteBuffer: IWriteBuffer;

    procedure WriteInt8(AValue: Int8);
    procedure WriteInt16(AValue: Int16);
    procedure WriteInt32(AValue: Int32);
    procedure WriteInt64(AValue: Int64);

    procedure WriteUInt8(AValue: UInt8);
    procedure WriteUInt16(AValue: UInt16);
    procedure WriteUInt32(AValue: UInt32);
    procedure WriteUInt64(AValue: UInt64);

    procedure WriteSingle(AValue: Single);
    procedure WriteDouble(AValue: Double);

    procedure WriteData(AValue: Pointer; ASize: Cardinal);

    procedure WriteLStr(const AValue: AnsiString);
    procedure WriteWStr(const AValue: WideString);

    procedure WriteVector(const AValue: TVector);

    function ReadInt8: Int8;
    function ReadInt16: Int16;
    function ReadInt32: Int32;
    function ReadInt64: Int64;

    function ReadUInt8: UInt8;
    function ReadUInt16: UInt16;
    function ReadUInt32: UInt32;
    function ReadUInt64: UInt64;

    function ReadSingle: Single;
    function ReadDouble: Double;

    function ReadData(ADest: Pointer; ASize: Cardinal): Boolean;

    function ReadLStr(ALength: Cardinal = 0): AnsiString;
    function ReadWStr(ALength: Cardinal = 0): WideString;

    function ReadVector: TVector;
  protected
    FKind: TBufferKind;
    FBuffer: Pointer;
    FPosition: Cardinal;
    FCapacity: Cardinal;
    FOverflowed: Boolean;

    function Read<T>: T; overload;
    procedure Read(var AData; ASize: Cardinal); overload;

    procedure Write<T>(A: T); overload;
    procedure Write(const AData; ASize: Cardinal); overload;

    constructor Create(AData: Pointer; ASize: Cardinal; AKind: TBufferKind); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;

    class function CreateRead(AData: Pointer; ASize: Cardinal): IReadBuffer; static;
    class function CreateWrite: IWriteBuffer; overload; static;
    class function CreateWrite(AData: Pointer; ASize: Cardinal): IWriteBuffer; overload; static;

    property Data: PByte read GetData;
    property Size: Cardinal read GetSize;
    property Capacity: Cardinal read GetCapacity;
    property Overflowed: Boolean read IsOverflowed;
    property Eof: Boolean read IsEof;
    property Current: PByte read GetCurrent;
  end;

implementation

{ TReadAndWriteBuffer }

procedure TReadAndWriteBuffer.Clear;
begin
  case FKind of
    bkReadBuf:
    begin
      FPosition := 0;
      FOverflowed := False;
    end;

    bkWriteBuf, bkWriteBufExt:
    begin
      if FKind <> bkWriteBufExt then
      begin
        if FBuffer <> nil then
          FreeMemory(FBuffer);
      end;

      FBuffer := nil;
      FPosition := 0;
      FCapacity := 0;
      FOverflowed := False;
    end;
  end;
end;

constructor TReadAndWriteBuffer.Create;
begin
  raise ENoConstructException.Create('Cannot construct a TReadAndWriteBuffer in this manner');
end;

class function TReadAndWriteBuffer.CreateRead(AData: Pointer; ASize: Cardinal): IReadBuffer;
begin
  Result := TReadAndWriteBuffer.Create(AData, ASize, bkReadBuf);
end;

constructor TReadAndWriteBuffer.Create(AData: Pointer; ASize: Cardinal;
  AKind: TBufferKind);
begin
  FKind := AKind;
  FBuffer := AData;
  FPosition := 0;
  FCapacity := ASize;
  FOverflowed := False;
end;

class function TReadAndWriteBuffer.CreateWrite: IWriteBuffer;
begin
  Result := TReadAndWriteBuffer.Create(nil, 0, bkWriteBuf);
end;

class function TReadAndWriteBuffer.CreateWrite(AData: Pointer;
  ASize: Cardinal): IWriteBuffer;
begin
  Result := TReadAndWriteBuffer.Create(AData, ASize, bkWriteBufExt);
end;

destructor TReadAndWriteBuffer.Destroy;
begin
  if FKind = bkWriteBuf then
    FreeMemory(FBuffer);

  inherited;
end;

function TReadAndWriteBuffer.GetCapacity: Cardinal;
begin
  Result := FCapacity;
end;

function TReadAndWriteBuffer.GetCurrent: PByte;
begin
  if FBuffer = nil then
    Exit(nil);

  Result := @PByte(FBuffer)[FPosition];
end;

function TReadAndWriteBuffer.GetData: PByte;
begin
  Result := FBuffer;
end;

function TReadAndWriteBuffer.GetSize: Cardinal;
begin
  Result := FPosition;
end;

function TReadAndWriteBuffer.IsAnsiString<T>: Boolean;
begin
  Result := (TypeInfo(T) = TypeInfo(RawByteString)) or
    (TypeInfo(T) = TypeInfo(UnicodeString)) or
    (TypeInfo(T) = TypeInfo(UTF8String))
    {$IFDEF MSWINDOWS} or (TypeInfo(T) = TypeInfo(AnsiString)) {$ENDIF}
end;

function TReadAndWriteBuffer.IsEof: Boolean;
begin
  Result := FPosition >= FCapacity;
end;

function TReadAndWriteBuffer.IsOverflowed: Boolean;
begin
  Result := FOverflowed;
end;

function TReadAndWriteBuffer.IsWideString<T>: Boolean;
begin
  Result := (TypeInfo(T) = TypeInfo(WideString));
end;

procedure TReadAndWriteBuffer.Read(var AData; ASize: Cardinal);
begin
  if FBuffer = nil then
    Exit;

  if FPosition + ASize > FCapacity then
  begin
    FOverflowed := True;
    FillChar(AData, ASize, 255);
    Exit;
  end;

  Move(PByte(FBuffer)[FPosition], AData, ASize);
  Inc(FPosition, ASize);
end;

function TReadAndWriteBuffer.Read<T>: T;
var
  Len: Integer;
  WStr: WideString absolute Result;
  LStr: RawByteString absolute Result;
begin
  if IsManagedType(T) then
  begin
    if IsWideString<T> then
    begin
      UniqueString(WStr);

      if Eof then
        WStr := ''
      else
      begin
        Len := StrLen(PWideChar(Current)) * SizeOf(WideChar);
        SetLength(WStr, Len);
        Read(PWideChar(WStr)^, Len);
        Inc(FPosition, SizeOf(WideChar));
      end;
    end
    else
    if IsAnsiString<T> then
    begin
      UniqueString(PAnsiString(@LStr)^); // Is it even legal?

      if Eof then
        LStr := ''
      else
      begin
        Len := StrLen(PAnsiChar(Current));
        SetLength(LStr, Len);
        Read(PAnsiChar(LStr)^, Len * SizeOf(AnsiChar));
        Inc(FPosition, SizeOf(AnsiChar));
      end;
    end
    else
      raise Exception.Create('TBuffer.Read<T>: Tried to read unsupported managed type.');
  end
  else
  begin
    Read(Result, SizeOf(T));
  end;
end;

function TReadAndWriteBuffer.ReadInt8: Int8;
begin
  Result := Read<Int8>;
end;

function TReadAndWriteBuffer.ReadInt16: Int16;
begin
  Result := Read<Int16>;
end;

function TReadAndWriteBuffer.ReadInt32: Int32;
begin
  Result := Read<Int32>;
end;

function TReadAndWriteBuffer.ReadInt64: Int64;
begin
  Result := Read<Int64>;
end;

function TReadAndWriteBuffer.ReadUInt8: UInt8;
begin
  Result := Read<UInt8>;
end;

function TReadAndWriteBuffer.ReadUInt16: UInt16;
begin
  Result := Read<UInt16>;
end;

function TReadAndWriteBuffer.ReadUInt32: UInt32;
begin
  Result := Read<UInt32>;
end;

function TReadAndWriteBuffer.ReadUInt64: UInt64;
begin
  Result := Read<UInt64>;
end;

function TReadAndWriteBuffer.ReadSingle: Single;
begin
  Result := Read<Single>;
end;

function TReadAndWriteBuffer.ReadData(ADest: Pointer; ASize: Cardinal): Boolean;
begin
  if (ADest = nil) or (ASize = 0) then
    Exit(False);

  Read(ADest^, ASize);

  if FOverflowed then
    Exit(False);

  Exit(True);
end;

function TReadAndWriteBuffer.ReadDouble: Double;
begin
  Result := Read<Double>;
end;

function TReadAndWriteBuffer.ReadLStr(ALength: Cardinal): AnsiString;
begin
  if ALength = 0 then
    Result := Read<AnsiString>
  else
  begin
    SetLength(Result, ALength * SizeOf(AnsiChar));
    Read(Result[1], ALength);

    if Overflowed then
      Exit('');
  end;

  if (Length(Result) = 1) and (Result[1] = #0) then
    Result := '';
end;

function TReadAndWriteBuffer.ReadWStr(ALength: Cardinal): WideString;
begin
  if ALength = 0 then
    Result := Read<WideString>
  else
  begin
    SetLength(Result, ALength * SizeOf(WideChar));
    Read(Result[1], ALength);

    if Overflowed then
      Exit('');
  end;

  if (Length(Result) = 1) and (Result[1] = #0) then
    Result := '';
end;

procedure TReadAndWriteBuffer.Seek(AValue: Integer; AKind: TSeekKind);
begin
  if FOverflowed then
    Exit;

  case AKind of
    skSet:
    begin
      FPosition := Cardinal(AValue);
    end;

    skCur:
    begin
      Inc(FPosition, AValue);
    end;

    skEnd:
    begin
      FPosition := FCapacity;
      Inc(FPosition, AValue);
    end;
  end;

  if FPosition > FCapacity then
  begin
    FOverflowed := True;
    Exit;
  end;
end;

function TReadAndWriteBuffer.ToReadBuffer: IReadBuffer;
begin
  case FKind of
    bkUnknown: raise Exception.Create('TReadAndWriteBuffer: Incorrect buffer kind.');
    bkReadBuf: Result := TReadAndWriteBuffer.Create(FBuffer, FCapacity, bkReadBuf);
    bkWriteBuf: Result := Self;
  end;
end;

function TReadAndWriteBuffer.ToWriteBuffer: IWriteBuffer;
begin
  case FKind of
    bkUnknown: raise Exception.Create('TReadAndWriteBuffer: Incorrect buffer kind.');
    bkReadBuf: Result := Self;
    bkWriteBuf: Result := TReadAndWriteBuffer.Create(FBuffer, FCapacity, bkReadBuf);
  end;
end;

function TReadAndWriteBuffer.ReadVector: TVector;
begin
  Result := Read<TVector>;
end;

procedure TReadAndWriteBuffer.Write(const AData; ASize: Cardinal);
var
  Size: Cardinal;
begin
  if FPosition + ASize > FCapacity then
  begin
    Size := FPosition + ASize + 256;
    FBuffer := ReallocMemory(FBuffer, Size);
    FCapacity := Size;
  end;

  Move(AData, PByte(FBuffer)[FPosition], ASize);
  Inc(FPosition, ASize);
end;

procedure TReadAndWriteBuffer.Write<T>(A: T);
var
  WStr: WideString absolute A;
  LStr: RawByteString absolute A;
begin
  if IsManagedType(A) then // interface, string or dynamic array
  begin
    if IsWideString<T> then
    begin
      if PPointer(@A)^ <> nil then
        Write(PPointer(@A)^^, Length(WStr) * SizeOf(WideChar) + SizeOf(WideChar))
      else
        Write<Byte>(0);
    end
    else
    if IsAnsiString<T> then
    begin
      if PPointer(@A)^ <> nil then
        Write(PPointer(@A)^^, Length(LStr) * SizeOf(AnsiChar) + SizeOf(AnsiChar))
      else
        Write<Byte>(0);
    end
    else
      raise Exception.Create('TBuffer.Write<T>: Tried to write unsupported managed type.');
  end
  else
  begin
    Write(A, SizeOf(A));
  end;
end;

procedure TReadAndWriteBuffer.WriteInt8(AValue: Int8);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteInt16(AValue: Int16);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteInt32(AValue: Int32);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteInt64(AValue: Int64);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteUInt8(AValue: UInt8);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteUInt16(AValue: UInt16);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteUInt32(AValue: UInt32);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteUInt64(AValue: UInt64);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteSingle(AValue: Single);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteData(AValue: Pointer; ASize: Cardinal);
begin
  if (AValue = nil) or (ASize = 0) then
    Exit;

  Write(AValue^, ASize);
end;

procedure TReadAndWriteBuffer.WriteDouble(AValue: Double);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteLStr(const AValue: AnsiString);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteWStr(const AValue: WideString);
begin
  Write(AValue);
end;

procedure TReadAndWriteBuffer.WriteVector(const AValue: TVector);
begin
  Write(AValue);
end;

end.
