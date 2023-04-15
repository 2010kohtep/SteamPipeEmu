unit Xander.Console;

{$I Default.inc}

interface

uses
  System.SysUtils, Winapi.Windows;

type
  TConsoleColor =
  (
    BLACK = 0,

    DARKBLUE          = FOREGROUND_BLUE,
    DARKGREEN         = FOREGROUND_GREEN,
    DARKCYAN          = FOREGROUND_GREEN or FOREGROUND_BLUE,
    DARKRED           = FOREGROUND_RED,
    DARKMAGENTA       = FOREGROUND_RED or FOREGROUND_BLUE,
    DARKYELLOW        = FOREGROUND_RED or FOREGROUND_GREEN,
    DARKGRAY          = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE,
    GRAY              = FOREGROUND_INTENSITY,
    BLUE              = FOREGROUND_INTENSITY or FOREGROUND_BLUE,
    GREEN             = FOREGROUND_INTENSITY or FOREGROUND_GREEN,
    CYAN              = FOREGROUND_INTENSITY or FOREGROUND_GREEN or FOREGROUND_BLUE,
    RED               = FOREGROUND_INTENSITY or FOREGROUND_RED,
    MAGENTA           = FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_BLUE,
    YELLOW            = FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN,
    WHITE             = FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE
  );

type
  TPrintFlags = set of (pfNone = 0, pfNoLineBreak, pfEnginePrint);

  TConsoleBasic = class
  private
    class var FInited: Boolean;
    class var FSysConsole: Boolean;
    class var FConsole: Cardinal;
    class var FDefColor: Integer;

    class procedure SetTextColor(Color: TConsoleColor); static;
    class procedure RestoreTextColor; static;
  public
    class constructor Create;

    class procedure Init(SysConsole: Boolean); static;

    class procedure Write(const Text: string; Flags: TPrintFlags = []); overload; static;
    class procedure Write(const Fmt: string; const Args: array of const; Flags: TPrintFlags = []); overload; static;

    class procedure Write(const Text: string; Color: TConsoleColor; Flags: TPrintFlags = []); overload; static;
    class procedure Write(const Fmt: string; const Args: array of const; Color: TConsoleColor; Flags: TPrintFlags = []); overload; static;

    class procedure Clear; static;
  end;

  TConsole = class(TConsoleBasic)
  public
    class procedure Warning(const Text: string; Flags: TPrintFlags = []); overload; static;
    class procedure Warning(const Fmt: string; const Args: array of const; Flags: TPrintFlags = []); overload; static;

    class procedure Error(const Text: string; Flags: TPrintFlags = []); overload; static;
    class procedure Error(const Fmt: string; const Args: array of const; Flags: TPrintFlags = []); overload; static;

    class procedure Important(const Text: string; Flags: TPrintFlags = []); overload; static;
    class procedure Important(const Fmt: string; const Args: array of const; Flags: TPrintFlags = []); overload; static;

    class procedure Success(const Text: string; Flags: TPrintFlags = []); overload; static;
    class procedure Success(const Fmt: string; const Args: array of const; Flags: TPrintFlags = []); overload; static;
  end;

implementation

{ TConsole }

class procedure TConsoleBasic.Clear;
var
  L: LongInt;
  Coord: TCoord;
begin
  Coord.X := 0;
  Coord.Y := 0;
  SetConsoleCursorPosition(FConsole, Coord);

  for L := 0 to 63 do
    WriteLn(FConsole);

  SetConsoleCursorPosition(FConsole, Coord);
end;

class constructor TConsoleBasic.Create;
begin
  FInited := False;
  FSysConsole := False;
  FConsole := INVALID_HANDLE_VALUE;
  FDefColor := -1;
end;

class procedure TConsoleBasic.Init(SysConsole: Boolean);
begin
  if SysConsole then
  begin
    FConsole := GetStdHandle(STD_OUTPUT_HANDLE);
    FSysConsole := True;
  end;

  FInited := True;
end;

class procedure TConsoleBasic.RestoreTextColor;
begin
  if not FInited then
    Exit;

  if FDefColor <> -1 then
    SetConsoleTextAttribute(FConsole, FDefColor);
end;

class procedure TConsoleBasic.SetTextColor(Color: TConsoleColor);
var
  CI: TConsoleScreenBufferInfo;
begin
  if not FInited then
    Exit;

  if FDefColor = -1 then
  begin
    GetConsoleScreenBufferInfo(FConsole, CI);
    FDefColor := CI.wAttributes;
  end;

  SetConsoleTextAttribute(FConsole, Ord(Color));
end;

procedure Msg(Fmt: PAnsiChar); cdecl varargs; external 'tier0.dll' delayed; // TODO: Replace with some good code

class procedure TConsoleBasic.Write(const Text: string; Flags: TPrintFlags);
begin
  if not FInited then
    Exit;

  System.Write(Text);

  if pfEnginePrint in Flags then
    Msg('%ls', Text);

  if not (pfNoLineBreak in Flags) then
  begin
    System.WriteLn;

    if pfEnginePrint in Flags then
      Msg(#10);
  end;
end;

class procedure TConsoleBasic.Write(const Fmt: string; const Args: array of const;
  Flags: TPrintFlags);
var
  Text: string;
begin
  Text := Format(Fmt, Args);
  Write(Text, Flags);
end;

class procedure TConsoleBasic.Write(const Fmt: string; const Args: array of const;
  Color: TConsoleColor; Flags: TPrintFlags);
begin
  SetTextColor(Color);
  Write(Fmt, Args, Flags);
  RestoreTextColor;
end;

class procedure TConsoleBasic.Write(const Text: string; Color: TConsoleColor;
  Flags: TPrintFlags);
begin
  SetTextColor(Color);
  Write(Text, Flags);
  RestoreTextColor;
end;

{ TConsoleMsg }

class procedure TConsole.Error(const Fmt: string; const Args: array of const;
  Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Fmt, Args, TConsoleColor.RED, Flags);
end;

class procedure TConsole.Important(const Text: string; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Text, TConsoleColor.CYAN, Flags);
end;

class procedure TConsole.Important(const Fmt: string;
  const Args: array of const; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Fmt, Args, TConsoleColor.CYAN, Flags);
end;

class procedure TConsole.Success(const Text: string; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Text, TConsoleColor.GREEN, Flags);
end;

class procedure TConsole.Success(const Fmt: string; const Args: array of const;
  Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Fmt, Args, TConsoleColor.GREEN, Flags);
end;

class procedure TConsole.Error(const Text: string; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Text, TConsoleColor.RED, Flags);
end;

class procedure TConsole.Warning(const Fmt: string;
  const Args: array of const; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Fmt, Args, TConsoleColor.YELLOW, Flags);
end;

class procedure TConsole.Warning(const Text: string; Flags: TPrintFlags);
begin
  TConsoleBasic.Write(Text, TConsoleColor.YELLOW, Flags);
end;

end.
