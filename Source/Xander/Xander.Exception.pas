(*========= (C) Copyright 2017-2019, Alexander B. All rights reserved. ========*)
(*                                                                             *)
(*  Name:                                                                      *)
(*    Protector.Exception                                                      *)
(*                                                                             *)
(*  Description:                                                               *)
(*    Handling Game Exceptions with Vector Exception Chains.                   *)
(*                                                                             *)
(*=============================================================================*)

unit Xander.Exception;

{$I Default.inc}

interface

uses
  System.SysUtils,
  System.JSON,
  System.SyncObjs,

  Winapi.Windows,
  Winapi.PsAPI,

  REST.Json,

  Xander.Memoria,
  Xander.Helpers;

type
  TExceptionBuster = class
  strict private
    class var FCriticalSection: TCriticalSection;
    class var FErrorRaised: Boolean;
    class var FPreviousDumpName: string;
    class var FErrorText: string;
  strict private
    class procedure CreateExceptionDump(const Data: TJSONObject); static;
    class function ConstructProtectorDumpFileName: string; static;

    (* Converts exception information to JSON format. *)
    class function DelphiExceptionToJSON(const E: Exception): TJSONObject; static;
    class function VectorExceptionToJSON(const E: TExceptionPointers): TJSONObject; static;

    class function MemoryToJSON(Memory: Pointer): TJSONObject; static;
    class function StackToJSON(Context: PContext): TJSONObject; static;
    class function CallchainToJSON(Context: PContext): TJSONObject; static;
    class function RegistersToJSON(Context: PContext): TJSONObject; static;
    class function ModulesToJSON(Context: PContext): TJSONObject; static;

    class procedure HandleExceptionFinal(const Data: TJSONObject); static;
  public
    class constructor Create;

    class procedure HandleException(const E: Exception); overload; static;
    class procedure HandleException(const E: TExceptionPointers); overload; static;

    class procedure RaiseError(CloseApp: Boolean); static;

    class property CriticalSection: TCriticalSection read FCriticalSection;
    class property PreviousDumpName: string read FPreviousDumpName;
    class property ErrorRaised: Boolean read FErrorRaised write FErrorRaised;
    class property ErrorText: string read FErrorText write FErrorText;
  end;

const
  EXCEPTION_EXECUTE_HANDLER = 1;

  // The next VEH handler, if any, is executed. If there are no more handlers,
  // the stack is expanded to look for SEH handlers.
  EXCEPTION_CONTINUE_SEARCH = 0;

  // The handlers are not executed further, processing by means of SEH is not
  // performed, and control is transferred to the point in the program where the
  // exception occurred. As with SEH, this is not always possible.
  EXCEPTION_CONTINUE_EXECUTION = -1;

procedure CreateExceptionHandler;
procedure DestroyExceptionHandler;

procedure DisableVectoredHandlerForOnce;
procedure ResetVectoredHandler;

type
  TOnVEHPrepare = function(const ExceptionInfo: TExceptionPointers): Boolean; cdecl;
  TOnVEH = function(Module: THandle; const Name: string; const ExceptionInfo: TExceptionPointers): Integer; cdecl;
  TOnDumpBegin = procedure(Dump: TJSONObject); cdecl;
  TOnDumpEnd = procedure(Dump: TJSONObject); cdecl;
  TOnRaiseError = procedure; cdecl;

function SetOnVEHPrepare(Callback: TOnVEHPrepare): TOnVEHPrepare;
function SetOnVEH(Callback: TOnVEH): TOnVEH;
function SetOnDumpBegin(Callback: TOnDumpBegin): TOnDumpBegin;
function SetOnDumpEnd(Callback: TOnDumpEnd): TOnDumpEnd;
function SetOnRaiseError(Callback: TOnRaiseError): TOnRaiseError;

implementation

(*********************************************)
(*                 Import                    *)
(*********************************************)

function RemoveVectoredExceptionHandler(Handler: Pointer): LongWord; stdcall; external 'kernel32.dll';
function AddVectoredExceptionHandler(FirstHandler: Cardinal; const VectoredHandler: Pointer): Pointer; stdcall; external 'kernel32.dll';

(*********************************************)
(*                 Events                    *)
(*********************************************)

var
  OnVEHPrepare: TOnVEHPrepare = nil;
  OnVEH: TOnVEH = nil;
  OnDumpBegin: TOnDumpBegin = nil;
  OnDumpEnd: TOnDumpEnd = nil;
  OnRaiseError: TOnRaiseError = nil;

function SetOnVEH(Callback: TOnVEH): TOnVEH;
begin
  Result := OnVEH;
  OnVEH := Callback;
end;

function SetOnVEHPrepare(Callback: TOnVEHPrepare): TOnVEHPrepare;
begin
  Result := OnVEHPrepare;
  OnVEHPrepare := Callback;
end;

function SetOnDumpBegin(Callback: TOnDumpBegin): TOnDumpBegin;
begin
  Result := OnDumpBegin;
  OnDumpBegin := Callback;
end;

function SetOnDumpEnd(Callback: TOnDumpEnd): TOnDumpEnd;
begin
  Result := OnDumpEnd;
  OnDumpEnd := Callback;
end;

function SetOnRaiseError(Callback: TOnRaiseError): TOnRaiseError;
begin
  Result := OnRaiseError;
  OnRaiseError := Callback;
end;

(*********************************************)
(*         Temporary VEH disable             *)
(*********************************************)

var
  VectoredHandlerDisabled: Boolean = False;

procedure DisableVectoredHandlerForOnce;
begin
  VectoredHandlerDisabled := True;
end;

procedure ResetVectoredHandler;
begin
  VectoredHandlerDisabled := False;
end;

(*********************************************)
(*       Local helper functions              *)
(*********************************************)

function IntToHex(Value: Integer): string; overload;
begin
  Result := System.SysUtils.IntToHex(Value, 8);
end;

function IntToHex(Value: Pointer): string; overload;
begin
  Result := System.SysUtils.IntToHex(Integer(Value), 8);
end;

(*********************************************)
(*              VEH Implementation           *)
(*********************************************)

function VectorExceptionHandler(const ExceptionInfo: TExceptionPointers): Longint; stdcall;
var
  H: THandle;
  Name: string;
begin
  if VectoredHandlerDisabled then
  begin
    VectoredHandlerDisabled := False;
    Exit(EXCEPTION_CONTINUE_SEARCH);
  end;

  if Assigned(OnVEHPrepare) then
  begin
    if not OnVEHPrepare(ExceptionInfo) then
      Exit(EXCEPTION_CONTINUE_SEARCH);
  end;

  TExceptionBuster.CriticalSection.Enter;

  try
    (* If the exception is "Access Violation", then handle it. *)
    if ExceptionInfo.ExceptionRecord.ExceptionCode = EXCEPTION_ACCESS_VIOLATION then
    begin
      H := TMemory.GetBaseAddr(ExceptionInfo.ExceptionRecord.ExceptionAddress);
      Name := ExtractFileName(GetModuleName(H)).ToLower;

      if Assigned(OnVEH) then
      begin
        Result := OnVEH(H, Name, ExceptionInfo);
        Exit;
      end;
    end;

    Result := EXCEPTION_CONTINUE_SEARCH;
  finally
    TExceptionBuster.CriticalSection.Leave;
  end;
end;

{ TExceptionNotificator }

class function TExceptionBuster.CallchainToJSON(Context: PContext): TJSONObject;
const
  MAX_CALLS = 32;
var
  I: Integer;
  ESP: PPointer;
  Base: HMODULE;
  Ext, Module, Address: string;
begin
  Result := TJSONObject.Create;
  ESP := PPointer(Context.Esp and not $3);

  if ESP = nil then
    Exit;

  I := 0;

  repeat
    with Result do
    begin
      if not TMemory.ValidateMemory(ESP) then
        Exit;

      if I > MAX_CALLS then
        Exit;

      try
        if not TMemory.IsExecutable(ESP^) then
          Continue;

        if GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS_SAFE, Pointer(ESP^), Base) then
        begin
          Module := GetModuleName(Base);
          Module := ExtractFileName(Module);
          Ext := ExtractFileExt(Module);

          if SameText(Ext, '.dll') then
            Module := ChangeFileExt(Module, '');

          Address := System.SysUtils.Format('%s.%.08X', [Module, Cardinal(ESP^) - Base]);
        end
        else
          Address := IntToHex(Integer(ESP^), 8);

        AddPair('Address', Address);
      finally
        Inc(ESP);
      end;
    end;

    Inc(I);
  until False;
end;

class function TExceptionBuster.ConstructProtectorDumpFileName: string;
begin
  Result := 'Exception_' + DateToStr(Now) + '_' + TimeToStr(Now);
  Result := StringReplace(Result, ':', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := Result + '.json';

  FPreviousDumpName := ExtractFileDir(GetModuleName(0)) + '\' + Result;
end;

class constructor TExceptionBuster.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  FErrorRaised := False;
  FPreviousDumpName := '(null)';
  FErrorText := '';
end;

class procedure TExceptionBuster.CreateExceptionDump(const Data: TJSONObject);
const
  ReportHeader =
  '// This file is created by Game Protector due to unrecoverable error. It'#13#10 +
  '// contains data necessary to locate and remove this error. Please describe'#13#10 +
  '// circumstances that preceded exception and send this report to product developer.'#13#10 +
  '//'#13#10 +
  '// Feel free to remove any private data. Thank you for your help!'#13#10#13#10;
var
  F: TextFile;
  Name: string;
begin
  Name := Format('%s\%s', [ExtractFileDir(ParamStr(0)), ConstructProtectorDumpFileName]);

  AssignFile(F, Name);
  ReWrite(F);
  Write(F, ReportHeader);
  Write(F, TJSon.Format(Data));
  CloseFile(F);
end;

function IsStringChar(Value: AnsiChar): Boolean;
begin
  Result := (Value = #13) or (Value = #10) or (Value in [#32..#126]);
end;

function IsString(Value: PAnsiChar; Len: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Len - 1 do
    if not IsStringChar(Value[I]) then
      Exit(False);

  Exit(True);
end;

function BuildStringFromAddress(Addr: Pointer): AnsiString;
var
  I: Integer;
  Len: Integer;
begin
  SetLength(Result, 256);
  FillChar(Result[1], Length(Result), 0);

  Len := 0;
  repeat
    if not IsStringChar(PAnsiChar(Addr)[Len]) then
      Break;

    Inc(Len);
    if Len = Length(Result) then
      Break;
  until False;

  if Len = 0 then
    Exit('');

  Move(Addr^, Result[1], Len);
  SetLength(Result, Len);

  for I := 1 to Len do
    if (Result[I] = #13) or (Result[I] = #10) then
      Result[I] := '.';
end;

class function TExceptionBuster.StackToJSON(Context: PContext): TJSONObject;
const
  MAX_ELEMENTS = 256;
var
  ESP: PCardinal;

  I: Integer;

  Base: HMODULE;
  Belong: TJSONObject;
  S: AnsiString;
begin
  Result := TJSONObject.Create;

  ESP := PCardinal(Context.Esp and not $3);

  if ESP = nil then
    Exit;

  for I := 0 to MAX_ELEMENTS - 1 do
  begin
    with TJSONObject.Create do
    begin
      if not TMemory.ValidateMemory(@ESP) then
        Exit;

      if TMemory.ValidateMemory(ESP) then
      begin
        AddPair('Value', IntToHex(ESP^));

        if GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS_SAFE, Pointer(ESP^), Base) then
        begin
          Belong := TJSONObject.Create;
          Belong.AddPair('Name', ExtractFileName(GetModuleName(Base)));
          Belong.AddPair('Relative', IntToHex(ESP^ - Base));
          Belong.AddPair('Memory Dump', MemoryToJSON(PPointer(ESP)^));

          This.AddPair('Belongs', Belong);
        end;

        if IsString(PAnsiChar(ESP), 3) then
        begin
          S := BuildStringFromAddress(ESP);
          AddPair('String', S);
        end
        else
        if TMemory.ValidateMemory(PPointer(ESP)^) and IsString(PAnsiChar(ESP^), 3) then
        begin
          S := BuildStringFromAddress(PPointer(ESP^));
          AddPair('PString', S);
        end;
      end
      else
      begin
        AddPair('Value', '???');
      end;

      Result.AddPair(IntToHex(Cardinal(ESP)), This);
    end;

    Inc(ESP);
  end;
end;

class function TExceptionBuster.RegistersToJSON(Context: PContext): TJSONObject;
begin
  Result := TJSONObject.Create;

  with Result do
  begin
    AddPair('EAX', IntToHex(Context.Eax));
    AddPair('EDX', IntToHex(Context.Edx));
    AddPair('ECX', IntToHex(Context.Ecx));
    AddPair('EDI', IntToHex(Context.Edi));
    AddPair('ESI', IntToHex(Context.Esi));
    AddPair('EBX', IntToHex(Context.Ebx));
    AddPair('EBP', IntToHex(Context.Ebp));
    AddPair('ESP', IntToHex(Context.Esp));
    AddPair('EIP', IntToHex(Context.Eip));
    AddPair('EFlags', IntToHex(Context.EFlags));
  end;
end;

function NtQueryInformationThread(ThreadHandle: THandle; ThreadInformationClass: Integer; out ThreadInformation: Pointer; ThreadInformationLength: Cardinal; out ReturnLength: Cardinal): Cardinal; stdcall; external 'ntdll';

function OpenThread(DesiredAccess: LongWord; InheritHandle: BOOL; dwThreadId: LongWord): LongWord; stdcall; external 'kernel32.dll';

const
  ThreadQuerySetWin32StartAddress = 9;

  THREAD_QUERY_INFORMATION = $0040;

function GetThreadStartAddress(ThreadID: Integer): Pointer;
var
  hThread: THandle;
  ThreadStartAddress: Pointer;
begin
  hThread := OpenThread(THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then
    Exit(nil);

  try
    if NtQueryInformationThread(hThread, ThreadQuerySetWin32StartAddress, ThreadStartAddress, SizeOf(ThreadStartAddress), PCardinal(nil)^) = 0 then
      Result := ThreadStartAddress
    else
      Exit(nil);
  finally
    CloseHandle(hThread);
  end;
end;

function GetBeautyThreadStartAddress(ThreadId: Integer): string;
begin
  Result := TMemory.BeautifyPointer(GetThreadStartAddress(ThreadId));
end;

class function TExceptionBuster.VectorExceptionToJSON(
  const E: TExceptionPointers): TJSONObject;
var
  Base: Cardinal;

  Id: Integer;
begin
  try
    Result := TJSONObject.Create;

    with Result do
    begin
      with E.ExceptionRecord^ do
      begin
        Base := TMemory.GetBaseAddr(ExceptionAddress);

        AddPair('Code',             IntToHex(ExceptionCode));
        AddPair('Flags',            IntToHex(ExceptionFlags));
        AddPair('Absolute Address', IntToHex(ExceptionAddress));
        AddPair('Relative Address', IntToHex(Cardinal(ExceptionAddress) - Base));
        AddPair('Memory Dump',      MemoryToJSON(ExceptionAddress));

        AddPair('Module Base', IntToHex(Base));

        if Base = 0 then
          AddPair('Module Name', '')
        else
          AddPair('Module Name', GetModuleName(Base));

        Id := GetCurrentThreadId;
        AddPair('Thread Id', IntToStr(Id));
        AddPair('Thread Start', GetBeautyThreadStartAddress(Id));

        with E do
        begin
          AddPair('Registers', RegistersToJSON(ContextRecord));
          AddPair('Executable Stack', CallchainToJSON(ContextRecord));
          AddPair('Stack', StackToJSON(ContextRecord));
          AddPair('Modules', ModulesToJSON(ContextRecord));
        end;
      end;

      (* Vector exceptions do not generate text messages. *)
      AddPair('Message', 'nil');
    end;
  except
    Result := nil;
  end;
end;

class procedure TExceptionBuster.HandleException(const E: Exception);
var
  J: TJSONObject;
begin
  J := DelphiExceptionToJSON(E);

  if J <> nil then
    HandleExceptionFinal(J);
end;

class procedure TExceptionBuster.HandleException(const E: TExceptionPointers);
var
  J: TJSONObject;
begin
  J := VectorExceptionToJSON(E);

  if J <> nil then
    HandleExceptionFinal(J);
end;

class procedure TExceptionBuster.HandleExceptionFinal(const Data: TJSONObject);
var
  J: TJSONObject;
begin
  if Data <> nil then
  begin
    J := TJSONObject.Create;

    if Assigned(OnDumpBegin) then
      OnDumpBegin(J);

    J.AddPair('Exception', Data);

    CreateExceptionDump(J);

    if Assigned(OnDumpEnd) then
      OnDumpEnd(J);

    J.Free;
  end;
end;

class procedure TExceptionBuster.RaiseError(CloseApp: Boolean);
begin
  FErrorRaised := True;

  if Assigned(OnRaiseError) then
    OnRaiseError();

  if ErrorText = '' then
    Exit;

  (* Display a message with the collected text. *)
  MessageBox(HWND_DESKTOP, PChar(ErrorText), 'Error', MB_ICONWARNING or MB_SYSTEMMODAL);

  if CloseApp then
    ExitProcess(0);
end;

class function TExceptionBuster.DelphiExceptionToJSON(const E: Exception): TJSONObject;
var
  Base: Cardinal;
begin
  try
    Result := TJSONObject.Create;

    with Result do
    begin
      if E is EExternal then
      begin
        with (E as EExternal).ExceptionRecord^ do
        begin
          Base := TMemory.GetBaseAddr(ExceptionAddress);

          AddPair('Code',             IntToHex(ExceptionCode));
          AddPair('Flags',            IntToHex(ExceptionFlags));
          AddPair('Absolute Address', IntToHex(ExceptionAddress));
          AddPair('Relative Address', IntToHex(Cardinal(ExceptionAddress) - Base));

          AddPair('Module Base', IntToHex(Base));

          if Base = 0 then
            AddPair('Module Name', '')
          else
            AddPair('Module Name', GetModuleName(Base));
        end;
      end;

      AddPair('Message', E.Message);
    end;
  except
    Result := nil;
  end;
end;

class function TExceptionBuster.MemoryToJSON(Memory: Pointer): TJSONObject;
var
  I: Integer;
  MemPos: PByte;
  MemEnd: Pointer;
  S: string;
begin
  if Memory = nil then
    Exit(nil);

  MemEnd := TMemory.Transpose(Memory,  $40);
  MemPos := TMemory.Transpose(Memory, -$20);

  if not TMemory.ValidateMemory(MemPos) or not TMemory.ValidateMemory(MemEnd) then
    Exit(nil);

  try
    S := '';
    I := 0;
    Result := TJSONObject.Create;

    while Cardinal(MemPos) < Cardinal(MemEnd) do
    begin
      if (I + 1) mod $11 = 0 then
      begin
        if TMemory.Transpose(MemPos, -$10) = Memory then
          S := '>' + S
        else
          S := ' ' + S;

        Result.AddPair(IntToHex(Integer(MemPos) - $10, 8), S);

        S := '';
        I := 0;
      end;

      S := S + ' ' + IntToHex(MemPos^, 2);

      Inc(MemPos);
      Inc(I);
    end;
  except
    Result := nil;
  end;
end;

class function TExceptionBuster.ModulesToJSON(Context: PContext): TJSONObject;
var
  Mods: array[0..1023] of HMODULE;
  ModName: array[0..MAX_PATH] of Char;

  H: THandle;
  Needed: Cardinal;
  I: Integer;

  Info: TJSONObject;
begin
  try
    Result := TJSONObject.Create;

    with Result do
    begin
      H := GetCurrentProcess;

      if EnumProcessModules(H, @Mods, SizeOf(Mods), Needed) then
      begin
        for I := 0 to (Needed div SizeOf(Mods[0])) - 1 do
        begin
          if GetModuleFileNameEx(H, Mods[I], ModName, Length(ModName) - 1) > 0 then
          begin
            Info := TJSONObject.Create;
            Info.AddPair('Base', IntToHex(Integer(Mods[I]), 8));
            Info.AddPair('Last', IntToHex(Cardinal(Mods[I]) + TMemory.GetModuleSize(Mods[I]) - 1, 8));
            Info.AddPair('Size', TMemory.GetModuleSize(Mods[I]).ToString);

            This.AddPair(ExtractFileName(ModName), Info);
          end;
        end;
      end;
    end;
  except
    Result := nil;
  end;
end;

var
  ExceptionHandler: Pointer = nil;

procedure CreateExceptionHandler;
begin
  ExceptionHandler := AddVectoredExceptionHandler(1, @VectorExceptionHandler);

{$IFDEF DEBUG}
  if ExceptionHandler <> nil then
    WriteLn('Vectored exception handler registered. Handle: ', IntToHex(Integer(ExceptionHandler), 8))
  else
    WriteLn('Failed to create vectored exception handler.');
{$ENDIF}
end;

procedure DestroyExceptionHandler;
begin
  if ExceptionHandler <> nil then
  begin
  {$IFDEF DEBUG}
    if RemoveVectoredExceptionHandler(ExceptionHandler) = 0 then
      WriteLn('Failed to remove exception handler.')
    else
      WriteLn('Vectored exception handler successfully removed.');
  {$ELSE}
    RemoveVectoredExceptionHandler(ExceptionHandler);
  {$ENDIF}
    ExceptionHandler := nil;
  end;
end;

end.
