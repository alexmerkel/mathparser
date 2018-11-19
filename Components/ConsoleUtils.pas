{ *********************************************************************** }
{                                                                         }
{ ConsoleUtils                                                            }
{                                                                         }
{ Copyright (c) 2018 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ConsoleUtils;

{$B-}

interface

uses
  Windows, Classes, SyncThread, NumberConsts;

type
  TOutputMethod = procedure(const S: string) of object;
  TSimpleOutputMethod = procedure(Data: Pointer; const S: string);

  THandleKind = (hkRPipe, hkWPipe, hkProcess);
  THandleFlag = array[THandleKind] of Boolean;

  TKey = record
    Key: Word;
    SpecialKey: Boolean;
    Shift: TShiftState;
  end;

  TExecuteThread = class(TSyncThread)
  private
    FExitCode: Longword;
    FLine: string;
    FText: string;
    FExeName: string;
    FRPipe: THandle;
    FWPipe: THandle;
    FFlag: THandleFlag;
    FOutput: TOutputMethod;
    FPI: TProcessInformation;
    procedure SetLine(const Value: string);
  protected
    procedure Work; override;
    procedure Done; override;
    property Text: string read FText write FText;
    property Flag: THandleFlag read FFlag write FFlag;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Stop; override;
    function TypeKey(const Key: array of TKey): Boolean; virtual;
    function CtrlBreak: Boolean; virtual;
    function CtrlC: Boolean; virtual;
    function CtrlCY: Boolean; virtual;
    procedure DoOutput;
    property Line: string read FLine write SetLine;
    property ExeName: string read FExeName;
    function CreatePipe: Boolean; virtual;
    procedure CloseRPipe; virtual;
    procedure CloseWPipe; virtual;
    function CreateProcess: Boolean; virtual;
    procedure CloseProcess; virtual;
    function ProcessActive(const Time: Longword = 0): Boolean; virtual;
    procedure DeleteProcess; virtual;
    property ExitCode: Longword read FExitCode write FExitCode;
    property RPipe: THandle read FRPipe write FRPipe;
    property WPipe: THandle read FWPipe write FWPipe;
    property PI: TProcessInformation read FPI write FPI;
    property Output: TOutputMethod read FOutput write FOutput;
  end;

var
  ExecuteThread: TExecuteThread;

const
  BufferLength = 512;
  ErrorCode = High(Integer);
  AbortCode = ErrorCode - 1;
  InternalTimerName = 'Timer';

function ToOutput(Output: TSimpleOutputMethod; Data: Pointer = nil): TOutputMethod;
function Key(const AKey: Word; const AShift: TShiftState = []; const ASpecialKey: Boolean = False): TKey;
function ThreadActive: Boolean;
function GetExeName(const Line: string): string;
function Execute(const ALine: string; const AOutput: TOutputMethod = nil): Longword;
function TypeKey(const Key: array of TKey): Boolean;
function CtrlBreak: Boolean;
function CtrlC: Boolean;

implementation

uses
  Forms, StrUtils, SysUtils, TextConsts, TextUtils, Types;

function ToOutput(Output: TSimpleOutputMethod; Data: Pointer = nil): TOutputMethod;
begin
  with TMethod(Result) do
  begin
    Code := @Output;
    Data := Data;
  end;
end;

function Key(const AKey: Word; const AShift: TShiftState; const ASpecialKey: Boolean): TKey;
begin
  with Result do
  begin
    Key := AKey;
    SpecialKey := ASpecialKey;
    Shift := AShift;
  end;
end;

function ThreadActive: Boolean;
begin
  Result := Assigned(ExecuteThread) and not ExecuteThread.Stopped;
end;

function GetExeName(const Line: string): string;
begin
  if AnsiStartsText(DoubleQuote, Line) then
    Result := Extract(Line, DoubleQuote, 1, True)
  else
    Result := Extract(Line, Space, 0, True);
end;

function Execute(const ALine: string; const AOutput: TOutputMethod): Longword;
var
  Handle: THandle;
begin
  if ThreadActive then Result := ErrorCode
  else begin
    if not Assigned(ExecuteThread) then ExecuteThread := TExecuteThread.Create(nil);
    ExecuteThread.Line := ALine;
    ExecuteThread.Output := AOutput;
    ExecuteThread.Start;
    Handle := ExecuteThread.Handle;
    if Handle = 0 then Result := ErrorCode
    else begin
      while True do
        case MsgWaitForMultipleObjects(1, Handle, False, INFINITE, QS_ALLINPUT) of
          WAIT_OBJECT_0: Break;
          WAIT_OBJECT_0 + 1: Application.ProcessMessages;
          WAIT_FAILED: RaiseLastOSError;
        else
          Break;
        end;
      Result := ExecuteThread.ExitCode;
    end;
  end;
end;

function TypeKey(const Key: array of TKey): Boolean;
begin
  Result := ThreadActive and ExecuteThread.TypeKey(Key);
end;

function CtrlBreak: Boolean;
begin
  Result := ThreadActive and ExecuteThread.CtrlBreak;
end;

function CtrlC: Boolean;
begin
  Result := ThreadActive and ExecuteThread.CtrlC;
end;

{ TExecuteThread }

procedure TExecuteThread.CloseProcess;
begin
  if not FFlag[hkProcess] then
  begin
    CloseHandle(FPI.hProcess);
    CloseHandle(FPI.hThread);
    FFlag[hkProcess] := True;
  end;
end;

procedure TExecuteThread.CloseRPipe;
begin
  if not FFlag[hkRPipe] then
  begin
    CloseHandle(FRPipe);
    FFlag[hkRPipe] := True;
  end;
end;

procedure TExecuteThread.CloseWPipe;
begin
  if not FFlag[hkWPipe] then
  begin
    CloseHandle(FWPipe);
    FFlag[hkWPipe] := True;
  end;
end;

constructor TExecuteThread.Create(AOwner: TComponent);
begin
  inherited;
  Timer := TSyncTimer.Create(Self);
  with Timer do
  begin
    Name := InternalTimerName;
    SetSubComponent(True);
  end;
end;

function TExecuteThread.CreatePipe: Boolean;
var
  SA: TSecurityAttributes;
begin
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  Result := Windows.CreatePipe(FRPipe, FWPipe, @SA, 0);
end;

function TExecuteThread.CreateProcess: Boolean;
var
  SI: TStartupInfo;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  SI.wShowWindow := SW_HIDE;
  SI.hStdOutput := FWPipe;
  SI.hStdError := FWPipe;
  Result := Windows.CreateProcess(nil, PChar(FLine), nil, nil, True, CREATE_NEW_PROCESS_GROUP, nil, nil, SI, FPI);
end;

function TExecuteThread.CtrlBreak: Boolean;
begin
  Result := TypeKey([Key(VK_CANCEL, [ssCtrl])]);
end;

function TExecuteThread.CtrlC: Boolean;
begin
  Result := TypeKey([Key(Ord('C'), [ssCtrl])]);
end;

function TExecuteThread.CtrlCY: Boolean;
begin
  Result := TypeKey([Key(Ord('C'), [ssCtrl]), Key(Ord('Y')), Key(VK_RETURN)]);
end;

procedure TExecuteThread.DeleteProcess;
begin
  FExitCode := ErrorCode;
  TerminateProcess(FPI.hProcess, ErrorCode);
  CloseProcess;
end;

destructor TExecuteThread.Destroy;
begin
  ExecuteThread := nil;
  inherited;
end;

procedure TExecuteThread.Done;
begin
  inherited;
  ExecuteThread.RaiseFatalException;
end;

procedure TExecuteThread.DoOutput;
begin
  if Assigned(FOutput) then FOutput(FText);
end;

function TExecuteThread.ProcessActive(const Time: Longword): Boolean;
begin
  Result := not FFlag[hkProcess] and (WaitForSingleObject(FPI.hProcess, Time) = WAIT_TIMEOUT);
end;

procedure TExecuteThread.SetLine(const Value: string);
begin
  FLine := Value;
  FExeName := GetExeName(FLine);
end;

procedure TExecuteThread.Stop;
const
  WaitTime = 1000;
begin
  inherited;
  if ProcessActive then
  begin
    GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, FPI.dwProcessId);
    if ProcessActive(WaitTime) then DeleteProcess;
  end;
end;

function TExecuteThread.TypeKey(const Key: array of TKey): Boolean;
type
  PWindowSearch = ^TWindowSearch;
  TWindowSearch = record
    WText, CName: string;
    Window: THandle;
  end;
 
  function EachWindow(Window: THandle; Param: LPARAM): BOOL; stdcall;
  var
    WS: PWindowSearch absolute Param;
    Buffer: array[0..MAX_PATH - 1] of Char;
  begin
    FillChar(Buffer, SizeOf(Buffer), 0);
    GetWindowText(Window, Buffer, SizeOf(Buffer));
    Result := IndexOf(Buffer, WS.WText, True) = 0;
    if not Result then
    begin
      FillChar(Buffer, SizeOf(Buffer), 0);
      GetClassName(Window, Buffer, SizeOf(Buffer));
      Result := not AnsiSameText(WS.CName, Buffer);
      if not Result then WS.Window := Window;
    end;
  end;

  function Find: THandle;
  const
    CName = 'ConsoleWindowClass';
  var
    WS: TWindowSearch;
  begin
    FillChar(WS, SizeOf(WS), 0);
    WS.WText := ExtractFileName(FExeName);
    WS.CName := CName;
    EnumWindows(@EachWindow, Integer(@WS));
    Result := WS.Window;
  end;

type
  TSK = record
    S, K: Byte;
  end;
  PByteSet = ^TByteSet;
  TByteSet = set of 0..7;
const
  SKArray: array[0..2] of TSK = ((S: Ord(ssCtrl); K: VK_CONTROL), (S: Ord(ssShift); K: VK_SHIFT), (S: Ord(ssAlt); K: VK_MENU));
var
  Window: THandle;
  I, J: Integer;
  S: PByteSet;
  Flag: Longword;
begin
  Window := Find;
  Result := IsWindow(Window);
  if Result then
  begin
    SetForegroundWindow(Window);
    for I := Low(Key) to High(Key) do
    begin
      S := PByteSet(@Key[I].Shift);
      for J := Low(SKArray) to High(SKArray) do
        if SKArray[J].S in S^ then
          keybd_event(SKArray[J].K, MapVirtualKey(SKArray[J].K, 0), 0, 0);
      if Key[I].SpecialKey then
        Flag := KEYEVENTF_EXTENDEDKEY
      else
        Flag := 0;
      keybd_event(Key[I].Key, MapVirtualKey(Key[I].Key, 0), Flag, 0);
      Flag := Flag or KEYEVENTF_KEYUP;
      keybd_event(Key[I].Key, MapVirtualKey(Key[I].Key, 0), Flag, 0);
      for J := High(SKArray) downto Low(SKArray) do
        if SKArray[J].S in S^ then
          keybd_event(SKArray[J].K, MapVirtualKey(SKArray[J].K, 0), KEYEVENTF_KEYUP, 0);
    end;
  end;
end;

procedure TExecuteThread.Work;
const
  StopTime = 100;
var
  BytesRead, BytesToRead: Longword;
  S: string;
  Buffer: array[0..BufferLength - 1] of Char;
  I, J, K: Integer;
begin
  FillChar(FFlag, SizeOf(FFlag), 0);
  if not CreatePipe then RaiseLastOSError;
  try
    if not CreateProcess then RaiseLastOSError;
    try
      CloseWPipe;
      BytesToRead := BufferLength;
      BytesRead := 0;
      S := '';
      while not Stopped and ReadFile(FRPipe, Buffer, BytesToRead, BytesRead, nil) do
      begin
        Buffer[BytesRead] := #0;
        S := S + Buffer;
        J := 1;
        K := Length(LB);
        repeat
          I := PosEx(LB, S, J);
          if I > 0 then
          begin
            FText := Copy(S, J, I - J);
            Synchronize(DoOutput);
            J := I + K;
          end
          else S := Copy(S, J, MaxInt);
        until I = 0;
      end;
      if S <> '' then
      begin
        FText := S;
        Synchronize(DoOutput);
      end;
      repeat
      until Stopped or not ProcessActive(StopTime);
      if Stopped then
        FExitCode := AbortCode
      else
        if not FFlag[hkProcess] and not GetExitCodeProcess(FPI.hProcess, FExitCode) then RaiseLastOSError;
    finally
      CloseProcess;
    end;
  finally
    CloseRPipe;
  end;
end;

initialization

finalization
  ExecuteThread.Free;

end.
