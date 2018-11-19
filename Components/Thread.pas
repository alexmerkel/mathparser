{ *********************************************************************** }
{                                                                         }
{ Thread                                                                  }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Thread;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, SyncObjs, Classes, Forms;

const
  DefaultSuspended = False;
  DefaultStackSize = 0;
  DefaultPriority = tpNormal;
  DefaultStopTime = 100;
  MinimumStopTime = 100;

type
  PSynchronizeData = ^TSynchronizeData;
  TSynchronizeData = record
    Thread: TObject;
    Method: TThreadMethod;
    Event: THandle;
  end;

  TThread = class(TComponent)
  private
    FSuspended: Boolean;
    FForceStopped: Boolean;
    FStopTime: Integer;
    FStopped: Boolean;
    FThreadId: Longword;
    FHandle: THandle;
    FStarted: Boolean;
    FFatalException: TObject;
    FReturn: Integer;
    FPriority: TThreadPriority;
    FStackSize: Integer;
    function GetActive: Boolean;
    function GetFinished: Boolean;
    procedure SetPriority(const Value: TThreadPriority);
    procedure SetSuspended(const Value: Boolean);
  protected
    procedure CheckThreadError(const ErrorCode: Integer); overload; virtual;
    procedure CheckThreadError(const Success: Boolean); overload; virtual;
    procedure Synchronize(const Data: PSynchronizeData); overload; virtual;
    procedure Work; virtual; abstract;
    procedure Done; virtual; abstract;
    property Return: Integer read FReturn write FReturn;
    property Started: Boolean read FStarted write FStarted;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Synchronize(const AMethod: TThreadMethod); overload; virtual;
    function Resume: Integer; virtual;
    function Start: Boolean; virtual;
    function Suspend: Integer; virtual;
    procedure Stop; virtual;
    function ForceStop(const Time: Integer = MinimumStopTime): Boolean; virtual;
    function WaitFor(const Time: Longword = 0): Boolean; virtual;
    procedure RaiseFatalException; virtual;
    property Active: Boolean read GetActive;
    property ForceStopped: Boolean read FForceStopped;
    property Stopped: Boolean read FStopped write FStopped;
    property Handle: THandle read FHandle;
    property ThreadId: Longword read FThreadId;
    property Finished: Boolean read GetFinished;
    property FatalException: TObject read FFatalException write FFatalException;
  published
    property StackSize: Integer read FStackSize write FStackSize default DefaultStackSize;
    property Suspended: Boolean read FSuspended write SetSuspended default DefaultSuspended;
    property Priority: TThreadPriority read FPriority write SetPriority default DefaultPriority;
    property StopTime: Integer read FStopTime write FStopTime default DefaultStopTime;
  end;

const
  ThreadCreateError = 'Thread creation error: %s';
  ThreadError = 'Thread Error: %s (%d)';
  STACK_SIZE_PARAM_IS_A_RESERVATION = $00010000;
  Priorities: array [TThreadPriority] of Integer = (
    THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL,
    THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

var
  SyncList: TList = nil;
  ThreadLock: TRTLCriticalSection;

function MainThread: Boolean;
function CheckSynchronize: Boolean;

implementation

function MainThread: Boolean;
begin
  Result := GetCurrentThreadId = MainThreadId;
end;

function CheckSynchronize: Boolean;
var
  ASyncList: TList;
  I: Integer;
  Data: PSynchronizeData;
begin
  ASyncList := nil;
  EnterCriticalSection(ThreadLock);
  try
    ASyncList := TInterlocked.Exchange(SyncList, ASyncList);
    try
      Result := Assigned(ASyncList) and (ASyncList.Count > 0);
      if Result then
      begin
        for I := 0 to ASyncList.Count - 1 do
        begin
          Data := ASyncList[I];
          LeaveCriticalSection(ThreadLock);
          try
            try
              Data.Method;
            except
              Application.HandleException(Data.Thread);
            end;
          finally
            EnterCriticalSection(ThreadLock);
          end;
          SetEvent(Data.Event);
        end;
      end;
    finally
      ASyncList.Free;
    end;
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

function ThreadMethod(Thread: TThread): Integer;

  procedure Catch;
  begin
    if Assigned(Thread.FFatalException) then Thread.FFatalException.Free;
    Thread.FFatalException := AcquireExceptionObject;
  end;

begin
  FreeAndNil(Thread.FFatalException);
  try
    if not Thread.Stopped then
    try
      Thread.Work;
    except
      Catch;
    end;
  finally
    Result := Thread.FReturn;
    Thread.FStarted := False;
    try
      Thread.Done;
    except
      Catch;
    end;
    EndThread(Result);
  end;
end;

{ TThread }

procedure TThread.CheckThreadError(const ErrorCode: Integer);
begin
  if ErrorCode <> 0 then raise EThread.CreateFmt(ThreadError, [SysErrorMessage(ErrorCode), ErrorCode]);
end;

procedure TThread.CheckThreadError(const Success: Boolean);
begin
  if not Success then CheckThreadError(GetLastError);
end;

constructor TThread.Create(AOwner: TComponent);
begin
  inherited;
  FStackSize := DefaultStackSize;
  FSuspended := DefaultSuspended;
  FPriority := DefaultPriority;
  FStopTime := DefaultStopTime;
end;

destructor TThread.Destroy;
begin
  if FStarted then
  begin
    if FSuspended then Resume;
    ForceStop(FStopTime);
  end;
  FFatalException.Free;
  inherited;
end;

function TThread.ForceStop(const Time: Integer): Boolean;
begin
  Result := FStarted;
  if Result then
  begin
    Stop;
    Result := (WaitForSingleObject(FHandle, Time) = WAIT_TIMEOUT) and TerminateThread(FHandle, 0);
    if Result then
    begin
      FStarted := False;
      FForceStopped := True;
      FStopped := True;
      Done;
    end;
  end;
end;

function TThread.GetActive: Boolean;
begin
  Result := FStarted;
end;

function TThread.GetFinished: Boolean;
begin
  Result := not FStarted;
end;

procedure TThread.RaiseFatalException;
begin
  if Assigned(FFatalException) then
  try
    raise FFatalException;
  finally
    FFatalException := nil;
  end;
end;

function TThread.Resume: Integer;
begin
  if FSuspended then
  begin
    Result := ResumeThread(FHandle);
    FSuspended := False;
  end
  else Result := 0
end;

procedure TThread.SetPriority(const Value: TThreadPriority);
begin
  if Value <> FPriority then
  begin
    FPriority := Value;
    if FStarted then CheckThreadError(SetThreadPriority(FHandle, Priorities[Value]));
  end;
end;

procedure TThread.SetSuspended(const Value: Boolean);
begin
  if Value <> FSuspended then
    if not FStarted then
      FSuspended := Value
    else
      if Value then Suspend
      else Resume;
end;

function TThread.Start: Boolean;
begin
  Result := not FStarted;
  if Result then
  begin
    FStarted := True;
    FForceStopped := False;
    FStopped := False;
    if FSuspended then
      if FStackSize > 0 then
        FHandle := BeginThread(nil, FStackSize, @ThreadMethod, Pointer(Self), CREATE_SUSPENDED or STACK_SIZE_PARAM_IS_A_RESERVATION, FThreadId)
      else
        FHandle := BeginThread(nil, 0, @ThreadMethod, Pointer(Self), CREATE_SUSPENDED, FThreadId)
    else
      if FStackSize > 0 then
        FHandle := BeginThread(nil, FStackSize, @ThreadMethod, Pointer(Self), STACK_SIZE_PARAM_IS_A_RESERVATION, FThreadId)
      else
        FHandle := BeginThread(nil, 0, @ThreadMethod, Pointer(Self), 0, FThreadId);
    if FHandle = 0 then raise Exception.CreateFmt(ThreadCreateError, [SysErrorMessage(GetLastError)]);
    CheckThreadError(SetThreadPriority(FHandle, Priorities[FPriority]));
  end;
end;

procedure TThread.Stop;
begin
  FStopped := True;
end;

function TThread.Suspend: Integer;
begin
  if FSuspended then Result := 0
  else begin
    Result := SuspendThread(FHandle);
    FSuspended := True;
  end;
end;

procedure TThread.Synchronize(const Data: PSynchronizeData);
begin
  if GetCurrentThreadId = MainThreadId then Data.Method
  else begin
    Data.Event := CreateEvent(nil, True, False, nil);
    try
      EnterCriticalSection(ThreadLock);
      try
        if not Assigned(SyncList) then SyncList := TList.Create;
        SyncList.Add(Data);
      finally
        LeaveCriticalSection(ThreadLock);
      end;
      WaitForSingleObject(Data.Event, INFINITE);
    finally
      CloseHandle(Data.Event);
    end;
  end;
end;

procedure TThread.Synchronize(const AMethod: TThreadMethod);
var
  Data: TSynchronizeData;
begin
  with Data do
  begin
    Thread := Self;
    Method := AMethod;
  end;
  Synchronize(@Data);
end;

function TThread.WaitFor(const Time: Longword): Boolean;
begin
  Result := FStarted and (WaitForSingleObject(FHandle, Time) <> WAIT_TIMEOUT);
end;

initialization
  InitializeCriticalSection(ThreadLock);

finalization
  DeleteCriticalSection(ThreadLock);
  FreeAndNil(SyncList);

end.
