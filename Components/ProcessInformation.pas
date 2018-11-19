{ *********************************************************************** }
{                                                                         }
{ ProcessInformation                                                      }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ProcessInformation;

{$B-}

interface

uses
  Windows;

const
  SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeOfDayInformation = 3;
  SystemProcessesAndThreadsInformation = 5;

  STATUS_INFO_LENGTH_MISMATCH = $C0000004;

type
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  SYSTEM_BASIC_INFORMATION = packed record
    AlwaysZero              : ULONG;
    uKeMaximumIncrement     : ULONG;
    uPageSize               : ULONG;
    uMmNumberOfPhysicalPages: ULONG;
    uMmLowestPhysicalPage   : ULONG;
    uMmHighestPhysicalPage  : ULONG;
    uAllocationGranularity  : ULONG;
    pLowestUserAddress      : POINTER;
    pMmHighestUserAddress   : POINTER;
    uKeActiveProcessors     : POINTER;
    bKeNumberProcessors     : BYTE;
    Filler                  : array [0..2] of BYTE;
  end;

  PSYSTEM_PERFORMANCE_INFORMATION = ^SYSTEM_PERFORMANCE_INFORMATION;
  SYSTEM_PERFORMANCE_INFORMATION = packed record
    nIdleTime               : INT64;
    dwSpare                 : array [0..75]of DWORD;
  end;

  PSYSTEM_TIME_INFORMATION = ^SYSTEM_TIME_INFORMATION;
  SYSTEM_TIME_INFORMATION = packed record
    nKeBootTime             : INT64;
    nKeSystemTime           : INT64;
    nExpTimeZoneBias        : INT64;
    uCurrentTimeZoneId      : ULONG;
    dwReserved              : DWORD;
  end;

  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  SYSTEM_THREADS  = packed record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: Pointer;
    UniqueProcess: DWORD;
    UniqueThread: DWORD;
    Priority: Integer;
    BasePriority: Integer;
    ContextSwitchCount: ULONG;
    State: Longint;
    WaitReason: Longint;
  end;

  PSYSTEM_PROCESS_INFORMATION = ^SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = packed record
    NextOffset: ULONG;
    ThreadCount: ULONG;
    Reserved1: array [0..5] of ULONG; // Что такое, пока не понятно...
    CreateTime: FILETIME;
    UserTime: FILETIME;
    KernelTime: FILETIME;
    ModuleNameLength: WORD;
    ModuleNameMaxLength: WORD;
    ModuleName: PWideChar;
    BasePriority: ULONG;
    ProcessID: ULONG;
    InheritedFromUniqueProcessID: ULONG;
    HandleCount: ULONG;
    Reserved2: array[0..1] of ULONG; // Что такое, пока не понятно...
    PeakVirtualSize: ULONG;
    VirtualSize: ULONG;
    PageFaultCount: ULONG;
    PeakWorkingSetSize: ULONG;
    WorkingSetSize: ULONG;
    QuotaPeakPagedPoolUsage: ULONG;
    QuotaPagedPoolUsage: ULONG;
    QuotaPeakNonPagedPoolUsage: ULONG;
    QuotaNonPagedPoolUsage: ULONG;
    PageFileUsage: ULONG;
    PeakPageFileUsage: ULONG;
    PrivatePageCount: ULONG;
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
    ThreadInfo: array [0..0] of SYSTEM_THREADS;
  end;

  TString = array[Word] of Char;

  TAccount = record
    UserName, DomainName: TString;
  end;

  PProcess = ^TProcess;
  TProcess = record
    Information: SYSTEM_PROCESS_INFORMATION;
    Account: TAccount;
  end;
  TProcessArray = array of TProcess;


  TProcessInformation = class
  private
    FProcessArray: TProcessArray;
    function GetItem(Index: Integer): PProcess;
    function GetItemCount: Integer;
  protected
    function GetAccount(const ProcessHandle: THandle; out Account: TAccount): Boolean; virtual;
    property ProcessArray: TProcessArray read FProcessArray write FProcessArray;
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    function Update: Boolean; virtual;
    property Item[Index: Integer]: PProcess read GetItem; default;
    property ItemCount: Integer read GetItemCount;
  end;

  function NtQuerySystemInformation(
    SystemInformationClass: DWORD;   // тип требуемой информации
    SystemInformation: Pointer;      // указатель на буфер, в который вернется информация
    SystemInformationLength: DWORD;  // размер буфера в байтах
    var ReturnLength: DWORD          // сколько байт было возвращено или требуется
    ): DWORD; stdcall; external 'ntdll.dll';

implementation

function Add(var ProcessArray: TProcessArray; const Process: TProcess): Integer;
begin
  Result := Length(ProcessArray);
  SetLength(ProcessArray, Result + 1);
  ProcessArray[Result] := Process;
end;

{ TProcessManager }

procedure TProcessInformation.Clear;
begin
  FProcessArray := nil;
end;

destructor TProcessInformation.Destroy;
begin
  FProcessArray := nil;
  inherited;
end;

function TProcessInformation.GetAccount(const ProcessHandle: THandle; out Account: TAccount): Boolean;
var
  TokenHandle: THandle;
  ReturnLength: Longword;
  TokenUser: PTokenUser;
  Use: SID_NAME_USE;
begin
  Result := OpenProcessToken(ProcessHandle, TOKEN_QUERY, TokenHandle);
  if Result then
  try
    GetTokenInformation(TokenHandle, Windows.TokenUser, nil, 0, ReturnLength);
    Result := GetLastError = ERROR_INSUFFICIENT_BUFFER;
    if Result then
    begin
      TokenUser := AllocMem(ReturnLength);
      if Assigned(TokenUser) then
      try
        Result := GetTokenInformation(TokenHandle, Windows.TokenUser, TokenUser, ReturnLength, ReturnLength);
        if Result then
        begin
          ReturnLength := SizeOf(TString);
          Result := LookupAccountSid(nil, TokenUser^.User.Sid, Account.UserName, ReturnLength, Account.DomainName, ReturnLength, Use);
        end;
      finally
        FreeMem(TokenUser);
      end;
    end;
  finally
    CloseHandle(TokenHandle);
  end;
end;

function TProcessInformation.GetItem(Index: Integer): PProcess;
begin
  if (Index >= 0) and (Index < Length(FProcessArray)) then
    Result := @FProcessArray[Index]
  else
    Result := nil;
end;

function TProcessInformation.GetItemCount: Integer;
begin
  Result := Length(FProcessArray);
end;

function TProcessInformation.Update: Boolean;
var
  ReturnLength: Longword;
  P: Pointer;
  Information: PSYSTEM_PROCESS_INFORMATION;
  ProcessHandle: THandle;
  Process: TProcess;
begin
  ReturnLength := 0;
  Result := (NtQuerySystemInformation(SystemProcessesAndThreadsInformation, nil, 0, ReturnLength) = STATUS_INFO_LENGTH_MISMATCH) and (ReturnLength > 0);
  if Result then
  begin
    P := AllocMem(ReturnLength);
    try
      Result := NtQuerySystemInformation(SystemProcessesAndThreadsInformation, P, ReturnLength, ReturnLength) = 0;
      if Result then
      begin
        FProcessArray := nil;
        Information := P;
        repeat
          ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, True, Information.ProcessID);
          try
            if ProcessHandle <> 0 then
            begin
              Process.Information := Information^;
              GetAccount(ProcessHandle, Process.Account);
              Add(FProcessArray, Process);
            end;
          finally
            CloseHandle(ProcessHandle);
          end;
          Information := PAnsiChar(Information) + Information.NextOffset;
        until Information.NextOffset = 0;
      end;
    finally
      FreeMem(P);
    end;
  end;
end;

end.
