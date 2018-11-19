{ *********************************************************************** }
{                                                                         }
{ SystemInformation                                                       }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit SystemInformation;

{$B-}

interface

uses
  Windows, SystemTypes;

type
  WKSTA_INFO_100 = record
    wki100_platform_id: Integer;
    wki100_computername: PWideChar;
    wki100_langroup: PWideChar;
    wki100_ver_major: Integer;
    wki100_ver_minor: Integer;
  end;

  TNetApiBufferFree = function(BufPtr: Pointer): Integer; stdcall;
  TNetWkstaGetInfo = function(ServerName: PWideChar; Level: Integer; var BufPtr: Pointer): Integer; stdcall;

function GetComputerName(out DomainName: string): Boolean; overload;
function GetComputerName: string; overload;
function GetDomainName(out DomainName: string): Boolean; overload;
function GetDomainName: string; overload;

var
  UserName, ComputerName, DomainName: string;

implementation

const
  NetApi32Name = 'netapi32.dll';
  NetApiBufferFreeName = 'NetApiBufferFree';
  NetWkstaGetInfoName = 'NetWkstaGetInfo';

var
  NetApi32: THandle;
  NetApiBufferFree: TNetApiBufferFree;
  NetWkstaGetInfo: TNetWkstaGetInfo;

function GetComputerName(out DomainName: string): Boolean;
var
  Buffer: ^WKSTA_INFO_100;
begin
  Result := NetWkstaGetInfo(nil, 100, Pointer(Buffer)) = 0;
  if Result then
  try
    DomainName := WideCharToString(Buffer^.wki100_computername);
  finally
    NetApiBufferFree(Buffer);
  end;
end;

function GetComputerName: string;
begin
  if not GetComputerName(Result) then Result := '';
end;

function GetDomainName(out DomainName: string): Boolean;
var
  Buffer: ^WKSTA_INFO_100;
begin
  Result := NetWkstaGetInfo(nil, 100, Pointer(Buffer)) = 0;
  if Result then
  try
    DomainName := WideCharToString(Buffer^.wki100_langroup);
  finally
    NetApiBufferFree(Buffer);
  end;
end;

function GetDomainName: string;
begin
  if not GetDomainName(Result) then Result := '';
end;

initialization
  if NetApi32 = 0 then NetApi32 := LoadLibrary(NetApi32Name);
  if not Assigned(NetApiBufferFree) then NetApiBufferFree := GetProcAddress(NetApi32, NetApiBufferFreeName);
  if not Assigned(NetWkstaGetInfo) then NetWkstaGetInfo := GetProcAddress(NetApi32, NetWkstaGetInfoName);
  ComputerName := GetComputerName;
  DomainName := GetDomainName;

finalization
  if NetApi32 <> 0 then FreeLibrary(NetApi32);

end.
