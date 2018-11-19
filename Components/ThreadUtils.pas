{ *********************************************************************** }
{                                                                         }
{ ThreadUtils                                                             }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ThreadUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}Thread;

procedure Enter(var Lock: TRTLCriticalSection);
function TryEnter(var Lock: TRTLCriticalSection): Boolean;
procedure Leave(var Lock: TRTLCriticalSection);

implementation

procedure Enter(var Lock: TRTLCriticalSection);
begin
  EnterCriticalSection(Lock);
end;

function TryEnter(var Lock: TRTLCriticalSection): Boolean;
begin
  Result := TryEnterCriticalSection(Lock);
end;

procedure Leave(var Lock: TRTLCriticalSection);
begin
  LeaveCriticalSection(Lock);
end;

end.
