{ *********************************************************************** }
{                                                                         }
{ ExactTimer                                                              }
{                                                                         }
{ Copyright (c) 2015 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ExactTimer;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}{$IFDEF DELPHI_XE7}Winapi.Messages, {$ELSE}Messages, {$ENDIF}
  SysUtils, Classes, Forms;

type
  TTimerType = (ttOneShot, ttPeriodic);

const
  DefaultElapse = 1000;
  DefaultTimerType = ttPeriodic;

type
  TExactTimer = class(TComponent)
  private
    FTimerType: TTimerType;
    FActive: Boolean;
    FHandle: THandle;
    FOnTimer: TNotifyEvent;
    FElapse: Longword;
  protected
    procedure WindowMethod(var Message: TMessage); virtual;
    procedure DoTimer; virtual;
    property Handle: THandle read FHandle write FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetTimer: Boolean; virtual;
  published
    property TimerType: TTimerType read FTimerType write FTimerType default DefaultTimerType;
    property Active: Boolean read FActive write FActive default False;
    property Elapse: Longword read FElapse write FElapse default DefaultElapse;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TExactTimer]);
end;

{ TExactTimer }

constructor TExactTimer.Create(AOwner: TComponent);
begin
  inherited;
  FHandle := Classes.AllocateHWnd(WindowMethod);
  FTimerType := DefaultTimerType;
  FElapse := DefaultElapse;
end;

destructor TExactTimer.Destroy;
begin
  FActive := False;
  SetTimer;
  Classes.DeallocateHWnd(FHandle);
  inherited;
end;

procedure TExactTimer.DoTimer;
begin
  if FTimerType = ttOneShot then
  begin
    FActive := False;
    SetTimer;
  end;
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

function TExactTimer.SetTimer: Boolean;
begin
  KillTimer(FHandle, 1);
  Result := FActive and (FElapse > 0) and Assigned(FOnTimer) and ({$IFDEF DELPHI_XE7}WinApi.Windows{$ELSE}Windows{$ENDIF}.SetTimer(FHandle, 1, FElapse, nil) <> 0) or not FActive;
end;

procedure TExactTimer.WindowMethod(var Message: TMessage);
begin
  if Message.Msg = WM_TIMER then
    DoTimer
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

end.
