{ *********************************************************************** }
{                                                                         }
{ Connector                                                               }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Connector;

{$B-}
{$I Directives.inc}

interface

uses
  SysUtils, Classes, EventUtils, Notifier, Parser, ParseTypes, ValueTypes;

type
  // property Parser = nil (property Connector <> nil) or property Parser <> nil (property Connector = nil)
  TCustomConnector = class(TCustomAddressee)
  protected
    function GetFunctionName: string; virtual; abstract;
    function GetNotifier: TNotifier; virtual; abstract;
    function GetPriority: Integer; virtual; abstract;
    function GetRemoteParser: TParser; virtual; abstract;
    procedure SetFunctionName(const Value: string); virtual; abstract;
    procedure SetNotifier(const Value: TNotifier); virtual; abstract;
    procedure SetPriority(const Value: Integer); virtual; abstract;
    procedure Suspend; virtual; abstract;
  public
    function Add(var Handle: Integer; const Name: string; const Priority: Integer; const Event: TFunctionEvent): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Delete(const Index: Integer): Boolean; virtual; abstract;
    function IndexOf(const AName: string): Integer; virtual; abstract;
    property RemoteParser: TParser read GetRemoteParser;
    property Notifier: TNotifier read GetNotifier write SetNotifier;
  published
    property FunctionName: string read GetFunctionName write SetFunctionName;
    property Priority: Integer read GetPriority write SetPriority;
  end;

  TConnector = class(TCustomConnector)
  private
    FEventData: TEventData;
    FFunctionEvent: TFunctionEvent;
    FFunctionHandle: Integer;
    FFunctionName: string;
    FNotifier: TNotifier;
    FPriority: Integer;
    function GetConnector: TConnector;
    function GetParser: TParser;
    procedure SetConnector(const Value: TConnector);
    procedure SetParser(const Value: TParser);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function GetFunctionName: string; override;
    function GetNotifier: TNotifier; override;
    function GetPriority: Integer; override;
    function GetRemoteParser: TParser; override;
    procedure SetFunctionName(const Value: string); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetNotifier(const Value: TNotifier); override;
    procedure SetPriority(const Value: Integer); override;
    procedure Connect; override;
    procedure Suspend; override;
    procedure Disconnect; override;
    function DoEvent(const Event: TFunctionEvent; const AFunction: PFunction; const AType: PType;
      out Value: TValue; const LValue, RValue: TValue;
      const ParameterArray: TParameterArray): Boolean; virtual;
    function Custom(const AFunction: PFunction; const AType: PType;
      out Value: TValue; const LValue, RValue: TValue;
      const ParameterArray: TParameterArray): Boolean; virtual;
    property FunctionEvent: TFunctionEvent read FFunctionEvent write FFunctionEvent;
    property FunctionHandle: Integer read FFunctionHandle write FFunctionHandle;
    property EventData: TEventData read FEventData write FEventData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    function Add(var Index: Integer; const Name: string; const Priority: Integer; const Event: TFunctionEvent): Boolean; override;
    procedure Clear; override;
    function Delete(const Index: Integer): Boolean; override;
    function IndexOf(const AName: string): Integer; override;
  published
    property Connector: TConnector read GetConnector write SetConnector;
    property Parser: TParser read GetParser write SetParser;
  end;

procedure Register;

implementation

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}MemoryUtils, TextUtils;

procedure Register;
begin
  RegisterComponents('Samples', [TConnector]);
end;

{ TConnector }

function TConnector.Add(var Index: Integer; const Name: string; const Priority: Integer; const Event: TFunctionEvent): Boolean;
begin
  Result := (Name <> '') and (IndexOf(Name) < 0);
  if Result then
    Index := EventUtils.Add(FEventData, MakeEvent(Index, Name, Priority, Event))
  else
    Index := -1;
end;

procedure TConnector.Clear;
begin
  FEventData.Events := nil;
end;

procedure TConnector.Connect;
begin
  inherited;
  if Available(Parser) then FFunctionEvent := Parser.Connect(Custom, Self)
  else if Available(Connector) then
  begin
    Connector.Notifier.Add(Self);
    Connector.Add(FFunctionHandle, FFunctionName, FPriority, Custom);
  end;
  if Available(RemoteParser) then FNotifier.Notify(ntConnect, Self);
end;

constructor TConnector.Create(AOwner: TComponent);
begin
  inherited;
  FNotifier := TNotifier.Create(Self);
  FFunctionName := CreateGuid;
end;

function TConnector.Custom(const AFunction: PFunction; const AType: PType;
  out Value: TValue; const LValue, RValue: TValue;
  const ParameterArray: TParameterArray): Boolean;
var
  I: Integer;
begin
  SortEvents(FEventData);
  for I := Low(FEventData.Events) to High(FEventData.Events) do
  begin
    Result := DoEvent(FEventData.Events[I].Event, AFunction, AType, Value, LValue, RValue, ParameterArray);
    if Result then Exit;
  end;
  Result := DoEvent(FFunctionEvent, AFunction, AType, Value, LValue, RValue, ParameterArray);
end;

function TConnector.Delete(const Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(FEventData.Events);
  Result := MemoryUtils.Delete(FEventData.Events, Index * SizeOf(TEvent), SizeOf(TEvent), I * SizeOf(TEvent));
  if Result then SetLength(FEventData.Events, I - 1);
end;

destructor TConnector.Destroy;
begin
  Disconnect;
  FNotifier.Notify(ntDisconnect, Self);
  Clear;
  inherited;
end;

procedure TConnector.Disconnect;
begin
  inherited;
  Suspend;
  if Available(Parser) then
    Parser.Addressee := nil
  else
    inherited Parser := nil;
  if Available(Connector) then
    Connector.Notifier.Delete(Self)
  else
    inherited Connector := nil;
end;

function TConnector.DoEvent(const Event: TFunctionEvent; const AFunction: PFunction;
  const AType: PType; out Value: TValue; const LValue, RValue: TValue;
  const ParameterArray: TParameterArray): Boolean;
begin
  Result := Assigned(Event) and Event(AFunction, AType, Value, LValue, RValue, ParameterArray);
end;

function TConnector.GetConnector: TConnector;
begin
  Result := TConnector(inherited Connector);
end;

function TConnector.GetFunctionName: string;
begin
  Result := FFunctionName;
end;

function TConnector.GetNotifier: TNotifier;
begin
  Result := FNotifier;
end;

function TConnector.GetParser: TParser;
begin
  Result := TParser(inherited Parser);
end;

function TConnector.GetPriority: Integer;
begin
  Result := FPriority;
end;

function TConnector.GetRemoteParser: TParser;
var
  AConnector: TCustomConnector;
begin
  Result := Parser;
  if not Assigned(Result) then
  begin
    AConnector := Connector;
    while Assigned(AConnector) and not Assigned(Result) do
    begin
      Result := TParser(AConnector.Parser);
      AConnector := TCustomConnector(AConnector.Connector);
    end;
  end;
end;

function TConnector.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  for I := Low(FEventData.Events) to High(FEventData.Events) do
    if TextUtils.SameText(AName, FEventData.Events[I].Name) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TConnector.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if ((Component = Parser) or (Component = Connector)) and (Operation = opRemove) then Disconnect;
end;

procedure TConnector.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  case NotifyType of
    ntConnect: Connect;
    ntSuspend: Suspend;
    ntDisconnect: Disconnect;
  end;
  if NotifyAttribute[NotifyType].Redirect then Notifier.Notify(NotifyType, Sender);
end;

procedure TConnector.SetConnector(const Value: TConnector);
begin
  if (Value <> Self) and (Value <> Connector) then
  begin
    Disconnect;
    inherited Connector := Value;
    inherited Parser := nil;
    Connect;
  end;
end;

procedure TConnector.SetFunctionName(const Value: string);
begin
  FFunctionName := Value;
end;

procedure TConnector.SetName(const NewName: TComponentName);
begin
  if TextUtils.SameText(Name, FFunctionName) then FFunctionName := NewName;
  inherited;
end;

procedure TConnector.SetNotifier(const Value: TNotifier);
begin
  FNotifier := Value;
end;

procedure TConnector.SetParser(const Value: TParser);
begin
  if Value <> Parser then
  begin
    Disconnect;
    inherited Connector := nil;
    inherited Parser := Value;
    Connect;
  end;
end;

procedure TConnector.SetPriority(const Value: Integer);
begin
  FPriority := Value;
end;

procedure TConnector.Suspend;
begin
  if Available(Parser) then
  begin
    if Available(Parser) then Parser.BeforeFunction := FFunctionEvent;
    FNotifier.Notify(ntSuspend, Self);
  end;
  FFunctionEvent := nil;
  if Available(Connector) then
  begin
    if Available(Connector) then Connector.Delete(FFunctionHandle);
    FNotifier.Notify(ntSuspend, Self);
  end;
end;

end.
