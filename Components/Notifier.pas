{ *********************************************************************** }
{                                                                         }
{ Notifier                                                                }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Notifier;

{$B-}

interface

uses
  Classes;

type
  PNotifyType = ^TNotifyType;
  TNotifyType = (ntConnect, ntSuspend, ntDisconnect, ntCompile, ntBFA, ntAFA, ntBFD, ntAFD, ntBTA, ntATA, ntBTD, ntATD);

  PNotifyAttribute = ^TNotifyAttribute;
  TNotifyAttribute = record
    Reliable, Permanent, Redirect: Boolean;
  end;

  TCustomAddressee = class(TComponent)
  private
    FParser: TComponent;
    FConnector: TComponent;
  protected
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
  public
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); virtual; abstract;
    property Connector: TComponent read FConnector write FConnector;
    property Parser: TComponent read FParser write FParser;
  end;

  TAddressee = class(TCustomAddressee)
  private
    function GetConnector: TComponent;
    procedure SetConnector(const Value: TComponent);
  protected
    procedure Connect; override;
    procedure Disconnect; override;
  public
    property Connector: TComponent read GetConnector write SetConnector;
  end;

  // property Parser = Connector.RemoteParser
  TPlugin = class(TAddressee)
  protected
    procedure Connect; override;
    // ¬ случае размещени€ компонентов на форме в design-time пор€док их создани€ неопределен
    function FindParser: Boolean; virtual;
  end;

  TAddresseeArray = array of TCustomAddressee;

  TNotifier = class(TComponent)
  private
    FAddresseeArray: TAddresseeArray;
  protected
    property AddresseeArray: TAddresseeArray read FAddresseeArray write FAddresseeArray;
  public
    destructor Destroy; override;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); virtual;
    function Add(const Addressee: TCustomAddressee): Integer; virtual;
    procedure Clear; virtual;
    function Delete(const Addressee: TCustomAddressee): Boolean; overload; virtual;
    function Delete(const Index: Integer): Boolean; overload; virtual;
    function IndexOf(const Addressee: TCustomAddressee): Integer; virtual;
  end;

const
  NotifyAttribute: array[TNotifyType] of TNotifyAttribute = (
    (Reliable: True; Permanent: False; Redirect: False), // ntConnect
    (Reliable: True; Permanent: False; Redirect: False), // ntSuspend
    (Reliable: True; Permanent: False; Redirect: False), // ntDisconnect
    (Reliable: False; Permanent: False; Redirect: True), // ntCompile
    (Reliable: False; Permanent: True; Redirect: True), // ntBFA
    (Reliable: False; Permanent: True; Redirect: True), // ntAFA
    (Reliable: False; Permanent: True; Redirect: True), // ntBFD
    (Reliable: False; Permanent: True; Redirect: True), // ntAFD
    (Reliable: False; Permanent: True; Redirect: True), // ntBTA
    (Reliable: False; Permanent: True; Redirect: True), // ntATA
    (Reliable: False; Permanent: True; Redirect: True), // ntBTD
    (Reliable: False; Permanent: True; Redirect: True)); // ntATD

function Available(const Component: TComponent): Boolean;

implementation

uses
  Connector, MemoryUtils;

function Available(const Component: TComponent): Boolean;
begin
  Result := Assigned(Component) and not (csDestroying in Component.ComponentState);
end;

{ TAddressee }

procedure TAddressee.Connect;
var
  AConnector: TCustomConnector;
begin
  AConnector := TCustomConnector(Connector);
  if Assigned(AConnector) then AConnector.Notifier.Add(Self);
end;

procedure TAddressee.Disconnect;
var
  AConnector: TCustomConnector;
begin
  AConnector := TCustomConnector(Connector);
  if Available(AConnector) then
    AConnector.Notifier.Delete(Self)
  else
    inherited Connector := nil;
end;

function TAddressee.GetConnector: TComponent;
begin
  Result := inherited Connector;
end;

procedure TAddressee.SetConnector(const Value: TComponent);
begin
  if Value <> Connector then
  begin
    Disconnect;
    inherited Connector := Value;
    Connect;
  end;
end;

{ TPlugin }

procedure TPlugin.Connect;
begin
  inherited;
  if Available(Connector) then Parser := TCustomConnector(Connector).RemoteParser;
end;

function TPlugin.FindParser: Boolean;
begin
  if not Available(Parser) and Available(Connector) then Parser := TCustomConnector(Connector).RemoteParser;
  Result := Available(Parser);
end;

{ TNotifier }

function TNotifier.Add(const Addressee: TCustomAddressee): Integer;
begin
  if IndexOf(Addressee) < 0 then
  begin
    Result := Length(FAddresseeArray);
    SetLength(FAddresseeArray, Result + 1);
    MemoryUtils.Add(FAddresseeArray, @Addressee, Result * SizeOf(TCustomAddressee), SizeOf(TCustomAddressee));
  end
  else Result := -1;
end;

procedure TNotifier.Clear;
begin
  FAddresseeArray := nil;
end;

function TNotifier.Delete(const Addressee: TCustomAddressee): Boolean;
begin
  Result := Delete(IndexOf(Addressee));
end;

function TNotifier.Delete(const Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(FAddresseeArray);
  Result := MemoryUtils.Delete(FAddresseeArray, Index * SizeOf(TCustomAddressee), SizeOf(TCustomAddressee), I * SizeOf(TCustomAddressee));
  if Result then SetLength(FAddresseeArray, I - 1);
end;

destructor TNotifier.Destroy;
begin
  Clear;
  inherited;
end;

function TNotifier.IndexOf(const Addressee: TCustomAddressee): Integer;
begin
  Result := MemoryUtils.IndexOf(FAddresseeArray, @Addressee, Length(FAddresseeArray), SizeOf(TCustomAddressee));
end;

{$WARNINGS OFF}
procedure TNotifier.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
var
  AAddresseeArray: TAddresseeArray;
  I: Integer;
begin
  if Available(Self) then
  begin
    AAddresseeArray := Copy(FAddresseeArray);
    try
      for I := Low(AAddresseeArray) to High(AAddresseeArray) do
        if Available(AAddresseeArray[I]) then
          AAddresseeArray[I].Notify(NotifyType, Sender);
    finally
      AAddresseeArray := nil;
    end;
  end;
end;
{$WARNINGS ON}

end.
