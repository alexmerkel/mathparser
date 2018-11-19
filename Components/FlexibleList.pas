{ *********************************************************************** }
{                                                                         }
{ FlexibleList                                                            }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit FlexibleList;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SyncObjs, Classes, FastList;

type
  TListType = (ltList, ltFast);

const
  DefaultListType = ltFast;

type
  TFlexibleList = class(TComponent)
  private
    FListType: TListType;
    FList: TStrings;
    {$IFDEF DELPHI_7}
    function GetNameValueSeparator: Char;
    {$ENDIF}
    procedure SetList(const Value: TStrings);
    procedure SetListType(const Value: TListType);
    {$IFDEF DELPHI_7}
    procedure SetNameValueSeparator(const Value: Char);
    {$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF DELPHI_7}
    property NameValueSeparator: Char read GetNameValueSeparator
      write SetNameValueSeparator;
    {$ENDIF}
  published
    property List: TStrings read FList write SetList;
    property ListType: TListType read FListType write SetListType default DefaultListType;
  end;

implementation

uses
  NumberConsts;

{ TFlexibleList }

procedure TFlexibleList.AssignTo(Dest: TPersistent);
var
  AList: TFlexibleList absolute Dest;
begin
  if Dest is TFlexibleList then
  begin
    AList.List := List;
    AList.ListType := ListType;
  end
  else inherited;
end;

constructor TFlexibleList.Create(AOwner: TComponent);
begin
  inherited;
  ListType := DefaultListType;
end;

destructor TFlexibleList.Destroy;
begin
  FList.Free;
  inherited;
end;

{$IFDEF DELPHI_7}

function TFlexibleList.GetNameValueSeparator: Char;
begin
  Result := FList.NameValueSeparator;
end;

{$ENDIF}

procedure TFlexibleList.SetList(const Value: TStrings);
begin
  FList.Assign(Value);
end;

procedure TFlexibleList.SetListType(const Value: TListType);
var
  AList: TStrings;
begin
  if not Assigned(FList) or (FListType <> Value) then
  begin
    case Value of
      ltFast: AList := TFastList.Create;
    else AList := TStringList.Create;
    end;
    try
      if Assigned(FList) then AList.Assign(FList);
      FList := TInterlocked.Exchange(AList, FList);
      FListType := Value;
    finally
      AList.Free;
    end;
  end;
end;

{$IFDEF DELPHI_7}

procedure TFlexibleList.SetNameValueSeparator(const Value: Char);
begin
  FList.NameValueSeparator := Value;
end;

{$ENDIF}

end.
