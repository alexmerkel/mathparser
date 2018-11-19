{ *********************************************************************** }
{                                                                         }
{ Cache                                                                   }
{                                                                         }
{ Copyright (c) 2014 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Cache;

{$B-}
{$I Directives.inc}

interface

uses
  Classes, FlexibleList, Notifier, ParseTypes, ValueTypes, VariableUtils;

const
  DefaultEnabled = True;
  DefaultCacheType = ltFast;
  DefaultCountToCache = 1000;
  DefaultMinCountToCache = 10;
  DefaultMaxCountToCache = 100000;
  DefaultSmartCache = True;

type
  TCache = class(TPlugin)
  private
    FSmartCache: Boolean;
    FEnabled: Boolean;
    FCountToCache: Integer;
    FIndex: Integer;
    FUsageCountVariableName: string;
    FCapacityFormula: string;
    FRestrictFormula: string;
    FCountToCacheVariableName: string;
    FMatchCountVariableName: string;
    FMinCountToCacheVariableName: string;
    FMaxCountToCacheVariableName: string;
    FList: TFlexibleList;
    FCapacityScript: TScript;
    FRestrictScript: TScript;
    FScriptType: TScriptType;
    FUsageCount: TValue;
    FMatchCount: TValue;
    FMinCount: TValue;
    FMaxCount: TValue;
    FVariable: TVariableArray;
    function GetCacheType: TListType;
    function GetCapacity: Integer;
    function GetMaxCountToCache: Integer;
    function GetMinCountToCache: Integer;
    {$IFDEF DELPHI_7}
    function GetNameValueSeparator: Char;
    {$ENDIF}
    function GetRestrict: Integer;
    procedure SetCacheType(const Value: TListType);
    procedure SetCapacityFormula(const Value: string);
    procedure SetMaxCountToCache(const Value: Integer);
    procedure SetMinCountToCache(const Value: Integer);
    {$IFDEF DELPHI_7}
    procedure SetNameValueSeparator(const Value: Char);
    {$ENDIF}
    procedure SetRestrictFormula(const Value: string);
  protected
    procedure SetName(const NewName: TComponentName); override;
    function Next: Integer; virtual;
    function Resizable: Boolean; virtual;
    property List: TFlexibleList read FList write FList;
    property Index: Integer read FIndex write FIndex;
    property MatchCount: TValue read FMatchCount write FMatchCount;
    property UsageCount: TValue read FUsageCount write FUsageCount;
    property MinCount: TValue read FMinCount write FMinCount;
    property MaxCount: TValue read FMaxCount write FMaxCount;
    property CapacityScript: TScript read FCapacityScript write FCapacityScript;
    property RestrictScript: TScript read FRestrictScript write FRestrictScript;
    property Capacity: Integer read GetCapacity;
    property Restrict: Integer read GetRestrict;
    property Variable: TVariableArray read FVariable write FVariable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    function Compile: Boolean; virtual;
    procedure Clear; virtual;
    procedure Setup; virtual;
    property ScriptType: TScriptType read FScriptType write FScriptType;
    property MatchCountVariableName: string read FMatchCountVariableName;
    property UsageCountVariableName: string read FUsageCountVariableName;
    property MinCountToCacheVariableName: string read FMinCountToCacheVariableName;
    property MaxCountToCacheVariableName: string read FMaxCountToCacheVariableName;
    property CountToCacheVariableName: string read FCountToCacheVariableName;
    {$IFDEF DELPHI_7}
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    {$ENDIF}
  published
    property Enabled: Boolean read FEnabled write FEnabled default DefaultEnabled;
    property CacheType: TListType read GetCacheType write SetCacheType default DefaultCacheType;
    property CountToCache: Integer read FCountToCache write FCountToCache default DefaultCountToCache;
    property MinCountToCache: Integer read GetMinCountToCache write SetMinCountToCache default DefaultMinCountToCache;
    property MaxCountToCache: Integer read GetMaxCountToCache write SetMaxCountToCache default DefaultMaxCountToCache;
    property SmartCache: Boolean read FSmartCache write FSmartCache default DefaultSmartCache;
    property CapacityFormula: string read FCapacityFormula write SetCapacityFormula;
    property RestrictFormula: string read FRestrictFormula write SetRestrictFormula;
  end;

const
  DefaultMatchCountName = '%s.%s.MatchCount';
  DefaultUsageCountName = '%s.%s.UsageCount';
  DefaultMinCountToCacheName = '%s.%s.MinCountToCache';
  DefaultMaxCountToCacheName = '%s.%s.MaxCountToCache';
  DefaultCountToCacheName = '%s.%s.CountToCache';
  DefaultCapacityFormula = '%s / %s * %s';
  DefaultRestrictFormula = '(%0:s / %1:s < 1 / 4) or (%0:s / %1:s > 3 / 4)';

implementation

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}Math, ParseMessages,
  Parser, SysUtils, TextConsts, TextUtils, ValueConsts, ValueUtils;

{ TCache }

procedure TCache.Clear;
begin
  FMatchCount.Signed32 := 0;
  FUsageCount.Signed32 := 0;
end;

function TCache.Compile: Boolean;
var
  P: TParser;
  I: Integer;
  AFunction: PFunction;
  Data: TVariableData;
begin
  Result := FEnabled and FSmartCache and FindParser and Assigned(FVariable);
  if Result then
  try
    P := TParser(Parser);
    P.BeginUpdate;
    try
      for I := Low(FVariable) to High(FVariable) do
      begin
        AFunction := P.FindFunction(FVariable[I].Name);
        if Assigned(AFunction) then
        begin
          FSmartCache := (AFunction.Method.MethodType = mtVariable) and (AFunction.Method.Variable.VariableType = vtValue) and
            (AFunction.Method.Variable.Variable = FVariable[I].Variable);
          if not FSmartCache then Exit;
        end
        else begin
          Data := VariableData(FVariable[I].Name, False, vtInteger);
          SendMessage(P.WindowHandle, WM_ADDVARIABLE, Integer(@Data), Integer(FVariable[I].Variable));
        end;
      end;
    finally
      P.EndUpdate;
    end;
    P.StringToScript(FCapacityFormula, FCapacityScript);
    P.Optimize(FCapacityScript);
    P.StringToScript(FRestrictFormula, FRestrictScript);
    P.Optimize(FRestrictScript);
  except
    Result := False;
  end;
end;

constructor TCache.Create(AOwner: TComponent);
begin
  inherited;
  FList := TFlexibleList.Create(Self);
  FEnabled := DefaultEnabled;
  CacheType := DefaultCacheType;
  FCountToCache := DefaultCountToCache;
  FSmartCache := DefaultSmartCache;
  AssignInteger(FMinCount, DefaultMinCountToCache);
  AssignInteger(FMaxCount, DefaultMaxCountToCache);
end;

destructor TCache.Destroy;
begin
  Disconnect;
  FCapacityScript := nil;
  FRestrictScript := nil;
  Clear;
  FVariable := nil;
  inherited;
end;

function TCache.GetCacheType: TListType;
begin
  Result := FList.ListType;
end;

function TCache.GetCapacity: Integer;
begin
  if not Assigned(FCapacityScript) then Compile;
  if Assigned(FCapacityScript) then
    Result := Convert(TParser(Parser).Execute(FCapacityScript)^, vtInteger).Signed32
  else
    Result := 0;
end;

function TCache.GetMaxCountToCache: Integer;
begin
  Result := FMaxCount.Signed32;
end;

function TCache.GetMinCountToCache: Integer;
begin
  Result := FMinCount.Signed32;
end;

function TCache.GetRestrict: Integer;
begin
  if not Assigned(FRestrictScript) then Compile;
  if Assigned(FRestrictScript) then
    Result := Convert(TParser(Parser).Execute(FRestrictScript)^, vtInteger).Signed32
  else
    Result := 0;
end;

{$IFDEF DELPHI_7}
function TCache.GetNameValueSeparator: Char;
begin
  Result := FList.NameValueSeparator;
end;
{$ENDIF}

function TCache.Next: Integer;
begin
  if FIndex >= FList.List.Count then FIndex := 0;
  Inc(FIndex);
  Result := FIndex - 1;
end;

procedure TCache.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  if NotifyType = ntDisconnect then
  begin
    Disconnect;
    FCapacityScript := nil;
    FRestrictScript := nil;
    Clear;
  end;
end;

function TCache.Resizable: Boolean;
begin
  Result := Restrict = TParser(Parser).TrueValue;
end;

procedure TCache.SetCacheType(const Value: TListType);
begin
  FList.ListType := Value;
end;

procedure TCache.SetCapacityFormula(const Value: string);
begin
  FCapacityFormula := Value;
  Compile;
end;

procedure TCache.SetMaxCountToCache(const Value: Integer);
begin
  AssignInteger(FMaxCount, Value);
end;

procedure TCache.SetMinCountToCache(const Value: Integer);
begin
  AssignInteger(FMinCount, Value);
end;

procedure TCache.SetName(const NewName: TComponentName);
var
  S: string;
begin
  inherited;
  if Assigned(Owner) and (Owner.Name <> '') then
    S := Owner.Name
  else
    S := UniqueVariableName;
  FMatchCountVariableName := Format(DefaultMatchCountName, [S, Name]);
  FUsageCountVariableName := Format(DefaultUsageCountName, [S, Name]);
  FMinCountToCacheVariableName := Format(DefaultMinCountToCacheName, [S, Name]);
  FMaxCountToCacheVariableName := Format(DefaultMaxCountToCacheName, [S, Name]);
  FCountToCacheVariableName := Format(DefaultCountToCacheName, [S, Name]);
  FCapacityFormula := Format(DefaultCapacityFormula, [FMatchCountVariableName, FUsageCountVariableName, FMaxCountToCacheVariableName]);
  FRestrictFormula := Format(DefaultRestrictFormula, [FMatchCountVariableName, FUsageCountVariableName]);
  FVariable := nil;
  VariableUtils.Add(FVariable, FMatchCountVariableName, @FMatchCount);
  VariableUtils.Add(FVariable, FUsageCountVariableName, @FUsageCount);
  VariableUtils.Add(FVariable, FMinCountToCacheVariableName, @FMinCount);
  VariableUtils.Add(FVariable, FMaxCountToCacheVariableName, @FMaxCount);
  VariableUtils.Add(FVariable, FCountToCacheVariableName, @FCountToCache);
  FMatchCount := EmptyValue;
  FUsageCount := EmptyValue;
  FMatchCount.ValueType := vtInteger;
  FUsageCount.ValueType := vtInteger;
end;

{$IFDEF DELPHI_7}
procedure TCache.SetNameValueSeparator(const Value: Char);
begin
  FList.NameValueSeparator := Value;
end;
{$ENDIF}

procedure TCache.SetRestrictFormula(const Value: string);
begin
  FRestrictFormula := Value;
  Compile;
end;

procedure TCache.Setup;
begin
  if FEnabled and FSmartCache then
  begin
    FSmartCache := FindParser;
    if FSmartCache then
    begin
      Inc(FUsageCount.Signed32);
      if (FUsageCount.Signed32 mod FCountToCache = 0) and Resizable then
      begin
        FCountToCache := EnsureRange(Capacity, FMinCount.Signed32, FMaxCount.Signed32);
        FMatchCount.Signed32 := 0;
        FUsageCount.Signed32 := 0;
      end;
    end;
  end;
end;

end.
