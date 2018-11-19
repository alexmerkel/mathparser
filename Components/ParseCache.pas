{ *********************************************************************** }
{                                                                         }
{ ParseCache                                                              }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseCache;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}Classes, Cache, Notifier,
  ParseTypes;

type
  PCacheData = ^TCacheData;
  TCacheData = record
    Script: TScript;
  end;

  TLiteCache = class(TCache)
  private
    function GetCacheData(const Index: Integer): PCacheData;
  public
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    procedure AssignData(const Index: Integer; const Value: TCacheData); virtual;
    procedure Add(const Text: string; const Script: TScript); virtual;
    procedure Clear; override;
    function Find(const Text: string): TScript; virtual;
    property Data[const Index: Integer]: PCacheData read GetCacheData;
  end;

const
  HardMinCountToCache = 10;
  HardMaxCountToCache = 100000;

type
  THardCache = class(TLiteCache)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Add(const Text: string; const Script: TScript); override;
    function Find(const Text: string): TScript; override;
  published
    property MinCountToCache default HardMinCountToCache;
    property MaxCountToCache default HardMaxCountToCache;
  end;

const
  DefaultScriptType = stScript;

type
  TParseCache = class(TComponent)
  private
    FLock: PRTLCriticalSection;
    FHardCache: THardCache;
    FLiteCache: TLiteCache;
    FScriptType: TScriptType;
    procedure SetScriptType(const Value: TScriptType);
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const Text: string; const Script: TScript); virtual;
    procedure Clear; virtual;
    procedure WriteScriptType; virtual;
    function Find(const Text: string): TScript; virtual;
    property Lock: PRTLCriticalSection read FLock write FLock;
  published
    property ScriptType: TScriptType read FScriptType write SetScriptType default DefaultScriptType;
    property LiteCache: TLiteCache read FLiteCache;
    property HardCache: THardCache read FHardCache;
  end;

const
  InternalLiteCacheName = 'LiteCache';
  InternalHardCacheName = 'HardCache';

function MakeData(const Script: TScript): TCacheData;

implementation

uses
  Parser, ParseUtils, ValueTypes, ValueUtils;

function MakeData(const Script: TScript): TCacheData;
begin
  Result.Script := Copy(Script);
end;

{ TLiteCache }

procedure TLiteCache.Add(const Text: string; const Script: TScript);
var
  Data: PCacheData;
  I: Integer;
begin
  if FindParser and (TParser(Parser).UpdateCount = 0) and Enabled and (List.List.IndexOf(Text) < 0) then
  begin
    List.List.BeginUpdate;
    try
      New(Data);
      if List.List.Count < CountToCache then
      begin
        Data^ := MakeData(Script);
        List.List.AddObject(Text, TObject(Data));
      end
      else begin
        I := Next;
        AssignData(I, MakeData(Script));
        List.List[I] := Text;
      end;
    finally
      List.List.EndUpdate;
    end;
  end;
end;

procedure TLiteCache.AssignData(const Index: Integer; const Value: TCacheData);
begin
  PCacheData(List.List.Objects[Index])^ := Value;
end;

procedure TLiteCache.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.List.Count - 1 do if Assigned(List.List.Objects[I]) then
  begin
    Data[I].Script := nil;
    Dispose(PCacheData(List.List.Objects[I]));
  end;
  List.List.Clear;
end;

function TLiteCache.Find(const Text: string): TScript;
var
  I: Integer;
  AData: PCacheData;
begin
  if Enabled then
  begin
    I := List.List.IndexOf(Text);
    if I < 0 then Result := nil
    else begin
      AData := Data[I];
      if Assigned(AData) then
        Result := Copy(AData.Script)
      else
        Result := nil;
      if SmartCache then MatchCount := MakeValue(MatchCount.Signed32 + 1);
    end;
    Setup;
  end
  else Result := nil;
end;

function TLiteCache.GetCacheData(const Index: Integer): PCacheData;
begin
  Result := PCacheData(List.List.Objects[Index]);
end;

procedure TLiteCache.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  case NotifyType of
    ntBFD, ntBTD:
      begin
        CapacityScript := nil;
        RestrictScript := nil;
        Clear;
      end;
  end;
end;

{ THardCache }

procedure THardCache.Add(const Text: string; const Script: TScript);
begin
  if FindParser and (TParser(Parser).UpdateCount = 0) and Enabled then
    inherited Add(MakeTemplate(TParser(Parser), TParser(Parser).FData, Text, nil), Script);
end;

constructor THardCache.Create(AOwner: TComponent);
begin
  inherited;
  MinCountToCache := HardMinCountToCache;
  MaxCountToCache := HardMaxCountToCache;
end;

function THardCache.Find(const Text: string): TScript;
var
  S: string;
  ValueArray: TValueArray;
  Index: Integer;
begin
  if not Enabled then Result := nil
  else
    if FindParser then
    begin
      S := MakeTemplate(TParser(Parser), TParser(Parser).FData, Text, @ValueArray);
      try
        Result := inherited Find(S);
        if Assigned(Result) then
        begin
          Index := 0;
          WriteValue(Integer(Result), Index, ValueArray, ScriptType);
        end;
      finally
        ValueArray := nil;
      end;
    end;
end;

{ TParseCache }

procedure TParseCache.Add(const Text: string; const Script: TScript);
begin
  inherited;
  FLiteCache.Add(Text, Script);
  FHardCache.Add(Text, Script);
end;

procedure TParseCache.Clear;
begin
  FLiteCache.Clear;
  FHardCache.Clear;
end;

constructor TParseCache.Create(AOwner: TComponent);
begin
  inherited;
  New(FLock);
  InitializeCriticalSection(FLock^);
  FLiteCache := TLiteCache.Create(Self);
  FHardCache := THardCache.Create(Self);
  FScriptType := DefaultScriptType;
  WriteScriptType;
end;

destructor TParseCache.Destroy;
begin
  DeleteCriticalSection(FLock^);
  Dispose(FLock);
  inherited;
end;

function TParseCache.Find(const Text: string): TScript;
begin
  Result := FLiteCache.Find(Text);
  if not Assigned(Result) then Result := FHardCache.Find(Text);
end;

procedure TParseCache.SetName(const NewName: TComponentName);
begin
  inherited;
  with FLiteCache do
  begin
    Name := InternalLiteCacheName;
    SetSubComponent(True);
  end;
  with FHardCache do
  begin
    Name := InternalHardCacheName;
    SetSubComponent(True);
  end;
end;

procedure TParseCache.SetScriptType(const Value: TScriptType);
begin
  if FScriptType <> Value then
  begin
    FScriptType := Value;
    WriteScriptType;
  end;
end;

procedure TParseCache.WriteScriptType;
begin
  FLiteCache.ScriptType := FScriptType;
  FHardCache.ScriptType := FScriptType;
end;

end.
