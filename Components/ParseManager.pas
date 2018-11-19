{ *********************************************************************** }
{                                                                         }
{ ParseManager                                                            }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseManager;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Classes, Connector,
  Notifier, ParseCommon, ParseConsts, Parser, ParseTypes, TextConsts, ValueTypes;

const
  DefaultEnabled = True;
  DefaultMaxCount = 0;
  DefaultOptimization = True;
  DefaultRaiseError = True;

type
  EParseManagerError = class(Exception);

  TItemFlag = (ifExecuteOK, ifOptimizeOK, ifPermanentExecute, ifLock);
  TItemFlags = set of TItemFlag;

  PItem = ^TItem;
  TItem = record
    Item: TCommonItem;
    Flags: TItemFlags;
  end;

  TStateType = (stNecessaryExecute, stPermanentExecute, stPossibleSimplify);

  TParseManager = class(TPlugin)
  private
    FRaiseError: Boolean;
    FEnabled: Boolean;
    FOptimization: Boolean;
    FDefaultValue: TValue;
    FMaxCount: Integer;
    FErrorValue: PValue;
    FLock: PRTLCriticalSection;
    FIndex: Integer;
    FList: TStrings;
    function GetConnector: TCustomConnector;
    function GetCount: Integer;
    function GetItem(Index: Integer): PItem;
    function GetParser: TParser;
    function GetState(const AItem: PItem; const StateType: TStateType): Boolean;
    procedure SetConnector(const Value: TCustomConnector);
    procedure SetList(const Value: TStrings);
    procedure SetParser(const Value: TParser);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
    procedure Delete(const AItem: PItem); overload; virtual;
    procedure InternalDelete(Count: Integer); virtual;
    function Next: Integer; virtual;
    property State[const AItem: PItem; const StateType: TStateType]: Boolean read GetState;
    property Index: Integer read FIndex write FIndex;
    property Lock: PRTLCriticalSection read FLock write FLock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignTo(Target: TPersistent); override;
    procedure AppendTo(Target: TParseManager); virtual;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    function AssignValue(const Text, Value: string): Integer; overload; virtual;
    function AssignValue(const Text: string; Value: TValue): Integer; overload; virtual;
    function FindItem(const Text: string; ItemName: PString = nil): PItem; virtual;
    function Compile(const Index: Integer): PItem; virtual;
    function AsValue(const Text: string): TValue; overload; virtual;
    function AsValue(const Index: Integer): TValue; overload; virtual;
    function AsByte(const Text: string): Byte; overload; virtual;
    function AsByte(const Index: Integer): Byte; overload; virtual;
    function AsShortint(const Text: string): Shortint; overload; virtual;
    function AsShortint(const Index: Integer): Shortint; overload; virtual;
    function AsWord(const Text: string): Word; overload; virtual;
    function AsWord(const Index: Integer): Word; overload; virtual;
    function AsSmallint(const Text: string): Smallint; overload; virtual;
    function AsSmallint(const Index: Integer): Smallint; overload; virtual;
    function AsLongword(const Text: string): Longword; overload; virtual;
    function AsLongword(const Index: Integer): Longword; overload; virtual;
    function AsInteger(const Text: string): Integer; overload; virtual;
    function AsInteger(const Index: Integer): Integer; overload; virtual;
    function AsInt64(const Text: string): Int64; overload; virtual;
    function AsInt64(const Index: Integer): Int64; overload; virtual;
    function AsSingle(const Text: string): Single; overload; virtual;
    function AsSingle(const Index: Integer): Single; overload; virtual;
    function AsDouble(const Text: string): Double; overload; virtual;
    function AsDouble(const Index: Integer): Double; overload; virtual;
    function AsExtended(const Text: string): Extended; overload; virtual;
    function AsExtended(const Index: Integer): Extended; overload; virtual;
    function AsBoolean(const Text: string): Boolean; overload; virtual;
    function AsBoolean(const Index: Integer): Boolean; overload; virtual;
    function AsPointer(const Text: string): Pointer; overload; virtual;
    function AsPointer(const Index: Integer): Pointer; overload; virtual;
    function AsString(const Text: string): string; overload; virtual;
    function AsString(const Index: Integer): string; overload; virtual;
    procedure Optimize(const Index: Integer); overload; virtual;
    procedure Optimize; overload; virtual;
    procedure Clear; virtual;
    procedure Delete(Count: Integer); overload; virtual;
    function LoadFromFile(const FileName: string): Boolean; virtual;
    procedure SaveToFile(const FileName: string); virtual;
    property Parser: TParser read GetParser write SetParser;
    property ErrorValue: PValue read FErrorValue write FErrorValue;
    property List: TStrings read FList write SetList;
    property Item[Index: Integer]: PItem read GetItem;
    property Count: Integer read GetCount;
  published
    property Enabled: Boolean read FEnabled write FEnabled default DefaultEnabled;
    property DefaultValue: TValue read FDefaultValue write FDefaultValue;
    property RaiseError: Boolean read FRaiseError write FRaiseError default DefaultRaiseError;
    property MaxCount: Integer read FMaxCount write FMaxCount default DefaultMaxCount;
    property Optimization: Boolean read FOptimization write FOptimization default DefaultOptimization;
    property Connector: TCustomConnector read GetConnector write SetConnector;
  end;

procedure Register;

implementation

uses
  FastList, FileUtils, IniFiles, Math, NumberConsts, ParseUtils, StrUtils, TextUtils,
  ThreadUtils, Types, ValueConsts, ValueUtils;

procedure Register;
begin
  RegisterComponents('Samples', [TParseManager]);
end;

{ TParseManager }

procedure TParseManager.AppendTo(Target: TParseManager);
var
  I: Integer;
  Item: PItem;
begin
  for I := 0 to FList.Count - 1 do
    if Target.FList.IndexOf(FList[I]) < 0 then
    begin
      if Assigned(FList.Objects[I]) then
      begin
        New(Item);
        Item^ := PItem(FList.Objects[I])^;
      end
      else Item := nil;
      Target.FList.AddObject(FList[I], Pointer(Item));
    end;
end;

function TParseManager.AsBoolean(const Text: string): Boolean;
begin
  Result := Boolean(AsByte(Text));
end;

function TParseManager.AsBoolean(const Index: Integer): Boolean;
begin
  Result := Boolean(AsByte(Index));
end;

function TParseManager.AsByte(const Text: string): Byte;
begin
  Result := Convert(AsValue(Text), vtByte).Unsigned8;
end;

function TParseManager.AsByte(const Index: Integer): Byte;
begin
  Result := Convert(AsValue(Index), vtByte).Unsigned8;
end;

function TParseManager.AsDouble(const Text: string): Double;
begin
  Result := Convert(AsValue(Text), vtDouble).Float64;
end;

function TParseManager.AsDouble(const Index: Integer): Double;
begin
  Result := Convert(AsValue(Index), vtDouble).Float64;
end;

function TParseManager.AsExtended(const Text: string): Extended;
begin
  Result := Convert(AsValue(Text), vtExtended).Float80;
end;

function TParseManager.AsExtended(const Index: Integer): Extended;
begin
  Result := Convert(AsValue(Index), vtExtended).Float80;
end;

function TParseManager.AsInt64(const Text: string): Int64;
begin
  Result := Convert(AsValue(Text), vtInt64).Signed64;
end;

function TParseManager.AsInt64(const Index: Integer): Int64;
begin
  Result := Convert(AsValue(Index), vtInt64).Signed64;
end;

function TParseManager.AsInteger(const Text: string): Integer;
begin
  Result := Convert(AsValue(Text), vtInteger).Signed32;
end;

function TParseManager.AsInteger(const Index: Integer): Integer;
begin
  Result := Convert(AsValue(Index), vtInteger).Signed32;
end;

function TParseManager.AsLongword(const Text: string): Longword;
begin
  Result := Convert(AsValue(Text), vtLongword).Unsigned32;
end;

function TParseManager.AsLongword(const Index: Integer): Longword;
begin
  Result := Convert(AsValue(Index), vtLongword).Unsigned32;
end;

function TParseManager.AsPointer(const Text: string): Pointer;
begin
  Result := Pointer(AsInt64(Text));
end;

function TParseManager.AsPointer(const Index: Integer): Pointer;
begin
  Result := Pointer(AsInt64(Index));
end;

function TParseManager.AsShortint(const Text: string): Shortint;
begin
  Result := Convert(AsValue(Text), vtShortint).Signed8;
end;

function TParseManager.AsShortint(const Index: Integer): Shortint;
begin
  Result := Convert(AsValue(Index), vtShortint).Signed8;
end;

function TParseManager.AssignValue(const Text, Value: string): Integer;
var
  AItem: PItem;
begin
  Result := ParseCommon.IndexOf(FList, Text, False);
  if Result < 0 then
    Result := FList.Add(Value)
  else
    FList[Result] := Value;
  AItem := Compile(Result);
  if Assigned(AItem) then Exclude(AItem.Flags, ifExecuteOK);
end;

procedure TParseManager.AssignTo(Target: TPersistent);
var
  ATarget: TParseManager absolute Target;
begin
  if Target is TParseManager then
  begin
    ATarget.Clear;
    AppendTo(ATarget);
  end
  else inherited;
end;

function TParseManager.AssignValue(const Text: string; Value: TValue): Integer;
var
  ItemName: string;
  AItem: PItem;
begin
  Result := ParseCommon.IndexOf(FList, Text, False, @ItemName);
  if Result < 0 then
  begin
    New(AItem);
    Result := FList.AddObject(ItemName, Pointer(AItem));
    ZeroMemory(AItem, SizeOf(TItem));
  end
  else AItem := Pointer(FList.Objects[Result]);
  ParseCommon.AssignValue(AItem.Item, Value);
end;

function TParseManager.AsSingle(const Text: string): Single;
begin
  Result := Convert(AsValue(Text), vtSingle).Float32;
end;

function TParseManager.AsSingle(const Index: Integer): Single;
begin
  Result := Convert(AsValue(Index), vtSingle).Float32;
end;

function TParseManager.AsSmallint(const Text: string): Smallint;
begin
  Result := Convert(AsValue(Text), vtSmallint).Signed16;
end;

function TParseManager.AsSmallint(const Index: Integer): Smallint;
begin
  Result := Convert(AsValue(Index), vtSmallint).Signed16;
end;

function TParseManager.AsString(const Text: string): string;
begin
  Result := ValueToText(AsValue(Text));
end;

function TParseManager.AsString(const Index: Integer): string;
begin
  Result := ValueToText(AsValue(Index));
end;

function TParseManager.AsValue(const Text: string): TValue;
var
  I: Integer;
  Script: TScript;
begin
  if FindParser then
    if FEnabled and (FMaxCount > 0) then
    begin
      Enter(FLock^);
      try
        I := ParseCommon.IndexOf(FList, Text, False);
        if I < 0 then
        begin
          if FList.Count < FMaxCount then I := FList.Add(Text)
          else begin
            I := Next;
            FList[I] := Text;
          end;
          Compile(I);
        end;
        Result := AsValue(I);
      finally
        Leave(FLock^);
      end;
    end
    else begin
      Parser.StringToScript(Text, Script);
      try
        Result := Parser.Execute(Script)^;
      finally
        Script := nil;
      end;
    end
  else Result := EmptyValue;
end;

function TParseManager.AsValue(const Index: Integer): TValue;
var
  AItem: PItem;
begin
  if FEnabled and FindParser then
  begin
    AItem := Pointer(FList.Objects[Index]);
    if not Assigned(AItem) then AItem := Compile(Index);
    if Assigned(AItem) then
      case AItem.Item.ItemType of
        itNumber: Result := AItem.Item.Value;
        itScript:
          begin
            if State[AItem, stPermanentExecute] then Include(AItem.Flags, ifPermanentExecute);
            if State[AItem, stNecessaryExecute] then
              Result := Parser.Execute(AItem.Item.Script)^
            else
              Result := PScriptHeader(AItem.Item.Script).Value;
            Include(AItem.Flags, ifExecuteOK);
          end;
      else
        if Assigned(AItem.Item.Script) then
        begin
          AItem.Item.ItemType := itScript;
          Result := Parser.Execute(AItem.Item.Script)^;
        end
        else begin
          AItem.Item.ItemType := itNumber;
          Result := AItem.Item.Value;
        end;
      end
    else Result := EmptyValue;
  end
  else Result := AsValue(FList[Index]);
end;

function TParseManager.AsWord(const Text: string): Word;
begin
  Result := Convert(AsValue(Text), vtWord).Unsigned16;
end;

function TParseManager.AsWord(const Index: Integer): Word;
begin
  Result := Convert(AsValue(Index), vtWord).Unsigned16;
end;

procedure TParseManager.Clear;
begin
  InternalDelete(FList.Count);
  FList.Clear;
end;

function TParseManager.Compile(const Index: Integer): PItem;
var
  ItemName: string;
begin
  if FindParser then
  begin
    Delete(PItem(FList.Objects[Index]));
    New(Result);
    FList.Objects[Index] := Pointer(Result);
    ZeroMemory(Result, SizeOf(TItem));
    try
      ItemName := Trim(FList[Index]);
      if TrimText(ItemName, ParseCommon.Lock) then
      begin
        FList[Index] := ItemName;
        Include(Result.Flags, ifLock);
      end;
      Parser.StringToScript(FList[Index], Result.Item.Script);
      if FOptimization then Optimize(Index);
    except
      if Assigned(FErrorValue) then ParseCommon.AssignValue(Result.Item, FErrorValue^)
      else begin
        Delete(PItem(FList.Objects[Index]));
        FList.Objects[Index] := nil;
      end;
      if FRaiseError then raise;
    end;
  end
  else Result := nil;
end;

constructor TParseManager.Create(AOwner: TComponent);
begin
  inherited;
  New(FLock);
  InitializeCriticalSection(FLock^);
  FList := TFastList.Create;
  TFastList(FList).IndexTypes := [ttNameValue, ttName];
  FMaxCount := DefaultMaxCount;
  FEnabled := DefaultEnabled;
  FDefaultValue := EmptyValue;
  FOptimization := DefaultOptimization;
  FRaiseError := DefaultRaiseError;
end;

procedure TParseManager.Delete(Count: Integer);
var
  I: Integer;
begin
  if Count > FList.Count then Count := FList.Count;
  InternalDelete(Count);
  for I := Count - 1 downto 0 do FList.Delete(I);
end;

procedure TParseManager.Delete(const AItem: PItem);
begin
  if Assigned(AItem) then
  begin
    AItem.Item.Script := nil;
    Dispose(AItem);
  end;
end;

destructor TParseManager.Destroy;
begin
  Disconnect;
  Clear;
  FList.Free;
  DeleteCriticalSection(FLock^);
  Dispose(FLock);
  inherited;
end;

function TParseManager.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TParseManager.Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := EParseManagerError.CreateFmt(Message, Arguments);
end;

function TParseManager.FindItem(const Text: string; ItemName: PString): PItem;
var
  I: Integer;
begin
  I := ParseCommon.IndexOf(FList, Text, False, ItemName);
  if I < 0 then
    Result := nil
  else
    Result := Pointer(FList.Objects[I]);
end;

function TParseManager.GetConnector: TCustomConnector;
begin
  Result := TCustomConnector(inherited Connector);
end;

function TParseManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TParseManager.GetItem(Index: Integer): PItem;
begin
  Result := Pointer(FList.Objects[Index]);
end;

function TParseManager.GetParser: TParser;
begin
  Result := TParser(inherited Parser);
end;

function TParseManager.GetState(const AItem: PItem; const StateType: TStateType): Boolean;
begin
  Result := Assigned(AItem);
  if Result then
    case StateType of
      stNecessaryExecute:
        Result := not (ifExecuteOK in AItem.Flags) or (ifPermanentExecute in AItem.Flags) or (ifLock in AItem.Flags);
      stPermanentExecute:
        Result := FindParser and not (ifExecuteOK in AItem.Flags) and not (ifLock in AItem.Flags) and not Helper.Optimizable(AItem.Item.Script, Parser.FData);
      stPossibleSimplify:
        Result := not (ifLock in AItem.Flags) and Optimal(AItem.Item.Script, stScript);
    end;
end;

procedure TParseManager.InternalDelete(Count: Integer);
var
  I: Integer;
begin
  if Count > FList.Count then Count := FList.Count;
  for I := 0 to Count - 1 do
  begin
    Delete(PItem(FList.Objects[I]));
    FList.Objects[I] := nil;
  end;
end;

function TParseManager.LoadFromFile(const FileName: string): Boolean;
var
  FileIndex: Integer;
  S, AItem, BItem: string;
  Value: TValue;
begin
  Clear;
  Result := FileExists(FileName);
  if Result then
  begin
    FileIndex := CreateFile;
    try
      Result := OpenFile(FileName, otOpen);
      if Result then
      begin
        Reset(Files[FileIndex]^);
        while not Eof(Files[FileIndex]^) do
        begin
          ReadLn(Files[FileIndex]^, S);
          AItem := Trim(SubText(S, Equal, AIndex, False));
          BItem := Trim(SubText(S, Equal, BIndex, False));
          if TryTextToValue(BItem, Value) then AssignValue(AItem, Value)
          else AssignValue(AItem, BItem)
        end;
      end;
    finally
      DisposeFile(FileIndex);
    end;
  end;
end;

function TParseManager.Next: Integer;
begin
  if FIndex >= FList.Count then FIndex := 0;
  Inc(FIndex);
  Result := FIndex - 1;
end;

procedure TParseManager.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Component = Connector) and (Operation = opRemove) then
  begin
    Disconnect;
    Connector := nil;
  end;
end;

procedure TParseManager.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  case NotifyType of
    ntCompile: InternalDelete(FList.Count);
    ntDisconnect: Disconnect;
  end;
end;

procedure TParseManager.Optimize(const Index: Integer);
var
  AItem: PItem;
begin
  if FindParser then
  begin
    AItem := Pointer(FList.Objects[Index]);
    if not Assigned(AItem) then AItem := Compile(Index);
    if not (ifOptimizeOK in AItem.Flags) then
    begin
      Parser.Optimize(AItem.Item.Script);
      if State[AItem, stPossibleSimplify] then
        ParseCommon.AssignValue(AItem.Item, Parser.Execute(AItem.Item.Script)^)
      else
        AItem.Item.ItemType := itScript;
      Include(AItem.Flags, ifOptimizeOK);
    end;
  end;
end;

procedure TParseManager.Optimize;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do Optimize(I);
end;

procedure TParseManager.SaveToFile(const FileName: string);
var
  I: Integer;
begin
  FileIndex := CreateFile;
  try
    if OpenFile(FileName, otRewrite) then
    try
      for I := 0 to FList.Count - 1 do
        FileUtils.Write(FList[I] + Space + Equal + Space + ValueToText(AsValue(I)));
    finally
      SaveFile;
    end;
  finally
    DisposeFile(FileIndex);
  end;
end;

procedure TParseManager.SetConnector(const Value: TCustomConnector);
begin
  inherited Connector := Value;
end;

procedure TParseManager.SetList(const Value: TStrings);
begin
  FList.Assign(Value);
end;

procedure TParseManager.SetParser(const Value: TParser);
begin
  inherited Parser := Value;
end;

end.
