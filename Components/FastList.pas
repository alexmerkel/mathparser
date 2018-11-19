{ *********************************************************************** }
{                                                                         }
{ FastList                                                                }
{                                                                         }
{ Copyright (c) 2014 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit FastList;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}Classes, Types;

type
  TBind = record
    HashCode, Index: Integer;
  end;

  TBindArray = array of TBind;

  TTextType = (ttNameValue, ttName);
  TTextTypes = set of TTextType;

  TIndex = record
    BindArray: array[TTextType] of TBindArray;
    Live: array[TTextType] of Boolean;
  end;

  TIndexArray = array of TIndex;

  TTextData = record
    HashCode, TextCode: Integer;
  end;

const
  DefaultCodeCount = 1000;

type
  TFastList = class;
  TOffsetType = (otNegative, otPositive);

  THash = class
  private
    FList: TFastList;
    FIndexArray: TIndexArray;
    function GetArrayCount: Integer;
    procedure SetArrayCount(const Value: Integer);
    procedure SetList(const Value: TFastList);
  protected
    function GetText(const TextType: TTextType; const Index: Integer): string; virtual;
    procedure SetText(const TextType: TTextType; const Index: Integer; const Value: string); virtual;
    function Check(const TextType: TTextType; const TextCode, Index: Integer): Boolean; overload; virtual;
    function Check(const TextType: TTextType; const Text: string): Boolean; overload; virtual;
    function Add(const TextType: TTextType; const Index: Integer; const Data: TTextData): Integer; overload; virtual;
    function IndexOf(const TextType: TTextType; const Data: TTextData; const S: string; const KnownIndex: Integer = -1;
      const Recurse: Boolean = True): Integer; overload; virtual;
    function RoughIndexOf(const TextType: TTextType; const Data: TTextData): Integer; overload; virtual;
    function ExactIndexOf(const TextType: TTextType; const TextCode, Index: Integer; const S: string; const KnownIndex: Integer = -1): Integer; virtual;
    function Search(const TextType: TTextType; const TextCode: Integer; const S: string): Integer; overload; virtual;
    procedure Synchronize(const TextType: TTextType; const Index: Integer; const OffsetType: TOffsetType); {$IFDEF DELPHI_XE5}overload; {$ENDIF}virtual;
    {$IFDEF DELPHI_XE5}
    procedure Synchronize(const TypeArray: array of TTextType; const Index: Integer; const OffsetType: TOffsetType); overload; virtual;
    procedure Synchronize(const Index: Integer; const OffsetType: TOffsetType); overload; virtual;
    {$ENDIF}
  public
    constructor Create(const AList: TFastList); virtual;
    destructor Destroy; override;
    function Add(const TextType: TTextType; const Index: Integer; const S: string; const TextCode: PInteger = nil): Integer; overload; virtual;
    function Delete(const TextType: TTextType; const TextCode, Index: Integer): Boolean; virtual;
    function Put(const TextType: TTextType; const FromText, ToText: string; const KnownIndex: Integer; const FromTextCode: PInteger = nil;
      const ToTextCode: PInteger = nil): Boolean; virtual;
    function IndexOf(const TextType: TTextType; const S: string; out TextCode: Integer; const KnownIndex: Integer = -1): Integer; overload; virtual;
    function Find(const TextType: TTextType; const S: string; out TextCode, Index: Integer; const KnownIndex: Integer = -1): Boolean; overload; virtual;
    function MakeHashCode(const S: string): Integer; virtual;
    function MakeTextCode(const HashCode: Integer): Integer; virtual;
    function MakeTextData(const S: string): TTextData; virtual;
    procedure CreateIndex(const TextType: TTextType); virtual;
    procedure DeleteIndex(const TextType: TTextType; const TextCode: Integer); overload; virtual;
    procedure DeleteIndex(const TextType: TTextType); overload; virtual;
    procedure DeleteIndex; overload; virtual;
    procedure EnableIndex(const TextType: TTextType; const TextCode: Integer); overload; virtual;
    procedure EnableIndex(const TextType: TTextType); overload; virtual;
    procedure EnableIndex; overload; virtual;
    property List: TFastList read FList write SetList;
    property IndexArray: TIndexArray read FIndexArray write FIndexArray;
    property Text[const TextType: TTextType; const Index: Integer]: string read GetText write SetText;
    property ArrayCount: Integer read GetArrayCount write SetArrayCount;
  end;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  TStringItemList = array of TStringItem;
  TFastListSortCompare = function(List: TFastList; Index1, Index2: Integer): Integer;

  TIndexFlag = (ifDelete, ifEnable, ifIgnore, ifUpdate);
  TIndexFlags = array[TTextType] of set of TIndexFlag;

  TCodeArray = array[TTextType, TIndexFlag] of TIntegerDynArray;

  TFastList = class(TStrings)
  private
    FCapacity: Integer;
    FCaseSensitive: Boolean;
    FCodeArray: TCodeArray;
    FCount: Integer;
    FDuplicates: TDuplicates;
    FHash: THash;
    FIndexFlags: TIndexFlags;
    FIndexTypes: TTextTypes;
    FList: TStringItemList;
    FLock: TRTLCriticalSection;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    {$IFDEF DELPHI_XE5}
    FOwnsObject: Boolean;
    {$ENDIF}
    FSorted: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TFastListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetIndexTypes(const Value: TTextTypes);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
    procedure SetFlag(const TextType: TTextType; const IndexFlag: TIndexFlag); overload; virtual;
    {$IFDEF DELPHI_XE5}
    procedure SetFlag(const TextType: TTextType; const FlagArray: array of TIndexFlag); overload; virtual;
    procedure SetFlag(const TypeArray: array of TTextType; const IndexFlag: TIndexFlag); overload; virtual;
    {$ENDIF}
    procedure SetFlag(const TypeArray: array of TTextType; const FlagArray: array of TIndexFlag); overload; virtual;
    function AddCode(const TextType: TTextType; const IndexFlag: TIndexFlag; const TextCode: Integer): Integer; overload; virtual;
    procedure AddCode(const TextType: TTextType; const IndexFlag: TIndexFlag; const CodeArray: array of Integer); overload; virtual;
    procedure DeleteCodeArray(const TextType: TTextType); overload; virtual;
    procedure DeleteCodeArray(const TypeArray: array of TTextType); overload; virtual;
    procedure DeleteCodeArray; overload; virtual;
    procedure Reset(const TextType: TTextType); virtual;
    procedure UpdateIndex; virtual;
    function IndexOf(const TextType: TTextType; const S: string): Integer; reintroduce; overload; virtual;
    property Hash: THash read FHash write FHash;
    property IndexFlags: TIndexFlags read FIndexFlags write FIndexFlags;
    property CodeArray: TCodeArray read FCodeArray write FCodeArray;
    property Lock: TRTLCriticalSection read FLock write FLock;
  public
    constructor Create; overload; virtual;
    {$IFDEF DELPHI_XE5}
    constructor Create(OwnsObjects: Boolean); overload; virtual;
    {$ENDIF}
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    function CheckIndex(const Index: Integer): Boolean; virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure EndUpdate;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; overload; override;
    function IndexOfName(const Name: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TFastListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property IndexTypes: TTextTypes read FIndexTypes write SetIndexTypes;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    {$IFDEF DELPHI_XE5}
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    {$ENDIF}
  end;

function MakeTextData(const AHashCode, ATextCode: Integer): TTextData;

function Compare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function BinarySearch(const Target: TBindArray; const HashCode: Integer): Integer;
function Search(const Target: TBindArray; const HashCode: Integer): Integer; overload;

function Compare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure Exchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TBindArray);

function MakeBind(const AHashCode, AIndex: Integer): TBind;

function Add(var Target: TBindArray; const HashCode, Index: Integer): Integer; overload;
function Add(var Target: TBindArray; const Value: TBind): Integer; overload;
function Delete(var Target: TBindArray; const Index: Integer): Boolean;

implementation

uses
  Math, MemoryUtils, NumberUtils, RTLConsts, SysUtils, {$IFNDEF DELPHI_7}TextConsts, {$ENDIF}TextUtils,
  ThreadUtils;

function MakeTextData(const AHashCode, ATextCode: Integer): TTextData;
begin
  FillChar(Result, SizeOf(TTextData), 0);
  with Result do
  begin
    HashCode := AHashCode;
    TextCode := ATextCode;
  end;
end;

function Compare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship;
var
  BindArray: TBindArray absolute Target;
begin
  Result := CompareValue(PInteger(Value)^, BindArray[Index].HashCode);
end;

function BinarySearch(const Target: TBindArray; const HashCode: Integer): Integer;
begin
  Result := MemoryUtils.BinarySearch(Target, Low(Target), High(Target), Compare, @HashCode);
end;

function Search(const Target: TBindArray; const HashCode: Integer): Integer;
var
  I: Integer;
begin
  for I := Low(Target) to High(Target) do
    if Target[I].HashCode = HashCode then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function Compare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  BindArray: TBindArray absolute Target;
begin
  Result := CompareValue(BindArray[AIndex].HashCode, BindArray[BIndex].HashCode);
end;

procedure Exchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  Bind: TBind;
  BindArray: TBindArray absolute Target;
begin
  Bind := BindArray[AIndex];
  BindArray[AIndex] := BindArray[BIndex];
  BindArray[BIndex] := Bind;
end;

procedure Sort(var Target: TBindArray);
begin
  QSort(Target, Low(Target), High(Target), Compare, Exchange);
end;

function MakeBind(const AHashCode, AIndex: Integer): TBind;
begin
  FillChar(Result, SizeOf(TBind), 0);
  with Result do
  begin
    HashCode := AHashCode;
    Index := AIndex;
  end;
end;

function Add(var Target: TBindArray; const HashCode, Index: Integer): Integer;
begin
  Result := Add(Target, MakeBind(HashCode, Index));
end;

function Add(var Target: TBindArray; const Value: TBind): Integer;
begin
  Result := Length(Target);
  SetLength(Target, Result + 1);
  Target[Result] := Value;
end;

function Delete(var Target: TBindArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := MemoryUtils.Delete(Target, Index * SizeOf(TBind), SizeOf(TBind), Size * SizeOf(TBind));
  if Result then SetLength(Target, Size - 1);
end;

{ THash }

function THash.Add(const TextType: TTextType; const Index: Integer; const Data: TTextData): Integer;
begin
  Result := FastList.Add(FIndexArray[Data.TextCode].BindArray[TextType], Data.HashCode, Index);
  FIndexArray[Data.TextCode].Live[TextType] := False;
end;

function THash.Add(const TextType: TTextType; const Index: Integer; const S: string; const TextCode: PInteger = nil): Integer;
var
  Data: TTextData;
begin
  if FList.CaseSensitive then
    Data := MakeTextData(S)
  else
    Data := MakeTextData(AnsiLowerCase(S));
  if Assigned(TextCode) then TextCode^ := Data.TextCode;
  Result := Add(TextType, Index, Data);
end;

function THash.Check(const TextType: TTextType; const TextCode, Index: Integer): Boolean;
begin
  Result := (Index >= Low(FIndexArray[TextCode].BindArray[TextType])) and (Index <= High(FIndexArray[TextCode].BindArray[TextType]));
end;

function THash.Check(const TextType: TTextType; const Text: string): Boolean;
begin
  Result := (TextType = ttNameValue) or (TextType = ttName) and (Text <> '');
end;

constructor THash.Create(const AList: TFastList);
begin
  FList := AList;
  SetLength(FIndexArray, DefaultCodeCount);
end;

procedure THash.CreateIndex(const TextType: TTextType);
var
  I: Integer;
  Item: string;
begin
  DeleteIndex(TextType);
  for I := 0 to FList.Count - 1 do
  begin
    Item := Text[TextType, I];
    if Check(TextType, Item) then Add(TextType, I, Item);
  end;
 end;

function THash.Delete(const TextType: TTextType; const TextCode, Index: Integer): Boolean;
begin
  Result := Check(TextType, TextCode, Index) and FastList.Delete(FIndexArray[TextCode].BindArray[TextType], Index);
end;

procedure THash.DeleteIndex(const TextType: TTextType; const TextCode: Integer);
begin
  FIndexArray[TextCode].BindArray[TextType] := nil;
end;

procedure THash.DeleteIndex(const TextType: TTextType);
var
  I: Integer;
begin
  for I := Low(FIndexArray) to High(FIndexArray) do DeleteIndex(TextType, I);
end;

procedure THash.DeleteIndex;
var
  I: TTextType;
begin
  for I := Low(TTextType) to High(TTextType) do DeleteIndex(I);
end;

destructor THash.Destroy;
begin
  DeleteIndex;
  FIndexArray := nil;
  inherited;
end;

procedure THash.EnableIndex(const TextType: TTextType; const TextCode: Integer);
begin
  Sort(FIndexArray[TextCode].BindArray[TextType]);
  FIndexArray[TextCode].Live[TextType] := True;
end;

procedure THash.EnableIndex(const TextType: TTextType);
var
  I: Integer;
begin
  for I := Low(FIndexArray) to High(FIndexArray) do
  begin
    EnableIndex(TextType, I);
    FIndexArray[I].Live[TextType] := True;
  end;
end;

procedure THash.EnableIndex;
var
  I: TTextType;
begin
  for I := Low(TTextType) to High(TTextType) do EnableIndex(I);
end;

function THash.ExactIndexOf(const TextType: TTextType; const TextCode, Index: Integer; const S: string;
  const KnownIndex: Integer): Integer;
var
  HashCode, I: Integer;

  function Match: Boolean;
  begin
    Result := ((KnownIndex < 0) or (KnownIndex = FIndexArray[TextCode].BindArray[TextType, I].Index)) and
      TextUtils.SameText(Text[TextType, FIndexArray[TextCode].BindArray[TextType, I].Index], S);
  end;

begin
  if FIndexArray[TextCode].Live[TextType] and Check(TextType, TextCode, Index) then
  begin
    HashCode := FIndexArray[TextCode].BindArray[TextType, Index].HashCode;
    for I := Index downto Low(FIndexArray[TextCode].BindArray[TextType]) do
    begin
      if HashCode <> FIndexArray[TextCode].BindArray[TextType, I].HashCode then Break;
      if Match then
      begin
        Result := I;
        Exit;
      end;
    end;
    if Index < High(FIndexArray[TextCode].BindArray[TextType]) then for I := Index + 1 to High(FIndexArray[TextCode].BindArray[TextType]) do
    begin
      if HashCode <> FIndexArray[TextCode].BindArray[TextType, I].HashCode then Break;
      if Match then
      begin
        Result := I;
        Exit;
      end;
    end;
    Result := -1;
  end
  else Result := -1;
end;

function THash.Find(const TextType: TTextType; const S: string; out TextCode, Index: Integer; const KnownIndex: Integer): Boolean;
begin
  Index := IndexOf(TextType, S, TextCode, KnownIndex);
  Result := Index >= 0;
end;

function THash.GetArrayCount: Integer;
begin
  Result := Length(FIndexArray);
end;

function THash.GetText(const TextType: TTextType; const Index: Integer): string;
begin
  case TextType of
    ttName: Result := FList.Names[Index];
  else
    Result := FList[Index];
  end;
  if not FList.CaseSensitive then Result := AnsiLowerCase(Result);
end;

function THash.IndexOf(const TextType: TTextType; const Data: TTextData; const S: string; const KnownIndex: Integer;
  const Recurse: Boolean): Integer;
begin
  if FIndexArray[Data.TextCode].Live[TextType] then
  begin
    Result := RoughIndexOf(TextType, Data);
    if Result >= 0 then
    begin
      Result := ExactIndexOf(TextType, Data.TextCode, Result, S, KnownIndex);
      if (Result < 0) and Recurse then
      begin
        CreateIndex(TextType);
        EnableIndex(TextType);
        Result := IndexOf(TextType, Data, S, KnownIndex, False);
      end;
    end;
  end
  else Result := Search(TextType, Data.TextCode, S);
end;

function THash.IndexOf(const TextType: TTextType; const S: string; out TextCode: Integer; const KnownIndex: Integer): Integer;
var
  HashCode: Integer;
begin
  if FList.CaseSensitive then
    HashCode := MakeHashCode(S)
  else
    HashCode := MakeHashCode(AnsiLowerCase(S));
  TextCode := MakeTextCode(HashCode);
  if FList.CaseSensitive then
    Result := IndexOf(TextType, FastList.MakeTextData(HashCode, TextCode), S, KnownIndex)
  else
    Result := IndexOf(TextType, FastList.MakeTextData(HashCode, TextCode), AnsiLowerCase(S), KnownIndex);
end;

function THash.MakeHashCode(const S: string): Integer;
begin
  Result := NumberUtils.GetHashCode(S);
end;

function THash.MakeTextCode(const HashCode: Integer): Integer;
begin
  Result := EnsureRange(Abs(HashCode) mod Length(FIndexArray), Low(FIndexArray), High(FIndexArray));
end;

function THash.MakeTextData(const S: string): TTextData;
begin
  FillChar(Result, SizeOf(TTextData), 0);
  Result.HashCode := MakeHashCode(S);
  Result.TextCode := MakeTextCode(Result.HashCode);
end;

function THash.Put(const TextType: TTextType; const FromText, ToText: string; const KnownIndex: Integer;
  const FromTextCode, ToTextCode: PInteger): Boolean;
var
  I, J: Integer;
  Bind: TBind;
begin
  Result := Find(TextType, FromText, I, J, KnownIndex);
  if Result then
  begin
    if Assigned(FromTextCode) then FromTextCode^ := I;
    Bind := FIndexArray[I].BindArray[TextType, J];
    FastList.Delete(FIndexArray[I].BindArray[TextType], J);
    FIndexArray[I].Live[TextType] := False;
    if FList.CaseSensitive then
      Bind.HashCode := MakeHashCode(ToText)
    else
      Bind.HashCode := MakeHashCode(AnsiLowerCase(ToText));
    I := MakeTextCode(Bind.HashCode);
    FastList.Add(FIndexArray[I].BindArray[TextType], Bind);
    FIndexArray[I].Live[TextType] := False;
    if Assigned(ToTextCode) then ToTextCode^ := I;
  end;
end;

function THash.RoughIndexOf(const TextType: TTextType; const Data: TTextData): Integer;
begin
  if FIndexArray[Data.TextCode].Live[TextType] then
    Result := BinarySearch(FIndexArray[Data.TextCode].BindArray[TextType], Data.HashCode)
  else
    Result := FastList.Search(FIndexArray[Data.TextCode].BindArray[TextType], Data.HashCode);
end;

function THash.Search(const TextType: TTextType; const TextCode: Integer; const S: string): Integer;
var
  I: Integer;
begin
  for I := Low(FIndexArray[TextCode].BindArray[TextType]) to High(FIndexArray[TextCode].BindArray[TextType]) do
    if TextUtils.SameText(Text[TextType, FIndexArray[TextCode].BindArray[TextType, I].Index], S) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure THash.SetArrayCount(const Value: Integer);
begin
  if (Value > 0) and (Value <> Length(FIndexArray)) then
  begin
    DeleteIndex;
    SetLength(FIndexArray, Value);
  end;
end;

procedure THash.SetList(const Value: TFastList);
begin
  FList := Value;
  DeleteIndex;
end;

procedure THash.SetText(const TextType: TTextType; const Index: Integer; const Value: string);
begin
  case TextType of
    ttName:
      if Value <> '' then
        FList[Index] := Value + {$IFDEF DELPHI_7}FList.NameValueSeparator{$ELSE}Equal{$ENDIF} + {$IFDEF DELPHI_7}FList.ValueFromIndex[Index]{$ELSE}GetValueFromIndex(FList, Index){$ENDIF};
  else
    FList[Index] := Value;
  end;
end;

procedure THash.Synchronize(const TextType: TTextType; const Index: Integer; const OffsetType: TOffsetType);
var
  I, J, K, Count, Total: Integer;
  Flag: Boolean;
begin
  Count := 0;
  case OffsetType of
    otNegative:
    begin
      K := -1;
      Total := FList.Count - Index - 1;
    end;
  else
    K := 1;
    Total := FList.Count - Index;
  end;
  for I := Low(FIndexArray) to High(FIndexArray) do
    for J := Low(FIndexArray[I].BindArray[TextType]) to High(FIndexArray[I].BindArray[TextType]) do
    begin
      case OffsetType of
        otNegative: Flag := FIndexArray[I].BindArray[TextType, J].Index >= Index + 1;
      else
        Flag := FIndexArray[I].BindArray[TextType, J].Index >= Index;
      end;
      if Flag then
      begin
        Inc(FIndexArray[I].BindArray[TextType, J].Index, K);
        Inc(Count);
        if Count >= Total then Exit;
      end;
    end;
end;

{$IFDEF DELPHI_XE5}
procedure THash.Synchronize(const TypeArray: array of TTextType; const Index: Integer; const OffsetType: TOffsetType);
var
  I: Integer;
begin
  for I := Low(TypeArray) to High(TypeArray) do Synchronize(TypeArray[I], Index, OffsetType);
end;

procedure THash.Synchronize(const Index: Integer; const OffsetType: TOffsetType);
var
  I: TTextType;
begin
  for I := Low(TTextType) to High(TTextType) do Synchronize(I, Index, OffsetType);
end;
{$ENDIF}

{ TFastList }

function TFastList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

function TFastList.AddCode(const TextType: TTextType; const IndexFlag: TIndexFlag; const TextCode: Integer): Integer;
begin
  if (IndexFlag < Low(FCodeArray[TextType])) or (IndexFlag > High(FCodeArray[TextType])) then
    Result := -1
  else
    Result := MemoryUtils.Add(FCodeArray[TextType, IndexFlag], TextCode);
end;

procedure TFastList.AddCode(const TextType: TTextType; const IndexFlag: TIndexFlag; const CodeArray: array of Integer);
var
  I: Integer;
begin
  for I := Low(CodeArray) to High(CodeArray) do AddCode(TextType, IndexFlag, CodeArray[I]);
end;

function TFastList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Enter(FLock);
  try
    if not Sorted then
      Result := FCount
    else
      if Find(S, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
    InsertItem(Result, S, AObject);
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.AddStrings(Strings: TStrings);
begin
  Enter(FLock);
  try
    SetFlag([ttNameValue, ttName], [ifIgnore, ifUpdate]);
    DeleteCodeArray;
    inherited;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.Assign(Source: TPersistent);
begin
  Enter(FLock);
  try
    SetFlag([ttNameValue, ttName], [ifIgnore, ifUpdate]);
    DeleteCodeArray;
    inherited;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.BeginUpdate;
begin
  inherited;
  SetFlag([ttNameValue, ttName], [ifIgnore, ifUpdate]);
  DeleteCodeArray;
end;

procedure TFastList.Changed;
begin
  if UpdateCount = 0 then
  begin
    UpdateIndex;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TFastList.Changing;
begin
  if (UpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

function TFastList.CheckIndex(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
end;

procedure TFastList.Clear;
{$IFDEF DELPHI_XE5}
var
  I: Integer;
  Temp: TArray<TObject>;
{$ENDIF}
begin
  Enter(FLock);
  try
    if FCount <> 0 then
    begin
      Changing;
      {$IFDEF DELPHI_XE5}
      // If the list owns the Objects gather them and free after the list is disposed
      if OwnsObjects then
      begin
        SetLength(Temp, FCount);
        for I := 0 to FCount - 1 do Temp[I] := FList[I].FObject;
      end;
      {$ENDIF}
      FCount := 0;
      SetCapacity(0);
      {$IFDEF DELPHI_XE5}
      // Free the objects that were owned by the list
      if Length(Temp) > 0 then
        for I := 0 to Length(Temp) - 1 do Temp[I].Free;
      {$ENDIF}
      {$IFDEF DELPHI_XE5}
      SetFlag([ttNameValue, ttName], ifDelete);
      {$ELSE}
      SetFlag([ttNameValue, ttName], [ifDelete]);
      {$ENDIF}
      DeleteCodeArray;
      Changed;
    end;
  finally
    Leave(FLock);
  end;
end;

function TFastList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;

constructor TFastList.Create;
begin
  inherited;
  InitializeCriticalSection(FLock);
  FHash := THash.Create(Self);
  FIndexTypes := [ttNameValue];
end;

{$IFDEF DELPHI_XE5}
constructor TFastList.Create(OwnsObjects: Boolean);
begin
  Create;
  FOwnsObject := OwnsObjects;
end;
{$ENDIF}

procedure TFastList.CustomSort(Compare: TFastListSortCompare);
begin
  Enter(FLock);
  try
    if not Sorted and (FCount > 1) then
    begin
      Changing;
      QuickSort(0, FCount - 1, Compare);
      {$IFDEF DELPHI_XE5}
      SetFlag([ttNameValue, ttName], ifUpdate);
      {$ELSE}
      SetFlag([ttNameValue, ttName], [ifUpdate]);
      {$ENDIF}
      DeleteCodeArray;
      Changed;
    end;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.Delete(Index: Integer);
var
{$IFDEF DELPHI_XE5}
  Obj: TObject;
{$ENDIF}
  I: TTextType;
  J, K: Integer;
  Item: string;
begin
  Enter(FLock);
  try
    if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
    Changing;
    for I := Low(TTextType) to High(TTextType) do
      if I in FIndexTypes then
      begin
        case I of
          ttNameValue: Item := Strings[Index];
        else
          Item := Names[Index];
        end;
        if FHash.Check(I, Item) and (not FHash.Find(I, Item, J, K, Index) or not FHash.Delete(I, J, K)) then Reset(I);
        if (Index < Count - 1) then FHash.Synchronize(I, Index, otNegative);
      end;
    {$IFDEF DELPHI_XE5}
    // If this list owns its objects then free the associated TObject with this index
    if OwnsObjects then
      Obj := FList[Index].FObject
    else
      Obj := nil;
    {$ENDIF}
    // Direct memory writing to managed array follows
    //  see http://dn.embarcadero.com/article/33423
    // Explicitly finalize the element we about to stomp on with move
    Finalize(FList[Index]);
    Dec(FCount);
    if Index < FCount then
    begin
      System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TStringItem));
      // Make sure there is no danglng pointer in the last (now unused) element
      PPointer(@FList[FCount].FString)^ := nil;
      PPointer(@FList[FCount].FObject)^ := nil;
    end;
    {$IFDEF DELPHI_XE5}
    if Obj <> nil then Obj.Free;
    {$ENDIF}
    Changed;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.DeleteCodeArray(const TextType: TTextType);
var
  I: TIndexFlag;
begin
  for I := Low(FCodeArray[TextType]) to High(FCodeArray[TextType]) do
    FCodeArray[TextType, I] := nil;
end;

procedure TFastList.DeleteCodeArray(const TypeArray: array of TTextType);
var
  I: Integer;
begin
  for I := Low(TypeArray) to High(TypeArray) do DeleteCodeArray(TypeArray[I]);
end;

procedure TFastList.DeleteCodeArray;
var
  I: TTextType;
begin
  for I := Low(TTextType) to High(TTextType) do DeleteCodeArray(I);
end;

destructor TFastList.Destroy;
{$IFDEF DELPHI_XE5}
var
  I: Integer;
  Temp: TArray<TObject>;
{$ENDIF}
begin
  FOnChange := nil;
  FOnChanging := nil;
  {$IFDEF DELPHI_XE5}
  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do Temp[I] := FList[I].FObject;
  end;
  {$ENDIF}
  inherited;
  FCount := 0;
  SetCapacity(0);
  {$IFDEF DELPHI_XE5}
  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do Temp[I].DisposeOf;
  {$ENDIF}
  FHash.Free;
  DeleteCriticalSection(FLock);
  DeleteCodeArray;
end;

procedure TFastList.EndUpdate;
begin
  inherited;
  UpdateIndex;
end;

procedure TFastList.Exchange(Index1, Index2: Integer);
begin
  Enter(FLock);
  try
    if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
    if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
    Changing;
    ExchangeItems(Index1, Index2);
    {$IFDEF DELPHI_XE5}
    SetFlag([ttNameValue, ttName], ifUpdate);
    {$ELSE}
    SetFlag([ttNameValue, ttName], [ifUpdate]);
    {$ENDIF}
    DeleteCodeArray;
    Changed;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PStringItem;
begin
  Enter(FLock);
  try
    Item1 := @FList[Index1];
    Item2 := @FList[Index2];
    Temp := Pointer(Item1^.FString);
    Pointer(Item1^.FString) := Pointer(Item2^.FString);
    Pointer(Item2^.FString) := Temp;
    Temp := Pointer(Item1^.FObject);
    Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
    Pointer(Item2^.FObject) := Temp;
  finally
    Leave(FLock);
  end;
end;

function TFastList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Enter(FLock);
  try
    Result := False;
    L := 0;
    H := FCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompareStrings(FList[I].FString, S);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          if Duplicates <> dupAccept then L := I;
        end;
      end;
    end;
    Index := L;
  finally
    Leave(FLock);
  end;
end;

function TFastList.Get(Index: Integer): string;
begin
  if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
  Result := FList[Index].FString;
end;

function TFastList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TFastList.GetCount: Integer;
begin
  Result := FCount;
end;

function TFastList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TFastList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TFastList.IndexOf(const S: string): Integer;
begin
  Enter(FLock);
  try
    if ttNameValue in FIndexTypes then Result := IndexOf(ttNameValue, S)
    else
      if not Sorted then
        Result := inherited IndexOf(S)
      else
        if not Find(S, Result) then Result := -1;
  finally
    Leave(FLock);
  end;
end;

function TFastList.IndexOf(const TextType: TTextType; const S: string): Integer;
var
  TextCode: Integer;
begin
  case TextType of
    ttNameValue:
      if FHash.Find(ttNameValue, S, TextCode, Result) then
        Result := FHash.IndexArray[TextCode].BindArray[ttNameValue, Result].Index
      else
        Result := -1;
    ttName:
      if FHash.Find(ttName, S, TextCode, Result) then
        Result := FHash.IndexArray[TextCode].BindArray[ttName, Result].Index
      else
        Result := -1;
  else
    Result := -1;
  end;
end;

function TFastList.IndexOfName(const Name: string): Integer;
begin
  Enter(FLock);
  try
    if ttName in FIndexTypes then
      Result := IndexOf(ttName, Name)
    else
      Result := inherited IndexOfName(Name);
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

procedure TFastList.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  I: TTextType;
  J: Integer;
  Item: string;
begin
  Enter(FLock);
  try
    Changing;
    if FCount = FCapacity then Grow;
    if Index < FCount then
      System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TStringItem));
    Pointer(FList[Index].FString) := nil;
    Pointer(FList[Index].FObject) := nil;
    FList[Index].FObject := AObject;
    FList[Index].FString := S;
    Inc(FCount);
    for I := Low(TTextType) to High(TTextType) do
      if (I in FIndexTypes) and not (ifIgnore in FIndexFlags[I]) then
      begin
        if (Index < Count - 1) then FHash.Synchronize(I, Index, otPositive);
        case I of
          ttNameValue: Item := S;
        else
          Item := ExtractName(S);
        end;
        if FHash.Check(I, Item) then
        begin
          FHash.Add(I, Index, Item, @J);
          SetFlag(I, ifEnable);
          AddCode(I, ifEnable, J);
        end;
      end;
    Changed;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  Enter(FLock);
  try
    if Sorted then Error(@SSortedListError, 0);
    if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
    InsertItem(Index, S, AObject);
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.LoadFromFile(const FileName: string);
begin
  Enter(FLock);
  try
    SetFlag([ttNameValue, ttName], [ifIgnore, ifUpdate]);
    DeleteCodeArray;
    inherited;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.LoadFromStream(Stream: TStream);
begin
  Enter(FLock);
  try
    SetFlag([ttNameValue, ttName], [ifIgnore, ifUpdate]);
    DeleteCodeArray;
    inherited;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.Put(Index: Integer; const S: string);
var
  FromItem, ToItem: string;
  I: TTextType;
  J, K: Integer;
begin
  Enter(FLock);
  try
    if Sorted then Error(@SSortedListError, 0);
    if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
    Changing;
    for I := Low(TTextType) to High(TTextType) do
      if I in FIndexTypes then
      begin
        case I of
          ttNameValue:
          begin
            FromItem := Strings[Index];
            ToItem := S;
          end;
        else
          FromItem := Names[Index];
          ToItem := ExtractName(S);
        end;
        if FHash.Check(I, ToItem) then
          if FHash.Check(I, FromItem) then
            if FHash.Put(I, FromItem, ToItem, Index, @J, @K) then
            begin
              SetFlag(I, ifEnable);
              AddCode(I, ifEnable, [J, K]);
            end
            else Reset(I)
          else begin
            FHash.Add(I, Index, ToItem, @J);
            SetFlag(I, ifEnable);
            AddCode(I, ifEnable, J);
          end
        else
          if FHash.Check(I, FromItem) and (not FHash.Find(I, FromItem, J, K, Index) or not FHash.Delete(I, J, K)) then Reset(I)
      end;
    FList[Index].FString := S;
    Changed;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.PutObject(Index: Integer; AObject: TObject);
begin
  Enter(FLock);
  try
    if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
    Changing;
    FList[Index].FObject := AObject;
    Changed;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.QuickSort(L, R: Integer; SCompare: TFastListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TFastList.Reset(const TextType: TTextType);
begin
  SetFlag(TextType, ifUpdate);
  DeleteCodeArray(TextType);
end;

procedure TFastList.SetCapacity(NewCapacity: Integer);
begin
  Enter(FLock);
  try
    if NewCapacity < FCount then Error(@SListCapacityError, NewCapacity);
    if NewCapacity <> FCapacity then
    begin
      SetLength(FList, NewCapacity);
      FCapacity := NewCapacity;
    end;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.SetCaseSensitive(const Value: Boolean);
begin
  Enter(FLock);
  try
    if Value <> FCaseSensitive then
    begin
      FCaseSensitive := Value;
      if Sorted then
      begin
        // Calling Sort won't sort the list because CustomSort will
        // only sort the list if it's not already sorted
        Sorted := False;
        Sorted := True;
      end;
      {$IFDEF DELPHI_XE5}
      SetFlag([ttNameValue, ttName], ifUpdate);
      {$ELSE}
      SetFlag([ttNameValue, ttName], [ifUpdate]);
      {$ENDIF}
      DeleteCodeArray;
      UpdateIndex;
    end;
  finally
    Leave(FLock);
  end;
end;

{$IFDEF DELPHI_XE5}
procedure TFastList.SetFlag(const TypeArray: array of TTextType; const IndexFlag: TIndexFlag);
var
  I: Integer;
begin
  for I := Low(TypeArray) to High(TypeArray) do SetFlag(TypeArray[I], IndexFlag);
end;
{$ENDIF}

procedure TFastList.SetFlag(const TextType: TTextType; const IndexFlag: TIndexFlag);
begin
  if TextType in FIndexTypes then Include(FIndexFlags[TextType], IndexFlag);
end;

procedure TFastList.SetFlag(const TypeArray: array of TTextType; const FlagArray: array of TIndexFlag);
var
  I, J: Integer;
begin
  for I := Low(TypeArray) to High(TypeArray) do for J := Low(FlagArray) to High(FlagArray) do
    SetFlag(TypeArray[I], FlagArray[J]);
end;

{$IFDEF DELPHI_XE5}
procedure TFastList.SetFlag(const TextType: TTextType; const FlagArray: array of TIndexFlag);
var
  I: Integer;
begin
  for I := Low(FlagArray) to High(FlagArray) do SetFlag(TextType, FlagArray[I]);
end;
{$ENDIF}

procedure TFastList.SetIndexTypes(const Value: TTextTypes);
begin
  Enter(FLock);
  try
    if FIndexTypes <> Value then
    begin
      FIndexTypes := Value;
      {$IFDEF DELPHI_XE5}
      SetFlag([ttNameValue, ttName], ifUpdate);
      {$ELSE}
      SetFlag([ttNameValue, ttName], [ifUpdate]);
      {$ENDIF}
      DeleteCodeArray;
      UpdateIndex;
    end;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.SetSorted(Value: Boolean);
begin
  Enter(FLock);
  try
    if FSorted <> Value then
    begin
      if Value then Sort;
      FSorted := Value;
    end;
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.SetUpdateState(Updating: Boolean);
begin
  Enter(FLock);
  try
    if Updating then Changing else Changed;
  finally
    Leave(FLock);
  end;
end;

function FastListCompareStrings(List: TFastList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString, List.FList[Index2].FString);
end;

procedure TFastList.Sort;
begin
  Enter(FLock);
  try
    CustomSort(FastListCompareStrings);
  finally
    Leave(FLock);
  end;
end;

procedure TFastList.UpdateIndex;
var
  I: TTextType;
  J: Integer;
begin
  for I := Low(TTextType) to High(TTextType) do if (I in FIndexTypes) and (FIndexFlags[I] <> []) then
  begin
    if FIndexFlags[I] = [ifDelete] then
      if Assigned(FCodeArray[I, ifDelete]) then
        for J := Low(FCodeArray[I, ifDelete]) to High(FCodeArray[I, ifDelete]) do FHash.DeleteIndex(I, FCodeArray[I, ifDelete, J])
      else
        FHash.DeleteIndex(I)
    else if FIndexFlags[I] = [ifEnable] then
      if Assigned(FCodeArray[I, ifEnable]) then
        for J := Low(FCodeArray[I, ifEnable]) to High(FCodeArray[I, ifEnable]) do FHash.EnableIndex(I, FCodeArray[I, ifEnable, J])
      else
        FHash.EnableIndex(I)
    else begin
      FHash.CreateIndex(I);
      FHash.EnableIndex(I);
    end;
    FIndexFlags[I] := [];
    DeleteCodeArray(I);
  end;
end;

end.
