{ *********************************************************************** }
{                                                                         }
{ TextList                                                                }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit TextList;

{$B-}
{$I Directives.inc}

interface

uses
  Windows, SysUtils, Classes, DB, DBClient, IniFiles, MidasLib, Types, TextConsts;

type
  EDataListError = class(Exception);

  TFieldType = (ftAText, ftBText, ftAData, ftBData);
  TFieldData = record
    Name: string;
    DataType: DB.TFieldType;
    Size: Integer;
  end;
  TIndexType = (itAText, itBText, itATextBText, itBData);
  TIndexData = record
    FieldName, Name: string;
    Options: TIndexOptions;
  end;

  TDataList = class
  private
    FDataSet: TClientDataSet;
    FUpdateCount: Integer;
    FDataMode: TBlobStreamMode;
    FMaxCount: Integer;
    FBTextField: TField;
    FATextField: TField;
    FIndex: Integer;
    FBDataField: TField;
    FADataField: TBlobField;
    FData: TClientBlobStream;
    function GetAData: TByteDynArray;
    function GetAItem(Index: Integer): string;
    function GetAItemData(Index: Integer): TByteDynArray;
    function GetATextSize: Integer;
    function GetBData: Integer;
    function GetBItem(Index: Integer): string;
    function GetBItemData(Index: Integer): Integer;
    function GetBTextSize: Integer;
    function GetCount: Integer;
    function GetRecNo: Integer;
    function GetStream(Mode: TBlobStreamMode): TClientBlobStream;
    procedure SetAData(const Value: TByteDynArray);
    procedure SetAItem(Index: Integer; const Value: string);
    procedure SetAItemData(Index: Integer; const Value: TByteDynArray);
    procedure SetBData(const Value: Integer);
    procedure SetBItem(Index: Integer; const Value: string);
    procedure SetBItemData(Index: Integer; const Value: Integer);
    procedure SetRecNo(const Value: Integer);
  protected
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
    function Find(IndexType: TIndexType; const Value: array of const): Boolean; virtual;
    procedure Write(const AText, BText: string; const AData: TByteDynArray; BData: Integer); virtual;
    function Next: Integer; virtual;
    property DataSet: TClientDataSet read FDataSet write FDataSet;
    property AData: TByteDynArray read GetAData write SetAData;
    property BData: Integer read GetBData write SetBData;
    property ATextField: TField read FATextField write FATextField;
    property BTextField: TField read FBTextField write FBTextField;
    property ADataField: TBlobField read FADataField write FADataField;
    property BDataField: TField read FBDataField write FBDataField;
    property Data: TClientBlobStream read FData write FData;
    property DataMode: TBlobStreamMode read FDataMode write FDataMode;
    property Index: Integer read FIndex write FIndex;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string; Format: TDataPacketFormat = dfBinary); virtual;
    function Add(const AText, BText: string; const AData: TByteDynArray;
      BData: Integer): Boolean; overload; virtual;
    function Add(const Text: string): Boolean; overload; virtual;
    function Add(const Text: string; const AData: TByteDynArray): Boolean; overload; virtual;
    function Add(const Text: string; const AData: Integer): Boolean; overload; virtual;
    procedure Put(Index: Integer; const AText, BText: string; const AData: TByteDynArray;
      BData: Integer); overload; virtual;
    procedure Put(Index: Integer; const Text: string); overload; virtual;
    procedure Put(Index: Integer; const Text: string; const AData: TByteDynArray); overload; virtual;
    procedure Put(Index: Integer; const Text: string; const AData: Integer); overload; virtual;
    function FindData(IndexType: TIndexType; const Value: array of const): TByteDynArray; virtual;
    function IndexOf(IndexType: TIndexType; const Value: array of const): Integer; virtual;
    procedure Clear; virtual;
    procedure Apply; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property ATextSize: Integer read GetATextSize;
    property BTextSize: Integer read GetBTextSize;
    property Count: Integer read GetCount;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property UpdateCount: Integer read FUpdateCount;
    property AItem[Index: Integer]: string read GetAItem write SetAItem;
    property BItem[Index: Integer]: string read GetBItem write SetBItem;
    property AItemData[Index: Integer]: TByteDynArray read GetAItemData write SetAItemData;
    property BItemData[Index: Integer]: Integer read GetBItemData write SetBItemData;
    property Stream[Mode: TBlobStreamMode]: TClientBlobStream read GetStream;
  end;

  EListError = class(Exception);

  TTextList = class
  private
    FTextDelimiter: string;
    FDelimiter: string;
    FLineBreak: string;
    FLock: PRTLCriticalSection;
    FList: TDataList;
    function GetAText(Index: Integer): string;
    function GetBText(Text: string): string;
    function GetBTextFromIndex(Index: Integer): string;
    function GetCount: Integer;
    function GetData(Index: Integer): TObject;
    function GetItem(Index: Integer): string;
    function GetText: string;
    procedure SetAText(Index: Integer; const Value: string);
    procedure SetBText(Text: string; const Value: string);
    procedure SetBTextFromIndex(Index: Integer; const Value: string);
    procedure SetData(Index: Integer; const Value: TObject);
    procedure SetDelimiter(const Value: string);
    procedure SetItem(Index: Integer; const Value: string);
    procedure SetLineBreak(const Value: string);
    procedure SetText(const Value: string);
  protected
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
    function ExtractAText(const Text: string): string; virtual;
    function ExtractBText(const Text: string): string; virtual;
    property List: TDataList read FList write FList;
    property Lock: PRTLCriticalSection read FLock write FLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Text: string): Integer; overload; virtual;
    function Add(const Text: string; AData: TObject): Integer; overload; virtual;
    procedure Add(List: TTextList); overload; virtual;
    procedure Put(Index: Integer; const Text: string); overload; virtual;
    procedure Put(Index: Integer; const Text: string; AData: TObject); overload; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    function IndexOf(const Text: string; IndexType: TIndexType = itATextBText): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    property Count: Integer read GetCount;
    property Delimiter: string read FDelimiter write SetDelimiter;
    property TextDelimiter: string read FTextDelimiter write FTextDelimiter;
    property LineBreak: string read FLineBreak write SetLineBreak;
    property AText[Index: Integer]: string read GetAText write SetAText;
    property BText[AText: string]: string read GetBText write SetBText;
    property BTextFromIndex[Index: Integer]: string read GetBTextFromIndex
      write SetBTextFromIndex;
    property Data[Index: Integer]: TObject read GetData write SetData;
    property Item[Index: Integer]: string read GetItem write SetItem; default;
    property Text: string read GetText write SetText;
  end;

  TFastStringList = class(TStrings)
  private
    FList: TTextList;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetDelimiter: string; virtual;
    function GetLineBreak: string; virtual;
    function GetName(Index: Integer): string; virtual;
    function GetNameValueSeparator: string; virtual;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: string; override;
    function GetUpdateCount: Integer; virtual;
    function GetValue(const Name: string): string; virtual;
    function GetValueFromIndex(Index: Integer): string; virtual;
    procedure Put(Index: Integer; const Value: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetDelimiter(const Value: string); virtual;
    procedure SetLineBreak(const Value: string); virtual;
    procedure SetNameValueSeparator(const Value: string);
    procedure SetTextStr(const Value: string); override;
    procedure SetValue(const Name, Value: string); virtual;
    procedure SetValueFromIndex(Index: Integer; const Value: string); virtual;
    property List: TTextList read FList write FList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure EndUpdate; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
    function IndexOfObject(AObject: TObject): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure SaveToFile(const FileName: string); override;
    property UpdateCount: Integer read GetUpdateCount;
    property Count: Integer read GetCount;
    property Delimiter: string read GetDelimiter write SetDelimiter;
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: string read GetNameValueSeparator write SetNameValueSeparator;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
  end;

const
  DefaultDeleteCount = 0;
  DefaultMaxCount = 1000;
  FieldData: array[TFieldType] of TFieldData = (
    (Name: 'AText'; DataType: ftString; Size: 512), (Name: 'BText'; DataType: ftString; Size: 512),
    (Name: 'AData'; DataType: ftBlob), (Name: 'BData'; DataType: ftInteger));
  IndexData: array[TIndexType] of TIndexData = (
    (FieldName: 'AText'; Name: 'ATextIndex'; Options: [ixUnique, ixCaseInsensitive]),
    (FieldName: 'BText'; Name: 'BTextIndex'; Options: [ixCaseInsensitive]),
    (FieldName: 'AText;BText'; Name: 'ATextBTextIndex'; Options: [ixCaseInsensitive]),
    (FieldName: 'BData'; Name: 'BDataIndex'; Options: []));
  ValueError = 'Invalid value for field "%s"';
  DefaultDelimiter = Semicolon;
  DefaultTextDelimiter = Equal;
  DefaultLineBreak = sLineBreak;

implementation

uses
  DSIntf, FileUtils, NumberConsts, RTLConsts, TextBuilder, TextUtils, ThreadUtils,
  Variants;

{ TDataList }

function TDataList.Add(const Text: string): Boolean;
begin
  Result := Add(Text, '', nil, 0);
end;

function TDataList.Add(const AText, BText: string; const AData: TByteDynArray;
  BData: Integer): Boolean;
begin
  Result := not Find(itAText, [AText]);
  if Result then
    if Count < FMaxCount then
    begin
      FDataSet.Append;
      Write(AText, BText, AData, BData);
      FDataSet.Post;
    end
    else begin
      FDataSet.RecNo := FIndex;
      FDataSet.Edit;
      Write(AText, BText, AData, BData);
      FDataSet.Post;
    end;
end;

function TDataList.Add(const Text: string; const AData: Integer): Boolean;
begin
  Result := Add(Text, '', nil, AData);
end;

function TDataList.Add(const Text: string; const AData: TByteDynArray): Boolean;
begin
  Result := Add(Text, '', AData, 0);
end;

procedure TDataList.Apply;
begin
  FreeAndNil(FData);
end;

procedure TDataList.BeginUpdate;
begin
  if FUpdateCount = 0 then FDataSet.IndexName := szDEFAULT_ORDER;
  Inc(FUpdateCount);
end;

procedure TDataList.Clear;
begin
  FreeAndNil(FData);
  if FDataSet.RecordCount > 0 then FDataSet.EmptyDataSet;
end;

constructor TDataList.Create;
var
  I: TFieldType;
  FieldDef: TFieldDef;
  J: TIndexType;
  IndexDef: TIndexDef;
begin
  FDataSet := TClientDataSet.Create(nil);
  for I := Low(TFieldType) to High(TFieldType) do
  begin
    FieldDef := FDataSet.FieldDefs.AddFieldDef;
    with FieldDef do
    begin
      Name := FieldData[I].Name;
      DataType := FieldData[I].DataType;
      Size := FieldData[I].Size;
    end;
  end;
  for J := Low(TIndexType) to High(TIndexType) do
  begin
    IndexDef := FDataSet.IndexDefs.AddIndexDef;
    with IndexDef do
    begin
      Fields := IndexData[J].FieldName;
      Name := IndexData[J].Name;
      Options := IndexData[J].Options;
    end;
  end;
  with FDataSet do
  begin
    CreateDataSet;
    LogChanges := False;
    FATextField := FieldByName(FieldData[ftAText].Name);
    FBTextField := FieldByName(FieldData[ftBText].Name);
    FADataField := TBlobField(FieldByName(FieldData[ftAData].Name));
    FBDataField := FieldByName(FieldData[ftBData].Name);
  end;
  FMaxCount := DefaultMaxCount;
end;

destructor TDataList.Destroy;
begin
  FreeAndNil(FData);
  FDataSet.Free;
  inherited;
end;

procedure TDataList.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TDataList.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TDataList.Error(const Message: string;
  const Arguments: array of const): Exception;
begin
  Result := EDataListError.CreateFmt(Message, Arguments);
end;

function TDataList.Find(IndexType: TIndexType; const Value: array of const): Boolean;
var
  Data: Variant;
  I, J: Integer;
  Options: TLocateOptions;
begin
  Result := FDataSet.RecordCount > 0;
  if Result then
    if FUpdateCount = 0 then
    begin
      I := FDataSet.FieldCount - 1;
      J := High(Value);
      if I < J then J := I;
      FDataSet.IndexName := IndexData[IndexType].Name;
      FDataSet.SetKey;
      for I := 0 to J do FDataSet.Fields[I].AssignValue(Value[I]);
      Result := FDataSet.GotoKey;
    end
    else begin
      Data := VarArrayCreate([Low(Value), High(Value)], varVariant);
      for I := Low(Value) to High(Value) do
        with Value[I] do
          case VType of
            vtInteger:
              Data[I] := VInteger;
            vtBoolean:
              Data[I] := VBoolean;
            vtChar:
              Data[I] := VChar;
            vtWideChar:
              Data[I] := VWideChar;
            vtExtended:
              Data[I] := VExtended^;
            vtString:
              Data[I] := string(VString^);
            vtPChar:
              Data[I] := string(VPChar);
            vtAnsiString:
              Data[I] := AnsiString(VAnsiString);
            vtCurrency:
              Data[I] := VCurrency^;
            vtVariant:
              if not VarIsClear(VVariant^) then Data[I] := VVariant^;
            vtWideString:
              Data[I] := WideString(VWideString);
            vtInt64:
              Data[I] := VInt64^;
            {$IFDEF UNICODE}
            vtUnicodeString:
              Data[I] := string(VUnicodeString);
            {$ENDIF}
          else raise Error(ValueError, [IndexData[IndexType].FieldName]);
          end;
      Options := [];
      if ixCaseInsensitive in IndexData[IndexType].Options then
        Include(Options, loCaseInsensitive);
      Result := FDataSet.Locate(IndexData[IndexType].FieldName, Data, Options);
    end;
end;

function TDataList.FindData(IndexType: TIndexType;
  const Value: array of const): TByteDynArray;
begin
  if Find(IndexType, Value) then Result := AData
  else Result := nil;
end;

function TDataList.GetAData: TByteDynArray;
begin
  SetLength(Result, Stream[bmRead].Size);
  Stream[bmRead].Position := 0;
  Stream[bmRead].Read(Pointer(Result)^, Stream[bmRead].Size);
  Apply;
end;

function TDataList.GetAItem(Index: Integer): string;
begin
  RecNo := Index;
  Result := FATextField.AsString;
end;

function TDataList.GetAItemData(Index: Integer): TByteDynArray;
begin
  RecNo := Index;
  Result := AData;
end;

function TDataList.GetATextSize: Integer;
begin
  Result := FATextField.Size;
end;

function TDataList.GetBData: Integer;
begin
  Result := FBDataField.AsInteger;
end;

function TDataList.GetBItem(Index: Integer): string;
begin
  RecNo := Index;
  Result := FBTextField.AsString;
end;

function TDataList.GetBItemData(Index: Integer): Integer;
begin
  RecNo := Index;
  Result := BData;
end;

function TDataList.GetBTextSize: Integer;
begin
  Result := FBTextField.Size;
end;

function TDataList.GetCount: Integer;
begin
  Result := FDataSet.RecordCount;
end;

function TDataList.GetRecNo: Integer;
begin
  Result := FDataSet.RecNo - 1;
end;

function TDataList.GetStream(Mode: TBlobStreamMode): TClientBlobStream;
begin
  if Assigned(FData) and (FDataMode <> Mode) then FreeAndNil(FData);
  if not Assigned(FData) then
  begin
    FDataMode := Mode;
    FData := TClientBlobStream(FDataSet.CreateBlobStream(FADataField, FDataMode));
  end;
  Result := FData;
end;

function TDataList.IndexOf(IndexType: TIndexType;
  const Value: array of const): Integer;
begin
  if Find(IndexType, Value) then Result := RecNo
  else Result := -1;
end;

procedure TDataList.LoadFromFile(const FileName: string);
begin
  FDataSet.LoadFromFile(FileName);
end;

function TDataList.Next: Integer;
begin
  if FIndex >= Count then FIndex := 0;
  Inc(FIndex);
  Result := FIndex;
end;

procedure TDataList.Put(Index: Integer; const Text: string);
begin
  Put(Index, Text, '', nil, 0);
end;

procedure TDataList.Put(Index: Integer; const AText, BText: string;
  const AData: TByteDynArray; BData: Integer);
begin
  RecNo := Index;
  FDataSet.Edit;
  Write(AText, BText, AData, BData);
  FDataSet.Post;
end;

procedure TDataList.Put(Index: Integer; const Text: string;
  const AData: Integer);
begin
  Put(Index, Text, '', nil, AData);
end;

procedure TDataList.Put(Index: Integer; const Text: string;
  const AData: TByteDynArray);
begin
  Put(Index, Text, '', AData, 0);
end;

procedure TDataList.SaveToFile(const FileName: string;
  Format: TDataPacketFormat);
begin
  FDataSet.SaveToFile(FileName, Format);
end;

procedure TDataList.SetAData(const Value: TByteDynArray);
begin
  FDataSet.Edit;
  Stream[bmWrite].Clear;
  Stream[bmWrite].Write(Pointer(Value)^, Length(Value));
  Apply;
  FDataSet.Post;
end;

procedure TDataList.SetAItem(Index: Integer; const Value: string);
begin
  RecNo := Index;
  FDataSet.Edit;
  FATextField.AsString := Value;
  FDataSet.Post;
end;

procedure TDataList.SetAItemData(Index: Integer; const Value: TByteDynArray);
begin
  RecNo := Index;
  AData := Value;
end;

procedure TDataList.SetBData(const Value: Integer);
begin
  FDataSet.Edit;
  FBDataField.AsInteger := Value;
  FDataSet.Post;
end;

procedure TDataList.SetBItem(Index: Integer; const Value: string);
begin
  RecNo := Index;
  FDataSet.Edit;
  FBTextField.AsString := Value;
  FDataSet.Post;
end;

procedure TDataList.SetBItemData(Index: Integer; const Value: Integer);
begin
  RecNo := Index;
  BData := Value;
end;

procedure TDataList.SetRecNo(const Value: Integer);
begin
  if (Value < 0) or (Value >= FDataSet.RecordCount) then
    raise Error(SListIndexError, [Value]);
  FDataSet.RecNo := Value + 1;
end;

procedure TDataList.Write(const AText, BText: string;
  const AData: TByteDynArray; BData: Integer);
begin
  FATextField.AsString := AText;
  FBTextField.AsString := BText;
  if Assigned(AData) then
  begin
    Stream[bmWrite].Clear;
    Stream[bmWrite].Write(Pointer(AData)^, Length(AData));
    Apply;
  end;
  FBDataField.AsInteger := BData;
end;

{ TTextList }

function TTextList.Add(const Text: string): Integer;
begin
  Result := Add(Text, nil);
end;

procedure TTextList.Add(List: TTextList);
var
  I: Integer;
begin
  Enter(FLock^);
  try
    for I := 0 to List.Count - 1 do Add(List[I], List.Data[I]);
  finally
    Leave(FLock^);
  end;
end;

function TTextList.Add(const Text: string; AData: TObject): Integer;
var
  StringArray: TStringDynArray;
  I: Integer;
begin
  Enter(FLock^);
  try
    Split(Text, FLineBreak, StringArray, True);
    try
      Result := -1;
      for I := Low(StringArray) to High(StringArray) do
      begin
        FList.FDataSet.Append;
        FList.FATextField.AsString := ExtractAText(StringArray[I]);
        FList.FBTextField.AsString := ExtractBText(StringArray[I]);
        FList.FBDataField.AsInteger := Integer(AData);
        FList.FDataSet.Post;
        if I = Low(StringArray) then Result := FList.RecNo;
      end;
    finally
      StringArray := nil;
    end;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.Clear;
begin
  Enter(FLock^);
  try
    FList.Clear;
  finally
    Leave(FLock^);
  end;
end;

constructor TTextList.Create;
begin
  FList := TDataList.Create;
  New(FLock);
  InitializeCriticalSection(FLock^);
  FDelimiter := DefaultDelimiter;
  FTextDelimiter := DefaultTextDelimiter;
  FLineBreak := DefaultLineBreak;
end;

procedure TTextList.Delete(Index: Integer);
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    FList.FDataSet.Delete;
  finally
    Leave(FLock^);
  end;
end;

destructor TTextList.Destroy;
begin
  FList.Free;
  DeleteCriticalSection(FLock^);
  Dispose(FLock);
end;

function TTextList.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TTextList.Error(const Message: string;
  const Arguments: array of const): Exception;
begin
  Result := EListError.CreateFmt(Message, Arguments);
end;

function TTextList.ExtractAText(const Text: string): string;
begin
  Enter(FLock^);
  try
    Result := Extract(Text, FTextDelimiter, AIndex, True);
  finally
    Leave(FLock^);
  end;
end;

function TTextList.ExtractBText(const Text: string): string;
var
  I: Integer;
begin
  Enter(FLock^);
  try
    I := AnsiPos(FTextDelimiter, Text);
    if I = 0 then Result := ''
    else Result := Copy(Text, I + Length(FTextDelimiter), MaxInt);
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetAText(Index: Integer): string;
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    Result := FList.FATextField.AsString;
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetBText(Text: string): string;
begin
  Enter(FLock^);
  try
    if FList.Find(itAText, [Text]) then Result := FList.FBTextField.AsString
    else Result := '';
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetBTextFromIndex(Index: Integer): string;
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    Result := FList.FBTextField.AsString;
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTextList.GetData(Index: Integer): TObject;
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    Result := TObject(FList.BDataField.AsInteger);
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetItem(Index: Integer): string;
begin
  Enter(FLock^);
  try
    Result := BTextFromIndex[Index];
    if Result = '' then Result := AText[Index]
    else Result := AText[Index] + FTextDelimiter + Result;
  finally
    Leave(FLock^);
  end;
end;

function TTextList.GetText: string;
var
  Builder: TTextBuilder;
  I: Integer;
begin
  Enter(FLock^);
  try
    Builder := TTextBuilder.Create;
    try
      for I := 0 to Count - 1 do
        if I = 0 then Builder.Append(Item[I])
        else Builder.Append(FLineBreak + Item[I]);
      Result := Builder.Text;
    finally
      Builder.Free;
    end;
  finally
    Leave(FLock^);
  end;
end;

function TTextList.IndexOf(const Text: string; IndexType: TIndexType): Integer;
begin
  Enter(FLock^);
  try
    case IndexType of
      itAText, itBText:
        Result := FList.IndexOf(IndexType, [Text]);
      itATextBText:
        Result := FList.IndexOf(IndexType, [ExtractAText(Text), ExtractBText(Text)]);
    else Result := -1;
    end;
  finally
    Leave(FLock^);
  end;
end;

function TTextList.IndexOfObject(AObject: TObject): Integer;
begin
  Enter(FLock^);
  try
    if FList.Find(itBData, [IntToStr(Integer(AObject))]) then
      Result := FList.RecNo
    else Result := -1;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.Put(Index: Integer; const Text: string);
begin
  Put(Index, Text, nil);
end;

procedure TTextList.Put(Index: Integer; const Text: string;
  AData: TObject);
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    FList.FDataSet.Edit;
    FList.FATextField.AsString := ExtractAText(Text);
    FList.FBTextField.AsString := ExtractBText(Text);
    FList.FDataSet.Post;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetAText(Index: Integer; const Value: string);
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    FList.FDataSet.Edit;
    FList.FATextField.AsString := Value;
    FList.FDataSet.Post;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetBText(Text: string; const Value: string);
begin
  Enter(FLock^);
  try
    if FList.Find(itAText, [Text]) then FList.FDataSet.Edit
    else begin
      FList.FDataSet.Append;
      FList.FATextField.AsString := Text;
    end;
    FList.FBTextField.AsString := Value;
    FList.FDataSet.Post;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetBTextFromIndex(Index: Integer; const Value: string);
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    FList.FDataSet.Edit;
    FList.FBTextField.AsString := Value;
    FList.FDataSet.Post;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetData(Index: Integer; const Value: TObject);
begin
  Enter(FLock^);
  try
    FList.RecNo := Index;
    FList.FDataSet.Edit;
    FList.BDataField.AsInteger := Integer(Value);
    FList.FDataSet.Post;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetDelimiter(const Value: string);
begin
  Enter(FLock^);
  try
    FDelimiter := Value;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetItem(Index: Integer; const Value: string);
begin
  Put(Index, Value);
end;

procedure TTextList.SetLineBreak(const Value: string);
begin
  Enter(FLock^);
  try
    FLineBreak := Value;
  finally
    Leave(FLock^);
  end;
end;

procedure TTextList.SetText(const Value: string);
begin
  Enter(FLock^);
  try
    Clear;
    Add(Value);
  finally
    Leave(FLock^);
  end;
end;

{ TFastStringList }

function TFastStringList.Add(const S: string): Integer;
begin
  Result := FList.Add(S);
end;

function TFastStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := FList.Add(S, AObject);
end;

procedure TFastStringList.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
end;

procedure TFastStringList.Assign(Source: TPersistent);
begin
  Clear;
  {$IFDEF DELPHI_7}
  NameValueSeparator := TStrings(Source).NameValueSeparator;
  {$ENDIF}
  Delimiter := TStrings(Source).Delimiter;
  {$IFDEF DELPHI_2005}
  LineBreak := TStrings(Source).LineBreak;
  {$ENDIF}
  AddStrings(TStrings(Source));
end;

procedure TFastStringList.BeginUpdate;
begin
  inherited;
  FList.FList.BeginUpdate;
end;

procedure TFastStringList.Clear;
begin
  FList.Clear;
end;

constructor TFastStringList.Create;
begin
  FList := TTextList.Create;
end;

procedure TFastStringList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TFastStringList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TFastStringList.EndUpdate;
begin
  FList.FList.EndUpdate;
  inherited;
end;

procedure TFastStringList.Exchange(Index1, Index2: Integer);
begin
end;

function TFastStringList.Get(Index: Integer): string;
begin
  Result := FList.Item[Index];
end;

function TFastStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFastStringList.GetDelimiter: string;
begin
  Result := FList.Delimiter;
end;

function TFastStringList.GetLineBreak: string;
begin
  Result := FList.LineBreak;
end;

function TFastStringList.GetName(Index: Integer): string;
begin
  Result := FList.AText[Index];
end;

function TFastStringList.GetNameValueSeparator: string;
begin
  Result := FList.FTextDelimiter;
end;

function TFastStringList.GetObject(Index: Integer): TObject;
begin
  Result := FList.Data[Index];
end;

function TFastStringList.GetTextStr: string;
begin
  Result := FList.Text;
end;

function TFastStringList.GetUpdateCount: Integer;
begin
  Result := FList.FList.UpdateCount;
end;

function TFastStringList.GetValue(const Name: string): string;
begin
  Result := FList.BText[Name];
end;

function TFastStringList.GetValueFromIndex(Index: Integer): string;
begin
  Result := FList.BTextFromIndex[Index];
end;

function TFastStringList.IndexOf(const S: string): Integer;
begin
  Result := FList.IndexOf(S);
end;

function TFastStringList.IndexOfName(const Name: string): Integer;
begin
  Result := FList.IndexOf(Name, itAText);
end;

function TFastStringList.IndexOfObject(AObject: TObject): Integer;
begin
  Result := FList.IndexOfObject(AObject);
end;

procedure TFastStringList.Insert(Index: Integer; const S: string);
begin
  FList.Add(S);
end;

procedure TFastStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  FList.Add(S, AObject);
end;

procedure TFastStringList.LoadFromFile(const FileName: string);
var
  S: string;
begin
  FileIndex := CreateFile;
  try
    if OpenFile(FileName, otOpen) then
    begin
      Reset(Files[FileIndex]^);
      while not Eof(Files[FileIndex]^) do
      begin
        ReadLn(Files[FileIndex]^, S);
        Add(S);
      end;
    end;
  finally
    DisposeFile(FileIndex);
  end;
end;

procedure TFastStringList.Move(CurIndex, NewIndex: Integer);
begin
end;

procedure TFastStringList.Put(Index: Integer; const Value: string);
begin
  FList.Item[Index] := Value;
end;

procedure TFastStringList.PutObject(Index: Integer; AObject: TObject);
begin
  FList.Data[Index] := AObject;
end;

procedure TFastStringList.SaveToFile(const FileName: string);
var
  I: Integer;
begin
  FileIndex := CreateFile;
  try
    OpenFile(FileName, otRewrite);
    for I := 0 to Count - 1 do FileUtils.Write(Strings[I]);
    SaveFile;
  finally
    DisposeFile(FileIndex);
  end;
end;

procedure TFastStringList.SetDelimiter(const Value: string);
begin
  FList.Delimiter := Value;
end;

procedure TFastStringList.SetLineBreak(const Value: string);
begin
  FList.LineBreak := Value;
end;

procedure TFastStringList.SetNameValueSeparator(const Value: string);
begin
  FList.FTextDelimiter := Value;
end;

procedure TFastStringList.SetTextStr(const Value: string);
begin
  FList.Text := Value;
end;

procedure TFastStringList.SetValue(const Name, Value: string);
begin
  FList.BText[Name] := Value;
end;

procedure TFastStringList.SetValueFromIndex(Index: Integer;
  const Value: string);
begin
  FList.BTextFromIndex[Index] := Value;
end;

end.
