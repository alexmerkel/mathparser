{ *********************************************************************** }
{                                                                         }
{ BlobManager                                                             }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit BlobManager;

{$B-}

interface

uses
  Windows, SysUtils, Classes, Graphics, Types, MemoryUtils, NumberConsts;

type
  TDataType = (dtData, dtText, dtGraphic);

  TItemName = array[0..1024 - 1] of Char;

  TBlobHeader = record
    Count: Integer;
    DataType: array of TDataType;
    Name: array of TItemName;
    From, Size: array of Integer;
  end;
  PBlobHeader = ^TBlobHeader;

  TDataItem = record
    DataType: TDataType;
    Name: TItemName;
    Stream: TMemoryStream;
  end;
  PDataItem = ^TDataItem;

  TDataItems = array of TDataItem;
  PDataItems = ^TDataItems;

  THeaderType = (htCommon, htHidden);
  TItemType = (itCommon, itHidden);

  TBlobNotifyEvent = procedure(const Sender: TObject; const Stream: TStream) of object;
  TBlobDeleteEvent = procedure(const Sender: TObject; const ItemType: TItemType; const ItemIndex: Integer) of object;

  TCustomBlobManager = class(TComponent)
  protected
    function GetAutoLoad: Boolean; virtual; abstract;
    function GetData: string; virtual; abstract;
    function GetHeader: TBlobHeader; virtual; abstract;
    function GetHiddenHeader: TBlobHeader; virtual; abstract;
    function GetHiddenItems: TDataItems; virtual; abstract;
    function GetItems: TDataItems; virtual; abstract;
    function GetItemText(ItemType: TItemType; Index: Integer): string; virtual; abstract;
    function GetMark: string; virtual; abstract;
    function GetText(Item: TDataItem): string; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAutoLoad(const Value: Boolean); virtual; abstract;
    procedure SetData(const Value: string); virtual; abstract;
    procedure SetHeader(const Value: TBlobHeader); virtual; abstract;
    procedure SetHiddenHeader(const Value: TBlobHeader); virtual; abstract;
    procedure SetHiddenItems(const Value: TDataItems); virtual; abstract;
    procedure SetItems(const Value: TDataItems); virtual; abstract;
    procedure SetItemText(ItemType: TItemType; Index: Integer; const Value: string); virtual; abstract;
    procedure SetMark(const Value: string); virtual; abstract;
    procedure SetText(Item: TDataItem; const Value: string); virtual; abstract;
  public
    function LoadFromStream(const Stream: TStream): Boolean; virtual; abstract;
    procedure SaveToStream(const Stream: TMemoryStream); virtual; abstract;

    function LoadFromStrings(const Strings: TStrings): Boolean; virtual; abstract;
    procedure SaveToStrings(const Strings: TStrings); virtual; abstract;

    function Import(const ItemType: TItemType; const DataType: TDataType; const AName: string; const AData: Pointer;
      const ASize: Int64): Integer; overload; virtual; abstract;
    function Import(const ItemType: TItemType; const DataType: TDataType; const AName: string;
      const Stream: TMemoryStream): Integer; overload; virtual; abstract;
    function Import(const DataType: TDataType; const AName: string; const AData: Pointer; const ASize: Int64): Integer; overload; virtual; abstract;
    function Import(const DataType: TDataType; const AName: string; const Stream: TMemoryStream): Integer; overload; virtual; abstract;

    function ImportData(const AName: string; const AData: Pointer; const ASize: Int64): Integer; overload; virtual; abstract;
    function ImportData(const AName: string; const Stream: TMemoryStream): Integer; overload; virtual; abstract;

    function ImportText(const AName, AText: string): Integer; virtual; abstract;
    function ImportGraphic(const AName: string; const Graphic: TGraphic): Integer; virtual; abstract;

    function Export(const ItemType: TItemType; const Index: Integer; const AData: Pointer): Boolean; overload; virtual; abstract;
    function Export(const Index: Integer; const AData: Pointer): Boolean; overload; virtual; abstract;
    function ExportText(const Index: Integer; out AText: string): Boolean; virtual; abstract;
    function ExportGraphic(const Index: Integer; const Graphic: TGraphic): Boolean; virtual; abstract;

    function Replace(const ItemType: TItemType; const Index: Integer; const AName: string; const AData: Pointer = nil;
      const ASize: Int64 = 0): Boolean; overload; virtual; abstract;
    function Replace(const ItemType: TItemType; const Index: Integer; const AName: string;
      const Stream: TMemoryStream): Boolean; overload; virtual; abstract;
    function Replace(const Index: Integer; const AName: string; const AData: Pointer = nil; const ASize: Int64 = 0): Boolean; overload; virtual; abstract;
    function Replace(const Index: Integer; const AName: string; const Stream: TMemoryStream): Boolean; overload; virtual; abstract;
    function ReplaceText(const Index: Integer; const AName, AText: string): Boolean; virtual; abstract;
    function ReplaceGraphic(const Index: Integer; const AName: string; const Graphic: TGraphic): Boolean; virtual; abstract;

    procedure Clear; virtual;
    procedure DeleteHeader(const HeaderType: THeaderType); overload; virtual; abstract;
    procedure DeleteHeader; overload; virtual; abstract;
    function DeleteItem(const ItemType: TItemType; const Index: Integer): Boolean; overload; virtual; abstract;
    function DeleteItem(const Index: Integer): Boolean; overload; virtual; abstract;
    function ClearItems(const ItemType: TItemType): PDataItems; overload; virtual; abstract;
    function ClearItems: PDataItems; overload; virtual; abstract;
    function IndexOf(const ItemType: TItemType; const AName: string): Integer; overload; virtual; abstract;
    function IndexOf(const AName: string): Integer; overload; virtual; abstract;
    function Find(const ItemType: TItemType; const AName: string): PDataItem; overload; virtual; abstract;
    function Find(const AName: string): PDataItem; overload; virtual; abstract;
    function Find(const ItemType: TItemType; const AName: string; out Index: Integer): Boolean; overload; virtual; abstract;
    function Find(const AName: string; out Index: Integer): Boolean; overload; virtual; abstract;
    function Find(const ItemType: TItemType; const AName: string; out Item: PDataItem): Boolean; overload; virtual; abstract;
    function Find(const AName: string; out Item: PDataItem): Boolean; overload; virtual; abstract;

    property Header: TBlobHeader read GetHeader write SetHeader;
    property HiddenHeader: TBlobHeader read GetHiddenHeader write SetHiddenHeader;
    property Items: TDataItems read GetItems write SetItems;
    property HiddenItems: TDataItems read GetHiddenItems write SetHiddenItems;

    property AText[Item: TDataItem]: string read GetText write SetText;
    property ItemText[ItemType: TItemType; Index: Integer]: string read GetItemText write SetItemText;
  published
    property Data: string read GetData write SetData;
    property Mark: string read GetMark write SetMark;
    property AutoLoad: Boolean read GetAutoLoad write SetAutoLoad default True;
  end;

  TBlobManager = class(TCustomBlobManager)
  private
    FAfterClear: TNotifyEvent;
    FAfterDelete: TBlobDeleteEvent;
    FAfterLoading: TBlobNotifyEvent;
    FAfterSaving: TBlobNotifyEvent;
    FAutoLoad: Boolean;
    FBeforeClear: TNotifyEvent;
    FBeforeDelete: TBlobDeleteEvent;
    FBeforeLoading: TBlobNotifyEvent;
    FBeforeSaving: TBlobNotifyEvent;
    FCompression: Boolean;
    FHeader: TBlobHeader;
    FHiddenHeader: TBlobHeader;
    FHiddenItems: TDataItems;
    FItems: TDataItems;
    FMark: string;
  protected
    function GetAutoLoad: Boolean; override;
    function GetData: string; override;
    function GetHeader: TBlobHeader; override;
    function GetHiddenHeader: TBlobHeader; override;
    function GetHiddenItems: TDataItems; override;
    function GetItems: TDataItems; override;
    function GetItemText(ItemType: TItemType; Index: Integer): string; override;
    function GetMark: string; override;
    function GetText(Item: TDataItem): string; override;
    procedure SetAutoLoad(const Value: Boolean); override;
    procedure SetData(const Value: string); override;
    procedure SetHeader(const Value: TBlobHeader); override;
    procedure SetHiddenHeader(const Value: TBlobHeader); override;
    procedure SetHiddenItems(const Value: TDataItems); override;
    procedure SetItems(const Value: TDataItems); override;
    procedure SetItemText(ItemType: TItemType; Index: Integer; const Value: string); override;
    procedure SetMark(const Value: string); override;
    procedure SetText(Item: TDataItem; const Value: string); override;

    procedure DoAfterLoading(const Stream: TStream); virtual;
    procedure DoBeforeLoading(const Stream: TStream); virtual;
    procedure DoAfterSaving(const Stream: TStream); virtual;
    procedure DoBeforeSaving(const Stream: TStream); virtual;
    procedure DoAfterDelete(const ItemType: TItemType; const ItemIndex: Integer); virtual;
    procedure DoBeforeDelete(const ItemType: TItemType; const ItemIndex: Integer); virtual;
    procedure DoAfterClear; virtual;
    procedure DoBeforeClear; virtual;

    function GetHeaderByType(const HeaderType: THeaderType): PBlobHeader; virtual;
    function CreateHeader(const HeaderType: THeaderType; const Count: Integer): PBlobHeader; virtual;

    function GetItemsByType(const ItemType: TItemType): PDataItems; virtual;
    function ImportItem(const ItemType: TItemType; const Item: TDataItem): Integer; virtual;

    function LoadHeader(const Stream: TStream; const HeaderType: THeaderType): Boolean; virtual;

    function CheckItem(const Stream: TStream; const HeaderType: THeaderType; const Index: Integer): Boolean; virtual;
    function LoadItem(const Stream: TStream; const HeaderType: THeaderType; const Index: Integer; out DataItem: TDataItem): Boolean; virtual;
    function LoadItems(const Stream: TStream; const HeaderType: THeaderType; const ItemType: TItemType): Boolean; virtual;

    procedure WriteHeader(const Stream: TStream; const ItemType: TItemType; out Reference: TIntegerDynArray); virtual;
    procedure WriteItems(const Stream: TMemoryStream; const ItemType: TItemType; const Reference: TIntegerDynArray); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadFromStream(const Stream: TStream): Boolean; override;
    procedure SaveToStream(const Stream: TMemoryStream); override;

    function LoadFromStrings(const Strings: TStrings): Boolean; override;
    procedure SaveToStrings(const Strings: TStrings); override;

    function Import(const ItemType: TItemType; const DataType: TDataType; const AName: string; const AData: Pointer;
      const ASize: Int64): Integer; overload; override;
    function Import(const ItemType: TItemType; const DataType: TDataType; const AName: string;
      const Stream: TMemoryStream): Integer; overload; override;
    function Import(const DataType: TDataType; const AName: string; const AData: Pointer; const ASize: Int64): Integer; overload; override;
    function Import(const DataType: TDataType; const AName: string; const Stream: TMemoryStream): Integer; overload; override;
    function ImportData(const AName: string; const AData: Pointer; const ASize: Int64): Integer; overload; override;
    function ImportData(const AName: string; const Stream: TMemoryStream): Integer; overload; override;
    function ImportText(const AName, AText: string): Integer; override;
    function ImportGraphic(const AName: string; const Graphic: TGraphic): Integer; override;

    function Export(const ItemType: TItemType; const Index: Integer; const AData: Pointer): Boolean; overload; override;
    function Export(const Index: Integer; const AData: Pointer): Boolean; overload; override;
    function ExportText(const Index: Integer; out AText: string): Boolean; override;
    function ExportGraphic(const Index: Integer; const Graphic: TGraphic): Boolean; override;

    function Replace(const ItemType: TItemType; const Index: Integer; const AName: string; const AData: Pointer = nil;
      const ASize: Int64 = 0): Boolean; overload; override;
    function Replace(const ItemType: TItemType; const Index: Integer; const AName: string;
      const Stream: TMemoryStream): Boolean; overload; override;
    function Replace(const Index: Integer; const AName: string; const AData: Pointer = nil; const ASize: Int64 = 0): Boolean; overload; override;
    function Replace(const Index: Integer; const AName: string; const Stream: TMemoryStream): Boolean; overload; override;
    function ReplaceText(const Index: Integer; const AName, AText: string): Boolean; override;
    function ReplaceGraphic(const Index: Integer; const AName: string; const Graphic: TGraphic): Boolean; override;

    procedure DeleteHeader(const HeaderType: THeaderType); overload; override;
    procedure DeleteHeader; overload; override;
    function DeleteItem(const ItemType: TItemType; const Index: Integer): Boolean; overload; override;
    function DeleteItem(const Index: Integer): Boolean; overload; override;
    function ClearItems(const ItemType: TItemType): PDataItems; overload; override;
    function ClearItems: PDataItems; overload; override;
    function IndexOf(const ItemType: TItemType; const AName: string): Integer; overload; override;
    function IndexOf(const AName: string): Integer; overload; override;
    function Find(const ItemType: TItemType; const AName: string): PDataItem; overload; override;
    function Find(const AName: string): PDataItem; overload; override;
    function Find(const ItemType: TItemType; const AName: string; out Index: Integer): Boolean; overload; override;
    function Find(const AName: string; out Index: Integer): Boolean; overload; override;
    function Find(const ItemType: TItemType; const AName: string; out Item: PDataItem): Boolean; overload; override;
    function Find(const AName: string; out Item: PDataItem): Boolean; overload; override;

    property ItemText[ItemType: TItemType; Index: Integer]: string read GetItemText write SetItemText;
  published
    property AData: string read GetData write SetData;
    property Compression: Boolean read FCompression write FCompression default False;
    property AfterLoading: TBlobNotifyEvent read FAfterLoading write FAfterLoading;
    property BeforeLoading: TBlobNotifyEvent read FBeforeLoading write FBeforeLoading;
    property AfterSaving: TBlobNotifyEvent read FAfterSaving write FAfterSaving;
    property BeforeSaving: TBlobNotifyEvent read FBeforeSaving write FBeforeSaving;
    property AfterDelete: TBlobDeleteEvent read FAfterDelete write FAfterDelete;
    property BeforeDelete: TBlobDeleteEvent read FBeforeDelete write FBeforeDelete;
    property AfterClear: TNotifyEvent read FAfterClear write FAfterClear;
    property BeforeClear: TNotifyEvent read FBeforeClear write FBeforeClear;
  end;

const
  DefaultMark = '{7C5C3776-CD8B-4061-8858-D4C97794B7EC}';
  CharSize = 2;
  DefaultCharCount = 32;

function MakeItemName(const AName: string): TItemName;
function MakeDataItem(const ADataType: TDataType; const AName: string; const AStream: TMemoryStream): TDataItem;

function Eof(const Stream: TStream; const Length: Integer): Boolean; overload;
function ReadValue(const Stream: TStream; var Value; Length: Integer): Boolean;

function ReadMark(const Mark: string; const Stream: TStream): Boolean;
procedure WriteMark(const Mark: string; Stream: TStream);

procedure StreamToStrings(const Stream: TMemoryStream; const Strings: TStrings; const CharCount: Integer);
function StringsToStream(const Strings: TStrings; const Stream: TMemoryStream): Boolean;

procedure Register;

implementation

uses
  ZUtils, TextConsts, TextUtils;

procedure Register;
begin
  RegisterComponents('Samples', [TBlobManager]);
end;

function MakeItemName(const AName: string): TItemName;
begin
  FillChar(Result, SizeOf(TItemName), 0);
  StrLCopy(Result, PChar(AName), SizeOf(TItemName));
end;

function MakeDataItem(const ADataType: TDataType; const AName: string; const AStream: TMemoryStream): TDataItem;
begin
  FillChar(Result, SizeOf(TDataItem), 0);
  with Result do
  begin
    DataType := ADataType;
    Name := MakeItemName(AName);
    Stream := AStream;
  end;
end;

function Eof(const Stream: TStream; const Length: Integer): Boolean;
begin
  Result := Stream.Position + Length > Stream.Size;
end;

function ReadValue(const Stream: TStream; var Value; Length: Integer): Boolean;
begin
  Result := not Eof(Stream, Length);
  if Result then Stream.Read(Value, Length);
end;

function ReadMark(const Mark: string; const Stream: TStream): Boolean;
var
  I: Integer;
  S: string;
begin
  Stream.Position := 0;
  I := Length(Mark);
  Result := not Eof(Stream, I);
  if Result then
  begin
    SetLength(S, I);
    ReadValue(Stream, PAnsiChar(S)^, I);
    Result := SameText(Mark, S);
  end;
end;

procedure WriteMark(const Mark: string; Stream: TStream);
begin
  Stream.Size := 0;
  Stream.Write(PAnsiChar(Mark)^, Length(Mark));
end;

procedure StreamToStrings(const Stream: TMemoryStream; const Strings: TStrings; const CharCount: Integer);

  procedure Move(var Target: string; const Source: string; const Index: Integer; var ASize: Integer);
  var
    I, J: Integer;
  begin
    I := Length(Source) * SizeOf(Char);
    J := (Index + I - SizeOf(Char)) div SizeOf(Char);
    if J > ASize then
    begin
      ASize := J;
      SetLength(Target, ASize);
    end;
    CopyMemory(PAnsiChar(Target) + Index - 1, PAnsiChar(Source), I);
  end;

var
  I, J, K, ASize: Integer;
  AText, Number: string;
begin
  Strings.Clear;
  J := Integer(Stream.Memory);
  K := 1;
  I := Stream.Size * CharSize;
  ASize := I + (I div CharCount - 1) * Integer(Length(CR + LF));
  SetLength(AText, ASize);
  ZeroMemory(PAnsiChar(AText), ASize * SizeOf(Char));
  for I := 0 to Stream.Size - 1 do
  begin
    if (I > 0) and (I mod CharCount = 0) then
      Number := CR + LF + IntToHex(PByte(J)^, CharSize)
    else
      Number := IntToHex(PByte(J)^, CharSize);
    Move(AText, Number, K, ASize);
    Inc(K, Length(Number));
    Inc(J);
  end;
  Strings.Text := AText;
end;

function StringsToStream(const Strings: TStrings; const Stream: TMemoryStream): Boolean;
var
  I, J, K: Integer;
  AText, Number: string;
begin
  Stream.Clear;
  Result := Strings.Count > 0;
  if Result then
    for I := 0 to Strings.Count - 1 do
    begin
      AText := Trim(Strings[I]);
      J := 1;
      Number := Copy(AText, J, CharSize);
      while Number <> '' do
      begin
        Result := TryStrToInt(Dollar + Number, K);
        if Result then
        begin
          Stream.Write(K, SizeOf(Byte));
          Inc(J, CharSize);
          Number := Copy(AText, J, CharSize);
        end
        else Break;
      end;
    end;
end;

{ TCustomBlobManager }

procedure TCustomBlobManager.AssignTo(Dest: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Dest is TCustomBlobManager then
  begin
    Stream := TMemoryStream.Create;
    try
      SaveToStream(Stream);
      Stream.Position := 0;
      TCustomBlobManager(Dest).LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else inherited AssignTo(Dest);
end;

procedure TCustomBlobManager.Clear;
var
  I: Integer;
begin
  for I := Ord(Low(TItemType)) to Ord(High(TItemType)) do ClearItems(TItemType(I));
  for I := Ord(Low(THeaderType)) to Ord(High(THeaderType)) do DeleteHeader(THeaderType(I));
end;

{ TBlobManager }

function TBlobManager.CheckItem(const Stream: TStream; const HeaderType: THeaderType; const Index: Integer): Boolean;
var
  H: PBlobHeader;
begin
  H := GetHeaderByType(HeaderType);
  Result := (Index >= 0) and (Index < H.Count) and (H.From[Index] + H.Size[Index] <= Stream.Size);
end;

function TBlobManager.ClearItems(const ItemType: TItemType): PDataItems;
var
  I: Integer;
begin
  DoBeforeClear;
  Result := GetItemsByType(ItemType);
  for I := Low(Result^) to High(Result^) do Result^[I].Stream.Free;
  Result^ := nil;
  DoAfterClear;
end;

function TBlobManager.ClearItems: PDataItems;
begin
  Result := ClearItems(itCommon);
end;

constructor TBlobManager.Create(AOwner: TComponent);
begin
  inherited;
  FMark := DefaultMark;
  FAutoLoad := True;
end;

function TBlobManager.CreateHeader(const HeaderType: THeaderType; const Count: Integer): PBlobHeader;
begin
  Result := GetHeaderByType(HeaderType);
  Result.Count := Count;
  SetLength(Result.DataType, Count);
  SetLength(Result.Name, Count);
  SetLength(Result.From, Count);
  SetLength(Result.Size, Count);
end;

procedure TBlobManager.DeleteHeader(const HeaderType: THeaderType);
var
  H: PBlobHeader;
begin
  H := GetHeaderByType(HeaderType);
  FillChar(H^, SizeOf(TBlobHeader), 0);
end;

procedure TBlobManager.DeleteHeader;
begin
  DeleteHeader(htCommon);
end;

function TBlobManager.DeleteItem(const ItemType: TItemType; const Index: Integer): Boolean;
var
  AItems: PDataItems;
  I: Integer;
begin
  DoBeforeDelete(ItemType, Index);
  AItems := GetItemsByType(ItemType);
  Result := (Index >= 0) and (Index < Length(AItems^));
  if Result then
  begin
    FreeAndNil(AItems^[Index].Stream);
    I := Length(AItems^);
    Result := Delete(AItems^, Index * SizeOf(TDataItem), SizeOf(TDataItem), I * SizeOf(TDataItem));
    if Result then SetLength(AItems^, I - 1);
    DoAfterDelete(ItemType, Index);
  end;
end;

function TBlobManager.DeleteItem(const Index: Integer): Boolean;
begin
  Result := DeleteItem(itCommon, Index);
end;

destructor TBlobManager.Destroy;
begin
  DeleteHeader(htCommon);
  DeleteHeader(htHidden);
  ClearItems(itCommon);
  ClearItems(itHidden);
  inherited;
end;

procedure TBlobManager.DoAfterClear;
begin
  if Assigned(FAfterClear) then FAfterClear(Self);
end;

procedure TBlobManager.DoAfterDelete(const ItemType: TItemType; const ItemIndex: Integer);
begin
  if Assigned(FAfterDelete) then FAfterDelete(Self, ItemType, ItemIndex);
end;

procedure TBlobManager.DoAfterLoading(const Stream: TStream);
begin
  if Assigned(FAfterLoading) then FAfterLoading(Self, Stream);
end;

procedure TBlobManager.DoAfterSaving(const Stream: TStream);
begin
  if Assigned(FAfterSaving) then FAfterSaving(Self, Stream);
end;

procedure TBlobManager.DoBeforeClear;
begin
  if Assigned(FBeforeClear) then FBeforeClear(Self);
end;

procedure TBlobManager.DoBeforeDelete(const ItemType: TItemType; const ItemIndex: Integer);
begin
  if Assigned(FBeforeDelete) then FBeforeDelete(Self, ItemType, ItemIndex);
end;

procedure TBlobManager.DoBeforeLoading(const Stream: TStream);
begin
  if Assigned(FBeforeLoading) then FBeforeLoading(Self, Stream);
end;

procedure TBlobManager.DoBeforeSaving(const Stream: TStream);
begin
  if Assigned(FBeforeSaving) then FBeforeSaving(Self, Stream);
end;

function TBlobManager.Export(const Index: Integer; const AData: Pointer): Boolean;
begin
  Result := Export(itCommon, Index, AData);
end;

function TBlobManager.Export(const ItemType: TItemType; const Index: Integer; const AData: Pointer): Boolean;
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  Result := (Index >= 0) and (Index < Length(AItems^));
  if Result then
  begin
    AItems^[Index].Stream.Position := 0;
    AItems^[Index].Stream.Read(AData^, AItems^[Index].Stream.Size);
  end;
end;

function TBlobManager.ExportGraphic(const Index: Integer; const Graphic: TGraphic): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FItems));
  if Result then
  begin
    FItems[Index].Stream.Position := 0;
    Graphic.LoadFromStream(FItems[Index].Stream);
  end;
end;

function TBlobManager.ExportText(const Index: Integer; out AText: string): Boolean;
var
  I: Integer;
begin
  Result := (Index >= 0) and (Index < Length(FItems));
  if Result then
  begin
    I := FItems[Index].Stream.Size;
    SetLength(AText, I);
    FItems[Index].Stream.Position := 0;
    FItems[Index].Stream.Read(PAnsiChar(AText)^, I);
  end;
end;

function TBlobManager.Find(const ItemType: TItemType; const AName: string; out Index: Integer): Boolean;
var
  AItems: PDataItems;
  I: Integer;
begin
  AItems := GetItemsByType(ItemType);
  for I := Low(AItems^) to High(AItems^) do
  begin
    Result := SameText(AItems^[I].Name, AName);
    if Result then
    begin
      Index := I;
      Exit;
    end;
  end;
  Result := False;
end;

function TBlobManager.Find(const AName: string): PDataItem;
begin
  Result := Find(itCommon, AName);
end;

function TBlobManager.Find(const ItemType: TItemType; const AName: string): PDataItem;
var
  AItems: PDataItems;
  I: Integer;
begin
  AItems := GetItemsByType(ItemType);
  for I := Low(AItems^) to High(AItems^) do if SameText(AItems^[I].Name, AName) then
  begin
    Result := @AItems^[I];
    Exit;
  end;
  Result := nil;
end;

function TBlobManager.Find(const AName: string; out Item: PDataItem): Boolean;
begin
  Result := Find(itCommon, AName, Item);
end;

function TBlobManager.Find(const ItemType: TItemType; const AName: string; out Item: PDataItem): Boolean;
var
  AItems: PDataItems;
  I: Integer;
begin
  AItems := GetItemsByType(ItemType);
  for I := Low(AItems^) to High(AItems^) do
  begin
    Result := SameText(AItems^[I].Name, AName);
    if Result then
    begin
      Item := @AItems^[I];
      Exit;
    end;
  end;
  Result := False;
end;

function TBlobManager.Find(const AName: string; out Index: Integer): Boolean;
begin
  Result := Find(itCommon, AName, Index);
end;

function TBlobManager.GetAutoLoad: Boolean;
begin
  Result := FAutoLoad;
end;

function TBlobManager.GetData: string;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    SaveToStrings(List);
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TBlobManager.GetHeader: TBlobHeader;
begin
  Result := FHeader;
end;

function TBlobManager.GetHeaderByType(const HeaderType: THeaderType): PBlobHeader;
begin
  if HeaderType = htCommon then Result := @FHeader
  else Result := @FHiddenHeader;
end;

function TBlobManager.GetHiddenHeader: TBlobHeader;
begin
  Result := FHiddenHeader;
end;

function TBlobManager.GetHiddenItems: TDataItems;
begin
  Result := FHiddenItems;
end;

function TBlobManager.GetItems: TDataItems;
begin
  Result := FItems;
end;

function TBlobManager.GetItemText(ItemType: TItemType; Index: Integer): string;
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  if (Index >= 0) and (Index < Length(AItems^)) and (AItems^[Index].DataType = dtText) then
    Result := GetText(AItems^[Index])
  else
    Result := '';
end;

function TBlobManager.GetItemsByType(const ItemType: TItemType): PDataItems;
begin
  if ItemType = itCommon then
    Result := @FItems
  else
    Result := @FHiddenItems;
end;

function TBlobManager.GetMark: string;
begin
  Result := FMark;
end;

function TBlobManager.GetText(Item: TDataItem): string;
var
  I: Integer;
begin
  I := Item.Stream.Size;
  SetLength(Result, I);
  Item.Stream.Position := 0;
  Item.Stream.Read(PAnsiChar(Result)^, I);
end;

function TBlobManager.Import(const DataType: TDataType; const AName: string; const AData: Pointer; const ASize: Int64): Integer;
begin
  Result := Import(itCommon, DataType, AName, AData, ASize);
end;

function TBlobManager.Import(const ItemType: TItemType; const DataType: TDataType; const AName: string;
  const Stream: TMemoryStream): Integer;
begin
  Result := ImportItem(ItemType, MakeDataItem(DataType, AName, Stream));
end;

function TBlobManager.Import(const ItemType: TItemType; const DataType: TDataType; const AName: string; const AData: Pointer;
  const ASize: Int64): Integer;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.Size := ASize;
  Stream.Write(AData^, ASize);
  Result := ImportItem(ItemType, MakeDataItem(DataType, AName, Stream));
end;

function TBlobManager.Import(const DataType: TDataType; const AName: string; const Stream: TMemoryStream): Integer;
begin
  Result := Import(itCommon, DataType, AName, Stream);
end;

function TBlobManager.ImportData(const AName: string; const Stream: TMemoryStream): Integer;
begin
  Result := Import(dtData, AName, Stream);
end;

function TBlobManager.ImportData(const AName: string; const AData: Pointer; const ASize: Int64): Integer;
begin
  Result := Import(dtData, AName, AData, ASize);
end;

function TBlobManager.ImportGraphic(const AName: string; const Graphic: TGraphic): Integer;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Graphic.SaveToStream(Stream);
  Result := ImportItem(itCommon, MakeDataItem(dtGraphic, AName, Stream));
end;

function TBlobManager.ImportItem(const ItemType: TItemType; const Item: TDataItem): Integer;
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  Result := Length(AItems^);
  SetLength(AItems^, Result + 1);
  AItems^[Result] := Item;
end;

function TBlobManager.ImportText(const AName, AText: string): Integer;
begin
  Result := Import(dtText, AName, PAnsiChar(AText), Length(AText));
end;

function TBlobManager.IndexOf(const AName: string): Integer;
begin
  Result := IndexOf(itCommon, AName);
end;

function TBlobManager.IndexOf(const ItemType: TItemType; const AName: string): Integer;
begin
  if not Find(ItemType, AName, Result) then Result := -1;
end;

function TBlobManager.LoadFromStream(const Stream: TStream): Boolean;
begin
  DoBeforeLoading(Stream);
  if FCompression then Decompress(Stream);
  Result := ReadMark(FMark, Stream);
  if Result then
  begin
    LoadHeader(Stream, htCommon);
    LoadHeader(Stream, htHidden);
    if FAutoLoad then
    begin
      LoadItems(Stream, htCommon, itCommon);
      LoadItems(Stream, htHidden, itHidden);
    end
    else begin
      ClearItems(itCommon);
      ClearItems(itHidden);
    end;
  end;
  DoAfterLoading(Stream);
end;

function TBlobManager.LoadFromStrings(const Strings: TStrings): Boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    StringsToStream(Strings, Stream);
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TBlobManager.LoadHeader(const Stream: TStream; const HeaderType: THeaderType): Boolean;
var
  I, J, K: Integer;
  H: PBlobHeader;
begin
  Result := ReadValue(Stream, J, SizeOf(J));
  if Result then
  begin
    H := CreateHeader(HeaderType, J);
    for I := 0 to J - 1 do
    begin
      Result := ReadValue(Stream, H.DataType[I], SizeOf(TDataType)) and ReadValue(Stream, K, SizeOf(K)) and not Eof(Stream, K);
      if not Result then Break;
      FillChar(H.Name[I], SizeOf(TItemName), 0);
      Stream.Read(H.Name[I][0], K);
      Result := ReadValue(Stream, H.From[I], SizeOf(Integer)) and ReadValue(Stream, H.Size[I], SizeOf(Integer));
      if not Result then Break;
    end;
  end;
  if not Result then DeleteHeader(HeaderType);
end;

function TBlobManager.LoadItem(const Stream: TStream; const HeaderType: THeaderType; const Index: Integer; out DataItem: TDataItem): Boolean;
var
  H: PBlobHeader;
begin
  Result := CheckItem(Stream, HeaderType, Index);
  if Result then
  begin
    H := GetHeaderByType(HeaderType);
    DataItem.DataType := H.DataType[Index];
    DataItem.Name := H.Name[Index];
    DataItem.Stream := TMemoryStream.Create;
    DataItem.Stream.Size := H.Size[Index];
    Stream.Position := H.From[Index];
    Result := ReadValue(Stream, DataItem.Stream.Memory^, H.Size[Index]);
  end;
end;

function TBlobManager.LoadItems(const Stream: TStream; const HeaderType: THeaderType; const ItemType: TItemType): Boolean;
var
  H: PBlobHeader;
  AItems: PDataItems;
  I, J: Integer;
begin
  H := GetHeaderByType(HeaderType);
  AItems := ClearItems(ItemType);
  Result := H.Count > 0;
  if Result then
  begin
    for I := 0 to H.Count - 1 do
    begin
      J := Length(AItems^);
      SetLength(AItems^, J + 1);
      Result := LoadItem(Stream, HeaderType, I, AItems^[J]);
      if not Result then Break;
    end;
    if not Result then ClearItems(ItemType);
  end;
end;

function TBlobManager.Replace(const ItemType: TItemType; const Index: Integer; const AName: string;
  const Stream: TMemoryStream): Boolean;
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  Result := (Index >= 0) and (Index < Length(AItems^));
  if Result then
  begin
    AItems^[Index].Name := MakeItemName(AName);
    AItems^[Index].Stream.Free;
    AItems^[Index].Stream := Stream;
  end;
end;

function TBlobManager.Replace(const ItemType: TItemType; const Index: Integer; const AName: string;
  const AData: Pointer; const ASize: Int64): Boolean;
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  Result := (Index >= 0) and (Index < Length(AItems^));
  if Result then
  begin
    AItems^[Index].Name := MakeItemName(AName);
    AItems^[Index].Stream.Size := ASize;
    AItems^[Index].Stream.Position := 0;
    AItems^[Index].Stream.Write(AData^, ASize);
  end;
end;

function TBlobManager.Replace(const Index: Integer; const AName: string; const AData: Pointer; const ASize: Int64): Boolean;
begin
  Result := Replace(itCommon, Index, AName, AData, ASize);
end;

function TBlobManager.Replace(const Index: Integer; const AName: string; const Stream: TMemoryStream): Boolean;
begin
  Result := Replace(itCommon, Index, AName, Stream);
end;

function TBlobManager.ReplaceGraphic(const Index: Integer; const AName: string; const Graphic: TGraphic): Boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Graphic.SaveToStream(Stream);
  Result := Replace(Index, AName, Stream);
end;

function TBlobManager.ReplaceText(const Index: Integer; const AName, AText: string): Boolean;
begin
  Result := Replace(Index, AName, PAnsiChar(AText), Length(AText));
end;

procedure TBlobManager.SaveToStream(const Stream: TMemoryStream);
var
  Reference: array[TItemType] of TIntegerDynArray;
  I: TItemType;
begin
  DoBeforeSaving(Stream);
  WriteMark(FMark, Stream);
  try
    WriteHeader(Stream, itCommon, Reference[itCommon]);
    WriteHeader(Stream, itHidden, Reference[itHidden]);
    WriteItems(Stream, itCommon, Reference[itCommon]);
    WriteItems(Stream, itHidden, Reference[itHidden]);
  finally
    for I := Low(TItemType) to High(TItemType) do Reference[I] := nil;
  end;
  if FCompression then Compress(Stream);
  DoAfterSaving(Stream);
end;

procedure TBlobManager.SaveToStrings(const Strings: TStrings);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    StreamToStrings(Stream, Strings, DefaultCharCount);
  finally
    Stream.Free;
  end;
end;

procedure TBlobManager.SetAutoLoad(const Value: Boolean);
begin
  FAutoLoad := Value;
end;

procedure TBlobManager.SetData(const Value: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := Value;
    LoadFromStrings(List);
  finally
    List.Free;
  end;
end;

procedure TBlobManager.SetHeader(const Value: TBlobHeader);
begin
  FHeader := Value;
end;

procedure TBlobManager.SetHiddenHeader(const Value: TBlobHeader);
begin
  FHiddenHeader := Value;
end;

procedure TBlobManager.SetHiddenItems(const Value: TDataItems);
begin
  FHiddenItems := Value;
end;

procedure TBlobManager.SetItems(const Value: TDataItems);
begin
  FItems := Value;
end;

procedure TBlobManager.SetItemText(ItemType: TItemType; Index: Integer; const Value: string);
var
  AItems: PDataItems;
begin
  AItems := GetItemsByType(ItemType);
  if (Index >= 0) and (Index < Length(AItems^)) then SetText(AItems^[Index], Value);
end;

procedure TBlobManager.SetMark(const Value: string);
begin
  FMark := Value;
end;

procedure TBlobManager.SetText(Item: TDataItem; const Value: string);
begin
  Item.Stream.Size := Length(Value);
  Item.Stream.Position := 0;
  Item.Stream.Write(PAnsiChar(Value)^, Item.Stream.Size);
end;

procedure TBlobManager.WriteHeader(const Stream: TStream; const ItemType: TItemType; out Reference: TIntegerDynArray);
var
  AItems: PDataItems;
  I, J: Integer;
begin
  AItems := GetItemsByType(ItemType);
  I := Length(AItems^);
  Stream.Write(I, SizeOf(I));
  SetLength(Reference, I);
  for I := Low(AItems^) to High(AItems^) do
  begin
    Stream.Write(AItems^[I].DataType, SizeOf(AItems^[I].DataType));
    J := Length(AItems^[I].Name);
    Stream.Write(J, SizeOf(J));
    Stream.Write(AItems^[I].Name[0], J);
    Reference[I] := Stream.Position;
    J := 0;
    Stream.Write(J, SizeOf(J));
    Stream.Write(J, SizeOf(J));
  end;
end;

procedure TBlobManager.WriteItems(const Stream: TMemoryStream; const ItemType: TItemType; const Reference: TIntegerDynArray);
var
  AItems: PDataItems;
  I, J: Integer;
begin
  AItems := GetItemsByType(ItemType);
  for I := Low(AItems^) to High(AItems^) do
  begin
    PInteger(Integer(Stream.Memory) + Reference[I])^ := Stream.Position;
    J := AItems^[I].Stream.Size;
    PInteger(Integer(Stream.Memory) + Reference[I] + SizeOf(Integer))^ := J;
    Stream.Write(AItems^[I].Stream.Memory^, J);
  end;
end;

end.
