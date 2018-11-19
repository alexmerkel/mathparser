{ *********************************************************************** }
{                                                                         }
{ ParseUtils                                                              }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Types, ParseErrors,
  Parser, ParseTypes, TextConsts, ValueTypes, ValueUtils;

{$I Integer.inc}

type
  TLockType = (ltBracket, ltChar);
  TLock = record
    FromIndex, TillIndex: Integer;
  end;
  TLockArray = array of TLock;

  TTextArray = TByteDynArray;

  TBracketMethod = function(var Text: string; const FromIndex, TillIndex: Integer; const Data: Pointer): TError of object;

  PFindData = ^TFindData;
  TFindData = record
    Code, Index, FromIndex: Integer;
    Item: ^PScriptItem;
  end;

  TParseHelper = class
  protected
    function FindMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
      const Item: PScriptItem; const Data: Pointer): Boolean; virtual;
    function FunctionArrayMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
      const Item: PScriptItem; const Data: Pointer): Boolean; virtual;
  public
    function FindItem(const Script: TScript; const ACode, AFromIndex: Integer): PScriptItem;
    procedure GetFunctionArray(const Script: TScript; out FArray: TIntegerDynArray);
    function Optimizable(const Script: TScript; const Data: PFunctionData): Boolean;
  end;

function MakeHandleArray(const HandleArray: array of Integer): TIntegerDynArray;
procedure SortHandleArray(var HandleArray: TIntegerDynArray);

function ParseScript(Index: NativeInt; const Method: TScriptMethod; const Data: Pointer): Boolean;
procedure BuildScript(const Parser: TCustomParser; const ItemArray: TScriptArray; out Script: TScript;
  const Optimize: Boolean; Number: PNumber = nil);

function CheckFHandle(const FData: PFunctionData; const Handle: Integer): Boolean;
function CheckTHandle(const TData: PTypeData; const Handle: Integer): Boolean;

function RestoreText(const Parser: TCustomParser; const FromText: string; out ToText: string; const SA: TScriptArray; const Parameter: Boolean = False): Integer; overload;
function RestoreText(const Parser: TCustomParser; const Text: string; const SA: TScriptArray; const Parameter: Boolean = False): string; overload;
function RestoreText(const Parser: TCustomParser; const TextArray: TTextItemArray; const SA: TScriptArray;
  const Parameter: Boolean = False): string; overload;

procedure Read(const Parser: TCustomParser; var Index: NativeInt; out AFunction: PFunction);
function ParameterToScript(const Script: TScript; const THandle: Integer = -1): TScript;
function GetScriptFromValue(const Parser: TCustomParser; Value: TValue; const TypeFlag: Boolean; const ScriptType: TScriptType): TScript; overload;
function GetScriptFromValue(const Parser: TCustomParser; const Number: TNumber; const ScriptType: TScriptType): TScript; overload;
function GetValueFromScript(const Parser: TCustomParser; const Script: TScript; const ScriptType: TScriptType; const UserType: PUserType = nil): TValue;

function GetParameter(const Parser: TCustomParser; const Parameter: TParameter): TParameter;
function AsValue(const Parser: TCustomParser; const Parameter: TParameter): TValue;
function AsByte(const Parser: TCustomParser; const Parameter: TParameter): Byte;
function AsShortint(const Parser: TCustomParser; const Parameter: TParameter): Shortint;
function AsWord(const Parser: TCustomParser; const Parameter: TParameter): Word;
function AsSmallint(const Parser: TCustomParser; const Parameter: TParameter): Smallint;
function AsLongword(const Parser: TCustomParser; const Parameter: TParameter): Longword;
function AsInteger(const Parser: TCustomParser; const Parameter: TParameter): Integer;
function AsInt64(const Parser: TCustomParser; const Parameter: TParameter): Int64;
function AsSingle(const Parser: TCustomParser; const Parameter: TParameter): Single;
function AsDouble(const Parser: TCustomParser; const Parameter: TParameter): Double;
function AsExtended(const Parser: TCustomParser; const Parameter: TParameter): Extended;
function AsBoolean(const Parser: TCustomParser; const Parameter: TParameter): Boolean; overload;
function AsText(const Parser: TCustomParser; const Parameter: TParameter): string;
function AsBoolean(const Parser: TCustomParser; const ParameterArray: TParameterArray;
  const Index: Integer; const Default: Boolean): Boolean; overload;

function Optimal(const Script: TScript; const ScriptType: TScriptType): Boolean;
function TypeFlag(const Script: TScript; const ScriptType: TScriptType): Boolean;

function GetFlagArray(const Text: string; const LockType: TLockType; const FData: PFunctionData; const TData: PTypeData; out FlagArray: TFunctionFlagArray;
  const CleanText: PString = nil; const TypeArray: PHandleArray = nil; const Method: TFunctionFlagMethod = nil): Boolean;
function GetItemArray(const Text: string; const FData: PFunctionData; const TData: PTypeData; out ItemArray: TTextItemArray;
  const InternalHandle: Integer = -1; const LockType: TLockType = ltChar): Boolean;

function TrimPOperator(var Text: string): Boolean;
function FindSign(const Text: string; var Index: Integer; out Sign: Boolean): Boolean;
function GetSign(const Text: string; const Index: PInteger = nil): Boolean; overload;
function GetSign(const Item: TTextItem; const TOHArray: TIntegerDynArray): Boolean; overload;
function SetSign(const Text: string): string;
function TrimFunction(const Data: PFunctionData; var Text: string; const Trim: Boolean): PFunction; overload;
function TrimType(const Data: PTypeData; const StringHandle: Integer; var Text: string; const Trim: Boolean): PType; overload;

procedure WriteItemHeader(const ItemHeader: PItemHeader; const ASign: Boolean; const THandle, DefaultTypeHandle: Integer);

function GetISIndex(const Text: string; out Parameter: Boolean): Integer; overload;
function GetISIndex(var Text: string; const Parameter: PBoolean): Integer; overload;
function GetIS(const Text: string; const SA: TScriptArray; out Parameter: Boolean): TScript;

function GetLockArray(const Text: string; const LockBracket: TBracket): TLockArray; overload;
function GetLockArray(const Text: string; const LockChar: Char): TLockArray; overload;
function GetTextArray(const Text: string; const LockBracket: TBracket): TTextArray; overload;
function GetTextArray(const Text: string; const LockChar: Char): TTextArray; overload;
function GetTextArray(const Text: string; const LockArray: TLockArray): TTextArray; overload;
function GetTextArray(const Text: string; const LockType: TLockType): TTextArray; overload;
function Locked(const Index: Integer; const LockArray: TLockArray): Boolean; overload;
function Locked(const Index: Integer; const TextArray: TTextArray): Boolean; overload;

procedure ParseBracket(var Text: string; const Bracket: TBracket; const Method: TBracketMethod; const Data: Pointer; out Error: TError); overload;
procedure ParseBracket(var Text: string; const Bracket: TBracket; const Method: TBracketMethod; const Data: Pointer); overload;
function NextBracket(const Text: string; const Bracket: TBracket; Index: Integer): Integer;
function PrevBracket(const Text: string; const Bracket: TBracket; Index: Integer): Integer;
function Embrace(const Text: string; const Bracket: TBracket): string;
function InBrace(const Text: string; const Bracket: TBracket): Boolean;
procedure DeleteBracket(var Text: string; const Index: Integer; const Bracket: TBracket);
procedure ChangeBracket(var Text: string; const Index: Integer; const BracketFrom, BracketTo: TBracket); overload;
function BracketFunction(const AFunction: PFunction): Boolean;
procedure ChangeBracket(var Text: string; const Data: PFunctionData; const BracketFrom, BracketTo: TBracket); overload;

function Lower(const Text: string): string;
function Upper(const Text: string): string;
function Whole(const Text: string; const Index, Count: Integer): Boolean;
function FollowingText(const Text: string; const Index: Integer): string;
function PrecedingText(const Text: string; const Index: Integer): string;
function WholeValue(const Parser: TCustomParser; const Text: string; const Index, Count: Integer): Boolean;

function GotoBody(const Data: string; const Bracket: TBracket; var Index: Integer; const Count: Integer): Boolean;
function FindBody(const Name, Data: string; const Bracket: TBracket; out Index, Count: Integer): Boolean;
function BodyText(const Name, Data: string; const Bracket: TBracket; out Text: string): Boolean;

function Quote(const Text: string): string;
function QuoteDouble(const Text, FunctionName: string; const Bracket: TBracket): string;
function InQuote(const Text: string): Boolean;

function MakeTemplate(const Parser: TCustomParser; const Data: PFunctionData; const Text: string; const ValueArray: PValueArray;
  const NumberTemplate: string = Inquiry): string;
function WriteValue(Index: NativeInt; var ValueIndex: Integer; const ValueArray: TValueArray;
  const ScriptType: TScriptType): Boolean;

function Add(var Script: TScript; const Value: Smallint): Integer; overload;
function Add(var Script: TScript; const Value: Word): Integer; overload;
function Add(var Script: TScript; const Value: Integer): Integer; overload;
function Add(var Script: TScript; const Value: Longword): Integer; overload;
function Add(var Script: TScript; const Value: Int64): Integer; overload;
function Add(var Script: TScript; const Value: Single): Integer; overload;
function Add(var Script: TScript; const Value: Double): Integer; overload;
function Add(var Script: TScript; const Value: Extended): Integer; overload;
function Add(var Script: TScript; const Value: TValue): Integer; overload;

function Add(var Script: TScript; const Value: TScriptNumber): Integer; overload;
function Add(var Script: TScript; const Value: TScriptFunction): Integer; overload;
function Add(var Target: TScript; const Source: TScript): Integer; overload;
function Add(var Script: TScript; const Source: Pointer; const Count: Integer): Integer; overload;
function Add(var Script: TScript; const Text: string): Integer; overload;

function Add(var SA: TScriptArray; const Value: TScript): Integer; overload;
function Add(var Data: TFunctionData; const AFunction: TFunction): Integer; overload;
function Add(var Data: TTypeData; const AType: TType): Integer; overload;
function Add(var ParameterArray: TParameterArray; const ATHandle: Integer; const AValue: TValue): Integer; overload;
function Add(var ParameterArray: TParameterArray; const ATHandle: Integer; const AValue: string): Integer; overload;
function Add(var LockArray: TLockArray; const AFromIndex, ATillIndex: Integer): Integer; overload;
function Add(var FlagArray: TFunctionFlagArray; const Flag: TFunctionFlag): Integer; overload;
function Add(var FlagArray: TFunctionFlagArray; const Index, Handle: Integer): Integer; overload;
function Add(var ItemArray: TTextItemArray; const TextItem: TTextItem): Integer; overload;
function Add(var ItemArray: TTextItemArray; const Text: string; const FHandle, THandle: Integer): Integer; overload;

procedure IncNumber(var Number: TNumber; const AValue: TValue); overload;
procedure IncNumber(var Number: TNumber; const Parser: TCustomParser; const Script: TScript; const ScriptType: TScriptType); overload;

procedure Delete(var SA: TScriptArray);

function Insert(var Script: TScript; Value: Int64; Index: Integer): Boolean; overload;
function Insert(var Script: TScript; Value, Index: Integer): Boolean; overload;
function Insert(var Script: TScript; Value: Byte; Index: Integer): Boolean; overload;

function GetFunction(const Data: PFunctionData; Index: Integer; out AFunction: PFunction): Boolean; overload;
function FunctionByIndex(const Data: PFunctionData; const Index: Integer): PFunction;
function FunctionCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
procedure FunctionExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
function Prepare(const Data: PFunctionData): Boolean; overload;

function GetType(const Data: PTypeData; Index: Integer; out AType: PType): Boolean; overload;
function TypeByIndex(const Data: PTypeData; const Index: Integer): PType;
function TypeCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
procedure TypeExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
function Prepare(const Data: PTypeData): Boolean; overload;

function MakeTypeHandle(const Data: TTypeData; const ValueType: TValueType; const DefaultTypeHandle: Integer): Integer;
function AssignUserType(const Parser: TCustomParser; const ItemHeader: PItemHeader; const TypeFlag: Boolean; const ValueType: TValueType): Boolean;
function GetType(const Parser: TCustomParser; const ItemHeader: PItemHeader): PType; overload;

function TestFormula(const Parser: TCustomParser; const Text: string; out Script: TScript): TError;
function FindFormula(const Parser: TCustomParser; const Text: string; out Script: TScript): Boolean;

var
  Helper: TParseHelper;
  LockBracket: TBracket;
  LockChar: Char;

implementation

uses
  Cache, FlagCache, FlexibleList, ItemCache, Math, MemoryUtils, NumberConsts, ParseConsts,
  StrUtils, TextBuilder, TextUtils, ValueConsts;

{ TParseHelper }

function TParseHelper.FindItem(const Script: TScript; const ACode, AFromIndex: Integer): PScriptItem;
var
  FindData: TFindData;
begin
  Result := nil;
  FillChar(FindData, SizeOf(TFindData), 0);
  with FindData do
  begin
    Code := ACode;
    FromIndex := AFromIndex;
    Item := @Result;
  end;
  ParseScript(NativeInt(Script), FindMethod, @FindData);
end;

function TParseHelper.FindMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
  const Item: PScriptItem; const Data: Pointer): Boolean;
var
  FindData: ^TFindData absolute Data;
begin
  Result := FindData.Index < FindData.FromIndex;
  if Result then Inc(FindData.Index)
  else Result := FindData.Code <> Item.Code;
  if Result then
    case Item.Code of
      NumberCode:
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
      FunctionCode:
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptFunction));
      StringCode:
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
      ScriptCode, ParameterCode:
        begin
          ParseScript(Index + SizeOf(TCode), FindMethod, Data);
          Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
        end;
    else raise Error(ScriptError);
    end
  else FindData.Item^ := Item;
end;

function TParseHelper.FunctionArrayMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
  const Item: PScriptItem; const Data: Pointer): Boolean;
var
  FArray: ^TIntegerDynArray absolute Data;
begin
  case Item.Code of
    NumberCode: Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
    FunctionCode:
      begin
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptFunction));
        Add(FArray^, Item.ScriptFunction.Handle);
      end;
    StringCode:
      Inc(Index, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
    ScriptCode, ParameterCode:
      begin
        ParseScript(Index + SizeOf(TCode), FunctionArrayMethod, Data);
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
      end;
  else raise Error(ScriptError);
  end;
  Result := True;
end;

procedure TParseHelper.GetFunctionArray(const Script: TScript; out FArray: TIntegerDynArray);
begin
  ParseScript(Integer(Script), FunctionArrayMethod, @FArray);
end;

function TParseHelper.Optimizable(const Script: TScript; const Data: PFunctionData): Boolean;
var
  FArray: TIntegerDynArray;
  I: Integer;
begin
  GetFunctionArray(Script, FArray);
  try
    for I := Low(FArray) to High(FArray) do
      if not Data.FArray[FArray[I]].Optimizable then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  finally
    FArray := nil;
  end;
end;

function MakeHandleArray(const HandleArray: array of Integer): TIntegerDynArray;
var
  I: Integer;
begin
  I := Length(HandleArray);
  SetLength(Result, I);
  CopyMemory(Result, @HandleArray, I * SizeOf(Integer));
end;

function HandleCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  HandleArray: TIntegerDynArray absolute Target;
begin
  Result := CompareValue(HandleArray[BIndex], HandleArray[AIndex]);
end;

procedure HandleExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Integer;
  HandleArray: TIntegerDynArray absolute Target;
begin
  I := HandleArray[AIndex];
  HandleArray[AIndex] := HandleArray[BIndex];
  HandleArray[BIndex] := I;
end;

procedure SortHandleArray(var HandleArray: TIntegerDynArray);
begin
  QSort(HandleArray, Low(HandleArray), High(HandleArray), HandleCompare, HandleExchange);
end;

function ParseScript(Index: NativeInt; const Method: TScriptMethod; const Data: Pointer): Boolean;
var
  I, J, K: NativeInt;
  Header: PScriptHeader absolute I;
  ItemHeader: PItemHeader absolute J;
  Item: Pointer absolute K;
begin
  if Assigned(Method) then
  begin
    I := Index;
    Inc(Index, Header.HeaderSize);
    while Index - I < Header.ScriptSize do
    begin
      J := Index;
      Inc(Index, SizeOf(TItemHeader));
      while Index - J < ItemHeader.Size do
      begin
        K := Index;
        if not Method(Index, Header, ItemHeader, Item, Data) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;
  Result := True;
end;

procedure BuildScript(const Parser: TCustomParser; const ItemArray: TScriptArray; out Script: TScript; const Optimize: Boolean; Number: PNumber);
var
  ANumber: TNumber;
  I, J, K: Integer;
  AItemArray: TScriptArray;
  ItemHeader: PItemHeader;
  Item: PScriptItem;
  FlagArray: TIntegerDynArray;
  Header: PScriptHeader absolute Script;
begin
  if not Assigned(Number) then
  begin
    FillChar(ANumber, SizeOf(TNumber), 0);
    Number := @ANumber;
  end;
  try
    if Optimize then
    begin
      for I := Low(ItemArray) to High(ItemArray) do
        if Assigned(ItemArray[I]) then
          if Optimal(ItemArray[I], stScriptItem) then
            IncNumber(Number^, Parser, ItemArray[I], stScriptItem)
          else
            Add(AItemArray, ItemArray[I]);
      if Number.Alive then
      begin
        Script := GetScriptFromValue(Parser, Number^, stScriptItem);
        try
          Add(AItemArray, Script);
        finally
          Script := nil;
        end;
      end;
    end
    else AItemArray := ItemArray;
    Script := nil;
    K := 0;
    try
      for I := Low(AItemArray) to High(AItemArray) do
        if Assigned(AItemArray[I]) then
        begin
          ItemHeader := Pointer(AItemArray[I]);
          J := SizeOf(TItemHeader);
          while J < ItemHeader.Size do
          begin
            Item := @AItemArray[I][J];
            case Item.Code of
              NumberCode:
                Inc(J, SizeOf(TCode) + SizeOf(TScriptNumber));
              FunctionCode:
                Inc(J, SizeOf(TCode) + SizeOf(TScriptFunction));
              StringCode:
                Inc(J, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
              ScriptCode:
                begin
                  Add(FlagArray, Integer(K + J + SizeOf(TCode)));
                  Inc(J, SizeOf(TCode) + Item.Script.Header.ScriptSize);
                end;
              ParameterCode:
                Inc(J, SizeOf(TCode) + Item.Script.Header.ScriptSize);
            else raise Error(ScriptError);
            end;
          end;
          Inc(K, ItemHeader.Size);
        end;
      I := Length(FlagArray);
      K := SizeOf(TScriptHeader) + I * SizeOf(Integer);
      Resize(Script, K);
      Header.ScriptCount := I;
      J := SizeOf(TScriptHeader);
      for I := Low(FlagArray) to High(FlagArray) do
      begin
        PInteger(@Script[J])^ := K + FlagArray[I];
        Inc(J, SizeOf(Integer));
      end;
      for I := Low(AItemArray) to High(AItemArray) do Add(Script, AItemArray[I]);
      Header.ScriptSize := Length(Script);
      Header.HeaderSize := K;
    finally
      FlagArray := nil;
    end;
  finally
    Delete(AItemArray);
  end;
end;

function CheckFHandle(const FData: PFunctionData; const Handle: Integer): Boolean;
begin
  Result := (Handle >= Low(FData.FArray)) and (Handle <= High(FData.FArray));
end;

function CheckTHandle(const TData: PTypeData; const Handle: Integer): Boolean;
begin
  Result := (Handle >= Low(TData.TArray)) and (Handle <= High(TData.TArray));
end;

{$WARNINGS OFF}
function RestoreText(const Parser: TCustomParser; const FromText: string; out ToText: string; const SA: TScriptArray; const Parameter: Boolean = False): Integer;
var
  Builder: TTextBuilder;
  StringArray: TStringDynArray;
  I, J: Integer;
  Script: TScript;
  S: string;
begin
  Builder := TTextBuilder.Create;
  try
    Split(FromText, BracketArray[btBrace][btLeft], StringArray, True);
    try
      Result := 0;
      for I := Low(StringArray) to High(StringArray) do
        if Contains(StringArray[I], BracketArray[btBrace][btRight]) then
        begin
          J := GetISIndex(StringArray[I], nil);
          if Builder.Size = 0 then
            Builder.Append(Parser.ScriptToString(SA[J], rmUser) + StringArray[I])
          else begin
            if Parameter then
            begin
              Script := ParameterToScript(SA[J], Parser.DefaultTypeHandle);
              try
                S := Parser.ScriptToString(Script, rmUser);
              finally
                Script := nil;
              end;
            end
            else S := Parser.ScriptToString(SA[J], rmUser);
            Builder.Append(Embrace(S + StringArray[I], Parser.Bracket));
          end;
          Inc(Result);
        end
        else Builder.Append(StringArray[I]);
    finally
      StringArray := nil;
    end;
    ToText := Builder.Text;
  finally
    Builder.Free;
  end;
end;
{$WARNINGS ON}

function RestoreText(const Parser: TCustomParser; const Text: string; const SA: TScriptArray; const Parameter: Boolean = False): string;
begin
  RestoreText(Parser, Text, Result, SA, Parameter);
end;

function RestoreText(const Parser: TCustomParser; const TextArray: TTextItemArray; const SA: TScriptArray;
  const Parameter: Boolean): string;
var
  Builder: TTextBuilder;
  I: Integer;
  S: string;
begin
  Builder := TTextBuilder.Create;
  try
    for I := Low(TextArray) to High(TextArray) do
    begin
      if TextArray[I].THandle >= 0 then Builder.Append(Parser.TData.TArray[TextArray[I].THandle].Name, Space);
      if RestoreText(Parser, TextArray[I].Text, S, SA, Parameter) > 0 then S := Embrace(S, Parser.Bracket);
      Builder.Append(S, Space);
    end;
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

procedure Read(const Parser: TCustomParser; var Index: NativeInt; out AFunction: PFunction);
var
  Item: PScriptItem absolute Index;
begin
  if (Item.Code = FunctionCode) and Parser.GetFunction(Item.ScriptFunction.Handle, AFunction) then
    Inc(Index, SizeOf(TCode) + SizeOf(TScriptFunction))
  else
    raise Error(ScriptError);
end;

function ParameterToScript(const Script: TScript; const THandle: Integer): TScript;
begin
  Resize(Result, SizeOf(TScriptHeader) + SizeOf(TItemHeader));
  WriteScript(Result, Script, True);
  with PItemHeader(@Result[SizeOf(TScriptHeader)])^ do
  begin
    Size := Length(Result) - SizeOf(TScriptHeader);
    UserType.Handle := THandle;
  end;
  PScriptHeader(Result).ScriptSize := Length(Script);
end;

{$WARNINGS OFF}
function GetScriptFromValue(const Parser: TCustomParser; Value: TValue; const TypeFlag: Boolean; const ScriptType: TScriptType): TScript;
var
  ItemHeader: PItemHeader absolute Result;
  Script: TScript;
  ScriptHeader: PScriptHeader absolute Result;
begin
  Result := nil;
  case ScriptType of
    stScriptItem:
      begin
        Resize(Result, SizeOf(TItemHeader));
        ItemHeader.Size := SizeOf(TItemHeader) + SizeOf(TCode) + SizeOf(TScriptNumber);
        ItemHeader.Sign := Ord(LessZero(Value));
        Value := Positive(Value);
        ItemHeader.UserType.Active := TypeFlag;
        ItemHeader.UserType.Handle := MakeTypeHandle(Parser.TData^, Value.ValueType, Parser.DefaultTypeHandle);
        WriteNumber(Result, Value);
      end;
    stScript:
      begin
        Resize(Result, SizeOf(TScriptHeader));
        Script := GetScriptFromValue(Parser, Value, TypeFlag, stScriptItem);
        try
          Add(Result, Script, Length(Script));
        finally
          Script := nil;
        end;
        ScriptHeader.ScriptSize := Length(Result);
        ScriptHeader.HeaderSize := SizeOf(TScriptHeader);
      end;
  end;
end;
{$WARNINGS ON}

function GetScriptFromValue(const Parser: TCustomParser; const Number: TNumber; const ScriptType: TScriptType): TScript;
begin
  Result := GetScriptFromValue(Parser, Number.Value, False, ScriptType);
end;

function GetValueFromScript(const Parser: TCustomParser; const Script: TScript; const ScriptType: TScriptType; const UserType: PUserType): TValue;
var
  ItemHeader: PItemHeader absolute Script;
begin
  case ScriptType of
    stScriptItem:
      begin
        Result := PScriptItem(@Script[SizeOf(TItemHeader)]).ScriptNumber.Value;
        if Assigned(UserType) then UserType^ := ItemHeader.UserType;
        if Boolean(ItemHeader.Sign) then Result := Negative(Result);
      end;
    stScript:
      Result := GetValueFromScript(Parser, @Script[SizeOf(TScriptHeader)], stScriptItem, UserType);
  end;
end;

function GetParameter(const Parser: TCustomParser; const Parameter: TParameter): TParameter;
var
  I, J: NativeInt;
  Item: PScriptItem absolute I;
  ItemHeader: PItemHeader;
begin
  with Parameter.Value do
  begin
    I := Int64Rec.Lo;
    ItemHeader := PItemHeader(Int64Rec.Hi);
  end;
  Result.THandle := ItemHeader.UserType.Handle;
  case Item.Code of
    NumberCode:
      Result.Value := Item.ScriptNumber.Value;
    FunctionCode:
      Result.Value := Parser.ExecuteFunction(I, ItemHeader, EmptyValue);
    StringCode:
      begin
        J := Item.ScriptString.Size;
        if J > SizeOf(TString) then J := SizeOf(TString);
        ZeroMemory(@Result.Text, SizeOf(TString));
        CopyMemory(@Result.Text, PAnsiChar(I) + SizeOf(TCode) + SizeOf(TScriptString), J);
      end;
    ScriptCode:
      Result.Value := Parser.Execute(Integer(@Item.Script.Header))^;
  else raise ParseErrors.Error(ScriptError);
  end;
  if (Item.Code <> StringCode) and Boolean(ItemHeader.Sign) then
    Result.Value := Negative(Result.Value);
end;

function AsValue(const Parser: TCustomParser; const Parameter: TParameter): TValue;
begin
  Result := GetParameter(Parser, Parameter).Value;
end;

function AsByte(const Parser: TCustomParser; const Parameter: TParameter): Byte;
begin
  Result := Convert(AsValue(Parser, Parameter), vtByte).Unsigned8;
end;

function AsShortint(const Parser: TCustomParser; const Parameter: TParameter): Shortint;
begin
  Result := Convert(AsValue(Parser, Parameter), vtShortint).Signed8;
end;

function AsWord(const Parser: TCustomParser; const Parameter: TParameter): Word;
begin
  Result := Convert(AsValue(Parser, Parameter), vtWord).Unsigned16;
end;

function AsSmallint(const Parser: TCustomParser; const Parameter: TParameter): Smallint;
begin
  Result := Convert(AsValue(Parser, Parameter), vtSmallint).Signed16;
end;

function AsLongword(const Parser: TCustomParser; const Parameter: TParameter): Longword;
begin
  Result := Convert(AsValue(Parser, Parameter), vtLongword).Unsigned32;
end;

function AsInteger(const Parser: TCustomParser; const Parameter: TParameter): Integer;
begin
  Result := Convert(AsValue(Parser, Parameter), vtInteger).Signed32;
end;

function AsInt64(const Parser: TCustomParser; const Parameter: TParameter): Int64;
begin
  Result := Convert(AsValue(Parser, Parameter), vtInt64).Signed64;
end;

function AsSingle(const Parser: TCustomParser; const Parameter: TParameter): Single;
begin
  Result := Convert(AsValue(Parser, Parameter), vtSingle).Float32;
end;

function AsDouble(const Parser: TCustomParser; const Parameter: TParameter): Double;
begin
  Result := Convert(AsValue(Parser, Parameter), vtDouble).Float64;
end;

function AsExtended(const Parser: TCustomParser; const Parameter: TParameter): Extended;
begin
  Result := Convert(AsValue(Parser, Parameter), vtExtended).Float80;
end;

function AsBoolean(const Parser: TCustomParser; const Parameter: TParameter): Boolean;
begin
  Result := Boolean(Convert(AsValue(Parser, Parameter), vtInteger).Signed32);
end;

function AsText(const Parser: TCustomParser; const Parameter: TParameter): string;
begin
  Result := GetParameter(Parser, Parameter).Text;
end;

function AsBoolean(const Parser: TCustomParser; const ParameterArray: TParameterArray;
  const Index: Integer; const Default: Boolean): Boolean;
begin
  if Default then
    Result := (High(ParameterArray) < Index) or AsBoolean(Parser, ParameterArray[Index])
  else
    Result := (High(ParameterArray) >= Index) and AsBoolean(Parser, ParameterArray[Index]);
end;

function Optimal(const Script: TScript; const ScriptType: TScriptType): Boolean;
const
  ScriptSize: array[TScriptType] of Integer = (SizeOf(TItemHeader) + SizeOf(TCode) + SizeOf(TScriptNumber), SizeOf(TScriptHeader) +
    SizeOf(TItemHeader) + SizeOf(TCode) + SizeOf(TScriptNumber));
  CodeOffset: array[TScriptType] of Integer = (SizeOf(TItemHeader), SizeOf(TScriptHeader) + SizeOf(TItemHeader));
begin
  Result := Assigned(Script);
  if Result then
    case ScriptType of
      stScriptItem:
        Result := (PItemHeader(Script).Size = ScriptSize[ScriptType]) and (PInteger(@Script[CodeOffset[ScriptType]])^ = NumberCode);
      stScript:
        Result := (PScriptHeader(Script).ScriptSize = ScriptSize[ScriptType]) and (PInteger(@Script[CodeOffset[ScriptType]])^ = NumberCode);
    end;
end;

function TypeFlag(const Script: TScript; const ScriptType: TScriptType): Boolean;
begin
  Result := Optimal(Script, stScript);
  if Result then
    case ScriptType of
      stScriptItem:
        Result := PItemHeader(Script).UserType.Active;
      stScript:
        Result := PItemHeader(@Script[SizeOf(TScriptHeader)]).UserType.Active;
    end;
end;

function GetFunction(const Data: PFunctionData; Index: Integer; out AFunction: PFunction): Boolean;
begin
  Result := (Index >= Low(Data.FOrder)) and (Index <= High(Data.FOrder)) and (Data.FOrder[Index] >= Low(Data.FArray)) and
    (Data.FOrder[Index] <= High(Data.FArray));
  if Result then
    AFunction := @Data.FArray[Data.FOrder[Index]]
  else
    AFunction := nil;
end;

function FunctionByIndex(const Data: PFunctionData; const Index: Integer): PFunction;
begin
  GetFunction(Data, Index, Result);
end;

function GetFlagArray(const Text: string; const LockType: TLockType; const FData: PFunctionData; const TData: PTypeData; out FlagArray: TFunctionFlagArray;
  const CleanText: PString; const TypeArray: PHandleArray; const Method: TFunctionFlagMethod): Boolean;
type
  TItem = record
    Index: Integer;
    THandle: PInteger;
  end;

  function MakeTemplate: string;
  const
    Template = '(0x%x)(0x%x)(0x%x)(%s)';
  begin
    Result := Format(Template, [Integer(FData), Integer(TData), Integer(@Method), Text]);
  end;

  procedure Resize(var Target: THandleArray; const Size: Integer);
  var
    ASize, I: Integer;
  begin
    ASize := Length(Target);
    SetLength(Target, Size);
    for I := ASize to Size - 1 do Target[I] := -1;
  end;

  function MakeItem(const AIndex: Integer; const AHandle: PInteger): TItem;
  begin
    FillChar(Result, SizeOf(TItem), 0);
    with Result do
    begin
      Index := AIndex;
      THandle := AHandle;
    end;
  end;

  function Add(const AText: string; const BText: TTextBuilder; const Index: Integer; var Prior: TItem;
    const AType: PType; var ATypeArray: THandleArray): Boolean; overload;
  var
    I: Integer;
  begin
    Result := Assigned(AType);
    if Result then
    begin
      if Assigned(Prior.THandle) then
        I := Prior.Index + Integer(StrLen(TData.TArray[Prior.THandle^].Name))
      else
        I := 1;
      BText.Append(Trim(Copy(AText, I, Index - I)), Space);
      if BText.Size > 0 then
        I := BText.Size + 1
      else
        I := 0;
      Resize(ATypeArray, I + 1);
      ATypeArray[I] := AType.Handle^;
    end;
  end;

  function Add(const Index, FHandle: Integer): Boolean; overload;
  begin
    Result := not Assigned(Method) or not CheckFHandle(FData, FHandle) or Method(@FData.FArray[FHandle]);
    if Result then ParseUtils.Add(FlagArray, Index, FHandle);
  end;

var
  AText: string;
  TextArray: TTextArray;
  BText: TTextBuilder;
  ATypeArray: THandleArray;
  Prior: TItem;
  I, J, K, Count: Integer;
  AType: PType;
  AFunction: PFunction;
begin
  if not Assigned(FData.FlagCache) or not TFlagCache(FData.FlagCache).Find(MakeTemplate, FlagArray, CleanText, TypeArray) then
  begin
    Prepare(FData);
    AText := Text;
    TextArray := GetTextArray(AText, LockType);
    try
      ATypeArray := nil;
      if Assigned(TData) then
      begin
        BText := TTextBuilder.Create;
        try
          FillChar(Prior, SizeOf(TItem), 0);
          I := 1;
          Count := Length(AText);
          while I <= Count do if Locked(I, TextArray) or CharInSet(AText[I], Blanks) then Inc(I)
          else begin
            J := Low(TData.TOrder);
            K := 1;
            while GetType(TData, J, AType) do
            begin
              K := StrLen(AType.Name);
              if (K = 0) or (I + K - 1 > Count) or not Whole(AText, I, K) or not SameText(AType.Name, @AText[I], K) then
              begin
                Inc(J);
                K := 1;
              end
              else begin
                Add(AText, BText, I, Prior, AType, ATypeArray);
                Prior := MakeItem(I, AType.Handle);
                Break;
              end;
            end;
            Inc(I, K);
          end;
          if Assigned(Prior.THandle) then
            I := Prior.Index + Integer(StrLen(TData.TArray[Prior.THandle^].Name))
          else
            I := 1;
          BText.Append(Trim(Copy(AText, I, MaxInt)), Space);
          AText := BText.Text;
        finally
          BText.Free;
        end;
      end;
      if Assigned(CleanText) then CleanText^ := AText;
      if Assigned(TypeArray) then TypeArray^ := ATypeArray;
      FlagArray := nil;
      I := 1;
      Count := Length(AText);
      while I <= Count do if Locked(I, TextArray) or CharInSet(AText[I], Blanks) then Inc(I)
      else begin
        J := Low(FData.FOrder);
        K := 1;
        while GetFunction(FData, J, AFunction) do
        begin
          K := StrLen(AFunction.Name);
          if (K = 0) or (I + K - 1 > Count) or not SameText(AFunction.Name, @AText[I], K) then
          begin
            Inc(J);
            K := 1;
          end
          else begin
            if Assigned(AFunction.Handle) then
              Add(I, AFunction.Handle^)
            else
              Add(I, -1);
            Break;
          end;
        end;
        Inc(I, K);
      end;
    finally
      TextArray := nil;
    end;
    if Assigned(FData.FlagCache) then TFlagCache(FData.FlagCache).Add(MakeTemplate, FlagArray, CleanText, TypeArray);
  end;
  Result := Assigned(FlagArray);
end;

function GetItemArray(const Text: string; const FData: PFunctionData; const TData: PTypeData; out ItemArray: TTextItemArray;
  const InternalHandle: Integer; const LockType: TLockType): Boolean;
var
  FlagArray: TFunctionFlagArray;
  AText, S: string;
  TypeArray: THandleArray;
  I, J, K, FHandle: Integer;

  function GetHandle(const Index: Integer): Integer;
  begin
    if (Index < Low(TypeArray)) or (Index > High(TypeArray)) then
      Result := -1
    else
      Result := TypeArray[Index];
  end;

begin
  ItemArray := nil;
  if not Assigned(FData.ItemCache) or not TItemCache(FData.ItemCache).Find(Text, ItemArray) then
  begin
    if GetFlagArray(Text, LockType, FData, TData, FlagArray, @AText, @TypeArray) then
    try
      for I := Low(FlagArray) to Length(FlagArray) do
      begin
        if I > Low(FlagArray) then
        begin
          J := FlagArray[I - 1].Index;
          if I < Length(FlagArray) then
            K := FlagArray[I].Index - J
          else
            K := MaxInt;
          FHandle := FlagArray[I - 1].Handle;
        end
        else begin
          J := 1;
          K := FlagArray[I].Index - 1;
          FHandle := -1;
        end;
        S := Copy(AText, J, K);
        if Trim(S) <> '' then
          if (FHandle < 0) or (InternalHandle < 0) or (FHandle = InternalHandle) then
            Add(ItemArray, Trim(S), FHandle, GetHandle(J - 1))
          else begin
            Add(ItemArray, FData.FArray[FHandle].Name, FHandle, GetHandle(J - 1));
            J := StrLen(FData.FArray[FHandle].Name) + 1;
            K := Length(S);
            while (J <= K) and (S[J] = Space) do Inc(J);
            S := Trim(Copy(S, J, MaxInt));
            if S <> '' then
            begin
              if I > Low(FlagArray) then Inc(J, FlagArray[I - 1].Index - 1);
              Add(ItemArray, S, -1, GetHandle(J - 1));
            end;
          end;
      end;
    finally
      FlagArray := nil;
      TypeArray := nil;
    end;
    if not Assigned(ItemArray) then
    begin
      S := Trim(AText);
      if S <> '' then Add(ItemArray, S, -1, GetHandle(0));
    end;
    if Assigned(FData.ItemCache) then TItemCache(FData.ItemCache).Add(Text, ItemArray);
  end;
  Result := Assigned(ItemArray);
end;

function TrimPOperator(var Text: string): Boolean;
var
  I: Integer;
begin
  repeat
    for I := Ord(Low(POperatorArray)) to Ord(High(POperatorArray)) do
    begin
      Result := TrimText(Text, POperatorArray[TParameterOperator(I)]);
      if Result then Break;
    end;
  until not Result;
end;

function FindSign(const Text: string; var Index: Integer; out Sign: Boolean): Boolean;
var
  J: TTextOperator;
begin
  Result := (Index >= 0) and (Index <= Length(Text));
  if Result then
  begin
    while (Index <= Length(Text)) and CharInSet(Text[Index], Blanks) do Inc(Index);
    for J := Low(TOperatorArray) to High(TOperatorArray) do
      if Text[Index] = TOperatorArray[J] then
      begin
        Sign := J = toNegative;
        Exit;
      end;
    Result := False;
  end;
end;

function GetSign(const Text: string; const Index: PInteger): Boolean; overload;
var
  I: Integer;
  Sign, Flag: Boolean;
begin
  Result := False;
  if Assigned(Index) then
    I := Index^
  else
    I := 1;
  repeat
    Flag := FindSign(Text, I, Sign);
    if Flag then
    begin
      if Result then
        Result := Result xor Sign
      else
        Result := Sign;
      Inc(I);
    end;
  until not Flag;
  if Assigned(Index) then Index^ := I;
end;

function GetSign(const Item: TTextItem; const TOHArray: TIntegerDynArray): Boolean;
begin
  Result := Item.FHandle = TOHArray[Integer(toNegative)];
end;

function SetSign(const Text: string): string;
var
  I: Integer;
begin
  I := 1;
  if GetSign(Text, @I) then
    Result := TOperatorArray[toNegative] + Copy(Text, I, MaxInt)
  else
    Result := Copy(Text, I, MaxInt);
end;

function TrimFunction(const Data: PFunctionData; var Text: string; const Trim: Boolean): PFunction;
var
  I: Integer;
  AFunction: PFunction;
begin
  Prepare(Data);
  for I := Low(Data.FOrder) to High(Data.FOrder) do
    if GetFunction(Data, I, AFunction) and TrimText(Text, AFunction.Name, Trim) then
    begin
      Result := AFunction;
      Exit;
    end;
  Result := nil;
end;

function TrimType(const Data: PTypeData; const StringHandle: Integer; var Text: string; const Trim: Boolean): PType;
var
  I: Integer;
  AType: PType;
begin
  if InQuote(Text) then Result := @Data.TArray[StringHandle]
  else begin
    for I := Low(Data.TOrder) to High(Data.TOrder) do
      if GetType(Data, I, AType) and Whole(Text, 1, StrLen(AType.Name)) and TrimText(Text, AType.Name, Trim) then
      begin
        Result := AType;
        Exit;
      end;
    Result := nil;
  end;
end;

procedure WriteItemHeader(const ItemHeader: PItemHeader; const ASign: Boolean; const THandle, DefaultTypeHandle: Integer);
begin
  ItemHeader.Sign := Ord(ASign);
  if THandle < 0 then
    ItemHeader.UserType.Handle := DefaultTypeHandle
  else
    ItemHeader.UserType.Handle := THandle;
end;

function GetISIndex(const Text: string; out Parameter: Boolean): Integer;
var
  S: string;
begin
  S := Extract(Text, BracketArray[btBrace][btLeft], BIndex, True);
  Parameter := TrimText(S, ParameterPrefix);
  Result := StrToInt(Extract(S, BracketArray[btBrace][btRight], AIndex, True));
end;

function GetISIndex(var Text: string; const Parameter: PBoolean): Integer;
var
  I: Integer;
begin
  if Assigned(Parameter) then
    Parameter^ := TrimText(Text, ParameterPrefix)
  else
    TrimText(Text, ParameterPrefix);
  I := Pos(BracketArray[btBrace][btRight], Text);
  if I > 0 then
  begin
    Result := StrToInt(Copy(Text, 1, I - 1));
    System.Delete(Text, 1, I);
  end
  else Result := StrToInt(Text);
end;

function GetIS(const Text: string; const SA: TScriptArray; out Parameter: Boolean): TScript;
var
  I: Integer;
begin
  I := GetISIndex(Text, Parameter);
  if (I < Low(SA)) or (I > High(SA)) then
    Result := nil
  else
    Result := SA[I];
end;

function GetLockArray(const Text: string; const LockBracket: TBracket): TLockArray;
var
  I, J: Integer;
begin
  Result := nil;
  I := 1;
  while I < Length(Text) do
    if Text[I] = LockBracket[btLeft] then
    begin
      J := NextBracket(Text, LockBracket, I);
      if J > 0 then
      begin
        Add(Result, I, J);
        I := J;
      end
      else Inc(I);
    end
    else Inc(I);
end;

function GetLockArray(const Text: string; const LockChar: Char): TLockArray;
var
  I, J: Integer;
begin
  Result := nil;
  J := -1;
  for I := 1 to Length(Text) do
    if (Text[I] = LockChar) and ((I = 1) or (Text[I - 1] <> LockChar)) and
      ((I = Length(Text)) or (Text[I + 1] <> LockChar)) then
        if J < 0 then J := I
        else begin
          Add(Result, J, I);
          J := -1;
        end;
end;

function GetTextArray(const Text: string; const LockBracket: TBracket): TTextArray;
var
  LockArray: TLockArray;
begin
  LockArray := GetLockArray(Text, LockBracket);
  try
    Result := GetTextArray(Text, LockArray);
  finally
    LockArray := nil;
  end;
end;

function GetTextArray(const Text: string; const LockChar: Char): TTextArray;
var
  LockArray: TLockArray;
begin
  LockArray := GetLockArray(Text, LockChar);
  try
    Result := GetTextArray(Text, LockArray);
  finally
    LockArray := nil;
  end;
end;

function GetTextArray(const Text: string; const LockArray: TLockArray): TTextArray;
var
  I, J: Integer;
begin
  Resize(Result, Length(Text));
  for I := Low(LockArray) to High(LockArray) do
    for J := LockArray[I].FromIndex to LockArray[I].TillIndex - 1 do Result[J] := Ord(True);
end;

function GetTextArray(const Text: string; const LockType: TLockType): TTextArray;
begin
  case LockType of
    ltBracket: Result := GetTextArray(Text, LockBracket);
    ltChar: Result := GetTextArray(Text, LockChar);
  end;
end;

function Locked(const Index: Integer; const LockArray: TLockArray): Boolean;
var
  I: Integer;
begin
  for I := Low(LockArray) to High(LockArray) do
    if (Index >= LockArray[I].FromIndex) and (Index <= LockArray[I].TillIndex) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function Locked(const Index: Integer; const TextArray: TTextArray): Boolean;
begin
  Result := (Index < 1) or (Index > Length(TextArray)) or Boolean(TextArray[Index - 1]);
end;

procedure ParseBracket(var Text: string; const Bracket: TBracket; const Method: TBracketMethod; const Data: Pointer; out Error: TError);
var
  I, J: Integer;
  TextArray: TTextArray;
  BD: TBD;

  procedure Reset;
  begin
    I := 1;
    TextArray := GetTextArray(Text, ltChar);
    FillChar(BD, SizeOf(TBD), 0);
    BD.FromIndex := MaxInt;
  end;

begin
  FillChar(Error, SizeOf(TError), 0);
  if Assigned(Method) then
    if STCount(Text, Bracket[btLeft], True) <> STCount(Text, Bracket[btRight], True) then
      Error := MakeError(etBracketError, BracketError)
    else
      try
        Reset;
        J := Length(Text);
        while I <= J do
        begin
          if not Locked(I, TextArray) then
            if Text[I] = Bracket[btLeft] then
            begin
              if BD.FromIndex > I then BD.FromIndex := I;
              Inc(BD.FromCount);
            end
            else if Text[I] = Bracket[btRight] then
            begin
              BD.TillIndex := I;
              Inc(BD.TillCount);
            end;
          if (BD.FromCount > 0) and (BD.FromCount = BD.TillCount) and (BD.FromIndex < BD.TillIndex) then
          begin
            Error := Method(Text, BD.FromIndex, BD.TillIndex, Data);
            if Error.ErrorType = etSuccess then
              Reset
            else
              Break
          end
          else Inc(I);
        end;
      finally
        TextArray := nil;
      end;
end;

procedure ParseBracket(var Text: string; const Bracket: TBracket; const Method: TBracketMethod; const Data: Pointer);
var
  Error: TError;
begin
  ParseBracket(Text, Bracket, Method, Data, Error);
  if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
end;

function NextBracket(const Text: string; const Bracket: TBracket; Index: Integer): Integer;
var
  I, J, K: Integer;
begin
  Result := 0;
  I := 0;
  J := 0;
  K := Length(Text);
  while Index <= K do
  begin
    if Text[Index] = Bracket[btLeft] then
      Inc(I)
    else
      if Text[Index] = Bracket[btRight] then Inc(J);
    if (I > 0) and (I = J) then
    begin
      Result := Index;
      Break;
    end
    else Inc(Index);
  end;
end;

function PrevBracket(const Text: string; const Bracket: TBracket; Index: Integer): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  I := 0;
  J := 0;
  while Index > 0 do
  begin
    if Text[Index] = Bracket[btLeft] then
      Inc(I)
    else
      if Text[Index] = Bracket[btRight] then Inc(J);
    if (I > 0) and (I = J) then
    begin
      Result := Index;
      Break;
    end
    else Dec(Index);
  end;
end;

function Embrace(const Text: string; const Bracket: TBracket): string;
begin
  Result := Bracket[btLeft] + Text + Bracket[btRight];
end;

function InBrace(const Text: string; const Bracket: TBracket): Boolean;
var
  I: Integer;
begin
  I := Length(Text);
  Result := (I >= Length(Bracket)) and (Text[1] = Bracket[btLeft]) and (Text[I] = Bracket[btRight]);
end;

procedure DeleteBracket(var Text: string; const Index: Integer; const Bracket: TBracket);
var
  I, J: Integer;
begin
  for I := Index to Length(Text) do
  begin
    if Text[I] = Bracket[btLeft] then
    begin
      J := NextBracket(Text, Bracket, Index);
      if J > 0 then
      begin
        System.Delete(Text, I, 1);
        System.Delete(Text, J - 1, 1);
      end;
      Break;
    end
    else if not CharInSet(Text[I], Blanks) then Break;
  end;
end;

procedure ChangeBracket(var Text: string; const Index: Integer; const BracketFrom, BracketTo: TBracket);
var
  I, J: Integer;
begin
  for I := Index to Length(Text) do
  begin
    if Text[I] = BracketFrom[btLeft] then
    begin
      J := NextBracket(Text, BracketFrom, Index);
      if J > 0 then
      begin
        Text[I] := BracketTo[btLeft];
        Text[J] := BracketTo[btRight];
      end;
      Break;
    end
    else if not CharInSet(Text[I], Blanks) then Break;
  end;
end;

function BracketFunction(const AFunction: PFunction): Boolean;
begin
  Result := AFunction.Method.Parameter.Count > 0;
end;

procedure ChangeBracket(var Text: string; const Data: PFunctionData; const BracketFrom, BracketTo: TBracket);
var
  FlagArray: TFunctionFlagArray;
  I: Integer;
begin
  if GetFlagArray(Text, ltChar, Data, nil, FlagArray, nil, nil, BracketFunction) then
  try
    for I := Low(FlagArray) to High(FlagArray) do
      ChangeBracket(Text, FlagArray[I].Index + Integer(StrLen(Data.FArray[FlagArray[I].Handle].Name)), BracketFrom, BracketTo);
  finally
    FlagArray := nil;
  end;
end;

function Lower(const Text: string): string;
var
  Builder: TTextBuilder;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    for I := 0 to STCount(Text, LockChar, True) do
      if Odd(I) then
        Builder.Append(SubText(Text, LockChar, I, False))
      else
        Builder.Append(LowerCase(SubText(Text, LockChar, I, False)));
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function Upper(const Text: string): string;
var
  Builder: TTextBuilder;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    for I := 0 to STCount(Text, LockChar, True) do
      if Odd(I) then
        Builder.Append(SubText(Text, LockChar, I, False))
      else
        Builder.Append(UpperCase(SubText(Text, LockChar, I, False)));
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function Whole(const Text: string; const Index, Count: Integer): Boolean;
begin
  Result := (Index + Count - 1 <= Length(Text)) and
    ((Index = 1) or CharInSet(Text[Index - 1], DelimiterArray)) and
    ((Index + Count - 1 = Length(Text)) or
    CharInSet(Text[Index + Count], DelimiterArray));
end;

function FollowingText(const Text: string; const Index: Integer): string;
var
  I, J: Integer;
begin
  I := Index;
  J := Length(Text);
  while (I < J) and not CharInSet(Text[I + 1], DelimiterArray) do Inc(I);
  Result := Copy(Text, Index, I - Index + 1);
end;

function PrecedingText(const Text: string; const Index: Integer): string;
var
  I: Integer;
begin
  I := Index;
  while (I > 1) and not CharInSet(Text[I - 1], DelimiterArray) do Dec(I);
  Result := Copy(Text, I, Index - I);
end;

function WholeValue(const Parser: TCustomParser; const Text: string; const Index, Count: Integer): Boolean;
var
  I, J: Integer;
  FlagArray: TFunctionFlagArray;
  Flag: PFunctionFlag;

  function Check(const S: string): Boolean;
  begin
    Result := (Trim(S) = '') or Assigned(Parser) and not Assigned(Parser.FindFunction(S)) and not Assigned(Parser.FindType(S));
  end;

begin
  Result := Whole(Text, Index, Count);
  if not Result then
  begin
    Result := Check(PrecedingText(Text, Index)) and Check(FollowingText(Text, Index + Count));
    if Result then
    begin
      GetFlagArray(Text, ltChar, Parser.FData, nil, FlagArray);
      try
        J := Index + Count;
        for I := Low(FlagArray) to High(FlagArray) do
        begin
          Flag := @FlagArray[I];
          if (Flag.Index <= Index) and (Flag.Index + Integer(StrLen(Parser.FData.FArray[Flag.Handle].Name)) >= J) then
          begin
            Result := False;
            Exit;
          end;
        end;
      finally
        FlagArray := nil;
      end;
    end;
  end;
end;

function GotoBody(const Data: string; const Bracket: TBracket; var Index: Integer; const Count: Integer): Boolean;
var
  I: Integer;
begin
  Result := Whole(Data, Index, Count);
  if Result then
  begin
    Inc(Index, Count);
    I := Length(Data);
    while (Index <= I) and CharInSet(Data[Index], Blanks) do Inc(Index);
    Result := Data[Index] = Bracket[btLeft];
  end;
end;

function FindBody(const Name, Data: string; const Bracket: TBracket; out Index, Count: Integer): Boolean;
var
  TextArray: TTextArray;

  function Find: Boolean;
  var
    I: Integer;
  begin
    Result := Index > 0;
    if Result then
    begin
      Result := not Locked(Index, TextArray);
      I := Index;
      if Result then Result := GotoBody(Data, Bracket, Index, Count);
      if not Result then
      begin
        Index := PosEx(Name, Data, I + Count);
        Result := Find;
      end;
    end;
  end;

begin
  Index := Pos(Name, Data);
  Result := Index > 0;
  if Result then
  begin
    TextArray := GetTextArray(Data, Bracket);
    try
      Count := Length(Name);
      Result := Find;
    finally
      TextArray := nil;
    end;
    if Result then
    begin
      Count := NextBracket(Data, Bracket, Index) - Index;
      Result := Count > 0;
    end;
  end;
end;

function BodyText(const Name, Data: string; const Bracket: TBracket; out Text: string): Boolean;
var
  I, J: Integer;
begin
  Result := FindBody(Name, Data, Bracket, I, J);
  if Result then Text := Copy(Data, I + 1, J - 1)
  else Text := '';
end;

function Quote(const Text: string): string;
var
  I, J: Integer;
begin
  Result := Trim(Text);
  I := 1;
  J := Length(Result);
  while Result[I] = LockChar do Inc(I);
  if I > J then Result := ''
  else begin
    while Result[J] = LockChar do Dec(J);
    Result := Trim(Copy(Result, I, J - I + 1));
  end;
end;

function QuoteDouble(const Text, FunctionName: string; const Bracket: TBracket): string;
var
  I, J, AIndex, BIndex: Integer;
begin
  Result := Text;
  I := Pos(FunctionName, Result);
  if I > 0 then
  begin
    I := PosEx(Bracket[btLeft], Result, I + Length(FunctionName));
    if I > 0 then
    begin
      J := PosEx(LockChar, Result, I);
      if (J > 0) and (J < Length(Result)) and (Result[J + 1] = LockChar) then
      begin
        AIndex := J;
        I := NextBracket(Result, Bracket, I);
        while (I > 0) and (Result[I] <> LockChar) do Dec(I);
        BIndex := I;
        if (BIndex < Length(Result)) and (Result[BIndex] = LockChar) and
          (AIndex < BIndex) and (Result[BIndex] = LockChar) then
          begin
            System.Delete(Result, AIndex, 1);
            System.Delete(Result, BIndex - 1, 1);
          end;
      end;
    end;
  end;
end;

function InQuote(const Text: string): Boolean;
begin
  Result := (Length(Text) > 1) and (Text[1] = LockChar) and (Text[Length(Text)] = LockChar);
end;

function MakeTemplate(const Parser: TCustomParser; const Data: PFunctionData; const Text: string; const ValueArray: PValueArray;
  const NumberTemplate: string): string;
var
  Builder: TTextBuilder;
  TextArray: TTextArray;
  I, J, K, L, Count: Integer;
  Sign, Flag: Boolean;
  Value: TValue;
begin
  Builder := TTextBuilder.Create(Length(Text));
  try
    TextArray := GetTextArray(Text, ltChar);
    try
      I := 1;
      Count := Length(Text);
      while I <= Count do
        if Locked(I, TextArray) then
        begin
          Builder.Append(Text[I]);
          Inc(I)
        end
        else if CharInSet(Text[I], Signs + Digits) then
        begin
          K := I;
          Flag := CharInSet(Text[I], Signs);
          if Flag then
            Sign := GetSign(Text, @I)
          else
            Sign := False;
          while (I <= Count) and CharInSet(Text[I], Blanks) do Inc(I);
          J := I;
          L := 0;
          while (J <= Count) and CharInSet(Text[J], Digits) do
          begin
            Inc(J);
            if (J < Count) and (Text[J] = {$IFDEF DELPHI_XE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}) and
              CharInSet(Text[J + 1], Digits) then
                if L = 0 then
                begin
                  Inc(J);
                  Inc(L);
                end
                else Break;
          end;
          Dec(J, I);
          if WholeValue(Parser, Text, I, J) and TryTextToValue(Copy(Text, I, J), Value) then
          begin
            if Flag then
              Builder.Append(TOperatorArray[toPositive] + NumberTemplate)
            else
              Builder.Append(NumberTemplate);
            if Assigned(ValueArray) then
              if Sign then
                ValueTypes.Add(ValueArray^, ValueUtils.Negative(Value))
              else
                ValueTypes.Add(ValueArray^, Value);
          end
          else Builder.Append(Copy(Text, K, I + J - K));
          Inc(I, J);
        end
        else begin
          if not CharInSet(Text[I], Blanks) then Builder.Append(Text[I]);
          Inc(I);
        end;
      Result := Builder.Text;
      if Result = '' then Result := NumberChar[ntZero];
    finally
      TextArray := nil;
    end;
  finally
    Builder.Free;
  end;
end;

function WriteValue(Index: NativeInt; var ValueIndex: Integer; const ValueArray: TValueArray;
  const ScriptType: TScriptType): Boolean;
var
  I, J: NativeInt;
  Header: PScriptHeader absolute I;
  ItemHeader: PItemHeader absolute J;
  Item: PScriptItem absolute Index;
begin
  Result := Assigned(ValueArray) and (ValueIndex < Length(ValueArray));
  if Result then
    case ScriptType of
      stScriptItem:
        begin
          J := Index;
          Inc(Index, SizeOf(TItemHeader));
          ItemHeader.Sign := 0;
          while Index - J < ItemHeader.Size do
            case Item.Code of
              NumberCode:
                begin
                  if not Boolean(ItemHeader.Sign) then ItemHeader.Sign := Integer(LessZero(ValueArray[ValueIndex]));
                  Item.ScriptNumber.Value := Positive(ValueArray[ValueIndex]);
                  Inc(ValueIndex);
                  if ValueIndex > High(ValueArray) then Exit;
                  Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
                end;
              FunctionCode:
                Inc(Index, SizeOf(TCode) + SizeOf(TScriptFunction));
              StringCode:
                Inc(Index, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
              ScriptCode, ParameterCode:
                begin
                  WriteValue(Index + SizeOf(TCode), ValueIndex, ValueArray, ScriptType);
                  if ValueIndex > High(ValueArray) then Exit;
                  Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
                end;
            else raise Error(ScriptError);
            end;
        end;
      stScript:
        begin
          I := Index;
          Inc(Index, Header.HeaderSize);
          while Index - I < Header.ScriptSize do
          begin
            J := Index;
            Inc(Index, SizeOf(TItemHeader));
            ItemHeader.Sign := 0;
            while Index - J < ItemHeader.Size do
              case Item.Code of
                NumberCode:
                  begin
                    if not Boolean(ItemHeader.Sign) then ItemHeader.Sign := Integer(LessZero(ValueArray[ValueIndex]));
                    Item.ScriptNumber.Value := Positive(ValueArray[ValueIndex]);
                    Inc(ValueIndex);
                    if ValueIndex > High(ValueArray) then Exit;
                    Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
                  end;
                FunctionCode:
                  Inc(Index, SizeOf(TCode) + SizeOf(TScriptFunction));
                StringCode:
                  Inc(Index, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
                ScriptCode, ParameterCode:
                  begin
                    WriteValue(Index + SizeOf(TCode), ValueIndex, ValueArray, ScriptType);
                    if ValueIndex > High(ValueArray) then Exit;
                    Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
                  end;
              else raise Error(ScriptError);
              end;
          end;
        end;
    end;
end;

function Add(var Script: TScript; const Value: Smallint): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Smallint));
  PSmallint(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: Word): Integer;
begin
  Result := Add(Script, Smallint(Value));
end;

function Add(var Script: TScript; const Value: Integer): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Integer));
  PInteger(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: Longword): Integer;
begin
  Result := Add(Script, Integer(Value));
end;

function Add(var Script: TScript; const Value: Int64): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Int64));
  PInt64(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: Single): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Double));
  PDouble(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: Double): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Double));
  PDouble(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: Extended): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(Double));
  PDouble(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: TValue): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(TValue));
  PValue(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: TScriptNumber): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(TScriptNumber));
  PScriptNumber(@Script[Result])^ := Value;
end;

function Add(var Script: TScript; const Value: TScriptFunction): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + SizeOf(TScriptFunction));
  PScriptFunction(@Script[Result])^ := Value;
end;

function Add(var Target: TScript; const Source: TScript): Integer;
begin
  Result := Add(Target, Source, Length(Source));
end;

function Add(var Script: TScript; const Source: Pointer; const Count: Integer): Integer;
begin
  Result := Length(Script);
  SetLength(Script, Result + Count);
  CopyMemory(@Script[Result], Source, Count);
end;

function Add(var Script: TScript; const Text: string): Integer;
begin
  Result := Add(Script, PAnsiChar(Text), Length(Text) * SizeOf(Char));
end;

function Add(var SA: TScriptArray; const Value: TScript): Integer;
begin
  Result := Length(SA);
  SetLength(SA, Result + 1);
  if Assigned(Value) then Add(SA[Result], Value, Length(Value));
end;

function Add(var Data: TFunctionData; const AFunction: TFunction): Integer;
var
  I: Integer;
  BFunction: PFunction;
begin
  for I := Low(Data.FArray) to High(Data.FArray) do
  begin
    BFunction := @Data.FArray[I];
    if StrLen(BFunction.Name) = 0 then
    begin
      Result := I;
      BFunction^ := AFunction;
      Exit;
    end;
  end;
  Result := Length(Data.FArray);
  SetLength(Data.FArray, Result + 1);
  Add(Data.FArray, @AFunction, Result * SizeOf(TFunction), SizeOf(TFunction));
  Data.Prepared := False;
end;

function Add(var Data: TTypeData; const AType: TType): Integer;
var
  I: Integer;
  BType: PType;
begin
  for I := Low(Data.TArray) to High(Data.TArray) do
  begin
    BType := @Data.TArray[I];
    if StrLen(BType.Name) = 0 then
    begin
      Result := I;
      BType^ := AType;
      Exit;
    end;
  end;
  Result := Length(Data.TArray);
  SetLength(Data.TArray, Result + 1);
  Add(Data.TArray, @AType, Result * SizeOf(TType), SizeOf(TType));
  Data.Prepared := False;
end;

function Add(var ParameterArray: TParameterArray; const ATHandle: Integer; const AValue: TValue): Integer;
begin
  Result := Length(ParameterArray);
  SetLength(ParameterArray, Result + 1);
  with ParameterArray[Result] do
  begin
    THandle := ATHandle;
    Value := AValue;
  end;
end;

function Add(var ParameterArray: TParameterArray; const ATHandle: Integer; const AValue: string): Integer;
begin
  Result := Length(ParameterArray);
  SetLength(ParameterArray, Result + 1);
  with ParameterArray[Result] do
  begin
    THandle := ATHandle;
    StrLCopy(Text, PChar(AValue), SizeOf(TString) - 1);
  end;
end;

function Add(var LockArray: TLockArray; const AFromIndex, ATillIndex: Integer): Integer;
begin
  Result := Length(LockArray);
  SetLength(LockArray, Result + 1);
  with LockArray[Result] do
  begin
    FromIndex := AFromIndex;
    TillIndex := ATillIndex;
  end;
end;

function Add(var FlagArray: TFunctionFlagArray; const Flag: TFunctionFlag): Integer;
begin
  Result := Length(FlagArray);
  SetLength(FlagArray, Result + 1);
  FlagArray[Result] := Flag;
end;

function Add(var FlagArray: TFunctionFlagArray; const Index, Handle: Integer): Integer;
begin
  Result := Add(FlagArray, MakeFunctionFlag(Index, Handle));
end;

function Add(var ItemArray: TTextItemArray; const TextItem: TTextItem): Integer;
begin
  Result := Length(ItemArray);
  SetLength(ItemArray, Result + 1);
  ItemArray[Result] := TextItem;
end;

function Add(var ItemArray: TTextItemArray; const Text: string; const FHandle, THandle: Integer): Integer;
begin
  Result := Add(ItemArray, MakeTextItem(Text, FHandle, THandle));
end;

procedure IncNumber(var Number: TNumber; const AValue: TValue);
begin
  if Number.Alive then Number.Value := Operation(Number.Value, AValue, otAdd)
  else begin
    Number.Value := AValue;
    Number.Alive := True;
  end;
end;

procedure IncNumber(var Number: TNumber; const Parser: TCustomParser; const Script: TScript; const ScriptType: TScriptType);
begin
  IncNumber(Number, GetValueFromScript(Parser, Script, ScriptType));
end;

procedure Delete(var SA: TScriptArray);
var
  I: Integer;
begin
  for I := Low(SA) to High(SA) do SA[I] := nil;
  SA := nil;
end;

function Insert(var Script: TScript; Value: Int64; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(Script);
  SetLength(Script, I + SizeOf(Int64));
  Result := Insert(Script, @Value, Index, I, SizeOf(Int64));
end;

function Insert(var Script: TScript; Value, Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(Script);
  SetLength(Script, I + SizeOf(Integer));
  Result := Insert(Script, @Value, Index, I, SizeOf(Integer));
end;

function Insert(var Script: TScript; Value: Byte; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(Script);
  SetLength(Script, I + SizeOf(Byte));
  Result := Insert(Script, @Value, Index, I, SizeOf(Byte));
end;

function FunctionCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  AData: PFunctionData absolute Target;
begin
  Result := CompareValue(StrLen(FunctionByIndex(AData, BIndex).Name), StrLen(FunctionByIndex(AData, AIndex).Name));
end;

procedure FunctionExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Integer;
  AData: PFunctionData absolute Target;
begin
  I := AData.FOrder[AIndex];
  AData.FOrder[AIndex] := AData.FOrder[BIndex];
  AData.FOrder[BIndex] := I;
end;

function Prepare(const Data: PFunctionData): Boolean;
var
  I: Integer;
begin
  Result := not Data.Prepared;
  if Result then
  begin
    SetLength(Data.FOrder, Length(Data.FArray));
    for I := Low(Data.FOrder) to High(Data.FOrder) do
      Data.FOrder[I] := I;
    QSort(Data, Low(Data.FArray), High(Data.FArray), FunctionCompare, FunctionExchange);
    for I := Low(Data.FArray) to High(Data.FArray) do
    begin
      Synchronize(@Data.FArray[I]);
      if Assigned(Data.FArray[I].Handle) then Data.FArray[I].Handle^ := I;
    end;
    if not Assigned(Data.NameList) then
    begin
      Data.NameList := TFlexibleList.Create(nil);
      {$IFDEF DELPHI_7}
      Data.NameList.NameValueSeparator := Pipe;
      {$ENDIF}
    end;
    Data.NameList.List.Clear;
    for I := Low(Data.FArray) to High(Data.FArray) do
      Data.NameList.List.AddObject(Data.FArray[I].Name, Pointer(I));
    Data.Prepared := True;
    Cache.TCache(Data.FlagCache).Compile;
    Cache.TCache(Data.ItemCache).Compile;
  end;
end;

function GetType(const Data: PTypeData; Index: Integer; out AType: PType): Boolean;
begin
  Result := (Index >= Low(Data.TOrder)) and (Index <= High(Data.TOrder)) and (Data.TOrder[Index] >= Low(Data.TArray)) and
    (Data.TOrder[Index] <= High(Data.TArray));
  if Result then
    AType := @Data.TArray[Data.TOrder[Index]]
  else
    AType := nil;
end;

function TypeByIndex(const Data: PTypeData; const Index: Integer): PType;
begin
  GetType(Data, Index, Result);
end;

function TypeCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  AData: PTypeData absolute Target;
begin
  Result := CompareValue(StrLen(TypeByIndex(AData, BIndex).Name), StrLen(TypeByIndex(AData, AIndex).Name));
end;

procedure TypeExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  AData: PTypeData absolute Target;
  I: Integer;
begin
  I := AData.TOrder[AIndex];
  AData.TOrder[AIndex] := AData.TOrder[BIndex];
  AData.TOrder[BIndex] := I;
end;

function Prepare(const Data: PTypeData): Boolean;
var
  I: Integer;
begin
  Result := not Data.Prepared;
  if Result then
  begin
    SetLength(Data.TOrder, Length(Data.TArray));
    for I := Low(Data.TOrder) to High(Data.TOrder) do Data.TOrder[I] := I;
    QSort(Data, Low(Data.TArray), High(Data.TArray), TypeCompare, TypeExchange);
    if not Assigned(Data.NameList) then
    begin
      Data.NameList := TFlexibleList.Create(nil);
      {$IFDEF DELPHI_7}
      Data.NameList.NameValueSeparator := Pipe;
      {$ENDIF}
    end;
    Data.NameList.List.Clear;
    for I := Low(Data.TArray) to High(Data.TArray) do
      Data.NameList.List.AddObject(Data.TArray[I].Name, Pointer(I));
    Data.Prepared := True;
  end;
end;

function MakeTypeHandle(const Data: TTypeData; const ValueType: TValueType; const DefaultTypeHandle: Integer): Integer;
var
  I: Integer;
begin
  for I := Low(Data.TArray) to High(Data.TArray) do
    if Data.TArray[I].ValueType = ValueType then
    begin
      Result := I;
      Exit;
    end;
  Result := DefaultTypeHandle;
end;

function AssignUserType(const Parser: TCustomParser; const ItemHeader: PItemHeader; const TypeFlag: Boolean; const ValueType: TValueType): Boolean;
var
  AType: PType;
begin
  Result := Parser.GetType(ItemHeader.UserType.Handle, AType);
  if Result then
  begin
    ItemHeader.UserType.Active := TypeFlag;
    ItemHeader.UserType.Handle := MakeTypeHandle(Parser.TData^, ValueType, Parser.DefaultTypeHandle);
  end;
end;

function GetType(const Parser: TCustomParser; const ItemHeader: PItemHeader): PType;
begin
  if not Parser.GetType(ItemHeader.UserType.Handle, Result) then Result := Parser.GetType(Parser.DefaultTypeHandle);
end;

function TestFormula(const Parser: TCustomParser; const Text: string; out Script: TScript): TError;
begin
  try
    Parser.StringToScript(Text, Script, Result);
  except
    on E: Exception do Result := MakeError(etUnknown, E.Message);
  end;
  if Result.ErrorType <> etSuccess then Script := nil;
end;

function FindFormula(const Parser: TCustomParser; const Text: string; out Script: TScript): Boolean;
var
  S: string;
  I, J: Integer;
begin
  S := Trim(Text);
  for I := 1 to Length(S) do for J := Length(S) downto I do
  begin
    Result := TestFormula(Parser, Copy(S, I, J - I + 1), Script).ErrorType = etSuccess;
    if Result then Exit;
  end;
  Result := False;
end;

initialization
  Helper := TParseHelper.Create;
  LockBracket := BracketArray[btParenthesis];
  LockChar := DoubleQuote;

finalization
  Helper.Free;

end.
