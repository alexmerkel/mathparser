{ *********************************************************************** }
{                                                                         }
{ ParseTypes                                                              }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseTypes;

{$B-}

interface

uses
  SysUtils, Types, FlexibleList, ValueTypes;

const
  StringLength = 1024;

type
  TItemCode = (icNumber, icFunction, icString, icScript, icParameter);

  TString = array[0..StringLength - 1] of Char;

  PHandleArray = ^THandleArray;
  THandleArray = array of Integer;

  PPType = ^PType;

  PType = ^TType;
  TType = record
    Handle: PInteger;
    ValueType: TValueType;
    Name: TString;
  end;

  TTypeArray = array of TType;

  TTypeOrder = TIntegerDynArray;

  PTypeData = ^TTypeData;
  TTypeData = record
    TArray: TTypeArray;
    TOrder: TTypeOrder;
    NameList: TFlexibleList;
    Prepared: Boolean;
  end;

  TParameter = record
    THandle: Integer;
    case Byte of
      0: (Value: TValue);
      1: (Text: TString);
  end;
  PParameterArray = ^TParameterArray;
  TParameterArray = array of TParameter;

  PFunction = ^TFunction;
  TFunctionKind = (fkHandle, fkMethod);

  TParameterlessMethod = function(const AFunction: PFunction; const AType: PType): TValue of object;
  TSingleParameterMethod = function(const AFunction: PFunction; const AType: PType;
    const Value: TValue): TValue of object;
  TDoubleParameterMethod = function(const AFunction: PFunction; const AType: PType;
    const LValue, RValue: TValue): TValue of object;
  TParameterArrayMethod = function(const AFunction: PFunction; const AType: PType;
    const ParameterArray: TParameterArray): TValue of object;

  TMethodType = (mtEmpty, mtParameterless, mtSingleParameter, mtDoubleParameter, mtParameterArray, mtVariable);

  TParameterKind = (pkValue, pkReference);

  PFunctionParameter = ^TFunctionParameter;
  TFunctionParameter = record
    Kind: TParameterKind;
    LParameter, RParameter: Boolean;
    Count: Integer;
  end;

  TVariableType = (vtValue, vtLiveValue);

  PFunctionVariable = ^TFunctionVariable;
  TFunctionVariable = record
    VariableType: TVariableType;
    // TFunction.Handle = @TFunctionVariable.Handle
    Handle: Integer;
    case Byte of
      0: (Variable: PValue);
      1: (LiveVariable: TLiveValue);
  end;

  PFunctionMethod = ^TFunctionMethod;
  TFunctionMethod = record
    Parameter: TFunctionParameter;
    MethodType: TMethodType;
    Variable: TFunctionVariable;
    case Byte of
      0: (AMethod: TParameterlessMethod);
      1: (BMethod: TSingleParameterMethod);
      2: (CMethod: TDoubleParameterMethod);
      3: (DMethod: TParameterArrayMethod);
  end;

  TFunctionPriority = (fpLower, fpNormal, fpHigher);
  TPriorityCoverage = (pcLocal, pcTotal);

  TPriority = record
    Priority: TFunctionPriority;
    Coverage: TPriorityCoverage;
  end;

  TFunction = record
    Handle: PInteger;
    ReturnType: TValueType;
    Kind: TFunctionKind;
    Name: TString;
    Method: TFunctionMethod;
    Optimizable: Boolean;
    Priority: TPriority;
  end;
  TFunctionArray = array of TFunction;
  PFunctionArray = array of PFunction;

  TFunctionOrder = TIntegerDynArray;

  PFunctionData = ^TFunctionData;
  TFunctionData = record
    FArray: TFunctionArray;
    FOrder: TFunctionOrder;
    NameList: TFlexibleList;
    FlagCache, ItemCache: TObject;
    Prepared: Boolean;
  end;

  PScript = ^TScript;
  TScript = TByteDynArray;

  TScriptType = (stScriptItem, stScript);

  PScriptArray = ^TScriptArray;
  TScriptArray = array of TScript;

  TItemData = record
    Code: TItemCode;
    case Byte of
      0: (Number: PValue);
      1: (Handle: PInteger);
      2: (Text: PString);
      3: (Script: PScript);
  end;

  TBracketType = (btLeft, btRight);
  TBracket = array[TBracketType] of Char;

  TBD = record
    FromIndex, FromCount, TillIndex, TillCount: Integer;
  end;

  PNumber = ^TNumber;
  TNumber = record
    Value: TValue;
    Alive: Boolean;
  end;

  PScriptHeader = ^TScriptHeader;
  TScriptHeader = record
    Value: TValue;
    ScriptSize: Integer;
    ScriptCount: Integer;
    HeaderSize: Integer;
  end;

  PUserType = ^TUserType;
  TUserType = record
    Active: Boolean;
    Handle: Integer;
  end;

  PItemHeader = ^TItemHeader;
  TItemHeader = record
    Size: Integer;
    Sign: Integer;
    UserType: TUserType;
  end;

  TCode = Integer;

  PScriptNumber = ^TScriptNumber;
  TScriptNumber = record
    Value: TValue;
  end;

  PScriptFunction = ^TScriptFunction;
  TScriptFunction = record
    Handle: Integer;
  end;

  PInternalScript = ^TInternalScript;
  TInternalScript = record
    Header: TScriptHeader;
  end;

  TScriptString = record
    Size: Integer;
  end;

  PScriptItem = ^TScriptItem;
  TScriptItem = packed record
    Code: TCode;
    case Byte of
      0: (ScriptNumber: TScriptNumber);
      1: (ScriptFunction: TScriptFunction);
      2: (Script: TInternalScript);
      3: (ScriptString: TScriptString);
  end;

  TScriptMethod = function(var Index: Integer; const Header: PScriptHeader;
    const ItemHeader: PItemHeader; const Item: PScriptItem;
    const Data: Pointer): Boolean of object;

  TFunctionFlagMethod = function(const AFunction: PFunction): Boolean;

  PFunctionFlag = ^TFunctionFlag;
  TFunctionFlag = record
    Index, Handle: Integer;
  end;
  TFunctionFlagArray = array of TFunctionFlag;

  PTextItem = ^TTextItem;
  TTextItem = record
    Text: string;
    FHandle, THandle: Integer;
  end;
  TTextItemArray = array of TTextItem;
  TTextItemMultiArray = array of TTextItemArray;

function MakeType(const AName: string; var AHandle: Integer; const AValueType: TValueType): TType;
function MakeFunction(const AName: string; var AHandle: Integer; const AReturnType: TValueType; const AKind: TFunctionKind; const AMethod: TFunctionMethod;
  const AOptimizable: Boolean; const APriority: TPriority): TFunction; overload;
function MakeParameter(const ALParameter, ARParameter: Boolean; const ACount: Integer; const AKind: TParameterKind = pkValue): TFunctionParameter;
function MakeFunctionVariable(const AVariable: PValue; const AHandle: PInteger = nil): TFunctionVariable; overload;
function MakeFunctionVariable(const AVariable: TLiveValue; const AHandle: PInteger = nil): TFunctionVariable; overload;
function MakeFunctionMethod(const Parameter: TFunctionParameter): TFunctionMethod; overload;
function MakeFunctionMethod(const LParameter, RParameter: Boolean; const Count: Integer; const Kind: TParameterKind = pkValue): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TParameterlessMethod): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TSingleParameterMethod; const AParameter: TFunctionParameter): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TSingleParameterMethod; const LParameter, RParameter: Boolean; Kind: TParameterKind = pkValue): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TDoubleParameterMethod): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TParameterArrayMethod; const AParameter: TFunctionParameter): TFunctionMethod; overload;
function MakeFunctionMethod(const Method: TParameterArrayMethod; const Count: Integer; const Kind: TParameterKind = pkValue): TFunctionMethod; overload;
function MakeFunctionMethod(const AVariable: TFunctionVariable): TFunctionMethod; overload;
function MakeFunctionMethod(const Variable: PValue; const AHandle: PInteger = nil): TFunctionMethod; overload;
function MakeFunctionMethod(const Variable: TLiveValue; const AHandle: PInteger = nil): TFunctionMethod; overload;
function MakePriority(const APriority: TFunctionPriority; const ACoverage: TPriorityCoverage): TPriority;
function MakeItemData(var ANumber: TValue): TItemData; overload;
function MakeItemData(var AHandle: Integer): TItemData; overload;
function MakeItemData(var AText: TString): TItemData; overload;
function MakeItemData(var AScript: TScript; const Parameter: Boolean): TItemData; overload;
function MakeNumber(const AValue: TValue; const AAlive: Boolean): TNumber;
function MakeUserType(const AActive: Boolean; const AHandle: Integer): TUserType; overload;
function MakeUserType(const AType: PType; DefaultTypeHandle: Integer): TUserType; overload;
function MakeFunctionFlag(const AIndex, AHandle: Integer): TFunctionFlag;
function MakeTextItem(const AText: string; const AFHandle, ATHandle: Integer): TTextItem;
function MakeFunction(const AFunction: PFunction; const AHandle: PInteger): Boolean; overload;
function MakeVariable(const AFunction: PFunction; const AVariable: PValue): Boolean;
function Synchronize(const AFunction: PFunction): Boolean;
procedure WriteNumber(var Script: TScript; const Value: TValue);
procedure WriteFunction(var Script: TScript; const AHandle: Integer);
procedure WriteScript(var Target: TScript; const Source: TScript; const Parameter: Boolean; const ItemIndex: PInteger = nil);
procedure WriteString(var Script: TScript; const S: string); overload;
procedure WriteString(var Script: TScript; const Source: Pointer; const Count: Integer); overload;

implementation

uses
  MemoryUtils, ParseConsts, ParseUtils, ValueUtils;

function MakeType(const AName: string; var AHandle: Integer; const AValueType: TValueType): TType;
begin
  FillChar(Result, SizeOf(TType), 0);
  with Result do
  begin
    StrLCopy(Name, PChar(AName), SizeOf(TString) - 1);
    Handle := @AHandle;
    ValueType := AValueType;
  end;
end;

function MakeFunction(const AName: string; var AHandle: Integer; const AReturnType: TValueType; const AKind: TFunctionKind; const AMethod: TFunctionMethod;
  const AOptimizable: Boolean; const APriority: TPriority): TFunction;
begin
  FillChar(Result, SizeOf(TFunction), 0);
  with Result do
  begin
    StrLCopy(Name, PChar(AName), SizeOf(TString) - 1);
    Handle := @AHandle;
    ReturnType := AReturnType;
    Kind := AKind;
    Method := AMethod;
    Optimizable := AOptimizable;
    Priority := APriority;
  end;
end;

function MakeParameter(const ALParameter, ARParameter: Boolean; const ACount: Integer; const AKind: TParameterKind): TFunctionParameter;
begin
  FillChar(Result, SizeOf(TFunctionParameter), 0);
  with Result do
  begin
    LParameter := ALParameter;
    RParameter := ARParameter;
    Count := ACount;
    Kind := AKind;
  end;
end;

function MakeFunctionVariable(const AVariable: PValue; const AHandle: PInteger): TFunctionVariable;
begin
  FillChar(Result, SizeOf(TFunctionVariable), 0);
  with Result do
  begin
    VariableType := vtValue;
    if Assigned(AHandle) then Handle := AHandle^;
    Variable := AVariable;
  end;
end;

function MakeFunctionVariable(const AVariable: TLiveValue; const AHandle: PInteger): TFunctionVariable;
begin
  FillChar(Result, SizeOf(TFunctionVariable), 0);
  with Result do
  begin
    VariableType := vtLiveValue;
    if Assigned(AHandle) then Handle := AHandle^;
    LiveVariable := AVariable;
  end;
end;

function MakeFunctionMethod(const Parameter: TFunctionParameter): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  Result.Parameter := Parameter;
end;

function MakeFunctionMethod(const LParameter, RParameter: Boolean; const Count: Integer;
  const Kind: TParameterKind = pkValue): TFunctionMethod;
begin
  Result := MakeFunctionMethod(MakeParameter(LParameter, RParameter, Count, Kind));
end;

function MakeFunctionMethod(const Method: TParameterlessMethod): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  with Result do
  begin
    MethodType := mtParameterless;
    AMethod := Method;
  end;
end;

function MakeFunctionMethod(const Method: TSingleParameterMethod; const AParameter: TFunctionParameter): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  with Result do
  begin
    Parameter := AParameter;
    MethodType := mtSingleParameter;
    BMethod := Method;
  end;
end;

function MakeFunctionMethod(const Method: TSingleParameterMethod; const LParameter, RParameter: Boolean; Kind: TParameterKind): TFunctionMethod;
begin
  Result := MakeFunctionMethod(Method, MakeParameter(LParameter, RParameter, 0, Kind));
end;

function MakeFunctionMethod(const Method: TDoubleParameterMethod): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  with Result do
  begin
    Parameter.LParameter := True;
    Parameter.RParameter := True;
    MethodType := mtDoubleParameter;
    CMethod := Method;
  end;
end;

function MakeFunctionMethod(const Method: TParameterArrayMethod; const AParameter: TFunctionParameter): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  with Result do
  begin
    Parameter := AParameter;
    MethodType := mtParameterArray;
    DMethod := Method;
  end;
end;

function MakeFunctionMethod(const Method: TParameterArrayMethod; const Count: Integer; const Kind: TParameterKind = pkValue): TFunctionMethod;
begin
  Result := MakeFunctionMethod(Method, MakeParameter(False, False, Count, Kind));
end;

function MakeFunctionMethod(const AVariable: TFunctionVariable): TFunctionMethod;
begin
  FillChar(Result, SizeOf(TFunctionMethod), 0);
  with Result do
  begin
    MethodType := mtVariable;
    Variable := AVariable;
  end;
end;

function MakeFunctionMethod(const Variable: PValue; const AHandle: PInteger): TFunctionMethod;
begin
  Result := MakeFunctionMethod(MakeFunctionVariable(Variable, AHandle));
end;

function MakeFunctionMethod(const Variable: TLiveValue; const AHandle: PInteger): TFunctionMethod;
begin
  Result := MakeFunctionMethod(MakeFunctionVariable(Variable, AHandle));
end;

function MakePriority(const APriority: TFunctionPriority; const ACoverage: TPriorityCoverage): TPriority;
begin
  FillChar(Result, SizeOf(TPriority), 0);
  with Result do
  begin
    Priority := APriority;
    Coverage := ACoverage;
  end;
end;

function MakeItemData(var ANumber: TValue): TItemData;
begin
  FillChar(Result, SizeOf(TItemData), 0);
  with Result do
  begin
    Code := icNumber;
    Number := @ANumber;
  end;
end;

function MakeItemData(var AHandle: Integer): TItemData;
begin
  FillChar(Result, SizeOf(TItemData), 0);
  with Result do
  begin
    Code := icFunction;
    Handle := @AHandle;
  end;
end;

function MakeItemData(var AText: TString): TItemData;
begin
  FillChar(Result, SizeOf(TItemData), 0);
  with Result do
  begin
    Code := icString;
    Text := @AText;
  end;
end;

function MakeItemData(var AScript: TScript; const Parameter: Boolean): TItemData;
begin
  FillChar(Result, SizeOf(TItemData), 0);
  with Result do
  begin
    Code := TItemCode(Ord(icScript) + Ord(Parameter));
    Script := @AScript;
  end;
end;

function MakeNumber(const AValue: TValue; const AAlive: Boolean): TNumber;
begin
  FillChar(Result, SizeOf(TNumber), 0);
  with Result do
  begin
    Value := AValue;
    Alive := AAlive;
  end;
end;

function MakeUserType(const AActive: Boolean; const AHandle: Integer): TUserType;
begin
  FillChar(Result, SizeOf(TUserType), 0);
  with Result do
  begin
    Active := AActive;
    Handle := AHandle;
  end;
end;

function MakeUserType(const AType: PType; DefaultTypeHandle: Integer): TUserType;
begin
  if Assigned(AType) then DefaultTypeHandle := AType.Handle^;
  Result := MakeUserType(Assigned(AType), DefaultTypeHandle);
end;

function MakeFunctionFlag(const AIndex, AHandle: Integer): TFunctionFlag;
begin
  FillChar(Result, SizeOf(TFunctionFlag), 0);
  with Result do
  begin
    Index := AIndex;
    Handle := AHandle;
  end;
end;

function MakeTextItem(const AText: string; const AFHandle, ATHandle: Integer): TTextItem;
begin
  FillChar(Result, SizeOf(TTextItem), 0);
  with Result do
  begin
    Text := AText;
    FHandle := AFHandle;
    THandle := ATHandle;
  end;
end;

function MakeFunction(const AFunction: PFunction; const AHandle: PInteger): Boolean;
begin
  Result := (AFunction.Kind = fkMethod) and (AFunction.Method.MethodType = mtVariable);
  if Result then
  begin
    AFunction.Handle := AHandle;
    if Assigned(AFunction.Handle) then
      AFunction.Handle^ := AFunction.Method.Variable.Handle;
    AFunction.Kind := fkHandle;
  end;
end;

function MakeVariable(const AFunction: PFunction; const AVariable: PValue): Boolean;
begin
  Result := AFunction.Kind = fkHandle;
  if Result then
  begin
    if Assigned(AFunction.Handle) then
      AFunction.Method.Variable.Handle := AFunction.Handle^;
    AFunction.Handle := @AFunction.Method.Variable.Handle;
    AFunction.Method.MethodType := mtVariable;
    AFunction.Method.Variable.Variable := AVariable;
    AFunction.Kind := fkMethod;
  end;
end;

function Synchronize(const AFunction: PFunction): Boolean;
begin
  Result := (AFunction.Kind = fkMethod) and (AFunction.Method.MethodType = mtVariable);
  if Result then AFunction.Handle := @AFunction.Method.Variable.Handle;
end;

procedure WriteNumber(var Script: TScript; const Value: TValue);
var
  Number: TScriptNumber;
begin
  Add(Script, Integer(NumberCode));
  FillChar(Number, SizeOf(TScriptNumber), 0);
  Number.Value := Value;
  Add(Script, Number);
end;

procedure WriteFunction(var Script: TScript; const AHandle: Integer);
var
  AFunction: TScriptFunction;
begin
  Add(Script, Integer(FunctionCode));
  FillChar(AFunction, SizeOf(TScriptFunction), 0);
  AFunction.Handle := AHandle;
  Add(Script, AFunction);
end;

procedure WriteScript(var Target: TScript; const Source: TScript; const Parameter: Boolean; const ItemIndex: PInteger);
var
  I, J: Integer;
  Header: PScriptHeader absolute Target;
begin
  if Parameter then Add(Target, Integer(ParameterCode))
  else
    if Assigned(ItemIndex) then
    begin
      //  Увеличиваем адреса вложенных сценариев на SizeOf(Integer), так как будет добавлен еще один адрес
      J := SizeOf(TScriptHeader);
      for I := 0 to Header.ScriptCount - 1 do
      begin
        Inc(PInteger(@Target[J])^, SizeOf(Integer));
        Inc(J, SizeOf(Integer));
      end;
      //  Определяем адрес конца заголовка скрипта
      I := SizeOf(TScriptHeader) + Header.ScriptCount * SizeOf(Integer);
      Inc(Header.ScriptCount);
      //  Добавляем в конец заголовка скрипта (I) адрес нового вложенного сценария (J)
      //  Адрес нового вложенного сценария состоит из:
      //    1) длина скрипта (Length(Target))
      //    2) длина адреса нового вложенного сценария (SizeOf(Integer))
      //    3) длина флага вложенного сценария (SizeOf(TCode))
      J := Length(Target) + SizeOf(Integer) + SizeOf(TCode);
      Insert(Target, J, I);
      //  Увеличиваем количество вложенных сценариев
      if Assigned(ItemIndex) then Inc(ItemIndex^, SizeOf(Integer));
      //  Добавляем флаг вложенного сценария
      Add(Target, Integer(ScriptCode));
    end
    else Add(Target, Integer(ScriptCode));
  Add(Target, Source, Length(Source));
end;

procedure WriteString(var Script: TScript; const S: string);
begin
  Add(Script, Integer(StringCode));
  Add(Script, Length(S) * SizeOf(Char));
  Add(Script, S);
end;

procedure WriteString(var Script: TScript; const Source: Pointer; const Count: Integer);
begin
  Add(Script, Integer(StringCode));
  Add(Script, Count);
  Add(Script, Source, Count);
end;

end.
