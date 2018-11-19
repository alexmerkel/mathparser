{ *********************************************************************** }
{                                                                         }
{ ParseMethod                                                             }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseMethod;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, FlexibleList,
  ParseTypes, Types, ValueTypes;

type
  TVariableContainer = class
  private
    FList: TFlexibleList;
  protected
    function Find(const Name: string; Index: PInteger = nil): Boolean; virtual;
    property List: TFlexibleList read FList write FList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetVariable(const Name: string; const Value: PValue); virtual;
    function Delete(const Name: string): Boolean; overload; virtual;
    procedure Clear; virtual;
  end;

  TCustomMethod = class
  private
    FParser: TObject;
  public
    constructor Create(AParser: TObject); virtual;
    property Parser: TObject read FParser write FParser;
  end;

  TMethod = class(TCustomMethod)
  private
    FContainer: TVariableContainer;
  protected
    property Container: TVariableContainer read FContainer write FContainer;
  public
    constructor Create(AParser: TObject); override;
    destructor Destroy; override;
    function VoidMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function NewMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function DeleteMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function FindMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function SetMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function ScriptMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function ForMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function RepeatMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function WhileMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MultiplyMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function DivideMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function SuccMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function PredMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function NotMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function AndMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function OrMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function XorMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function ShlMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function ShrMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function SameValueMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function IsZeroMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function IfMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function IfThenMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function EnsureRangeMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function StrToIntMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function StrToIntDefMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function StrToFloatMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function StrToFloatDefMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function ParseMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function TrueMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function FalseMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function EqualMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function NotEqualMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function AboveMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function BelowMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function AboveOrEqualMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function BelowOrEqualMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function GetEpsilonMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function SetEpsilonMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function SetDecimalSeparatorMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
  end;

  TMathMethod = class(TCustomMethod)
  public
    function DivMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function ModMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function DegreeMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function FactorialMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SqrMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SqrtMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function IntMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function RoundMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function RoundToMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function TruncMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function AbsMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function FracMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function LnMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function LgMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function LogMethod(const AFunction: PFunction; const AType: PType;
      const LValue, RValue: TValue): TValue; virtual;
    function ExpMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function RandomMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SinMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcSinMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SinHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcSinHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CosMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCosMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CosHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCosHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function TanMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcTanMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function TanHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcTanHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CoTanMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCoTanMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CoTanHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCoTanHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SecMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcSecMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function SecHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcSecHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CscMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCscMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CscHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcCscHMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function ArcTan2Method(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function YearMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function MonthMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function DayMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function DayOfWeekMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function HourMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function MinuteMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function SecondMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function MSecondMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function TimeMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function DateMethod(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function GetYearMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetMonthMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetDayMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetDayOfWeekMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetHourMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetMinuteMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetSecondMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetMSecondMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function EncodeTimeMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function EncodeDateMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function EncodeDateTimeMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function GetTickCount(const AFunction: PFunction; const AType: PType): TValue; virtual;
    function HypotMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function RadToDegMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function RadToGradMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function RadToCycleMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function DegToRadMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function DegToGradMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function DegToCycleMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function GradToRadMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function GradToDegMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function GradToCycleMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CycleToRadMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CycleToDegMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function CycleToGradMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function LnXP1Method(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function Log10Method(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function Log2Method(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function IntPowerMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function PowerMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function LdexpMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function CeilMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function FloorMethod(const AFunction: PFunction; const AType: PType;
      const Value: TValue): TValue; virtual;
    function PolyMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MeanMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function SumMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function SumIntMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function SumOfSquaresMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MinValueMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MinIntValueMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MinMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MaxValueMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MaxIntValueMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function MaxMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function StdDevMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function PopnStdDevMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function VarianceMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function PopnVarianceMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function TotalVarianceMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function NormMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function RandGMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function RandomRangeMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
    function RandomFromMethod(const AFunction: PFunction; const AType: PType;
      const ParameterArray: TParameterArray): TValue; virtual;
  end;

  TValueRelationship = (vrBelow, vrEqual, vrAbove);
  TDoubleDynArray = array of Double;

const
  ATypeError = '%s type found, but %s type expected for the %s function parameter with index %d';
  BTypeError = '%s type is not applicable for the %s function parameter with index %d';

var
  MethodLock: TRTLCriticalSection;

procedure Check(const AFunction: PFunction; const ParameterArray: TParameterArray; const ParameterCount: Integer; Relationship: TValueRelationship = vrEqual); overload;
procedure Check(const AFunction: PFunction; const ParameterArray: TParameterArray; const MinParameterCount, MaxParameterCount: Integer); overload;
procedure Check(const Parser: TObject; const AFunction: PFunction; const Index, THandle: Integer; const TextFlag: Boolean); overload;
procedure Check(const Parser: TObject; const AFunction: PFunction; const ParameterArray: TParameterArray; const TextFlag: Boolean); overload;

function FloatArray(const ParameterArray: TParameterArray; const FromIndex, TillIndex: Integer): TDoubleDynArray;
function IntegerArray(const ParameterArray: TParameterArray; const FromIndex, TillIndex: Integer): TIntegerDynArray;

function Factorial(const Value: Smallint): Int64;

implementation

uses
  DateUtils, Math, MemoryUtils, NumberConsts, NumberUtils, ParseCommon, ParseConsts, ParseErrors,
  ParseManager, Parser, ParseUtils, ThreadUtils, ValueConsts, ValueErrors, ValueUtils;

procedure Check(const AFunction: PFunction; const ParameterArray: TParameterArray; const ParameterCount: Integer; Relationship: TValueRelationship);
begin
  case Relationship of
    vrBelow:
      if Length(ParameterArray) < ParameterCount then
        raise ParseErrors.Error(BParameterExpectError, [AFunction.Name]);
    vrEqual:
      begin
        if Length(ParameterArray) < ParameterCount then
          raise ParseErrors.Error(BParameterExpectError, [AFunction.Name]);
        if Length(ParameterArray) > ParameterCount then
          raise ParseErrors.Error(BParameterExcessError, [AFunction.Name]);
      end;
    vrAbove:
      if Length(ParameterArray) > ParameterCount then
        raise ParseErrors.Error(BParameterExcessError, [AFunction.Name]);
  end;
end;

procedure Check(const AFunction: PFunction; const ParameterArray: TParameterArray; const MinParameterCount, MaxParameterCount: Integer);
begin
  Check(AFunction, ParameterArray, MinParameterCount, vrBelow);
  Check(AFunction, ParameterArray, MaxParameterCount, vrAbove);
end;

procedure Check(const Parser: TObject; const AFunction: PFunction; const Index, THandle: Integer; const TextFlag: Boolean);
var
  P: TParser absolute Parser;
  AType, BType: PType;
begin
  if P.GetType(THandle, AType) and P.GetType(P.StringHandle, BType) then
  begin
    if TextFlag and (THandle <> P.StringHandle) then
      raise ParseErrors.Error(ATypeError, [AType.Name, BType.Name, AFunction.Name, Index + 1]);
    if not TextFlag and (THandle = P.StringHandle) then
      raise ParseErrors.Error(BTypeError, [AType.Name, AFunction.Name, Index + 1]);
  end;
end;

procedure Check(const Parser: TObject; const AFunction: PFunction; const ParameterArray: TParameterArray; const TextFlag: Boolean);
var
  I: Integer;
begin
  for I := Low(ParameterArray) to High(ParameterArray) do Check(Parser, AFunction, I, ParameterArray[I].THandle, TextFlag);
end;

function FloatArray(const ParameterArray: TParameterArray; const FromIndex, TillIndex: Integer): TDoubleDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ParameterArray));
  for I := FromIndex to TillIndex do Result[I] := Convert(ParameterArray[I].Value, vtExtended).Float80;
end;

function IntegerArray(const ParameterArray: TParameterArray; const FromIndex, TillIndex: Integer): TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ParameterArray));
  for I := FromIndex to TillIndex do
    Result[I] := Convert(ParameterArray[I].Value, vtInteger).Signed32;
end;

function Factorial(const Value: Smallint): Int64;
var
  I: Integer;
begin
  Result := 1;
  for I := 1 to Value do Result := Result * I;
end;

{ TVariableContainer }

procedure TVariableContainer.Clear;
var
  I: Integer;
  Value: PValue;
begin
  for I := 0 to FList.List.Count - 1 do
  begin
    Value := PValue(FList.List.Objects[I]);
    if Assigned(Value) then Dispose(Value);
  end;
end;

constructor TVariableContainer.Create;
begin
  FList := TFlexibleList.Create(nil);
end;

function TVariableContainer.Delete(const Name: string): Boolean;
var
  I: Integer;
  Value: PValue;
begin
  Result := Find(Name, @I);
  if Result then
  begin
    Value := PValue(FList.List.Objects[I]);
    if Assigned(Value) then Dispose(Value);
    FList.List.Delete(I);
  end;
end;

destructor TVariableContainer.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TVariableContainer.Find(const Name: string; Index: PInteger): Boolean;
var
  I: Integer;
begin
  if not Assigned(Index) then Index := @I;
  Index^ := ParseCommon.IndexOf(FList.List, Name, False);
  Result := Index^ >= 0;
end;

procedure TVariableContainer.SetVariable(const Name: string; const Value: PValue);
var
  I: Integer;
begin
  if Find(Name, @I) then FList.List.Objects[I] := TObject(Value)
  else FList.List.AddObject(Name, TObject(Value));
end;

{ TCustomMethod }

constructor TCustomMethod.Create(AParser: TObject);
begin
  FParser := AParser;
end;

{ TMethod }

function TMethod.AndMethod(const AFunction: PFunction; const AType: PType; const LValue, RValue: TValue): TValue;
begin
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned8 and RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Unsigned8 and RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned8 and RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned8 and RValue.Signed16);
        vtLongword: AssignInt64(Result, Int64(LValue.Unsigned8) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned8) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Unsigned8) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned8 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned8 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Unsigned8 and Round(RValue.Float80));
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed8 and RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed8 and RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed8 and RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Signed8 and RValue.Signed16);
        vtLongword: AssignInt64(Result, Int64(LValue.Signed8) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Signed8) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Signed8) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed8 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed8 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Signed8 and Round(RValue.Float80));
      end;
    vtWord:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned16 and RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Unsigned16 and RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned16 and RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned16 and RValue.Signed16);
        vtLongword: AssignInt64(Result, Int64(LValue.Unsigned16) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned16) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Unsigned16) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned16 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned16 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Unsigned16 and Round(RValue.Float80));
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed16 and RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed16 and RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed16 and RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Signed16 and RValue.Signed16);
        vtLongword: AssignInt64(Result, Int64(LValue.Signed16) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Signed16) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Signed16) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed16 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed16 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Signed16 and Round(RValue.Float80));
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Unsigned8));
        vtShortint: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Signed8));
        vtWord: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Unsigned16));
        vtSmallint: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Signed16));
        vtLongword: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned32) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Unsigned32) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned32 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned32 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Unsigned32 and Round(RValue.Float80));
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Unsigned8));
        vtShortint: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Signed8));
        vtWord: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Unsigned16));
        vtSmallint: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Signed16));
        vtLongword: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Signed32) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Signed32) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed32 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed32 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Signed32 and Round(RValue.Float80));
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Unsigned8));
        vtShortint: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Signed8));
        vtWord: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Unsigned16));
        vtSmallint: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Signed16));
        vtLongword: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, Int64(LValue.Signed64) and Int64(RValue.Signed32));
        vtInt64: AssignInt64(Result, Int64(LValue.Signed64) and RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed64 and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed64 and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, LValue.Signed64 and Round(RValue.Float80));
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte: AssignSingle(Result, Round(LValue.Float32) and RValue.Unsigned8);
        vtShortint: AssignSingle(Result, Round(LValue.Float32) and RValue.Signed8);
        vtWord: AssignSingle(Result, Round(LValue.Float32) and RValue.Unsigned16);
        vtSmallint: AssignSingle(Result, Round(LValue.Float32) and RValue.Signed16);
        vtLongword: AssignSingle(Result, Round(LValue.Float32) and RValue.Unsigned32);
        vtInteger: AssignSingle(Result, Round(LValue.Float32) and RValue.Signed32);
        vtInt64: AssignSingle(Result, Round(LValue.Float32) and RValue.Signed64);
        vtSingle: AssignSingle(Result, Round(LValue.Float32) and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, Round(LValue.Float32) and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) and Round(RValue.Float80));
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte: AssignDouble(Result, Round(LValue.Float32) and RValue.Unsigned8);
        vtShortint: AssignDouble(Result, Round(LValue.Float32) and RValue.Signed8);
        vtWord: AssignDouble(Result, Round(LValue.Float32) and RValue.Unsigned16);
        vtSmallint: AssignDouble(Result, Round(LValue.Float32) and RValue.Signed16);
        vtLongword: AssignDouble(Result, Round(LValue.Float32) and RValue.Unsigned32);
        vtInteger: AssignDouble(Result, Round(LValue.Float32) and RValue.Signed32);
        vtInt64: AssignDouble(Result, Round(LValue.Float32) and RValue.Signed64);
        vtSingle: AssignDouble(Result, Round(LValue.Float32) and Round(RValue.Float32));
        vtDouble: AssignDouble(Result, Round(LValue.Float32) and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) and Round(RValue.Float80));
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float80) and RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float80) and RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float80) and RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float80) and RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float80) and RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float80) and RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float80) and RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float80) and Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float80) and Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float80) and Round(RValue.Float80));
      end;
  end;
end;

constructor TMethod.Create(AParser: TObject);
begin
  inherited;
  FContainer := TVariableContainer.Create;
end;

function TMethod.DeleteMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
  Parameter: TParameter;
  S: string;
  BFunction: PFunction;
begin
  Check(AFunction, ParameterArray, DeleteParameterCount);
  P := TCustomParser(FParser);
  Parameter := GetParameter(P, ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Trim(Parameter.Text);
  BFunction := P.FindFunction(S);
  if Assigned(BFunction) and (BFunction.Method.MethodType = mtVariable) then
  begin
    P.DeleteFunction(BFunction.Handle^);
    FContainer.Delete(S);
    AssignInteger(Result, P.TrueValue);
  end
  else AssignInteger(Result, P.FalseValue);
end;

destructor TMethod.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TMethod.DivideMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := Operation(LValue, RValue, otFloatDivide);
end;

function TMethod.EnsureRangeMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  AValue, BValue, CValue: TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AValue := ParameterArray[AIndex].Value;
  BValue := ParameterArray[BIndex].Value;
  CValue := ParameterArray[CIndex].Value;
  if (AValue.ValueType in FloatTypes) or (BValue.ValueType in FloatTypes) then
    AssignDouble(Result, EnsureRange(Convert(AValue, vtExtended).Float80, Convert(BValue, vtExtended).Float80, Convert(CValue, vtExtended).Float80))
  else
    AssignInt64(Result, EnsureRange(Convert(AValue, vtInt64).Signed64, Convert(BValue, vtInt64).Signed64, Convert(CValue, vtInt64).Signed64));
end;

function TMethod.EqualMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 = RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 = RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 = Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 = Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 = Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 = RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 = RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 = Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 = Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 = RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 = RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 = RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 = Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 = RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 = RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 = RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 = RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 = RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 = RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 = RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if Equal(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Equal(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Equal(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Equal(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Equal(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Equal(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Equal(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if Equal(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Equal(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Equal(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Equal(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Equal(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Equal(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Equal(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if Equal(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Equal(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Equal(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Equal(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Equal(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Equal(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Equal(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Equal(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Equal(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Equal(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.FalseMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignInteger(Result, TParser(FParser).FalseValue);
end;

function TMethod.FindMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
  Parameter: TParameter;
  S: string;
  BFunction: PFunction;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  P := TCustomParser(FParser);
  Parameter := GetParameter(P, ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Trim(Parameter.Text);
  BFunction := P.FindFunction(S);
  if Assigned(BFunction) and (BFunction.Method.MethodType = mtVariable) then
    AssignInteger(Result, P.TrueValue)
  else
    AssignInteger(Result, P.FalseValue);
end;

function TMethod.ForMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
  Parameter: TParameter;
  S: string;
  BFunction: PFunction;
  Variable: PFunctionVariable;
  Value: PValue;
  BType: PType;
begin
  Check(AFunction, ParameterArray, ForParameterCount);
  Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
  Check(FParser, AFunction, CIndex, ParameterArray[CIndex].THandle, False);
  Check(FParser, AFunction, DIndex, ParameterArray[DIndex].THandle, False);
  P := TCustomParser(FParser);
  Parameter := GetParameter(P, ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Parameter.Text;
  BFunction := P.FindFunction(S);
  if Assigned(BFunction) then
  begin
    Value := nil;
    Variable := @BFunction.Method.Variable;
    case Variable.VariableType of
      vtValue: Variable.Variable^ := AsValue(P, ParameterArray[BIndex]);
      vtLiveValue: AssignValue(Variable.LiveVariable, AsValue(P, ParameterArray[BIndex]));
    end;
  end
  else begin
    Parameter := GetParameter(P, ParameterArray[BIndex]);
    New(Value);
    Value^ := Parameter.Value;
    if P.GetType(Parameter.THandle, BType) then Value^ := Convert(Value^, BType.ValueType);
    P.AddVariable(S, Value^, True);
  end;
  Result := EmptyValue;
  while AsBoolean(P, ParameterArray[CIndex]) do
    Result := Operation(Result, AsValue(P, ParameterArray[DIndex]), otAdd);
  if Assigned(Value) then
  begin
    P.DeleteVariable(Value^);
    Dispose(Value);
  end;
end;

function TMethod.GetEpsilonMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignDouble(Result, Epsilon);
end;

function TMethod.GetMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TParameter;
  S: string;
  BFunction: PFunction;
  Variable: PFunctionVariable;
begin
  Check(AFunction, ParameterArray, GetParameterCount);
  Parameter := GetParameter(TCustomParser(FParser), ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Trim(Parameter.Text);
  BFunction := TParser(FParser).FindFunction(S);
  if Assigned(BFunction) and (BFunction.Method.MethodType = mtVariable) then
  begin
    Variable := @BFunction.Method.Variable;
    case Variable.VariableType of
      vtValue: Result := Variable.Variable^;
      vtLiveValue: Result := MakeValue(Variable.LiveVariable);
    end;
  end
  else Result := EmptyValue;
end;

function TMethod.AboveMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 > RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 > RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 > Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 > Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 > Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 > RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 > RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 > Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 > Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 > RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 > RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 > RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 > Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 > RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 > RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 > RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 > RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 > RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 > RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 > RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if Above(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Above(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Above(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Above(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Above(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Above(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Above(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if Above(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Above(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Above(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Above(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Above(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Above(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Above(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if Above(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Above(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Above(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Above(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Above(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Above(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Above(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Above(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Above(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Above(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.AboveOrEqualMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 >= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 >= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 >= Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 >= Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 >= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 >= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 >= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 >= Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 >= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 >= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 >= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 >= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 >= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 >= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 >= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 >= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 >= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 >= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 >= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 >= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if AboveOrEqual(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if AboveOrEqual(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if AboveOrEqual(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if AboveOrEqual(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if AboveOrEqual(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if AboveOrEqual(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if AboveOrEqual(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if AboveOrEqual(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if AboveOrEqual(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if AboveOrEqual(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if AboveOrEqual(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if AboveOrEqual(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if AboveOrEqual(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if AboveOrEqual(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if AboveOrEqual(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if AboveOrEqual(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if AboveOrEqual(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if AboveOrEqual(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if AboveOrEqual(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if AboveOrEqual(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if AboveOrEqual(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if AboveOrEqual(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if AboveOrEqual(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if AboveOrEqual(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.IfMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
begin
  Check(AFunction, ParameterArray, IfMinParameterCount, IfMaxParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  P := TCustomParser(FParser);
  if AsBoolean(P, ParameterArray[AIndex]) then
    Result := AsValue(P, ParameterArray[BIndex])
  else
    if Length(ParameterArray) > CIndex then
      Result := AsValue(P, ParameterArray[CIndex])
    else
      Result := EmptyValue;
end;

function TMethod.IfThenMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  AValue, BValue, CValue: TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AValue := ParameterArray[AIndex].Value;
  BValue := ParameterArray[BIndex].Value;
  CValue := ParameterArray[CIndex].Value;
  if Boolean(Convert(AValue, vtInteger).Signed32) then Result := BValue
  else Result := CValue;
end;

function TMethod.IsZeroMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TParser;
  AValue: TValue;
  BValue: Double;
begin
  Check(AFunction, ParameterArray, IsZeroMinParameterCount, IsZeroMaxParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  P := TParser(FParser);
  AValue := ParameterArray[AIndex].Value;
  if Length(ParameterArray) > BIndex then
  begin
    BValue := Convert(ParameterArray[BIndex].Value, vtExtended).Float80;
    if IsZero(Convert(AValue, vtExtended).Float80, BValue) then
      AssignInteger(Result, P.TrueValue)
    else
      AssignInteger(Result, P.FalseValue);
  end
  else
    if IsZero(Convert(AValue, vtExtended).Float80) then
      AssignInteger(Result, P.TrueValue)
    else
      AssignInteger(Result, P.FalseValue);
end;

function TMethod.BelowMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 < RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 < RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 < Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 < Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 < Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 < RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 < RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 < Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 < Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 < RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 < RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 < RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 < Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 < RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 < RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 < RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 < RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 < RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 < RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 < RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if Below(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Below(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Below(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Below(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Below(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Below(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Below(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if Below(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Below(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Below(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Below(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Below(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Below(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Below(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if Below(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Below(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if Below(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Below(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if Below(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Below(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if Below(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if Below(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if Below(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if Below(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.BelowOrEqualMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 <= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 <= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 <= Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 <= Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 <= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 <= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 <= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 <= Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 <= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 <= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 <= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 <= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 <= Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 <= RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 <= RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 <= RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 <= RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 <= RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 <= RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 <= RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if BelowOrEqual(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if BelowOrEqual(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if BelowOrEqual(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if BelowOrEqual(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if BelowOrEqual(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if BelowOrEqual(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if BelowOrEqual(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if BelowOrEqual(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if BelowOrEqual(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if BelowOrEqual(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if BelowOrEqual(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if BelowOrEqual(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if BelowOrEqual(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if BelowOrEqual(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if BelowOrEqual(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if BelowOrEqual(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if BelowOrEqual(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if BelowOrEqual(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if BelowOrEqual(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if BelowOrEqual(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if BelowOrEqual(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if BelowOrEqual(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if BelowOrEqual(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if BelowOrEqual(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.MultiplyMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := Operation(LValue, RValue, otMultiply);
end;

function TMethod.NewMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
  Parameter: TParameter;
  S: string;
  Value: PValue;
  BType: PType;
begin
  Check(AFunction, ParameterArray, NewMinParameterCount, NewMaxParameterCount);
  Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
  P := TCustomParser(FParser);
  Parameter := GetParameter(P, ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Trim(Parameter.Text);
  if Assigned(P.FindFunction(S)) then AssignInteger(Result, P.FalseValue)
  else begin
    Parameter := GetParameter(P, ParameterArray[BIndex]);
    P.BeginUpdate;
    try
      New(Value);
      try
        Value^ := Parameter.Value;
        if P.GetType(Parameter.THandle, BType) then Value^ := Convert(Value^, BType.ValueType);
        P.AddVariable(S, Value^, AsBoolean(P, ParameterArray, CIndex, False));
        FContainer.SetVariable(S, Value);
      except
        Dispose(Value);
        raise;
      end;
    finally
      P.EndUpdate;
    end;
    AssignInteger(Result, P.TrueValue);
  end;
end;

function TMethod.NotEqualMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  P: TParser;
begin
  P := TParser(FParser);
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned8 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned8) <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned8 <> RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Unsigned8 <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned8 <> RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned8 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned8 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Unsigned8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Unsigned8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Unsigned8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed8 <> Int64(RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed8 <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed8 <> Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed8 <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed8 <> Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed8 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed8 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Signed8, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Signed8, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Signed8, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtWord:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned16 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned16) <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned16 <> RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned16) <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned16 <> RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Unsigned16 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned16 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Unsigned16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Unsigned16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Unsigned16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed16 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed16 <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed16 <> Int64(RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed16 <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed16 <> Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed16 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed16 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Signed16, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Signed16, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Signed16, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte:
          if LValue.Unsigned32 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if Int64(LValue.Unsigned32) <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Unsigned32 <> RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if Int64(LValue.Unsigned32) <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Unsigned32 <> RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if Int64(LValue.Unsigned32) <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Unsigned32 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Unsigned32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Unsigned32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Unsigned32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed32 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed32 <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed32 <> RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed32 <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed32 <> Int64(RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed32 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed32 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Signed32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Signed32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Signed32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte:
          if LValue.Signed64 <> RValue.Unsigned8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if LValue.Signed64 <> RValue.Signed8 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if LValue.Signed64 <> RValue.Unsigned16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if LValue.Signed64 <> RValue.Signed16 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if LValue.Signed64 <> RValue.Unsigned32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if LValue.Signed64 <> RValue.Signed32 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if LValue.Signed64 <> RValue.Signed64 then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Signed64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Signed64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Signed64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte:
          if not Equal(LValue.Float32, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if not Equal(LValue.Float32, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if not Equal(LValue.Float32, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if not Equal(LValue.Float32, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if not Equal(LValue.Float32, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if not Equal(LValue.Float32, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if not Equal(LValue.Float32, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Float32, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Float32, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Float32, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte:
          if not Equal(LValue.Float64, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if not Equal(LValue.Float64, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if not Equal(LValue.Float64, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if not Equal(LValue.Float64, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if not Equal(LValue.Float64, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if not Equal(LValue.Float64, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if not Equal(LValue.Float64, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Float64, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Float64, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Float64, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte:
          if not Equal(LValue.Float80, RValue.Unsigned8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtShortint:
          if not Equal(LValue.Float80, RValue.Signed8) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtWord:
          if not Equal(LValue.Float80, RValue.Unsigned16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSmallint:
          if not Equal(LValue.Float80, RValue.Signed16) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtLongword:
          if not Equal(LValue.Float80, RValue.Unsigned32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInteger:
          if not Equal(LValue.Float80, RValue.Signed32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtInt64:
          if not Equal(LValue.Float80, RValue.Signed64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtSingle:
          if not Equal(LValue.Float80, RValue.Float32) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtDouble:
          if not Equal(LValue.Float80, RValue.Float64) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
        vtExtended:
          if not Equal(LValue.Float80, RValue.Float80) then Result.Signed32 := P.TrueValue
          else Result.Signed32 := P.FalseValue;
      end;
  end;
  Result.ValueType := vtInteger;
end;

function TMethod.NotMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  Result := EmptyValue;
  case Value.ValueType of
    vtByte: AssignSmallint(Result, not Value.Unsigned8);
    vtShortint: AssignSmallint(Result, not Value.Signed8);
    vtWord: AssignInteger(Result, not Value.Unsigned16);
    vtSmallint: AssignInteger(Result, not Value.Signed16);
    vtLongword: AssignInt64(Result, not Value.Unsigned32);
    vtInteger: AssignInt64(Result, not Value.Signed32);
    vtInt64: AssignInt64(Result, not Value.Signed64);
    vtSingle: AssignInt64(Result, not Round(Value.Float32));
    vtDouble: AssignInt64(Result, not Round(Value.Float64));
    vtExtended: AssignInt64(Result, not Round(Value.Float80));
  end;
end;

function TMethod.OrMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned8 or RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned8) or RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned8 or RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned8 or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned8 or RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned8 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned8 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned8 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned8 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned8 or Round(RValue.Float80));
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed8 or Int64(RValue.Unsigned8));
        vtShortint: AssignInteger(Result, LValue.Signed8 or RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed8 or Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed8 or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed8 or Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed8 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed8 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed8 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed8 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed8 or Round(RValue.Float80));
      end;
    vtWord:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned16 or RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned16) or RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned16 or RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, Int64(LValue.Unsigned16) or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned16 or RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned16 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned16 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned16 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned16 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned16 or Round(RValue.Float80));
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed16 or RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed16 or RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed16 or Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed16 or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed16 or Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed16 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed16 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed16 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed16 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed16 or Round(RValue.Float80));
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Unsigned32 or RValue.Unsigned8);
        vtShortint: AssignInt64(Result, Int64(LValue.Unsigned32) or RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Unsigned32 or RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, Int64(LValue.Unsigned32) or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned32 or RValue.Unsigned32);
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned32) or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned32 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned32 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned32 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned32 or Round(RValue.Float80));
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed32 or RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed32 or RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed32 or RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed32 or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed32 or Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed32 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed32 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed32 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed32 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed32 or Round(RValue.Float80));
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed64 or RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed64 or RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed64 or RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed64 or RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed64 or RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Signed64 or RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed64 or RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed64 or Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed64 or Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed64 or Round(RValue.Float80));
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float32) or RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float32) or RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float32) or RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float32) or RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float32) or RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float32) or RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float32) or RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float32) or Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float32) or Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) or Round(RValue.Float80));
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte: AssignDouble(Result, Round(LValue.Float64) or RValue.Unsigned8);
        vtShortint: AssignDouble(Result, Round(LValue.Float64) or RValue.Signed8);
        vtWord: AssignDouble(Result, Round(LValue.Float64) or RValue.Unsigned16);
        vtSmallint: AssignDouble(Result, Round(LValue.Float64) or RValue.Signed16);
        vtLongword: AssignDouble(Result, Round(LValue.Float64) or RValue.Unsigned32);
        vtInteger: AssignDouble(Result, Round(LValue.Float64) or RValue.Signed32);
        vtInt64: AssignDouble(Result, Round(LValue.Float64) or RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float64) or Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float64) or Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float64) or Round(RValue.Float80));
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float80) or RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float80) or RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float80) or RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float80) or RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float80) or RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float80) or RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float80) or RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float80) or Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float80) or Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float80) or Round(RValue.Float80));
      end;
  end;
end;

function TMethod.ParseMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TParser;
  PM: TParseManager;
begin
  Enter(MethodLock);
  try
    Check(AFunction, ParameterArray, ParseMinParameterCount, ParseMaxParameterCount);
    if Length(ParameterArray) > BIndex then
      Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
    P := TParser(FParser);
    PM := TParseManager(P.ParseManager);
    PM.Optimization := (Length(ParameterArray) > BIndex) and (Convert(ParameterArray[BIndex].Value, vtInteger).Signed32 = P.TrueValue);
    Result := PM.AsValue(QuoteDouble(ParameterArray[AIndex].Text, P.FData.FArray[P.ParseHandle].Name, P.Bracket));
  finally
    Leave(MethodLock);
  end;
end;

function TMethod.PredMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  case Value.ValueType of
    vtByte: AssignSmallint(Result, Pred(Value.Unsigned8));
    vtShortint: AssignSmallint(Result, Pred(Value.Signed8));
    vtWord: AssignInteger(Result, Pred(Value.Unsigned16));
    vtSmallint: AssignInteger(Result, Pred(Value.Signed16));
    vtLongword: AssignInt64(Result, Pred(Value.Unsigned32));
    vtInteger: AssignInt64(Result, Pred(Value.Signed32));
    vtInt64: AssignInt64(Result, Pred(Value.Signed64));
    vtSingle: AssignInt64(Result, Pred(Round(Value.Float32)));
    vtDouble: AssignInt64(Result, Pred(Round(Value.Float64)));
    vtExtended: AssignInt64(Result, Pred(Round(Value.Float80)));
  end;
end;

function TMethod.RepeatMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
begin
  Check(AFunction, ParameterArray, RepeatParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  P := TCustomParser(FParser);
  repeat
    Result := Operation(Result, AsValue(P, ParameterArray[AIndex]), otAdd);
  until AsBoolean(P, ParameterArray[BIndex]);
end;

function TMethod.SameValueMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TParser;
  AValue, BValue: TValue;
  CValue: Double;
begin
  Check(AFunction, ParameterArray, SameValueMinParameterCount, SameValueMaxParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  P := TParser(FParser);
  AValue := ParameterArray[AIndex].Value;
  BValue := ParameterArray[BIndex].Value;
  if Length(ParameterArray) > CIndex then
  begin
    CValue := Convert(ParameterArray[CIndex].Value, vtExtended).Float80;
    if Equal(Convert(AValue, vtExtended).Float80, Convert(BValue, vtExtended).Float80, CValue) then
      AssignInteger(Result, P.TrueValue)
    else
      AssignInteger(Result, P.FalseValue);
  end
  else
    if Equal(Convert(AValue, vtExtended).Float80, Convert(BValue, vtExtended).Float80) then
      AssignInteger(Result, P.TrueValue)
    else
      AssignInteger(Result, P.FalseValue);
end;

function TMethod.ScriptMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  I: Integer;
begin
  Check(AFunction, ParameterArray, ScriptMinParameterCount, ScriptMaxParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  Result := EmptyValue;
  for I := Low(ParameterArray) to High(ParameterArray) do
    Result := Operation(Result, AsValue(TCustomParser(FParser), ParameterArray[I]), otAdd);
end;

function TMethod.SetDecimalSeparatorMethod(const AFunction: PFunction;
  const AType: PType; const ParameterArray: TParameterArray): TValue;
var
  P: TParser;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, True);
  P := TParser(FParser);
  if StrLen(ParameterArray[AIndex].Text) = 1 then
  begin
    {$IFDEF DELPHI_XE}
    FormatSettings.DecimalSeparator := ParameterArray[AIndex].Text[0];
    {$ELSE}
    DecimalSeparator := ParameterArray[AIndex].Text[0];
    {$ENDIF}
    AssignInteger(Result, P.TrueValue);
  end
  else AssignInteger(Result, P.FalseValue);
end;

function TMethod.SetEpsilonMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TParser;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  P := TParser(FParser);
  Epsilon := Convert(ParameterArray[AIndex].Value, vtExtended).Float80;
  AssignInteger(Result, P.TrueValue);
end;

function TMethod.SetMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
  Parameter: TParameter;
  S: string;
  BFunction: PFunction;
  Variable: PFunctionVariable;
begin
  Check(AFunction, ParameterArray, SetParameterCount);
  Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
  P := TCustomParser(FParser);
  Parameter := GetParameter(P, ParameterArray[AIndex]);
  Check(FParser, AFunction, AIndex, Parameter.THandle, True);
  S := Trim(Parameter.Text);
  BFunction := P.FindFunction(S);
  if Assigned(BFunction) and (BFunction.Method.MethodType = mtVariable) then
  begin
    Variable := @BFunction.Method.Variable;
    case Variable.VariableType of
      vtValue: Variable.Variable^ := AsValue(P, ParameterArray[BIndex]);
      vtLiveValue: AssignValue(Variable.LiveVariable, AsValue(P, ParameterArray[BIndex]));
    end;
    AssignInteger(Result, P.TrueValue);
  end
  else AssignInteger(Result, P.FalseValue);
end;

function TMethod.ShlMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned8 shl RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned8) shl RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned8 shl RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned8 shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned8 shl RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned8 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned8 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned8 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned8 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned8 shl Round(RValue.Float80));
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed8 shl Int64(RValue.Unsigned8));
        vtShortint: AssignInteger(Result, LValue.Signed8 shl RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed8 shl Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed8 shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed8 shl Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed8 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed8 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed8 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed8 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed8 shl Round(RValue.Float80));
      end;
    vtWord:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned16 shl RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned16) shl RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned16 shl RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, Int64(LValue.Unsigned16) shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned16 shl RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned16 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned16 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned16 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned16 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned16 shl Round(RValue.Float80));
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed16 shl RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed16 shl RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed16 shl Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed16 shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed16 shl Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed16 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed16 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed16 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed16 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed16 shl Round(RValue.Float80));
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Unsigned32 shl RValue.Unsigned8);
        vtShortint: AssignInt64(Result, Int64(LValue.Unsigned32) shl RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Unsigned32 shl RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, Int64(LValue.Unsigned32) shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned32 shl RValue.Unsigned32);
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned32) shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned32 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned32 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned32 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned32 shl Round(RValue.Float80));
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed32 shl RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed32 shl RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed32 shl RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed32 shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed32 shl Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed32 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed32 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed32 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed32 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed32 shl Round(RValue.Float80));
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed64 shl RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed64 shl RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed64 shl RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed64 shl RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed64 shl RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Signed64 shl RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed64 shl RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed64 shl Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed64 shl Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed64 shl Round(RValue.Float80));
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float32) shl RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float32) shl RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float32) shl RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float32) shl RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float32) shl RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float32) shl RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float32) shl RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float32) shl Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float32) shl Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) shl Round(RValue.Float80));
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte: AssignDouble(Result, Round(LValue.Float64) shl RValue.Unsigned8);
        vtShortint: AssignDouble(Result, Round(LValue.Float64) shl RValue.Signed8);
        vtWord: AssignDouble(Result, Round(LValue.Float64) shl RValue.Unsigned16);
        vtSmallint: AssignDouble(Result, Round(LValue.Float64) shl RValue.Signed16);
        vtLongword: AssignDouble(Result, Round(LValue.Float64) shl RValue.Unsigned32);
        vtInteger: AssignDouble(Result, Round(LValue.Float64) shl RValue.Signed32);
        vtInt64: AssignDouble(Result, Round(LValue.Float64) shl RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float64) shl Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float64) shl Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float64) shl Round(RValue.Float80));
      end;
    vtExtended:
      begin
        case RValue.ValueType of
          vtByte: Result.Float80 := Round(LValue.Float64) shl RValue.Unsigned8;
          vtShortint: Result.Float80 := Round(LValue.Float64) shl RValue.Signed8;
          vtWord: Result.Float80 := Round(LValue.Float64) shl RValue.Unsigned16;
          vtSmallint: Result.Float80 := Round(LValue.Float64) shl RValue.Signed16;
          vtLongword: Result.Float80 := Round(LValue.Float64) shl RValue.Unsigned32;
          vtInteger: Result.Float80 := Round(LValue.Float64) shl RValue.Signed32;
          vtInt64: Result.Float80 := Round(LValue.Float64) shl RValue.Signed64;
          vtSingle: Result.Float80 := Round(LValue.Float64) shl Round(RValue.Float32);
          vtDouble: Result.Float80 := Round(LValue.Float64) shl Round(RValue.Float64);
        end;
        Result.ValueType := vtExtended;
      end;
  end;
end;

function TMethod.ShrMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned8 shr RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned8) shr RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned8 shr RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned8 shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned8 shr RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned8 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned8 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned8 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned8 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned8 shr Round(RValue.Float80));
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed8 shr Int64(RValue.Unsigned8));
        vtShortint: AssignInteger(Result, LValue.Signed8 shr RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed8 shr Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed8 shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed8 shr Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed8 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed8 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed8 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed8 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed8 shr Round(RValue.Float80));
      end;
    vtWord:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned16 shr RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned16) shr RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned16 shr RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, Int64(LValue.Unsigned16) shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned16 shr RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned16 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned16 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned16 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned16 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned16 shr Round(RValue.Float80));
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed16 shr RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed16 shr RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed16 shr Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed16 shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed16 shr Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed16 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed16 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed16 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed16 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed16 shr Round(RValue.Float80));
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Unsigned32 shr RValue.Unsigned8);
        vtShortint: AssignInt64(Result, Int64(LValue.Unsigned32) shr RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Unsigned32 shr RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, Int64(LValue.Unsigned32) shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned32 shr RValue.Unsigned32);
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned32) shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned32 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned32 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned32 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned32 shr Round(RValue.Float80));
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed32 shr RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed32 shr RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed32 shr RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed32 shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed32 shr Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed32 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed32 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed32 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed32 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed32 shr Round(RValue.Float80));
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed64 shr RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed64 shr RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed64 shr RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed64 shr RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed64 shr RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Signed64 shr RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed64 shr RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed64 shr Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed64 shr Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed64 shr Round(RValue.Float80));
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float32) shr RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float32) shr RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float32) shr RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float32) shr RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float32) shr RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float32) shr RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float32) shr RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float32) shr Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float32) shr Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) shr Round(RValue.Float80));
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte: AssignDouble(Result, Round(LValue.Float64) shr RValue.Unsigned8);
        vtShortint: AssignDouble(Result, Round(LValue.Float64) shr RValue.Signed8);
        vtWord: AssignDouble(Result, Round(LValue.Float64) shr RValue.Unsigned16);
        vtSmallint: AssignDouble(Result, Round(LValue.Float64) shr RValue.Signed16);
        vtLongword: AssignDouble(Result, Round(LValue.Float64) shr RValue.Unsigned32);
        vtInteger: AssignDouble(Result, Round(LValue.Float64) shr RValue.Signed32);
        vtInt64: AssignDouble(Result, Round(LValue.Float64) shr RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float64) shr Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float64) shr Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float64) shr Round(RValue.Float80));
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float80) shr RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float80) shr RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float80) shr RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float80) shr RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float80) shr RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float80) shr RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float80) shr RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float80) shr Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float80) shr Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float80) shr Round(RValue.Float80));
      end;
  end;
end;

function TMethod.StrToFloatDefMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, AIndex, ParameterArray[AIndex].THandle, True);
  Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
  AssignDouble(Result, StrToFloatDef(ParameterArray[AIndex].Text, Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMethod.StrToFloatMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, True);
  AssignDouble(Result, StrToFloat(ParameterArray[AIndex].Text));
end;

function TMethod.StrToIntDefMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, AIndex, ParameterArray[AIndex].THandle, True);
  Check(FParser, AFunction, BIndex, ParameterArray[BIndex].THandle, False);
  AssignInt64(Result, StrToInt64Def(ParameterArray[AIndex].Text, Convert(ParameterArray[BIndex].Value, vtInt64).Signed64));
end;

function TMethod.StrToIntMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, True);
  AssignInt64(Result, StrToInt64(ParameterArray[AIndex].Text));
end;

function TMethod.SuccMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  case Value.ValueType of
    vtByte: AssignSmallint(Result, Succ(Value.Unsigned8));
    vtShortint: AssignSmallint(Result, Succ(Value.Signed8));
    vtWord: AssignInteger(Result, Succ(Value.Unsigned16));
    vtSmallint: AssignInteger(Result, Succ(Value.Signed16));
    vtLongword: AssignInt64(Result, Succ(Value.Unsigned32));
    vtInteger: AssignInt64(Result, Succ(Value.Signed32));
    vtInt64: AssignInt64(Result, Succ(Value.Signed64));
    vtSingle: AssignInt64(Result, Succ(Round(Value.Float32)));
    vtDouble: AssignInt64(Result, Succ(Round(Value.Float64)));
    vtExtended: AssignInt64(Result, Succ(Round(Value.Float80)));
  end;
end;

function TMethod.TrueMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignInteger(Result, TParser(FParser).TrueValue);
end;

function TMethod.VoidMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  Result := EmptyValue;
end;

function TMethod.WhileMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  P: TCustomParser;
begin
  Check(AFunction, ParameterArray, WhileParameterCount);
  Check(FParser, AFunction, ParameterArray, False);
  P := TCustomParser(FParser);
  while AsBoolean(P, ParameterArray[AIndex]) do
    Result := Operation(Result, AsValue(P, ParameterArray[BIndex]), otAdd);
end;

function TMethod.XorMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  Result := EmptyValue;
  case LValue.ValueType of
    vtByte:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned8 xor RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned8) xor RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned8 xor RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, LValue.Unsigned8 xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned8 xor RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned8 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned8 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned8 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned8 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned8 xor Round(RValue.Float80));
      end;
    vtShortint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed8 xor Int64(RValue.Unsigned8));
        vtShortint: AssignInteger(Result, LValue.Signed8 xor RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed8 xor Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed8 xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed8 xor Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed8 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed8 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed8 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed8 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed8 xor Round(RValue.Float80));
      end;
    vtWord:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Unsigned16 xor RValue.Unsigned8);
        vtShortint: AssignInteger(Result, Int64(LValue.Unsigned16) xor RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Unsigned16 xor RValue.Unsigned16);
        vtSmallint: AssignInteger(Result, Int64(LValue.Unsigned16) xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned16 xor RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Unsigned16 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned16 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned16 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned16 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned16 xor Round(RValue.Float80));
      end;
    vtSmallint:
      case RValue.ValueType of
        vtByte: AssignInteger(Result, LValue.Signed16 xor RValue.Unsigned8);
        vtShortint: AssignInteger(Result, LValue.Signed16 xor RValue.Signed8);
        vtWord: AssignInteger(Result, LValue.Signed16 xor Int64(RValue.Unsigned16));
        vtSmallint: AssignInteger(Result, LValue.Signed16 xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed16 xor Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed16 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed16 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed16 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed16 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed16 xor Round(RValue.Float80));
      end;
    vtLongword:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Unsigned32 xor RValue.Unsigned8);
        vtShortint: AssignInt64(Result, Int64(LValue.Unsigned32) xor RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Unsigned32 xor RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, Int64(LValue.Unsigned32) xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Unsigned32 xor RValue.Unsigned32);
        vtInteger: AssignInt64(Result, Int64(LValue.Unsigned32) xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Unsigned32 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Unsigned32 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Unsigned32 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Unsigned32 xor Round(RValue.Float80));
      end;
    vtInteger:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed32 xor RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed32 xor RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed32 xor RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed32 xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed32 xor Int64(RValue.Unsigned32));
        vtInteger: AssignInt64(Result, LValue.Signed32 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed32 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed32 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed32 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed32 xor Round(RValue.Float80));
      end;
    vtInt64:
      case RValue.ValueType of
        vtByte: AssignInt64(Result, LValue.Signed64 xor RValue.Unsigned8);
        vtShortint: AssignInt64(Result, LValue.Signed64 xor RValue.Signed8);
        vtWord: AssignInt64(Result, LValue.Signed64 xor RValue.Unsigned16);
        vtSmallint: AssignInt64(Result, LValue.Signed64 xor RValue.Signed16);
        vtLongword: AssignInt64(Result, LValue.Signed64 xor RValue.Unsigned32);
        vtInteger: AssignInt64(Result, LValue.Signed64 xor RValue.Signed32);
        vtInt64: AssignInt64(Result, LValue.Signed64 xor RValue.Signed64);
        vtSingle: AssignSingle(Result, LValue.Signed64 xor Round(RValue.Float32));
        vtDouble: AssignDouble(Result, LValue.Signed64 xor Round(RValue.Float64));
        vtExtended: AssignDouble(Result, LValue.Signed64 xor Round(RValue.Float80));
      end;
    vtSingle:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float32) xor RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float32) xor RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float32) xor RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float32) xor RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float32) xor RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float32) xor RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float32) xor RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float32) xor Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float32) xor Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float32) xor Round(RValue.Float80));
      end;
    vtDouble:
      case RValue.ValueType of
        vtByte: AssignDouble(Result, Round(LValue.Float64) xor RValue.Unsigned8);
        vtShortint: AssignDouble(Result, Round(LValue.Float64) xor RValue.Signed8);
        vtWord: AssignDouble(Result, Round(LValue.Float64) xor RValue.Unsigned16);
        vtSmallint: AssignDouble(Result, Round(LValue.Float64) xor RValue.Signed16);
        vtLongword: AssignDouble(Result, Round(LValue.Float64) xor RValue.Unsigned32);
        vtInteger: AssignDouble(Result, Round(LValue.Float64) xor RValue.Signed32);
        vtInt64: AssignDouble(Result, Round(LValue.Float64) xor RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float64) xor Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float64) xor Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float64) xor Round(RValue.Float80));
      end;
    vtExtended:
      case RValue.ValueType of
        vtByte: AssignExtended(Result, Round(LValue.Float80) xor RValue.Unsigned8);
        vtShortint: AssignExtended(Result, Round(LValue.Float80) xor RValue.Signed8);
        vtWord: AssignExtended(Result, Round(LValue.Float80) xor RValue.Unsigned16);
        vtSmallint: AssignExtended(Result, Round(LValue.Float80) xor RValue.Signed16);
        vtLongword: AssignExtended(Result, Round(LValue.Float80) xor RValue.Unsigned32);
        vtInteger: AssignExtended(Result, Round(LValue.Float80) xor RValue.Signed32);
        vtInt64: AssignExtended(Result, Round(LValue.Float80) xor RValue.Signed64);
        vtSingle: AssignExtended(Result, Round(LValue.Float80) xor Round(RValue.Float32));
        vtDouble: AssignExtended(Result, Round(LValue.Float80) xor Round(RValue.Float64));
        vtExtended: AssignExtended(Result, Round(LValue.Float80) xor Round(RValue.Float80));
      end;
  end;
end;

{ TMathMethod }

function TMathMethod.AbsMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  Result := Positive(Value);
end;

function TMathMethod.ArcCosHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and (AValue < 1) then
    AssignDouble(Result, NaN)
  else
    AssignDouble(Result, ArcCosH(AValue));
end;

function TMathMethod.ArcCosMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and ((AValue < -1) or (AValue > 1)) then
    AssignDouble(Result, NaN)
  else
    AssignDouble(Result, ArcCos(AValue));
end;

function TMathMethod.ArcCoTanHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcCotH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcCoTanMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcCot(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcCscHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcCscH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcCscMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcCsc(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcSecHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcSecH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcSecMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcSec(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcSinHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcSinH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.ArcSinMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and ((AValue < -1) or (AValue > 1)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, ArcSin(AValue));
end;

function TMathMethod.ArcTan2Method(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, ArcTan2(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.ArcTanHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and ((AValue < -1) or (AValue > 1)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, ArcTanH(AValue));
end;

function TMathMethod.ArcTanMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, ArcTan(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CeilMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Ceil(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CosHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, CosH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CosMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Cos(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CoTanHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, CotH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CoTanMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and IsZero(Sin(AValue)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, CoTan(AValue));
end;

function TMathMethod.CscHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and IsZero(AValue) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, CscH(AValue));
end;

function TMathMethod.CscMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and IsZero(Sin(AValue)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, Csc(AValue));
end;

function TMathMethod.CycleToDegMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, CycleToDeg(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CycleToGradMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, CycleToGrad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.CycleToRadMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, CycleToRad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.DateMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignDouble(Result, Date);
end;

function TMathMethod.DayMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wDay);
end;

function TMathMethod.DayOfWeekMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wDayOfWeek);
end;

function TMathMethod.DegreeMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  AssignDouble(Result, Power(Convert(LValue, vtExtended).Float80, Convert(RValue, vtExtended).Float80));
end;

function TMathMethod.DegToCycleMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, DegToCycle(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.DegToGradMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, DegToGrad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.DegToRadMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, DegToRad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.DivMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  AValue, BValue: Integer;
begin
  AValue := Convert(LValue, vtInteger).Signed32;
  BValue := Convert(RValue, vtInteger).Signed32;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and (BValue = 0) then
    AssignDouble(Result, NaN)
  else AssignInteger(Result, AValue div BValue);
end;

function TMathMethod.EncodeDateMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Year, Month, Day: Word;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  Year := Convert(ParameterArray[AIndex].Value, vtWord).Unsigned16;
  Month := Convert(ParameterArray[BIndex].Value, vtWord).Unsigned16;
  Day := Convert(ParameterArray[CIndex].Value, vtWord).Unsigned16;
  AssignDouble(Result, EncodeDate(Year, Month, Day));
end;

function TMathMethod.EncodeDateTimeMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  Year := Convert(ParameterArray[AIndex].Value, vtWord).Unsigned16;
  Month := Convert(ParameterArray[BIndex].Value, vtWord).Unsigned16;
  Day := Convert(ParameterArray[CIndex].Value, vtWord).Unsigned16;
  Hour := Convert(ParameterArray[DIndex].Value, vtWord).Unsigned16;
  Min := Convert(ParameterArray[EIndex].Value, vtWord).Unsigned16;
  Sec := Convert(ParameterArray[FIndex].Value, vtWord).Unsigned16;
  MSec := Convert(ParameterArray[GIndex].Value, vtWord).Unsigned16;
  AssignDouble(Result, EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec));
end;

function TMathMethod.EncodeTimeMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Hour, Min, Sec, MSec: Word;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  Hour := Convert(ParameterArray[AIndex].Value, vtWord).Unsigned16;
  Min := Convert(ParameterArray[BIndex].Value, vtWord).Unsigned16;
  Sec := Convert(ParameterArray[CIndex].Value, vtWord).Unsigned16;
  MSec := Convert(ParameterArray[DIndex].Value, vtWord).Unsigned16;
  AssignDouble(Result, EncodeTime(Hour, Min, Sec, MSec));
end;

function TMathMethod.ExpMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Exp(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.FactorialMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignInt64(Result, Factorial(Convert(Value, vtSmallint).Signed16));
end;

function TMathMethod.FloorMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Floor(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.FracMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Frac(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.GetDayMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, DayOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetDayOfWeekMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, DayOfWeek(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetHourMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, HourOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetMinuteMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, MinuteOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetMonthMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, MonthOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetMSecondMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, MilliSecondOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetSecondMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, SecondOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GetTickCount(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignLongword(Result, {$IFDEF DELPHI_XE7}WinApi.Windows.GetTickCount{$ELSE}Windows.GetTickCount{$ENDIF});
end;

function TMathMethod.GetYearMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignWord(Result, YearOf(Convert(ParameterArray[AIndex].Value, vtExtended).Float80));
end;

function TMathMethod.GradToCycleMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, GradToCycle(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.GradToDegMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, GradToDeg(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.GradToRadMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, GradToRad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.HourMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wHour);
end;

function TMathMethod.HypotMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, Hypot(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.IntMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Int(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.IntPowerMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, IntPower(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtInteger).Signed32));
end;

function TMathMethod.LdexpMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, Ldexp(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtInteger).Signed32));
end;

function TMathMethod.LgMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Log10(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.LnMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Ln(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.LnXP1Method(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, LnXP1(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.Log10Method(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Log10(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.Log2Method(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Log2(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.LogMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
begin
  AssignDouble(Result, LogN(Convert(LValue, vtExtended).Float80,
    Convert(RValue, vtExtended).Float80));
end;

function TMathMethod.MaxIntValueMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TIntegerDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := IntegerArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignInteger(Result, MaxIntValue(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.MaxMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, Max(Convert(ParameterArray[AIndex].Value, vtExtended).Float80, Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.MaxValueMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignDouble(Result, MaxValue(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.MeanMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, Mean(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.MinIntValueMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TIntegerDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := IntegerArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignInteger(Result, MinIntValue(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.MinMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, Min(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.MinuteMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wMinute);
end;

function TMathMethod.MinValueMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignDouble(Result, MinValue(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.ModMethod(const AFunction: PFunction; const AType: PType;
  const LValue, RValue: TValue): TValue;
var
  AValue, BValue: Integer;
begin
  AValue := Convert(LValue, vtInteger).Signed32;
  BValue := Convert(RValue, vtInteger).Signed32;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and (BValue = 0) then
    AssignDouble(Result, NaN)
  else AssignInteger(Result, AValue mod BValue);
end;

function TMathMethod.MonthMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wMonth);
end;

function TMathMethod.MSecondMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wMilliseconds);
end;

function TMathMethod.NormMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignDouble(Result, Norm(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.PolyMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, BIndex, High(ParameterArray));
  try
    try
      AssignDouble(Result, Poly(Convert(ParameterArray[AIndex].Value, vtExtended).Float80, Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.PopnStdDevMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, PopnStdDev(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.PopnVarianceMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, PopnVariance(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.PowerMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, Power(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.RadToCycleMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, RadToCycle(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.RadToDegMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, RadToDeg(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.RadToGradMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, RadToGrad(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.RandGMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, RandG(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    Convert(ParameterArray[BIndex].Value, vtExtended).Float80));
end;

function TMathMethod.RandomFromMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignDouble(Result, RandomFrom(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.RandomMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignInteger(Result, Random(Convert(Value, vtInteger).Signed32));
end;

function TMathMethod.RandomRangeMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignInteger(Result, RandomRange(Convert(ParameterArray[AIndex].Value, vtInteger).Signed32,
    Convert(ParameterArray[BIndex].Value, vtInteger).Signed32));
end;

function TMathMethod.RoundMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Round(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.RoundToMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count);
  Check(FParser, AFunction, ParameterArray, False);
  AssignDouble(Result, RoundTo(Convert(ParameterArray[AIndex].Value, vtExtended).Float80,
    TRoundToRange(Convert(ParameterArray[BIndex].Value, vtInteger).Signed32)));
end;

function TMathMethod.SecHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, SecH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.SecMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and IsZero(Cos(AValue)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, Sec(AValue));
end;

function TMathMethod.SecondMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wSecond);
end;

function TMathMethod.SinHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, SinH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.SinMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Sin(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.SqrMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  case Value.ValueType of
    vtByte: AssignDouble(Result, Sqr(Value.Unsigned8));
    vtShortint: AssignDouble(Result, Sqr(Value.Signed8));
    vtWord: AssignDouble(Result, Sqr(Value.Unsigned16));
    vtSmallint: AssignDouble(Result, Sqr(Value.Signed16));
    vtLongword: AssignDouble(Result, Sqr(Value.Unsigned32));
    vtInteger: AssignDouble(Result, Sqr(Value.Signed32));
    vtInt64: AssignDouble(Result, Sqr(Value.Signed64));
    vtSingle: AssignDouble(Result, Sqr(Round(Value.Float32)));
    vtDouble: AssignDouble(Result, Sqr(Round(Value.Float64)));
    vtExtended: AssignDouble(Result, Sqr(Round(Value.Float80)));
  end;
end;

function TMathMethod.SqrtMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  case Value.ValueType of
    vtByte: AssignDouble(Result, Sqrt(Value.Unsigned8));
    vtShortint: AssignDouble(Result, Sqrt(Value.Signed8));
    vtWord: AssignDouble(Result, Sqrt(Value.Unsigned16));
    vtSmallint: AssignDouble(Result, Sqrt(Value.Signed16));
    vtLongword: AssignDouble(Result, Sqrt(Value.Unsigned32));
    vtInteger: AssignDouble(Result, Sqrt(Value.Signed32));
    {$IFDEF DELPHI_2005}
    vtInt64: AssignDouble(Result, Sqrt(Value.Signed64));
    {$ELSE}
    vtInt64: AssignDouble(Result, Sqrt(Value.Signed32));
    {$ENDIF}
    {$IFDEF DELPHI_2005}
    vtSingle: AssignDouble(Result, Sqrt(Round(Value.Float32)));
    {$ELSE}
    vtSingle: AssignDouble(Result, Sqrt(Integer(Round(Value.Float32))));
    {$ENDIF}
    {$IFDEF DELPHI_2005}
    vtDouble: AssignDouble(Result, Sqrt(Round(Value.Float64)));
    {$ELSE}
    vtDouble: AssignDouble(Result, Sqrt(Integer(Round(Value.Float64))));
    {$ENDIF}
    {$IFDEF DELPHI_2005}
    vtExtended: AssignDouble(Result, Sqrt(Round(Value.Float80)));
    {$ELSE}
    vtExtended: AssignDouble(Result, Sqrt(Integer(Round(Value.Float80))));
    {$ENDIF}
  end;
end;

function TMathMethod.StdDevMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, StdDev(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.SumIntMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TIntegerDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := IntegerArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, SumInt(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.SumMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, Sum(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.SumOfSquaresMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    AssignDouble(Result, SumOfSquares(Parameter));
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.TanHMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, TanH(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.TanMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
var
  AValue: Double;
begin
  AValue := Convert(Value, vtExtended).Float80;
  if (etZeroDivide in TParser(FParser).ExceptionTypes) and IsZero(Cos(AValue)) then
    AssignDouble(Result, NaN)
  else AssignDouble(Result, Tan(AValue));
end;

function TMathMethod.TimeMethod(const AFunction: PFunction; const AType: PType): TValue;
begin
  AssignDouble(Result, Time);
end;

function TMathMethod.TotalVarianceMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, TotalVariance(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.TruncMethod(const AFunction: PFunction; const AType: PType;
  const Value: TValue): TValue;
begin
  AssignDouble(Result, Trunc(Convert(Value, vtExtended).Float80));
end;

function TMathMethod.VarianceMethod(const AFunction: PFunction; const AType: PType;
  const ParameterArray: TParameterArray): TValue;
var
  Parameter: TDoubleDynArray;
begin
  Check(AFunction, ParameterArray, AFunction.Method.Parameter.Count, vrBelow);
  Check(FParser, AFunction, ParameterArray, False);
  Parameter := FloatArray(ParameterArray, Low(ParameterArray), High(ParameterArray));
  try
    try
      AssignDouble(Result, Variance(Parameter));
    except
      AssignDouble(Result, NaN);
    end;
  finally
    Parameter := nil;
  end;
end;

function TMathMethod.YearMethod(const AFunction: PFunction; const AType: PType): TValue;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  AssignWord(Result, SystemTime.wYear);
end;

initialization
  InitializeCriticalSection(MethodLock);

finalization
  DeleteCriticalSection(MethodLock);

end.
