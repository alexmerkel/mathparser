{ *********************************************************************** }
{                                                                         }
{ ValueUtils                                                              }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ValueUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, ValueTypes;

type
  TOperationType = (otAdd, otSubtract, otMultiply, otIntegerDivide, otFloatDivide);

procedure AssignByte(var Target: TValue; const Source: Byte);
procedure AssignShortint(var Target: TValue; const Source: Shortint);
procedure AssignWord(var Target: TValue; const Source: Word);
procedure AssignSmallint(var Target: TValue; const Source: Smallint);
procedure AssignLongword(var Target: TValue; const Source: Longword);
procedure AssignInteger(var Target: TValue; const Source: Integer);
procedure AssignPointer(var Target: TValue; const Source: Pointer);
procedure AssignInt64(var Target: TValue; const Source: Int64);
procedure AssignSingle(var Target: TValue; const Source: Single);
procedure AssignDouble(var Target: TValue; const Source: Double);
procedure AssignExtended(var Target: TValue; const Source: Extended);

function AssignValue(var Target: TLiveValue; const Source: TValue): Boolean;

function MakeValue(const Value: Byte): TValue; overload;
function MakeValue(const Value: Shortint): TValue; overload;
function MakeValue(const Value: Word): TValue; overload;
function MakeValue(const Value: Smallint): TValue; overload;
function MakeValue(const Value: Longword): TValue; overload;
function MakeValue(const Value: Integer): TValue; overload;
function MakeValue(const Value: Int64): TValue; overload;
function MakeValue(const Value: Single): TValue; overload;
function MakeValue(const Value: Double): TValue; overload;
function MakeValue(const Value: Extended): TValue; overload;
function MakeValue(const Value: TLiveValue): TValue; overload;

function MakeLiveValue(var Value: Byte): TLiveValue; overload;
function MakeLiveValue(var Value: Shortint): TLiveValue; overload;
function MakeLiveValue(var Value: Word): TLiveValue; overload;
function MakeLiveValue(var Value: Smallint): TLiveValue; overload;
function MakeLiveValue(var Value: Longword): TLiveValue; overload;
function MakeLiveValue(var Value: Integer): TLiveValue; overload;
function MakeLiveValue(var Value: Int64): TLiveValue; overload;
function MakeLiveValue(var Value: Single): TLiveValue; overload;
function MakeLiveValue(var Value: Double): TLiveValue; overload;
function MakeLiveValue(var Value: Extended): TLiveValue; overload;

function GetValueType(const Value: Int64): TValueType;
function LessZero(const Value: TValue): Boolean;
function TextToValue(const Text: string): TValue;
function TryTextToValue(const Source: string; var Target: TValue): Boolean;
function ValueToText(const Value: TValue): string;
function Convert(const Value: TValue; ValueType: TValueType): TValue;
function Operation(const AValue, BValue: TValue; OperationType: TOperationType): TValue;
function Negative(const Value: TValue): TValue;
function Positive(const Value: TValue): TValue;

implementation

uses
  ValueConsts;

procedure AssignByte(var Target: TValue; const Source: Byte);
begin
  Target.Unsigned8 := Source;
  Target.ValueType := vtByte;
end;

procedure AssignShortint(var Target: TValue; const Source: Shortint);
begin
  Target.Signed8 := Source;
  Target.ValueType := vtShortint;
end;

procedure AssignWord(var Target: TValue; const Source: Word);
begin
  Target.Unsigned16 := Source;
  Target.ValueType := vtWord;
end;

procedure AssignSmallint(var Target: TValue; const Source: Smallint);
begin
  Target.Signed16 := Source;
  Target.ValueType := vtSmallint;
end;

procedure AssignLongword(var Target: TValue; const Source: Longword);
begin
  Target.Unsigned32 := Source;
  Target.ValueType := vtLongword;
end;

procedure AssignInteger(var Target: TValue; const Source: Integer);
begin
  Target.Signed32 := Source;
  Target.ValueType := vtInteger;
end;

procedure AssignPointer(var Target: TValue; const Source: Pointer);
begin
  Target.Signed32 := Integer(Source);
  Target.ValueType := vtInteger;
end;

procedure AssignInt64(var Target: TValue; const Source: Int64);
begin
  Target.Signed64 := Source;
  Target.ValueType := vtInt64;
end;

procedure AssignSingle(var Target: TValue; const Source: Single);
begin
  Target.Float32 := Source;
  Target.ValueType := vtSingle;
end;

procedure AssignDouble(var Target: TValue; const Source: Double);
begin
  Target.Float64 := Source;
  Target.ValueType := vtDouble;
end;

procedure AssignExtended(var Target: TValue; const Source: Extended);
begin
  Target.Float80 := Source;
  Target.ValueType := vtExtended;
end;

function AssignValue(var Target: TLiveValue; const Source: TValue): Boolean;
begin
  Result := True;
  case Target.ValueType of
    vtByte: Target.Unsigned8^ := Convert(Source, vtByte).Unsigned8;
    vtShortint: Target.Signed8^ := Convert(Source, vtShortint).Signed8;
    vtWord: Target.Unsigned16^ := Convert(Source, vtWord).Unsigned16;
    vtSmallint: Target.Signed16^ := Convert(Source, vtSmallint).Signed16;
    vtLongword: Target.Unsigned32^ := Convert(Source, vtLongword).Unsigned32;
    vtInteger: Target.Signed32^ := Convert(Source, vtInteger).Signed32;
    vtInt64: Target.Signed64^ := Convert(Source, vtInt64).Signed64;
    vtSingle: Target.Float32^ := Convert(Source, vtSingle).Float32;
    vtDouble: Target.Float64^ := Convert(Source, vtDouble).Float64;
    vtExtended: Target.Float80^ := Convert(Source, vtExtended).Float80;
  else Result := False;
  end;
end;

function MakeValue(const Value: Byte): TValue;
begin
  AssignByte(Result, Value);
end;

function MakeValue(const Value: Shortint): TValue;
begin
  AssignShortint(Result, Value);
end;

function MakeValue(const Value: Word): TValue;
begin
  AssignWord(Result, Value);
end;

function MakeValue(const Value: Smallint): TValue;
begin
  AssignSmallint(Result, Value);
end;

function MakeValue(const Value: Longword): TValue;
begin
  AssignLongword(Result, Value);
end;

function MakeValue(const Value: Integer): TValue;
begin
  AssignInteger(Result, Value);
end;

function MakeValue(const Value: Int64): TValue;
begin
  AssignInt64(Result, Value);
end;

function MakeValue(const Value: Single): TValue;
begin
  AssignSingle(Result, Value);
end;

function MakeValue(const Value: Double): TValue;
begin
  AssignDouble(Result, Value);
end;

function MakeValue(const Value: Extended): TValue;
begin
  AssignExtended(Result, Value);
end;

function MakeValue(const Value: TLiveValue): TValue;
begin
  case Value.ValueType of
    vtByte: Result := MakeValue(Value.Unsigned8^);
    vtShortint: Result := MakeValue(Value.Signed8^);
    vtWord: Result := MakeValue(Value.Unsigned16^);
    vtSmallint: Result := MakeValue(Value.Signed16^);
    vtLongword: Result := MakeValue(Value.Unsigned32^);
    vtInteger: Result := MakeValue(Value.Signed32^);
    vtInt64: Result := MakeValue(Value.Signed64^);
    vtSingle: Result := MakeValue(Value.Float32^);
    vtDouble: Result := MakeValue(Value.Float64^);
    vtExtended: Result := MakeValue(Value.Float80^);
  else FillChar(Result, SizeOf(TValue), 0);
  end;
end;

function MakeLiveValue(var Value: Byte): TLiveValue;
begin
  Result.Unsigned8 := @Value;
  Result.ValueType := vtByte;
end;

function MakeLiveValue(var Value: Shortint): TLiveValue;
begin
  Result.Signed8 := @Value;
  Result.ValueType := vtShortint;
end;

function MakeLiveValue(var Value: Word): TLiveValue;
begin
  Result.Unsigned16 := @Value;
  Result.ValueType := vtWord;
end;

function MakeLiveValue(var Value: Smallint): TLiveValue;
begin
  Result.Signed16 := @Value;
  Result.ValueType := vtSmallint;
end;

function MakeLiveValue(var Value: Longword): TLiveValue;
begin
  Result.Unsigned32 := @Value;
  Result.ValueType := vtLongword;
end;

function MakeLiveValue(var Value: Integer): TLiveValue;
begin
  Result.Signed32 := @Value;
  Result.ValueType := vtInteger;
end;

function MakeLiveValue(var Value: Int64): TLiveValue;
begin
  Result.Signed64 := @Value;
  Result.ValueType := vtInt64;
end;

function MakeLiveValue(var Value: Single): TLiveValue;
begin
  Result.Float32 := @Value;
  Result.ValueType := vtSingle;
end;

function MakeLiveValue(var Value: Double): TLiveValue;
begin
  Result.Float64 := @Value;
  Result.ValueType := vtDouble;
end;

function MakeLiveValue(var Value: Extended): TLiveValue;
begin
  Result.Float80 := @Value;
  Result.ValueType := vtExtended;
end;

function GetValueType(const Value: Int64): TValueType;
begin
  if Value > 0 then
    if Value > High(Longword) then Result := vtInt64
    else if Value > High(Word) then Result := vtLongword
    else if Value > High(Byte) then Result := vtWord
    else Result := vtByte
  else
    if Value < - High(Integer) - 1 then Result := vtInt64
    else if Value < - High(Smallint) - 1 then Result := vtInteger
    else if Value < - High(Shortint) - 1 then Result := vtSmallint
    else Result := vtShortint;
end;

function LessZero(const Value: TValue): Boolean;
begin
  Result := not (Value.ValueType in UnsignedTypes);
  if Result then
    case Value.ValueType of
      vtShortint: Result := Value.Signed8 < 0;
      vtSmallint: Result := Value.Signed16 < 0;
      vtInteger: Result := Value.Signed32 < 0;
      vtInt64: Result := Value.Signed64 < 0;
      vtSingle: Result := Value.Float32 < 0;
      vtDouble: Result := Value.Float64 < 0;
      vtExtended: Result := Value.Float80 < 0;
    end;
end;

function TextToValue(const Text: string): TValue;
begin
  if TryStrToInt64(Text, Result.Signed64) then
  begin
    Result.ValueType := GetValueType(Result.Signed64);
    case Result.ValueType of
      vtByte: AssignByte(Result, Result.Signed64);
      vtShortint: AssignShortint(Result, Result.Signed64);
      vtWord: AssignWord(Result, Result.Signed64);
      vtSmallint: AssignSmallint(Result, Result.Signed64);
      vtLongword: AssignLongword(Result, Result.Signed64);
      vtInteger: AssignInteger(Result, Result.Signed64);
    end;
  end
  else
    if TryStrToFloat(Text, Result.Float80) then
      Result.ValueType := vtExtended
    else
      Result.ValueType := vtUnknown;
end;

function TryTextToValue(const Source: string; var Target: TValue): Boolean;
var
  Value: TValue;
begin
  Value := TextToValue(Source);
  Result := Value.ValueType <> vtUnknown;
  if Result then Target := Value;
end;

function ValueToText(const Value: TValue): string;
begin
  case Value.ValueType of
    vtByte: Result := IntToStr(Value.Unsigned8);
    vtShortint: Result := IntToStr(Value.Signed8);
    vtWord: Result := IntToStr(Value.Unsigned16);
    vtSmallint: Result := IntToStr(Value.Signed16);
    vtLongword: Result := IntToStr(Value.Unsigned32);
    vtInteger: Result := IntToStr(Value.Signed32);
    vtInt64: Result := IntToStr(Value.Signed64);
    vtSingle: Result := FloatToStr(Value.Float32);
    vtDouble: Result := FloatToStr(Value.Float64);
    vtExtended: Result := FloatToStr(Value.Float80);
  end;
end;

function Convert(const Value: TValue; ValueType: TValueType): TValue;
begin
  Result := EmptyValue;
  case Value.ValueType of
    vtByte:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Unsigned8;
        vtShortint: Result.Signed8 := Value.Unsigned8;
        vtWord: Result.Unsigned16 := Value.Unsigned8;
        vtSmallint: Result.Signed16 := Value.Unsigned8;
        vtLongword: Result.Unsigned32 := Value.Unsigned8;
        vtInteger: Result.Signed32 := Value.Unsigned8;
        vtInt64: Result.Signed64 := Value.Unsigned8;
        vtSingle: Result.Float32 := Value.Unsigned8;
        vtDouble: Result.Float64 := Value.Unsigned8;
        vtExtended: Result.Float80 := Value.Unsigned8;
      end;
    vtShortint:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Signed8;
        vtShortint: Result.Signed8 := Value.Signed8;
        vtWord: Result.Unsigned16 := Value.Signed8;
        vtSmallint: Result.Signed16 := Value.Signed8;
        vtLongword: Result.Unsigned32 := Value.Signed8;
        vtInteger: Result.Signed32 := Value.Signed8;
        vtInt64: Result.Signed64 := Value.Signed8;
        vtSingle: Result.Float32 := Value.Signed8;
        vtDouble: Result.Float64 := Value.Signed8;
        vtExtended: Result.Float80 := Value.Signed8;
      end;
    vtWord:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Unsigned16;
        vtShortint: Result.Signed8 := Value.Unsigned16;
        vtWord: Result.Unsigned16 := Value.Unsigned16;
        vtSmallint: Result.Signed16 := Value.Unsigned16;
        vtLongword: Result.Unsigned32 := Value.Unsigned16;
        vtInteger: Result.Signed32 := Value.Unsigned16;
        vtInt64: Result.Signed64 := Value.Unsigned16;
        vtSingle: Result.Float32 := Value.Unsigned16;
        vtDouble: Result.Float64 := Value.Unsigned16;
        vtExtended: Result.Float80 := Value.Unsigned16;
      end;
    vtSmallint:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Signed16;
        vtShortint: Result.Signed8 := Value.Signed16;
        vtWord: Result.Unsigned16 := Value.Signed16;
        vtSmallint: Result.Signed16 := Value.Signed16;
        vtLongword: Result.Unsigned32 := Value.Signed16;
        vtInteger: Result.Signed32 := Value.Signed16;
        vtInt64: Result.Signed64 := Value.Signed16;
        vtSingle: Result.Float32 := Value.Signed16;
        vtDouble: Result.Float64 := Value.Signed16;
        vtExtended: Result.Float80 := Value.Signed16;
      end;
    vtLongword:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Unsigned32;
        vtShortint: Result.Signed8 := Value.Unsigned32;
        vtWord: Result.Unsigned16 := Value.Unsigned32;
        vtSmallint: Result.Signed16 := Value.Unsigned32;
        vtLongword: Result.Unsigned32 := Value.Unsigned32;
        vtInteger: Result.Signed32 := Value.Unsigned32;
        vtInt64: Result.Signed64 := Value.Unsigned32;
        vtSingle: Result.Float32 := Value.Unsigned32;
        vtDouble: Result.Float64 := Value.Unsigned32;
        vtExtended: Result.Float80 := Value.Unsigned32;
      end;
    vtInteger:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Signed32;
        vtShortint: Result.Signed8 := Value.Signed32;
        vtWord: Result.Unsigned16 := Value.Signed32;
        vtSmallint: Result.Signed16 := Value.Signed32;
        vtLongword: Result.Unsigned32 := Value.Signed32;
        vtInteger: Result.Signed32 := Value.Signed32;
        vtInt64: Result.Signed64 := Value.Signed32;
        vtSingle: Result.Float32 := Value.Signed32;
        vtDouble: Result.Float64 := Value.Signed32;
        vtExtended: Result.Float80 := Value.Signed32;
      end;
    vtInt64:
      case ValueType of
        vtByte: Result.Unsigned8 := Value.Signed64;
        vtShortint: Result.Signed8 := Value.Signed64;
        vtWord: Result.Unsigned16 := Value.Signed64;
        vtSmallint: Result.Signed16 := Value.Signed64;
        vtLongword: Result.Unsigned32 := Value.Signed64;
        vtInteger: Result.Signed32 := Value.Signed64;
        vtInt64: Result.Signed64 := Value.Signed64;
        vtSingle: Result.Float32 := Value.Signed64;
        vtDouble: Result.Float64 := Value.Signed64;
        vtExtended: Result.Float80 := Value.Signed64;
      end;
    vtSingle:
      case ValueType of
        vtByte: Result.Unsigned8 := Round(Value.Float32);
        vtShortint: Result.Signed8 := Round(Value.Float32);
        vtWord: Result.Unsigned16 := Round(Value.Float32);
        vtSmallint: Result.Signed16 := Round(Value.Float32);
        vtLongword: Result.Unsigned32 := Round(Value.Float32);
        vtInteger: Result.Signed32 := Round(Value.Float32);
        vtInt64: Result.Signed64 := Round(Value.Float32);
        vtSingle: Result.Float32 := Value.Float32;
        vtDouble: Result.Float64 := Value.Float32;
        vtExtended: Result.Float80 := Value.Float32;
      end;
    vtDouble:
      case ValueType of
        vtByte: Result.Unsigned8 := Round(Value.Float64);
        vtShortint: Result.Signed8 := Round(Value.Float64);
        vtWord: Result.Unsigned16 := Round(Value.Float64);
        vtSmallint: Result.Signed16 := Round(Value.Float64);
        vtLongword: Result.Unsigned32 := Round(Value.Float64);
        vtInteger: Result.Signed32 := Round(Value.Float64);
        vtInt64: Result.Signed64 := Round(Value.Float64);
        vtSingle: Result.Float32 := Value.Float64;
        vtDouble: Result.Float64 := Value.Float64;
        vtExtended: Result.Float80 := Value.Float64;
      end;
    vtExtended:
      case ValueType of
        vtByte: Result.Unsigned8 := Round(Value.Float80);
        vtShortint: Result.Signed8 := Round(Value.Float80);
        vtWord: Result.Unsigned16 := Round(Value.Float80);
        vtSmallint: Result.Signed16 := Round(Value.Float80);
        vtLongword: Result.Unsigned32 := Round(Value.Float80);
        vtInteger: Result.Signed32 := Round(Value.Float80);
        vtInt64: Result.Signed64 := Round(Value.Float80);
        vtSingle: Result.Float32 := Value.Float80;
        vtDouble: Result.Float64 := Value.Float80;
        vtExtended: Result.Float80 := Value.Float80;
      end;
  end;
  Result.ValueType := ValueType;
end;

function Operation(const AValue, BValue: TValue; OperationType: TOperationType): TValue;
begin
  Result := EmptyValue;
  case OperationType of
    otAdd:
      case AValue.ValueType of
        vtByte:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned8 + BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned8 + BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Unsigned8 + BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Unsigned8 + BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned8) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned8) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned8) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned8 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned8 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned8 + BValue.Float80);
          end;
        vtShortint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed8 + BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed8 + BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Signed8 + BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Signed8 + BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Signed8) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed8) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed8) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed8 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed8 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed8 + BValue.Float80);
          end;
        vtWord:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned16 + BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned16 + BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Unsigned16 + BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Unsigned16 + BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned16) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned16) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned16) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned16 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned16 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned16 + BValue.Float80);
          end;
        vtSmallint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed16 + BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed16 + BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Signed16 + BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Signed16 + BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Signed16) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed16) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed16) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed16 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed16 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed16 + BValue.Float80);
          end;
        vtLongword:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned32) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned32) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned32 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned32 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned32 + BValue.Float80);
          end;
        vtInteger:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed32) + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed32) + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed32 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed32 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed32 + BValue.Float80);
          end;
        vtInt64:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed64 + Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, AValue.Signed64 + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed64 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed64 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed64 + BValue.Float80);
          end;
        vtSingle:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float32 + BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float32 + BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float32 + BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float32 + BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float32 + BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float32 + BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float32 + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float32 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float32 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float32 + BValue.Float80);
          end;
        vtDouble:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float64 + BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float64 + BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float64 + BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float64 + BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float64 + BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float64 + BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float64 + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float64 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float64 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float64 + BValue.Float80);
          end;
        vtExtended:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float80 + BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float80 + BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float80 + BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float80 + BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float80 + BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float80 + BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float80 + BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float80 + BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float80 + BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float80 + BValue.Float80);
          end;
      end;
    otSubtract:
      case AValue.ValueType of
        vtByte:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned8 - BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned8 - BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Unsigned8 - BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Unsigned8 - BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned8) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned8) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned8) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned8 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned8 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned8 - BValue.Float80);
          end;
        vtShortint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed8 - BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed8 - BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Signed8 - BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Signed8 - BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Signed8) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed8) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed8) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed8 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed8 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed8 - BValue.Float80);
          end;
        vtWord:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned16 - BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned16 - BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Unsigned16 - BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Unsigned16 - BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned16) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned16) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned16) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned16 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned16 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned16 - BValue.Float80);
          end;
        vtSmallint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed16 - BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed16 - BValue.Signed8);
            vtWord: AssignInteger(Result, AValue.Signed16 - BValue.Unsigned16);
            vtSmallint: AssignInteger(Result, AValue.Signed16 - BValue.Signed16);
            vtLongword: AssignInt64(Result, Int64(AValue.Signed16) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed16) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed16) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed16 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed16 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed16 - BValue.Float80);
          end;
        vtLongword:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned32) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned32) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned32 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned32 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned32 - BValue.Float80);
          end;
        vtInteger:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed32) - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed32) - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed32 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed32 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed32 - BValue.Float80);
          end;
        vtInt64:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed64 - Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, AValue.Signed64 - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed64 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed64 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed64 - BValue.Float80);
          end;
        vtSingle:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float32 - BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float32 - BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float32 - BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float32 - BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float32 - BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float32 - BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float32 - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float32 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float32 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float32 - BValue.Float80);
          end;
        vtDouble:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float64 - BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float64 - BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float64 - BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float64 - BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float64 - BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float64 - BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float64 - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float64 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float64 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float64 - BValue.Float80);
          end;
        vtExtended:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float80 - BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float80 - BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float80 - BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float80 - BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float80 - BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float80 - BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float80 - BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float80 - BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float80 - BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float80 - BValue.Float80);
          end;
      end;
    otMultiply:
      case AValue.ValueType of
        vtByte:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned8 * BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned8 * BValue.Signed8);
            vtWord: AssignInt64(Result, Int64(AValue.Unsigned8) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned8) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned8) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned8) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned8) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned8 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned8 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned8 * BValue.Float80);
          end;
        vtShortint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed8 * BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed8 * BValue.Signed8);
            vtWord: AssignInt64(Result, Int64(AValue.Signed8) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Signed8) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Signed8) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed8) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed8) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed8 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed8 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed8 * BValue.Float80);
          end;
        vtWord:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned16) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned16) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned16 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned16 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned16 * BValue.Float80);
          end;
        vtSmallint:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed16) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed16) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed16 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed16 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed16 * BValue.Float80);
          end;
        vtLongword:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned32) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Unsigned32) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned32 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned32 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned32 * BValue.Float80);
          end;
        vtInteger:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, Int64(AValue.Signed32) * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, Int64(AValue.Signed32) * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed32 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed32 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed32 * BValue.Float80);
          end;
        vtInt64:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Unsigned8));
            vtShortint: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Signed8));
            vtWord: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Unsigned16));
            vtSmallint: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Signed16));
            vtLongword: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed64 * Int64(BValue.Signed32));
            vtInt64: AssignInt64(Result, AValue.Signed64 * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed64 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed64 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed64 * BValue.Float80);
          end;
        vtSingle:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float32 * BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float32 * BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float32 * BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float32 * BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float32 * BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float32 * BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float32 * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float32 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float32 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float32 * BValue.Float80);
          end;
        vtDouble:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float64 * BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float64 * BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float64 * BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float64 * BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float64 * BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float64 * BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float64 * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float64 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float64 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float64 * BValue.Float80);
          end;
        vtExtended:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float80 * BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float80 * BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float80 * BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float80 * BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float80 * BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float80 * BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float80 * BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float80 * BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float80 * BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float80 * BValue.Float80);
          end;
      end;
    otIntegerDivide:
      case AValue.ValueType of
        vtByte:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Unsigned8 mod BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Unsigned8 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Unsigned8 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Unsigned8 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Unsigned8 mod BValue.Unsigned32);
            vtInteger: AssignInt64(Result, AValue.Unsigned8 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Unsigned8 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned8 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Unsigned8 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Unsigned8 mod Round(BValue.Float80));
          end;
        vtShortint:
          case BValue.ValueType of
            vtByte: AssignInteger(Result, AValue.Signed8 mod BValue.Unsigned8);
            vtShortint: AssignInteger(Result, AValue.Signed8 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Signed8 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Signed8 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Signed8 mod Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed8 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Signed8 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed8 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Signed8 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Signed8 mod Round(BValue.Float80));
          end;
        vtWord:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Unsigned16 mod BValue.Unsigned8);
            vtShortint: AssignInt64(Result, AValue.Unsigned16 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Unsigned16 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Unsigned16 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Unsigned16 mod BValue.Unsigned32);
            vtInteger: AssignInt64(Result, AValue.Unsigned16 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Unsigned16 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned16 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Unsigned16 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Unsigned16 mod Round(BValue.Float80));
          end;
        vtSmallint:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed16 mod BValue.Unsigned8);
            vtShortint: AssignInt64(Result, AValue.Signed16 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Signed16 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Signed16 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Signed16 mod Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed16 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Signed16 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed16 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Signed16 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Signed16 mod Round(BValue.Float80));
          end;
        vtLongword:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Unsigned32 mod BValue.Unsigned8);
            vtShortint: AssignInt64(Result, Int64(AValue.Unsigned32) mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Unsigned32 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, Int64(AValue.Unsigned32) mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Unsigned32 mod BValue.Unsigned32);
            vtInteger: AssignInt64(Result, Int64(AValue.Unsigned32) mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Unsigned32 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned32 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Unsigned32 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Unsigned32 mod Round(BValue.Float80));
          end;
        vtInteger:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed32 mod BValue.Unsigned8);
            vtShortint: AssignInt64(Result, AValue.Signed32 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Signed32 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Signed32 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Signed32 mod Int64(BValue.Unsigned32));
            vtInteger: AssignInt64(Result, AValue.Signed32 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Signed32 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed32 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Signed32 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Signed32 mod Round(BValue.Float80));
          end;
        vtInt64:
          case BValue.ValueType of
            vtByte: AssignInt64(Result, AValue.Signed64 mod BValue.Unsigned8);
            vtShortint: AssignInt64(Result, AValue.Signed64 mod BValue.Signed8);
            vtWord: AssignInt64(Result, AValue.Signed64 mod BValue.Unsigned16);
            vtSmallint: AssignInt64(Result, AValue.Signed64 mod BValue.Signed16);
            vtLongword: AssignInt64(Result, AValue.Signed64 mod BValue.Unsigned32);
            vtInteger: AssignInt64(Result, AValue.Signed64 mod BValue.Signed32);
            vtInt64: AssignInt64(Result, AValue.Signed64 mod BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed64 mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, AValue.Signed64 mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, AValue.Signed64 mod Round(BValue.Float80));
          end;
        vtSingle:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, Round(AValue.Float32) mod BValue.Unsigned8);
            vtShortint: AssignExtended(Result, Round(AValue.Float32) mod BValue.Signed8);
            vtWord: AssignExtended(Result, Round(AValue.Float32) mod BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, Round(AValue.Float32) mod BValue.Signed16);
            vtLongword: AssignExtended(Result, Round(AValue.Float32) mod BValue.Unsigned32);
            vtInteger: AssignExtended(Result, Round(AValue.Float32) mod BValue.Signed32);
            vtInt64: AssignExtended(Result, Round(AValue.Float32) mod BValue.Signed64);
            vtSingle: AssignExtended(Result, Round(AValue.Float32) mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, Round(AValue.Float32) mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, Round(AValue.Float32) mod Round(BValue.Float80));
          end;
        vtDouble:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, Round(AValue.Float64) mod BValue.Unsigned8);
            vtShortint: AssignExtended(Result, Round(AValue.Float64) mod BValue.Signed8);
            vtWord: AssignExtended(Result, Round(AValue.Float64) mod BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, Round(AValue.Float64) mod BValue.Signed16);
            vtLongword: AssignExtended(Result, Round(AValue.Float64) mod BValue.Unsigned32);
            vtInteger: AssignExtended(Result, Round(AValue.Float64) mod BValue.Signed32);
            vtInt64: AssignExtended(Result, Round(AValue.Float64) mod BValue.Signed64);
            vtSingle: AssignExtended(Result, Round(AValue.Float64) mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, Round(AValue.Float64) mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, Round(AValue.Float64) mod Round(BValue.Float80));
          end;
        vtExtended:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, Round(AValue.Float80) mod BValue.Unsigned8);
            vtShortint: AssignExtended(Result, Round(AValue.Float80) mod BValue.Signed8);
            vtWord: AssignExtended(Result, Round(AValue.Float80) mod BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, Round(AValue.Float80) mod BValue.Signed16);
            vtLongword: AssignExtended(Result, Round(AValue.Float80) mod BValue.Unsigned32);
            vtInteger: AssignExtended(Result, Round(AValue.Float80) mod BValue.Signed32);
            vtInt64: AssignExtended(Result, Round(AValue.Float80) mod BValue.Signed64);
            vtSingle: AssignExtended(Result, Round(AValue.Float80) mod Round(BValue.Float32));
            vtDouble: AssignExtended(Result, Round(AValue.Float80) mod Round(BValue.Float64));
            vtExtended: AssignExtended(Result, Round(AValue.Float80) mod Round(BValue.Float80));
          end;
      end;
    otFloatDivide:
      case AValue.ValueType of
        vtByte:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Unsigned8 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Unsigned8 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Unsigned8 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Unsigned8 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Unsigned8 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Unsigned8 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Unsigned8 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned8 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned8 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned8 / BValue.Float80);
          end;
        vtShortint:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Signed8 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Signed8 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Signed8 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Signed8 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Signed8 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Signed8 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Signed8 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed8 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed8 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed8 / BValue.Float80);
          end;
        vtWord:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Unsigned16 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Unsigned16 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Unsigned16 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Unsigned16 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Unsigned16 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Unsigned16 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Unsigned16 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned16 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned16 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned16 / BValue.Float80);
          end;
        vtSmallint:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Signed16 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Signed16 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Signed16 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Signed16 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Signed16 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Signed16 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Signed16 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed16 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed16 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed16 / BValue.Float80);
          end;
        vtLongword:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Unsigned32 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Unsigned32 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Unsigned32 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Unsigned32 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Unsigned32 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Unsigned32 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Unsigned32 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Unsigned32 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Unsigned32 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Unsigned32 / BValue.Float80);
          end;
        vtInteger:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Signed32 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Signed32 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Signed32 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Signed32 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Signed32 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Signed32 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Signed32 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed32 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed32 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed32 / BValue.Float80);
          end;
        vtInt64:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Signed64 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Signed64 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Signed64 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Signed64 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Signed64 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Signed64 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Signed64 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Signed64 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Signed64 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Signed64 / BValue.Float80);
          end;
        vtSingle:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float32 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float32 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float32 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float32 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float32 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float32 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float32 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float32 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float32 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float32 / BValue.Float80);
          end;
        vtDouble:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float64 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float64 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float64 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float64 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float64 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float64 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float64 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float64 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float64 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float64 / BValue.Float80);
          end;
        vtExtended:
          case BValue.ValueType of
            vtByte: AssignExtended(Result, AValue.Float80 / BValue.Unsigned8);
            vtShortint: AssignExtended(Result, AValue.Float80 / BValue.Signed8);
            vtWord: AssignExtended(Result, AValue.Float80 / BValue.Unsigned16);
            vtSmallint: AssignExtended(Result, AValue.Float80 / BValue.Signed16);
            vtLongword: AssignExtended(Result, AValue.Float80 / BValue.Unsigned32);
            vtInteger: AssignExtended(Result, AValue.Float80 / BValue.Signed32);
            vtInt64: AssignExtended(Result, AValue.Float80 / BValue.Signed64);
            vtSingle: AssignExtended(Result, AValue.Float80 / BValue.Float32);
            vtDouble: AssignExtended(Result, AValue.Float80 / BValue.Float64);
            vtExtended: AssignExtended(Result, AValue.Float80 / BValue.Float80);
          end;
      end;
  end;
end;

function Negative(const Value: TValue): TValue;
begin
  case Value.ValueType of
    vtByte: AssignSmallint(Result, - Value.Unsigned8);
    vtShortint: AssignSmallint(Result, - Value.Signed8);
    vtWord: AssignInteger(Result, - Value.Unsigned16);
    vtSmallint: AssignInteger(Result, - Value.Signed16);
    vtLongword: AssignInt64(Result, - Int64(Value.Unsigned32));
    vtInteger: AssignInt64(Result, - Int64(Value.Signed32));
    vtInt64: AssignInt64(Result, - Value.Signed64);
    vtSingle: AssignDouble(Result, - Value.Float32);
    vtDouble: AssignDouble(Result, - Value.Float64);
    vtExtended: AssignExtended(Result, - Value.Float80);
  end;
end;

function Positive(const Value: TValue): TValue;
begin
  if Value.ValueType in UnsignedTypes then Result := Value
  else
    case Value.ValueType of
      vtShortint:
        if Value.Signed8 < 0 then
          AssignSmallint(Result, - Value.Signed8)
        else
          Result := Value;
      vtSmallint:
        if Value.Signed16 < 0 then
          AssignInteger(Result, - Value.Signed16)
        else
          Result := Value;
      vtInteger:
        if Value.Signed32 < 0 then
          AssignInt64(Result, - Int64(Value.Signed32))
        else
          Result := Value;
      vtInt64:
        if Value.Signed64 < 0 then
          AssignInt64(Result, - Value.Signed64)
        else
          Result := Value;
      vtSingle: AssignDouble(Result, Abs(Value.Float32));
      vtDouble: AssignDouble(Result, Abs(Value.Float64));
      vtExtended: AssignExtended(Result, Abs(Value.Float80));
    end;
end;

end.
