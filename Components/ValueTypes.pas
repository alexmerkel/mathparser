{ *********************************************************************** }
{                                                                         }
{ ValueTypes                                                              }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ValueTypes;

{$B-}

interface

uses
  SysUtils;

type
  PValueType = ^TValueType;
  TValueType = (vtUnknown, vtByte, vtShortint, vtWord, vtSmallint, vtLongword, vtInteger, vtInt64, vtSingle, vtDouble, vtExtended);

  PValue = ^TValue;
  TValue = record
    ValueType: TValueType;
    case Byte of
      0: (ByteArray: array[0..9] of Byte);
      1: (Unsigned8: Byte);
      2: (Signed8: Shortint);
      3: (Unsigned16: Word);
      4: (Signed16: Smallint);
      5: (Unsigned32: Longword);
      6: (Signed32: Integer);
      7: (Signed64: Int64);
      8: (Float32: Single);
      9: (Float64: Double);
      10: (Float80: Extended);
      11: (WordRec: WordRec);
      12: (LongRec: LongRec);
      13: (Int64Rec: Int64Rec);
  end;

  PValueArray = ^TValueArray;
  TValueArray = array of TValue;

  PLiveValue = ^TLiveValue;
  TLiveValue = record
    ValueType: TValueType;
    case Byte of
      1: (Unsigned8: PByte);
      2: (Signed8: PShortint);
      3: (Unsigned16: PWord);
      4: (Signed16: PSmallint);
      5: (Unsigned32: PLongword);
      6: (Signed32: PInteger);
      7: (Signed64: PInt64);
      8: (Float32: PSingle);
      9: (Float64: PDouble);
      10: (Float80: PExtended);
  end;

const
  FloatTypes = [vtSingle, vtDouble, vtExtended];
  IntegerTypes = [vtByte, vtShortint, vtWord, vtSmallint, vtLongword, vtInteger, vtInt64];
  SignedTypes = [vtShortint, vtSmallint, vtInteger, vtInt64, vtSingle, vtDouble, vtExtended];
  UnsignedTypes = [vtByte, vtWord, vtLongword];

function Add(var Target: TValueArray; const Value: TValue): Integer;
function Delete(var Target: TValueArray; const Index: Integer): Boolean;
function Next(const ValueType: TValueType): TValueType;
function Assignable(const Source, Target: TValueType): Boolean;

implementation

uses
  MemoryUtils;

function Add(var Target: TValueArray; const Value: TValue): Integer;
begin
  Result := Length(Target);
  SetLength(Target, Result + 1);
  MemoryUtils.Add(Target, @Value, Result * SizeOf(TValue), SizeOf(TValue));
end;

function Delete(var Target: TValueArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := MemoryUtils.Delete(Target, Index * SizeOf(TValue), SizeOf(TValue), Size * SizeOf(TValue));
  if Result then SetLength(Target, Size - 1);
end;

function Next(const ValueType: TValueType): TValueType;
begin
  case ValueType of
    vtByte: Result := vtWord;
    vtShortint: Result := vtSmallint;
    vtWord: Result := vtLongword;
    vtSmallint: Result := vtInteger;
    vtLongword, vtInteger: Result := vtInt64;
    vtSingle: Result := vtDouble;
    vtDouble: Result := vtExtended;
  else Result := ValueType;
  end;
end;

function Assignable(const Source, Target: TValueType): Boolean;
begin
  case Target of
    vtShortint, vtSmallint, vtInteger, vtInt64, vtSingle, vtDouble, vtExtended:
      Result := Source in [Target..High(TValueType)];
    vtByte, vtWord, vtLongword:
      Result := Source in [Target, Next(Target)..High(TValueType)];
  else Result := False;
  end;
end;

end.
