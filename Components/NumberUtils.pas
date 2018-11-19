{ *********************************************************************** }
{                                                                         }
{ NumberUtils                                                             }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit NumberUtils;

{$B-}
{$I Directives.inc}

interface

uses
  SysUtils, SysConst, Classes;

const
  BitCount = 8;

type
  TBits = class(Classes.TBits)
  public
    procedure Open(const Value: Int64); overload; virtual;
    procedure Open(const Value: Integer); overload; virtual;
    procedure Open(const Value: Byte); overload; virtual;
    procedure Save(out Value: Int64); overload; virtual;
    procedure Save(out Value: Integer); overload; virtual;
    procedure Save(out Value: Byte); overload; virtual;
  end;

function GetHashCode(const Memory: Pointer; const Size: Integer): Integer; overload;
function GetHashCode(const Value: string): Integer; overload;

function Equal(const AValue, BValue: Extended; AEpsilon: Extended = 0): Boolean;
function Above(const AValue, BValue: Extended; AEpsilon: Extended = 0): Boolean;
function AboveOrEqual(const AValue, BValue: Extended; AEpsilon: Extended = 0): Boolean;
function Below(const AValue, BValue: Extended; AEpsilon: Extended = 0): Boolean;
function BelowOrEqual(const AValue, BValue: Extended; AEpsilon: Extended = 0): Boolean;

function NextStep(const Value, Distance: Extended; const Index: Integer; const Epsilon: Extended = 0): Extended;

function Positive(const Value: Integer): Integer;

function FracSize(const Value: Extended): Integer;

var
  Bits: TBits;
  Epsilon: Extended = 0;

implementation

uses
  Math, {$IFNDEF UNICODE}TextUtils, {$ENDIF}Types;

{ TBits }

procedure TBits.Open(const Value: Int64);
var
  I: Integer;
begin
  Size := SizeOf(Value) * BitCount;
  for I := 0 to Size - 1 do Bits[I] := (Value and Round(IntPower(2, I))) > 0;
end;

procedure TBits.Open(const Value: Integer);
var
  I: Integer;
begin
  Size := SizeOf(Value) * BitCount;
  for I := 0 to Size - 1 do Bits[I] := (Value and Round(IntPower(2, I))) > 0;
end;

procedure TBits.Open(const Value: Byte);
var
  I: Integer;
begin
  Size := SizeOf(Value) * BitCount;
  for I := 0 to Size - 1 do Bits[I] := (Value and Round(IntPower(2, I))) > 0;
end;

procedure TBits.Save(out Value: Int64);
var
  I: Integer;
begin
  Value := 0;
  for I := 0 to Size - 1 do if Bits[I] then Value := Value or Round(IntPower(2, I));
end;

procedure TBits.Save(out Value: Integer);
var
  I: Integer;
begin
  Value := 0;
  for I := 0 to Size - 1 do if Bits[I] then Value := Value or Round(IntPower(2, I));
end;

procedure TBits.Save(out Value: Byte);
var
  I: Integer;
begin
  Value := 0;
  for I := 0 to Size - 1 do if Bits[I] then Value := Value or Round(IntPower(2, I));
end;

function Bit(const Value: Int64; const Index: Integer): Boolean;
begin
  Bits.Open(Value);
  Result := Bits.Bits[Index];
end;

function GetHashCode(const Memory: Pointer; const Size: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Size - 1 do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor PByte(Integer(Memory) + I)^;
end;

function GetHashCode(const Value: string): Integer;
begin
  Result := GetHashCode(PAnsiChar(Value), Length(Value));
end;

function Equal(const AValue, BValue: Extended; AEpsilon: Extended): Boolean;
begin
  if AEpsilon = 0 then AEpsilon := Epsilon;  
  Result := SameValue(AValue, BValue, AEpsilon);
end;

function Above(const AValue, BValue: Extended; AEpsilon: Extended): Boolean;
begin
  if AEpsilon = 0 then AEpsilon := Epsilon;
  Result := CompareValue(AValue, BValue, AEpsilon) = GreaterThanValue;
end;

function AboveOrEqual(const AValue, BValue: Extended; AEpsilon: Extended): Boolean;
begin
  if AEpsilon = 0 then AEpsilon := Epsilon;  
  Result := CompareValue(AValue, BValue, AEpsilon) <> LessThanValue;
end;

function Below(const AValue, BValue: Extended; AEpsilon: Extended): Boolean;
begin
  if AEpsilon = 0 then AEpsilon := Epsilon;
  Result := CompareValue(AValue, BValue, AEpsilon) = LessThanValue;
end;

function BelowOrEqual(const AValue, BValue: Extended; AEpsilon: Extended): Boolean;
begin
  if AEpsilon = 0 then AEpsilon := Epsilon;
  Result := CompareValue(AValue, BValue, AEpsilon) <> GreaterThanValue;
end;

function NextStep(const Value, Distance: Extended; const Index: Integer; const Epsilon: Extended): Extended;
var
  A: Extended;
begin
  A := Value / Distance;
  if (Index = 0) and Equal(Frac(A), 0, Epsilon) then
    Result := Value
  else
    Result := (Int(A) + Index) * Distance;
end;

function Positive(const Value: Integer): Integer;
begin
  if Value < 0 then
    Result := - Value
  else
    Result := Value;
end;

function FracSize(const Value: Extended): Integer;
begin
  if Value > 0 then
  begin
    Result := Round(Log10(1 / Value));
    if Result < 0 then Result := 0;
  end
  else Result := 0;
end;

initialization
  Bits := TBits.Create;

finalization
  Bits.Free;

end.
