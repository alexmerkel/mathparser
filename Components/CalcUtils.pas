{ *********************************************************************** }
{                                                                         }
{ CalcUtils                                                               }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit CalcUtils;

{$B-}

interface

uses
  Calculator, ValueTypes;

function AsValue(const Text: string): TValue;
function AsByte(const Text: string): Byte;
function AsShortint(const Text: string): Shortint;
function AsWord(const Text: string): Word;
function AsSmallint(const Text: string): Smallint;
function AsLongword(const Text: string): Longword;
function AsInteger(const Text: string): Integer;
function AsInt64(const Text: string): Int64;
function AsSingle(const Text: string): Single;
function AsDouble(const Text: string): Double;
function AsExtended(const Text: string): Extended;
function AsBoolean(const Text: string): Boolean;
function AsPointer(const Text: string): Pointer;
function AsString(const Text: string): string;

function TryTextToValue(const Text: string; out Value: TValue): Boolean;
function TryTextToByte(const Text: string; out Value: Byte): Boolean;
function TryTextToShortint(const Text: string; out Value: Shortint): Boolean;
function TryTextToWord(const Text: string; out Value: Word): Boolean;
function TryTextToSmallint(const Text: string; out Value: Smallint): Boolean;
function TryTextToLongword(const Text: string; out Value: Longword): Boolean;
function TryTextToInteger(const Text: string; out Value: Integer): Boolean;
function TryTextToInt64(const Text: string; out Value: Int64): Boolean;
function TryTextToSingle(const Text: string; out Value: Single): Boolean;
function TryTextToDouble(const Text: string; out Value: Double): Boolean;
function TryTextToExtended(const Text: string; out Value: Extended): Boolean;
function TryTextToBoolean(const Text: string; out Value: Boolean): Boolean;
function TryTextToPointer(const Text: string; out Value: Pointer): Boolean;
function TryTextToString(const Text: string; out Value: string): Boolean;

function TextToValueDef(const Text: string; const Default: TValue): TValue;
function TextToByteDef(const Text: string; const Default: Byte): Byte;
function TextToShortintDef(const Text: string; const Default: Shortint): Shortint;
function TextToWordDef(const Text: string; const Default: Word): Word;
function TextToSmallintDef(const Text: string; const Default: Smallint): Smallint;
function TextToLongwordDef(const Text: string; const Default: Longword): Longword;
function TextToIntegerDef(const Text: string; const Default: Integer): Integer;
function TextToInt64Def(const Text: string; const Default: Int64): Int64;
function TextToSingleDef(const Text: string; const Default: Single): Single;
function TextToDoubleDef(const Text: string; const Default: Double): Double;
function TextToExtendedDef(const Text: string; const Default: Extended): Extended;
function TextToBooleanDef(const Text: string; const Default: Boolean): Boolean;
function TextToPointerDef(const Text: string; const Default: Pointer): Pointer;
function TextToStringDef(const Text: string; const Default: string): string;

var
  Calc: TCalculator;

implementation

function AsValue(const Text: string): TValue;
begin
  Result := Calc.AsValue(Text);
end;

function AsByte(const Text: string): Byte;
begin
  Result := Calc.AsByte(Text);
end;

function AsShortint(const Text: string): Shortint;
begin
  Result := Calc.AsShortint(Text);
end;

function AsWord(const Text: string): Word;
begin
  Result := Calc.AsWord(Text);
end;

function AsSmallint(const Text: string): Smallint;
begin
  Result := Calc.AsSmallint(Text);
end;

function AsLongword(const Text: string): Longword;
begin
  Result := Calc.AsLongword(Text);
end;

function AsInteger(const Text: string): Integer;
begin
  Result := Calc.AsInteger(Text);
end;

function AsInt64(const Text: string): Int64;
begin
  Result := Calc.AsInt64(Text);
end;

function AsSingle(const Text: string): Single;
begin
  Result := Calc.AsSingle(Text);
end;

function AsDouble(const Text: string): Double;
begin
  Result := Calc.AsDouble(Text);
end;

function AsExtended(const Text: string): Extended;
begin
  Result := Calc.AsExtended(Text);
end;

function AsBoolean(const Text: string): Boolean;
begin
  Result := Calc.AsBoolean(Text);
end;

function AsPointer(const Text: string): Pointer;
begin
  Result := Calc.AsPointer(Text);
end;

function AsString(const Text: string): string;
begin
  Result := Calc.AsString(Text);
end;

function TryTextToValue(const Text: string; out Value: TValue): Boolean;
begin
  Result := Calc.TryTextToValue(Text, Value);
end;

function TryTextToByte(const Text: string; out Value: Byte): Boolean;
begin
  Result := Calc.TryTextToByte(Text, Value);
end;

function TryTextToShortint(const Text: string; out Value: Shortint): Boolean;
begin
  Result := Calc.TryTextToShortint(Text, Value);
end;

function TryTextToWord(const Text: string; out Value: Word): Boolean;
begin
  Result := Calc.TryTextToWord(Text, Value);
end;

function TryTextToSmallint(const Text: string; out Value: Smallint): Boolean;
begin
  Result := Calc.TryTextToSmallint(Text, Value);
end;

function TryTextToLongword(const Text: string; out Value: Longword): Boolean;
begin
  Result := Calc.TryTextToLongword(Text, Value);
end;

function TryTextToInteger(const Text: string; out Value: Integer): Boolean;
begin
  Result := Calc.TryTextToInteger(Text, Value);
end;

function TryTextToInt64(const Text: string; out Value: Int64): Boolean;
begin
  Result := Calc.TryTextToInt64(Text, Value);
end;

function TryTextToSingle(const Text: string; out Value: Single): Boolean;
begin
  Result := Calc.TryTextToSingle(Text, Value);
end;

function TryTextToDouble(const Text: string; out Value: Double): Boolean;
begin
  Result := Calc.TryTextToDouble(Text, Value);
end;

function TryTextToExtended(const Text: string; out Value: Extended): Boolean;
begin
  Result := Calc.TryTextToExtended(Text, Value);
end;

function TryTextToBoolean(const Text: string; out Value: Boolean): Boolean;
begin
  Result := Calc.TryTextToBoolean(Text, Value);
end;

function TryTextToPointer(const Text: string; out Value: Pointer): Boolean;
begin
  Result := Calc.TryTextToPointer(Text, Value);
end;

function TryTextToString(const Text: string; out Value: string): Boolean;
begin
  Result := Calc.TryTextToString(Text, Value);
end;

function TextToValueDef(const Text: string; const Default: TValue): TValue;
begin
  Result := Calc.TextToValueDef(Text, Default);
end;

function TextToByteDef(const Text: string; const Default: Byte): Byte;
begin
  Result := Calc.TextToByteDef(Text, Default);
end;

function TextToShortintDef(const Text: string; const Default: Shortint): Shortint;
begin
  Result := Calc.TextToShortintDef(Text, Default);
end;

function TextToWordDef(const Text: string; const Default: Word): Word;
begin
  Result := Calc.TextToWordDef(Text, Default);
end;

function TextToSmallintDef(const Text: string; const Default: Smallint): Smallint;
begin
  Result := Calc.TextToSmallintDef(Text, Default);
end;

function TextToLongwordDef(const Text: string; const Default: Longword): Longword;
begin
  Result := Calc.TextToLongwordDef(Text, Default);
end;

function TextToIntegerDef(const Text: string; const Default: Integer): Integer;
begin
  Result := Calc.TextToIntegerDef(Text, Default);
end;

function TextToInt64Def(const Text: string; const Default: Int64): Int64;
begin
  Result := Calc.TextToInt64Def(Text, Default);
end;

function TextToSingleDef(const Text: string; const Default: Single): Single;
begin
  Result := Calc.TextToSingleDef(Text, Default);
end;

function TextToDoubleDef(const Text: string; const Default: Double): Double;
begin
  Result := Calc.TextToDoubleDef(Text, Default);
end;

function TextToExtendedDef(const Text: string; const Default: Extended): Extended;
begin
  Result := Calc.TextToExtendedDef(Text, Default);
end;

function TextToBooleanDef(const Text: string; const Default: Boolean): Boolean;
begin
  Result := Calc.TextToBooleanDef(Text, Default);
end;

function TextToPointerDef(const Text: string; const Default: Pointer): Pointer;
begin
  Result := Calc.TextToPointerDef(Text, Default);
end;

function TextToStringDef(const Text: string; const Default: string): string;
begin
  Result := Calc.TextToStringDef(Text, Default);
end;

initialization
  Calc := TCalculator.Create(nil);

finalization
  Calc.Free;

end.
