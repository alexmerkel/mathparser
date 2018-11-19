{ *********************************************************************** }
{                                                                         }
{ MemoryUtils                                                             }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit MemoryUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Types;

type
  TCharDynArray = array of Char;

  TMoveNext = function(var Index: Integer; const Count: Integer; const Value: Pointer; const Data: Pointer = nil): Boolean;
  TSearchCompare = function(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship;
  TSortCompare = function(const AIndex, BIndex: Integer; const Target: Pointer; const Data: Pointer = nil): TValueRelationship;
  TSortExchange = procedure(const AIndex, BIndex: Integer; const Target: Pointer; const Data: Pointer = nil);

procedure Add(const Target, Source: Pointer; const TargetSize, SourceSize: Integer); overload;
function Add(var Target: TInt64DynArray; const Value: Int64; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TIntegerDynArray; const Value: Integer; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TByteDynArray; const Value: Byte; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TSingleDynArray; const Value: Single; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TDoubleDynArray; const Value: Double; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TCharDynArray; const Value: Char; const Unique: Boolean = False): Integer; overload;
function Add(var Target: TStringDynArray; const Value: string; const Unique: Boolean = False): Integer; overload;

procedure Add(var Target: TInt64DynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;
procedure Add(var Target: TIntegerDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;
procedure Add(var Target: TByteDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;
procedure Add(var Target: TSingleDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;
procedure Add(var Target: TDoubleDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;
procedure Add(var Target: TCharDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean = False); overload;

function Int64MoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TInt64DynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;
function IntegerMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TIntegerDynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;
function ByteMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TByteDynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;
function SingleMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TSingleDynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;
function DoubleMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TDoubleDynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;
function CharMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
procedure Add(var Target: TCharDynArray; const FromIndex, Count: Integer; const Unique: Boolean = False); overload;

function Delete(const Target: Pointer; const Index, Count, TargetSize: Integer): Boolean; overload;
function Delete(var Target: TInt64DynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TIntegerDynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TByteDynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TSingleDynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TDoubleDynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TCharDynArray; const Index: Integer): Boolean; overload;
function Delete(var Target: TStringDynArray; const Index: Integer): Boolean; overload;

function IndexOf(const Target, Source: Pointer; const Count, SourceSize: Integer): Integer; overload;
function IndexOf(const Target: TInt64DynArray; const Value: Int64): Integer; overload;
function IndexOf(const Target: TIntegerDynArray; const Value: Integer): Integer; overload;
function IndexOf(const Target: TByteDynArray; const Value: Byte): Integer; overload;
function IndexOf(const Target: TSingleDynArray; const Value: Single): Integer; overload;
function IndexOf(const Target: TDoubleDynArray; const Value: Double): Integer; overload;
function IndexOf(const Target: TCharDynArray; const Value: Char): Integer; overload;
function IndexOf(const Target: TStringDynArray; const Value: string): Integer; overload;

function Insert(const Target, Source: Pointer; const Index, TargetSize, SourceSize: Integer): Boolean; overload;
function Insert(var Target: TInt64DynArray; const Value: Int64; Index: Integer): Boolean; overload;
function Insert(var Target: TIntegerDynArray; const Value: Integer; Index: Integer): Boolean; overload;
function Insert(var Target: TByteDynArray; const Value: Byte; Index: Integer): Boolean; overload;
function Insert(var Target: TSingleDynArray; const Value: Single; Index: Integer): Boolean; overload;
function Insert(var Target: TDoubleDynArray; const Value: Double; Index: Integer): Boolean; overload;
function Insert(var Target: TCharDynArray; const Value: Char; Index: Integer): Boolean; overload;

function BinarySearch(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSearchCompare;
  const Value: Pointer; const Data: Pointer = nil): Integer;
function Int64Compare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function Search(const Target: TInt64DynArray; const Value: Int64): Integer; overload;
function IntegerCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function Search(const Target: TIntegerDynArray; const Value: Integer): Integer; overload;
function ByteCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function Search(const Target: TByteDynArray; const Value: Byte): Integer; overload;
function SingleCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function Search(const Target: TSingleDynArray; const Value: Single): Integer; overload;
function DoubleCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship; overload;
function Search(const Target: TDoubleDynArray; const Value: Double): Integer; overload;

procedure HSort(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSortCompare;
  const Exchange: TSortExchange; const Data: Pointer = nil);
procedure QSort(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSortCompare;
  const Exchange: TSortExchange; const Data: Pointer = nil);
function Int64Compare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure Int64Exchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TInt64DynArray); overload;
function IntegerCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure IntegerExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TIntegerDynArray); overload;
function ByteCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure ByteExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TByteDynArray); overload;
function SingleCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure SingleExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TSingleDynArray); overload;
function DoubleCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship; overload;
procedure DoubleExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TDoubleDynArray); overload;
function CharCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
procedure CharExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TCharDynArray); overload;
function StringCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
procedure StringExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure Sort(var Target: TStringDynArray); overload;

procedure Resize(const Target: Pointer; const PriorSize, Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TInt64DynArray; const Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TIntegerDynArray; const Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TByteDynArray; const Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TSingleDynArray; const Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TDoubleDynArray; const Size: Integer; const Fill: Byte = 0); overload;
procedure Resize(var Target: TCharDynArray; const Size: Integer; const Fill: Byte = 0); overload;

procedure Segment(const Source, Target: Pointer; const Index, Size: Integer); overload;
function Segment(const Target: TInt64DynArray; const Index, Size: Integer): TInt64DynArray; overload;
function Segment(const Target: TIntegerDynArray; const Index, Size: Integer): TIntegerDynArray; overload;
function Segment(const Target: TByteDynArray; const Index, Size: Integer): TByteDynArray; overload;
function Segment(const Target: TSingleDynArray; const Index, Size: Integer): TSingleDynArray; overload;
function Segment(const Target: TDoubleDynArray; const Index, Size: Integer): TDoubleDynArray; overload;
function Segment(const Target: TCharDynArray; const Index, Size: Integer): TCharDynArray; overload;

procedure SetRandom(var Target: TIntegerDynArray);

implementation

uses
  Math;

procedure Add(const Target, Source: Pointer; const TargetSize, SourceSize: Integer);
begin
  CopyMemory(PAnsiChar(Target) + TargetSize, Source, SourceSize);
end;

function Add(var Target: TInt64DynArray; const Value: Int64;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Int64), SizeOf(Int64));
  end
  else Result := -1;
end;

function Add(var Target: TIntegerDynArray; const Value: Integer;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Integer), SizeOf(Integer));
  end
  else Result := -1;
end;

function Add(var Target: TByteDynArray; const Value: Byte;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Byte), SizeOf(Byte));
  end
  else Result := -1;
end;

function Add(var Target: TSingleDynArray; const Value: Single;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Single), SizeOf(Single));
  end
  else Result := -1;
end;

function Add(var Target: TDoubleDynArray; const Value: Double;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Double), SizeOf(Double));
  end
  else Result := -1;
end;

function Add(var Target: TCharDynArray; const Value: Char;
  const Unique: Boolean = False): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Add(Target, @Value, Result * SizeOf(Char), SizeOf(Char));
  end
  else Result := -1;
end;

function Add(var Target: TStringDynArray; const Value: string;
  const Unique: Boolean): Integer;
begin
  if not Unique or Unique and (IndexOf(Target, Value) < 0) then
  begin
    Result := Length(Target);
    SetLength(Target, Result + 1);
    Target[Result] := Value;
  end
  else Result := -1;
end;

procedure Add(var Target: TInt64DynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Int64;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

procedure Add(var Target: TIntegerDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Integer;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

procedure Add(var Target: TByteDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Byte;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

procedure Add(var Target: TSingleDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Single;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

procedure Add(var Target: TDoubleDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Double;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

procedure Add(var Target: TCharDynArray; const Count: Integer; const MoveNext: TMoveNext;
  const Data: Pointer; const Unique: Boolean);
var
  I: Integer;
  Value: Char;
begin
  I := 0;
  while I < Count do
    if MoveNext(I, Count, @Value, Data) then Add(Target, Value, Unique);
end;

function Int64MoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PInt64(Value)^ := PInteger(Data)^ + Index;
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TInt64DynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, Int64MoveNext, @FromIndex, Unique);
end;

function IntegerMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PInteger(Value)^ := PInteger(Data)^ + Index;
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TIntegerDynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, IntegerMoveNext, @FromIndex, Unique);
end;

function ByteMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PByte(Value)^ := PInteger(Data)^ + Index;
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TByteDynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, ByteMoveNext, @FromIndex, Unique);
end;

function SingleMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PSingle(Value)^ := PInteger(Data)^ + Index;
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TSingleDynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, SingleMoveNext, @FromIndex, Unique);
end;

function DoubleMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PDouble(Value)^ := PInteger(Data)^ + Index;
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TDoubleDynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, DoubleMoveNext, @FromIndex, Unique);
end;

function CharMoveNext(var Index: Integer; const Count: Integer; const Value, Data: Pointer): Boolean;
begin
  PChar(Value)^ := Chr(PInteger(Data)^ + Index);
  Inc(Index);
  Result := True;
end;

procedure Add(var Target: TCharDynArray; const FromIndex, Count: Integer; const Unique: Boolean);
begin
  Add(Target, Count, CharMoveNext, @FromIndex, Unique);
end;

function Delete(const Target: Pointer; const Index, Count, TargetSize: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index + Count <= TargetSize);
  if Result and (Index + Count < TargetSize) then
    MoveMemory(PAnsiChar(Target) + Index, PAnsiChar(Target) + Index + Count, TargetSize - Index - Count);
end;

function Delete(var Target: TInt64DynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Int64), SizeOf(Int64), Size * SizeOf(Int64));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TIntegerDynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Integer), SizeOf(Integer), Size * SizeOf(Integer));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TByteDynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Byte), SizeOf(Byte), Size * SizeOf(Byte));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TSingleDynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Single), SizeOf(Single), Size * SizeOf(Single));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TDoubleDynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Double), SizeOf(Double), Size * SizeOf(Double));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TCharDynArray; const Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  Result := Delete(Target, Index * SizeOf(Char), SizeOf(Char), Size * SizeOf(Char));
  if Result then SetLength(Target, Size - 1);
end;

function Delete(var Target: TStringDynArray; const Index: Integer): Boolean;
var
  I, J: Integer;
  ATarget: TStringDynArray;
begin
  I := Length(Target);
  Result := (Index >= 0) and (Index < I);
  if Result then
  begin
    SetLength(ATarget, I - 1);
    J := 0;
    for I := Low(Target) to High(Target) do
      if I = Index then
        Inc(J)
      else
        ATarget[I - J] := Target[I];
    Target := ATarget;
  end;
end;

function IndexOf(const Target, Source: Pointer; const Count, SourceSize: Integer): Integer;
var
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while I < Count do
  begin
    if CompareMem(Source, PAnsiChar(Target) + J, SourceSize) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I);
    Inc(J, SourceSize);
  end;
  Result := -1;
end;

function IndexOf(const Target: TInt64DynArray; const Value: Int64): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Int64));
end;

function IndexOf(const Target: TIntegerDynArray; const Value: Integer): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Integer));
end;

function IndexOf(const Target: TByteDynArray; const Value: Byte): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Byte));
end;

function IndexOf(const Target: TSingleDynArray; const Value: Single): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Single));
end;

function IndexOf(const Target: TDoubleDynArray; const Value: Double): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Double));
end;

function IndexOf(const Target: TCharDynArray; const Value: Char): Integer;
begin
  Result := IndexOf(Target, @Value, Length(Target), SizeOf(Char));
end;

function IndexOf(const Target: TStringDynArray; const Value: string): Integer;
var
  I: Integer;
begin
  for I := Low(Target) to High(Target) do
    if SameText(Target[I], Value) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function Insert(const Target, Source: Pointer; const Index, TargetSize, SourceSize: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= TargetSize);
  if Result then
  begin
    if Index < TargetSize then
      MoveMemory(PAnsiChar(Target) + Index + SourceSize, PAnsiChar(Target) + Index, TargetSize - Index);
    CopyMemory(PAnsiChar(Target) + Index, Source, SourceSize);
  end;
end;

function Insert(var Target: TInt64DynArray; const Value: Int64; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Int64), Size * SizeOf(Int64), SizeOf(Int64));
end;

function Insert(var Target: TIntegerDynArray; const Value: Integer; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Integer), Size * SizeOf(Integer), SizeOf(Integer));
end;

function Insert(var Target: TByteDynArray; const Value: Byte; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Byte), Size * SizeOf(Byte), SizeOf(Byte));
end;

function Insert(var Target: TSingleDynArray; const Value: Single; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Single), Size * SizeOf(Single), SizeOf(Single));
end;

function Insert(var Target: TDoubleDynArray; const Value: Double; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Double), Size * SizeOf(Double), SizeOf(Double));
end;

function Insert(var Target: TCharDynArray; const Value: Char; Index: Integer): Boolean;
var
  Size: Integer;
begin
  Size := Length(Target);
  SetLength(Target, Size + 1);
  Result := Insert(Target, @Value, Index * SizeOf(Char), Size * SizeOf(Char), SizeOf(Char));
end;

function BinarySearch(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSearchCompare;
  const Value, Data: Pointer): Integer;
begin
  while FromIndex <= TillIndex do
  begin
    Result := (FromIndex + TillIndex) div 2;
    case Compare(Target, Value, Result, Data) of
      LessThanValue: TillIndex := Result - 1;
      GreaterThanValue: FromIndex := Result + 1;
    else Exit;
    end;
  end;
  Result := -1;
end;

function Int64Compare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer): TValueRelationship;
var
  Int64Array: TInt64DynArray absolute Target;
begin
  Result := CompareValue(PInt64(Value)^, Int64Array[Index]);
end;

function Search(const Target: TInt64DynArray; const Value: Int64): Integer;
begin
  Result := BinarySearch(Target, Low(Target), High(Target), Int64Compare, @Value);
end;

function IntegerCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer = nil): TValueRelationship;
var
  IntegerArray: TIntegerDynArray absolute Target;
begin
  Result := CompareValue(PInteger(Value)^, IntegerArray[Index]);
end;

function Search(const Target: TIntegerDynArray; const Value: Integer): Integer;
begin
  Result := BinarySearch(Target, Low(Target), High(Target), IntegerCompare, @Value);
end;

function ByteCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer): TValueRelationship;
var
  ByteArray: TByteDynArray absolute Target;
begin
  Result := CompareValue(PByte(Value)^, ByteArray[Index]);
end;

function Search(const Target: TByteDynArray; const Value: Byte): Integer;
begin
  Result := BinarySearch(Target, Low(Target), High(Target), ByteCompare, @Value);
end;

function SingleCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer): TValueRelationship;
var
  SingleArray: TSingleDynArray absolute Target;
begin
  Result := CompareValue(PSingle(Value)^, SingleArray[Index]);
end;

function Search(const Target: TSingleDynArray; const Value: Single): Integer;
begin
  Result := BinarySearch(Target, Low(Target), High(Target), SingleCompare, @Value);
end;

function DoubleCompare(const Target, Value: Pointer; const Index: Integer; const Data: Pointer): TValueRelationship;
var
  DoubleArray: TDoubleDynArray absolute Target;
begin
  Result := CompareValue(PDouble(Value)^, DoubleArray[Index]);
end;

function Search(const Target: TDoubleDynArray; const Value: Double): Integer;
begin
  Result := BinarySearch(Target, Low(Target), High(Target), DoubleCompare, @Value);
end;

procedure HSort(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSortCompare;
  const Exchange: TSortExchange; const Data: Pointer);
var
  I, J, Child, Parent: Integer;
begin
  if Assigned(Compare) and Assigned(Exchange) and (FromIndex < TillIndex) then
    for I := TillIndex downto FromIndex + 1 do
    begin
      for J := FromIndex + 1 to I do
      begin
        Child := J;
        while Child > FromIndex do
        begin
          Parent := (Child - 1) div 2;
          if Compare(Parent, Child, Target, Data) < 0 then
          begin
            Exchange(Parent, Child, Target, Data);
            Child := Parent;
          end
          else Break;
        end;
      end;
      Exchange(FromIndex, I, Target, Data);
    end;
end;

procedure QSort(const Target: Pointer; FromIndex, TillIndex: Integer; const Compare: TSortCompare;
  const Exchange: TSortExchange; const Data: Pointer);
var
  I, J, Mean: Integer;
begin
  if Assigned(Compare) and Assigned(Exchange) then
    while FromIndex < TillIndex do
    begin
      I := FromIndex;
      J := TillIndex;
      Mean := (FromIndex + TillIndex) div 2;
      repeat
        while Compare(I, Mean, Target, Data) < 0 do Inc(I);
        while Compare(J, Mean, Target, Data) > 0 do Dec(J);
        if I <= J then
        begin
          if I < J then
          begin
            if Mean = I then
              Mean := J
            else
              if Mean = J then Mean := I;
            Exchange(I, J, Target, Data);
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if FromIndex < J then QSort(Target, FromIndex, J, Compare, Exchange, Data);
      FromIndex := I;
    end;
end;

function Int64Compare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  Int64Array: TInt64DynArray absolute Target;
begin
  Result := CompareValue(Int64Array[AIndex], Int64Array[BIndex]);
end;

procedure Int64Exchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Int64;
  Int64Array: TInt64DynArray absolute Target;
begin
  I := Int64Array[AIndex];
  Int64Array[AIndex] := Int64Array[BIndex];
  Int64Array[BIndex] := I;
end;

procedure Sort(var Target: TInt64DynArray);
begin
  QSort(Target, Low(Target), High(Target), Int64Compare, Int64Exchange);
end;

function IntegerCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  IntegerArray: TIntegerDynArray absolute Target;
begin
  Result := CompareValue(IntegerArray[AIndex], IntegerArray[BIndex]);
end;

procedure IntegerExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Integer;
  IntegerArray: TIntegerDynArray absolute Target;
begin
  I := IntegerArray[AIndex];
  IntegerArray[AIndex] := IntegerArray[BIndex];
  IntegerArray[BIndex] := I;
end;

procedure Sort(var Target: TIntegerDynArray);
begin
  QSort(Target, Low(Target), High(Target), IntegerCompare, IntegerExchange);
end;

function ByteCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  ByteArray: TByteDynArray absolute Target;
begin
  Result := CompareValue(ByteArray[AIndex], ByteArray[BIndex]);
end;

procedure ByteExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Byte;
  ByteArray: TByteDynArray absolute Target;
begin
  I := ByteArray[AIndex];
  ByteArray[AIndex] := ByteArray[BIndex];
  ByteArray[BIndex] := I;
end;

procedure Sort(var Target: TByteDynArray);
begin
  QSort(Target, Low(Target), High(Target), ByteCompare, ByteExchange);
end;

function SingleCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  SingleArray: TSingleDynArray absolute Target;
begin
  Result := CompareValue(SingleArray[AIndex], SingleArray[BIndex]);
end;

procedure SingleExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Single;
  SingleArray: TSingleDynArray absolute Target;
begin
  I := SingleArray[AIndex];
  SingleArray[AIndex] := SingleArray[BIndex];
  SingleArray[BIndex] := I;
end;

procedure Sort(var Target: TSingleDynArray);
begin
  QSort(Target, Low(Target), High(Target), SingleCompare, SingleExchange);
end;

function DoubleCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  DoubleArray: TDoubleDynArray absolute Target;
begin
  Result := CompareValue(DoubleArray[AIndex], DoubleArray[BIndex]);
end;

procedure DoubleExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Double;
  DoubleArray: TDoubleDynArray absolute Target;
begin
  I := DoubleArray[AIndex];
  DoubleArray[AIndex] := DoubleArray[BIndex];
  DoubleArray[BIndex] := I;
end;

procedure Sort(var Target: TDoubleDynArray);
begin
  QSort(Target, Low(Target), High(Target), DoubleCompare, DoubleExchange);
end;

function CharCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  CharArray: TCharDynArray absolute Target;
begin
  Result := AnsiCompareStr(CharArray[AIndex], CharArray[BIndex]);
end;

procedure CharExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  I: Char;
  CharArray: TCharDynArray absolute Target;
begin
  I := CharArray[AIndex];
  CharArray[AIndex] := CharArray[BIndex];
  CharArray[BIndex] := I;
end;

procedure Sort(var Target: TCharDynArray);
begin
  QSort(Target, Low(Target), High(Target), CharCompare, CharExchange);
end;

function StringCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  StringArray: TStringDynArray absolute Target;
begin
  Result := CompareValue(Length(StringArray[AIndex]), Length(StringArray[BIndex]));
end;

procedure StringExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  S: string;
  StringArray: TStringDynArray absolute Target;
begin
  S := StringArray[AIndex];
  StringArray[AIndex] := StringArray[BIndex];
  StringArray[BIndex] := S;
end;

procedure Sort(var Target: TStringDynArray);
begin
  QSort(Target, Low(Target), High(Target), StringCompare, StringExchange);
end;

procedure Resize(const Target: Pointer; const PriorSize, Size: Integer; const Fill: Byte);
begin
  if Size > PriorSize then FillMemory(PAnsiChar(Target) + PriorSize, Size - PriorSize, Fill);
end;

procedure Resize(var Target: TInt64DynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Int64), Size * SizeOf(Int64), Fill);
end;

procedure Resize(var Target: TIntegerDynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Integer), Size * SizeOf(Integer), Fill);
end;

procedure Resize(var Target: TByteDynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Byte), Size * SizeOf(Byte), Fill);
end;

procedure Resize(var Target: TSingleDynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Single), Size * SizeOf(Single), Fill);
end;

procedure Resize(var Target: TDoubleDynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Double), Size * SizeOf(Double), Fill);
end;

procedure Resize(var Target: TCharDynArray; const Size: Integer; const Fill: Byte);
var
  ASize: Integer;
begin
  ASize := Length(Target);
  SetLength(Target, Size);
  Resize(Target, ASize * SizeOf(Char), Size * SizeOf(Char), Fill);
end;

procedure Segment(const Source, Target: Pointer; const Index, Size: Integer);
begin
  CopyMemory(Target, PAnsiChar(Source) + Index, Size);
end;

function Segment(const Target: TInt64DynArray; const Index, Size: Integer): TInt64DynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Int64), Size * SizeOf(Int64));
end;

function Segment(const Target: TIntegerDynArray; const Index, Size: Integer): TIntegerDynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Integer), Size * SizeOf(Integer));
end;

function Segment(const Target: TByteDynArray; const Index, Size: Integer): TByteDynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Byte), Size * SizeOf(Byte));
end;

function Segment(const Target: TSingleDynArray; const Index, Size: Integer): TSingleDynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Single), Size * SizeOf(Single));
end;

function Segment(const Target: TDoubleDynArray; const Index, Size: Integer): TDoubleDynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Double), Size * SizeOf(Double));
end;

function Segment(const Target: TCharDynArray; const Index, Size: Integer): TCharDynArray;
begin
  SetLength(Result, Size);
  Segment(Target, Result, Index * SizeOf(Char), Size * SizeOf(Char));
end;

procedure SetRandom(var Target: TIntegerDynArray);
var
  I, J, Count: Integer;
  Order: TIntegerDynArray;
begin
  Count := Length(Target);
  SetLength(Order, Count);
  try
    for I := Low(Order) to High(Order) do Order[I] := I;
    for I := Low(Target) to High(Target) do
    begin
      J := Random(Count - I);
      Target[I] := Order[J];
      Order[J] := Order[Count - I - 1];
    end;
  finally
    Order := nil;
  end;
end;

initialization
  Randomize;

end.
