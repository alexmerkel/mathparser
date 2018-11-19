{ *********************************************************************** }
{                                                                         }
{ TextUtils                                                               }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit TextUtils;

{$B-}

interface

uses
  Windows, SysUtils, Classes, Types, TextConsts;

type
  TFromType = (ftStart, ftEnd);

function PosEx(const SubStr, S: string; const Offset: Cardinal = 1): Integer;
function GetValueFromIndex(const Strings: TStrings; const Index: Integer): string;
procedure SetValueFromIndex(const Strings: TStrings; Index: Integer; const Value: string);

function From(const Text: WideString; Index, Count: Integer): WideString; overload;
function From(const Text: AnsiString; Index, Count: Integer): AnsiString; overload;
function ToString(const Source: WideString; out Target: AnsiString): Boolean; overload;
function ToString(const Text: WideString): AnsiString; overload;
function ToString(const Source: AnsiString; out Target: WideString): Boolean; overload;
function ToString(const Text: AnsiString): WideString; overload;
function ToChar(const Source: WideChar; out Target: AnsiChar): Boolean; overload;
function ToChar(const Source: AnsiChar; out Target: WideChar): Boolean; overload;
function ToChar(const C: WideChar): AnsiChar; overload;
function ToChar(const C: AnsiChar): WideChar; overload;
function OemToString(const Text: string): string;
{$IFDEF UNICODE}
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
{$ELSE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

function TextCopy(const Target: PChar; const Size: Integer; const Source: string): Integer;
function CompareText(const AText, BText: PChar; const ASize, BSize: Integer): Integer;
function SameText(const AText, BText: PChar; const Size: Integer = 0): Boolean; overload;
function SameText(const AText, BText: string): Boolean; overload;
function Consists(const Text: string; const CharSet: TSysCharSet): Boolean;
function Contains(const Text, SubText: string): Boolean;

function DeleteText(var Text: string; const Count: Integer; const From: TFromType = ftStart; const Trim: Boolean = True): Boolean;
function TrimText(const Text: string; const CharSet: TSysCharSet; MaxCount: Integer = 0): string; overload;
function TrimText(const Text: string; const CharSet: TSysCharSet; const From: TFromType; MaxCount: Integer = 0): string; overload;
function TrimText(var Text: string; const SubText: string; const Trim: Boolean = True): Boolean; overload;

function IndexOf(const Text, SubText: string; const IgnoreCase: Boolean; const Offset: Integer = 1): Integer; overload;
function IndexOf(const Text, Delimiter, SubText: string; const IgnoreCase: Boolean): Integer; overload;
function Extract(const Text, Delimiter: string; const Index: Integer; const IgnoreCase: Boolean): string;
function SubText(const Text, Delimiter: string; const Index: Integer; const IgnoreCase: Boolean): string;
function STCount(const Text, SubText: string; const IgnoreCase: Boolean): Integer;
function ReplaceSubText(const Text, SubText, NewSubText: string; const IgnoreCase: Boolean): string;

function Split(const Text: string; const Delimiter: string; var StringArray: TStringDynArray; const IgnoreCase: Boolean; const MaxCount: Integer = 0): Boolean; overload;
function Write(const Text, Delimiter: string; var StringArray: TStringDynArray; const IgnoreCase: Boolean; const MaxCount: Integer = 0): Boolean;
function DelimitedText(const Text, FromDelimiter, ToDelimiter: string; const Index, Count: Integer; const IgnoreCase: Boolean): string; overload;
function DelimitedText(const StringArray: TStringDynArray; const Delimiter: string; MaxCount: Integer = 0): string; overload;

function Duplicate(const Text: string; Index: Integer): Boolean;

function EmptyArray(const StringArray: TStringDynArray): Boolean;
function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Trim: Boolean = True): string; overload;
function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Integer; const Trim: Boolean = True): Integer; overload;
function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Single; const Trim: Boolean = True): Single; overload;
function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Double; const Trim: Boolean = True): Double; overload;

function Encode(const Text: string; const MultiLine: Boolean = False; const CharSet: TSysCharSet = []): string;
function Decode(const Text: string; const CharSet: TSysCharSet = []): string;

function CreateGuid(Guid: PGuid = nil): string;
function UniqueName: string;

function GetTextHeight(const Text: string; Rect: TRect; const FontHandle: THandle): Integer;
function GetTextSize(const Text: string; const FontHandle: THandle): TSize;

const
  {$IFDEF UNICODE}
  DigitCount = 4;
  {$ELSE}
  DigitCount = 2;
  {$ENDIF}

  CSTR_LESS_THAN = 1;
  CSTR_EQUAL = 2;
  CSTR_GREATER_THAN = 3;

var
  CodePage: Integer = 1251;

implementation

uses
  MemoryUtils, TextBuilder, StrUtils;

const
  AnsiFlags = WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  WideFlags = MB_PRECOMPOSED;

var
  DC: HDC;

function PosEx(const SubStr, S: string; const Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function GetValueFromIndex(const Strings: TStrings; const Index: Integer): string;
begin
  if Index < 0 then
    Result := ''
  else
    Result := SubText(Strings[Index], Equal, 1, False);
end;

procedure SetValueFromIndex(const Strings: TStrings; Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Strings.Add('');
    Strings[Index] := Strings.Names[Index] + Equal + Value;
  end
  else if Index >= 0 then Strings.Delete(Index);
end;

function From(const Text: WideString; Index, Count: Integer): WideString;
var
  I, J: Integer;
begin
  I := Length(Text);
  if Index < 1 then
    Index := 0
  else begin
    Dec(Index);
    if Index > I then
      Index := I;
  end;
  if Count < 0 then
    J := 0
  else begin
    J := I - Index;
    if J > Count then
      J := Count;
  end;
  SetLength(Result, J);
  if J > 0 then
    CopyMemory(Pointer(Result), PWideChar(Text) + Index, J * SizeOf(WideChar));
end;

function From(const Text: AnsiString; Index, Count: Integer): AnsiString;
var
  I, J: Integer;
begin
  I := Length(Text);
  if Index < 1 then
    Index := 0
  else begin
    Dec(Index);
    if Index > I then
      Index := I;
  end;
  if Count < 0 then
    J := 0
  else begin
    J := I - Index;
    if J > Count then
      J := Count;
  end;
  SetLength(Result, J);
  if J > 0 then
    CopyMemory(Pointer(Result), PAnsiChar(Text) + Index, J * SizeOf(AnsiChar));
end;

function ToString(const Source: WideString; out Target: AnsiString): Boolean;
var
  I: Integer;
begin
  I := WideCharToMultiByte(CodePage, AnsiFlags, PWideChar(Source), -1, nil, 0, nil, nil) - 1;
  Result := I > 0;
  if Result then
  begin
    SetLength(Target, I);
    WideCharToMultiByte(CodePage, AnsiFlags, PWideChar(Source), -1, PAnsiChar(Target), I, nil, nil);
  end;
end;

function ToString(const Text: WideString): AnsiString;
begin
  if not ToString(Text, Result) then Result := '';
end;

function ToString(const Source: AnsiString; out Target: WideString): Boolean;
var
  I: Integer;
begin
  I := MultiByteToWideChar(CodePage, WideFlags, PAnsiChar(Source), -1, nil, 0) - 1;
  Result := I > 0;
  if Result then
  begin
    SetLength(Target, I);
    MultiByteToWideChar(CodePage, WideFlags, PAnsiChar(Source), -1, PWideChar(Target), I);
  end;
end;

function ToString(const Text: AnsiString): WideString;
begin
  if not ToString(Text, Result) then Result := '';
end;

function ToChar(const Source: WideChar; out Target: AnsiChar): Boolean;
begin
  Result := WideCharToMultiByte(CodePage, AnsiFlags, @Source, 1, @Target, SizeOf(AnsiChar), nil, nil) <> 0;
end;

function ToChar(const Source: AnsiChar; out Target: WideChar): Boolean;
begin
  Result := MultiByteToWideChar(CodePage, WideFlags, @Source, SizeOf(AnsiChar), @Target, 1) <> 0;
end;

function ToChar(const C: WideChar): AnsiChar;
begin
  if not ToChar(C, Result) then Result := #0;
end;

function ToChar(const C: AnsiChar): WideChar;
begin
  if not ToChar(C, Result) then Result := #0;
end;

function OemToString(const Text: string): string;
var
  I: Integer;
begin
  I := Length(Text);
  SetLength(Result, I);
  OemToCharBuff(PAnsiChar(Text), PChar(Result), I);
end;

{$IFDEF UNICODE}
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
var
  A: AnsiChar;
begin
  Result := (WideCharToMultiByte(CodePage, AnsiFlags, @C, 1, @A, SizeOf(AnsiChar), nil, nil) <> 0) and (A in CharSet);
end;
{$ELSE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function TextCopy(const Target: PChar; const Size: Integer; const Source: string): Integer;
begin
  Result := Length(Source);
  if Result > Size - 1 then Result := Size - 1;
  StrLCopy(Target, PChar(Source), Result);
end;

function CompareText(const AText, BText: PChar; const ASize, BSize: Integer): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, AText, ASize, BText, BSize);
end;

function SameText(const AText, BText: PChar; const Size: Integer): Boolean;
var
  I, J: Integer;
begin
  if Size > 0 then I := Size
  else begin
    I := StrLen(AText);
    J := StrLen(BText);
    if I > J then I := J;
  end;
  Result := CompareText(AText, BText, I, I) = CSTR_EQUAL;
end;

function SameText(const AText, BText: string): Boolean;
begin
  Result := (Length(AText) = Length(BText)) and (SameText(PChar(AText), PChar(BText)));
end;

function Consists(const Text: string; const CharSet: TSysCharSet): Boolean;
var
  I: Integer;
begin
  Result := Text <> '';
  if Result then
  begin
    for I := 1 to Length(Text) do
      if not CharInSet(Text[I], CharSet) then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;
end;

function Contains(const Text, SubText: string): Boolean;
begin
  Result := Pos(SubText, Text) > 0;
end;

function DeleteText(var Text: string; const Count: Integer; const From: TFromType; const Trim: Boolean): Boolean;
begin
  Result := Count <= Length(Text);
  if Result then
    case From of
      ftEnd:
        if Trim then
          Text := TrimRight(Copy(Text, 1, Length(Text) - Count))
        else
          Text := Copy(Text, 1, Length(Text) - Count);
    else
      if Trim then
        Text := TrimLeft(Copy(Text, Count + 1, Length(Text) - Count))
      else
        Text := Copy(Text, Count + 1, Length(Text) - Count);
    end;
end;

function TrimText(const Text: string; const CharSet: TSysCharSet; MaxCount: Integer): string;
var
  I, J: Integer;
begin
  I := 1;
  J := Length(Text);
  while (I <= J) and CharInSet(Text[I], CharSet) and ((MaxCount = 0) or (MaxCount >= I)) do Inc(I);
  if I > J then Result := ''
  else begin
    if MaxCount > 0 then MaxCount := J - MaxCount + 1;
    while CharInSet(Text[J], CharSet) and ((MaxCount = 0) or (MaxCount <= J)) do Dec(J);
    Result := Copy(Text, I, J - I + 1);
  end;
end;

function TrimText(const Text: string; const CharSet: TSysCharSet; const From: TFromType; MaxCount: Integer): string;
var
  I, J: Integer;
begin
  case From of
    ftEnd:
      begin
        I := Length(Text);
        if MaxCount > 0 then MaxCount := I - MaxCount + 1;
        while (I > 0) and CharInSet(Text[I], CharSet) and ((MaxCount = 0) or (MaxCount <= I)) do Dec(I);
        Result := Copy(Text, 1, I);
      end;
  else
    I := 1;
    J := Length(Text);
    while (I <= J) and CharInSet(Text[I], CharSet) and ((MaxCount = 0) or (MaxCount >= I)) do Inc(I);
    Result := Copy(Text, I, MaxInt);
  end;
end;

function TrimText(var Text: string; const SubText: string; const Trim: Boolean): Boolean;
var
  I: Integer;
begin
  I := Length(SubText);
  Result := (I <= Length(Text)) and SameText(PChar(Text), PChar(SubText), I);
  if Result then DeleteText(Text, I, ftStart, Trim);
end;

function IndexOf(const Text, SubText: string; const IgnoreCase: Boolean; const Offset: Integer): Integer;
var
  I, J, K: Integer;
begin
  if IgnoreCase then
  begin
    I := Offset - 1;
    J := Length(SubText);
    K := Length(Text);
    while I + J <= K do
      if SameText(PChar(Text) + I, PChar(SubText), J) then
      begin
        Result := I + 1;
        Exit;
      end
      else Inc(I);
    Result := 0;
  end
  else Result := PosEx(SubText, Text, Offset);
end;

function IndexOf(const Text, Delimiter, SubText: string; const IgnoreCase: Boolean): Integer;
var
  I: Integer;
begin
  if TextUtils.SameText(Text, SubText) then Result := 0
  else begin
    for I := 0 to STCount(Text, Delimiter, IgnoreCase) do
      if TextUtils.SameText(TextUtils.SubText(Text, Delimiter, I, IgnoreCase), SubText) then
      begin
        Result := I;
        Exit;
      end;
    Result := -1;
  end;
end;

function Extract(const Text, Delimiter: string; const Index: Integer; const IgnoreCase: Boolean): string;
begin
  Result := SubText(Text, Delimiter, Index, IgnoreCase);
  if (Index = 0) and (Result = '') and not AnsiStartsText(Delimiter, Text) then
    Result := Text;
end;

function SubText(const Text, Delimiter: string; const Index: Integer; const IgnoreCase: Boolean): string;
var
  I, J, K, L: Integer;
begin
  J := 0;
  K := 0;
  L := Length(Delimiter);
  for I := 0 to Index do
  begin
    K := J;
    if I > 0 then
    begin
      if J = 0 then Break;
      J := IndexOf(Text, Delimiter, IgnoreCase, J + L)
    end
    else J := IndexOf(Text, Delimiter, IgnoreCase);
  end;
  if (J > 0) or (K > 0) then
  begin
    if J = 0 then J := Length(Text) + 1;
    if K > 0 then Inc(K, L)
    else K := 1;
    Result := Copy(Text, K, J - K);
  end
  else Result := '';
end;

function STCount(const Text, SubText: string; const IgnoreCase: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  if SubText <> '' then
  begin
    I := IndexOf(Text, SubText, IgnoreCase);
    while I > 0 do
    begin
      Inc(Result);
      I := IndexOf(Text, SubText, IgnoreCase, I + Length(SubText));
    end;
  end;
end;

function ReplaceSubText(const Text, SubText, NewSubText: string; const IgnoreCase: Boolean): string;
var
  Builder: TTextBuilder;
  StringArray: TStringDynArray;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    Split(Text, SubText, StringArray, IgnoreCase);
    try
      for I := Low(StringArray) to High(StringArray) do
        if I > Low(StringArray) then
          Builder.Append(NewSubText + StringArray[I])
        else
          Builder.Append(StringArray[I]);
    finally
      StringArray := nil;
    end;
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function Split(const Text: string; const Delimiter: string; var StringArray: TStringDynArray; const IgnoreCase: Boolean; const MaxCount: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := (MaxCount <= 0) or (Length(StringArray) < MaxCount);
  if Result then
  begin
    {
    J := 1;
    K := Length(Delimiter);
    repeat
      I := PosEx(Delimiter, Text, J);
      if I > 0 then
      begin
        S := Copy(Text, J, I - J);
        J := I + K;
      end
      else S := Copy(S, J, MaxInt);
      Add(StringArray, S);
      if (MaxCount > 0) and (Length(StringArray) >= MaxCount) then Break;
    until I = 0;
    }
    J := STCount(Text, Delimiter, IgnoreCase);
    if J > 0 then
      for I := 0 to J do
      begin
        Add(StringArray, SubText(Text, Delimiter, I, IgnoreCase));
        if (MaxCount > 0) and (Length(StringArray) >= MaxCount) then Break;
      end
    else Add(StringArray, Text);
  end;
end;

function Write(const Text, Delimiter: string; var StringArray: TStringDynArray; const IgnoreCase: Boolean; const MaxCount: Integer): Boolean;
begin
  Result := (Text <> '') and Split(Text, Delimiter, StringArray, IgnoreCase, MaxCount);
end;

function DelimitedText(const Text, FromDelimiter, ToDelimiter: string; const Index, Count: Integer; const IgnoreCase: Boolean): string;
var
  Builder: TTextBuilder;
  StringArray: TStringDynArray;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    Split(Text, FromDelimiter, StringArray, IgnoreCase);
    try
      if (Index >= 0) and (Index + Count <= Length(StringArray)) then
        for I := Index to Index + Count - 1 do
          if I > Index then
            Builder.Append(ToDelimiter + StringArray[I])
          else
            Builder.Append(StringArray[I]);
      Result := Builder.Text;
    finally
      StringArray := nil;
    end;
  finally
    Builder.Free;
  end;
end;

function DelimitedText(const StringArray: TStringDynArray; const Delimiter: string; MaxCount: Integer = 0): string;
var
  Builder: TTextBuilder;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    if MaxCount = 0 then MaxCount := Length(StringArray);
    for I := Low(StringArray) to MaxCount - 1 do
      if I > Low(StringArray) then
        Builder.Append(Delimiter + StringArray[I])
      else
        Builder.Append(StringArray[I]);
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function Duplicate(const Text: string; Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
    if (I <> Index) and (Text[I] = Text[Index]) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function EmptyArray(const StringArray: TStringDynArray): Boolean;
var
  I: Integer;
begin
  for I := Low(StringArray) to High(StringArray) do
    if Trim(StringArray[I]) <> '' then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Trim: Boolean): string;
begin
  if Index < Length(StringArray) then
    if Trim then
      Result := SysUtils.Trim(StringArray[Index])
    else
      Result := StringArray[Index]
  else Result := '';
end;

function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Integer; const Trim: Boolean = True): Integer;
begin
  Result := StrToIntDef(ArrayValue(StringArray, Index, Trim), Default);
end;

function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Single; const Trim: Boolean = True): Single;
begin
  Result := StrToFloatDef(ArrayValue(StringArray, Index, Trim), Default);
end;

function ArrayValue(const StringArray: TStringDynArray; const Index: Integer; const Default: Double; const Trim: Boolean = True): Double;
begin
  Result := StrToFloatDef(ArrayValue(StringArray, Index, Trim), Default);
end;

function Encode(const Text: string; const MultiLine: Boolean; const CharSet: TSysCharSet): string;
var
  Builder: TTextBuilder;
  I: Integer;
begin
  Builder := TTextBuilder.Create;
  try
    for I := 1 to Length(Text) do
      if MultiLine and CharInSet(Text[I], Breaks) or (CharSet <> []) and not CharInSet(Text[I], CharSet) then
        Builder.Append(Text[I])
      else
        Builder.Append(Percent + IntToHex(Ord(Text[I]), DigitCount));
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function Decode(const Text: string; const CharSet: TSysCharSet): string;
var
  Builder: TTextBuilder;
  I, J: Integer;
begin
  Result := ReplaceSubText(Text, Percent + Percent, Percent + IntToHex(Ord(Percent), DigitCount), False);
  Builder := TTextBuilder.Create;
  try
    I := 1;
    while I <= Length(Text) do
      if (Text[I] = Percent) and TryStrToInt(Dollar + Copy(Text, I + 1, DigitCount), J) and ((CharSet = []) or CharInSet(Chr(J), CharSet)) then
      begin
        Builder.Append(Chr(J));
        Inc(I, DigitCount + 1);
      end
      else begin
        Builder.Append(Text[I]);
        Inc(I);
      end;
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function CreateGuid(Guid: PGuid): string;
var
  AGuid: TGuid;
begin
  if not Assigned(Guid) then Guid := @AGuid;
  if SysUtils.CreateGuid(Guid^) = S_OK then
    Result := GuidToString(Guid^)
  else
    Result := '';
end;

function UniqueName: string;
const
  GuidTemplate = '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x';
  GuidSize = 32;
var
  Guid: TGuid;
begin
  if SysUtils.CreateGuid(Guid) = S_OK then
  begin
    SetLength(Result, GuidSize);
    StrLFmt(PChar(Result), GuidSize, GuidTemplate, [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3], Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
  end
  else Result := '';
end;

function GetTextHeight(const Text: string; Rect: TRect; const FontHandle: THandle): Integer;
var
  Handle: THandle;
begin
  Handle := SelectObject(DC, FontHandle);
  try
    Result := DrawText(DC, PChar(Text), Length(Text), Rect, DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX or DT_CALCRECT);
  finally
    SelectObject(DC, Handle);
  end;
end;

function GetTextSize(const Text: string; const FontHandle: THandle): TSize;
var
  Handle: THandle;
begin
  Handle := SelectObject(DC, FontHandle);
  try
    GetTextExtentPoint(DC, PChar(Text), Length(Text), Result);
  finally
    SelectObject(DC, Handle);
  end;
end;

initialization
  DC := GetDC(0);

finalization
  ReleaseDC(0, DC);

end.
