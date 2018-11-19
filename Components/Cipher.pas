{ *********************************************************************** }
{                                                                         }
{ Cipher                                                                  }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (post@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit Cipher;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Classes, Numeration, TextConsts;

type
  TBufferSize = SizeOf(Byte)..SizeOf(Int64) div SizeOf(Char);

  TCustomCipher = class(TComponent)
  protected
    function GetBufferSize: TBufferSize; virtual; abstract;
    function GetDelimiter: Char; virtual; abstract;
    function GetKey: string; virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    function GetNumeration: TNumeration; virtual; abstract;
    function GetStart: Integer; virtual; abstract;
    procedure SetBufferSize(const Value: TBufferSize); virtual; abstract;
    procedure SetDelimiter(const Value: Char); virtual; abstract;
    procedure SetKey(const Value: string); virtual; abstract;
    procedure SetLines(const Value: TStrings); virtual; abstract;
    procedure SetStart(const Value: Integer); virtual; abstract;
  public
    function CheckKey(const Key: string): Boolean; virtual; abstract;
    function BuildKey(const Size: Integer; const S: string): string; virtual; abstract;
    procedure Encrypt; virtual; abstract;
    procedure Decrypt; virtual; abstract;
    property Numeration: TNumeration read GetNumeration;
  published
    property Lines: TStrings read GetLines write SetLines;
    property Key: string read GetKey write SetKey;
    property Start: Integer read GetStart write SetStart;
    property BufferSize: TBufferSize read GetBufferSize write SetBufferSize default High(TBufferSize);
    property Delimiter: Char read GetDelimiter write SetDelimiter default Space;
  end;

  ECipherError = class(Exception);

  TBuffer = array[0..SizeOf(Int64) div SizeOf(Char) - 1] of Char;

  TCipher = class(TCustomCipher)
  private
    FBufferSize: TBufferSize;
    FDelimiter: Char;
    FKey: string;
    FLines: TStrings;
    FNumeration: TNumeration;
    FStart: Integer;
  protected
    function GetBufferSize: TBufferSize; override;
    function GetDelimiter: Char; override;
    function GetKey: string; override;
    function GetLines: TStrings; override;
    function GetNumeration: TNumeration; override;
    function GetStart: Integer; override;
    procedure SetBufferSize(const Value: TBufferSize); override;
    procedure SetDelimiter(const Value: Char); override;
    procedure SetKey(const Value: string); override;
    procedure SetLines(const Value: TStrings); override;
    procedure SetStart(const Value: Integer); override;
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckKey(const Key: string): Boolean; override;
    function BuildKey(const Size: Integer; const S: string): string; override;
    procedure Encrypt; overload; override;
    procedure Encrypt(const AKey: string); reintroduce; overload; virtual;
    procedure Decrypt; overload; override;
    procedure Decrypt(const AKey: string); reintroduce; overload; virtual;
  end;

const
  KeyError = '"%s" is not valid key';

procedure Register;

implementation

uses
  MemoryUtils, TextBuilder, TextUtils, Types;

procedure Register;
begin
  RegisterComponents('Samples', [TCipher]);
end;

{ TCipher }

function TCipher.BuildKey(const Size: Integer; const S: string): string;
var
  Index: TIntegerDynArray;
  I, J: Integer;
begin
  I := Length(S);
  SetLength(Index, I);
  try
    SetRandom(Index);
    if Size < I then
      J := Size
    else
      J := I;
    SetLength(Result, J);
    for I := Low(Index) to J - 1 do Result[I + 1] := S[Index[I] + 1];
  finally
    Index := nil;
  end;
end;

function TCipher.CheckKey(const Key: string): Boolean;
begin
  Result := FNumeration.Check(Key);
end;

constructor TCipher.Create(AOwner: TComponent);
begin
  inherited;
  FNumeration := TNumeration.Create(Self);
  FNumeration.SetSubComponent(True);
  FLines := TStringList.Create;
  FBufferSize := High(TBufferSize);
  FDelimiter := Space;
end;

procedure TCipher.Decrypt(const AKey: string);
begin
  Key := AKey;
  Decrypt;
end;

procedure TCipher.Decrypt;
var
  StringArray: TStringDynArray;
  Builder: TTextBuilder;
  I, J: Integer;
  K: Int64;
  Buffer: TBuffer;
begin
  Split(FLines.Text, FDelimiter, StringArray, False);
  try
    FLines.Clear;
    Builder := TTextBuilder.Create;
    try
      J := FStart;
      for I := Low(StringArray) to High(StringArray) do
      begin
        K := FNumeration.ConvertFrom(Trim(StringArray[I]), J);
        ZeroMemory(@Buffer, Length(Buffer) * SizeOf(Char));
        CopyMemory(@Buffer, @K, SizeOf(Int64));
        Builder.Append(Buffer);
        if J < FNumeration.Count - 1 then
          Inc(J)
        else
          J := 0;
      end;
      FLines.Text := Builder.Text;
    finally
      Builder.Free;
    end;
  finally
    StringArray := nil;
  end;
end;

destructor TCipher.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TCipher.Encrypt(const AKey: string);
begin
  Key := AKey;
  Encrypt;
end;

procedure TCipher.Encrypt;
var
  Text: string;
  Builder: TTextBuilder;
  Buffer: TBuffer;
  I, J: Integer;
begin
  Text := FLines.Text;
  FLines.Clear;
  Builder := TTextBuilder.Create;
  try
    I := 0;
    J := FStart;
    while I < Length(Text) do
    begin
      ZeroMemory(@Buffer, Length(Buffer) * SizeOf(Char));
      CopyMemory(@Buffer, PAnsiChar(Text) + I * SizeOf(Char), FBufferSize * SizeOf(Char));
      Builder.Append(FNumeration.ConvertTo(PInt64(@Buffer)^, J), FDelimiter);
      Inc(I, FBufferSize);
      if J < FNumeration.Count - 1 then
        Inc(J)
      else
        J := 0;
    end;
    FLines.Text := Builder.Text;
  finally
    Builder.Free;
  end;
end;

function TCipher.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TCipher.Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := ECipherError.CreateFmt(Message, Arguments);
end;

function TCipher.GetBufferSize: TBufferSize;
begin
  Result := FBufferSize;
end;

function TCipher.GetDelimiter: Char;
begin
  Result := FDelimiter;
end;

function TCipher.GetKey: string;
begin
  Result := FKey;
end;

function TCipher.GetLines: TStrings;
begin
  Result := FLines;
end;

function TCipher.GetNumeration: TNumeration;
begin
  Result := FNumeration;
end;

function TCipher.GetStart: Integer;
begin
  Result := FStart;
end;

procedure TCipher.SetBufferSize(const Value: TBufferSize);
begin
  FBufferSize := Value;
end;

procedure TCipher.SetDelimiter(const Value: Char);
begin
  FDelimiter := Value;
end;

procedure TCipher.SetKey(const Value: string);
var
  I: Integer;
  S: string;
begin
  FKey := Value;
  FNumeration.Clear;
  if Key <> '' then
  begin
    I := Length(FKey);
    S := FKey;
    repeat
      if FNumeration.Add(S) < 0 then
      begin
        FNumeration.Clear;
        raise Error(KeyError, [FKey]);
      end;
      S := Copy(S, 2, I) + Copy(S, 1, 1);
    until S = FKey;
  end;
end;

procedure TCipher.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TCipher.SetStart(const Value: Integer);
begin
  FStart := Value;
end;

end.
