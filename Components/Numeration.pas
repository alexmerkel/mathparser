{ *********************************************************************** }
{                                                                         }
{ Numeration                                                              }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (post@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit Numeration;

{$B-}

interface

uses
  SysUtils, Classes, TextConsts;

type
  TCustomNumeration = class(TComponent)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetNegativeChar: Char; virtual; abstract;
    function GetNumeration(const Index: Integer): string; virtual; abstract;
    function GetPositiveChar: Char; virtual; abstract;
    procedure SetNegativeChar(const Value: Char); virtual; abstract;
    procedure SetNumeration(const Index: Integer; const Value: string); virtual; abstract;
    procedure SetPositiveChar(const Value: Char); virtual; abstract;
  public
    function Add(const Value: string): Integer; virtual; abstract;
    procedure Delete(const Index: Integer); virtual; abstract;
    function Check(const Value: string): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function ConvertFrom(Value: string; const Index: Integer): Int64; virtual; abstract;
    function ConvertTo(Value: Int64; const Index: Integer): string; virtual; abstract;
    property Count: Integer read GetCount;
    property Numeration[const Index: Integer]: string read GetNumeration write SetNumeration; default;
  published
    property NegativeChar: Char read GetNegativeChar write SetNegativeChar;
    property PositiveChar: Char read GetPositiveChar write SetPositiveChar;
  end;

  ENumerationError = class(Exception);

  TNumeration = class(TCustomNumeration)
  private
    FList: TStrings;
    FNegativeChar: Char;
    FPositiveChar: Char;
  protected
    function GetCount: Integer; override;
    function GetNegativeChar: Char; override;
    function GetNumeration(const Index: Integer): string; override;
    function GetPositiveChar: Char; override;
    procedure SetNegativeChar(const Value: Char); override;
    procedure SetNumeration(const Index: Integer; const Value: string); override;
    procedure SetPositiveChar(const Value: Char); override;
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const Value: string): Integer; override;
    procedure Delete(const Index: Integer); override;
    function Check(const Value: string): Boolean; override;
    procedure Clear; override;
    function ConvertFrom(Value: string; const Index: Integer): Int64; override;
    function ConvertTo(Value: Int64; const Index: Integer): string; override;
  published
    property NegativeChar default Minus;
    property PositiveChar default Plus;
  end;

const
  ValueError = '"%s" is not valid value';

procedure Register;

implementation

uses
  Math, TextUtils;

procedure Register;
begin
  RegisterComponents('Samples', [TNumeration]);
end;

{ TNumeration }

function TNumeration.Add(const Value: string): Integer;
begin
  if Check(Value) then
    Result := FList.Add(Value)
  else
    Result := -1;
end;

function TNumeration.Check(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := (Length(Value) > 1) and not Contains(Value, FNegativeChar) and not Contains(Value, FPositiveChar);
  if Result then
    for I := 1 to Length(Value) do
      if Duplicate(Value, I) then
      begin
        Result := False;
        Break;
      end;
end;

procedure TNumeration.Clear;
begin
  inherited;
  FList.Clear;
end;

function TNumeration.ConvertFrom(Value: string; const Index: Integer): Int64;
var
  Flag: Boolean;
  S: string;
  I, J, K, L: Integer;
begin
  if Value = '' then raise Error(ValueError, [Value]);
  Result := 0;
  Flag := TrimText(Value, FNegativeChar);
  S := FList[Index];
  J := Length(Value);
  L := Length(S);
  for I := 1 to J do
  begin
    K := AnsiPos(Value[I], S);
    if K = 0 then
      raise Error(ValueError, [Value])
    else
      Result := Result + (K - 1) * Trunc(Power(L, J - I));
  end;
  if Flag then Result := - Result;
end;

function TNumeration.ConvertTo(Value: Int64; const Index: Integer): string;
var
  Negative: Boolean;
  S: string;
  I: Int64;
  J: Integer;
begin
  Result := '';
  Negative := Value < 0;
  Value := Abs(Value);
  S := FList[Index];
  J := Length(S);
  repeat
    I := Value;
    Value := Value div J;
    I := I - Value * J;
    Result :=  S[I + 1] + Result;
  until Value = 0;
  if Negative then Result := FNegativeChar + Result;
end;

constructor TNumeration.Create(AOwner: TComponent);
begin
  inherited;
  FList := TStringList.Create;
  FNegativeChar := Minus;
  FPositiveChar := Plus;
end;

procedure TNumeration.Delete(const Index: Integer);
begin
  inherited;
  FList.Delete(Index);
end;

destructor TNumeration.Destroy;
begin
  FList.Free;
  inherited;
end;

function TNumeration.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TNumeration.Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := ENumerationError.CreateFmt(Message, Arguments);
end;

function TNumeration.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNumeration.GetNegativeChar: Char;
begin
  Result := FNegativeChar;
end;

function TNumeration.GetNumeration(const Index: Integer): string;
begin
  Result := FList[Index];
end;

function TNumeration.GetPositiveChar: Char;
begin
  Result := FPositiveChar;
end;

procedure TNumeration.SetNegativeChar(const Value: Char);
begin
  FNegativeChar := Value;
end;

procedure TNumeration.SetNumeration(const Index: Integer; const Value: string);
begin
  FList[Index] := Value;
end;

procedure TNumeration.SetPositiveChar(const Value: Char);
begin
  FPositiveChar := Value;
end;

end.
