{ *********************************************************************** }
{                                                                         }
{ ParseValidator                                                          }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseValidator;

{$B-}
{$I Directives.inc}

interface

uses
  SysUtils, Types, ParseErrors, ParseTypes, TextConsts;

type
  TReserveType = (rtName, rtText);

  EValidatorError = class(Exception);

  TValidator = class
  protected
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
  public
    function Check(const AText: string; const AType: TReserveType): TError; virtual;
  end;

var
  Validator: TValidator;
  Reserve: array[TReserveType] of string;

implementation

uses
  NumberConsts, ParseUtils, TextUtils;

{ TValidator }

{$WARNINGS OFF}
function TValidator.Check(const AText: string; const AType: TReserveType): TError;
var
  LockArray: TLockArray;
  I: Integer;
begin
  if AType = rtText then LockArray := GetLockArray(AText, ParseUtils.LockChar);
  try
    for I := 1 to Length(AText) do
      if ((AType <> rtText) or not Locked(I, LockArray)) and Contains(Reserve[AType], AText[I]) then
      begin
        Result := MakeError(etReserveError, EText(ReserveError, [AText, Reserve[AType][I]]));
        Exit;
      end;
  finally
    LockArray := nil;
  end;
  if (AType = rtName) and (Length(AText) > 0) and Number(AText[1]) then
    Result := MakeError(etDefinitionError, EText(DefinitionError, [AText]))
  else
    FillChar(Result, SizeOf(TError), 0);
end;
{$WARNINGS ON}

function TValidator.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TValidator.Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := EValidatorError.CreateFmt(Message, Arguments);
end;

initialization
  Validator := TValidator.Create;
  Reserve[rtName] := LeftBrace + RightBrace + LeftParenthesis + RightParenthesis +
    LeftBracket + RightBracket + Comma + {$IFDEF DELPHI_XE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF} +
    DoubleQuote + Minus + Plus + TextConsts.Quote + Semicolon + Space;
  Reserve[rtText] := LeftBrace + RightBrace + Pipe + TextConsts.Quote;

finalization
  Validator.Free;

end.
