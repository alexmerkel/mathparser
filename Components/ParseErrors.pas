{ *********************************************************************** }
{                                                                         }
{ ParseErrors                                                             }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseErrors;

{$B-}

interface

uses
  SysUtils;

type
  TErrorType = (etSuccess, etUnknown, etAMutualExcessError, etBMutualExcessError, etBracketError,
    etEmptyTextError, etFunctionHandleError, etFunctionExpectError, etElementError, etRExcessError,
    etLTextExcessError, etLTextExpectError, etDefinitionError, etAParameterExcessError,
    etBParameterExcessError, etAParameterExpectError, etBParameterExpectError, etReserveError,
    etRTextExcessError, etRTextExpectError, etScriptError, etStringError, etStringTypeError,
    etNumberTypeError, etFunctionTypeError, etScriptTypeError, etTextError, etSizeError);

  PError = ^TError;
  TError = record
    ErrorType: TErrorType;
    ErrorText: string;
  end;

  EParserError = class(Exception);

const
  ErrorMessage = '%s: "%s"';
  AMutualExcessError = 'Function "%s" requires expression after itself, function "%s" requires expression before itself';
  BMutualExcessError = 'Function "%s" does not require expression after itself, function "%s" does not require expression before itself';
  BracketError = 'Bracket character expected';
  EmptyTextError = 'Empty text';
  FunctionHandleError = 'Function handle error: "%s"';
  FunctionExpectError = 'Function expected: "%s"';
  ElementError = 'Unknown element: "%s"';
  RExcessError = '"%s" does not require expression after itself';
  LTextExcessError = 'Function "%s" does not require expression before itself';
  LTextExpectError = 'Function "%s" requires expression before itself';
  DefinitionError = '"%s" cannot start with a number';
  AParameterExcessError = 'Function "%s" does not require parameters';
  BParameterExcessError = 'Too much parameters for function "%s"';
  AParameterExpectError = 'Function "%s" requires parameters';
  BParameterExpectError = 'Not enough parameters for function "%s"';
  ReserveError = 'Text "%s" contains reserved character: "%s"';
  RTextExcessError = 'Function "%s" does not require expression after itself';
  RTextExpectError = 'Function "%s" requires expression after itself';
  ScriptError = 'Script error';
  StringError = '"%s" cannot be the part of math expression';
  StringTypeError = '"%s" cannot have type of string';
  NumberTypeError = 'Cannot apply "%s" type to "%s" number in "%s" expression';
  FunctionTypeError = 'Cannot apply "%s" type to "%s" function in "%s" expression';
  ScriptTypeError = 'Cannot apply "%s" type to "%s" script in "%s" expression';
  TextError = 'Expression expected: "%s"';
  SizeError = 'Incomplete expression';

function MakeError(const AErrorType: TErrorType; const AErrorText: string): TError;
function Error(const Message: string): Exception; overload;
function EText(const Message: string; const Arguments: array of const): string; overload;
function Error(const Message: string; const Arguments: array of const): Exception; overload;
function Error(const Text, Message: string): Exception; overload;
function EText(const Text, Message: string; const Arguments: array of const): string; overload;
function Error(const Text, Message: string; const Arguments: array of const): Exception; overload;

implementation

function MakeError(const AErrorType: TErrorType; const AErrorText: string): TError;
begin
  FillChar(Result, SizeOf(TError), 0);
  with Result do
  begin
    ErrorType := AErrorType;
    ErrorText := AErrorText;
  end;
end;

function Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function EText(const Message: string; const Arguments: array of const): string;
begin
  Result := Format(Message, Arguments);
end;

function Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := EParserError.Create(EText(Message, Arguments));
end;

function Error(const Text, Message: string): Exception;
begin
  Result := Error(Text, Message, []);
end;

function EText(const Text, Message: string; const Arguments: array of const): string;
begin
  Result := Format(ErrorMessage, [Format(Message, Arguments), Text]);
end;

function Error(const Text, Message: string; const Arguments: array of const): Exception;
begin
  Result := Error(EText(Text, Message, Arguments));
end;

end.
