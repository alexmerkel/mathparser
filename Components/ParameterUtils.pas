{ *********************************************************************** }
{                                                                         }
{ ParameterUtils                                                          }
{                                                                         }
{ Copyright (c) 2018 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParameterUtils;

{$B-}

interface

uses
  SysUtils, StrUtils;

type
  TParameterType = (ptSmall, ptLarge);
  TParameter = array[TParameterType] of string;
  EParameterError = class(Exception);

function GetLine: string;
function Parameter(const Small, Large: string): TParameter;

procedure RegisterParameter(const ParameterArray: array of TParameter);
function FindParameter(const Parameter: string): Boolean;

function CheckParameter(const ParameterArray: array of TParameter; out Parameter: TParameter): Boolean; overload;
procedure CheckParameter(const ParameterArray: array of TParameter); overload;
procedure CheckCommand(Command: TParameter; const ParameterArray: array of TParameter);
function GetString(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: string): Boolean; overload;
function GetString(const Text: string; Parameter: TParameter; var Index: Integer; Count: Integer; var Value: string): Boolean; overload;
function GetInteger(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: Integer): Boolean; overload;
function GetInteger(const Text: string; Parameter: TParameter; var Index: Integer; Count: Integer; var Value: Integer): Boolean; overload;
function GetBoolean(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: Boolean): Boolean; overload;
function GetBoolean(const Text: string; const Parameter: TParameter; var Index: Integer; Count: Integer; var Value: Boolean): Boolean; overload;

const
  Prefix = '-';
  ParameterExpectError = 'Отсутствует обязательный параметр "%s". Командная строка: "%s"';
  ParameterExcessError = 'Неподдерживаемый параметр: "%s". Командная строка: "%s"';
  CommandError = 'Отсутствует параметр "%s", необходимый для выполнения команды "%s". Командная строка: "%s"';

implementation

uses
  Classes, Types, MemoryUtils, TextBuilder, TextConsts;

var
  ParameterList: TStringList;

function GetParameter(Parameter: TParameter): string;
var
  B: TTextBuilder;
  I: TParameterType;
begin
  B := TTextBuilder.Create;
  try
    for I := Low(TParameterType) to High(TParameterType) do B.Append(Parameter[I], Pipe);
    Result := B.Text;
  finally
    B.Free;
  end;
end;

function GetLine: string;
var
  B: TTextBuilder;
  I: Integer;
begin
  B := TTextBuilder.Create;
  try
    for I := 1 to ParamCount do B.Append(ParamStr(I), Space);
    Result := B.Text;
  finally
    B.Free;
  end;
end;

function Parameter(const Small, Large: string): TParameter;
begin
  Result[ptSmall] := Small;
  Result[ptLarge] := Large;
end;

procedure RegisterParameter(const ParameterArray: array of TParameter);
var
  I: TParameterType;
  J: Integer;
  Parameter: string;
begin
  for I := Low(TParameterType) to High(TParameterType) do for J := Low(ParameterArray) to High(ParameterArray) do
  begin
    Parameter := Trim(ParameterArray[J, I]);
    if Parameter <> '' then ParameterList.Add(Parameter);
  end;
end;

function FindParameter(const Parameter: string): Boolean;
var
  I: Integer;
begin
  Result := ParameterList.Find(Parameter, I);
end;

function CheckParameter(const ParameterArray: array of TParameter; out Parameter: TParameter): Boolean;

  function Same(Index: Integer; Parameter: TParameter): Boolean;
  var
    I: TParameterType;
  begin
    for I := Low(TParameterType) to High(TParameterType) do
    begin
      Result := SameText(ParamStr(Index), Parameter[I]);
      if Result then Exit;
    end;
    Result := False;
  end;

  function Find(Parameter: TParameter): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to ParamCount do
    begin
      Result := Same(I, Parameter);
      if Result then Exit;
    end;
    Result := False;
  end;

var
  I: Integer;
begin
  for I := Low(ParameterArray) to High(ParameterArray) do
  begin
    Result := Find(ParameterArray[I]);
    if not Result then
    begin
      Parameter := ParameterArray[I];
      Exit;
    end;
  end;
  Result := True;
end;

procedure CheckParameter(const ParameterArray: array of TParameter);
var
  Parameter: TParameter;
begin
  if not CheckParameter(ParameterArray, Parameter) then
    raise EParameterError.CreateFmt(ParameterExpectError, [GetParameter(Parameter), GetLine]);
end;

procedure CheckCommand(Command: TParameter; const ParameterArray: array of TParameter);
var
  Parameter: TParameter;
begin
  if not CheckParameter(ParameterArray, Parameter) then
    raise EParameterError.CreateFmt(CommandError, [GetParameter(Parameter), GetParameter(Command), GetLine]);
end;

function GetString(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: string): Boolean;
var
  S: string;
begin
  Result := SameText(Text, Parameter);
  if Result then
  begin
    Inc(Index);
    if Index <= Count then
    begin
      S := Trim(ParamStr(Index));
      if not AnsiStartsText(Prefix, S) then
      begin
        Value := S;
        Inc(Index);
      end;
    end;
  end;
end;

function GetString(const Text: string; Parameter: TParameter; var Index: Integer; Count: Integer; var Value: string): Boolean;
var
  I: TParameterType;
begin
  for I := Low(TParameterType) to High(TParameterType) do
  begin
    Result := GetString(Text, Parameter[I], Index, Count, Value);
    if Result then Exit;
  end;
  Result := False;
end;

function GetInteger(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: Integer): Boolean;
var
  S: string;
begin
  Result := SameText(Text, Parameter);
  if Result then
  begin
    Inc(Index);
    if Index <= Count then
    begin
      S := Trim(ParamStr(Index));
      if not AnsiStartsText(Prefix, S) and TryStrToInt(S, Value) then Inc(Index);
    end;
  end;
end;

function GetInteger(const Text: string; Parameter: TParameter; var Index: Integer; Count: Integer; var Value: Integer): Boolean;
var
  I: TParameterType;
begin
  for I := Low(TParameterType) to High(TParameterType) do
  begin
    Result := GetInteger(Text, Parameter[I], Index, Count, Value);
    if Result then Exit;
  end;
  Result := False;
end;

function GetBoolean(const Text, Parameter: string; var Index: Integer; Count: Integer; var Value: Boolean): Boolean;
begin
  Result := SameText(Text, Parameter);
  if Result then
  begin
    Inc(Index);
    Value := True;
  end;
end;

function GetBoolean(const Text: string; const Parameter: TParameter; var Index: Integer; Count: Integer; var Value: Boolean): Boolean;
var
  I: TParameterType;
begin
  for I := Low(TParameterType) to High(TParameterType) do
  begin
    Result := GetBoolean(Text, Parameter[I], Index, Count, Value);
    if Result then Exit;
  end;
  Result := False;
end;

initialization
  ParameterList := TStringList.Create;
  ParameterList.Duplicates := dupIgnore;
  ParameterList.Sorted := True;

finalization
  ParameterList.Free;

end.
