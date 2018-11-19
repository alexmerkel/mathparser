{ *********************************************************************** }
{                                                                         }
{ ValueErrors                                                             }
{                                                                         }
{ Copyright (c) 2008 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ValueErrors;

{$B-}

interface

uses
  SysUtils;

type
  EValueError = class(Exception);

const
  UnknownTypeError = 'Unknown type error';

function Error(const Message: string): Exception; overload;
function Error(const Message: string; const Arguments: array of const): Exception; overload;

implementation

function Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function Error(const Message: string; const Arguments: array of const): Exception;
begin
  Result := EValueError.CreateFmt(Message, Arguments);
end;

end.
