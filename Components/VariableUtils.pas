{ *********************************************************************** }
{                                                                         }
{ VariableUtils                                                           }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit VariableUtils;

{$B-}

interface

uses
  ValueTypes;

type
  TVariable = record
    Name: string;
    Variable: PValue;
  end;
  TVariableArray = array of TVariable;

const
  VariablePrefix = '$';

function Add(var Target: TVariableArray; const AName: string; const AVariable: PValue): Integer;
function UniqueVariableName: string;
function Delete(var Target: TVariableArray; const Index: Integer): Boolean;

implementation

uses
  TextUtils;

function Add(var Target: TVariableArray; const AName: string; const AVariable: PValue): Integer;
begin
  Result := Length(Target);
  SetLength(Target, Result + 1);
  with Target[Result] do
  begin
    Name := AName;
    Variable := AVariable;
  end;
end;

function UniqueVariableName: string;
begin
  Result := UniqueName;
  if Result <> '' then Result := VariablePrefix + Result;
end;

function Delete(var Target: TVariableArray; const Index: Integer): Boolean;
var
  I, J: Integer;
  ATarget: TVariableArray;
begin
  I := Length(Target);
  Result := (Index >= 0) and (Index < I);
  if Result then
  begin
    SetLength(ATarget, I - 1);
    J := 0;
    for I := Low(Target) to High(Target) do
      if I = Index then Inc(J)
      else ATarget[I - J] := Target[I];
    Target := ATarget;
  end;
end;

end.
