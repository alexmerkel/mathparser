{ *********************************************************************** }
{                                                                         }
{ EventUtils                                                              }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit EventUtils;

{$B-}

interface

uses SysUtils, Types, Math, Parser, ParseTypes;

type
  TEvent = record
    Index: PInteger;
    Name: TString;
    Priority: Integer;
    Event: TFunctionEvent;
  end;
  TEvents = array of TEvent;

  TEventData = record
    Events: TEvents;
    Outdated: Boolean;
  end;

function Add(var Data: TEventData; AEvent: TEvent): Integer;
function EventCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
procedure EventExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
procedure SortEvents(var Data: TEventData);
function MakeEvent(var AIndex: Integer; const AName: string; APriority: Integer;
  AEvent: TFunctionEvent): TEvent;

implementation

uses MemoryUtils;

function Add(var Data: TEventData; AEvent: TEvent): Integer;
begin
  Result := Length(Data.Events);
  SetLength(Data.Events, Result + 1);
  Data.Events[Result] := AEvent;
  Data.Outdated := True;
end;

function EventCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  Events: TEvents absolute Target;
begin
  Result := CompareValue(Events[BIndex].Priority, Events[AIndex].Priority);
end;

procedure EventExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  Event: TEvent;
  Events: TEvents absolute Target;
begin
  Event := Events[AIndex];
  Events[AIndex] := Events[BIndex];
  Events[AIndex].Index^ := AIndex;
  Events[BIndex] := Event;
  Events[BIndex].Index^ := BIndex;
end;

procedure SortEvents(var Data: TEventData);
begin
  if Data.Outdated then
  begin
    QSort(Data.Events, Low(Data.Events), High(Data.Events), EventCompare, EventExchange);
    Data.Outdated := True;
  end;
end;

function MakeEvent(var AIndex: Integer; const AName: string; APriority: Integer;
  AEvent: TFunctionEvent): TEvent;
begin
  with Result do
  begin
    Index := @AIndex;
    Event := AEvent;
    StrLCopy(Name, PChar(AName), SizeOf(TString) - 1);
    Priority := APriority;
  end;
end;

end.
