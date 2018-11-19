{ *********************************************************************** }
{                                                                         }
{ FlagCache                                                               }
{                                                                         }
{ Copyright (c) 2014 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit FlagCache;

{$B-}
{$I Directives.inc}

interface

uses
  Classes, Cache, Notifier, ParseTypes;

type
  PCacheData = ^TCacheData;
  TCacheData = record
    FlagArray: TFunctionFlagArray;
    CleanText: string;
    TypeArray: THandleArray;
  end;

  TFlagCache = class(TCache)
  private
    function GetCacheData(const Index: Integer): PCacheData;
  public
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    procedure AssignData(const Index: Integer; const Value: TCacheData); virtual;
    procedure Add(const Text: string; const FlagArray: TFunctionFlagArray; const CleanText: PString; const TypeArray: PHandleArray); virtual;
    procedure Clear; override;
    function Find(const Text: string; out FlagArray: TFunctionFlagArray; const CleanText: PString; const TypeArray: PHandleArray): Boolean; virtual;
    property Data[const Index: Integer]: PCacheData read GetCacheData;
  end;

function MakeData(const AFlagArray: TFunctionFlagArray; const ACleanText: PString; const ATypeArray: PHandleArray): TCacheData;

implementation

uses
  Parser, ValueUtils;

function MakeData(const AFlagArray: TFunctionFlagArray; const ACleanText: PString; const ATypeArray: PHandleArray): TCacheData;
begin
  FillChar(Result, SizeOf(TCacheData), 0);
  with Result do
  begin
    FlagArray := Copy(AFlagArray);
    if Assigned(ACleanText) then
      CleanText := ACleanText^
    else
      CleanText := '';
    if Assigned(ATypeArray) then
      TypeArray := Copy(ATypeArray^)
    else
      TypeArray := nil;
  end;
end;

{ TFlagCache }

procedure TFlagCache.Add(const Text: string; const FlagArray: TFunctionFlagArray; const CleanText: PString; const TypeArray: PHandleArray);
var
  Data: PCacheData;
  I: Integer;
begin
  if FindParser and (TParser(Parser).UpdateCount = 0) and Enabled and (List.List.IndexOf(Text) < 0) then
  begin
    List.List.BeginUpdate;
    try
      New(Data);
      if List.List.Count < CountToCache then
      begin
        Data^ := MakeData(FlagArray, CleanText, TypeArray);
        List.List.AddObject(Text, TObject(Data));
      end
      else begin
        I := Next;
        AssignData(I, MakeData(FlagArray, CleanText, TypeArray));
        List.List[I] := Text;
      end;
    finally
      List.List.EndUpdate;
    end;
  end;
end;

procedure TFlagCache.AssignData(const Index: Integer; const Value: TCacheData);
begin
  PCacheData(List.List.Objects[Index])^ := Value;
end;

procedure TFlagCache.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.List.Count - 1 do if Assigned(List.List.Objects[I]) then
  begin
    Data[I].FlagArray := nil;
    Data[I].TypeArray := nil;
    Dispose(PCacheData(List.List.Objects[I]));
  end;
  List.List.Clear;
end;

function TFlagCache.Find(const Text: string; out FlagArray: TFunctionFlagArray; const CleanText: PString; const TypeArray: PHandleArray): Boolean;

  procedure Reset;
  begin
    FlagArray := nil;
    if Assigned(CleanText) then CleanText^ := '';
    if Assigned(TypeArray) then TypeArray^ := nil;
  end;

var
  I: Integer;
  AData: PCacheData;
begin
  Result := Enabled;
  if Result then
  begin
    I := List.List.IndexOf(Text);
    Result := I >= 0;
    if Result then
    begin
      AData := Data[I];
      if Assigned(AData) then
      begin
        FlagArray := Copy(AData.FlagArray);
        if Assigned(CleanText) then CleanText^ := AData.CleanText;
        if Assigned(TypeArray) then TypeArray^ := Copy(AData.TypeArray);
      end
      else Reset;
      if SmartCache then MatchCount := MakeValue(MatchCount.Signed32 + 1);
    end
    else Reset;
    Setup;
  end
  else Reset;
end;

function TFlagCache.GetCacheData(const Index: Integer): PCacheData;
begin
  Result := PCacheData(List.List.Objects[Index]);
end;

procedure TFlagCache.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  case NotifyType of
    ntBFD, ntBTD:
      begin
        CapacityScript := nil;
        RestrictScript := nil;
        Clear;
      end;
  end;
end;

end.
