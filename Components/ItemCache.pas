{ *********************************************************************** }
{                                                                         }
{ ItemCache                                                               }
{                                                                         }
{ Copyright (c) 2014 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ItemCache;

{$B-}
{$I Directives.inc}

interface

uses
  Classes, Cache, Notifier, ParseTypes;

type
  PCacheData = ^TCacheData;
  TCacheData = record
    ItemArray: TTextItemArray;
  end;

  TItemCache = class(TCache)
  private
    function GetCacheData(const Index: Integer): PCacheData;
  public
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    procedure AssignData(const Index: Integer; const Value: TCacheData); virtual;
    procedure Add(const Text: string; const ItemArray: TTextItemArray); virtual;
    procedure Clear; override;
    function Find(const Text: string; out ItemArray: TTextItemArray): Boolean; virtual;
    property Data[const Index: Integer]: PCacheData read GetCacheData;
  end;

function MakeData(const ItemArray: TTextItemArray): TCacheData;

implementation

uses
  Parser, ValueUtils;

function MakeData(const ItemArray: TTextItemArray): TCacheData;
begin
  Result.ItemArray := Copy(ItemArray);
end;

{ TItemCache }

procedure TItemCache.Add(const Text: string; const ItemArray: TTextItemArray);
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
        Data^ := MakeData(ItemArray);
        List.List.AddObject(Text, TObject(Data))
      end
      else begin
        I := Next;
        AssignData(I, MakeData(ItemArray));
        List.List[I] := Text;
      end;
    finally
      List.List.EndUpdate;
    end;
  end;
end;

procedure TItemCache.AssignData(const Index: Integer; const Value: TCacheData);
begin
  PCacheData(List.List.Objects[Index])^ := Value;
end;

procedure TItemCache.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.List.Count - 1 do if Assigned(List.List.Objects[I]) then
  begin
    Data[I].ItemArray := nil;
    Dispose(PCacheData(List.List.Objects[I]));
  end;
  List.List.Clear;
end;

function TItemCache.Find(const Text: string; out ItemArray: TTextItemArray): Boolean;
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
        ItemArray := Copy(AData.ItemArray)
      else
        ItemArray := nil;
      if SmartCache then MatchCount := MakeValue(MatchCount.Signed32 + 1);
    end
    else ItemArray := nil;
    Setup;
  end
  else ItemArray := nil;
end;

function TItemCache.GetCacheData(const Index: Integer): PCacheData;
begin
  Result := PCacheData(List.List.Objects[Index]);
end;

procedure TItemCache.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
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
