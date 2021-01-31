{ *********************************************************************** }
{                                                                         }
{ ParseField                                                              }
{                                                                         }
{ Copyright (c) 2010 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseField;

{$B-}
{$I Directives.inc}

interface

uses
  Windows, SysUtils, Classes, Variants, DB, Connector, Calculator, FlexibleList,
  ParseTypes, ValueTypes;

type
  PItem = ^TItem;
  TItem = record
    Field: TField;
    FunctionHandle: Integer;
  end;

  TStringField = class(DB.TStringField)
  private
    FCalculator: TCalculator;
    FFunctionHandle: Integer;
    FPrepared: Boolean;
    FFunctionName: string;
    FList: TFlexibleList;
    FPriority: Integer;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function GetAsString: string; override;
    function Custom(const AFunction: PFunction; const AType: PType;
      out Value: TValue; const LValue, RValue: TValue;
      const ParameterArray: TParameterArray): Boolean; virtual;
    procedure Delete(Index: Integer); virtual;
    property List: TFlexibleList read FList write FList;
    property FunctionHandle: Integer read FFunctionHandle write FFunctionHandle;
    property FunctionName: string read FFunctionName write FFunctionName;
    property Priority: Integer read FPriority write FPriority;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function MakeFunctionName(const DataSet: TDataSet; const Field: TField): string; virtual;
    function Prepare: Boolean; virtual;
    property Calculator: TCalculator read FCalculator write FCalculator;
    property Prepared: Boolean read FPrepared write FPrepared;
  published
    property EditMask;
    property FixedChar;
    property Size;
    property Transliterate;
  end;

implementation

uses
  Notifier, TextConsts, TextUtils, ValueConsts, ValueUtils;

{ TStringField }

procedure TStringField.Clear;
var
  I: Integer;
begin
  if Available(FCalculator) then
  begin
    FCalculator.CheckThread;
    FCalculator.Parser.BeginUpdate;
  end;
  try
    for I := FList.List.Count - 1 downto 0 do Delete(I);
    FList.List.Clear;
  finally
    if Available(FCalculator) then
    begin
      FCalculator.Parser.EndUpdate;
      FCalculator.Parser.Notify(ntCompile, Self);
    end;
  end;
end;

constructor TStringField.Create(AOwner: TComponent);
begin
  inherited;
  FList := TFlexibleList.Create(Self);
  FFunctionName := CreateGuid;
  FCalculator := TCalculator.Create(Self);
  FCalculator.Connector.Add(FFunctionHandle, FFunctionName, FPriority, Custom);
end;

function TStringField.Custom(const AFunction: PFunction; const AType: PType;
  out Value: TValue; const LValue, RValue: TValue;
  const ParameterArray: TParameterArray): Boolean;
var
  I: Integer;
  Item: PItem;
begin
  I := FList.List.IndexOf(AFunction.Name);
  if I < 0 then
    Item := nil
  else
    Item := Pointer(FList.List.Objects[I]);
  Result := Assigned(Item);
  if Result then
    if Item.Field.DataSet.Active then
      Value := TextToValue(Item.Field.AsString)
    else
      Value := EmptyValue;
end;

procedure TStringField.Delete(Index: Integer);
var
  Item: PItem;
begin
  Item := Pointer(FList.List.Objects[Index]);
  if Assigned(Item) then
  begin
    FList.List.Objects[Index] := nil;
    if Available(FCalculator) then
      FCalculator.Parser.DeleteFunction(Item.FunctionHandle);
    Dispose(Item);
  end;
end;

destructor TStringField.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TStringField.GetAsString: string;
begin
  if (DefaultExpression = '') or (csDesigning in ComponentState) then
    Result := inherited GetAsString
  else begin
    if not FPrepared then FPrepared := Prepare;
    if Available(FCalculator) then
    try
      Result := FCalculator.AsString(DefaultExpression);
    except
      Result := ConstraintErrorMessage;
    end;
  end;
end;

function TStringField.MakeFunctionName(const DataSet: TDataSet;
  const Field: TField): string;
begin
  Result := DataSet.Name + Dot + Field.FieldName;
end;

procedure TStringField.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  FPrepared := False;
end;

function TStringField.Prepare: Boolean;
var
  I, J: Integer;
  AOwner: TComponent;
  Item: PItem;
  ADataSet: TDataSet;
  Field: TField;
  S: string;
begin
  inherited;
  Clear;
  Result := Available(FCalculator);
  if Result then
  begin
    FCalculator.CheckThread;
    FCalculator.Parser.BeginUpdate;
    try
      Result := Assigned(DataSet);
      if Result then
        AOwner := DataSet.Owner
      else
        AOwner := nil;
      if Assigned(AOwner) then
      begin
        FList.List.BeginUpdate;
        try
          Item := nil;
          for I := 0 to AOwner.ComponentCount - 1 do
            if AOwner.Components[I] is TDataSet then
            begin
              ADataSet := TDataSet(AOwner.Components[I]);
              if ADataSet.Name <> '' then
                for J := 0 to ADataSet.FieldCount - 1 do
                begin
                  Field := ADataSet.Fields[J];
                  if (Field <> Self) and (Field.Name <> '') then
                  begin
                    if not Assigned(Item) then New(Item);
                    try
                      ZeroMemory(Item, SizeOf(TItem));
                      Item.Field := Field;
                      S := MakeFunctionName(ADataSet, Item.Field);
                      FCalculator.Parser.AddFunction(S, Item.FunctionHandle, fkHandle, MakeFunctionMethod(False, False, 0), False, vtDouble);                                         
                      {$IFDEF VER150}
                        FList.List.AddObject(S, TObject(Item));
                      {$ELSE}
                        FList.List.AddObject(S, PAnsiChar(Item));
                      {$ENDIF}                      
                      Item := nil;
                    except
                      Dispose(Item);
                      raise;
                    end;
                  end;
                end;
              if Assigned(Item) then Dispose(Item);              
            end;
        finally
          FList.List.EndUpdate;
        end;
      end;
    finally
      FCalculator.Parser.EndUpdate;
      FCalculator.Parser.Notify(ntCompile, Self);
    end;
  end;
end;

end.
