{ *********************************************************************** }
{                                                                         }
{ ParseValueList                                                          }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ParseValueList;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Classes, Connector,
  FlexibleList, Notifier, ParseCommon, ParseConsts, Parser, ParseTypes, TextConsts, Types,
  ValueTypes;

const
  DefaultOptimization = True;
  DefaultRaiseError = True;

type
  EParseValueListError = class(Exception);

  PItem = ^TItem;
  TItem = record
    Item: TCommonItem;
    FHandle: Integer;
  end;

  TParseValueList = class(TPlugin)
  private
    FOptimization: Boolean;
    FRaiseError: Boolean;
    FFunctionHandle: Integer;
    FPriority: Integer;
    FFunctionName: string;
    FFlexibleList: TFlexibleList;
    FErrorValue: TValue;
    function GetConnector: TCustomConnector;
    function GetList: TStrings;
    function GetListType: TListType;
    function GetParser: TParser;
    procedure SetConnector(const Value: TCustomConnector);
    procedure SetFlexibleList(const Value: TFlexibleList);
    procedure SetList(const Value: TStrings);
    procedure SetListType(const Value: TListType);
    procedure SetParser(const Value: TParser);
  protected
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure Connect; override;
    procedure Suspend; virtual;
    procedure Disconnect; override;
    procedure SetName(const NewName: TComponentName); override;
    function Error(const Message: string): Exception; overload; virtual;
    function Error(const Message: string; const Arguments: array of const): Exception; overload; virtual;
    function Compile(const Index: Integer): Boolean; overload; virtual;
    procedure Compile; overload; virtual;
    procedure Delete(const Index: Integer); virtual;
    function Recurse(const FunctionHandle: Integer; const Script: TScript): Boolean; virtual;
    function GetFunction(const Item: PItem): PFunction; overload; virtual;
    function GetFunction(const Item: PItem; out AFunction: PFunction): Boolean; overload; virtual;
    function Custom(const AFunction: PFunction; const AType: PType;
      out Value: TValue; const LValue, RValue: TValue;
      const ParameterArray: TParameterArray): Boolean; virtual;
    property FunctionHandle: Integer read FFunctionHandle write FFunctionHandle;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const AListType: TListType); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); override;
    function AssignValue(const AName, Value: string; out Index: Integer): Boolean; overload; virtual;
    function AssignValue(const AName, Value: string): Integer; overload; virtual;
    procedure Clear; virtual;
    function Find(const AName: string; ItemName: PString = nil): PItem; virtual;
    function IndexOf(const AName: string; ItemName: PString = nil): Integer; virtual;
    function Attach: Boolean; virtual;
    procedure Detach; virtual;
    property Parser: TParser read GetParser write SetParser;
  published
    property Connector: TCustomConnector read GetConnector write SetConnector;
    property FlexibleList: TFlexibleList read FFlexibleList write SetFlexibleList;
    property List: TStrings read GetList write SetList;
    property ListType: TListType read GetListType write SetListType stored False;
    property ErrorValue: TValue read FErrorValue write FErrorValue;
    property Optimization: Boolean read FOptimization write FOptimization default DefaultOptimization;
    property RaiseError: Boolean read FRaiseError write FRaiseError default DefaultRaiseError;
    property FunctionName: string read FFunctionName write FFunctionName;
    property Priority: Integer read FPriority write FPriority;
  end;

const
  InternalFlexibleListName = 'FlexibleList';
  RecurseError = 'Function "%s" recursively uses itself';

procedure Register;

implementation

uses
  EventUtils, MemoryUtils, ParseUtils, StrUtils, TextUtils, ValueConsts, ValueUtils;

procedure Register;
begin
  RegisterComponents('Samples', [TParseValueList]);
end;

{ TParseValueList }

function TParseValueList.AssignValue(const AName, Value: string; out Index: Integer): Boolean;
var
  S, ItemName: string;
  Item: PItem;
begin
  Result := FindParser;
  if Result then
  begin
    List.BeginUpdate;
    try
      if Trim(Value) = '' then S := ValueToText(EmptyValue)
      else S := Value;
      Index := ParseCommon.IndexOf(List, AName, True, @ItemName);
      if Index < 0 then
      begin
        Result := not Assigned(Parser.FindFunction(ItemName));
        if Result then
        begin
          Parser.BeginUpdate;
          try
            New(Item);
            try
              ZeroMemory(Item, SizeOf(TItem));
              if TryTextToValue(S, Item.Item.Value) then
              begin
                Result := Parser.AddVariable(ItemName, Item.Item.Value, not AnsiStartsText(Lock, AName));
                Item.FHandle := -1;
              end
              else Result := Parser.AddFunction(ItemName, Item.FHandle, fkHandle, MakeFunctionMethod(False, False, 0), not AnsiStartsText(Lock, AName));
              if Result then
              begin
                Index := List.Add(ItemName + {$IFDEF DELPHI_7}List.NameValueSeparator{$ELSE}Equal{$ENDIF} + S);
                List.Objects[Index] := Pointer(Item);
              end
              else Dispose(Item);
            except
              Dispose(Item);
              raise;
            end;
          finally
            Parser.EndUpdate;
          end;
          if Result then Compile(Index);
        end;
      end
      else begin
        {$IFDEF DELPHI_7}
        List.ValueFromIndex[Index] := S;
        {$ELSE}
        SetValueFromIndex(List, Index, S);
        {$ENDIF}
        Result := Compile(Index);
      end;
    finally
      List.EndUpdate;
    end;
  end
  else Index := -1;
end;

function TParseValueList.AssignValue(const AName, Value: string): Integer;
begin
  if not AssignValue(AName, Value, Result) then Result := -1;  
end;

function TParseValueList.Attach: Boolean;
var
  Item: PItem;
  I: Integer;
  S: string;
begin
  Result := FindParser;
  if Result then
  begin
    Parser.BeginUpdate;
    try
      List.BeginUpdate;
      try
        Item := nil;
        for I := 0 to List.Count - 1 do
        begin
          if not Assigned(Item) then
          begin
            New(Item);
            ZeroMemory(Item, SizeOf(TItem));
          end;
          S := Trim(List.Names[I]);
          if Parser.AddFunction(S, Item.FHandle, fkHandle, MakeFunctionMethod(False, False, 0), not TrimText(S, Lock), vtDouble) then
          begin
            List.Objects[I] := Pointer(Item);
            Item := nil;
          end;
          List[I] := S + {$IFDEF DELPHI_7}List.NameValueSeparator{$ELSE}Equal{$ENDIF} + Trim({$IFDEF DELPHI_7}List.ValueFromIndex[I]{$ELSE}GetValueFromIndex(List, I){$ENDIF});
        end;
      finally
        List.EndUpdate;
      end;
      if Assigned(Item) then Dispose(Item);
    finally
      Parser.EndUpdate;
    end;
    Compile;
  end;
end;

procedure TParseValueList.Clear;
begin
  Detach;
  List.Clear;
end;

procedure TParseValueList.Compile;
var
  I: Integer;
begin
  if FindParser then
  begin
    List.BeginUpdate;
    try
      for I := 0 to List.Count - 1 do Compile(I);
    finally
      List.EndUpdate;
    end;
  end;
end;

function TParseValueList.Compile(const Index: Integer): Boolean;
var
  Item: PItem;
  AFunction: PFunction;
  S: string;
begin
  Result := FindParser;
  if Result then
  begin
    Item := Pointer(List.Objects[Index]);
    try
      Result := GetFunction(Item, AFunction);
      if Result then
      begin
        Item.Item.Script := nil;
        S := {$IFDEF DELPHI_7}List.ValueFromIndex[Index]{$ELSE}GetValueFromIndex(List, Index){$ENDIF};
        if TryTextToValue(S, Item.Item.Value) then MakeVariable(AFunction, @Item.Item.Value)
        else begin
          Parser.StringToScript(S, Item.Item.Script);
          Result := GetFunction(Item, AFunction);
          if Result then
          begin
            if not Helper.Optimizable(Item.Item.Script, Parser.FData) then
              AFunction.Optimizable := False;
            if Recurse(AFunction.Handle^, Item.Item.Script) then
              if FRaiseError then
                raise Error(RecurseError, [List.Names[Index]])
              else
                ParseCommon.AssignValue(Item.Item, FErrorValue)
            else begin
              if FOptimization then Parser.Optimize(Item.Item.Script);
              if Optimal(Item.Item.Script, stScript) then
              begin
                ParseCommon.AssignValue(Item.Item, Parser.Execute(Item.Item.Script)^);
                MakeVariable(AFunction, @Item.Item.Value);
              end
              else begin
                MakeFunction(AFunction, @Item.FHandle);
                Item.Item.ItemType := itScript;
              end;
            end;
          end;
        end;
      end;
    except
      ParseCommon.AssignValue(Item.Item, FErrorValue);
      {$IFDEF DELPHI_7}
      List.ValueFromIndex[Index] := ValueToText(FErrorValue);
      {$ELSE}
      SetValueFromIndex(List, Index, ValueToText(FErrorValue));
      {$ENDIF}
      if FRaiseError then raise;
    end;
  end;
end;

procedure TParseValueList.Connect;
begin
  inherited;
  if Assigned(Connector) then Connector.Add(FFunctionHandle, FFunctionName, FPriority, Custom);
  Attach;
end;

constructor TParseValueList.Create(AOwner: TComponent);
begin
  inherited;
  FFlexibleList := TFlexibleList.Create(Self);
  with FFlexibleList do
  begin
    Name := InternalFlexibleListName;
    SetSubComponent(True);
  end;
  AssignDouble(FErrorValue, 0);
  FOptimization := DefaultOptimization;
  FRaiseError := DefaultRaiseError;
  if Name = '' then FFunctionName := CreateGuid;
end;

constructor TParseValueList.Create(const AOwner: TComponent; const AListType: TListType);
begin
  Create(AOwner);
  ListType := AListType;
end;

function TParseValueList.Custom(const AFunction: PFunction; const AType: PType;
  out Value: TValue; const LValue, RValue: TValue;
  const ParameterArray: TParameterArray): Boolean;
var
  I: Integer;
  Item: PItem;
begin
  if FindParser then
  begin
    I := List.IndexOfName(AFunction.Name);
    if I < 0 then
      Item := nil
    else
      Item := Pointer(List.Objects[I]);
    Result := Assigned(Item);
    if Result then
      case Item.Item.ItemType of
        itNumber: Value := Item.Item.Value;
        itScript: Value := Parser.Execute(Item.Item.Script)^;
      else Value := EmptyValue;
      end;
  end
  else Result := False;
end;

procedure TParseValueList.Delete(const Index: Integer);
var
  Item: PItem;
begin
  Item := Pointer(List.Objects[Index]);
  if Assigned(Item) then
  begin
    List.Objects[Index] := nil;
    if FindParser then
      if Item.FHandle < 0 then
        Parser.DeleteVariable(Item.Item.Value)
      else
        Parser.DeleteFunction(Item.FHandle);
    Dispose(Item);
  end;
end;

destructor TParseValueList.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TParseValueList.Detach;
var
  I: Integer;
begin
  if FindParser then Parser.BeginUpdate;
  try
    List.BeginUpdate;
    try
      for I := List.Count - 1 downto 0 do Delete(I);
    finally
      List.EndUpdate;
    end;
  finally
    if FindParser then
    begin
      Parser.EndUpdate;
      Parser.Notify(ntCompile, Self);
    end;
  end;
end;

procedure TParseValueList.Disconnect;
begin
  Suspend;
  inherited;
end;

function TParseValueList.Error(const Message: string): Exception;
begin
  Result := Error(Message, []);
end;

function TParseValueList.Error(const Message: string;
  const Arguments: array of const): Exception;
begin
  Result := EParseValueListError.CreateFmt(Message, Arguments);
end;

function TParseValueList.Find(const AName: string; ItemName: PString): PItem;
var
  I: Integer;
begin
  I := IndexOf(AName, ItemName);
  if I < 0 then
    Result := nil
  else
    Result := Pointer(List.Objects[I]);
end;

function TParseValueList.GetConnector: TCustomConnector;
begin
  Result := TCustomConnector(inherited Connector);
end;

function TParseValueList.GetFunction(const Item: PItem): PFunction;
begin
  if not GetFunction(Item, Result) then Result := nil;
end;

function TParseValueList.GetFunction(const Item: PItem; out AFunction: PFunction): Boolean;
begin
  if Assigned(Item) then
    if Item.FHandle < 0 then
      AFunction := Parser.GetFunction(@Item.Item.Value)
    else
      AFunction := Parser.GetFunction(Item.FHandle)
  else
    AFunction := nil;
  Result := Assigned(AFunction);
end;

function TParseValueList.GetList: TStrings;
begin
  Result := FFlexibleList.List;
end;

function TParseValueList.GetListType: TListType;
begin
  Result := FFlexibleList.ListType;
end;

function TParseValueList.GetParser: TParser;
begin
  Result := TParser(inherited Parser);
end;

function TParseValueList.IndexOf(const AName: string; ItemName: PString): Integer;
begin
  Result := ParseCommon.IndexOf(List, AName, True, ItemName);
end;

procedure TParseValueList.Loaded;
begin
  inherited;
  if List.Count > 0 then Attach;
end;

procedure TParseValueList.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Component = Connector) and (Operation = opRemove) then
  begin
    Disconnect;
    Connector := nil;
  end;
end;

procedure TParseValueList.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  inherited;
  case NotifyType of
    ntConnect: Connect;
    ntSuspend: Suspend;
    ntDisconnect: Disconnect;
    ntCompile: Compile;
  end;
end;

function TParseValueList.Recurse(const FunctionHandle: Integer; const Script: TScript): Boolean;
var
  AFunction: PFunction;
  FunctionArray: TIntegerDynArray;
  I: Integer;
begin
  Result := FindParser and Parser.GetFunction(FunctionHandle, AFunction) and (not AFunction.Optimizable or not FOptimization);
  if Result then
  begin
    Helper.GetFunctionArray(Script, FunctionArray);
    try
      for I := Low(FunctionArray) to High(FunctionArray) do
        if FunctionHandle = FunctionArray[I] then
        begin
          Result := True;
          Exit;
        end;
      Result := False;
    finally
      FunctionArray := nil;
    end;
  end;
end;

procedure TParseValueList.SetConnector(const Value: TCustomConnector);
begin
  inherited Connector := Value;
end;

procedure TParseValueList.SetFlexibleList(const Value: TFlexibleList);
begin
  Detach;
  FFlexibleList.Assign(Value);
  Attach;
end;

procedure TParseValueList.SetList(const Value: TStrings);
begin
  FFlexibleList.List := Value;
end;

procedure TParseValueList.SetListType(const Value: TListType);
begin
  FFlexibleList.ListType := Value;
end;

procedure TParseValueList.SetName(const NewName: TComponentName);
begin
  if SysUtils.SameText(Name, FFunctionName) then FFunctionName := NewName;
  inherited;
end;

procedure TParseValueList.SetParser(const Value: TParser);
begin
  inherited Parser := Value;
end;

procedure TParseValueList.Suspend;
begin
  Detach;
  if Available(Connector) then Connector.Delete(FFunctionHandle);
  Parser := nil;
end;

end.
