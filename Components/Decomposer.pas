{ *********************************************************************** }
{                                                                         }
{ Decomposer                                                              }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Decomposer;

interface

uses
  Windows, Classes, SysUtils, Generics.Collections, Calculator, Numeration,
  ParseConsts, ParseTypes, ParseUtils, TextConsts, ValueTypes;

type
  EDecomposerError = class(Exception);

  TDecomposer = class
  public
  const
    DefaultNumeration = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
    DefaultMaxValue = High(Shortint);
    DefaultMinValue = - High(Shortint) - 1;
    DefaultFunctionCount = 1;
    DefaultVariableCount = 10;
    DefaultVariableItemCount = 2;
    VariablePrefix = '$';
    FunctionIdent = 'Decomposer.Function';
    FunctionNameIdent = 'Name';
    FunctionLParameterIdent = 'LParameter';
    FunctionRParameterIdent = 'RParameter';
    FunctionCountIdent = 'Count';
    VariableIdent = 'Decomposer.Variable';
    VariableNameIdent = 'Name';
    VariableTextIdent = 'Text';
  type
    TFunction = record
      Name: TString;
      LParameter, RParameter: Boolean;
      Count: Integer;
    end;
    TVariable = record
      Name, Text: TString;
    end;
  private
    FMinValue: Integer;
    FFunctionCount: Integer;
    FCalculator: TCalculator;
    FVariableCount: Integer;
    FNumerationIndex: Integer;
    FFunctionList: TList<TFunction>;
    FMaxValue: Integer;
    FNumeration: TNumeration;
    FVariableList: TList<TVariable>;
    FVariableItemCount: Integer;
  protected
    function MakeVariableName: string; virtual;
    function InternalDecompose(const Count: Integer): string; virtual;
    property Numeration: TNumeration read FNumeration write FNumeration;
    property NumerationIndex: Integer read FNumerationIndex write FNumerationIndex;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Open(const FileName: string): Boolean; overload; virtual;
    procedure Save(const FileName: string); overload; virtual;
    function Open(const Stream: TStream): Boolean; overload; virtual;
    procedure Save(const Stream: TStream); overload; virtual;
    function Decompose(const Value: TValue): string; virtual;
    procedure MakeVariableList; virtual;
    procedure CopyVariableList; virtual;
    property Calculator: TCalculator read FCalculator write FCalculator;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property FunctionList: TList<TFunction> read FFunctionList write FFunctionList;
    property VariableList: TList<TVariable> read FVariableList write FVariableList;
    property FunctionCount: Integer read FFunctionCount write FFunctionCount;
    property VariableCount: Integer read FVariableCount write FVariableCount;
    property VariableItemCount: Integer read FVariableItemCount write FVariableItemCount;
  end;

const
  FunctionExpectError = 'The functions are not defined';
  FunctionNumberError = 'The number of functions should be at least one';

implementation

uses
  Math, MemoryUtils, NumberUtils, SuperObject, TextBuilder, TextUtils, Types,
  ValueUtils;

{ TDecomposer }

procedure TDecomposer.CopyVariableList;
var
  AItem: TVariable;
  BItem: TFunction;
begin
  for AItem in FVariableList do
  begin
    FillChar(BItem, SizeOf(TFunction), 0);
    BItem.Name := AItem.Name;
    FFunctionList.Add(BItem);
  end;
end;

constructor TDecomposer.Create;
begin
  FCalculator := TCalculator.Create(nil);
  FNumeration := TNumeration.Create(nil);
  FNumerationIndex := FNumeration.Add(DefaultNumeration);
  FMaxValue := DefaultMaxValue;
  FMinValue := DefaultMinValue;
  FFunctionList := TList<TFunction>.Create;
  FVariableList := TList<TVariable>.Create;
  FFunctionCount := DefaultFunctionCount;
  FVariableCount := DefaultVariableCount;
  FVariableItemCount := DefaultVariableItemCount;
end;

function TDecomposer.Decompose(const Value: TValue): string;
var
  Builder: TTextBuilder;
  Epsilon: TValue;
begin
  Builder := TTextBuilder.Create;
  try
    Builder.Append(InternalDecompose(FFunctionCount));
    Epsilon := Operation(Value, FCalculator.AsValue(Builder.Text), otSubtract);
    if LessZero(Epsilon) then
      Builder.Append(Space + TOperatorArray[toNegative] + Space + ValueToText(Positive(Epsilon)))
    else
      Builder.Append(Space + TOperatorArray[toPositive] + Space + ValueToText(Epsilon));
    Result := Builder.Text;
  finally
    Builder.Free;
  end;
end;

destructor TDecomposer.Destroy;
begin
  FCalculator.Free;
  FNumeration.Free;
  FFunctionList.Free;
  FVariableList.Free;
  inherited;
end;

function TDecomposer.InternalDecompose(const Count: Integer): string;

  function Embrace(const Value: Integer): string;
  begin
    if Value < 0 then
      Result := ParseUtils.Embrace(IntToStr(Value), BracketArray[btParenthesis])
    else
      Result := IntToStr(Value);
  end;

  function RandomValue: Integer;
  begin
    Result := FMinValue + Random(FMaxValue - FMinValue + 1);
  end;

var
  Builder: TTextBuilder;
  Index: TIntegerDynArray;
  I, J, K, L: Integer;
  Item: TFunction;
begin
  if FFunctionList.Count = 0 then raise EDecomposerError.Create(FunctionExpectError);
  if Count > 0 then
  begin
    Builder := TTextBuilder.Create;
    try
      SetLength(Index, FFunctionList.Count);
      try
        SetRandom(Index);
        L := 0;
        for I := 0 to Count - 1 do
        begin
          Item := FFunctionList[Index[L]];
          Inc(L);
          if L >= FFunctionList.Count then
          begin
            SetRandom(Index);
            L := 0;
          end;
          if Item.Count > 0 then
          begin
            Builder.Append(string(Item.Name) + Space + LeftParenthesis);
            for J := 0 to Item.Count - 1 do
            begin
              K := RandomValue;
              if J > 0 then
                Builder.Append(Space + Embrace(K))
              else
                Builder.Append(Embrace(K));
            end;
            Builder.Append(RightParenthesis);
          end
          else begin
            if Item.LParameter then Builder.Append(Embrace(RandomValue) + Space);
            Builder.Append(Item.Name);
            if Item.RParameter then Builder.Append(Space + Embrace(RandomValue));
          end;
          if I < Count - 1 then
          begin
            J := Random(Length(TOperatorArray));
            Builder.Append(Space + TOperatorArray[TTextOperator(J)] + Space);
          end;
        end;
      finally
        Index := nil;
      end;
      Result := Builder.Text;
    finally
      Builder.Free
    end;
  end
  else raise EDecomposerError.Create(FunctionNumberError);
end;

procedure TDecomposer.MakeVariableList;
var
  I: Integer;
  AName, Value: string;
  Item: TVariable;
begin
  for I := 0 to FVariableCount - 1 do
  begin
    AName := MakeVariableName;
    Value := InternalDecompose(FVariableItemCount);
    TextCopy(Item.Name, SizeOf(TString) div SizeOf(Char), AName);
    TextCopy(Item.Text, SizeOf(TString) div SizeOf(Char), Value);
    FVariableList.Add(Item);
    FCalculator.ItemValue[Item.Name] := Item.Text;
  end;
end;

function TDecomposer.MakeVariableName: string;
begin
  Result := VariablePrefix + FNumeration.ConvertTo(NumberUtils.GetHashCode(CreateGuid), FNumerationIndex);
end;

function TDecomposer.Open(const Stream: TStream): Boolean;
var
  JO, JI: ISuperObject;
  I: Integer;
  AItem: TFunction;
  BItem: TVariable;
begin
  Result := Stream.Size > 0;
  if Result then
  begin
    JO := TSuperObject.ParseStream(Stream, False);
    Result := Assigned(JO.A[FunctionIdent]) and (JO.A[FunctionIdent].Length > 0);
    if Result then
    begin
      for I := 0 to JO.A[FunctionIdent].Length - 1 do
      begin
        FillChar(AItem, SizeOf(TFunction), 0);
        JI := JO.A[FunctionIdent].O[I];
        StrCopy(AItem.Name, PChar(JI.S[FunctionNameIdent]));
        AItem.LParameter := JI.B[FunctionLParameterIdent];
        AItem.RParameter := JI.B[FunctionRParameterIdent];
        AItem.Count := JI.I[FunctionCountIdent];
        FFunctionList.Add(AItem);
      end;
      if Assigned(JO.A[VariableIdent]) and (JO.A[VariableIdent].Length > 0) then
      begin
        for I := 0 to JO.A[VariableIdent].Length - 1 do
        begin
          FillChar(BItem, SizeOf(TVariable), 0);
          JI := JO.A[VariableIdent].O[I];
          StrCopy(BItem.Name, PChar(JI.S[VariableNameIdent]));
          StrCopy(BItem.Text, PChar(JI.S[VariableTextIdent]));
          FVariableList.Add(BItem);
          FCalculator.ItemValue[BItem.Name] := BItem.Text;
        end;
        //CopyVariableList;
      end;
    end;
  end;
end;

function TDecomposer.Open(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := Open(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDecomposer.Save(const Stream: TStream);
var
  JO, JI: ISuperObject;
  AItem: TFunction;
  BItem: TVariable;
begin
  JO := SO;
  JO.O[FunctionIdent] := SA([]);
  for AItem in FFunctionList do
  begin
    JI := SO;
    JI.S[FunctionNameIdent] := AItem.Name;
    JI.B[FunctionLParameterIdent] := AItem.LParameter;
    JI.B[FunctionRParameterIdent] := AItem.RParameter;
    JI.I[FunctionCountIdent] := AItem.Count;
    JO.A[FunctionIdent].Add(JI);
  end;
  JO.O[VariableIdent] := SA([]);
  for BItem in FVariableList do
  begin
    JI := SO;
    JI.S[VariableNameIdent] := BItem.Name;
    JI.S[VariableTextIdent] := BItem.Text;
    JO.A[VariableIdent].Add(JI);
  end;
  JO.SaveTo(Stream, True);
end;

procedure TDecomposer.Save(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Save(Stream);
  finally
    Stream.Free;
  end;
end;

initialization
  Randomize;

end.
