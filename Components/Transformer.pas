{ *********************************************************************** }
{                                                                         }
{ Transformer                                                             }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Transformer;

{$B-}

interface

uses
  Windows, Classes, Generics.Collections, Parser, ParseTypes;

type
  TTransformer = class
  public
  const
    BindIdent = 'Transformer.Bind';
    SourceIdent = 'Source';
    TargetIdent = 'Target';
  type
    TBind = record
      Source, Target: TString;
    end;
  private
    FParser: TCustomParser;
    FList: TList<TBind>;
    procedure SetParser(const Value: TCustomParser);
  protected
    function Transform(const Restore: Boolean = False): Integer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Open(const FileName: string): Boolean; overload; virtual;
    procedure Save(const FileName: string); overload; virtual;
    function Open(const Stream: TStream): Boolean; overload; virtual;
    procedure Save(const Stream: TStream); overload; virtual;
    property List: TList<TBind> read FList write FList;
    property Parser: TCustomParser read FParser write SetParser;
  end;

implementation

uses
  Math, Notifier, SuperObject, SysUtils;

{ TTransformer }

constructor TTransformer.Create;
begin
  FList := TList<TBind>.Create;
end;

destructor TTransformer.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTransformer.Open(const Stream: TStream): Boolean;
var
  JO, JI: ISuperObject;
  I: Integer;
  Item: TBind;
begin
  Result := Stream.Size > 0;
  if Result then
  begin
    JO := TSuperObject.ParseStream(Stream, False);
    Result := JO.A[BindIdent].Length > 0;
    if Result then
      for I := 0 to JO.A[BindIdent].Length - 1 do
      begin
        FillChar(Item, SizeOf(TFunction), 0);
        JI := JO.A[BindIdent].O[I];
        StrCopy(Item.Source, PChar(JI.S[SourceIdent]));
        StrCopy(Item.Target, PChar(JI.S[TargetIdent]));
        FList.Add(Item);
      end;
  end;
end;

function TTransformer.Open(const FileName: string): Boolean;
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

procedure TTransformer.Save(const Stream: TStream);
var
  JO, JI: ISuperObject;
  Item: TBind;
begin
  JO := SO;
  JO.O[BindIdent] := SA([]);
  for Item in FList do
  begin
    JI := SO;
    JI.S[SourceIdent] := Item.Source;
    JI.S[TargetIdent] := Item.Target;
    JO.A[BindIdent].Add(JI);
  end;
  JO.SaveTo(Stream, True);
end;

procedure TTransformer.Save(const FileName: string);
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

procedure TTransformer.SetParser(const Value: TCustomParser);
begin
  if FParser <> Value then
  begin
    if Assigned(FParser) then Transform(True);
    FParser := Value;
    if Assigned(FParser) then Transform;
  end;
end;

function TTransformer.Transform(const Restore: Boolean): Integer;
var
  Item: TBind;
  Source, Target: string;
  AFunction: PFunction;
begin
  Result := 0;
  if Assigned(FParser) then
  begin
    FParser.BeginUpdate;
    try
      for Item in FList do
      begin
        if Restore then
        begin
          Source := Item.Target;
          Target := Item.Source;
        end
        else begin
          Source := Item.Source;
          Target := Item.Target;
        end;
        AFunction := FParser.FindFunction(Target);
        if not Assigned(AFunction) then
        begin
          AFunction := FParser.FindFunction(Source);
          if Assigned(AFunction) then
          begin
            StrLCopy(AFunction.Name, PChar(Target), IfThen(Length(Target) > SizeOf(TString) - 1, SizeOf(TString) - 1, Length(Target)));
            Inc(Result);
          end;
        end;
      end;
      if Result > 0 then
      begin
        FParser.FData.Prepared := False;
        FParser.Prepare;
      end;
    finally
      FParser.EndUpdate;
    end;
    FParser.Notify(ntCompile, FParser);
  end;
end;

end.
