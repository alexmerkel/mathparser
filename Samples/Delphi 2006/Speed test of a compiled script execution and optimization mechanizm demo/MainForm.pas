unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Forms, StdCtrls, ComCtrls, ExtCtrls, Controls, Classes, Parser,
  ParseTypes, ValueTypes;

type
  TMain = class(TForm)
    ABevel: TBevel;
    BBevel: TBevel;
    bExecute: TButton;
    edFormula: TEdit;
    edX: TEdit;
    edY: TEdit;
    laCount: TLabel;
    laItems: TLabel;
    laOptimalItems: TLabel;
    laRepeatCount: TLabel;
    laReport: TLabel;
    laText: TLabel;
    laXValue: TLabel;
    laYValue: TLabel;
    meItems: TMemo;
    meOptimalItems: TMemo;
    meReport: TMemo;
    Panel: TPanel;
    rgTypeRetrieveMode: TRadioGroup;
    tbCount: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure tbCountChange(Sender: TObject);
    procedure bExecuteClick(Sender: TObject);
    procedure meItemsChange(Sender: TObject);
    procedure edXChange(Sender: TObject);
    procedure edYChange(Sender: TObject);
  private
    FXVariable: Integer;
    FYVariable: Integer;
    FParser: TParser;
    FRepeatCount: Integer;
  protected
    property XVariable: Integer read FXVariable write FXVariable;
    property YVariable: Integer read FYVariable write FYVariable;
  public
    procedure Status(const Script: TScript; const TickCount: Double = 0; const Detailed: Boolean = False);
    property Parser: TParser read FParser write FParser;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
  end;

const
  Multiplier = 10000;
  ByteSeparator = '-';
  LongwordSeparator = '----';
  CountMessage = 'Repeat count: #,###';
  TextMessage = 'Formula: "%s"';
  OptimalTextMessage = 'Optimal formula: "%s"';
  ScriptMessage = 'Script as an array of bytes:';
  OutputMessage = 'Result: %s';
  RepeatMessage = 'Repeat count: %d';
  TimeMessage = 'Execution time: %d seconds %d milliseconds';
  NumberMessage = '%d) %s';

var
  Main: TMain;

implementation

uses
  ValueUtils;

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  FParser := TMathParser.Create(Self);
  FParser.Cached := False;

  // Add variables "X" and "Y":

  FParser.AddVariable('X', FXVariable);
  FParser.AddVariable('Y', FYVariable);

  edXChange(Sender);
  edYChange(Sender);
  tbCountChange(Sender);
  meItemsChange(Sender);
end;

procedure TMain.edXChange(Sender: TObject);
const
  ErrorValue = 0;
begin
  try
    FParser.StringToScript(edX.Text);
    FXVariable := Convert(FParser.Execute^, vtInteger).Signed32;
  except
    FXVariable := ErrorValue;
  end;
end;

procedure TMain.edYChange(Sender: TObject);
const
  ErrorValue = 0;
begin
  try
    FParser.StringToScript(edY.Text);
    FYVariable := Convert(FParser.Execute^, vtInteger).Signed32;
  except
    FYVariable := ErrorValue;
  end;
end;

procedure TMain.tbCountChange(Sender: TObject);
begin
  FRepeatCount := tbCount.Position * Multiplier;
  laCount.Caption := FormatFloat(CountMessage, FRepeatCount);
end;

procedure TMain.bExecuteClick(Sender: TObject);
var
  Script: TScript;
  I: Integer;
  TickCount: Double;
begin
  Screen.Cursor := crHourGlass;
  try
    try
      FParser.StringToScript(edFormula.Text, Script);
      with meReport.Lines do
      begin;
        Clear;
        Add(Format(TextMessage, [FParser.ScriptToString(Script)]));
        Add('');
      end;
      Status(Script);
      FParser.Optimize(Script);
      with meReport.Lines do
      begin
        Add('');
        Add(Format(OptimalTextMessage, [FParser.ScriptToString(Script)]));
        Add('');
      end;
      TickCount := GetTickCount;
      for I := 1 to FRepeatCount do FParser.Execute(Script);
      TickCount := GetTickCount - TickCount;
      Status(Script, TickCount, True);
    finally
      Script := nil;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.meItemsChange(Sender: TObject);
var
  TypeMode: TRetrieveMode;
  I: Integer;
begin
  meOptimalItems.Lines.Clear;
  TypeMode := TRetrieveMode(rgTypeRetrieveMode.ItemIndex);
  for I := 0 to meItems.Lines.Count - 1 do
  begin
    try
      FParser.StringToScript(meItems.Lines[I]);
      FParser.Optimize;
      meOptimalItems.Lines.Add(Format(NumberMessage, [I + 1, FParser.ScriptToString(TypeMode)]));
    except
      on E: EParserError do meOptimalItems.Lines.Text := E.Message
      else raise;
    end;
  end;
end;

procedure TMain.Status(const Script: TScript; const TickCount: Double; const Detailed: Boolean);
const
  Thousand = 1000;
var
  I: Integer;
  S, Separator: string;
  A: Extended;
begin
  S := '';
  for I := Low(Script) to High(Script) do
  begin
    if I mod SizeOf(Longword) = 0 then
      Separator := LongwordSeparator
    else
      Separator := ByteSeparator;
    if I = Low(Script) then
      S := IntToStr(Script[I])
    else
      S := S + Separator + IntToStr(Script[I]);
  end;
  with meReport.Lines do
  begin
    Add(ScriptMessage);
    Add(S);
    if Detailed then
    begin
      Add('');
      Add(Format(RepeatMessage, [FRepeatCount]));
      Add(Format(OutputMessage, [ValueToText(FParser.Execute(Script)^)]));
      A := TickCount / Thousand;
      Add(Format(TimeMessage, [Trunc(A), Round(Frac(A) * Thousand)]));
    end;
  end;
end;

end.
