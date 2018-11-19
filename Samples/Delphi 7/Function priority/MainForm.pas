unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  CheckLst, Parser;

type
  TMain = class(TForm)
    cbCoverage: TCheckListBox;
    cbPrioritize: TCheckBox;
    cbPriority: TCheckListBox;
    edDecompiledFormula: TEdit;
    edFormula: TEdit;
    gbFunction: TGroupBox;
    laError: TLabel;
    tcFunction: TTabControl;
    TPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure edFormulaChange(Sender: TObject);
    procedure cbPrioritizeClick(Sender: TObject);
    procedure tcFunctionChange(Sender: TObject);
    procedure ClickCheck(Sender: TObject);
  private
    FParser: TParser;
  public
    procedure Compile;
    procedure OpenFunction;
    procedure SaveFunction;
    procedure Error(const Error: Boolean; const Message: string = '');
    property Parser: TParser read FParser write FParser;
  end;

var
  Main: TMain;

implementation

uses
  ParseUtils, ParseErrors, ParseTypes, Types;

{$R *.dfm}

procedure TMain.Compile;
const
  TextError = 'Empty formula';
  MinorFunction = 'Formula contains functions which do not require expression before or after itself';
  FunctionError = 'Formula contains no functions';
var
  S: string;
  Script: TScript;
  I, J: Integer;
  FArray: TIntegerDynArray;
  AFunction: PFunction;
begin
  Screen.Cursor := crHourGlass;
  try
    try
      S := Trim(edFormula.Text);
      if S = '' then Error(True, TextError)
      else begin
        FParser.StringToScript(S, Script);
        try
          Helper.GetFunctionArray(Script, FArray);
          try
            J := tcFunction.TabIndex;
            tcFunction.Tabs.Clear;
            for I := Low(FArray) to High(FArray) do
              if FParser.GetFunction(FArray[I], AFunction) and (AFunction.Method.Parameter.LParameter or AFunction.Method.Parameter.RParameter) and
                (tcFunction.Tabs.IndexOfObject(Pointer(AFunction.Handle^)) < 0) then
                  tcFunction.Tabs.AddObject(AFunction.Name, Pointer(AFunction.Handle^));
            if tcFunction.Tabs.Count > 0 then
            begin
              if (J >= 0) and (J < tcFunction.Tabs.Count) then tcFunction.TabIndex := J;
              OpenFunction;
              Error(False);
            end
            else
              if Assigned(FArray) then
                Error(True, MinorFunction)
              else
                Error(True, FunctionError);
            edDecompiledFormula.Text := FParser.ScriptToString(Script);
          finally
            FArray := nil;
          end;
        finally
          Script := nil;
        end;
      end;
    except
      on E: EParserError do Error(True, E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.OpenFunction;
var
  I: Integer;
  AFunction: PFunction;
begin
  {$IFDEF DELPHI_2010}
  cbPriority.CheckAll(cbUnchecked, False, False);
  cbCoverage.CheckAll(cbUnchecked, False, False);
  {$ELSE}
  for I := 0 to cbPriority.Count - 1 do cbPriority.Checked[I] := False;
  for I := 0 to cbCoverage.Count - 1 do cbCoverage.Checked[I] := False;
  {$ENDIF}
  if tcFunction.TabIndex < 0 then
    I := -1
  else
    I := Integer(tcFunction.Tabs.Objects[tcFunction.TabIndex]);
  if FParser.GetFunction(I, AFunction) then
  begin
    cbPriority.Checked[Ord(AFunction.Priority.Priority)] := True;
    cbCoverage.Checked[Ord(AFunction.Priority.Coverage)] := True;
  end;
end;

procedure TMain.SaveFunction;
var
  I: Integer;
  AFunction: PFunction;
begin
  if tcFunction.TabIndex < 0 then
    I := -1
  else
    I := Integer(tcFunction.Tabs.Objects[tcFunction.TabIndex]);
  if FParser.GetFunction(I, AFunction) then
  begin
    for I := 0 to cbPriority.Count - 1 do
      if cbPriority.Checked[I] then
      begin
        AFunction.Priority.Priority := TFunctionPriority(I);
        Break;
      end;
    for I := 0 to cbCoverage.Count - 1 do
      if cbCoverage.Checked[I] then
      begin
        AFunction.Priority.Coverage := TPriorityCoverage(I);
        Break;
      end;
  end;
end;

procedure TMain.Error(const Error: Boolean; const Message: string);
begin
  gbFunction.Visible := not Error;
  laError.Visible := Error;
  if Error then laError.Caption := Message;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FParser := TMathParser.Create(Self);
  FParser.Cached := False;
  Compile;
end;

procedure TMain.edFormulaChange(Sender: TObject);
begin
  Compile;
end;

procedure TMain.cbPrioritizeClick(Sender: TObject);
begin
  FParser.Prioritize := cbPrioritize.Checked;
  Compile;
end;

procedure TMain.tcFunctionChange(Sender: TObject);
begin
  OpenFunction;
end;

procedure TMain.ClickCheck(Sender: TObject);
var
  I, J: Integer;
  ListBox: TCheckListBox absolute Sender;
begin
  J := ListBox.ItemIndex;
  if (J >= 0) and ListBox.Checked[J] then
    for I := 0 to ListBox.Count - 1 do if (I <> J) and ListBox.Checked[I] then
      ListBox.Checked[I] := False;
  SaveFunction;
  Compile;
end;

end.
