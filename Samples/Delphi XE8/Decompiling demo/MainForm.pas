unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Parser,
  ParseTypes;

type
  TMain = class(TForm)
    ABevel: TBevel;
    BBevel: TBevel;
    bCompile: TButton;
    bDecompile: TButton;
    edDecompiledFormula: TEdit;
    edFormula: TEdit;
    laByteCode: TLabel;
    laDecompiledScript: TLabel;
    laFormula: TLabel;
    Memo: TMemo;
    rgTypeRetrieveMode: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bCompileClick(Sender: TObject);
    procedure bDecompileClick(Sender: TObject);
  private
    FScript: TScript;
    FParser: TParser;
  public
    function ScriptToByteCode(const Script: TScript): string;
    property Parser: TParser read FParser write FParser;
    property Script: TScript read FScript write FScript;
  end;

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
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FScript := nil;
end;

function TMain.ScriptToByteCode(const Script: TScript): string;
const
  ByteSeparator = '-';
  LongwordSeparator = '----';
var
  I: Integer;
  Separator: string;
begin
  Result := '';
  for I := Low(Script) to High(Script) do
  begin
    if I mod SizeOf(Longword) = 0 then
      Separator := LongwordSeparator
    else
      Separator := ByteSeparator;
    if I = Low(Script) then
      Result := IntToStr(Script[I])
    else
      Result := Result + Separator + IntToStr(Script[I]);
  end;
end;

procedure TMain.bCompileClick(Sender: TObject);
begin
  try
    FParser.StringToScript(edFormula.Text, FScript);
  except
    FScript := nil;
    raise;
  end;
  Caption := Format('Result: %s', [ValueToText(FParser.Execute(FScript)^)]);
  Memo.Lines.Text := ScriptToByteCode(FScript);
end;

procedure TMain.bDecompileClick(Sender: TObject);
var
  TypeMode: TRetrieveMode;
begin
  if Assigned(FScript) then
  begin
    TypeMode := TRetrieveMode(rgTypeRetrieveMode.ItemIndex);
    edDecompiledFormula.Text := FParser.ScriptToString(FScript, TypeMode);
  end
  else ShowMessage('There is no compiled formula!');
end;

end.
