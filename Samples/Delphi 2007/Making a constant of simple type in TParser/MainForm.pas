unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Dialogs, Parser,
  ParseTypes, ValueTypes;

type
  TMain = class(TForm)
    Bevel: TBevel;
    bExecute: TButton;
    edFormula: TEdit;
    laHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bExecuteClick(Sender: TObject);
  private
    FParser: TParser;
  public
    property Parser: TParser read FParser write FParser;
  end;

var
  Main: TMain;

implementation

uses
  ValueUtils;

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  // Create a new instance of TMathParser object:
  FParser := TMathParser.Create(Self);
  FParser.AddConstant('MyVar1', 100);
  FParser.AddConstant('MyVar2', 50.5);
  FParser.AddConstant('MyVar3', True);
end;

procedure TMain.bExecuteClick(Sender: TObject);
var
  Script: TScript;
begin
  // Compile the formula to the Script:
  FParser.StringToScript(edFormula.Text, Script);
  try

    {
      Execute method returns pointer to the TValue type. To convert TValue to
      string we use ValueToText method from ValueUtils unit:
    }
    ShowMessage( ValueToText( FParser.Execute(Script)^ ) );

  finally
    Script := nil;
  end;
end;

end.