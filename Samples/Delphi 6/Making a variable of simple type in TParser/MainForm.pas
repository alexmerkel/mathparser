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
    FMyVar2: Double;
    FMyVar3: Boolean;
    FMyVar1: Integer;
    FParser: TParser;
  public
    property Parser: TParser read FParser write FParser;
    property MyVar1: Integer read FMyVar1 write FMyVar1;
    property MyVar2: Double read FMyVar2 write FMyVar2;
    property MyVar3: Boolean read FMyVar3 write FMyVar3;
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

  FParser.AddVariable('MyVar1', FMyVar1);
  FParser.AddVariable('MyVar2', FMyVar2);
  FParser.AddVariable('MyVar3', FMyVar3);

  FMyVar1 := 100;
  FMyVar2 := 50.5;
  FMyVar3 := True;
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