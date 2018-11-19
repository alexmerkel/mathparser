unit MainForm;

interface

{$B-}
{$I Directives.inc}

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls;

type
  TMain = class(TForm)
    bCalculate: TButton;
    leInput: TLabeledEdit;
    leOutput: TLabeledEdit;
    procedure bCalculateClick(Sender: TObject);
  end;

var
  Main: TMain;

implementation

uses
  CalcUtils;

{$R *.dfm}

procedure TMain.bCalculateClick(Sender: TObject);
begin
  leOutput.Text := AsString(leInput.Text);
end;

end.
