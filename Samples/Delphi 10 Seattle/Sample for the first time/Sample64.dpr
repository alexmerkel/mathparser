program Sample64;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
