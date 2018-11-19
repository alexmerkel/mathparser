program Parse;

uses
  Forms,
  MainForm in 'MainForm.pas' {Main};

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
