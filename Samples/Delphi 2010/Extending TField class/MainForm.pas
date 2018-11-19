unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Forms, StdCtrls, ExtCtrls, DB, DBClient,
  DBCtrls, Grids, DBGrids, Controls, Classes, ParseField;

type
  TMain = class(TForm)
    DataSet_A: TClientDataSet;
    DataSet_B: TClientDataSet;
    DataSource_A: TDataSource;
    DataSource_B: TDataSource;
    Splitter: TSplitter;
    Panel_B: TPanel;
    Panel_A: TPanel;
    DBGrid_A: TDBGrid;
    DBGrid_B: TDBGrid;
    DBNavigator_A: TDBNavigator;
    DBNavigator_B: TDBNavigator;
    leFormula_A: TLabeledEdit;
    leErrorText_A: TLabeledEdit;
    DataSet_BString: TStringField;
    DataSet_BInteger: TIntegerField;
    DataSet_AString: TStringField;
    DataSet_AInteger: TIntegerField;
    leFormula_B: TLabeledEdit;
    leErrorText_B: TLabeledEdit;
    LPanel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure leFormula_AChange(Sender: TObject);
    procedure leErrorText_AChange(Sender: TObject);
    procedure leFormula_BChange(Sender: TObject);
    procedure leErrorText_BChange(Sender: TObject);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  DataSet_AString.DefaultExpression := leFormula_A.Text;
  DataSet_AString.ConstraintErrorMessage := leErrorText_A.Text;
  DataSet_BString.DefaultExpression := leFormula_B.Text;
  DataSet_BString.ConstraintErrorMessage := leErrorText_B.Text;
  DataSet_A.InsertRecord([0, 1]);
  DataSet_A.InsertRecord([0, 2]);
  DataSet_B.InsertRecord([0, 3]);
  DataSet_B.InsertRecord([0, 4]);
end;

procedure TMain.FormResize(Sender: TObject);
begin
  Panel_A.ClientWidth := (ClientWidth - Splitter.ClientWidth) div 2;
end;

procedure TMain.leFormula_AChange(Sender: TObject);
begin
  DataSet_AString.DefaultExpression := leFormula_A.Text;
end;

procedure TMain.leErrorText_AChange(Sender: TObject);
begin
  DataSet_AString.ConstraintErrorMessage := leErrorText_A.Text;
end;

procedure TMain.leFormula_BChange(Sender: TObject);
begin
  DataSet_BString.DefaultExpression := leFormula_B.Text;
end;

procedure TMain.leErrorText_BChange(Sender: TObject);
begin
  DataSet_BString.ConstraintErrorMessage := leErrorText_B.Text;
end;

end.
