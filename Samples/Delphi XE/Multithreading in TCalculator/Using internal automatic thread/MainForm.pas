unit MainForm;

interface

{$B-}
{$I Directives.inc}

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Calculator;

type
  TMain = class(TForm)
    bCalculate: TButton;
    leInput: TLabeledEdit;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure bCalculateClick(Sender: TObject);
  private
    FCalculator: TCalculator;
    FTickCount: Longword;
  public
    procedure CalculatorThreadDone(Sender: TObject);
    property Calculator: TCalculator read FCalculator write FCalculator;
    property TickCount: Longword read FTickCount write FTickCount;
  end;

var
  Main: TMain;

implementation

uses
  ValueUtils;

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  FCalculator := TCalculator.Create(Self);
  FCalculator.Thread.OnDone := CalculatorThreadDone;
  FCalculator.Thread.ThreadCount := 5;
end;

procedure TMain.bCalculateClick(Sender: TObject);
var
  I: Integer;
begin
  // Disable the button:
  bCalculate.Enabled := False;

  // Delete buffer, containing previously added expressions to calculate:
  FCalculator.Thread.Clear;

  // Clear the memo:

  Memo.Clear;

  // Prepare the internal Parser for calculations (this is not really necessary -
  // Prepare method is called automatically, but this time it is called manually
  // beforehand to reach the maximum time measurement accuracy:

  FCalculator.Parser.Prepare;

  // Prepare Parser cache for calculations. It is necessary because first use of
  // cache causes adding some statistics functions. Adding a new functions is not
  // allowed in Calculator when threads are on:

  FCalculator.Parser.Cache.Prepare;

  // Add expressions to calculate to the FCalculator buffer (this is the buffer
  // which was cleared by FCalculator.Thread.Clear method). The buffer consists
  // from two fields (FCalculator.Thread.Item[Index].Text and
  // FCalculator.Thread.Item[Index].Value), the expression to calculate lies in
  // first field. After all threads are done, the second field will contain
  // the result:

  for I := 0 to 5000 - 1 do
    FCalculator.Thread.AddText(leInput.Text + ' + ' + IntToStr(I));

  // Show message before run:
  ShowMessage('Starting...');

  // Obtain the start time:
  FTickCount := GetTickCount;

  // Run the threads:
  FCalculator.Thread.Execute;

  // Wait until TCalculator.Thread.OnDone event fires
end;

procedure TMain.CalculatorThreadDone(Sender: TObject);
var
  I: Integer;
  Expression, Result: string;
begin
  try
    // Obtain the calculation time:
    FTickCount := GetTickCount - FTickCount;

    // Show the calculation time:
    Caption := Format('Calculation time: %d sec %d msec', [Trunc(FTickCount / 1000),
      FTickCount - Trunc(FTickCount / 1000) * 1000]);

    // Show the expressions and its results in Memo:
    Screen.Cursor := crHourGlass;
    try
      Memo.Lines.BeginUpdate;
      try
        for I := 0 to FCalculator.Thread.ItemCount - 1 do
        begin
          Expression := FCalculator.Thread.Item[I].Text;

          // The expression result is represented by TValue structure, which is in
          // ValueTypes unit. We use function ValueToText, which is in ValueUtils
          // unit, to convert variable of TValue type into the string representation:

          Result := ValueToText( FCalculator.Thread.Item[I].Value );

          Memo.Lines.Add( Format('%d) %s = %s', [I + 1, Expression, Result] ) );

          Application.ProcessMessages;
        end;
      finally
        Memo.Lines.EndUpdate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

  finally
    // Enable the button:
    bCalculate.Enabled := True;
  end;
end;

end.
