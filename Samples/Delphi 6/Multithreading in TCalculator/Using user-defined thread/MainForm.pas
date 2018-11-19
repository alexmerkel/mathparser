unit MainForm;

interface

{$B-}
{$I Directives.inc}

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Contnrs,
  Calculator, ParseManager;

type
  TMain = class(TForm)
    bCalculate: TButton;
    leInput: TLabeledEdit;
    Memo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bCalculateClick(Sender: TObject);
  private
    FCalculator: TCalculator;
    FTickCount: Longword;
    FExpressionList: TStrings;
    FThreadList: TObjectList;
  public
    property Calculator: TCalculator read FCalculator write FCalculator;
    property ThreadList: TObjectList read FThreadList write FThreadList;
    property ExpressionList: TStrings read FExpressionList write FExpressionList;
    property TickCount: Longword read FTickCount write FTickCount;
  end;

  TParseThread = class(Classes.TThread)
  private
    FParseManager: TParseManager;
    FToIndex: Integer;
    FFromIndex: Integer;
  protected
    procedure Execute; override;
    property ParseManager: TParseManager read FParseManager write FParseManager;
  public
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    property FromIndex: Integer read FFromIndex write FFromIndex;
    property ToIndex: Integer read FToIndex write FToIndex;
  end;

var
  Main: TMain;
  Lock: TRTLCriticalSection;

implementation

uses
  ValueUtils;

{$R *.dfm}

{ TParseThread }

constructor TParseThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FParseManager := TParseManager.Create(nil);
  FParseManager.MaxCount := 0;
end;

destructor TParseThread.Destroy;
begin
  FParseManager.Free;
  inherited;
end;

procedure TParseThread.Execute;
var
  I: Integer;
  Expression, Result: string;
begin
  inherited;
  I := FFromIndex;
  while not Terminated and Assigned(Main) and (I <= FToIndex) do
  begin
    Expression := Main.FExpressionList[I];

    {
      The following methods of TCalculator do not require synchronization:
        - AsValue
        - AsByte
        - AsShortint
        - AsWord
        - AsSmallint
        - AsLongword
        - AsInteger
        - AsInt64
        - AsSingle
        - AsDouble
        - AsExtended
        - AsBoolean
        - AsPointer
        - AsString
      All other methods require synchronization when being used within the thread.
    }

    Result := FParseManager.AsString(Expression);

    // Write Result to the Main.ExpressionList:
    EnterCriticalSection(Lock);
    try
      Main.FExpressionList[I] := Expression + ' = ' + Result;
    finally
      LeaveCriticalSection(Lock);
    end;

    Inc(I);
  end;
end;

{ TMain }

procedure TMain.bCalculateClick(Sender: TObject);
var
  I: Integer;
  Thread: TParseThread;
begin
  // Disable the button:
  bCalculate.Enabled := False;
  try
    // Free all user-defined threads:
    FThreadList.Clear;

    // Empty ExpressionList, containing previously added expressions to calculate:
    FExpressionList.Clear;

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

    // Add expressions to calculate to the ExpressionList:

    for I := 0 to 5000 - 1 do
      ExpressionList.Add(leInput.Text + ' + ' + IntToStr(I));

    // Here we create and setup a user-defined Threads:

    // Thread 1:
    Thread := TParseThread.Create(True);
    Thread.ParseManager.Parser := FCalculator.Parser;
    Thread.ParseManager.Connector := FCalculator.Connector;
    FThreadList.Add(Thread);
    // Set a range of expressions in ExpressionList which thread will calculate:
    Thread.FromIndex := 0;
    Thread.ToIndex := 999;

    // Thread 2:
    Thread := TParseThread.Create(True);
    Thread.ParseManager.Parser := FCalculator.Parser;
    Thread.ParseManager.Connector := FCalculator.Connector;
    FThreadList.Add(Thread);
    Thread.FromIndex := 1000;
    Thread.ToIndex := 1999;
    // Thread 3:
    Thread := TParseThread.Create(True);
    Thread.ParseManager.Parser := FCalculator.Parser;
    Thread.ParseManager.Connector := FCalculator.Connector;
    FThreadList.Add(Thread);
    Thread.FromIndex := 2000;
    Thread.ToIndex := 2999;
    // Thread 4:
    Thread := TParseThread.Create(True);
    Thread.ParseManager.Parser := FCalculator.Parser;
    Thread.ParseManager.Connector := FCalculator.Connector;
    FThreadList.Add(Thread);
    Thread.FromIndex := 3000;
    Thread.ToIndex := 3999;
    // Thread 5:
    Thread := TParseThread.Create(True);
    Thread.ParseManager.Parser := FCalculator.Parser;
    Thread.ParseManager.Connector := FCalculator.Connector;
    FThreadList.Add(Thread);
    Thread.FromIndex := 4000;
    Thread.ToIndex := 4999;

    // All five threads are ready to calculate the ExpressionList. Now we just
    // need to run them:

    // Show message before run:
    ShowMessage('Starting...');

    // Obtain the start time:
    FTickCount := GetTickCount;

    // Here we start all the threads:
    for I := 0 to FThreadList.Count - 1 do
      TParseThread(FThreadList[I]).Resume;

    // Wait until all threads are done:
    for I := 0 to FThreadList.Count - 1 do
      TParseThread(FThreadList[I]).WaitFor;

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
        for I := 0 to FExpressionList.Count - 1 do
          Memo.Lines.Add( Format('%d) %s', [I + 1, FExpressionList[I]] ) );

        Application.ProcessMessages;
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

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Main := nil;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FCalculator := TCalculator.Create(Self);
  FThreadList := TObjectList.Create;
  FExpressionList := TStringList.Create;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FThreadList.Free;
  FExpressionList.Free;
end;

initialization
  InitializeCriticalSection(Lock);

finalization
  DeleteCriticalSection(Lock);

end.
