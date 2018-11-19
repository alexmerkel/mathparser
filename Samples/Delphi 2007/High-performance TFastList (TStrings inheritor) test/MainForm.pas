unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Classes, Controls, Forms, Dialogs, ActnList, StdCtrls, CheckLst,
  ComCtrls, IniFiles, FastList;

type
  TMain = class(TForm)
    ActionList: TActionList;
    bETMakeFile: TButton;
    bETMeasure: TButton;
    bSTMake: TButton;
    bSTMeasure: TButton;
    ETMeasure: TAction;
    gbETListClass: TGroupBox;
    gbETModificationCount: TGroupBox;
    gbETSearchCount: TGroupBox;
    gbETStringCount: TGroupBox;
    gbETStringLength: TGroupBox;
    gbSTListClass: TGroupBox;
    gbSTSearchCount: TGroupBox;
    gbSTStringCount: TGroupBox;
    gbSTStringLength: TGroupBox;
    lbETListClass: TCheckListBox;
    lbSTListClass: TCheckListBox;
    lETModificationCount: TLabel;
    lETSearchCount: TLabel;
    lETStringCount: TLabel;
    lETStringLength: TLabel;
    lSTSearchCount: TLabel;
    lSTStringCount: TLabel;
    lSTStringLength: TLabel;
    Make: TAction;
    mETDescription: TMemo;
    mSTDescription: TMemo;
    pcTest: TPageControl;
    StatusBar: TStatusBar;
    STMeasure: TAction;
    tbETModificationCount: TTrackBar;
    tbETSearchCount: TTrackBar;
    tbETStringCount: TTrackBar;
    tbETStringLength: TTrackBar;
    tbSTSearchCount: TTrackBar;
    tbSTStringCount: TTrackBar;
    tbSTStringLength: TTrackBar;
    tsExtendedTest: TTabSheet;
    tsSimpleTest: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClickCheck(Sender: TObject);
    procedure MakeExecute(Sender: TObject);
    procedure MeasureExecute(Sender: TObject);
    procedure MeasureUpdate(Sender: TObject);
    procedure tbSTStringCountChange(Sender: TObject);
    procedure tbSTSearchCountChange(Sender: TObject);
    procedure tbSTStringLengthChange(Sender: TObject);
    procedure tbETStringLengthChange(Sender: TObject);
    procedure tbETStringCountChange(Sender: TObject);
    procedure tbETSearchCountChange(Sender: TObject);
    procedure tbETModificationCountChange(Sender: TObject);
  private
    FETFileName: string;
    FSTFileName: string;
  protected
    function MakeString: string; virtual;
  public
    property STFileName: string read FSTFileName write FSTFileName;
    property ETFileName: string read FETFileName write FETFileName;
  end;

  TListType = (ltStringList, ltHashedStringList, ltFastList);
//  TListClass = class of TStrings;
//
//const
//  ClassArray: array[TListType] of TClass = (TStringList, THashedStringList, TFastList);

var
  Main: TMain;

implementation

uses
  FileUtils, NumberConsts;

{$R *.dfm}

function TMain.MakeString: string;
const
  Template = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  I: Integer;
begin
  if pcTest.ActivePage = tsSimpleTest then
    SetLength(Result, lSTStringLength.Tag)
  else
    SetLength(Result, lETStringLength.Tag);
  for I := 1 to Length(Result) do Result[I] := Template[1 + Random(Length(Template))];
end;

procedure TMain.FormCreate(Sender: TObject);
const
  STFileName = 'STFile.txt';
  ETFileName = 'ETFile.txt';
begin
  lbSTListClass.Tag := Ord(ltHashedStringList);
  lbSTListClass.Checked[lbSTListClass.Tag] := True;
  lbETListClass.Tag := Ord(ltHashedStringList);
  lbETListClass.Checked[lbETListClass.Tag] := True;
  FSTFileName := ExtractFilePath(Application.ExeName) + STFileName;
  FETFileName := ExtractFilePath(Application.ExeName) + ETFileName;
end;

procedure TMain.ListBoxClickCheck(Sender: TObject);
var
  I, J: Integer;
  ListBox: TCheckListBox absolute Sender;
begin
  J := ListBox.ItemIndex;
  if (J >= 0) and ListBox.Checked[J] then
  begin
    ListBox.Tag := J;
    for I := 0 to ListBox.Count - 1 do if (I <> J) and ListBox.Checked[I] then
      ListBox.Checked[I] := False;
  end;
  ListBox.Checked[J] := True;
end;

procedure TMain.MeasureExecute(Sender: TObject);
const
  Thousand = 1000;
  FileMessage = 'File is beeing loaded...';
  TestMessage = 'Search test is being performed...';
  TimeMessage = '%s time: %d seconds %d milliseconds';
  ListError = '%s error!';
var
  List: TStrings;
  TickCount: Double;
  I, J, K: Integer;
  Value: Extended;
begin
  Screen.Cursor := crHourGlass;
  try
    pcTest.Enabled := False;
    try
      if pcTest.ActivePage = tsSimpleTest then
        I := lbSTListClass.Tag
      else
        I := lbETListClass.Tag;
      case TListType(I) of
        ltHashedStringList: List := THashedStringList.Create;
        ltFastList: List := TFastList.Create;
      else
        List := TStringList.Create;
      end;
      try
        try
          StatusBar.SimpleText := FileMessage;
          try
            if pcTest.ActivePage = tsSimpleTest then
              List.LoadFromFile(FSTFileName)
            else
              List.LoadFromFile(FETFileName);
          finally
            StatusBar.SimpleText := TestMessage;
          end;
          if pcTest.ActivePage = tsSimpleTest then
          begin
            TickCount := GetTickCount;
            for I := 0 to lSTSearchCount.Tag - 1 do List.IndexOf(List[Random(List.Count)]);
            TickCount := GetTickCount - TickCount;
          end
          else begin
            if List.Count > lETModificationCount.Tag then
            begin
              J := List.Count div lETModificationCount.Tag;
              K := 0;
            end
            else begin
              J := 0;
              K := lETModificationCount.Tag div List.Count;
            end;
            TickCount := GetTickCount;
            for I := 0 to lSTSearchCount.Tag - 1 do
            begin
              Assert(List.IndexOf(List[Random(List.Count)]) >= 0, Format(ListError, [List.ClassName]));
              case Random(2) of
                0:
                  if J = 0 then
                  begin
                    for J := 0 to K - 1 do List[Random(List.Count)] := MakeString;
                    J := 0;
                  end
                  else if I mod J = 0 then List[Random(List.Count)] := MakeString;
              else
                if J = 0 then
                begin
                  for J := 0 to K - 1 do
                  begin
                    List.Delete(Random(List.Count));
                    List.Insert(Random(List.Count), MakeString);
                  end;
                  J := 0;
                end
                else
                  if I mod J = 0 then
                  begin
                    List.Delete(Random(List.Count));
                    List.Insert(Random(List.Count), MakeString);
                  end;
              end;
            end;
            TickCount := GetTickCount - TickCount;
          end;
          Value := TickCount / Thousand;
          Caption := Format(TimeMessage, [List.ClassName, Trunc(Value), Round(Frac(Value) * Thousand)]);
        finally
          StatusBar.SimpleText := '';
        end;
      finally
        List.Free;
      end;
    finally
      pcTest.Enabled := True;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.MeasureUpdate(Sender: TObject);
var
  Action: TAction absolute Sender;
begin
  if pcTest.ActivePage = tsSimpleTest then
    Action.Enabled := FileExists(FSTFileName)
  else
    Action.Enabled := FileExists(FETFileName);
end;

procedure TMain.MakeExecute(Sender: TObject);
const
  FileMessage = 'File is beeing created...';
  DoneMessage = 'File of %d strings of %d characters each is created';
var
  I: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    StatusBar.SimpleText := FileMessage;
    try
      pcTest.Enabled := False;
      try
        FileIndex := CreateFile;
        try
          if pcTest.ActivePage = tsSimpleTest then
          begin
            OpenFile(FSTFileName, otRewrite);
            for I := 0 to lSTStringCount.Tag - 1 do Write(MakeString);
          end
          else begin
            OpenFile(FETFileName, otRewrite);
            for I := 0 to lETStringCount.Tag - 1 do Write(MakeString);
          end;
          SaveFile;
          if pcTest.ActivePage = tsSimpleTest then
            Caption := Format(DoneMessage, [lSTStringCount.Tag, lSTStringLength.Tag])
          else
            Caption := Format(DoneMessage, [lETStringCount.Tag, lETStringLength.Tag]);
        finally
          DisposeFile(FileIndex);
        end;
      finally
        pcTest.Enabled := True;
      end;
    finally
      StatusBar.SimpleText := '';
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.tbSTStringLengthChange(Sender: TObject);
const
  Factor = 100;
begin
  lSTStringLength.Tag := tbSTStringLength.Position * Factor;
  lSTStringLength.Caption := IntToStr(lSTStringLength.Tag);
end;

procedure TMain.tbSTStringCountChange(Sender: TObject);
const
  Factor = 100000;
begin
  lSTStringCount.Tag := tbSTStringCount.Position * Factor;
  lSTStringCount.Caption := IntToStr(lSTStringCount.Tag);
end;

procedure TMain.tbSTSearchCountChange(Sender: TObject);
const
  Factor = 1000;
begin
  lSTSearchCount.Tag := tbSTSearchCount.Position * Factor;
  lSTSearchCount.Caption := IntToStr(lSTSearchCount.Tag);
end;

procedure TMain.tbETStringCountChange(Sender: TObject);
const
  Factor = 1000;
begin
  lETStringCount.Tag := tbETStringCount.Position * Factor;
  lETStringCount.Caption := IntToStr(lETStringCount.Tag);
end;

procedure TMain.tbETStringLengthChange(Sender: TObject);
const
  Factor = 100;
begin
  lETStringLength.Tag := tbETStringLength.Position * Factor;
  lETStringLength.Caption := IntToStr(lETStringLength.Tag);
end;

procedure TMain.tbETSearchCountChange(Sender: TObject);
const
  Factor = 1000;
begin
  lETSearchCount.Tag := tbETSearchCount.Position * Factor;
  lETSearchCount.Caption := IntToStr(lETSearchCount.Tag);
end;

procedure TMain.tbETModificationCountChange(Sender: TObject);
const
  Factor = 1000;
begin
  lETModificationCount.Tag := tbETModificationCount.Position * Factor;
  lETModificationCount.Caption := IntToStr(lETModificationCount.Tag);
end;

initialization
  Randomize;

end.
