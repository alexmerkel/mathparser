{ *********************************************************************** }
{                                                                         }
{ Graph builder                                                           }
{                                                                         }
{ Copyright (c) 2016 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit MainForm;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, AppEvnts,
  ImgList, StdActns, ActnList, ActnMan, ComCtrls, StdCtrls, ToolWin, Buttons, Grids,
  ExtCtrls, Contnrs, HTTPApp, HTTPProd, OleCtrls, SHDocVw, System.Actions, FastList,
  GraphicTypes, Graph, Vcl.PlatformDefaultStyleActnCtrls;

const
  FormulaSGCode = 1;
  FormulaSGColCount = 4;
  ActiveColumn = 0;
  FormulaColumn = 1;
  TracingColumn = 2;
  RemoveColumn = 3;
  ObjectColumn = FormulaColumn;

  WM_REMOVE = WM_USER + 1;

type
  TScrollBox = class(Forms.TScrollBox);

  TEdit = class(StdCtrls.TEdit)
  private
    FDecrementValue: string;
    FIncrementValue: string;
    FOutputRestrict: string;
    FBorderRestrict: string;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure Loaded; override;
    procedure Change; override;
  public
    property BorderRestrict: string read FBorderRestrict write FBorderRestrict;
    property OutputRestrict: string read FOutputRestrict write FOutputRestrict;
    property IncrementValue: string read FIncrementValue write FIncrementValue;
    property DecrementValue: string read FDecrementValue write FDecrementValue;
  end;

  PCellData = ^TCellData;
  TCellData = record
    FormulaIndex: Integer;
    AB, RB: TSpeedButton;
  end;

  TEventType = (etDelete, etEnable);
  TCellEvent = procedure(const Data: PCellData; const EventType: TEventType) of object;

  TStringGrid = class(Grids.TStringGrid)
  private
    FCE: TCellEvent;
    FInplaceList: TObjectList;
    function GetCD(Index: Integer): PCellData;
    function GetEmpty: Boolean;
    function GetFormula(Index: Integer): string;
    function GetTracing(Index: Integer): string;
    procedure SetCD(Index: Integer; const Value: PCellData);
    procedure SetFormula(Index: Integer; const Value: string);
    procedure SetTracing(Index: Integer; const Value: string);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMRemove(var Message: TMessage); message WM_REMOVE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function CanEditShow: Boolean; override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure Paint; override;
    procedure ResizeColumn; virtual;
    procedure Make(const Index: Integer); overload; virtual;
    procedure Make; overload; virtual;
    procedure Delete(const Index: Integer); virtual;
    procedure Active(Sender: TObject); virtual;
    procedure Remove(Sender: TObject); virtual;
    function Check: Boolean; overload; virtual;
    function Check(const Index: Integer): Boolean; overload; virtual;
    procedure DoCellEvent(const Data: PCellData; const EventType: TEventType); virtual;
    property InplaceList: TObjectList read FInplaceList write FInplaceList;
    property Empty: Boolean read GetEmpty;
  public
    destructor Destroy; override;
    function Add: Integer; virtual;
    procedure Clear; virtual;
    procedure Arrange(const Rect: TRect; const Inplace: TControl); overload; virtual;
    procedure Arrange; overload; virtual;
    function GetCellData(const Index: Integer; out Data: PCellData): Boolean; virtual;
    property CD[Index: Integer]: PCellData read GetCD write SetCD;
    property Formula[Index: Integer]: string read GetFormula write SetFormula;
    property Tracing[Index: Integer]: string read GetTracing write SetTracing;
    property CE: TCellEvent read FCE write FCE;
  end;

  TMain = class(TForm)
    AE: TApplicationEvents;
    AM: TActionManager;
    ATB: TToolBar;
    BTB: TToolBar;
    cbFormula: TComboBox;
    cbLayout: TComboBox;
    CD: TColorDialog;
    CTB: TToolBar;
    DTB: TToolBar;
    eAccuracy: TEdit;
    eCenterX: TEdit;
    eCenterY: TEdit;
    eHSpacing: TEdit;
    eMargin: TEdit;
    eMaxX: TEdit;
    eMaxY: TEdit;
    eMaxZoom: TEdit;
    eMinZoom: TEdit;
    ePenWidth: TEdit;
    eQuality: TEdit;
    eTransparency: TEdit;
    eVSpacing: TEdit;
    eZoomInFactor: TEdit;
    eZoomOutFactor: TEdit;
    FExit: TFileExit;
    GAntialias: TAction;
    GAutoquality: TAction;
    GAxis: TAction;
    GClear: TAction;
    GColor: TAction;
    GCopy: TAction;
    GDraw: TAction;
    GExtreme: TAction;
    gFormula: TStringGrid;
    GGrid: TAction;
    GMulticolor: TAction;
    GOverlap: TAction;
    GPolar: TAction;
    GRectangular: TAction;
    GRefresh: TAction;
    GSign: TAction;
    GTracing: TAction;
    IL: TImageList;
    LBox: TScrollBox;
    LPanel: TPanel;
    Panel: TPanel;
    PC: TPageControl;
    PP: TPageProducer;
    SB: TStatusBar;
    Splitter: TSplitter;
    tbQuality: TTrackBar;
    tsGraph: TTabSheet;
    tsReport: TTabSheet;
    udAccuracy: TUpDown;
    udMargin: TUpDown;
    udPenWidth: TUpDown;
    udQuality: TUpDown;
    udTransparency: TUpDown;
    WB: TWebBrowser;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SGResize(Sender: TObject);
    procedure UDChanging(Sender: TObject; var AllowChange: Boolean);
    procedure AEHint(Sender: TObject);
    procedure GRectangularUpdate(Sender: TObject);
    procedure GPolarUpdate(Sender: TObject);
    procedure GDrawExecute(Sender: TObject);
    procedure GDrawUpdate(Sender: TObject);
    procedure GGridExecute(Sender: TObject);
    procedure GGridUpdate(Sender: TObject);
    procedure GAxisExecute(Sender: TObject);
    procedure GAxisUpdate(Sender: TObject);
    procedure GTracingExecute(Sender: TObject);
    procedure GTracingUpdate(Sender: TObject);
    procedure GOverlapExecute(Sender: TObject);
    procedure GOverlapUpdate(Sender: TObject);
    procedure GExtremeExecute(Sender: TObject);
    procedure GExtremeUpdate(Sender: TObject);
    procedure GAntialiasExecute(Sender: TObject);
    procedure GAntialiasUpdate(Sender: TObject);
    procedure GMulticolorExecute(Sender: TObject);
    procedure GMulticolorUpdate(Sender: TObject);
    procedure GAutoqualityExecute(Sender: TObject);
    procedure GAutoqualityUpdate(Sender: TObject);
    procedure GSignExecute(Sender: TObject);
    procedure GSignUpdate(Sender: TObject);
    procedure GCopyExecute(Sender: TObject);
    procedure GClearExecute(Sender: TObject);
    procedure GRefreshExecute(Sender: TObject);
    procedure GColorExecute(Sender: TObject);
    procedure GColorUpdate(Sender: TObject);
    procedure cbFormulaKeyPress(Sender: TObject; var Key: Char);
    procedure tbQualityChange(Sender: TObject);
    procedure eCenterXChange(Sender: TObject);
    procedure eCenterYChange(Sender: TObject);
    procedure eMaxXChange(Sender: TObject);
    procedure eMaxYChange(Sender: TObject);
    procedure eAccuracyChange(Sender: TObject);
    procedure eQualityChange(Sender: TObject);
    procedure ePenWidthChange(Sender: TObject);
    procedure eTransparencyChange(Sender: TObject);
    procedure cbLayoutChange(Sender: TObject);
    procedure eMarginChange(Sender: TObject);
    procedure eHSpacingChange(Sender: TObject);
    procedure eVSpacingChange(Sender: TObject);
    procedure eMinZoomChange(Sender: TObject);
    procedure eMaxZoomChange(Sender: TObject);
    procedure eZoomInFactorChange(Sender: TObject);
    procedure eZoomOutFactorChange(Sender: TObject);
    procedure CSChange(Sender: TObject);
    procedure gFormulaDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure GClearUpdate(Sender: TObject);
    procedure gFormulaSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure PCChange(Sender: TObject);
    procedure PPHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
  private
    FUpdateCount: Integer;
    FTraceList: TFastList;
    FGraph: TGraph;
  protected
    procedure Open; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    procedure CE(const Data: PCellData; const EventType: TEventType); virtual;
    procedure OffsetChange(Sender: TObject); virtual;
    procedure Clear; virtual;
    procedure ShowHint(const Index: Integer; const Hint: string = ''); virtual;
    procedure AddArray(const Value: array of string); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); reintroduce; overload;
    procedure TraceDone(Sender: TObject); virtual;
    procedure Trace(const Formula, Output: string); virtual;
    procedure RectangularTrace(Sender: TObject; const FormulaIndex: Integer; const Point: TExactPoint); virtual;
    procedure PolarTrace(Sender: TObject; const FormulaIndex: Integer; const Angle: Extended; const Point: TExactPoint); virtual;
    property TraceList: TFastList read FTraceList write FTraceList;
    property Graph: TGraph read FGraph write FGraph;
  end;

const
  EPanel = 1;

  DateTag = 'date';
  CSTag = 'cs';
  CSCenterTag = 'cscenter';
  CSSizeTag = 'cssize';
  FormulaTag = 'formula';
  OverlapTag = 'overlap';
  ExtremeTag = 'extreme';
  VaryRadiusTag = 'varyradius';
  VoidRadiusTag = 'voidradius';

  RectangularArray: array[0..10] of string =
    ('Exp (Sin X) - 2 * Cos (4 * X) + Sin (2 * X / 24) ^ 5',
    '(Cos X ^ 2 + Sin X) * (Sin X ^ 2 + Cos X ^ 3 + X) / (X - Sin X) + 1 / X',
    '(X * Sin Cos (X * Sin X)) / Tan X ^ 2',
    '1 / (X / Cos X - X * Sin X ^ 2)',
    '(Cos X ^ 2 + Sin X) * (Sin X ^ 2 + Cos X ^ 3 + X)',
    '1 / X ^ 2 - 2 * Sin (X ^ 2)',
    'X ^ 2 mod X * Sin X',
    'X * Sin X',
    'X * Sin X + X * Cos (1 - Int X)',
    '5 * ArcSin (10 * Tan (Abs (0.5 * X ^ 2 / X)) / Abs (200 * Sin (10000 * Abs (10 * X)))) + 0.05 * X ^ 2 -3',
    'if (Sin (1000 * X), Sqrt (25 - X ^ 2) + 1.5 * ArcCos (Cos (3 * Pi * X)), - Sqrt (25 - X ^ 2))');

  PolarArray: array[0..8] of string =
    ('5 * sin (T * 3)',
    '5 * sin (T * 5)',
    '5 * cos (T * 2)',
    '5 * cos (T * 4)',
    '5 * cos (T * 6)',
    'T * (Sin T ^ 2 + Cos T ^ 3 + T) / 8',
    '1 / T ^ 2 - 2 * Sin (T ^ 2)',
    'Exp (Sin T) - 2 * Cos (4 * T) + Sin ( (2 * T - Pi) / 24 ) ^ 5',
    '(1 + Sin T) * (1 + 0.9 * Cos (8 * T)) * (1 + 0.1 * Cos (24 * T)) * (0.9 + 0.05 * Cos (200 * T))');

var
  Main: TMain;

implementation

uses
  {$IFDEF DELPHI_XE7}WinApi.ActiveX, {$ELSE}ActiveX, {$ENDIF}CalcUtils, Clipbrd, GraphicUtils,
  Math, NumberUtils, Parser, TextBuilder, TextConsts, TextUtils, Types;

{$R *.dfm}

{ TEdit }

procedure TEdit.Change;
var
  S: string;
begin
  if (Trim(FOutputRestrict) <> '') and TryTextToString(Format(FOutputRestrict, [Text]), S) then Text := S;
  inherited;
end;

procedure TEdit.Loaded;
const
  Delimiter = '|';
begin
  inherited;
  FBorderRestrict := Trim(SubText(HelpKeyword, Delimiter, 0, True));
  FOutputRestrict := Trim(SubText(HelpKeyword, Delimiter, 1, True));
  FIncrementValue := Trim(SubText(HelpKeyword, Delimiter, 2, True));
  FDecrementValue := Trim(SubText(HelpKeyword, Delimiter, 3, True));
  HelpKeyword := '';
end;

procedure TEdit.WMMouseWheel(var Message: TWMMouseWheel);
var
  S, Value: string;
begin
  inherited;
  if Message.WheelDelta > 0 then
    S := Format(Trim(FIncrementValue), [Text])
  else
    S := Format(Trim(FDecrementValue), [Text]);
  if (S <> '') and TryTextToString(S, Value) and ((Trim(FBorderRestrict) = '') or AsBoolean(Format(FBorderRestrict, [Value]))) then Text := Value;
end;

{ TStringGrid }

procedure TStringGrid.Active(Sender: TObject);
var
  Button: TSpeedButton absolute Sender;
  Data: PCellData;
begin
  if GetCellData(Button.Tag, Data) then DoCellEvent(Data, etEnable);
end;

function TStringGrid.Add: Integer;
begin
  if Check then
  begin
    if not Empty then
    begin
      RowCount := RowCount + 1;
      Rows[RowCount - 1].Clear;
    end;
    Result := RowCount - 1;
    Make(Result);
  end
  else Result := -1;
end;

procedure TStringGrid.Arrange;
var
  I: Integer;
  Data: PCellData;
begin
  for I := FixedRows to RowCount - 1 do
    if GetCellData(I, Data) then
    begin
      if Assigned(Data.AB) then Arrange(CellRect(ActiveColumn, I), Data.AB);
      if Assigned(Data.RB) then Arrange(CellRect(RemoveColumn, I), Data.RB);
    end;
end;

procedure TStringGrid.Arrange(const Rect: TRect; const Inplace: TControl);
begin
  Inplace.BoundsRect := Classes.Rect(Rect.Left, Rect.Top, Rect.Right - 1, Rect.Bottom - 1);
  Inplace.Invalidate;
end;

function TStringGrid.Check: Boolean;
begin
  Result := (Tag = FormulaSGCode) and (ColCount = FormulaSGColCount);
end;

function TStringGrid.CanEditShow: Boolean;
var
  Data: PCellData;
begin
  Result := inherited CanEditShow and (not Check or Check and not Empty and (Col = FormulaColumn) and GetCellData(Row, Data) and Data.AB.Down);
end;

function TStringGrid.Check(const Index: Integer): Boolean;
begin
  Result := (Index >= FixedRows) and (Index < RowCount);
end;

procedure TStringGrid.Clear;
var
  I: Integer;
  Data: PCellData;
begin
  if Check then
  begin
    for I := FixedRows to RowCount - 1 do
      if GetCellData(I, Data) then
      begin
        CD[I] := nil;
        Dispose(Data);
        Rows[I].Clear;
      end;
    RowCount := FixedRows + 1;
    FInplaceList.Clear;
  end;
  inherited;
end;

procedure TStringGrid.ColWidthsChanged;
begin
  inherited;
  if Check then Arrange;
end;

procedure TStringGrid.Delete(const Index: Integer);
var
  I: Integer;
  Data: PCellData;
begin
  if Check and Check(Index) then
  begin
    if GetCellData(Index, Data) then Dispose(Data);
    CD[Index] := nil;
    for I := Index to RowCount - 1 do
      if I < RowCount - 1 then
      begin
        Rows[I].Assign(Rows[I + 1]);
        if GetCellData(I, Data) then
        begin
          Dec(Data.FormulaIndex);
          if Assigned(Data.AB) then
          begin
            Data.AB.Tag := I;
            Data.AB.GroupIndex := I + 1;
          end;
          if Assigned(Data.RB) then Data.RB.Tag := I;
        end;
      end
      else Rows[I].Clear;
    if RowCount - 1 > FixedRows then RowCount := RowCount - 1;
  end;
end;

destructor TStringGrid.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStringGrid.DoCellEvent(const Data: PCellData; const EventType: TEventType);
begin
  if Assigned(FCE) then FCE(Data, EventType);
end;

function TStringGrid.GetCD(Index: Integer): PCellData;
begin
  if Check(Index) then
    Result := PCellData(Rows[Index].Objects[ObjectColumn])
  else
    Result := nil;
end;

function TStringGrid.GetCellData(const Index: Integer; out Data: PCellData): Boolean;
begin
  Data := CD[Index];
  Result := Assigned(Data);
end;

function TStringGrid.GetEmpty: Boolean;
begin
  Result := (RowCount = FixedRows + 1) and not Assigned(CD[RowCount - 1]);
end;

function TStringGrid.GetFormula(Index: Integer): string;
begin
  if Check(Index) then
    Result := Rows[Index][FormulaColumn]
  else
    Result := '';
end;

function TStringGrid.GetTracing(Index: Integer): string;
begin
  if Check(Index) then
    Result := Rows[Index][TracingColumn]
  else
    Result := '';
end;

procedure TStringGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Check then
    case Key of
      VK_RETURN, VK_ESCAPE: if EditorMode then EditorMode := False;
      VK_DELETE: Delete(Row);
    end;
end;

procedure TStringGrid.Loaded;
const
  FormulaText = 'Formula';
  TracingText = 'Tracing';
begin
  inherited;
  if Check then
  begin
    DefaultDrawing := False;
    DoubleBuffered := True;
    FInplaceList := TObjectList.Create;
    Cols[FormulaColumn].Text := FormulaText;
    Cols[TracingColumn].Text := TracingText;
  end;
end;

procedure TStringGrid.Make(const Index: Integer);
const
  ActiveText = 'Active';
  RemoveText = 'Remove';
var
  Data: PCellData;
begin
  if Check and Check(Index) and not Assigned(CD[Index]) then
  begin
    System.New(Data);
    CD[Index] := Data;
    ZeroMemory(Data, SizeOf(TCellData));
    Data.AB := TSpeedButton.Create(nil);
    FInplaceList.Add(Data.AB);
    Data.AB.AllowAllUp := True;
    //Data.AB.Flat := True;
    Data.AB.Tag := Index;
    Data.AB.GroupIndex := Index + 1;
    Data.AB.Down := True;
    Data.AB.OnClick := Active;
    Data.AB.Caption := ActiveText;
    Arrange(CellRect(ActiveColumn, Index), Data.AB);
    Data.AB.Parent := Self;
    Data.RB := TSpeedButton.Create(nil);
    FInplaceList.Add(Data.RB);
    //Data.RB.Flat := True;
    Data.RB.Tag := Index;
    Data.RB.OnClick := Remove;
    Data.RB.Caption := RemoveText;
    Arrange(CellRect(RemoveColumn, Index), Data.RB);
    Data.RB.Parent := Self;
  end;
end;

procedure TStringGrid.Make;
var
  I: Integer;
begin
  if Check then for I := FixedRows to RowCount - 1 do Make(I);
end;

procedure TStringGrid.Paint;
var
  I, J: Integer;
  S: string;
  Rect: TRect;
begin
  inherited;
  if Check and not DefaultDrawing then
    for I := 0 to ColCount - 1 do for J := 0 to RowCount - 1 do
    begin
      S := Cols[I][J];
      Rect := CellRect(I, J);
      if J < FixedRows then
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(Rect);
        DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_LEFT or BF_RIGHT or BF_TOP or BF_BOTTOM);
        DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_LEFT or BF_RIGHT or BF_TOP or BF_BOTTOM);
        Canvas.Font.Color := clBlack;
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end
      else begin
        DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_LEFT or BF_RIGHT or BF_TOP or BF_BOTTOM);
        DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_LEFT or BF_RIGHT or BF_TOP or BF_BOTTOM);
      end;
    end;
end;

procedure TStringGrid.Remove(Sender: TObject);
begin
  PostMessage(Handle, WM_REMOVE, Integer(Sender), 0);
end;

procedure TStringGrid.ResizeColumn;
const
  ActiveRatio = 0.1;
  FormulaRatio = 0.3;
  TracingRatio = 0.5;
  RemoveRatio = 0.1;
begin
  if Check then
  begin
    ColWidths[ActiveColumn] := Round(ClientWidth * ActiveRatio) - GridLineWidth;
    ColWidths[FormulaColumn] := Round(ClientWidth * FormulaRatio) - GridLineWidth;
    ColWidths[TracingColumn] := Round(ClientWidth * TracingRatio) - GridLineWidth;
    ColWidths[RemoveColumn] := Round(ClientWidth * RemoveRatio) - GridLineWidth;
    Arrange;
  end;
end;

procedure TStringGrid.RowHeightsChanged;
begin
  inherited;
  if Check then Arrange;
end;

procedure TStringGrid.SetCD(Index: Integer; const Value: PCellData);
begin
  if Check(Index) then Rows[Index].Objects[ObjectColumn] := Pointer(Value);
end;

procedure TStringGrid.SetFormula(Index: Integer; const Value: string);
begin
  if Check(Index) then Rows[Index][FormulaColumn] := Value;
end;

procedure TStringGrid.SetTracing(Index: Integer; const Value: string);
begin
  if Check(Index) then Rows[Index][TracingColumn] := Value;
end;

procedure TStringGrid.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if Check then Arrange;
end;

procedure TStringGrid.WMRemove(var Message: TMessage);
var
  Button: TSpeedButton;
  I: Integer;
  Data: PCellData;
begin
  Button := TSpeedButton(Message.WParam);
  if Assigned(Button) then
  try
    I := Button.Tag;
    if GetCellData(I, Data) then
    begin
      DoCellEvent(Data, etDelete);
      if Assigned(Data.AB) then FInplaceList.Remove(Data.AB);
      if Assigned(Data.RB) then FInplaceList.Remove(Data.RB);
    end;
    Delete(I);
  finally
    FInplaceList.Remove(Button);
  end;
  Arrange;
end;

procedure TStringGrid.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if Check then Arrange;
end;

{ TMain }

procedure TMain.Open;
begin
  eCenterX.Text := FloatToStr(FGraph.Offset.X);
  eCenterY.Text := FloatToStr(FGraph.Offset.Y);
  eMaxX.Text := FloatToStr(FGraph.MaxX * 2);
  eMaxY.Text := FloatToStr(FGraph.MaxY * 2);
  udAccuracy.Position := FGraph.Accuracy;
  tbQuality.Position := FGraph.Quality;
  udQuality.Position := FGraph.Quality;
  udPenWidth.Position := FGraph.GraphPen.Width;
  udTransparency.Position := FGraph.SignBlendValue;
  cbLayout.ItemIndex := Ord(FGraph.SignLayout);
  udMargin.Position := FGraph.SignMargin;
  eHSpacing.Text := FloatToStr(FGraph.HSpacing);
  eVSpacing.Text := FloatToStr(FGraph.VSpacing);
  eMinZoom.Text := FloatToStr(FGraph.MinZoom);
  eMaxZoom.Text := FloatToStr(FGraph.MaxZoom);
  eZoomInFactor.Text := FloatToStr(FGraph.ZoomInFactor);
  eZoomOutFactor.Text := FloatToStr(FGraph.ZoomOutFactor);
end;

procedure TMain.BeginUpdate;
begin
  InterlockedIncrement(FUpdateCount);
end;

procedure TMain.EndUpdate;
begin
  InterlockedDecrement(FUpdateCount);
end;

procedure TMain.Clear;
begin
  BeginUpdate;
  try
    FGraph.Clear;
    gFormula.Clear;
    ShowHint(-1);
  finally
    EndUpdate;
  end;
end;

procedure TMain.ShowHint(const Index: Integer; const Hint: string);
var
  I: Integer;
begin
  if Index < 0 then
    for I := 0 to SB.Panels.Count - 1 do SB.Panels[I].Text := ''
  else
    SB.Panels[Index].Text := Hint;
end;

procedure TMain.AddArray(const Value: array of string);
var
  I: Integer;
begin
  for I := Low(Value) to High(Value) do cbFormula.Items.Add(Value[I]);
end;

procedure TMain.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not gFormula.EditorMode and not FGraph.Focused then FGraph.SetFocus;
end;

procedure TMain.CE(const Data: PCellData; const EventType: TEventType);
begin
  if (FUpdateCount = 0) and Assigned(Data) and FGraph.Formula.CheckIndex(Data.FormulaIndex) then
    case EventType of
      etDelete:
        begin
          FGraph.ForceStop;
          FGraph.Formula.Delete(Data.FormulaIndex);
          FGraph.Build;
          gFormula.Invalidate;
        end;
      etEnable:
        begin
          FGraph.Formula.Visible[Data.FormulaIndex] := Data.AB.Down;
          FGraph.Invalidate;
          gFormula.EditorMode := False;
          gFormula.Invalidate;
        end;
    end;
end;

procedure TMain.OffsetChange(Sender: TObject);
begin
  BeginUpdate;
  try
    eCenterX.Text := FloatToStr(FGraph.Offset.X);
    eCenterY.Text := FloatToStr(FGraph.Offset.Y);
    eMaxX.Text := FloatToStr(FGraph.MaxX * 2);
    eMaxY.Text := FloatToStr(FGraph.MaxY * 2);
  finally
    EndUpdate;
  end;
end;

procedure TMain.TraceDone(Sender: TObject);
var
  I, J: Integer;
  S: string;
begin
  for I := gFormula.FixedRows to gFormula.RowCount - 1 do
  begin
    J := FTraceList.IndexOfName(gFormula.Formula[I]);
    if J < 0 then gFormula.Tracing[I] := ''
    else begin
      S := Trim({$IFDEF DELPHI_7}FTraceList.ValueFromIndex[J]{$ELSE}GetValueFromIndex(FTraceList, J){$ENDIF});
      if not TextUtils.SameText(S, Trim(gFormula.Tracing[I])) then gFormula.Tracing[I] := S;
    end;
  end;
  FTraceList.Clear;
end;

procedure TMain.Trace(const Formula, Output: string);
var
  I: Integer;
begin
  I := FTraceList.IndexOfName(Formula);
  if I < 0 then
    FTraceList.Add(Formula + Equal + Output)
  else
    {$IFDEF DELPHI_7}FTraceList.ValueFromIndex[I] := Output{$ELSE}SetValueFromIndex(FTraceList, I, Output){$ENDIF};
end;

procedure TMain.RectangularTrace(Sender: TObject; const FormulaIndex: Integer; const Point: TExactPoint);
const
  TraceText = 'X = %s: Y = %s';
begin
  Trace(FGraph.Formula[FormulaIndex], Format(TraceText, [FormatFloat(FGraph.XFormat, Point.X), FormatFloat(FGraph.YFormat, Point.Y)]));
end;

procedure TMain.PolarTrace(Sender: TObject; const FormulaIndex: Integer; const Angle: Extended; const Point: TExactPoint);
const
  TraceText = 'Angle: %s degrees (%s radians); X = %s: Y = %s';
begin
  Trace(FGraph.Formula[FormulaIndex], Format(TraceText, [FormatFloat(FGraph.AngleFormat, RadToDeg(Angle)), FormatFloat(FGraph.AngleFormat, Angle), FormatFloat(FGraph.XFormat, Point.X), FormatFloat(FGraph.YFormat, Point.Y)]))
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FGraph.ForceStop;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  LBox.VertScrollBar.Range := LPanel.Height;
  LBox.AdjustSize;
  FTraceList := TFastList.Create;
  FTraceList.IndexTypes := [ttNameValue, ttName];
  FGraph := TGraph.Create(Self);
  FGraph.OnMouseMove := MouseMove;
  FGraph.OnOffsetChange := OffsetChange;
  FGraph.OnTraceDone := TraceDone;
  FGraph.OnRectangularTrace := RectangularTrace;
  FGraph.OnPolarTrace := PolarTrace;
  FGraph.Parent := tsGraph;
  FGraph.Align := alClient;
  gFormula.CE := CE;
  Open;
  AddArray(RectangularArray);
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FTraceList.Free;
end;

procedure TMain.SGResize(Sender: TObject);
begin
  gFormula.ResizeColumn;
end;

procedure TMain.UDChanging(Sender: TObject; var AllowChange: Boolean);
begin
  FGraph.Invalidate;
end;

procedure TMain.AEHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    SB.SimplePanel := True;
    SB.SimpleText := Application.Hint
  end
  else SB.SimplePanel := False;
end;

procedure TMain.GRectangularUpdate(Sender: TObject);
begin
  GRectangular.Checked := FGraph.CS = csRectangular;
end;

procedure TMain.GPolarUpdate(Sender: TObject);
begin
  GPolar.Checked := FGraph.CS = csPolar;
end;

procedure TMain.GDrawExecute(Sender: TObject);
var
  I, J: Integer;
  Data: PCellData;
begin
  Screen.Cursor := crHourGlass;
  try
    if FGraph.Formula.IndexOf(cbFormula.Text) < 0 then
    begin
      FGraph.ForceStop;
      I := FGraph.Formula.Add(cbFormula.Text);
      try
        FGraph.Build;
        BeginUpdate;
        try
          J := gFormula.Add;
          if gFormula.GetCellData(J, Data) then
          begin
            Data.FormulaIndex := I;
            gFormula.Formula[J] := FGraph.Formula[I];
          end;
        finally
          EndUpdate;
        end;
        gFormula.Invalidate;
        ShowHint(-1);
      except
        on E: Exception do
        begin
          Clear;
          ShowHint(EPanel, E.Message);
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.GDrawUpdate(Sender: TObject);
begin
  GDraw.Enabled := Trim(cbFormula.Text) <> '';
end;

procedure TMain.GGridExecute(Sender: TObject);
begin
  FGraph.ShowGrid := not FGraph.ShowGrid;
  FGraph.Invalidate;
end;

procedure TMain.GGridUpdate(Sender: TObject);
begin
  GGrid.Checked := FGraph.ShowGrid;
end;

procedure TMain.GAxisExecute(Sender: TObject);
begin
  FGraph.ShowAxis := not FGraph.ShowAxis;
  FGraph.Invalidate;
end;

procedure TMain.GAxisUpdate(Sender: TObject);
begin
  GAxis.Checked := FGraph.ShowAxis;
end;

procedure TMain.GTracingExecute(Sender: TObject);
begin
  FGraph.Tracing := not FGraph.Tracing;
  FGraph.Invalidate;
end;

procedure TMain.GTracingUpdate(Sender: TObject);
begin
  GTracing.Checked := FGraph.Tracing;
end;

procedure TMain.GOverlapExecute(Sender: TObject);
begin
  FGraph.Overlap := not FGraph.Overlap;
  FGraph.Invalidate;
end;

procedure TMain.GOverlapUpdate(Sender: TObject);
begin
  GOverlap.Checked := FGraph.Overlap;
end;

procedure TMain.GExtremeExecute(Sender: TObject);
begin
  FGraph.Extreme := not FGraph.Extreme;
  FGraph.Invalidate;
end;

procedure TMain.GExtremeUpdate(Sender: TObject);
begin
  GExtreme.Checked := FGraph.Extreme;
end;

procedure TMain.GAntialiasExecute(Sender: TObject);
begin
  FGraph.Antialias := not FGraph.Antialias;
  FGraph.Invalidate;
end;

procedure TMain.GAntialiasUpdate(Sender: TObject);
begin
  GAntialias.Checked := FGraph.Antialias;
end;

procedure TMain.GMulticolorExecute(Sender: TObject);
begin
  FGraph.MultiColor := not FGraph.MultiColor;
  FGraph.Invalidate;
  gFormula.Invalidate;
end;

procedure TMain.GMulticolorUpdate(Sender: TObject);
begin
  GMultiColor.Checked := FGraph.MultiColor;
end;

procedure TMain.GAutoqualityExecute(Sender: TObject);
begin
  FGraph.AutoQuality := not FGraph.AutoQuality;
  FGraph.Build;
end;

procedure TMain.GAutoqualityUpdate(Sender: TObject);
begin
  GAutoQuality.Checked := FGraph.AutoQuality;
end;

procedure TMain.GSignExecute(Sender: TObject);
begin
  FGraph.Sign := not FGraph.Sign;
  FGraph.Build;
end;

procedure TMain.GSignUpdate(Sender: TObject);
begin
  GSign.Checked := FGraph.Sign;
end;

procedure TMain.GCopyExecute(Sender: TObject);
begin
  ClipBoard.Assign(FGraph.Buffer);
end;

procedure TMain.GClearExecute(Sender: TObject);
begin
  Clear;
  FGraph.Invalidate;
end;

procedure TMain.GClearUpdate(Sender: TObject);
begin
  GClear.Enabled := FGraph.Formula.Count > 0;
end;

procedure TMain.GRefreshExecute(Sender: TObject);
begin
  FGraph.Invalidate;
end;

procedure TMain.GColorExecute(Sender: TObject);
begin
  CD.Color := FGraph.GraphPen.Color;
  if CD.Execute then
  begin
    FGraph.GraphPen.Color := CD.Color;
    FGraph.Invalidate;
  end;
end;

procedure TMain.GColorUpdate(Sender: TObject);
begin
  GColor.Enabled := not FGraph.MultiColor;
end;

procedure TMain.cbFormulaKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_RETURN) then
  begin
    Key := #0;
    GDraw.Execute;
  end;
end;

procedure TMain.tbQualityChange(Sender: TObject);
begin
  FGraph.Quality := tbQuality.Position;
  udQuality.Position := FGraph.Quality;
end;

procedure TMain.eCenterXChange(Sender: TObject);
var
  Value: Extended;
begin
  if (FUpdateCount = 0) and TryStrToFloat(eCenterX.Text, Value) then
  begin
    FGraph.Offset := MakePoint(Value, FGraph.Offset.Y);
    FGraph.Build;
  end;
end;

procedure TMain.eCenterYChange(Sender: TObject);
var
  Value: Extended;
begin
  if (FUpdateCount = 0) and TryStrToFloat(eCenterY.Text, Value) then
  begin
    FGraph.Offset := MakePoint(FGraph.Offset.X, Value);
    FGraph.Build;
  end;
end;

procedure TMain.eMaxXChange(Sender: TObject);
var
  Value: Extended;
begin
  if (FUpdateCount = 0) and TryStrToFloat(eMaxX.Text, Value) then
  begin
    FGraph.MaxX := Value / 2;
    FGraph.Build;
  end;
end;

procedure TMain.eMaxYChange(Sender: TObject);
var
  Value: Extended;
begin
  if (FUpdateCount = 0) and TryStrToFloat(eMaxY.Text, Value) then
  begin
    FGraph.MaxY := Value / 2;
    FGraph.Build;
  end;
end;

procedure TMain.eAccuracyChange(Sender: TObject);
begin
  FGraph.Accuracy := udAccuracy.Position;
  FGraph.Build;
end;

procedure TMain.eQualityChange(Sender: TObject);
begin
  FGraph.Quality := udQuality.Position;
  tbQuality.Position := FGraph.Quality;
  FGraph.Build;
end;

procedure TMain.ePenWidthChange(Sender: TObject);
begin
  FGraph.GraphPen.Width := udPenWidth.Position;
  FGraph.Invalidate;
end;

procedure TMain.eTransparencyChange(Sender: TObject);
begin
  FGraph.SignBlendValue := udTransparency.Position;
  FGraph.Invalidate;
  gFormula.Invalidate;
end;

procedure TMain.cbLayoutChange(Sender: TObject);
begin
  FGraph.SignLayout := TLayoutType(cbLayout.ItemIndex);
  FGraph.Invalidate;
end;

procedure TMain.eMarginChange(Sender: TObject);
begin
  FGraph.SignMargin := udMargin.Position;
  FGraph.Invalidate;
end;

procedure TMain.eHSpacingChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eHSpacing.Text, Value) then
  begin
    FGraph.HSpacing := Value;
    FGraph.Invalidate;
  end;
end;

procedure TMain.eVSpacingChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eVSpacing.Text, Value) then
  begin
    FGraph.VSpacing := Value;
    FGraph.Invalidate;
  end;
end;

procedure TMain.eMinZoomChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eMinZoom.Text, Value) then
  begin
    FGraph.MinZoom := Value;
    FGraph.Build;
  end;
end;

procedure TMain.eMaxZoomChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eMaxZoom.Text, Value) then
  begin
    FGraph.MaxZoom := Value;
    FGraph.Zoom(ztNone);
  end;
end;

procedure TMain.eZoomInFactorChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eZoomInFactor.Text, Value) then FGraph.ZoomInFactor := Value;
end;

procedure TMain.eZoomOutFactorChange(Sender: TObject);
var
  Value: Extended;
begin
  if TryStrToFloat(eZoomOutFactor.Text, Value) then FGraph.ZoomOutFactor := Value;
end;

procedure TMain.CSChange(Sender: TObject);
var
  Value: TCoordinateSystem;
begin
  Value := TCoordinateSystem(TComponent(Sender).Tag);
  if Value <> FGraph.CS then
  begin
    Clear;
    if Assigned(FGraph) then FGraph.CS := Value;
    FGraph.Invalidate;
    cbFormula.Items.Clear;
    case FGraph.CS of
      csRectangular: AddArray(RectangularArray);
    else
      AddArray(PolarArray);
    end;
    Open;
  end;
end;

procedure TMain.gFormulaDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Data: PCellData;
  A, B: TColor;
  I, J, K: Byte;
  S: string;
begin
  Data := gFormula.CD[ARow];
  if (ACol = FormulaColumn) and FGraph.MultiColor and Assigned(Data) and Data.AB.Down then
  begin
    A := ColorToRGB(gFormula.Color);
    B := FGraph.Formula.Data[Data.FormulaIndex].Color;
    I := GetTransparency(GetRValue(A), GetRValue(B), FGraph.SignBlendValue);
    J := GetTransparency(GetGValue(A), GetGValue(B), FGraph.SignBlendValue);
    K := GetTransparency(GetBValue(A), GetBValue(B), FGraph.SignBlendValue);
    gFormula.Canvas.Brush.Color := RGB(I, J, K);
    gFormula.Canvas.Font.Assign(FGraph.SignFont);
  end
  else begin
    gFormula.Canvas.Font.Assign(gFormula.Font);
    if Assigned(Data) and Data.AB.Down then
      gFormula.Canvas.Brush.Color := clWhite
    else begin
      gFormula.Canvas.Brush.Color := clBtnFace;
      gFormula.Canvas.Font.Color := clGrayText;
    end;
  end;
  gFormula.Canvas.FillRect(Rect);
  S := Trim(gFormula.Cols[ACol][ARow]);
  if S <> '' then
  begin
    I := (Rect.Bottom - Rect.Top - gFormula.Canvas.TextHeight(S)) div 2;
    gFormula.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + I, S);
  end;
end;

procedure TMain.gFormulaSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var
  Data: PCellData;
begin
  if (FUpdateCount = 0) and not gFormula.EditorMode and gFormula.GetCellData(ARow, Data) and FGraph.Formula.CheckIndex(Data.FormulaIndex) then
  begin
    FGraph.Formula[Data.FormulaIndex] := gFormula.Formula[ARow];
    FGraph.Build;
  end;
end;

procedure TMain.PCChange(Sender: TObject);
const
  ReportFileName = 'report.html';
var
  FileName: string;
  List: TStringList;
begin
  if PC.ActivePage = tsReport then
  begin
    FileName := ExtractFilePath(Application.ExeName) + ReportFileName;
    List := TStringList.Create;
    try
      List.Text := PP.Content;
      List.SaveToFile(FileName);
    finally
      List.Free;
    end;
    WB.Navigate(FileName);
  end;
end;

procedure TMain.PPHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
const
  CSTemplate = '<dd>%s</dd>';
  CenterTemplate = '<dd>X: %s</dd><dd>Y: %s</dd>';
  SizeTemplate = '<dd>Horizontal: %s</dd><dd>Vertical: %s</dd>';
  FormulaTemplate = 'Y = %s';
  ROTemplate = '<p><li>Name: %s<br>X: %s<br>Y: %s<br>Distance from center: %s<br>%s</li><p>';
  ATemplate = '%s degrees (%s radians)';
  POTemplate = '<p><li>Name: %s<br>Angle<ol><li>%s</li><li>%s</li></ol>X: %s<br>Y: %s<br>Distance from center: %s<br>%s</li></p>';
  RETemplate = '<p><li>X: %s<br>Y: %s<br>Distance from center: %s</li><p>';
  PETemplate = '<p><li>Angle: %s<br>X: %s<br>Y: %s<br>Distance from center: %s</li><p>';
  VaryTemplate = '<dd>%s</dd>';
  VoidTemplate = '<dd>%s</dd>';

  function FromCenter(const Point: TExactPoint): string;
  begin
    if FGraph.XDigitCount > FGraph.YDigitCount then
      Result := FormatFloat(FGraph.XFormat, GetDistance(ZeroPoint, Point))
    else
      Result := FormatFloat(FGraph.YFormat, GetDistance(ZeroPoint, Point));
  end;

  function FormatText(const Index: Integer; const Text: string): string;
  const
    Prefix = '<div style=''color: white; background-color:#%.2x%.2x%.2x''>';
    Suffix = '</div>';
  var
    A, B: TColor;
    I, J, K: Integer;
  begin
    A := ColorToRGB(clWhite);
    B := FGraph.Formula.Data[Index].Color;
    I := GetTransparency(GetRValue(A), GetRValue(B), FGraph.SignBlendValue);
    J := GetTransparency(GetGValue(A), GetGValue(B), FGraph.SignBlendValue);
    K := GetTransparency(GetBValue(A), GetBValue(B), FGraph.SignBlendValue);
    Result := Format(Prefix, [I, J, K]) + Text + Suffix;
  end;

  function FormatFormula(const Index: Integer): string;
  var
    Data: PFormulaData;
  begin
    Data := FGraph.Formula.Data[Index];
    if Assigned(Data) then
      if FGraph.MultiColor then
        Result := FormatText(Index, Format(FormulaTemplate, [FGraph.Parser.ScriptToString(FGraph.SA[Data.ScriptIndex], rmUser)]))
      else
        Result := Format(FormulaTemplate, [FGraph.Parser.ScriptToString(FGraph.SA[Data.ScriptIndex], rmUser)])
    else
      Result := '';
  end;

  function AddExtreme(const Back, Face: TPlace; const EA: TExactPointArray): string;
  var
    Builder: TTextBuilder;
    I, J, K, L: Integer;
    Point: TExactPoint;
    X, Y, A: string;
  begin
    Builder := TTextBuilder.Create;
    try
      for I := Back.ArrayIndex to Face.ArrayIndex do if GetRange(EA, Back, Face, I, K, L) then
        for J := K to L do
        begin
          Point := EA[I][J];
          X := FormatFloat(FGraph.XFormat, Point.X);
          Y := FormatFloat(FGraph.YFormat, Point.Y);
          case FGraph.CS of
            csRectangular:
              Builder.Append(Format(RETemplate, [X, Y, FromCenter(Point)]));
          else
            A := Format(ATemplate, [FormatFloat(FGraph.AngleFormat, RadToDeg(Point.Angle)), FormatFloat(FGraph.AngleFormat, Point.Angle)]);
            Builder.Append(Format(PETemplate, [A, X, Y, FromCenter(Point)]));
          end;
        end;
      Result := Builder.Text;
    finally
      Builder.Free;
    end;
  end;

var
  Builder: TTextBuilder;
  I, J: Integer;
  Overlap: TOverlap;
  X, Y, A, B: string;
  Data: PFormulaData;
begin
  if TextUtils.SameText(Trim(TagString), DateTag) then ReplaceText := DateTimeToStr(Now)
  else if TextUtils.SameText(Trim(TagString), CSTag) then
    case FGraph.CS of
      csRectangular: ReplaceText := Format(CSTemplate, ['Rectangular'])
    else
      ReplaceText := Format(CSTemplate, ['Polar']);
    end
  else if TextUtils.SameText(Trim(TagString), CSCenterTag) then
    ReplaceText := Format(CenterTemplate, [FormatFloat(FGraph.XFormat, FGraph.Offset.X), FormatFloat(FGraph.YFormat, FGraph.Offset.Y)])
  else if TextUtils.SameText(Trim(TagString), CSSizeTag) then
    ReplaceText := Format(SizeTemplate, [FormatFloat(FGraph.XFormat, FGraph.MaxX * 2), FormatFloat(FGraph.YFormat, FGraph.MaxY * 2)])
  else if TextUtils.SameText(Trim(TagString), FormulaTag) then
  begin
    Builder := TTextBuilder.Create;
    try
      Builder.Append('<ol>');
      for I := 0 to FGraph.Formula.Count - 1 do if FGraph.Formula.Active[I] then Builder.Append('<li>' + FormatFormula(I) + '</li>');
      Builder.Append('</ol>');
      ReplaceText := Builder.Text;
    finally
      Builder.Free;
    end;
  end
  else if TextUtils.SameText(Trim(TagString), OverlapTag) then
  begin
    if FGraph.Overlap and not FGraph.Busy and (FGraph.Formula.ActiveCount > 0) then
    begin
      Builder := TTextBuilder.Create;
      try
        for I := 0 to FGraph.Formula.Count - 1 do if FGraph.Formula.Active[I] then
        begin
          Builder.Append('<p>' + FormatFormula(I) + '</p><ol style=''font-size: xx-small''>');
          for J := Low(FGraph.OverlapArray) to High(FGraph.OverlapArray) do
          begin
            Overlap := FGraph.OverlapArray[J];
            if ((Overlap.AFormula = I) or (Overlap.BFormula = I)) and FGraph.Formula.Active[Overlap.AFormula] and FGraph.Formula.Active[Overlap.BFormula] then
            begin
              X := FormatFloat(FGraph.XFormat, Overlap.Point.X);
              Y := FormatFloat(FGraph.YFormat, Overlap.Point.Y);
              case FGraph.CS of
                csRectangular:
                  if Overlap.AFormula = I then
                    Builder.Append(Format(ROTemplate, [FGraph.OverlapName[J], X, Y, FromCenter(Overlap.Point), FormatFormula(Overlap.BFormula)]))
                  else
                    Builder.Append(Format(ROTemplate, [FGraph.OverlapName[J], X, Y, FromCenter(Overlap.Point), FormatFormula(Overlap.AFormula)]));
              else
                if FGraph.MultiColor then
                begin
                  A := FormatText(Overlap.AFormula, Format(ATemplate, [FormatFloat(FGraph.AngleFormat, RadToDeg(Overlap.AAngle)), FormatFloat(FGraph.AngleFormat, Overlap.AAngle)]));
                  B := FormatText(Overlap.BFormula, Format(ATemplate, [FormatFloat(FGraph.AngleFormat, RadToDeg(Overlap.BAngle)), FormatFloat(FGraph.AngleFormat, Overlap.BAngle)]));
                end
                else begin
                  A := Format(ATemplate, [FormatFloat(FGraph.AngleFormat, RadToDeg(Overlap.AAngle)), FormatFloat(FGraph.AngleFormat, Overlap.AAngle)]);
                  B := Format(ATemplate, [FormatFloat(FGraph.AngleFormat, RadToDeg(Overlap.BAngle)), FormatFloat(FGraph.AngleFormat, Overlap.BAngle)]);
                end;
                if Overlap.AFormula = I then
                  Builder.Append(Format(POTemplate, [FGraph.OverlapName[J], A, B, X, Y, FromCenter(Overlap.Point), FormatFormula(Overlap.BFormula)]))
                else
                  Builder.Append(Format(POTemplate, [FGraph.OverlapName[J], B, A, X, Y, FromCenter(Overlap.Point), FormatFormula(Overlap.AFormula)]));
              end;
            end;
          end;
          Builder.Append('</ol>');
        end;
        ReplaceText := Builder.Text;
      finally
        Builder.Free;
      end;
    end;
  end
  else if TextUtils.SameText(Trim(TagString), VaryRadiusTag) then
    ReplaceText := Format(VaryTemplate, [FormatFloat(FGraph.XYFormat, FGraph.ExtremeVaryRadius)])
  else if TextUtils.SameText(Trim(TagString), VoidRadiusTag) then
    ReplaceText := Format(VoidTemplate, [FormatFloat(FGraph.XYFormat, FGraph.ExtremeVoidRadius)])
  else if TextUtils.SameText(Trim(TagString), ExtremeTag) then
  begin
    if FGraph.Extreme and not FGraph.Busy and (FGraph.Formula.ActiveCount > 0) then
    begin
      Builder := TTextBuilder.Create;
      try
        for I := 0 to FGraph.Formula.Count - 1 do if FGraph.Formula.Active[I] then
        begin
          Data := FGraph.Formula.Data[I];
          if Assigned(Data) then
          begin
            A := Trim(AddExtreme(Data.MinBack, Data.MinFace, FGraph.MinArray));
            if A <> '' then
              Builder.Append('<p>' + FormatFormula(I) + '</p>Minimum:<ol style=''font-size: xx-small''>' + A + '</ol>');
            A := Trim(AddExtreme(Data.MaxBack, Data.MaxFace, FGraph.MaxArray));
            if A <> '' then
              Builder.Append('Maximum:<ol style=''font-size: xx-small''>' + A + '</ol>');
          end;
        end;
        ReplaceText := Builder.Text;
      finally
        Builder.Free;
      end;
    end;
  end
  else ReplaceText := '';
end;

initialization
  RegisterClasses([TSpeedButton, TCheckBox, TGroupBox, TLabel, TToolBar, TToolButton]);
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
