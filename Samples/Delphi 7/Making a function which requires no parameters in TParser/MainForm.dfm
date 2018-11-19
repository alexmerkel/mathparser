object Main: TMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Sample'
  ClientHeight = 77
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object laHint: TLabel
    Left = 8
    Top = 8
    Width = 473
    Height = 16
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = 'We have the new function "Three" which simply returns number 3:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    WordWrap = True
  end
  object Bevel: TBevel
    Left = 8
    Top = 32
    Width = 476
    Height = 37
    Shape = bsFrame
  end
  object eFormula: TEdit
    Left = 97
    Top = 40
    Width = 379
    Height = 21
    TabOrder = 1
    Text = 'Three'
  end
  object bExecute: TButton
    Left = 16
    Top = 40
    Width = 75
    Height = 21
    Caption = 'Execute'
    Default = True
    TabOrder = 0
    OnClick = bExecuteClick
  end
end
