object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Main'
  ClientHeight = 514
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object bCalculate: TButton
    Left = 266
    Top = 25
    Width = 100
    Height = 21
    Caption = 'Execute'
    Default = True
    TabOrder = 0
    OnClick = bCalculateClick
  end
  object leInput: TLabeledEdit
    Left = 8
    Top = 25
    Width = 250
    Height = 21
    EditLabel.Width = 136
    EditLabel.Height = 13
    EditLabel.Caption = 'Any expression to calculate:'
    EditLabel.Transparent = True
    TabOrder = 1
    Text = '2 * 2'
  end
  object Memo: TMemo
    Left = 8
    Top = 54
    Width = 358
    Height = 450
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
