object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample'
  ClientHeight = 100
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object leInput: TLabeledEdit
    Left = 8
    Top = 25
    Width = 250
    Height = 21
    EditLabel.Width = 132
    EditLabel.Height = 13
    EditLabel.Caption = 'Any expression to calculate:'
    EditLabel.Transparent = True
    TabOrder = 0
    Text = '2 * 2'
  end
  object leOutput: TLabeledEdit
    Left = 8
    Top = 71
    Width = 250
    Height = 21
    EditLabel.Width = 33
    EditLabel.Height = 13
    EditLabel.Caption = 'Result:'
    TabOrder = 2
  end
  object bCalculate: TButton
    Left = 266
    Top = 25
    Width = 75
    Height = 21
    Caption = 'Calculate'
    Default = True
    TabOrder = 1
    OnClick = bCalculateClick
  end
end
