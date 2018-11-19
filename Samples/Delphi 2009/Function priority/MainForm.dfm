object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample'
  ClientHeight = 249
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object laError: TLabel
    Left = 0
    Top = 90
    Width = 542
    Height = 159
    Align = alClient
    Alignment = taCenter
    Layout = tlCenter
  end
  object TPanel: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 90
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object TLabel
      Left = 78
      Top = 8
      Width = 52
      Height = 16
      Caption = 'Formula:'
    end
    object TLabel
      Left = 8
      Top = 39
      Width = 122
      Height = 16
      Caption = 'Decompiled Formula:'
    end
    object edFormula: TEdit
      Left = 136
      Top = 5
      Width = 395
      Height = 24
      TabOrder = 0
      Text = '1 * 2 ^ 3 - 4 = 5 + 6 / 7'
      OnChange = edFormulaChange
    end
    object edDecompiledFormula: TEdit
      Left = 136
      Top = 35
      Width = 395
      Height = 24
      ReadOnly = True
      TabOrder = 1
      OnChange = edFormulaChange
    end
    object cbPrioritize: TCheckBox
      Left = 136
      Top = 67
      Width = 395
      Height = 17
      Caption = 'Prioritize'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbPrioritizeClick
    end
  end
  object gbFunction: TGroupBox
    Left = 0
    Top = 90
    Width = 542
    Height = 159
    Align = alClient
    Caption = 'Function List:'
    TabOrder = 1
    object tcFunction: TTabControl
      Left = 2
      Top = 18
      Width = 538
      Height = 139
      Align = alClient
      TabOrder = 0
      OnChange = tcFunctionChange
      object TLabel
        Left = 8
        Top = 30
        Width = 97
        Height = 16
        Caption = 'Function Priority:'
      end
      object TLabel
        Left = 271
        Top = 30
        Width = 103
        Height = 16
        Caption = 'Priority Coverage:'
      end
      object cbPriority: TCheckListBox
        Left = 8
        Top = 52
        Width = 257
        Height = 68
        OnClickCheck = ClickCheck
        ItemHeight = 16
        Items.Strings = (
          'Lower'
          'Normal'
          'Higher')
        TabOrder = 0
      end
      object cbCoverage: TCheckListBox
        Left = 271
        Top = 52
        Width = 258
        Height = 69
        OnClickCheck = ClickCheck
        ItemHeight = 16
        Items.Strings = (
          'Local'
          'Total')
        TabOrder = 1
      end
    end
  end
end
