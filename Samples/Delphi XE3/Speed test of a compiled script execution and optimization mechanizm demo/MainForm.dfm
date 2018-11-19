object Main: TMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 
    'Speed test of a compiled script execution and optimization mecha' +
    'nizm demo'
  ClientHeight = 480
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TPanel
    Left = 450
    Top = 0
    Width = 350
    Height = 480
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object laItems: TLabel
      Left = 0
      Top = 8
      Width = 345
      Height = 26
      AutoSize = False
      Caption = 
        'The list contains formulas to show how parser calculates them. P' +
        'lease feel free to edit the existing formulas or add new ones:'
      WordWrap = True
    end
    object laOptimalItems: TLabel
      Left = 0
      Top = 296
      Width = 345
      Height = 13
      AutoSize = False
      Caption = 'The list contains optimized formulas taken from the list above:'
    end
    object meOptimalItems: TMemo
      Left = 0
      Top = 315
      Width = 348
      Height = 161
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object meItems: TMemo
      Left = 0
      Top = 40
      Width = 345
      Height = 161
      Lines.Strings = (
        '2 * 2 + Double X'
        '(7 + 6 + Byte Y) * (5! + Cos (4 * X + 2 * Y))'
        'Pi * 2 + Sin 1 + Cos Y')
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = meItemsChange
    end
    object rgTypeRetrieveMode: TRadioGroup
      Left = 0
      Top = 207
      Width = 345
      Height = 83
      Caption = 'The way how to include types in the optimized formulas:'
      ItemIndex = 1
      Items.Strings = (
        'No types'
        'User-declared types'
        'All types')
      TabOrder = 1
      OnClick = meItemsChange
    end
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 480
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object BBevel: TBevel
      Left = 8
      Top = 51
      Width = 436
      Height = 64
      Shape = bsFrame
    end
    object ABevel: TBevel
      Left = 8
      Top = 8
      Width = 436
      Height = 37
      Shape = bsFrame
    end
    object laText: TLabel
      Left = 97
      Top = 19
      Width = 40
      Height = 13
      Caption = 'Formula:'
    end
    object laXValue: TLabel
      Left = 18
      Top = 63
      Width = 79
      Height = 13
      Caption = 'X variable value:'
    end
    object laYValue: TLabel
      Left = 18
      Top = 89
      Width = 79
      Height = 13
      Caption = 'Y variable value:'
    end
    object laReport: TLabel
      Left = 10
      Top = 121
      Width = 434
      Height = 13
      AutoSize = False
      Caption = 'Execution report:'
    end
    object TPanel
      Left = 0
      Top = 396
      Width = 450
      Height = 84
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object laCount: TLabel
        Left = 10
        Top = 63
        Width = 434
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object laRepeatCount: TLabel
        Left = 10
        Top = 0
        Width = 428
        Height = 26
        AutoSize = False
        Caption = 
          'The trackbar controls how many times parser calculates forumla a' +
          'bove to measure its calculation speed:'
        WordWrap = True
      end
      object tbCount: TTrackBar
        Left = 10
        Top = 32
        Width = 434
        Height = 25
        Max = 1000
        Min = 1
        PageSize = 10
        Frequency = 50
        Position = 1
        TabOrder = 0
        OnChange = tbCountChange
      end
    end
    object meReport: TMemo
      Left = 10
      Top = 140
      Width = 434
      Height = 250
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 4
    end
    object bExecute: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Execute'
      Default = True
      TabOrder = 0
      OnClick = bExecuteClick
    end
    object edFormula: TEdit
      Left = 143
      Top = 16
      Width = 294
      Height = 21
      TabOrder = 1
      Text = 'X * (2 + 2)'
    end
    object edX: TEdit
      Left = 103
      Top = 59
      Width = 334
      Height = 21
      TabOrder = 2
      Text = '1 + 2'
    end
    object edY: TEdit
      Left = 103
      Top = 86
      Width = 334
      Height = 21
      TabOrder = 3
      Text = '3 + 4'
    end
  end
end
