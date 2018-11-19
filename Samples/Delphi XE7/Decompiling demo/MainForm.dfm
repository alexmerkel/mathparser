object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample'
  ClientHeight = 357
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BBevel: TBevel
    Left = 8
    Top = 137
    Width = 526
    Height = 37
    Shape = bsFrame
  end
  object ABevel: TBevel
    Left = 8
    Top = 8
    Width = 526
    Height = 37
    Shape = bsFrame
  end
  object laByteCode: TLabel
    Left = 8
    Top = 182
    Width = 168
    Height = 13
    Caption = 'Byte-code of the compiled formula:'
  end
  object laFormula: TLabel
    Left = 16
    Top = 19
    Width = 93
    Height = 13
    Caption = 'Formula to compile:'
  end
  object laDecompiledScript: TLabel
    Left = 16
    Top = 148
    Width = 97
    Height = 13
    AutoSize = False
    Caption = 'Decompiled formula:'
  end
  object TLabel
    Left = 8
    Top = 51
    Width = 91
    Height = 39
    Caption = 'Step 1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TLabel
    Left = 443
    Top = 51
    Width = 91
    Height = 39
    Caption = 'Step 2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Memo: TMemo
    Left = 8
    Top = 199
    Width = 526
    Height = 150
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object edFormula: TEdit
    Left = 119
    Top = 16
    Width = 407
    Height = 21
    TabOrder = 0
    Text = '1 + Word 65535 + 2.0'
  end
  object bCompile: TButton
    Left = 8
    Top = 96
    Width = 100
    Height = 21
    Caption = 'Compile formula'
    Default = True
    TabOrder = 1
    OnClick = bCompileClick
  end
  object bDecompile: TButton
    Left = 434
    Top = 96
    Width = 100
    Height = 21
    Caption = 'Decompile formula'
    TabOrder = 3
    OnClick = bDecompileClick
  end
  object edDecompiledFormula: TEdit
    Left = 119
    Top = 145
    Width = 407
    Height = 21
    TabOrder = 4
  end
  object rgTypeRetrieveMode: TRadioGroup
    Left = 114
    Top = 53
    Width = 314
    Height = 76
    Caption = 'The way how to include types in the decompiled formula:'
    ItemIndex = 1
    Items.Strings = (
      'No types'
      'User-declared types'
      'All types')
    TabOrder = 2
  end
end
