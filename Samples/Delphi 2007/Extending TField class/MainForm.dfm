object Main: TMain
  Left = 0
  Top = 0
  Width = 708
  Height = 527
  Caption = 'TParseField demo'
  Color = clBtnFace
  Constraints.MinHeight = 527
  Constraints.MinWidth = 708
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 347
    Top = 108
    Width = 5
    Height = 387
    AutoSnap = False
  end
  object LPanel: TLabel
    Left = 0
    Top = 0
    Width = 700
    Height = 108
    Align = alTop
    Caption = 
      'The sample contains two datasets named DataSet_A and DataSet_B. ' +
      'DataSets contain only the standard Delphi fields. However, if we' +
      ' use ParseField unit, then any field of string type changes its ' +
      'default behavior. String field calculates any expression stored ' +
      'in Field.DefaultExpression property and returns the result. If c' +
      'alculation is failed then the field returns Field.ConstraintErro' +
      'rMessage property. It is important that we can refer to any fiel' +
      'd of any dataset in the mathematic formula (but only if the data' +
      'set lies on the same form):'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Panel_B: TPanel
    Left = 352
    Top = 108
    Width = 348
    Height = 387
    Align = alClient
    TabOrder = 1
    object DBGrid_B: TDBGrid
      Left = 1
      Top = 49
      Width = 346
      Height = 243
      Align = alClient
      DataSource = DataSource_B
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object DBNavigator_B: TDBNavigator
      Left = 1
      Top = 24
      Width = 346
      Height = 25
      DataSource = DataSource_B
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
      Align = alTop
      Flat = True
      TabOrder = 0
    end
    object TPanel
      Left = 1
      Top = 292
      Width = 346
      Height = 94
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        346
        94)
      object leFormula_B: TLabeledEdit
        Left = 8
        Top = 24
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 276
        EditLabel.Height = 13
        EditLabel.Caption = 'The DataSet_B.String.DefaultExpression property value: '
        TabOrder = 0
        Text = '2 + 2'
        OnChange = leFormula_BChange
      end
      object leErrorText_B: TLabeledEdit
        Left = 8
        Top = 65
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 305
        EditLabel.Height = 13
        EditLabel.Caption = 'The DataSet_B.String.ConstraintErrorMessage property value: '
        TabOrder = 1
        Text = 'Error in formula'
        OnChange = leErrorText_BChange
      end
    end
    object TPanel
      Left = 1
      Top = 1
      Width = 346
      Height = 23
      Align = alTop
      AutoSize = True
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 3
      object TLabel
        Left = 2
        Top = 2
        Width = 342
        Height = 19
        Align = alTop
        Alignment = taCenter
        Caption = 'DataSet_B'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object Panel_A: TPanel
    Left = 0
    Top = 108
    Width = 347
    Height = 387
    Align = alLeft
    TabOrder = 0
    object DBGrid_A: TDBGrid
      Left = 1
      Top = 49
      Width = 345
      Height = 243
      Align = alClient
      DataSource = DataSource_A
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object DBNavigator_A: TDBNavigator
      Left = 1
      Top = 24
      Width = 345
      Height = 25
      DataSource = DataSource_A
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
      Align = alTop
      Flat = True
      TabOrder = 0
    end
    object TPanel
      Left = 1
      Top = 292
      Width = 345
      Height = 94
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        345
        94)
      object leFormula_A: TLabeledEdit
        Left = 8
        Top = 24
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 277
        EditLabel.Height = 13
        EditLabel.Caption = 'The DataSet_A.String.DefaultExpression property value: '
        TabOrder = 0
        Text = 'DataSet_A.Integer + DataSet_B.Integer'
        OnChange = leFormula_AChange
      end
      object leErrorText_A: TLabeledEdit
        Left = 8
        Top = 65
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 306
        EditLabel.Height = 13
        EditLabel.Caption = 'The DataSet_A.String.ConstraintErrorMessage property value: '
        TabOrder = 1
        Text = 'Error in formula'
        OnChange = leErrorText_AChange
      end
    end
    object TPanel
      Left = 1
      Top = 1
      Width = 345
      Height = 23
      Align = alTop
      AutoSize = True
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 3
      object TLabel
        Left = 2
        Top = 2
        Width = 341
        Height = 19
        Align = alTop
        Alignment = taCenter
        Caption = 'DataSet_A'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object DataSet_A: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'String'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Integer'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 224
    Data = {
      450000009619E0BD010000001800000002000000000003000000450006537472
      696E67010049000000010005574944544802000200140007496E746567657204
      000100000000000000}
    object DataSet_AString: TStringField
      FieldName = 'String'
    end
    object DataSet_AInteger: TIntegerField
      FieldName = 'Integer'
    end
  end
  object DataSet_B: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'String'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Integer'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 360
    Top = 224
    Data = {
      450000009619E0BD010000001800000002000000000003000000450006537472
      696E67010049000000010005574944544802000200140007496E746567657204
      000100000000000000}
    object DataSet_BString: TStringField
      FieldName = 'String'
    end
    object DataSet_BInteger: TIntegerField
      FieldName = 'Integer'
    end
  end
  object DataSource_A: TDataSource
    DataSet = DataSet_A
    Left = 40
    Top = 224
  end
  object DataSource_B: TDataSource
    DataSet = DataSet_B
    Left = 392
    Top = 224
  end
end
