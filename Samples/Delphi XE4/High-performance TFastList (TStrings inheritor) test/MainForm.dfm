object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample'
  ClientHeight = 457
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 438
    Width = 502
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pcTest: TPageControl
    Left = 0
    Top = 0
    Width = 502
    Height = 438
    ActivePage = tsSimpleTest
    Align = alClient
    TabIndex = 0
    TabOrder = 1
    object tsSimpleTest: TTabSheet
      Caption = 'Simple Test'
      object gbSTListClass: TGroupBox
        Left = 251
        Top = 171
        Width = 235
        Height = 90
        Caption = 'List class:'
        TabOrder = 7
        object lbSTListClass: TCheckListBox
          Left = 8
          Top = 24
          Width = 219
          Height = 57
          OnClickCheck = ListBoxClickCheck
          ItemHeight = 13
          Items.Strings = (
            'TStringList'
            'THashedStringList'
            'TFastList')
          TabOrder = 0
        end
      end
      object gbSTSearchCount: TGroupBox
        Left = 8
        Top = 171
        Width = 235
        Height = 90
        Caption = 'Number of search iterations:'
        TabOrder = 5
        object lSTSearchCount: TLabel
          Tag = 5000
          Left = 183
          Top = 32
          Width = 24
          Height = 13
          Caption = '5000'
        end
      end
      object gbSTStringCount: TGroupBox
        Left = 251
        Top = 73
        Width = 235
        Height = 90
        Caption = 'Number of strings in file:'
        TabOrder = 3
        object lSTStringCount: TLabel
          Tag = 100000
          Left = 183
          Top = 32
          Width = 36
          Height = 13
          Caption = '100000'
        end
      end
      object gbSTStringLength: TGroupBox
        Left = 8
        Top = 73
        Width = 235
        Height = 90
        Caption = 'String length:'
        TabOrder = 1
        object lSTStringLength: TLabel
          Tag = 100
          Left = 183
          Top = 32
          Width = 18
          Height = 13
          Caption = '100'
        end
      end
      object mSTDescription: TMemo
        Left = 8
        Top = 8
        Width = 478
        Height = 57
        TabStop = False
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          
            'The sample shows time required to find a number of strings for T' +
            'Strings inheritors. Within the test each '
          
            'list contains equal number of text elements. First of all we mak' +
            'e file containing a certain number of '
          
            'strings. Then we choose number of random strings to find. And fi' +
            'nally we load file into the list and '
          'measure the time spent for searching')
        ReadOnly = True
        TabOrder = 0
      end
      object tbSTStringLength: TTrackBar
        Left = 16
        Top = 92
        Width = 169
        Height = 39
        Max = 5
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 1
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbSTStringLengthChange
      end
      object tbSTSearchCount: TTrackBar
        Left = 16
        Top = 192
        Width = 169
        Height = 39
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 5
        SelEnd = 0
        SelStart = 0
        TabOrder = 6
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbSTSearchCountChange
      end
      object tbSTStringCount: TTrackBar
        Left = 259
        Top = 92
        Width = 169
        Height = 39
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 1
        SelEnd = 0
        SelStart = 0
        TabOrder = 4
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbSTStringCountChange
      end
      object bSTMake: TButton
        Left = 8
        Top = 269
        Width = 235
        Height = 25
        Action = Make
        TabOrder = 8
      end
      object bSTMeasure: TButton
        Left = 251
        Top = 269
        Width = 235
        Height = 25
        Action = STMeasure
        Default = True
        TabOrder = 9
      end
    end
    object tsExtendedTest: TTabSheet
      Caption = 'Extended Test'
      ImageIndex = 1
      object gbETSearchCount: TGroupBox
        Left = 8
        Top = 211
        Width = 235
        Height = 90
        Caption = 'Number of search iterations:'
        TabOrder = 5
        object lETSearchCount: TLabel
          Tag = 5000
          Left = 183
          Top = 32
          Width = 24
          Height = 13
          Caption = '5000'
        end
      end
      object mETDescription: TMemo
        Left = 8
        Top = 8
        Width = 478
        Height = 97
        TabStop = False
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          
            'The simple test on previous page is not objective enough. It sim' +
            'ply shows time needed to search for a '
          
            'number of strings. In practice instead of simple searchings the ' +
            'combination of searchings and '
          
            'modifyings is more important. In this case the disadvantages of ' +
            'THashedStringList appear. '
          
            'THashedStringList overrides "Changed" method and each time it is' +
            ' called THashedStringList marks '
          
            'its hash table as invalid. The following call "IndexOf" or "Inde' +
            'xOfName" method causes the hash '
          
            'table rebuild. Such mechanism has a negative impact on performan' +
            'ce. Here for the certain TStrings '
          'inheritor we measure time spent for searchings and modifyings')
        ReadOnly = True
        TabOrder = 0
      end
      object gbETStringLength: TGroupBox
        Left = 8
        Top = 113
        Width = 235
        Height = 90
        Caption = 'String length:'
        TabOrder = 1
        object lETStringLength: TLabel
          Tag = 100
          Left = 183
          Top = 32
          Width = 18
          Height = 13
          Caption = '100'
        end
      end
      object tbETStringLength: TTrackBar
        Left = 16
        Top = 132
        Width = 169
        Height = 39
        Max = 5
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 1
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbETStringLengthChange
      end
      object gbETStringCount: TGroupBox
        Left = 251
        Top = 113
        Width = 235
        Height = 90
        Caption = 'Number of strings in file:'
        TabOrder = 3
        object lETStringCount: TLabel
          Tag = 1000
          Left = 183
          Top = 32
          Width = 24
          Height = 13
          Caption = '1000'
        end
      end
      object tbETStringCount: TTrackBar
        Left = 259
        Top = 132
        Width = 169
        Height = 39
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 1
        SelEnd = 0
        SelStart = 0
        TabOrder = 4
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbETStringCountChange
      end
      object tbETSearchCount: TTrackBar
        Left = 16
        Top = 232
        Width = 169
        Height = 39
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 5
        SelEnd = 0
        SelStart = 0
        TabOrder = 6
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbETSearchCountChange
      end
      object gbETListClass: TGroupBox
        Left = 8
        Top = 309
        Width = 235
        Height = 90
        Caption = 'List class:'
        TabOrder = 9
        object lbETListClass: TCheckListBox
          Left = 8
          Top = 24
          Width = 219
          Height = 57
          OnClickCheck = ListBoxClickCheck
          ItemHeight = 13
          Items.Strings = (
            'TStringList'
            'THashedStringList'
            'TFastList')
          TabOrder = 0
        end
      end
      object bETMakeFile: TButton
        Left = 251
        Top = 335
        Width = 235
        Height = 25
        Action = Make
        TabOrder = 10
      end
      object bETMeasure: TButton
        Left = 251
        Top = 366
        Width = 235
        Height = 25
        Action = ETMeasure
        Default = True
        TabOrder = 11
      end
      object gbETModificationCount: TGroupBox
        Left = 251
        Top = 211
        Width = 235
        Height = 90
        Caption = 'Modification count:'
        TabOrder = 7
        object lETModificationCount: TLabel
          Tag = 1000
          Left = 183
          Top = 32
          Width = 24
          Height = 13
          Caption = '1000'
        end
      end
      object tbETModificationCount: TTrackBar
        Left = 259
        Top = 232
        Width = 169
        Height = 39
        Max = 5
        Min = 1
        Orientation = trHorizontal
        PageSize = 1
        Frequency = 1
        Position = 1
        SelEnd = 0
        SelStart = 0
        TabOrder = 8
        TickMarks = tmBoth
        TickStyle = tsAuto
        OnChange = tbETModificationCountChange
      end
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 16
    object Make: TAction
      Caption = 'Make File'
      OnExecute = MakeExecute
    end
    object STMeasure: TAction
      Category = 'Simple Test'
      Caption = 'Measure Search Time'
      OnExecute = MeasureExecute
      OnUpdate = MeasureUpdate
    end
    object ETMeasure: TAction
      Category = 'Extended test'
      Caption = 'Measure Search Time'
      OnExecute = MeasureExecute
      OnUpdate = MeasureUpdate
    end
  end
end
