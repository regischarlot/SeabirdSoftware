object frmMetaData: TfrmMetaData
  Left = 342
  Top = 152
  Width = 565
  Height = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 161
    Top = 0
    Width = 3
    Height = 387
    Cursor = crHSplit
  end
  object tvObjects: TTreeView
    Left = 0
    Top = 0
    Width = 161
    Height = 387
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvObjectsChange
  end
  object lvElements: TListView
    Left = 164
    Top = 0
    Width = 393
    Height = 387
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
        Width = 131
      end
      item
        Caption = 'Name'
        Width = 129
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawItem = lvElementsCustomDrawItem
  end
  object ToolBar2: TToolBar
    Left = 59
    Top = 98
    Width = 184
    Height = 22
    Align = alNone
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Flat = True
    TabOrder = 2
    Transparent = True
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton4'
      ImageIndex = 3
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Caption = 'ToolButton5'
      ImageIndex = 4
    end
    object ToolButton12: TToolButton
      Left = 46
      Top = 0
      Caption = 'ToolButton7'
      ImageIndex = 6
    end
    object ToolButton3: TToolButton
      Left = 69
      Top = 0
      Caption = 'ToolButton6'
      ImageIndex = 5
    end
    object ToolButton13: TToolButton
      Left = 92
      Top = 0
      Caption = 'ToolButton8'
      ImageIndex = 7
    end
    object ToolButton14: TToolButton
      Left = 115
      Top = 0
      Caption = 'ToolButton9'
      ImageIndex = 8
    end
    object ToolButton15: TToolButton
      Left = 138
      Top = 0
      Caption = 'ToolButton10'
      ImageIndex = 9
    end
    object ToolButton16: TToolButton
      Left = 161
      Top = 0
      Caption = 'ToolButton11'
      ImageIndex = 10
    end
  end
  object MainMenu1: TMainMenu
    Top = 40
    object mnuObject: TMenuItem
      Caption = 'Objects'
      GroupIndex = 3
      object mnuObjectNew: TMenuItem
        Caption = 'New'
        object mnuObjectNewObject: TMenuItem
          Caption = 'Object'
        end
        object mnuObjectNewAttribute: TMenuItem
          Caption = 'Attribute'
        end
        object mnuObjectNewField: TMenuItem
          Caption = 'Field'
        end
        object mnuObjectNewDisplay: TMenuItem
          Caption = 'Display'
        end
        object mnuObjectNewSQL: TMenuItem
          Caption = 'SQL'
        end
        object mnuObjectNewMenu: TMenuItem
          Caption = 'Menu'
        end
        object mnuObjectNewRule: TMenuItem
          Caption = 'Rule'
        end
        object TMenuItem
        end
      end
    end
  end
end
