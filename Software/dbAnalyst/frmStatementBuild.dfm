object frmStatementBuild: TfrmStatementBuild
  Left = 380
  Top = 278
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Query Wizard'
  ClientHeight = 399
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 321
    Width = 537
    Height = 2
    Align = alBottom
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 323
    Width = 537
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      537
      35)
    object btnRefreshSchema: TSpeedButton
      Left = 5
      Top = 7
      Width = 24
      Height = 24
      Hint = 'Refresh Schema Information'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF0000000000FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF0000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF0000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
        FF0000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF0000000000FF00FF00FF00FF000000000000000000000000000000
        0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      OnClick = btnRefreshSchemaClick
    end
    object btnNext: TButton
      Left = 146
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Go to next tab'
      Anchors = [akLeft, akBottom]
      Caption = '&Next'
      TabOrder = 1
      OnClick = pnlBottonButtonClick
    end
    object btnCancel: TButton
      Left = 380
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Cancel Information Entry'
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnPrevious: TButton
      Left = 68
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Go to previous tab'
      Anchors = [akLeft, akBottom]
      Caption = '&Previous'
      TabOrder = 0
      OnClick = pnlBottonButtonClick
    end
    object btnHelp: TButton
      Left = 458
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Help'
      Anchors = [akLeft, akBottom]
      Caption = '&Help'
      Enabled = False
      TabOrder = 5
    end
    object btnShowSQL: TButton
      Left = 302
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Help'
      Anchors = [akLeft, akBottom]
      Caption = 'Show &SQL'
      TabOrder = 3
      OnClick = btnShowSQLClick
    end
    object btnFinish: TButton
      Left = 224
      Top = 7
      Width = 75
      Height = 24
      Hint = 'Go to next tab'
      Anchors = [akLeft, akBottom]
      Caption = '&Finish'
      TabOrder = 2
      OnClick = btnFinishClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 73
    Width = 537
    Height = 248
    ActivePage = tbsSort
    Align = alClient
    TabOrder = 0
    OnChange = SetDisplay
    object tbsObjects: TTabSheet
      Caption = 'Objects'
      OnShow = SetDisplay
      DesignSize = (
        529
        220)
      object lblPool: TLabel
        Left = 1
        Top = 2
        Width = 107
        Height = 13
        Caption = 'Database Object Pool:'
      end
      object lblSelected: TLabel
        Left = 266
        Top = 2
        Width = 133
        Height = 13
        Caption = 'Selected Database Objects:'
      end
      object lstPool: TListBox
        Left = 1
        Top = 19
        Width = 232
        Height = 199
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 0
        OnClick = SetState
        OnDblClick = btnObjectsClick
      end
      object lstSelected: TListBox
        Left = 265
        Top = 19
        Width = 232
        Height = 199
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 4
        OnClick = SetState
        OnDblClick = btnObjectsClick
      end
      object btnObjectsAdd: TButton
        Left = 237
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Add Selected Database Objects'
        Caption = '>'
        TabOrder = 1
        OnClick = btnObjectsClick
      end
      object btnObjectsRemove: TButton
        Left = 237
        Top = 95
        Width = 25
        Height = 25
        Hint = 'Remove Selected Objects'
        Caption = '<'
        TabOrder = 2
        OnClick = btnObjectsClick
      end
      object btnObjectsRemoveAll: TButton
        Left = 237
        Top = 124
        Width = 25
        Height = 25
        Hint = 'Remove All Objects'
        Caption = '<<'
        TabOrder = 3
        OnClick = btnObjectsClick
      end
    end
    object tbsColumns: TTabSheet
      Caption = 'Columns'
      ImageIndex = 1
      OnShow = SetDisplay
      DesignSize = (
        529
        220)
      object lblObjectColumns: TLabel
        Left = 1
        Top = 2
        Width = 77
        Height = 13
        Caption = 'Object Columns:'
      end
      object lblSelectedColumns: TLabel
        Left = 265
        Top = 2
        Width = 88
        Height = 13
        Caption = 'Selected Columns:'
      end
      object lstObjectColumns: TListBox
        Left = 1
        Top = 19
        Width = 232
        Height = 199
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnClick = SetState
        OnDblClick = btnColumnsClick
      end
      object lstSelectedColumns: TImageListBox
        Left = 265
        Top = 19
        Width = 232
        Height = 178
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 5
        OnClick = SetState
        OnDblClick = btnColumnsClick
      end
      object btnColumnsAdd: TButton
        Left = 237
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Add Selected Object Columns'
        Caption = '>'
        TabOrder = 1
        OnClick = btnColumnsClick
      end
      object btnColumnsAddAll: TButton
        Left = 237
        Top = 95
        Width = 25
        Height = 25
        Hint = 'Add All Object Columns'
        Caption = '>>'
        TabOrder = 2
        OnClick = btnColumnsClick
      end
      object btnColumnsRemove: TButton
        Left = 237
        Top = 124
        Width = 25
        Height = 25
        Hint = 'Remove Selected Columns'
        Caption = '<'
        TabOrder = 3
        OnClick = btnColumnsClick
      end
      object btnColumnsRemoveAll: TButton
        Left = 237
        Top = 153
        Width = 25
        Height = 25
        Hint = 'Remove All Columns'
        Caption = '<<'
        TabOrder = 4
        OnClick = btnColumnsClick
      end
      object btnDistinct: TCheckBox
        Left = 265
        Top = 201
        Width = 210
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Return Distinct Values'
        TabOrder = 6
        OnClick = btnDistinctClick
      end
      object btnColumnsUp: TBitBtn
        Left = 501
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Move Up'
        Enabled = False
        TabOrder = 7
        OnClick = btnColumnsUpDownClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000000000000000000000000000000000000000000000000000000000000000
          00808080FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
          0000000000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000000000000000000000000000000000000000000000000000008080
          80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
          0000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF000000000000000000000000000000808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000808080FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object btnColumnsDown: TBitBtn
        Left = 501
        Top = 95
        Width = 25
        Height = 25
        Hint = 'Move Down'
        Enabled = False
        TabOrder = 8
        OnClick = btnColumnsUpDownClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000000000000000000000000000808080FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
          0000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000000000000000000000000000000000000000000000000000000000000000
          00808080FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
          0000000000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object btnColumnsEdit: TBitBtn
        Left = 501
        Top = 124
        Width = 25
        Height = 28
        Hint = 'Edit'
        Caption = 'E'
        Enabled = False
        TabOrder = 9
        OnClick = btnColumnsEditClick
      end
    end
    object tbsWhere: TTabSheet
      Caption = 'Conditions'
      ImageIndex = 2
      OnShow = SetDisplay
      object lstConditions: TImageListBox
        Left = 0
        Top = 0
        Width = 454
        Height = 220
        Style = lbOwnerDrawFixed
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = SetState
        OnDblClick = btnConditionsClick
      end
      object pnlConditionButtons: TPanel
        Left = 454
        Top = 0
        Width = 75
        Height = 220
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object btnConditionEdit: TButton
          Left = 4
          Top = 29
          Width = 70
          Height = 25
          Hint = 'Edit Condition'
          Caption = '&Edit'
          Enabled = False
          TabOrder = 1
          OnClick = btnConditionsClick
        end
        object btnConditionDelete: TButton
          Left = 4
          Top = 57
          Width = 70
          Height = 25
          Hint = 'Delete Condition'
          Caption = '&Delete'
          Enabled = False
          TabOrder = 2
          OnClick = btnConditionDeleteClick
        end
        object btnConditionAdd: TButton
          Left = 4
          Top = 1
          Width = 70
          Height = 25
          Hint = 'Add Condition'
          Caption = '&Add'
          TabOrder = 0
          OnClick = btnConditionsClick
        end
      end
    end
    object tbsSort: TTabSheet
      Caption = 'Sort'
      ImageIndex = 3
      DesignSize = (
        529
        220)
      object lblSelectedOrdering: TLabel
        Left = 265
        Top = 2
        Width = 126
        Height = 13
        Caption = 'Selected Column Ordering:'
      end
      object lblObjectOrdering: TLabel
        Left = 1
        Top = 2
        Width = 77
        Height = 13
        Caption = 'Object Columns:'
      end
      object btnOrderAsc: TSpeedButton
        Left = 501
        Top = 130
        Width = 25
        Height = 25
        Hint = 'Ordering is Ascending'
        GroupIndex = 4
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          8080808080808080808080808080808080808080808080808080808080808080
          80808080FFFFFFFFFFFFFFFFFF00008000008000008000008000008000008000
          0080000080000080000080000080000080000080808080FFFFFFFFFFFF0000FF
          0000800000800000800000800000800000800000800000800000800000800000
          80808080FFFFFFFFFFFFFFFFFFFFFFFF0000FF00008000008000008000008000
          0080000080000080000080000080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000FF000080000080000080000080000080000080000080808080FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF00008000008000
          0080000080000080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF0000FF000080000080000080808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF00
          0080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        OnClick = btnOrderAscDescClick
      end
      object btnOrderDesc: TSpeedButton
        Left = 501
        Top = 159
        Width = 25
        Height = 25
        Hint = 'Ordering is Descending'
        GroupIndex = 4
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000080808080FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00008000
          0080000080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF000080000080000080000080000080808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00008000008000008000
          0080000080000080000080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000800000800000800000800000800000800000800000800000808080
          80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00008000008000008000008000008000
          0080000080000080000080000080000080808080FFFFFFFFFFFFFFFFFF000080
          0000800000800000800000800000800000800000800000800000800000800000
          80000080808080FFFFFFFFFFFF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        OnClick = btnOrderAscDescClick
      end
      object lstObjectOrdering: TListBox
        Left = 1
        Top = 19
        Width = 232
        Height = 199
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnClick = SetState
        OnDblClick = btnOrderClick
      end
      object btnOrderAdd: TButton
        Left = 237
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Add Selected Object Columns'
        Caption = '>'
        TabOrder = 1
        OnClick = btnOrderClick
      end
      object btnOrderAddAll: TButton
        Left = 237
        Top = 95
        Width = 25
        Height = 25
        Hint = 'Add All Object Columns'
        Caption = '>>'
        TabOrder = 2
        OnClick = btnOrderClick
      end
      object btnOrderRemove: TButton
        Left = 237
        Top = 124
        Width = 25
        Height = 25
        Hint = 'Remove Selected Order Columns'
        Caption = '<'
        TabOrder = 3
        OnClick = btnOrderClick
      end
      object btnOrderRemoveAll: TButton
        Left = 237
        Top = 153
        Width = 25
        Height = 25
        Hint = 'Remove All Order Columns'
        Caption = '<<'
        TabOrder = 4
        OnClick = btnOrderClick
      end
      object lstSelectedOrdering: TImageListBox
        Left = 265
        Top = 19
        Width = 232
        Height = 199
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 16
        MultiSelect = True
        TabOrder = 5
        OnClick = SetState
        OnDblClick = btnOrderClick
        ImageList = imgSort
      end
      object btnOrderUp: TBitBtn
        Left = 501
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Move Up'
        Enabled = False
        TabOrder = 6
        OnClick = btnOrderUpDownClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000000000000000000000000000000000000000000000000000000000000000
          00808080FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
          0000000000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000000000000000000000000000000000000000000000000000008080
          80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
          0000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF000000000000000000000000000000808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000808080FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object btnOrderDown: TBitBtn
        Left = 501
        Top = 95
        Width = 25
        Height = 25
        Hint = 'Move Down'
        Enabled = False
        TabOrder = 7
        OnClick = btnOrderUpDownClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000000000000000000000000000808080FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
          0000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000000000000000000000000000000000000000000000000000000000000000
          00808080FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
          0000000000000000000000000000000000808080FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000808080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 537
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object imgHeader: TImage
      Left = 4
      Top = 4
      Width = 64
      Height = 65
      AutoSize = True
      Stretch = True
    end
    object lblHeaderDescription: TLabel
      Left = 88
      Top = 23
      Width = 289
      Height = 42
      AutoSize = False
      WordWrap = True
    end
    object lblHeaderTitle: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Build a SQL Query using the Query Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlSQL: TPanel
    Left = 0
    Top = 358
    Width = 537
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 3
    object mmoSQL: TSynEdit
      Left = 4
      Top = 4
      Width = 529
      Height = 33
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      OnClick = SetState
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Width = 0
      Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
      RightEdge = -1
    end
  end
  object imgSort: TImageList
    Left = 328
    Top = 112
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000000080000000800080808000000000000000000000000000000000000000
      0000000000000000000000008000000080000000800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000080000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000000080008080800000000000000000000000000000000000000000000000
      0000000000000000800000008000000080000000800000008000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000080000000800000008000000080000000800000008000000080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000080000000800000008000000080000000800000008000000080008080
      8000000000000000000000000000000000000000000000000000000000000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000800000008000000080000000800000008000808080000000
      0000000000000000000000000000000000000000000000000000000080000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000000080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000008000000080000000800080808000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      8000000080000000800000008000000080000000800000008000000080000000
      8000000080000000800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000080008080800000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000C003FE7F000000008001FC3F00000000
      8003F81F00000000C007F00F00000000E00FE00700000000F01FC00300000000
      F83F800100000000FC7F800300000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
