object frmDataImport: TfrmDataImport
  Left = 311
  Top = 175
  BorderStyle = bsToolWindow
  Caption = 'Import Content'
  ClientHeight = 412
  ClientWidth = 385
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
    Top = 375
    Width = 385
    Height = 2
    Align = alBottom
  end
  object PageControl: TPageControl
    Left = 0
    Top = 73
    Width = 385
    Height = 302
    ActivePage = tbsImport
    Align = alClient
    TabOrder = 0
    TabStop = False
    OnChange = PageControlChange
    OnChanging = PageControlChanging
    object tbsFileName: TTabSheet
      Caption = ' Step 1 '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblFileNameLabel: TLabel
        Left = 32
        Top = 56
        Width = 136
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'File Name:'
      end
      object Label1: TLabel
        Left = 32
        Top = 195
        Width = 136
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Commit transactions every :'
      end
      object Label2: TLabel
        Left = 235
        Top = 195
        Width = 35
        Height = 13
        Caption = 'records'
      end
      object lblLogFileLabel: TLabel
        Left = 32
        Top = 118
        Width = 136
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Log File Name:'
      end
      object lblLogFile: TLabel
        Left = 174
        Top = 115
        Width = 179
        Height = 36
        AutoSize = False
        Caption = 'xx'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label4: TLabel
        Left = 32
        Top = 226
        Width = 136
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Missing Objects:'
      end
      object edtCommitPoint: TEdit
        Left = 174
        Top = 192
        Width = 43
        Height = 21
        TabOrder = 4
        Text = '100'
        OnChange = SetState
        OnKeyPress = edtCommitPointKeyPress
      end
      object btnUpDown: TUpDown
        Left = 217
        Top = 192
        Width = 12
        Height = 21
        Associate = edtCommitPoint
        Min = 10
        Max = 10000
        Position = 100
        TabOrder = 6
      end
      object edtFileName: TEdit
        Left = 174
        Top = 53
        Width = 135
        Height = 21
        TabOrder = 1
        OnChange = SetState
      end
      object StaticText1: TStaticText
        Left = 32
        Top = 24
        Width = 222
        Height = 17
        Caption = 'Select a file, and indicate process preferences'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object StaticText2: TStaticText
        Left = 32
        Top = 86
        Width = 216
        Height = 17
        Caption = 'Content import actions will be logged to a file.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object StaticText3: TStaticText
        Left = 32
        Top = 163
        Width = 306
        Height = 17
        Caption = 
          'Import options: adjust parameters to expedite the import process' +
          '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object btnFileName: TButton
        Left = 313
        Top = 53
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 2
        OnClick = btnFileNameClick
      end
      object btnCreateObjects: TCheckBox
        Left = 174
        Top = 225
        Width = 185
        Height = 17
        Caption = 'Create Missing Database Objects'
        TabOrder = 7
        OnClick = SetState
      end
    end
    object tbsObjects: TTabSheet
      Caption = ' Step 2 '
      OnShow = tbsObjectsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        377
        274)
      object lblInitialObjectSelection: TLabel
        Left = 205
        Top = 53
        Width = 78
        Height = 13
        Caption = 'Object Selection'
      end
      object lblObjectPool: TLabel
        Left = 9
        Top = 53
        Width = 55
        Height = 13
        Caption = 'Object Pool'
      end
      object lstObjectPool: TListBox
        Left = 9
        Top = 70
        Width = 160
        Height = 193
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 0
        OnClick = SetState
        OnDblClick = MoveObjectsClick
      end
      object btnAddAll: TButton
        Left = 174
        Top = 87
        Width = 26
        Height = 25
        Caption = '>>'
        Enabled = False
        TabOrder = 1
        OnClick = MoveObjectsClick
      end
      object btnAdd: TButton
        Left = 174
        Top = 116
        Width = 26
        Height = 25
        Caption = '>'
        Enabled = False
        TabOrder = 2
        OnClick = MoveObjectsClick
      end
      object btnRemove: TButton
        Left = 174
        Top = 145
        Width = 26
        Height = 25
        Caption = '<'
        Enabled = False
        TabOrder = 3
        OnClick = MoveObjectsClick
      end
      object btnRemoveAll: TButton
        Left = 174
        Top = 174
        Width = 26
        Height = 25
        Caption = '<<'
        Enabled = False
        TabOrder = 4
        OnClick = MoveObjectsClick
      end
      object lstSelectedObjects: TListBox
        Left = 205
        Top = 70
        Width = 160
        Height = 193
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 5
        OnClick = SetState
        OnDblClick = MoveObjectsClick
      end
      object lblStep2: TStaticText
        Left = 32
        Top = 24
        Width = 430
        Height = 21
        AutoSize = False
        Caption = 'Select the objects to import from the Export Data file.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
    end
    object tbsRename: TTabSheet
      Caption = ' Step 3 '
      ImageIndex = 4
      OnShow = tbsRenameShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        377
        274)
      object lvObjects: TListView
        Left = 9
        Top = 53
        Width = 356
        Height = 174
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Create'
            MaxWidth = 20
            MinWidth = 20
            Width = 20
          end
          item
            Caption = 'Object'
            Width = 298
          end
          item
            AutoSize = True
            Caption = '#Rows'
          end>
        HideSelection = False
        LargeImages = lstImages
        ReadOnly = True
        RowSelect = True
        SmallImages = lstImages
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = SetState
        OnCompare = lvObjectsCompare
        OnDblClick = btnStep3EditClick
        OnResize = lvObjectsResize
      end
      object btnStep3Edit: TButton
        Left = 291
        Top = 235
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Edit'
        Enabled = False
        TabOrder = 1
        OnClick = btnStep3EditClick
      end
      object StaticText4: TStaticText
        Left = 32
        Top = 24
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Revise object creation scripts. Checked items will be created.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object tbsImport: TTabSheet
      Caption = ' Step 4 '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblObjectInProgress: TLabel
        Left = 88
        Top = 170
        Width = 341
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
      end
      object lblObjectName: TLabel
        Left = 174
        Top = 52
        Width = 160
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Color = clBtnFace
        ParentColor = False
      end
      object lblObjectSelection: TLabel
        Left = 9
        Top = 52
        Width = 110
        Height = 13
        Caption = 'Objects to be Imported:'
      end
      object barProgress: TProgressBar
        Left = 9
        Top = 73
        Width = 356
        Height = 13
        TabOrder = 1
      end
      object lvImported: TListView
        Left = 9
        Top = 93
        Width = 356
        Height = 167
        Columns = <
          item
            Caption = ' '
            Width = 18
          end
          item
            Caption = 'Objects'
            Width = 263
          end
          item
            Caption = '# Rows'
            Width = 80
          end
          item
            Caption = 'Imported'
            Width = 80
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SmallImages = frmMain.AlertImageList
        TabOrder = 0
        ViewStyle = vsReport
      end
      object lblStep3: TStaticText
        Left = 32
        Top = 24
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Database content is being imported.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object imgHeader: TImage
      Left = 4
      Top = 4
      Width = 64
      Height = 65
      AutoSize = True
      Stretch = True
    end
    object lblHeader: TLabel
      Left = 88
      Top = 23
      Width = 289
      Height = 42
      AutoSize = False
      Caption = 'Page Context Help'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Import Database Content'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 385
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnPrevious: TButton
      Left = 54
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Previous'
      Enabled = False
      TabOrder = 0
      OnClick = btnPreviousClick
    end
    object btnNext: TButton
      Left = 137
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Next'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnCancel: TButton
      Left = 220
      Top = 7
      Width = 80
      Height = 24
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnHelp: TButton
      Left = 303
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Help'
      Enabled = False
      TabOrder = 3
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'export'
    Filter = 'Export Files (*.export)|*.export|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 8
  end
  object lstImages: TImageList
    Left = 328
    Top = 8
    Bitmap = {
      494C010102000400140010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      0000000000000000000000000000000000000000000000000000CED6DE00CED6
      DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6
      DE00CED6DE00CED6DE0000000000000000000000000000000000CED6DE00CED6
      DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6DE00CED6
      DE00CED6DE00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B0000000000949C
      AD00949CAD00949CAD00949CAD00949CAD00949CAD00949CAD00949CAD00949C
      AD00949CAD00CED6DE000000000000000000000000005A637B0000000000949C
      AD00949CAD00949CAD00949CAD00949CAD00949CAD00949CAD00949CAD00949C
      AD00949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE000000000000000000000000005A637B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CAD00CED6DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A637B005A637B005A63
      7B005A637B005A637B005A637B005A637B005A637B005A637B005A637B005A63
      7B005A637B00000000000000000000000000000000005A637B005A637B005A63
      7B005A637B005A637B005A637B005A637B005A637B005A637B005A637B005A63
      7B005A637B000000000000000000000000000000000000000000000000000000
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
      C003C0030000000080038003000000009FF39FF3000000009FF3939300000000
      9FF39113000000009FF39833000000009FF39C73000000009FF3983300000000
      9FF39113000000009FF39393000000009FF39FF3000000008003800300000000
      8007800700000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
