object frmData2: TfrmData2
  Left = 364
  Top = 166
  ClientHeight = 495
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000001FF99999994FFCCCCCCC0000000000001
    FF99999994FFCCCCCCC4000000000001FF99999994FFCCCCCCC4400000000001
    FF99999994FFCCCCCCC4440000000001FF99999994FFCCCCCCC4444000000001
    FF99999994FFCCCCCCC4444000000001FF99999994FFCCCCCCC4444000000001
    FF99999994FFCCCCCCC4444000000001FF99999994FFCCCCCCC4444000000001
    FFFFFFFFF4FFFFFFFFF4444000000001FFFFFFFFF4FFFFFFFFF7444000000000
    00000000000000000007744000000002FFAAAAAAA3FFBBBBBBB0774000000002
    FFAAAAAAA3FFBBBBBBB3077000000002FFAAAAAAA3FFBBBBBBB3307000000002
    FFAAAAAAA3FFBBBBBBB3330000000002FFAAAAAAA3FFBBBBBBB3333000000002
    FFAAAAAAA3FFBBBBBBB3333000000002FFAAAAAAA3FFBBBBBBB3333000000002
    FFAAAAAAA3FFBBBBBBB3333000000002FFAAAAAAA3FFBBBBBBB3333000000002
    FFFFFFFFF3FFFFFFFFF3333000000002FFFFFFFFF3FFFFFFFFF7333000000000
    2222222227733333333773300000000002222222227733333333773000000000
    0022222222277333333337700000000000022222222277333333337000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFE00001FFE00000FFE000007FE000003FE000001FE000000FE000
    000FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE000
    000FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE000
    000FE000000FF000000FF800000FFC00000FFE00001FFFFFFFFFFFFFFFFF}
  Menu = DataMainMenu
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  Visible = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spltBottom: TdfsSplitter
    Left = 0
    Top = 366
    Width = 567
    Height = 9
    Cursor = crVSplit
    Align = alBottom
    ButtonHighlightColor = clGradientInactiveCaption
    ExplicitTop = 315
    ExplicitWidth = 573
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 567
    Height = 366
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object spltrAnalysis: TdfsSplitter
      Left = 250
      Top = 0
      Width = 9
      Height = 366
      Align = alLeft
      ResizeStyle = rsLine
      ButtonHighlightColor = clGradientInactiveCaption
      ExplicitHeight = 315
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 250
      Height = 366
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 2
      TabOrder = 0
      object tvObjects: TTreeView
        Left = 2
        Top = 2
        Width = 246
        Height = 362
        Hint = '|Database Object Hierarchy'
        Align = alClient
        HideSelection = False
        Images = imgObjects
        Indent = 19
        PopupMenu = popObjects
        ReadOnly = True
        TabOrder = 0
        OnChange = tvObjectsChange
        OnChanging = tvObjectsChanging
        OnCollapsing = tvObjectsCollapsing
        OnDeletion = tvObjectsDeletion
        OnEnter = onSelMove
        OnExit = OnDisplayFieldExit
        OnExpanding = tvObjectsExpanding
        OnExpanded = tvObjectsExpand
      end
    end
    object PageControl: TPageControl
      Left = 259
      Top = 0
      Width = 308
      Height = 366
      ActivePage = tsScripts
      Align = alClient
      TabOrder = 1
      OnChange = OnDisplay
      object tsScripts: TTabSheet
        Caption = 'Script'
        OnResize = tsScriptsResize
        object wbOutput: TWebBrowser
          Left = 0
          Top = 0
          Width = 300
          Height = 338
          Align = alClient
          PopupMenu = popObjectScript
          TabOrder = 0
          OnEnter = onSelMove
          OnExit = OnDisplayFieldExit
          OnBeforeNavigate2 = wbOutputBeforeNavigate2
          OnNavigateComplete2 = wbOutputNavigateComplete2
          ExplicitLeft = 2
          ExplicitTop = -2
          ControlData = {
            4C000000021F0000EF2200000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
      object tsData: TTabSheet
        Caption = 'Data'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 318
        object dbListView: TDBListView
          Left = 0
          Top = 4
          Width = 300
          Height = 334
          BackgroundColor = clWindow
          SelectedColumnColor = clCaptionText
          SelectionColor = clSilver
          CurrentLineColor = clGray
          IsReadOnly = True
          IsAsynchronous = False
          Align = alClient
          AllocBy = 200
          BevelOuter = bvNone
          Columns = <>
          HideSelection = False
          OwnerDraw = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnEnter = onSelMove
          OnExit = OnDisplayFieldExit
          ExplicitHeight = 314
        end
        object lblDataObject: TStaticText
          Left = 0
          Top = 0
          Width = 4
          Height = 4
          Align = alTop
          TabOrder = 1
        end
      end
      object tsGraph: TTabSheet
        Caption = 'Graph'
        ImageIndex = 3
        OnShow = OnGraphShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 318
        object pnlGraph: TGraphPanel
          Left = 0
          Top = 0
          Width = 300
          Height = 338
          Align = alClient
          DoubleBuffered = True
          Color = clCream
          ParentColor = False
          ParentDoubleBuffered = False
          PopupMenu = popObjects
          TabOrder = 0
          OnEnter = onSelMove
          OnExit = OnDisplayFieldExit
          MultiSelect = False
          ImageIcons = imgObjects
          ImageAlerts = lstSmallImages
          Selected = -1
          Caption = 'pnlGraph'
          UseDockManager = False
          ExplicitHeight = 318
        end
      end
    end
  end
  object PageControlBottom: TPageControl
    Left = 0
    Top = 375
    Width = 567
    Height = 120
    ActivePage = tsAlert
    Align = alBottom
    MultiLine = True
    TabOrder = 2
    OnChange = OnDisplay
    OnResize = PageControlBottomResize
    object tsAlert: TTabSheet
      Caption = 'Alerts'
      object lvAlerts: TListView
        Left = 0
        Top = 0
        Width = 559
        Height = 92
        Align = alClient
        Columns = <
          item
            MaxWidth = 20
            MinWidth = 20
            Width = 20
          end
          item
            Caption = 'Object'
            Width = 150
          end
          item
            Caption = 'Message'
            Width = 300
          end
          item
            AutoSize = True
            Caption = 'Description'
          end>
        HideSelection = False
        LargeImages = frmMain.AlertImageList
        ReadOnly = True
        RowSelect = True
        SmallImages = frmMain.AlertImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvAlertsColumnClick
        OnCompare = lvAlertsCompare
        OnEnter = onSelMove
        OnExit = OnDisplayFieldExit
      end
    end
    object tsErrors: TTabSheet
      Caption = 'Script Errors'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvErrors: TListView
        Left = 0
        Top = 0
        Width = 559
        Height = 92
        Align = alClient
        Columns = <
          item
            MaxWidth = 20
            MinWidth = 20
            Width = 20
          end
          item
            Caption = 'Object'
            Width = 150
          end
          item
            Caption = 'Message'
            Width = 300
          end
          item
            AutoSize = True
            Caption = 'Description'
          end>
        HideSelection = False
        LargeImages = frmMain.AlertImageList
        ReadOnly = True
        RowSelect = True
        SmallImages = frmMain.AlertImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnEnter = onSelMove
        OnExit = OnDisplayFieldExit
      end
    end
    object tsGridErrors: TTabSheet
      Caption = 'Data Errors'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lstDataErrors: TListBox
        Left = 0
        Top = 0
        Width = 559
        Height = 92
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object ToolBar: TToolBar
    Left = 119
    Top = 200
    Width = 448
    Height = 22
    Align = alNone
    AutoSize = True
    ButtonWidth = 24
    DrawingStyle = dsGradient
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = frmMain.ImageList
    TabOrder = 1
    Wrapable = False
    object btnRefresh: TToolButton
      Left = 0
      Top = 0
      Action = actEditRefresh
    end
    object btnToolbarSep4: TToolButton
      Left = 24
      Top = 0
      Width = 8
      Caption = 'btnToolbarSep4'
      ImageIndex = 32
      Style = tbsSeparator
    end
    object btnFind: TToolButton
      Left = 32
      Top = 0
      Action = actEditFind
    end
    object btnFindNext: TToolButton
      Left = 56
      Top = 0
      Action = actEditFindNext
    end
    object btnToolbarSep1: TToolButton
      Left = 80
      Top = 0
      Width = 8
      ImageIndex = 23
      Style = tbsSeparator
    end
    object btnExpand: TToolButton
      Left = 88
      Top = 0
      Action = actCollapse
    end
    object btnCollapse: TToolButton
      Left = 112
      Top = 0
      Action = actExpand
    end
    object btnToolbarSep2: TToolButton
      Left = 136
      Top = 0
      Width = 8
      Style = tbsSeparator
    end
    object btnViewContent: TToolButton
      Left = 144
      Top = 0
      Action = actObjectsDisplay
      DropdownMenu = popViewContent
      PopupMenu = popViewContent
      Style = tbsDropDown
    end
    object btnToolbarSep3: TToolButton
      Left = 183
      Top = 0
      Width = 8
      ImageIndex = 32
      Style = tbsSeparator
    end
    object btnObjectNew: TToolButton
      Left = 191
      Top = 0
      Action = actObjectCreate
      DropdownMenu = popObjectsCreate
      PopupMenu = popObjectsCreate
      Style = tbsDropDown
    end
    object btnObjectEdit: TToolButton
      Left = 230
      Top = 0
      Action = actObjectEdit
    end
    object btnObjectDelete: TToolButton
      Left = 254
      Top = 0
      Action = actObjectDrop
    end
    object btnToolbarSep5: TToolButton
      Left = 278
      Top = 0
      Width = 8
      Caption = 'btnToolbarSep5'
      ImageIndex = 32
      Style = tbsSeparator
    end
    object btnExecute: TToolButton
      Left = 286
      Top = 0
      Action = actEditExecute
    end
    object btnToolbarSep6: TToolButton
      Left = 310
      Top = 0
      Width = 10
      Caption = 'btnToolbarSep6'
      ImageIndex = 32
      Style = tbsSeparator
    end
    object btnExport: TToolButton
      Left = 320
      Top = 0
      Action = actToolsExport
    end
    object btnImport: TToolButton
      Left = 344
      Top = 0
      Action = actToolsImport
    end
    object btnDumpData: TToolButton
      Left = 368
      Top = 0
      Action = actToolsExportText
    end
    object btnTranslate: TToolButton
      Left = 392
      Top = 0
      Action = actStructureTranslate
    end
    object ToolButton1: TToolButton
      Left = 416
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 32
      Style = tbsSeparator
    end
    object btnHelpMaster: TToolButton
      Left = 424
      Top = 0
      Action = actHelpMaster
    end
  end
  object popObjectScript: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = popObjectScriptPopup
    Left = 112
    Top = 48
  end
  object DataMainMenu: TMainMenu
    Images = frmMain.ImageList
    Left = 32
    Top = 88
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 1
      OnClick = SetState
      object mnuEditUndo: TMenuItem
        Action = actEditUndo
      end
      object mnuEditCut: TMenuItem
        Action = actEditCut
      end
      object mnuEditCopy: TMenuItem
        Action = actEditCopy
      end
      object mnuEditPaste: TMenuItem
        Action = actEditPaste
      end
      object mnuEditSelectAll: TMenuItem
        Action = actEditSelectAll
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actEditFind
      end
      object mnuEditFindNext: TMenuItem
        Action = actEditFindNext
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuEditExecute: TMenuItem
        Action = actEditExecute
        GroupIndex = 4
      end
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      GroupIndex = 4
    end
    object mnuObjects: TMenuItem
      Caption = '&Structure'
      GroupIndex = 4
      object mnuEditRefresh: TMenuItem
        Action = actEditRefresh
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuObjectsExpand: TMenuItem
        Action = actExpand
      end
      object mnuObjectsCollapse: TMenuItem
        Action = actCollapse
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mnuObjectCreateObject: TMenuItem
        Caption = '&Create Object'
        Enabled = False
        Hint = 'Create ...'
        ImageIndex = 62
      end
      object mnuObjectsEditObject: TMenuItem
        Action = actObjectEdit
      end
      object mnuObjectsDeleteObject: TMenuItem
        Action = actObjectDrop
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuStructureCompareCreateSnapshot: TMenuItem
        Action = actStructureCreateSnapshot
        GroupIndex = 4
      end
      object mnuStructureCompareCompare: TMenuItem
        Action = actStructureCompare
        GroupIndex = 4
      end
      object mnuStructureTranslate: TMenuItem
        Action = actStructureTranslate
        GroupIndex = 4
      end
      object N18: TMenuItem
        Caption = '-'
        GroupIndex = 4
      end
      object mnuObjectsScript: TMenuItem
        Action = actObjectsScript
        GroupIndex = 4
      end
      object mnuObjectsOptions: TMenuItem
        Action = actObjectOptions
        GroupIndex = 4
      end
    end
    object Content1: TMenuItem
      Caption = '&Content'
      GroupIndex = 4
      object mnuObjectsContentDisplay: TMenuItem
        Caption = 'View'
        GroupIndex = 4
        Hint = 'Display Object Content'
        ImageIndex = 41
        OnClick = onNull
        object mnuObjectsContentDisplaySQLConsole: TMenuItem
          Action = actObjectsDisplayQuery
        end
        object mnuObjectsContentDisplaySpreadSheet: TMenuItem
          Tag = 1
          Action = actObjectsDisplayGrid
        end
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 4
      end
      object Export1: TMenuItem
        Action = actToolsExport
        GroupIndex = 4
      end
      object Import1: TMenuItem
        Action = actToolsImport
        GroupIndex = 4
      end
      object DumpData1: TMenuItem
        Action = actToolsExportText
        GroupIndex = 4
      end
    end
    object mnuAction: TMenuItem
      Action = actAction
      GroupIndex = 5
    end
    object mnuFavorites: TMenuItem
      Caption = 'F&avorites'
      GroupIndex = 7
      OnClick = mnuFavoritesClick
      object mnuFavoritesHidden: TMenuItem
        Visible = False
      end
    end
  end
  object popObjects: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = OnPreparePopup
    Left = 16
    Top = 48
    object N11: TMenuItem
      Caption = '-'
    end
    object popObjectsCreateObject: TMenuItem
      Action = actObjectCreate
    end
    object popObjectsEditObject: TMenuItem
      Action = actObjectEdit
    end
    object popObjectsDeleteObject: TMenuItem
      Action = actObjectDrop
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object popObjectsRefresh: TMenuItem
      Action = actEditRefresh
    end
    object popObjectsContentDisplay: TMenuItem
      Caption = '&Content'
      Hint = 'Display Object Content'
      ImageIndex = 41
      OnClick = onNull
      object popObjectsContentDisplaySQLConsole: TMenuItem
        Action = actObjectsDisplayQuery
      end
      object popObjectsContentDisplayGrid: TMenuItem
        Tag = 1
        Action = actObjectsDisplayGrid
      end
    end
    object popObjectsContentImport: TMenuItem
      Action = actToolsImport
    end
    object popObjectsContentExport: TMenuItem
      Action = actToolsExport
    end
    object popObjectContentExportText: TMenuItem
      Action = actToolsExportText
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object popObjectsOptions: TMenuItem
      Action = actObjectOptions
    end
  end
  object popViewContent: TPopupMenu
    Images = frmMain.ImageList
    Left = 48
    Top = 48
    object popViewContentSQL: TMenuItem
      Action = actObjectsDisplayQuery
    end
    object popViewContentGrid: TMenuItem
      Tag = 1
      Action = actObjectsDisplayGrid
    end
  end
  object ActionList: TActionList
    Images = frmMain.ImageList
    State = asSuspended
    Left = 144
    Top = 232
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      GroupIndex = 2
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 5
      ShortCut = 16472
      OnExecute = ClipboardAction
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      GroupIndex = 2
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = ClipboardAction
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      GroupIndex = 2
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 4
      ShortCut = 16470
      OnExecute = ClipboardAction
    end
    object actEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      GroupIndex = 2
      Hint = 'Select All|Selects the entire document'
      ImageIndex = 87
      ShortCut = 16449
      OnExecute = ClipboardAction
    end
    object actEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      GroupIndex = 2
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 39
      ShortCut = 16474
      OnExecute = ClipboardAction
    end
    object actEditRefresh: TAction
      Category = 'Edit'
      Caption = '&Refresh'
      GroupIndex = 2
      Hint = 'Refresh'
      ImageIndex = 3
      ShortCut = 116
      OnExecute = onRefresh
    end
    object actToolsExport: TAction
      Category = 'Edit'
      Caption = 'E&xport'
      Enabled = False
      GroupIndex = 2
      Hint = 'Export Content'
      ImageIndex = 43
      OnExecute = onExportContent
    end
    object actToolsImport: TAction
      Category = 'Edit'
      Caption = '&Import'
      Enabled = False
      GroupIndex = 2
      Hint = 'Import Content'
      ImageIndex = 42
      OnExecute = onImportContent
    end
    object actStructureTranslate: TAction
      Category = 'Structure'
      Caption = '&Translate Schema'
      Enabled = False
      GroupIndex = 2
      Hint = 'Translate Schema'
      ImageIndex = 31
      OnExecute = onTranslate
    end
    object actEditExecute: TAction
      Category = 'Edit'
      Caption = '&Execute'
      Hint = 'Execute Selected SQL Statement'
      ImageIndex = 13
      OnExecute = onExecute
    end
    object actObjectsScript: TAction
      Category = 'Objects'
      Caption = '&Script'
      GroupIndex = 2
      Hint = 'Script'
      OnExecute = onNull
    end
    object actObjectsDisplay: TAction
      Category = 'Objects'
      Caption = '&Content'
      GroupIndex = 2
      Hint = 'Display Object Content'
      ImageIndex = 41
      ShortCut = 16453
      OnExecute = onNull
    end
    object actObjectsDisplayQuery: TAction
      Category = 'Objects'
      Caption = '&Text Query'
      GroupIndex = 2
      Hint = 'Display Object Content using the Query Console'
      ImageIndex = 22
      OnExecute = onDisplayContent
    end
    object actObjectsDisplayGrid: TAction
      Tag = 1
      Category = 'Objects'
      Caption = '&Grid Query'
      GroupIndex = 2
      Hint = 'Display Object Content using the Grid Console'
      ImageIndex = 47
      OnExecute = onDisplayContent
    end
    object actAction: TAction
      Category = 'Edit'
      Caption = '&Action'
      GroupIndex = 4
    end
    object actExpand: TAction
      Category = 'Objects'
      Caption = 'E&xpand'
      GroupIndex = 2
      Hint = 'Expand'
      ImageIndex = 11
      OnExecute = onExpand
    end
    object actCollapse: TAction
      Category = 'Objects'
      Caption = 'C&ollapse'
      GroupIndex = 2
      Hint = 'Collapse'
      ImageIndex = 12
      OnExecute = onCollapse
    end
    object actGraphContinueDraw: TAction
      Category = 'Edit'
      Caption = '&Continue Draw'
      GroupIndex = 2
      Hint = 'Continue Draw'
    end
    object actGraph2D: TAction
      Caption = '2-Dimensional'
      Checked = True
      GroupIndex = 1
      Hint = 'Continue 2-Dimensional Draw'
    end
    object actGraph3D: TAction
      Caption = '3-Dimensional'
      Enabled = False
      GroupIndex = 1
      Hint = 'Continue 3-Dimensional Draw'
    end
    object actGraph: TAction
      Category = 'Edit'
      Caption = '&Graph'
      OnExecute = onNull
    end
    object actEditFind: TAction
      Category = 'Edit'
      Caption = '&Find'
      Hint = 'Find'
      ImageIndex = 53
      ShortCut = 16454
      OnExecute = actEditFindExecute
    end
    object actEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find &Next'
      Hint = 'Find Next'
      ImageIndex = 54
      ShortCut = 114
      OnExecute = onFindNext
    end
    object actObjectEdit: TAction
      Category = 'Objects'
      Caption = 'E&dit Object'
      Enabled = False
      Hint = 'Edit Object'
      ImageIndex = 61
      ShortCut = 113
      OnExecute = actObjectEditExecute
    end
    object actObjectCreate: TAction
      Category = 'Objects'
      Caption = '&Create Object'
      Enabled = False
      Hint = 'Create Object'
      ImageIndex = 62
      OnExecute = actObjectCreateExecute
    end
    object actObjectDrop: TAction
      Category = 'Objects'
      Caption = 'Drop Object'
      Enabled = False
      Hint = 'Drop (i.e. Destroy) Object'
      ImageIndex = 63
      ShortCut = 46
      OnExecute = OnDropObject
    end
    object actObjectOptions: TAction
      Category = 'Objects'
      Caption = '&Options'
      Hint = 'Display Options'
      OnExecute = onDisplayOptions
    end
    object actEditExecuteAll: TAction
      Caption = 'Execute All'
      Hint = 'Execute All Alert SQL Statements'
    end
    object actToolsExportText: TAction
      Category = 'Objects'
      Caption = 'Export as Text'
      Enabled = False
      Hint = 'Export Text'
      ImageIndex = 66
      OnExecute = onDataDump
    end
    object actStructureCompare: TAction
      Category = 'Structure'
      Caption = 'Compare Schemas'
      OnExecute = onSnapShotCompare
    end
    object actStructureCreateSnapshot: TAction
      Category = 'Structure'
      Caption = 'Create Schema Snapshot'
      OnExecute = onSave
    end
    object actHelpMaster: TAction
      Caption = 'Access Online Help'
      Enabled = False
      Hint = 'Access Top Online Help'
      ImageIndex = 68
      OnExecute = onMasterHelp
    end
  end
  object dlgFind: TFindDialog
    OnFind = onFindNext
    Left = 320
    Top = 216
  end
  object popObjectsCreate: TPopupMenu
    Left = 80
    Top = 48
  end
  object FormEvents: TApplicationEvents
    OnShowHint = FormEventsShowHint
    Left = 32
    Top = 232
  end
  object timerGraph: TTimer
    Enabled = False
    Interval = 300
    OnTimer = OnDisplay
    Left = 528
    Top = 56
  end
  object imgObjects: TImageList
    Left = 144
    Top = 56
    Bitmap = {
      494C010120002400480010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
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
      0000000000000000000000000000000000000000000000000000000000008989
      890070707000707070007070700085858500BDBDBD0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F3F3
      F300E4E4E400DEDEDE00DEDEDE00E1E1E100EAEAEA00F6F6F600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0979700C097
      9700C0979700C0979700C09797005A50500084848400D3D3D300000000000000
      0000000000000000000000000000000000000000000000000000EAEAEA00BEBE
      BE00989898008A8A8A008A8A8A0092929200A4A4A400BDBDBD00D7D7D700EDED
      ED00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF0000008000000080000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      00000000000000000000000000000000000000000000D0979700DDD9D900E1E0
      E000DBD2D200DDCDCD00C5ADAD00B48A8A005F5F5F009C9C9C00B9B9B900BCBC
      BC00C9C9C90000000000000000000000000000000000F6F6F600D2D0C700D5D0
      B700D3C6A900D1C3A500C6BCA200A8A49400787871006868680084848400AEAE
      AE00DDDDDD000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      8000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000808000000000000000000000FFFF000000
      00000000000000808000008080000000000000000000D0979700F3F3F300F2F2
      F200DFDEDE00DBC0C000CCA0A000C09292005D5151006A5E5E00726464006B65
      650073737300A0A0A000000000000000000000000000EAE4D900FFF7D300FFE7
      BE00F6D7B800E2B8AA00E6BCBC00ECC7AF00FFE8C000E6DFBF008E8D80006565
      6500A1A1A100E3E3E30000000000000000000000000000000000000000000000
      00000000FF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000080800000808000FFFFFF000000
      00000080800000FFFF0000000000000000000000000000000000C0979700E0C0
      C000D9C5C500D0979700CC7B7B00BF8A8A00C0979700C0979700C0979700C097
      9700866A6A0069696900B6B6B6000000000000000000E6CCAB00FCECC200F5DC
      C200E1C3C300E8D2D20001990100BFE3B300BFE3B300BFE3B300FFFBDB00B8B8
      A40073737300C9C9C9000000000000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D00000000000000FF000000FF00000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF0000000000008080000000000000000000DDDDDD00AE969600D489
      8900D4898900D4898900C6616100DBC7C700E1E0E000DDD9D900DDCDCD00CCB9
      B900C099990088727200B5B5B5000000000000000000EBC39100F3C89800EDDC
      DC00E8D2D200E8D2D20001990100019901000199010001990100AFDF9B00FDED
      C40077777300B7B7B700F3F3F3000000000000000000C0B0A000FFFFFF00FFF0
      F000FFE8E000F0D8C0004D4D4D000000FF0000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF0000000000000000000000
      0000FFFFFF0000FFFF00008080000000000000000000C6B69300F6B35400F6B3
      5400F6B35400F6B35400F6B35400F6B35400CB797900DFDEDE00D6B0B000CC93
      9300C08F8F00A08A8A000000000000000000F2EDE800FAC48B00FBF1E900F6ED
      ED00EBD7D700E1B3B300019901000199010051B95100CFEAC60041B33B00FCD7
      A40085827900AAAAAA00EDEDED000000000000000000D0B8A000FFFFFF00E0C8
      C000C0B0A000FFF0F0004D4D4D00000000000000FF0000000000000080000000
      0000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008080000000000000000000FFFFFF000000
      000000000000FFFFFF00000000000000000000000000C6B69300FFBC4700FFBC
      4700FFC04E00FFC04E00FFBE4A00F9B14400CB797900D9C5C500D0979700CC6E
      6E00976A6A008A8A8A00CCCCCC0000000000EFDECB00FFDFBD00FFFFFE00FFFF
      FF00D6ACAC00EABCBC009FC68C00BFDDB10041B34100AFDEA600AFDF9E00CFD3
      9200A09380009C9C9C00E7E7E7000000000000000000D0B8A000FFFFFF00FFFF
      FF00FFFFFF00FFF8FF004D4D4D00FFFFFF000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000808080000000
      000000000000808080000000000000FFFF000080800080808000FFFFFF000000
      000000FFFF0000808000000000000000000000000000C6B69300FFCC6700FFCC
      6700FFCC6700FFCC6700FFCC6700FFCA6400CB797900CD848400CD848400CB79
      7900704D4D005B5B5B009090900000000000EED8BE00FFF6E000FFFFF500ECD9
      D500D0A1A100E7BCBC0051AF4700FFFFF50071C671000199010001990100FFE1
      B400A69680008F8F8F00E0E0E0000000000000000000F0A89000F0A88000F098
      70004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D00000000000000
      00000000FF000000000000000000000000000000000000000000C0C0C0008080
      800080808000FFFFFF0000000000008080000000000000000000FFFFFF000000
      00000000000000FFFF00000000000000000000000000C6B69300F7CF8500FFD9
      8100FFD98100FFD98100FFD57A00FFCC6700CB797900FFB33400FFB33400F6B3
      5400F6B35400CB79790070707000D1D1D100F7E2C000FFFFE900FFFFEC00ECD9
      CF00EAD4CD00E2CCCC00AAC59500119F110021A621000199010001990100FFEA
      C200D1C6A90083838300DADADA000000000000000000F0A89000FF800000FF80
      0000C0B0A000FFFFFF00FFF0F000FFE8E000F0D8C0004D4D4D00000080000000
      80000000FF00000000000000000000000000000000008080800000000000FFFF
      FF0080808000FFFFFF00FFFFFF0000000000FFFFFF00C0C0C000808080000000
      00000000000000000000000000000000000000000000C6B69300F7CF8500FFE0
      8F00FFE59900FFE59900FFE59900F7CF8500CB797900FFCC6700FFCC6700FFC7
      5D00FFC75D00CB79790070707000D1D1D100FFEFC900FFFFE000FFFFE300FFFF
      E700FFFFEB00EAF8FA00EFE6E800ACD3A300ACD3A30071C6710001990100FFF7
      DA00D0CCB20076767600D4D4D4000000000000000000F0A89000F0A88000F098
      7000D0B8A000FFFFFF00E0C8C000C0B0A000FFF0F0004D4D4D00000000000000
      FF000000FF0000000000000000000000000000000000C0C0C000FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000000000000000000000000000000000000C6B69300F7CF8500FFE0
      8F00F4E7B100FFFCC600FFE79C00EDCA9100CB797900FFCC6700FFCC6700FFCC
      6700FFCC6700CB79790070707000D1D1D100FFF2CD00FFFFDE00FFFFDE00FCFF
      E000E5FFF400E4FFFA00DFFCFC00FDFAFA00FDFAFA00FFFFFD00FFFFF300FFFE
      EF00FFFFDE0070707000D1D1D100000000000000000000000000000000008080
      8000D0B8A000FFFFFF00FFFFFF00FFFFFF00FFF8FF004D4D4D00000000000000
      0000000000008080800000000000000000008080800000000000808080000000
      000000000000FFFFFF0000000000000000008080800000000000000000000000
      00000000000000000000000000000000000000000000CBBABA00CBBABA00CBBA
      BA00CBBABA00CFA68900CFA68900CFA68900CB797900FFE59900FFE59900FFDF
      8C00FFD17100CB79790070707000D1D1D100FDE7C000FFE0BC00FFE0BC00D9EE
      D600D9EED600D9EED600D9EED600D9EED600D9EED600E1EEF000FCE4B700FFFB
      D800FFFFDE0070707000D1D1D100000000000000000000000000000000000000
      0000F0A89000F0A88000F0987000E0906000E0885000E0784000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000FFFFFF00C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6B69300F7CF8500FFE08F00FFE89F00FFEAA300FFE5
      9900FBDA8E00CB79790070707000D1D1D10000000000FDF1E400F9EEE200A9E2
      FF00DFFFFF00DFFFFF00DFFFFF00DFFFFF00E5FFFF00A9E2FF00F9C38900FCCD
      9900FFF5D00089898900DADADA00000000000000000000000000000000000000
      0000F0A89000FF800000FF800000FF800000FF800000D0683000000000000000
      0000000000000000000000000000000000000000000080808000C0C0C0000000
      0000FFFFFF00808080000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6B69300F7CF8500FFE08F00F4E7B100FFFCC600F5DC
      9D00EDCA9100CB7979009595950000000000000000000000000000000000ECF9
      FF00A9E2FF00A9E2FF00A9E2FF00BBFCFF00C0FFFF00A9E2FF00FBC68F00FECA
      9500FFCF9D00C5C5C500EFEFEF00000000000000000000000000000000000000
      0000F0A89000F0A88000F0987000E0906000E0885000E0805000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CBBABA00CBBABA00CBBABA00CBBABA00CFA68900CFA6
      8900CFA68900CB79790000000000000000000000000000000000000000000000
      0000000000000000000000000000A9E2FF00A9E2FF00A9E2FF00E6D2BE00F1D8
      BF00F2E5D800F4F4F40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C891
      8A00B74C2A00DE543000E25D3C00D6654A00B1513300B0492800B2523B009470
      6600000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF0000008000000080000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0A090006048300060483000604830006048300060483000604830006048
      300060483000000000000000000000000000000000000000000000000000C068
      5800E9573400F6605900FB706700F4754E00F28F5D00ED7B5D00FF6E5800CC5A
      2B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      8000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000B0A09000E0C8C000D0C0B000D0B8B000D0B8B000C0A89000B0A09000B0A0
      900060483000000000000000000000000000000000000000000000000000DC8E
      7D00E1593C00FF816D00F7806700D2784F00F6CE9A00E7A17C00FF715A00F169
      4C00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0A09000FFFFFF00FFFFFF00FFF8FF00D0B8B000F0D8C000F0D0B000B0A0
      9000604830000000000000000000000000000000000000000000000000000000
      0000EA745100FF876700EF7B5800FCA07100FFDBA400E8A77B00FA795800EA7A
      5000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF0000000000000000000000FF000000FF00000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0A090004048D0001018C0001018C000D0C0B000F0D8D000F0D8C000C0A8
      9000604830000000000000000000000000000000000000000000000000000000
      0000000000008F452D007A3736004B386900413E6C009A535000DF7256000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF0000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0A09000E0D0D000D0C8C000D0C0C000D0C0B000C0B0A000C0A8A000C0A8
      9000604830000000000000000000000000000000000000000000000000000000
      0000585F50000E09180000104E00002D9900022DA600171F790078628C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000080000000000000000000000000000000FF0000000000000080000000
      0000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D0B0A000FFFFFF00FFFFFF00FFFFFF00E0D0C000FFF8F000FFF0F000C0B0
      A000604830000000000000000000000000000000000000000000000000006252
      53001E0D0A0005103C00154799002451B2001A4AB400124EB20019346C000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D0B8A0004048D0001018C0001018C000E0D0C000FFF8FF00FFF8F000D0B8
      B00060483000000000000000000000000000000000000000000000000000201E
      1D00291A1700133269001C68D9001E6ADA002A6EC7001D6CD3000E4295000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000FF000000FF00000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F0A89000F0A88000F0A08000E0906000E0805000E0784000E0704000E070
      4000D06030000000000000000000000000000000000000000000000000002629
      27003033230024395800347CE1002887FC003489EC002587F2001761AF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000800000008000000080000000FF000000FF00000080000000
      80000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F0A89000FFC0A000FFB89000FFB09000FFA88000F0A07000F0987000F098
      6000D06830000000000000000000000000000000000000000000000000003D46
      4300284040001F3C410018529D00257BEB003D9DF700339BFF001562B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000F0A89000F0A89000F0A88000F0987000E0906000E0886000E0784000E078
      4000E07040000000000000000000000000000000000000000000000000007B86
      83004F4447005C4B4E005F5A570039445A002130780019307B00263653000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005E65580065614E0099888B00A598A6006A6E6800393F3400949599000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000837E7B005D645D004D595B006C616900AFA4A600000000000000
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
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
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
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000000000000000000000000000000000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000808000008080
      0000808000008080000080000000000000000000000080800000808000008080
      0000808000008000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      570000000000000000000000000000000000000000000000000080800000FFFF
      0000808000008080000080000000000000000000000080800000FFFF00008080
      0000808000008000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D8500000000000000000000000000000000000000000080800000FFFF
      0000FFFF00008080000080000000000000000000000080800000FFFF0000FFFF
      0000808000008000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000008080
      0000808000008080000000000000000000000000000000000000808000008080
      0000808000000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000000000
      0000000000008080000080800000808000008080000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000000000
      00000000000080800000FFFF0000808000008080000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080800000FFFF0000FFFF00008080000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080800000808000008080000000000000000000000000
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
      0000000000000000000000000000000000000000660000000000000000000000
      000000000000000000000000000000000000000000000000000080808000557F
      7F0080808000555F7F00557F7F00555F55008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000660000000000000066000066990000006600000000000000
      00000000000000000000000000000000000000000000557F5500AA5F5500AA3F
      2A00555F2A00AA5F2A00AA3F2A00555F5500555F5500555F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000660066CCFF0000006600006699000066990066CCFF00000066000000
      00000000660000000000000000000000000000000000AA5F5500FF5F2A00FF5F
      5500FF7F5500FF5F5500AA7F2A00FF5F5500AA5F2A00555F2A004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000008000000080000000800000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      660066CCFF0066CCFF0066CCFF00000066000066990066CCFF0066CCFF000000
      66000066990000006600000000000000000000000000FF7F5500FF5F5500FF7F
      5500FF7F7F00FF7F5500F0CAA600FF5F5500FF5F5500AA7F2A00A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A0004D4D4D0000000000C0C0C000FFFF0000C0C0
      C00000000000000000000000000000000000C0C0C000FFFF0000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000080
      000000FF00000080000000800000800000008000000080000000800000000080
      00000080000000000000000000000000000000000000000000000000660066CC
      FF0066CCFF0099CCFF0066CCFF0066CCFF000000660066CCFF00000066000066
      9900006699000066660000006600000000000000000000000000FF5F5500FF7F
      5500FF7F5500F0CAA600F0CAA600FF7F5500FF5F5500C0C0C000B2B2B200A4A0
      A000A4A0A000A4A0A000A4A0A0004D4D4D0000000000FFFF0000C0C0C000FFFF
      000000000000000000000000000000000000FFFF0000C0C0C000FFFF00000000
      000000000000000000000000000000000000000000000000000000800000C0C0
      C00000FF00000080000000800000FF000000FF00000000800000008000000080
      000000800000008000000000000000000000000000000000660066CCFF0066CC
      FF0066CCFF0066CCFF0066CCFF0066CCFF000000000000000000006699000066
      66000066990000669900006666000000800000000000000000004D4D4D00AA5F
      5500551F5500003F5500AA5F5500AA5F5500C0C0C000C0C0C000C0C0C000A4A0
      A000B2B2B200A4A0A000A4A0A0004D4D4D0000000000FFFFFF00FFFF0000C0C0
      C00000000000000000000000000000000000FFFFFF00FFFF0000C0C0C0000000
      0000000000000000000000000000000000000000000000000000808080000080
      00000080000080800000FF000000FF000000FF0000000080000000FF00000080
      0000008000000080000000000000000000000000660066CCFF0066CCFF0099CC
      FF0066CCFF0066CCFF0066CCFF00000000000033330000333300000000000066
      9900006666000066990000669900006666000000000000000000001F2A000000
      0000001F7F00553FAA00001FAA00001F5500555F5500A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A0004D4D4D0000000000FFFFFF00FFFFFF00FFFF
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF00000000
      000000000000000000000000000000000000000000000000000080808000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000C0C0C00000FF
      0000008000000080000000000000000000000000660066CCFF0066CCFF0066CC
      FF0066CCFF0066CCFF0000000000003333000033330000333300003333000000
      00000066990000669900006666000066990000000000553F2A00001F2A00003F
      5500003FAA00005FAA00005FAA00001FAA00555F5500FFECCC00C0C0C000A4A0
      A000C0C0C000B2B2B200A4A0A0004D4D4D008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000080808000FF00
      0000FF000000FF000000FF000000FF000000FF00000080000000FF000000C0C0
      C00000FF0000008000000000000000000000000000000000660066CCFF0066CC
      FF0066CCFF000000000000333300003333000033330000333300003333000033
      33000000000000666600006699000000660000000000001F2A00551F2A00001F
      AA00005FD400557FD400005FD400005FAA00555F5500FFECCC00FFECCC00A4A0
      A000C0C0C000C0C0C000A4A0A0004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FF00
      0000FF0000000080000000800000FF000000FF000000FF00000080000000FF00
      00000080000000800000000000000000000000000000000000000000660066CC
      FF00000000000033330000333300003333000033330000333300003333000000
      00006666990000008000000066000000000000000000551F2A00003F2A00555F
      5500009FFF00559FFF00559FFF00555FD400555F5500A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A0004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000008000000080000000FF000000800000FF00000000800000FF0000008000
      0000FF0000000000000000000000000000000066660000669900003366000000
      0000006699000000000000333300003333000033330000333300003333000000
      000066CCFF0000006600000000000000000000000000555F7F00553F5500553F
      2A00553F5500003FAA00005FD400005F7F00F0FBFF00F0FBFF00FFECCC00A4A0
      A000FFECCC00C0C0C000A4A0A0004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C00000FF000000FF000000FF0000008000000080000000800000FF00
      00008000000080000000000000000000000000669900F0FBFF0099FFFF000000
      66000066990000666600000000000033330000333300003333000000000066CC
      FF00000066000000000000000000000000000000000000000000553F2A00555F
      7F00A4A0A000A4A0A000553F5500553F5500F0FBFF00F0FBFF00F0FBFF00A4A0
      A000FFECCC00FFECCC00A4A0A0004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800080808000FFFFFF00C0C0C00000FF000000800000008000000080
      000080000000000000000000000000000000000000000066990099FFFF00F0FB
      FF0000006600006699000066990000000000000000000000000066CCFF000000
      66000000000000000000000000000000000000000000000000004D4D4D00555F
      5500555F5500553F5500555F5500FF800000FF800000FF800000FF800000FF80
      0000FF800000FF800000FF8000004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000808080000000
      00000000000000000000000000000000000000000000000000000066660099FF
      FF00F0FBFF0000006600006699000066660066CCFF0066CCFF00000066000000
      00000000000000000000000000000000000000000000000000004D4D4D00FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF800000FF800000FF8000004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000066
      990099FFFF00F0FBFF00000066000066990066CCFF0000006600000000000000
      00000000000000000000000000000000000000000000000000004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000006699000000660000006600000080000000660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      99009999990000000000000000009999990000FFFF00CCCCCC0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000080800000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900000000000000
      000000000000000000000000000000FFFF0099999900000000009999990000FF
      FF00000000000000000000000000000000000000000000000000000000008080
      80000080800000808000FFFFFF0000808000FFFFFF000080800000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000000000000000000000000000000000000099999900000000000000
      000000000000000000000000000000FFFF0000000000FFFF00000000000000FF
      FF00000000000000000000000000000000000000000000000000000000008080
      80000080800000FFFF00FFFFFF00FFFFFF000080800000FFFF00008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000048568900263A8B001327850023296C0035375C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000008000000000000000000000000000000099999900000000000000
      00000000000000000000000000000000000000FFFF000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000808000FFFFFF0000808000FFFFFF000080800000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000003A55AC00042CBC00001ECD000030CF000A2FB50012289000191F
      5700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000800000000000000000000000000000009999990000000000CCCC
      CC00CCCCCC000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000FFFF00FFFFFF00FFFFFF000080800000FFFF00008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00005768A0000939D4002356F6000C48FF000B3EE100244ED3000C31B7001222
      86005B5D85000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000000000000000000000000
      0000800000000000000000000000000000000000000099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000808000FFFFFF0000808000FFFFFF000080800000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000395AC500134EFE00AEC1FE00BBCCFF001D55FF00BCCDFF00A2B4ED000A2F
      B600242D6D000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000800000000000
      000000000000800000000000000000000000000000009999990000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC000000000099999900000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000FFFF00FFFFFF00FFFFFF000080800000FFFF00008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00004C70E5003E6CFC003868FC00A4BBFF00EDF1FF00A5BCFF000C48FF000034
      D200132985000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000008000
      0000800000000000000000000000000000000000000099999900000000000000
      0000000000000000000000000000000000000000000099999900000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000808000FFFFFF0000808000FFFFFF000080800000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      00005472D5008BA7FF006E92FF00C9D6FE00DDE6FF00BBCCFF00124DFF000029
      D70021368F000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000008000000000000000000000000000
      000000000000000000000000000000000000000000009999990000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC000000000099999900000000005F99
      62006299600000990000009A0000000000000000000000000000000000008080
      80000080800000FFFF00FFFFFF00FFFFFF000080800000FFFF00008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00006679B4007897F800EAEFFF00C5D3FF00426FFC00A6BBFC00BFCFFF001235
      CA006E7BB0000000000000000000000000000000000000000000000000008000
      000000000000800000000000000000000000800000000000000000000000FF00
      0000000000000000000000000000000000000000000099999900000000000000
      00000000000000000000000000000000000000000000999999006199640066CC
      660062FE630066CC660000990000009700000000000000000000000000008080
      80000080800000808000FFFFFF0000808000FFFFFF0000808000FFFFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000006882D8007797F7008BA7FF003968FC002F60F8002C5FFB00324E
      AA00000000000000000000000000000000000000000000000000000000008000
      0000000000000000000080000000800000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999009999990000000000609A600065FF
      640062FF610062FE630066CC6600019900000000000000000000000000008080
      800000808000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000007488CE005071DD005174E5004367D7007388CD000000
      0000000000000000000000000000000000000000000000000000000000008000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      00009B01A000FD65FE00FF65FF00FE65FF009C009B009A009B0060996200FFFE
      FF0062FE630061FF640066CC6600629A5F000000000000000000000000008080
      8000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      000099009A00FF65FF00FF64FE00FF64FE0098029C009F009C009D009E006099
      62000000000064FF610000970000649862000000000000000000000000000000
      000080808000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      00000000000000000000FF000000FF000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009D009E00FF64FE00FFCCFF00FFCCFF009D009E009A009D009D019C000000
      0000659A61006299620062986300000000000000000000000000000000000000
      0000000000008080800080808000808080008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009D019B00FFCCFF00FFCCFF00FFCCFF009A019C009F009C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C009D009D019C009C009D0000000000000000000000
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
      0000000000008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808000008080000080800000808000008080000080800000808000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080000000000000FFFF0000FFFF0000FFFF0000FFFF0000808000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00080808000FFFF0000FFFF0000FFFF000080808000808080008080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080800000FFFF0000FFFF0000FFFF000080800000008080000080
      800080808000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080808000FFFF0000808080008080800000FFFF008080
      8000008080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080800000C0C0C0008080000000FFFF0000FFFF0000FF
      FF00008080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000000FF000000FF0080808000808000008080800000FFFF0000FFFF0000FF
      FF00008080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000000FF000000FF000000FF00808080000000000000FFFF0000FFFF008080
      800000808000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000000FF000000FF000000FF000000FF000080800000808000008080000080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000000FF000000FF000000FF000000FF008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080008080800000000000000000000000
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
      000000000000000000000000000000000000000000000000000000000000F8F8
      F800DEDEDD00D1D0D000CBCACA00C8C8C700C8C8C800CCCCCC00D4D3D300E6E5
      E500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D00000000000000000000000000000000000000
      000000000000000000000000000000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D000000000000000000E5E5E500A8A4A200A097
      9400B2A9A400D6CDCA00ECE6E400F3EEEC00EDE8E500DBD2CE00B8AAA4009F92
      8D009E979400B8B6B500F7F7F70000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D0000000000000000004D4D4D00A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0B0A000FFFFFF00FFF0
      F000FFE8E000F0D8C0004D4D4D0000000000F2F2F2008F868200B1998E00CDBB
      B100E0D6D200F2EDEB00F8F5F400FAF8F700F6F4F300E9E3E000CCBEB800C0AB
      A200CBB7AD00AF9B9100A8A4A30000000000000000004D4D4D00A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D0000000000000000004D4D4D00F0FBFF00F0FB
      FF00A4A0A000FFECCC00FFECCC00C0C0C000C0C0C000B2B2B200A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0B8A000FFFFFF00E0C8
      C000C0B0A000FFF0F0004D4D4D0000000000EFEFEF00998A8200BAA29700B4A1
      9800D0C5C100EAE6E500F6F5F400FBFAF900F9F8F800EBE8E600C5B7B200A894
      8900B8A39900CBB4A900A59C970000000000000000004D4D4D00F0FBFF00F0FB
      FF00A4A0A000FFECCC00FFECCC00C0C0C000C0C0C000B2B2B200A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D0000000000000000004D4D4D00F0FBFF00F0FB
      FF00A4A0A000FFECCC00FFECCC0000000000C0C0C000C0C0C000A4A0A000B2B2
      B200A4A0A000A4A0A0004D4D4D00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0B8A000FFFFFF00FFFF
      FF00FFFFFF00FFF8FF004D4D4D0000000000F5F5F400B19F9700AD968B00B5A4
      9D00D8D0CC00ECE8E600F1EFED00F3EFED00F2EFED00EDE8E700D4CAC600B9A7
      9F00B9A69D00BCA89D00C1B3AC0000000000000000004D4D4D00F0FBFF00F0FB
      FF00A4A0A000FFECCC00FFECCC00C0C0C000C0C0C000C0C0C000A4A0A000B2B2
      B200A4A0A000A4A0A0004D4D4D0000000000000000004D4D4D00A4A0A000A4A0
      A000A4A0A000A4A0A00000000000046B0B0000000000A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D00000000004D4D4D004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D0000000000C078500080584000F0A89000F0A88000F098
      7000E0906000E0885000E078400000000000F9F8F700BAAAA200C1B0A800BDAD
      A500D1C6C100E4DDDA00EBE7E400EDE9E700ECE8E600E4DDDA00CCBEB800B9A7
      9E00C0AFA600C9B9B100C5B8B20000000000000000004D4D4D00A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D0000000000000000004D4D4D00FFFFFF00FFFF
      FF00A4A0A00000000000046B0B002DB85100046B0B0000000000A4A0A000C0C0
      C000B2B2B200A4A0A0004D4D4D0000000000C0B0A000FFFFFF00FFF0F000FFE8
      E000F0D8C0004D4D4D00906040000000000000000000F0A89000FF800000FF80
      0000FF800000FF800000D068300000000000FBFAFA00B7A69E00B39E9400B19F
      9600CEC3BE00E8E3E100F5F3F200FAF9F800F9F8F700ECE8E600C7BAB400AA94
      8A00B6A19700BDA99F00C6BAB40000000000000000004D4D4D00FFFFFF00FFFF
      FF00A4A0A000F0FBFF00FFECCC00FFECCC00FFECCC00C0C0C000A4A0A000C0C0
      C000B2B2B200A4A0A0004D4D4D0000000000000000004D4D4D00FFFFFF00FFFF
      FF0000000000046B0B0027AC450046E37A0035CA5C00046B0B0000000000C0C0
      C000C0C0C000A4A0A0004D4D4D0000000000D0B8A000FFFFFF00E0C8C000C0B0
      A000FFF0F0004D4D4D00000000000000000000000000F0A89000F0A88000F098
      7000E0906000E0885000E080500000000000F5F3F300AA968D00AC958A00AD9B
      9200D0C5C200EAE7E500F7F6F500FBFAF900FAF9F800EFEBEA00CABFBA00AB96
      8C00B49F9500BCA89D00C1B3AD0000000000000000004D4D4D00FFFFFF00FFFF
      FF00A4A0A000F0FBFF00F0FBFF00FFECCC00FFECCC00FFECCC00A4A0A000C0C0
      C000C0C0C000A4A0A0004D4D4D0000000000000000004D4D4D00A4A0A0000000
      0000046B0B001D99350041DE750035CC5D002BC24D001AA73200046B0B000000
      0000A4A0A000A4A0A0004D4D4D0000000000D0B8A000FFFFFF00FFFFFF00FFFF
      FF00FFF8FF004D4D4D0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F4F300AD9A9100AA938800AD9B
      9200D0C7C300ECE8E600F8F6F600FAFAFA00FAF9F800EFEBEA00CABEB900AB95
      8C00B49F9500BCA89E00C3B5AE0000000000000000004D4D4D00A4A0A000A4A0
      A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0A000A4A0
      A000A4A0A000A4A0A0004D4D4D0000000000000000004D4D4D0000000000046B
      0B00107D1D0036CE600032C95A0027BC47001DB0360014A5270009871300046B
      0B0000000000A4A0A0004D4D4D0000000000F0A89000F0A88000F0987000E090
      6000E0885000E0784000D080500085634F0000000000000000004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D00F7F5F500B19F9700B39E9500BDAF
      A800D8CFCC00E7E1DE00EBE5E300EBE6E300EAE5E200E6DFDC00D3C8C300C0B0
      AA00C4B3AA00C2B0A700C3B6B00000000000000000004D4D4D00FFFFFF00FFFF
      FF00A4A0A000F0FBFF00F0FBFF00F0FBFF00F0FBFF00FFECCC00A4A0A000FFEC
      CC00C0C0C000A4A0A0004D4D4D00000000000000000000000000046B0B00046B
      0B00046B0B00046B0B0015932A001FB5380012A12300046B0B00046B0B00046B
      0B00046B0B00000000004D4D4D0000000000F0A89000FF800000FF800000FF80
      0000FF800000D06830000000000000000000C0785000825D4700C0B0A000FFFF
      FF00FFF0F000FFE8E000F0D8C0004D4D4D00FBFAF900BAAAA300B09A9000AF9C
      9400D0C6C100EBE6E500F6F4F300F8F6F700F8F5F500EBE6E400C8BAB400AB95
      8C00B49E9400BCA99F00C4B8B20000000000000000004D4D4D00FFFFFF00FFFF
      FF00A4A0A000FFFFFF00FFFFFF00F0FBFF00F0FBFF00F0FBFF00A4A0A000FFEC
      CC00FFECCC00A4A0A0004D4D4D0000000000000000004D4D4D00000000000000
      000000000000000000000F871F0013A92600098E12002D975400000000000000
      000000000000FF8000004D4D4D0000000000F0A89000F0A88000F0987000E090
      6000E0885000E080500000000000000000000000000000000000D0B8A000FFFF
      FF00E0C8C000C0B0A000FFF0F0004D4D4D00F6F4F300AB978E00A88F8500AF9C
      9500D5CDC900F0EDEB00F8F7F700FBFAFA00FAF9F800EDEAE800C8BBB600AA94
      8B00B6A19800BEAAA000BCADA70000000000000000004D4D4D00FF800000FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF800000FF8000004D4D4D0000000000000000004D4D4D00FF800000FF80
      0000FF800000FF8000000A8014000A9A1400047B0B00FF800000FF800000FF80
      0000FF800000FF8000004D4D4D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D0B8A000FFFF
      FF00FFFFFF00FFFFFF00FFF8FF004D4D4D00F7F6F500AF9D9400A78E8400AE9C
      9500D7D0CC00F2EFEF00FCFCFB00FEFEFE00FCFCFB00EDEAE800C6B9B300A993
      8A00B6A29800BDA9A000BDAEA80000000000000000004D4D4D00FF800000FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF800000FF8000004D4D4D0000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D0005830C0006910D00036811004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F0A89000F0A8
      8000F0987000E0906000E0885000E0784000F9F8F700B4A19900AF998F00B5A4
      9B00D2C8C300E4DDD800E9E3E000EAE3E100E8E2DF00DED6D200C4B6AF00B19D
      9400BDAAA100C4B2A900BAAAA30000000000000000004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D4D004D4D
      4D004D4D4D004D4D4D004D4D4D00000000000000000000000000000000000000
      00000000000058B27E0005860D00047E0A001E812D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F0A89000FF80
      0000FF800000FF800000FF800000D0683000FDFDFD00C9BEB800B6A49B00CFC3
      BC00DBD1CC00DDD5CF00DDD3CF00DDD2CD00DDD3CE00DDD4CF00DFD5D100DBD2
      CD00D3C8C200C0B1AA00D7CECA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039AC7E00057F0B0004800B00157F2E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F0A89000F0A8
      8000F0987000E0906000E0885000E080500000000000FBFAFA00EDE9E700E0DA
      D600D6CDC900D0C5C100CEC5C000CFC6C100D0C6C100D1C8C400D4CBC700DAD3
      CF00E4DEDB00F1EDEC00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002C9D
      67000369080004740A002C9C6700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000E07FE03FFC0FFF8FC03FC00FF807FE89
      80078007F007FC0080038003E003FC01C00180038003FE00800180018003FC00
      800300018003F800800100018003C0018001000180038001800000018003001B
      800000018003001F80000001E023001F80000001F01F803FFC008001F03F803F
      FC01E001F03FD17FFC03FE03FFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FC0FFFFFFFFFE00FF807E00FF007E00FF1E7DFC7F007E00FE233C017F007F00F
      E003F637F007F81FE003F7F7F007F01FE003F637F007E01FE733F7F7F007E01F
      E003F637F007E01FE003F7F1F007E01FE003F005F007E01FE7E3F803FFFFF01F
      FFDFFFFFFFFFF83FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFC1FFC1FFC1FFC1FF80FF80FF80FF80FF007F007F007F007
      F007F007F007F007F007F007F007F007F007F007F007F007F007F007F007F007
      F80FF80FF80FF80FFC1FFC1FFC1FFC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFE3C7FC1FFC1FFC1FC183F80FF80FF80FC183F007F007F007
      C183F007F007F007E3C7F007F007F007FFFFF007F007F007FC7FF007F007F007
      F83FF80FF80FF80FF83FFC1FFC1FFC1FF83FFFFFFFFFFFFFFC7FFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FC07FFFFFFC1FFA3F803F
      FFFFF007F01780008F1FE003E0038000060FE003C001C000060FC0018000C000
      060FC0010000C000060FC00100008000090FC00180008000BFEEC001C0018000
      DDF6E00300038000EDFAE0030007C000F5FDF007800FC000FBFFFC1FC01FC000
      FFFFFFFFE03FC000FFFFFFFFF07FFFFFFC3FFFFFFFFF8407F00FFFFFFFFFB803
      E007FFFFFFE7B209E007FC1FFFDBB913E007F80FFFFBA5A7E007F007F877BFFF
      E007F007FBDBA0BFE007F007FCE7BFBFE007F007EF7FA0A1E007F007EB6FBF80
      E007F80FECC78040E007FC1FEFEFF000E007FFFFCFEFF008F00FFFFFEC1FF011
      F81FFFFFFFFFF81FFFFFFFFFFFFFFC7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FF77FFBBFF77F80FFEFBFF7DFEFBF00FEEFBBF7DBEFBF40FEDADBED6BDADF007
      EDDDBEEEBDDDF803EDDD86EEBDDDF803EDADBAD685ADE003EDFDBAFEBDFDE003
      EEFBBB7DBEFBE08382FB877D82FBE007FF77FFBBFF77E03FFFFFFFFFFFFFE07F
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE00FFFFF8001FF81800180018001FF81
      000180018001FF81000180018101FF8100018001828102010001800184410181
      000180018821038100018001901103FF00018001A00900C000018001C0050300
      00018001BC3903C0000180018001FFC0000180018001FFC000018001F87FFFC0
      0001FFFFF0FFFFC08001FFFFE1FFFFFF00000000000000000000000000000000
      000000000000}
  end
  object imgObjectsMasks: TImageList
    Left = 176
    Top = 56
    Bitmap = {
      494C01010A000E00340010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000203090002030
      9000203080002028700000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006F6F40003F3F
      1400434314004E4E44007F7F7E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001038C0002050D0001038
      C0001030B0001030A00010207000000000000000000000000000000000000000
      0000000000000000000000000000000000009D9DA4006F6F28007D7D05007171
      00007373000067670000525218007F7F7E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001040E0004068E0003058E0000038
      E0000038D0001030C0001030A000202870000000000000000000000000000000
      000000000000000000000000000000000000A1A1A7007A7A450069691F004141
      00004545000049490500626229002F2F06000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003068F000F0F8FF00FFF8FF00FFF8
      FF00FFF8FF00FFF8FF00FFF8FF00203090000000000000000000000000000000
      00000000000000000000000000000000000094949A007B7B36006E6E0B005C5C
      00005D5D0000656502005B5B1300141406000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004070F000FFF8FF00FFF8FF00FFF8
      FF00FFF8FF00FFF8FF00FFF8FF00203090000000000000000000000000000000
      00000000000000000000000000000000000094949A007B7B36006E6E0B005C5C
      00005D5D0000656502005B5B1300141406000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003068FF0080A0F0006080F0004060
      E0002058E0000038D0001038C0002038A0000000000000000000000000000000
      0000000000000000000000000000000000009F9FA60082824500ABAB20008383
      00008C8C0000606001006B6B4200303019000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003068F00090A8F0006080
      F0005078E0002048D0002038B000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5AE0095953400FCFC0000FFFF
      0000FFFF0000C9C900009090190057574A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003060F0003060
      F0003060E0002050D00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000959544006262
      1400656514007D7D440000000000000000000000000000000000000000000000
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
      000000000000000000000000000000000000FFFFFF001131B900FFFFFF00FFFF
      FF001131B900FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000082D6EE0089E8
      FA0030C0E00089E8FA0093E0EE00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000080000000800000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D3D9DB00ACB6BD00ADB6BD00ADB6BD00ADB6BD00ADB6BD00ADB6BD00ADB6
      BD00ADB6BD00ADB6BD00ACB6BD00D3D9DB000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF001131B9001131B900FFFF
      FF001131B900FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000093E0F50030B8E00080E8
      FF0060C8E00090F0FF0030B8E00093E0F5000000000000000000000000000000
      00000000000000000000FFFFFF000080000000FF000000FF000000800000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00004CCBFB0004B4FD0003B8FE0003B8FE0003B9FE0004C3FE0004C3FE0003B9
      FE0003B8FE0003B8FE0004B4FD004CCBFB000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF001131B9001131
      B9001131B900FFFFFF001131B9001131B9000000000000000000000000000000
      0000000000000000000000000000000000000000000095E8FA0090F0FF00C0F8
      FF00B0E8F000C0F8FF0090F0FF0095E8FA000000000000000000000000000000
      00000000000000000000FFFFFF0000FF000000FF000000FF000000FF00000080
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000091E0FE0002C0FF0000D1FF0000D1FF0000D4FF00005263000052630000D4
      FF0000D1FF0000D1FF0002C0FF0091E0FE000000000000000000000000000000
      00000000000000000000FFFFFF001F5CD5001F5CD5001F5CD5002E86F2002E86
      F2001131B9001131B9001131B900FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000020A8E00050C0E000B0E8
      F00000000000B0E8F00050C0E00030B8E0000000000000000000000000000000
      000000000000FFFFFF000080000000FF000000FF0000FFFFFF0000FF000000FF
      000000800000FFFFFF0000000000000000000000000000000000000000000000
      0000000000002CC7FB0002CAFF0000D1FF0000D4FF00005263000052630000D4
      FF0000D1FF0002CAFF002CC7FB00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF001F5CD5001E249600FFFF
      FF002E86F2001131B900FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000095E8FA0090F0FF00C0F8
      FF00B0E8F000C0F8FF0090F0FF0095E8FA000000000000000000000000000000
      000000000000FFFFFF0000FF000000FF0000FFFFFF0000000000FFFFFF0000FF
      000000FF000000800000FFFFFF00000000000000000000000000000000000000
      00000000000090E1FC0004C3FE0000D4FF0000DAFF00009EBC00009EBC0000DA
      FF0000D4FF0004C3FE0090E1FC00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF001F5CD5001F5CD5001E2496001E24
      96002E86F2001131B9001131B9001131B9000000000000000000000000000000
      0000000000000000000000000000000000000000000093E0F50030B8E00090F0
      FF0060C0E00090F0FF0030B8E00093DCEE000000000000000000000000000000
      000000000000FFFFFF0000FF000000FF0000FFFFFF000000000000000000FFFF
      FF0000FF000000FF000000800000FFFFFF000000000000000000000000000000
      000000000000000000002CC7FB0000D1FF0000E4FF00004754000047540000E4
      FF0000D1FF002CC7FB0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF001F5CD5001F5CD500FFFFFF001F5CD5001F5C
      D5001F5CD500FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000082D6EE0095E8
      FA0020B0E00089E8FA0093E0EE00000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000FFFFFF0000FF000000FF0000008000000000000000000000000000000000
      00000000000000000000000000002CC7FB0000D2FD0000333A0000333A0000D2
      FD002CC7FB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF001F5CD500FFFF
      FF001F5CD5001F5CD500FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FF000000FF00000000000000000000000000000000
      000000000000000000000000000098E6FD0002C6FB0000444E0000444E0002C6
      FB0098E6FD000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF001F5CD500FFFF
      FF00FFFFFF001F5CD500FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000002CC7FB0000D0F90000D0F9002CC7
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000090E3FC0002CAFF0002CAFF0090E3
      FC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004CCBFB004CCBFB000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003CC7FF002C8BE6002C8BE6002C8B
      E6002C8BE6002C8BE6003CC7FF00000000000000000000000000000000000000
      000000000000000000000000000000000000FB2FFD001F0FED001F0FED001F0F
      ED001F0FED001F0FED00FB2FFD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000257BF000257BF000257B
      F000257BF000257BF00000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005A5A5A005A5A
      5A005A5A5A005A5A5A0063636300000000000000000000000000000000000000
      00000000000000000000000000003CC7FF002C8BE6005A5A5A005A5A5A005A5A
      5A005A5A5A00636363002C8BE6003CC7FF000000000000000000000000000000
      0000000000000000000000000000FB2FFD001F0FED005A5A5A005A5A5A005A5A
      5A005A5A5A00636363001F0FED00FB2FFD000000000000000000000000000000
      000000000000000000000000000000000000257BF0005A5A5A005A5A5A005A5A
      5A005A5A5A0063636300257BF000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000073737300ADAD
      AD009C9C9C007373730042424200000000000000000000000000000000000000
      00000000000000000000000000002C8BE6000000000073737300ADADAD009C9C
      9C007373730042424200000000002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED000000000073737300ADADAD009C9C
      9C007373730042424200000000001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF0000000000073737300ADADAD009C9C
      9C00737373004242420000000000257BF0000000000000000000000000000000
      0000000000000000000000000000000000000000000042424200A5A5A500B5B5
      B500ADADAD008C8C8C007B7B7B00212121000000000000000000000000000000
      00000000000000000000000000002C8BE60042424200A5A5A500B5B5B500ADAD
      AD008C8C8C007B7B7B00212121002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED0042424200A5A5A500B5B5B500ADAD
      AD008C8C8C007B7B7B00212121001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF00042424200A5A5A500B5B5B500ADAD
      AD008C8C8C007B7B7B0021212100257BF0000000000000000000000000000000
      00000000000000000000000000000000000000000000424242009C9C9C00CECE
      CE00B5B5B5009494940073737300212121000000000000000000000000000000
      00000000000000000000000000002C8BE600424242009C9C9C00CECECE00B5B5
      B5009494940073737300212121002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED00424242009C9C9C00CECECE00B5B5
      B5009494940073737300212121001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF000424242009C9C9C00CECECE00B5B5
      B500949494007373730021212100257BF0000000000000000000000000000000
      0000000000000000000000000000000000000000000039393900ADADAD00CECE
      CE00B5B5B500949494007B7B7B00212121000000000000000000000000000000
      00000000000000000000000000002C8BE60039393900ADADAD00CECECE00B5B5
      B500949494007B7B7B00212121002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED0039393900ADADAD00CECECE00B5B5
      B500949494007B7B7B00212121001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF00039393900ADADAD00CECECE00B5B5
      B500949494007B7B7B0021212100257BF0000000000000000000000000000000
      0000000000000000000000000000000000000000000039393900B5B5B500C6C6
      C600ADADAD00949494007B7B7B00212121000000000000000000000000000000
      00000000000000000000000000002C8BE60039393900B5B5B500C6C6C600ADAD
      AD00949494007B7B7B00212121002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED0039393900B5B5B500C6C6C600ADAD
      AD00949494007B7B7B00212121001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF00039393900B5B5B500C6C6C600ADAD
      AD00949494007B7B7B0021212100257BF0000000000000000000000000000000
      00000000000000000000000000000000000000000000524A5200CECECE00D6D6
      D600DEDEDE00D6D6D600BDBDBD00313131000000000000000000000000000000
      00000000000000000000000000002C8BE600524A5200CECECE00D6D6D600DEDE
      DE00D6D6D600BDBDBD00313131002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED00524A5200CECECE00D6D6D600DEDE
      DE00D6D6D600BDBDBD00313131001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF000524A5200CECECE00D6D6D600DEDE
      DE00D6D6D600BDBDBD0031313100257BF0000000000000000000000000000000
      00000000000000000000000000000000000000000000212121009C9C9C00B5B5
      B500B5B5B500B5B5B500A5A5A500181818000000000000000000000000000000
      00000000000000000000000000002C8BE600212121009C9C9C00B5B5B500B5B5
      B500B5B5B500A5A5A500181818002C8BE6000000000000000000000000000000
      00000000000000000000000000001F0FED00212121009C9C9C00B5B5B500B5B5
      B500B5B5B500A5A5A500181818001F0FED000000000000000000000000000000
      0000000000000000000000000000257BF000212121009C9C9C00B5B5B500B5B5
      B500B5B5B500A5A5A50018181800257BF0000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005A5A5A005A5A
      5A005A5A5A005A5A5A005A5A5A00000000000000000000000000000000000000
      00000000000000000000000000003CC7FF002C8BE6005A5A5A005A5A5A005A5A
      5A005A5A5A005A5A5A002C8BE6003CC7FF000000000000000000000000000000
      0000000000000000000000000000FB2FFD001F0FED005A5A5A005A5A5A005A5A
      5A005A5A5A005A5A5A001F0FED00FB2FFD000000000000000000000000000000
      000000000000000000000000000000000000257BF0005A5A5A005A5A5A005A5A
      5A005A5A5A005A5A5A00257BF000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003CC7FF002C8BE6002C8BE6002C8B
      E6002C8BE6002C8BE6003CC7FF00000000000000000000000000000000000000
      000000000000000000000000000000000000FB2FFD001F0FED001F0FED001F0F
      ED001F0FED001F0FED00FB2FFD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000257BF000257BF000257B
      F000257BF000257BF00000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFC3FFC100000000FF81FF0000000000
      FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF0000000000
      FF81FF0000000000FFC3FFC300000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FF03FFC1FE1FF000FF00FF80FC0FF000
      FE00FF80FC07F000FC00FF88F803F801FE00FF80F841F801FE00FF80F860FC03
      FC00FFC1FCF0FE07FE01FFFFFFF8FE07FF81FFFFFFFCFF0FFFDBFFFFFFFFFF0F
      FFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF01FF01FF83FFC1FE00FE00FF01FF80
      FE00FE00FE00FF80FE00FE00FE00FF80FE00FE00FE00FF80FE00FE00FE00FF80
      FE00FE00FE00FF80FE00FE00FE00FF80FE00FE00FE00FFC1FE00FE00FF01FFFF
      FF01FF01FF83FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object lstSmallImages: TImageList
    Height = 12
    Width = 12
    Left = 176
    Top = 96
    Bitmap = {
      494C01010200040034000C000C00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000000C00000001002000000000000009
      00000000000000000000000000000000000000000000ACB6BD00ADB6BD00ADB6
      BD00ADB6BD00ADB6BD00ADB6BD00ADB6BD00ADB6BD00ADB6BD00ACB6BD00D3D9
      DB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004CCBFB0004B4FD0003B8FE0003B8
      FE0003B9FE0004C3FE0004C3FE0003B9FE0003B8FE0003B8FE0004B4FD004CCB
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000091E0FE0002C0FF0000D1FF0000D1
      FF0000D4FF00005263000052630000D4FF0000D1FF0000D1FF0002C0FF0091E0
      FE00000000000000000000000000000000002030900020309000203080002028
      7000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002CC7FB0002CAFF0000D1
      FF0000D4FF00005263000052630000D4FF0000D1FF0002CAFF002CC7FB000000
      00000000000000000000000000001038C0002050D0001038C0001030B0001030
      A000102070000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000090E1FC0004C3FE0000D4
      FF0000DAFF00009EBC00009EBC0000DAFF0000D4FF0004C3FE0090E1FC000000
      000000000000000000001040E0004068E0003058E0000038E0000038D0001030
      C0001030A0002028700000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002CC7FB0000D1
      FF0000E4FF00004754000047540000E4FF0000D1FF002CC7FB00000000000000
      000000000000000000003068F000F0F8FF00FFF8FF00FFF8FF00FFF8FF00FFF8
      FF00FFF8FF002030900000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002CC7
      FB0000D2FD0000333A0000333A0000D2FD002CC7FB0000000000000000000000
      000000000000000000004070F000FFF8FF00FFF8FF00FFF8FF00FFF8FF00FFF8
      FF00FFF8FF002030900000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000098E6
      FD0002C6FB0000444E0000444E0002C6FB0098E6FD0000000000000000000000
      000000000000000000003068FF0080A0F0006080F0004060E0002058E0000038
      D0001038C0002038A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002CC7FB0000D0F90000D0F9002CC7FB000000000000000000000000000000
      00000000000000000000000000003068F00090A8F0006080F0005078E0002048
      D0002038B0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000090E3FC0002CAFF0002CAFF0090E3FC000000000000000000000000000000
      0000000000000000000000000000000000003060F0003060F0003060E0002050
      D000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004CCBFB004CCBFB00000000000000000000000000000000000000
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
      28000000300000000C0000000100010000000000600000000000000000000000
      000000000000000000000000FFFFFF00800FFF0000000000000FFF0000000000
      000F0F0000000000801E070000000000801C030000000000C03C030000000000
      E07C030000000000E07C030000000000F0FE070000000000F0FF0F0000000000
      F9FFFF0000000000FFFFFF000000000000000000000000000000000000000000
      000000000000}
  end
end
