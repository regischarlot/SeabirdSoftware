object frmQuery: TfrmQuery
  Left = 378
  Top = 142
  ClientHeight = 744
  ClientWidth = 633
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
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000004FFCCCCCCCCCCC0000000000000000004FF
    CCCCCCCCCCC4000000080000000004FFCCCCCCCCCCC4400000008000000004FF
    CCCCCCCCCCC4440000000800000004FFCCCCCCCCCCC4444000000080000004FF
    CCCCCCCCCCC4444000F00008000004FFCCCCCCCCCCC44440000F0000800004FF
    CCCCCCCCCCC444400000F000080004FFCCCCCCCCCCC4444000000F00008004FF
    CCCCCCCCCCC44440000000F0008004FFCCCCCCCCCCC4444000000080008004FF
    CCCCCCCCCCC444400000080000F004FFCCCCCCCCCCC44440000080000F0004FF
    FFFFFFFFFFF4444000080000F00004FFFFFFFFFFFFF744400080000F00000044
    4444444444477440000000F000000004444444444444774000000F0000000000
    44444444444447700000F000000000000444444444444470000F000000000000
    0044444444444447000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8001FFFF8000FFFF8000
    7CFF80003C7F80001C3F80000C1F80000C0F80000E0780000F0380000F818000
    0FC180000FC180000F8180000F0380000E0780000C0FC0000C1FE0000C3FF000
    0C7FF8000CFFFC000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 744
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object spltrQuery: TdfsSplitter
      Left = 0
      Top = 200
      Width = 633
      Height = 9
      Cursor = crVSplit
      Align = alTop
      OnMoved = spltrQueryMoved
      ButtonHighlightColor = clGradientInactiveCaption
      ExplicitLeft = 1
      ExplicitTop = 201
      ExplicitWidth = 635
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 209
      Width = 633
      Height = 535
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pgDisplay: TPageControl
        Left = 0
        Top = 0
        Width = 633
        Height = 535
        ActivePage = tsGrid
        Align = alClient
        OwnerDraw = True
        TabOrder = 0
        OnChange = pgDisplayChange
        OnDrawTab = pgDisplayDrawTab
        object tsText: TTabSheet
          Tag = -1
          Caption = 'Text'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object mmoQueryResult: TSynEdit
            Left = 0
            Top = 0
            Width = 625
            Height = 507
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            PopupMenu = popQueryResult
            TabOrder = 0
            OnClick = SetState
            OnEnter = onSelMove
            OnExit = OnDisplayFieldExit
            BorderStyle = bsNone
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
            Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ReadOnly = True
            RightEdge = -1
            OnStatusChange = OnEditStatusChange
          end
        end
        object tsGrid: TTabSheet
          Tag = -1
          Caption = 'Grid'
          ImageIndex = 1
          object dbListView: TDBListView
            Left = 0
            Top = 0
            Width = 625
            Height = 507
            BackgroundColor = clWindow
            SelectedColumnColor = clCaptionText
            SelectionColor = clSilver
            CurrentLineColor = clGray
            IsReadOnly = True
            IsAsynchronous = False
            Align = alClient
            AllocBy = 200
            BevelEdges = []
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Columns = <>
            HideSelection = False
            OwnerDraw = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = popListView
            TabOrder = 0
            ViewStyle = vsReport
            OnEnter = onSelMove
            OnExit = OnDisplayFieldExit
          end
        end
        object tsHint: TTabSheet
          Tag = -1
          Caption = 'SQL Hint'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object wbAnalysis: TWebBrowser
            Left = 0
            Top = 0
            Width = 625
            Height = 507
            Align = alClient
            TabOrder = 0
            OnNavigateComplete2 = wbAnalysisNavigateComplete2
            ExplicitWidth = 438
            ExplicitHeight = 284
            ControlData = {
              4C00000098400000663400000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E12620A000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 633
      Height = 200
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      OnResize = pnlTopResize
      object mmoSQL: TSynEdit
        Left = 6
        Top = 0
        Width = 627
        Height = 200
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = popSQL
        TabOrder = 1
        OnClick = mmoSQLClick
        OnEnter = onSelMove
        OnExit = OnDisplayFieldExit
        OnKeyDown = mmoSQLKeyDown
        OnKeyUp = mmoSQLKeyUp
        BorderStyle = bsNone
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabIndent]
        RightEdge = -1
        TabWidth = 4
        WantTabs = True
        OnChange = mmoSQLChange
        OnScroll = mmoSQLScroll
        OnStatusChange = OnEditStatusChange
      end
      object pnlProgress: TProgress
        Left = 0
        Top = 0
        Width = 6
        Height = 200
        Align = alLeft
        BevelOuter = bvNone
        Color = clWindow
        ParentBackground = False
        TabOrder = 0
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 133
    Width = 480
    Height = 22
    Align = alNone
    AutoSize = True
    ButtonWidth = 24
    Color = clBtnFace
    DrawingStyle = dsGradient
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = frmMain.ImageList
    ParentColor = False
    TabOrder = 0
    Wrapable = False
    object btnCut: TToolButton
      Left = 0
      Top = 0
      Action = actEditCut
    end
    object btnCopy: TToolButton
      Left = 24
      Top = 0
      Action = actEditCopy
    end
    object btnPaste: TToolButton
      Left = 48
      Top = 0
      Action = actEditPaste
    end
    object ToolButton9: TToolButton
      Left = 72
      Top = 0
      Width = 8
      Caption = 'ToolButton9'
      ImageIndex = 30
      Style = tbsSeparator
    end
    object btnFind: TToolButton
      Left = 80
      Top = 0
      Action = actEditFind
    end
    object btnFindNext: TToolButton
      Left = 104
      Top = 0
      Action = actEditFindNext
    end
    object btnFindPrev: TToolButton
      Left = 128
      Top = 0
      Action = actEditFindPrev
    end
    object btnReplace: TToolButton
      Left = 152
      Top = 0
      Action = actEditReplace
    end
    object ToolButton7: TToolButton
      Left = 176
      Top = 0
      Width = 8
      ImageIndex = 30
      Style = tbsSeparator
    end
    object btnActionParagraphMark: TToolButton
      Left = 184
      Top = 0
      Action = actAddParagraph
    end
    object btnEnableCodeCompletion: TToolButton
      Left = 208
      Top = 0
      Hint = 'Enable Code Auto-Complete'
      Action = actCodeCompletionEnable
      ImageIndex = 89
    end
    object ToolButton4: TToolButton
      Left = 232
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 30
      Style = tbsSeparator
    end
    object btnActionStart: TToolButton
      Left = 240
      Top = 0
      Action = actActionStart
    end
    object btnActionStop: TToolButton
      Left = 264
      Top = 0
      Action = actActionStop
    end
    object btnAnalyze: TToolButton
      Left = 288
      Top = 0
      Action = actActionAnalyze
    end
    object ToolButton3: TToolButton
      Left = 312
      Top = 0
      Width = 8
      ImageIndex = 29
      Style = tbsSeparator
    end
    object btnActionCommit: TToolButton
      Left = 320
      Top = 0
      Action = actActionCommit
    end
    object btnActionRollback: TToolButton
      Left = 344
      Top = 0
      Action = actActionRollback
    end
    object btnActionAutoCommit: TToolButton
      Left = 368
      Top = 0
      Action = actActionAutoCommit
    end
    object ToolButton2: TToolButton
      Left = 392
      Top = 0
      Width = 8
      ImageIndex = 26
      Style = tbsSeparator
    end
    object btnReportFormat: TToolButton
      Left = 400
      Top = 0
      Action = actActionReportOutput
    end
    object btnSpool: TToolButton
      Left = 424
      Top = 0
      Action = actActionSpool
    end
    object ToolButton1: TToolButton
      Left = 448
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 45
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 456
      Top = 0
      Action = actHelpMaster
    end
  end
  object DataMainMenu: TMainMenu
    Images = frmMain.ImageList
    Left = 56
    Top = 64
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 1
      OnClick = ClipboardAction
      object mnuEditUndo: TMenuItem
        Action = actEditUndo
      end
      object mnuEditRedo: TMenuItem
        Action = actEditRedo
      end
      object N4: TMenuItem
        Caption = '-'
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
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actEditFind
      end
      object mnuEditFindNext: TMenuItem
        Action = actEditFindNext
      end
      object mnuEditFindPrev: TMenuItem
        Action = actEditFindPrev
      end
      object mnuEditReplace: TMenuItem
        Action = actEditReplace
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuEditUpperCase: TMenuItem
        Action = actEditUpperCase
      end
      object mnuEditLowerCase: TMenuItem
        Action = actEditLowerCase
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object mnuObjectStructure: TMenuItem
        Caption = 'Object Structure'
        object mnuObjectStructureListColumns: TMenuItem
          Action = actObjectStructureListColumns
        end
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object mnuEditParagraphMark: TMenuItem
        Action = actAddParagraph
      end
      object mnuEnableCodeCompletion: TMenuItem
        Caption = 'Auto-Complete'
        Hint = 'Code Auto-Complete'
        ImageIndex = 89
        object mnuCodeCompletionEnable: TMenuItem
          Action = actCodeCompletionEnable
        end
        object mnuCodeCompletionRefresh: TMenuItem
          Action = actCodeCompletionRefresh
        end
        object mnuCodeCompletionShowColumnList: TMenuItem
          Action = actCodeCompletion_ShowColumnList
        end
        object mnuCodeCompletionShowTableList: TMenuItem
          Action = actCodeCompletion_ShowTableList
        end
      end
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      GroupIndex = 4
    end
    object mnuAction: TMenuItem
      Caption = 'Act&ion'
      GroupIndex = 4
      object mnuActionStart: TMenuItem
        Action = actActionStart
      end
      object mnuActionStop: TMenuItem
        Action = actActionStop
      end
      object mnuActionAnalyze: TMenuItem
        Action = actActionAnalyze
        GroupIndex = 1
      end
      object ExecuteBlock1: TMenuItem
        Action = actActionExecuteBlock
        GroupIndex = 1
      end
      object ExecutetoOutput1: TMenuItem
        Action = actActionExecuteToOutput
        GroupIndex = 1
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuActionCommit: TMenuItem
        Action = actActionCommit
        GroupIndex = 1
      end
      object mnuActionRollback: TMenuItem
        Action = actActionRollback
        GroupIndex = 1
      end
      object mnuActionAutoCommit: TMenuItem
        Action = actActionAutoCommit
        GroupIndex = 1
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuActionMemorize: TMenuItem
        Action = actActionMemorize
        GroupIndex = 1
      end
      object N13: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuActionReportOutput: TMenuItem
        Caption = 'Report Output'
        GroupIndex = 1
        Hint = 'Display Query Execution into Grid'
        ImageIndex = 10
      end
      object mnuActionSpool: TMenuItem
        Action = actActionSpool
        GroupIndex = 1
      end
      object mnuActionReadOnly: TMenuItem
        Action = actEditReadOnly
        GroupIndex = 1
      end
    end
    object mnuFavorites: TMenuItem
      Caption = 'F&avorites'
      GroupIndex = 4
      OnClick = mnuFavoritesClick
      object mnuFavoritesAdd: TMenuItem
        Action = actFavoritesAdd
      end
      object mnuFavoritesEdit: TMenuItem
        Action = actFavoritesEdit
      end
      object N3: TMenuItem
        Caption = '-'
      end
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    FilterIndex = 0
    InitialDir = '.'
    Options = [ofPathMustExist, ofEnableSizing]
    Left = 120
    Top = 24
  end
  object ActionList: TActionList
    Images = frmMain.ImageList
    State = asSuspended
    Left = 88
    Top = 24
    object actActionStart: TAction
      Caption = 'Start'
      Enabled = False
      Hint = 'Start Query Execution / Refresh Query Display'
      ImageIndex = 13
      ShortCut = 16453
      OnExecute = onStart
    end
    object actActionStop: TAction
      Caption = 'Stop'
      Enabled = False
      Hint = 'Stop Query Execution'
      ImageIndex = 17
      ShortCut = 16452
    end
    object actActionPause: TAction
      Caption = 'Pause'
      Enabled = False
      Hint = 'Pause Query Execution'
      ImageIndex = 18
    end
    object actActionCommit: TAction
      Caption = 'Commit'
      Hint = 'Commit SQL State'
      ImageIndex = 27
      ShortCut = 32835
      OnExecute = onCommit
    end
    object actActionRollback: TAction
      Caption = 'Rollback'
      Hint = 'Rollback SQL State'
      ImageIndex = 26
      ShortCut = 32850
      OnExecute = onRollback
    end
    object actActionReportOutput: TAction
      Caption = 'Report Output'
      Hint = 'Display Query Execution into Grid'
      ImageIndex = 10
      ShortCut = 16468
      OnExecute = onReportOutput
    end
    object actActionAutoCommit: TAction
      Caption = 'Auto Commit'
      Hint = 'Auto Commit SQL State'
      ImageIndex = 25
      OnExecute = onAutoCommit
    end
    object actActionAnalyze: TAction
      Caption = 'Analyze'
      Enabled = False
      Hint = 'Analyze SQL Statement'
      ImageIndex = 79
      ShortCut = 24645
      OnExecute = onAnalyze
    end
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 5
      ShortCut = 16472
      OnExecute = ClipboardAction
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = ClipboardAction
    end
    object actActionMemorize: TAction
      Caption = '&Memorize'
      Enabled = False
      Hint = 'Memorize SQL Statement'
      ShortCut = 16461
      OnExecute = onMemorize
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 4
      ShortCut = 16470
      OnExecute = ClipboardAction
    end
    object actEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select A&ll'
      Hint = 'Select All|Selects the entire document'
      ImageIndex = 87
      ShortCut = 16449
      OnExecute = ClipboardAction
    end
    object actEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 35
      ShortCut = 16474
      OnExecute = ClipboardAction
    end
    object actActionSpool: TAction
      Caption = 'Spoo&l'
      Hint = 'Spool SQL console output to file'
      ImageIndex = 44
      OnExecute = onSpool
    end
    object actActionExecuteToOutput: TAction
      Caption = 'Execute to Output'
      Hint = 'Exeute SQL Statement to sent content to external output'
      ImageIndex = 66
      OnExecute = OnActionExecuteToOutput
    end
    object actEditRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      ImageIndex = 52
      ShortCut = 16473
      OnExecute = ClipboardAction
    end
    object actFavoritesAdd: TAction
      Caption = '&Add'
      Hint = 'Add to Favorite SQL Statements'
      OnExecute = mnuFavoritesAddClick
    end
    object actFavoritesEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit Favorite SQL Statement'
      OnExecute = actFavoritesEditClick
    end
    object actEditFind: TAction
      Caption = '&Find'
      Hint = 'Find String'
      ImageIndex = 53
      ShortCut = 16454
      OnExecute = onFind
    end
    object actEditReplace: TAction
      Caption = '&Replace'
      Hint = 'Replace String'
      ImageIndex = 55
      ShortCut = 16466
      OnExecute = onReplace
    end
    object actEditUpperCase: TAction
      Caption = 'Upper Case'
      Hint = 'Upper Case String'
      ImageIndex = 56
      ShortCut = 16469
      OnExecute = onUpperCase
    end
    object actEditLowerCase: TAction
      Caption = 'Lower Case'
      Hint = 'Lower Case String'
      ImageIndex = 57
      ShortCut = 16460
      OnExecute = onLowerCase
    end
    object actAddParagraph: TAction
      Caption = '&Paragraph Mark'
      Hint = 'Insert Paragraph Mark'
      ImageIndex = 64
      ShortCut = 32848
      OnExecute = OnInsertParagraphMark
    end
    object actCodeCompletion_ShowTableList: TAction
      Caption = 'Show Table List'
      ShortCut = 24660
      OnExecute = onShowTableList
    end
    object actEditReadOnly: TAction
      Category = 'Edit'
      Caption = 'ReadOnly Grid Display'
      OnExecute = OnReadOnly
    end
    object actEditFindPrev: TAction
      Caption = 'Find &Previous'
      Hint = 'Find Previous Occurence of String'
      ImageIndex = 84
      ShortCut = 8306
      OnExecute = onFindPrev
    end
    object actEditFindNext: TAction
      Caption = 'Find &Next'
      Hint = 'Find Next Occurence of String'
      ImageIndex = 54
      ShortCut = 114
      OnExecute = onFindNext
    end
    object actCodeCompletionEnable: TAction
      Caption = 'Enable'
      Hint = 'Enable Auto-Completion'
      OnExecute = actCodeCompletionEnableExecute
    end
    object actActionExecuteBlock: TAction
      Caption = 'Execute &Block'
      Enabled = False
      ImageIndex = 90
      ShortCut = 16450
      OnExecute = OnExecuteBlock
    end
    object actCodeCompletionRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 3
      ShortCut = 116
      OnExecute = actCodeCompletionRefreshExecute
    end
    object actCodeCompletion_ShowColumnList: TAction
      Caption = 'Show Column List'
      ShortCut = 24643
      OnExecute = onShowColumnList
    end
    object actObjectStructureListColumns: TAction
      Caption = 'List Columns'
      OnExecute = actObjectStructureListColumnsExecute
    end
    object actHelpMaster: TAction
      Caption = 'Access Online Help'
      Enabled = False
      Hint = 'Access Online Help'
      ImageIndex = 68
      OnExecute = OnMasterHelp
    end
  end
  object actObjectCreate: TAction
    Caption = 'New &Object'
  end
  object dlgSpool: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    FilterIndex = 0
    InitialDir = '.'
    Options = [ofPathMustExist, ofEnableSizing]
    Left = 120
    Top = 56
  end
  object popSQL: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = SetState
    Left = 16
    Top = 16
    object popSQLUndo: TMenuItem
      Action = actEditUndo
    end
    object popSQLRedo: TMenuItem
      Action = actEditRedo
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object popSQLCut: TMenuItem
      Action = actEditCut
    end
    object popSQLCopy: TMenuItem
      Action = actEditCopy
    end
    object popSQLPaste: TMenuItem
      Action = actEditPaste
    end
    object popSQLSelectAll: TMenuItem
      Action = actEditSelectAll
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object popSQLFind: TMenuItem
      Action = actEditFind
    end
    object popSQLFindNext: TMenuItem
      Action = actEditFindNext
    end
    object popSQLFindPrevious: TMenuItem
      Action = actEditFindPrev
    end
    object popSQLReplace: TMenuItem
      Action = actEditReplace
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object popSQLUpperCase: TMenuItem
      Action = actEditUpperCase
    end
    object popSQLLowerCase: TMenuItem
      Action = actEditLowerCase
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object popSQLStart: TMenuItem
      Action = actActionStart
    end
    object popSQLAnalyzeSQLStatements: TMenuItem
      Action = actActionAnalyze
    end
    object popSQLExecutetoOutput: TMenuItem
      Action = actActionExecuteToOutput
    end
    object popSQLMemorizeStatement: TMenuItem
      Action = actActionMemorize
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object popObjectStructure: TMenuItem
      Caption = 'Object Structure'
      object popObjectStructureListColumns: TMenuItem
        Action = actObjectStructureListColumns
      end
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object popSQLParagraphMark: TMenuItem
      Action = actAddParagraph
    end
  end
  object popQueryResult: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = SetState
    Left = 16
    Top = 272
    object popQueryResultCopy: TMenuItem
      Action = actEditCopy
    end
    object popQueryResultSelectAll: TMenuItem
      Action = actEditSelectAll
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object popQueryResultFind: TMenuItem
      Action = actEditFind
    end
    object popQueryResultFindNext: TMenuItem
      Action = actEditFindNext
    end
    object popQueryResultFindPrevious: TMenuItem
      Action = actEditFindPrev
    end
  end
  object dlgOpenTextFile: TOpenTextFileDialog
    DefaultExt = 'sql'
    Filter = 'SQL files (*.sql)|*.sql|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 312
  end
  object dlgSaveTextFile: TSaveTextFileDialog
    DefaultExt = 'sql'
    Filter = 'SQL files (*.sql)|*.sql|All Files (*.*)|*.*'
    Left = 328
    Top = 312
  end
  object popListView: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = SetState
    Left = 48
    Top = 272
    object popListViewCopy: TMenuItem
      Action = actEditCopy
    end
    object popListViewReadOnly: TMenuItem
      Action = actEditReadOnly
    end
  end
  object timerIntelliSense: TTimer
    Enabled = False
    Interval = 500
    OnTimer = timerIntelliSenseTimer
    Left = 592
  end
  object imgAnalysis: TImageList
    Left = 363
    Top = 312
    Bitmap = {
      494C010103000400280010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      00000000000000000000000000000099FF000033660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000046B9D3000E8CA9000E8CA9000E8C
      A9000E8CA9000E8CA9000E8CA9000E8CA9000E8CA9000E8CA9000E8CA9000E8C
      A9000E8CA9000E8CA9000E8CA90046B9D3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCFFFF000099FF000033CC000033CC000033CC00003366000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000035B4D10024C9E90018C6F10014C2
      F00012C1F00011C0F00012C1F00012C1F00012C1F00013C1F00014C2F00014C3
      F00015C3F00016C4F10020C3E40035B4D1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCFF
      FF000099FF000066FF000066FF000066FF000066FF000033CC000033CC000033
      CC00003366000000000000000000000000000000000000000000000000000033
      6600000044000000440000004400000000000000000000000000000000000000
      0000000000000000000000000000000000006FC8DC0017AECF0021CDF2001AC8
      F10017C5F10015C4F10015C3F000000000000000000018C6F1001AC8F1001DCA
      F2001DCAF20020CCF10015ABCD006FC8DC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CCFFFF000099FF000066
      FF000066FF000066FF000033CC000033CC000033660000003300003366000033
      99000033CC000033CC000033660000000000000000000033660000336600DEF8
      FF0084FDFF0033F8FF000CFAFF00000044000000440000000000000000000000
      0000000000000000000000000000000000000000000035B4D10025C6E40025D1
      F30020CDF2001ECBF2001DCAF200000000000000000021CDF30025D1F30026D2
      F40027D3F40024C5E30035B4D100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000066
      FF000066FF000033CC000099FF000066FF000033CC0000339900000033000033
      6600003399000000000000000000000000000033660068FAFF0085FEFF00E2FB
      FF00F7FEFF00C0FCFF008FF9FF0086E7FF0091E1FF0046F0FE00000044000000
      4400000000000000000000000000000000000000000080CEE00016ACCD0033DB
      F4002FD9F5002CD7F5002BD6F4002BD6F4002DD7F5002FD9F5002FD9F50031DB
      F50032DAF30015AACB0080CEE000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000330033CCFF000066FF000033CC0000339900000000000000
      000000000000000000000000000000000000003366000ED6FF0025F9FF0032FE
      FF0029D4FF000E92FF000BB9FF000AA9FF0014D2FF000175D50011C7FD0012DA
      FF0000669900006699000000000000000000000000000000000035B4D1002AC6
      DF003EE6F8003DE4F7003CE3F7001B6770001B6770003AE2F7003AE2F7003BE3
      F70029C7E00035B4D10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033CCFF000066FF000066FF000033CC00000000000000
      0000000000000000000000000000000000000000000001A8FF00049DFF00048F
      FF000396FF0011C4FB001ADFFF000FC2FF000AB2FE000190F100019BFF000278
      FF0000A8FF0002C7FF00000044000000000000000000000000008ED3E30015AA
      CB0046EAF6004AF0FA0048EEF900000000000000000044EAF90044EAF9003FE5
      F50015AACB008ED3E30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033CCFF000099FF000066FF000033CC00000000000000
      00000000000000000000000000000000000000000000006BA1000588FF0007C6
      FF000BA1FF000EBAF80019DCFF000FC3FF0004A6FF00019EFF00019CFF00008D
      FF00006DFF000066990002C8FF000066990000000000000000000000000035B4
      D10028C2DB004EF3FB004DF1FA00000000000000000049EFFA0049EFFA002AC6
      DF0035B4D1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099FFFF000099FF000066FF000033CC00000000000000
      0000000000000000000000000000000000000000000000000000006699000CC0
      FF0061D8FF0006A2F00014D2FF0014D0FF0004A6FF00009AFF00009AFF000091
      FF000088FF0014D2FF00026DD000003366000000000000000000000000009CD7
      E50014A9CB0048EAF40050F4FB0000000000000000004EF2FA0047EAF60014A9
      CB009CD7E5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000033CCFF000066FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000006699000AA9
      FF008BE7FF00027AD80019DDFF0015D3FF000BB7FF00019FFF00009CFF000098
      FF00008DFF000083FF000191FF00000000000000000000000000000000000000
      000036B4D10025BFD9005AF6FB00000000000000000051F5FB002CC6DD0036B4
      D100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000033CCFF000066FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000066
      CC00D2F3FF002DD6FF0007ABF40011C9FF0014D0FF000BBAFF0001A1FF0004A3
      FF00008FFF000079FF0000336600000000000000000000000000000000000000
      0000ABDDE90013A9CA0047E8F30051F5FB0051F5FB004AEDF60014A9CB00ABDD
      E900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099FFFF000066FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000066
      CC000BA4FF00D1FAFF001AB9F4000DB0F20016D4FF0017C6FF0023BDFF0021C7
      FF0003A1FF000075FF0000000000000000000000000000000000000000000000
      00000000000036B4D10023BBD60051F5FB0051F5FB002BC6DD0036B4D1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099FFFF000066FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000066CC0010A6FF00B0F6FF0030C6F9000677D40078E9FF008EEBFF0053CF
      FF00038EFF000066990000000000000000000000000000000000000000000000
      000000000000BBE2ED0013A8CA0044E5F1004AECF50014A9CB00BBE2ED000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000066CC000066CC0011CFFF002BDCFF0018A6EB00037ADD000275
      F900006699000000000000000000000000000000000000000000000000000000
      0000000000000000000036B4D10020B9D5002BC6DD0036B4D100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000066CC00006DD2000066CC000066CC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C9E9F10026AFCE0026AFCE00C9E9F100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FE7FFFFFFFFF0000F81FFFFF00000000
      E007FFFF000000008001E1FF000000000000803F800100008001000F80010000
      E0070003C0030000F81F8001C0030000F81F8000E0070000F81FC000E0070000
      FC3FC001F00F0000FC3FE001F00F0000FC3FE003F81F0000FC3FF003F81F0000
      FE7FF807FC3F0000FE7FFE1FFC3F000000000000000000000000000000000000
      000000000000}
  end
  object timerExecution: TTimer
    Interval = 400
    OnTimer = timerExecutionTimer
    Left = 592
    Top = 72
  end
end
