object frmConnection: TfrmConnection
  Left = 438
  Top = 239
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsSizeToolWin
  Caption = 'Database Connections'
  ClientHeight = 312
  ClientWidth = 453
  Color = clBtnFace
  Constraints.MinHeight = 287
  Constraints.MinWidth = 461
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 453
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
      Caption = 
        'Select a database connection profile to connect to a database, o' +
        'r alternatively, add, edit or delete database connection profile' +
        's.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Database Connection Profiles'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 73
    Width = 453
    Height = 239
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    Caption = 'Panel2'
    TabOrder = 0
    object lvConnections: TListView
      Left = 6
      Top = 32
      Width = 441
      Height = 173
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Tag = 1
          Width = 150
        end
        item
          Caption = 'Definition'
          Tag = 1
          Width = 150
        end
        item
          Caption = 'Provider'
          Tag = 1
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'XML'
          Tag = 1
          WidthType = (
            -13)
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = popList
      TabOrder = 0
      ViewStyle = vsList
      OnChange = lvConnectionsChange
      OnClick = lstConnectionsClick
      OnColumnClick = lvConnectionsColumnClick
      OnDblClick = OnConnect
      OnKeyPress = lvConnectionsKeyPress
      ExplicitHeight = 171
    end
    object ToolBar1: TToolBar
      Tag = 1
      Left = 6
      Top = 6
      Width = 441
      Height = 26
      ButtonHeight = 24
      ButtonWidth = 24
      Color = clBtnFace
      DrawingStyle = dsGradient
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = frmMain.ImageList
      ParentColor = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      object btnConnect: TToolButton
        Left = 0
        Top = 0
        Action = actConnect
      end
      object btnAdd: TToolButton
        Left = 24
        Top = 0
        Action = actAdd
      end
      object btnEdit: TToolButton
        Left = 48
        Top = 0
        Action = actEdit
      end
      object btnDelete: TToolButton
        Left = 72
        Top = 0
        Action = actDelete
      end
    end
    object pnlBottom: TPanel
      Left = 6
      Top = 205
      Width = 441
      Height = 28
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        441
        28)
      object btnClose: TButton
        Left = 365
        Top = 4
        Width = 75
        Height = 24
        Action = actClose
        Anchors = [akTop, akRight]
        Cancel = True
        TabOrder = 1
      end
      object btnUseWizard: TCheckBox
        Left = 1
        Top = 4
        Width = 137
        Height = 24
        Caption = 'Use Connection &Wizard'
        TabOrder = 2
      end
      object btnConnect2: TButton
        Left = 284
        Top = 4
        Width = 75
        Height = 24
        Action = actConnect
        Anchors = [akTop, akRight]
        Default = True
        TabOrder = 0
      end
    end
  end
  object popList: TPopupMenu
    Images = frmMain.ImageList
    OnPopup = popListPopup
    Left = 32
    Top = 120
    object popListDetailsView: TMenuItem
      Caption = 'View'
      ImageIndex = 75
      object popListViewLargeIcons: TMenuItem
        Action = actViewLargeIcons
        RadioItem = True
      end
      object popListViewSmallIcons: TMenuItem
        Tag = 1
        Action = actViewSmallIcons
        RadioItem = True
      end
      object popListViewList: TMenuItem
        Tag = 2
        Action = actViewList
        RadioItem = True
      end
      object popListViewDetails: TMenuItem
        Tag = 3
        Action = actViewDetails
        RadioItem = True
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Add1: TMenuItem
      Action = actAdd
    end
    object Edit1: TMenuItem
      Action = actEdit
    end
    object Delete1: TMenuItem
      Action = actDelete
    end
  end
  object ActionList: TActionList
    Images = frmMain.ImageList
    Left = 65
    Top = 120
    object actAdd: TAction
      Caption = '&Add'
      Hint = 'Add a new database connection setting'
      ImageIndex = 70
      ShortCut = 16449
      OnExecute = OnAdd
    end
    object actEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit database connection settings'
      ImageIndex = 72
      ShortCut = 16453
      OnExecute = OnEdit
    end
    object actDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete database connection settings'
      ImageIndex = 71
      ShortCut = 16452
      OnExecute = OnDelete
    end
    object actViewLargeIcons: TAction
      Caption = 'Large Icons'
      OnExecute = popListClick
    end
    object actViewSmallIcons: TAction
      Tag = 1
      Caption = 'Small Icons'
      OnExecute = popListClick
    end
    object actViewList: TAction
      Tag = 2
      Caption = 'List'
      OnExecute = popListClick
    end
    object actViewDetails: TAction
      Tag = 3
      Caption = 'Details'
      OnExecute = popListClick
    end
    object actConnect: TAction
      Caption = 'C&onnect'
      Hint = 'Connect to a data source using database connection settings'
      ImageIndex = 69
      ShortCut = 13
      OnExecute = OnConnect
    end
    object actClose: TAction
      Caption = '&Close'
      Hint = 'Close'
      ShortCut = 27
      OnExecute = OnClose
    end
    object actHelp: TAction
      Caption = 'actHelp'
      ImageIndex = 74
    end
    object actView: TAction
      Caption = 'View'
      ImageIndex = 75
    end
  end
end

