object frmDataExport: TfrmDataExport
  Left = 385
  Top = 337
  BorderStyle = bsToolWindow
  Caption = '\'
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
    ActivePage = tbsFileName
    Align = alClient
    TabOrder = 0
    object tbsObjects: TTabSheet
      Caption = ' Step 1 '
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
        Width = 81
        Height = 13
        Caption = 'Object Selection:'
      end
      object lblObjectPool: TLabel
        Left = 9
        Top = 53
        Width = 58
        Height = 13
        Caption = 'Object Pool:'
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
        OnDblClick = btnMoveObjects
      end
      object btnAddAll: TButton
        Left = 174
        Top = 87
        Width = 26
        Height = 25
        Caption = '>>'
        Enabled = False
        TabOrder = 1
        OnClick = btnMoveObjects
      end
      object btnAdd: TButton
        Left = 174
        Top = 116
        Width = 26
        Height = 25
        Caption = '>'
        Enabled = False
        TabOrder = 2
        OnClick = btnMoveObjects
      end
      object btnRemove: TButton
        Left = 174
        Top = 145
        Width = 26
        Height = 25
        Caption = '<'
        Enabled = False
        TabOrder = 3
        OnClick = btnMoveObjects
      end
      object btnRemoveAll: TButton
        Left = 174
        Top = 174
        Width = 26
        Height = 25
        Caption = '<<'
        Enabled = False
        TabOrder = 4
        OnClick = btnMoveObjects
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
        OnDblClick = btnMoveObjects
      end
      object lblStep1: TStaticText
        Left = 32
        Top = 24
        Width = 430
        Height = 17
        AutoSize = False
        Caption = 'Select the database objects to export data from.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
    end
    object tbsFileName: TTabSheet
      Caption = ' Step 2 '
      OnShow = tbsFileNameShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblFileName: TLabel
        Left = 118
        Top = 56
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'File Name:'
      end
      object edtFileName: TEdit
        Left = 174
        Top = 53
        Width = 135
        Height = 21
        TabOrder = 0
        OnChange = SetState
      end
      object btnFileName: TButton
        Left = 313
        Top = 53
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 1
        OnClick = btnFileNameClick
      end
      object lblStep2: TStaticText
        Left = 32
        Top = 24
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Enter a file name for the export file.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object tbsExport: TTabSheet
      Caption = ' Step 3 '
      OnShow = tbsExportShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        377
        274)
      object lblObjectInProgress: TLabel
        Left = 88
        Top = 170
        Width = 341
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
      end
      object lblObjectSelection: TLabel
        Left = 9
        Top = 52
        Width = 111
        Height = 13
        Caption = 'Objects to be Exported:'
      end
      object barProgress: TProgressBar
        Left = 9
        Top = 247
        Width = 356
        Height = 12
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
      object lblStep3: TStaticText
        Left = 32
        Top = 24
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Database content is being exported.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object lvExport: TListView
        Left = 9
        Top = 73
        Width = 356
        Height = 167
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Object'
            Width = 250
          end
          item
            AutoSize = True
            Caption = '# Rows'
          end>
        HideSelection = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
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
      Caption = 'Translate the current database schema to a target database type.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Export Database Content'
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
  object dlgSave: TSaveDialog
    DefaultExt = 'export'
    Filter = 'Export Files (*.export)|*.export|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 296
    Top = 8
  end
end
