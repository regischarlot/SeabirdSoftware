object frmDataSnapshot: TfrmDataSnapshot
  Left = 385
  Top = 337
  BorderStyle = bsToolWindow
  Caption = 'Create Schema Snapshot'
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
    ActivePage = tbsFileName
    Align = alClient
    TabOrder = 0
    object tbsFileName: TTabSheet
      Caption = ' Step 1 '
      OnShow = tbsFileNameShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        377
        274)
      object lblFileName: TLabel
        Left = 8
        Top = 78
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'File Name:'
      end
      object Label4: TLabel
        Left = 16
        Top = 24
        Width = 42
        Height = 13
        Alignment = taRightJustify
        Caption = 'Schema:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object edtFileName: TEdit
        Left = 64
        Top = 75
        Width = 245
        Height = 21
        TabOrder = 1
        OnChange = SetState
      end
      object btnFileName: TButton
        Left = 313
        Top = 75
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 2
        OnClick = btnFileNameClick
      end
      object lblStep2: TStaticText
        Left = 8
        Top = 53
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
        TabOrder = 5
      end
      object StaticText2: TStaticText
        Left = 8
        Top = 132
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Specify object types to be exported'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
      object lstObjects: TCheckListBox
        Left = 8
        Top = 155
        Width = 360
        Height = 107
        OnClickCheck = lstObjectsClickCheck
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 4
      end
      object btnTransformNames: TCheckBox
        Left = 8
        Top = 109
        Width = 366
        Height = 17
        Caption = 'Replace Schema Name with generic schema identifier'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object cboTarget: TComboBox
        Left = 64
        Top = 21
        Width = 270
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = SetState
      end
      object StaticText1: TStaticText
        Left = 8
        Top = 3
        Width = 302
        Height = 17
        AutoSize = False
        Caption = 'Select a Schema'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
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
      Caption = 
        'Select a name for the target snapshot file, the object types to ' +
        'be exported.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Create Schema Snapshot'
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
    object btnNext: TButton
      Left = 54
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Export'
      Default = True
      Enabled = False
      TabOrder = 0
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
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 303
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Help'
      Enabled = False
      TabOrder = 2
    end
    object Button1: TButton
      Left = 137
      Top = 7
      Width = 80
      Height = 24
      Caption = 'Advanced'
      Enabled = False
      TabOrder = 3
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'Export Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 296
    Top = 8
  end
end
