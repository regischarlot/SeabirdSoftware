object frmDataDump: TfrmDataDump
  Left = 325
  Top = 143
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsToolWindow
  Caption = 'Extract Table Content'
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
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object lblDefinitionSource: TLabel
    Left = 32
    Top = 122
    Width = 302
    Height = 15
    AutoSize = False
    Caption = 'Select the format of output:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object bvlBottom: TBevel
    Left = 0
    Top = 375
    Width = 385
    Height = 2
    Align = alBottom
  end
  object lblTargetOutput: TLabel
    Left = 32
    Top = 244
    Width = 302
    Height = 20
    AutoSize = False
    Caption = 'Select the target output:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblProgress: TLabel
    Left = 32
    Top = 342
    Width = 73
    Height = 20
    AutoSize = False
    Caption = 'Progress:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblProgressText: TLabel
    Left = 109
    Top = 342
    Width = 225
    Height = 20
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblColumnHeading: TLabel
    Left = 34
    Top = 81
    Width = 302
    Height = 20
    AutoSize = False
    Caption = 'Header'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 385
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
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
    object btnExtract: TButton
      Left = 137
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Extract'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = btnExtractClick
    end
    object btnHelp: TButton
      Left = 303
      Top = 7
      Width = 80
      Height = 24
      Cancel = True
      Caption = '&Help'
      Enabled = False
      TabOrder = 2
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 4
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
        'Export content in one of multiple formats, to clipboard or file ' +
        'output.'
      WordWrap = True
    end
    object lblBoldTitle: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Extract Content as Text'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlOutputFormat: TPanel
    Left = 32
    Top = 143
    Width = 300
    Height = 99
    BevelOuter = bvNone
    TabOrder = 1
    object btnCSV: TRadioButton
      Left = 2
      Top = -1
      Width = 295
      Height = 17
      Caption = 'Comma-separated text output'
      TabOrder = 0
      OnClick = OnFormatOuput
    end
    object btnTab: TRadioButton
      Tag = 1
      Left = 2
      Top = 19
      Width = 295
      Height = 17
      Caption = 'Tab-separated text output'
      TabOrder = 1
      OnClick = OnFormatOuput
    end
    object btnHTML: TRadioButton
      Tag = 2
      Left = 2
      Top = 39
      Width = 295
      Height = 17
      Caption = 'HTML Table output'
      TabOrder = 2
      OnClick = OnFormatOuput
    end
    object btnXML: TRadioButton
      Tag = 3
      Left = 2
      Top = 59
      Width = 295
      Height = 17
      Caption = 'XML Table output'
      TabOrder = 3
      OnClick = OnFormatOuput
    end
    object btnInsertStatement: TRadioButton
      Tag = 4
      Left = 2
      Top = 78
      Width = 295
      Height = 17
      Caption = 'Insert Statement Output'
      TabOrder = 4
      OnClick = OnFormatOuput
    end
  end
  object pnlTargetOutput: TPanel
    Left = 32
    Top = 264
    Width = 305
    Height = 70
    BevelOuter = bvNone
    TabOrder = 2
    object btnClipboard: TRadioButton
      Tag = 1
      Left = 2
      Top = 0
      Width = 295
      Height = 17
      Caption = 'Clipboard'
      TabOrder = 0
      OnClick = OnTargetOutput
    end
    object btnFile: TRadioButton
      Tag = 2
      Left = 2
      Top = 20
      Width = 295
      Height = 17
      Caption = 'File'
      TabOrder = 1
      OnClick = OnTargetOutput
    end
    object edtFileName: TEdit
      Left = 19
      Top = 40
      Width = 257
      Height = 21
      TabOrder = 2
      OnChange = SetButtonState
    end
    object btnFileName: TButton
      Left = 280
      Top = 40
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btnFileNameClick
    end
  end
  object btnGenerateColumnHeadings: TCheckBox
    Left = 34
    Top = 99
    Width = 231
    Height = 17
    Caption = 'Generate Column Headings'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Export Files (*.export)|*.export|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 344
    Top = 88
  end
end
