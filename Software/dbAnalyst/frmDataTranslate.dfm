object frmDataTranslate: TfrmDataTranslate
  Left = 221
  Top = 104
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsToolWindow
  Caption = 'Schema Translation'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblDefinitionSource: TLabel
    Left = 32
    Top = 97
    Width = 302
    Height = 30
    AutoSize = False
    Caption = 
      'Select a Database Type that will indicate the target relational ' +
      'database for translation. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clActiveCaption
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Bevel1: TBevel
    Left = 0
    Top = 375
    Width = 385
    Height = 2
    Align = alBottom
  end
  object lblDatabaseType: TLabel
    Left = 32
    Top = 143
    Width = 136
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Database Type:'
    WordWrap = True
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 385
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
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
    object btnTranslate: TButton
      Left = 137
      Top = 7
      Width = 80
      Height = 24
      Caption = '&Translate'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
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
  object cboXML: TComboBox
    Left = 174
    Top = 140
    Width = 160
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnClick = cboXMLClick
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
    TabOrder = 2
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
      Caption = 'Translate a Database Schema'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
end
