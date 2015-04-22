object frmDataImportEdit: TfrmDataImportEdit
  Left = 310
  Top = 257
  Width = 424
  Height = 315
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Data Import Object Properties'
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
  DesignSize = (
    416
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object lblObjectName: TLabel
    Left = 8
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Object Name'
  end
  object lblScript: TLabel
    Left = 8
    Top = 48
    Width = 69
    Height = 13
    Caption = 'Creation Script'
  end
  object edtObjectName: TEdit
    Left = 8
    Top = 24
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object mmoScript: TMemo
    Left = 8
    Top = 64
    Width = 397
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 175
    Top = 255
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 2
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 253
    Top = 255
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnCreateObject: TCheckBox
    Left = 291
    Top = 26
    Width = 113
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '(re)Create Object'
    TabOrder = 5
    OnClick = btnCreateObjectClick
  end
  object btnHelp: TButton
    Left = 331
    Top = 255
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Help'
    Enabled = False
    TabOrder = 3
  end
end
