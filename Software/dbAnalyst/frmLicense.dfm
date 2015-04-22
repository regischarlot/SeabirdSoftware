object frmLicensing: TfrmLicensing
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Application Licensing'
  ClientHeight = 171
  ClientWidth = 292
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
  object lblLicense: TLabel
    Left = 56
    Top = 47
    Width = 145
    Height = 13
    Caption = 'Please enter a license number:'
    Transparent = True
  end
  object lblLicenseInfo: TLabel
    Left = 5
    Top = 5
    Width = 284
    Height = 43
    AutoSize = False
    Caption = 
      'A software license is required to use this application. Such lic' +
      'ense can be obtained http://www.seabirdsoftware.com, under the s' +
      'oftware section.'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 4
    Top = 93
    Width = 284
    Height = 43
    AutoSize = False
    Caption = 
      'An expired license may be renewed by downloading a new time-limi' +
      'ted version of the software, or by purchasing a license at http:' +
      '//www.seabirdsoftware.com.'
    WordWrap = True
  end
  object edtLicense1: TEdit
    Left = 56
    Top = 64
    Width = 57
    Height = 21
    CharCase = ecUpperCase
    Color = 14150130
    MaxLength = 5
    TabOrder = 0
    OnChange = LicenseChange
    OnKeyPress = edtLicenseKeyPress
  end
  object btnOK: TButton
    Left = 134
    Top = 141
    Width = 75
    Height = 23
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
  end
  object edtLicense2: TEdit
    Left = 116
    Top = 64
    Width = 57
    Height = 21
    CharCase = ecUpperCase
    Color = 14150130
    MaxLength = 5
    TabOrder = 1
    OnChange = LicenseChange
    OnKeyPress = edtLicenseKeyPress
  end
  object edtLicense3: TEdit
    Left = 176
    Top = 64
    Width = 57
    Height = 21
    CharCase = ecUpperCase
    Color = 14150130
    MaxLength = 5
    TabOrder = 2
    OnChange = LicenseChange
    OnKeyPress = edtLicenseKeyPress
  end
  object btnCancel: TButton
    Left = 211
    Top = 141
    Width = 75
    Height = 23
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object btnVisit: TButton
    Left = 5
    Top = 141
    Width = 75
    Height = 25
    Caption = 'Visit Seabird!'
    TabOrder = 3
    OnClick = btnVisitClick
  end
end
