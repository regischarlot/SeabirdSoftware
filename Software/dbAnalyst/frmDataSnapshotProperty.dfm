object frmDataSnapshotProperty: TfrmDataSnapshotProperty
  Left = 385
  Top = 337
  BorderStyle = bsSizeToolWin
  Caption = 'Properties'
  ClientHeight = 412
  ClientWidth = 354
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
    Width = 354
    Height = 2
    Align = alBottom
    ExplicitWidth = 385
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 354
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      354
      73)
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
      Width = 260
      Height = 42
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Detail comparison results resulting with SQL Statements '
      WordWrap = True
      ExplicitWidth = 289
    end
    object Label1: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Object Comparison Properties'
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
    Width = 354
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      354
      35)
    object btnClose: TButton
      Left = 191
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Close'
      ModalResult = 1
      TabOrder = 0
    end
    object btnHelp: TButton
      Left = 274
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Caption = '&Help'
      Enabled = False
      TabOrder = 1
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 73
    Width = 354
    Height = 302
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlCenterTop: TPanel
      Left = 0
      Top = 0
      Width = 354
      Height = 302
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      ExplicitHeight = 145
      object lblDetails: TLabel
        Left = 4
        Top = 4
        Width = 346
        Height = 13
        Align = alTop
        Caption = 'Details'
        ExplicitWidth = 32
      end
      object mmoDetails: TMemo
        Left = 4
        Top = 17
        Width = 346
        Height = 281
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitHeight = 124
      end
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
