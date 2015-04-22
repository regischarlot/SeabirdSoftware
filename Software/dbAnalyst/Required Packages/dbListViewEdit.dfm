object frmdbListViewEdit: TfrmdbListViewEdit
  Left = 308
  Top = 238
  BorderStyle = bsSizeToolWin
  Caption = 'frmdbListViewEdit'
  ClientHeight = 188
  ClientWidth = 317
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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 317
    Height = 159
    ActivePage = tsOptions
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 157
    object tsData: TTabSheet
      Caption = '&Data'
      ExplicitHeight = 129
      object mmoValue: TMemo
        Left = 0
        Top = 0
        Width = 309
        Height = 131
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitHeight = 129
      end
    end
    object tsOptions: TTabSheet
      Caption = '&Options'
      ImageIndex = 1
      ExplicitHeight = 129
      object btnHexadecimalDisplay: TCheckBox
        Left = 4
        Top = 27
        Width = 165
        Height = 17
        Caption = '&Hexadecimal Display'
        TabOrder = 1
        OnClick = ButtonClick
      end
      object btnFixCRLF: TCheckBox
        Left = 4
        Top = 7
        Width = 117
        Height = 17
        Caption = 'Fix Stream CR/LF '
        TabOrder = 0
        OnClick = ButtonClick
      end
      object btnWrapDisplay: TCheckBox
        Left = 4
        Top = 47
        Width = 165
        Height = 17
        Caption = '&Wrap Display'
        TabOrder = 2
        OnClick = ButtonClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 159
    Width = 317
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      317
      29)
    object btnOK: TButton
      Left = 161
      Top = 4
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 239
      Top = 4
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
