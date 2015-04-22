object frmStatementBuildDistinctValues: TfrmStatementBuildDistinctValues
  Left = 293
  Top = 178
  BorderStyle = bsToolWindow
  Caption = 'Query Wizard: Distinct Values'
  ClientHeight = 225
  ClientWidth = 259
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
    259
    225)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 26
    Top = 198
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 104
    Top = 198
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 182
    Top = 198
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Help'
    Enabled = False
    TabOrder = 3
  end
  object lstDistinctValues: TListBox
    Left = 2
    Top = 3
    Width = 254
    Height = 191
    ItemHeight = 13
    TabOrder = 0
    OnClick = lstDistinctValuesClick
    OnDblClick = btnOKClick
  end
end
