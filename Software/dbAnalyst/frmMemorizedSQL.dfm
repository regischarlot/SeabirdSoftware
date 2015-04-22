object frmMemorizedSQL: TfrmMemorizedSQL
  Left = 360
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Memorized SQL Statements'
  ClientHeight = 326
  ClientWidth = 187
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tvSQL: TTreeView
    Left = 0
    Top = 25
    Width = 187
    Height = 301
    Align = alClient
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 0
    OnChange = tvSQLChange
    OnDblClick = popExecuteClick
    OnExpanding = tvSQLExpanding
    ExplicitHeight = 293
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 187
    Height = 25
    Align = alTop
    TabOrder = 1
    DesignSize = (
      187
      25)
    object cboRoots: TComboBox
      Left = 0
      Top = 2
      Width = 186
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 0
      TabOrder = 0
      OnChange = cboRootsChange
    end
  end
  object PopupMenu: TPopupMenu
    Left = 152
    Top = 16
    object popExecute: TMenuItem
      Caption = '&Execute'
      Default = True
      OnClick = popExecuteClick
    end
    object popPastetoQuery: TMenuItem
      Caption = '&Paste to Query'
      OnClick = popExecuteClick
    end
  end
end
