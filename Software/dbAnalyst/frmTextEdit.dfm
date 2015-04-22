object frmTextEdit: TfrmTextEdit
  Left = 259
  Top = 217
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Edit SQL'
  ClientHeight = 285
  ClientWidth = 371
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
  PixelsPerInch = 96
  TextHeight = 13
  object mmoSQL: TSynEdit
    Left = 0
    Top = 0
    Width = 371
    Height = 254
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    OnEnter = onSelMove
    OnExit = onSelMove
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Width = 0
    Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    RightEdge = -1
    OnStatusChange = mmoSQLStatusChange
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 254
    Width = 371
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      371
      31)
    object btnOK: TButton
      Left = 138
      Top = 5
      Width = 75
      Height = 24
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 216
      Top = 5
      Width = 75
      Height = 24
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 294
      Top = 5
      Width = 75
      Height = 24
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Help'
      Enabled = False
      TabOrder = 2
    end
    object lblStatus: TStaticText
      Left = 2
      Top = 8
      Width = 135
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 3
    end
  end
end
