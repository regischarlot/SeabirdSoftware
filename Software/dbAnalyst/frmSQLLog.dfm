object frmSQLLog: TfrmSQLLog
  Left = 360
  Top = 209
  Width = 199
  Height = 356
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lvLog: TListView
    Left = 0
    Top = 0
    Width = 187
    Height = 318
    Align = alClient
    Columns = <
      item
        Caption = '#'
        Width = 30
      end
      item
        AutoSize = True
        Caption = 'SQL'
      end
      item
        Alignment = taRightJustify
        Caption = 'Time'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = popExecuteClick
    OnKeyPress = lvLogKeyPress
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
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
    object popParse: TMenuItem
      Caption = 'P&arse'
      OnClick = popParseClick
    end
    object popSavetoFile: TMenuItem
      Caption = '&Save to File'
      Enabled = False
    end
    object popClear: TMenuItem
      Caption = 'Clear'
      OnClick = popClearClick
    end
  end
end
