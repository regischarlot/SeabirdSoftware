object frmStatementBuildCondition: TfrmStatementBuildCondition
  Left = 293
  Top = 178
  BorderStyle = bsToolWindow
  Caption = 'Query Wizard: Condition'
  ClientHeight = 178
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    379
    178)
  PixelsPerInch = 96
  TextHeight = 13
  object lblConditionLHSColumn: TLabel
    Left = 3
    Top = 34
    Width = 35
    Height = 13
    Caption = 'Column'
  end
  object lblConditionOperator: TLabel
    Left = 151
    Top = 34
    Width = 41
    Height = 13
    Caption = 'Operator'
  end
  object bvlRHSValue: TBevel
    Left = 223
    Top = 106
    Width = 154
    Height = 38
    Shape = bsFrame
  end
  object Bevel1: TBevel
    Left = 223
    Top = 38
    Width = 154
    Height = 55
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 3
    Top = 2
    Width = 370
    Height = 26
    AutoSize = False
    Caption = 
      'Select a left hand side column, an operator, and either a right ' +
      'hand side column or a value.'
    WordWrap = True
  end
  object btnConditionRHSColumn: TRadioButton
    Left = 217
    Top = 30
    Width = 58
    Height = 17
    Caption = 'Column'
    TabOrder = 3
    OnClick = SetState
  end
  object btnConditionRHSDistinctValues: TButton
    Left = 351
    Top = 118
    Width = 21
    Height = 21
    Hint = 
      'Click here to select a value from all right-hand column distinct' +
      ' values'
    Caption = '...'
    Enabled = False
    TabOrder = 8
    OnClick = btnConditionRHSDistinctValuesClick
  end
  object btnConditionRHSValue: TRadioButton
    Left = 217
    Top = 98
    Width = 50
    Height = 17
    Caption = 'Value'
    TabOrder = 6
    OnClick = SetState
  end
  object cboConditionLHSColumn: TComboBox
    Left = 3
    Top = 50
    Width = 145
    Height = 21
    Hint = 'Select a right-hand side condition column'
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = SetState
  end
  object cboConditionOperator: TComboBox
    Left = 150
    Top = 50
    Width = 65
    Height = 21
    Hint = 'Select an operator'
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = SetState
  end
  object cboConditionRHSColumn: TComboBox
    Left = 227
    Top = 50
    Width = 145
    Height = 21
    Hint = 'Select a left-hand side condition column'
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    OnChange = SetState
  end
  object edtConditionRHSValue: TEdit
    Left = 227
    Top = 118
    Width = 121
    Height = 21
    Hint = 'Enter a Select a left-hand side condition value'
    Enabled = False
    TabOrder = 7
    OnChange = SetState
  end
  object btnOK: TButton
    Left = 146
    Top = 151
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    Enabled = False
    TabOrder = 9
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 224
    Top = 151
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object btnHelp: TButton
    Left = 302
    Top = 151
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Help'
    Enabled = False
    TabOrder = 11
  end
  object btnLeftOuterJoin: TCheckBox
    Left = 3
    Top = 73
    Width = 70
    Height = 17
    Caption = 'Outer Join'
    TabOrder = 1
    OnClick = btnOuterJoinClick
  end
  object btnRightOuterJoin: TCheckBox
    Left = 227
    Top = 73
    Width = 70
    Height = 17
    Caption = 'Outer Join'
    TabOrder = 5
    OnClick = btnOuterJoinClick
  end
end
