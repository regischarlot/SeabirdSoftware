object frmConnectionWizard: TfrmConnectionWizard
  Left = 672
  Top = 171
  ActiveControl = edtName
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Connection Wizard'
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 375
    Width = 385
    Height = 2
    Align = alBottom
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 385
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      385
      35)
    object btnCancel: TButton
      Left = 220
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnNext: TButton
      Left = 137
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Caption = '&Next'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnHelp: TButton
      Left = 303
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Caption = '&Help'
      Enabled = False
      TabOrder = 3
    end
    object btnPrevious: TButton
      Left = 54
      Top = 7
      Width = 80
      Height = 24
      Anchors = [akTop, akRight]
      Caption = '&Previous'
      Enabled = False
      TabOrder = 0
      OnClick = btnPreviousClick
    end
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
    TabOrder = 1
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
      Caption = 'Select a connection name, a database type and a default form.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 80
      Top = 6
      Width = 289
      Height = 17
      AutoSize = False
      Caption = 'Manage a Database Connection Profile'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 73
    Width = 385
    Height = 302
    ActivePage = tsName
    Align = alClient
    MultiLine = True
    TabOrder = 2
    TabStop = False
    object tsName: TTabSheet
      Caption = 'Step 1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblConnectionName: TLabel
        Left = 16
        Top = 46
        Width = 140
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Name:'
        WordWrap = True
      end
      object lblDatabaseType: TLabel
        Left = 16
        Top = 109
        Width = 140
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Database Type:'
        WordWrap = True
      end
      object lblConnectionNameDescription: TLabel
        Left = 16
        Top = 21
        Width = 522
        Height = 13
        Caption = 
          'Create a name for this database connection, such as '#39'Sales Datab' +
          'ase Connection'#39' or '#39'My Favorite Connection'#39'.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblDatabaseTypeDescription: TLabel
        Left = 16
        Top = 75
        Width = 252
        Height = 13
        Caption = 'Identify the type of database you want to connect to. '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblTool: TLabel
        Left = 24
        Top = 193
        Width = 65
        Height = 17
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Form:'
      end
      object lblToolDescription: TLabel
        Left = 40
        Top = 157
        Width = 256
        Height = 13
        Caption = 'Select the dbAnalyst form that will be initially displayed.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblUseOneConnectionDescription: TLabel
        Left = 50
        Top = 218
        Width = 648
        Height = 13
        Caption = 
          'By default, all windows create their own database connection. Us' +
          'e only one database connection for all tool windows after databa' +
          'se login'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblUseOneConnection: TLabel
        Left = 3
        Top = 237
        Width = 140
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Connection:'
        WordWrap = True
      end
      object edtName: TEdit
        Left = 160
        Top = 42
        Width = 160
        Height = 21
        TabOrder = 0
        OnExit = OnFieldExit
      end
      object cboDatabaseType: TComboBox
        Left = 160
        Top = 105
        Width = 160
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnExit = OnFieldExit
      end
      object cboDefaultForm: TComboBoxEx
        Left = 96
        Top = 190
        Width = 260
        Height = 22
        ItemsEx = <>
        Style = csExDropDownList
        TabOrder = 2
        OnExit = OnFieldExit
        Images = frmMain.ImageList
      end
      object cbxUseOneConnection: TCheckBox
        Left = 160
        Top = 237
        Width = 129
        Height = 17
        Caption = 'Use One Connection'
        TabOrder = 3
      end
    end
    object tsParameters: TTabSheet
      Caption = 'Step 2'
      ImageIndex = 1
      OnShow = tsParametersShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblProvidersDescription: TLabel
        Left = 16
        Top = 3
        Width = 245
        Height = 13
        Caption = 'Select a Provider to connect to the target database.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clActiveCaption
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblProviders: TLabel
        Left = 24
        Top = 22
        Width = 91
        Height = 13
        Caption = 'Database Provider:'
      end
      object lblProviderDescription: TLabel
        Left = 20
        Top = 49
        Width = 276
        Height = 27
        AutoSize = False
        Caption = 'Provider Description'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object cboProviders: TComboBox
        Left = 121
        Top = 22
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = OnFieldExit
      end
      object pcStep2: TPageControl
        Left = 16
        Top = 88
        Width = 289
        Height = 169
        TabOrder = 1
      end
    end
    object tsManual: TTabSheet
      Caption = 'Manual Setup'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        377
        274)
      object lblConnectionName2: TLabel
        Left = 16
        Top = 6
        Width = 140
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Name:'
        WordWrap = True
      end
      object lblDatabaseType2: TLabel
        Left = 16
        Top = 37
        Width = 140
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Database Type:'
        WordWrap = True
      end
      object lblTool2: TLabel
        Left = 40
        Top = 65
        Width = 65
        Height = 17
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Form:'
      end
      object lblLoginPrompt2: TLabel
        Left = 88
        Top = 96
        Width = 65
        Height = 13
        Caption = 'Login Prompt:'
      end
      object lblConnectionString2: TLabel
        Left = 48
        Top = 128
        Width = 87
        Height = 13
        Caption = 'Connection String:'
      end
      object edtName2: TEdit
        Left = 160
        Top = 2
        Width = 160
        Height = 21
        TabOrder = 0
        OnExit = OnFieldExit
      end
      object cboDatabaseType2: TComboBox
        Left = 160
        Top = 33
        Width = 160
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = OnFieldExit
        OnClick = OnFieldExit
        OnExit = OnFieldExit
      end
      object cboDefaultForm2: TComboBoxEx
        Left = 112
        Top = 62
        Width = 260
        Height = 22
        ItemsEx = <>
        Style = csExDropDownList
        TabOrder = 2
        OnExit = OnFieldExit
        Images = frmMain.ImageList
      end
      object btnConnectionString2: TButton
        Left = 142
        Top = 127
        Width = 163
        Height = 18
        Caption = 'Create/&Edit Connection String'
        TabOrder = 3
        OnClick = btnConnectionString2Click
      end
      object btnLoginPrompt2: TCheckBox
        Left = 160
        Top = 91
        Width = 169
        Height = 17
        Caption = '&Present a login prompt'
        TabOrder = 4
        OnClick = OnFieldExit
        OnExit = OnFieldExit
      end
      object btnTestConnection2: TButton
        Left = 283
        Top = 243
        Width = 91
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Test Connection'
        TabOrder = 5
        OnClick = OnTestConnection
      end
      object mmoConnectionString2: TMemo
        Left = 144
        Top = 152
        Width = 185
        Height = 68
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 6
      end
    end
  end
  object ImageList: TImageList
    Left = 16
    Top = 320
    Bitmap = {
      494C010103000400080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008D6D5900DDD6D3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007088900060809000607880005070
      8000506070004058600040485000303840002030300020203000101820001010
      1000101020000000000000000000000000000000000000000000000000001493
      CC001493CC001493CC00409AC600498EB4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BFB2AD00CE9F7000AB794E00DED8D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007088900090A0B00070B0D0000090
      D0000090D0000090D0000090C0001088C0001080B0001080B0002078A0002070
      90002048600077828B0000000000000000000000000000000000000000001493
      CC0077C5E20055C5F20030B0E600199DD5000B8FCA000181BD001C86BA004C9F
      C7002E93C2000000000000000000000000000000000000000000000000000000
      00000000000000000000C6BBBB00966C4C00FFF7C40094644200C3B8B7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008088900080C0D00090A8B00080E0
      FF0060D0FF0050C8FF0050C8FF0040C0F00030B0F00030A8F00020A0E0001090
      D0002068800048505A000000000000000000000000001190C5003A9DC8001493
      CC0030A4D500A5EBFF0067DDFF0067DDFF0067DDFF0067DDFF0046C5EE0029AD
      DE001898D1001A94CC0000000000000000000000000000000000000000000000
      00009072620089644C008C684B00CAAA8700FFDCB300AF87650054280E006643
      3500BEB0AB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008090A00080D0F00090A8B00090C0
      D00070D8FF0060D0FF0060D0FF0050C8FF0050C0FF0040B8F00030B0F00030A8
      F0001088D00020486000929CA80000000000000000001190C500A1DDF2001493
      CC0036AAE100BBEBF70074EEFF0074EEFF0074EEFF0074EEFF0074EEFF0074EE
      FF0056CEFA001797CF005591B300000000000000000000000000B6A39900A788
      6B00D2B69900E7CAA800FDE4BF00FFE9C700FFF8D100F5D6B300D8B89300AD89
      65005F30160082665B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008090A00080D8F00080C8E00090A8
      B00080E0FF0070D0FF0060D8FF0060D0FF0060D0FF0050C8FF0040C0F00040B8
      F00030B0F000206880005174880000000000000000001493CC0091D4EA001493
      CC005CC1F40077C5E20090FFFF0081FFFF0081FFFF0081FFFF0081FFFF0081FF
      FF0067DEFF003EB5E300249CCF000000000000000000C6B3A400CAB59A00EDD5
      B800FFEBCC00F8D6B700C0715000E9956400DD8D5D00DFA67C00FFE7C300F4D6
      B100D7B6940074482A00866C6200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008098A00090E0F00090E0FF0090A8
      B00090B8C00070D8FF0060D8FF0060D8FF0060D8FF0060D0FF0050D0FF0050C8
      FF0040B8F00030A0E0004664740098A1AF00000000001493CC0049BDFB001493
      CC0065CBFF0026A1D500C5EEF700CBFFFF00C0FFFF00A9FFFF0098FFFF0098FF
      FF0070DEFF007CD7FA001D9ACF0071B0CE0000000000CBB8A200EED8C100FFEF
      D100FFEECF00FFFFE200B48163007A0101008F2F0700F7E5C400FFEDCB00FFE7
      C300F5D7B700DABD9B005C2E1500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008098A00090E0F000A0E8FF0080C8
      E00090A8B00080E0FF0080E0FF0080E0FF0080E0FF0080E0FF0080E0FF0080E0
      FF0070D8FF0070D8FF0050A8D000727F8D00000000001493CC0049BDFB001493
      CC007BE0FF005FD8FB003ABDE6002CB1DE003AB1D90088CEE600DEFFFF00CEFF
      FF0088DEFF0097DEFF006DC5E500239ACD00C3AF9D00E8DBC700FFF0D700FFEC
      D200FFE7CE00FFFFED00CFB2950070010100A4593800FFFFF000FFE7C900FFE5
      C600FFEAC900FCE3C500AD8A6C008D736A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000090A0A000A0E8F000A0E8FF00A0E8
      FF0090B0C00090B0C00090A8B00090A8B00080A0B00080A0B0008098A0008098
      A0008090A0008090A0008088900070889000000000001493CC0049BDFB001493
      CC0092F8FF007FFBFF007FFBFF007FFBFF007FFBFF004ED4EA004CBDDE0052BD
      DE0067BDDE0077C5E200A4DAED001493CC00CFBFAE00F8E7D500FFF4DF00FFEC
      D600FFECD600FFFFF300CDAF940075010100A5583700FFFFF100FFE8CF00FFE5
      CA00FFEACF00FFEBD100E4CAAF00856759000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000090A0B000A0E8F000A0F0FF00A0E8
      FF00A0E8FF0080D8FF0060D8FF0060D8FF0060D8FF0060D8FF0060D8FF0060D8
      FF0070889000000000000000000000000000000000001493CC0060D5FF001493
      CC00B1FEFF0081FFFF0081FFFF00ACFFFF00B6F6FB00BDFFFF009AF7FB009FFF
      FF0024ADD500378AB5001493CC001493CC00CFC2B700FDEFE000FFF4E300FFEF
      DE00FFF0DC00FFFFFF00D6C1AA0078010100A6593700FFFFF800FFEDD500FFE9
      D000FFECD200FFF2D900ECD7C000836759000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000090A0B000A0F0F000B0F0F000A0F0
      FF00A0E8FF00A0E8FF0070D8FF0090A0A0008098A0008098A0008090A0008090
      900070889000000000000000000000000000000000001493CC007BF7FF0026A1
      D50081CEE60090DEEE00CEFFFF0088CEE6001493CC001493CC001493CC001493
      CC001493CC008AB8D0000000000000000000D0C7BD00FDF5E900FFF8EB00FFF2
      E600FFFDEF00EFE6DA008C3E1F00660101008C3D2400FFFFFE00FFF0DD00FFEB
      D600FFEED900FFFAE500F2DEC900947B6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000090A8B000A0D0E000B0F0F000B0F0
      F000A0F0FF00A0E8FF0090A0B00097AFB9000000000000000000000000000000
      000000000000000000000000000000000000000000001493CC0081FFFF0081FF
      FF001493CC001493CC001493CC001493CC0081FFFF0081FFFF000181BD000000
      000000000000000000000000000000000000DDD2C600F7F4EF00FFFFF900FFF6
      EC00FFFFF900E6D9C900B3938200BAA59A00C2AEA300FFFFF800FFF4E300FFEE
      DC00FFFAEB00FFFFFC00E2CAB400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000090A8B00090A8B00090A8
      B00090A8B00090A8B0009EAFB800000000000000000000000000000000000000
      000000000000ABA4A9000000000000000000000000001493CC00C5FFFF00B6FF
      FF00C5FFFF001493CC001493CC001493CC001493CC001493CC001493CC000000
      000000000000000000000000000000000000F0E8E000E4E5E500FFFFFF00FFFF
      FA00FFFBF300FFFFFF00FFFFFA00F9C8A600FFFEEE00FFFFFF00FFF5E800FFFA
      EF00FFFFFF00FFFFFF00AC948500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000090786000A59B9900B3BB
      C600B1B6C000A0908000ADABAB0090786000000000006491AF001493CC001493
      CC001493CC007694AC0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E9E4DF00EDEDEF00FFFF
      FF00FFFFFF00FFFFFF008E584F005E0101009A462900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00D0BBAC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEB2B500A0908000A088
      8000B0988000A9A5A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EDE8E300F2F4
      F500FFFFFF00FFFFFF00D7CEC90063424500C5B5AF00FFFFFF00FFFFFF00FFFF
      FE00E5DBD2000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F2F1
      F000E6DFD800F5F2EE00FFFFFF00FFFFFF00FFFFFF00FAF7F300D4C8BF00E1DA
      D700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFF9F00000007E0FFFE1F0000
      0003E007FC1F000000038003F007000000018001C00300000001800180010000
      0000800080010000000080000000000000008000000000000007800000000000
      000780030000000000FF801F0001000081FB801F00010000FF8083FF80030000
      FF83FFFFC0070000FFFFFFFFE00F000000000000000000000000000000000000
      000000000000}
  end
end
