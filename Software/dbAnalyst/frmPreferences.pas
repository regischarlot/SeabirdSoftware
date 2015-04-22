unit frmPreferences;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, PreferenceLib, ConnectionLib,
  PreferencePanel, ExtCtrls,
  daGlobals;

type
  TfrmPreferences = class(TForm)
    pnlBottom: TPanel;
    PageControl: TPageControl;
    tbsR1: TTabSheet;
    btnOK: TButton;
    btnCancel: TButton;
    dlgColor: TColorDialog;
    dlgFont: TFontDialog;
    tbsL: TTabSheet;
    lblLicense: TLabel;
    lblLicenseText: TLabel;
    Bevel1: TBevel;
    btnLicense: TButton;
    lblBrowseSeabird: TLabel;
    btnHelp: TButton;
    tbsH1: TTabSheet;
    Image1: TImage;
    tbsC1: TTabSheet;
    lblFieldDisplayWidth: TLabel;
    edtFieldDisplayWidth: TEdit;
    btnudFieldDisplayWidth: TUpDown;
    btnWrapSQLCommand: TCheckBox;
    btnWrapText: TCheckBox;
    tbsQB1: TTabSheet;
    btnEnableHilighting: TCheckBox;
    Image2: TImage;
    btnWrapSQLOutput: TCheckBox;
    btnAutoCommit: TCheckBox;
    tbsSM1: TTabSheet;
    lblTimeInterval: TLabel;
    edtTimeInterval: TEdit;
    udTimeInterval: TUpDown;
    btnEnableTimer: TCheckBox;
    btnEditorRefreshObject: TCheckBox;
    btnCreateScriptHint: TCheckBox;
    btnShowDatasetRowCount: TCheckBox;
    btnMemorizeAllStatements: TCheckBox;
    btnResizeProportionally: TCheckBox;
    btnShowOutputHeader: TCheckBox;
    btnEnableCurrentLineBackground: TCheckBox;
    btnShowSpecialChars: TCheckBox;
    btnAddSQLStmtOutput: TCheckBox;
    btnPromptBeforeClosingWindow: TCheckBox;
    pnlLeft: TPanel;
    tvOptions: TTreeView;
    tbsR2: TTabSheet;
    tbsR3: TTabSheet;
    tbsC2: TTabSheet;
    tbsC3: TTabSheet;
    tbsSM2: TTabSheet;
    tbsSM3: TTabSheet;
    tbsH2: TTabSheet;
    tbsLT1: TTabSheet;
    ppRE_GraphFont: TPreferencePanel;
    ppRE_GraphShadowFont: TPreferencePanel;
    ppRE_ErrorFont: TPreferencePanel;
    ppRE_ObjectFont: TPreferencePanel;
    ppRE_HeaderFont: TPreferencePanel;
    ppRE_BackgroundNode: TPreferencePanel;
    ppRE_Node: TPreferencePanel;
    ppRE_SelectedNode: TPreferencePanel;
    ppRE_ChildNode: TPreferencePanel;
    ppRE_ParentNode: TPreferencePanel;
    ppRE_ShadowNode: TPreferencePanel;
    ppRE_EditorRequiredField: TPreferencePanel;
    ppSC_ConsoleFont: TPreferencePanel;
    ppSC_GridFont: TPreferencePanel;
    ppSC_BackgroundColor: TPreferencePanel;
    ppSC_GridBackgroundColor: TPreferencePanel;
    ppSC_GridSelectedCell: TPreferencePanel;
    ppSC_GridSelectedLine: TPreferencePanel;
    ppSC_GridSelectedColumn: TPreferencePanel;
    ppSC_CurrLineBackgroundColor: TPreferencePanel;
    ppSC_ExecutionTab: TPreferencePanel;
    ppQW_GridBackgroundColor: TPreferencePanel;
    ppQW_GridSelectedCell: TPreferencePanel;
    ppQW_GridSelectedLine: TPreferencePanel;
    ppQW_GridSelectedColumn: TPreferencePanel;
    ppSH_Keyword: TPreferencePanel;
    ppSH_Datatype: TPreferencePanel;
    ppSH_Function: TPreferencePanel;
    ppSH_String: TPreferencePanel;
    ppSH_Comment: TPreferencePanel;
    ppSH_Package: TPreferencePanel;
    ppSH_Exception: TPreferencePanel;
    ppSH_Number: TPreferencePanel;
    ppSH_AnchorBackground: TPreferencePanel;
    ppSH_Anchor: TPreferencePanel;
    Bevel2: TBevel;
    tbsC4: TTabSheet;
    ppSC_ScriptTab: TPreferencePanel;
    ppSC_InellisenseColor: TPreferencePanel;
    ppSC_IntellisenseDismiss: TCheckBox;
    ppSC_IntellisenseDismissAmountLabel: TLabel;
    ppSC_IntellisenseDismissAmount: TEdit;
    ppSC_IntellisenseDismissAmountUpDown: TUpDown;
    ppSC_IntellisenseDelayPopupLabel: TLabel;
    ppSC_IntellisenseDelayPopup: TComboBox;
    ppSC_NullCellColor: TPreferencePanel;
    ppSM_GridFont: TPreferencePanel;
    ppSM_BackgroundColor: TPreferencePanel;
    ppSM_GridBackgroundColor: TPreferencePanel;
    ppSM_GridSelectedCell: TPreferencePanel;
    ppSM_GridSelectedLine: TPreferencePanel;
    ppSM_GridSelectedColumn: TPreferencePanel;
    ppSM_NullCellColor: TPreferencePanel;

    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblBrowseSeabirdClick(Sender: TObject);
    procedure btnLicenseClick(Sender: TObject);
    procedure edtFieldDisplayWidthKeyPress(Sender: TObject; var Key: Char);
    procedure btnudFieldDisplayWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure edtFieldDisplayWidthExit(Sender: TObject);
    procedure btnEnableHilightingClick(Sender: TObject);
    procedure btnEnableTimerClick(Sender: TObject);
    procedure SetPage(value: TeForm);
    procedure FormCreate(Sender: TObject);
    procedure tvOptionsChange(Sender: TObject; Node: TTreeNode);
    procedure ppSC_IntellisenseDismissClick(Sender: TObject);
    procedure ppSC_IntellisenseDismissAmountUpDownClick(Sender: TObject; Button: TUDBtnType);

  private
    m_objPreferences: TcPreferenceList;
    m_objConnection: TcConnection;

  public
    property PreferenceList: TcPreferenceList read m_objPreferences write m_objPreferences;
    property Connection: TcConnection read m_objConnection write m_objConnection;
  end;

implementation

uses
  daResourceStrings,
  Math,
  CustomControls,
  frmWebService;      // TfrmWebService

{$R *.DFM}

const
  kaiINTELLISENSEDELAYPOPUP: array[0 .. 9] of integer =
    (500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000);
  kasINTELLISENSEDELAYPOPUP: array[0 .. 9] of String=
    ('0.5 seconds', '1 second', '1.5 seconds', '2 seconds', '2.5 seconds', '3 seconds', '3.5 seconds', '4 seconds', '4.5 seconds', '5 seconds');

// TfrmPreferences
//   FormCreate
//
procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

// TfrmPreferences
//   FormShow
//
procedure TfrmPreferences.FormShow(Sender: TObject);
var
  i: longint;
begin
  if m_objPreferences <> nil then
  begin
    //
    // Reverse Engineering
    btnWrapText.Checked                    := m_objPreferences.StringVal[krsPREF_WRAPTEXT] = krsTRUE;
    lblLicenseText.Caption                 := m_objPreferences.LicenseStatus;
    btnEditorRefreshObject.Checked         := AnsiCompareText(m_objPreferences.StringVal[krsPREF_EDITORRELOADOBJECT], krsTRUE) = 0;
    btnCreateScriptHint.Checked            := AnsiCompareText(m_objPreferences.StringVal[krsPREF_CREATESCRIPTHINT], krsFALSE) <> 0;
    //                                     
    ppRE_Node.Color                        := m_objPreferences.Color[krsPREF_COLORNODE];
    ppRE_SelectedNode.Color                := m_objPreferences.Color[krsPREF_COLORSELECTEDNODE];
    ppRE_ChildNode.Color                   := m_objPreferences.Color[krsPREF_COLORDEPENDENCYNODE];
    ppRE_ParentNode.Color                  := m_objPreferences.Color[krsPREF_COLORTODEPENDENCYNODE];
    ppRE_BackgroundNode.Color              := m_objPreferences.Color[krsPREF_COLORBACKGROUNDCOLOR];
    ppRE_ShadowNode.Color                  := m_objPreferences.Color[krsPREF_COLORSHADOWNODE];
    ppRE_EditorRequiredField.Color         := m_objPreferences.Color[krsPREF_COLOREDITORREQUIREDFIELD];
    //                                     
    ppRE_GraphFont.Font                    := m_objPreferences.Font[krsPREF_FONTGRAPH];
    ppRE_GraphShadowFont.Font              := m_objPreferences.Font[krsPREF_FONTSHADOWGRAPH];
    ppRE_ErrorFont.Font                    := m_objPreferences.Font[krsPREF_FONTERROR];
    ppRE_ObjectFont.Font                   := m_objPreferences.Font[krsPREF_FONTOBJECTTEXT];
    ppRE_HeaderFont.Font                   := m_objPreferences.Font[krsPREF_FONTOBJECTHEADER];
    //
    // SQL Console
    edtFieldDisplayWidth.Text              := m_objPreferences.StringVal[krsPREF_FIELDWIDTH];
    btnWrapSQLCommand.Checked              := m_objPreferences.StringVal[krsPREF_CONSOLEWRAPTEXT] = krsTRUE;
    ppSC_BackgroundColor.Color             := m_objPreferences.Color[krsPREF_CONSOLEBACKGROUNDCOLOR];
    ppSC_GridBackgroundColor.Color         := m_objPreferences.Color[krsPREF_CONSOLEGRIDBACKGROUNDCOLOR];
    ppSC_GridSelectedCell.Color            := m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTIONCOLOR];
    ppSC_GridSelectedLine.Color            := m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTEDLINECOLOR];
    ppSC_GridSelectedColumn.Color          := m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTEDCOLUMNCOLOR];
    ppSC_CurrLineBackgroundColor.Color     := m_objPreferences.Color[krsPREF_CONSOLECURRENTLINEBACKROUND];
    ppSC_NullCellColor.Color               := m_objPreferences.Color[krsPREF_CONSOLEGRIDNULLCELLCOLOR];
    ppSC_ExecutionTab.Color                := m_objPreferences.Color[krsPREF_CONSOLEEXCUTIONTAB];
    ppSC_ScriptTab.Color                   := m_objPreferences.Color[krsPREF_CONSOLESCRIPTTAB];
    btnWrapSQLOutput.Checked               := m_objPreferences.StringVal[krsPREF_CONSOLEOUTPUTWRAPTEXT] = krsTRUE;
    btnAutoCommit.Checked                  := m_objPreferences.StringVal[krsPREF_CONSOLEAUTOCOMMIT] <> krsFALSE;
    btnShowDatasetRowCount.Checked         := m_objPreferences.StringVal[krsPREF_CONSOLESHOWDATASETROWCOUNT] <> krsFALSE;
    btnMemorizeAllStatements.Checked       := m_objPreferences.StringVal[krsPREF_CONSOLEMEMORIZEALLSTATEMENTS] = krsTRUE;
    btnResizeProportionally.Checked        := m_objPreferences.StringVal[krsPREF_CONSOLERESIZEPROPORTIONALLY] = krsTRUE;
    btnShowOutputHeader.Checked            := m_objPreferences.StringVal[krsPREF_CONSOLESHOWOUTPUTHEADER] <> krsFALSE;
    btnEnableCurrentLineBackground.Checked := m_objPreferences.StringVal[krsPREF_CONSOLEENABLECURRENTLINEBACKGROUND] <> krsFALSE;
    btnShowSpecialChars.Checked            := m_objPreferences.StringVal[krsPREF_CONSOLESHOWSPECIALCHARS] = krsTRUE;
    btnAddSQLStmtOutput.Checked            := m_objPreferences.StringVal[krsPREF_CONSOLEADDSQLSTATEMENTOUTPUT] = krsTRUE;
    btnPromptBeforeClosingWindow.Checked   := m_objPreferences.StringVal[krsPREF_CONSOLEPROMPTBEFORECLOSINGWINDOW] <> krsFALSE;
    // Console IntelliSense
    ppSC_InellisenseColor.Color            := m_objPreferences.Color[krsPREF_CONSOLEINTELLISENSECOLOR];
    ppSC_IntellisenseDismiss.Checked       := m_objPreferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISS] <> krsFALSE;
    ppSC_IntellisenseDismissAmount.Text    := m_objPreferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISSVALUE];
    ppSC_IntellisenseDismissClick(nil);
    //
    with ppSC_IntellisenseDelayPopup do
    try
      Items.BeginUpdate;
      Items.Clear;
      for i := low(kaiINTELLISENSEDELAYPOPUP) to high(kaiINTELLISENSEDELAYPOPUP) do
      begin
        Items.AddObject(kasINTELLISENSEDELAYPOPUP[i], pointer(kaiINTELLISENSEDELAYPOPUP[i]));
        if kaiINTELLISENSEDELAYPOPUP[i] = m_objPreferences.IntegerVal[krsPREF_CONSOLEINTELLISENSEDELAY] then
          ItemIndex := i;
      end;
    finally
      Items.EndUpdate;
    end;
    //
    ppSC_ConsoleFont.Font                  := m_objPreferences.Font[krsPREF_FONTCONSOLE];
    ppSC_GridFont.Font                     := m_objPreferences.Font[krsPREF_FONTCONSOLEGRID];
    //
    // Syntax Hilighting
    btnEnableHilighting.Checked            := m_objPreferences.StringVal[krsPREF_ENABLESYNTAXHILITING] <> krsFALSE;
    ppSH_Keyword.Color                     := m_objPreferences.Color[krsPREF_SYNTAXKEYWORD];
    ppSH_Datatype.Color                    := m_objPreferences.Color[krsPREF_SYNTAXDATATYPE];
    ppSH_Function.Color                    := m_objPreferences.Color[krsPREF_SYNTAXFUNCTION];
    ppSH_String.Color                      := m_objPreferences.Color[krsPREF_SYNTAXMARK];
    ppSH_Comment.Color                     := m_objPreferences.Color[krsPREF_SYNTAXCOMMENT];
    ppSH_Package.Color                     := m_objPreferences.Color[krsPREF_SYNTAXPACKAGE];
    ppSH_Exception.Color                   := m_objPreferences.Color[krsPREF_SYNTAXEXCEPTION];
    ppSH_Number.Color                      := m_objPreferences.Color[krsPREF_SYNTAXNUMBER];
    ppSH_Anchor.Color                      := m_objPreferences.Color[krsPREF_SYNTAXANCHOR];
    ppSH_AnchorBackground.Color            := m_objPreferences.Color[krsPREF_SYNTAXANCHORHOVER];
    //
    // Query Wizard
    ppQW_GridBackgroundColor.Color         := m_objPreferences.Color[krsPREF_QUERYGRIDBACKGROUNDCOLOR];
    ppQW_GridSelectedCell.Color            := m_objPreferences.Color[krsPREF_QUERYGRIDSELECTIONCOLOR];
    ppQW_GridSelectedLine.Color            := m_objPreferences.Color[krsPREF_QUERYGRIDSELECTEDLINECOLOR];
    ppQW_GridSelectedColumn.Color          := m_objPreferences.Color[krsPREF_QUERYGRIDSELECTEDCOLUMNCOLOR];
    //
    // Session Manager
    btnEnableTimer.Checked                 := m_objPreferences.StringVal[krsPREF_SESSIONENABLEDTIMER] = krsTRUE;
    edtTimeInterval.Text                   := m_objPreferences.StringVal[krsPREF_SESSIONTIMEINTERVAL];
    ppSM_GridFont.Font                     := m_objPreferences.Font[krsPREF_SESSIONGRIDFONT];
    ppSM_BackgroundColor.Color             := m_objPreferences.Color[krsPREF_SESSIONBACKGROUNDCOLOR];
    ppSM_GridBackgroundColor.Color         := m_objPreferences.Color[krsPREF_SESSIONGRIDBACKGROUNDCOLOR];
    ppSM_GridSelectedCell.Color            := m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTIONCOLOR];
    ppSM_GridSelectedLine.Color            := m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTEDLINECOLOR];
    ppSM_GridSelectedColumn.Color          := m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTEDCOLUMNCOLOR];
    ppSM_NullCellColor.Color               := m_objPreferences.Color[krsPREF_SESSIONGRIDNULLCELLCOLOR];
  end;
  //tbsSchemaExplorer.Caption := kasFORMSET[efsObjects];
  //tbsSQLConsole.Caption := kasFORMSET[efsQuery];
  //tbsQueryWizard.Caption := kasFORMSET[efsGrid];
  for i := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[i].TabVisible := FALSE;
  tvOptions.FullExpand;
  tvOptions.Items[0].Selected := TRUE;
  btnEnableTimerClick(Sender);
end;

// TfrmPreferences
//   btnOKClick
//
procedure TfrmPreferences.btnOKClick(Sender: TObject);
begin
  if m_objPreferences <> nil then
  begin
    //
    // Reverse Engineering
    m_objPreferences.StringVal[krsPREF_WRAPTEXT]                           := kasBOOL[btnWrapText.Checked];
    m_objPreferences.StringVal[krsPREF_EDITORRELOADOBJECT]                 := kasBOOL[btnEditorRefreshObject.Checked];
    m_objPreferences.StringVal[krsPREF_CREATESCRIPTHINT]                   := kasBOOL[btnCreateScriptHint.Checked];
    //
    m_objPreferences.Color[krsPREF_COLORNODE]                              := ppRE_Node.Color;
    m_objPreferences.Color[krsPREF_COLORSELECTEDNODE]                      := ppRE_SelectedNode.Color;
    m_objPreferences.Color[krsPREF_COLORDEPENDENCYNODE]                    := ppRE_ChildNode.Color;
    m_objPreferences.Color[krsPREF_COLORTODEPENDENCYNODE]                  := ppRE_ParentNode.Color;
    m_objPreferences.Color[krsPREF_COLORBACKGROUNDCOLOR]                   := ppRE_BackgroundNode.Color;
    m_objPreferences.Color[krsPREF_COLORSHADOWNODE]                        := ppRE_ShadowNode.Color;
    m_objPreferences.Color[krsPREF_COLOREDITORREQUIREDFIELD]               := ppRE_EditorRequiredField.Color;
    //
    m_objPreferences.Font[krsPREF_FONTGRAPH]                               := ppRE_GraphFont.Font;
    m_objPreferences.Font[krsPREF_FONTSHADOWGRAPH]                         := ppRE_GraphShadowFont.Font;
    m_objPreferences.Font[krsPREF_FONTERROR]                               := ppRE_ErrorFont.Font;
    m_objPreferences.Font[krsPREF_FONTOBJECTTEXT]                          := ppRE_ObjectFont.Font;
    m_objPreferences.Font[krsPREF_FONTOBJECTHEADER]                        := ppRE_HeaderFont.Font;
    //
    // SQL Console
    m_objPreferences.StringVal[krsPREF_FIELDWIDTH]                         := edtFieldDisplayWidth.Text;
    m_objPreferences.StringVal[krsPREF_CONSOLEWRAPTEXT]                    := kasBOOL[btnWrapSQLCommand.Checked];
    m_objPreferences.Color[krsPREF_CONSOLEBACKGROUNDCOLOR]                 := ppSC_BackgroundColor.Color;
    m_objPreferences.Color[krsPREF_CONSOLEGRIDBACKGROUNDCOLOR]             := ppSC_GridBackgroundColor.Color;
    m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTIONCOLOR]              := ppSC_GridSelectedCell.Color;
    m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTEDLINECOLOR]           := ppSC_GridSelectedLine.Color;
    m_objPreferences.Color[krsPREF_CONSOLEGRIDSELECTEDCOLUMNCOLOR]         := ppSC_GridSelectedColumn.Color;
    m_objPreferences.Color[krsPREF_CONSOLECURRENTLINEBACKROUND]            := ppSC_CurrLineBackgroundColor.Color;
    m_objPreferences.Color[krsPREF_CONSOLEGRIDNULLCELLCOLOR]               := ppSC_NullCellColor.Color;
    m_objPreferences.Color[krsPREF_CONSOLEEXCUTIONTAB]                     := ppSC_ExecutionTab.Color;
    m_objPreferences.Color[krsPREF_CONSOLESCRIPTTAB]                       := ppSC_ScriptTab.Color;
    m_objPreferences.StringVal[krsPREF_CONSOLEOUTPUTWRAPTEXT]              := kasBOOL[btnWrapSQLOutput.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEAUTOCOMMIT]                  := kasBOOL[btnAutoCommit.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLESHOWDATASETROWCOUNT]         := kasBOOL[btnShowDatasetRowCount.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEMEMORIZEALLSTATEMENTS]       := kasBOOL[btnMemorizeAllStatements.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLERESIZEPROPORTIONALLY]        := kasBOOL[btnResizeProportionally.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLESHOWOUTPUTHEADER]            := kasBOOL[btnShowOutputHeader.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEENABLECURRENTLINEBACKGROUND] := kasBOOL[btnEnableCurrentLineBackground.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLESHOWSPECIALCHARS]            := kasBOOL[btnShowSpecialChars.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEADDSQLSTATEMENTOUTPUT]       := kasBOOL[btnAddSQLStmtOutput.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEPROMPTBEFORECLOSINGWINDOW]   := kasBOOL[btnPromptBeforeClosingWindow.Checked];
    m_objPreferences.Font[krsPREF_FONTCONSOLE]                             := ppSC_ConsoleFont.Font;
    m_objPreferences.Font[krsPREF_FONTCONSOLEGRID]                         := ppSC_GridFont.Font;
    // Console IntelliSense
    m_objPreferences.Color[krsPREF_CONSOLEINTELLISENSECOLOR]               := ppSC_InellisenseColor.Color;
    m_objPreferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISS]         := kasBOOL[ppSC_IntellisenseDismiss.Checked];
    m_objPreferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISSVALUE]    := ppSC_IntellisenseDismissAmount.Text;
    //
    with ppSC_IntellisenseDelayPopup do
    begin
      if ItemIndex <> kiUNDEFINED then
        m_objPreferences.IntegerVal[krsPREF_CONSOLEINTELLISENSEDELAY] := longint(Items.Objects[ItemIndex])
      else
        m_objPreferences.IntegerVal[krsPREF_CONSOLEINTELLISENSEDELAY] := kiINTELLLISENSEDELAY;
    end;
    //
    // Syntax Hilighting
    m_objPreferences.StringVal[krsPREF_ENABLESYNTAXHILITING]               := kasBOOL[btnEnableHilighting.Checked];
    m_objPreferences.Color[krsPREF_SYNTAXKEYWORD]                          := ppSH_Keyword.Color;
    m_objPreferences.Color[krsPREF_SYNTAXDATATYPE]                         := ppSH_Datatype.Color;
    m_objPreferences.Color[krsPREF_SYNTAXFUNCTION]                         := ppSH_Function.Color;
    m_objPreferences.Color[krsPREF_SYNTAXMARK]                             := ppSH_String.Color;
    m_objPreferences.Color[krsPREF_SYNTAXCOMMENT]                          := ppSH_Comment.Color;
    m_objPreferences.Color[krsPREF_SYNTAXPACKAGE]                          := ppSH_Package.Color;
    m_objPreferences.Color[krsPREF_SYNTAXEXCEPTION]                        := ppSH_Exception.Color;
    m_objPreferences.Color[krsPREF_SYNTAXNUMBER]                           := ppSH_Number.Color;
    m_objPreferences.Color[krsPREF_SYNTAXANCHOR]                           := ppSH_Anchor.Color;
    m_objPreferences.Color[krsPREF_SYNTAXANCHORHOVER]                      := ppSH_AnchorBackground.Color;
    //
    // Query Wizard
    m_objPreferences.Color[krsPREF_QUERYGRIDBACKGROUNDCOLOR]               := ppQW_GridBackgroundColor.Color;
    m_objPreferences.Color[krsPREF_QUERYGRIDSELECTIONCOLOR]                := ppQW_GridSelectedCell.Color;
    m_objPreferences.Color[krsPREF_QUERYGRIDSELECTEDLINECOLOR]             := ppQW_GridSelectedLine.Color;
    m_objPreferences.Color[krsPREF_QUERYGRIDSELECTEDCOLUMNCOLOR]           := ppQW_GridSelectedColumn.Color;
    //
    // Session Manager
    m_objPreferences.StringVal[krsPREF_SESSIONENABLEDTIMER]                := kasBOOL[btnEnableTimer.Checked];
    m_objPreferences.StringVal[krsPREF_SESSIONTIMEINTERVAL]                := edtTimeInterval.Text;
    m_objPreferences.Font[krsPREF_SESSIONGRIDFONT]                         := ppSM_GridFont.Font;
    m_objPreferences.Color[krsPREF_SESSIONBACKGROUNDCOLOR]                 := ppSM_BackgroundColor.Color;
    m_objPreferences.Color[krsPREF_SESSIONGRIDBACKGROUNDCOLOR]             := ppSM_GridBackgroundColor.Color;
    m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTIONCOLOR]              := ppSM_GridSelectedCell.Color;
    m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTEDLINECOLOR]           := ppSM_GridSelectedLine.Color;
    m_objPreferences.Color[krsPREF_SESSIONGRIDSELECTEDCOLUMNCOLOR]         := ppSM_GridSelectedColumn.Color;
    m_objPreferences.Color[krsPREF_SESSIONGRIDNULLCELLCOLOR]               := ppSM_NullCellColor.Color;
  end;
  ModalResult := mrOK;
end;

// TfrmPreferences
//   lstElementsDrawItem
//
procedure TfrmPreferences.lblBrowseSeabirdClick(Sender: TObject);
begin
  ShellOpenFile('http://www.SeabirdSoftware.com/');
end;

// TfrmPreferences
//   btnLicenseClick
//
procedure TfrmPreferences.btnLicenseClick(Sender: TObject);
var
  frm: TfrmWebService;
begin
  frm := nil;
  try
    frm := TfrmWebService.Create(self);
    frm.Preferences := m_objPreferences;
    frm.ShowModal;
  finally
    frm.release;
  end;
  lblLicenseText.Caption := m_objPreferences.LicenseStatus;
end;

// TfrmPreferences
//   edtFieldDisplayWidthKeyPress
//
procedure TfrmPreferences.edtFieldDisplayWidthKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0' .. '9', #8]) then
    Key := #0;
end;

// TfrmPreferences
//   btnudFieldDisplayWidthClick
//
procedure TfrmPreferences.btnudFieldDisplayWidthClick(Sender: TObject; Button: TUDBtnType);
var
  L: longint;
begin
  L := strtointdef(edtFieldDisplayWidth.Text, kriSQLMAXCOLUMNSIZE);
  case Button of
    btNext:
      inc(L);
    btPrev:
      dec(L);
  end;
  edtFieldDisplayWidth.Text := inttostr(L);
  edtFieldDisplayWidthExit(Sender);
end;

// TfrmPreferences
//   edtFieldDisplayWidthExit
//
procedure TfrmPreferences.edtFieldDisplayWidthExit(Sender: TObject);
var
  L: longint;
begin
  L := strtointdef(edtFieldDisplayWidth.Text, kriSQLMAXCOLUMNSIZE);
  if L > kriSQLMAXCOLUMNSIZE then
    L := kriSQLMAXCOLUMNSIZE
  else if L < kriSQLMINCOLUMNSIZE then
    L := kriSQLMINCOLUMNSIZE;
  edtFieldDisplayWidth.Text := inttostr(L);
end;

// TfrmPreferences
//   btnEnableHilightingClick
//
procedure TfrmPreferences.btnEnableHilightingClick(Sender: TObject);
var
  b: boolean;
begin
  b := btnEnableHilighting.Checked;
  ppSH_Keyword.Enabled := b;
  ppSH_Datatype.Enabled := b;
  ppSH_Function.Enabled := b;
  ppSH_String.Enabled := b;
  ppSH_Comment.Enabled := b;
  ppSH_Package.Enabled := b;
  ppSH_Exception.Enabled := b;
  ppSH_Number.Enabled := b;
  ppSH_AnchorBackground.Enabled := b;
  ppSH_Anchor.Enabled := b;
end;

// TfrmPreferences
//   btnEnableTimerClick
//
procedure TfrmPreferences.btnEnableTimerClick(Sender: TObject);
begin
  lblTimeInterval.Enabled := btnEnableTimer.Checked;
  edtTimeInterval.Enabled := btnEnableTimer.Checked;
  udTimeInterval.Enabled := btnEnableTimer.Checked;
end;

// TfrmPreferences
//   tvOptionsChange
//
procedure TfrmPreferences.tvOptionsChange(Sender: TObject; Node: TTreeNode);
var
  i: longint;
begin
  for i := 0 to PageControl.PageCount - 1 do
    if PageControl.Pages[i].Tag = Node.ImageIndex then
    begin
      PageControl.ActivePageIndex := i;
      break;
    end;
end;

// TfrmPreferences
//   SetPage
//
procedure TfrmPreferences.SetPage(value: TeForm);
begin
  case value of
    efsObjects:
      PageControl.ActivePage := tbsR1;
    efsQuery:
      PageControl.ActivePage := tbsC1;
    efsGrid:
      PageControl.ActivePage := tbsQB1;
    efsSessionManager:
      PageControl.ActivePage := tbsSM1;
    efsStoredQuery:
      PageControl.ActivePage := tbsR1;
    efsLogParser:
      PageControl.ActivePage := tbsR1;
  end;
end;

// TfrmPreferences
//   ppSC_IntellisenseDismissClick
//
procedure TfrmPreferences.ppSC_IntellisenseDismissClick(Sender: TObject);
begin
  ppSC_IntellisenseDismissAmountLabel.Enabled := ppSC_IntellisenseDismiss.Checked;
  ppSC_IntellisenseDismissAmount.Enabled := ppSC_IntellisenseDismiss.Checked;;
  ppSC_IntellisenseDismissAmountUpDown.Enabled := ppSC_IntellisenseDismiss.Checked;
end;

// TfrmPreferences
//   ppSC_IntellisenseDismissAmountUpDownClick
//
procedure TfrmPreferences.ppSC_IntellisenseDismissAmountUpDownClick(Sender: TObject; Button: TUDBtnType);
var
  L: longint;
begin
  L := strtointdef(ppSC_IntellisenseDismissAmount.Text, kiINTELLLISENSECONTROL_TIMER);
  case Button of
    btNext:
      inc(L);
    btPrev:
      dec(L);
  end;
  ppSC_IntellisenseDismissAmount.Text := inttostr(L);
  edtFieldDisplayWidthExit(Sender);
end;

end.
