unit frmConnectionWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConnectionLib,
  ComCtrls, ExtCtrls, PreferenceLib, daObjectLib, DataLib, ImgList,
  ConnectionBuilderLib;

type
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TfrmConnectionWizard is the data-driven dynamic data connection
  *              wizard.
  *
  * Inheritance:
  *   TFrom
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 07/10/00 Regis Created
  *
  ******************************************************************************}
  TfrmConnectionWizard = class(TForm)
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnNext: TButton;
    btnHelp: TButton;
    btnPrevious: TButton;
    Bevel1: TBevel;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;

    PageControl: TPageControl;
    tsName: TTabSheet;
    lblConnectionName: TLabel;
    lblDatabaseType: TLabel;
    lblConnectionNameDescription: TLabel;
    lblDatabaseTypeDescription: TLabel;
    lblTool: TLabel;
    lblToolDescription: TLabel;
    edtName: TEdit;
    cboDatabaseType: TComboBox;
    cboDefaultForm: TComboBoxEx;
    tsParameters: TTabSheet;
    tsManual: TTabSheet;
    lblConnectionName2: TLabel;
    edtName2: TEdit;
    lblDatabaseType2: TLabel;
    cboDatabaseType2: TComboBox;
    cboDefaultForm2: TComboBoxEx;
    lblTool2: TLabel;
    btnConnectionString2: TButton;
    btnLoginPrompt2: TCheckBox;
    btnTestConnection2: TButton;
    lblLoginPrompt2: TLabel;
    lblConnectionString2: TLabel;
    mmoConnectionString2: TMemo;
    Label1: TLabel;
    lblProvidersDescription: TLabel;
    lblProviders: TLabel;
    cboProviders: TComboBox;
    ImageList: TImageList;
    lblUseOneConnectionDescription: TLabel;
    cbxUseOneConnection: TCheckBox;
    lblUseOneConnection: TLabel;
    pcStep2: TPageControl;
    lblProviderDescription: TLabel;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure tsParametersShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnFieldExit(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure OnTestConnection(Sender: TObject);
    procedure btnConnectionString2Click(Sender: TObject);
    procedure OnTestFTP(Sender: TObject);

  private
    // Private Members
    //
    m_bNewXML: boolean;
    m_objMeta: TcMetaData;
    m_objLogon: TcObject;
    m_objPreference: TcPreference;
    m_objConnection: TcConnection;
    m_objOriginalPreference: TcPreference;
    m_objBuilder: TcConnectionBuilder;
    m_bUseWizard: boolean;
    m_iInitialHeight: longint;

  private
    // Private declarations
    //
    procedure   SetXML(value: String);
    function    GetXML: String;
    function    IsFull(parPage: longint): boolean;
    procedure   SetPreference(value: TcPreference);
    procedure   SetHeader(Index: longint);

  public
    // Public Properties
    //
    property    Preference: TcPreference                                        write SetPreference;
    property    Connection: TcConnection             read m_objConnection       write m_objConnection;
    property    UseWizard: boolean                   read m_bUseWizard          write m_bUseWizard;
    property    InitialHeight: longint               read m_iInitialHeight      write m_iInitialHeight;
  end;

implementation

uses
  strUtils,
  daGlobals,
  ComObj,
  Math,
  Main,
  FormLib,
  daResourceStrings,
  Buttons,
  Variants,
  ADODB_TLB,
  FTPLib;

const
  kiLABELLEFT           = 32;
  kiLABELWIDTH          = 136;
  kiEDITLEFT            = 174;
  kiEDITWIDTH           = 160;
  kiSPACING             = 12;
  kiTOPSTART            = 24;
  kiPAGECONTROLBORDER   = 4;

  kasNEXTCAPTION: array[0 .. 2] of String =
    ('&Next', '&Finish', '&OK');

  kasPAGECAPTION: array[0 .. 2] of String =
    ('Select a connection name, a database type and a default form.',
     'Select database-specific connection profile properties.',
     'Enter connection name, database type, default form, login prompt and connection string profile parameters.');

{$R *.DFM}

//
// TfrmConnectionWizard
//


// TfrmConnectionWizard
//   FormCreate
//
procedure TfrmConnectionWizard.FormCreate(Sender: TObject);
begin
  m_iInitialHeight := Height;
  m_objMeta := TcMetaData.Create(nil);
  m_objLogon := nil;
  m_objConnection := nil;
  m_objPreference := TcPreference.Create(nil);
  m_objOriginalPreference := nil;
  m_objBuilder := TcConnectionBuilder.Create(nil);
  m_objBuilder.ImageList := ImageList;
  m_objBuilder.OnFieldExit := OnFieldExit;
  m_objBuilder.frmCanvas := self;
  m_objBuilder.SetConstants(kiLABELLEFT, kiLABELWIDTH, kiEDITLEFT, kiEDITWIDTH, kiSPACING, kiTOPSTART, kiPAGECONTROLBORDER);
  m_objBuilder.hdlTestConnection := OnTestConnection;
  m_objBuilder.hdlTestFTP := OnTestFTP;
  m_bNewXML := TRUE;
end;

// TfrmConnectionWizard
//   FormDestroy
//
procedure TfrmConnectionWizard.FormDestroy(Sender: TObject);
begin
  m_objMeta.free;
  m_objPreference.Free;
  m_objBuilder.free;
end;

// TfrmConnectionWizard
//   FormShow
//
procedure TfrmConnectionWizard.FormShow(Sender: TObject);
var
  i, Y, L: longint;
begin
  m_objBuilder.UseWizard := m_bUseWizard;
  if not FileExists(GetXML) then
    SetXML(ksEMPTY);
  if (Connection <> nil) and (Connection.lstXML <> nil) and (cboDatabaseType.Items.Count = 0) then
    for i := 0 to Connection.lstXML.Count - 1 do
    begin
      cboDatabaseType.Items.AddObject(Connection.lstXML[i].sValue, pointer(Connection.lstXML[i]));
      cboDatabaseType2.Items.AddObject(Connection.lstXML[i].sValue, pointer(Connection.lstXML[i]));
      if AnsiCompareText(Connection.lstXML[i].sName, GetXML) = 0 then
      begin
        cboDatabaseType.ItemIndex := cboDatabaseType.Items.Count - 1;
        cboDatabaseType2.ItemIndex := cboDatabaseType2.Items.Count - 1;
      end;
    end;

  PageControl.Style := tsButtons;
  tsName.TabVisible := FALSE;
  tsParameters.TabVisible := FALSE;
  tsManual.TabVisible := FALSE;
  if m_bUseWizard then
  begin
    SetHeader(0);
    PageControl.ActivePageIndex := 0;

    y := m_objBuilder.cstTOPSTART;
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, Y, 0, tsName, TObject(lblConnectionNameDescription), nil, ksEMPTY);
    L := Y;
    Y := m_objBuilder.SetObject(ewoEdit, ksEMPTY, Y, 0, tsName, TObject(edtName), nil, m_objPreference.sName);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, edtName.Height, tsName, TObject(lblConnectionName), nil, ksEMPTY);

    inc(Y, m_objBuilder.cstSPACING);
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, Y, 0, tsName, TObject(lblDatabaseTypeDescription), nil, ksEMPTY);
    L := Y;
    Y := m_objBuilder.SetObject(ewoCombo, ksEMPTY, Y, 0, tsName, TObject(cboDatabaseType), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, cboDatabaseType.Height, tsName, TObject(lblDatabaseType), nil, ksEMPTY);

    inc(Y, m_objBuilder.cstSPACING);
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, Y, 0, tsName, TObject(lblToolDescription), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCombo, ksEMPTY, Y, 0, tsName, TObject(cboDefaultForm), nil, ksEMPTY);
    Y := m_objBuilder.SetObject(ewoCaption, ksEMPTY, Y, cboDefaultForm.Height, tsName, TObject(lblTool), nil, ksEMPTY);
    SetFormType(cboDefaultForm, nil, m_objPreference.eForm, nil);

    inc(Y, m_objBuilder.cstSPACING);
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, Y, 0, tsName, TObject(lblUseOneConnectionDescription), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCheckbox, ksEMPTY, Y, 0, tsName, TObject(cbxUseOneConnection), nil, kasBOOL[m_objPreference.bOneConnection]);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, Y, cbxUseOneConnection.Height + 2, tsName, TObject(lblUseOneConnection), nil, ksEMPTY);

    edtName.SetFocus;
    SetState(Sender);
  end
  else
  begin
    btnPrevious.Visible := FALSE;
    SetHeader(2);
    PageControl.ActivePageIndex := 2;
    y := m_objBuilder.cstTOPSTART;
    L := Y;
    Y := m_objBuilder.SetObject(ewoEdit, ksEMPTY, Y, 0, tsManual, TObject(edtName2), nil, m_objPreference.sName);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, edtName2.Height, tsManual, TObject(lblConnectionName2), nil, ksEMPTY);
    inc(Y, m_objBuilder.cstSPACING);
    L := Y;
    Y := m_objBuilder.SetObject(ewoCombo, ksEMPTY, Y, 0, tsManual, TObject(cboDatabaseType2), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, cboDatabaseType2.Height, tsManual, TObject(lblDatabaseType2), nil, ksEMPTY);
    inc(Y, m_objBuilder.cstSPACING);
    L := Y;
    Y := m_objBuilder.SetObject(ewoCombo, ksEMPTY, Y, 0, tsManual, TObject(cboDefaultForm2), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, cboDefaultForm2.Height, tsManual, TObject(lblTool2), nil, ksEMPTY);
    inc(Y, m_objBuilder.cstSPACING);

    L := Y;
    Y := m_objBuilder.SetObject(ewoCheckBox, ksEMPTY, Y, 0, tsManual, TObject(btnLoginPrompt2), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, btnLoginPrompt2.Height, tsManual, TObject(lblLoginPrompt2), nil, ksEMPTY);
    inc(Y, m_objBuilder.cstSPACING);

    L := Y;
    Y := m_objBuilder.SetObject(ewoButton, ksEMPTY, Y, 0, tsManual, TObject(btnConnectionString2), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, btnConnectionString2.Height, tsManual, TObject(lblConnectionString2), nil, ksEMPTY);
    inc(Y, m_objBuilder.cstSPACING div 2);
    m_objBuilder.SetObject(ewoMemo, ksEMPTY, Y, 0, tsManual, TObject(mmoConnectionString2), nil, ksEMPTY);

    mmoConnectionString2.Text := m_objPreference.sADO;
    btnLoginPrompt2.Checked := m_objPreference.bLoginPrompt;

    SetFormType(cboDefaultForm2, nil, m_objPreference.eForm, nil);
    edtName2.SetFocus;
    SetState(Sender);
  end;
  OnFieldExit(cboProviders);
end;

// TfrmConnectionWizard
//   FormClose
//
procedure TfrmConnectionWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmConnectionWizard
//   SetState
//
procedure TfrmConnectionWizard.SetState(Sender: TObject);
begin
  btnPrevious.Enabled := PageControl.ActivePageIndex > 0;
  btnNext.Caption := kasNEXTCAPTION[PageControl.ActivePageIndex];
  btnNext.Enabled := IsFull(PageControl.ActivePageIndex);
  // btnTestConnection.Enabled := IsFull(1);
  btnTestConnection2.Enabled := IsFull(2);
end;

// TfrmConnectionWizard
//   btnNextClick
//
procedure TfrmConnectionWizard.btnNextClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0:
      begin
        if btnNext.Enabled and (PageControl.ActivePageIndex < PageControl.PageCount - 1) then
        begin
          tsParametersShow(Sender);
          PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
        end;
        SetHeader(1);
        SetState(Sender);
      end;
    1, 2:
      begin
        m_objOriginalPreference.Copy(m_objPreference);
        ModalResult := mrOK;
      end;
  end;
  PageControl.SetFocus;
end;

// TfrmConnectionWizard
//   btnPreviousClick
//
procedure TfrmConnectionWizard.btnPreviousClick(Sender: TObject);
begin
  if btnPrevious.Enabled and (PageControl.ActivePageIndex > 0) then
    PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
  SetHeader(0);
  SetState(Sender);
  PageControl.SetFocus;
end;

// TfrmConnectionWizard
//   OnFieldExit
//
procedure TfrmConnectionWizard.OnFieldExit(Sender: TObject);
var
  p: TcObject;
begin
  if (Sender = edtName) then
    m_objPreference.sName := edtName.Text
  else if (Sender = cboDefaultForm) then
    with cboDefaultForm do
      m_objPreference.eForm := TeForm(ItemsEx[ItemIndex].Data)
  else if (Sender = cbxUseOneConnection) then
    m_objPreference.bOneConnection := cbxUseOneConnection.Checked
  else if (Sender = edtName2) then
    m_objPreference.sName := edtName2.Text
  else if (Sender = cboDefaultForm2) then
    with cboDefaultForm2 do
      m_objPreference.eForm := TeForm(ItemsEx[ItemIndex].Data)
  else if (Sender = btnLoginPrompt2) then
    m_objPreference.bLoginPrompt := btnLoginPrompt2.Checked
  else if ((Sender = cboDatabaseType) or (Sender = cboDatabaseType2)) and ((Sender as TComboBox).ItemIndex <> kiUNDEFINED) then
  begin
    p := TcObject((Sender as TComboBox).Items.Objects[(Sender as TComboBox).ItemIndex]);
    if p <> nil then
    begin
      SetXML(p.sName);
      m_objPreference.sDescription := p.sValue;
    end;
  end
  else if (Sender = cboProviders) then
  begin
   with cboProviders do
    if ItemIndex <> kiUNDEFINED then
    begin
      p := TcObject(Items.Objects[ItemIndex]);
      if (p <> nil) and (p is TcMetaData) then
      begin
        m_objPreference.eMode := StringToConnectionMode((p as TcMetaData).Attribute[krsXML_MODE]);
        m_objPreference.sProvider := p.sName;
        m_objPreference.sComment := p.FindFirstValue(enComment);
        lblProviderDescription.Caption := AnsiReplaceText(m_objPreference.sComment, '\n', ksCR);
        lblProviderDescription.Font.Color := clRed;
      end;
    end;
  end;
  SetState(Sender);
end;

// TfrmConnectionWizard
//   tsParametersShow
//
procedure TfrmConnectionWizard.tsParametersShow(Sender: TObject);
var
  i, L, Y: longint;
  p: TcMetaData;
begin
  if m_bNewXML and (m_objPreference.sMetaXML <> ksEMPTY) then
  try
    screen.Cursor := crHourGlass;
    // Clean up form first.
    Height := m_iInitialHeight;
    m_objBuilder.Clear;
    for i := pcStep2.PageCount - 1 downto 0 do
      pcStep2.Pages[i].free;
    pcStep2.Height := 20;
    // Build form
    m_objMeta.Clear;
    m_objMeta.XML := FileToString(m_objPreference.sMetaXML, ecsAnsi);
    p := m_objMeta.Find(enFEATURE, krsLOGON) as TcMetaData;
    m_objBuilder.Root := p;
    // Provider
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, m_objBuilder.cstTOPSTART, 0, tsParameters, TObject(lblProvidersDescription), nil, ksEMPTY);
    L := Y;
    Y := m_objBuilder.SetObject(ewoCombo, ksEMPTY, Y, 0, tsParameters, TObject(cboProviders), nil, ksEMPTY);
    Y := m_objBuilder.SetObject(ewoDescription, ksEMPTY, Y + 4, 0, tsParameters, TObject(lblProviderDescription), nil, ksEMPTY);
    m_objBuilder.SetObject(ewoCaption, ksEMPTY, L, cboProviders.Height, tsParameters, TObject(lblProviders), nil, ksEMPTY);
    m_objBuilder.SetProviderList(cboProviders, p);
    inc(Y, m_objBuilder.cstSPACING);
    // Page Control
    m_objBuilder.SetObject(ewoPageControl, ksEMPTY, Y, 0, tsParameters, pcStep2, nil, ksEMPTY);
    // Build other components
    m_objBuilder.Build(ibtSetup, pcStep2, self, Y);
    Top := max((Screen.Height - Height) div 2, 0);
    m_bNewXML := FALSE;
    //
    OnFieldExit(cboProviders);
  finally
    screen.Cursor := crDefault;
  end;
end;

// TfrmConnectionWizard
//   SetXML
//
procedure TfrmConnectionWizard.SetXML(value: String);
begin
  m_bNewXML := m_bNewXML or (value = ksEMPTY) or (m_objPreference.sMetaXML <> value);
  m_objPreference.sMetaXML := value;
end;

// TfrmConnectionWizard
//   GetXML
//
function TfrmConnectionWizard.GetXML: String;
begin
  result := m_objPreference.sMetaXML;
end;

// TfrmConnectionWizard
//   IsFull
//
function TfrmConnectionWizard.IsFull(parPage: longint): boolean;
begin
  // Page = 0
  result := (m_objPreference.sName <> ksEMPTY) and
            (m_objPreference.sMetaXML <> ksEMPTY) and
            ((m_bUseWizard and (cboDefaultForm.ItemIndex <> kiUNDEFINED)) or (not m_bUseWizard and (cboDefaultForm2.ItemIndex <> kiUNDEFINED)));
  // Page = 1
  if result and (PageControl.ActivePageIndex = 1) then
    result := result and m_objBuilder.IsFull;
  // Page = 2
  if result and (PageControl.ActivePageIndex = 2) then
    result := result and (m_objPreference.sADO <> ksEMPTY);
end;

// TfrmConnectionWizard
//   SetPreference
//
procedure TfrmConnectionWizard.SetPreference(value: TcPreference);
begin
  m_objOriginalPreference := value;
  m_objPreference.Copy(value);
  m_objBuilder.Preference := m_objPreference;
end;

// TfrmConnectionWizard
//   SetHeader
//
procedure TfrmConnectionWizard.SetHeader(Index: longint);
const
  kiTranslate: array[0 .. 2] of longint =
    (1, 2, 0);
begin
  if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
    (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, kiTranslate[Index]);
  lblHeader.Caption := kasPAGECAPTION[Index];
  imgHeader.Refresh;
end;

// TfrmConnectionWizard
//   OnTestConnection
//
procedure TfrmConnectionWizard.OnTestConnection(Sender: TObject);
var
  p: TcConnection;
begin
  p := nil;
  try
    screen.cursor := crHourGlass;
    p := TcConnection.Create(nil);
    p.Copy(m_objPreference);
    try
      if p.Open then
      begin
        Application.MessageBox(PChar(Format('Connection Succeeded.%s[%s]', [ksCR + ksCR, m_objPreference.Text])), krsINFORMATION, MB_OK + MB_ICONINFORMATION);
        p.Close;
      end
      else
        Application.MessageBox(PChar(Format('Connection Failed.%s[%s]', [ksCR + ksCR, m_objPreference.Text])), krsERROR, MB_OK + MB_ICONSTOP);
    except
      on E: Exception do
        Application.MessageBox(PChar(Format('Connection Failed: %s%s[%s]', [E.Message, ksCR + ksCR, m_objPreference.Text])), krsERROR, MB_OK + MB_ICONSTOP);
    end;
  finally
    p.free;
    screen.cursor := crDefault;
  end;
end;

// TfrmConnectionWizard
//   OnTestFTP
//
procedure TfrmConnectionWizard.OnTestFTP(Sender: TObject);
begin
  try
    screen.cursor := crHourGlass;
    try
      if TestFTPLink(m_objPreference.Attribute[krsXML_LOGHOST], m_objPreference.Attribute[krsXML_LOGUSERNAME], m_objPreference.Attribute[krsXML_LOGPASSWORD], m_objPreference.Attribute[krsXML_LOGSECUREFTP] = krsTRUE) then
        Application.MessageBox(PChar(Format('FTP connection to host ''%s'', user name ''%s'', succeeded.', [m_objPreference.Attribute[krsXML_LOGHOST], m_objPreference.Attribute[krsXML_LOGUSERNAME]])), krsINFORMATION, MB_OK + MB_ICONINFORMATION)
      else
        Application.MessageBox(PChar(Format('FTP connection to host ''%s'', user name ''%s'', failed.', [m_objPreference.Attribute[krsXML_LOGHOST], m_objPreference.Attribute[krsXML_LOGUSERNAME]])), krsERROR, MB_OK + MB_ICONSTOP);
    except
      on E: Exception do
        Application.MessageBox(PChar(Format('FTP connection to host ''%s'', user name ''%s'', failed: %s', [m_objPreference.Attribute[krsXML_LOGHOST], m_objPreference.Attribute[krsXML_LOGUSERNAME], E.Message])), krsERROR, MB_OK + MB_ICONSTOP);
    end;
  finally
    screen.cursor := crDefault;
  end;
end;

// TfrmConnection
//   btnConnectionString2Click
//
procedure TfrmConnectionWizard.btnConnectionString2Click(Sender: TObject);
var
  s: String;
begin
  s := PromptDataSource(Handle, m_objPreference.sADO);
  if s <> ksEMPTY then
  begin
    m_objPreference.sADO := s;
    mmoConnectionString2.Text := m_objPreference.sADO;
  end;
  SetState(Sender);
end;

end.



