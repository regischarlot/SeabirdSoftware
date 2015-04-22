unit frmConnectionLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConnectionLib, ComCtrls, PreferenceLib, ExtCtrls,
  ConnectionBuilderLib, ImgList;

type
  TfrmConnectionLogin = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    lblForm: TLabel;
    cboForms: TComboBoxEx;
    bvlSeparation: TBevel;
    ImageList: TImageList;
    btnUpdateParameters: TCheckBox;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure OnGetConnectionString(Sender: TObject);

  private
    // Private members
    //
    m_objPreference: TcPreference;
    m_objBuilder: TcConnectionBuilder;
    m_objConnection: TcConnection;
    m_iInitialHeight: longint;

  private
    // Private declarations
    //
    procedure   SetPreference(value: TcPreference);

  public
    // Public Properties
    //
    property    Connection: TcConnection             read m_objConnection       write m_objConnection;
    property    Preference: TcPreference             read m_objPreference       write SetPreference;
    property    InitialHeight: longint               read m_iInitialHeight      write m_iInitialHeight;
  end;

implementation

uses
  daGlobals,
  DataLib,
  daResourceStrings,
  FormLib,
  daObjectLib,
  ClipBrd;

const
  kiLABELLEFT           = 12;
  kiLABELWIDTH          = 90;
  kiEDITLEFT            = 110;
  kiEDITWIDTH           = 175;
  kiSPACING             = 4;
  kiTOPSTART            = 8;
  kiPAGECONTROLBORDER   = 4;

{$R *.dfm}

// TfrmConnectionLogin
//   FormCreate
//
procedure TfrmConnectionLogin.FormCreate(Sender: TObject);
begin
  m_iInitialHeight := Height;
  m_objConnection := nil;
  m_objPreference := nil;
  m_objBuilder := TcConnectionBuilder.Create(nil);
  m_objBuilder.OnFieldExit := SetState;
  m_objBuilder.frmCanvas := self;
  m_objBuilder.SetConstants(kiLABELLEFT, kiLABELWIDTH, kiEDITLEFT, kiEDITWIDTH, kiSPACING, kiTOPSTART, kiPAGECONTROLBORDER);
  m_objBuilder.ImageList := ImageList;
  m_objBuilder.hdlGetConnectionString := OnGetConnectionString;
end;

// TfrmConnectionLogin
//   FormDestroy
//
procedure TfrmConnectionLogin.FormDestroy(Sender: TObject);
begin
  m_objBuilder.free;
end;

// TfrmConnectionLogin
//   FormShow
//
procedure TfrmConnectionLogin.FormShow(Sender: TObject);
var
  L, M: longint;
  p: TcMetaData;
begin
  try
    screen.Cursor := crHourGlass;
    if (m_objConnection <> nil) and (m_objConnection.FormSet <> nil) and ((m_objConnection.FormSet as TcFormSet).MetaData <> nil) then
    begin
      p := (m_objConnection.FormSet as TcFormSet).MetaData.Find(enFEATURE, krsLOGON) as TcMetaData;
      m_objBuilder.Root := p;
      L := m_objBuilder.Build(ibtLogon, self, self, kiUNDEFINED);
      if not m_objBuilder.IsEmpty then
      begin
        L := m_objBuilder.SetObject(ewoBevel, ksEMPTY, L + kiSECTIONINTERVAL, 0, self, TObject(bvlSeparation), nil, ksEMPTY);
        inc(L, m_objBuilder.cstSPACING);
      end
      else
        bvlSeparation.Visible := FALSE;
      // Forms Combo Box
      M := L;
      L := m_objBuilder.SetObject(ewoCombo, ksEMPTY, L, 0, self, TObject(cboForms), nil, ksEMPTY);
      m_objBuilder.SetObject(ewoCaption, ksEMPTY, M, cboForms.Height, self, TObject(lblForm), nil, ksEMPTY);
      inc(L, m_objBuilder.cstSPACING);
      //
      m_objBuilder.SetObject(ewoCheckBox, ksEMPTY, btnUpdateParameters.Top, 0, pnlBottom, TObject(btnUpdateParameters), nil, ksEMPTY);
      btnUpdateParameters.Left := 8;
      btnUpdateParameters.Width := 70;
      // Final Form Height
      Height := Height - (pnlBottom.Top - L ) + m_objBuilder.cstSPACING;
    end;
  finally
    screen.Cursor := crDefault;
  end;
  SetFormType(cboForms, nil, Connection.eForm, nil);
  Caption := Format(' Login to ''%s''', [Connection.sName]);
end;

// TfrmConnectionLogin
//   FormClose
//
procedure TfrmConnectionLogin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmConnectionLogin
//   btnOKClick
//
procedure TfrmConnectionLogin.btnOKClick(Sender: TObject);
begin
  Connection.Copy(m_objPreference);
  Connection.eForm := TeForm(cboForms.ItemsEx[cboForms.ItemIndex].Data);
  ModalResult := mrOK;
end;

// TfrmConnectionLogin
//   SetPreference
//
procedure TfrmConnectionLogin.SetPreference(value: TcPreference);
begin
  m_objPreference := value;
  m_objBuilder.Preference := value;
end;

// TfrmConnectionLogin
//   SetState
//
procedure TfrmConnectionLogin.SetState(Sender: TObject);
begin
  btnOK.Enabled := m_objBuilder.IsFull;
end;

// TfrmConnectionLogin
//   OnGetConnectionString
//
procedure TfrmConnectionLogin.OnGetConnectionString(Sender: TObject);
begin
  if Application.MessageBox(PChar(Format('Connection string is:%s%s%sSend Connection String to Clipboard?', [ksCR, m_objConnection.ConnectionString, ksCR + ksCR])), krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYes then
  begin
    Clipboard.AsText := m_objConnection.ConnectionString;
    Application.MessageBox('Connection string sent to clipboard.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
  end;
end;

end.

