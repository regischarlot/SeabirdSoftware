unit frmWebService;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ImgList, PreferenceLib;

type
  TeRegisterMode = (ermNew, ermExisting);

  TfrmWebService = class(TForm)
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnNext: TButton;
    btnHelp: TButton;
    btnPrevious: TButton;
    Panel2: TPanel;
    imgHeader: TImage;
    lblHeader: tstatictext;
    Label1: tstatictext;
    PageControl: TPageControl;
    tsPage1: TTabSheet;
    lblPage1_EmailAddress: TStaticText;
    edtPage1_EmailAddress: TEdit;
    tsPage2: TTabSheet;
    lblPage1_Password: TStaticText;
    edtPage1_Password: TEdit;
    lblPage1_Note2: TStaticText;
    lblPage1_Note3: TStaticText;
    btnExistingUserInformation: TRadioButton;
    btnNewRegistration: TRadioButton;
    lblPage2_FirstName: TStaticText;
    edtPage2_FirstName: TEdit;
    lblPage2_LastName: TStaticText;
    edtPage2_LastName: TEdit;
    lblPage2_EmailAddress: TStaticText;
    edtPage2_EmailAddress: TEdit;
    lblPage2_Password1: TStaticText;
    edtPage2_Password1: TEdit;
    tsPage3: TTabSheet;
    lblPage2_Note1: TStaticText;
    lblPage2_Password2: TStaticText;
    edtPage2_Password2: TEdit;
    btnPage2_Newsletter: TCheckBox;
    btnPage2_NewReleases: TCheckBox;
    lblPage1_Note1: TStaticText;
    lblPage2_Note2: TStaticText;
    lblForgotPassword: TStaticText;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblResult: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure OnKeyPress_Field(Sender: TObject; var Key: Char);
    procedure OnKeyPress_Password(Sender: TObject; var Key: Char);
    procedure OnKeyPress_Email(Sender: TObject; var Key: Char);
    procedure OnKeyPress_AZ(Sender: TObject; var Key: Char);
    procedure OnEditChange(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure OnPage1_RadioButtonClick(Sender: TObject);
    procedure OnForgotPassword(Sender: TObject);

  private
    // private methods
    //
    function  OnRegister(eMode: TeRegisterMode; parList: TStringList): boolean;
    function  CF(value: TEdit; sError: String): boolean;
    function  CPWDLEN(value: TEdit): boolean;

  private
    // private methods
    //
    m_objPreferences: TcPreferenceList;

  public
    // Public declarations
    //
    property Preferences: TcPreferenceList read m_objPreferences write m_objPreferences;
  end;

implementation

{$R *.dfm}

uses
  comObj,
  strUtils,
  daResourceStrings,
  daGlobals,
  WebServiceLib;

const
  kiPage0 = 0;
  kiPage1 = 1;
  kiPage2 = 2;

// TfrmWebService
//   FormShow
//
procedure TfrmWebService.FormShow(Sender: TObject);
begin
  btnExistingUserInformation.Checked := TRUE;
  if m_objPreferences <> nil then
    edtPage1_EmailAddress.text := m_objPreferences.StringVal[krsUSER_EMAIL];
  OnPage1_RadioButtonClick(nil);
  PageControl.Style := tsButtons;
  tsPage1.TabVisible := FALSE;
  tsPage2.TabVisible := FALSE;
  tsPage3.TabVisible := FALSE;
  PageControl.ActivePage := tsPage1;
  OnEditChange(edtPage1_EmailAddress);
  OnEditChange(edtPage1_Password);
  OnEditChange(edtPage2_FirstName);
  OnEditChange(edtPage2_LastName);
  OnEditChange(edtPage2_EmailAddress);
  OnEditChange(edtPage2_Password1);
  OnEditChange(edtPage2_Password2);
  SetState(nil);
end;

// TfrmWebService
//   OnPage1_RadioButtonClick
//
procedure TfrmWebService.OnPage1_RadioButtonClick(Sender: TObject);
var
  b: boolean;
const
  col: array[boolean] of TColor = (clBtnFace, clWindow);
begin
  b := btnExistingUserInformation.Checked;
  lblPage1_EmailAddress.Enabled := b;
  edtPage1_EmailAddress.Enabled := b;
  lblPage1_Password.Enabled := b;
  edtPage1_Password.Enabled := b;
  lblForgotPassword.Enabled := b;
  OnEditChange(edtPage1_EmailAddress);
  OnEditChange(edtPage1_Password);
  SetState(Sender);
end;

// TfrmWebService
//   SetState
//
procedure TfrmWebService.SetState(Sender: TObject);
const
  kasBTNNEXT_NEW: array[0 .. 2] of String =
    ('&Next', '&Register', '&Close');
  kasBTNNEXT_EXISTING: array[0 .. 2] of String =
    ('&Register', '&Register', '&Close');
  kasBTNCANCEL: array[0 .. 2] of String =
    ('&Cancel', '&Cancel', '&Cancel');
  kasNEXTENABLED: array[0 .. 2] of boolean =
    (TRUE, TRUE, TRUE);
  kasPREVIOUSENABLED: array[0 .. 2] of boolean =
    (FALSE, TRUE, TRUE);
  krsPAGECAPTION: array[0 .. 2] of String = (
    'Provide basic demographic information to either create or retrieve licensing  information.',
    'A short registration is necesary in order to start dbAnalyst. We do not share email information with any third party. This information is used to uniquely identify users. ',
    'Registration Summary');
begin
  if (PageControl.ActivePageIndex in [0 .. 2]) and PageControl.ActivePage.Visible then
  begin
    if btnExistingUserInformation.Checked then
      btnNext.Caption := kasBTNNEXT_EXISTING[PageControl.ActivePageIndex]
    else
      btnNext.Caption := kasBTNNEXT_NEW[PageControl.ActivePageIndex];
    btnNext.Enabled := kasNEXTENABLED[PageControl.ActivePageIndex] and ((PageControl.ActivePageIndex <> kiPAGE2) or ((PageControl.ActivePageIndex = kiPAGE2) and (m_objPreferences.LicenseLevel > ellNone)));
    btnPrevious.Enabled := kasPREVIOUSENABLED[PageControl.ActivePageIndex];
    lblHeader.Caption := krsPAGECAPTION[PageControl.ActivePageIndex];
    btnCancel.Caption := kasBTNCANCEL[PageControl.ActivePageIndex];
    btnCancel.Enabled := m_objPreferences.LicenseLevel > ellNone;
  end;
end;

// TfrmWebService
//   btnNextClick
//
procedure TfrmWebService.btnNextClick(Sender: TObject);

  procedure ProcessWebService(parMode: TeRegisterMode);
  var
    lst: TStringList;
  begin
    btnCancel.ModalResult := mrCancel;

    lst := nil;
    try
      lst := TStringList.Create;
      if OnRegister(parMode, lst) then
      begin
        PageControl.ActivePageIndex := kiPage2;
        m_objPreferences.StringVal[krsLICENSEKEY] := lst.values['LICENSE'];
        m_objPreferences.StringVal[krsUSER_FULLNAME] := trim(lst.values['FIRST_NAME'] + ' ' + lst.values['LAST_NAME']);
        m_objPreferences.StringVal[krsUSER_SINCEDATE] := lst.values['DATE_SINCE'];
        m_objPreferences.StringVal[krsUSER_EMAIL] := lst.values['EMAIL'];
        m_objPreferences.WriteToRegistry;
        lblResult.Caption := AnsiReplaceText(lst.values['MESSAGE'], '\n', ksCR);
        btnCancel.ModalResult := mrOK;
      end;
    finally
      lst.Free;
    end;
  end;

begin
  case PageControl.ActivePageIndex of
    kiPage0:
      begin
        if btnExistingUserInformation.Checked then
        begin
          if CF(edtPage1_EmailAddress, 'Please provide an email address.') and
             CF(edtPage1_Password, 'Please enter your password.') and
             CPWDLEN(edtPage1_Password) then
            ProcessWebService(ermExisting);
        end
        else
        begin
          PageControl.ActivePageIndex := kiPage1;
          edtPage2_FirstName.SetFocus;
        end;
      end;
    kiPage1:
      if CF(edtPage2_FirstName, 'Please provide your first name.') and
         CF(edtPage2_Lastname, 'Please enter your last name.') and
         CF(edtPage2_EmailAddress, 'Please provide your email address.') and
         CF(edtPage2_Password1, 'Please enter your password.') and
         CF(edtPage2_Password2, 'Please re-enter your password.') and
         CPWDLEN(edtPage2_Password1) then
      begin
        if edtPage2_Password1.Text <> edtPage2_Password2.Text then
        begin
          Application.MessageBox('Passwords do not match.', krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
          edtPage2_Password2.SetFocus;
        end
        else
          ProcessWebService(ermNew);
      end;
    kiPage2:
      if m_objPreferences.LicenseLevel > ellNone then
        Close;
  end;
  Application.ProcessMessages;
  SetState(Sender);
end;

// TfrmWebService
//   btnPreviousClick
//
procedure TfrmWebService.btnPreviousClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    kiPage1:
      PageControl.ActivePageIndex := kiPage0;
    kiPage2:
      PageControl.ActivePageIndex := kiPage0;
  end;
  SetState(Sender);
end;

// TfrmWebService
//   OnEditChange
//
procedure TfrmWebService.OnEditChange(Sender: TObject);
begin
  if (Sender <> nil) and (sender is TComponent) and ((sender as TComponent).Tag <> 0) then
  begin
    if (Sender is TEdit) then
    begin
      if not (Sender as TEdit).Enabled then
        (Sender as TEdit).Color := clBtnFace
      else if (Sender as TEdit).Text <> ksEMPTY then
        (Sender as TEdit).Color := clWindow
      else if (Sender as TEdit).Text = ksEMPTY then
        (Sender as TEdit).Color := $90FCFF;
    end;

  end;
end;

// TfrmWebService
//   CF
//
function TfrmWebService.CF(value: TEdit; sError: String): boolean;
begin
  result := value.Text <> ksEMPTY;
  if not result then
  begin
    Application.MessageBox(PChar(sError), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
    value.SetFocus;
  end;
end;

// TfrmWebService
//   CPWDLEN
//
function TfrmWebService.CPWDLEN(value: TEdit): boolean;
const
  kiPWDLEN = 4;
begin
  result := (value.Text <> ksEMPTY) and (length(value.Text) >= kiPWDLEN);
  if not result then
  begin
    Application.MessageBox(PChar(Format('Password length must be greater or equal than %d characters.', [kiPWDLEN])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
    value.SetFocus;
  end;
end;

// TfrmWebService
//   OnForgotPassword
//
procedure TfrmWebService.OnForgotPassword(Sender: TObject);
var
  p: TcWebServiceCall;
begin
  p := nil;
  if CF(edtPage1_EmailAddress, 'Please provide an email address.') then
  try
    screen.Cursor := crHourGlass;
    p := TcWebServiceCall.Create(nil);
    if p.OnForgotPassword(edtPage1_EmailAddress.Text) then
      Application.MessageBox(PChar(p.sMessage), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION)
    else
      Application.MessageBox(PChar(p.sError), krsERROR, MB_OK + MB_ICONERROR);
    edtPage1_EmailAddress.SetFocus;
  finally
    screen.Cursor := crDefault;
    p.free;
  end;
end;

// TfrmWebService
//   OnRegister
//
function TfrmWebService.OnRegister(eMode: TeRegisterMode; parList: TStringList): boolean;
var
  s: String;
  p: TcWebServiceCall;
const
  kabBOOLTONUM: array[boolean] of longint =
    (0, 1);
begin
  p := nil;
  try
    screen.Cursor := crHourGlass;
    p := TcWebServiceCall.Create(nil);
    parList.Clear;
    // Message XML
    case eMode of
      ermNew:
        s := Format('<%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%d</%s><%s>%d</%s></%s>',
                    [krsXML_TRANSACTION,
                       krsXML_MODE,        'NEW',                                       krsXML_MODE,
                       krsXML_LASTNAME,    uppercase(trim(edtPage2_LastName.Text)),     krsXML_LASTNAME,
                       krsXML_FIRSTNAME,   uppercase(trim(edtPage2_FirstName.Text)),    krsXML_FIRSTNAME,
                       krsXML_EMAIL,       uppercase(trim(edtPage2_EmailAddress.Text)), krsXML_EMAIL,
                       krsXML_PASSWORD,    uppercase(trim(edtPage2_Password1.Text)),    krsXML_PASSWORD,
                       krsXML_MAC,         GetMACID,                                    krsXML_MAC,
                       krsXML_CPU,         GetCPUID,                                    krsXML_CPU,
                       krsXML_NEWSLETTER,  kabBOOLTONUM[btnPage2_Newsletter.Checked],   krsXML_NEWSLETTER,
                       krsXML_NEWRELEASES, kabBOOLTONUM[btnPage2_NewReleases.Checked],  krsXML_NEWRELEASES,
                     krsXML_TRANSACTION]);
      ermExisting:
        s := Format('<%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s><%s>%s</%s></%s>',
                    [krsXML_TRANSACTION,
                       krsXML_MODE,        'EXISTING',                                  krsXML_MODE,
                       krsXML_EMAIL,       uppercase(trim(edtPage1_EmailAddress.Text)), krsXML_EMAIL,
                       krsXML_PASSWORD,    uppercase(trim(edtPage1_Password.Text)),     krsXML_PASSWORD,
                       krsXML_MAC,         GetMACID,                                    krsXML_MAC,
                       krsXML_CPU,         GetCPUID,                                    krsXML_CPU,
                     krsXML_TRANSACTION]);
    end;
    result := p.OnRegister(s);
    parList.Assign(p.Pairs);
    if not result then
      Application.MessageBox(PChar(p.sError), krsERROR, MB_OK + MB_ICONERROR);
  finally
    screen.Cursor := crDefault;
    p.free;
  end;
end;

// TfrmWebService
//   OnKeyPress_AZ
//
procedure TfrmWebService.OnKeyPress_AZ(Sender: TObject; var Key: Char);
begin
  if not CharInSet(uppercase(Key)[1], [#8, 'A' .. 'Z', ' ', '0' .. '9']) then
    key := #0;
end;

// TfrmWebService
//   OnKeyPress_Email
//
procedure TfrmWebService.OnKeyPress_Email(Sender: TObject; var Key: Char);
begin
  if not CharInSet(uppercase(Key)[1], [#8, 'A' .. 'Z', '0' .. '9', '@', '_', '.', '+', '-']) then
    key := #0;
end;

// TfrmWebService
//   OnKeyPress_Password
//
procedure TfrmWebService.OnKeyPress_Password(Sender: TObject; var Key: Char);
begin
  if not CharInSet(uppercase(Key)[1], [#8, 'A' .. 'Z', '0' .. '9', '@', '_', '.']) then
    key := #0;
end;

// TfrmWebService
//   OnKeyPress_Field
//
procedure TfrmWebService.OnKeyPress_Field(Sender: TObject; var Key: Char);
begin
  if not CharInSet(uppercase(Key)[1], [#8, 'A' .. 'Z', '0' .. '9', ' ', ',', '-']) then
    key := #0;
end;

end.
