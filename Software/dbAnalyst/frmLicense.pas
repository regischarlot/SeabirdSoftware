unit frmLicense;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PreferenceLib;

type
  TfrmLicensing = class(TForm)
    edtLicense1: TEdit;
    lblLicense: TLabel;
    btnOK: TButton;
    edtLicense2: TEdit;
    edtLicense3: TEdit;
    lblLicenseInfo: TLabel;
    btnCancel: TButton;
    Label1: TLabel;
    btnVisit: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LicenseChange(Sender: TObject);
    procedure edtLicenseKeyPress(Sender: TObject; var Key: Char);
    procedure btnVisitClick(Sender: TObject);

  private
    m_objPreferences: TcPreferenceList;

  public
    property Preferences: TcPreferenceList read m_objPreferences write m_objPreferences;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daResourceStrings;

// TfrmLicensing
//   FormCreate
//
procedure TfrmLicensing.FormCreate(Sender: TObject);
begin
  m_objPreferences := nil;
end;

// TfrmLicensing
//   FormShow
//
procedure TfrmLicensing.FormShow(Sender: TObject);
begin
  if (m_objPreferences <> nil) and (m_objPreferences.StringVal[krsLICENSEKEY] <> ksEMPTY) then
  begin
    edtLicense1.Text := system.copy(m_objPreferences.StringVal[krsLICENSEKEY], 1, 5);
    edtLicense2.Text := system.copy(m_objPreferences.StringVal[krsLICENSEKEY], 6, 5);
    edtLicense3.Text := system.copy(m_objPreferences.StringVal[krsLICENSEKEY], 11, 5);
    if (m_objPreferences.StringVal[krsLICENSEKEY] <> ksEMPTY) and not m_objPreferences.LicenseIsValid then
      Application.MessageBox('Your current software license has expired. Please visit http://SeabirdSoftware.com to request a new license.', krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
  end;
end;

// TfrmLicensing
//   FormClose
//
procedure TfrmLicensing.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmLicensing
//   btnCancelClick
//
procedure TfrmLicensing.btnCancelClick(Sender: TObject);
begin
  modalResult := mrCancel;
end;

// TfrmLicensing
//   LicenseFieldKeyUp
//
procedure TfrmLicensing.LicenseChange(Sender: TObject);
var
  iDate, iUser, iLicenseNumber: longint;
begin
  if (Sender <> nil) and (Sender is TEdit) and (length((Sender as TEdit).Text) = 5) then
  begin
    btnOK.Enabled := Decode(edtLicense1.Text + edtLicense2.Text + edtLicense3.Text, iDate, iUser, iLicenseNumber);
    if Sender = edtLicense1 then
      edtLicense2.SetFocus
    else if Sender = edtLicense2 then
      edtLicense3.SetFocus
    else if (Sender = edtLicense3) and btnOK.Enabled then
      btnOK.SetFocus;
  end;
end;

// TfrmLicensing
//   btnOKClick
//
procedure TfrmLicensing.btnOKClick(Sender: TObject);
var
  iDate, iUser, iLicenseNumber: longint;
  s: String;
begin
  s := edtLicense1.Text + edtLicense2.Text + edtLicense3.Text;
  if Decode(s, iDate, iUser, iLicenseNumber) then
  begin
    m_objPreferences.StringVal[krsLICENSEKEY] := s;
    m_objPreferences.WriteToRegistry; // Make sure it makes it back to the registry right away
    ModalResult := mrOK;
  end;
end;

// TfrmLicensing
//   edtLicenseKeyPress
//
procedure TfrmLicensing.edtLicenseKeyPress(Sender: TObject; var Key: Char);
begin
  if not (((uppercase(Key) >= 'A') and (uppercase(Key) <= 'Z')) or (Key = #8)) then
    Key := #0;
end;

// TfrmLicensing
//   btnVisitClick
//
procedure TfrmLicensing.btnVisitClick(Sender: TObject);
begin
  ShellOpenFile('http://www.SeabirdSoftware.com/');
end;

end.
