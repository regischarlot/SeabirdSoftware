unit frmAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, PreferenceLib;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    imgLogo: TImage;
    Timer: TTimer;
    lblVersion: TLabel;
    lblLicenseExpiration: TLabel;
    lblInfo: TLabel;
    lblUserName: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnGetInformation(Sender: TObject);

  private
    m_objPreferences: TcPreferenceList;

  public
    property Preferences: TcPreferenceList read m_objPreferences write m_objPreferences;
  end;

implementation

uses
  daGlobals,
  daResourceStrings,
  WebServiceLib,
  strUtils;

{$R *.DFM}

const
  kaiWINDOWHEIGHT: array[boolean] of integer = (175, 288);

// TfrmAbout
//   FormCreate
//
procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  m_objPreferences := nil;
end;

// TfrmAbout
//   FormShow
//
procedure TfrmAbout.FormShow(Sender: TObject);
var
  s: String;
begin
  Height := kaiWINDOWHEIGHT[FALSE];
  timer.enabled := not (fsModal in FormState);
  lblVersion.Caption := GetapplicationVersion + ksCR + 'Started ' + m_objPreferences.StringVal[krsPREF_STARTUPCOUNT] + ' times.';
  lblLicenseExpiration.Caption := m_objPreferences.LicenseStatus;
  s := Format('Registered to %s', [m_objPreferences.StringVal[krsUSER_FULLNAME]]);
  if m_objPreferences.StringVal[krsUSER_SINCEDATE] <> ksEMPTY then
    s := s + ksCR + Format('Since %s', [m_objPreferences.StringVal[krsUSER_SINCEDATE]]);
  lblUserName.Caption := s;
  OnGetInformation(Sender);
end;

// TfrmAbout
//   FormClose
//
procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmAbout
//   btnCloseClick
//
procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// TfrmAbout
//   btnCloseClick
//
procedure TfrmAbout.OnGetInformation(Sender: TObject);
var
  p: TcWebServiceCall;
  s, sv: String;
const
  kabVERSIONMATCHED: array[boolean] of string =
    ('Please upgrade your dbAnalyst version to the latest production version.', 'Your dbAnalyst version is up to date.');
begin
  // Get Information?
  p := nil;
  try
    p := TcWebServiceCall.Create(nil);
    p.Preferences := m_objPreferences;
    try
      if p.OnGetInfo then
      begin
        sv := p.GetInfoTag('last_version');
        if CompareVersion(sv, GetapplicationVersion) > 0 then
        begin                             
          s := Format('Version %s is the current production version of dbAnalyst: %s' , [sv, kabVERSIONMATCHED[sv = GetapplicationVersion]]);
          s := s + ksCR + ksCR + p.GetInfoTag('news') + ksCR;
          lblInfo.Caption := s;
          Height := kaiWINDOWHEIGHT[TRUE];
        end;
      end;
    except
      //
    end;
  finally
    p.free;
  end;
end;

end.
