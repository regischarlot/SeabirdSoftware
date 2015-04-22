unit frmDataPreferences;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, ConnectionLib, CheckLst,
  ExtCtrls;

type
  TfrmDataPreference = class(TForm)
    dlgSave: TSaveDialog;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    PageControl: TPageControl;
    tsGraph: TTabSheet;
    tsScript: TTabSheet;
    lstCheckBoxes: TCheckListBox;
    lblSections: TLabel;
    lstSections: TCheckListBox;
    lblObjectTypes: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);

  private
    m_objData: TcData;
    m_objMetaData: TcMetaData;
    m_objConnection: TcConnection;

  private
    procedure   DisplayObjects(Sender: TObject);

  public
    property    dpData: TcData                  read m_objData                  write m_objData;
    property    dpMetaData: TcMetaData          read m_objMetaData              write m_objMetaData;
    property    Connection: TcConnection        read m_objConnection            write m_objConnection;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daObjectLib,
  daResourceStrings;

// TfrmDataPreference
//   FormCreate
//
procedure TfrmDataPreference.FormCreate(Sender: TObject);
begin
  m_objData := nil;
  m_objMetaData := nil;
  m_objConnection := nil;
end;

// TfrmDataPreference
//   FormClose
//
procedure TfrmDataPreference.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataPreference
//   FormShow
//
procedure TfrmDataPreference.FormShow(Sender: TObject);
begin
  DisplayObjects(Sender);
end;

// TfrmDataPreference
//   DisplayObjects
//
procedure TfrmDataPreference.DisplayObjects(Sender: TObject);

  procedure Traverse(parObject: TcMetaData);
  var
    i, L: longint;
    p: TcObject;
  begin
    if (parObject.eType = enOBJECT) and (AnsiCompareText(parObject.Attribute[krsGRAPH], krsTRUE) = 0) then
    begin
      p := parObject.Ancestor(enObject);
      if p <> nil then
      begin
        L := lstCheckBoxes.Items.AddObject(InitCap(p.sName), pointer(p));
        lstCheckBoxes.Checked[L] := m_objMetaData.Preferences.AsBoolean[p.sName];
      end;
    end;
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcMetaData) then
        Traverse(parObject[i] as TcMetaData);
  end;

var
  p: TcObject;
  i, L: longint;
begin
  if m_objMetaData <> nil then
  begin
    //
    // Graph Filter
    with lstCheckBoxes do                          
    begin
      Items.BeginUpdate;
      Items.Clear;
      Traverse(m_objMetaData);
      Items.EndUpdate;
    end;
    //
    // Script Sections
    with lstSections do
    begin
      Items.BeginUpdate;
      Items.Clear;
      p := m_objMetaData.Sections;
      if (p <> nil) and (p is TcObject) then
        for i := 0 to p.count - 1 do
        begin
          L := Items.AddObject(InitCap(p[i].sName), pointer(p[i]));
          Checked[L] := p[i].sValue <> krsFALSE;
        end;
      Items.EndUpdate;
    end;
  end;
  btnOK.Enabled := m_objMetaData <> nil;
end;

// TfrmDataPreference
//   btnOKClick
//
procedure TfrmDataPreference.btnOKClick(Sender: TObject);
var
  p: TcObject;
  i: longint;
begin
  if (m_objMetaData <> nil) and (m_objMetaData.Preferences <> nil) then
  begin
    //
    // Graph Filter
    for i:= 0 to lstCheckBoxes.Items.Count - 1 do
    begin
      p := TcObject(lstCheckBoxes.Items.Objects[i]);
      if p <> nil then
        m_objMetaData.Preferences.AsBoolean[p.sName] := lstCheckBoxes.Checked[i];
    end;
    m_objMetaData.Preferences.Save;
    //
    // Script Sections
    for i:= 0 to lstSections.Items.Count - 1 do
    begin
      p := TcObject(lstSections.Items.Objects[i]);
      if p <> nil then
        p.sValue := kasBOOL[lstSections.Checked[i]];
    end;
    //
    // Done
    ModalResult := mrOK;
  end;
end;

end.




