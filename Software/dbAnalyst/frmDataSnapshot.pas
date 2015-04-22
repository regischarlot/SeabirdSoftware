unit frmDataSnapshot;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, ConnectionLib, ExtCtrls,
  FormLib, CheckLst, daObjectLib;

type
  TfrmDataSnapshot = class(TForm)
    PageControl: TPageControl;
    tbsFileName: TTabSheet;
    lblFileName: TLabel;
    edtFileName: TEdit;
    btnFileName: TButton;
    dlgSave: TSaveDialog;
    lblStep2: TStaticText;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label1: TLabel;
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnNext: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    StaticText2: TStaticText;
    lstObjects: TCheckListBox;
    Button1: TButton;
    btnTransformNames: TCheckBox;
    Label4: TLabel;
    cboTarget: TComboBox;
    StaticText1: TStaticText;
    procedure lstObjectsClickCheck(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure tbsExportShow(Sender: TObject);
    procedure tbsFileNameShow(Sender: TObject);
    function ExportProcess(Sender: TObject): boolean;
    procedure SetList(Sender: TObject);
    procedure SetTargetSchemaList(Sender: TObject);
    function GetRootUser: TcObject;

  private
    // Private Members
    //
    m_objData: TcData;
    m_objMetaData: TcMetaData;
    m_objConnection: TcConnection;
    m_lstObjects: TcBag;
    m_lstreplacements: TStringList;
    m_sSchema: String;

  private
    // Private declarations
    //
    procedure   SetHeader(Index: longint);

  public
    // Public Properties
    //
    property    objData: TcData               read m_objData            write m_objData;
    property    objMetaData: TcMetaData       read m_objMetaData        write m_objMetaData;
    property    Connection: TcConnection      read m_objConnection      write m_objConnection;
    property    Schema: String                                          write m_sSchema;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daResourceStrings,
  daStreamLib,
  ExecuteLib,
  ActiveX,
  AxCtrls,
  comObj;

// TfrmDataSnapshot
//   FormCreate
//
procedure TfrmDataSnapshot.FormCreate(Sender: TObject);
begin
  m_objMetaData := nil;
  m_objConnection := nil;
  m_objData := nil;
  m_lstObjects := TcBag.Create(nil);
  m_lstReplacements := TStringList.Create;
  m_sSchema := ksEMPTY;
end;

// TfrmDataSnapshot
//   FormDestroy
//
procedure TfrmDataSnapshot.FormDestroy(Sender: TObject);
begin
  m_lstObjects.free;
  m_lstReplacements.Free;
end;

// TfrmDataSnapshot
//   FormClose
//
procedure TfrmDataSnapshot.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataSnapshot
//   FormShow
//
procedure TfrmDataSnapshot.FormShow(Sender: TObject);
begin
  PageControl.Style := tsButtons;
  tbsFileName.TabVisible := FALSE;
  SetList(nil);
  SetTargetSchemaList(Sender);
  SetHeader(0);
end;

// TfrmDataSnapshot
//   btnNextClick
//
procedure TfrmDataSnapshot.btnNextClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tbsFileName) and (edtFileName.Text = ksEMPTY) then
    Application.MessageBox('Please enter a file name before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else if ExportProcess(nil) then
    Close;
end;

// TfrmDataSnapshot
//   btnPreviousClick
//
procedure TfrmDataSnapshot.btnPreviousClick(Sender: TObject);
begin
  with PageControl do
    if ActivePageIndex > 0 then
      SetHeader(PageControl.ActivePageIndex - 1);
end;

// TfrmDataSnapshot
//   SetState
//
procedure TfrmDataSnapshot.SetState(Sender: TObject);
begin
  btnNext.Enabled := (edtFileName.Text <> ksEMPTY) and (GetRootUser <> nil);
end;

// TfrmDataSnapshot
//   btnFileNameClick
//
procedure TfrmDataSnapshot.btnFileNameClick(Sender: TObject);
begin
  dlgSave.FileName := edtFileName.Text;
  if dlgSave.Execute then
    edtFileName.Text := dlgSave.FileName;
end;

// TfrmDataSnapshot
//   tbsExportShow
//
procedure TfrmDataSnapshot.tbsExportShow(Sender: TObject);
begin
  //
end;

// TfrmDataSnapshot
//   GetRootUser
//
function TfrmDataSnapshot.GetRootUser: TcObject;
begin
  result := nil;
  if (cboTarget.ItemIndex <> kiUNDEFINED) then
    result := m_objData.Find(krsUSER, cboTarget.Items[cboTarget.ItemIndex], 1);
end;

// TfrmDataSnapshot
//   ExportProcess
//
function TfrmDataSnapshot.ExportProcess(Sender: TObject): boolean;
const
  krsCOULDNOTCREATEFILE = 'The request file, "%s", could not be created. Please enter a different file name.';
var
  p: TcObject;
begin
  p := GetRootUser;
  if (p <> nil) and (p is TcData) then
  try
    screen.Cursor := crHourGlass;
    // Load Schema
    (p as TcData).Load((p as TcData).MetaData, [elfRefresh, elfRecursive]);
    // Replacement values
    m_lstReplacements.Clear;
    if btnTransformNames.Checked then
      m_lstReplacements.Values[p.sValue] := krsPLACEHOLDER_USER;
    // Create String
    StringToFile(edtFileName.Text, p.XML_Filtered(m_lstObjects, m_lstReplacements), ecsAnsi);
  finally
    screen.Cursor := crDefault;
  end;
  Application.MessageBox('Export Complete.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
  result := TRUE;
end;

// TfrmDataSnapshot
//   tbsFileNameShow
//
procedure TfrmDataSnapshot.tbsFileNameShow(Sender: TObject);
begin
  edtFileName.SetFocus;
end;

// TfrmDataSnapshot
//   SetHeader
//
procedure TfrmDataSnapshot.SetHeader(Index: longint);
begin
  if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
    (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, 0);
  PageControl.ActivePageIndex := Index;
  imgHeader.Refresh;
  SetState(nil);
end;

// TfrmDataSnapshot
//   SetList
//
procedure TfrmDataSnapshot.SetList(Sender: TObject);

  procedure Traverse(parObject: TcMetaData);
  var
    i, L: longint;
  begin
    if (parObject.eType = enObject) and (parObject.Parent <> nil) and (parObject.Parent is TcMetaData) and ((parObject.Parent as TcMetaData).eType = enObject) and (AnsiCompareText(parObject.sName, krsUSER) <> 0) then
    begin
      L := lstObjects.Items.AddObject(parObject.DisplayName, pointer(parObject));
      m_lstObjects.Add(parObject);
      lstObjects.Checked[L] := TRUE;
    end;
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcMetaData) then
        Traverse(parObject[i] as TcMetaData);
  end;

begin
  if m_objMetaData <> nil then
    //
    // Graph Filter
    with lstObjects do
    begin
      Items.BeginUpdate;
      Items.Clear;
      Traverse(m_objMetaData);
      Items.EndUpdate;
    end;
end;

// TfrmDataSnapshot
//   lstObjectsClickCheck
//
procedure TfrmDataSnapshot.lstObjectsClickCheck(Sender: TObject);
var
  L: longint;
  p: TcObject;
begin
  L := lstObjects.ItemIndex;
  if L <> kiUNDEFINED then
  begin
    p := TcObject(lstObjects.Items.Objects[L]);
    if (p <> nil) and lstObjects.Checked[L] and (m_lstObjects.IndexOf(p) = kiUNDEFINED) then
      m_lstObjects.Add(p)
    else if (p <> nil) and not lstObjects.Checked[L] and (m_lstObjects.IndexOf(p) <> kiUNDEFINED) then
      m_lstObjects.Delete(p);
  end;
end;

// TfrmDataSnapshot
//   SetTargetSchemaList
//
procedure TfrmDataSnapshot.SetTargetSchemaList(Sender: TObject);
var
  p: TcObject;
  i, L: longint;
begin
  if (m_objData <> nil) and (m_objData.MetaData <> nil) then
  begin
    L := kiUNDEFINED;
    p := m_objData.MetaData.Find([enObject], krsUSER);
    if (p <> nil) and (p is TcMetaData) then
    begin
      cboTarget.Items.BeginUpdate;
      cboTarget.Items.Clear;
      for i := 0 to (p as TcMetaData).lstIndex.Count - 1 do
      begin
        cboTarget.Items.AddObject((p as TcMetaData).lstIndex[i], (p as TcMetaData).lstIndex.Objects[i]);
        if (p as TcMetaData).lstIndex[i] = m_sSchema then
          L := i;
      end;
      cboTarget.ItemIndex := L;
      cboTarget.Items.EndUpdate;
    end;
  end;
end;

end.




