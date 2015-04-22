unit frmDataSnapshotDifference;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, ConnectionLib, ExtCtrls,
  FormLib, ToolWin, ImgList, SourceListBox, daObjectLib, CheckLst,
  Menus, ActnList;

type
  TfrmDataSnapshotDifference = class(TForm)
    PageControl: TPageControl;
    tbsFileName: TTabSheet;
    tbsSnapshots: TTabSheet;
    lblObjectInProgress: TLabel;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label1: TLabel;
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    lblMetaDeploymentStructure: TStaticText;
    lblReferenceSchemaDone: TStaticText;
    aniReverseEngineer: TAnimate;
    lblReverseEngineer: TStaticText;
    lblTargetSchemaDone: TStaticText;
    StaticText1: TStaticText;
    tbsCompare: TTabSheet;
    dlgOpen: TOpenDialog;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    btnRefresh: TToolButton;
    btnExecute: TToolButton;
    btnProperties: TToolButton;
    ImageList: TImageList;
    lstItems: TSourceListBox;
    Panel3: TPanel;
    Label2: TLabel;
    grpReference: TGroupBox;
    cbxConnection: TRadioButton;
    edtConnection: TEdit;
    btnConnection: TButton;
    cbxFile: TRadioButton;
    lblFile: TLabel;
    edtFile: TEdit;
    btnFile: TButton;
    ImageList1: TImageList;
    lblConnection: TLabel;
    ActionList1: TActionList;
    actResultRefresh: TAction;
    actResultExecute: TAction;
    actResultProperties: TAction;
    PopupMenu: TPopupMenu;
    Execute1: TMenuItem;
    Properties1: TMenuItem;
    lblLogFiles: TStaticText;
    TabSheet1: TTabSheet;
    StaticText2: TStaticText;
    lstObjects: TCheckListBox;
    btnIncludeExtra: TCheckBox;
    btnIncludeMissing: TCheckBox;
    grpTarget: TGroupBox;
    Label4: TLabel;
    cboTarget: TComboBox;
    cbxLoaded: TRadioButton;
    lblLoaded: TLabel;
    cboLoaded: TComboBox;
    Panel4: TPanel;
    cbxDetails: TCheckBox;
    procedure actResultPropertiesExecute(Sender: TObject);
    procedure actResultExecuteExecute(Sender: TObject);
    procedure actResultRefreshExecute(Sender: TObject);
    procedure lstObjectsClickCheck(Sender: TObject);
    procedure OnStep1Click(Sender: TObject);
    procedure btnConnectionClick(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure SetTargetSchemaList(parCombo: TComboBox);
    function  GetRootUser: TcObject;

  private
    // Private Members
    //
    m_objData: TcData;
    m_objTargetData: TcData;
    m_objTargetMeta: TcMetaData;
    m_objReferenceData: TcData;
    m_objReferenceMeta: TcMetaData;
    m_objConnection: TcConnection;
    m_bPage2, m_bPage3: boolean;
    m_pDifferences: TcObject;
    m_lstObjects: TcBag;
    m_sSchema: String;

  private
    // Private declarations
    //
    procedure   SetHeader(Index: longint);
    function    LoadFromDatabase(parData: TcData; parMetaData: TcMetaData; parSchema: String): boolean;
    function    LoadFromFile(parData: TcData; parMetaData: TcMetaData; parFile: String): boolean;
    procedure   SetAnimation(value: boolean);
    function    CompareSchemas(Sender: TObject): boolean;
    procedure   SetList(Sender: TObject);
    procedure   SetMetaData(value: TcMetaData);

  public
    // Public Properties
    //
    property    objData: TcData               read m_objData            write m_objData;
    property    objMetaData: TcMetaData                                 write SetMetaData;
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
  comObj,
  Main,
  frmDataSnapshotProperty; // TTfrmDataSnapshotProperty

// TfrmDataSnapshotDifference
//   FormCreate
//
procedure TfrmDataSnapshotDifference.FormCreate(Sender: TObject);
begin
  m_objData := nil;
  m_objTargetMeta := TcMetaData.Create(nil);
  m_objReferenceMeta := TcMetaData.Create(nil);
  m_objConnection := nil;
  m_objTargetData := TcData.Create(nil);
  m_objReferenceData := TcData.Create(nil);
  m_bPage2 := FALSE;
  m_bPage3 := FALSE;
  m_pDifferences := nil;
  m_lstObjects := TcBag.Create(nil);
  m_sSchema := ksEMPTY;
end;

// TfrmDataSnapshotDifference
//   FormDestroy
//
procedure TfrmDataSnapshotDifference.FormDestroy(Sender: TObject);
begin
  m_objTargetdata.Free;
  m_objReferenceData.Free;
  m_pDifferences.free;
  m_lstObjects.Free;
  m_objTargetMeta.Free;
  m_objReferenceMeta.Free;
  m_objConnection := nil;
end;

// TfrmDataSnapshotDifference
//   FormClose
//
procedure TfrmDataSnapshotDifference.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataSnapshotDifference
//   FormShow
//
procedure TfrmDataSnapshotDifference.FormShow(Sender: TObject);
begin
  Caption := 'Compare Schema Snapshot';
  PageControl.Style := tsButtons;
  PageControl.Pages[0].TabVisible := FALSE;
  PageControl.Pages[1].TabVisible := FALSE;
  PageControl.Pages[2].TabVisible := FALSE;
  PageControl.Pages[3].TabVisible := FALSE;
  SetList(nil);
  SetTargetSchemaList(cboTarget);
  SetTargetSchemaList(cboLoaded);
  OnStep1Click(nil);
  SetHeader(0);
  cbxConnection.Checked := TRUE;
end;

// TfrmDataSnapshotDifference
//   btnNextClick
//
procedure TfrmDataSnapshotDifference.btnNextClick(Sender: TObject);
begin
  if (PageControl.ActivePageIndex = 0) and (edtFile.Text = ksEMPTY) then
    Application.MessageBox('Please enter a file name before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else if (PageControl.ActivePageIndex = 0) and not FileExists(edtFile.Text) then
    Application.MessageBox('Please enter a valid file name before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else with PageControl do
    if ActivePageIndex < PageCount - 1 then
      SetHeader(PageControl.ActivePageIndex + 1);
end;

// TfrmDataSnapshotDifference
//   btnPreviousClick
//
procedure TfrmDataSnapshotDifference.btnPreviousClick(Sender: TObject);
begin
  with PageControl do
    if ActivePageIndex > 0 then
      SetHeader(PageControl.ActivePageIndex - 1);
end;

// TfrmDataSnapshotDifference
//   SetState
//
procedure TfrmDataSnapshotDifference.SetState(Sender: TObject);
begin
  btnNext.Enabled :=     ((PageControl.ActivePageIndex = 0) and ( (cbxFile.Checked and (edtFile.Text <> ksEMPTY)) or cbxConnection.Checked or (cbxLoaded.Checked and (cboLoaded.ItemIndex <> kiUNDEFINED))) and
                                                                (cboTarget.ItemIndex <> kiUNDEFINED) and (GetRootUser <> nil)) or
                         ((PageControl.ActivePageIndex = 1)) or
                         ((PageControl.ActivePageIndex = 2) and m_bPage2) or
                         ((PageControl.ActivePageIndex = 3));
  btnPrevious.Enabled := ((PageControl.ActivePageIndex = 1) and m_bPage2) or
                         (PageControl.ActivePageIndex = 2) or
                         (PageControl.ActivePageIndex = 3);
end;

// TfrmDataSnapshotDifference
//   btnFileNameClick
//
procedure TfrmDataSnapshotDifference.btnFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edtFile.Text;
  if dlgOpen.Execute then
    edtFile.Text := dlgOpen.FileName;
end;

// TfrmDataSnapshotDifference
//   SetHeader
//
procedure TfrmDataSnapshotDifference.SetHeader(Index: longint);
const
  kasPAGECAPTION: array[0 .. 3] of String =
    ('Select Reference & Target Schema Snapshot File.',
     'Select Objects for Comparison.',
     'Load Structures for Reference and Target Schemas.',
     'Schema Comparison Result.');
  kasNEXTCAPTION: array[0 .. 3] of String =
    ('&Next',
     '&Next',
     '&Next',
     '&Next');
  kasCANCELCAPTION: array[0 .. 3] of String =
    ('&Cancel',
     '&Cancel',
     '&Cancel',
     '&Close');
var
  s1, s2: String;
begin
  PageControl.ActivePageIndex := Index;
  if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
    (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, Index + 1);
  lblHeader.Caption := kasPAGECAPTION[Index];
  btnNext.Caption := kasNEXTCAPTION[Index];
  btnCancel.Caption := kasCANCELCAPTION[Index];
  imgHeader.Refresh;
  btnNext.Visible := Index <> 3;
  btnPrevious.Visible := Index <> 3;
  SetState(nil);
  case Index of
    2:
      if not m_bPage2 then
      try
        screen.Cursor := crHourGlass;
        if cbxConnection.Checked then
          //
        else if cbxFile.Checked then
          lblReferenceSchemaDone.Visible := LoadFromFile(m_objReferenceData, m_objReferenceMeta, edtFile.Text)
        else if cbxLoaded.Checked then
          lblReferenceSchemaDone.Visible := LoadFromDatabase(m_objReferenceData, m_objReferenceMeta, cboLoaded.Items[cboLoaded.ItemIndex]);
        Application.ProcessMessages;
        lblTargetSchemaDone.Visible := LoadFromDatabase(m_objTargetData, m_objTargetMeta, cboTarget.Items[cboTarget.ItemIndex]);
        Application.ProcessMessages;
        // Create log
        s1 := Format('%sComparison %s Reference.xml', [GetFilePath(Application.ExeName), FormatDateTime('mm.dd.yyyy hh.nn.ss', now)]);
        StringToFile(s1, m_objReferenceData.XML, ecsAnsi);
        s2 := Format('%sComparison %s Target.xml', [GetFilePath(Application.ExeName), FormatDateTime('mm.dd.yyyy hh.nn.ss', now)]);
        StringToFile(s2, m_objTargetData.XML, ecsAnsi);
        lblLogFiles.Caption := Format('Reference Schema XML saved as ''%s''.%sTarget Schema XML saved as ''%s''.', [s1, ksCR, s2]);
        // Done.
        m_bPage2 := TRUE;
      finally
        screen.cursor := crDefault;
      end;
    3:
      if not m_bPage3 then
        m_bPage3 := CompareSchemas(lstItems);
  end;
  SetState(nil);
end;

// TfrmDataSnapshotDifference
//   LoadFromDatabase
//
function TfrmDataSnapshotDifference.LoadFromDatabase(parData: TcData; parMetaData: TcMetaData; parSchema: String): boolean;
var
  p: TcObject;
begin
  result := FALSE;
  p := parMetaData.Find([enObject], krsUSER);
  if (p <> nil) and (p is TcMetaData) then
  try
    SetAnimation(TRUE);
    parData.MetaData := p as TcMetaData;
    parData.sValue := parSchema;
    parData.Load(p as TcMetaData, [elfRefresh, elfRecursive]);
    result := parData.Count > 0;
  finally
    SetAnimation(FALSE);
  end;
end;

// TfrmDataSnapshotDifference
//   LoadFromFile
//
function TfrmDataSnapshotDifference.LoadFromFile(parData: TcData; parMetaData: TcMetaData; parFile: String): boolean;
begin
  try
    SetAnimation(TRUE);
    parData.MetaData := parMetaData;
    parData.XML := FileToString(parFile, ecsAnsi);
    result := parData.Count > 0;
  finally
    SetAnimation(FALSE);
  end;
end;

// TfrmDataSnapshotDifference
//   SetAnimation
//
procedure TfrmDataSnapshotDifference.SetAnimation(value: boolean);
begin
  aniReverseEngineer.Visible := value;
  aniReverseEngineer.Active := value;
end;

// TfrmDataSnapshotDifference
//   CompareSchemas
//
function TfrmDataSnapshotDifference.CompareSchemas(Sender: TObject): boolean;
var
  lst: TStringList;
  e: TeDifferenceSet;
begin
  try
    screen.Cursor := crHourGlass;
    lst := nil;
    try
      // Exclusion List
      lst := TStringList.Create;
      lst.Values[krsPLACEHOLDER_USER] := m_objTargetData.sValue;
      // Comparison Set
      e := [ecDifference];
      if btnIncludeExtra.Checked then
        e := e + [ecExtra];
      if btnIncludeMissing.Checked then
        e := e + [ecMissing];
      // Process differences
      if m_pDifferences <> nil then
        m_pDifferences.free;
      m_pDifferences := m_objReferenceData.CompareSchemas(m_objTargetData, lst, e, m_lstObjects);
      try
        lstItems.Items.BeginUpdate;
        lstItems.Clear;
        m_pDifferences.SendToObject(Sender);
      finally
        lstItems.Items.EndUpdate;
      end;
    finally
      lst.free;
    end;
    result := TRUE;
  finally
    screen.Cursor := crDefault;
  end;
end;

// TfrmDataSnapshotDifference
//   btnConnectionClick
//
procedure TfrmDataSnapshotDifference.btnConnectionClick(Sender: TObject);
var
  f: TcFormSet;
begin
  try
    screen.Cursor := crHourGlass;
    f := TcFormSet.Create(nil);
    f.ImageList := frmMain.ImageList;
    f.HeaderImageList := frmMain.HeaderImageList;
    f.AlertImageList := frmMain.AlertImageList;
    f.Preferences := frmMain.Preferences;
    f.frmLog := frmMain.LogForm;
    if f.Connection.Select then
    begin
      Application.ProcessMessages;
      edtConnection.Text := f.Connection.Text;
    end;
    f.free;
  finally
    screen.Cursor := crDefault;
  end;
end;

// TfrmDataSnapshotDifference
//   btnConnectionClick
//
procedure TfrmDataSnapshotDifference.OnStep1Click(Sender: TObject);
const
  kasCOLOR: array[boolean] of TColor =
    (clBtnFace, clWindow);
begin
  edtFile.Enabled := cbxFile.Checked;
  lblFile.Enabled := cbxFile.Checked;
  btnFile.Enabled := cbxFile.Checked;
  edtConnection.Enabled := cbxConnection.Checked;
  lblConnection.Enabled := cbxConnection.Checked;
  btnConnection.Enabled := cbxConnection.Checked;
  cboLoaded.Enabled := cbxLoaded.Checked;
  lblLoaded.Enabled := cbxLoaded.Checked;
  edtFile.Color := kasCOLOR[edtFile.Enabled];
  edtConnection.Color := kasCOLOR[edtConnection.Enabled];
end;

// TfrmDataSnapshotDifference
//   SetList
//
procedure TfrmDataSnapshotDifference.SetList(Sender: TObject);

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
  if m_objTargetMeta <> nil then
    //
    // Graph Filter
    with lstObjects do
    begin
      Items.BeginUpdate;
      Items.Clear;
      Traverse(m_objTargetMeta);
      Items.EndUpdate;
    end;
end;

// TfrmDataSnapshotDifference
//   lstObjectsClickCheck
//
procedure TfrmDataSnapshotDifference.lstObjectsClickCheck(Sender: TObject);
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

// TfrmDataSnapshotDifference
//   actResultRefreshExecute
//
procedure TfrmDataSnapshotDifference.actResultRefreshExecute(Sender: TObject);
begin
  CompareSchemas(lstItems);
  lstItemsClick(nil);
end;

// TfrmDataSnapshotDifference
//   actResultExecuteExecute
//
procedure TfrmDataSnapshotDifference.actResultExecuteExecute(Sender: TObject);
var
  p: TcObject;
  m_objExecute: TcExecute;
  b: boolean;
begin
  p := nil;
  if lstItems.ItemIndex <> kiUNDEFINED then
    p := TcObject(lstItems.items.Objects[lstItems.ItemIndex]);
  if (p <> nil) and (p is TcDataDifference) then
  begin
    // Process SQL Statement
    m_objExecute := nil;
    try
      m_objExecute := TcExecute.Create(nil);
      m_objExecute.Connection := m_objConnection;
      try
        b := m_objExecute.Execute((p as TcDataDifference).sValue, TObject(nil));
      except
        b := FALSE;
      end;
    finally
      m_objExecute.free;
    end;
    if b then
    begin
      // Set the enabled of the line to FALSE
      with lstItems do
        LineEnabled[ItemIndex] := FALSE;
      // Go to then Next Item
      with lstItems do
        if ItemIndex < Items.Count - 1 then
          ItemIndex := ItemIndex + 1;
    end;
    // Set Enables
    lstItemsClick(nil);
  end;
end;

// TfrmDataSnapshotDifference
//   actResultPropertiesExecute
//
procedure TfrmDataSnapshotDifference.actResultPropertiesExecute(Sender: TObject);
var
  p: TcObject;
  frm: TfrmDataSnapshotProperty;
begin
  p := nil;
  if lstItems.ItemIndex <> kiUNDEFINED then
    p := TcObject(lstItems.items.Objects[lstItems.ItemIndex]);
  if (p <> nil) and (p is TcDataDifference) then
  begin
    frm := nil;
    try
      frm := TfrmDataSnapshotProperty.Create(self);
      frm.DataDifference := p as TcDataDifference;
      frm.Connection := m_objConnection;
      frm.ShowModal;
    finally
      frm.Release;
    end;
  end;
end;

// TfrmDataSnapshotDifference
//   lstItemsClick
//
procedure TfrmDataSnapshotDifference.lstItemsClick(Sender: TObject);
var
  p: TcObject;
begin
  p := nil;
  if lstItems.ItemIndex <> kiUNDEFINED then
    p := TcObject(lstItems.items.Objects[lstItems.ItemIndex]);
  actResultExecute.Enabled := p <> nil;
  actResultProperties.Enabled := p <> nil;
end;

// TfrmDataSnapshotDifference
//   SetTargetSchemaList
//
procedure TfrmDataSnapshotDifference.SetTargetSchemaList(parCombo: TComboBox);
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
      parCombo.Items.BeginUpdate;
      parCombo.Items.Clear;
      for i := 0 to (p as TcMetaData).lstIndex.Count - 1 do
      begin
        parCombo.Items.AddObject((p as TcMetaData).lstIndex[i], (p as TcMetaData).lstIndex.Objects[i]);
        if (p as TcMetaData).lstIndex[i] = m_sSchema then
          L := i;
      end;
      parCombo.ItemIndex := L;
      parCombo.Items.EndUpdate;
    end;
  end;
end;

// TfrmDataSnapshotDifference
//   SetMetaData
//
procedure TfrmDataSnapshotDifference.SetMetaData(value: TcMetaData);
begin
  m_objTargetMeta.Clear;
  m_objTargetMeta.Copy(value);
  m_objReferenceMeta.Clear;
  m_objReferenceMeta.Copy(value);
end;

// TfrmDataSnapshotDifference
//   GetRootUser
//
function TfrmDataSnapshotDifference.GetRootUser: TcObject;
begin
  result := nil;
  if (cboTarget.ItemIndex <> kiUNDEFINED) then
    result := m_objData.Find(krsUSER, cboTarget.Items[cboTarget.ItemIndex], 1);
end;

end.




