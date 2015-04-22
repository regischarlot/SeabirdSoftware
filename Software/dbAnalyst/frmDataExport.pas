unit frmDataExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, ConnectionLib, ExtCtrls,
  FormLib;

type
  TfrmDataExport = class(TForm)
    PageControl: TPageControl;
    tbsObjects: TTabSheet;
    tbsFileName: TTabSheet;
    tbsExport: TTabSheet;
    lstObjectPool: TListBox;
    btnAddAll: TButton;
    btnAdd: TButton;
    btnRemove: TButton;
    btnRemoveAll: TButton;
    lstSelectedObjects: TListBox;
    lblInitialObjectSelection: TLabel;
    lblObjectPool: TLabel;
    barProgress: TProgressBar;
    lblObjectInProgress: TLabel;
    lblFileName: TLabel;
    edtFileName: TEdit;
    btnFileName: TButton;
    dlgSave: TSaveDialog;
    lblObjectSelection: TLabel;
    lblStep1: TStaticText;
    lblStep2: TStaticText;
    lblStep3: TStaticText;
    lvExport: TListView;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure btnMoveObjects(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure tbsExportShow(Sender: TObject);
    procedure tbsFileNameShow(Sender: TObject);
    procedure ExportProcess(Sender: TObject);

  private
    // Private Members
    //
    m_objExportData: TcData;
    m_objExportMetaData: TcMetaData;
    m_objConnection: TcConnection;

  private
    // Private declarations
    //
    procedure   DisplayObjects(Sender: TObject);
    procedure   SetHeader(Index: longint);

  public
    // Public Properties
    //
    property    objExportData: TcData         read m_objExportData      write m_objExportData;
    property    objExportMetaData: TcMetaData read m_objExportMetaData  write m_objExportMetaData;
    property    Connection: TcConnection      read m_objConnection      write m_objConnection;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daObjectLib,
  daResourceStrings,
  daStreamLib,
  ExecuteLib,
  ActiveX,
  AxCtrls,
  comObj;

// TfrmDataExport
//   FormCreate
//
procedure TfrmDataExport.FormCreate(Sender: TObject);
begin
  m_objExportData := nil;
  m_objExportMetaData := nil;
  m_objConnection := nil;
end;

// TfrmDataExport
//   FormClose
//
procedure TfrmDataExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataExport
//   FormShow
//
procedure TfrmDataExport.FormShow(Sender: TObject);
begin
  if m_objExportData <> nil then
    Caption := Format('Export Content from ''%s''', [m_objExportData.sValue]);
  PageControl.Style := tsButtons;
  tbsObjects.TabVisible := FALSE;
  tbsFileName.TabVisible := FALSE;
  tbsExport.TabVisible := FALSE;
  DisplayObjects(Sender);
  SetHeader(0);
end;

// TfrmDataExport
//   btnNextClick
//
procedure TfrmDataExport.btnNextClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tbsObjects) and (lstSelectedObjects.Items.Count = 0) then
    Application.MessageBox('Please select one or more objects before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else if (PageControl.ActivePage = tbsFileName) and (edtFileName.Text = ksEMPTY) then
    Application.MessageBox('Please enter a file name before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else with PageControl do
    if ActivePageIndex < PageCount - 1 then
      SetHeader(PageControl.ActivePageIndex + 1);
end;

// TfrmDataExport
//   btnPreviousClick
//
procedure TfrmDataExport.btnPreviousClick(Sender: TObject);
begin
  with PageControl do
    if ActivePageIndex > 0 then
      SetHeader(PageControl.ActivePageIndex - 1);
end;

// TfrmDataExport
//   DisplayObjects
//
procedure TfrmDataExport.DisplayObjects(Sender: TObject);

  procedure SubDisplayObjects(parObject: TcData);
  var
    i: longint;
  begin
    if (parObject.eType = enObject) and (parObject.MetaData <> nil) and (parObject.MetaData.Find(enSQL, krsEXPORT) <> nil) then
      lstObjectPool.Items.AddObject(parObject.sValue, parObject);
    for i := 0 to parObject.count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcData) then
        SubDisplayObjects(parObject[i] as TcData);
  end;

begin
  with lstObjectPool.Items do
  try
    screen.Cursor := crHourGlass;
    Clear;
    BeginUpdate;
    SubDisplayObjects(m_objExportData);
    EndUpdate;
  finally
    screen.Cursor := crDefault;
  end;
end;

// TfrmDataExport
//   SetState
//
procedure TfrmDataExport.SetState(Sender: TObject);
begin
  btnAdd.Enabled := lstObjectPool.SelCount > 0;
  btnAddAll.Enabled := lstObjectPool.Items.Count > 0;
  btnRemove.Enabled := lstSelectedObjects.SelCount > 0;
  btnRemoveAll.Enabled := lstSelectedObjects.Items.Count > 0;
  btnNext.Enabled := ((PageControl.ActivePageIndex = 0) and (lstSelectedObjects.Items.Count > 0)) or
                     ((PageControl.ActivePageIndex = 1) and (edtFileName.Text <> ksEMPTY)) or
                     ((PageControl.ActivePageIndex = 2));
  btnPrevious.Enabled := PageControl.ActivePageIndex > 0;
end;

// TfrmDataExport
//   btnMoveObjects
//
procedure TfrmDataExport.btnMoveObjects(Sender: TObject);
var
  i: longint;
begin
  lstObjectPool.Items.BeginUpdate;
  lstSelectedObjects.Items.BeginUpdate;
  //
  // Add & Add All
  if (Sender = btnAddAll) or (Sender = btnAdd) or (Sender = lstObjectPool) then
    for i := lstObjectPool.Items.Count - 1 downto 0 do
      if (Sender = btnAddAll) or (((Sender = btnAdd) or (Sender = lstObjectPool)) and lstObjectPool.Selected[i]) then
      begin
        lstSelectedObjects.Items.AddObject(lstObjectPool.Items[i], lstObjectPool.Items.Objects[i]);
        lstObjectPool.Items.Delete(i);
      end;
  //
  // Remove & Remove All
  if ((Sender = btnRemove) and (lstSelectedObjects.SelCount > 0)) or (Sender = btnRemoveAll) or (Sender = lstSelectedObjects) then
    for i := lstSelectedObjects.Items.Count - 1 downto 0 do
      if (Sender = btnRemoveAll) or (((Sender = btnRemove) or (Sender = lstSelectedObjects)) and lstSelectedObjects.Selected[i]) then
      begin
        lstObjectPool.Items.AddObject(lstSelectedObjects.Items[i], lstSelectedObjects.Items.Objects[i]);
        lstSelectedObjects.Items.Delete(i);
      end;
  //
  lstObjectPool.Items.EndUpdate;
  lstSelectedObjects.Items.EndUpdate;
  SetState(Sender);
end;

// TfrmDataExport
//   btnFileNameClick
//
procedure TfrmDataExport.btnFileNameClick(Sender: TObject);
begin
  dlgSave.FileName := edtFileName.Text;
  if dlgSave.Execute then
    edtFileName.Text := dlgSave.FileName;
end;

// TfrmDataExport
//   tbsExportShow
//
procedure TfrmDataExport.tbsExportShow(Sender: TObject);
var
  i: longint;
  p: TListItem;
begin
  lvExport.Items.BeginUpdate;
  lvExport.Items.Clear;
  for i := 0 to lstSelectedObjects.Items.Count - 1 do
  begin
    p := lvExport.Items.Add;
    p.Caption := lstSelectedObjects.Items[i];
    p.SubItems.Add(ksEMPTY);
    p.Data := lstSelectedObjects.Items.Objects[i];
  end;
  lvExport.Items.EndUpdate;
  barProgress.Position := 0;
end;

// TfrmDataExport
//   ExportProcess
//
procedure TfrmDataExport.ExportProcess(Sender: TObject);
const
  krsCOULDNOTCREATEFILE = 'The request file, "%s", could not be created. Please enter a different file name.';
var
  RootStorage, objStorage: IStorage;
  strm: TcDataStream;
  p: TcData;
  i, L: longint;
  e: TcExecute;
  Stream: IStream;
  li: TListItem;
begin
  strm := nil;
  RootStorage := nil;
  try
    if SUCCEEDED(StgCreateDocFile(PWideChar(WideString(edtFileName.Text)), STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, RootStorage)) then
    begin
      //
      // A. Executable Information
      if SUCCEEDED(RootStorage.CreateStream(krsIE_APPLICATION, STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, Stream)) then
      begin
        try
          strm := TcDataStream.Create(Stream);
          strm.AsAnsiString := AnsiString(GetApplicationVersion);
        finally
          strm.Free;
        end;
        Stream := nil;
      end;
      //
      // B. Objects
      barProgress.Min := 0;
      barProgress.Max := lvExport.Items.Count;
      for i := 0 to lvExport.Items.Count - 1 do
      begin
        p := TcData(lvExport.Items[i].Data);
        if (p <> nil) and (p is TcData) then
        begin
          lblObjectInProgress.Caption := lvExport.Items[i].Caption;
          barProgress.Position := i + 1;
          application.ProcessMessages;
          // Make sure object structure is loaded.
          p.Load(p.MetaData, [elfAll, elfRecursive]);
          // Proceed with export
          li := lvExport.Items[i];
          li.MakeVisible(TRUE);
          if SUCCEEDED(RootStorage.CreateStorage(PWideChar(WideString(li.Caption)), STGM_READWRITE or STGM_CREATE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, objStorage)) then
          begin
            //
            // 1. Data Reference
            if SUCCEEDED(objStorage.CreateStream(krsIE_REFERENCE, STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, Stream)) then
            begin
              try
                strm := TcDataStream.Create(Stream);
                strm.AsInteger := p.Tag;
              finally
                strm.Free;
              end;
              Stream := nil;
            end;
            //
            // 2. Structure
            if SUCCEEDED(objStorage.CreateStream(krsIE_STRUCTURE, STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, Stream)) then
            begin
              try
                strm := TcDataStream.Create(Stream);
                strm.AsAnsiString:= AnsiString(p.XML);
              finally
                strm.Free;
              end;
              Stream := nil;
            end;
            //
            // 3. Is the object including content..?
            if (p.MetaData <> nil) and (p.MetaData.Find(enSQL, krsEXPORT) <> nil) then
            begin
              //
              // 3.1. Data Stream
              L := 0;
              if SUCCEEDED(objStorage.CreateStream(krsIE_DATA, STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, Stream)) then
              begin
                try
                  strm := TcDataStream.Create(Stream);
                  e := nil;
                  try
                    e := TcExecute.Create(nil);
                    e.Connection := m_objConnection;
                    e.Execute(trim((p as TcData).Statement([enSQL], krsEXPORT)), strm);
                    L := e.RowCount;
                  finally
                    e.free;
                  end;
                finally
                  strm.Free;
                end;
                Stream := nil;
              end;
              li.SubItems[0] := inttostr(L);
              //
              // 3.2. Row Count
              if SUCCEEDED(objStorage.CreateStream(krsIE_ROWCOUNT, STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, Stream)) then
              begin
                try
                  strm := TcDataStream.Create(Stream);
                  strm.AsInteger := L;
                finally
                  strm.Free;
                end;
                Stream := nil;
              end;
            end;
            objStorage := nil;
          end;
        end;
      end;
      Application.MessageBox('Export Complete.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
    end
    else
      Application.MessageBox(PChar(Format(krsCOULDNOTCREATEFILE, [edtFileName.Text])), krsINFORMATION, MB_OK + MB_ICONINFORMATION);
  finally
    RootStorage := nil;
  end;
  //
  // Finalize Import
  btnCancel.Caption := '&Close';
  btnCancel.ModalResult := mrOK;
  tbsFileName.TabVisible := FALSE;
  tbsObjects.TabVisible := FALSE;
  lblObjectInProgress.Caption := ksEMPTY;
end;

// TfrmDataExport
//   tbsFileNameShow
//
procedure TfrmDataExport.tbsFileNameShow(Sender: TObject);
begin
  edtFileName.SetFocus;
end;

// TfrmDataExport
//   SetHeader
//
procedure TfrmDataExport.SetHeader(Index: longint);
const
  kasPAGECAPTION: array[0 .. 2] of String =
    ('Select one or more database objects to be exported.',
     'Select a name for the target export file.',
     'Database content is about to be exported. Follow the export progress as content is sent to the export file.');
  kasNEXTCAPTION: array[0 .. 2] of String =
    ('&Next',
     '&Export',
     '&Next');
var
  b: boolean;
  L: longint;
begin
  b := TRUE;
  if Index = 2 then
  begin
    Application.ProcessMessages;
    L := Application.MessageBox('Proceed to export?', krsINFORMATION, MB_YESNOCANCEL + MB_ICONINFORMATION);
    b := L = idYES;
    if L = idCANCEL then
      Close;
  end;
  if b then
  begin
    PageControl.ActivePageIndex := Index;
    if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
      (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, Index + 1);
    lblHeader.Caption := kasPAGECAPTION[Index];
    btnNext.Caption := kasNEXTCAPTION[Index];
    imgHeader.Refresh;
    btnNext.Visible := Index <> 2;
    btnPrevious.Visible := Index <> 2;
    SetState(nil);
    if Index = 2 then
      ExportProcess(nil);
  end;
end;

end.




