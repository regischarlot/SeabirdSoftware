unit frmDataImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, daObjectLib, ImgList, ConnectionLib,
  ExtCtrls, FormLib;

type
  TfrmDataImport = class(TForm)
    dlgOpen: TOpenDialog;
    PageControl: TPageControl;
    tbsObjects: TTabSheet;
    lblInitialObjectSelection: TLabel;
    lblObjectPool: TLabel;
    lstObjectPool: TListBox;
    btnAddAll: TButton;
    btnAdd: TButton;
    btnRemove: TButton;
    btnRemoveAll: TButton;
    lstSelectedObjects: TListBox;
    tbsFileName: TTabSheet;
    lblFileNameLabel: TLabel;
    tbsImport: TTabSheet;
    lblObjectInProgress: TLabel;
    barProgress: TProgressBar;
    tbsRename: TTabSheet;
    lvObjects: TListView;
    btnStep3Edit: TButton;
    lstImages: TImageList;
    lblObjectName: TLabel;
    Label1: TLabel;
    edtCommitPoint: TEdit;
    Label2: TLabel;
    btnUpDown: TUpDown;
    lblStep2: TStaticText;
    lvImported: TListView;
    lblLogFileLabel: TLabel;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    edtFileName: TEdit;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblLogFile: TLabel;
    StaticText3: TStaticText;
    btnFileName: TButton;
    StaticText4: TStaticText;
    lblStep3: TStaticText;
    lblObjectSelection: TLabel;
    Label4: TLabel;
    btnCreateObjects: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure MoveObjectsClick(Sender: TObject);
    procedure tbsRenameShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbsObjectsShow(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure lvObjectsResize(Sender: TObject);
    procedure btnStep3EditClick(Sender: TObject);
    procedure lvObjectsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ImportProcess(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure edtCommitPointKeyPress(Sender: TObject; var Key: Char);
    procedure btnFileNameClick(Sender: TObject);

  private
    // Private Members
    //
    m_lstImport: TcCollection;
    m_objMetaData: TcMetaData;
    m_objConnection: TcConnection;
    m_sLogFile: String;
    m_objFormData: TcData;

  private
    // Private declarations
    //
    function    CanImport: boolean;
    function    ReadImportFile(value: String): boolean;
    procedure   ResultReport(Value: longint);
    procedure   SetMetaData(value: TcMetaData);
    procedure   SetHeader(Index: longint);
    function    IsFull(parPage: longint): boolean;

  public
    // Public Properties
    //
    property    objMetaData: TcMetaData     read m_objMetaData     write SetMetaData;
    property    objConnection: TcConnection read m_objConnection   write m_objConnection;
    property    objFormData: TcData         read m_objFormData     write m_objFormData;
  end;

  TcImportOption = (eioHasContent, eioSelected, eioCreateObject, eioExists);
  TcImportOptions = set of TcImportOption;

  TcImport = class(TcObject)
  private
    // Private Members
    //
    m_iRowCount: longint;
    m_eOptions: TcImportOptions;
    m_sCreate: String;
    m_xmlStructure: String;
    m_objData: TcData;

  public
    // Public declarations
    //
    constructor Create(parParent: TcObject); override;
    destructor  Destroy; override;

  public
    // Public Properties
    //
    property iRowCount: longint             read m_iRowCount       write m_iRowCount;
    property eOptions: TcImportOptions      read m_eOptions        write m_eOptions;
    property sCreate: String                read m_sCreate         write m_sCreate;
    property Data: TcData                   read m_objData         write m_objData;
    property xmlStructure: String           read m_xmlStructure    write m_xmlStructure;
  end;

implementation

{$R *.dfm}

uses
  ActiveX,
  AxCtrls,
  comObj,
  daGlobals,
  daStreamLib,
  ExecuteLib,
  daResourceStrings,
  frmDataImportEdit;

const
  kiIMAGE: array[boolean] of longint = (0, 1);

//
// TcImport
//

// TcImport
//   Create
//
constructor TcImport.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_iRowCount := 0;
  m_eOptions := [];
  m_sCreate := ksEMPTY;
  m_objData := TcData.Create(nil);
  m_xmlStructure := ksEMPTY;
end;

// TcImport
//   Destroy
//
Destructor TcImport.Destroy;
begin
  m_objData.free;
  inherited Destroy;
end;

//
// TfrmDataExport
//

// TfrmDataExport
//   FormCreate
//
procedure TfrmDataImport.FormCreate(Sender: TObject);
begin
  m_lstImport := TcCollection.Create(nil);
  m_objMetaData := TcMetaData.Create(nil);
  m_objFormData := nil;
  m_sLogFile := Format('%sdbAnalyst %s.log', [GetFilePath(Application.ExeName), FormatDateTime('yyyymmddhhnnsszzz', now)]);
end;

// TfrmDataExport
//   FormDestroy
//
procedure TfrmDataImport.FormDestroy(Sender: TObject);
begin
  m_lstImport.free;
  m_objMetaData.free;
end;

// TfrmDataExport
//   FormShow
//
procedure TfrmDataImport.FormShow(Sender: TObject);
begin
  if m_objFormData <> nil then
    Caption := Format('Import Content into ''%s''', [m_objFormData.sValue]);
  PageControl.Style := tsButtons;
  PageControl.Pages[0].TabVisible := FALSE;
  PageControl.Pages[1].TabVisible := FALSE;
  PageControl.Pages[2].TabVisible := FALSE;
  PageControl.Pages[3].TabVisible := FALSE;
  lblLogFile.Caption := m_sLogFile;
  SetHeader(0);
  edtFileName.SetFocus;
end;

// TfrmDataExport
//   FormClose
//
procedure TfrmDataImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataExport
//   ReadImportFile
//
function TfrmDataImport.ReadImportFile(value: String): boolean;

  procedure CreateList(var lst: TStringList; parData: TcData);
  var
    i: longint;
    s: String;
  begin
    if parData.eType = enObject then
    begin
      s := parData.Header;
      if s <> ksEMPTY then
        lst.AddObject(s, parData);
    end;
    for i := 0 to parData.count - 1 do
      if (parData[i] <> nil) and (parData[i] is TcData) then
        CreateList(lst, parData[i] as TcData);
  end;

var
  RootStorage, objStorage: IStorage;
  Enum: IEnumSTATSTG;
  StatStg: TStatStg;
  NumFetched: integer;
  p: TcImport;
  Stream: IStream;
  strm: TcDataStream;
  lstExisting: TStringList;
  lstRead: TStringList;
  sOptions: String;
begin
  try
    // Empty options section for now.
    sOptions := m_objMetaData.SectionsAsText;
    m_objMetaData.SectionsAsText := m_objMetaData.EmptySectionsAsText;
    //
    lstExisting := nil;
    lstRead := nil;
    try
      Screen.Cursor := crHourGlass;
      lstExisting := TStringList.Create;
      lstRead := TStringList.Create;
      if m_objFormData <> nil then
        CreateList(lstExisting, m_objFormData);
      m_lstImport.Clear;
      result := FileExists(value);
      if result then
      begin
        result := SUCCEEDED(StgOpenStorage(PWideChar(WideString(value)), nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, RootStorage));
        if not result then
          Application.MessageBox(PChar(Format('''%s'' could not be opened. Please check that this file is not opened into an other application.', [value])), 'Error', MB_OK + MB_ICONWARNING)
        else
        begin
          //
          // 1. Proceed by element
          if SUCCEEDED(RootStorage.EnumElements(0, nil, 0, Enum)) then
          begin
            while Enum.Next(1, StatStg, @NumFetched) = S_OK do
              if StatStg.dwType = STGTY_STORAGE then
              begin
                p := TcImport.Create(nil);
                m_lstImport.Add(p);
                p.sName := StatStg.pwcsName;
                p.m_eOptions := [];
                p.iRowCount := 0;
                if SUCCEEDED(RootStorage.OpenStorage(PWideChar(WideString(p.sName)), nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, objStorage)) then
                begin
                  //
                  // 1.1. Reference
                  strm := nil;
                  if SUCCEEDED(objStorage.OpenStream(krsIE_REFERENCE, nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, Stream)) then
                  begin
                    try
                      strm := TcDataStream.Create(Stream);
                      p.Tag := strm.AsInteger;
                    finally
                      strm.Free;
                    end;
                    Stream := nil;
                  end;
                  //
                  // 1.2. Structure
                  if SUCCEEDED(objStorage.OpenStream(krsIE_STRUCTURE, nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, Stream)) then
                  begin
                    try
                      strm := TcDataStream.Create(Stream);
                      p.xmlStructure := String(strm.AsAnsiString);
                    finally
                      strm.Free;
                    end;
                    Stream := nil;
                  end;
                  //
                  // 1.3. Row Count
                  strm := nil;
                  if SUCCEEDED(objStorage.OpenStream(krsIE_ROWCOUNT, nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, Stream)) then
                  begin
                    try
                      strm := TcDataStream.Create(Stream);
                      p.iRowCount := strm.AsInteger;
                      p.eOptions := p.eOptions + [eioHasContent];
                    finally
                      strm.Free;
                    end;
                    Stream := nil;
                  end;
                  //
                  // 1.4. Free up resources
                  objStorage := nil;
                end;
                // 2. Locate objData, inject XML, and set relationships
                p.Data.Clear;
                p.Data.MetaData := m_objMetaData;
                p.Data.Parent := m_objFormData;
                p.Data.XML := p.xmlStructure;
                p.sCreate := TextToText(p.Data.Statement([enScript], krsCREATE));
                if lstExisting.IndexOf(p.sName) <> kiUNDEFINED then
                  p.eOptions := p.eOptions + [eioExists]
                else
                  p.eOptions := p.eOptions + [eioCreateObject];
              end;

          end;
        end;
      end;
    finally
      screen.Cursor := crDefault;
      lstExisting.Free;
      lstRead.free;
    end;
  finally
    m_objMetaData.SectionsAsText := sOptions;
  end;
end;

// TfrmDataExport
//   tbsObjectsShow
//
procedure TfrmDataImport.tbsObjectsShow(Sender: TObject);
var
  i: longint;
  p: TcImport;
begin
  lstObjectPool.Items.BeginUpdate;
  lstObjectPool.Items.Clear;
  lstSelectedObjects.Items.BeginUpdate;
  lstSelectedObjects.Items.Clear;
  for i := 0 to m_lstImport.Count - 1 do
  begin
    p := m_lstImport[i] as TcImport;
    if eioSelected in p.m_eOptions then
      lstSelectedObjects.Items.AddObject(p.sName, p)
    else
      lstObjectPool.Items.AddObject(p.sName, p);
  end;
  lstObjectPool.Items.EndUpdate;
  lstSelectedObjects.Items.EndUpdate;
end;

// TfrmDataImport
//   SetState
//
procedure TfrmDataImport.SetState(Sender: TObject);
begin
  btnAdd.Enabled := lstObjectPool.SelCount > 0;
  btnAddAll.Enabled := lstObjectPool.Items.Count > 0;
  btnRemove.Enabled := lstSelectedObjects.SelCount > 0;
  btnRemoveAll.Enabled := lstSelectedObjects.Items.Count > 0;
  btnNext.Enabled := IsFull(PageControl.ActivePageIndex);
  btnPrevious.Enabled := PageControl.ActivePageIndex > 0;
  btnStep3Edit.Enabled := (PageControl.ActivePage = tbsRename) and (lvObjects.Selected <> nil);
  Application.ProcessMessages;
end;

// TfrmDataImport
//   IsFull
//
function TfrmDataImport.IsFull(parPage: longint): boolean;
begin
  // Page = 0
  result := (PageControl.ActivePageIndex >= 0) and
            (edtFileName.Text <> ksEMPTY) and
            (edtCommitPoint.Text <> ksEMPTY);
  // Page = 1
  if result and (PageControl.ActivePageIndex >= 1) then
    result := result and (lstSelectedObjects.Items.Count > 0);
end;

// TfrmDataImport
//   btnNextClick
//
procedure TfrmDataImport.btnNextClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tbsObjects) and (lstSelectedObjects.Items.Count = 0) then
    Application.MessageBox('Please select one or more objects before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else if (PageControl.ActivePage = tbsFileName) and (edtFileName.Text = ksEMPTY) then
    Application.MessageBox('Please enter a file name before proceeding to the next page.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
  else with PageControl do
    if ActivePageIndex < PageCount - 1 then
      SetHeader(ActivePageIndex + 1);
end;

// TfrmDataImport
//   btnPreviousClick
//
procedure TfrmDataImport.btnPreviousClick(Sender: TObject);
begin
  with PageControl do
    if ActivePageIndex > 0 then
      SetHeader(ActivePageIndex - 1);
end;

// TfrmDataImport
//   MoveObjectsClick
//
procedure TfrmDataImport.MoveObjectsClick(Sender: TObject);
var
  i: longint;
  p: TcImport;
begin
  lstObjectPool.Items.BeginUpdate;
  lstSelectedObjects.Items.BeginUpdate;
  //
  // Add & Add All
  if (Sender = btnAddAll) or (Sender = btnAdd) or (Sender = lstObjectPool) then
    for i := lstObjectPool.Items.Count - 1 downto 0 do
      if (Sender = btnAddAll) or (((Sender = btnAdd) or (Sender = lstObjectPool)) and lstObjectPool.Selected[i]) then
      begin
        p := lstObjectPool.Items.Objects[i] as TcImport;
        lstSelectedObjects.Items.AddObject(p.sName, p);
        lstObjectPool.Items.Delete(i);
        p.eOptions := p.eOptions + [eioSelected];
      end;
  //
  // Remove & Remove All
  if ((Sender = btnRemove) and (lstSelectedObjects.SelCount > 0)) or (Sender = btnRemoveAll) or (Sender = lstSelectedObjects) then
    for i := lstSelectedObjects.Items.Count - 1 downto 0 do
      if (Sender = btnRemoveAll) or (((Sender = btnRemove) or (Sender = lstSelectedObjects)) and lstSelectedObjects.Selected[i]) then
      begin
        p := lstSelectedObjects.Items.Objects[i] as TcImport;
        lstObjectPool.Items.AddObject(p.sName, p);
        lstSelectedObjects.Items.Delete(i);
        p.eOptions := p.eOptions - [eioSelected];
      end;
  //
  lstObjectPool.Items.EndUpdate;
  lstSelectedObjects.Items.EndUpdate;
  SetState(Sender);
end;

// TfrmDataImport
//   tbsRenameShow
//
procedure TfrmDataImport.tbsRenameShow(Sender: TObject);
var
  i: longint;
  L: TListItem;
  p: TcImport;
begin
  with lvObjects.Items do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to m_lstImport.Count - 1 do
    begin
      p := m_lstImport[i] as TcImport;
      if eioSelected in p.eOptions then
      begin
        L := Add;
        L.Caption := ksEMPTY;
        L.SubItems.Add(p.sName);
        if eioHasContent in p.eOptions then
          L.SubItems.Add(inttostr(p.iRowCount))
        else
          L.SubItems.Add(ksEMPTY);
        L.Data := p;
        L.ImageIndex := kiIMAGE[eioCreateObject in p.eOptions];
      end;
    end;
    EndUpdate;
  end;
end;

// TfrmDataExport
//   PageControlChanging
//
procedure TfrmDataImport.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := ((PageControl.ActivePage = tbsFileName) and (edtFileName.Text <> ksEMPTY)) or
                 ((PageControl.ActivePage = tbsObjects)) or
                 ((PageControl.ActivePage = tbsRename) and (lstSelectedObjects.Items.Count > 0)) or
                 ((PageControl.ActivePage = tbsImport) and (lstSelectedObjects.Items.Count > 0));
end;

// TfrmDataExport
//   PageControlChange
//
procedure TfrmDataImport.PageControlChange(Sender: TObject);
var
  b: boolean;
begin
  PageControlChanging(PageControl.ActivePage, b);
  if not b then
    btnPreviousClick(Sender)
  else
  begin
    SetState(Sender);
    if CanImport and (PageControl.ActivePage = tbsImport) then
    begin
      Application.ProcessMessages;
      case Application.MessageBox('Process to import?', krsINFORMATION, MB_YESNOCANCEL + MB_ICONINFORMATION) of
       idYES:
         ImportProcess(Sender);
       idNO:
         PageControl.SelectNextPage(FALSE);
       idCANCEL:
         Close;
      end;
    end;
  end;
  Application.ProcessMessages;
end;

// TfrmDataExport
//   lvObjectsResize
//
procedure TfrmDataImport.lvObjectsResize(Sender: TObject);
begin
  with lvObjects do
    Columns[0].Width := width - Columns[1].Width - Columns[2].Width - 4;
end;

// TfrmDataExport
//   btnStep3EditClick
//
procedure TfrmDataImport.btnStep3EditClick(Sender: TObject);
var
  p: TcImport;
begin
  if lvObjects.Selected <> nil then
  begin
    p := TcImport(lvObjects.Selected.Data);
    if p <> nil then
      with TfrmDataImportEdit.Create(self) do
      try
        objImport := p;
        ShowModal;
        lvObjects.Selected.ImageIndex := kiIMAGE[eioCreateObject in p.eOptions];
      finally
        release
      end;
  end;
end;

// TfrmDataExport
//   lvObjectsCompare
//
procedure TfrmDataImport.lvObjectsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := -1;
  if TcImport(Item1.Data).sName > TcImport(Item2.Data).sName then
    Compare := 1;
end;

// TfrmDataExport
//   ImportProcess
//
procedure TfrmDataImport.ImportProcess(Sender: TObject);

  // Tools
  //   SortByDependency
  //   > Dependency List sorting
  procedure SortByDependency(var parList: TcBag);
    //
    // IsOrdered
    function IsOrdered(obj: TcImport; var lst: TcBag): boolean;

      function localFind(list: TcBag; value: TcObject): longint;
      var
        i: longint;
        p: TcObject;
      begin
        result := kiUNDEFINED;
        for i := 0 to list.Count - 1 do
        begin
          p := list[i];
          if (p <> nil) and (p is TcImport) and ((p as TcImport).Data = value) then
          begin
            result := i;
            break;
          end;
        end;
      end;

    var
      i: longint;
      b: boolean;
      q: TcObject;
      p: TcData;
    begin
      result := (obj <> nil) and (obj.Data <> nil);
      if result then
      begin
        p := obj.Data;
        for i := 0 to p.FromDependency.Count - 1 do
        begin
          q := p.FromDependency[i].Ancestor(enObject);
          b := localFind(lst, q) <> kiUNDEFINED; // Has it been added already?
          if not b then
            b := localFind(parList, q) = kiUNDEFINED; // Is it actually part of the list?
          result := result and b;
          if not result then
            break;
        end;
      end;
    end;

  var
    lst: TcBag;
    p: TcObject;
    i, L: longint;
  begin
    lst := nil;
    try
      lst := TcBag.Create(nil);
      while TRUE do
      begin
        // Remember how many elements were there
        L := parList.Count;
        // Pass 1 - Add all possible elements
        for i := 0 to parList.Count - 1 do
        begin
          p := parList[i];
          if (p <> nil) and (p is TcImport) and IsOrdered(p as TcImport, lst) then
          begin
            lst.Add(p);
            parList[i] := nil;
          end;
        end;
        // Pass 2 - Get rid of all nil entries
        for i := parList.Count - 1 downto 0 do
          if parList[i] = nil then
            parList.Delete(i);
        // Compare progress
        if L = parList.Count then // i.e. None.
        begin
          for i := 0 to parList.Count - 1 do
            lst.Add(parList[i]);
          break;
        end;
      end;
      // Finally, transfer temporary list into result
      parList.Clear;
      for i := 0 to lst.Count - 1 do
        parList.Add(lst[i]);
    finally
      lst.free;
    end;
  end;

  // Tool
  //   Remove Comment lines from SQL statement
  function RemoveComments(value: String): String;
  var
    lst: TStringList;
    i: longint;
  begin
    lst := nil;
    try
      lst := TStringList.Create;
      lst.Text := trim(value);
      for i := lst.count - 1 downto 0 do
        if (system.copy(trim(lst[i]), 1, 2) = '--') or (system.copy(trim(lst[i]), 1, 2) = '\\') then
          lst.Delete(i);
      result := lst.Text;
    finally
      lst.Free;
    end;
  end;

  // Tool
  //   add content of message window
  procedure Log(strm: TMemoryStream; value: String);
  begin
    if value <> ksEMPTY then
    begin
      value := TextToText(value + '\n');
      strm.Write(value[1], length(value));
    end;
  end;

  // Tool
  //   Get next statement from sql statement
  function GetStatement(parStream: TcTokenStream): String;
  var
    i, L, M: longint;
  begin
    result := ksEMPTY;
    with parStream do
    begin
      L := iTokenStart;
      while (Token.ID <> _SEMICOLON) and not EOS do
        Match(_WILDCARD);
      M := iTokenStart;
      if not EOS and (Token.ID = _SEMICOLON) then
        Match(_SEMICOLON); // _SEMICOLON
      for i := L to M - 1 do
        result := result + PChar(parStream.Memory)[i];
    end;
    result := RemoveComments(result);
  end;

type
  TeObjectLevel = (eolOK, eolLevel1, eolLevel2);
const
  kiaOBJECTLEVEL: array[TeObjectLevel] of longint = (kiUNDEFINED, 1, 2);
var
  i, L, k: longint;
  p: TcImport;
  s: String;
  cmd: TcExecute;
  parStream: TcTokenStream;
  bag: TcBag;
  RootStorage, objStorage: IStorage;
  Stream: IStream;
  b: boolean;
  ir: TcImportResult;
  strm: TcDataStream;
  li: TListItem;
  e: TeObjectLevel;
  strmLog: TMemoryStream;
begin
  //
  // 0. Sort objects
  bag := nil;
  strmLog := nil;
  if CanImport and
     SUCCEEDED(StgOpenStorage(PWideChar(WideString(edtFileName.Text)), nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, RootStorage)) then
  try
    screen.Cursor := crHourGlass;
    tbsFileName.TabVisible := FALSE;
    tbsObjects.TabVisible := FALSE;
    tbsRename.TabVisible := FALSE;
    PageControl.ActivePage := tbsImport;
    btnNext.Visible := FALSE;
    btnPrevious.Visible := FALSE;
    lvImported.Items.Clear;
    strmLog := TMemoryStream.Create;

    //
    // 1. Dependency sorting
    bag := TcBag.Create(nil);
    for i := 0 to m_lstImport.Count - 1 do
      bag.add(m_lstImport[i]);
    SortByDependency(bag);

    //
    // 2. Compute # of actions + rows
    L := 0;
    for i := 0 to bag.count - 1 do
    begin
      p := bag[i] as TcImport;
      if eioSelected in p.eOptions then
        L := L + p.iRowCount + 1;
    end;
    barProgress.Min := 0;
    barProgress.Max := L;
    barProgress.Position := 0;
    b := TRUE;
    parStream := nil;
    try
      parStream := TcTokenStream.Create;
      cmd := nil;
      try
        cmd := TcExecute.Create(nil);
        cmd.Connection := m_objConnection;
        cmd.hdlResult := ResultReport;

        if btnCreateObjects.Checked then
        begin
          //
          // 3. Drop & Recreate, or Create objects
          for i := 0 to bag.count - 1 do
          begin
            p := bag[i] as TcImport;
            lblObjectName.Caption := p.sName;
            Application.ProcessMessages;
            //
            // Create object?
            //
            //   3.a. Delete Objects?
            if (eioSelected in p.eOptions) and (eioExists in p.eOptions) and (eioCreateObject in p.eOptions) then
            begin
              s := ksEMPTY;
              if p.Data <> nil then
                s := RemoveComments(TextToText(p.Data.Statement([enScript], krsDROP)));
              if s <> ksEMPTY then
              begin
                parStream.AsValue := s;
                repeat
                  try
                    s := trim(GetStatement(parStream));
                    if s <> ksEMPTY then
                    try
                      cmd.Execute(s); // Parameters are also set here!
                      Log(strmLog, Format('Dropped object %s', [p.sName]));
                    except
                      on E: Exception do
                        Log(strmLog, TextToText(Format('Drop SQL statement for object %s failed\n%s\n%s', [p.sName, cmd.Error, s])));
                    end;
                  except
                    on E:Exception do
                      Log(strmLog, E.Message);
                  end;
                until parStream.EOS or (s = ksEMPTY);
              end;
            end;
            //
            //   3.b. Create Objects?
            if (eioSelected in p.eOptions) and (eioCreateObject in p.eOptions) then
            begin // m_sCreate
              s := RemoveComments(p.m_sCreate);
              if s <> ksEMPTY then
              begin
                parStream.AsValue := s;
                repeat
                  try
                    s := trim(GetStatement(parStream));
                    if s <> ksEMPTY then
                    try
                      for k := 0 to ItemCount(s, kcPARAGRAPH) - 1 do
                        if Item(s, kcPARAGRAPH, k) <> ksEMPTy then
                          cmd.Execute(Item(s, kcPARAGRAPH, k));
                      Log(strmLog, Format('Created object %s', [p.sName]));
                    except
                      on E: Exception do
                        Log(strmLog, TextToText(Format('Create SQL statement for object %s failed\n%s\n%s', [p.sName, cmd.Error, s])));
                    end;
                  except
                    on E:Exception do
                      Log(strmLog, E.Message);
                  end;
                until parStream.EOS or (s = ksEMPTY);
              end;
            end;
            barProgress.StepIt;
          end;
        end;

        //
        // 4. Pre-Import operations
        if m_objMetaData.ProcessSQL(krsIE_PREIMPORT, m_objFormData) then
          Log(strmLog, 'Performed Successfuly Pre-Import Operations')
        else
          Log(strmLog, 'Failed Pre-Import Operations: ' + m_objMetaData.Error[elSQL]);
        //
        // 5. Import data
        for i := 0 to bag.count - 1 do
        begin
          p := bag[i] as TcImport;
          if (eioHasContent in p.eOptions) and
             (eioSelected in p.eOptions) and
             SUCCEEDED(RootStorage.OpenStorage(PWideChar(WideString(p.sName)), nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, objStorage)) then
          begin
            lblObjectName.Caption := p.sName;
            e := eolOK;
            li := lvImported.Items.Add;
            li.Caption := ksEMPTY;
            li.SubItems.Add(p.sName);
            li.SubItems.Add(inttostr(p.iRowCount));
            li.SubItems.Add(ksEMPTY);
            li.ImageIndex := kiUNDEFINED;
            li.MakeVisible(TRUE);
            lvImported.Selected := li;
            Application.ProcessMessages;
            // Pre-Table-Import operations
            if (p.Data <> nil) and (not p.Data.ProcessSQL([enSQL], krsIE_PRETABLEIMPORT)) then
            begin
              Log(strmLog, Format('%s: %s', [p.sName, p.Data.MetaData.Error[elSQL]]));
              inc(e);
            end;
            // Data Import
            strm := nil;
            if SUCCEEDED(objStorage.OpenStream(krsIE_DATA, nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, Stream)) then
            try
              strm := TcDataStream.Create(Stream);
              cmd.CommitPoint := strtointdef(edtCommitPoint.text, kiUNDEFINED);
              ir := cmd.Import(p.sName, strm, strmLog, m_objMetaData.Option[krsOPTION_TABLEDELIMITER], m_objMetaData.Option[krsOPTION_COLUMNDELIMITER]);
              if ir.bSuccess then
                Log(strmLog, Format('%s: Imported successfuly %d records', [p.sName, ir.iRecImported]))
              else
              begin
                Log(strmLog, Format('%s: Imported %d out of %d records', [p.sName, ir.iRecImported, ir.iRecTotal]));
                inc(e);
              end;
              li.SubItems[2] := inttostr(ir.iRecImported);
              barProgress.StepBy(ir.iRecTotal);
              b := b and ir.bSuccess;
            finally
              strm.Free;
            end;
            objStorage := nil;
            // Post-Table-Import operations
            if (p.Data <> nil) and (not p.Data.ProcessSQL([enSQL], krsIE_POSTTABLEIMPORT)) then
              Log(strmLog, Format('%s: %s', [p.sName, p.Data.MetaData.Error[elSQL]]));
            li.ImageIndex := kiaOBJECTLEVEL[e];
          end;
          Application.ProcessMessages;
        end;
        //
        // 6. Post-Import operations
        if m_objMetaData.ProcessSQL(krsIE_POSTIMPORT, m_objFormData) then
          Log(strmLog, 'Performed Successfuly Post-Import Operations')
        else
          Log(strmLog, 'Failed Post-Import Operations: ' + m_objMetaData.Error[elSQL]);

      finally
        cmd.free;
      end;
    finally
      parStream.free;
    end;
    lblObjectName.Caption := ksEMPTY;
    if b then
      Log(strmLog, 'Import was Successful.')
    else
      Log(strmLog, 'Import presented warning and/or errors. Check log file for details.');
    //
    // Create Log file
    strmLog.SaveToFile(m_sLogFile);
  finally
    bag.free;
    strmLog.free;
    screen.Cursor := crDefault;
  end;
  //
  // Finalize Import
  btnCancel.Caption := '&Close';
  btnCancel.ModalResult := mrOK;
end;

// TfrmDataExport
//   ResultReport
//
procedure TfrmDataImport.ResultReport(Value: longint);
begin
  if (lvImported.Selected <> nil) and (lvImported.Selected.SubItems.Count >= 3) then
  begin
    lvImported.Selected.SubItems[2] := inttostr(value);
    Application.ProcessMessages;
  end;
end;

// TfrmDataExport
//   CanImport
//
function TfrmDataImport.CanImport: boolean;
begin
  result := TRUE;
end;

// TfrmDataExport
//   edtCommitAmountKeyPress
//
procedure TfrmDataImport.edtCommitPointKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(key, ['0' .. '9', #8]) then
    key := #0;
end;

// TfrmDataExport
//   SetMetaData
//
procedure TfrmDataImport.SetMetaData(value: TcMetaData);
begin
  m_objMetaData.Clear;
  m_objMetaData.Copy(value);
end;

// TfrmDataImport
//   SetHeader
//
procedure TfrmDataImport.SetHeader(Index: longint);
const
  kasPAGECAPTION: array[0 .. 3] of String =
    ('Select a file and import process parameters.',
     'Select one or more import file objects to be imported.',
     'Verify and edit database creation scripts for objects that are about to be created.',
     'Export file content is about to be imported into the database. Follow the import progress as content is read export file and inserted into the database.');
  kasNEXTCAPTION: array[0 .. 3] of String =
    ('&Next',
     '&Next',
     '&Next',
     '&Import');
var
  b: boolean;
  L: longint;
begin
  b := TRUE;
  if Index = 3 then
  begin
    Application.ProcessMessages;
    L := Application.MessageBox('Proceed to import?', krsINFORMATION, MB_YESNOCANCEL + MB_ICONINFORMATION);
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
    btnNext.Visible := Index <> 3;
    btnPrevious.Visible := Index <> 3;
    SetState(nil);
    if Index = 3 then
      ImportProcess(nil);
  end;
end;

// TfrmDataImport
//   btnFileNameClick
//
procedure TfrmDataImport.btnFileNameClick(Sender: TObject);
begin
  dlgOpen.FileName := edtFileName.Text;
  if dlgOpen.Execute and (dlgOpen.FileName <> ksEMPTY) and ReadImportFile(dlgOpen.FileName) then
    edtFileName.Text := dlgOpen.FileName
  else
    edtFileName.Text := ksEMPTY;
  SetState(Sender);
end;

end.

