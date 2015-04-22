unit frmData2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, StdCtrls,
  PreferenceLib, ConnectionLib, DataLib, GraphPanel, ToolWin, ImgList, ADODB_TLB, AppEvnts,
  daObjectLib,
  FormLib,
  daGlobals,
  StdActns, ActnList, FavoriteLib,
  OleCtrls, SHDocVw, dbListView, ExecuteLib,
  dfsSplitter,
  IEDocHostUIHandler,
  SynEdit,
  Controls, HTMLColorLib;

type
  TeAlertTextType = (eattAlert, eattSQL, eattBoth);

  TfrmData2 = class(TcForm)
    tvObjects: TTreeView;
    spltrAnalysis: TdfsSplitter;
    popObjectScript: TPopupMenu;
    DataMainMenu: TMainMenu;
    mnuAction: TMenuItem;
    mnuObjects: TMenuItem;
    mnuObjectsScript: TMenuItem;
    popObjects: TPopupMenu;
    mnuEdit: TMenuItem;
    mnuEditRefresh: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditCut: TMenuItem;
    popObjectsRefresh: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuObjectsContentDisplay: TMenuItem;
    popObjectsContentExport: TMenuItem;
    popObjectsContentImport: TMenuItem;
    N2: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuObjectsContentDisplaySQLConsole: TMenuItem;
    mnuObjectsContentDisplaySpreadSheet: TMenuItem;
    popObjectsContentDisplay: TMenuItem;
    popObjectsContentDisplaySQLConsole: TMenuItem;
    popObjectsContentDisplayGrid: TMenuItem;
    ToolBar: TToolBar;
    btnExpand: TToolButton;
    btnCollapse: TToolButton;
    btnToolbarSep2: TToolButton;
    btnViewContent: TToolButton;
    popViewContent: TPopupMenu;
    popViewContentSQL: TMenuItem;
    popViewContentGrid: TMenuItem;
    btnRefresh: TToolButton;
    btnToolbarSep1: TToolButton;
    btnTranslate: TToolButton;
    btnToolbarSep3: TToolButton;
    ActionList: TActionList;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditRefresh: TAction;
    actToolsExport: TAction;
    actToolsImport: TAction;
    actStructureTranslate: TAction;
    actObjectsScript: TAction;
    actObjectsDisplay: TAction;
    actObjectsDisplayQuery: TAction;
    actObjectsDisplayGrid: TAction;
    actAction: TAction;
    actExpand: TAction;
    actCollapse: TAction;
    actGraphContinueDraw: TAction;
    actGraph2D: TAction;
    actGraph3D: TAction;
    mnuObjectsExpand: TMenuItem;
    mnuObjectsCollapse: TMenuItem;
    N6: TMenuItem;
    actGraph: TAction;
    btnExport: TToolButton;
    btnImport: TToolButton;
    mnuFavorites: TMenuItem;
    mnuFavoritesHidden: TMenuItem;
    btnExecute: TToolButton;
    actEditExecute: TAction;
    mnuEditExecute: TMenuItem;
    actEditFind: TAction;
    actEditFindNext: TAction;
    N8: TMenuItem;
    N4: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditFindNext: TMenuItem;
    dlgFind: TFindDialog;
    btnFind: TToolButton;
    btnToolbarSep4: TToolButton;
    btnFindNext: TToolButton;
    actObjectEdit: TAction;
    N9: TMenuItem;
    mnuObjectsEditObject: TMenuItem;
    N11: TMenuItem;
    popObjectsEditObject: TMenuItem;
    btnObjectEdit: TToolButton;
    btnObjectNew: TToolButton;
    actObjectCreate: TAction;
    mnuObjectCreateObject: TMenuItem;
    btnToolbarSep6: TToolButton;
    popObjectsCreateObject: TMenuItem;
    popObjectsCreate: TPopupMenu;
    btnObjectDelete: TToolButton;
    actObjectDrop: TAction;
    mnuObjectsDeleteObject: TMenuItem;
    popObjectsDeleteObject: TMenuItem;
    actObjectOptions: TAction;
    mnuObjectsOptions: TMenuItem;
    popObjectsOptions: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    btnToolbarSep5: TToolButton;
    pnlLeft: TPanel;
    actEditExecuteAll: TAction;
    FormEvents: TApplicationEvents;
    timerGraph: TTimer;
    imgObjects: TImageList;
    imgObjectsMasks: TImageList;
    mnuTools: TMenuItem;
    lstSmallImages: TImageList;
    PageControl: TPageControl;
    tsScripts: TTabSheet;
    wbOutput: TWebBrowser;
    tsData: TTabSheet;
    dbListView: TDBListView;
    lblDataObject: TStaticText;
    tsGraph: TTabSheet;
    pnlGraph: TGraphPanel;
    btnDumpData: TToolButton;
    actToolsExportText: TAction;
    DumpData1: TMenuItem;
    popObjectContentExportText: TMenuItem;
    actStructureCompare: TAction;
    mnuStructureTranslate: TMenuItem;
    Export1: TMenuItem;
    Import1: TMenuItem;
    Content1: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    mnuStructureCompareCompare: TMenuItem;
    actStructureCreateSnapshot: TAction;
    mnuStructureCompareCreateSnapshot: TMenuItem;
    pnlTop: TPanel;
    spltBottom: TdfsSplitter;
    PageControlBottom: TPageControl;
    tsAlert: TTabSheet;
    lvAlerts: TListView;
    tsErrors: TTabSheet;
    lvErrors: TListView;
    tsGridErrors: TTabSheet;
    lstDataErrors: TListBox;
    ToolButton1: TToolButton;
    btnHelpMaster: TToolButton;
    actHelpMaster: TAction;

    procedure wbOutputNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure OnDisplayFieldExit(Sender: TObject);
    procedure PageControlBottomResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure onPreparePopup(Sender: TObject);
    procedure popObjectScriptClick(Sender: TObject);
    procedure FormActivate(Sender: TObject); override;
    procedure FormDeactivate(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
    procedure onScriptMenuClick(Sender: TObject);
    procedure onDisplayContent(Sender: TObject);
    procedure tvObjectsExpand(Sender: TObject; Node: TTreeNode);
    procedure SetState(Sender: TObject);
    procedure ClipboardAction(Sender: TObject);
    procedure mnuFavoritesClick(Sender: TObject);
    procedure onPreferenceChange(Sender: TObject); override;
    procedure tvObjectsDeletion(Sender: TObject; Node: TTreeNode);
    procedure actEditFindExecute(Sender: TObject);
    procedure actObjectEditExecute(Sender: TObject);
    procedure actObjectCreateExecute(Sender: TObject);
    procedure FormEventsShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure SetSchemaBitmaps;
    procedure GetAlerts(value: TcData);
    procedure DisplayAlerts;
    function  GetAlertText(parObject: TcObject; eparType: TeAlertTextType): String;
    function  GetAlertItem(Sender: TObject): TListItem;
    procedure onListViewMessage(Sender: TObject; value: String);

    procedure onRefresh(Sender: TObject);
    procedure onSave(Sender: TObject);
    procedure onClose(Sender: TObject);
    procedure onExpand(Sender: TObject);
    procedure onCollapse(Sender: TObject);
    procedure onNull(Sender: TObject);
    procedure onExecute(Sender: TObject);
    procedure onSelMove(Sender: TObject);
    procedure onFindNext(Sender: TObject);
    procedure onDropObject(Sender: TObject);
    procedure onDisplayOptions(Sender: TObject);
    procedure tvObjectsExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure OnDisplayStructure(Sender: TObject);
    procedure tvObjectsChange(Sender: TObject; Node: TTreeNode);
    procedure tvObjectsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure OnDisplayContent2(Sender: TObject);
    procedure OnDisplay(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tsScriptsResize(Sender: TObject);
    procedure OnGraphShow(Sender: TObject);
    procedure pnlGraphSelect(const sType, sName: string);
    procedure onTranslate(Sender: TObject);
    procedure onExportContent(Sender: TObject);
    procedure onImportContent(Sender: TObject);
    procedure tvObjectsCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure wbOutputBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure lvAlertsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvAlertsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure onDataDump(Sender: TObject);
    procedure onSnapShotCompare(Sender: TObject);
    procedure DoRefresh(Sender: TObject);
    function  GetBrowserSelection(value: TWebBrowser): String;
    procedure onMasterHelp(Sender: TObject);
    procedure popObjectScriptPopup(Sender: TObject);

  private
    // Private Members
    //
    m_objData: TcData;
    m_objMetaData: TcMetaData;
    m_objExecute: TcExecute;
    m_pScriptNode, m_pDataNode: TTreeNode;
    m_objUserData: TcData;
    m_iColumnToSort: longint;
    m_bColumnOrder: boolean;
    m_iDisplayLock: longint;
    m_FDocHostUIHandler: TDocHostUIHandler;
    m_objColorParser: TcHTMLColorParser;

  protected
    // Protected Declarations
    //
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // Private Declarations
    //
    procedure   SetMenus(Sender: TObject);
    procedure   OnCreateObjectSelection(Sender: TObject);
    procedure   mnuObjectCreateObjectSelection(Sender: TObject);
    procedure   onObjectTypeCreation(sender: TObject);
    function    GetScriptIndex: String;
    procedure   DisplayResult(value: string);
    function    CheckLoaded(parObject: TcObject; bparAll: boolean): boolean; overload;
    function    CheckLoaded(parData: TcData; parMetaData: TcMetaData; bparAll: boolean): boolean; overload;
    procedure   SetAnimation(value: boolean);
    procedure   pnlGraphChangeLocation(const ID: longint; Value: TdPoint);
    function    GetRootUser: TcObject; overload;
    function    GetRootUser(value: TTreeNode): TcObject; overload;
    function    PushCursor(value: TCursor): longint;
    function    PopCursor: longint;
    function    IsBusy: boolean;
    procedure   SetSchemaOutput(value: TcObject; parIsSummary: boolean);
    procedure   SetSemaphore(value: String; bparIncrement: longint);
    function    GetSemaphore(value: String): longint;
    procedure   CheckDisplay(value: TcData);
    procedure   DisplayErrors;
    procedure   DisplayHeader(value: TTreeNode);
    procedure   tvObjects_Lock(value: longint);
    procedure   DisplayDataErrors(Sender: TObject; value: String);
    function    IsLicensed: boolean;

  public
    // Public Declarations
    //
    procedure   Display(Sender: TObject);
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; override;
    function    Finalize: boolean; override;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; overload; override;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; overload; override;
    function    GetObjectItem(Sender: TObject): TTreeNode;

  public
    //
    // Public properties
    property    Semaphore[value: String]: longint       read GetSemaphore       write SetSemaphore;
  end;

implementation

uses
  Math,
  Variants,
  ClipBrd,
  ComObj,
  daResourceStrings,
  ProcessAlertLib,      // TProcessAlerts
  daStreamLib,
  frmEditor,            // TfrmEditor
  frmDataPreferences,   // TfrmDataPreferences
  frmDataSnapshot,      // TfrmDataSnapshot
  frmDataSnapshotDifference, // TTfrmDataSnapshotDifference
  frmDataTranslate,     // TfrmDataTranslate
  frmDataExport,        // TfrmDataExport
  frmDataImport,        // TfrmDataImport
  strUtils,
  StatementLib,
  MSHTML,
  ActiveX,
  IEConst;

{$R *.DFM}
{$R extResources.res}

const
  kiSETLOCK   = +1;
  kiUNSETLOCK = -1;

// TfrmData2
//   FormCreate
//
procedure TfrmData2.FormCreate(Sender: TObject);
begin
  m_objData := TcData.Create(nil);
  m_objMetaData := TcMetaData.Create(nil);
  Toolbar.Parent := ControlBar;
  Toolbar.Top := 0;
  m_objExecute := TcExecute.Create(nil);
  m_objExecute.onSetButton := nil;
  PageControl.ActivePageIndex := 0;
  m_pScriptNode := nil;
  m_pDataNode := nil;
  m_objUserData := nil;
  SetSchemaBitmaps;
  pnlGraph.OnChangeLocation := pnlGraphChangeLocation;
  CursorStack.RegisterComponent(wbOutput);
  CursorStack.RegisterComponent(dbListView);
  pnlGraph.OnSelect := pnlGraphSelect;
  tsErrors.TabVisible := FALSE;
  m_iDisplayLock := 0;
  if GetWinVersion >= wvWinXP then
  begin
    tvObjects.DoubleBuffered := TRUE;
    dbListView.DoubleBuffered := TRUE;
  end;
  m_FDocHostUIHandler := TDocHostUIHandler.Create;
  m_FDocHostUIHandler.WebBrowser := wbOutput;
  dbListView.OnDisplayErrors := DisplayDataErrors;
  // Colorizer
  m_objColorParser := TcHTMLColorParser.Create(nil);
end;

// TfrmData2
//   FormDestroy
//
procedure TfrmData2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(m_objExecute);
  m_objData.free;
  m_objMetaData.Free;
  m_objData := nil;
  pnlGraph.OnChangeLocation := nil;
  m_FDocHostUIHandler.WebBrowser := nil;
  FreeAndNil(m_FDocHostUIHandler);
  FreeAndNil(m_objColorParser);
end;

// TfrmData2
//   FormShow
//
procedure TfrmData2.FormShow(Sender: TObject);
begin
  SetToolbarLocation(ControlBar, Toolbar);
  onDisplayContent(popViewContentSQL);
  actObjectsDisplay.OnExecute := onDisplayContent;
end;

// TfrmData2
//   FormCloseQuery
//
procedure TfrmData2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not IsBusy and not CursorStack.HasSemaphore and (m_objExecute.State = eesIdle);
  if CanClose and formset.Connection.Connected and FormSet.Support[efsTransaction] and inherited CheckConnection(TRUE) and FormSet.Connection.InTransaction then
    with FormSet.Connection do
      case Application.MessageBox('Press "Yes" to commit pending transactions, "No" to rollback pending transactions, "Cancel" otherwise.', krsINFORMATION, MB_YESNOCANCEL + MB_ICONEXCLAMATION) of
        idYES:
          CommitTrans;
        idNO:
          RollbackTrans;
        idCancel:
          CanClose := FALSE;
      end;
end;

// TfrmData2
//   Initialize
//
function TfrmData2.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;

  procedure PopulatePopupObjectCreate(parMetaData: TcMetaData; parMenu: TComponent; parEvent: TNotifyEvent; parCheck: boolean);

    procedure SubPopulate(parMetaData: TcMetaData);
    var
      i: longint;
      p: TMenuItem;
    begin
      if (parMetaData.eType = enEDITOR) then
      begin
        p := TMenuItem.Create(parMenu);
        if parMenu is TMenuItem then
          (parMenu as TMenuItem).Add(p)
        else if parMenu is TPopupMenu then
          (parMenu as TPopupMenu).Items.Add(p);
        p.Caption := parMetaData.DisplayName;
        p.Tag := longint(parMetaData);
        p.RadioItem := parCheck;
        p.OnClick := parEvent;
        if ((parMenu is TMenuItem) and ((parMenu as TMenuItem).Count = 1)) or
           ((parMenu is TPopupMenu) and ((parMenu as TPopupMenu).Items.Count = 1)) then
          OnCreateObjectSelection(p);
      end
      else for i := 0 to parMetaData.Count - 1 do
        if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) then
          SubPopulate(parMetaData[i] as TcMetaData);
    end;

  begin
    if parMenu is TMenuItem then
      (parMenu as TMenuItem).Clear;
    SubPopulate(parMetaData);
  end;

{$IFDEF SBS_DEBUG}
var
  d: TDateTime;
{$ENDIF}
begin
  inherited Initialize(eparMode, eparParameter);
  result := FALSE;
  try
    Application.ProcessMessages;
    PushCursor(crAppStart);
    m_objMetaData.Clear;
    m_objMetaData.Copy(FormSet.MetaData);
    m_objMetaData.Preferences := FormSet.MetaData.Preferences;
    m_objColorParser.MetaData := FormSet.MetaData;
    m_objColorParser.Preferences := FormSet.Preferences;
    case eparMode of
      //
      //   Load Schema from database
      efiFromDatabase:
      try
        m_objData.MetaData := m_objMetaData;
        if CheckConnection(TRUE) then
        begin
          Application.ProcessMessages;
          if not m_objMetaData.IsEmpty then
          begin
            try
              m_objData.Clear;
              tvObjects_Lock(kiSETLOCK);
              tvObjects.Items.Clear;
              m_objData.procDisplay := Display;
              result := m_objData.Load(m_objMetaData, [elfAll]);
              GetAlerts(m_objData);
              tvObjects_Lock(kiUNSETLOCK);
            except
              on E:Exception do
                m_objMetaData.SetError(elFatal, E.Message, ksEMPTY);
            end;
            SetMenus(self);
            m_objMetaData.Properties[krsORIGIN] := FormSet.Connection.ConnectionString;
          end;
          m_objExecute.Connection := Formset.Connection;
          dbListView.Connection := Formset.Connection;
          dbListView.OnDisplayMessage := onListViewMessage;
        end;
        // Reset modified State
        m_objData.State := edsDB;
      except
        //
      end;
      //
      //   Translation
      //
      efiFromTranslation:
      begin
        Application.ProcessMessages;
        if not m_objMetaData.IsEmpty and (FormSet.objDataAncestor <> nil) and (FormSet.objDataAncestor is TcData) then
        begin
          StatusText([psPanel2], Format('Started Translation of ''%s''...', [Header]), esiNone);
          try
            tvObjects.Items.Clear;
            tvObjects_Lock(kiSETLOCK);
            m_objData.procDisplay := Display;
            m_objData.MetaData := m_objMetaData;
            m_objData.Translate(FormSet.objDataAncestor as TcData);
{$IFDEF SBS_DEBUG}
            d := now;
{$ENDIF}
            m_objData.Display;
{$IFDEF SBS_DEBUG}
            FormSet.SetLog('Display Data Tree', ksEMPTY, now - d, []);
{$ENDIF}
            tvObjects_Lock(kiUNSETLOCK);
            result := TRUE;
          except
            on E:Exception do
              m_objMetaData.SetError(elFatal, E.Message, ksEMPTY);
          end;
          SetMenus(self);
          StatusText([psPanel2], Format('Completed Translation of ''%s''', [Header]), esiNone);
        end;
      end;
    end;
    //
    FormSet.InitializeToolMenu(mnuTools);
    PopulatePopupObjectCreate(m_objMetaData, popObjectsCreate, OnCreateObjectSelection, TRUE);
    PopulatePopupObjectCreate(m_objMetaData, mnuObjectCreateObject, mnuObjectCreateObjectSelection, FALSE);
    // Root Online Help?
    actHelpMaster.Enabled := FormSet.HasMasterOnlineHelp;
    //
    DisplayErrors;
    OnDisplay(nil);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   Finalize
//
function TfrmData2.Finalize: boolean;
begin
  result := not IsBusy and not CursorStack.HasSemaphore;
  if result then
  try
    FormDeactivate(nil);
    Semaphore[krsALERT] := +1; // Disable alert display.
    PushCursor(crHourGlass);
    m_objExecute.Connection := nil;
    dbListView.Connection := nil;
    if m_objMetaData <> nil then
      m_objMetaData.ClearDisplayRefefence;
    tvObjects.Items.Clear;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   FormActivate
//
procedure TfrmData2.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  SetMainMenu(mmNew,          FALSE, TNotifyEvent(nil));
  SetMainMenu(mmSave,         TRUE,  onSave);
  SetMainMenu(mmSaveAs,       TRUE,  onSave);
  SetMainMenu(mmClose,        TRUE,  onClose);
  SetMainMenu(mmReOpen,       FALSE, FALSE, TNotifyEvent(nil));
  SetState(Sender);
  onSelMove(Sender);
  ActionList.State := asNormal;
end;

// TfrmData2
//   FormDeactivate
//
procedure TfrmData2.FormDeactivate(Sender: TObject);
begin
  inherited FormDeactivate(nil);
  ActionList.State := asSuspended;
  SetMainMenu(mmNew,          FALSE, TNotifyEvent(nil));
  SetMainMenu(mmSave,         FALSE, TNotifyEvent(nil));
  SetMainMenu(mmSaveAs,       FALSE, TNotifyEvent(nil));
  SetMainMenu(mmClose,        FALSE, TNotifyEvent(nil));
  SetMainMenu(mmReOpen,       FALSE, FALSE, TNotifyEvent(nil));
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmData2
//   tsScriptsResize
//
procedure TfrmData2.tsScriptsResize(Sender: TObject);
begin
  //
end;

// TfrmData2
//   DisplayHeader
//
procedure TfrmData2.DisplayHeader(value: TTreeNode);
var
  p: TcObject;
begin
  if (value <> nil) and (value.data <> nil) then
  begin
    p := TcObject(value.data);
    if (p <> nil) and (p is TcMetaData) then
      value.Text := Format('%s (%d)', [(p as TcMetaData).DisplayName, (p as TcMetaData).CountData(GetRootUser(value))]);
  end;
end;

// TfrmData2
//   Display
//
procedure TfrmData2.Display(Sender: TObject);

  function GetNode(parData: TcData): TTreeNode;
  var
    p, m: TTreeNode;
    i: longint;
    s: String;
  begin
    result := nil;
    if parData.MetaData.HasDisplayName then
    begin
      result := parData.objDisplay as TTreeNode;
      if result = nil then
      try
        // A. Get a meta data element to attach to.
        p := nil;
        if parData.Parent <> nil then
          p := GetNode(parData.Parent as TcData);
        // B. Create TreeNode for TcData
        m := nil;
        if p <> nil then
        begin
          for i := 0 to p.Count - 1 do
            if p.Item[i].Data = parData.MetaData then
            begin
              m := p.Item[i];
              break;
            end;
          if m = nil then
          begin
            m := tvObjects.Items.AddChildObject(p, parData.MetaData.DisplayName, parData.MetaData);
            m.ImageIndex := longint(parData.MetaData.eIcon) - 1;
            m.SelectedIndex := m.ImageIndex;
          end;
          DisplayHeader(m);
        end;
        //
        s := parData.Header;
        if s = ksEMPTY then
          s := parData.MetaData.DisplayName;
        result := tvObjects.Items.AddChildObject(m, s, parData);
        result.HasChildren := parData.MetaData.CanHaveChildren;
        result.ImageIndex := longint(parData.Icon) - 1;
        result.SelectedIndex := result.ImageIndex;
        parData.objDisplay := result;
        if parData.MetaData.CountObjects([enSQL, enOpenSchema]) = 0 then
        begin
          CheckLoaded(parData, FALSE);
          result.Expand(FALSE);
        end;
      except
        on E:Exception do
          Application.MessageBox(PChar(E.Message), krsERROR, MB_OK + MB_ICONEXCLAMATION);
      end
    end;
  end;

begin
  try
    tvObjects_Lock(kiSETLOCK);
    if (Sender <> nil) and (Sender is TcData) and ((Sender as TcData) <> m_objData) then
      GetNode(Sender as TcData);
  finally
    tvObjects_Lock(kiUNSETLOCK);
  end;
end;

// TfrmData2
//   GetScriptIndex
//
function TfrmData2.GetScriptIndex: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to mnuObjectsScript.Count - 1 do
    if mnuObjectsScript[i].Checked then
    begin
      result := mnuObjectsScript[i].Caption;
      break;
    end;
  result := AnsiReplaceStr(result, '&', ksEMPTY);
  if result = ksEMPTY then
    result := krsCREATE;
end;

// TfrmData2
//   tvObjectsExpand
//
procedure TfrmData2.tvObjectsExpand(Sender: TObject; Node: TTreeNode);
begin
  // Rfresh Display if Meta data
  if (Node.Data <> nil) and (PageControl.ActivePage = tsScripts) then
    OnDisplay(Sender);
  // Reset States
  SetState(Sender);
end;

// TfrmData2
//   GetObjectItem
//
function TfrmData2.GetObjectItem(Sender: TObject): TTreeNode;
var
  i: longint;
begin
  result := nil;
  if Sender <> nil then
    for i := 0 to tvObjects.Items.Count - 1 do
      if tvObjects.Items[i].Data = Sender then
      begin
        result := tvObjects.Items[i];
        break;
      end;
  if (result <> nil) and (tvObjects.Selected <> result) then
  begin
    tvObjects.Selected := result;
    tvObjects.Selected.MakeVisible;
  end;
end;

// TfrmData2
//   SetMenus
//
procedure TfrmData2.SetMenus(Sender: TObject);

  function SetMenu(Value: String; Checked: boolean): TMenuItem;
  begin
    result := TMenuItem.Create(Self);
    result.Caption := Value;
    result.onClick := onScriptMenuClick;
    result.GroupIndex := 1;
    result.RadioItem := TRUE;
    result.Checked := Checked;
  end;

  function SetMenu2(Value: TAction): TMenuItem;
  begin
    result := TMenuItem.Create(Self);
    result.Action  := value;
  end;

var
  lst: TStringList;
  i: longint;
begin
  lst := nil;
  try
    lst := m_objMetaData.ScriptTypes;
    for i := 0 to lst.count - 1 do
    begin
      mnuObjectsScript.Add(SetMenu(lst[i], i = 0));
      popObjectScript.Items.Add(SetMenu(lst[i], i = 0));
    end;
    mnuObjectsScript.Add(SetMenu(krsINSTANCEXML, FALSE));
    popObjectScript.Items.Add(SetMenu(krsINSTANCEXML, FALSE));
    mnuObjectsScript.Add(SetMenu(krsMETAXML, FALSE));
    popObjectScript.Items.Add(SetMenu(krsMETAXML, FALSE));
    //
    popObjectScript.Items.Add(SetMenu('-', FALSE));
    popObjectScript.Items.Add(SetMenu2(actEditFind));
    popObjectScript.Items.Add(SetMenu2(actEditFindNext));
    popObjectScript.Items.Add(SetMenu('-', FALSE));
    popObjectScript.Items.Add(SetMenu2(actEditCopy));
    popObjectScript.Items.Add(SetMenu2(actEditSelectAll));
    popObjectScript.Items.Add(SetMenu2(actEditExecute));
    popObjectScript.Items.Add(SetMenu2(actToolsExportText));
    popObjectScript.Items.Add(SetMenu('-', FALSE));
    popObjectScript.Items.Add(SetMenu2(actObjectOptions));
    SetState(Sender);
  finally
    lst.free;
  end;
end;

// TfrmData2
//   onScriptMenuClick
//
procedure TfrmData2.onScriptMenuClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    mnuObjectsScript[(Sender as TMenuItem).MenuIndex].Checked := TRUE;
    popObjectScript.Items[(Sender as TMenuItem).MenuIndex].Checked := TRUE;
  end;
  m_pScriptNode := nil;
  OnDisplayStructure(Sender);
end;

//
// ACTION menu functions
//

// TfrmData2
//   onPreparePopup
//
procedure TfrmData2.onPreparePopup(Sender: TObject);

  procedure CleanMenu(objMenu: TMenuItem);
  var
    i: longint;
  begin
   for i := objMenu.Count - 1 downto 0 do
     if objMenu.Items[i].Tag <> 0 then
       objMenu.Delete(i);
  end;

  procedure CreateMenuItem(objMenu: TMenuItem; objMetaData: TcObject; bChecked, bEnabled: boolean);
  var
    m: TMenuItem;
  begin
    m := TMenuItem.Create(objMenu);
    objMenu.Insert(0, m);
    m.Caption := (objMetaData as TcMetaData).DisplayName;
    m.Tag := longint(pointer(objMetaData));
    m.onClick := popObjectScriptClick;
    m.Checked := bChecked;
    m.Enabled:= bEnabled;
  end;

var
  p: TcObject;
  i, j: longint;
  b, bEnabled: boolean;
  s: String;
begin
  // 1. Clean up popup menu and Action Menu
  CleanMenu(mnuAction);
  CleanMenu(popObjects.Items);
  // 2. Create menu entries
  if (tvObjects.Selected <> nil) and (tvObjects.Selected.data <> nil) then
  begin
    p := TcObject(tvObjects.Selected.data);
    if p is TcData then
    begin
      p := (p as TcData).MetaData;
      // Check for menu enabling
      bEnabled := CheckConnection(FALSE);
      // Find the Menu object
      if p is TcMetaData then
        for i := 0 to p.Count - 1 do
          if p[i].eType = enMenu then
            for j := p[i].Count - 1 downto 0 do
            begin
              b := FALSE;
              s := (p[i][j] as TcMetaData).Attribute[krsCHECKED];
              if s <> ksEMPTY then
                b := (p[i] as TcMetaData).Expression(s, TcData(tvObjects.Selected.data), p[i] as TcMetaData, ksEMPTY, []);
              CreateMenuItem(popObjects.Items, p[i][j], b, bEnabled);
              CreateMenuItem(mnuAction, p[i][j], b, bEnabled);
            end;
    end;
  end;
  // 3. Enabled / Disable Action menu
  SetState(Sender);
end;

// TfrmData2
//   popObjectScriptClick
//
procedure TfrmData2.popObjectScriptClick(Sender: TObject);
var
  p: TcObject;
  s, sError: String;
  i: longint;
  b: boolean;
begin
  if (Sender is TComponent) and ((Sender as TComponent).Tag <> 0) and (tvObjects.Selected <> nil) and (tvObjects.Selected.data<> nil) then
  try
    PushCursor(crAppStart);
    p := TcObject((Sender as TComponent).Tag);
    if (p <> nil) and (p is TcMetaData) then
    begin
      sError := ksEMPTY;
      b := TRUE;
      // Execute SQLs
      for i := 0 to p.count - 1 do
        if p[i].eType = enSQL then
        begin
          s := VarToStr((p[i] as TcMetaData).Expression(p[i].sValue, TcData(tvObjects.Selected.data), p[i] as TcMetaData, ksEMPTY, []));
          s := StripHTMLTags(s); // Strip extra content
          if Application.MessageBox(PChar(Format('Execute "%s"?', [s])), krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES then
          begin
            b := ExecuteItem(s, sError);
            if not b then
            begin
              Application.MessageBox(PChar(Format('"%s" execution failed. %s%s', [s, ksCR, sError])), 'Error', MB_OK + MB_ICONSTOP);
              break;
            end;
          end;
        end;
      if not b then
        Application.MessageBox('Not all statements were successfuly executed.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
      else
      begin
        // Display result
        s := ksEMPTY;
        for i := 0 to p.count - 1 do
          if p[i].eType = enDISPLAY then
            s := trim(s + ksCR + VarToStr((p[i] as TcMetaData).Expression(p[i].sValue, TcData(tvObjects.Selected.data), p[i] as TcMetaData, ksEMPTY, [])));
        if s = ksEMPTY then
          s := 'The Statement execution was successful. Note that the current state of the database may have been altered: please refresh your database display.';
        Application.MessageBox(PChar(s), krsINFORMATION, MB_OK + MB_ICONINFORMATION);
      end;
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onSave
//
procedure TfrmData2.onSave(Sender: TObject);
var
  f: TfrmDataSnapshot;
begin
  if IsLicensed and not IsBusy then
  try
    PushCursor(crAppStart);
    f := TfrmDataSnapshot.Create(self);
    f.objData := m_objData;
    f.objMetaData := m_objMetaData;
    f.Connection := FormSet.Connection;
    if GetRootUser <> nil then
      f.Schema := GetRootUser.sValue;
    f.ShowModal;
    CheckDisplay(m_objData);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onClose
//
procedure TfrmData2.onClose(Sender: TObject);
begin
  Close;
end;

// TfrmData2
//   DoRefresh
//
procedure TfrmData2.onRefresh(Sender: TObject);
begin
  if not IsBusy and (tvObjects.Selected <> nil) then
  try
    PushCursor(crAppStart);
    DoRefresh(Sender);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   DoRefresh
//
procedure TfrmData2.DoRefresh(Sender: TObject);

  // Tool
  //   DeletePreviousNodes
  //
  procedure DeletePreviousNodes(parData: TcData; parMetaData: TcMetaData);
  var
    i: longint;
    r: TcObject;
  begin
    if parMetaData.eType = enObject then
    begin
      // Delete previous nodes
      for i := parMetaData.lstIndex.Count - 1 downto 0 do
      begin
        r := TcObject(parMetaData.lstIndex.Objects[i]);
        if (r <> nil) and (r is TcData) and (r.Parent <> nil) and parData.Has(r) then
        begin
          // Delete TTreeNode
          if ((r as TcData).objDisplay <> nil) and ((r as TcData).objDisplay is TTreeNode) then
            ((r as TcData).objDisplay as TTreeNode).Delete;
          // Delete TcData
          r.Parent.Delete(r);
        end;
      end;
    end;
    // Explore down.
    for i := 0 to parMetaData.Count - 1 do
      if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) and (parMetaData[i].eType = enObject) then
        DeletePreviousNodes(parData, parMetaData[i] as TcMetaData);
  end;

var
  p, r: TcObject;
  b: boolean;
  i: longint;
begin
  if tvObjects.Selected <> nil then
  begin
    // Lock Display
    tvObjects_Lock(kiSETLOCK);
    b := tvObjects.Selected.Expanded;
    // Get current node
    p := TcObject(tvObjects.Selected.Data);
    // Is this a TcData -Type tree node?
    if (p <> nil) and (p is TcData) then
    begin
      r := GetRootUser;
      if (r <> nil) and (r is TcData) then
      try
        // Delete previous nodes
        for i := p.count - 1 downto 0 do
          if (p[i] <> nil) and (p[i] is TcData) and ((p[i] as TcData).MetaData <> nil) then
            DeletePreviousNodes(p[i] as TcData, (p[i] as TcData).MetaData);
        (p as TcData).ClearChildren;
        (p as TcData).Load((p as TcData).MetaData, [elfRefresh]);
        GetAlerts(p as TcData);
        CheckDisplay(p as TcData);
        OnDisplay(Sender);
      except
        //
      end
    end
    // OK. We got a meta node -type tree node. Let's proceed.
    else if (p <> nil) and (p is TcMetaData) then
    try
      // Get Root Node
      r := GetRootUser;
      if (r <> nil) and (r is TcData) then
      begin
        // Delete previous nodes
        DeletePreviousNodes(r as TcData, p as TcMetaData);
        // Rebuild List
        (r as TcData).Load(p as TcMetaData, [elfAll, elfRefresh, elfSkim]);
        GetAlerts(r as TcData);
        CheckDisplay(r as TcData);
        OnDisplay(Sender);
      end;
    except
      //
    end;
    m_pScriptNode := nil;
    // Unlock Display
    tvObjects.Selected.Expanded := b;
    tvObjects_Lock(kiUNSETLOCK);
  end;
end;

// TfrmData2
//   onExpand
//
procedure TfrmData2.onExpand(Sender: TObject);
begin
  if not IsBusy then
  try
    PushCursor(crAppStart);
    if (tvObjects.Selected <> nil) then
      tvObjects.Selected.Expand(TRUE);
    SetState(Sender);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onCollapse
//
procedure TfrmData2.onCollapse(Sender: TObject);
begin
  if not IsBusy then
  try
    PushCursor(crAppStart);
    if (tvObjects.Selected <> nil) then
      tvObjects.Selected.Collapse(TRUE);
    SetState(Sender);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   SetState
//
procedure TfrmData2.SetState(Sender: TObject);
var
  p: TTreeNode;
  b: boolean;
  d, e, q: TcObject;
begin
  // Expand & Collapse
  actExpand.Enabled := (tvObjects.Selected <> nil) and tvObjects.Selected.HasChildren and not tvObjects.Selected.Expanded;
  actCollapse.Enabled := (tvObjects.Selected <> nil) and tvObjects.Selected.HasChildren and tvObjects.Selected.Expanded;
  // 'Content' menu items.
  p := tvObjects.Selected;
  q := nil;
  if p <> nil then
    q := p.Data;
  b := (q <> nil) and
       (
        ((q is TcData) and ((q as TcData).MetaData <> nil) and ((q as TcData).MetaData.Find(enSQL, krsCONTENT) <> nil)) or
        ((q is TcMetaData) and ((q as TcMetaData).Find(enSQL, krsCONTENT) <> nil))
       );
  actObjectsDisplay.Enabled := b;
  actObjectsDisplayGrid.Enabled := b;
  actObjectsDisplayQuery.Enabled := b;
  actToolsExportText.Enabled := b;
  // Import / Export
  d := GetRootUser;
  b := (d <> nil) and (d is TcData);
  actToolsExport.Enabled := b;
  actToolsImport.Enabled := b;
  // Structure
  actStructureCompare.Enabled := b;
  actStructureCreateSnapshot.Enabled := b;
  actStructureTranslate.Enabled := b;
  // Refresh
  actEditRefresh.Enabled := (p <> nil);
  // Clipboard
  actEditCut.Enabled := (ActiveControl <> nil) and (ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).SelText <> ksEMPTY);
  actEditCopy.Enabled := (ActiveControl <> nil) and (((ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).SelText <> ksEMPTY)) or (ActiveControl is TWebBrowser));
  actEditPaste.Enabled := (ActiveControl <> nil) and (ActiveControl is TSynEdit) and (ClipBoard.AsText <> ksEMPTY);
  actEditUndo.Enabled := (ActiveControl <> nil) and (ActiveControl is TSynEdit) and (ActiveControl as TSynEdit).CanUndo;
  actEditSelectAll.Enabled := (ActiveControl <> nil) and (ActiveControl is TSynEdit);
  actEditExecute.Enabled := (PageControl.ActivePage = tsScripts) and (GetBrowserSelection(wbOutput) <> ksEMPTY);
  actEditFind.Enabled := (ActiveControl <> nil) and (ActiveControl is TSynEdit);
  actEditFindNext.Enabled := actEditFind.Enabled and (dlgFind.FindText <> ksEMPTY);
  // Script
  actObjectsScript.Enabled := mnuObjectsScript.Count > 0;
  // Actions
  mnuAction.Enabled := mnuAction.Count > 0;
  // Edit
  d := nil;
  e := nil;
  if (tvObjects.Selected <> nil) and (tvObjects.Selected.data <> nil) then
  begin
    d := TcObject(tvObjects.Selected.data);
    e := d;
    if d is TcData then
      d := (d as TcData).MetaData;
  end;
  actObjectEdit.Enabled := (e <> nil) and (e is TcData) and (d <> nil) and (d is TcMetaData) and (d as TcMetaData).Has(enEDITOR, ksEMPTY);
  actObjectCreate.Enabled := (tvObjects.Selected <> nil) and (popObjectsCreate.Items.Count > 0);
  mnuObjectCreateObject.Enabled := actObjectCreate.Enabled;
  actObjectDrop.Enabled := (e <> nil) and (e is TcData) and ((e as TcData).MetaData <> nil) and ((e as TcData).MetaData.Find(enScript, krsDROP) <> nil);
  actObjectEdit.Caption := 'E&dit Object';
  actObjectDrop.Caption := 'D&rop Object';
  if d <> nil then
  begin
    actObjectEdit.Caption := Format('E&dit ''%s''', [InitCap(d.sName)]);
    actObjectDrop.Caption := Format('D&rop ''%s''', [InitCap(d.sName)]);
  end;
  actObjectEdit.Hint := AnsiReplaceText(actObjectEdit.Hint, '&', '');
end;

// TfrmData2
//   onDisplayContent
//
procedure TfrmData2.onDisplayContent(Sender: TObject);
var
  p: TcObject;
  s: String;
const
  kaeFORMTYPES: array[boolean] of TeForm =
    (efsGrid, efsQuery);
begin
  if (sender <> nil) and (sender is TAction) and (((Sender as TAction).ActionComponent = popViewContentSQL) or ((Sender as TAction).ActionComponent = popViewContentGrid)) then
  begin
    btnViewContent.ImageIndex := ((Sender as TAction).ActionComponent as TMenuItem).ImageIndex;
    btnViewContent.Tag := ((Sender as TAction).ActionComponent as TMenuItem).Tag;
    actObjectsDisplay.Tag := ((Sender as TAction).ActionComponent as TMenuItem).Tag;
  end
  else if (sender <> nil) and (sender is TMenuItem) and ((Sender = popViewContentSQL) or (Sender = popViewContentGrid)) then
  begin
    btnViewContent.ImageIndex := (Sender as TMenuItem).ImageIndex;
    btnViewContent.Tag := (Sender as TMenuItem).Tag;
    actObjectsDisplay.Tag := (Sender as TMenuItem).Tag;
  end
  else if tvObjects.Selected <> nil then
  begin
    p := tvObjects.Selected.Data;
    if (p <> nil) and (p is TcData) and ((p as TcData).MetaData <> nil) and ((p as TcData).MetaData.Find(enSQL, krsCONTENT) <> nil) then
    begin
      (p as TcData).MetaData.hdlQuery := nil;
      s := trim((p as TcData).Statement([enSQL], krsCONTENT));
      s := StripHTMLTags(s); // Strip extra content
      if s <> ksEMPTY then
        FormSet.ExecuteSQL(kaeFORMTYPES[(Sender as TComponent).Tag = 0], s, eqtUndefined, []);
    end;
  end;
end;

// TfrmData2
//   ClipboardAction
//
procedure TfrmData2.ClipboardAction(Sender: TObject);
begin
  //
  // SynEdit
  if (ActiveControl <> nil) and (ActiveControl is TSynEdit) then
  begin
    if sender = actEditCopy then // CTRL-C
      (ActiveControl as TSynEdit).CopyToClipBoard
    else if sender = actEditCut then // CTRL-X
      (ActiveControl as TSynEdit).CutToClipBoard
    else if sender = actEditPaste then // CTRL-V
      (ActiveControl as TSynEdit).PasteFromClipBoard
    else if (sender = actEditUndo) then // CTRL-Z
      (ActiveControl as TSynEdit).Undo
    else if (sender = actEditSelectAll) then // CTRL-A
      (ActiveControl as TSynEdit).SelectAll;
  end
  //
  // Rich Edit
  else if (ActiveControl <> nil) and (ActiveControl is TRichEdit) then
    Clipboard.AsText := (ActiveControl as TRichEdit).Text
  //
  // Browser
  else if (ActiveControl <> nil) and (ActiveControl is TWebBrowser) then
    (ActiveControl as TWebBrowser).ExecWB(OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT);
end;

// TfrmData2
//   ExecuteItem (1)
//
function TfrmData2.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
begin
  // Nothing to do
  result := TRUE;
end;

// TfrmData2
//   ExecuteItem (2)
//
function TfrmData2.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  // Nothing to do
  result := TRUE;
end;

// TfrmData2
//   onPreferenceChange
//
procedure TfrmData2.onPreferenceChange(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    dbListView.BackgroundColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDBACKGROUNDCOLOR];
    dbListView.SelectedColumnColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTEDCOLUMNCOLOR];
    dbListView.SelectionColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTIONCOLOR];
    dbListView.CurrentLineColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTEDLINECOLOR];
    dbListView.NullCellColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDNULLCELLCOLOR];
  end;
end;

// TfrmData2
//   onNull
//
procedure TfrmData2.onNull(Sender: TObject);
begin
  //
end;

// TfrmData2
//   mnuFavoritesClick
//
procedure TfrmData2.mnuFavoritesClick(Sender: TObject);
begin
  // Fill in menu
  Formset.Favorites.SendToObject(mnuFavorites);
end;

// TfrmData2
//   onExecute
//
procedure TfrmData2.onExecute(Sender: TObject);
var
  s: String;
begin
  if not IsBusy then
  try
    PushCursor(crAppStart);
    s := GetBrowserSelection(wbOutput);
    if (s <> ksEMPTY) and (Application.MessageBox(PChar('Execute SQL Statement?' + ksCR + s), krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = idYES) then
      FormSet.ExecuteSQL(efsQuery, s, eqtUndefined, []);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onFind
//
procedure TfrmData2.actEditFindExecute(Sender: TObject);
begin
  dlgFind.Tag := 0;
  if (activecontrol <> nil) and (activecontrol is TWebBrowser) and (GetBrowserSelection(activecontrol as TWebBrowser) <> ksEMPTY) then
    dlgFind.FindText := GetBrowserSelection(activecontrol as TWebBrowser);
  dlgFind.Execute;
end;

// TfrmData2
//   onFindNext
//
procedure TfrmData2.onFindNext(Sender: TObject);
begin
  // dlgFind.Tag := 1;
end;

// TfrmData2
//   actObjectEditExecute
//
procedure TfrmData2.actObjectEditExecute(Sender: TObject);

  procedure DeleteNodeWithoutID(value: TcObject);
  var
    i: longint;
  begin
    for i := value.count - 1 downto 0 do
      if (value[i].eType = enObject) and
         (value[i] is TcData) and
         ((value[i] as TcData).MetaData <> nil) and
         ((value[i] as TcData).MetaData.Attribute[krsID] = ksEMPTY) then
        value.Delete(i)
      else
        DeleteNodeWithoutID(value[i]);
  end;

var
  frm: TfrmEditor;
  d, e: TcObject;
  p: TcData;
begin
  frm := nil;
  if IsLicensed and
     not IsBusy and
     (tvObjects.Selected <> nil) and
     (tvObjects.Selected.data <> nil) and
     (FormSet <> nil) and
     (m_objMetaData <> nil) then
  try
    PushCursor(crAppStart);
    d := TcObject(tvObjects.Selected.data);
    if (d <> nil) and (d is TcData) and ((d as TcData).MetaData <> nil) then
    begin
      CheckLoaded(d, FALSE);
      p := nil;
      e := (d as TcData).MetaData.Find(enEditor, ksEMPTY);
      if (e <> nil) and (e is TcMetaData) then
      try
        // Refresh data from database
        if (AnsiCompareText(FormSet.Preferences.StringVal[krsPREF_EDITORRELOADOBJECT], krsTRUE) = 0) and
           (AnsiCompareText(m_objMetaData.Option[krsOPTION_REFRESHBEFOREEDIT], krsTRUE) = 0) then
        begin
          // Set Doubtfull state
          DeleteNodeWithoutID(d as TcData);
          // Re- Reverse-Engineer data
          (d as TcData).Load((d as TcData).MetaData, [elfRefresh]);
          // Do data Display
          (d as TcData).Display;
        end;
        // Make a copy for edits.
        p := TcData.Create(d.parent);
        p.Copy(d);
        try
          frm := TfrmEditor.Create(self);
          frm.FormSet := FormSet;
          frm.Data := p as TcData;
          frm.Mode := fmEdit;
          frm.MetaEditor := e as TcMetaData;
          if frm.ShowModal = mrOK then
          begin
            // Refresh Object Header Display
            tvObjects.Selected.Text := p.ObjectID;
            (d as TcData).ObjectID := p.ObjectID;
            // Request cache to be updated
            DoRefresh(Sender);
          end;
        finally
          frm.Release;
        end;
      finally
        p.Free;
      end;
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   actObjectNewExecute
//
procedure TfrmData2.actObjectCreateExecute(Sender: TObject);
var
  frm: TfrmEditor;
  m, q, e, r: TcObject;
  p: TcData;
begin
  frm := nil;
  if IsLicensed and
     not IsBusy and
     (GetRootUser <> nil) and
     (actObjectCreate.Tag <> 0) and
     (tvObjects.Selected <> nil) then
  try
    PushCursor(crAppStart);
    // Identify Data & Meta Data
    e := TcObject(pointer(actObjectCreate.Tag));
    if (e <> nil) and (e is TcMetaData) and (e.eType = enEDITOR) and (e.Parent <> nil) then
    begin
      // m = the meta data type of the object.
      m := (e as TcMetaData).ParentObject([enObject]);
      if (m <> nil) and (m is TcMetaData) and (m.eType = enObject) and (m.Parent <> nil) and (m.Parent is TcMetaData) then
      begin
        // Get the meta data of the object to attach to.
        r := (m.Parent as TcMetaData).ParentObject([enObject]);
        // Find a target parent of type metadata r
        q := tvObjects.Selected.Data;
        repeat
          if (q <> nil) and (q is TcMetaData) then
          begin
            if tvObjects.Selected.Parent <> nil then
              q := tvObjects.Selected.Parent.Data
            else
              q := nil;
          end
          else if (r <> nil) and (q <> nil) and (q is TcData) and ((q as TcData).MetaData = r) then
            break
          else if (q <> nil) and (q.parent <> nil) then
            q := q.Parent;
        until (q = nil) or
              ((q <> nil) and (q is TcData) and ((q as TcData).MetaData = r));
        // Create and display form
        if (q <> nil) and (q is TcData) then
        begin
          p := TcData.Create(GetRootUser);
          p.MetaData := m as TcMetaData;
          try
            frm := TfrmEditor.Create(self);
            frm.FormSet := FormSet;
            frm.Data := p as TcData;
            frm.Mode := fmNew;
            frm.MetaEditor := e as TcMetaData;
            if frm.ShowModal = mrOK then
            begin
              // Add to hierarchy
              p.Parent := q;
              q.Add(p);
              p.State := edsDB;
              // Get the element name
              p.sValue := p.ObjectID;
              // Display TcData Element
              p.ClearChildren(enItem, ksEMPTY); // Empty element, to force refresh from database
              Display(p);
              OnDisplayStructure(Sender);
            end
            else
              p.Free;
          finally
            frm.Release;
          end;
        end;
      end;
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   OnCreateObjectSelection
//
procedure TfrmData2.OnCreateObjectSelection(Sender: TObject);
var
  s: String;
  L: longint;
begin
  if Sender is TMenuItem then
  begin
    L := (Sender as TMenuItem).MenuIndex;
    popObjectsCreate.Items[L].Checked := TRUE;
    s := AnsiReplaceText((Sender as TMenuItem).Caption, '&', ksEMPTY);
    actObjectCreate.Caption := Format('&Create ''%s''', [s]);
    actObjectCreate.Hint := Format('Create ''%s''', [s]);
    actObjectCreate.Tag := (Sender as TMenuItem).Tag;
  end;
end;

// TfrmData2
//   mnuObjectCreateObjectSelection
//
procedure TfrmData2.mnuObjectCreateObjectSelection(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    OnCreateObjectSelection(Sender);
    actObjectCreateExecute(sender);
  end;
end;

// TfrmData2
//   OnDropObject
//
procedure TfrmData2.OnDropObject(Sender: TObject);
var
  sel, selp: TTreeNode;
  d: TcObject;
  s, sError : String;
  m_objExecute: TcExecute;
  stmt: TcStatementList;
  bFailed: boolean;
  i: longint;
const
  krsFAILEDSTATEMENTS = 'Statements failed. Please check your schema.';
begin
  sel := tvObjects.Selected;
  if IsLicensed and
     not IsBusy and
     (sel <> nil) and
     (sel.data <> nil) then
  try
    PushCursor(crAppStart);
    d := TcObject(sel.data);
    if (d is TcData) and
       ((d as TcData).MetaData <> nil) and
       ((d as TcData).MetaData.Find(enScript, krsDROP) <> nil) and
       (Application.MessageBox(PChar(Format('Permanently drop %s "%s"?', [lowercase((d as TcData).MetaData.DisplayName), sel.text])), krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES) then
    begin
      s := (d as TcData).Statement([enScript], krsDROP);
      stmt := nil;
      bFailed := FALSE;
      if s = ksEMPTY then
        Application.MessageBox('No SQL Statement to execute!' + ksCR + 'Hint: Refresh your schema.', krsINFORMATION, MB_OK + MB_ICONEXCLAMATION)
      else
      try
        stmt := TcStatementList.Create(nil);
        if stmt.Parse(estParagraph, TextToText(s)) then
        begin
          m_objExecute := nil;
          try
            m_objExecute := TcExecute.Create(nil);
            m_objExecute.Connection := Formset.Connection;
            m_objExecute.IsReadOnly := TRUE;
            m_objExecute.IsAsynchronous := FALSE;
            for i := 0 to stmt.count - 1 do
              if (stmt[i] <> nil) and (stmt[i] is TcStatement) and ((stmt[i] as TcStatement).SQL <> ksEMPTY) then
              try
                s := TextToText((stmt[i] as TcStatement).SQL);
                s := StripHTMLTags(s); // Strip extra content
                s := trim(AnsiReplaceText(s, ksCR, ' '));
                if s <> ksEMPTY then
                begin
                  m_objExecute.Execute(s);
                  sError := m_objExecute.Error;
                  if sError <> ksEMPTY then
                  begin
                    Application.MessageBox(PChar(trim(s) + ksCR + ksCR + sError), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
                    bFailed := TRUE;
                  end;
                end;
              except
                on E: Exception do
                begin
                  Application.MessageBox(PChar(trim(s) + ksCR + ksCR + E.Message), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
                  bFailed := TRUE;
                end;
              end;
          finally
            m_objExecute.Free;
          end;
        end;
        if bFailed then
          Application.MessageBox(krsFAILEDSTATEMENTS, krsINFORMATION, MB_OK + MB_ICONINFORMATION)
        else
        begin
          selp := sel.parent;
          sel.Delete;
          if d.Parent <> nil then
          begin
            (d as TcData).RemoveDependency;
            d.Parent.Delete(d.ParentIndex);
          end;
          // refresh parent count
          if (selp <> nil) and (selp.Data <> nil) then 
            DisplayHeader(selp);
        end;
      finally
        stmt.free;
      end;
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onObjectTypeCreation
//
procedure TfrmData2.onObjectTypeCreation(sender: TObject);
var
  p, e, v: TcObject;
  m: TMenuItem;
  i: longint;
begin
  v := nil;
  if (sender <> nil) and (sender is TTreeNode) then
    v := (sender as TTreeNode).Data;
  p := nil;
  if (v <> nil) and (v is TcData) then
    p := (v as TcData).MetaData
  else if (v <> nil) and (v is TcMetaData) then
    p := v;
  if p <> nil then
  begin
    repeat
      m := nil;
      for i := 0 to popObjectsCreate.Items.count - 1 do
      begin
        e := TcObject(pointer(popObjectsCreate.Items[i].Tag));
        if (e <> nil) and (e.Parent = p) then
        begin
          m := popObjectsCreate.Items[i];
          break;
        end;
      end;
      if (m = nil) and (p <> nil) then
        p := p.Parent;
    until (p = nil) or (m <> nil);
    if m <> nil then
      OnCreateObjectSelection(m);
  end;
end;

// TfrmData2
//   onDisplayOptions
//
procedure TfrmData2.onDisplayOptions(Sender: TObject);
begin
  with TfrmDataPreference.Create(self) do
  begin
    dpData := m_objData;
    dpMetaData := m_objMetaData;
    Connection := FormSet.Connection;
    if ShowModal = mrOK then
    begin
      OnDisplayStructure(Sender);
    end;
  end;
end;

// TfrmData2
//   FormEventsShowHint
//
procedure TfrmData2.FormEventsShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  p, m: TcObject;
  s: String;
  op: TObject;
begin
  if (HintInfo.HintControl <> nil) and (HintInfo.HintControl is TObjectPanel) then
  begin
    op := HintInfo.HintControl as TObjectPanel;
    if (op <> nil) and (op is TObjectPanel) then
    begin
      m := m_objData.MetaData.Find((op as TObjectPanel).sMetaName);
      if (m <> nil) and (m is TcMetaData) then
      begin
        p := m_objData.FindInstance(m as TcMetaData, (op as TObjectPanel).sDataName);
        if (p <> nil) and (p is TcData) and (p.eType = enObject) then
        begin
          s := (p as TcData).Hint;
          if (AnsiCompareText(FormSet.Preferences.StringVal[krsPREF_CREATESCRIPTHINT], krsFALSE) <> 0) and CheckLoaded(p, FALSE) then
            s := s + ksCR + ksCR + trim(TextToText((p as TcData).Statement([enScript], GetScriptIndex)));
          s := StripHTMLTags(s);
          if s <> ksEMPTY then
          begin
            HintStr := s;
            HintInfo.HideTimeout := 20 * 1000;
            HintInfo.HintColor := clCream;
            HintInfo.HintMaxWidth := 400;
          end;
        end;
      end;
    end;
  end;
end;

// TfrmData2
//   tvObjectsDeletion
//
procedure TfrmData2.tvObjectsDeletion(Sender: TObject; Node: TTreeNode);
var
  p: TcObject;
begin
  //
  p := Node.Data;
  if (p <> nil) and (p is TcCustomData) then
    (p as TcCustomData).objDisplay := nil;
end;

// TfrmData2
//   tvObjectsExpanding
//
procedure TfrmData2.tvObjectsExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  AllowExpansion := CheckLoaded(TcObject(Node.Data), FALSE);
end;

// TfrmData2
//   tvObjectsCollapsing
//
procedure TfrmData2.tvObjectsCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
  SetState(Sender);
end;

// TfrmData2
//   OnDisplay
//
procedure TfrmData2.OnDisplay(Sender: TObject);
begin
  try
    PushCursor(crAppStart);
    timerGraph.Enabled := FALSE;
    m_pScriptNode := nil;
    if PageControl.ActivePage = tsScripts then
    begin
      OnDisplayStructure(Sender);
      onSelMove(wbOutput);
    end
    else if PageControl.ActivePage = tsData then
    begin
      if IsLicensed then
      begin
        OnDisplayContent2(Sender);
        dbListView.SendDisplayMessage(Sender);
      end;
    end
    else if PageControl.ActivePage = tsGraph then
    begin
      OnGraphShow(Sender);
      StatusText([psPanel1], ksEMPTY, esiNone);
    end;
    // Alerts?
    GetAlertItem(Sender);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   SetSchemaOutput
//
procedure TfrmData2.SetSchemaOutput(value: TcObject; parIsSummary: boolean);

const
  ksRES_SUMMARY =  'SCHEMAEXPLORER_SUMMARY';
  ksRES_OBJECT =   'SCHEMAEXPLORER_OBJECT';
  ksRES_DATA =     'SCHEMAEXPLORER_DATA';
  ksRES_METADATA = 'SCHEMAEXPLORER_METADATA';

  // Tool
  //   GetObjectText
  //
  function GetObjectText(Node: TcObject): string;
  var
    m: String;
    d: TDateTime;
  begin
    result := ksEMPTY;
    m := GetScriptIndex;
    // Instance Data XML
    if m = krsINSTANCEXML then
       result := (Node as TcData).HTML
    else
    begin
      d := now;
      result := (Node as TcData).Script[m];
      if result = ksEMPTY then
      begin
        result := (Node as TcData).Statement([enScript], m);
        FormSet.SetLog(Format('Script for %s ''%s''', [(Node as TcData).MetaData.DisplayName, Node.sValue]), ksEMPTY, now - d, []);
      end;
      result := (Node as TcData).ScriptHeader + result;
      // Colorize result!
      result := m_objColorParser.Colorize(TextToText(result));
    end;
  end;

  // Tool
  //   GetFullText
  //
  function GetFullText(value: TcObject; parResName: String): string;

    // Tools
    //   SetNodes
    //
    function SetNodes(value: TcObject): TcBag;

      // Tool
      //   SetNodes_Data
      //
      procedure SetNodes_Data(parData: TcData; parMetaData: TcMetaData; lst: TcBag);
      var
        i: longint;
      begin
        // Instance Data Node
        CheckLoaded(parData, parMetaData, TRUE);
        for i := 0 to parMetaData.Count - 1 do
          if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) and (parMetaData[i].eType = enObject) then
            SetNodes_Data(parData, parMetaData[i] as TcMetaData, lst);
        if parData.MetaData = parMetaData then
          lst.Add(parData)
        else for i := 0 to parMetaData.lstIndex.Count - 1 do
          if parData.Has(TcObject(parMetaData.lstIndex.Objects[i])) then
            lst.Add(TcObject(parMetaData.lstIndex.Objects[i]));
      end;

      // Tool
      //   SetNodes_MetaData
      //
      procedure SetNodes_MetaData(parData: TcData; parMetaData: TcMetaData; lst: TcBag);
      var
        i: longint;
      begin
        if parData.MetaData = parMetaData then
          SetNodes_Data(parData, parData.MetaData, lst)
        else
        begin
          CheckLoaded(parData, parMetaData, TRUE);
          for i := 0 to parMetaData.lstIndex.Count - 1 do
            if parData.Has(TcObject(parMetaData.lstIndex.Objects[i])) then
              lst.Add(TcObject(parMetaData.lstIndex.Objects[i]));
        end;
      end;

    begin
      result := TcBag.Create(nil);
      if (value <> nil) and (value is TcData) then
        SetNodes_Data(value as TcData, (value as TcData).MetaData, result)
      else if (value <> nil) and (value is TcMetaData) and (GetRootUser <> nil) and (GetRootUser is TcData) then
        SetNodes_MetaData(GetRootUser as TcData, value as TcMetaData, result)
    end;

  var
    i: longint;
    lst: TcBag;
    p: TcObject;
    s, sT, sN: String;
  begin
    lst := nil;
    result := ksEMPTY;
    if (value <> nil) and (value is TcObject) then
    try
      //
      // 1. Fill the list in
      lst := SetNodes(value);
      m_objMetaData.SortByDependency(lst);
      //
      // 2. Get the text
      SetProgressBar(TRUE, 0, lst.count - 1);
      if GetScriptIndex <> krsMETAXML then
        for i := 0 to lst.count - 1 do
        try
          SetProgressBar(TRUE, i, lst.count - 1);
          p := lst[i] as TcData;
          // Text
          s := ParagraphToText(TextToText(GetObjectText(p)));
          if s <> ksEMPTY then
            result := result + s + '<br><br>';
          // Header
          sT := (p as TcData).MetaData.Description;
          sN := ksEMPTY;
          if i = 0 then
          begin
            sT := (p as TcData).MetaData.DisplayName;
            sN := (p as TcData).Header;
          end;
        except
          on E: Exception do
            (lst[i] as TcData).MetaData.SetError(elFatal, E.Message, ksEMPTY);
        end
      else
      begin
        if (value <> nil) and (value is TcData) then
          result := result + (value as TcData).MetaData.XML
        else if (value <> nil) and (value is TcMetaData) then
          result := result + (value as TcMetaData).XML;
      end;
      //
      // 3. reset the progress bar
      SetProgressBar(FALSE, 0, 0);
      //
      // 4. Set up HTML output.
      s := result;
      result := LoadTextResource(parResName);
      result := AnsiReplaceText(result, '%ref_ApplicationPath%', GetFilePath(Application.ExeName));
      result := AnsiReplaceText(result, '%ref_ObjectType%', sT);
      result := AnsiReplaceText(result, '%ref_ObjectName%', sN);
      result := AnsiReplaceText(result, '%ref_Content%', s);
      result := AnsiReplaceText(result, '%ref_anchor%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHOR]));
      result := AnsiReplaceText(result, '%ref_anchorhoverbg%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHORHOVER]));
    finally
      lst.free;
    end;
  end;

  // Tool
  //   GetMetaDataHTML
  //
  function GetMetaDataHTML(value: TcObject; parResName: String): string;
  var
    s: String;
  begin
    result := ksEMPTY;
    s := ksEMPTY;
    if value is TcData then
      s := (value as TcData).MetaData.XML
    else if value is TcMetaData then
      s := (value as TcMetaData).XML;
    if s <> ksEMPTY then
    begin
      result := LoadTextResource(parResName);
      result := AnsiReplaceText(result, '%ref_ApplicationPath%', GetFilePath(Application.ExeName));
      result := AnsiReplaceText(result, '%ref_ObjectName%', value.sName);
      result := AnsiReplaceText(result, '%ref_Content%', s);
      result := AnsiReplaceText(result, '%ref_anchor%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHOR]));
      result := AnsiReplaceText(result, '%ref_anchorhoverbg%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHORHOVER]));
    end;
  end;

  // Tool
  //   GetSummaryHTML
  //
  function GetSummaryHTML(value: TcObject; parResName: String): string;

    function GetObjectCount(value: TcObject): String;
    var
      i, L: longint;
      s: String;
    begin
        result := ksEMPTY;
        // Children
        if (value is TcData) then
        begin
          for i := 0 to (value as TcData).MetaData.Count - 1 do
            if (value as TcData).MetaData[i] <> nil then
              result := result + GetObjectCount((value as TcData).MetaData[i])
        end
        else if (value is TcMetaData) then
          for i := 0 to (value as TcMetaData).Count - 1 do
            if value[i] <> nil then
              result := result + GetObjectCount(value[i]);
        // Self
        if (value <> nil) and (value is TcMetaData) and (value.eType = enObject) and ((value as TcMetaData).lstIndex.Count > 0) then
          with value as TcMetaData do
          begin
            // Count Objects
            L := (value as TcMetaData).CountData(GetRootUser);
            // Object Name
            s := DisplayName;
            if L > 1 then
              s := Description;
            // Create Output
            if L > 0 then
              result := Format('<tr><td align="right">%d&nbsp;</td><td><a href="%s:%d">%s</a></td></tr>', [L, krsSE_META, Tag, s]) + result;
          end;
    end;

  begin
    result := ksEMPTY;
    if value <> nil then
    begin
      result := LoadTextResource(parResName);
      if value is TcData then
      begin
        result := AnsiReplaceText(result, '%ref_ObjectType%', (value as TcData).MetaData.DisplayName);
        result := AnsiReplaceText(result, '%ref_ObjectName%', (value as TcData).Header);
        result := AnsiReplaceText(result, '%ref_FullScript%', Format('%s:%d', [krsSE_INSTANCE, Value.Tag]));
        result := AnsiReplaceText(result, '%ref_Content%', GetObjectCount(value));
        result := AnsiReplaceText(result, '%ref_Help%', (value as TcData).MetaData.OnlineHelpStr);
      end
      else
      begin
        result := AnsiReplaceText(result, '%ref_ObjectType%', (value as TcMetaData).Description);
        result := AnsiReplaceText(result, '%ref_ObjectName%', ksEMPTY);
        result := AnsiReplaceText(result, '%ref_FullScript%', Format('%s:%d', [krsSE_META, Value.Tag]));
        result := AnsiReplaceText(result, '%ref_Content%', GetObjectCount(value));
        result := AnsiReplaceText(result, '%ref_Help%', (value as TcMetaData).OnlineHelpStr);
      end;
      result := AnsiReplaceText(result, '%ref_ApplicationPath%', GetFilePath(Application.ExeName));
      result := AnsiReplaceText(result, '%ref_anchor%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHOR]));
      result := AnsiReplaceText(result, '%ref_anchorhoverbg%', '#' + ColorToHex(FormSet.Preferences.Color[krsPREF_SYNTAXANCHORHOVER]));
    end;
  end;

begin
// woops: mmoOutput.WordWrap := FormSet.Preferences.StringVal[krsPREF_WRAPTEXT] = krsTRUE;


  // Proceed to Display
  application.ProcessMessages;
  tvObjects_Lock(kiSETLOCK);
  if (value <> nil) then
  begin
    // Make Sure Object is Loaded
    if (value is Tcdata) and ((value as TcData).MetaData <> nil) and ((value as TcData).MetaData.CountObjects([enObject]) = 0) then
      CheckLoaded(value, FALSE);
    // Generate Script Text
    // A. Summarization
    if parIsSummary then
      DisplayResult(GetSummaryHTML(value, ksRES_SUMMARY))
    // B. Instance Data
    else if GetScriptIndex = krsINSTANCEXML then
      DisplayResult(GetFullText(value, ksRES_DATA))
    // C. XML for meta/instance data
    else if GetScriptIndex = krsMETAXML then
      DisplayResult(GetMetaDataHTML(value, ksRES_METADATA))
    // D. Generic script, parsed
    else
      DisplayResult(GetFullText(value, ksRES_OBJECT));
  end;
  tvObjects_Lock(kiUNSETLOCK);
end;

// TfrmData2
//   OnDisplayStructure
//
procedure TfrmData2.OnDisplayStructure(Sender: TObject);
var
  objNode: TTreeNode;
  p: TcObject;
begin
  objNode := tvObjects.Selected;
  // Proceed to Display
  application.ProcessMessages;
  if (objNode <> nil) and (m_pScriptNode <> objNode) then
  try
    PushCursor(crAppStart);
    SetAnimation(TRUE);
    try
      // Get Text
      p := TcObject(objNode.Data);
      if p <> nil then
        SetSchemaOutput(p, (AnsiCompareText(p.sName, krsUSER) = 0) or (p is TcMetadata));
      // Prepare Popup Menus
      onPreparePopup(Sender);
      //
      onObjectTypeCreation(objNode);
    except
      //
    end;
    SetState(Sender);
    DisplayErrors;
    m_pScriptNode := objNode;
  finally
    PopCursor;
    SetAnimation(FALSE);
  end;
end;

// TfrmData2
//   wbOutputBeforeNavigate2
//
procedure TfrmData2.wbOutputBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  p, q: TcObject;
begin
  Cancel := TRUE;
  if AnsiCompareText(system.copy(VarToStr(URL), 1, 7), 'http://') = 0 then
    FormSet.GetOnlineHelp(VarToStr(URL))
  else
  begin
    Cancel := FALSE;
    // Get the pointer
    p := nil;
    if AnsiCompareText(Item(VarToStr(URL), ':', 0), krsSE_META) = 0 then
      p := m_objData.MetaData.Find(strtointdef(Item(VarToStr(URL), ':', 1), kiUNDEFINED))
    else if AnsiCompareText(Item(VarToStr(URL), ':', 0), krsSE_INSTANCE) = 0 then
      p := m_objData.Find(strtointdef(Item(VarToStr(URL), ':', 1), kiUNDEFINED));
    // Create Display
    if p <> nil then
    try
      PushCursor(crHourGlass);
      SetAnimation(TRUE);
      // Highlight position?
      q := nil;
      if tvObjects.Selected <> nil then
        q := tvObjects.Selected.Data;
      if (q = nil) or ((q <> nil) and (p.Tag <> q.Tag)) then
        GetObjectItem(p);
      // Set Display
      try
        SetSchemaOutput(p, FALSE);
        // Prepare Popup Menus
        onPreparePopup(Sender);
        //
        onObjectTypeCreation(tvObjects.Selected);
      except
        //
      end;
      SetState(Sender);
      DisplayErrors;
      m_pScriptNode := tvObjects.Selected;
      Cancel := TRUE;
    finally
      PopCursor;
      SetAnimation(FALSE);
    end;
  end;
end;

// TfrmData2
//   CheckLoaded (1)
//
function TfrmData2.CheckLoaded(parObject: TcObject; bparAll: boolean): boolean;
begin
  result := TRUE;
  if (parObject <> nil) and
     (  (parObject is TcData) and
        (
          (
            not (parObject as TcData).IsLoaded and
            ((parObject as TcData).objDisplay <> nil) and
            ((parObject as TcData).objDisplay is TTreeNode)
          )
          and
          not ((AnsiCompareText(parObject.sName, krsUSER) = 0) and (parObject.CountObjects([enObject]) > 0))
        )
     ) then
    result := CheckLoaded(parObject as TcData, (parObject as TcData).MetaData, bparAll);
end;

// TfrmData2
//   CheckLoaded (2)
//
function TfrmData2.CheckLoaded(parData: TcData; parMetaData: TcMetaData; bparAll: boolean): boolean;
var
  e: TeLoadFlagSet;
begin
  try
    e := [];
    if bparAll and (AnsiCompareText(parMetaData.sName, krsUSER) <> 0) then
      e := [elfAll];
    PushCursor(crAppStart);
    result := parData.Load(parMetaData, e);
    GetAlerts(parData);
    CheckDisplay(parData);
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   OnDisplayContent2
//
procedure TfrmData2.OnDisplayContent2(Sender: TObject);

  procedure ClearGrid;
  begin
    m_pDataNode := nil;
    dbListView.Clear;
    lblDataObject.Caption := ksEMPTY;
  end;

var
  p: TcObject;
  s: String;
begin
  p := nil;
  if tvObjects.Selected <> nil then
    p := tvObjects.Selected.Data;
  if (p <> nil) and (m_pDataNode <> tvObjects.Selected) and (p is TcData) and ((p as TcData).MetaData <> nil) and ((p as TcData).MetaData.Find(enSQL, krsCONTENT) <> nil) then
  try
    SetAnimation(TRUE);
    s := trim((p as TcData).Statement([enSQL], krsCONTENT));
    s := StripHTMLTags(s); // Strip extra content
    if (s <> ksEMPTY) and CheckConnection(TRUE) then
    try
      PushCursor(crAppStart);
      try
        m_objExecute.Clear;
        dbListView.Close;
        lblDataObject.Caption := Format('%s %s', [(p as TcData).MetaData.DisplayName, (p as TcData).Header]);
        m_objExecute.IsReadOnly := TRUE;
        m_objExecute.IsAsynchronous := FormSet.Support[efsAsynchronous];
        m_objExecute.Execute(s, dbListView);
        m_pDataNode := tvObjects.Selected;
      except
        //
      end;
    finally
      PopCursor;
    end
    else
      ClearGrid;
  finally
    SetAnimation(FALSE);
  end
  else
    ClearGrid;
end;

// TfrmData2
//   tvObjectsChange
//
procedure TfrmData2.tvObjectsChange(Sender: TObject; Node: TTreeNode);
begin
  timerGraph.Enabled := TRUE;
end;

// TfrmData2
//   tvObjectsChanging
//
procedure TfrmData2.tvObjectsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  timerGraph.Enabled := FALSE;
end;

// TfrmData2
//   SetAnimation
//
procedure TfrmData2.SetAnimation(value: boolean);
begin
  //aniBusy.Active := Value;
  //aniBusy.Visible := Value;
  Application.ProcessMessages;
end;

// TfrmData2
//   DisplayResult
//
procedure TfrmData2.DisplayResult(value: string);
begin
  SetBrowserContent(wbOutput, value);
end;

// TfrmData2
//   SetSchemaBitmaps
//
procedure TfrmData2.SetSchemaBitmaps;
var
  bmp: TBitmap;
  e: TeIcon;
  i: longint;
begin
  bmp := nil;
  try
    //
    // Make sure array is to specs
    bmp := TBitmap.Create;
    while imgObjects.Count <= longint(high(TeIcon)) do
    begin
      bmp := TBitmap.Create;
      bmp.Height := imgObjects.Height;
      bmp.Width := imgObjects.Width;
      imgObjects.Add(bmp, nil);
    end;
    //
    // Duplicate array with image masks
    bmp.Canvas.Brush.Color := tvObjects.Color;
    for i := 0 to imgObjectsMasks.Count - 1 do
    begin
      for e := low(TeIcon) to high(TeIcon) do
      begin
        bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
        imgObjects.Draw(bmp.Canvas, 0, 0, longint(e), dsNormal, itImage);
        imgObjectsMasks.Draw(bmp.Canvas, 0, 0, i, dsNormal, itImage);
        imgObjects.Add(bmp, nil);
      end;
    end;
  finally
    bmp.Free;
  end;
end;

// TfrmData2
//   OnGraphShow
//
procedure TfrmData2.OnGraphShow(Sender: TObject);

  function GetData(value: TTreeNode): TcObject;
  begin
    result := nil;
    if (value <> nil) and (value.Data <> nil) then
      result := TcObject(value.Data);
    if (result <> nil) and (result is TcMetaData) and (value.Parent <> nil) then
      result := GetData(value.Parent);
  end;

var
  s: String;
  p, q: TcObject;
  d: TDateTime;
begin
  if Semaphore[krsGRAPH] = 0 then
  try
    d := now;
    PushCursor(crAppStart);
    Semaphore[krsGRAPH] := +1;
    s := ksEMPTY;
    // Make Sure Object is Loaded
    q := GetData(tvObjects.Selected);
    // Find the parent USER
    p := q;
    while (p <> nil) and (AnsiCompareText(p.sName, krsUSER) <> 0) and (p.Parent <> nil) do
      p := p.Parent;
    if (p <> nil) and (p is TcData) and (AnsiCompareText(p.sName, krsUSER) = 0) then
    begin
      // Display a new graph
      if m_objUserData <> p then
      begin
        CheckLoaded(p, TRUE);
        m_objUserData := p as TcData;
        pnlGraph.XML := m_objUserData.GraphXML;
      end
      // or, select a node within the graph
      else if pnlGraph.Selected <> q.Tag then
        pnlGraph.Selected := q.Tag;
    end;
    FormSet.SetLog('Display Graph', ksEMPTY, now - d, []);
  finally
    Semaphore[krsGRAPH] := -1;
    PopCursor;
  end;
end;

// TfrmData2
//   pnlGraphSelect
//
procedure TfrmData2.pnlGraphSelect(const sType, sName: string);
var
  m, p: TcObject;
  n: TTreeNode;
begin
  if m_objUserData <> nil then
  begin
    m := m_objData.MetaData.Find(sType);
    if (m <> nil) and (m is TcMetaData) then
    begin
      p := m_objData.FindInstance(m as TcMetaData, sName);
      if (p <> nil) and (p is TcCustomData) and ((p as TcCustomData).objDisplay <> nil) and ((p as TcCustomData).objDisplay is TTreeNode) then
      begin
        n := (p as TcCustomData).objDisplay as TTreeNode;
        (n.TreeView as TTreeView).Selected := n;
      end;
    end;
  end;
end;

// TfrmData2
//   pnlGraphChangeLocation
//
procedure TfrmData2.pnlGraphChangeLocation(const ID: longint; Value: TdPoint);
var
  p: TcObject;
begin
  if m_objUserData <> nil then
  begin
    p := m_objUserData.Find(ID);
    if (p <> nil) and (p is TcData) then
    begin
      (p as TcData).X := trunc(Value.X);
      (p as TcData).Y := trunc(Value.Y);
    end;
  end;
end;

// TfrmData2
//   onTranslate
//
procedure TfrmData2.onTranslate(Sender: TObject);
var
  b: boolean;
  f: TcFormSet;
  frm: TfrmDataTranslate;
  p: TcObject;
begin
  if IsLicensed and not IsBusy then
  try
    PushCursor(crAppStart);
    b := FALSE;
    f := nil;
    p := GetRootUser;
    if (p <> nil) and (p is TcData) then
    try
      // Make Sure Structure is loaded
      (p as Tcdata).Load((p as TcData).MetaData, [elfRecursive]);
      CheckDisplay(p as TcData);
      // Diaplay Modal
      f := TcFormSet.Create(FormSet.Parent);
      f.ImageList := FormSet.ImageList;
      f.Preferences := FormSet.Preferences;
      f.AlertImageList := FormSet.AlertImageList;
      f.HeaderImageList := FormSet.HeaderImageList;
      frm := nil;
      try
        frm := TfrmDataTranslate.Create(self);
        frm.FormSet := f;
        b := frm.ShowModal = mrOK;
        if b then
        begin
          Application.ProcessMessages;
          f.objDataAncestor := p as TcData;
          FormSet.Parent.Add(f);
          f.CreateForm(efsObjects, frmOwner, efiFromTranslation, ksEMPTY);
        end;
      finally
        frm.Release;
      end;
    finally
      if not b then
        f.free
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onExportContent
//
procedure TfrmData2.onExportContent(Sender: TObject);
var
  f: TfrmDataExport;
  p: TcObject;
begin
  if IsLicensed and not IsBusy then
  try
    PushCursor(crAppStart);
    p := GetRootUser;
    if (p <> nil) and (p is TcData) then
    begin
      f := TfrmDataExport.Create(self);
      f.objExportData := p as TcData;
      f.objExportMetaData := m_objMetaData;
      f.Connection := FormSet.Connection;
      f.ShowModal;
      CheckDisplay(p as TcData);
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onImportContent
//
procedure TfrmData2.onImportContent(Sender: TObject);
var
  f: TfrmDataImport;
  p: TcObject;
begin
  if IsLicensed and not IsBusy then
  try
    PushCursor(crAppStart);
    p := GetRootUser;
    if (p <> nil) and (p is TcData) then
    begin
      f := TfrmDataImport.Create(self);
      f.objMetaData := m_objMetaData;
      f.objFormData := p as TcData;
      f.objConnection := FormSet.Connection;
      f.ShowModal;
      CheckDisplay(p as TcData);
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   GetRootUser (1)
//
function TfrmData2.GetRootUser(value: TTreeNode): TcObject;
var
  p: TcObject;
  m: TcObject;
begin
  result := nil;
  m := nil;
  if (FormSet <> nil) and (m_objMetaData <> nil) then
    m := m_objMetaData.Find([enObject], krsUSER);
  if (m <> nil) and (value <> nil) then
  begin
    p := TcObject(value.Data);
    if (p <> nil) and (p is TcMetaData) and (value.Parent <> nil) then
      p := TcObject(value.Parent.Data);
    if (p <> nil) and (p is TcData) then
    repeat
      if (p <> nil) and (p is TcData) and ((p as TcData).MetaData <> m) and (p.parent <> nil) then
        p := p.Parent;
    until (p = nil) or
          ((p <> nil) and (p is TcData) and ((p as TcData).MetaData = m)) or
          ((p.Parent = nil) and (p is TcData) and ((p as TcData).MetaData <> m));
    if (p <> nil) and (p is TcData) and ((p as TcData).MetaData = m) then
      result := p as TcData;
  end;
end;

// TfrmData2
//   GetRootUser (2)
//
function TfrmData2.GetRootUser: TcObject;
begin
  result := GetRootUser(tvObjects.Selected);
end;

// TfrmData2
//   PushCursor
//
function TfrmData2.PushCursor(value: TCursor): longint;
begin
  result := CursorStack.Push(value);
end;

// TfrmData2
//   PopCursor
//
function TfrmData2.PopCursor: longint;
begin
  result := CursorStack.Pop;
end;

// TfrmData2
//   IsBusy
//
function TfrmData2.IsBusy: boolean;
begin
  result := CursorStack.IsBusy;
end;

// TfrmData2
//   GetAlerts
//
procedure TfrmData2.GetAlerts(value: TcData);
var
  p: TProcessAlert;
begin
  try
    p := TProcessAlert.Create(True); { create suspended second process does not run yet }
    p.Priority := tpLower; { set the priority to lower than normal }
    p.Data := value;
    p.DisplayAlerts := DisplayAlerts;
    Semaphore[krsALERT] := +1;
    p.Start; { now run the thread }
  except
    //
  end;
end;

// TfrmData2
//   DisplayAlerts
//
procedure TfrmData2.DisplayAlerts;

  procedure SubDisplayAlert(parData: TcData);
  var
    i, L: longint;
    p: TListItem;
    q: TcData;
    r: TcRuleresult;
    s: String;
  begin
    // Send rule processing result into list box
    for i := 0 to parData.lstRuleProcessing.count - 1 do
    begin
      q := parData.HeaderObject;
      if q <> nil then
      begin
        L := longint((parData.lstRuleProcessing[i] as TcRuleResult).eLevel);
        r := parData.lstRuleProcessing[i] as TcRuleResult;
        p := lvAlerts.Items.Add;
        p.Caption := ksEMPTY;
        p.ImageIndex := L - 1 + kiICONSTART;
        p.SubItems.Add(q.Header);
        p.SubItems.Add(r.sValue);
        s := GetAlertText(r, eattAlert);
        if s = ksEMPTY then
          s := GetAlertText(r, eattSQL);
        p.SubItems.Add(s);
        p.Data := r;
        q.iAlertLevel := max(q.iAlertLevel, L);
      end;
    end;
    // Process child elements
    for i := 0 to parData.count - 1 do
      SubDisplayAlert(parData[i] as TcData);
  end;

begin
  if Semaphore[krsALERT] <= 1 then
  begin
    Application.Hint := 'Displaying Alerts ..';
    Application.ProcessMessages;
    lvAlerts.Items.BeginUpdate;
    lvAlerts.Items.Clear;
    SubDisplayAlert(m_objData);
    lvAlerts.Items.EndUpdate;
    Application.Hint := ksEMPTY;
  end;
  Semaphore[krsALERT] := -1;
end;

// TfrmData2
//   SetSemaphore
//
procedure TfrmData2.SetSemaphore(value: String; bparIncrement: longint);
begin
  CursorStack.Semaphore[value] := bparIncrement;
  if CursorStack.Semaphore[value] = 0 then
    SetState(ActiveControl);
end;

// TfrmData2
//   GetSemaphore
//
function TfrmData2.GetSemaphore(value: String): longint;
begin
  result := CursorStack.Semaphore[value];
end;

// TfrmData2
//   lvAlertsColumnClick
//
procedure TfrmData2.lvAlertsColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Sender is TListView then
  begin
    m_iColumnToSort := Column.Index;
    (Sender as TListView).AlphaSort;
    Column.Tag := longint(Column.Tag = 0);
    m_bColumnOrder := Column.Tag = 0;
  end;
end;

// TfrmData2
//   lvAlertsCompare
//
procedure TfrmData2.lvAlertsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if m_iColumnToSort = 0 then
    Compare := longint(longint(Item1.ImageIndex) > longint(Item2.ImageIndex)) * 2 - 1
  else
    Compare := CompareText(Item1.SubItems[m_iColumnToSort - 1], Item2.SubItems[m_iColumnToSort - 1]);
  if m_bColumnOrder then
    Compare := 2 - Compare;
end;

// TfrmData2
//   GetAlertText
//
function TfrmData2.GetAlertText(parObject: TcObject; eparType: TeAlertTextType): String;
var
  s: String;
begin
  result := ksEMPTY;
  if (parObject <> nil) and
     (parObject.parent <> nil) and
     (parObject.parent is TcData) and
     (parObject is TcRuleResult) and
     ((parObject as TcRuleResult).Rule <> nil) then
    case eparType of
      eattAlert:
        result:= TextToText((parObject as TcRuleResult).Rule.Statement(parObject.Parent as TcData, [enInfo], ksEMPTY));
      eattSQL:
        result := TextToText((parObject as TcRuleResult).Rule.Statement(parObject.Parent as TcData, [enSQL], ksEMPTY));
      eattBoth:
        begin
          result := ksEMPTY;
          // Info
          s := TextToText((parObject as TcRuleResult).Rule.Statement(parObject.Parent as TcData, [enInfo], ksEMPTY));
          if s <> ksEMPTY then
            result := result + Format(FormSet.Preferences.FontHeader[krsPREF_FONTOBJECTHEADER], [TextToRTF(s) + '\par ']);
          // SQL
          s := TextToText((parObject as TcRuleResult).Rule.Statement(parObject.Parent as TcData, [enSQL], ksEMPTY));
          if s <> ksEMPTY then
            result := result + Format(FormSet.Preferences.FontHeader[krsPREF_FONTOBJECTTEXT], [TextToRTF(s)]);
        end;
    end;
end;

// TfrmData2
//   GetAlertItem
//
function TfrmData2.GetAlertItem(Sender: TObject): TListItem;
var
  i: longint;
  p: TObject;
  objNode: TTreeNode;
begin
  result := nil;
  objNode := tvObjects.Selected;
  if (CursorStack.Semaphore[krsALERT] = 0) and (objNode <> nil) and (m_pScriptNode <> objNode) then
  begin
    for i := 0 to lvAlerts.Items.Count - 1 do
    begin
      p := nil;
      with lvAlerts.Items[i] do
        if Data <> nil then
          p := TcRuleResult(Data).Parent;
      if (p <> nil) and (p is TcData) then
        p := (p as TcData).HeaderObject;
      if p = objNode.Data then
      begin
        result := lvAlerts.Items[i];
        break;
      end;
    end;
    if (result <> nil) and (lvAlerts.Selected <> result) then
    begin
      lvAlerts.Selected := nil;  // The next statement does not deselect!
      lvAlerts.Selected := result;
    end;
    m_pScriptNode := objNode;
  end;
end;

// TfrmData2
//   onDataDump
//
procedure TfrmData2.onDataDump(Sender: TObject);
var
  p: TcObject;
begin
  if not IsBusy then
  try
    PushCursor(crAppStart);
    p := tvObjects.Selected.Data;
    if (p <> nil) and
       (
         ((p is TcData) and ((p as TcData).MetaData <> nil) and ((p as TcData).MetaData.Find(enSQL, krsCONTENT) <> nil)) or
         ((p <> nil) and (p is TcMetaData) and ((p as TcMetaData).Find(enSQL, krsCONTENT) <> nil))
       ) then
    begin
      if p is TcData then
        (p as TcData).MetaData.hdlQuery := nil
      else if p is TcMetaData then
        (p as TcMetaData).hdlQuery := nil;
      Formset.OnDataDump(p);
    end;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   onSnapShotCompare
//
procedure TfrmData2.onSnapShotCompare(Sender: TObject);
var
  f: TfrmDataSnapshotDifference;
begin
  if IsLicensed and not IsBusy then
  try
    PushCursor(crAppStart);
    f := TfrmDataSnapshotDifference.Create(self);
    f.objData := m_objData;
    f.objMetaData := m_objMetaData;
    f.Connection := FormSet.Connection;
    if GetRootUser <> nil then
      f.Schema := GetRootUser.sValue;
    f.ShowModal;
    tvObjects_Lock(kiSETLOCK);
    m_objData.Display;
    tvObjects_Lock(kiUNSETLOCK);
    DisplayErrors;
  finally
    PopCursor;
  end;
end;

// TfrmData2
//   CheckDisplay
//
procedure TfrmData2.CheckDisplay(value: TcData);
begin
  tvObjects_Lock(kiSETLOCK);
  value.UpdateIcon;
  tvObjects_Lock(kiUNSETLOCK);
end;

// TfrmData2
//   DisplayErrors
//
procedure TfrmData2.DisplayErrors;
begin
  tsErrors.TabVisible := m_objMetaData.DisplayErrors(lvErrors);
end;

// TfrmData2
//   PageControlBottomResize
//
procedure TfrmData2.PageControlBottomResize(Sender: TObject);
begin
  lvAlerts.Width := lvAlerts.Width + 1;
  lvErrors.Width := lvErrors.Width + 1
end;

// TfrmData2
//   tvObjects_Lock
//
procedure TfrmData2.tvObjects_Lock(value: longint);
begin
  if (m_iDisplayLock = 0) and (value > 0) then
    tvObjects.Items.BeginUpdate;
  inc(m_iDisplayLock, value);
  if (m_iDisplayLock = 0) and (value < 0) then
    tvObjects.Items.EndUpdate;
end;

// TfrmData2
//   onListViewMessage
//
procedure TfrmData2.onListViewMessage(Sender: TObject; value: String);
begin
  StatusText([psPanel1], value, esiGridRight);
end;

// TfrmData2
//   OnDisplayFieldExit
//
procedure TfrmData2.OnDisplayFieldExit(Sender: TObject);
begin
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmData2
//   onSelMove
//
procedure TfrmData2.onSelMove(Sender: TObject);
var
  L: longint;
begin
  SetState(Sender);
  if (eFormState >= efmInitialize) and (Sender <> nil) and (Sender = ActiveControl) then
  begin
    if (Sender is TSynEdit) then
      with (Sender as TSynEdit) do
        StatusText([psPanel1], Format(krsSTATUS_COLLINE, [CaretY, CaretX]), esiTextRight)
    else if ((Sender = wbOutput) or (Sender is TGraphPanel)) then
      StatusText([psPanel1], ksEMPTY, esiTextRight)
    else if (Sender is TListView) then
      with (Sender as TListView) do
      begin
        L := 0;
        if Selected <> nil then
          L := Selected.Index;
        StatusText([psPanel1], Format('Line %d', [L]), esiGridBottom);
      end
    else if (Sender is TdbListView) then
      (Sender as TdbListView).SendDisplayMessage(Sender)
    else
      StatusText([psPanel1], ksEMPTY, esiNone);
  end;
end;

// TfrmData2
//   WebBrowser1NavigateComplete2 Method
//
procedure TfrmData2.wbOutputNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  hr: HResult;
  CustDoc: ICustomDoc;
begin
  if m_FDocHostUIHandler <> nil then
  begin
    hr := wbOutput.Document.QueryInterface(ICustomDoc, CustDoc);
    if hr = S_OK then
      CustDoc.SetUIHandler(m_FDocHostUIHandler);
  end;
end;

// TfrmData2
//   GetLocalToolbar
//
function TfrmData2.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmData2
//   DisplayDataErrors Method
//
procedure TfrmData2.DisplayDataErrors(Sender: TObject; value: String);
begin
  lstDataErrors.Items.Text := value;
  if value = ksEMPTY then
  begin
    if PageControlBottom.ActivePage = tsGridErrors then
      PageControlBottom.ActivePage := tsAlert;
    tsGridErrors.TabVisible := FALSE;
  end
  else
  begin
    if not tsGridErrors.TabVisible then
    begin
      tsGridErrors.TabVisible := TRUE;
      PageControlBottom.ActivePage := tsGridErrors;
    end;
  end;
end;

// TfrmData2
//   GetLocalToolbar
//
function TfrmData2.IsLicensed: boolean;
begin
  result := TRUE;
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    result := FormSet.Preferences.IsLicensed;
    if not result then
      Application.MessageBox(krsDENIEDFEATURE, krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
  end;
end;

// TfrmData2
//   GetBrowserSelection
//
function TfrmData2.GetBrowserSelection(value: TWebBrowser): String;
var
  pSel: IHTMLSelectionObject;
  pRange: IHTMLTxtRange;
  Document: IHTMLDocument2;
begin
  Document := value.Document as IHTMLDocument2;
  if Document <> nil then
  begin
    pSel := Document.selection as IHTMLSelectionObject;
    if pSel <> nil then
    begin
      if pSel.type_ = 'Text' then
      begin
        pRange := pSel.createRange as IHTMLTxtRange;
        if pRange <> nil then
          result := pRange.text;
      end;
    end;
  end;
end;

// TfrmData2
//   onMasterHelp
//
procedure TfrmData2.onMasterHelp(Sender: TObject);
begin
  FormSet.GetOnlineHelp(ksEMPTY);
end;

// TfrmData2
//   popObjectScriptPopup
//
procedure TfrmData2.popObjectScriptPopup(Sender: TObject);
begin
  ActiveControl := wbOutput;
  SetState(Sender);
end;

// TfrmData2
//   ReConnect
//
function TfrmData2.ReConnect: boolean;
begin
  result := TRUE;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize;
end.


