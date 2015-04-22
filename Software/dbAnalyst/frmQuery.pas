unit frmQuery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, StdCtrls,
  PreferenceLib, ConnectionLib, DataLib, ToolWin, ImgList, ADODB_TLB, AppEvnts,
  daObjectLib,
  FormLib,
  daGlobals,
  ExecuteLib,
  FavoriteLib,
  StatementLib,
  Progress,
  Main,
  StdActns, ActnList, dbListView,
  ExtDlgs,
  dfsSplitter,
  SynEdit,
  SynEditSearchReplaceLib,
  IntelliSenseLib,
  OleCtrls,
  SHDocVw,
  IEDocHostUIHandler;

type
  TfrmQuery = class(TcForm)
    DataMainMenu: TMainMenu;
    dlgSave: TSaveDialog;
    mnuEdit: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuTools: TMenuItem;
    ToolBar: TToolBar;
    btnActionStart: TToolButton;
    btnActionStop: TToolButton;
    mnuAction: TMenuItem;
    mnuActionStop: TMenuItem;
    mnuActionStart: TMenuItem;
    btnActionAutoCommit: TToolButton;
    btnActionCommit: TToolButton;
    btnActionRollback: TToolButton;
    N2: TMenuItem;
    mnuActionCommit: TMenuItem;
    mnuActionRollback: TMenuItem;
    mnuActionAutoCommit: TMenuItem;
    ToolButton2: TToolButton;
    btnReportFormat: TToolButton;
    mnuActionReportOutput: TMenuItem;
    ToolButton3: TToolButton;
    btnAnalyze: TToolButton;
    mnuActionAnalyze: TMenuItem;
    N1: TMenuItem;
    ActionList: TActionList;
    actActionStart: TAction;
    actActionStop: TAction;
    actActionPause: TAction;
    actActionCommit: TAction;
    actActionRollback: TAction;
    actActionAutoCommit: TAction;
    actActionReportOutput: TAction;
    actActionAnalyze: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    pnlBottom: TPanel;
    btnSpool: TToolButton;
    actActionSpool: TAction;
    mnuActionSpool: TMenuItem;
    dlgSpool: TSaveDialog;
    mnuFavorites: TMenuItem;
    mnuFavoritesAdd: TMenuItem;
    N3: TMenuItem;
    pnlClient: TPanel;
    actEditRedo: TAction;
    mnuEditRedo: TMenuItem;
    N4: TMenuItem;
    actFavoritesAdd: TAction;
    actFavoritesEdit: TAction;
    mnuFavoritesEdit: TMenuItem;
    actEditFind: TAction;
    N5: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditFindNext: TMenuItem;
    btnFind: TToolButton;
    btnFindNext: TToolButton;
    ToolButton7: TToolButton;
    btnReplace: TToolButton;
    actEditReplace: TAction;
    mnuEditReplace: TMenuItem;
    popSQL: TPopupMenu;
    popQueryResult: TPopupMenu;
    popSQLUndo: TMenuItem;
    popSQLRedo: TMenuItem;
    N6: TMenuItem;
    popSQLCut: TMenuItem;
    popSQLCopy: TMenuItem;
    popSQLPaste: TMenuItem;
    popSQLSelectAll: TMenuItem;
    N7: TMenuItem;
    popSQLFind: TMenuItem;
    popSQLFindNext: TMenuItem;
    popSQLReplace: TMenuItem;
    popQueryResultCopy: TMenuItem;
    popQueryResultSelectAll: TMenuItem;
    N9: TMenuItem;
    popQueryResultFind: TMenuItem;
    popQueryResultFindNext: TMenuItem;
    actEditUpperCase: TAction;
    actEditLowerCase: TAction;
    N8: TMenuItem;
    mnuEditUpperCase: TMenuItem;
    mnuEditLowerCase: TMenuItem;
    N10: TMenuItem;
    popSQLUpperCase: TMenuItem;
    popSQLLowerCase: TMenuItem;
    actObjectCreate: TAction;
    ToolButton4: TToolButton;
    btnActionParagraphMark: TToolButton;
    actAddParagraph: TAction;
    N11: TMenuItem;
    mnuEditParagraphMark: TMenuItem;
    N12: TMenuItem;
    popSQLParagraphMark: TMenuItem;
    actActionMemorize: TAction;
    N13: TMenuItem;
    mnuActionMemorize: TMenuItem;
    pgDisplay: TPageControl;
    tsText: TTabSheet;
    tsGrid: TTabSheet;
    dbListView: TDBListView;
    actActionExecuteToOutput: TAction;
    ExecutetoOutput1: TMenuItem;
    popSQLStart: TMenuItem;
    popSQLMemorizeStatement: TMenuItem;
    N14: TMenuItem;
    popSQLAnalyzeSQLStatements: TMenuItem;
    popSQLExecutetoOutput: TMenuItem;
    dlgOpenTextFile: TOpenTextFileDialog;
    dlgSaveTextFile: TSaveTextFileDialog;
    pnlTop: TPanel;
    pnlProgress: TProgress;
    popListView: TPopupMenu;
    popListViewCopy: TMenuItem;
    actEditReadOnly: TAction;
    popListViewReadOnly: TMenuItem;
    mnuActionReadOnly: TMenuItem;
    spltrQuery: TdfsSplitter;
    mmoSQL: TSynEdit;
    mmoQueryResult: TSynEdit;
    actEditFindPrev: TAction;
    mnuEditFindPrev: TMenuItem;
    actEditFindNext: TAction;
    popSQLFindPrevious: TMenuItem;
    btnFindPrev: TToolButton;
    popQueryResultFindPrevious: TMenuItem;
    btnCut: TToolButton;
    btnCopy: TToolButton;
    btnPaste: TToolButton;
    ToolButton9: TToolButton;
    actCodeCompletionEnable: TAction;
    btnEnableCodeCompletion: TToolButton;
    mnuEnableCodeCompletion: TMenuItem;
    tsHint: TTabSheet;
    actActionExecuteBlock: TAction;
    ExecuteBlock1: TMenuItem;
    mnuCodeCompletionEnable: TMenuItem;
    actCodeCompletionRefresh: TAction;
    mnuCodeCompletionRefresh: TMenuItem;
    actCodeCompletion_ShowTableList: TAction;
    actCodeCompletion_ShowColumnList: TAction;
    mnuCodeCompletionShowColumnList: TMenuItem;
    mnuCodeCompletionShowTableList: TMenuItem;
    timerIntelliSense: TTimer;
    N15: TMenuItem;
    popObjectStructure: TMenuItem;
    actObjectStructureListColumns: TAction;
    popObjectStructureListColumns: TMenuItem;
    N16: TMenuItem;
    mnuObjectStructure: TMenuItem;
    mnuObjectStructureListColumns: TMenuItem;
    wbAnalysis: TWebBrowser;
    imgAnalysis: TImageList;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    actHelpMaster: TAction;
    timerExecution: TTimer;

    procedure onFindPrev(Sender: TObject);
    procedure OnDisplayFieldExit(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure spltrQueryMoved(Sender: TObject);
    procedure FormActivate(Sender: TObject); override;
    procedure FormDeactivate(Sender: TObject); override;
    procedure ClipboardAction(Sender: TObject);
    procedure mnuFavoritesClick(Sender: TObject);
    procedure onClose(Sender: TObject);
    procedure onStart(Sender: TObject);
    procedure onFileReOpen(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure onAutoCommit(Sender: TObject);
    procedure onCommit(Sender: TObject);
    procedure onRollback(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetState(Sender: TObject);
    procedure SetFont(Sender: TObject);
    procedure SetAnimation(value: boolean);
    procedure FormShow(Sender: TObject);
    procedure SetButtons(eparState: TeQueryButtonSet);
    procedure FormDestroy(Sender: TObject);
    procedure onAnalyze(Sender: TObject);
    procedure onReportOutput(Sender: TObject);
    procedure onListViewMessage(Sender: TObject; value: String);
    procedure onSpool(Sender: TObject);
    procedure mnuFavoritesAddClick(Sender: TObject);
    procedure onSelMove(Sender: TObject);
    procedure actFavoritesEditClick(Sender: TObject);
    procedure onFind(Sender: TObject);
    procedure onFindNext(Sender: TObject);
    procedure onReplace(Sender: TObject);
    procedure onUpperCase(Sender: TObject);
    procedure onLowerCase(Sender: TObject);
    procedure OnInsertParagraphMark(Sender: TObject);
    procedure onMemorize(Sender: TObject);
    procedure pgDisplayChange(Sender: TObject);
    procedure OnFileOpen(Sender: TObject);
    procedure OnFileSave(Sender: TObject);
    procedure OnActionExecuteToOutput(Sender: TObject);
    procedure OnReadOnly(Sender: TObject);
    procedure OnEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure mmoSQLChange(Sender: TObject);
    procedure mmoSQLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actCodeCompletionEnableExecute(Sender: TObject);
    procedure mmoSQLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mmoSQLClick(Sender: TObject);
    procedure mmoSQLScroll(Sender: TObject; ScrollBar: TScrollBarKind);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure pnlTopResize(Sender: TObject);
    procedure OnExecuteBlock(Sender: TObject);
    procedure actCodeCompletionRefreshExecute(Sender: TObject);
    procedure onShowTableList(Sender: TObject);
    procedure onShowColumnList(Sender: TObject);
    procedure timerIntelliSenseTimer(Sender: TObject);
    procedure actObjectStructureListColumnsExecute(Sender: TObject);
    procedure pgDisplayDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure wbAnalysisNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure OnMasterHelp(Sender: TObject);
    procedure timerExecutionTimer(Sender: TObject);
    procedure getExecutionTime(value: double);

  private
    // Private Declarations
    //
    m_eQueryState: TeQueryButtonSet;
    m_objExecute: TcExecute;
    m_bAnalyze: boolean;
    m_objConnection: TcConnection;
    m_sSpoolFileName: String;
    m_sFindText: string;
    m_eFindOptions: TFindOptions;
    m_sFileName: String;
    m_rRatio: double;
    m_bOneConnection: boolean;
    m_seSearchReplace: TcSynEditSearchReplace;
    // Context Sensitive Parsing
    m_objContextStmt: TisStatement;
    m_objTableList: TisTableList;
    m_objIndexList: TisIndexList;
    m_objIntelliSense: TIntelliSenseControl;
    m_iLastKey: word;
    m_FDocHostUIHandler: TDocHostUIHandler;
    m_iTimeStart: DWord;

  protected
    // Protected Declarations
    //
    procedure   SetDisplayType(value: TeQueryType); override;
    function    CheckConnection(bparConnect: boolean): boolean; override;
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // Private Methods
    //
    function    GetCurrentStatement(eparType: TeParseType): TrStmtPos;
    function    InitializeConnection: boolean;
    function    FinalizeConnection: boolean;
    function    GetDisplayMode: TeQueryType;
    procedure   SetTab(parLineStart, parLineEnd: longint; parMode: TeProgressMode);

  public
    // Public Methods
    //
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; override;
    function    Finalize: boolean; override;
    procedure   onPreferenceChange(Sender: TObject); override;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; override;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; override;
  end;

implementation

uses
  Math,
  strUtils,
  Variants,
  ClipBrd,
  ComObj,
  daResourceStrings,
  daStreamLib,
  IEConst;

{$R *.DFM}

// TfrmQuery
//   FormCreate
//
procedure TfrmQuery.FormCreate(Sender: TObject);
begin
  m_eQueryState := [ebsStart];
  m_objExecute := TcExecute.Create(nil);
  dbListView.IsReadOnly := TRUE;
  m_objExecute.onSetButton := SetButtons;
  actActionStop.onExecute := m_objExecute.onStop;
  actActionPause.onExecute := m_objExecute.onPause;
  m_bAnalyze := FALSE;
  m_sSpoolFileName := ksEMPTY;
  m_sFindText := ksEMPTY;
  m_eFindOptions := [];
  m_sFileName := ksEMPTY;
  spltrQueryMoved(Sender);
  m_bOneConnection := FALSE;
  m_objConnection := nil;
  if GetWinVersion >= wvWinXP then
  begin
    mmoSQL.DoubleBuffered := TRUE;
    mmoQueryResult.DoubleBuffered := TRUE;
    dbListView.DoubleBuffered := TRUE;
  end;
  CursorStack.RegisterComponent(mmoSQL);
  CursorStack.RegisterComponent(mmoQueryResult);
  CursorStack.RegisterComponent(dbListView);
  m_seSearchReplace := TcSynEditSearchReplace.Create(self);
  mmoSQL.OnReplaceText := m_seSearchReplace.SynEditorReplaceText;
  // IntelliSense
  m_objContextStmt := TisStatement.Create(nil);
  m_objTableList := TisTableList.Create(nil);
  m_objIndexList := TisIndexList.Create(nil);
  m_objIntelliSense := TIntelliSenseControl.Create(self);
  m_objIntelliSense.Parent := self;
  m_objIntelliSense.Control := mmoSQL;
  m_objIntelliSense.Tables := m_objTableList;
  m_objIntelliSense.Indexes := m_objIndexList;
  m_objContextStmt.IntelliSenseControl := m_objIntelliSense;
  // Browser
  m_FDocHostUIHandler := TDocHostUIHandler.Create;
  m_FDocHostUIHandler.WebBrowser := wbAnalysis;
end;

// TfrmQuery
//   FormDestroy
//
procedure TfrmQuery.FormDestroy(Sender: TObject);
begin
  FreeAndNil(m_objExecute);
  FreeAndNil(m_seSearchReplace);
  if not m_bOneConnection then
    FreeAndNil(m_objConnection);
  FreeAndNil(m_objContextStmt);
  FreeAndNil(m_objTableList);
  FreeAndNil(m_objIndexList);
  m_FDocHostUIHandler.WebBrowser := nil;
  m_FDocHostUIHandler.Free;
  m_FDocHostUIHandler := nil;
end;

// TfrmQuery
//   FormShow
//
procedure TfrmQuery.FormShow(Sender: TObject);
begin
  SetToolbarLocation(ControlBar, Toolbar);
  SetState(Sender);
end;

// TfrmQuery
//   FormActivate
//
procedure TfrmQuery.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  SetMainMenu(mmOpen,         TRUE,  OnFileOpen);
  SetMainMenu(mmSave,         TRUE,  OnFileSave);
  SetMainMenu(mmSaveAs,       FALSE, TNotifyEvent(nil));
  SetMainMenu(mmClose,        TRUE,  onClose);
  SetMainMenu(mmReOpen,       TRUE,  TRUE, OnFileReOpen);
  SetLRUMenu(onFileReOpen);
  SetState(Sender);
  ActionList.State := asNormal;
end;

// TfrmQuery
//   FormDeactivate
//
procedure TfrmQuery.FormDeactivate(Sender: TObject);
begin
  ActionList.State := asSuspended;
  inherited FormDeactivate(nil);
  dbListView.onDeActivate(Sender);
  SetMainMenu(mmOpen,         FALSE, TNotifyEvent(nil));
  SetMainMenu(mmSave,         FALSE, TNotifyEvent(nil));
  SetMainMenu(mmSaveAs,       FALSE, TNotifyEvent(nil));
  SetMainMenu(mmClose,        FALSE, TNotifyEvent(nil));
  SetMainMenu(mmReOpen,       FALSE, FALSE, TNotifyEvent(nil));
  SetLRUMenu(nil);
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmQuery
//   InitializeConnection
//
function TfrmQuery.InitializeConnection: boolean;
begin
  if m_objConnection = nil then
  begin
    case m_bOneConnection of
      FALSE:
        begin
          m_objConnection := TcConnection.Create(nil);
          m_objConnection.Copy(FormSet.Connection);
        end;
      TRUE:
        m_objConnection := FormSet.Connection;
    end;
  end;
  m_objExecute.Connection := m_objConnection;
  dbListView.Connection := m_objConnection;
  result := m_objConnection.Connected;
  if not result then
  begin
    m_objConnection.Copy(FormSet.Connection);
    result := m_objConnection.Open and FormSet.ApplySessionParameters(m_objConnection);
    if result and FormSet.Support[efsTransaction] then
      m_objConnection.BeginTrans;
  end;
  //
  if FormSet.Preferences.IsLicensed then
    pgDisplay.ActivePage := tsText
  else
    pgDisplay.ActivePage := tsGrid;
  tsGrid.TabVisible := FormSet.Preferences.IsLicensed;
  tsHint.TabVisible := FormSet.Preferences.IsLicensed;
  // Context Sensitive features
  if result then
  begin
    m_objTableList.MetaData := FormSet.MetaData;
    m_objIndexList.MetaData := FormSet.MetaData;
    actCodeCompletionRefreshExecute(nil);
    m_objIntelliSense.sPseudoColumns := Formset.MetaData.Option[krsOPTION_PSEUDOCOLUMNS];
    m_objContextStmt.PreferenceList := FormSet.Preferences;
  end;
end;

// TfrmQuery
//   Initialize
//
function TfrmQuery.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
var
  bAutoCommit: boolean;
begin
  inherited Initialize(eparMode, eparParameter);
  m_bOneConnection := FormSet.Connection.bOneConnection;
  result := CheckConnection(TRUE);
  if result then
    InitializeConnection;
  bAutoCommit := FALSE;
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
    bAutoCommit := FormSet.Preferences.StringVal[krsPREF_CONSOLEAUTOCOMMIT] <> krsFALSE;
  FormSet.InitializeToolMenu(mnuTools);
  dbListView.OnDisplayMessage := onListViewMessage;
  m_objConnection.procLog := FormSet.Connection.procLog;
  m_objConnection.bAutoCommit := bAutoCommit;
  FormSet.SetHighlighter(mmoSQL);
  onPreferenceChange(nil);
  OnStart(nil);
  OnSelMove(mmoSQL);
  // Root Online Help?
  actHelpMaster.Enabled := FormSet.HasMasterOnlineHelp;
  // State
  SetState(nil);
end;

// TfrmQuery
//   FinalizeConnection
//
function TfrmQuery.FinalizeConnection: boolean;
begin
  try
    m_objConnection.CommitTrans;
    if not m_bOneConnection then
      m_objConnection.Close;
  except
    //
  end;
  m_objExecute.Connection := nil;
  dbListView.Connection := nil;
  if not m_bOneConnection then
    m_objConnection.procLog := nil;
  result := TRUE;
end;

// TfrmQuery
//   Finalize
//
function TfrmQuery.Finalize: boolean;
begin
  FormDeactivate(nil);
  FinalizeConnection;
  dbListView.OnDisplayMessage := nil;
  mmoSQL.Highlighter := nil;
  result := TRUE;
end;

// TfrmQuery
//   FormCloseQuery
//
procedure TfrmQuery.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := TRUE;
  if frmMain.Preferences.StringVal[krsPREF_CONSOLEPROMPTBEFORECLOSINGWINDOW] <> krsFALSE then
    CanClose := Application.MessageBox('Do you want to close this Console Window?' + ksCR + 'Press "Yes" to close this window, "No" to cancel closing this window.', 'Confirmation', MB_YESNO + MB_ICONEXCLAMATION) = idYES;
  if CanClose then
  begin
    CanClose := m_objExecute.State = eesIdle;
    if CanClose and FormSet.Support[efsTransaction] and not m_objConnection.bAutoCommit and inherited CheckConnection(TRUE) and m_objConnection.InTransaction then
      with m_objConnection do
        case Application.MessageBox('Press "Yes" to commit pending transactions, "No" to rollback pending transactions, "Cancel" otherwise.', krsINFORMATION, MB_YESNOCANCEL + MB_ICONEXCLAMATION) of
          idYES:
            CommitTrans;
          idNO:
            RollbackTrans;
          idCancel:
            CanClose := FALSE;
        end;
  end;
  if CanClose then
    OnCloseQuery := nil;
end;

//
// Menu Items handling
//

// TfrmQuery
//   onClose
//
procedure TfrmQuery.onClose(Sender: TObject);
begin
  Close;
end;

// TfrmQuery
//   CurrentStatement
//
function TfrmQuery.GetCurrentStatement(eparType: TeParseType): TrStmtPos;
var
  p: TcStatementList;
begin
  p := nil;
  try
    p := TcStatementList.Create(nil);
    result := p.GetCurrentStatement(eparType, mmoSQL);
  finally
    p.Free;
  end;
end;

// TfrmQuery
//   onStart
//
procedure TfrmQuery.onStart(Sender: TObject);

  // Tools
  //   AnalyzeSQL
  //
  procedure AnalyzeSQL(parObject: TcMetaData; value: String; m_objDisplay: TObject);

    procedure AnalyzeSQLItem(parObject: TcMetaData; value: String; m_objDisplay: TObject; parItem: longint);
    var
      i: longint;
      e: TcExecuteResultSet;
    begin
      if parObject <> nil then
      begin
        //
        // enField
        for i := 0 to parObject.Count - 1 do
          if (parObject[i] <> nil) and (parObject[i] is TcMetaData) and ((parObject[i] as TcMetaData).eType = enFIELD) then
            m_objExecute.FieldWidth[parObject[i].sName] := strtointdef((parObject[i] as TcMetaData).Attribute[krsWIDTH], kiUNDEFINED);
        //
        // enSQL
        if parObject.eType = enSQL then
        begin
          e := [erRecordSetWithData];
          if parItem > 0 then
            e := e + [erAddToResult];
          if parObject.HasAttribute[krsVISIBLE] and (uppercase(parObject.Attribute[krsVISIBLE]) <> krsTRUE) then
            e := e + [erDisplayNoResult];
          m_objExecute.Execute(trim(AnsiReplaceStr(parObject.sValue, '[SQL]', value)), m_objDisplay, e);
        end
        else for i := 0 to parObject.Count - 1 do
          if (parObject[i] <> nil) and (parObject[i] is TcMetaData) then
            AnalyzeSQLItem(parObject[i] as TcMetaData, value, m_objDisplay, parItem);
      end;
    end;

  var
    objStmt: TcStatementList;
    i: longint;
  begin
    objStmt := nil;
    try
      objStmt := TcStatementList.Create(nil);
      try
        if (((pos(kcPARAGRAPH, value) > 0) and objStmt.Parse(estParagraph, value)) or objStmt.Parse(estSemicolon, value)) and (objStmt.count > 0) then
          for i := 0 to objStmt.Count - 1 do
            AnalyzeSQLItem(parObject, (objStmt[i] as TcStatement).SQL, m_objDisplay, i);
      except
        //
      end;
    finally
      objStmt.Free;
    end;
  end;

var
  t, w: String;
  p: TObject;
  m: TcMetaData;
  L: longint;
  r: TrStmtPos;
begin
  m_objIntelliSense.Close(Sender);
  if CheckConnection(TRUE) then
  try
    CursorStack.Push(crAppStart);
    p := nil;
    if not (GetDisplayMode in [eqtText, eqtGrid]) then
      pgDisplay.ActivePage := tsText;
    case GetDisplayMode of
      eqtText:
        p := mmoQueryResult;
      eqtGrid:
        p := dbListView;
    end;
    r := GetCurrentStatement(estSemicolon);
    //
    // Regular SQL Statement
    try
      m_objExecute.Clear;
      m_objExecute.FetchType := StringToFetchType(Formset.MetaData.Option[krsOPTION_FETCHTYPE]);
      dbListView.Clear;
      if not m_bAnalyze then
      begin
        // Timer Display
        m_iTimeStart := GetTickCount();
        getExecutionTime(0);
        timerExecution.Enabled := TRUE;
        //
        w := WordByIndex(r.SQL, 1, L);
        //
        // Describe Command ?
        if (AnsiCompareText(w, krsCON_DESC) = 0) or (AnsiCompareText(w, krsCON_DESCRIBE) = 0) then
        begin
          // Get the Object Name
          t := WordByIndex(r.SQL, 2, L);
          // Execute the describe command
          if t <> ksEMPTY then
          begin
            m := FormSet.MetaData.Find(enFEATURE, krsCON_DESCRIBE) as TcMetaData;
            if (m <> nil) and (m.Find(enSQL, ksEMPTY) <> nil) then
            begin
              t := Format(m.Find(enSQL, ksEMPTY).sValue, [t]);
              if t <> ksEMPTY then
                r.SQL:= t;
            end;
          end;
        end;
        //
        // Regular processingg continued
        m_objExecute.IsAsynchronous := FormSet.Support[efsAsynchronous];
        if (FormSet <> nil) and (FormSet.Preferences <> nil) then
          m_objExecute.Preferences := FormSet.Preferences;
        m_objExecute.fctProgress := SetTab;
        m_objExecute.iLineOffset := r.iLineStart;
        m_objExecute.IsReadOnly := TRUE;
        if p = dbListView then
          m_objExecute.IsReadOnly := dbListView.IsReadOnly;
        m_objExecute.Execute(r.SQL, p);
        // Spooling?
        if (m_sSpoolFileName <> ksEMPTY) and (p = mmoQueryResult) then
          LogToFile(m_sSpoolFileName, trim(r.SQL) + ksCR + ksCR + mmoQueryResult.Text + ksCR);
      end
      else
      //
      // Statement is analyzed
        AnalyzeSQL(MetaData.Find(enFEATURE, krsANALYZE) as TcMetaData, r.SQL, p);
      mmoQueryResult.Lines.Append(m_objExecute.Error);
      if m_objConnection.bAutoCommit and (m_objConnection.eMode = ecmMDAC) then
        onCommit(nil);
      //
      // Reset Highlight
      SetTab(r.iLineStart, r.iLineEnd, epmScript);
    except
      if m_objConnection.bAutoCommit and (m_objConnection.eMode = ecmMDAC) then
        onRollback(nil);
    end;
  finally
    CursorStack.Pop;
    getExecutionTime((GetTickCount() - m_iTimeStart) / 1000);
    timerExecution.Enabled := FALSE;
  end;
  SetState(Sender);
end;

// TfrmQuery
//   OnExecuteBlock
//
procedure TfrmQuery.OnExecuteBlock(Sender: TObject);
var
  p: TObject;
  r: TrStmtPos;
begin
  m_objIntelliSense.Close(Sender);
  if CheckConnection(TRUE) then
  try
    CursorStack.Push(crAppStart);
    p := nil;
    if not (GetDisplayMode in [eqtText, eqtGrid]) then
      pgDisplay.ActivePage := tsText;
    case GetDisplayMode of
      eqtText:
        p := mmoQueryResult;
      eqtGrid:
        p := dbListView;
    end;
    r := GetCurrentStatement(estSemicolon);
    //
    // Regular SQL Statement
    try
      m_objExecute.Clear;
      dbListView.Close;
      m_objExecute.IsAsynchronous := FormSet.Support[efsAsynchronous];
      if (FormSet <> nil) and (FormSet.Preferences <> nil) then
        m_objExecute.Preferences := FormSet.Preferences;
      m_objExecute.fctProgress := SetTab;
      m_objExecute.iLineOffset := r.iLineStart;
      m_objExecute.IsReadOnly := TRUE;
      if p = dbListView then
        m_objExecute.IsReadOnly := dbListView.IsReadOnly;
      m_objExecute.Execute(r.SQL, p);
      // Spooling?
      if (m_sSpoolFileName <> ksEMPTY) and (p = mmoQueryResult) then
        LogToFile(m_sSpoolFileName, trim(r.SQL) + ksCR + ksCR + mmoQueryResult.Text + ksCR);
      mmoQueryResult.Lines.Append(m_objExecute.Error);
      if m_objConnection.bAutoCommit and (m_objConnection.eMode = ecmMDAC) then
        onCommit(nil);
      //
      // Reset Highlight
      SetTab(r.iLineStart, r.iLineEnd, epmScript);
    except
      if m_objConnection.bAutoCommit and (m_objConnection.eMode = ecmMDAC) then
        onRollback(nil);
    end;
  finally
    CursorStack.Pop;
  end;
  SetState(Sender);
end;

// TfrmQuery
//   SetButtons
//
procedure TfrmQuery.SetButtons(eparState: TeQueryButtonSet);
begin
  m_eQueryState := eparState;
  SetState(nil);
  SetAnimation((ebsStop in eparState) or (ebsAnimate in eparState));
end;

// TfrmQuery
//   ClipboardAction
//
procedure TfrmQuery.ClipboardAction(Sender: TObject);
begin
  SetState(Sender);
  if (ActiveControl <> nil) and (ActiveControl is TSynEdit) then
  begin
    if sender = actEditCopy then // CTRL-C
      (ActiveControl as TSynEdit).CopyToClipBoard
    else if sender = actEditCut then // CTRL-X
      (ActiveControl as TSynEdit).CutToClipBoard
    else if (sender = actEditPaste) and (ActiveControl = mmoSQL) then // CTRL-V
      (ActiveControl as TSynEdit).PasteFromClipBoard
    else if (sender = actEditUndo) then // CTRL-Z
      (ActiveControl as TSynEdit).Undo
    else if (sender = actEditRedo) then //
      (ActiveControl as TSynEdit).Redo
    else if (sender = actEditSelectAll) then // CTRL-A
      (ActiveControl as TSynEdit).SelectAll;
    (ActiveControl as TSynEdit).Refresh;
  end
  else if (ActiveControl <> nil) and (ActiveControl is TdbListView) then
  begin
    if sender = actEditCopy then // CTRL-C
      ClipBoard.AsText := (ActiveControl as TdbListView).CellText;
  end;
end;

// TfrmQuery
//   ExecuteItem
//
function TfrmQuery.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
var
  L: longint;
begin
  mmoSQL.Text := trim(mmoSQL.Text) + ksCR;
  mmoSQL.Refresh;
  L := length(mmoSQL.Text);
  Value := trim(TextToText(XMLToText(Value)));
  result := value <> ksEMPTY;
  if result then
  begin
    if (pos(kcPARAGRAPH, value) = 0) and (value[length(value)] <> ';') then
      value := value + ';';
    mmoSQL.Text := mmoSQL.Text + Value;
    mmoSQL.SelStart := L;
    mmoSQL.SelLength := length(value);
    if not (eqoPaste in eparOptionSet) then
      onStart(nil);
  end;
end;

// TfrmQuery
//   ExecuteItem
//
function TfrmQuery.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  result := ExecuteItem(value.SQL, eparOptionSet);
end;

// TfrmQuery
//   onAutoCommit
//
procedure TfrmQuery.onAutoCommit(Sender: TObject);
begin
  if m_objConnection <> nil then
    m_objConnection.bAutoCommit := not m_objConnection.bAutoCommit;
  SetState(Sender);
end;

// TfrmQuery
//   SetState
//
procedure TfrmQuery.SetState(Sender: TObject);
begin
  actActionStart.enabled        := ebsStart in m_eQueryState;
  actActionStop.enabled         := ebsStop in m_eQueryState;
  actActionPause.enabled        := ebsPause in m_eQueryState;
  actActionAutoCommit.Enabled   := (eDisplayType <> eqtGrid) and (FormSet <> nil) and FormSet.Support[efsTransaction];
  actActionAutoCommit.Checked   := actActionAutoCommit.Enabled and (m_objConnection <> nil) and m_objConnection.bAutoCommit;
  actActionCommit.Enabled       := not actActionAutoCommit.Checked and (m_objConnection <> nil) and m_objConnection.InTransaction;
  actActionRollback.Enabled     := actActionCommit.Enabled;
  actActionReportOutput.Checked := GetDisplayMode = eqtGrid;
  actActionReportOutput.Enabled := (FormSet <> nil) and (FormSet.Preferences <> nil) and FormSet.Preferences.IsLicensed;
  btnReportFormat.Enabled       := actActionReportOutput.Enabled;
  actActionAnalyze.enabled      := actActionStart.enabled and
                                   (FormSet <> nil) and
                                   (FormSet.MetaData.Find(enFEATURE, krsANALYZE) <> nil) and
                                   (FormSet.Preferences <> nil) and
                                   FormSet.Preferences.IsLicensed;
  actActionExecuteToOutput.enabled := actActionStart.enabled;
  actActionExecuteBlock.Enabled := (ebsStart in m_eQueryState) and (ActiveControl = mmoSQL) and (mmoSQL.SelText <> ksEMPTY);
  actEditCut.Enabled            := (ActiveControl <> nil) and
                                   (ActiveControl is TSynEdit) and
                                   not (ActiveControl as TSynEdit).ReadOnly and
                                   ((ActiveControl as TSynEdit).SelText <> ksEMPTY);
  actEditCopy.Enabled           := (ActiveControl <> nil) and
                                   (
                                    (
                                     (ActiveControl is TSynEdit) and
                                     ((ActiveControl as TSynEdit).SelText <> ksEMPTY)
                                    ) or
                                    (
                                     (ActiveControl is TdbListView) and
                                     (ActiveControl as TdbListView).CanClipboard
                                    )
                                   );
  actEditPaste.Enabled          := (ActiveControl = mmoSQL);
  actEditSelectAll.Enabled      := (ActiveControl <> nil) and (ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).Text <> ksEMPTY);
  actEditUndo.Enabled           := (ActiveControl = mmoSQL) and (ActiveControl as TSynEdit).CanUndo;
  actEditRedo.Enabled           := (ActiveControl = mmoSQL) and (ActiveControl as TSynEdit).CanRedo;
  actEditFind.Enabled           := (ActiveControl <> nil) and (ActiveControl is TSynEdit);
  actEditFindNext.Enabled       := actEditFind.Enabled and m_seSearchReplace.CanFindNext;
  actEditFindPrev.Enabled       := actEditFind.Enabled and m_seSearchReplace.CanFindPrev;
  actEditReplace.Enabled        := ActiveControl = mmoSQL;
  actActionSpool.Enabled        := eDisplayType <> eqtGrid;
  actEditLowerCase.Enabled      := (ActiveControl <> nil) and (ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).SelText <> ksEMPTY);
  actEditUpperCase.Enabled      := actEditLowerCase.Enabled;
  actEditReadonly.Checked       := (GetDisplayMode = eqtGrid) and dbListView.IsReadOnly;
  actEditReadonly.Enabled       := dbListView.Support[efsEdit];
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
    actActionMemorize.Enabled   := actActionStart.Enabled and not (FormSet.Preferences.StringVal[krsPREF_CONSOLEMEMORIZEALLSTATEMENTS] = krsTRUE);
  if (FormSet <> nil) then
  begin
    actCodeCompletionEnable.Checked := FormSet.bCodeCompletion;
    mnuCodeCompletionEnable.Checked := FormSet.bCodeCompletion;
  end;
  // Why, I don't know...
  btnActionCommit.Enabled := actActionCommit.Enabled;
  btnActionRollback.Enabled := actActionRollback.Enabled;
  // Hack
  btnFindNext.Enabled := actEditFindNext.Enabled;
  btnFindPrev.Enabled := actEditFindPrev.Enabled;
  btnCut.Enabled := actEditCut.Enabled;
  btnCopy.Enabled := actEditCopy.Enabled;
  btnPaste.Enabled := actEditPaste.Enabled;
  // Object Structure
  actObjectStructureListColumns.Enabled := (ebsStart in m_eQueryState) and (ActiveControl = mmoSQL) and (mmoSQL.SelText <> ksEMPTY);
  mnuObjectStructure.Enabled := actObjectStructureListColumns.Enabled;
  popObjectStructure.Enabled := actObjectStructureListColumns.Enabled;
  //
  Toolbar.Refresh;
end;

// TfrmQuery
//   onCommit
//
procedure TfrmQuery.onCommit(Sender: TObject);
begin
  if (m_objConnection <> nil) and FormSet.Support[efsTransaction] and (eDisplayType <> eqtGrid) and CheckConnection(TRUE) and m_objConnection.InTransaction then
    with m_objConnection do
    try
      CommitTrans;
      if Sender <> nil then
        Application.MessageBox('Transaction Commit Successful.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
      BeginTrans;
    except
      //
    end;
  SetState(Sender);
end;

// TfrmQuery
//   onRollback
//
procedure TfrmQuery.onRollback(Sender: TObject);
begin
  if (m_objConnection <> nil) and
     CheckConnection(TRUE) and
     m_objConnection.InTransaction and
     ((Sender = nil) or (Application.MessageBox('Rollback Transaction?', krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = idYES)) then
  with m_objConnection do
  try
    RollbackTrans;
    if Sender <> nil then
      Application.MessageBox('Transaction Rollback Successful.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
    BeginTrans;
  except
    //
  end;
  SetState(Sender);
end;

// TfrmQuery
//   SetFont
//
procedure TfrmQuery.SetFont(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    mmoSQL.Font.Assign(FormSet.Preferences.Font[krsPREF_FONTCONSOLE]);
    mmoQueryResult.Font.Assign(FormSet.Preferences.Font[krsPREF_FONTCONSOLE]);
    dbListView.Font.Assign(FormSet.Preferences.Font[krsPREF_FONTCONSOLEGRID]);
    dbListView.Invalidate;
  end;
end;

// TfrmQuery
//   SetAnimation
//
procedure TfrmQuery.SetAnimation(value: boolean);
begin
  //aniBusy.Active := Value;
  //aniBusy.Visible := Value;
  Application.ProcessMessages;
end;

// TfrmQuery
//   onPreferenceChange
//
procedure TfrmQuery.onPreferenceChange(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    // Visual Elements
    mmoSQL.WordWrap := FormSet.Preferences.StringVal[krsPREF_CONSOLEWRAPTEXT] = krsTRUE;
    mmoQueryResult.WordWrap := FormSet.Preferences.StringVal[krsPREF_CONSOLEOUTPUTWRAPTEXT] = krsTRUE;
    dbListView.BackgroundColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDBACKGROUNDCOLOR];
    dbListView.SelectedColumnColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTEDCOLUMNCOLOR];
    dbListView.SelectionColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTIONCOLOR];
    dbListView.CurrentLineColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDSELECTEDLINECOLOR];
    dbListView.NullCellColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDNULLCELLCOLOR];
    pnlProgress.Brush[epmExecute] := FormSet.Preferences.Color[krsPREF_CONSOLEEXCUTIONTAB];
    pnlProgress.Brush[epmScript] := FormSet.Preferences.Color[krsPREF_CONSOLESCRIPTTAB];
    mmoSQL.Color := FormSet.Preferences.Color[krsPREF_CONSOLEBACKGROUNDCOLOR];
    if FormSet.Preferences.StringVal[krsPREF_CONSOLESHOWSPECIALCHARS] = krsTRUE then
      mmoSQL.Options := mmoSQL.Options + [eoShowSpecialChars]
    else
      mmoSQL.Options := mmoSQL.Options - [eoShowSpecialChars];
    // SQL Statement Memorized
    if m_objConnection <> nil then
    begin
      if FormSet.Preferences.StringVal[krsPREF_CONSOLEMEMORIZEALLSTATEMENTS] = krsTRUE then
        m_objConnection.Options := m_objConnection.Options + [ecoMemorize]
      else
        m_objConnection.Options := m_objConnection.Options - [ecoMemorize];
    end;
  end;
  mmoQueryResult.Color := mmoSQL.Color;
  if (FormSet.Preferences.StringVal[krsPREF_CONSOLEENABLECURRENTLINEBACKGROUND] <> krsFALSE) then
  begin
    mmoSQL.ActiveLineColor := FormSet.Preferences.Color[krsPREF_CONSOLECURRENTLINEBACKROUND];
    mmoQueryResult.ActiveLineColor := mmoSQL.ActiveLineColor;
  end;
  SetFont(Sender);
  // Intellisense
  m_objIntelliSense.BackgroundColor := FormSet.Preferences.Color[krsPREF_CONSOLEINTELLISENSECOLOR];
  m_objIntelliSense.IsTimed := FormSet.Preferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISS] <> krsFALSE;
  m_objIntelliSense.TimerValue := strtointdef(FormSet.Preferences.StringVal[krsPREF_CONSOLEINTELLISENSEDISMISSVALUE], kiINTELLLISENSECONTROL_TIMER);
  timerIntelliSense.Interval := FormSet.Preferences.IntegerVal[krsPREF_CONSOLEINTELLISENSEDELAY];
  //
  SetState(Sender);
end;

// TfrmQuery
//   onAnalyze
//
procedure TfrmQuery.onAnalyze(Sender: TObject);
begin
  try
    m_bAnalyze := (FormSet.Preferences <> nil) and FormSet.Preferences.IsLicensed;
    OnStart(sender);
  finally
    m_bAnalyze := FALSE;
  end;
end;

// TfrmQuery
//   GetDisplayMode
//
function TfrmQuery.GetDisplayMode: TeQueryType;
begin
  result := eqtText;
  if pgDisplay.ActivePage = tsText then
    result := eqtText
  else if pgDisplay.ActivePage = tsGrid then
    result := eqtGrid
  else if pgDisplay.ActivePage = tsHint then
    result := eqtAnalysis;
end;

// TfrmQuery
//   pgDisplayChange
//
procedure TfrmQuery.pgDisplayChange(Sender: TObject);
begin
  eDisplayType := GetDisplayMode;
  case eDisplayType of
    eqtGrid:
      actActionStop.onExecute := dbListView.onStop;
    eqtText:
      actActionStop.onExecute := m_objExecute.onStop;
  end;
  SetDisplayType(eDisplayType);
end;

// TfrmQuery
//   onReportOutput
//
procedure TfrmQuery.onReportOutput(Sender: TObject);
const
  kaeTYPES: array[TeQueryType] of TeQueryType = (eqtGrid, eqtGrid, eqtText, eqtGrid);
begin
  if tsGrid.TabVisible then
    case kaeTYPES[GetDisplayMode] of
      eqtGrid:
        pgDisplay.ActivePage := tsGrid;
      eqtText:
        pgDisplay.ActivePage := tsText;
    end
  else
    pgDisplay.ActivePage := tsText;
  actActionReportOutput.Checked := GetDisplayMode = eqtGrid;
end;

// TfrmQuery
//   SetDisplayType
//
procedure TfrmQuery.SetDisplayType(value: TeQueryType);
begin
  inherited SetDisplayType(value);
  // Transaction State
  if FormSet.Support[efsTransaction] then
  begin
    if (value = eqtGrid) and m_objConnection.InTransaction then
      m_objConnection.CommitTrans
    else if (value = eqtText) and not m_objConnection.InTransaction then
      m_objConnection.BeginTrans;
  end;
  dbListView.Clear;
  mmoQueryResult.Text := ksEMPTY;
  // Visibles
  SetState(nil);
end;

// TfrmQuery
//   onListViewMessage
//
procedure TfrmQuery.onListViewMessage(Sender: TObject; value: String);
begin
  StatusText([psPanel1], value, esiGridBottom);
end;

// TfrmQuery
//   onSpool
//
procedure TfrmQuery.onSpool(Sender: TObject);
begin
  if not actActionSpool.Checked and dlgSpool.Execute then
  begin
    m_sSpoolFileName := dlgSpool.FileName;
    actActionSpool.Checked := TRUE;
  end
  else
  begin
    actActionSpool.Checked := FALSE;
    m_sSpoolFileName := ksEMPTY;
  end;
end;

// TfrmQuery
//   mnuQueriesClick
//
procedure TfrmQuery.mnuFavoritesClick(Sender: TObject);
begin
  // Fill in menu
  Formset.Favorites.SendToObject(mnuFavorites);
  // Enabling
  actFavoritesAdd.Enabled := (TextToXML(GetCurrentStatement(estSemicolon).SQL) <> ksEMPTY) and
                              Formset.Preferences.IsLicensed;
end;

// TfrmQuery
//   mnuQueriesAddClick
//
procedure TfrmQuery.mnuFavoritesAddClick(Sender: TObject);
var
  s: String;
begin
  if Formset.Preferences.IsLicensed then
  begin
    s := InputBox('Add Saved Query', 'Please specify a ''Saved Query'' heading', ksEMPTY);
    if (s <> ksEMPTY) and (TextToXML(GetCurrentStatement(estSemicolon).SQL) <> ksEMPTY) then
    begin
      FormSet.Favorites.Add(s, TextToXML(GetCurrentStatement(estSemicolon).SQL));
      FormSet.Favorites.Save;
    end;
  end;
end;

// TfrmQuery
//   actFavoritesEditClick
//
procedure TfrmQuery.actFavoritesEditClick(Sender: TObject);
begin
  if Formset.Preferences.IsLicensed then
    FormSet.Favorites.Edit;
end;

// TfrmQuery                                                                                                         `
//   onSelMove
//
procedure TfrmQuery.onSelMove(Sender: TObject);
var
  e: TeStatusIcon;
begin
  try
    SetState(Sender);
    if (eFormState >= efmInitialize) and (Sender <> nil) and (Sender is TSynEdit) then
      with (Sender as TSynEdit) do
      begin
        if Sender = ActiveControl then
        begin
          e := esiNone;
          if Sender = mmoSQL then
            e := esiTextTop
          else if Sender = mmoQueryResult then
            e := esiTextBottom;
          StatusText([psPanel1], Format(krsSTATUS_COLLINE, [CaretY, CaretX]), e);
        end;
      end
    else if (eFormState >= efmInitialize) and (Sender <> nil) and (Sender is TdbListView) and (Sender = ActiveControl) then
      (Sender as TdbListView).SendDisplayMessage(Sender);
  except
    on E:Exception do
      ;
  end;
end;

// TfrmQuery                                                                                                         `
//   OnEditStatusChange
//
procedure TfrmQuery.OnEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
 if [scCaretX, scCaretY] * Changes <> [] then
   onSelMove(Sender);
end;

// TfrmQuery
//   onFind
//
// Search: ShowSearchReplaceDialog(FALSE);
// Search Next: DoSearchReplaceText(FALSE, FALSE);
// Search Prev: DoSearchReplaceText(FALSE, TRUE);
// Search Replace: ShowSearchReplaceDialog(TRUE);
//
procedure TfrmQuery.onFind(Sender: TObject);
begin
  m_seSearchReplace.ShowSearchReplaceDialog(ActiveControl, FALSE);
  SetState(Sender);
end;

// TfrmQuery
//   onFindNext
//
procedure TfrmQuery.onFindNext(Sender: TObject);
begin
  m_seSearchReplace.DoSearchReplaceText(ActiveControl, FALSE, FALSE);
  SetState(Sender);
end;

// TfrmQuery
//   onFindPrev
//
procedure TfrmQuery.onFindPrev(Sender: TObject);
begin
  m_seSearchReplace.DoSearchReplaceText(ActiveControl, FALSE, TRUE);
  SetState(Sender);
end;

// TfrmQuery
//   onReplace
//   Note. Fixed infinite loop if replace text is empty.
//
procedure TfrmQuery.onReplace(Sender: TObject);
begin
  m_seSearchReplace.ShowSearchReplaceDialog(ActiveControl, TRUE);
  SetState(Sender);
end;

// TfrmQuery
//   onUpperCase
//
procedure TfrmQuery.onUpperCase(Sender: TObject);
var
  S, L: longint;
begin
  if (ActiveControl <> nil) and (ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).SelText <> ksEMPTY) then
    with ActiveControl as TSynEdit do
    begin
      S := SelStart;
      L := SelLength;
      SelText := UpperCase(SelText);
      SelStart := S;
      SelLength := L;
    end;
end;

// TfrmQuery
//   onLowerCase
//
procedure TfrmQuery.onLowerCase(Sender: TObject);
var
  S, L: longint;
begin
  if (ActiveControl <> nil) and (ActiveControl is TSynEdit) and ((ActiveControl as TSynEdit).SelText <> ksEMPTY) then
    with ActiveControl as TSynEdit do
    begin
      S := SelStart;
      L := SelLength;
      SelText := LowerCase(SelText);
      SelStart := S;
      SelLength := L;
    end;
end;

// TfrmQuery
//   OnInsertParagraphMark
//
procedure TfrmQuery.OnInsertParagraphMark(Sender: TObject);
begin
  if (ActiveControl <> nil) and (ActiveControl is TSynEdit) then
    with ActiveControl as TSynEdit do
      SelText := SelText + kcPARAGRAPH;
end;

// TfrmQuery
//   CheckConnection
//
function TfrmQuery.CheckConnection(bparConnect: boolean): boolean;
begin
  result := inherited CheckConnection(bparConnect);
  if result and (m_objConnection <> nil) and not m_objConnection.Connected then
    InitializeConnection;
end;

// TfrmQuery
//   onMemorize
//
procedure TfrmQuery.onMemorize(Sender: TObject);
begin
  Formset.Memorize(GetCurrentStatement(estSemicolon).SQL);
end;

// TfrmQuery
//   OnActionExecuteToOutput
//
procedure TfrmQuery.OnActionExecuteToOutput(Sender: TObject);
var
  s: String;
begin
  try
    CursorStack.Push(crHourGlass);
    s := GetCurrentStatement(estSemicolon).SQL;
    if s <> ksEMPTY then
      Formset.OnDataDump(s);
  finally
    CursorStack.Pop;
  end;
end;

// TfrmQuery
//   OnFileOpen
//
procedure TfrmQuery.OnFileOpen(Sender: TObject);
begin
  try
    CursorStack.Push(crHourGlass);
    dlgOpenTextFile.FileName := m_sFileName;
    if dlgOpenTextFile.Execute then
    begin
      m_sFileName := dlgOpenTextFile.FileName;
      mmoSQL.Text := FileToString(m_sFileName, ecsAnsi);
      mmoSQL.Refresh;
      FormSet.AddLRU(m_sFileName);
      SetLRUMenu(onFileReOpen);
    end;
  finally
    CursorStack.Pop;
  end;
end;

// TfrmQuery
//   onFileReOpen
//
procedure TfrmQuery.onFileReOpen(Sender: TObject);
var
  p: TcObject;
begin
  if (sender <> nil) and (Sender is TMenuItem) and ((Sender as TMenuItem).Caption <> ksEMPTY) then
  try
    CursorStack.Push(crHourGlass);
    p := TcObject((Sender as TMenuItem).Tag);
    m_sFileName := p.sName;
    mmoSQL.Text := FileToString(m_sFileName, ecsAnsi);
    mmoSQL.Refresh;
    FormSet.AddLRU(m_sFileName);
    SetLRUMenu(onFileReOpen);
  finally
    CursorStack.Pop;
  end;
end;

// TfrmQuery
//   OnFileSave
//
procedure TfrmQuery.OnFileSave(Sender: TObject);
begin
  try
    CursorStack.Push(crHourGlass);
    dlgSaveTextFile.FileName := m_sFileName;
    if dlgSaveTextFile.Execute then
    begin
      m_sFileName := dlgSaveTextFile.FileName;
      StringToFile(m_sFileName, mmoSQL.Text, ecsAnsi);
      FormSet.AddLRU(m_sFileName);
      SetLRUMenu(onFileReOpen);
    end;
  finally
    CursorStack.Pop;
  end;
end;

// TfrmQuery
//   spltrQueryMoved
//
procedure TfrmQuery.spltrQueryMoved(Sender: TObject);
begin
  m_rRatio := spltrQuery.Top / Height;
end;

// TfrmQuery
//   FormResize
//
procedure TfrmQuery.FormResize(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) and (FormSet.Preferences.StringVal[krsPREF_CONSOLERESIZEPROPORTIONALLY] = krsTRUE) then
    mmoSQL.Height := round(Height * m_rRatio);
end;

// TfrmQuery
//   SetTab
//
procedure TfrmQuery.SetTab(parLineStart, parLineEnd: longint; parMode: TeProgressMode);
begin
  pnlProgress.iLineHeight := mmoSQL.LineHeight;
  pnlProgress.iTopOrigin := mmoSQL.TopLine;
  pnlProgress.iStart := parLineStart;
  pnlProgress.iEnd := parLineEnd;
  pnlProgress.eMode := parMode;
  pnlProgress.Repaint;
end;

// TfrmQuery
//   OnReadOnly
//
procedure TfrmQuery.OnReadOnly(Sender: TObject);
begin
  dbListView.IsReadOnly := not dbListView.IsReadOnly;
  SetState(sender);
end;

// TfrmQuery
//   OnDisplayFieldExit
//
procedure TfrmQuery.OnDisplayFieldExit(Sender: TObject);
begin
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmQuery
//   GetLocalToolbar
//
function TfrmQuery.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmQuery
//   mmoSQLClick
//
procedure TfrmQuery.mmoSQLClick(Sender: TObject);
var
  r: TrStmtPos;
begin
  m_objIntelliSense.Close(Sender);
  r := GetCurrentStatement(estSemicolon);
  SetTab(r.iLineStart, r.iLineEnd, epmScript);
  timerIntelliSense.Enabled := m_objIntelliSense.CanSense;
  SetState(sender);
end;

// TfrmQuery
//   mmoSQLScroll
//
procedure TfrmQuery.mmoSQLScroll(Sender: TObject; ScrollBar: TScrollBarKind);
begin
  // Progress Bar
  mmoSQLChange(Sender);
  // IntelliSense
  if m_objIntelliSense.Visible then
    m_objIntelliSense.Display;
end;

// TfrmQuery
//   pnlTopResize
//
procedure TfrmQuery.pnlTopResize(Sender: TObject);
begin
  if m_objIntelliSense.Visible then
    m_objIntelliSense.Display;
end;

// TfrmQuery
//   mmoSQLChange
//
procedure TfrmQuery.mmoSQLChange(Sender: TObject);
var
  r: TrStmtPos;
begin
  r := GetCurrentStatement(estSemicolon);
  SetTab(r.iLineStart, r.iLineEnd, epmScript);
{$IFDEF SBS_DEBUG}
  StatusText([psPanel2], Format('%d objects', [gMemList.Count]), esiNone);
{$ENDIF}
end;

// TfrmQuery
//   tmrCodeCompletionTimer
//
procedure TfrmQuery.timerIntelliSenseTimer(Sender: TObject);
var
  r: TrStmtPos;
  s: string;
  b: boolean;
const
  kabICON: array[boolean] of integer = (-1, 2);
begin
  timerIntelliSense.Enabled := FALSE;
  if (FormSet <> nil) and
     FormSet.bCodeCompletion and
     m_objIntelliSense.CanDisplay(m_iLastKey) and
     FormSet.Preferences.IsLicensed then
  begin
    r := GetCurrentStatement(estSemicolon);
    m_objContextStmt.ForeCast(system.copy(r.SQL, 1, mmoSQL.SelStart - r.iStart), r.iStart);
    s := m_objContextStmt.GetHTML([ectStatement], b);
    tsHint.Tag := kabICON[b];
    SetBrowserContent(wbAnalysis, s);
    pgDisplay.Refresh;
  end;
end;

// TfrmQuery
//   mmoSQLKeyDown
//
procedure TfrmQuery.mmoSQLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  m_iLastKey := Key;
  m_objIntelliSense.FilterKeyPress(Key, Shift, ekDown);
end;

// TfrmQuery
//   mmoSQLKeyUp
//
procedure TfrmQuery.mmoSQLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  r: TrStmtPos;
begin
  m_objIntelliSense.FilterKeyPress(Key, Shift, ekUp);
  r := GetCurrentStatement(estSemicolon);
  SetTab(r.iLineStart, r.iLineEnd, epmScript);
  timerIntelliSense.Enabled := m_objIntelliSense.CanSense and (key in [VK_LEFT, VK_RIGHT, VK_TAB, VK_RETURN, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_SPACE, VK_OEM_COMMA, VK_OEM_PERIOD]);
end;

// TfrmQuery
//   actCodeCompletionExecute
//
procedure TfrmQuery.actCodeCompletionEnableExecute(Sender: TObject);
begin
  if (FormSet <> nil) then
    FormSet.bCodeCompletion := not FormSet.bCodeCompletion;
  SetState(Sender);
end;

// TfrmQuery
//   tvListChange
//
procedure TfrmQuery.tvListChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    mmoSQL.SelStart := TisStatement(Node.Data).iStart;
    with TisStatement(Node.Data) do
      mmoSQL.SelLength := iEnd - iStart + 1;
  end;
end;

// TfrmQuery
//   actCodeCompletionRefreshExecute
//
procedure TfrmQuery.actCodeCompletionRefreshExecute(Sender: TObject);
var
  d: TDateTime;
begin
  if CheckConnection(TRUE) then
  try
    CursorStack.Push(crAppStart);
    d := now;
    if m_objTableList.Load(FormSet.Connection) and m_objIndexList.Load(FormSet.Connection) then
      FormSet.SetLog(ksEMPTY, Format('Code Completion Support: %d Table%s, %d Index%s', [m_objTableList.Count, HasS(m_objTableList.Count), m_objIndexList.Count, HasS(m_objIndexList.Count, 'es')]), now - d, []);
    m_objContextStmt.TableList := m_objTableList;
    m_objContextStmt.IndexList := m_objIndexList;
  finally
    CursorStack.Pop;
  end;
  SetState(Sender);
end;

// TfrmQuery                                                                                                         `
//   onShowColumnList
//
procedure TfrmQuery.onShowColumnList(Sender: TObject);
begin
  m_objIntelliSense.Display(ettColumn, ksEMPTY, nil);
end;

// TfrmQuery                                                                                                         `
//   onShowTableList
//
procedure TfrmQuery.onShowTableList(Sender: TObject);
begin
  m_objIntelliSense.Display(ettTable, ksEMPTY, nil);
end;

// TfrmQuery                                                                                                         `
//   actObjectStructureListColumnsExecute
//
procedure TfrmQuery.actObjectStructureListColumnsExecute(Sender: TObject);
begin
  m_objIntelliSense.ObjectStructure_ColumnList(mmoSQL.SelText);
end;

// TfrmQuery                                                                                                         `
//   pgDisplayDrawTab
//
procedure TfrmQuery.pgDisplayDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  r: TRect;
  p: TSize;
  L, N: longint;
  ts: TTabSheet;
begin
  r := Rect;
  control.Canvas.brush.Color := clBtnFace;
  control.Canvas.FillRect(r);
  //InflateRect(r, -2, -2);
  control.Canvas.brush.Color := clBtnFace;
  control.Canvas.FillRect(r);
  ts := TPageControl(Control).Pages[TabIndex];
  p := control.Canvas.TextExtent(ts.Caption);
  L := ts.Tag;
  N := 0;
  if L <> kiUNDEFINED then
  begin
    ts.Caption := trim(ts.Caption) + '      ';
    N := imgAnalysis.Width + 1;
  end
  else
    ts.Caption := trim(ts.Caption);
  control.Canvas.TextRect(r,
                          r.Left + max(0, ((r.Right - r.Left) - p.cX) div 2) + N,
                          r.Top + max(0, ((r.Bottom - r.Top) - p.cY) div 2) + 2,
                          ts.Caption);
  if L <> kiUNDEFINED then
    imgAnalysis.Draw(control.Canvas, r.Left + 3, r.Top + 2, L);
end;

// TfrmQuery
//   WebBrowser1NavigateComplete2 Method
//
procedure TfrmQuery.wbAnalysisNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  hr: HResult;
  CustDoc: ICustomDoc;
begin
  if m_FDocHostUIHandler <> nil then
  begin
    hr := wbAnalysis.Document.QueryInterface(ICustomDoc, CustDoc);
    if hr = S_OK then
      CustDoc.SetUIHandler(m_FDocHostUIHandler);
  end;
end;

// TfrmQuery
//   onMasterHelp
//
procedure TfrmQuery.onMasterHelp(Sender: TObject);
begin
  FormSet.GetOnlineHelp(ksEMPTY);
end;

// TfrmQuery
//   timerExecutionTimer
//
procedure TfrmQuery.timerExecutionTimer(Sender: TObject);
begin
  getExecutionTime(trunc((GetTickCount() - m_iTimeStart) / 1000));
end;

// TfrmQuery
//   getExecutionTime
//
procedure TfrmQuery.getExecutionTime(value: double);
var
  s: String;
begin
  s := ksEMPTY;
  if value > 0 then
    s := format('%.2d:%.2d:%.2d', [trunc(value) div 3600, (trunc(value) mod 3600) div 60, trunc(value) mod 60]);
  if value - trunc(value) <> 0 then
    s := s + format('.%.3d', [trunc((value - trunc(value)) * 1000)]);
  StatusText([psPanel2], s, esiNone);
  Application.ProcessMessages;
end;

// TfrmQuery
//   ReConnect
//
function TfrmQuery.ReConnect: boolean;
begin
  FinalizeConnection;
  FreeAndNil(m_objConnection);
  result := InitializeConnection; //   result := (m_objConnection <> nil) and m_objConnection.Reconnect;
end;

end.


