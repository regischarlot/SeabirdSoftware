unit frmLogParser;

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
  ExtDlgs, dfsSplitter, OleCtrls, SHDocVw,
  OracleExtensions_TLB,
  IEDocHostUIHandler;

type
  TfrmLogParser = class(TcForm)
    actObjectCreate: TAction;
    ToolBar: TToolBar;
    btnRefresh: TToolButton;
    btnEnableTrace: TToolButton;
    popBrowser: TPopupMenu;
    popBrowserRefresh: TMenuItem;
    popSQLStatements: TPopupMenu;
    popSQLStatementsRefresh: TMenuItem;
    popBrowserCopyContent: TMenuItem;
    DataMainMenu: TMainMenu;
    mnuTools: TMenuItem;
    popSQLStatementsExecute: TMenuItem;
    btnExecute: TToolButton;
    pnlBottom: TPanel;
    lstSQLs: TListBox;
    dfsSplitter: TdfsSplitter;
    WebBrowser: TWebBrowser;
    dfsSplitter1: TdfsSplitter;
    pnlBottomLeft: TPanel;
    pnlBottomRight: TPanel;
    lblTraceLog: TStaticText;
    lblSQLStatement: TStaticText;
    popSessions: TPopupMenu;
    popSessionsRefresh: TMenuItem;
    ImageList: TImageList;
    btnDelete: TToolButton;
    popSessionsDelete: TMenuItem;
    ActionList1: TActionList;
    actSessionsRefresh: TAction;
    actDelete: TAction;
    actStmtExecute: TAction;
    actStmtRefresh: TAction;
    actSessionTrace: TAction;
    ToolButton1: TToolButton;
    popSessionEnableTrace: TMenuItem;
    actStmtCopy: TAction;
    actBrowserCopy: TAction;
    popSQLStatementsCopySQLStmt: TMenuItem;
    actSessionAutoTrace: TAction;
    ToolButton2: TToolButton;
    mnuAction: TMenuItem;
    mnuActionEnableTrace: TMenuItem;
    mnuActionDelete: TMenuItem;
    mnuActionSessionAutoTrace: TMenuItem;
    N1: TMenuItem;
    mnuActionExecute: TMenuItem;
    PageControl: TPageControl;
    tsActiveSessions: TTabSheet;
    tsTraceFiles: TTabSheet;
    lvSessions: TListView;
    lvTraceFiles: TListView;
    popLogFiles: TPopupMenu;
    popLogFilesRefresh: TMenuItem;
    popLogFilesDelete: TMenuItem;
    popBrowserCopySQLStatement: TMenuItem;
    popSQLStatementsCopyContent: TMenuItem;

    procedure   actBrowserCopyExecute(Sender: TObject);
    procedure   actStmtCopyExecute(Sender: TObject);
    procedure   OnSessionAutoTrace(Sender: TObject);
    procedure   OnDeleteEntry(Sender: TObject);
    procedure   OnEventSelectionChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure   OnExecuteClick(Sender: TObject);
    procedure   FormActivate(Sender: TObject); override;
    procedure   FormDeactivate(Sender: TObject); override;
    procedure   FormCreate(Sender: TObject);
    procedure   SetState(Sender: TObject);
    procedure   FormShow(Sender: TObject);
    procedure   FormDestroy(Sender: TObject);
    procedure   DisplayErrors(value: String);
    procedure   OnEnableTracing(Sender: TObject);
    procedure   OnChangeList(Sender: TObject);
    procedure   OnRefreshSQLList(Sender: TObject);
    procedure   OnSelectSQL(Sender: TObject);
    procedure   WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure   OnSessionList(Sender: TObject);
    procedure   OnSessionColumnClick(Sender: TObject; Column: TListColumn);

  private
    // Private members
    //
    m_ST: ISessionTrace;
    m_bEnabledTraceAction: boolean;
    m_FDocHostUIHandler: TDocHostUIHandler;
    m_iCurrentItem: TListItem;
    m_eEventType: TeSessionEventType;
    m_iEventID: longint;

  protected
    // Protected Declarations
    //
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // Private Methods
    //
    procedure   ClearWebBrowser;
    procedure   SetAnimation(value: boolean);
    procedure   SetSessionList(value: TeSessionEventType);
    function    GetContent(parType: String): String;

  public
    // Public Methods
    //
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; override;
    function    Finalize: boolean; override;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; override;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; override;
  end;

implementation

uses
  ComObj,
  daResourceStrings,
  strUtils,
  ClipBrd,
  MSHTML,
  ActiveX,
  Variants,
  IEConst,
  frmLogParserAutoTrace;

const kaEVENTTYPE: array[TeSessionEventType] of String =
  (ksEMPTY, krsXML_SESSION, krsXML_FILE);

{$R *.DFM}

// TfrmLogParser
//   FormCreate
//
procedure TfrmLogParser.FormCreate(Sender: TObject);
begin
  try
    m_ST := CreateComObject(CLASS_SessionTrace) as ISessionTrace;
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar(Format('Error instantiating Database Spy Required COM Object: %s', [E.Message])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
      m_ST := nil;
    end;
  end;
  m_bEnabledTraceAction := TRUE;
  m_FDocHostUIHandler := TDocHostUIHandler.Create;
  m_FDocHostUIHandler.WebBrowser := WebBrowser;
  if GetWinVersion >= wvWinXP then
  begin
    lvSessions.DoubleBuffered := TRUE;
    lstSQLs.DoubleBuffered := TRUE;
  end;
  m_iCurrentItem := nil;
end;

// TfrmLogParser
//   FormDestroy
//
procedure TfrmLogParser.FormDestroy(Sender: TObject);
begin
  m_ST := nil;
end;

// TfrmLogParser
//   FormShow
//
procedure TfrmLogParser.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsActiveSessions;
  SetToolbarLocation(ControlBar, Toolbar);
end;

// TfrmLogParser
//   FormActivate
//
procedure TfrmLogParser.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  SetState(Sender);
end;

// TfrmLogParser
//   FormDeactivate
//
procedure TfrmLogParser.FormDeactivate(Sender: TObject);
begin
  inherited FormDeactivate(nil);
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmLogParser
//   Initialize
//
function TfrmLogParser.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
begin
  inherited Initialize(eparMode, eparParameter);
  ClearWebBrowser;
  FormSet.InitializeToolMenu(mnuTools);
  if m_ST <> nil then
  try
    CursorStack.Push(crAppStart);
    with FormSet.Connection do
      m_ST.Login[Format('<xml>' +
                          '<%s>%s</%s>' + // krsXML_USERNAME
                          '<%s>%s</%s>' + // krsXML_PASSWORD
                          '<%s>%s</%s>' + // krsXML_DATASOURCE
                          '<%s>%s</%s>' + // krsXML_LOGHOST
                          '<%s>%s</%s>' + // krsXML_LOGHOSTTYPE
                          '<%s>%s</%s>' + // krsXML_LOGUSERNAME
                          '<%s>%s</%s>' + // krsXML_LOGPASSWORD
                          '<%s>%s</%s>' + // krsXML_LOGDOMAIN
                          '<%s>%s</%s>' + // krsXML_LOGFILEPATH
                          '<%s>%s</%s>' + // krsXML_LOGSECUREFTP
                        '</xml>',
                      [krsXML_USERNAME,     Attribute[krsXML_USERNAME],       krsXML_USERNAME,
                       krsXML_PASSWORD,     Attribute[krsXML_PASSWORD],       krsXML_PASSWORD,
                       krsXML_DATASOURCE,   Attribute[krsXML_DATASOURCE],     krsXML_DATASOURCE,
                       krsXML_LOGHOST,      Attribute[krsXML_LOGHOST],        krsXML_LOGHOST,
                       krsXML_LOGHOSTTYPE,  Attribute[krsXML_LOGHOSTTYPE],    krsXML_LOGHOSTTYPE,
                       krsXML_LOGUSERNAME,  Attribute[krsXML_LOGUSERNAME],    krsXML_LOGUSERNAME,
                       krsXML_LOGPASSWORD,  Attribute[krsXML_LOGPASSWORD],    krsXML_LOGPASSWORD,
                       krsXML_LOGDOMAIN,    Attribute[krsXML_LOGDOMAIN],      krsXML_LOGDOMAIN,
                       krsXML_LOGFILEPATH,  GetFilePath(Application.ExeName), krsXML_LOGFILEPATH,
                       krsXML_LOGSECUREFTP, Attribute[krsXML_LOGSECUREFTP],   krsXML_LOGSECUREFTP
                      ])];
    SetSessionList(esetSession);
    SetSessionList(esetFile);
    SetState(nil);
  finally
    CursorStack.Pop;
  end;
  result := TRUE;
end;

// TfrmLogParser
//   Finalize
//
function TfrmLogParser.Finalize: boolean;
begin
  FormDeactivate(nil);
  m_FDocHostUIHandler.WebBrowser := nil;
  m_FDocHostUIHandler.Free;
  m_FDocHostUIHandler := nil;
  Application.ProcessMessages;
  result := TRUE;
end;

// TfrmLogParser
//   ExecuteItem (1)
//
function TfrmLogParser.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
begin
  result := FALSE;
end;

// TfrmLogParser
//   ExecuteItem (2)
//
function TfrmLogParser.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  result := FALSE;
end;

// TfrmLogParser
//   SetState
//
procedure TfrmLogParser.SetState(Sender: TObject);
begin
  actSessionTrace.Enabled       := (pageControl.ActivePage = tsActiveSessions) and (lvSessions.Selected <> nil);
  actDelete.Enabled             := ((pageControl.ActivePage = tsActiveSessions) and (lvSessions.Selected <> nil)) or ((PageControl.ActivePage = tsTraceFiles) and (lvTRaceFiles.Selected <> nil));
  actStmtExecute.Enabled        := (lvSessions.Selected <> nil) and (lstSQLs.ItemIndex <> kiUNDEFINED);
  actStmtRefresh.Enabled        := ((pageControl.ActivePage = tsActiveSessions) and (lvSessions.Selected <> nil)) or ((PageControl.ActivePage = tsTraceFiles) and (lvTRaceFiles.Selected <> nil));
  actSessionAutoTrace.Enabled   := TRUE;
  btnEnableTrace.Down           := btnEnableTrace.Tag <> 0;
  actSessionTrace.Checked       := btnEnableTrace.Down;
  mnuActionEnableTrace.Checked  := btnEnableTrace.Down;
  popSessionEnableTrace.Checked := btnEnableTrace.Down;
  Toolbar.Refresh;
end;

// TfrmLogParser
//   SetSessionList
//
procedure TfrmLogParser.SetSessionList(value: TeSessionEventType);
var
  s: String;
  p, q: OLEVariant;
  i, j, L, c: longint;
  li: TListItem;
const
  kaiICON: array[0 .. 2] of longint = (kiUNDEFINED, 4, 1);
begin
  if m_ST <> nil then
  try
    CursorStack.Push(crAppStart);
    m_bEnabledTraceAction := FALSE;
    case value of
      //
      // Session List
      esetSession:
      begin
        s := m_ST.SessionList[Format('<xml><type>%s</type></xml>', [kaEVENTTYPE[esetSession]])];
        DisplayErrors(s);
        lvSessions.Items.BeginUpdate;
        // Current Line?
        L := kiUNDEFINED;
        c := kiUNDEFINED;
        if lvSessions.Selected <> nil then
          c := longint(lvSessions.Selected.Data);
        // Empty, to repopulate
        lvSessions.Items.Clear;
        try
          p := CreateOLEObject('Microsoft.XMLDOM');
          if p.LoadXML(s) and (p.ChildNodes.length > 0) then
          begin
            q := p.ChildNodes.item[0];
            for i := 0 to q.ChildNodes.length - 1 do
              if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') <> 0 then
              begin
                j := strtointdef(GetXMLValue(q.ChildNodes.Item[i], krsXML_TRACEENABLED), 0);
                li := lvSessions.Items.Add;
                li.Caption := ksEMPTY;
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_SESSIONID));
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_DATE));
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_MACHINE));
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_SCHEMA));
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_TITLE));
                li.Data := pointer(strtointdef(GetXMLValue(q.ChildNodes.Item[i], krsXML_ID), kiUNDEFINED));
                li.ImageIndex := kaiICON[j];
                li.StateIndex := kaiICON[j];
                if c = longint(li.Data) then
                  L := i;
              end;
          end;
         p := unassigned;
        except
          on E: Exception do
            p := unassigned;
        end;
        lvSessions.ItemIndex := L;
        if lvSessions.Selected <> nil then
          lvSessions.Selected.MakeVisible(TRUE);
        m_iCurrentItem := lvSessions.Selected;
        lvSessions.Items.EndUpdate;
      end;
      //
      // File List
      esetFile:
      begin
        s := m_ST.SessionList[Format('<xml><type>%s</type></xml>', [kaEVENTTYPE[esetFile]])];
        DisplayErrors(s);
        lvTraceFiles.Items.BeginUpdate;
        // Current Line?
        L := kiUNDEFINED;
        c := kiUNDEFINED;
        if lvTraceFiles.Selected <> nil then
          c := longint(lvTraceFiles.Selected.Data);
        // Empty, to repopulate
        lvTraceFiles.Items.Clear;
        try
          p := CreateOLEObject('Microsoft.XMLDOM');
          if p.LoadXML(s) and (p.ChildNodes.length > 0) then
          begin
            q := p.ChildNodes.item[0];
            for i := 0 to q.ChildNodes.length - 1 do
              if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') <> 0 then
              begin
                li := lvTraceFiles.Items.Add;
                li.Caption := GetXMLValue(q.ChildNodes.Item[i], krsXML_DATE);
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_SIZE));
                li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_TITLE));
                li.Data := pointer(strtointdef(GetXMLValue(q.ChildNodes.Item[i], krsXML_ID), kiUNDEFINED));
                if c = longint(li.Data) then
                  L := i;
              end;
          end;
         p := unassigned;
        except
          on E: Exception do
            p := unassigned;
        end;
        lvTraceFiles.ItemIndex := L;
        if lvTraceFiles.Selected <> nil then
          lvTraceFiles.Selected.MakeVisible(TRUE);
        m_iCurrentItem := lvTraceFiles.Selected;
        lvTraceFiles.Items.EndUpdate;
      end;
    end;
  finally
    CursorStack.Pop;
    m_bEnabledTraceAction := TRUE;
  end;
end;

// TfrmLogParser
//   OnSessionList
//
procedure TfrmLogParser.OnSessionList(Sender: TObject);
begin
  if m_ST <> nil then
  try
    CursorStack.Push(crAppStart);
    // Rfresh Session/File List
    if PageControl.ActivePage = tsActiveSessions then
      SetSessionList(esetSession)
    else if PageControl.ActivePage = tsTraceFiles then
      SetSessionList(esetFile);
    // Refresh SQL List
    OnRefreshSQLList(Sender);
  finally
    CursorStack.Pop;
  end;
  SetState(Sender);
end;

// TfrmLogParser
//   OnEnableTracing
//
procedure TfrmLogParser.OnEnableTracing(Sender: TObject);
var
  s: String;
begin
  if (m_ST <> nil) and (m_eEventType = esetSession) then
  try
    CursorStack.Push(crAppStart);
    if (lvSessions.Selected <> nil) and m_bEnabledTraceAction then
    begin
      s := m_ST.SessionTraceState[Format('<xml><type>%s</type><index>%d</index><state>%d</state></xml>', [kaEVENTTYPE[m_eEventType], m_iEventID, btnEnableTrace.Tag])];
      DisplayErrors(s);
      if xmlPart(s, krsXML_STATE) <> '0' then
        btnEnableTrace.Tag := 1
      else
        btnEnableTrace.Tag := 0;
      OnSessionList(Sender);
      OnChangeList(Sender);
    end;
  finally
    CursorStack.Pop;
  end;
  SetState(Sender);
end;

// TfrmLogParser
//   OnEventSelectionChange
//
procedure TfrmLogParser.OnEventSelectionChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Sender <> nil) and (Sender is TListView) and (Item = (Sender as TListView).Selected) and (m_iCurrentItem <> (Sender as TListView).Selected) then
    OnChangeList(Sender);
end;

// TfrmLogParser
//   OnChangeSession
//
procedure TfrmLogParser.OnChangeList(Sender: TObject);
begin
  if (m_ST <> nil) and (((PageControl.ActivePage = tsActiveSessions) and (m_iCurrentItem <> lvSessions.Selected)) or ((PageControl.ActivePage = tsTraceFiles) and (m_iCurrentItem <> lvTraceFiles.Selected))) then
  try
    CursorStack.Push(crHourGlass);
    m_bEnabledTraceAction := FALSE;
    // Selection?
    m_eEventType := esetUndefined;
    m_iEventID := kiUNDEFINED;
    if PageControl.ActivePage = tsActiveSessions then
    begin
      lvTraceFiles.Selected := nil;
      m_eEventType := esetSession;
      m_iEventID := longint(lvSessions.Selected.Data);
    end
    else if PageControl.ActivePage = tsTraceFiles then
    begin
      lvSessions.Selected := nil;
      m_eEventType := esetFile;
      m_iEventID := longint(lvTraceFiles.Selected.Data);
    end;
    //
    // Check Box
    btnEnableTrace.Tag := m_ST.IsSessionTraced[Format('<xml><index>%d</index></xml>', [m_iEventID])];
    if m_iEventID <> kiUNDEFINED then
    begin
      // List Display
      OnRefreshSQLList(sender);
      // Clear HTML Screen
      ClearWebBrowser;
    end;
    // Current Selection
    if PageControl.ActivePage = tsActiveSessions then
      m_iCurrentItem := lvSessions.Selected
    else if PageControl.ActivePage = tsTraceFiles then
      m_iCurrentItem := lvTraceFiles.Selected;
  finally
    m_bEnabledTraceAction := TRUE;
    CursorStack.Pop;
  end;
  SetState(Sender);
end;

// TfrmLogParser
//   OnSelectSQL Method
//
procedure TfrmLogParser.OnSelectSQL(Sender: TObject);
begin
  SetBrowserContent(WebBrowser, GetContent('HTML'));
end;

// TfrmLogParser
//   ClearWebBrowser Method
//
procedure TfrmLogParser.ClearWebBrowser;
begin
  SetBrowserContent(WebBrowser, '<html><body></body></html>');
end;

// TfrmLogParser
//   WebBrowser1NavigateComplete2 Method
//
procedure TfrmLogParser.WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  hr: HResult;
  CustDoc: ICustomDoc;
begin
  if m_FDocHostUIHandler <> nil then
  begin
    hr := WebBrowser.Document.QueryInterface(ICustomDoc, CustDoc);
    if hr = S_OK then
      CustDoc.SetUIHandler(m_FDocHostUIHandler);
  end;
end;

// TfrmLogParser
//   DisplayErrors
//
procedure TfrmLogParser.DisplayErrors(value: String);
var
  s: string;
  p, q, r: OLEVariant;
  i, j: longint;
begin
  // Get Error Message
  s := ksEMPTY;
  try
    p := CreateOLEObject('Microsoft.XMLDOM');
    if p.LoadXML(value) and (p.ChildNodes.length > 0) then
    begin
      q := p.ChildNodes.item[0];
      for i := 0 to q.ChildNodes.length - 1 do
        if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') = 0 then
        begin
          r := q.ChildNodes.item[i];
          for j := 0 to r.ChildNodes.length - 1 do
            s := s + trim(GetXMLValue(r.ChildNodes.Item[j])) + ksCR;
        end;
    end;
    p := unassigned;
  except
    on E: Exception do
      p := unassigned;
  end;
  // Display Message
  if s <> ksEMPTY then
    Application.MessageBox(PChar(s), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
end;

// TfrmLogParser
//   OnRefreshSQLList Method
//
procedure TfrmLogParser.OnRefreshSQLList(Sender: TObject);
var
  s: String;
  p, q: OLEVariant;
  i, L: longint;
begin
  if m_ST <> nil then
  try
    SetAnimation(TRUE);
    //
    // SQL List
    lstSQLs.Items.BeginUpdate;
    lstSQLs.Items.Clear;
    if (m_eEventType <> esetUndefined) and (m_iEventID <> kiUNDEFINED) then
    try
      CursorStack.Push(crAppStart);
      s := m_ST.SessionEventList[Format('<xml><type>%s</type><index>%d</index></xml>', [kaEVENTTYPE[m_eEventType], m_iEventID])];
      DisplayErrors(s);
      try
        p := CreateOLEObject('Microsoft.XMLDOM');
        if p.LoadXML(s) and (p.ChildNodes.length > 0) then
        begin
          q := p.ChildNodes.item[0];
          for i := 0 to q.ChildNodes.length - 1 do
            if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') <> 0 then
            begin
              L := strtointdef(GetXMLValue(q.ChildNodes.Item[i], krsXML_ID), kiUNDEFINED);
              lstSQLs.Items.AddObject(Format('%d. %s', [L, system.copy(GetXMLValue(q.ChildNodes.Item[i], krsXML_TITLE), 1, 100)]), pointer(L));
            end;
        end;
       p := unassigned;
      except
        on E: Exception do
          p := unassigned;
      end;
    finally
      CursorStack.Pop;
    end;
    lstSQLs.Items.EndUpdate;
  finally
    SetAnimation(FALSE);
  end;
  SetState(Sender);
end;

// TfrmLogParser
//   GetLocalToolbar
//
function TfrmLogParser.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmLogParser
//   SetAnimation
//
procedure TfrmLogParser.SetAnimation(value: boolean);
begin
  //aniBusy.Active := Value;
  //aniBusy.Visible := Value;
  Application.ProcessMessages;
end;

// TfrmLogParser
//   OnExecuteClick
//
procedure TfrmLogParser.OnExecuteClick(Sender: TObject);
var
  s: String;
begin
  s := GetContent('SQL');
  if (s <> ksEMPTY) and (Application.MessageBox(PChar('Execute SQL Statement?' + ksCR + s), krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = idYES) then
    FormSet.ExecuteSQL(efsQuery, s, eqtUndefined, []);
end;

// TfrmLogParser
//   OnDeleteEntry Method
//
procedure TfrmLogParser.OnDeleteEntry(Sender: TObject);
var
  s: String;
  L: longint;
begin
  // Delete Session
  if (m_ST <> nil) then
  begin
    //
    // Sessions
    if (PageControl.ActivePage = tsActiveSessions) and (lvSessions.Selected <> nil) then
    begin
      L := longint(lvSessions.Selected.Index);
      if (L <> kiUNDEFINED) and (Application.MessageBox(PChar(Format('Delete Session ''%s''?', [lvSessions.Selected.SubItems[0]])), krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = idYES) then
      try
        CursorStack.Push(crHourGlass);
        s := m_ST.DeleteSession[Format('<xml><%s>%d</%s></xml>', [krsXML_INDEX, L, krsXML_INDEX])];
        OnSessionList(Sender);
        DisplayErrors(s);
      finally
        CursorStack.Pop;
      end;
    end
    //
    // Log Files
    else if (PageControl.ActivePage = tsTraceFiles) and (lvTraceFiles.Selected <> nil) then
    begin
      L := longint(lvTraceFiles.Selected.Index);
      if (L <> kiUNDEFINED) and (Application.MessageBox(PChar(Format('Delete Log File ''%s''?', [lvTraceFiles.Selected.SubItems[1]])), krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = idYES) then
      try
        CursorStack.Push(crHourGlass);
        if m_ST.DeleteLogTrace[Format('<xml><%s>%d</%s></xml>', [krsXML_INDEX, L, krsXML_INDEX])] then
          OnSessionList(Sender);
        DisplayErrors(s);
      finally
        CursorStack.Pop;
      end;
    end;
  end;
end;

// TfrmLogParser
//   OnSessionAutoTrace Method
//
procedure TfrmLogParser.OnSessionAutoTrace(Sender: TObject);
var
  frm: TfrmLogParserAutoTrace;
begin
  frm := nil;
  if m_ST <> nil then 
  try
    frm := TfrmLogParserAutoTrace.Create(nil);
    frm.ST := m_ST;
    frm.ShowModal;
  finally
    frm.Release;
    frm.Free;
  end;
end;

// TfrmLogParser
//   ListViewSort Method
//
function ListViewSort(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
begin
  if lParam > 0 then
  begin
    if (Item1.ListView as TListView).Columns[lParam].Tag > 0 then
      result := CompareText(Item1.SubItems[lParam - 1], Item2.SubItems[lParam - 1])
    else
      result := CompareText(Item2.SubItems[lParam - 1], Item1.SubItems[lParam - 1]);
  end
  else
  begin
    if (Item1.ListView as TListView).Columns[lParam].Tag > 0 then
      result := CompareText(Item1.Caption, Item2.Caption)
    else
      result := CompareText(Item2.Caption, Item1.Caption);
  end
end;

// TfrmLogParser
//   OnSessionColumnClick Method
//
procedure TfrmLogParser.OnSessionColumnClick(Sender: TObject; Column: TListColumn);
begin
  if (Sender <> nil) and (sender is TListView) then
  try
    CursorStack.Push(crAppStart);
    if Column.Tag = 0 then
      Column.Tag := 1;
    (Sender as TListView).CustomSort(@ListViewSort, Column.Index);
    Column.Tag := -Column.Tag;
  finally
    CursorStack.Pop;
  end;
end;

// TfrmLogParser
//   GetContent Method
//
function TfrmLogParser.GetContent(parType: String): String;
var
  L: longint;
begin
  result := ksEMPTY;
  if (m_ST <> nil) and (lstSQLs.ItemIndex <> kiUNDEFINED) then
  try
    CursorStack.Push(crAppStart);
    L := longint(lstSQLs.Items.Objects[lstSQLs.ItemIndex]);
    if L <> kiUNDEFINED then
      result := m_ST.SessionEventDetail[Format('<xml><%s>%d</%s><%s>%s</%s></xml>', [krsXML_INDEX, L, krsXML_INDEX, krsXML_REQUEST, parType, krsXML_REQUEST])];
  finally
    CursorStack.Pop;
  end;
  SetState(nil);
end;

// TfrmLogParser
//   actStmtCopyExecute Method
//
procedure TfrmLogParser.actStmtCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := GetContent('SQL');
end;

// TfrmLogParser
//   actStmtCopyExecute Method
//
procedure TfrmLogParser.actBrowserCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := GetContent('HTML');
end;

// TfrmLogParser
//   ReConnect
//
function TfrmLogParser.ReConnect: boolean;
begin
  result := TRUE;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize;
end.
