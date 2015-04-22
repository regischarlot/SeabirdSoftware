unit Main;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls,
  ToolWin, ImgList, daGlobals, PreferenceLib, AppEvnts, daObjectLib, StdActns,
  ActnList, Grid, frmMemorizedSQL, frmHelp;

type
  TfrmMain = class(TForm)
    dlgPrint: TPrintDialog;
    dlgPrintSetup: TPrinterSetupDialog;
    dlgOpen: TOpenDialog;
    StatusLine: TStatusBar;
    appEvents: TApplicationEvents;
    AlertImageList: TImageList;
    ControlBar: TControlBar;
    btnToolSchema: TToolButton;
    btnConsole: TToolButton;
    btnGrid: TToolButton;
    btnSession: TToolButton;
    ActionList: TActionList;
    actFileClose: TWindowClose;
    actFileExit: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actWindowCascade: TWindowCascade;
    actWindowTileHorizontal: TWindowTileHorizontal;
    actWindowTileVertical: TWindowTileVertical;
    actWindowMinimizeAll: TWindowMinimizeAll;
    actWindowArrangeAll: TWindowArrange;
    actHelpAbout: TAction;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileConnect: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuFileN1: TMenuItem;
    mnuFilePreferences: TMenuItem;
    mnuFileN2: TMenuItem;
    mnuFilePrint: TMenuItem;
    mnuFilePrintSetup: TMenuItem;
    mnuFileN3: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    N1: TMenuItem;
    mnuEditRefresh: TMenuItem;
    mnuWindow: TMenuItem;
    mnuWindowCascade: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowTileItem2: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpContents: TMenuItem;
    mnuHelpSearch: TMenuItem;
    mnuHelpHowToUse: TMenuItem;
    N3: TMenuItem;
    mnuHelpAbout: TMenuItem;
    actFileConnect: TAction;
    actFilePreferences: TAction;
    actFilePrintSetup: TFilePrintSetup;
    actFilePrint: TAction;
    actEditUndo: TEditUndo;
    actEditSelectAll: TEditSelectAll;
    actEditRefresh: TAction;
    mnuView: TMenuItem;
    actViewNavigationSQLLog: TAction;
    mnuViewSQLLog: TMenuItem;
    ToolBar1: TToolBar;
    btnConnect: TToolButton;
    actToolSchema: TAction;
    actToolConsole: TAction;
    actToolGrid: TAction;
    actToolSession: TAction;
    cboFormList: TComboBoxEx;
    ToolButton_1: TToolButton;
    HeaderImageList: TImageList;
    actViewNavigationFind: TAction;
    mnuViewFindObject: TMenuItem;
    actFileNew: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    btnSave: TToolButton;
    mnuFileSave: TMenuItem;
    actViewNavigationMemorizedSQL: TAction;
    mnuViewMemorizedSQLs: TMenuItem;
    ImageList: TImageList;
    ImageList3: TImageList;
    ToolButton_2: TToolButton;
    ToolButton1: TToolButton;
    actFileOpen: TAction;
    Open1: TMenuItem;
    imgColLine: TImageList;
    btnDatabaseSpy: TToolButton;
    actToolLog: TAction;
    actFileReOpen: TAction;
    mnuFileN4: TMenuItem;
    btnOnlineHelp: TToolButton;
    actOnlineHelp: TAction;
    actFileReconnect: TAction;
    mnuFileReConnect: TMenuItem;

    procedure   OnConnect(Sender: TObject);
    procedure   OnReconnect(Sender: TObject);
    procedure   OnPrint(Sender: TObject);
    procedure   mnuFilePrintSetupClick(Sender: TObject);
    procedure   OnExit(Sender: TObject);
    procedure   HelpContents(Sender: TObject);
    procedure   HelpSearch(Sender: TObject);
    procedure   HelpHowToUse(Sender: TObject);
    procedure   OnPreferences(Sender: TObject);
    procedure   SetMainMenu(eMenu: TeMenu; bEnabled, bVisible: boolean; onEvent: TNotifyEvent);
    procedure   OnHelpAbout(Sender: TObject);
    procedure   SetProgressBar(bparVisible: boolean; iValue, iMax: longint);
    procedure   SetMenuState(eMenu: TeMenu; bEnabled: boolean);
    procedure   StatusLineResize(Sender: TObject);
    procedure   FormCreate(Sender: TObject);
    procedure   FormDestroy(Sender: TObject);
    procedure   StatusLineDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const aRect: TRect);
    procedure   appEventsHint(Sender: TObject);
    procedure   FormShow(Sender: TObject);
    procedure   FormClose(Sender: TObject; var Action: TCloseAction);
    procedure   mnuViewClick(Sender: TObject);
    procedure   OnShowToolWindow(Sender: TObject);
    procedure   StatusText(eparPanelSet: TePanelSet; value: String; eIcon: TeStatusIcon);
    procedure   SetFormFocus(Sender: TObject);
    procedure   cboFormListClick(Sender: TObject);
    procedure   ButtonClick(Sender: TObject);
    procedure   SetState(parFormSet: TcObject);
    procedure   SetLRUMenu(parFormSet: TcObject; value: TNotifyEvent);
    procedure   CheckMDACVersion(sender:TObject);
    function    GetInformation(caption, value: string): boolean;
    procedure   DiamissInformationForm(sender: TObject);

  private
    m_objPreferences: TcPreferenceList;
    m_iProgress, m_iMaxProgress: longint;
    m_objFormSetList: TcObject;
    m_frmLog: TForm;
    m_frmFind: TForm;
    m_frmMemorizedSQL: TfrmMemorizedSQL;
    m_eStatusIcon: TeStatusIcon;
    m_frmHelp: TfrmHelp;

  public
    property    Preferences: TcPreferenceList      read m_objPreferences;
    property    FindForm: TForm                    read m_frmFind;
    property    LogForm: TForm                     read m_frmLog;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math,
  Types,
  MemorizedSQLLib,
  strUtils,
  ConnectionLib,        // TcConnection
  daResourceStrings,
  FormLib,              // TcFormSet
  frmSQLLog,            // TfrmSQLLog
  frmDataFind,          // TfrmDataFind
  frmAbout,             // TfrmAbout
  frmWebService;        // TTfrmWebService

type
  TMyControl = class(TCustomControl)
  public
    property Canvas;
  end;

{$R *.DFM}

// TfrmMain
//   FormCreate
//
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  m_objPreferences := TcPreferenceList.Create(nil);

  m_iProgress := kiUNDEFINED;
  m_iMaxProgress := kiUNDEFINED;
  m_objFormSetList := TcFormSetList.Create(nil);
  m_eStatusIcon := esiNone;
  Application.ShowHint := TRUE;
  width := strtointdef(m_objPreferences.StringVal[krsWINDOWWIDTH], width);
  height := strtointdef(m_objPreferences.StringVal[krsWINDOWHEIGHT], height);
  if strtointdef(m_objPreferences.StringVal[krsWINDOWSTATE], 0) = 1 then
    WindowState := wsMaximized;
  // Startup count
  with m_objPreferences do
    IntegerVal[krsPREF_STARTUPCOUNT] := IntegerVal[krsPREF_STARTUPCOUNT] + 1;
  // Form List Management
  (m_objFormSetList as TcFormSetList).hdlFormFocus := SetFormFocus;
  // SQLLog
  m_frmLog := TfrmSQLLog.Create(self);
  (m_frmLog as TfrmSQLLog).Preferences := m_objPreferences;
  (m_objFormSetList as TcFormSetList).frmLog := m_frmLog as TfrmSQLLog;
  // Data Find
  m_frmFind := TfrmDataFind.Create(self);
  (m_frmFind as TfrmDataFind).Preferences := m_objPreferences;
  (m_objFormSetList as TcFormSetList).frmFind := m_frmFind as TfrmDataFind;
  // Memorized SQL
  m_frmMemorizedSQL := TfrmMemorizedSQL.Create(self);
  (m_frmMemorizedSQL as TfrmMemorizedSQL).Preferences := m_objPreferences;
  (m_frmMemorizedSQL as TfrmMemorizedSQL).Data := (m_objFormSetList as TcFormSetList).objMemorizedSQL;
  (m_objFormSetList as TcFormSetList).frmMemory := m_frmMemorizedSQL;
  StatusText([psPanel1], ksEMPTY, esiNone);
  m_frmHelp := nil;
  //
  Application.ProcessMessages;
end;

// TfrmMain
//   FormClose
//
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: longint;
begin
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].Close;
  Application.ProcessMessages;
  Action := caFree;
end;

// TfrmMain
//   FormDestroy
//
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  m_frmLog.Close;
  m_frmFind.Close;
  m_frmMemorizedSQL.Close;
  m_objFormSetList.free;
  if WindowState <> wsMaximized then
  begin
    m_objPreferences.StringVal[krsWINDOWWIDTH] := inttostr(width);
    m_objPreferences.StringVal[krsWINDOWHEIGHT] := inttostr(Height);
  end;
  m_objPreferences.StringVal[krsWINDOWSTATE] := inttostr(longint(WindowState = wsMaximized));
  m_objPreferences.WriteToRegistry;
  m_objPreferences.free;
  m_objPreferences := nil;
end;

// TfrmMain
//   FormShow
//
procedure TfrmMain.FormShow(Sender: TObject);
var
  frm: TfrmWebService;
begin
  Application.ProcessMessages;
  frm := nil;
  // Check License key
  if m_objPreferences.LicenseLevel = ellNone then
  try
    frm := TfrmWebService.Create(self);
    frm.Preferences := m_objPreferences;
    frm.ShowModal;
  finally
    frm.release;
  end;
  if m_objPreferences.LicenseLevel = ellNone then
    Application.Terminate
{$IFNDEF SBS_DEBUG}
  else with TfrmAbout.Create(self) do
  begin
    Preferences := m_objPreferences;
    Show;
  end
{$ENDIF};
  m_frmLog.Visible := Preferences.StringVal[krsPREF_SQLLOG_VISIBLE] = krsTRUE;
  m_frmMemorizedSQL.Visible := Preferences.StringVal[krsPREF_MEMORIZEDSQL_VISIBLE] = krsTRUE;
  m_frmFind.Visible := FALSE;
  CheckMDACVersion(sender);
  // GetInformation('http://www.seabirdsoftware.com/ReleaseNotes.html');
  SetFormFocus(nil);
end;

// TfrmMain
//   OnConnect
//
procedure TfrmMain.OnConnect(Sender: TObject);
var
  f: TcFormSet;
  e: TeForm;
begin
  try
    screen.Cursor := crHourGlass;
    f := TcFormSet.Create(m_objFormSetList);
    f.ImageList := ImageList;
    f.HeaderImageList := HeaderImageList;
    f.AlertImageList := AlertImageList;
    f.Preferences := m_objPreferences;
    f.frmLog := m_frmLog;
    if f.Connection.Select then
    begin
      Application.ProcessMessages;
      m_objFormSetList.Add(f);
      e := f.Connection.eForm;
      if not f.Allowed(e) then
        e := efsQuery;
      f.CreateForm(e, self, efiFromDatabase, ksEMPTY);
    end
    else
      f.free;
  finally
    screen.Cursor := crDefault;
  end;
end;

// TfrmMain
//   OnPrint
//
procedure TfrmMain.OnPrint(Sender: TObject);
begin
  if dlgPrint.Execute then
  begin
    { Add code to print current file }
  end;
end;

// TfrmMain
//   mnuFilePrintSetupClick
//
procedure TfrmMain.mnuFilePrintSetupClick(Sender: TObject);
begin
  dlgPrintSetup.Execute;
end;

// TfrmMain
//   OnExit
//
procedure TfrmMain.OnExit(Sender: TObject);
begin
  Close;
end;

// TfrmMain
//   HelpContents
//
procedure TfrmMain.HelpContents(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

// TfrmMain
//   HelpSearch
//
procedure TfrmMain.HelpSearch(Sender: TObject);
const
  EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

// TfrmMain
//   HelpHowToUse
//
procedure TfrmMain.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

// TfrmMain
//   OnPreferences
//
procedure TfrmMain.OnPreferences(Sender: TObject);
var
  e: TeForm;
begin
  e := efsObjects;
  if (ActiveMDIChild <> nil) and (ActiveMDIChild is TcForm) then
    e := (ActiveMDIChild as TcForm).eForm;
  if m_objPreferences.Edit(e) and (m_objFormSetList <> nil) and (m_objFormSetList is TcFormSetList) then
    (m_objFormSetList as TcFormSetList).onPreferenceChange(Sender);
end;

// TfrmMain
//   SetMainMenu
//
procedure TfrmMain.SetMainMenu(eMenu: TeMenu; bEnabled, bVisible: boolean; onEvent: TNotifyEvent);

  procedure SetHandler(Widget: TObject);
  begin
    if (Widget <> nil) and (Widget is TToolButton) then
    begin
      (Widget as TToolButton).Enabled := bEnabled and Assigned(onEvent);
      (Widget as TToolButton).onClick := onEvent;
    end
    else if (Widget <> nil) and (Widget is TMenuItem) then
    begin
      (Widget as TMenuItem).Enabled := bEnabled and Assigned(onEvent);
      (Widget as TMenuItem).onClick := onEvent;
      (Widget as TMenuItem).Visible := bVisible;
    end
    else if (Widget <> nil) and (Widget is TAction) then
    begin
      (Widget as TAction).Enabled := bEnabled and Assigned(onEvent);
      (Widget as TAction).onExecute := onEvent;
      (Widget as TAction).Visible := bVisible;
    end;
  end;

begin
  bEnabled := bEnabled and Assigned(onEvent);
  if not bEnabled then
    onEvent := nil;
  case eMenu of
    mmSave:
      SetHandler(actFileSave);
    mmSaveAs:
      SetHandler(actFileSaveAs);
    mmClose:
      SetHandler(actFileClose);
    mmNew:
      SetHandler(actFileNew);
    mmOpen:
      SetHandler(actFileOpen);
    mmReOpen:
      SetHandler(actFileReOpen);
  end;
end;

// TfrmMain
//   SetMenuState
//
procedure TfrmMain.SetMenuState(eMenu: TeMenu; bEnabled: boolean);
begin
  case eMenu of
    mmSave:
      actFileSave.Enabled := bEnabled;
    mmSaveAs:
      actFileSaveAs.Enabled := bEnabled;
    mmClose:
      actFileClose.Enabled := bEnabled;
    mmNew:
      actFileNew.Enabled := bEnabled;
    mmOpen:
      actFileOpen.Enabled := bEnabled;
    mmReOpen:
      actFileReOpen.Enabled := bEnabled;
  end;
end;

// TfrmMain
//   OnHelpAbout
//
procedure TfrmMain.OnHelpAbout(Sender: TObject);
begin
  with TfrmAbout.Create(self) do
  begin
    Preferences := m_objPreferences;
    ShowModal;
  end;
end;

// TfrmMain
//   SetProgressBar
//
procedure TfrmMain.SetProgressBar(bparVisible: boolean; iValue, iMax: longint);
begin
  //StatusLine.Panels[3].Style := psOwnerDraw;
  if not bparVisible then
    m_iMaxProgress := kiUNDEFINED
  else
  begin
    m_iMaxProgress := iMax;
    m_iProgress := iValue;
  end;
  StatusLine.Refresh;
end;

// TfrmMain
//   StatusLineResize
//
procedure TfrmMain.StatusLineResize(Sender: TObject);
var
  L: longint;
begin
  L := max(StatusLine.width - 200, 200);
  StatusLine.Panels[0].width := L div 2;
  StatusLine.Panels[1].width := L - StatusLine.Panels[0].width;
end;

// TfrmMain
//   StatusLineDrawPanel
//
procedure TfrmMain.StatusLineDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const aRect: TRect);
var
  L: longint;
begin
  if (m_iMaxProgress <= 0) then
    with StatusBar.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(aRect);
      L := aRect.Left;
      if Panel.Index = 3 then
        inc(L, imgColLine.Width + 3);
      TextRect(aRect, L, aRect.Top, Panel.Text);
      if (m_eStatusIcon <> esiNone) and (Panel.Index = 3) then
        imgColLine.Draw(StatusBar.Canvas, aRect.Left + 1, aRect.Top + 1, longint(m_eStatusIcon) - 1, dsNormal, itImage);
    end
    else
    with StatusBar.Canvas do
    begin
      L := round(((aRect.Right - aRect.Left + 1) * m_iProgress) / m_iMaxProgress);
      Brush.Color := clHighlight;
      FillRect(Rect(aRect.Left, aRect.Top, aRect.Left + L, aRect.Bottom));
      Brush.Color := clBtnFace;
      FillRect(Rect(aRect.Left + L + 1, aRect.Top, aRect.Right, aRect.Bottom));
    end;
end;

// TfrmMain
//   appEventsHint
//
procedure TfrmMain.appEventsHint(Sender: TObject);
begin
  StatusLine.Panels[0].Text := ' ' + Application.Hint;
end;

// TfrmMain
//   mnuViewClick
//
procedure TfrmMain.mnuViewClick(Sender: TObject);
begin
  actViewNavigationSQLLog.Checked := (m_frmLog <> nil) and m_frmLog.Visible;
  actViewNavigationFind.Checked := (m_frmFind <> nil) and m_frmFind.Visible;
  actViewNavigationMemorizedSQL.Checked := (m_frmMemorizedSQL <> nil) and m_frmMemorizedSQL.Visible;
end;

// TfrmMain
//   OnShowToolWindow
//
procedure TfrmMain.OnShowToolWindow(Sender: TObject);
var
  f: TForm;
begin
  f := nil;
  if sender = actViewNavigationSQLLog then
    f := m_frmLog
  else if sender = actViewNavigationFind then
    f := m_frmFind
  else if sender = actViewNavigationMemorizedSQL then
    f := m_frmMemorizedSQL;
  if f <> nil then
  begin
    f.Visible := not f.Visible;
    // Palette location check
    if f.Top < 0 then
      f.Top := 0;
    if f.Top + f.Height > Screen.Height then
      f.Top := Screen.Height - f.Height;
    if f.Left < 0 then
      f.Left := 0;
    if f.Left + f.Width > Screen.Width then
      f.Left := Screen.Width - f.Width;
  end;
  //
  // Memorized SQL case
  if (f <> nil) and (f is TfrmMemorizedSQL) then
  begin
    (f as TfrmMemorizedSQL).Display(mrDisplayCombo);
    (f as TfrmMemorizedSQL).Display(mrDisplayList);
  end;
end;

// TfrmMain
//   StatusText
//
procedure TfrmMain.StatusText(eparPanelSet: TePanelSet; value: String; eIcon: TeStatusIcon);
begin
  if psPanel1 in eparPanelSet then
    with StatusLine.Panels[3] do
    begin
      Text := value;
      m_eStatusIcon := eIcon;
      Refresh;
    end;
  if psPanel2 in eparPanelSet then
    StatusLine.Panels[1].Text := ' ' + value;
end;

// TfrmMain
//   SetState
//
procedure TfrmMain.SetState(parFormSet: TcObject);
var
  b: boolean;
begin
  // Enabled status
  with cboFormList do
    Enabled := ItemsEx.Count > 0;
  b := cboFormList.Enabled and (parFormSet <> nil) and ((parFormSet as TcFormSet).MetaData <> nil);
  actFileReconnect.Enabled := parFormSet <> nil;
  actToolSchema.Enabled  := b;
  actToolConsole.Enabled := b;
  actToolGrid.Enabled    := b;
  actToolSession.Enabled := b;
  actToolLog.Enabled     := b and (parFormSet as TcFormSet).MetaData.HasFeature[efsLogParser];
  actOnlineHelp.Enabled  := b;
  // Checked Status
  actToolSchema.Checked  := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsObjects] and ((parFormSet as TcFormSet).eForm = efsObjects));
  actToolConsole.Checked := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsQuery] and ((parFormSet as TcFormSet).eForm = efsQuery));
  actToolGrid.Checked    := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsGrid] and ((parFormSet as TcFormSet).eForm = efsGrid));
  actToolSession.Checked := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsSessionManager] and ((parFormSet as TcFormSet).eForm = efsSessionManager));
  actToolLog.Checked     := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsLogParser] and ((parFormSet as TcFormSet).eForm = efsLogParser));
  actOnlineHelp.Checked  := b and ((parFormSet as TcFormSet).MetaData.HasFeature[efsOnlineHelp] and ((parFormSet as TcFormSet).eForm = efsOnlineHelp));
end;

// TfrmMain
//   SetFormFocus
//
procedure TfrmMain.SetFormFocus(Sender: TObject);

  procedure OnRefresh(Sender: TObject);
  var
    i, L: longint;
  begin
    with cboFormList do
    begin
      ItemsEx.BeginUpdate;
      ItemsEx.Clear;
      if m_objFormSetList <> nil then
        for i := 0 to m_objFormSetList.Count - 1 do
          if (m_objFormSetList[i] <> nil) and (m_objFormSetList[i] is TcFormSet) then
          begin
            L := kaiFORMIMAGEINDEX[(m_objFormSetList[i] as TcFormSet).eForm];
            cboFormList.ItemsEx.AddItem((m_objFormSetList[i] as TcFormSet).Text, L, L, L, 0, m_objFormSetList[i]);
          end;
      ItemsEx.EndUpdate;
      Enabled := ItemsEx.Count > 0;
    end;
  end;

var
  p: TcObject;
  i: longint;
begin
  p := nil;
  OnRefresh(sender);
  if (Sender <> nil) and (Sender is TcForm) then
  begin
    p := (Sender as TcForm).FormSet;
    if p <> nil then
    begin
      if p is TcFormSet then
        (p as TcFormSet).eForm := (Sender as TcForm).eForm;
      for i := 0 to cboFormList.ItemsEx.Count - 1 do
        if cboFormList.ItemsEx[i].Data = p then
        begin
          cboFormList.ItemsEx[i].ImageIndex := kaiFORMIMAGEINDEX[(p as TcFormSet).eForm];
          cboFormList.ItemIndex := i;
          break;
        end;
      cboFormList.Invalidate;
    end;
  end;
  if (m_frmMemorizedSQL <> nil) and (m_frmMemorizedSQL is TfrmMemorizedSQL) then
    m_frmMemorizedSQL.SetFormFocus(sender);
  SetState(p);
end;

// TfrmMain
//   cboFormListClick
//
procedure TfrmMain.cboFormListClick(Sender: TObject);
var
  L: longint;
begin
  L := cboFormList.ItemIndex;
  if (m_objFormSetList <> nil) and (L < m_objFormSetList.Count) and (m_objFormSetList[L] <> nil) and (m_objFormSetList[L] is TcFormSet) then
    with m_objFormSetList[L] as TcFormSet do
      if FormList[eForm].Count > 0 then
        FormList[eForm][0].Show;
end;

// TfrmMain
//   ButtonClick
//
procedure TfrmMain.ButtonClick(Sender: TObject);
var
  p: TcObject;
begin
  if cboFormList.ItemIndex <> kiUNDEFINED then
  begin
    p := TcObject(cboFormList.Items.Objects[cboFormList.ItemIndex]);
    if (p <> nil) and (p is TcFormSet) and (Sender <> nil) and (Sender is TComponent) then
    begin
      (p as TcFormSet).onSwitchForm(TeForm((Sender as TComponent).Tag));
      SetState(p as TcFormSet);
    end;
  end;
end;

// TfrmMain
//   SetLRUMenu
//
procedure TfrmMain.SetLRUMenu(parFormSet: TcObject; value: TNotifyEvent);
var
  i: longint;
begin
  if (parFormSet <> nil) and (parFormSet is TcFormSet) then
    (parFormSet as TcFormSet).SetLRUMenu(mnuFile, value)
  else if (parFormSet = nil) then
    for i := mnuFile.Count - 1 downto 0 do
      if mnuFile[i].Tag <> 0 then
        mnuFile.Remove(mnuFile.Items[i]);
end;

// TfrmMain
//   CheckMDACVersion
//
procedure TfrmMain.CheckMDACVersion(sender: TObject);
var
  v1, v2: longint;
begin
  v1 := strtointdef(Item(ReadMADCVersion, '.', 0), 0);
  v2 := strtointdef(Item(ReadMADCVersion, '.', 1), 0);
  if not ((v1 > 2) or ((v1 = 2) and (v2 >= 7))) then
    Application.MessageBox(PChar(Format('dbAnalyst recommanded minimum Microsoft Data Access Components (MDAC/OLE DB) Version is 2.7. ' + ksCR + 'This computer currently runs version ''%s''. ' + ksCR + 'We do not guarantee system stability while accessing data.', [ReadMADCVersion])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
end;

// TfrmMain
//   GetInformation
//
function TfrmMain.GetInformation(caption, value: string): boolean;
begin
  if m_frmHelp = nil then
  begin
    m_frmHelp := TfrmHelp.Create(self);
    //m_frmHelp.Parent := self;
  end;
  if m_frmHelp <> nil then
  begin
    m_frmHelp.Visible := TRUE;
    m_frmHelp.Navigate(caption, value);
  end;
  result := (m_frmHelp <> nil) and m_frmHelp.Visible;
end;

// TfrmMain
//   DiamissInformationForm
//
procedure TfrmMain.DiamissInformationForm(sender: TObject);
begin
  m_frmHelp := nil;
end;

// TfrmMain
//   OnReconnect
//
procedure TfrmMain.OnReconnect(Sender: TObject);
var
  f: TcFormSet;
begin
  f := nil;
  if (ActiveMDIChild <> nil) and (ActiveMDIChild is TcForm) then
    f := (ActiveMDIChild as TcForm).FormSet;
  if (f <> nil) and
    (Application.MessageBox(PChar(Format('Reconnect to ''%s''?', [f.Text])), krsINFORMATION, MB_OKCANCEL + MB_ICONEXCLAMATION) = idOK) then
  begin
    if f.ReConnect then
      Application.MessageBox('Re-Connection successful.', krsINFORMATION, MB_OK + MB_ICONINFORMATION)
    else
      Application.MessageBox('Failed Re-Connection.', krsERROR, MB_OK + MB_ICONERROR);
  end;
  SetState(f);
end;

end.




