unit frmSessionManager;

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
  StdActns, ActnList, dbListView,
  dfsSplitter;

type
  TfrmSessionManager = class(TcForm)
    spltrQuery: TdfsSplitter;
    pnlTop: TPanel;
    lvSessions: TDBListView;
    ToolBar: TToolBar;
    btnRefresh: TToolButton;
    btnDeleteSession: TToolButton;
    pnlBottom: TPanel;
    lvSessionObjects: TDBListView;
    DataMainMenu: TMainMenu;
    mnuTools: TMenuItem;
    mnuAction: TMenuItem;
    mnuFavorites: TMenuItem;
    mnuFavoritesAdd: TMenuItem;
    mnuFavoritesEdit: TMenuItem;
    N3: TMenuItem;
    ActionList: TActionList;
    actFavoritesEdit: TAction;
    Timer: TTimer;
    actEditRefresh: TAction;
    actDeleteSession: TAction;
    PopupMenu: TPopupMenu;
    popRefresh: TMenuItem;
    popDeleteSession: TMenuItem;
    pnlBottomTop: TPanel;
    lblSessionObjects: TStaticText;

    procedure FormActivate(Sender: TObject); override;
    procedure FormDeactivate(Sender: TObject); override;
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure SetFont(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure onRefresh(Sender: TObject);
    procedure onQuery(parListView: TdbListView; parMetaData: TcMetaData);
    procedure lvSessionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure onDropLockClick(Sender: TObject);
    procedure SetValue;

  private
    // private methods
    //
    function    ExtractString(value: String): String;
    function    GetValue(value: String): String;
    function    FindMetaPointer(value: String): TcObject;
    procedure   BuildColumns(parListView: TdbListView; parMetaData: TcMetaData);

  protected
    // Protected Declarations
    //
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // private members
    //
    m_objExecute: TcExecute;
    m_bIsLocked: boolean;
    m_lstValue: TStringList;
    m_objSelected: TListItem;

  public
    // public methods
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
  daStreamLib;

{$R *.DFM}

// TfrmSessionManager
//   FormCreate
//
procedure TfrmSessionManager.FormCreate(Sender: TObject);
begin
  m_objExecute := TcExecute.Create(nil);
  m_objExecute.IsAsynchronous := FALSE;
  m_bIsLocked := FALSE;
  m_lstValue := TStringList.Create;
  m_objSelected := nil;
end;

// TfrmSessionManager
//   FormDestroy
//
procedure TfrmSessionManager.FormDestroy(Sender: TObject);
begin
  m_objExecute.free;
  m_objExecute := nil;
  m_lstValue.Free;
end;

// TfrmSessionManager
//   FormShow
//
procedure TfrmSessionManager.FormShow(Sender: TObject);
begin
  SetToolbarLocation(ControlBar, Toolbar);
end;

// TfrmSessionManager
//   FormActivate
//
procedure TfrmSessionManager.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  onRefresh(Sender);
  ActionList.State := asNormal;
end;

// TfrmSessionManager
//   FormDeactivate
//
procedure TfrmSessionManager.FormDeactivate(Sender: TObject);
begin
  inherited FormDeactivate(nil);
  ActionList.State := asSuspended;
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmSessionManager
//   Initialize
//
function TfrmSessionManager.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
var
  p: TcObject;
begin
  inherited Initialize(eparMode, eparParameter);
  result := FormSet.CheckConnection(TRUE);
  if result then
    m_objExecute.Connection := FormSet.Connection;
  FormSet.InitializeToolMenu(mnuTools);
  lvSessions.Connection := FormSet.Connection;
  lvSessionObjects.Connection := FormSet.Connection;
  // Build Sessions Columns
  p := FindMetaPointer(krsSESSION);
  if (p <> nil) and (p is TcMetaData) then
    BuildColumns(lvSessions, p as TcMetaData);
  // Build Session Object Columns
  p := FindMetaPointer(krsOBJECT);
  if (p <> nil) and (p is TcMetaData) then
    BuildColumns(lvSessionObjects, p as TcMetaData);
  // Populate Session with Data
  onRefresh(nil);
end;

// TfrmSessionManager
//   Finalize
//
function TfrmSessionManager.Finalize: boolean;
begin
  m_objExecute.Connection := nil;
  result := TRUE;
end;

// TfrmSessionManager
//   ExecuteItem
//
function TfrmSessionManager.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
begin
  result := TRUE;
end;

// TfrmSessionManager
//   ExecuteItem
//
function TfrmSessionManager.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  result := ExecuteItem(value.SQL, eparOptionSet);
end;

// TfrmSessionManager
//   onPreferenceChange
//
procedure TfrmSessionManager.onPreferenceChange(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    Timer.Interval := strtointdef(FormSet.Preferences.StringVal[krsPREF_SESSIONTIMEINTERVAL], 60) * 1000;
    Timer.Enabled := FormSet.Preferences.StringVal[krsPREF_SESSIONENABLEDTIMER] = krsTRUE;
    lvSessions.BackgroundColor           := FormSet.Preferences.Color[krsPREF_SESSIONGRIDBACKGROUNDCOLOR];
    lvSessions.SelectedColumnColor       := FormSet.Preferences.Color[krsPREF_SESSIONGRIDSELECTEDCOLUMNCOLOR];
    lvSessions.SelectionColor            := FormSet.Preferences.Color[krsPREF_SESSIONGRIDSELECTIONCOLOR];
    lvSessions.CurrentLineColor          := FormSet.Preferences.Color[krsPREF_SESSIONGRIDSELECTEDLINECOLOR];
    //lvSessions.CurrentLineColor        := FormSet.Preferences.Color[krsPREF_SESSIONCURRENTLINEBACKROUND];
    lvSessions.NullCellColor             := FormSet.Preferences.Color[krsPREF_SESSIONGRIDNULLCELLCOLOR];
    lvSessionObjects.BackgroundColor     := lvSessions.BackgroundColor;
    lvSessionObjects.SelectedColumnColor := lvSessions.SelectedColumnColor;
    lvSessionObjects.SelectionColor      := lvSessions.SelectionColor;
    lvSessionObjects.CurrentLineColor    := lvSessions.CurrentLineColor;
    lvSessionObjects.NullCellColor       := lvSessions.NullCellColor;
  end;
  SetFont(Sender);
end;

// TfrmSessionManager
//   SetFont
//
procedure TfrmSessionManager.SetFont(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    lvSessions.Font.Assign(FormSet.Preferences.Font[krsPREF_SESSIONGRIDFONT]);
    lvSessionObjects.Font.Assign(FormSet.Preferences.Font[krsPREF_SESSIONGRIDFONT]);
  end;
end;

// TfrmSessionManager
//   onQuery
//
procedure TfrmSessionManager.onQuery(parListView: TdbListView; parMetaData: TcMetaData);
var
  p: TcObject;
  s: String;
begin
  p := parMetaData.Find(enSQL, ksEMPTY);
  if (p <> nil) and (p is TcMetaData) then
  try
    s := VarToStr(FormSet.MetaData.Expression(p.sValue, nil, FormSet.MetaData, ksEMPTY, []));
    FormSet.Connection.SetLog(s, 0);
    parListView.SQL := s;
  except
    on E: Exception do
      Application.MessageBox(PChar(Format('Execution failed: %s', [trim(E.MEssage)])), krsERROR, MB_OK + MB_ICONEXCLAMATION);
  end;
end;

// TfrmSessionManager
//   BuildColumns
//
procedure TfrmSessionManager.BuildColumns(parListView: TdbListView; parMetaData: TcMetaData);
var
  i, L: longint;
  m: TcMetaData;
begin
  L := 0;
  for i := 0 to parMetaData.Count - 1 do
    if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) and ((parMetaData[i] as TcMetaData).eType = enField) then
    begin
      m := parMetaData[i] as TcMetaData;
      parListView.ColumnWidth[L] := Strtointdef(m.Attribute[krsWIDTH], kiUNDEFINED);
      parListView.ColumnCaption[L] := m.Attribute[krsDISPLAY];
      inc(L);
    end;
end;

// TfrmSessionManager
//   onRefresh
//
procedure TfrmSessionManager.onRefresh(Sender: TObject);
var
  p: TcObject;
begin
  if not m_bIsLocked then
  try
    m_bIsLocked := TRUE;
    m_objSelected := nil;
    m_lstValue.Clear;
    if (FormSet <> nil) and (FormSet.MetaData <> nil) then
    begin
      p := FindMetaPointer(krsSESSION);
      if (p <> nil) and (p is TcMetaData) then
        onQuery(lvSessions, p as TcMetaData);
      lvSessionObjects.Clear;
    end;
  finally
    m_bIsLocked := FALSE;
  end;
  SetState(sender);
end;

// TfrmSessionManager
//   lvSessionsChange
//
procedure TfrmSessionManager.lvSessionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  p: TcObject;
begin
  Application.ProcessMessages;
  if not m_bIsLocked and (lvSessions.Selected <> nil) and (m_objSelected <> lvSessions.Selected) then
  try
    m_bIsLocked := TRUE;
    // Get Title
    lblSessionObjects.Caption := ExtractString(krsOBJECTTITLE) + ' ';
    // Display Data
    p := FindMetaPointer(krsOBJECT);
    if (p <> nil) and (p is TcMetaData) then
    begin
      lvSessionObjects.Close;
      // 1. Set Callback
      (p as TcMetaData).hdlQuery := GetValue;
      // 2. Execute SQL
      onQuery(lvSessionObjects, p as TcMetaData);
      // Keep last display History
      m_objSelected := lvSessions.Selected;
    end;
  finally
    m_bIsLocked := FALSE;
  end;
  SetState(sender);
end;

// TfrmSessionManager
//   GetValue
//
function TfrmSessionManager.GetValue(value: String): String;
var
  p: TcObject;
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to m_lstValue.Count - 1 do
    if m_lstValue.Objects[i] <> nil then
    begin
      p := m_lstValue.Objects[i] as TcMetaData;
      if AnsiCompareText(p.sName, value) = 0 then
      begin
        result := m_lstValue[i];
        break;
      end;
    end;
end;

// TfrmSessionManager
//   SetValue
//
procedure TfrmSessionManager.SetValue;

  function GetField(parObject: TcObject; Index: longint): TcObject;
  var
    i, L: longint;
  begin
    result := nil;
    L := 0;
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcMetaData) and ((parObject[i] as TcMetaData).eType = enField) then
      begin
        if Index = L then
        begin
          result := parObject[i];
          break;
        end;
        inc(L);
      end;
  end;

var
  p, q: TcObject;
  li: TListItem;
  i: longint;
begin
  m_lstValue.Clear;
  p := FindMetaPointer(krsSESSION);
  li := lvSessions.Selected;
  if (p <> nil) and (p is TcMetaData) and (li <> nil) then
    for i := 0 to lvSessions.Columns.Count - 1 do
    begin
      q := GetField(p, i);
      if (p <> nil) and (p is TcMetaData) then
      begin
        if i = 0 then
          m_lstValue.AddObject(li.Caption, q)
        else
          m_lstValue.AddObject(li.SubItems[i - 1], q);
      end;
    end;
end;

// TfrmSessionManager
//   FindMetaPointer
//
function TfrmSessionManager.FindMetaPointer(value: String): TcObject;
begin
  result := nil;
  if FormSet <> nil then
    result := FormSet.MetaData.Find(enFEATURE, krsSESSIONMANAGER);
  if (result <> nil) and (result is TcMetaData) then
    result := (result as TcMetaData).Find(enOBJECT, value);
end;

// TfrmSessionManager
//   ExtractString
//
function TfrmSessionManager.ExtractString(value: String): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  SetValue;
  p := FindMetaPointer(value);
  if (p <> nil) and (p is TcMetaData) and (lvSessions.Selected <> nil) then
  begin
    // 1. Set Callback
    (p as TcMetaData).hdlQuery := GetValue;
    // 2. Execute SQL
    result := VarToStr(FormSet.MetaData.Expression(p.sValue, nil, FormSet.MetaData, ksEMPTY, []));
  end;
end;

// TfrmSessionManager
//   SetState
//
procedure TfrmSessionManager.SetState(Sender: TObject);
begin
  actDeleteSession.Enabled := lvSessions.Selected <> nil;
end;

// TfrmSessionManager
//   onDropLockClick
//
procedure TfrmSessionManager.onDropLockClick(Sender: TObject);
var
  s: String;
begin
  if not m_bIsLocked and (lvSessions.Selected <> nil) then
  try
    m_bIsLocked := TRUE;
    s := ExtractString(krsDELETEOBJECTLOCK);
    if (s <> ksEMPTY) and (Application.MessageBox('Delete Session?', krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES) then
    begin
      m_objExecute.Execute(s);
      s := m_objExecute.Error;
      if s <> ksEMPTY then
        Application.MessageBox(PChar(Format('An error occured: ''%s''', [trim(s)])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
      onRefresh(Sender);
    end;
  finally
    m_bIsLocked := FALSE;
  end;
end;

// TfrmSessionManager
//   GetLocalToolbar
//
function TfrmSessionManager.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmSessionManager
//   ReConnect
//
function TfrmSessionManager.ReConnect: boolean;
begin
  result := TRUE;
end;

end.
