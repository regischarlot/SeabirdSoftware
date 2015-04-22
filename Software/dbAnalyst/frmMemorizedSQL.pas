unit frmMemorizedSQL;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, FormLib, StdCtrls, Windows, daObjectLib, daGlobals,
  ExtCtrls, PreferenceLib, Menus, MemorizedSQLLib;

type
  TfrmMemorizedSQL = class(TForm)
    PopupMenu: TPopupMenu;
    popExecute: TMenuItem;
    popPastetoQuery: TMenuItem;
    tvSQL: TTreeView;
    Panel1: TPanel;
    cboRoots: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure popExecuteClick(Sender: TObject);
    procedure lvLogKeyPress(Sender: TObject; var Key: Char);
    procedure tvSQLExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure tvSQLChange(Sender: TObject; Node: TTreeNode);
    procedure cboRootsChange(Sender: TObject);

  private
    // private methods
    //
    m_lstPreferences: TcPreferenceList;
    m_objdata: TcMemorizedSQL;
    m_objFormSet: TcFormSet;

  published
    // private methods
    //
    procedure   CreateParams(var Params: TCreateParams); override;

  public
    // public methods
    //
    procedure   OnSetLog(lstLog: TcCollection; Index: longint);
    procedure   SetFormFocus(Sender: TObject); virtual;
    procedure   OnClearLog;
    procedure   Display(value: TeMemorizeResult);

  public
    property    Preferences: TcPreferenceList           read m_lstPreferences   write m_lstPreferences;
    property    Data: TcMemorizedSQL                    read m_objdata          write m_objdata;
    property    FormSet: TcFormSet                      read m_objFormSet       write m_objFormSet;
  end;

implementation

uses
  daResourceStrings,
  Types,
  ImgList,
  StatementLib;    // TcStatementList

{$R *.dfm}

// TfrmMemorizedSQL
//   FormCreate
//
procedure TfrmMemorizedSQL.FormCreate(Sender: TObject);
begin
  m_lstPreferences := nil;
  m_objdata := nil;
  m_objFormSet := nil;
end;

// TfrmMemorizedSQL
//   FormClose
//
procedure TfrmMemorizedSQL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_lstPreferences <> nil then
  begin
    m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_X] := inttostr(Left);
    m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_Y] := inttostr(Top);
    m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_HEIGHT] := inttostr(Height);
    m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_WIDTH] := inttostr(Width);
    m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_VISIBLE] := kasBOOL[Visible];
  end;
  Action := caHide;
end;

// TfrmMemorizedSQL
//   FormShow
//
procedure TfrmMemorizedSQL.FormShow(Sender: TObject);
begin
  if m_lstPreferences <> nil then
  begin
    Height := strtointdef(m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_HEIGHT], Height);
    Width := strtointdef(m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_WIDTH], Width);
    Left := strtointdef(m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_X], kiUNDEFINED);
    if Left < 0 then
      Left := 0;
    if Left > Screen.Width - Width then
      Left := Screen.Width - Width;
    Top := strtointdef(m_lstPreferences.StringVal[krsPREF_MEMORIZEDSQL_Y], 30);
    if Top < 0 then
      Top  := 0;
    if Top  > Screen.Height - Height then
      Top := Screen.Height - Height;
  end;
  if m_objData.IsEmpty then
    m_objData.Load(nil, kiUNDEFINED);
  Display(mrDisplayCombo);
end;

// TfrmMemorizedSQL
//   OnSetLog
//
procedure TfrmMemorizedSQL.Display(value: TeMemorizeResult);
begin
  case value of
    mrDisplayCombo:
      if m_objData <> nil then
      begin
        m_objData.SendToObject(cboRoots);
        if m_objFormSet <> nil then
          cboRoots.ItemIndex := m_objData.GetIndex(m_objFormSet.Connection);
      end;
    mrDisplayList:
      if (cboRoots.ItemIndex <> kiUNDEFINED) and (m_objData[cboRoots.ItemIndex] is TcMemorizedSQLItem) then
        (m_objData[cboRoots.ItemIndex] as TcMemorizedSQLItem).SendToObject(tvSQL)
      else
      begin
        tvSQL.Items.BeginUpdate;
        tvSQL.Items.Clear;
        tvSQL.Items.EndUpdate;
      end;
  end;
end;

// TfrmMemorizedSQL
//   OnSetLog
//
procedure TfrmMemorizedSQL.OnSetLog(lstLog: TcCollection; Index: longint);
begin
  //
end;

// TfrmMemorizedSQL
//   lvLogKeyPress
//
procedure TfrmMemorizedSQL.lvLogKeyPress(Sender: TObject; var Key: Char);
begin
  if key = kcCR then
    popExecuteClick(Sender);
end;

// TfrmMemorizedSQL
//   SetFormFocus
//
procedure TfrmMemorizedSQL.SetFormFocus(Sender: TObject);
var
  f: TcObject;
begin
  if (Sender = nil) then
  begin
    m_objFormSet := nil;
    Display(mrDisplayCombo);
    Display(mrDisplayList);
  end
  else if (Sender <> nil) then
  begin
    f := m_objFormSet;
    if (Sender is TcFormSet) and (m_objFormSet <> Sender as TcFormSet) then
      m_objFormSet := Sender as TcFormSet
    else if (Sender is TcForm) and (m_objFormSet <> (Sender as TcForm).FormSet) then
      m_objFormSet := (Sender as TcForm).FormSet;
    if (f <> m_objFormSet) and (m_objFormSet <> nil) and (m_objFormSet.Connection <> nil) then
    begin
      Display(mrDisplayCombo);
      Display(mrDisplayList);
    end;
  end;
end;

// TfrmMemorizedSQL
//   cboRootsChange
//
procedure TfrmMemorizedSQL.cboRootsChange(Sender: TObject);
begin
  Display(mrDisplayList)
end;

// TfrmMemorizedSQL
//   popExecuteClick
//
procedure TfrmMemorizedSQL.popExecuteClick(Sender: TObject);
var
  e: TeQueryOptions;
  p: TcObject;
begin
  if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) and (tvSQL.Selected <> nil) and (tvSQL.Selected.Data <> nil) then
  begin
    p := TcObject(tvSQL.Selected.Data);
    if p is TcMemorizedSQLItem then
    begin
      e := [];
      if Sender = popPastetoQuery then
        e := [eqoPaste];
      (m_objFormSet as TcFormSet).ExecuteSQL(efsQuery, p.sValue + ksCR + kcPARAGRAPH, eqtUndefined, e);
    end;
  end;
end;

// TfrmMemorizedSQL
//   CreateParams
//
procedure TfrmMemorizedSQL.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

// TfrmMemorizedSQL
//   tvSQLExpanding
//
procedure TfrmMemorizedSQL.tvSQLExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  p: TcObject;
begin
  if Node.Count = 0 then
  begin
    p := TcObject(Node.Data);
    if (p <> nil) and (p is TcMemorizedSQLItem) then
      (p as TcMemorizedSQLItem).SendToObject(Node);
  end;
end;

// TfrmMemorizedSQL
//   tvSQLExpanding
//
procedure TfrmMemorizedSQL.tvSQLChange(Sender: TObject; Node: TTreeNode);
begin
  popExecute.Enabled := (m_objFormSet <> nil) and (Node.Data <> nil) and (TObject(Node.Data).ClassType = TcMemorizedSQLItem);
  popPasteToQuery.Enabled := popExecute.Enabled;
end;

// TfrmMemorizedSQL
//   OnClearLog
//
procedure TfrmMemorizedSQL.OnClearLog;
begin
  m_objFormSet := nil;
  SetFormFocus(nil);
end;

end.
