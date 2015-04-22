unit frmSQLLog;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, FormLib, StdCtrls, Windows, daObjectLib, daGlobals,
  ExtCtrls, PreferenceLib, Menus;

type
  TfrmSQLLog = class(TForm)
    lvLog: TListView;
    PopupMenu: TPopupMenu;
    popExecute: TMenuItem;
    popPastetoQuery: TMenuItem;
    popParse: TMenuItem;
    popClear: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure popExecuteClick(Sender: TObject);
    procedure lvLogKeyPress(Sender: TObject; var Key: Char);
    procedure popParseClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure popClearClick(Sender: TObject);

  private
    // private methods
    //
    m_objFormSet: TcObject;
    m_lstPreferences: TcPreferenceList;

  published
    // private methods
    //
    procedure   CreateParams(var Params: TCreateParams); override;

  public
    // public methods
    //
    procedure   OnRefresh(sender: TObject);
    procedure   OnSetLog(lstLog: TcCollection; Index: longint);
    procedure   SetFormFocus(Sender: TObject); virtual;
    procedure   OnClearLog;

  public
    property    Preferences: TcPreferenceList           read m_lstPreferences   write m_lstPreferences;
  end;

implementation

uses
  daResourceStrings,
  Types,
  ImgList,
  StatementLib;    // TcStatementList

{$R *.dfm}

// TfrmSQLLog
//   FormCreate
//
procedure TfrmSQLLog.FormCreate(Sender: TObject);
begin
  m_objFormSet := nil;
  m_lstPreferences := nil;
end;

// TfrmSQLLog
//   FormClose
//
procedure TfrmSQLLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_lstPreferences <> nil then
  begin
    m_lstPreferences.StringVal[krsPREF_SQLLOG_X] := inttostr(Left);
    m_lstPreferences.StringVal[krsPREF_SQLLOG_Y] := inttostr(Top);
    m_lstPreferences.StringVal[krsPREF_SQLLOG_HEIGHT] := inttostr(Height);
    m_lstPreferences.StringVal[krsPREF_SQLLOG_WIDTH] := inttostr(Width);
    m_lstPreferences.StringVal[krsPREF_SQLLOG_VISIBLE] := kasBOOL[Visible];
  end;
  Action := caHide;
end;

// TfrmSQLLog
//   FormShow
//
procedure TfrmSQLLog.FormShow(Sender: TObject);
begin
  if m_lstPreferences <> nil then
  begin
    Height := strtointdef(m_lstPreferences.StringVal[krsPREF_SQLLOG_HEIGHT], Height);
    Width := strtointdef(m_lstPreferences.StringVal[krsPREF_SQLLOG_WIDTH], Width);
    Left := strtointdef(m_lstPreferences.StringVal[krsPREF_SQLLOG_X], kiUNDEFINED);
    if Left < 0 then
      Left := 0;
    if Left > Screen.Width - Width then
      Left := Screen.Width - Width;
    Top := strtointdef(m_lstPreferences.StringVal[krsPREF_SQLLOG_Y], 30);
    if Top < 0 then
      Top  := 0;
    if Top  > Screen.Height - Height then
      Top := Screen.Height - Height;
  end;
end;

// TfrmSQLLog
//   FormActivate
//
procedure TfrmSQLLog.FormActivate(Sender: TObject);
begin
  //
end;

// TfrmSQLLog
//   OnRefresh
//
procedure TfrmSQLLog.OnRefresh(sender: TObject);
begin
  //
end;

// TfrmSQLLog
//   OnSetLog
//
procedure TfrmSQLLog.OnSetLog(lstLog: TcCollection; Index: longint);
var
  i: longint;
  p: TListItem;
  s: String;
begin
  with lvLog do
  begin
    Items.BeginUpdate;
    if Index = kiUNDEFINED then
    begin
      Items.Clear;
      if (lstLog <> nil) then
        for i := lstLog.Count - 1 downto 0 do
        begin
          p := Items.Add;
          p.Data := lstLog[i];
          if (lstLog[i] <> nil) and (lstLog[i] is TcStatement) and ((lstLog[i] as TcStatement).iSQLNumber <> kiUNDEFINED) then
            p.Caption := inttostr((lstLog[i] as TcStatement).iSQLNumber);
          if (lstLog[i] <> nil) and (lstLog[i] is TcStatement) then
          begin
            p.SubItems.Add((lstLog[i] as TcStatement).SQL);
            p.SubItems.Add(Format('%6.3f', [(lstLog[i] as TcStatement).datDuration * 86400]));
          end;
        end;
    end
    else if (Index >= 0) and (Index < lstLog.Count) then
    begin
      p := Items.Insert(0);
      p.Data := lstLog[Index];
      if (lstLog[Index] <> nil) and (lstLog[Index] is TcStatement) and ((lstLog[Index] as TcStatement).iSQLNumber <> kiUNDEFINED) then
        p.Caption := inttostr((lstLog[Index] as TcStatement).iSQLNumber);
      if (lstLog[Index] <> nil) and (lstLog[Index] is TcStatement) then
      begin
        s := lstLog[Index].sName;
        if s = ksEMPTY then
          s := (lstLog[Index] as TcStatement).SQL;
        p.SubItems.Add(s);
        p.SubItems.Add(Format('%6.3f', [(lstLog[Index] as TcStatement).datDuration * 86400]))
      end;
    end;
    Items.EndUpdate;
  end;
end;

// TfrmSQLLog
//   lvLogKeyPress
//
procedure TfrmSQLLog.lvLogKeyPress(Sender: TObject; var Key: Char);
begin
  if key = kcCR then
    popExecuteClick(Sender);
end;

// TfrmSQLLog
//   SetFormFocus
//
procedure TfrmSQLLog.SetFormFocus(Sender: TObject);
begin
  if (Sender <> nil) and (Sender is TcFormSet) and (Sender <> m_objFormSet) then
  begin
    m_objFormSet := Sender as TcFormSet;
    OnSetLog((m_objFormSet as TcFormSet).lstLog, kiUNDEFINED);
  end;
end;

// TfrmSQLLog
//   popExecuteClick
//
procedure TfrmSQLLog.popExecuteClick(Sender: TObject);
var
  e: TeQueryOptions;
  p: TcObject;
begin
  if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) and (lvLog.Selected <> nil) and (lvLog.Selected.Data <> nil) then
  begin
    p := TcObject(lvLog.Selected.Data);
    if ((p is TcStatement) and (p as TcStatement).bIsValid) or not (p is TcStatement) then
    begin
      e := [];
      if Sender = popPastetoQuery then
        e := [eqoPaste];
      (m_objFormSet as TcFormSet).ExecuteSQL(efsQuery, TcObject(lvLog.Selected.Data).sValue + ksCR + kcPARAGRAPH, eqtUndefined, e);
    end;
  end;
end;

// TfrmSQLLog
//   CreateParams
//
procedure TfrmSQLLog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

// TfrmSQLLog
//   OnClearLog
//
procedure TfrmSQLLog.OnClearLog;
begin
  m_objFormSet := nil;
  OnSetLog(nil, kiUNDEFINED);
end;

// TfrmSQLLog
//   popParseClick
//
procedure TfrmSQLLog.popParseClick(Sender: TObject);
var
  p: TcStatement;
begin
  if (lvLog.Selected <> nil) and (lvLog.Selected.Data <> nil) then
  begin
    p := TcStatement(lvLog.Selected.Data);
    if p.Parse(estParsed, p.SQL) then
      Application.MessageBox(PChar(p.Text), 'Parsed', MB_OK);
  end;
end;

// TfrmSQLLog
//   PopupMenuPopup
//
procedure TfrmSQLLog.PopupMenuPopup(Sender: TObject);
var
  p: TcObject;
begin
  if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) and (lvLog.Selected <> nil) and (lvLog.Selected.Data <> nil) then
  begin
    p := TcObject(lvLog.Selected.Data);
    popExecute.Enabled := ((p is TcStatement) and (p as TcStatement).bIsValid) or not (p is TcStatement);
    popPasteToQuery.Enabled := popExecute.Enabled;
    popParse.Enabled := popExecute.Enabled;
  end;
end;

// TfrmSQLLog
//   popClearClick
//
procedure TfrmSQLLog.popClearClick(Sender: TObject);
begin
  with lvLog.Items do
  begin
    BeginUpdate;
    Clear;
    EndUpdate;
  end;
end;

end.
