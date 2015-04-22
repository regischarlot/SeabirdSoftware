unit frmConnection;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConnectionLib,
  ComCtrls, ExtCtrls, PreferenceLib, ImgList, Menus, ActnList, Buttons,
  ToolWin, Main;

type
  TfrmConnection = class(TForm)
    popList: TPopupMenu;
    popListViewLargeIcons: TMenuItem;
    popListViewSmallIcons: TMenuItem;
    popListViewList: TMenuItem;
    popListViewDetails: TMenuItem;
    popListDetailsView: TMenuItem;
    N1: TMenuItem;
    Add1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    ActionList: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actViewLargeIcons: TAction;
    actViewSmallIcons: TAction;
    actViewList: TAction;
    actViewDetails: TAction;
    actConnect: TAction;
    actClose: TAction;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Panel2: TPanel;
    lvConnections: TListView;
    Label1: TLabel;
    ToolBar1: TToolBar;
    btnConnect: TToolButton;
    btnAdd: TToolButton;
    btnEdit: TToolButton;
    btnDelete: TToolButton;
    actHelp: TAction;
    pnlBottom: TPanel;
    btnClose: TButton;
    actView: TAction;
    btnUseWizard: TCheckBox;
    btnConnect2: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OnConnect(Sender: TObject);
    procedure SetButtonState(Sender: TObject);
    procedure lstConnectionsClick(Sender: TObject);
    procedure OnEdit(Sender: TObject);
    procedure OnAdd(Sender: TObject);
    procedure OnDelete(Sender: TObject);
    procedure popListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure popListPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvConnectionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure OnClose(Sender: TObject);
    procedure lvConnectionsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvConnectionsKeyPress(Sender: TObject; var Key: Char);

  private
    m_objConnection: TcConnection;
    m_objPreferences: TcPreferenceList;

  private
    procedure SetDisplay(Preference: TcPreference);
    function  GetConnectionItem: TcPreference;
    procedure SetImageList(value: TImageList);

  public
    property Connection: TcConnection           read m_objConnection            write m_objConnection;
    property ImageList: TImageList                                              write SetImageList;
    property PreferenceList: TcPreferenceList   read m_objPreferences           write m_objPreferences;
  end;

implementation

uses
  daGlobals,
  DBLogDlg,
  FormLib,
  ADODB_TLB,
  daResourceStrings,
  frmConnectionWizard;  // TfrmConnectionWizard

{$R *.DFM}

// Tool
//   ListViewSort
//
function ListViewSort(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
begin
  if lParam = 0 then
  begin
    if (Item1.ListView as TListView).Columns[lParam].Tag > 0 then
      result := CompareText(Item1.Caption, Item2.Caption)
    else
      result := CompareText(Item2.Caption, Item1.Caption);
  end
  else
  begin
    if (Item1.ListView as TListView).Columns[lParam].Tag > 0 then
      result := CompareText(Item1.SubItems[lParam - 1], Item2.SubItems[lParam - 1])
    else
      result := CompareText(Item2.SubItems[lParam - 1], Item1.SubItems[lParam - 1]);
  end
end;

// TfrmConnection
//   FormCreate
//
procedure TfrmConnection.FormCreate(Sender: TObject);
begin
  m_objConnection := nil;
  m_objPreferences := nil;
end;

// TfrmConnection
//   FormClose
//
procedure TfrmConnection.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_objPreferences <> nil then
  begin
    m_objPreferences.StringVal[krsPREF_CONNECTIONFRMWIDTH] := inttostr(Width);
    m_objPreferences.StringVal[krsPREF_CONNECTIONFRMHEIGHT] := inttostr(Height);
    m_objPreferences.StringVal[krsPREF_CONNECTIONFRMDISPLAY] := inttostr(longint(lvConnections.ViewStyle));
    m_objPreferences.StringVal[krsPREF_CONNECTIONUSEWIZARD] := kasBOOL[btnUseWizard.Checked];
  end;
  Action := caFree;
end;

// TfrmConnection
//   FormShow
//
procedure TfrmConnection.FormShow(Sender: TObject);
begin
  if m_objPreferences <> nil then
  begin
    Width := strtointdef(m_objPreferences.StringVal[krsPREF_CONNECTIONFRMWIDTH], 300);
    Height := strtointdef(m_objPreferences.StringVal[krsPREF_CONNECTIONFRMHEIGHT], 200);
    lvConnections.ViewStyle := TViewStyle(strtointdef(m_objPreferences.StringVal[krsPREF_CONNECTIONFRMDISPLAY], longint(vsIcon)));
    btnUseWizard.Checked := m_objPreferences.StringVal[krsPREF_CONNECTIONUSEWIZARD] <> krsFALSE;
  end;
  if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
    (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, 0);
  SetDisplay(nil);
  SetButtonState(Sender);
  FormResize(Sender);
end;

// TfrmConnection
//   SetDisplay
//
procedure TfrmConnection.SetDisplay(Preference: TcPreference);
var
  i: longint;
  q: TcPreference;
  p: TListItem;
begin
  with lvConnections do
  begin
    Items.BeginUpdate;
    Items.Clear;
    if m_objPreferences <> nil then
      for i := 0 to m_objPreferences.Count - 1 do
        if (m_objPreferences[i] <> nil) and (m_objPreferences[i] is TcPreference) and ((m_objPreferences[i] as TcPreference).PreferenceType = eptConnection) then
        begin
          q := m_objPreferences[i] as TcPreference;
          p := Items.Add;
          p.Caption := q.sName;
          p.SubItems.Add(q.sDescription);
          p.SubItems.Add(q.sProvider);
          p.SubItems.Add(GetFileName(q.sMetaXML));
          p.Data := q;
          p.ImageIndex := kaiFORMIMAGEINDEX[q.eForm];
          if Preference = q then
            Selected := p;
        end;
    CustomSort(@ListViewSort, 0);
    Items.EndUpdate;
  end;
end;

// TfrmConnection
//   OnConnect
//
procedure TfrmConnection.OnConnect(Sender: TObject);
var
  b: boolean;
begin
  Screen.Cursor := crHourGlass;
  try
    if actConnect.Enabled then
    try
      // Is there a login required..?
      b := not m_objConnection.bLoginPrompt or m_objConnection.Login(GetConnectionItem, FALSE);
      Application.ProcessMessages;
      // Proceed to connect
      if b then
        with m_objConnection do
        begin
          Open;
          Application.ProcessMessages;
          if Connected then
            ModalResult := mrOK;
        end;
    except
      on E:Exception do
        Application.MessageBox(PChar(E.Message), krsABORT, MB_ICONSTOP + MB_OK);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

// TfrmConnection
//   SetButtonState
//
procedure TfrmConnection.SetButtonState(Sender: TObject);
var
  p: TcPreference;
begin
  p := GetConnectionItem;
  actConnect.Enabled := (p <> nil) and p.IsValid;
  actDelete.Enabled := p <> nil;
  actEdit.Enabled := p <> nil;
end;

// TfrmConnection
//   lstConnectionsClick
//
procedure TfrmConnection.lstConnectionsClick(Sender: TObject);
var
  p: TcPreference;
begin
  p := GetConnectionItem;
  if (p <> nil) and p.IsValid then
  begin
    (Connection.FormSet as TcFormSet).Clear;
    Connection.sXML               := p.sMetaXML;
    Connection.ConnectionString   := p.sADO;
    Connection.bLoginPrompt       := p.bLoginPrompt;
    Connection.eForm              := p.eForm;
    Connection.sName              := p.sName;
    Connection.eMode              := p.eMode;
    Connection.sUserName          := p.Attribute[krsXML_USERNAME];
    Connection.sPassword          := p.Attribute[krsXML_PASSWORD];
    Connection.sServer            := p.Attribute[krsXML_DATASOURCE];
    Connection.sConnectAs         := p.Attribute[krsXML_CONNECTAS];
    Connection.bOneConnection     := p.bOneConnection;
    Connection.AttributeKeys.Text := p.AttributeKeys.Text;
  end
  else if (p <> nil) and not p.IsValid then
    Application.MessageBox('The Connection profile parameters are not valid. Please edit the connection profile entry.', krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
  SetButtonState(Sender);
end;

// TfrmConnection
//   lvConnectionChange
//
procedure TfrmConnection.lvConnectionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  lstConnectionsClick(Sender);
end;

// TfrmConnection
//   GetConnectionItem
//
function TfrmConnection.GetConnectionItem: TcPreference;
begin
  result := nil;
  with lvConnections do
    if (Selected <> nil) and (Selected.Data <> nil) then
      result := TcPreference(Selected.Data);
end;

// TfrmConnection
//   OnEdit
//
procedure TfrmConnection.OnEdit(Sender: TObject);
var
  p: TcPreference;
  frm: TfrmConnectionWizard;
begin
  p := GetConnectionItem;
  frm := nil;
  if p <> nil then
  try
    frm := TfrmConnectionWizard.Create(nil);
    frm.Preference := p;
    frm.Connection := Connection;
    frm.UseWizard := btnUseWizard.Checked;
    if frm.ShowModal = idOK then
    begin
      SetDisplay(p);
      if m_objPreferences <> nil then
        m_objPreferences.WriteToRegistry;
      lvConnections.SetFocus;
    end;
  finally
    frm.release;
  end;
  SetButtonState(Sender);
  lvConnections.SetFocus;
end;

// TfrmConnection
//   OnAdd
//
procedure TfrmConnection.OnAdd(Sender: TObject);
var
  p: TcPreference;
  frm: TfrmConnectionWizard;
begin
  if m_objPreferences <> nil then
  begin
    p := TcPreference.Create(m_objPreferences);
    p.PreferenceType := eptConnection;
    frm := nil;
    try
      frm := TfrmConnectionWizard.Create(nil);
      frm.Preference := p;
      frm.Connection := Connection;
      frm.UseWizard := btnUseWizard.Checked;
      if frm.ShowModal = idOK then
      begin
        m_objPreferences.Add(p);
        SetDisplay(p);
        m_objPreferences.WriteToRegistry;
      end
      else
        p.Free;
    finally
      frm.release;
    end;
    SetButtonState(Sender);
  end;
end;

// TfrmConnection
//   OnDelete
//
procedure TfrmConnection.OnDelete(Sender: TObject);
var
  p: TcPreference;
begin
  p := GetConnectionItem;
  if (m_objPreferences <> nil) and (p <> nil) and (Application.MessageBox(PChar(Format('Delete database profile ''%s''?', [p.sName])), krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES) then
  begin
    m_objPreferences.Delete(p);
    m_objPreferences.WriteToRegistry;
    SetDisplay(nil);
  end;
  SetButtonState(Sender);
end;

// TfrmConnection
//   popListClick
//
procedure TfrmConnection.popListClick(Sender: TObject);
begin
  if (Sender <> nil) and (Sender is TComponent) then
    lvConnections.ViewStyle := TViewStyle((Sender as TComponent).Tag);
  FormResize(Sender);
end;

// TfrmConnection
//   FormResize
//
procedure TfrmConnection.FormResize(Sender: TObject);
begin
  lvConnections.Arrange(arDefault);
end;

// TfrmConnection
//   popListPopup
//
procedure TfrmConnection.popListPopup(Sender: TObject);
begin
  actViewLargeIcons.Checked := lvConnections.ViewStyle = vsIcon;
  actViewSmallIcons.Checked := lvConnections.ViewStyle = vsSmallIcon;
  actViewList.Checked := lvConnections.ViewStyle = vsList;
  actViewDetails.Checked := lvConnections.ViewStyle = vsReport;
end;

// TfrmConnection
//   SetImageList
//
procedure TfrmConnection.SetImageList(value: TImageList);
begin
  lvConnections.SmallImages := value;
  lvConnections.LargeImages := value;
end;

// TfrmConnection
//   OnClose
//
procedure TfrmConnection.OnClose(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// TfrmConnection
//   lvConnectionsColumnClick
//
procedure TfrmConnection.lvConnectionsColumnClick(Sender: TObject; Column: TListColumn);
begin
  Column.Tag := -1 * Column.Tag;
  lvConnections.CustomSort(@ListViewSort, Column.Index);
end;

// TfrmConnection
//   lvConnectionsKeyPress
//
procedure TfrmConnection.lvConnectionsKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
    OnConnect(Sender);
end;

end.
