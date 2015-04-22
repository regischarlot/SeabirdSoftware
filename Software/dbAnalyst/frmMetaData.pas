unit frmMetaData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, DataLib, StdCtrls, OleCtrls, ImgList, PreferenceLib,
  Menus, ToolWin;

type
  TfrmMetaData = class(TForm)
    tvObjects: TTreeView;
    Splitter1: TSplitter;
    lvElements: TListView;
    MainMenu1: TMainMenu;
    mnuObject: TMenuItem;
    mnuObjectNew: TMenuItem;
    mnuObjectNewObject: TMenuItem;
    mnuObjectNewField: TMenuItem;
    mnuObjectNewSQL: TMenuItem;
    mnuObjectNewMenu: TMenuItem;
    mnuObjectNewRule: TMenuItem;
    mnuObjectNewAttribute: TMenuItem;
    mnuObjectNewDisplay: TMenuItem;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton12: TToolButton;
    ToolButton3: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvObjectsChange(Sender: TObject; Node: TTreeNode);
    procedure tvDetailsCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure lvElementsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);

  private
    m_objMetaData: TcMetaData;
    m_objPreferences: TcPreferenceList;

  private
    procedure onSave(Sender: TObject);
    procedure onSaveAs(Sender: TObject);
    procedure onClose(Sender: TObject);

  public
    function Open(sFileName: String): boolean;
    property Preferences: TcPreferenceList read m_objPreferences write m_objPreferences;
  end;

implementation

{$R *.DFM}

uses
  Main,
  daGlobals,
  daResourceStrings,
  daObjectLib;

// TfrmMetaData
//   Open
//
function TfrmMetaData.Open(sFileName: String): boolean;
var
  s: String;
  q: TMemoryStream;
begin
  screen.cursor := crHourGlass;
  Caption := Format('Meta Editor: ''%s''', [sFileName]);
  q := nil;
  try
    q := TMemoryStream.Create;
    q.LoadFromFile(sFileName);
    s := copy(PChar(q.Memory), 1, q.Size);
  finally
    q.free;
  end;
  m_objMetaData.Clear;
  m_objMetaData.XML := s;
  m_objMetaData.SendToObject(tvObjects);
  tvObjects.FullExpand;
  screen.cursor := crDefault;
  result := TRUE;
end;

// TfrmMetaData
//   FormCreate
//
procedure TfrmMetaData.FormCreate(Sender: TObject);
begin
  m_objMetaData := TcMetaData.Create(nil);
end;

// TfrmMetaData
//   FormClose
//
procedure TfrmMetaData.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormDeactivate(Sender);
  m_objMetaData.free;
  Action := caFree;
end;

// TfrmMetaData
//   tvObjectsChange
//
procedure TfrmMetaData.tvObjectsChange(Sender: TObject; Node: TTreeNode);
var
  n: TcMetaData;
  i: longint;
  p: TListItem;
begin
  if Node.Data <> nil then
  begin
    lvElements.Items.BeginUpdate;
    lvElements.Items.Clear;
    n := Node.Data;
    for i := 0 to n.Count - 1 do
    if n[i].eType <> enObject then
      begin
        p := lvElements.Items.Add;
        p.Data := n[i];
        p.Caption := kasTYPE[n[i].eType];
        p.ImageIndex := longint(n[i].eType);
        p.SubItems.Add(n[i].sName);
        p.SubItems.Add(trim(n[i].sValue));
      end;
    lvElements.Items.EndUpdate;
  end;
end;

// TfrmMetaData
//   tvDetailsCollapsing
//
procedure TfrmMetaData.tvDetailsCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := FALSE;
end;

// TfrmMetaData
//   lvElementsCustomDrawItem
//
procedure TfrmMetaData.lvElementsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not(Item.Index Mod 6 in [0, 1, 2]) then
    Sender.Canvas.Brush.Color := StringToColor(Preferences.StringVal[krsPREF_COLORMETASTRIPE])
  else
    Sender.Canvas.Brush.Color := ColorToRGB((Sender as TListView).Color);
  Sender.Canvas.FillRect(Item.DisplayRect(drLabel));
  DefaultDraw := TRUE;
end;

// TfrmMetaData
//   FormActivate
//
procedure TfrmMetaData.FormActivate(Sender: TObject);
begin
  ToolBar2.Visible := TRUE;
  frmMain.SetMainMenu(mmSave, TRUE, onSave);
  frmMain.SetMainMenu(mmSaveAs, TRUE, onSaveAs);
  frmMain.SetMainMenu(mmClose, TRUE, onClose);
end;

// TfrmMetaData
//   FormDeactivate
//
procedure TfrmMetaData.FormDeactivate(Sender: TObject);
begin
  ToolBar2.Visible := FALSE;
  frmMain.SetMainMenu(mmSave, FALSE, nil);
  frmMain.SetMainMenu(mmSaveAs, FALSE, nil);
  frmMain.SetMainMenu(mmClose, FALSE, nil);
end;

// TfrmMetaData
//   onSave
//
procedure TfrmMetaData.onSave(Sender: TObject);
begin
  //
end;

// TfrmMetaData
//   onSaveAs
//
procedure TfrmMetaData.onSaveAs(Sender: TObject);
begin
  //
end;

// TfrmMetaData
//   onClose
//
procedure TfrmMetaData.onClose(Sender: TObject);
begin
  Close;
end;

end.
