unit frmGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, StdCtrls,
  PreferenceLib, ConnectionLib, DataLib, ToolWin, ImgList, AppEvnts,
  daObjectLib,
  FormLib,
  daGlobals,
  StatementLib,
  dbListView,
  ExecuteLib,
  FavoriteLib,
  StdActns,
  ActnList,
  Main,
  dfsSplitter;

type
  TeGridClearSection = (ecPanel, ecColumn, ecPart);
  TeGridClearSectionSet = set of TeGridClearSection;

  TfrmGrid = class(TcForm)
    DataMainMenu: TMainMenu;
    spltrQuery: TdfsSplitter;
    mnuTools: TMenuItem;
    pnlObjects: TPanel;
    ToolBar: TToolBar;
    btnRefresh: TToolButton;
    btnSQLEdit: TToolButton;
    lvGrid: TDBListView;
    btnAddObject: TToolButton;
    ActionList: TActionList;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditRefresh: TAction;
    mnuEdit: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditRefresh: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuEditN1: TMenuItem;
    actGridSQL: TAction;
    actGridAdd: TAction;
    mnuQuery: TMenuItem;
    mnuGridSQL: TMenuItem;
    mnuGridAdd: TMenuItem;
    mnuEditDelete: TMenuItem;
    actEditDelete: TAction;
    popGrid: TPopupMenu;
    popGridCopy: TMenuItem;
    popGridCut: TMenuItem;
    popGridPaste: TMenuItem;
    popGridUndo: TMenuItem;
    popGridDelete: TMenuItem;
    popGridRefresh: TMenuItem;
    popGridN1: TMenuItem;
    actEditClear: TAction;
    mnuEditClear: TMenuItem;
    ImageList: TImageList;
    actFavoritesAdd: TAction;
    mnuFavorites: TMenuItem;
    mnuFavoritesAdd: TMenuItem;
    actGridAddCondition: TAction;
    mnuGridAddCondition: TMenuItem;
    popGridAddCondition: TMenuItem;
    N1: TMenuItem;
    actGridDeleteColumn: TAction;
    popGridDeleteColumn: TMenuItem;
    mnuGridDeleteColumn: TMenuItem;
    ToolButton1: TToolButton;
    btnViewObjects: TToolButton;
    btnAutoRefresh: TToolButton;
    actShowAutoRefresh: TAction;
    actShowViewObjects: TAction;
    N2: TMenuItem;
    ViewObjects1: TMenuItem;
    AutoRefresh1: TMenuItem;
    actGridSort: TAction;
    mnuGridSort: TMenuItem;
    Sort1: TMenuItem;
    actEditNewRow: TAction;
    actEditDeleteRow: TAction;
    popGridAddNewRow: TMenuItem;
    popGridDeleteRow: TMenuItem;
    DeleteRow1: TMenuItem;
    AddNewRow1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    actFavoritesEdit: TAction;
    Edit1: TMenuItem;
    procedure  FormCreate(Sender: TObject);
    procedure  FormDestroy(Sender: TObject);
    procedure  FormActivate(Sender: TObject); override;
    procedure  FormDeactivate(Sender: TObject); override;
    procedure  onRefresh(Sender: TObject);
    procedure  FormShow(Sender: TObject);
    procedure  btnSQLEditClick(Sender: TObject);
    procedure  actGridAddExecute(Sender: TObject);
    procedure  SetState(Sender: TObject);
    procedure  onDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure  onDestinationPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure  onMovePanel(Sender: TObject);
    procedure  actEditClearExecute(Sender: TObject);
    procedure  ClipboardAction(Sender: TObject);
    procedure  actGridAddConditionExecute(Sender: TObject);
    procedure  actFavoritesAddExecute(Sender: TObject);
    procedure  mnuFavoritesClick(Sender: TObject);
    procedure  actGridDeleteColumnExecute(Sender: TObject);
    procedure  actShowAutoRefreshExecute(Sender: TObject);
    procedure  actShowViewObjectsExecute(Sender: TObject);
    procedure  pnlObjectsResize(Sender: TObject);
    procedure  actGridSortExecute(Sender: TObject);
    procedure  actFavoritesEditExecute(Sender: TObject);
    procedure  lvGridEndColumnResize(Sender: TCustomListView; columnindex, columnwidth: Integer);

  private
    // Private members
    //
    m_objStatement: TcStatement;
    m_objExecute: TcExecute;
    m_ipnlObjectsHeight: longint;

  protected
    // Protected Declarations
    //
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // Private methods
    //
    procedure   SetBoxes;
    procedure   ClearAction(value: TeGridClearSectionSet);
    function    RunSQL(value: boolean): boolean;

  protected
    // Protected methods
    //
    procedure   onListViewMessage(Sender: TObject; value: String);

  public
    // Public methods
    //
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; override;
    function    Finalize: boolean; override;
    procedure   onPreferenceChange(Sender: TObject); override;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; override;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; override;

  public
    // Public properties
    //
  end;

implementation

uses
  ADODB_TLB,
  Variants,
  PanelLib,
  EdgeLib,
  ClipBrd,
  ComObj,
  daResourceStrings,
  frmStatementBuildCondition; // TfrmStatementBuildCondition

{$R *.DFM}

// TfrmGrid
//   FormCreate
//
procedure TfrmGrid.FormCreate(Sender: TObject);
begin
  m_objStatement := TcStatement.Create(nil);
  m_objExecute := TcExecute.Create(nil);
end;

// TfrmGrid
//   FormDestroy
//
procedure TfrmGrid.FormDestroy(Sender: TObject);
begin
  m_objStatement.free;
  m_objExecute.free;
end;

// TfrmGrid
//   FormShow
//
procedure TfrmGrid.FormShow(Sender: TObject);
begin
  pnlObjectsResize(Sender);
  SetToolbarLocation(ControlBar, Toolbar);
  onPreferenceChange(Sender);
end;

// TfrmGrid
//   Finalize
//
function TfrmGrid.Finalize: boolean;
begin
  result := TRUE;
end;

// TfrmGrid
//   Initialize
//
function TfrmGrid.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
begin
  inherited Initialize(eparMode, eparParameter);
  result := CheckConnection(TRUE);
  FormSet.InitializeToolMenu(mnuTools);
  m_objExecute.Connection := Formset.Connection;
  lvGrid.Connection := FormSet.Connection;
  lvGrid.OnDisplayMessage := onListViewMessage;
  if Formset.MetaData.Option[krsOPTION_OUTERJOIN] = 'WHERE' then
    m_objStatement.eOptions := [esoOuterJoinWhere]
  else if Formset.MetaData.Option[krsOPTION_OUTERJOIN] = 'FROM' then
    m_objStatement.eOptions := [esoOuterJoinFrom];
  onRefresh(nil);
  FormActivate(nil);
  if (m_objStatement <> nil) and m_objStatement.IsEmpty and (eparMode <> efiFromFavorite) then
    actGridAddExecute(nil);
end;

// TfrmGrid
//   onPreferenceChange
//
procedure TfrmGrid.onPreferenceChange(Sender: TObject);
begin
  if (FormSet <> nil) and (FormSet.Preferences <> nil) then
  begin
    lvGrid.BackgroundColor := FormSet.Preferences.Color[krsPREF_QUERYGRIDBACKGROUNDCOLOR];
    lvGrid.SelectedColumnColor := FormSet.Preferences.Color[krsPREF_QUERYGRIDSELECTEDCOLUMNCOLOR];
    lvGrid.SelectionColor := FormSet.Preferences.Color[krsPREF_QUERYGRIDSELECTIONCOLOR];
    lvGrid.CurrentLineColor := FormSet.Preferences.Color[krsPREF_QUERYGRIDSELECTEDLINECOLOR];
    lvGrid.NullCellColor := FormSet.Preferences.Color[krsPREF_CONSOLEGRIDNULLCELLCOLOR];
    lvGrid.Font.Assign(FormSet.Preferences.Font[krsPREF_FONTCONSOLEGRID]);
    lvGrid.Invalidate;
  end;
end;

//
// Menu Items handling
//

// TfrmGrid
//   ExecuteItem (1)
//
function TfrmGrid.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
begin
  result := value <> ksEMPTY;
  if result then
  begin
    m_objStatement.SQL := Value;
    SetBoxes;
    RunSQL(actShowAutoRefresh.Checked);
  end;
  SetState(nil);
end;

// TfrmGrid
//   ExecuteItem (2)
//
function TfrmGrid.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  if (value <> nil) and (value.objStatement <> nil) and (value.objStatement is TcStatement) then
  begin
    m_objStatement.Copy(value.objStatement);
    m_objStatement.Schema := FormSet.Schema;
    if value.iPanelHeight > 0 then
      pnlObjects.Height := value.iPanelHeight;
    SetBoxes;
    RunSQL(actShowAutoRefresh.Checked);
  end;
  result := TRUE;
end;

// TfrmGrid
//   RunSQL
//
function TfrmGrid.RunSQL(value: boolean): boolean;
var
  i, L: longint;
  p: TcStatementPart;
begin
  result := TRUE;
  // Set Column Widths
  if value then
  try
    Screen.Cursor := crAppStart;
    L := 0;
    for i := 0 to m_objStatement.Count - 1 do
      if (m_objStatement[i] <> nil) and (m_objStatement[i] is TcStatementPart) and ((m_objStatement[i] as TcStatementPart).ePartType in kesCOLUMNSET) then
      begin
        p := m_objStatement[i] as TcStatementPart;
        lvGrid.ColumnWidth[L] := p.iColumnWidth;
        lvGrid.ColumnCaption[L] := p.sName;
        inc(L);
      end;
    //
    result := m_objExecute.Execute(m_objStatement.SQL, lvGrid);
    //
    Application.ProcessMessages;
    lvGridEndColumnResize(nil, kiUNDEFINED, kiUNDEFINED);
  finally
    Screen.Cursor := crDefault;
  end;
end;

// TfrmGrid
//   lvGridEndColumnResize
//
procedure TfrmGrid.lvGridEndColumnResize(Sender: TCustomListView; columnindex, columnwidth: Integer);
var
  i: longint;
  p: TcStatementPart;
begin
  for i := 0 to lvGrid.Columns.Count - 1 do
  begin
    p := m_objStatement.Part[kesCOLUMNSET, i];
    if (p <> nil) and (p is TcStatementPart) then
      p.iColumnWidth := lvGrid.Columns[i].Width;
  end;
end;

// TfrmGrid
//   FormActivate
//
procedure TfrmGrid.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  SetState(nil);
  ActionList.State := asNormal;
end;

// TfrmGrid
//   FormDeactivate
//
procedure TfrmGrid.FormDeactivate(Sender: TObject);
begin
  inherited FormDeactivate(Sender);
  ActionList.State := asSuspended;
  lvGrid.onDeActivate(Sender);
  StatusText([psPanel1], ksEMPTY, esiGridBottom);
end;

// TfrmGrid
//   onRefresh
//
procedure TfrmGrid.onRefresh(Sender: TObject);
begin
  lvGrid.Refresh;
  SetState(Sender);
end;

// TfrmGrid
//   onListViewMessage
//
procedure TfrmGrid.onListViewMessage(Sender: TObject; value: String);
begin
  StatusText([psPanel1], value, esiGridBottom);
end;

// TfrmGrid
//   btnSQLEditClick
//
procedure TfrmGrid.btnSQLEditClick(Sender: TObject);
begin
  if m_objStatement.Edit(FormSet) then
  begin
    SetBoxes;
    RunSQL(actShowAutoRefresh.Checked);
  end;
  SetState(Sender);
end;

// TfrmGrid
//   actGridAddExecute
//
procedure TfrmGrid.actGridAddExecute(Sender: TObject);
begin
  if m_objStatement.Build(FormSet) then
  begin
    SetBoxes;
    RunSQL(actShowAutoRefresh.Checked);
  end;
  SetState(Sender);
end;

// TfrmGrid
//   actEditClearExecute
//
procedure TfrmGrid.actEditClearExecute(Sender: TObject);
begin
  ClearAction([ecPanel, ecColumn, ecPart]);
end;

// TfrmGrid
//   ClearAction
//
procedure TfrmGrid.ClearAction(value: TeGridClearSectionSet);
var
  i: longint;
begin
  //
  // Panels
  if ecPanel in value then
    for i := pnlObjects.ComponentCount - 1 downto 0 do
    begin
      if (pnlObjects.Components[i] <> nil) and (pnlObjects.Components[i] is TvPanel) and ((pnlObjects.Components[i] as TvPanel).Data <> nil) then
        (pnlObjects.Components[i] as TvPanel).Data.Control := nil;
      pnlObjects.Components[i].Free;
    end;
  //
  // Columns
  if ecColumn in value then
    lvGrid.Columns.Clear;
  //
  // Statement Parts
  if ecPart in value then
    m_objStatement.Clear;
end;

// TfrmGrid
//   SetBoxes
//
procedure TfrmGrid.SetBoxes;
var
  i, j, L: longint;
  p: TvPanel;
  q: TcObject;
  e: TvEdge;
  t: TcStatementPart;
  c: TcStatementPartCondition;
begin
  ClearAction([ecPanel, ecColumn]);
  //
  // Create Objects
  //
  // A. Create Panels
  L := 0;
  for i := 0 to m_objStatement.Count - 1 do
    if (m_objStatement[i] <> nil) and
       (m_objStatement[i] is TcStatementPart) and
       ((m_objStatement[i] as TcStatementPart).ePartType in kesTABLESET) then
    begin
      t := m_objStatement[i] as TcStatementPart;
      p := TvPanel.Create(pnlObjects);
      p.Parent := pnlObjects;
      p.Name := Format('Panel_%d', [i]);
      p.ImageList := ImageList;
      if t.Location.IsEmpty then
      begin
        t.Location.Left := 5 + L * (10 + p.Width);
        t.Location.Top := 10;
        t.Location.Width := p.Width;
        t.Location.Height := p.Height;
        inc(L);
      end;
      t.Location.SetToControl(p);
      p.Caption := t.Text;
      p.Data := t;
      p.hdlEndDrag := onDestinationPanelEndDrag;
      p.hdlDragOver := onDragOver;
      p.onChangeLocation := onMovePanel;
      p.onScroll := onMovePanel;
      q := t.objPart;
      t.Control := p;
      if q <> nil then
        for j := 0 to q.count - 1 do
          p.AddColumn(q[j].Text, q[j]);
    end;
  //
  // B. Create Edges
  for i := 0 to m_objStatement.Count - 1 do
    if (m_objStatement[i] <> nil) and
       (m_objStatement[i] is TcStatementPartCondition) then
    begin
      c := m_objStatement[i] as TcStatementPartCondition;
      if (c.eRHSType = eorColumn) and (c.Control_RHS <> nil) and (c.Control_LHS <> nil) then
      begin
        e := TvEdge.Create(pnlObjects);
        e.Parent := pnlObjects;
        e.Name := Format('Edge_%d', [i]);
        e.ControlLHS := c.Control_LHS;
        e.ControlRHS := c.Control_RHS;
        e.iLHS := c.iLHS;
        e.iRHS := c.iRHS;
        e.Visible := TRUE;
      end;
    end;
end;

// TfrmGrid
//   SetState
//
procedure TfrmGrid.SetState(Sender: TObject);
begin
  actEditCopy.Enabled           := (ActiveControl <> nil) and (ActiveControl is TdbListView);
  actGridDeleteColumn.Enabled   := lvGrid.Coordinates.X <> kiUNDEFINED;
  actShowAutoRefresh.Enabled    := TRUE;
  actShowViewObjects.Enabled    := TRUE;
  actGridSort.Enabled           := actGridDeleteColumn.Enabled;
end;

// TfrmGrid
//   onDestinationPanelEndDrag
//
procedure TfrmGrid.onDestinationPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  pG: TcObject;
  pS: TcStatementPart;
  p: TcStatementPartCondition;
  p2: TcStatementPart;
  L: TPoint;
  N: Longint;
begin
  if (Sender <> nil) and (Sender is TCustomListBox) and ((Sender as TCustomListBox).ItemIndex <> kiUNDEFINED) and (Sender <> Target) and (Target <> nil)then
  begin
    //
    // ListBox to dbListView drag-an-drop
    //
    if (Target <> nil) and (Target is TdbListView) then
    begin
      // Recognize where to insert or add
      L := (Target as TdbListView).CellAt(Target, X, Y);
      N := kiUNDEFINED;
      if L.X <> kiUNDEFINED then
      begin
        pG := m_objStatement.Part[kesCOLUMNSET, L.X];
        if pG <> nil then
          N := m_objStatement.IndexOf(pG);
      end;
      // Left Hand Site
      with Sender as TCustomListBox do
        pG := TcObject(Items.Objects[ItemIndex]);
      // Create Object
      p2 := TcStatementPart.Create(m_objStatement);
      p2.Copy(pG);
      pS := TcStatementPart((Sender as TCustomListBox).Tag);
      (p2 as TcStatementPart).sPrefix := pS.sAlias;
      if (N <> kiUNDEFINED) and (N < m_objStatement.Count) then
        m_objStatement.Insert(N, p2)
      else
        m_objStatement.Add(p2);
    end
    //
    // ListBox to ListBox drag-an-drop
    //
    else if (Target <> nil) and (Target is TCustomListBox) and ((Target as TCustomListBox).ItemIndex <> kiUNDEFINED) then
    begin
      with Target as TCustomListBox do
        if Items[ItemIndex] <> krsSTAR then
          with Sender as TCustomListBox do
            if Items[ItemIndex] <> krsSTAR then
      begin
        // Create Object
        p := TcStatementPartCondition.Create(m_objStatement);
        m_objStatement.Add(p);
        p.SetCondition(TcStatementPart(TcObject((Sender as TCustomListBox).Items.Objects[(Sender as TCustomListBox).ItemIndex])),
                       ecoEqual,
                       TcStatementPart(TcObject((Target as TCustomListBox).Items.Objects[(Target as TCustomListBox).ItemIndex])));
        // Left Hand Site
        pS := TcStatementPart((Sender as TCustomListBox).Tag);
        p.LHS.sPrefix := pS.sAlias;
        // Right Hand Side
        pS := TcStatementPart((Target as TCustomListBox).Tag);
        p.RHS.sPrefix := pS.sAlias;
        // Draw boxes
        SetBoxes;
      end;
    end;
    m_objStatement.Check;
    // SetBoxes;
    RunSQL(actShowAutoRefresh.Checked);
    SetState(Sender);
  end;
end;

// TfrmGrid
//   onDragOver
//
procedure TfrmGrid.onDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  //
  // ListBox to ListBox
  if (Source <> nil) and (Source is TCustomListBox) and
     (Sender <> nil) and (Sender is TCustomListBox) and
     ((sender as TCustomListBox).ItemAtPos(Point(X, Y), TRUE) <> kiUNDEFINED) then
  begin
    with Source as TCustomListBox do
      Accept := Items[ItemIndex] <> krsSTAR;
    if Accept and (Sender <> Source) then
      with Sender as TCustomListBox do
        ItemIndex := ItemAtPos(Point(X, Y), TRUE);
  end
  // ListBox to dbListView, or
  // TvPanel to pnlObject
  else
    Accept := ((Source <> nil) and (Source is TCustomListBox) and (Sender <> nil) and (Sender = lvGrid)) or
              ((Sender = pnlObjects) and (Source is TvPanel));
end;

// TfrmGrid
//   onMovePanel
//
procedure TfrmGrid.onMovePanel(Sender: TObject);
var
  i: longint;
  e: TvEdge;
begin
  if (Sender <> nil) and (Sender is TvPanel) and ((Sender as TvPanel).Data <> nil) and ((Sender as TvPanel).Data is TcStatementPart) then
  begin
    (Sender as TvPanel).Data.Location.GetFromControl(Sender as TvPanel);
    // Refresh Edges
    for i := 0 to pnlObjects.ComponentCount - 1 do
      if (pnlObjects.Components[i] <> nil) and (pnlObjects.Components[i] is TvEdge) then
      begin
        e := pnlObjects.Components[i] as TvEdge;
        if (e.ControlLHS = Sender) or (e.ControlRHS = Sender) then
          e.PaintEdge(sender);
      end;
  end;
end;

// TfrmGrid
//   ClipboardAction
//
procedure TfrmGrid.ClipboardAction(Sender: TObject);
begin
  if (ActiveControl <> nil) and (ActiveControl is TdbListView) then
  begin
    if sender = actEditCopy then // CTRL-C
      ClipBoard.AsText := (ActiveControl as TdbListView).CellText;
  end;
end;

// TfrmGrid
//   actGridAddConditionExecute
//
procedure TfrmGrid.actGridAddConditionExecute(Sender: TObject);
var
  pLHS: TcStatementPart;
  sRHS: String;
  e: TeConditionOperator;
  frm: TfrmStatementBuildCondition;
  p: TcStatementPartCondition;
begin
  pLHS := nil;
  if (lvGrid.Coordinates.X <> kiUNDEFINED) and (lvGrid.Coordinates.Y <> kiUNDEFINED) then
    pLHS := m_objStatement.Part[kesCOLUMNSET, lvGrid.Coordinates.X];
  sRHS := lvGrid.CellText;
  e := ecoEqual;
  if ((sRHS = ksEMPTY) or (sRHS = krsNULL)) and
     (lvGrid.Coordinates.X <> kiUNDEFINED) and
     (lvGrid.Coordinates.Y <> kiUNDEFINED) then
    e := ecoIsNull;
  // Display Form
  frm := nil;
  try
    frm := TfrmStatementBuildCondition.Create(self);
    frm.FormSet := FormSet;
    frm.Statement := m_objStatement;
    p := TcStatementPartCondition.Create(nil);
    p.SetCondition(pLHS, e, sRHS);
    frm.Part := p;
    if frm.ShowModal = mrOK then
    begin
      p.Parent := m_objStatement;
      m_objStatement.Add(p);
      SetBoxes;
      RunSQL(TRUE);
    end
    else
      p.free;
    SetState(Sender);
  finally
    frm.release;
  end;
end;

// TfrmGrid
//   actFavoriteAddExecute
//
procedure TfrmGrid.actFavoritesAddExecute(Sender: TObject);
var
  s: String;
  p: TcFavoriteItem;
begin
  s := InputBox('Add Saved Query', 'Please specify a ''Saved Query'' heading', ksEMPTY);
  if (s <> ksEMPTY) and not m_objStatement.IsEmpty then
  begin
    p := FormSet.Favorites.Add(s, m_objStatement);
    p.iPanelHeight := pnlObjects.Height;
    FormSet.Favorites.Save;
  end;
end;

// TfrmGrid
//   actFavoritesEditExecute
//
procedure TfrmGrid.actFavoritesEditExecute(Sender: TObject);
begin
  FormSet.Favorites.Edit;
end;

// TfrmGrid
//   mnuFavoritesClick
//
procedure TfrmGrid.mnuFavoritesClick(Sender: TObject);
begin
  // Fill in menu
  Formset.Favorites.SendToObject(mnuFavorites);
  // Enabling
  actFavoritesAdd.Enabled := m_objStatement.XML <> ksEMPTY;
end;

// TfrmGrid
//   actGridDeleteColumnExecute
//
procedure TfrmGrid.actGridDeleteColumnExecute(Sender: TObject);
var
  p: TcStatementPart;
begin
  if lvGrid.Coordinates.X <> kiUNDEFINED then
  begin
    p := m_objStatement.Part[kesCOLUMNSET, lvGrid.Coordinates.X];
    if p <> nil then
    begin
      m_objStatement.Delete(p);
      SetBoxes;
      RunSQL(TRUE);
    end;
  end;
end;

// TfrmGrid
//   actShowAutoRefreshExecute
//
procedure TfrmGrid.actShowAutoRefreshExecute(Sender: TObject);
begin
  actShowAutoRefresh.Checked := not actShowAutoRefresh.Checked;
  RunSQL(actShowAutoRefresh.Checked);
end;

// TfrmGrid
//   actShowViewObjectsExecute
//
procedure TfrmGrid.actShowViewObjectsExecute(Sender: TObject);
var
  L: longint;
begin
  actShowViewObjects.Checked := not actShowViewObjects.Checked;
  L := m_ipnlObjectsHeight;
  if not actShowViewObjects.Checked then
  begin
    pnlObjects.Constraints.MinHeight := 0;
    pnlObjects.Height := 0;
    m_ipnlObjectsHeight := L;
  end
  else
  begin
    pnlObjects.Height := m_ipnlObjectsHeight;
    pnlObjects.Constraints.MinHeight := 30;
  end;
end;

// TfrmGrid
//   pnlObjectsResize
//
procedure TfrmGrid.pnlObjectsResize(Sender: TObject);
begin
  m_ipnlObjectsHeight := pnlObjects.Height;
end;

// TfrmGrid
//   actGridSortExecute
//
procedure TfrmGrid.actGridSortExecute(Sender: TObject);
var
  p, p2: TcStatementPart;
begin
  if lvGrid.Coordinates.X <> kiUNDEFINED then
  begin
    p := m_objStatement.Part[kesCOLUMNSET, lvGrid.Coordinates.X];
    if p <> nil then
    begin
      // Create Object
      p2 := TcStatementPart.Create(m_objStatement);
      p2.Copy(p);
      p2.ePartType := epOrder;
      m_objStatement.Add(p2);
      SetBoxes;
      RunSQL(TRUE);
    end;
  end;
end;

// TfrmGrid
//   GetLocalToolbar
//
function TfrmGrid.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmGrid
//   ReConnect
//
function TfrmGrid.ReConnect: boolean;
begin
  result := TRUE;
end;

end.


