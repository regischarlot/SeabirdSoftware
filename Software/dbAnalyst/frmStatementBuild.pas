unit frmStatementBuild;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FormLib, StatementLib, ComCtrls,
  daObjectLib, Buttons, ImgList, ImageListBoxLib, SynEdit;

type
  TfrmStatementBuild = class(TForm)
    pnlBottom: TPanel;
    btnNext: TButton;
    btnCancel: TButton;
    PageControl: TPageControl;
    tbsObjects: TTabSheet;
    lblPool: TLabel;
    lstPool: TListBox;
    lstSelected: TListBox;
    lblSelected: TLabel;
    btnPrevious: TButton;
    tbsColumns: TTabSheet;
    tbsWhere: TTabSheet;
    lblObjectColumns: TLabel;
    lstObjectColumns: TListBox;
    lblSelectedColumns: TLabel;
    lstSelectedColumns: TImageListBox;
    btnColumnsAdd: TButton;
    btnColumnsAddAll: TButton;
    btnColumnsRemove: TButton;
    btnColumnsRemoveAll: TButton;
    btnObjectsAdd: TButton;
    btnObjectsRemove: TButton;
    btnObjectsRemoveAll: TButton;
    lstConditions: TImageListBox;
    pnlConditionButtons: TPanel;
    btnConditionEdit: TButton;
    btnConditionDelete: TButton;
    btnConditionAdd: TButton;
    btnDistinct: TCheckBox;
    btnColumnsUp: TBitBtn;
    btnColumnsDown: TBitBtn;
    tbsSort: TTabSheet;
    lstObjectOrdering: TListBox;
    btnOrderAdd: TButton;
    btnOrderAddAll: TButton;
    btnOrderRemove: TButton;
    btnOrderRemoveAll: TButton;
    lstSelectedOrdering: TImageListBox;
    btnOrderUp: TBitBtn;
    btnOrderDown: TBitBtn;
    lblSelectedOrdering: TLabel;
    lblObjectOrdering: TLabel;
    btnHelp: TButton;
    btnColumnsEdit: TBitBtn;
    btnRefreshSchema: TSpeedButton;
    btnShowSQL: TButton;
    imgSort: TImageList;
    btnOrderAsc: TSpeedButton;
    btnOrderDesc: TSpeedButton;
    pnlTop: TPanel;
    imgHeader: TImage;
    lblHeaderDescription: TLabel;
    lblHeaderTitle: TLabel;
    pnlSQL: TPanel;
    Bevel1: TBevel;
    btnFinish: TButton;
    mmoSQL: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure pnlBottonButtonClick(Sender: TObject);
    procedure SetDisplay(Sender: TObject);
    procedure btnObjectsClick(Sender: TObject);
    procedure btnColumnsClick(Sender: TObject);
    procedure SetInitialDisplay(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConditionsClick(Sender: TObject);
    procedure btnConditionDeleteClick(Sender: TObject);
    procedure btnOrderClick(Sender: TObject);
    procedure btnColumnsUpDownClick(Sender: TObject);
    procedure btnOrderUpDownClick(Sender: TObject);
    procedure btnColumnsEditClick(Sender: TObject);
    procedure btnRefreshSchemaClick(Sender: TObject);
    procedure btnShowSQLClick(Sender: TObject);
    procedure btnDistinctClick(Sender: TObject);
    procedure btnOrderAscDescClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);

  private
    // Private methods
    //
    function  NumSelectedItems(parListBox: TCustomListBox): longint;
    procedure SetStatement(value: TcStatement);
    procedure SetFormset(value: TcFormset);
    procedure SetHeader(Index: longint);

  private
    // Private members
    //
    m_objStmt, m_objStatement: TcStatement;
    m_objCustom: TcObject;
    m_FormSet: TcFormSet;

  public
    // public properties
    //
    property Statement: TcStatement             read m_objStatement             write SetStatement;
    property FormSet: TcFormSet                 read m_FormSet                  write SetFormSet;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daResourceStrings,
  frmTextEdit,                // TextEdit()
  frmStatementBuildCondition; // TfrmStatementBuildCondition

const
  krsSHOWSQL = 'Show &SQL';
  krsHIDESQL = 'Hide &SQL';

// TfrmStatementBuild
//   FormCreate
//
procedure TfrmStatementBuild.FormCreate(Sender: TObject);
begin
  m_objStmt := TcStatement.Create(nil);
  m_objStatement := nil;
  m_objCustom := TcObject.Create(nil);
  m_FormSet := nil;
end;

// TfrmStatementBuild
//   FormDestroy
//
procedure TfrmStatementBuild.FormDestroy(Sender: TObject);
begin
  m_objStmt.free;
  m_objCustom.free;
end;

// TfrmStatementBuild
//   FormClose
//
procedure TfrmStatementBuild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mmoSQL.Highlighter := nil;
  Action := caFree;
end;

// TfrmStatementBuild
//   FormShow
//
procedure TfrmStatementBuild.FormShow(Sender: TObject);
begin
  PageControl.Style := tsButtons;
  PageControl.Pages[0].TabVisible := FALSE;
  PageControl.Pages[1].TabVisible := FALSE;
  PageControl.Pages[2].TabVisible := FALSE;
  PageControl.Pages[3].TabVisible := FALSE;
  SetHeader(0);
  SetInitialDisplay(Sender);
  SetDisplay(Sender);
  btnShowSQL.Caption := krsSHOWSQL;
  pnlSQL.Visible := FALSE;
end;

// TfrmStatementBuild
//   SetInitialDisplay
//
procedure TfrmStatementBuild.SetInitialDisplay(Sender: TObject);
var
  i: longint;
begin
  if m_objStmt <> nil then
  begin
    //
    // Objects
    //

    // Left Hand Side
    lstPool.Items.BeginUpdate;
    lstPool.Items.Clear;
    for i := 0 to m_FormSet.Schema.Count - 1 do
      if (m_FormSet.Schema[i] <> nil) and (m_FormSet.Schema[i] is TcSchemaObject) then
        lstPool.Items.AddObject(m_FormSet.Schema[i].sName, m_FormSet.Schema[i]);
    lstPool.Items.EndUpdate;
  end;
  SetState(Sender);
end;

// TfrmStatementBuild
//   SetDisplay
//
procedure TfrmStatementBuild.SetDisplay(Sender: TObject);
var
  p: TcObject;
  i, j, TI: longint;
  q: TcStatementPart;
  lst: TList;
begin
  if m_objStmt <> nil then
  begin
    m_objCustom.clear;
    //
    // Objects
    //
    // Right Hand Side
    lstSelected.Items.BeginUpdate;
    lstSelected.Items.Clear;
    for i := 0 to m_objStmt.Count - 1 do
      if (m_objStmt[i] <> nil) and (m_objStmt[i] is TcStatementPart) and ((m_objStmt[i] as TcStatementPart).ePartType in kesTABLESET) then
        lstSelected.Items.AddObject(m_objStmt[i].Text, m_objStmt[i]);
    lstSelected.Items.EndUpdate;

    //
    // Columns
    //
    // Columns / Left Hand Side & Conditions / Combo Boxes
    TI := lstObjectColumns.TopIndex;
    lstObjectColumns.Items.BeginUpdate;
    lstObjectColumns.Items.Clear;
    for i := 0 to m_objStmt.Count - 1 do
      if (m_objStmt[i] <> nil) and
         (m_objStmt[i] is TcStatementPart) and
         ((m_objStmt[i] as TcStatementPart).ePartType in kesTABLESET) then
      begin
        p := (m_objStmt[i] as TcStatementPart).objPart;
        if p <> nil then
          for j := 0 to p.count - 1 do
            if (p[j] <> nil) and (p[j] is TcSchemaObject) then
            begin
              q := TcStatementPart.Create(m_objCustom);
              m_objCustom.Add(q);
              q.Copy(p[j]);
              q.sPrefix := (m_objStmt[i] as TcStatementPart).sAlias;
              lstObjectColumns.Items.AddObject(q.Text, q);
            end;
      end;
    lstObjectColumns.TopIndex := TI;
    lstObjectColumns.Items.EndUpdate;
    btnDistinct.Checked := m_objStmt.bDistinct;
    // Right Hand Side
    lstSelectedColumns.Items.BeginUpdate;
    lst := nil;
    try
      lst := lstSelectedColumns.Selections;
      lstSelectedColumns.Items.Clear;
      for i := 0 to m_objStmt.Count - 1 do
        if (m_objStmt[i] <> nil) and (m_objStmt[i] is TcStatementPart) and ((m_objStmt[i] as TcStatementPart).ePartType in kesCOLUMNSET) then
          lstSelectedColumns.Items.AddObject(m_objStmt[i].Text, m_objStmt[i]);
      lstSelectedColumns.Selections := lst;
      lstSelectedColumns.Items.EndUpdate;
    finally
      lst.free;
    end;
    //
    // Where Clause
    //
    lstConditions.Items.BeginUpdate;
    lst := nil;
    try
      lst := lstConditions.Selections;
      lstConditions.Items.Clear;
      for i := 0 to m_objStmt.Count - 1 do
        if (m_objStmt[i] <> nil) and (m_objStmt[i] is TcStatementPartCondition) then
          lstConditions.Items.AddObject(m_objStmt[i].Text, m_objStmt[i]);
      lstConditions.Selections := lst;
      lstConditions.Items.EndUpdate;
    finally
      lst.free;
    end;
    //
    // Order by clause
    //
    TI := lstObjectOrdering.TopIndex;
    lstObjectOrdering.Items.BeginUpdate;
    lstObjectOrdering.Items.Clear;
    for i := 0 to m_objStmt.Count - 1 do
      if (m_objStmt[i] <> nil) and
         (m_objStmt[i] is TcStatementPart) and
         ((m_objStmt[i] as TcStatementPart).ePartType in kesTABLESET) then
      begin
        p := (m_objStmt[i] as TcStatementPart).objPart;
        if p <> nil then
          for j := 0 to p.count - 1 do
            if (p[j] <> nil) and (p[j] is TcSchemaObject) and (p[j].sName <> krsSTAR) then
            begin
              q := TcStatementPart.Create(m_objCustom);
              m_objCustom.Add(q);
              q.Copy(p[j]);
              q.ePartType := epOrder;
              q.sPrefix := (m_objStmt[i] as TcStatementPart).sAlias;
              lstObjectOrdering.Items.AddObject(q.Text, q);
            end;
      end;
    lstObjectOrdering.TopIndex := TI;
    lstObjectOrdering.Items.EndUpdate;
    // Right Hand Side
    lstSelectedOrdering.Items.BeginUpdate;
    lst := nil;
    try
      lst := lstSelectedOrdering.Selections;
      lstSelectedOrdering.Items.Clear;
      for i := 0 to m_objStmt.Count - 1 do
        if (m_objStmt[i] <> nil) and (m_objStmt[i] is TcStatementPart) and ((m_objStmt[i] as TcStatementPart).ePartType = epOrder) then
          lstSelectedOrdering.Items.AddObject(m_objStmt[i].Text, m_objStmt[i]);
      lstSelectedOrdering.Selections := lst;
      lstSelectedOrdering.Items.EndUpdate;
    finally
      lst.free;
    end;
    //
    // SQL Display
    mmoSQL.Lines.BeginUpdate;
    mmoSQL.Text := m_objStmt.SQL;
    mmoSQL.Lines.EndUpdate;
  end;
  // States
  SetState(Sender);
end;

// TfrmStatementBuild
//   SetState
//
procedure TfrmStatementBuild.SetState(Sender: TObject);
var
  p: TcStatementPart;
begin
  btnPrevious.Enabled := PageControl.ActivePage <> tbsObjects;
  btnNext.Enabled := PageControl.ActivePage <> tbsSort;
  btnFinish.Enabled := TRUE;
  //
  // Objects
  //
  if PageControl.ActivePage = tbsObjects then
  begin
    btnObjectsAdd.Enabled := NumSelectedItems(lstPool) > 0;
    btnObjectsRemove.Enabled := NumSelectedItems(lstSelected) > 0;
    btnObjectsRemoveAll.Enabled := lstSelected.Items.Count > 0;
  end
  //
  // Columns
  //
  else if PageControl.ActivePage = tbsColumns then
  begin
    btnColumnsAdd.Enabled := NumSelectedItems(lstObjectColumns) > 0;
    btnColumnsAddAll.Enabled := lstObjectColumns.Items.Count > 0;
    btnColumnsRemove.Enabled := NumSelectedItems(lstSelectedColumns) > 0;
    btnColumnsRemoveAll.Enabled := lstSelectedColumns.Items.Count > 0;
    with lstSelectedColumns do
    begin
      btnColumnsUp.Enabled := (NumSelectedItems(lstSelectedColumns) > 0) and (Items.Count > 0) and not Selected[0];
      btnColumnsDown.Enabled := (NumSelectedItems(lstSelectedColumns) > 0) and (Items.Count > 0) and not Selected[Items.Count - 1];
    end;
    btnColumnsEdit.Enabled := NumSelectedItems(lstSelectedColumns) = 1;
  end
  //
  // Where Clause
  //
  else if PageControl.ActivePage = tbsWhere then
  begin
    btnConditionEdit.Enabled := lstConditions.ItemIndex <> kiUNDEFINED;
    btnConditionDelete.Enabled := btnConditionEdit.Enabled;
  end
  //
  // Order Clause
  //
  else if PageControl.ActivePage = tbsSort then
  begin
    // Add Edit Remove
    btnOrderAdd.Enabled := NumSelectedItems(lstObjectOrdering) > 0;
    btnOrderAddAll.Enabled := lstObjectOrdering.Items.Count > 0;
    btnOrderRemove.Enabled := NumSelectedItems(lstSelectedOrdering) > 0;
    btnOrderRemoveAll.Enabled := lstSelectedOrdering.Items.Count > 0;
    // Asc Desc clause
    p := nil;
    if NumSelectedItems(lstSelectedOrdering) = 1 then
      with lstSelectedOrdering do
        p := TcStatementPart(Items.Objects[ItemIndex]);
    btnOrderAsc.Enabled := p <> nil;
    btnOrderDesc.Enabled := p <> nil;
    btnOrderAsc.Down := (p <> nil) and not (epoOrderDesc in p.Options);
    //btnOrderDesc.Down := (p <> nil) and (epoOrderDesc in p.Options);
    // Up Down
    with lstSelectedOrdering do
    begin
      btnOrderUp.Enabled := (NumSelectedItems(lstSelectedOrdering) > 0) and (Items.Count > 0) and not Selected[0];
      btnOrderDown.Enabled := (NumSelectedItems(lstSelectedOrdering) > 0) and (Items.Count > 0) and not Selected[Items.Count - 1];
    end;
  end;
end;

// TfrmStatementBuild
//   pnlBottonButtonClick
//
procedure TfrmStatementBuild.pnlBottonButtonClick(Sender: TObject);
begin
  with PageControl do
    if (Sender = btnPrevious) and (ActivePageIndex > 0) then
      SetHeader(PageControl.ActivePageIndex - 1)
    else if (Sender = btnNext) and (ActivePageIndex < PageCount - 1) then
      SetHeader(PageControl.ActivePageIndex + 1)
    else if (Sender = btnNext) and (ActivePageIndex = PageCount - 1) then
      btnFinishClick(Sender);
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnFinishClick
//
procedure TfrmStatementBuild.btnFinishClick(Sender: TObject);
begin
  m_objStmt.bDistinct := btnDistinct.Checked;
  m_objStatement.Copy(m_objStmt);
  m_objStatement.Schema := m_FormSet.Schema;
  ModalResult := mrOK;
end;

// TfrmStatementBuild
//   btnObjectsClick
//
procedure TfrmStatementBuild.btnObjectsClick(Sender: TObject);
var
  i, j, L: longint;
  p: TcStatementPart;
begin
  //
  // Add
  if (m_objStmt <> nil) and ((Sender = btnObjectsAdd) or (Sender = lstPool)) then
  begin
    for i := 0 to lstPool.Items.Count - 1 do
      if (lstPool.Selected[i]) and (lstPool.Items.Objects[i] <> nil) then
      begin
        // 1. Add object
        L := m_objStmt.Add(TcObject(lstPool.Items.Objects[i]));
        // 2. Establish relationships
        if (L <> kiUNDEFINED) and (m_objStmt.PartCount[kesTABLESET] > 1) then
          m_objStmt.EstablishRelation(m_objStmt[L] as TcStatementPart);
        lstPool.Selected[i] := FALSE;
      end;
  end
  //
  // Remove / Remove All
  else if (m_objStmt <> nil) and ((Sender = btnObjectsRemove) or (Sender = btnObjectsRemoveAll) or (Sender = lstSelected)) then
  begin
    for i := 0 to lstSelected.Items.Count - 1 do
      if ((lstSelected.Selected[i] and ((Sender = btnObjectsRemove) or (Sender = lstSelected))) or (Sender = btnObjectsRemoveAll)) and (lstSelected.Items.Objects[i] <> nil) then
      begin
        p := TcObject(lstSelected.Items.Objects[i]) as TcStatementPart;
        for j := m_objStmt.Count - 1 downto 0 do
          if (m_objStmt[j] <> nil) and
             (
               ( (m_objStmt[j] is TcStatementPart) and
                 ((m_objStmt[j] as TcStatementPart).ePartType in (kesCOLUMNSET + [epOrder])) and
                 ((m_objStmt[j] as TcStatementPart).sPrefix = p.sAlias)
               ) or
               ( (m_objStmt[j] is TcStatementPartCondition) and
                 (
                   ((m_objStmt[j] as TcStatementPartCondition).LHS.sPrefix = p.sAlias) or
                   ((m_objStmt[j] as TcStatementPartCondition).RHS.sPrefix = p.sAlias)
                 )
               )
             ) then
            m_objStmt.Delete(j);
        m_objStmt.Delete(p);
      end;
  end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   NumSelectedItems
//
function TfrmStatementBuild.NumSelectedItems(parListBox: TCustomListBox): longint;
var
  i: longint;
begin
  result := 0;
  for i := 0 to parListBox.Items.Count - 1 do
    if parListBox.Selected[i] then
      inc(result);
end;

// TfrmStatementBuild
//   btnColumnsClick
//
procedure TfrmStatementBuild.btnColumnsClick(Sender: TObject);
var
  i: longint;
  p: TcStatementPart;
begin
  //
  // Add
  if (m_objStmt <> nil) and ((Sender = btnColumnsAdd) or (Sender = btnColumnsAddAll) or (Sender = lstObjectColumns)) then
  begin
    for i := 0 to lstObjectColumns.Items.Count - 1 do
      if (lstObjectColumns.Selected[i] or (Sender = btnColumnsAddAll)) and (lstObjectColumns.Items.Objects[i] <> nil) then
      begin
        p := TcStatementPart.Create(m_objStmt);
        m_objStmt.Add(p);
        p.Copy(TcObject(lstObjectColumns.Items.Objects[i]));
      end;
  end
  //
  // Remove / Remove All
  else if (m_objStmt <> nil) and ((Sender = btnColumnsRemove) or (Sender = btnColumnsRemoveAll) or (Sender = lstSelectedColumns)) then
  begin
    for i := 0 to lstSelectedColumns.Items.Count - 1 do
      if ((lstSelectedColumns.Selected[i] and ((Sender = btnColumnsRemove) or (Sender = lstSelectedColumns))) or (Sender = btnColumnsRemoveAll)) and (lstSelectedColumns.Items.Objects[i] <> nil) then
        m_objStmt.Delete(TcObject(lstSelectedColumns.Items.Objects[i]));
  end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnConditionsAddClick
//
procedure TfrmStatementBuild.btnConditionsClick(Sender: TObject);
var
  frm: TfrmStatementBuildCondition;
begin
  frm := nil;
  if (((Sender = btnConditionEdit) or (Sender = lstConditions)) and (btnConditionEdit.Enabled) and (lstConditions.ItemIndex <> kiUNDEFINED)) or
     (Sender = btnConditionAdd) then
  try
    frm := TfrmStatementBuildCondition.Create(self);
    frm.FormSet := m_FormSet;
    frm.Statement := m_objStmt;
    if Sender = btnConditionAdd then
      frm.Part := nil
    else if ((Sender = btnConditionEdit) or (Sender = lstConditions)) then
      with lstConditions do
        frm.Part := TcStatementPartCondition(Items.Objects[ItemIndex]);
    if frm.ShowModal = mrOK then
      SetDisplay(Sender);
  finally
    frm.release;
  end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnConditionDeleteClick
//
procedure TfrmStatementBuild.btnConditionDeleteClick(Sender: TObject);
var
  p: TcStatementPartCondition;
begin
  if (btnConditionDelete.Enabled) and (lstConditions.ItemIndex <> kiUNDEFINED) then
  begin
    with lstConditions do
      p := TcStatementPartCondition(Items.Objects[ItemIndex]);
    if (p <> nil) and (Application.MessageBox(PChar(Format('%s: Delete Condition?', [p.Text])), krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES) then
    begin
      m_objStmt.Delete(p);
      SetDisplay(Sender);
    end;
  end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnOrderClick
//
procedure TfrmStatementBuild.btnOrderClick(Sender: TObject);
var
  i: longint;
  p: TcStatementPart;
begin
  //
  // Add
  if (m_objStmt <> nil) and ((Sender = btnOrderAdd) or (Sender = btnOrderAddAll) or (Sender = lstObjectOrdering)) then
  begin
    for i := 0 to lstObjectOrdering.Items.Count - 1 do
      if (lstObjectOrdering.Selected[i] or (Sender = btnOrderAddAll)) and (lstObjectOrdering.Items.Objects[i] <> nil) then
      begin
        p := TcStatementPart.Create(m_objStmt);
        m_objStmt.Add(p);
        p.Copy(TcObject(lstObjectOrdering.Items.Objects[i]));
      end;
  end
  //
  // Remove / Remove All
  else if (m_objStmt <> nil) and ((Sender = btnOrderRemove) or (Sender = btnOrderRemoveAll) or (Sender = lstSelectedOrdering)) then
  begin
    for i := 0 to lstSelectedOrdering.Items.Count - 1 do
      if ((lstSelectedOrdering.Selected[i] and ((Sender = btnOrderRemove) or (Sender = lstSelectedOrdering))) or (Sender = btnOrderRemoveAll)) and (lstSelectedOrdering.Items.Objects[i] <> nil) then
        m_objStmt.Delete(TcObject(lstSelectedOrdering.Items.Objects[i]));
  end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnColumnsUpDownClick
//
procedure TfrmStatementBuild.btnColumnsUpDownClick(Sender: TObject);
var
  p, q: TcStatementPart;
  i, L: longint;
begin
  //
  // UP
  //
  if Sender = btnColumnsUp then
    for i := 0 to lstSelectedColumns.Items.Count - 1 do
      if lstSelectedColumns.Selected[i] then
      begin
        p := lstSelectedColumns.Items.Objects[i] as TcStatementPart;
        L := p.PartIndex;
        if L >= 1 then
        begin
          q := m_objStmt.Part[[p.ePartType], L - 1];
          if q <> nil then
            m_objStmt.Exchange(p.ParentIndex, q.ParentIndex);
        end;
      end;
  //
  // DOWN
  //
  if Sender = btnColumnsDown then
    for i := lstSelectedColumns.Items.Count - 1 downto 0 do
      if lstSelectedColumns.Selected[i] then
      begin
        p := lstSelectedColumns.Items.Objects[i] as TcStatementPart;
        L := p.PartIndex;
        if L < m_objStmt.PartCount[[p.ePartType]] then
        begin
          q := m_objStmt.Part[[p.ePartType], L + 1];
          if q <> nil then
            m_objStmt.Exchange(p.ParentIndex, q.ParentIndex);
        end;
      end;
  //
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnOrderUpDownClick
//
procedure TfrmStatementBuild.btnOrderUpDownClick(Sender: TObject);
var
  p, q: TcStatementPart;
  i, L: longint;
begin
  //
  // UP
  //
  if Sender = btnOrderUp then
    for i := 0 to lstSelectedOrdering.Items.Count - 1 do
      if lstSelectedOrdering.Selected[i] then
      begin
        p := lstSelectedOrdering.Items.Objects[i] as TcStatementPart;
        L := p.PartIndex;
        if L >= 1 then
        begin
          q := m_objStmt.Part[[p.ePartType], L - 1];
          if q <> nil then
            m_objStmt.Exchange(p.ParentIndex, q.ParentIndex);
        end;
      end;
  //
  // DOWN
  //
  if Sender = btnOrderDown then
    for i := lstSelectedOrdering.Items.Count - 1 downto 0 do
      if lstSelectedOrdering.Selected[i] then
      begin
        p := lstSelectedOrdering.Items.Objects[i] as TcStatementPart;
        L := p.PartIndex;
        if L < m_objStmt.PartCount[[p.ePartType]] then
        begin
          q := m_objStmt.Part[[p.ePartType], L + 1];
          if q <> nil then
            m_objStmt.Exchange(p.ParentIndex, q.ParentIndex);
        end;
      end;
  //
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnColumnsEditClick
//
procedure TfrmStatementBuild.btnColumnsEditClick(Sender: TObject);
var
  i: longint;
  p: TcObject;
  s: String;
begin
  if NumSelectedItems(lstSelectedColumns) = 1 then
    for i := 0 to lstSelectedColumns.Items.Count - 1 do
      if lstSelectedColumns.Selected[i] then
      begin
        p := TcObject(lstSelectedColumns.Items.Objects[i]);
        if (p <> nil) and (p is TcStatementPart) then
        begin
          s := (p as TcStatementPart).Text;
          if EditText(m_FormSet, s) then
          begin
            (p as TcStatementPart).sRenamedAs := s;
            SetDisplay(Sender);
          end;
        end;
      end;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   SetFormset
//
procedure TfrmStatementBuild.SetFormset(value: TcFormset);
begin
  m_FormSet := value;
  if m_FormSet <> nil then
  begin
    m_objStmt.Schema := m_FormSet.Schema;
    mmoSQL.Font.Assign(m_FormSet.Preferences.Font[krsPREF_FONTCONSOLE]);
    m_FormSet.SetHighlighter(mmoSQL);
  end;
end;

// TfrmStatementBuild
//   SetStatement
//
procedure TfrmStatementBuild.SetStatement(value: TcStatement);
begin
  m_objStatement := value;
  m_objStmt.Copy(m_objStatement);
  if m_FormSet <> nil then
    m_objStmt.Schema := m_FormSet.Schema;
end;

// TfrmStatementBuild
//   btnRefreshSchemaClick
//
procedure TfrmStatementBuild.btnRefreshSchemaClick(Sender: TObject);
begin
  if m_FormSet <> nil then
  try
    screen.Cursor := crAppStart;
    m_FormSet.Schema.Clear;
    m_objStmt.Schema := m_FormSet.Schema;
    m_objStatement.Schema := m_FormSet.Schema;
    SetInitialDisplay(Sender);
    SetDisplay(Sender);
  finally
    screen.Cursor := crDefault;
  end;
  SetState(Sender);
end;

// TfrmStatementBuild
//   btnShowSQLClick
//
procedure TfrmStatementBuild.btnShowSQLClick(Sender: TObject);
const
  kiEXPANDBY = 150;
  kiPANELHEIGHT = 31;
const
  kasSHOWSQLCAPTION: array[boolean] of String =
    (krsSHOWSQL, krsHIDESQL);
begin
  pnlSQL.Height := kiEXPANDBY;

  pnlSQL.Visible := not pnlSQL.Visible;
  if pnlSQL.Visible then
    Height := Height + kiEXPANDBY
  else
    Height := Height - kiEXPANDBY;
  btnShowSQL.Caption := kasSHOWSQLCAPTION[pnlSQL.Visible];
end;

// TfrmStatementBuild
//   btnDistinctClick
//
procedure TfrmStatementBuild.btnDistinctClick(Sender: TObject);
begin
  m_objStmt.bDistinct := btnDistinct.Checked;
  SetDisplay(Sender);
end;

// TfrmStatementBuild
//   btnOrderAscDescClick
//
procedure TfrmStatementBuild.btnOrderAscDescClick(Sender: TObject);
var
  p: TcStatementPart;
begin
  if NumSelectedItems(lstSelectedOrdering) = 1 then
  begin
    with lstSelectedOrdering do
      p := TcStatementPart(Items.Objects[ItemIndex]);
    if p <> nil then
    begin
      if Sender = btnOrderAsc then
        p.Options := p.Options - [epoOrderDesc]
      else if Sender = btnOrderDesc then
        p.Options := p.Options + [epoOrderDesc];
      SetDisplay(Sender);
    end;
  end;
end;

// TfrmStatementBuild
//   SetHeader
//
procedure TfrmStatementBuild.SetHeader(Index: longint);
const
  kasPAGECAPTION: array[0 .. 3] of String =
    ('Select one or more objects from the database object pool to create a query.',
     'Select one or more object columns from the object column pool.',
     'Create retrieval conditions on database objects.',
     'Create sortinf order within database objects.');
  kasNEXTCAPTION: array[0 .. 3] of String =
    ('&Next', '&Next', '&Next', '&Finish');
begin
  PageControl.ActivePageIndex := Index;
  if (m_FormSet <> nil) then
    m_FormSet.SetHeaderImage(imgHeader, Index + 1);
  lblHeaderDescription.Caption := kasPAGECAPTION[Index];
  btnNext.Caption := kasNEXTCAPTION[Index];
  imgHeader.Refresh;
  SetState(nil);
end;

end.

