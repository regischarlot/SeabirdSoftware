unit frmStatementBuildCondition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StatementLib, daObjectLib, FormLib, ExtCtrls;

type
  TfrmStatementBuildCondition = class(TForm)
    lblConditionLHSColumn: TLabel;
    lblConditionOperator: TLabel;
    cboConditionLHSColumn: TComboBox;
    cboConditionOperator: TComboBox;
    btnConditionRHSColumn: TRadioButton;
    cboConditionRHSColumn: TComboBox;
    btnConditionRHSValue: TRadioButton;
    edtConditionRHSValue: TEdit;
    btnConditionRHSDistinctValues: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    btnLeftOuterJoin: TCheckBox;
    btnRightOuterJoin: TCheckBox;
    bvlRHSValue: TBevel;
    Bevel1: TBevel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConditionRHSDistinctValuesClick(Sender: TObject);
    procedure btnOuterJoinClick(Sender: TObject);

  private
    // Private methods
    //
    procedure SetCheck(Sender: TCheckBox; value: boolean);

  private
    // Private members
    //
    m_objFormSet: TcFormSet;
    m_objStatement: TcStatement;
    m_objStatementPart: TcStatementPartCondition;
    m_objCustom: TcObject;
    m_bNotReentrant: boolean;

  public
    // public properties
    //
    property FormSet: TcFormSet                         read m_objFormSet       write m_objFormSet;
    property Statement: TcStatement                     read m_objStatement     write m_objStatement;
    property Part: TcStatementPartCondition             read m_objStatementPart write m_objStatementPart;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daResourceStrings,
  frmStatementBuildDistinctValues; // TfrmStatementBuildDistinctValues

// TfrmStatementBuildCondition
//   FormCreate
//
procedure TfrmStatementBuildCondition.FormCreate(Sender: TObject);
begin
  m_objFormSet := nil;
  m_objStatement := nil;
  m_objCustom := TcObject.Create(nil);
  m_objStatementPart := nil;
  m_bNotReentrant := FALSE;
end;

// TfrmStatementBuildCondition
//   FormCreate
//
procedure TfrmStatementBuildCondition.FormDestroy(Sender: TObject);
begin
  m_objCustom.free;
end;

// frmStatementBuildCondition
//   FormShow
//
procedure TfrmStatementBuildCondition.FormShow(Sender: TObject);
var
  i, j: longint;
  e: TeConditionOperator;
  p: TcObject;
  q: TcStatementPart;
begin
  if m_objStatement <> nil then
  begin
    //
    // A. Set Default Content
    //

    // Operators
    cboConditionOperator.Items.BeginUpdate;
    cboConditionOperator.Items.Clear;
    for e := low(TeConditionOperator) to high(TeConditionOperator) do
      if kasCONDITIONOPERATORS[e] <> ksEMPTY then
        cboConditionOperator.Items.AddObject(kasCONDITIONOPERATORS[e], pointer(e));
    cboConditionOperator.ItemIndex := 0;
    cboConditionOperator.Items.EndUpdate;
    //
    // Columns
    //
    // Left Hand Side & Condition Combo Boxes
    cboConditionLHSColumn.Items.BeginUpdate;
    cboConditionLHSColumn.Items.Clear;
    cboConditionRHSColumn.Items.BeginUpdate;
    cboConditionRHSColumn.Items.Clear;
    m_objCustom.clear;
    for i := 0 to m_objStatement.Count - 1 do
      if (m_objStatement[i] <> nil) and
         (m_objStatement[i] is TcStatementPart) and
         ((m_objStatement[i] as TcStatementPart).ePartType in kesTABLESET) then
      begin
        p := (m_objStatement[i] as TcStatementPart).objPart;
        if p <> nil then
          for j := 0 to p.count - 1 do
            if (p[j] <> nil) and (p[j] is TcSchemaObject) then
            begin
              q := TcStatementPart.Create(m_objCustom);
              m_objCustom.Add(q);
              q.Copy(p[j]);
              q.sPrefix := (m_objStatement[i] as TcStatementPart).sAlias;
              if p[j].sName <> krsSTAR then
              begin
                cboConditionRHSColumn.Items.AddObject(q.Text, q);
                cboConditionLHSColumn.Items.AddObject(q.Text, q);
              end;
            end;
      end;
    cboConditionRHSColumn.Items.EndUpdate;
    cboConditionLHSColumn.Items.EndUpdate;

    //
    // B. Set Values
    //

    // Clear Condition
    cboConditionLHSColumn.ItemIndex := kiUNDEFINED;
    cboConditionOperator.ItemIndex := 0;
    btnConditionRHSColumn.Checked := FALSE;
    cboConditionRHSColumn.ItemIndex := kiUNDEFINED;
    btnConditionRHSValue.Checked := FALSE;
    edtConditionRHSValue.Text := ksEMPTY;
    // Set Display
    if (m_objStatementPart <> nil) and (m_objStatementPart is TcStatementPartCondition) then
    begin
      // Right End Side Column
      for i := 0 to cboConditionLHSColumn.Items.Count - 1 do
        if (cboConditionLHSColumn.Items.Objects[i] <> nil) and
           (TObject(cboConditionLHSColumn.Items.Objects[i]) is TcStatementPart) and
           (m_objStatementPart.LHS.Compare(TObject(cboConditionLHSColumn.Items.Objects[i]) as TcStatementPart)) then
        begin
          cboConditionLHSColumn.ItemIndex := i;
          break;
        end;
      // Operator
      for i := 0 to cboConditionOperator.Items.Count - 1 do
        if longint(cboConditionOperator.Items.Objects[i]) = longint(m_objStatementPart.eConditionOperator) then
        begin
          cboConditionOperator.ItemIndex := i;
          break;
        end;
      // Radio Buttons
      btnConditionRHSColumn.Checked := m_objStatementPart.eRHSType = eorColumn;
      btnConditionRHSValue.Checked := (m_objStatementPart.eRHSType = eorValue) and not (m_objStatementPart.eConditionOperator in [ecoIsNull, ecoIsNotNull]);
      // Right Hand Side Column
      if btnConditionRHSColumn.Checked then
        for i := 0 to cboConditionRHSColumn.Items.Count - 1 do
          if (cboConditionRHSColumn.Items.Objects[i] <> nil) and
             (TObject(cboConditionRHSColumn.Items.Objects[i]) is TcStatementPart) and
             (m_objStatementPart.RHS.Compare(TObject(cboConditionRHSColumn.Items.Objects[i]) as TcStatementPart)) then
          begin
            cboConditionRHSColumn.ItemIndex := i;
            break;
          end;
      // Right Hand Side Value
      if btnConditionRHSValue.Checked then
        edtConditionRHSValue.Text := m_objStatementPart.sRHS;
      // Outer join
      SetCheck(btnRightOuterJoin, esoRightOuter in m_objStatementPart.eOptions);
      SetCheck(btnLeftOuterJoin, esoLeftOuter in m_objStatementPart.eOptions);
    end;
  end;
  SetState(Sender);
end;

// frmStatementBuildCondition
//   btnOKClick
//
procedure TfrmStatementBuildCondition.btnOKClick(Sender: TObject);
var
  e: TeConditionOperator;
  pLHS, pRHS: TcStatementPart;
begin
  if (m_objStatement <> nil) and (Sender as TControl).Enabled then
  begin
    // Left Hand Side
    with cboConditionLHSColumn do
      pLHS := TcStatementPart(Items.Objects[ItemIndex]);
    // Operator
    with cboConditionOperator do
      e := TeConditionOperator(Items.Objects[ItemIndex]);
    // Right Hand Side
    pRHS := nil;
    if btnConditionRHSColumn.Checked then
      with cboConditionRHSColumn do
        pRHS := TcStatementPart(Items.Objects[ItemIndex]);
    // Create Object or Set object
    if m_objStatementPart = nil then
    begin
      if pRHS <> nil then
        m_objStatementPart := m_objStatement.AddCondition(pLHS, e, pRHS)
      else
        m_objStatementPart := m_objStatement.AddCondition(pLHS, e, edtConditionRHSValue.Text);
    end
    else
    begin
      if pRHS <> nil then
        m_objStatementPart.SetCondition(pLHS, e, pRHS)
      else
        m_objStatementPart.SetCondition(pLHS, e, edtConditionRHSValue.Text);
    end;
    // Indexes
    if (pLHS <> nil) and (pLHS.objPart <> nil) then
      m_objStatementPart.iLHS := pLHS.objPart.ParentIndex;
    if (pRHS <> nil) and (pRHS.objPart <> nil) then
      m_objStatementPart.iRHS := pRHS.objPart.ParentIndex;
    // Outer join
    if btnRightOuterJoin.Checked then
      m_objStatementPart.eOptions := m_objStatementPart.eOptions + [esoRightOuter]
    else
      m_objStatementPart.eOptions := m_objStatementPart.eOptions - [esoRightOuter];

    if btnLeftOuterJoin.Checked then
      m_objStatementPart.eOptions := m_objStatementPart.eOptions + [esoLeftOuter]
    else
      m_objStatementPart.eOptions := m_objStatementPart.eOptions - [esoLeftOuter];
    // Done.
    ModalResult := mrOK;
  end;
end;

// TfrmStatementBuildCondition
//   SetState
//
procedure TfrmStatementBuildCondition.SetState(Sender: TObject);
var
  e: TeConditionOperator;
begin
  e := ecoNone;
  with cboConditionOperator do
    if ItemIndex <> kiUNDEFINED then
      e := TeConditionOperator(Items.Objects[ItemIndex]);
  //
  btnConditionRHSColumn.Enabled := not (e in [ecoIsNull, ecoIsNotNull]);
  cboConditionRHSColumn.Enabled := btnConditionRHSColumn.Enabled and btnConditionRHSColumn.Checked;
  btnRightOuterJoin.Enabled := cboConditionRHSColumn.Enabled;
  btnConditionRHSValue.Enabled := btnConditionRHSColumn.Enabled;
  edtConditionRHSValue.Enabled := btnConditionRHSValue.Enabled and btnConditionRHSValue.Checked;
  btnConditionRHSDistinctValues.Enabled := edtConditionRHSValue.Enabled and (cboConditionLHSColumn.ItemIndex <> kiUNDEFINED);
  btnLeftOuterJoin.Enabled := cboConditionLHSColumn.ItemIndex <> kiUNDEFINED;
  //
  btnOK.Enabled := (cboConditionLHSColumn.ItemIndex <> kiUNDEFINED) and
                   (
                     (e in [ecoIsNull, ecoIsNotNull]) or
                     (
                       (e <> ecoNone) and
                       (
                         (
                           (kaeRHSREQUIREMENT[e] in [eorColumn, eorBoth]) and
                           btnConditionRHSColumn.Checked and
                           (cboConditionRHSColumn.ItemIndex <> kiUNDEFINED)
                         ) or
                         (
                           (kaeRHSREQUIREMENT[e] in [eorValue, eorBoth]) and
                           btnConditionRHSValue.Checked and
                           (edtConditionRHSValue.Text <> ksEMPTY)
                         )
                       )
                     )
                   );
end;

// TfrmStatementBuildCondition
//   btnConditionRHSDistinctValuesClick
//
procedure TfrmStatementBuildCondition.btnConditionRHSDistinctValuesClick(Sender: TObject);
var
  frm: TfrmStatementBuildDistinctValues;
begin
  frm := nil;
  if cboConditionLHSColumn.ItemIndex <> kiUNDEFINED then 
  try
    frm := TfrmStatementBuildDistinctValues.Create(self);
    with cboConditionLHSColumn do
      frm.Part := TcStatementPart(Items.Objects[ItemIndex]);
    frm.FormSet := m_objFormSet;
    frm.Value := edtConditionRHSValue.Text;
    if frm.ShowModal = mrOK then
      edtConditionRHSValue.Text := frm.Value;
  finally
    frm.Release;
  end;
end;

// TfrmStatementBuildCondition
//   btnOuterJoinClick
//
procedure TfrmStatementBuildCondition.btnOuterJoinClick(Sender: TObject);
begin
  if Sender is TCheckBox then
    SetCheck(Sender as TCheckBox, (Sender as TCheckBox).Checked);
end;

// TfrmStatementBuildCondition
//   SetCheck
//
procedure TfrmStatementBuildCondition.SetCheck(Sender: TCheckBox; value: boolean);
begin
  if not m_bNotReentrant then
  begin
    m_bNotReentrant := TRUE;
    if (Sender = btnLeftOuterJoin) and value then
      btnRightOuterJoin.Checked := FALSE
    else if (Sender = btnRightOuterJoin) and value then
      btnLeftOuterJoin.Checked := FALSE;
    Sender.Checked := value;
    m_bNotReentrant := FALSE;
  end;
end;

end.
