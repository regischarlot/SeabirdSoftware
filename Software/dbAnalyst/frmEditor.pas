unit frmEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Grid,
  FormLib, DataLib, ConnectionLib, ImgList, Buttons, ActnList,
  StdActns, SynEdit;

type
  TeFormMode = (fmEdit, fmNew);

  TfrmEditor = class(TForm)
    PageControl: TPageControl;
    tsGeneral: TTabSheet;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    ImageList: TImageList;
    pnlPage: TScrollBox;
    lblStatus: TLabel;
    ActionList: TActionList;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditUndo: TEditUndo;
    pnlTop: TPanel;
    imgHeader: TImage;
    lblHeaderDescription: TLabel;
    lblHeaderTitle: TLabel;
    pnlTop2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);

  private
    // Private methods
    //
    procedure   SetFormSet(value: TcFormSet);
    procedure   OnFieldLostFocus(Sender: TObject);
    procedure   OnExtendedKeyPress(Sender: TObject; var Key: Char);
    function    CheckAllRequired: TWinControl;
    procedure   OnSynStatusChanges(Sender: TObject; Changes: TSynStatusChanges);
    procedure   OnCursorLocation(Sender: TObject);

  private
    // Private members
    //
    m_FormSet: TcFormSet;
    m_objMetaEditor: TcMetaData;
    m_objData: TcData;
    m_eMode: TeFormMode;
    m_lstObjects: TList;
    m_acolField: array[boolean] of TColor;

  public
    // public properties
    //
    property    FormSet: TcFormSet              read m_FormSet                  write SetFormSet;
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Mode: TeFormMode                read m_eMode                    write m_eMode;
    property    MetaEditor: TcMetaData          read m_objMetaEditor            write m_objMetaEditor;
  end;

implementation

{$R *.dfm}

uses
  daObjectLib,
  daGlobals,
  daResourceStrings,
  ExecuteLib,
  StatementLib,
  Math,
  strUtils;

type
  //
  // TExtendedEdit
  //
  TExtendedEdit = class(TEdit)
  private
    // Private members
    //
    m_objData: TcData;
    m_objMetaEditor: TcMetaData;

  private
    // Private Members
    //
    function    GetCheckRequired: boolean;
    function    GetDisplay: String;

  public
    // public properties
    //
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Editor: TcMetaData              read m_objMetaEditor            write m_objMetaEditor;
    property    CheckRequired: boolean          read GetCheckRequired;
    property    Display: String                 read GetDisplay;
  end;

  //
  // TExtendedCombo
  //
  TExtendedCombo = class(TComboBox)
  private
    // Private members
    //
    m_objData: TcData;
    m_objMetaEditor: TcMetaData;

  private
    // Private Members
    //
    function    GetCheckRequired: boolean;
    function    GetDisplay: String;

  public
    // public properties
    //
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Editor: TcMetaData              read m_objMetaEditor            write m_objMetaEditor;
    property    CheckRequired: boolean          read GetCheckRequired;
    property    Display: String                 read GetDisplay;
  end;

  //
  // TExtendedCombo
  //
  TExtendedCheckbox = class(TCheckbox)
  private
    // Private members
    //
    m_objData: TcData;
    m_objMetaEditor: TcMetaData;
    FTRUE, FFALSE: String;
    FabBOOLEAN: array[boolean] of String;

  private
    // Private Members
    //
    function    GetCheckRequired: boolean;
    function    GetDisplay: String;
    function    GetBooleanValues: String;
    procedure   SetBooleanValues(value: String);
    function    GetBooleanState: String;
    procedure   SetBooleanState(value: String);

  public
    // public properties
    //
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Editor: TcMetaData              read m_objMetaEditor            write m_objMetaEditor;
    property    CheckRequired: boolean          read GetCheckRequired;
    property    Display: String                 read GetDisplay;
    property    BooleanValues: String           read GetBooleanValues           write SetBooleanValues;
    property    BooleanState: String            read GetBooleanState            write SetBooleanState;
  end;

  //
  // TExtendedMemo
  //
  TExtendedMemo = class(TSynEdit)
  private
    // Private members
    //
    m_objData: TcData;
    m_objMetaEditor: TcMetaData;

  private
    // Private Members
    //
    function    GetCheckRequired: boolean;
    function    GetDisplay: String;

  public
    // public properties
    //
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Editor: TcMetaData              read m_objMetaEditor            write m_objMetaEditor;
    property    CheckRequired: boolean          read GetCheckRequired;
    property    Display: String                 read GetDisplay;
  end;

  //
  // TExtendedGrid
  //
  TExtendedGrid = class(TCustomPanel)
  private
    // Private members
    //
    FBevel: TBevel;
    FLabel: TStaticText;
    FGrid: TGrid;
    FAdd, FDelete: TButton;
    m_objData: TcData;
    m_objMetaEditor: TcMetaData;

  private
    // Private Members
    //
    function    GetCheckRequired: boolean;
    function    GetDisplay: String;
    procedure   gridColumnsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure   SetOnChangeValue(value: TNotifyEvent);
    procedure   SetMetaEditor(value: TcMetaData);

  public
    // public properties
    //
    constructor Create(AOwner: TComponent); override;
    procedure   OnAdd(Sender: TObject);
    procedure   OnDelete(Sender: TObject);
    procedure   SetState(Sender: TObject);

  public
    // public properties
    //
    property    Data: TcData                    read m_objData                  write m_objData;
    property    Editor: TcMetaData              read m_objMetaEditor            write SetMetaEditor;
    property    CheckRequired: boolean          read GetCheckRequired;
    property    Display: String                 read GetDisplay;
    property    OnChangeValue: TNotifyEvent                                     write SetOnChangeValue;
    property    Grid: TGrid                     read FGrid                      write FGrid;
  end;

const
  krsSTATEMENTTYPE: array[TeFormMode] of String =
    (krsMODIFY, krsCREATE);

// TfrmEditor
//   FormCreate
//
procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  m_FormSet := nil;
  m_objMetaEditor := nil;
  m_objData := nil;
  m_eMode := fmEdit;
  m_lstObjects := TList.Create;
end;

// TfrmEditor
//   FormDestroy
//
procedure TfrmEditor.FormDestroy(Sender: TObject);
begin
  m_lstObjects.free;
end;

// TfrmEditor
//   SetFormSet
//
procedure TfrmEditor.SetFormSet(value: TcFormSet);
begin
  m_FormSet := value;
end;

// TfrmEditor
//   FormShow
//
procedure TfrmEditor.FormShow(Sender: TObject);

  function FixCRLF(value: String): String;
  begin
    value := AnsiReplaceText(value, ksCR, #31);
    value := AnsiReplaceText(value, ksCR, #31);
    value := AnsiReplaceText(value, #$D, #31);
    value := AnsiReplaceText(value, #$A, #31);
    value := AnsiReplaceText(value, #31, ksCR);
    result := value;
  end;

var
  i, j, k, L, M, H, W, v: longint;
  c: TGridColumn;
  p, q: TcMetaData;
  s: String;
  lst: TStringList;
  lbl: TLabel;
  edt: TExtendedEdit;
  mmo: TExtendedMemo;
  grd: TExtendedGrid;
  wc: TWinControl;
  cbo: TExtendedCombo;
  cb: TExtendedCheckbox;
const
  kasFORMMODE: array[TeFormMode] of String =
    ('Edit %s', 'Create %s');
  kasFORMMODEEXT: array[TeFormMode] of String =
    ('Edit a %s Object', 'Create a %s Object');
  kiMINWIDTH    = 150;
  kiMINHEIGHT   = 21;
  kiLABELLEFT   = 2;
  kiLABELWIDTH  = 103;
  kiOBJECTLEFT  = 110;
begin
  if (m_FormSet <> nil) and (m_FormSet.MetaData <> nil) and (m_objData <> nil) then
  begin
    m_acolField[FALSE] := clWindow;
    m_acolField[TRUE] := m_FormSet.Preferences.Color[krsPREF_COLOREDITORREQUIREDFIELD];
    m_FormSet.SetHeaderImage(imgHeader, 0);
    //
    // Table Parameters
    L := 12;
    if m_objMetaEditor <> nil then
    begin
      lblHeaderDescription.Caption := m_objMetaEditor.Attribute[krsDESCRIPTION];
      for k := 0 to m_objMetaEditor.count - 1 do
        if (m_objMetaEditor[k] <> nil) and (m_objMetaEditor[k] is TcMetaData) then
        begin
          p := m_objMetaEditor[k] as TcMetaData;
          H := strtointdef(p.Attribute[krsHEIGHT], kiMINHEIGHT);
          W := strtointdef(p.Attribute[krsWIDTH], kiMINHEIGHT);
          //
          // Field
          if (p.eType = enField) and (p.sName <> ksEMPTY) then
          begin
            // Label
            lbl := TLabel.create(pnlPage);
            lbl.Parent := pnlPage;
            lbl.AutoSize := FALSE;
            lbl.Left := kiLABELLEFT;
            lbl.Top := L;
            lbl.Width := kiLABELWIDTH;
            // lbl.height := H;
            lbl.Caption := p.Attribute[krsDISPLAY];
            lbl.Alignment := taRightJustify;
            case StringToGridDatatype(p.Attribute[krsDATATYPE]) of
              //
              // Edit Field (R/W) or Static Text (R/O)
              dtString, dtNumber:
              begin
                if (AnsiCompareText(p.Attribute[krsREADONLY], krsTRUE) <> 0) and
                   (AnsiCompareText(p.Attribute[krsREADONLY], krsSTATEMENTTYPE[m_eMode]) <> 0) then
                begin
                  edt := TExtendedEdit.create(pnlPage);
                  edt.Parent := pnlPage;
                  edt.Left := kiOBJECTLEFT;
                  edt.Top := L - 4;
                  if W > 0 then
                    edt.Width := W
                  else
                    edt.Width := pnlPage.Width - abs(W) - edt.Left;
                  if H > 0 then
                    edt.Height := H
                  else
                    edt.Height := pnlPage.Height - abs(H) - edt.Top;
                  edt.Data := m_objData.Child(p.sName) as TcData;
                  edt.Editor := p;
                  edt.Text := StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY));
                  edt.Color := m_acolField[not edt.CheckRequired];
                  //edt.Anchors := [akTop, akLeft, akRight];
                  if p.HasAttribute[krsMAXLENGTH] then
                    edt.MaxLength := strtointdef(p.Attribute[krsMAXLENGTH], 0);
                  edt.OnEnter := OnCursorLocation;
                  edt.OnExit := OnFieldLostFocus;
                  edt.OnChange := OnFieldLostFocus;
                  edt.OnKeyPress := OnExtendedKeyPress;
                  if AnsiContainsText(p.Attribute[krsMODE], krsUPPERCASE) then
                    edt.CharCase := ecUpperCase;
                  m_lstObjects.Add(edt);
                end
                else
                begin
                  lbl := TLabel.create(pnlPage);
                  lbl.Parent := pnlPage;
                  lbl.Left := kiOBJECTLEFT;
                  lbl.Top := L;
                  lbl.Width := w;
                  lbl.Height := H;
                  lbl.Caption := StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY));
                end;
              end;
              //
              // Syntax-hilited Memo Field (R/W or R/O)
              dtLongString:
              begin
                mmo := TExtendedMemo.create(pnlPage);
                mmo.Parent := pnlPage;
                mmo.Left := kiOBJECTLEFT;
                mmo.Top := L - 4;
                if W > 0 then
                  mmo.Width := W
                else
                  mmo.Width := pnlPage.Width - abs(W) - mmo.Left;
                if H > 0 then
                  mmo.Height := H
                else
                  mmo.Height := pnlPage.Height - abs(H) - mmo.Top;
                if m_FormSet <> nil then
                  FormSet.SetHighlighter(mmo);
                mmo.Data := m_objData.Child(p.sName) as TcData;
                mmo.Editor := p;
                mmo.HideSelection := False;
                mmo.ScrollBars := ssBoth;
                mmo.Anchors := [akTop, akLeft, akRight];
                mmo.WordWrap := FALSE;
                mmo.Text := FixCRLF(StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY)));
                mmo.Color := m_acolField[not mmo.CheckRequired];
                mmo.OnExit := OnFieldLostFocus;
                mmo.OnChange := OnFieldLostFocus;
                mmo.OnKeyPress := OnExtendedKeyPress;
                mmo.ReadOnly := (AnsiCompareText(p.Attribute[krsREADONLY], krsTRUE) = 0) or (AnsiCompareText(p.Attribute[krsREADONLY], krsSTATEMENTTYPE[m_eMode]) = 0);
                mmo.OnChange := OnCursorLocation;
                mmo.OnEnter := OnCursorLocation;
                mmo.OnStatusChange := OnSynStatusChanges;
                if (FormSet.Preferences.StringVal[krsPREF_CONSOLEENABLECURRENTLINEBACKGROUND] <> krsFALSE) then
                  mmo.ActiveLineColor := FormSet.Preferences.Color[krsPREF_CONSOLECURRENTLINEBACKROUND];
                mmo.Gutter.Width := 0;
                m_lstObjects.Add(mmo);
              end;
              //
              // Unsuported, yet
              dtBoolean, dtDictionary:
                ;
            end;
            inc(L, H + 3);
            //
            // Note?
            if p.HasAttribute[krsNOTE] then
            begin
              lbl := TLabel.create(pnlPage);
              lbl.Parent := pnlPage;
              lbl.Left := kiOBJECTLEFT;
              lbl.Top := L - 2;
              lbl.WordWrap := TRUE;
              lbl.AutoSize := TRUE;
              lbl.Width := 350;
              lbl.Caption := p.Attribute[krsNOTE];
              inc(L, lbl.height);
            end;
          end
          //
          // Combo
          else if (p.eType = enComboBox) and (p.sName <> ksEMPTY) then
          begin
            // Label
            lbl := TLabel.create(pnlPage);
            lbl.Parent := pnlPage;
            lbl.AutoSize := FALSE;
            lbl.Left := kiLABELLEFT;
            lbl.Top := L;
            lbl.Width := kiLABELWIDTH;
            // lbl.height := H;
            lbl.Caption := p.Attribute[krsDISPLAY];
            lbl.Alignment := taRightJustify;
            case StringToGridDatatype(p.Attribute[krsDATATYPE]) of
              //
              // Edit Field (R/W) or Static Text (R/O)
              dtString, dtNumber, dtLongString:
              begin
                if (AnsiCompareText(p.Attribute[krsREADONLY], krsTRUE) <> 0) and
                   (AnsiCompareText(p.Attribute[krsREADONLY], krsSTATEMENTTYPE[m_eMode]) <> 0) then
                begin
                  cbo := TExtendedCombo.create(pnlPage);
                  cbo.Style := csDropDownList;
                  cbo.Parent := pnlPage;
                  cbo.Left := kiOBJECTLEFT;
                  cbo.Top := L - 4;
                  if W > 0 then
                    cbo.Width := W
                  else
                    cbo.Width := pnlPage.Width - abs(W) - cbo.Left;
                  if H > 0 then
                    cbo.Height := H
                  else
                    cbo.Height := pnlPage.Height - abs(H) - cbo.Top;
                  cbo.Data := m_objData.Child(p.sName) as TcData;
                  cbo.Editor := p;
                  cbo.Items.BeginUpdate;
                  cbo.Items.Clear;;
                  for v := 0 to p.count - 1 do
                    if (p[v] <> nil) and (p[v].eType = enValue) then
                      cbo.Items.Add(p[v].sName);
                  cbo.Items.EndUpdate;
                  cbo.ItemIndex := cbo.Items.IndexOf(StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY)));
                  cbo.Color := m_acolField[not cbo.CheckRequired];
                  //cbo.Anchors := [akTop, akLeft, akRight];
                  if p.HasAttribute[krsMAXLENGTH] then
                    cbo.MaxLength := strtointdef(p.Attribute[krsMAXLENGTH], 0);
                  cbo.OnEnter := OnCursorLocation;
                  cbo.OnExit := OnFieldLostFocus;
                  cbo.OnChange := OnFieldLostFocus;
                  cbo.OnKeyPress := OnExtendedKeyPress;
                  if AnsiContainsText(p.Attribute[krsMODE], krsUPPERCASE) then
                    cbo.CharCase := ecUpperCase;
                  m_lstObjects.Add(cbo);
                end
                else
                begin
                  lbl := TLabel.create(pnlPage);
                  lbl.Parent := pnlPage;
                  lbl.Left := kiOBJECTLEFT;
                  lbl.Top := L;
                  lbl.Width := w;
                  lbl.Height := H;
                  lbl.Caption := StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY));
                end;
              end;
            end;
            inc(L, H + 3);
            //
            // Note?
            if p.HasAttribute[krsNOTE] then
            begin
              lbl := TLabel.create(pnlPage);
              lbl.Parent := pnlPage;
              lbl.Left := kiOBJECTLEFT;
              lbl.Top := L - 2;
              lbl.WordWrap := TRUE;
              lbl.AutoSize := TRUE;
              lbl.Width := 350;
              lbl.Caption := p.Attribute[krsNOTE];
              inc(L, lbl.height);
            end;
          end
          //
          // Checkbox
          else if (p.eType = enCheckbox) and (p.sName <> ksEMPTY) then
          begin
            // Label
            lbl := TLabel.create(pnlPage);
            lbl.Parent := pnlPage;
            lbl.AutoSize := FALSE;
            lbl.Left := kiLABELLEFT;
            lbl.Top := L;
            lbl.Width := kiLABELWIDTH;
            // lbl.height := H;
            lbl.Caption := p.Attribute[krsDISPLAY];
            lbl.Alignment := taRightJustify;
            case StringToGridDatatype(p.Attribute[krsDATATYPE]) of
              //
              // Edit Field (R/W) or Static Text (R/O)
              dtString, dtNumber, dtLongString:
              begin
                if (AnsiCompareText(p.Attribute[krsREADONLY], krsTRUE) <> 0) and
                   (AnsiCompareText(p.Attribute[krsREADONLY], krsSTATEMENTTYPE[m_eMode]) <> 0) then
                begin
                  cb := TExtendedCheckbox.create(pnlPage);
                  cb.Parent := pnlPage;
                  cb.Left := kiOBJECTLEFT;
                  cb.Top := L - 4;
                  if W > 0 then
                    cb.Width := W
                  else
                    cb.Width := pnlPage.Width - abs(W) - cb.Left;
                  if H > 0 then
                    cb.Height := H
                  else
                    cb.Height := pnlPage.Height - abs(H) - cb.Top;
                  cb.Caption := p.Attribute[krsDETAILS];
                  cb.Data := m_objData.Child(p.sName) as TcData;
                  cb.Editor := p;
                  cb.BooleanValues := p.Attribute[krsBOOLEANVALUES];
                  cb.BooleanState := StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY));
                  cb.OnEnter := OnCursorLocation;
                  cb.OnExit := OnFieldLostFocus;
                  cb.OnClick := OnFieldLostFocus;
                  cb.OnKeyPress := OnExtendedKeyPress;
                  m_lstObjects.Add(cb);
                end
                else
                begin
                  lbl := TLabel.create(pnlPage);
                  lbl.Parent := pnlPage;
                  lbl.Left := kiOBJECTLEFT;
                  lbl.Top := L;
                  lbl.Width := w;
                  lbl.Height := H;
                  lbl.Caption := StripHTMLTags(m_objData.IdentifierValue(p.sName, ksEMPTY));
                end;
              end;
            end;
            inc(L, H + 3);
            //
            // Note?
            if p.HasAttribute[krsNOTE] then
            begin
              lbl := TLabel.create(pnlPage);
              lbl.Parent := pnlPage;
              lbl.Left := kiOBJECTLEFT;
              lbl.Top := L - 2;
              lbl.WordWrap := TRUE;
              lbl.AutoSize := TRUE;
              lbl.Width := 350;
              lbl.Caption := p.Attribute[krsNOTE];
              inc(L, lbl.height);
            end;
          end
          //
          // Grid
          else if (p.eType = enGrid) and (p.sName <> ksEMPTY) then
          begin
            grd := TExtendedGrid.create(pnlPage);
            grd.Parent := pnlPage;
            grd.Left := 8;
            grd.Top := L;
            grd.OnChangeValue := SetState;
            grd.Grid.Images := ImageList;
            grd.Editor := p;
            if W > 0 then
              grd.Width := W
            else
              grd.Width := pnlPage.Width - abs(W) - grd.Left;
            if H > 0 then
              grd.Height := H
            else
              grd.Height := pnlPage.Height - abs(H) - grd.Top;
            grd.Anchors := [akTop, akLeft, akRight];
            grd.Data := m_objData;
            if p.HasAttribute[krsREADONLY] then
              grd.Grid.GridState := StringToGridState(p.Attribute[krsREADONLY]);
            m_lstObjects.Add(grd);
            //
            // Column definition
            M := 0;
            for i := 0 to p.Count - 1 do
              if (p[i] <> nil) and (p[i] is TcMetaData) and (p[i].eType = enField) then
              begin
                q := p[i] as TcMetaData;
                grd.Grid.Column[M].Width := strtointdef(q.Attribute[krsWIDTH], 100);
                s := q.sName;
                if q.HasAttribute[krsDISPLAY] and (q.Attribute[krsDISPLAY] <> ksEMPTY) then
                  s := q.Attribute[krsDISPLAY];
                grd.Grid.Column[M].Caption := s;
                grd.Grid.Column[M].DataType := StringToGridDatatype(q.Attribute[krsDATATYPE]);
                grd.Grid.Column[M].StateImage[TRUE] := 1;
                grd.Grid.Column[M].StateImage[FALSE] := 0;
                grd.Grid.Column[M].MaxLength := strtointdef(q.Attribute[krsMAXLENGTH], 0);
                grd.Grid.Column[M].MetaData := q;
                grd.Grid.Column[M].IsReadOnly := (AnsiCompareText(q.Attribute[krsREADONLY], krsTRUE) = 0) or (AnsiCompareText(q.Attribute[krsREADONLY], krsSTATEMENTTYPE[m_eMode]) = 0);
                grd.Grid.Column[M].Required := q.HasAttribute[krsMODE] and AnsiContainsText(q.Attribute[krsMODE], krsREQUIRED);
                grd.Grid.Column[M].Filter := q.sFilter;
                if AnsiContainsText(q.Attribute[krsMODE], krsUPPERCASE) then
                  grd.Grid.Column[M].CharCase := ecUpperCase;
                if q.HasAttribute[krsBOOLEANVALUES] then
                  grd.Grid.Column[M].BooleanValues := q.Attribute[krsBOOLEANVALUES];
                // Drop down value
                lst := nil;
                try
                  lst := TStringList.Create;
                  for j := 0 to q.count - 1 do
                    if (q[j] <> nil) and (q[j].eType = enValue) then
                      lst.Add(q[j].sName);
                  grd.Grid.ColumnValues[M] := lst;
                finally
                  lst.free;
                end;
                inc(M);
              end;
            //
            // Detail Values
            M := 0;
            s := p.sName;
            for i := 0 to m_objData.Count - 1 do
              if (m_objData[i] <> nil) and (m_objData[i] is TcData) and (AnsiCompareText(m_objData[i].sName, s) = 0) then
              begin
                for j := 0 to grd.Grid.ColumnCount - 1 do
                begin
                  c := grd.Grid.Column[j];
                  if c.MetaData <> nil then
                    grd.Grid.AsString[M, j] := (m_objData[i] as TcData).IdentifierValue(c.MetaData.sName, ksEMPTY);
                end;
                grd.Grid.Data[M] := m_objData[i];
                inc(M);
              end;
            if m_eMode = fmNew then
              grd.OnAdd(Sender);
            with grd.Grid do
              Width := width + 1;
            with grd.Grid do
              Width := width - 1;
            //
            inc(L, 100);
          end;
        end;
    end;
    pnlPage.Height := L;
  end;
  // Form caption
  if (m_objData <> nil) and (m_objData.MetaData <> nil) then
  begin
    Caption := Format(kasFORMMODE[m_eMode], [InitCap(m_objData.MetaData.sName)]);
    lblHeaderTitle.Caption := Format(kasFORMMODEEXT[m_eMode], [InitCap(m_objData.MetaData.sName)]);
  end;
  // focus, anchoring
  if (m_lstObjects.Count > 0) and (TObject(m_lstObjects[0]) is TWinControl) then
  begin
    // 1. Set last one anchoring
    wc := TWinControl(m_lstObjects[m_lstObjects.count - 1]);
    if (wc is TExtendedMemo) or (wc is TExtendedGrid) then
      wc.Anchors := wc.Anchors + [akBottom];
    // 2. Set focus on first one.
    TWinControl(m_lstObjects[0]).SetFocus;
  end;
  // Form size
  if (m_FormSet <> nil) and (m_FormSet.Preferences <> nil) then
  begin
    width := strtointdef(m_FormSet.Preferences.StringVal[krsEDITOR_WIDTH], kiEDITOR_WIDTH);
    height := strtointdef(m_FormSet.Preferences.StringVal[krsEDITOR_HEIGHT], kiEDITOR_HEIGHT);
  end;
  //
  SetState(Sender);
end;

// TfrmEditor
//   FormClose
//
procedure TfrmEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_FormSet.Preferences.StringVal[krsEDITOR_WIDTH] := inttostr(width);
  m_FormSet.Preferences.StringVal[krsEDITOR_HEIGHT] := inttostr(height);
  action := caHide;
end;

// TfrmEditor
//   OnFieldLostFocus
//
procedure TfrmEditor.OnFieldLostFocus(Sender: TObject);
var
  p: TcObject;
begin
  //
  // TExtendedEdit
  if (Sender <> nil) and (Sender is TExtendedEdit) then
  begin
    with Sender as TExtendedEdit do
      Color := m_acolField[not CheckRequired];
    if (Sender as TExtendedEdit).CheckRequired then
    begin
      p := (Sender as TExtendedEdit).Data;
      p.sValue := (Sender as TEdit).Text;
      m_objData.CascadeValue(p.sName, p.sValue);
    end;
  end
  //
  // TExtendedCombo
  else if (Sender <> nil) and (Sender is TExtendedCombo) then
  begin
    with Sender as TExtendedCombo do
      Color := m_acolField[not CheckRequired];
    if (Sender as TExtendedCombo).CheckRequired then
    begin
      p := (Sender as TExtendedCombo).Data;
      p.sValue := (Sender as TComboBox).Text;
      m_objData.CascadeValue(p.sName, p.sValue);
    end;
  end
  //
  // TExtendedCheckBox
  else if (Sender <> nil) and (Sender is TExtendedCheckBox) then
  begin
    with Sender as TExtendedCheckBox do
      Color := m_acolField[not CheckRequired];
    if (Sender as TExtendedCheckBox).CheckRequired then
    begin
      p := (Sender as TExtendedCheckBox).Data;
      p.sValue := (Sender as TExtendedCheckBox).BooleanState;
      m_objData.CascadeValue(p.sName, p.sValue);
    end;
  end
  //
  // TExtendedMemo
  else if (Sender <> nil) and (Sender is TExtendedMemo) then
  begin
    with Sender as TExtendedMemo do
      Color := m_acolField[not CheckRequired];
    if (Sender as TExtendedMemo).CheckRequired then
    begin
      p := (Sender as TExtendedMemo).Data;
      p.sValue := (Sender as TExtendedMemo).Text;
      m_objData.CascadeValue(p.sName, p.sValue);
    end;
  end;
  //
  SetState(Sender);
end;

// TfrmEditor
//   btnOKClick
//
procedure TfrmEditor.btnOKClick(Sender: TObject);
var
  sSQL, sError: String;
  m_objExecute: TcExecute;
  stmt: TcStatementList;
  i: longint;
  p: TWinControl;
  b: boolean;
const
  krsFAILEDSTATEMENTS = 'One or more statements failed. Please check the table and possibly retry modifications.' + ksCR +
                        'The current schema representation does no longer accuratly represent the database schema state. It is advised to refresh the schema representation.';
begin
  btnOK.SetFocus;
  try
    screen.cursor := crHourGlass;
    // Check required fields
    p := CheckAllRequired;
    if p <> nil then
    begin
      Application.MessageBox('Not all required fields have values. Please check required fields.', krsERROR, MB_OK + MB_ICONEXCLAMATION);
      p.SetFocus;
      exit;
    end;
    // Generate Modify Script
    sSQL := StripHTMLTags(TextToText(m_objData.Statement([enScript], krsSTATEMENTTYPE[m_eMode])));
    // Parse the text into several statements
    stmt := nil;
    b := TRUE;
    try
      stmt := TcStatementList.Create(nil);
      if stmt.Parse(estParagraph, sSQL) then
      begin
        m_objExecute := nil;
        try
          m_objExecute := TcExecute.Create(nil);
          m_objExecute.Connection := m_Formset.Connection;
          m_objExecute.IsReadOnly := TRUE;
          m_objExecute.IsAsynchronous := FALSE;
          sError := m_objExecute.Error;
          for i := 0 to stmt.count - 1 do
            if (stmt[i] <> nil) and (stmt[i] is TcStatement) and ((stmt[i] as TcStatement).SQL <> ksEMPTY) then
            try
              sSQL := (stmt[i] as TcStatement).SQL;
              m_objExecute.Execute(sSQL);
              sError := m_objExecute.Error;
              if sError <> ksEMPTY then
              begin
                Application.MessageBox(PChar(trim(sSQL) + ksCR + ksCR + sError), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
                b := FALSE;
              end;
            except
              on E: Exception do
              begin
                Application.MessageBox(PChar(trim(sSQL) + ksCR + ksCR + E.Message), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
                b := FALSE;
              end;
            end;
        finally
          m_objExecute.Free;
        end;
      end;
      if not b then
        Application.MessageBox(krsFAILEDSTATEMENTS, krsINFORMATION, MB_OK + MB_ICONINFORMATION);
    finally
      stmt.free;
    end;
  finally
    screen.Cursor := crDefault;
  end;
  // We are done.
  if b then
    ModalResult := mrOK;
end;

// TfrmEditor
//   OnExtendedKeyPress
//
procedure TfrmEditor.OnExtendedKeyPress(Sender: TObject; var Key: Char);
var
  p: TcObject;
begin
  if (Key > #31) and (Sender <> nil) then
  begin
    p := nil;
    if (Sender is TExtendedEdit) then
      p := (Sender as TExtendedEdit).Editor
    else if (Sender is TExtendedCombo) then
      p := (Sender as TExtendedCombo).Editor
    else if (Sender is TExtendedCheckbox) then
      p := (Sender as TExtendedCheckbox).Editor
    else if (Sender is TExtendedMemo) then
      p := (Sender as TExtendedMemo).Editor;
    if (p <> nil) and (p is TcMetaData) and ((p as TcMetaData).sFilter <> ksEMPTY) and not AnsiContainsText((p as TcMetaData).sFilter, key) then
    begin
      Key := #0;
      MessageBeep(MB_ICONASTERISK);
    end;
  end;
  OnFieldLostFocus(Sender);
end;

// TfrmEditor
//   SetState
//
procedure TfrmEditor.SetState(Sender: TObject);
var
  i: longint;
begin
  for i := 0 to m_lstObjects.count - 1 do
    if (m_lstObjects[i] <> nil) and (TObject(m_lstObjects[i]) is TExtendedGrid) then
      TExtendedGrid(m_lstObjects[i]).SetState(Sender);
  btnOK.Enabled := CheckAllRequired = nil;
end;

// TfrmEditor
//   CheckAllRequired
//
function TfrmEditor.CheckAllRequired: TWinControl;
var
  p: TObject;
  i: longint;
begin
  result := nil;
  for i := 0 to m_lstObjects.Count - 1 do
  begin
    p := m_lstObjects[i];
    if ((p <> nil) and (p is TExtendedEdit) and not (p as TExtendedEdit).CheckRequired) or
       ((p <> nil) and (p is TExtendedCombo) and not (p as TExtendedCombo).CheckRequired) or
       ((p <> nil) and (p is TExtendedCheckbox) and not (p as TExtendedCheckbox).CheckRequired) or
       ((p <> nil) and (p is TExtendedMemo) and not (p as TExtendedMemo).CheckRequired) or
       ((p <> nil) and (p is TGrid) and not (p as TGrid).CheckAllRequired) then
      result := p as TWinControl;
    if result <> nil then
      break;
  end;
end;

// TfrmEditor
//   OnCursorLocation
//
procedure TfrmEditor.OnCursorLocation(Sender: TObject);  
begin
  if (Sender <> nil) and (Sender is TSynEdit) then
    with (Sender as TSynEdit) do
      lblStatus.Caption := Format('%d/%d', [CaretY + 1, CaretX + 1])
  else
    lblStatus.Caption := ksEMPTY;
end;

// TfrmEditor
//   OnSynStatusChanges
//
procedure TfrmEditor.OnSynStatusChanges(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scCaretX, scCaretY] * Changes <> []) then
    OnCursorLocation(Sender);
end;

// TfrmEditor
//   actEditCutExecute
//
procedure TfrmEditor.actEditCutExecute(Sender: TObject);
begin
  if (activeControl <> nil) and (activeControl is TWinControl) then
    SendMessage(activeControl.Handle, WM_CUT, 0, 0);
  OnFieldLostFocus(activeControl);
end;

// TfrmEditor
//   actEditCopyExecute
//
procedure TfrmEditor.actEditCopyExecute(Sender: TObject);
begin
  if (activeControl <> nil) and (activeControl is TWinControl) then
    SendMessage(activeControl.Handle, WM_COPY, 0, 0);
  OnFieldLostFocus(activeControl);
end;

// TfrmEditor
//   actEditPasteExecute
//
procedure TfrmEditor.actEditPasteExecute(Sender: TObject);
begin
  if (activeControl <> nil) and (activeControl is TWinControl) then
    SendMessage((activeControl as TWinControl).Handle, WM_PASTE, 0, 0);
  OnFieldLostFocus(activeControl);
end;

// TfrmEditor
//   actEditUndoExecute
//
procedure TfrmEditor.actEditUndoExecute(Sender: TObject);
begin
  if (activeControl <> nil) and (activeControl is TWinControl) then
    SendMessage((activeControl as TWinControl).Handle, WM_UNDO, 0, 0);
  OnFieldLostFocus(activeControl);
end;

//
// TExtendedEdit
//

// TExtendedEdit
//   GetCheckRequired
//
function TExtendedEdit.GetCheckRequired: boolean;
begin
  result := TRUE;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) and (m_objMetaEditor as TcMetaData).HasAttribute[krsMODE] and AnsiContainsText((m_objMetaEditor as TcMetaData).Attribute[krsMODE], krsREQUIRED) then
    result := Text <> ksEMPTY;
end;

// TExtendedEdit
//   GetDisplay
//
function TExtendedEdit.GetDisplay: String;
begin
  result := ksEMPTY;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) then
    result := m_objMetaEditor.Attribute[krsDISPLAY];
end;

//
// TExtendedCombo
//

// TExtendedCombo
//   GetCheckRequired
//
function TExtendedCombo.GetCheckRequired: boolean;
begin
  result := TRUE;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) and (m_objMetaEditor as TcMetaData).HasAttribute[krsMODE] and AnsiContainsText((m_objMetaEditor as TcMetaData).Attribute[krsMODE], krsREQUIRED) then
    result := Text <> ksEMPTY;
end;

// TExtendedCombo
//   GetDisplay
//
function TExtendedCombo.GetDisplay: String;
begin
  result := ksEMPTY;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) then
    result := m_objMetaEditor.Attribute[krsDISPLAY];
end;

//
// TExtendedCheckbox
//

// TExtendedCheckbox
//   GetCheckRequired
//
function TExtendedCheckbox.GetCheckRequired: boolean;
begin
  result := TRUE;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) and (m_objMetaEditor as TcMetaData).HasAttribute[krsMODE] and AnsiContainsText((m_objMetaEditor as TcMetaData).Attribute[krsMODE], krsREQUIRED) then
    result := Text <> ksEMPTY;
end;

// TExtendedCheckbox
//   GetDisplay
//
function TExtendedCheckbox.GetDisplay: String;
begin
  result := ksEMPTY;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) then
    result := m_objMetaEditor.Attribute[krsDISPLAY];
end;

// TExtendedCheckbox
//   GetBooleanValues
//
function TExtendedCheckbox.GetBooleanValues: String;
begin
  result := Format('%s,%s', [FTRUE, FFALSE]);
end;

// TExtendedCheckbox
//   GetBooleanState
//
procedure TExtendedCheckbox.SetBooleanValues(value: String);
begin
  if value <> ksEMPTY then
  begin
    FTRUE := Item(value, ',', 0);
    FFALSE := Item(value, ',', 1);
    FabBOOLEAN[TRUE] := FTRUE;
    FabBOOLEAN[FALSE] := FFALSE;
  end;
end;

// TExtendedCheckbox
//   GetBooleanState
//
function TExtendedCheckbox.GetBooleanState: String;
begin
  result := FabBOOLEAN[checked];
end;

// TExtendedCheckbox
//   SetBooleanState
//
procedure TExtendedCheckbox.SetBooleanState(value: String);
begin
  checked := AnsiCompareText(FTRUE, value) = 0;
end;

//
// TExtendedMemo
//

// TExtendedMemo
//   GetCheckRequired
//
function TExtendedMemo.GetCheckRequired: boolean;
begin
  result := TRUE;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) and (m_objMetaEditor as TcMetaData).HasAttribute[krsMODE] and AnsiContainsText((m_objMetaEditor as TcMetaData).Attribute[krsMODE], krsREQUIRED) then
    result := Text <> ksEMPTY;
end;

// TExtendedMemo
//   GetDisplay
//
function TExtendedMemo.GetDisplay: String;
begin
  result := ksEMPTY;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) then
    result := m_objMetaEditor.Attribute[krsDISPLAY];
end;

//
// TExtendedGrid
//

// TExtendedGrid
//   Create
//
constructor TExtendedGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  Width := 484;
  Height := 247;
  BevelOuter := bvNone;
  // Bevel
  FBevel := TBevel.Create(self);
  FBevel.Parent := self;
  FBevel.Left := 2;
  FBevel.Top := 4;
  FBevel.Width := 480;
  FBevel.Height := 2;
  FBevel.Anchors := [akLeft, akTop, akRight];
  // Label
  FLabel := TStaticText.Create(self);
  FLabel.Parent := self;
  FLabel.Left := 2;
  FLabel.Top := 11;
  FLabel.Width := 47;
  FLabel.Height := 17;
  FLabel.Caption := 'Columns:';
  // Grid
  FGrid := TGrid.Create(self);
  FGrid.Parent := self;
  FGrid.Left := 2;
  FGrid.Top := 28;
  FGrid.Width := 480;
  FGrid.Height := 180;
  FGrid.BevelInner := bvNone;
  FGrid.BevelOuter := bvLowered;
  //FGrid.BackgroundColor := clWindow;
  //FGrid.SelectedColumnColor := clCaptionText;
  //FGrid.SelectionColor := clMenuBar;
  //FGrid.CurrentLineColor := clMenuHighlight;
  FGrid.ColumnSort := TRUE;
  FGrid.AllocBy := 20;
  FGrid.Anchors := [akLeft, akTop, akRight, akBottom];
  FGrid.HideSelection := False;
  FGrid.OwnerDraw := True;
  FGrid.RowSelect := True;
  FGrid.ViewStyle := vsReport;
  FGrid.OnChange := gridColumnsChange;
  FGrid.ColumnSort := FALSE;
  // Add Button
  FAdd := TButton.Create(self);
  FAdd.Parent := self;
  FAdd.Left := 330;
  FAdd.Top := 216;
  FAdd.Width := 75;
  FAdd.Height := 25;
  FAdd.Anchors := [akRight, akBottom];
  FAdd.Caption := '&Add';
  FAdd.Enabled := False;
  FAdd.OnClick := OnAdd;
  // Add Button
  FDelete := TButton.Create(self);
  FDelete.Parent := self;
  FDelete.Left := 407;
  FDelete.Top := 216;
  FDelete.Width := 75;
  FDelete.Height := 25;
  FDelete.Anchors := [akRight, akBottom];
  FDelete.Caption := '&Delete';
  FDelete.Enabled := False;
  FDelete.OnClick := OnDelete;
end;

// TExtendedGrid
//   gridColumnsChange
//
procedure TExtendedGrid.gridColumnsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  SetState(Sender);
end;

// TExtendedGrid
//   OnAdd
//
procedure TExtendedGrid.OnAdd(Sender: TObject);
var
  p: TcData;
  q: TcMetaData;
begin
  q := nil;
  if (m_objData <> nil) and (m_objData.MetaData <> nil) and (m_objMetaEditor <> nil) then
    q := m_objData.MetaData.Find([enObject, enItem], m_objMetaEditor.sName) as TcMetaData;
  if q <> nil then
    with FGrid do
      if (Items.Count = 0) or ((Items.Count > 0) and CheckRequiredFields(Items.Count - 1, FALSE)) and (q <> nil) then
      begin
        EditState := FALSE;
        p := TcData.Create(m_objData);
        m_objData.Add(p);
        p.MetaData := q;
        p.State := edsAdded;
        p.DescendAncestorValues;
        ItemIndex := Items.Add.Index;
        FGrid.Data[ItemIndex] := p;
        Selected.MakeVisible(FALSE);
        FGrid.Width := FGrid.width + 1;
        FGrid.Width := FGrid.width - 1;
      end;
  FGrid.OnChangeValue(Sender);
  SetState(Sender);
end;

// TExtendedGrid
//   OnDelete
//
procedure TExtendedGrid.OnDelete(Sender: TObject);
var
  p: TcData;
  L: longint;
begin
  if (FGrid.ItemIndex <> kiUNDEFINED) and (Application.MessageBox('Delete this Column?', krsINFORMATION, MB_YESNO + MB_ICONINFORMATION) = idYES) then
  begin
    with FGrid do
      p := Data[ItemIndex] as TcData;
    if (p <> nil) and (p is TcData) then
    begin
      L := FGrid.ItemIndex;
      p.State := edsdeleted;
      FGrid.DeleteSelected;
      FGrid.Width := FGrid.width + 1;
      FGrid.Width := FGrid.width - 1;
      if L > FGrid.Items.Count - 1 then
        L := FGrid.Items.Count - 1;
      with FGrid do
        if (L > kiUNDEFINED) and (L < Items.Count) then
          Selected := Items[L];
    end;
  end;
  FGrid.OnChangeValue(Sender);
  SetState(Sender);
  FGrid.SetFocus;
end;

// TExtendedGrid
//   SetState
//
procedure TExtendedGrid.SetState(Sender: TObject);
begin
  FAdd.Enabled := (m_objMetaEditor <> nil) and AnsiContainsText(m_objMetaEditor.Attribute[krsMODE], krsADD);
  FDelete.Enabled := (FGrid.Selected <> nil) and (m_objMetaEditor <> nil) and AnsiContainsText(m_objMetaEditor.Attribute[krsMODE], krsDELETE);
end;

// TExtendedGrid
//   SetOnChangeValue
//
procedure TExtendedGrid.SetOnChangeValue(value: TNotifyEvent);
begin
  FGrid.OnChangeValue := value;
end;

// TExtendedGrid
//   GetCheckRequired
//
function TExtendedGrid.GetCheckRequired: boolean;
begin
  result := FGrid.CheckAllRequired;
end;

// TExtendedGrid
//   GetDisplay
//
function TExtendedGrid.GetDisplay: String;
begin
  result := ksEMPTY;
  if (m_objMetaEditor <> nil) and (m_objMetaEditor is TcMetaData) then
    result := m_objMetaEditor.Attribute[krsDISPLAY];
end;

// TExtendedGrid
//   SetOnChangeValue
//
procedure TExtendedGrid.SetMetaEditor(value: TcMetaData);
begin
  m_objMetaEditor := value;
  FLabel.Caption := GetDisplay;
end;

end.


