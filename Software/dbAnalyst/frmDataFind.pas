unit frmDataFind;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, daObjectLib, PreferenceLib;

type
  TcDataFind = class;

  TfrmDataFind = class(TForm)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/15/05 Regis Created
  *
  ******************************************************************************}
    pnlClient: TPanel;
    pnlTop: TPanel;
    lstResult: TListView;
    btnFind: TSpeedButton;
    cboSearch: TComboBox;
    procedure   FormClose(Sender: TObject; var Action: TCloseAction);
    procedure   FormActivate(Sender: TObject);
    procedure   FormCreate(Sender: TObject);
    procedure   btnFindClick(Sender: TObject);
    procedure   edtFindKeyPress(Sender: TObject; var Key: Char);
    procedure   lstResultChange(Sender: TObject; Item: TListItem; Change: TItemChange);

  private
    // private methods
    //
    m_lstPreferences: TcPreferenceList;
    m_objDataFind: TcDataFind;

  private
    // private declarations
    //
    procedure   SetDataFind(value: TcDataFind);
    procedure   SetPreferences(value: TcPreferenceList);

  public
    // public methods
    //
    procedure   OnClearResult;
    procedure   SetState(value: boolean);

  public
    // public properties
    //
    property    Preferences: TcPreferenceList           read m_lstPreferences   write SetPreferences;
    property    objDataFind: TcDataFind                 read m_objDataFind      write SetDataFind;
  end;

  TcDataFind = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/19/05 Regis Created
  *
  ******************************************************************************}
  private
    // Private Members
    //
    m_lstSearch: TStringList;
    m_lstResult: TStringList;
    m_frmFind: TfrmDataFind;
    m_objFormSet: TcObject;

  private
    // Private declarations
    //
    procedure   SetParentForm(value: TfrmDataFind);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Search(value: String): longint;
    function    SelectObject(value: TcObject): boolean;
    procedure   DisplayResult;
    procedure   Disconnect;

  public
    // Public Properties
    //
    property    FormSet: TcObject                       read m_objFormSet       write m_objFormSet;
    property    lstSearch: TStringList                  read m_lstSearch        write m_lstSearch;
    property    lstResult: TStringList                  read m_lstResult        write m_lstResult;
    property    ParentForm: TfrmDataFind                read m_frmFind          write SetParentForm;
  end;

implementation

uses
  daGlobals,
  daResourceStrings,
  FormLib,
  frmData2;  // TfrmData2

{$R *.dfm}

//
// TfrmDataFind
//

// TfrmDataFind
//   FormCreate
//
procedure TfrmDataFind.FormCreate(Sender: TObject);
begin
  m_lstPreferences := nil;
  m_objDataFind := nil;
  SetState(FALSE);
end;

// TfrmDataFind
//   SetPreferences
//
procedure TfrmDataFind.SetPreferences(value: TcPreferenceList);
begin
  m_lstPreferences := value;
  if m_lstPreferences <> nil then
  begin
    Height := strtointdef(m_lstPreferences.StringVal[krsPREF_DATAFIND_HEIGHT], Height);
    Width := strtointdef(m_lstPreferences.StringVal[krsPREF_DATAFIND_WIDTH], Width);
    Left := strtointdef(m_lstPreferences.StringVal[krsPREF_DATAFIND_X], kiUNDEFINED);
    if Left < 0 then
      Left := 0;
    if Left > Screen.Width - Width then
      Left := Screen.Width - Width;
    Top := strtointdef(m_lstPreferences.StringVal[krsPREF_DATAFIND_Y], 30);
    if Top < 0 then
      Top  := 0;
    if Top  > Screen.Height - Height then
      Top := Screen.Height - Height;
  end;
end;

// TfrmDataFind
//   FormClose
//
procedure TfrmDataFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_lstPreferences <> nil then
  begin
    m_lstPreferences.StringVal[krsPREF_DATAFIND_X] := inttostr(Left);
    m_lstPreferences.StringVal[krsPREF_DATAFIND_Y] := inttostr(Top);
    m_lstPreferences.StringVal[krsPREF_DATAFIND_HEIGHT] := inttostr(Height);
    m_lstPreferences.StringVal[krsPREF_DATAFIND_WIDTH] := inttostr(Width);
    m_lstPreferences.StringVal[krsPREF_DATAFIND_VISIBLE] := kasBOOL[Visible];
    m_lstPreferences.WriteToRegistry;
  end;
  Action := caHide;
end;

// TfrmDataFind
//   FormActivate
//
procedure TfrmDataFind.FormActivate(Sender: TObject);
begin
  if visible and cboSearch.Enabled then
    cboSearch.SetFocus;
end;

// TfrmDataFind
//   OnClearResult
//
procedure TfrmDataFind.OnClearResult;
begin
  if m_objDataFind <> nil then
    m_objDataFind.Clear;
end;

// TfrmDataFind
//   SetFormFocus
//
procedure TfrmDataFind.SetDataFind(value: TcDataFind);
begin
  m_objDataFind := value;
  if m_objDataFind <> nil then
    m_objDataFind.DisplayResult
  else
  begin
    cboSearch.Clear;
    lstResult.Clear;
  end;
  SetState(value <> nil);
end;

// TfrmDataFind
//   btnFindClick
//
procedure TfrmDataFind.btnFindClick(Sender: TObject);
begin
  if (m_objDataFind <> nil) then
  begin
    m_objDataFind.Search(cboSearch.Text);
    m_objDataFind.DisplayResult;
  end;
end;

// TfrmDataFind
//   edtFindKeyPress
//
procedure TfrmDataFind.edtFindKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    btnFindClick(Sender);
    key := #0;
  end;
end;

// TfrmDataFind
//   SetState
//
procedure TfrmDataFind.SetState(value: boolean);
begin
  cboSearch.Enabled := value;
  lstResult.Enabled := value;
  btnFind.Enabled := value;
end;

// TfrmDataFind
//   lstResultChange
//
procedure TfrmDataFind.lstResultChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (m_objDataFind <> nil) and (lstResult.Selected <> nil) then
    m_objDataFind.SelectObject(lstResult.Selected.Data);
end;

//
// TcDataFind
//

// TcDataFind
//   SetParentForm
//
constructor TcDataFind.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_lstSearch := TStringList.Create;
  m_lstResult := TStringList.Create;
  m_lstResult.Sorted := TRUE;
  m_frmFind := nil;
  m_objFormSet := nil;
  Clear;
end;

// TcDataFind
//   Destroy
//
destructor TcDataFind.Destroy;
begin
  m_lstSearch.free;
  m_lstResult.free;
  inherited Destroy;
end;

// TcDataFind
//   Clear
//
procedure TcDataFind.Clear;
begin
  m_lstSearch.Clear;
  m_lstResult.Clear;
  if m_frmFind <> nil then
    m_frmFind.SetDataFind(self);
end;

// TcDataFind
//   Search
//
function TcDataFind.Search(value: String): longint;
begin
  result := 0;
  if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) and ((m_objFormSet as TcFormSet).MetaData <> nil) then
  begin
  (*
    result := (m_objFormSet as TcFormSet).MetaData.FindData(value, m_lstResult);
    m_lstSearch.Insert(0, value);
    DisplayResult;
  *)
  end;
end;

// TcDataFind
//   SetParentForm
//
procedure TcDataFind.SetParentForm(value: TfrmDataFind);
begin
  m_frmFind := value;
  if m_frmFind <> nil then
    m_frmFind.SetState(m_frmFind <> nil);
end;

// TcDataFind
//   SelectObject
//
function TcDataFind.SelectObject(value: TcObject): boolean;
var
  lst: TcFormList;
begin
  result := FALSE;
  if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) then
  begin
    lst := (m_objFormSet as TcFormSet).FormList[efsObjects];
    if (lst <> nil) and (lst.count > 0) and (lst.Forms[0] <> nil) and (lst.Forms[0] is TfrmData2) then
      (lst.Forms[0] as TfrmData2).GetObjectItem(value);
  end;
end;

// TcDataFind
//   DisplayResult
//
procedure TcDataFind.DisplayResult;
var
  i: longint;
  p: TListItem;
  q: TcObject;
begin
  if m_frmFind <> nil then
  begin
    // List View Object
    with m_frmFind.lstResult.Items do
    begin
      BeginUpdate;
      Clear;
      for i := 0 to m_lstResult.Count - 1 do
      begin
        q := m_lstResult.objects[i] as TcObject;
        p := Add;
        p.Caption := m_lstResult[i];
        p.SubItems.Add(Initcap(q.sName));
        p.Data := q;
      end;
      Application.Hint := Format('%d Result%s', [m_lstResult.Count, HasS(m_lstResult.Count)]);
      EndUpdate;
    end;
    // Combo box Serch field
    m_frmFind.cboSearch.Text := ksEMPTY;
    m_frmFind.cboSearch.Items.Assign(m_lstSearch);
    if m_lstSearch.Count > 0 then
      m_frmFind.cboSearch.ItemIndex := 0;
  end;
end;

// TcDataFind
//   Disconnect
//
procedure TcDataFind.Disconnect;
begin
  if m_frmFind <> nil then
    m_frmFind.objDataFind := nil;
end;

end.




