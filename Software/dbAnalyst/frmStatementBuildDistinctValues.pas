unit frmStatementBuildDistinctValues;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StatementLib, daObjectLib, FormLib;

type
  TfrmStatementBuildDistinctValues = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    lstDistinctValues: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstDistinctValuesClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);

  private
    // Private members
    //
    m_objStatementPart: TcStatementPart;
    m_objFormSet: TcFormSet;
    m_sValue: String;

  public
    // public properties
    //
    property Part: TcStatementPart                      read m_objStatementPart write m_objStatementPart;
    property FormSet: TcFormSet                         read m_objFormSet       write m_objFormSet;
    property Value: String                              read m_sValue           write m_sValue;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daResourceStrings,
  ADODB_TLB,       // RecordSet
  ExecuteLib;      // TcExecute

// TfrmStatementBuildDistinctValues
//   FormCreate
//
procedure TfrmStatementBuildDistinctValues.FormCreate(Sender: TObject);
begin
  m_objFormSet := nil;
  m_objStatementPart := nil;
  m_sValue := ksEMPTY;
end;

// frmStatementBuildDistinctValues
//   FormShow
//
procedure TfrmStatementBuildDistinctValues.FormShow(Sender: TObject);
var
  m_objExecute: TcExecute;
  rs: TcRecordSet;
begin
  m_objExecute := nil;
  if (m_objStatementPart <> nil) and (m_objFormSet <> nil) then
  try
    screen.Cursor := crAppStart;
    m_objExecute := TcExecute.Create(nil);
    m_objExecute.Connection := m_objFormset.Connection;
    rs := nil;
    with m_objStatementPart do
    try
      rs := m_objExecute.Execute(Format('select distinct %s from %s order by %s', [sColumn, sTable, sColumn]));
      if (rs <> nil) and rs.IsOpen then
      begin
        while not rs.EOF do
        begin
          lstDistinctValues.Items.Add(VarToStr(rs.Fields(0)));
          rs.MoveNext;
        end;
        rs.Close;
      end;
    finally
      rs.Free;
    end;
    if m_sValue <> ksEMPTY then
      with lstDistinctValues do
        ItemIndex := Items.IndexOf(m_sValue);
  finally
    m_objExecute.free;
    screen.Cursor := crDefault;
  end;
  SetState(Sender);
end;

// TfrmStatementBuildDistinctValues
//   SetState
//
procedure TfrmStatementBuildDistinctValues.SetState(Sender: TObject);
begin
  btnOK.Enabled := lstDistinctValues.ItemIndex <> kiUNDEFINED;
end;

// TfrmStatementBuildDistinctValues
//   SetState
//
procedure TfrmStatementBuildDistinctValues.lstDistinctValuesClick(Sender: TObject);
begin
  with lstDistinctValues do
    if ItemIndex <> kiUNDEFINED then
      m_sValue := Items[ItemIndex];
  SetState(Sender);
end;

// TfrmStatementBuildDistinctValues
//   btnOKClick
//
procedure TfrmStatementBuildDistinctValues.btnOKClick(Sender: TObject);
begin
  if btnOK.Enabled then
    ModalResult := mrOK;
end;

end.
