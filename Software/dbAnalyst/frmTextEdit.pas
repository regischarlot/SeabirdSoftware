unit frmTextEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FormLib, SynEdit;

type
  TfrmTextEdit = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    lblStatus: TStaticText;
    mmoSQL: TSynEdit;
    procedure mmoSQLStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure onSelMove(Sender: TObject);

  private
    // Private methods
    //
    function  GetTextStream: String;
    procedure SetTextStream(value: String);
    procedure SetFormSet(value: TcFormSet);

  private
    // Private members
    //
    m_FormSet: TcFormSet;

  public
    // public properties
    //
    property  TextStream: String         read GetTextStream      write SetTextStream;
    property  FormSet: TcFormSet         read m_FormSet          write SetFormSet;
  end;

function EditText(parFormSet: TcFormSet; var value: String): boolean;

implementation

{$R *.dfm}

uses
  daResourceStrings;

// TfrmTextEdit
//   EditText
//
function EditText(parFormSet: TcFormSet; var value: String): boolean;
var
  frm: TfrmTextEdit;
begin
  frm := nil;
  try
    frm := TfrmTextEdit.Create(nil);
    frm.FormSet := parFormSet;
    frm.TextStream := value;
    result := frm.ShowModal = mrOK;
    if result then
      value := frm.TextStream;
  finally
    frm.release;
  end;
end;


// TfrmTextEdit
//   FormCreate
//
procedure TfrmTextEdit.FormCreate(Sender: TObject);
begin
  m_FormSet := nil;
end;

// TfrmTextEdit
//   FormClose
//
procedure TfrmTextEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mmoSQL.Highlighter := nil;
  Action := caHide;
end;

// TfrmTextEdit
//   GetTextStream
//
function TfrmTextEdit.GetTextStream: String;
begin
  result := mmoSQL.Lines.Text;
end;

// TfrmTextEdit
//   SetTextStream
//
procedure TfrmTextEdit.SetTextStream(value: String);
begin
  with mmoSQL.Lines do
  begin
    BeginUpdate;
    Text := value;
    EndUpdate;
    onSelMove(mmoSQL);
  end;
end;

// TfrmTextEdit
//   SetFormSet
//
procedure TfrmTextEdit.SetFormSet(value: TcFormSet);       
begin
  m_FormSet := value;
  if m_FormSet <> nil then
    m_FormSet.SetHighlighter(mmoSQL);
end;

// TfrmTextEdit
//   onSelMove
//
procedure TfrmTextEdit.onSelMove(Sender: TObject);
begin
  if (Sender <> nil) and (Sender is TSynEdit) then
    with (Sender as TSynEdit) do
      lblStatus.Caption := Format('%d/%d', [CaretY + 1, CaretX + 1]);
end;

// TfrmTextEdit
//   mmoOutputStatusChange
//
procedure TfrmTextEdit.mmoSQLStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY] * Changes <> [] then
    onSelMove(Sender);
end;

end.
