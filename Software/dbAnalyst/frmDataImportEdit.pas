unit frmDataImportEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmDataImport;

type
  TfrmDataImportEdit = class(TForm)
    lblObjectName: TLabel;
    edtObjectName: TEdit;
    lblScript: TLabel;
    mmoScript: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    btnCreateObject: TCheckBox;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCreateObjectClick(Sender: TObject);
  public
    objImport: TcImport;
  end;

implementation

{$R *.dfm}

procedure TfrmDataImportEdit.FormCreate(Sender: TObject);
begin
  objImport := nil;
end;

procedure TfrmDataImportEdit.FormShow(Sender: TObject);
const
  kibCAPTION: array[boolean] of string = ('Create Object', 'Recreate Object');
begin
  if objImport <> nil then
  begin
    btnCreateObject.Caption := kibCAPTION[eioExists in objImport.eOptions];
    btnCreateObject.Checked := eioCreateObject in objImport.eOptions;
    edtObjectName.Text := objImport.sName;
    mmoScript.Text := objImport.sCreate;
    btnCreateObjectClick(Sender);
  end;
end;

procedure TfrmDataImportEdit.btnOKClick(Sender: TObject);
begin
  if objImport <> nil then
  begin
    objImport.sCreate := mmoScript.Text;
    if btnCreateObject.Checked then
      objImport.eOptions := objImport.eOptions + [eioCreateObject]
    else
      objImport.eOptions := objImport.eOptions - [eioCreateObject];
  end;
end;

procedure TfrmDataImportEdit.btnCreateObjectClick(Sender: TObject);
begin
  mmoScript.Enabled := btnCreateObject.Checked;
  lblScript.Enabled := mmoScript.Enabled;
end;

end.
