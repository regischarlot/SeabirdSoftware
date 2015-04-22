unit frmDataTranslate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConnectionLib,
  ComCtrls, ExtCtrls, PreferenceLib, FormLib;

type
  TfrmDataTranslate = class(TForm)
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnTranslate: TButton;
    lblDefinitionSource: TLabel;
    cboXML: TComboBox;
    btnHelp: TButton;
    Bevel1: TBevel;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label1: TLabel;
    lblDatabaseType: TLabel;
    procedure SetButtonState(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cboXMLClick(Sender: TObject);

  private
    // Private members
    //
    m_FormSet: TcFormSet;

  public
    // public properties
    //
    property    FormSet: TcFormSet                      read m_FormSet          write m_FormSet;
  end;

implementation

{$R *.DFM}

uses
  daGlobals,
  daResourceStrings,
  daObjectLib;

// TfrmConnection
//   FormShow
//
procedure TfrmDataTranslate.FormShow(Sender: TObject);
var
  lst: TcCollection;
  i: longint;
begin
  SetButtonState(Sender);
  lst := m_FormSet.Connection.lstXML;
  for i := 0 to lst.Count - 1 do
    cboXML.Items.AddObject(lst[i].sValue, pointer(lst[i]));
  cboXML.SetFocus;
  if m_FormSet <> nil then
    m_FormSet.SetHeaderImage(imgHeader, 0);
end;

// TfrmConnection
//   cboXMLClick
//
procedure TfrmDataTranslate.cboXMLClick(Sender: TObject);
var
  p: TcObject;
begin
  if cboXML.ItemIndex <> kiUNDEFINED then
  begin
    p := TcObject(cboXML.Items.Objects[cboXML.ItemIndex]);
    if p <> nil then
      m_FormSet.FileName := p.sName;
  end;
  SetButtonState(Sender);
end;

// TfrmDataTranslate
//   SetButtonState
//
procedure TfrmDataTranslate.SetButtonState(Sender: TObject);
begin
  btnTranslate.Enabled := cboXML.ItemIndex <> kiUNDEFINED;
end;

end.



