unit dbListViewEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, daGlobals;

type
  TfrmdbListViewEdit = class(TForm)
    mmoValue: TMemo;
    PageControl: TPageControl;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    tsData: TTabSheet;
    tsOptions: TTabSheet;
    btnHexadecimalDisplay: TCheckBox;
    btnFixCRLF: TCheckBox;
    btnWrapDisplay: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);

  private
    m_sValue: String;
    m_pCell: TPoint;
    m_esOptions: TDBListViewOptions;

  private
    procedure SetValue(value: String);
    function  GetValue: String;
    procedure SetDisplay(value: String);
    procedure SetOptions(value: TDBListViewOptions);

  public
    property Value: String                      read GetValue                   write SetValue;
    property Cell: TPoint                       read m_pCell                    write m_pCell;
    property Options: TDBListViewOptions        read m_esOptions                write SetOptions;

  end;

implementation

{$R *.dfm}

uses
  strUtils;

// TfrmdbListViewEdit
//   FormCreate
//
procedure TfrmdbListViewEdit.FormCreate(Sender: TObject);
begin
  m_esOptions := [];
end;

// TfrmdbListViewEdit
//   FormShow
//
procedure TfrmdbListViewEdit.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsData;
  mmoValue.ReadOnly := edboReadOnly in m_esOptions;
  tsOptions.TabVisible := mmoValue.ReadOnly;
  mmoValue.ReadOnly := edboReadOnly in m_esOptions;
  if edboReadOnly in m_esOptions then
  begin
    btnOK.Visible := FALSE;
    btnCancel.Caption := '&Close';
    btnCancel.Default := TRUE;
  end
  else
    mmoValue.SetFocus;
end;

// TfrmdbListViewEdit
//   SetValue
//
procedure TfrmdbListViewEdit.SetValue(value: String);
begin
  m_sValue := value;
  SetDisplay(m_sValue);
end;

// TfrmdbListViewEdit
//   GetValue
//
function TfrmdbListViewEdit.GetValue: String;
begin
  result := mmoValue.Text;
end;

// TfrmdbListViewEdit
//   SetOptions
//
procedure TfrmdbListViewEdit.SetOptions(value: TDBListViewOptions);
begin
  m_esOptions := value;
  btnHexadecimalDisplay.Checked := edboHex in m_esOptions;
  btnFixCRLF.Checked := edboFixCRLF in m_esOptions;
  btnWrapDisplay.Checked := edboWrap in m_esOptions;
  SetDisplay(m_sValue);
end;

// TfrmdbListViewEdit
//   ButtonClick
//
procedure TfrmdbListViewEdit.ButtonClick(Sender: TObject);
begin
  btnFixCRLF.Enabled := not btnHexadecimalDisplay.Checked;
  btnWrapDisplay.Enabled := not btnHexadecimalDisplay.Checked;
  if btnHexadecimalDisplay.Checked then
    m_esOptions := m_esOptions + [edboHex]
  else
    m_esOptions := m_esOptions - [edboHex];
  if btnFixCRLF.Checked then
    m_esOptions := m_esOptions + [edboFixCRLF]
  else
    m_esOptions := m_esOptions - [edboFixCRLF];
  if btnWrapDisplay.Checked then
    m_esOptions := m_esOptions + [edboWrap]
  else
    m_esOptions := m_esOptions - [edboWrap];
  SetDisplay(m_sValue);
end;

// TfrmdbListViewEdit
//   SetDisplay
//
procedure TfrmdbListViewEdit.SetDisplay(value: String);

  function Hex(value: longint): String;
  const
    ksHEX = '0123456789abcdef';
  begin
    result := ksHEX[value div 16 + 1] + ksHEX[value mod 16 + 1];
  end;

var
  s, t: String;
  i, j, K: longint;
const
  kiWIDTH = 16;
begin
  if [edboHex, edboReadOnly] * m_esOptions = [edboHex, edboReadOnly] then
  begin
    mmoValue.WordWrap := FALSE;
    s := ksEMPTY;
    for i := 0 to length(Value) div kiWIDTH do
    begin
      if s <> ksEMPTY then
        s := s + ksCR;
      s := s + Hex((i * kiWIDTH) div $10000) + Hex((i * kiWIDTH) div $100) + Hex((i * kiWIDTH) mod $100) + '  ';
      // Hex Characters
      t := ksEMPTY;
      for j := 1 to kiWIDTH do
      begin
        K := i * kiWIDTH + j;
        if K <= length(value) then
          t := t + Hex(ord(Value[K])) + ' ';
      end;
      s := s + t + DupeString(' ', 3 * kiWIDTH + 2 - length(t));
      // String Characters
      for j := 1 to kiWIDTH do
      begin
        K := i * kiWIDTH + j;
        if K <= length(value) then
        begin
          if (ord(Value[K]) >= 32) and (ord(Value[K]) <= 127) then
            s := s + Value[K]
          else
            s := s + '.';
        end;
      end;
    end;
  end
  else
  begin
    mmoValue.WordWrap := edboWrap in m_esOptions;
    s := Value;
    if edboFixCRLF in m_esOptions then
      s := FixCRLF(s);
  end;
  mmoValue.Text := s;
end;

end.
