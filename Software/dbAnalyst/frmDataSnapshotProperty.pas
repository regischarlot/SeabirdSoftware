unit frmDataSnapshotProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DataLib, ADODB_TLB, ConnectionLib, ExtCtrls,
  FormLib;

type
  TfrmDataSnapshotProperty = class(TForm)
    dlgSave: TSaveDialog;
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label1: TLabel;
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnClose: TButton;
    btnHelp: TButton;
    pnlCenter: TPanel;
    pnlCenterTop: TPanel;
    mmoDetails: TMemo;
    lblDetails: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    // Private Members
    //
    m_objDataDifference: TcDataDifference;
    m_objConnection: TcConnection;

  public
    // Public Properties
    //
    property    DataDifference: TcDataDifference  read m_objDataDifference      write m_objDataDifference;
    property    Connection: TcConnection          read m_objConnection          write m_objConnection;
  end;

implementation

{$R *.dfm}

uses
  daGlobals,
  daObjectLib,
  daResourceStrings,
  daStreamLib,
  ExecuteLib,
  ActiveX,
  AxCtrls,
  comObj;

// TfrmDataSnapshotProperty
//   FormCreate
//
procedure TfrmDataSnapshotProperty.FormCreate(Sender: TObject);
begin
  m_objDataDifference := nil;
  m_objConnection := nil;
end;

// TfrmDataSnapshotProperty
//   FormClose
//
procedure TfrmDataSnapshotProperty.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmDataSnapshotProperty
//   FormShow
//
procedure TfrmDataSnapshotProperty.FormShow(Sender: TObject);
begin
  if (m_objConnection <> nil) and (m_objConnection.Parent <> nil) and (m_objConnection.Parent is TcFormSet) then
    (m_objConnection.Parent as TcFormSet).SetHeaderImage(imgHeader, 0);
  // Display
  if m_objDataDifference <> nil then
    mmoDetails.Text := m_objDataDifference.Part[ediDetails];
end;

end.




