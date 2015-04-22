unit frmLogParserAutoTrace;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  OracleExtensions_TLB, ImgList;

type
  TfrmLogParserAutoTrace = class(TForm)
    Panel1: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    pnlBottom: TPanel;
    btnState: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    pnlCenter: TPanel;
    lvSchemas: TListView;
    ImageList: TImageList;

    procedure lvSchemasChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure OnAutoTrace(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    // Private Members
    //
    m_ST: ISessionTrace;

  private
    // Private declarations
    //
    procedure   DisplaySchemaList(Sender: TObject);
    procedure   DisplayErrors(value: String);

  public
    // Public Properties
    //
    property    ST: ISessionTrace           read m_ST              write m_ST;
  end;

implementation

{$R *.dfm}

uses
  ComObj,
  daGlobals,
  daResourceStrings;

const
  kiIMAGE: array[boolean] of longint = (0, 1);

//
// TfrmLogParserAutoTrace
//

// TfrmLogParserAutoTrace
//   FormCreate
//
procedure TfrmLogParserAutoTrace.FormCreate(Sender: TObject);
begin
  m_ST := nil;
end;

// TfrmLogParserAutoTrace
//   FormDestroy
//
procedure TfrmLogParserAutoTrace.FormDestroy(Sender: TObject);
begin
  m_ST := nil;;
end;

// TfrmLogParserAutoTrace
//   FormShow
//
procedure TfrmLogParserAutoTrace.FormShow(Sender: TObject);
begin
  DisplaySchemaList(Sender);
  lvSchemas.SetFocus;
end;

// TfrmLogParserAutoTrace
//   FormClose
//
procedure TfrmLogParserAutoTrace.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmLogParserAutoTrace
//   DisplayErrors
//
procedure TfrmLogParserAutoTrace.DisplayErrors(value: String);
var
  s: string;
  p, q, r: OLEVariant;
  i, j: longint;
begin
  // Get Error Message
  s := ksEMPTY;
  try
    p := CreateOLEObject('Microsoft.XMLDOM');
    if p.LoadXML(value) and (p.ChildNodes.length > 0) then
    begin
      q := p.ChildNodes.item[0];
      for i := 0 to q.ChildNodes.length - 1 do
        if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') = 0 then
        begin
          r := q.ChildNodes.item[i];
          for j := 0 to r.ChildNodes.length - 1 do
            s := s + trim(GetXMLValue(r.ChildNodes.Item[j])) + ksCR;
        end;
    end;
    p := unassigned;
  except
    on E: Exception do
      p := unassigned;
  end;
  // Display Message
  if s <> ksEMPTY then
    Application.MessageBox(PChar(s), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
end;

// TfrmLogParserAutoTrace
//   DisplaySchemaList
//
procedure TfrmLogParserAutoTrace.DisplaySchemaList(Sender: TObject);
var
  s: String;
  p, q: OLEVariant;
  i, j: longint;
  li: TListItem;
const
  kaiICON: array[boolean] of longint = (kiUNDEFINED, 0);
begin
  if m_ST <> nil then
  try
    screen.Cursor := crAppStart;
    s := m_ST.SchemaList[ksEMPTY];
    DisplayErrors(s);
    lvSchemas.Items.BeginUpdate;
    lvSchemas.Items.Clear;
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(s) and (p.ChildNodes.length > 0) then
      begin
        q := p.ChildNodes.item[0];
        for i := 0 to q.ChildNodes.length - 1 do
          if AnsiCompareText(q.ChildNodes.Item[i].nodeName, 'errors') <> 0 then
          begin
            j := strtointdef(GetXMLValue(q.ChildNodes.Item[i], krsXML_AUTOTRACE), 0);
            li := lvSchemas.Items.Add;
            li.Caption := inttostr(j);
            li.SubItems.Add(GetXMLValue(q.ChildNodes.Item[i], krsXML_NAME));
            li.ImageIndex := kaiICON[j = 1];
            li.StateIndex := kaiICON[j = 1];
          end;
      end;
     p := unassigned;
    except
      on E: Exception do
        p := unassigned;
    end;
    lvSchemas.Items.EndUpdate;
  finally
    screen.Cursor := crDefault;
  end;
  SetState(Sender);
end;

// TfrmLogParserAutoTrace
//   SetState
//
procedure TfrmLogParserAutoTrace.SetState(Sender: TObject);
begin
  btnState.Enabled := lvSchemas.Selected <> nil;
  btnState.Tag := kiUNDEFINED;
  if btnState.Enabled then
    btnState.Tag := strtointdef(lvSchemas.Selected.Caption, kiUNDEFINED);
  if btnState.Tag = 1 then
    btnState.Caption := 'Stop Trace'
  else
    btnState.Caption := 'Start Trace';
end;

// TfrmLogParser
//   OnAutoTrace Method
//
procedure TfrmLogParserAutoTrace.OnAutoTrace(Sender: TObject);
var
  s, sSchema: String;
const
  kabSS: array[boolean] of string =
    ('Stop', 'Start');
begin
  if (m_ST <> nil) and (lvSchemas.Selected <> nil) and (btnState.Tag in [0, 1]) then
  begin
    sSchema := lvSchemas.Selected.SubItems[0];
    if Application.MessageBox(PChar(Format('%s Auto Trace for Schema ''%s''?', [kabSS[1 - btnState.Tag = 1], sSchema])), krsINFORMATION, MB_OKCANCEL + MB_ICONEXCLAMATION) = idOK then
    try
      screen.Cursor := crAppStart;
      s := m_ST.SessionAutoTrace[Format('<xml><%s>%s</%s><%s>%d</%s></xml>', [krsXML_SCHEMA, sSchema, krsXML_SCHEMA, krsXML_STATE, 1 - btnState.Tag, krsXML_STATE])];
      DisplaySchemaList(Sender);
      DisplayErrors(s);
      Application.MessageBox(PChar(xmlPart(s, krsXML_ACTION)), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
    finally
      screen.Cursor := crDefault;
    end;
  end;
end;

// TfrmLogParserAutoTrace
//   lvSchemasChange Method
//
procedure TfrmLogParserAutoTrace.lvSchemasChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  SetState(Sender);
end;

end.

