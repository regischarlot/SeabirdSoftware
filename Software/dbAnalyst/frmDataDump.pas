unit frmDataDump;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConnectionLib,
  ComCtrls, ExtCtrls, PreferenceLib, FormLib, daObjectLib;

type
  TeOutputType = (eotCSV, eotTAB, eotHTML, eotXML, eotSQLInsert);

  TfrmDataDump = class(TForm)
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnExtract: TButton;
    lblDefinitionSource: TLabel;
    btnHelp: TButton;
    bvlBottom: TBevel;
    pnlTop: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    lblBoldTitle: TLabel;
    lblTargetOutput: TLabel;
    pnlOutputFormat: TPanel;
    btnCSV: TRadioButton;
    btnTab: TRadioButton;
    btnHTML: TRadioButton;
    btnXML: TRadioButton;
    pnlTargetOutput: TPanel;
    btnClipboard: TRadioButton;
    btnFile: TRadioButton;
    edtFileName: TEdit;
    btnFileName: TButton;
    dlgSave: TSaveDialog;
    lblProgress: TLabel;
    lblProgressText: TLabel;
    lblColumnHeading: TLabel;
    btnGenerateColumnHeadings: TCheckBox;
    btnInsertStatement: TRadioButton;
    procedure SetButtonState(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnTargetOutput(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure OnFormatOuput(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    // Private members
    //
    m_FormSet: TcFormSet;
    m_sSQL: String;
    m_iFormat: TeOutputType;
    m_objData: TcObject;

  public
    // public properties
    //
    property    FormSet: TcFormSet                      read m_FormSet          write m_FormSet;
    property    SQL: String                             read m_sSQL             write m_sSQL;
    property    Data: TcObject                          read m_objData          write m_objData;
  end;

implementation

{$R *.DFM}

uses
  Variants,
  daGlobals,
  daResourceStrings,
  ExecuteLib,
  StrUtils,
  DataLib,
  StatementLib,
  Clipbrd,
  ADODB_TLB;

const
  krsEXTENSION: array[TeOutputType] of string =
    ('csv', 'tab', 'html', 'xml', 'sql');
  krsFILTER: array[TeOutputType] of string =
    ('Comma-Separated Text File (*.csv)', 'Tab-Separated Text File (*.tab)', 'HTML File (*.html)', 'XML File (*.xml)', 'SQL File (*.sql)');

// TfrmConnection
//   FormCreate
//
procedure TfrmDataDump.FormCreate(Sender: TObject);
begin
  m_objData := nil;
  m_sSQL := ksEMPTY;
end;

// TfrmConnection
//   FormShow
//
procedure TfrmDataDump.FormShow(Sender: TObject);
var
  b: boolean;
begin
  btnCSV.Checked := TRUE;
  btnClipboard.Checked := TRUE;
  SetButtonState(Sender);
  if m_FormSet <> nil then
    m_FormSet.SetHeaderImage(imgHeader, 0);
  b := m_FormSet.Preferences.IsLicensed;
  btnTab.Enabled := b;
  btnXML.Enabled := b;
  btnInsertStatement.Enabled := b;
end;

// TfrmDataDump
//   SetButtonState
//
procedure TfrmDataDump.SetButtonState(Sender: TObject);
begin
  btnExtract.Enabled := (m_FormSet <> nil) and (btnClipboard.Checked or (btnFile.Checked and (edtFileName.Text <> ksEMPTY)));
end;

// TfrmDataDump
//   OnTargetOutput
//
procedure TfrmDataDump.OnTargetOutput(Sender: TObject);
begin
  edtFileName.Enabled := (Sender as TComponent).Tag = 2;
  btnFileName.Enabled := edtFileName.Enabled;
  SetButtonState(Sender);
end;

// TfrmDataDump
//   btnFileNameClick
//
procedure TfrmDataDump.btnFileNameClick(Sender: TObject);
begin
  dlgSave.FileName := edtFileName.Text;
  if dlgSave.FileName = ksEMPTY then
    dlgSave.FileName := '*.' + dlgSave.DefaultExt;
  if dlgSave.Execute then
    edtFileName.Text := dlgSave.FileName;
  SetButtonState(Sender);
end;

// TfrmDataDump
//   OnFormatOuput
//
procedure TfrmDataDump.OnFormatOuput(Sender: TObject);
begin
  m_iFormat := TeOutputType((Sender as TComponent).Tag);
  dlgSave.DefaultExt := krsEXTENSION[m_iFormat];
  dlgSave.Filter := krsFILTER[m_iFormat] + '|*.export|All Files (*.*)|*.*';
  SetButtonState(Sender);
end;

// TfrmConnection
//   btnExtractClick
//
procedure TfrmDataDump.btnExtractClick(Sender: TObject);

  // Tool
  //   GetOutput
  //
  procedure AppendStr(parList: TStringList; value: String);
  begin
    if value <> ksEMPTY then
      parList.Add(value);
  end;

  // Tool
  //   GetHeader
  //
  procedure GetHeader(parList: TStringList; parTableName: String);
  begin
    //
    // Global Header, Footers.
    // >> HTML
    if m_iFormat = eotHTML then
      AppendStr(parList, '<html>' + ksCR +
                         '<HEAD>' + ksCR +
                         '<style type="text/css"> ' + ksCR +
                         '<!-- ' + ksCR +
                         'div {font-family: sans-serif; font-size: 10pt; } ' + ksCR +
                         'td {font-family: sans-serif; font-size: 9pt; } ' + ksCR +
                         'TD.Title {color: white; background-color=#6780b8;} ' + ksCR +
                         'TD.OddRow {background-color=#f5f4e7;} ' + ksCR +
                         'TD.EvenRow {background-color=#ebead8;} ' + ksCR +
                         '--> ' + ksCR +
                         '</style>' + ksCR +
                         '</HEAD>')
    // >> XML
    else if m_iFormat = eotXML then
      AppendStr(parList, '<list>')
    // >> SQL
    else if m_iFormat = eotSQLInsert then
      AppendStr(parList, '-- ' + ksCR +
                         '-- ' + parTableName + ksCR +
                         '-- ');
  end;

  // Tool
  //   GetFooter
  //
  procedure GetFooter(parList: TStringList; parTableName: String);
  begin
    //
    // Global Header, Footers.
    // >> HTML
    if m_iFormat = eotHTML then
      AppendStr(parList, '</html>');
    // >> XML
    if m_iFormat = eotXML then
      AppendStr(parList, '</list>');
  end;

  // Tool
  //   GetOutput
  //
  procedure GetOutput(parList: TStringList; value, parTableName: String; parOrder: longint);
  var
    x: TcExecute;
    rs: TcRecordSet;
    i, L: longint;
    t1, t2, s: String;
  const
    krsTD_CLASS: array[0..1] of String = ('EvenRow', 'OddRow');
  begin
    x := nil;
    L := 0;
    try
      x := TcExecute.Create(nil);
      x.Connection := m_FormSet.Connection;
      rs := nil;
      try
        try
          //
          // A. Process query
          rs := x.Execute(value, Format('Export Data Content Statement #%d', [parOrder]));
          // Process the query if successful
          if (rs <> nil) and rs.IsOpen and not rs.EOF then
          begin
            // HTML Display
            if m_iFormat in [eotHTML, eotXML] then
              AppendStr(parList, Format('<div>%s</div>', [value]) + '<table cellpadding="1" cellspacing="1" bgcolor="B3B2A4">');
            //
            // generate Headers?
            if btnGenerateColumnHeadings.Checked then
              case m_iFormat of
                eotCSV, eotTAB: // Text Header
                begin
                  s := ksEMPTY;
                  for i := 0 to rs.FieldCount - 1 do
                  begin
                    if (m_iFormat = eotCSV) and (i > 0) then
                      s := s + ','
                    else if (m_iFormat = eotTAB) and (i > 0) then
                      s := s + kcTAB;
                    s := s + Format('"%s"', [rs.FieldName(i)]);
                  end;
                  AppendStr(parList, s);
                end;
                eotHTML: // HTML Header
                begin
                  AppendStr(parList, kcTAB + '<tr>');
                  for i := 0 to rs.FieldCount - 1 do
                    AppendStr(parList, Format('%s<td class="Title">%s</td>', [kcTAB + kcTAB, VarToStr(rs.Fields(i))]));
                  AppendStr(parList, kcTAB + '</tr>');
                end;
              end;
            //
            // Cells
            while not rs.EOF do
            begin
              if L mod 100 = 0 then
              begin
                lblProgressText.Caption := Format('Query #%d, Record # %d', [parOrder, L]);
                Application.ProcessMessages;
              end;
              case m_iFormat of
                eotCSV: // CSV
                begin
                  s := ksEMPTY;
                  for i := 0 to rs.FieldCount - 1 do
                  begin
                    if i > 0 then
                      s := s + ',';
                    s := s + Format('"%s"', [VarToStr(rs.Fields(i))]);
                  end;
                  AppendStr(parList, s);
                end;
                eotTAB: // TAB
                begin
                  s := ksEMPTY;
                  for i := 0 to rs.FieldCount - 1 do
                  begin
                    if i > 0 then
                      s := s + kcTAB;
                    s := s + VarToStr(rs.Fields(i));
                  end;
                  AppendStr(parList, s);
                end;
                eotHTML: // HTML
                  begin
                    AppendStr(parList, kcTAB + '<tr>');
                    for i := 0 to rs.FieldCount - 1 do
                      AppendStr(parList, Format('%s<td class="%s" name="%s">%s</td>', [kcTAB + kcTAB, krsTD_CLASS[L mod 2], rs.FieldName(i), TextToXML(VarToStr(rs.Fields(i)))]));
                    AppendStr(parList, kcTAB + '</tr>');
                  end;
                eotXML: // XML
                  begin
                    AppendStr(parList, kcTAB + '<row>');
                    for i := 0 to rs.FieldCount - 1 do
                      AppendStr(parList, Format('%s<cell name="%s">%s</cell>', [kcTAB + kcTAB, rs.FieldName(i), TextToXML(VarToStr(rs.Fields(i)))]));
                    AppendStr(parList, kcTAB + '</row>');
                  end;
                eotSQLInsert: // Insert Statement
                  begin
                    // Columns
                    t1 := ksEMPTY;
                    for i := 0 to rs.FieldCount - 1 do
                    begin
                      if i > 0 then
                        t1 := t1 + ', ';
                      t1 := t1 + rs.FieldName(i);
                    end;
                    // Values
                    t2 := ksEMPTY;
                    for i := 0 to rs.FieldCount - 1 do
                    begin
                      if i > 0 then
                        t2 := t2 + ', ';
                      case rs.FieldType(i) of
                        etUndefined, etNull, etAnsiString, etUnicodeString, etDate, etLongString, etBinary:
                          t2 := t2 + Format('''%s''', [AnsiReplaceText(VarToStr(rs.Fields(i)), '''', '''''')]);
                        etInteger, etBoolean, etFloat:
                        begin
                          s := VarToStr(rs.Fields(i));
                          if s = ksEMPTY then
                            s := 'null';
                          t2 := t2 + s;
                        end;
                      end;
                    end;
                    // String
                    AppendStr(parList, format('insert into %s (%s) values (%s);', [parTableName, t1, t2]));
                  end;
              end;
              inc(L);
              rs.MoveNext;
            end;
            rs.Close;
            // HTML Display
            if m_iFormat in [eotHTML, eotXML] then
              AppendStr(parList, '</table><br>');
          end
          else if not ((rs <> nil) and rs.IsOpen and rs.EOF) then
            Application.MessageBox(PChar(Format('Query failed: %s.', [x.Error])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
        except
          on E: Exception do
            Application.MessageBox(PChar(E.Message), krsINFORMATION, MB_OK + MB_ICONSTOP);
        end;
      finally
        rs.Free;
      end;
    finally
      x.free;
    end;
    lblProgressText.Caption := Format('Query #%d, %d Record%s', [parOrder, L, HasS(L)]);
    AppendStr(parList, ksCR);
  end;

var
  i, j: longint;
  s: String;
  objStmt: TcStatementList;
  p: TcObject;
  lst: TcBag;
  strm: TStringList;
begin
  if m_FormSet <> nil then
  begin
    // Get the Record Set Output
    strm := nil;
    try
      screen.Cursor := crHourGlass;
      strm := TStringList.Create;
      try
        //
        // A. Proceed by String
        //
        if m_sSQL <> ksEMPTY then
        begin
          objStmt := nil;
          try
            objStmt := TcStatementList.Create(nil);
            try
              if (((pos(kcPARAGRAPH, m_sSQL) > 0) and objStmt.Parse(estParagraph, m_sSQL)) or objStmt.Parse(estSemicolon, m_sSQL)) and (objStmt.count > 0) then
                for j := 0 to objStmt.Count - 1 do
                begin
                  s := (objStmt[j] as TcStatement).SQL;
                  GetHeader(strm, ksEMPTY);
                  GetOutput(strm, s, ksEMPTY, j);
                  GetFooter(strm, ksEMPTY);
                end;
            except
              //
            end;
          finally
            objStmt.free;
          end;
        end
        //
        // B. Proceed by Pointer, TcData, TcMetaData
        //
        else if m_objData <> nil then
        begin
          // Single Data
          if m_objData is TcData then
          begin
            s := trim((m_objData as TcData).Statement([enSQL], krsCONTENT));
            if trim(s) <> ksEMPTY then
            begin
              GetHeader(strm, m_objData.sValue);
              GetOutput(strm, s, m_objData.sValue, 1);
              GetFooter(strm, m_objData.sValue);
            end;
          end
          // All Tables
          else if m_objData is TcMetaData then
          begin
            lst := nil;
            try
              // Create List
              lst := TcBag.Create(nil);
              for i := 0 to (m_objData as TcMetaData).lstIndex.Count - 1 do
                lst.Add(TcObject((m_objData as TcMetaData).lstIndex.Objects[i]));
              // Sort List
              (m_objData as TcMetaData).SortByDependency(lst);
              // Process List
              for i := 0 to lst.Count - 1 do
              begin
                p := lst[i];
                if (p <> nil) and (p is Tcdata) then
                begin
                  s := trim((p as TcData).Statement([enSQL], krsCONTENT));
                  if trim(s) <> ksEMPTY then
                  begin
                    GetHeader(strm, p.sValue);
                    GetOutput(strm, s, p.sValue, i + 1);
                    GetFooter(strm, p.sValue);
                  end;
                end;
              end;
            finally
              lst.free;
            end;
          end;
        end;
        //
        // B. Save Query
        if strm.Text <> ksEMPTY then
        begin
          if btnClipboard.Checked then
          begin
            Clipboard.AsText := strm.Text;
            Application.MessageBox('Output was sent to clipboard.', krsINFORMATION, MB_OK + MB_ICONINFORMATION);
          end
          else
          begin
            strm.SaveToFile(edtFileName.Text);
            Application.MessageBox(PChar(Format('Output was save to ''%s''.', [edtFileName.Text])), krsINFORMATION, MB_OK + MB_ICONINFORMATION);
          end;
        end;
        ModalResult := mrOK;
      except
        on E: Exception do
          Application.MessageBox(PChar(e.Message), krsERROR, MB_OK + MB_ICONERROR);
      end;
    finally
      strm.free;
      screen.Cursor := crDefault;
    end;
  end;
end;

end.



