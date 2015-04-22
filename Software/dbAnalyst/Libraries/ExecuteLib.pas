unit ExecuteLib;

interface

{$M+}

uses
  daGlobals,
  daObjectLib,
  daStreamLib,
  Windows,
  OLEDB,
  Classes,
  ActiveX,
  sysUtils,
{$IFNDEF FACTORY}
  PreferenceLib,
{$ENDIF}
  ConnectionLib,
  ADODB_TLB,
  Oracle;

type
  TcImportResult = record
    bSuccess: boolean;
    iRecImported: longint;
    iRecTotal: longint;
  end;

  TcExecuteResult = (erAllRecordSet, erRecordSetWithData, erAddToResult, erDisplayNoResult);
  TcExecuteResultSet = set of TcExecuteResult;
  TeQueryButton = (ebsStart, ebsStop, ebsRestart, ebsPause, ebsAnimate);
  TeQueryButtonSet = set of TeQueryButton;
  TobjSetButtons = procedure (eparState: TeQueryButtonSet) of object;
  TeExecuteState = (eesIdle, eesExecute, eesFetch);
  ThdlExecuteResult = procedure (value: longint) of object;

  TcRecordSet = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcExecute is the base object for batch SQL execution.
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 08/24/06 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_eMode: TeConnectionMode;
    m_objRecordSet: Recordset;
    m_objOracleQuery: TOracleQuery;
    m_bIsUpdatable: boolean;
    m_objArray: OLEVariant;
    m_eFetchType: TeFetchType;

  private
    // Private declaration
    //
    function    GetIsOpen: boolean;
    function    GetEOF: boolean;
    function    GetErrors: String;
    function    GetRowCount: longint;
    function    GetAsString(Index: longint): string;
    function    GetFieldValue(Index: longint): OLEVariant; overload;
    function    GetFieldValue(Index: string): OLEVariant; overload;

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
    function    MoveFirst: boolean;
    procedure   MoveNext;
    procedure   Close;
    function    Fields(Index: longint): Variant; overload;
    function    Fields(Index: String): Variant; overload;
    function    FieldCount: longint;
    function    FieldName(Index: longint): String;
    function    FieldIndex(value: string): longint;
    function    FieldIsNull(Index: longint): boolean; overload;
    function    FieldIsNull(Index: String): boolean; overload;
    function    HasField(value: String): boolean;
    function    FieldSize(value: longint): longint;
    function    RecordAffected: longint;
    procedure   Move(NumRecords: Integer; Start: OleVariant);
    procedure   ReQuery;
    function    FieldType(Index: longint): TeDatatype;
    function    Update(sFieldName, sFieldValue: String): boolean;

  public
    // Public Properties
    //
    property    eMode: TeConnectionMode              read m_eMode               write m_eMode;
    property    IsOpen: boolean                      read GetIsOpen;
    property    EOF: boolean                         read GetEOF;
    property    Errors: string                       read GetErrors;
    property    IsUpdatable: boolean                 read m_bIsUpdatable         write m_bIsUpdatable;
    property    RowCount: longint                    read GetRowCount;
    property    AsString[value: longint]: String     read GetAsString;
    property    FetchType: TeFetchType               read m_eFetchType          write m_eFetchType;
  end;

  TcExecute = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcExecute is the base object for batch SQL execution.
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 09/09/01 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objDisplay: TObject;
    m_objConnection: TcConnection;
    m_iRowCount: longint;
    m_iCommitPoint: longint;
    m_sError: String;
    m_bStop: boolean;
    m_objSetButtons: TobjSetButtons;
    m_eState: TeExecuteState;
    m_hdlResult: ThdlExecuteResult;
    m_bIsReadOnly: boolean;
    m_bIsAsynchronous: boolean;
    m_lstWidth: TStringList;
{$IFNDEF FACTORY}
    m_bExecuteComplete: boolean;
    m_objPreferences: TcPreferenceList;
{$ENDIF}
    m_fctProgress: TfctProgress;
    m_iLineOffset: longint;
    m_eFetchType: TeFetchType;

  private
    // Private members
    //
    function    GetError: String;
    function    GetADOError: String;
    procedure   SetCommitPoint(value: longint);
{$IFNDEF FACTORY}
    procedure   onExecuteComplete(Sender: TObject; RecordsAffected: Integer; var pError: OleVariant; var adStatus: OleVariant; var pCommand: OleVariant; var pRecordset: OleVariant; var pConnection: OleVariant);
{$ENDIF}
    procedure   SetFieldWidth(Name: String; value: longint);
    procedure   SetError(value: String);
    procedure   SetOracleErrorException(e: EOracleError; value: String; parQuery: TOracleQuery); overload;
    procedure   SetOracleErrorException(e: Exception; value: String); overload;

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
    function    Execute(value: String): TcRecordSet; overload;
    function    ThreadedExecute(value: String): TcRecordSet; overload;
    function    Execute(value, sSQLName: String): TcRecordSet; overload;
    function    ExecuteUndirect(value: String): boolean; overload;
    function    ExecuteUndirect(value, sSQLName: String): boolean; overload;
{$IFNDEF FACTORY}
    function    Execute(value: String; m_objDisplay: TObject): boolean; overload;
    function    Execute(value: String; m_objDisplay: TObject; eparMode: TcExecuteResultSet): boolean; overload;
    function    Execute(value: String; m_objStream: TcDataStream): boolean; overload;
    function    Import(value: String; m_objStream: TcDataStream; strmLog: TMemoryStream; parTableDelimiter, parColumnDelimiter: String): TcImportResult;
{$ENDIF}
    procedure   onStop(Sender: TObject);
    procedure   onPause(Sender: TObject);
    procedure   onRestart(Sender: TObject);
    function    OpenSchema(Schema: SchemaEnum; vparObject: OleVariant; sSQLName: String): TcRecordSet;

  public
    // Public Properties
    //
    property    Connection: TcConnection             read m_objConnection       write m_objConnection;
    property    Error: String                        read GetError;
    property    RowCount: longint                    read m_iRowCount           write m_iRowCount;
    property    CommitPoint: longint                 read m_iCommitPoint        write SetCommitPoint;
    property    onSetButton: TobjSetButtons          read m_objSetButtons       write m_objSetButtons;
    property    State: TeExecuteState                read m_eState;
    property    hdlResult: ThdlExecuteResult         read m_hdlResult           write m_hdlResult;
    property    IsReadOnly: boolean                  read m_bIsReadOnly         write m_bIsReadOnly;
    property    IsAsynchronous: boolean              read m_bIsAsynchronous     write m_bIsAsynchronous;
    property    FieldWidth[value: String]: longint                              write SetFieldWidth;
{$IFNDEF FACTORY}
    property    Preferences: TcPreferenceList        read m_objPreferences      write m_objPreferences;
{$ENDIF}
    property    fctProgress: TfctProgress            read m_fctProgress         write m_fctProgress;
    property    iLineOffset: longint                 read m_iLineOffset         write m_iLineOffset;
    property    FetchType: TeFetchType               read m_eFetchType          write m_eFetchType;
  end;

  TeCommandType = (eadoctSQL, eadoctPackage);

  TcCommandParameter = class(TcObject)
  public
    // Public declarations
    //
    Value: OLEVariant;
    Datatype: longint;
    Direction: ParameterDirectionEnum;
  end;

  TcCommand = class(TObject)
  private
    // Private Declarations
    //
    m_sSQL: String;
    m_objConnection: TcConnection;
    m_objCommand: _Command;
    m_objParameters: TcCollection;
    m_eType: TeCommandType;

  private
    // Private Methods
    //
    procedure   SetConnection(const Value: TcConnection);
    // function    ParseSQL(const SQL: string): string;
    // procedure   MakeParameter(const Name: String);
    procedure   SetParameter(const Name: String; const iparType: longint; const Value: OLEVariant);
    function    GetParameters: Parameters;

  published
    // Published Methods
    //
    procedure   SetSQL(Index: Integer; const Value: String);
    procedure   SetParamAsInteger(const Name: String; const Value: longint);
    procedure   SetParamAsString(const Name: String; const Value: String);
    procedure   SetParamAsDate(const Name: String; const Value: TDateTime);
    procedure   SetParamAsCLOB(const Name: String; const Value: String);
    procedure   SetParamAsBoolean(const Name: String; const Value: boolean);
    procedure   SetParamAsFloat(const Name: String; const Value: double);
    procedure   SetParamAsBinary(const Name: String; const Value: OLEVariant);
    procedure   SetParamDir(const Name: String; const Value: ParameterDirectionEnum);
    function    GetParamAsInteger(const Name: String): longint;
    function    GetParamAsString(const Name: String): String;
    function    GetParamAsDate(const Name: String): TDateTime;
    function    GetParamAsBoolean(const Name: String): boolean;
    function    GetParamAsBinary(const Name: String): OLEVariant;
    function    GetParamAsFloat(const Name: String): double;
    function    GetParameter(const Name: String): TcCommandParameter;
    function    GetParamDir(const Name: String): ParameterDirectionEnum;

  public
    // Public Members
    //
    constructor Create; overload; virtual;
    destructor  Destroy; override;
    function    Execute: TcRecordSet;

  public
    // Public properties
    //
    property    Params:Parameters                           read GetParameters;
    property    Text: String                       index 0  read m_sSQL            write SetSQL;
    property    PackageName: String                index 1  read m_sSQL            write SetSQL;
    property    Connection: TcConnection                                           write SetConnection;
    property    ParamAsInteger[const Name: String]: longint read GetParamAsInteger write SetParamAsInteger;
    property    ParamAsString[const Name: String]: String   read GetParamAsString  write SetParamAsString;
    property    ParamAsDate[const Name: String]: TDateTime  read GetParamAsDate    write SetParamAsDate;
    property    ParamAsCLOB[const Name: String]: String     read GetParamAsString  write SetParamAsCLOB;
    property    ParamAsBoolean[const Name: String]: boolean read GetParamAsboolean write SetParamAsBoolean;
    property    ParamAsFloat[const Name: String]: double    read GetParamAsFloat   write SetParamAsFloat;
    property    ParamAsBinary[const Name: String]: OLEVariant read GetParamAsBinary   write SetParamAsBinary;
    property    ParamDirection[const Name: String]: ParameterDirectionEnum read GetParamDir write SetParamDir;
  end;

  TcExecuteThreaded = class(TThread)
  private
    // Private Methods
    //
    m_bExecutionState: boolean;
    m_objExecute: TcExecute;
    m_sSQL: String;
    m_objResult: TcRecordSet;

  protected
    // Protected Methods
    //
    Procedure Initialize;
    procedure Execute; override;

  public
    // Public properties
    //
    property objExecute: TcExecute         read m_objExecute       write m_objExecute;
    property bExecutionState: boolean      read m_bExecutionState  write m_bExecutionState;
    property SQL: String                   read m_sSQL             write m_sSQL;
  end;

implementation

uses
  strUtils,
  stdCtrls,
  Controls,
  Messages,
  Progress,
{$IFNDEF FACTORY}
  Main,
  dbListView,
  SynEdit,
  StatementLib,
{$ENDIF}
  Math,
  Forms,
  ComCtrls,
  ComObj,
  Variants,
  daResourceStrings;

//
// TcExecute
//

// TcExecute
//   Constructor
//
constructor TcExecute.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objConnection := nil;
  m_objDisplay := nil;
  m_iCommitPoint := 10;
  m_sError := ksEMPTY;
  m_objSetButtons := nil;
  m_bStop := TRUE;
  m_eState := eesIdle;
  m_hdlResult := nil;
  m_bIsReadOnly := FALSE;
  m_bIsAsynchronous := FALSE;
  m_lstWidth := TStringList.Create;
{$IFNDEF FACTORY}
  m_objPreferences := nil;
{$ENDIF}
  m_fctProgress := nil;
  m_iLineOffset := 0;
  m_eFetchType := eftGetRows;
end;

// TcExecute
//   Destroy
//
Destructor TcExecute.Destroy;
begin
  m_lstWidth.free;
  inherited Destroy;
end;

// TcExecute
//   Clear
//
procedure TcExecute.Clear;
begin
  inherited Clear;
  m_lstWidth.Clear;
end;

{$IFNDEF FACTORY}
// TcExecute
//   Execute (1)
//
function TcExecute.Execute(value: String; m_objDisplay: TObject): boolean;
begin
  result := Execute(value, m_objDisplay, [erAllRecordSet]);
end;
{$ENDIF}

{$IFNDEF FACTORY}
// TcExecute
//   Execute (2)
//
function TcExecute.Execute(value: String; m_objDisplay: TObject; eparMode: TcExecuteResultSet): boolean;

  // Tools
  //   SetDisplay
  //
  procedure SetDisplay(value: String);
  var
    L: longint;
  begin
    if (m_objDisplay <> nil) and (m_objDisplay is TSynEdit) then
    begin
      if pos(ksCR, value) = 0 then
        (m_objDisplay as TSynEdit).Lines.Add(value)
      else
      repeat
        L := pos(ksCR, value);
        if L > 0 then
        begin
          (m_objDisplay as TSynEdit).Lines.Add(system.copy(value, 1, L - 1));
          system.delete(value, 1, L + 1);
        end
        else
          (m_objDisplay as TSynEdit).Lines.Add(value);
      until L = 0;
    end;
  end;

  // Tools
  //   GotoLastLine
  //
  procedure GotoLastLine;
  begin
    if (m_objDisplay <> nil) and (m_objDisplay is TSynEdit) then
      with m_objDisplay as TSynEdit do
        CaretY := Lines.Count + 1;
  end;

  // Tools
  //   SetHeader
  //
  procedure SetHeader(value: string);
  var
    p: TListColumn;
  begin
    if (m_objDisplay <> nil) and (m_objDisplay is TListView) then
    begin
      p := (m_objDisplay as TListView).Columns.Add;
      if p <> nil then
        p.Caption := value;
    end;
  end;

  // Tools
  //   SizeColumns
  //
  procedure SizeColumns(value: longint);
  var
    i: longint;
  begin
    if (m_objDisplay <> nil) and (m_objDisplay is TListView) then
      with m_objDisplay as TListView do
        for i := 0 to Columns.count - 1 do
          Columns[i].Width := value;
  end;

  // Tools
  //   ClearDisplay
  //
  procedure ClearDisplay;
  begin
    if (m_objDisplay <> nil) and (m_objDisplay is TSynEdit) then
      (m_objDisplay as TSynEdit).lines.Clear
    else if (m_objDisplay <> nil) and (m_objDisplay is TListView) then
    begin
      (m_objDisplay as TListView).Items.Clear;
      (m_objDisplay as TListView).Columns.Clear;
    end;
  end;

  // Tools
  //   GetRecordSet
  //
  function GetRecordSet(value: String): TcRecordSet;
  var
    strm: TcTokenStream;
    L: longint;
    d: TDateTime;
  begin
    result := nil;
    if value <> ksEMPTY then
    begin
      //woops m_objConnection.adoConnection.Errors.Clear;
      //
      // OpenSchema Command
      if system.copy(uppercase(trim(value)), 1, length(krsOPENSCHEMA)) = uppercase(krsOPENSCHEMA) then
      begin
        // Get the SchemaEnum value
        L := kiUNDEFINED;
        strm := nil;
        try
          strm := TcTokenStream.Create;
          strm.AsValue := value;
          if not strm.EOS then
            strm.Match(_STRING);
          if not strm.EOS then
            L := strtointdef(strm.Token.Value, kiUNDEFINED);
        finally
          strm.Free;
        end;
        // Execute the openschema command
        if L <> kiUNDEFINED then
        try
          result := OpenSchema(L, VarArrayOf([Unassigned, Unassigned, Unassigned, Unassigned]), EmptyParam);
          SetDisplay(Format('%s %d', [krsOPENSCHEMA, L]));
        except
          on E: Exception do
          begin
            SetOracleErrorException(E, value);
            result := nil;
          end;
        end;
      end
      else
      //
      // Standard Command
      begin
        case m_objConnection.eMode of
          ecmMDAC:
            begin
              result := TcRecordSet.Create(nil);
              result.eMode := m_objConnection.eMode;
              result.FetchType := m_eFetchType;
              result.m_objRecordSet := CreateCOMObject(CLASS_RecordSet) as RecordSet;
              result.m_objRecordSet.CursorLocation := adUseClient;
              L := 0;
              d := now;
              if m_bIsAsynchronous then
                L := adAsyncExecute;
              try
                case m_bIsReadOnly of
                  FALSE:
                    result.m_objRecordSet.Open(value, m_objConnection.adoConnection, adOpenDynamic, adLockOptimistic, L);
                  TRUE:
                    result.m_objRecordSet.Open(value, m_objConnection.adoConnection, adOpenStatic, adLockReadOnly, L);
                end;
                result.MoveFirst;
                if m_objConnection <> nil then
                  m_objConnection.SetLog(value, now - d);
              except
                on E: Exception do
                  SetOracleErrorException(E, value);
              end;
              result.IsUpdatable := result.m_objRecordSet.Supports(adBookMark or adUpdate);
            end;
          ecmOracle:
            result := ThreadedExecute(value);
        end;
      end;
    end;
  end;

  // Tools
  //   DisplayRecordSet
  //
  function DisplayRecordSet(rs: TcRecordSet; varRecordAffected: OLEVariant; iMaxFieldWidth: longint): boolean;
  var
    s, t, v, sResult, u: String;
    i, j, L, c: longint;
    lst: TList;
    v2: OLEVariant;
  begin
    result := FALSE;
    sResult := ksEMPTY;
    lst := nil;
    if rs <> nil then
    try
      lst := TList.Create;
      //
      // Textual display
      if rs.IsOpen then
      begin
        //
        // TSynEdit Item
        //
        if m_objDisplay is TSynEdit then
        begin
          c := 0;
          s := ksEMPTY;
          t := ksEMPTY;
          for i := 0 to rs.FieldCount - 1 do
          begin
            L := min(rs.FieldSize(i), iMaxFieldWidth);
            if i < m_lstWidth.Count then
              L := longint(m_lstWidth.Objects[i]);
            lst.Add(pointer(L));
            u := rs.FieldName(i);
            if (i < m_lstWidth.Count) and (m_lstWidth[i] <> ksEMPTY) then
              u := m_lstWidth[i];
            s := s + pad(u, L) + ' ';
            t := t + RepeatStr(L, '-') + ' ';
          end;
          if (rs.FieldCount > 0) and ({$IFNDEF FACTORY}(m_objPreferences = nil) or ((m_objPreferences <> nil) and (m_objPreferences.StringVal[krsPREF_CONSOLESHOWOUTPUTHEADER] <> krsFALSE)){$ELSE}TRUE{$ENDIF}) then
          begin
            SetDisplay(s);
            SetDisplay(t);
          end;
          rs.MoveFirst;
          while not rs.EOF and not m_bStop do
          begin
            if (c > 0) and (c mod 10 = 0) then
              Application.ProcessMessages;
            case rs.eMode of
              ecmMDAC:
                case m_eFetchType of
                  //
                  // Get as much row as possible
                  eftGetRows:
                  begin
                    if VarArrayDimCount(rs.m_objArray) = 2 then
                      for j := VarArrayLowBound(rs.m_objArray, 2) to VarArrayHighBound(rs.m_objArray, 2) do
                      begin
                        s := ksEMPTY;
                        for i := VarArrayLowBound(rs.m_objArray, 1) to VarArrayHighBound(rs.m_objArray, 1) do
                        begin
                          v2 := VarArrayGet(rs.m_objArray, [i, j]);
                          L := longint(lst[i]);
                          if VarIsNull(v2) then
                            v := krsNULL
                          else
                            v := VarToStr(v2); // FieldToItems(v2, '~')
                          s := s + NoLineReturn(pad(v, L)) + ' ';
                        end;
                        SetDisplay(s);
                        inc(c);
                        if m_bStop then
                          break;
                      end;
                    rs.MoveNext;
                  end;
                  //
                  // Process with difficult ADO drivers
                  eftMoveNext:
                  begin
                    s := ksEMPTY;
                    for i := VarArrayLowBound(rs.m_objArray, 1) to VarArrayHighBound(rs.m_objArray, 1) do
                    begin
                      v2 := VarArrayGet(rs.m_objArray, [i]);
                      L := longint(lst[i]);
                      if VarIsNull(v2) then
                        v := krsNULL
                      else
                        v := VarToItems(v2, '~');
                      s := s + NoLineReturn(pad(v, L)) + ' ';
                    end;
                    SetDisplay(s);
                    inc(c);
                    if m_bStop then
                      break;
                    rs.MoveNext;
                  end;
                end;
              ecmOracle:
              begin
                try
                  s := ksEMPTY;
                  for i := 0 to rs.FieldCount - 1 do
                  begin
                    v2 := rs.Fields(i);
                    L := longint(lst[i]);
                    if VarIsNull(v2) then
                      v := krsNULL
                    else
                      v := VarToStr(v2); // FieldToItems(v2, '~')
                    s := s + NoLineReturn(pad(v, L)) + ' ';
                  end;
                except
                  on E: Exception do
                    SetDisplay('Error: ' + E.Message);
                end;
                SetDisplay(s);
                inc(c);
                if m_bStop then
                  break;
                rs.MoveNext;
              end;
            end;
          end;
          if m_sError = ksEMPTY then
            sResult := sResult + ksCR + 'Success.'
          else
            sResult := sResult + ksCR + 'Error!';
          if ( (m_objPreferences = nil) or ((m_objPreferences <> nil) and (m_objPreferences.StringVal[krsPREF_CONSOLESHOWDATASETROWCOUNT] <> krsFALSE))) then
          begin
            L := strtointdef(VarToStr(varRecordAffected), kiUNDEFINED);
            if (rs.FieldCount > 0) and not m_bStop then
              sResult := sResult + ksCR + Format('(%d row%s)', [c, HasS(c)])
            else if rs.FieldCount = 0 then
              sResult := sResult + ksCR + Format('(%d row%s affected)', [L, HasS(L)]);
          end;
          if m_bStop then
            sResult := sResult + ksCR + '([Fetch] Stopped by User)';
        end;
      end
      //
      //
      else if erAllRecordSet in eparMode then
      begin
        sResult := ksCR + 'Success.';
        L := strtointdef(VarToStr(varRecordAffected), 0);
        if (L >= 0) and ((m_objPreferences = nil) or ((m_objPreferences <> nil) and (m_objPreferences.StringVal[krsPREF_CONSOLESHOWDATASETROWCOUNT] <> krsFALSE))) then
          sResult := sResult + ksCR + Format('%d row%s affected.', [L, HasS(L)]);
        sResult := sResult + ksCR;
      end;

      if ((m_objPreferences = nil) or ((m_objPreferences <> nil) and (m_objPreferences.StringVal[krsPREF_CONSOLESHOWDATASETROWCOUNT] <> krsFALSE))) then
        SetDisplay(sResult);

      if m_sError <> ksEMPTY then
         SetDisplay(ksCR + TextToText(GetError))
      else
        result := TRUE;
      // Scroll!
      GotoLastLine;
    finally
      lst.free;
    end;
  end;

var
  objStmt: TcStatementList;
  sSQL, w: String;
  rs: TcRecordSet;
  ra: OLEVariant;
  L, i, j, k: longint;
  e: TeLogFlagSet;
begin
  result := TRUE;
  m_sError := ksEMPTY;
  objStmt := nil;
  m_bStop := FALSE;
  L := {$IFNDEF FACTORY}strtointdef(frmMain.Preferences.StringVal[krsPREF_FIELDWIDTH], kriSQLMAXCOLUMNSIZE){$ELSE}kriSQLMAXCOLUMNSIZE{$ENDIF};
  try
    //
    if [erAddToResult, erDisplayNoResult] * eparMode = [] then
      ClearDisplay;
    if Assigned(m_objSetButtons) then
      m_objSetButtons([]);
    objStmt := TcStatementList.Create(nil);
    try
      if ((pos(kcPARAGRAPH, value) > 0) and objStmt.Parse(estParagraph, value)) or objStmt.Parse(estSemicolon, value) then
      begin
        ra := null;
        for i := 0 to objStmt.Count - 1 do
        begin
          rs := nil;
          try
            Application.ProcessMessages;
            try
              sSQL := Unquote((objStmt[i] as TcStatement).SQL);
              w := WordByIndex(sSQL, 1, k);
              //
              // Progress?
              if Assigned(m_fctProgress) then
                with objStmt[i] as TcStatement do
                  m_fctProgress(rPos.iLineStart + m_iLineOffset, rPos.iLineEnd + m_iLineOffset, epmExecute);
              //
              // Echo Command ?
              if AnsiCompareText(w, krsCON_ECHO) = 0 then
              begin
                if (m_objDisplay is TSynEdit) then
                  (m_objDisplay as TSynEdit).Lines.Append(trim(system.copy(sSQL, k + 1, length(sSQL))));
              end
              //
              // Regular SQL Statement
              else if trim(sSQL) <> ksEMPTY then
              begin
                //
                // dbListView
                //
                if m_objDisplay is TdbListView then
                begin
                  if Assigned(m_objSetButtons) then
                    m_objSetButtons([ebsAnimate, ebsStop]);
                  // Either, visual display
                  if [erDisplayNoResult] * eparMode = [] then
                  begin
                    (m_objDisplay as TdbListView).IsReadOnly := m_bIsReadOnly;
                    (m_objDisplay as TdbListView).ClearColumns;
                    if (m_objDisplay as TdbListView).SQL = sSQL then
                      for j := 0 to m_lstWidth.Count - 1 do
                      begin
                        (m_objDisplay as TdbListView).ColumnCaption[j] := m_lstWidth[j];
                        (m_objDisplay as TdbListView).ColumnWidth[j] := longint(m_lstWidth.Objects[j]);
                      end
                    else
                      m_lstWidth.clear;
                    (m_objDisplay as TdbListView).SQL := sSQL;
                    for j := 0 to (m_objDisplay as TdbListView).Columns.Count - 1 do
                      FieldWidth[(m_objDisplay as TdbListView).ColumnCaption[j]] := (m_objDisplay as TdbListView).ColumnWidth[j];
                  end
                  else
                  // Or, non-visual display
                  begin
                    (m_objDisplay as TdbListView).Close;
                    if rs <> nil then
                    begin
                      rs.free;
                      rs := nil;
                    end;
                    m_eState := eesExecute;
                    if Assigned(m_objSetButtons) then
                      m_objSetButtons([ebsAnimate]);
                    rs := GetRecordSet(sSQL);
                  end;
                end
                //
                // Memo Field
                //
                else if not (m_objDisplay is TdbListView) then
                begin
                  // Display SQL Statement
                  if (frmMain.Preferences.StringVal[krsPREF_CONSOLEADDSQLSTATEMENTOUTPUT] = krsTRUE) and (m_objDisplay is TSynEdit) then
                    SetDisplay(ksCR + Format('[%s]', [sSQL]));
                  // Asynchronous?
                  if m_bIsAsynchronous then
                    m_objConnection.Connection.OnExecuteComplete := onExecuteComplete;
                  if rs <> nil then
                  begin
                    rs.free;
                    rs := nil;
                  end;
                  m_bExecuteComplete := FALSE;
                  m_eState := eesExecute;
                  if Assigned(m_objSetButtons) then
                    m_objSetButtons([ebsAnimate, ebsStop]);
                  rs := GetRecordSet(sSQL);
                  if m_bIsAsynchronous and (rs.eMode = ecmMDAC) then
                    while not m_bExecuteComplete and not m_bStop do
                    begin
                      Application.ProcessMessages;
                      //Sleep(20);
                    end;
                  // Statement was stopped
                  if m_bStop then
                    SetDisplay(ksCR + '([Execution] Stopped by User)')
                  else
                  begin
                    m_eState := eesFetch;
                    if [erDisplayNoResult] * eparMode = [] then
                      result := (rs <> nil) and DisplayRecordSet(rs, rs.RecordAffected, L) and result;
                  end;
                end;
                e := [elfValid];
                if (not (m_objDisplay is TdbListView) and (m_eState = eesFetch)) or ((m_objDisplay is TdbListView) and not (m_objDisplay as TdbListView).Stopped) then
                  e := e + [elfIncrement];
              end;
            except
              SetDisplay(TextToText(Format('[%s]\n%s\n', [sSQL, GetADOError])));
            end;
            if m_bStop then
              break;
          finally
            if (rs <> nil) and rs.IsOpen then
              rs.Close;
            rs.Free;
          end;
        end;
        if m_sError <> ksEMPTY then
          SetDisplay(ksCR + TextToText(GetError));
      end
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_OK);
    end;
    if Assigned(m_objSetButtons) then
      m_objSetButtons([ebsStart]);
  finally
    objStmt.free;
  end;
  m_bStop := TRUE;
  m_eState := eesIdle;
end;
{$ENDIF}

{$IFNDEF FACTORY}
// TcExecute
//   onExecuteComplete
//
procedure TcExecute.onExecuteComplete(Sender: TObject; RecordsAffected: Integer; var pError: OleVariant; var adStatus: OleVariant; var pCommand: OleVariant; var pRecordset: OleVariant; var pConnection: OleVariant);
begin
  m_bExecuteComplete := TRUE;
end;
{$ENDIF}

{$IFNDEF FACTORY}
// TcExecute
//   Execute (3)
//
function TcExecute.Execute(value: String; m_objStream: TcDataStream): boolean;
const
  kiBLOCKROW = 20;
  kiMAXFIELDS = 256;
var
  rs: TcRecordSet;
  i, pStart, pEnd, L: longint;
  lstTypes: array[0..kiMAXFIELDS] of TeDatatype;
  d: TDateTime;
begin
  result := FALSE;
  m_sError := ksEMPTY;
  m_bStop := FALSE;
  m_iRowCount := 0;
  value := Unquote(value);
  if value <> ksEMPTY then
  begin
    // make space for row count.
    pStart := m_objStream.Position;
    m_objStream.AsInteger := 0;
    m_iRowCount := 0;
    // Loop through query
    rs := nil;
    try
      d := now;
      try
        rs := Execute(value);
      except
        on E: Exception do
          SetOracleErrorException(E, value);
      end;
      m_objConnection.SetLog(value, now - d);
      if (rs <> nil) and rs.IsOpen then
      begin
        m_eState := eesFetch;
        if not rs.EOF then
        begin
          L := rs.FieldCount;
          m_objStream.AsInteger := L;
          // 1. Store field names
          for i := 0 to L - 1 do
          begin
            m_objStream.AsAnsiString := AnsiString(rs.FieldName(i));
            (*
            WOOPS

            m_objStream.AsInteger := rs.Fields[i].Attributes;
            m_objStream.AsInteger := rs.Fields[i].DefinedSize;
            m_objStream.AsInteger := rs.Fields[i].Type_;
            m_objStream.AsInteger := rs.Fields[i].Precision;
            m_objStream.AsInteger := rs.Fields[i].NumericScale;
            lstTypes[i] := ADOType2ShortType(rs.Fields[i].Type_, rs.Fields[i].Precision, rs.Fields[i].NumericScale);
            m_objStream.AsInteger := longint(lstTypes[i]);
            *)
          end;
          // 2. Store data
          while not rs.EOF do
          begin
            for i := 0 to L - 1 do
              m_objStream.AsTypedValue[lstTypes[i]] := rs.Fields(i);
            inc(m_iRowCount);
            rs.MoveNext;
          end;
        end;
        rs.Close;
        result := TRUE;
      end;
    finally
      rs.Free;
    end;
    // Set the row count at the start of the stream.
    pEnd := m_objStream.Position;
    m_objStream.Position := pStart;
    m_objStream.AsInteger := m_iRowCount;
    m_objStream.Position := pEnd;
  end;
  m_bStop := TRUE;
  m_eState := eesIdle;
end;
{$ENDIF}

// TcExecute
//   Execute (4)
//
function TcExecute.Execute(value: String): TcRecordSet;
var
  d: TDateTime;
  L: longint;
begin
  result := TcRecordSet.Create(nil);
  result.FetchType := m_eFetchType;
  m_sError := ksEMPTY;
  value := Unquote(value);
  if (m_objConnection <> nil) and m_objConnection.Connected and (value <> ksEMPTY) then
  begin
    result.eMode := m_objConnection.eMode;
    d := now;
    case result.eMode of
      //
      // ADO
      ecmMDAC:
        begin
          m_objConnection.adoConnection.Errors.Clear;
          result.m_objRecordSet := CreateCOMObject(CLASS_RecordSet) as RecordSet;
          result.m_objRecordSet.CursorLocation := adUseClient;
          L := 0;
          if m_bIsAsynchronous then
            L := adAsyncExecute;
          try
            case m_bIsReadOnly of
              FALSE:
                result.m_objRecordSet.Open(value, m_objConnection.adoConnection, adOpenDynamic, adLockOptimistic, L);
              TRUE:
                result.m_objRecordSet.Open(value, m_objConnection.adoConnection, adOpenStatic, adLockReadOnly, L);
            end;
            result.MoveFirst;
          except
            on E: Exception do
            begin
              SetOracleErrorException(E, Value);
              result.m_objRecordSet := nil;
            end;
          end;
          if result.m_objRecordSet <> nil then
            result.IsUpdatable := result.m_objRecordSet.Supports(adBookMark or adUpdate);
        end;
      //
      // Oracle
      ecmOracle:
        begin
          result.m_objOracleQuery := TOracleQuery.Create(nil);
          result.m_objOracleQuery.Session := m_objConnection.Session;
          result.m_objOracleQuery.SQL.Text := value;
          try
            result.m_objOracleQuery.Execute;
          except
            on E: EOracleError do
              SetOracleErrorException(E, value, result.m_objOracleQuery);
          end;
        end;
      end;
    m_objConnection.SetLog(value, now - d);
  end;
end;

// TcExecute
//   Execute (5)
//
function TcExecute.Execute(value, sSQLName: String): TcRecordSet;
var
  cmd: Command;
  v: OLEVariant;
  d: TDateTime;
begin
  result := TcRecordSet.Create(nil);
  result.FetchType := m_eFetchType;
  m_sError := ksEMPTY;
  value := Unquote(value);
  if (m_objConnection <> nil) and m_objConnection.Connected and (value <> ksEMPTY) then
  begin
    result.eMode := m_objConnection.eMode;
    case result.eMode of
      //
      // ADO
      ecmMDAC:
        begin
          cmd := nil;
          try
            m_objConnection.adoConnection.Errors.Clear;
            d := now;
            try
              cmd := CreateComObject(CLASS_Command) as Command;
              cmd.Set_ActiveConnection(m_objConnection.adoConnection);
              cmd.CommandType := adCmdText;
              cmd.Set_CommandText(Value);
              result.m_objRecordSet := cmd.Execute(v, EmptyParam, adCmdText);
              result.MoveFirst;
            except
              on E: Exception do
              begin
                SetOracleErrorException(E, Value);
                result := nil;
              end;
            end;
            m_objConnection.SetLog(sSQLName, value, now - d);
          finally
            cmd := nil;
          end;
        end;
      //
      // Oracle
      ecmOracle:
        begin
          d := now;
          result.m_objOracleQuery := TOracleQuery.Create(nil);
          result.m_objOracleQuery.Session := m_objConnection.Session;
          result.m_objOracleQuery.SQL.Text := Value;
          try
            result.m_objOracleQuery.Execute;
          except
            on E: EOracleError do
              SetOracleErrorException(E, Value, result.m_objOracleQuery);
          end;
          m_objConnection.SetLog(sSQLName, value, now - d);
        end;
    end;
  end;
end;

// TcExecute
//   OpenSchema
//
function TcExecute.OpenSchema(Schema: SchemaEnum; vparObject: OleVariant; sSQLName: String): TcRecordSet;
var
  d: TDateTime;
  v: OLEVariant;
begin
  result := TcRecordSet.Create(nil);
  result.FetchType := m_eFetchType;
  m_sError := ksEMPTY;
  if (m_objConnection <> nil) and m_objConnection.Connected then
  begin
    result.eMode := m_objConnection.eMode;
    case result.eMode of
      //
      // ADO
      ecmMDAC:
        begin
          m_objConnection.adoConnection.Errors.Clear;
          d := now;
          // function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): Recordset15; dispid 19;
          v := Unassigned;
          if vparObject <> ksEMPTY then
            v := vparObject;
          result.m_objRecordSet := m_objConnection.adoConnection.OpenSchema(Schema, VarArrayOf([Unassigned, Unassigned, v, Unassigned]), EmptyParam);
          if sSQLName = ksEMPTY then
            sSQLName := Format('%s %d', [krsOPENSCHEMA, longint(Schema)]);
          if VarToStr(vparObject) <> ksEMPTY then
            sSQLName := sSQLName + Format(' [$ID=''%s'']', [VarToStr(vparObject)]);
          m_objConnection.SetLog(sSQLName, now - d);
          result.MoveFirst;
        end;
    end;
  end;
end;

// TcExecute
//   ExecuteUndirect (1)
//
function TcExecute.ExecuteUndirect(value: String): boolean;
begin
  result := ExecuteUndirect(value, ksEMPTY);
end;

// TcExecute
//   ExecuteUndirect (2)
//
function TcExecute.ExecuteUndirect(value, sSQLName: String): boolean;
var
  rs: TcRecordSet;
begin
  rs := nil;
  try
    rs := Execute(value);
    if rs <> nil then
    begin
      while not rs.EOF do
      begin
        try
          Execute(sSQLName, VarToStr(rs.Fields(0)));
        except
          on E: Exception do
            SetOracleErrorException(E, value);
        end;
        rs.MoveNext;
      end;
      rs.Close;
    end;
    result := TRUE;
  finally
    rs.free;
  end;
end;

{$IFNDEF FACTORY}
// TcExecute
//   Import
//     Process stream to import data content
//
function TcExecute.Import(value: String; m_objStream: TcDataStream; strmLog: TMemoryStream; parTableDelimiter, parColumnDelimiter: String): TcImportResult;

  type
    TcFieldDefinition = record
      Name: String;
      Attribute: longint;
      DefinedSize: longint;
      Type_: DataTypeEnum;
      Precision: longint;
      Scale: longint;
      Datatype: TeDatatype
    end;

const
  kiMAXFIELD = 256;
var
  i, L, j: longint;
  sCols, sPrms, s, t, sSQL: String;
  cmd: TcCommand;
  lst: array[0 .. kiMAXFIELD - 1] of TcFieldDefinition;
  sErr: String;
begin
  m_sError := ksEMPTY;
  result.bSuccess := FALSE;
  if m_objStream <> nil then
  begin
    result.iRecImported := 0;
    m_objStream.Reset;
    m_objConnection.adoConnection.Errors.Clear;
    cmd := nil;
    try
      cmd := TcCommand.Create;
      cmd.Connection := m_objConnection;
      //
      // 1. Number of records
      result.iRecTotal := m_objStream.AsInteger; // Row count
      //
      // 2. Read table structure
      L := m_objStream.AsInteger; // Number of fields
      sCols := ksEMPTY;
      sPrms := ksEMPTY;
      for i := 0 to L - 1 do
      begin
        lst[i].Name := String(m_objStream.AsAnsiString);                      // Field Name
        lst[i].Attribute := m_objStream.AsInteger;                            // Field Attributes
        lst[i].DefinedSize := m_objStream.AsInteger;                          // Field DefinedSize;
        lst[i].Type_ := m_objStream.AsInteger;                                // Field Type_
        lst[i].Precision := m_objStream.AsInteger;                            // Field Precision;
        lst[i].Scale := m_objStream.AsInteger;                                // Field NumericScale;
        lst[i].Datatype := TeDatatype(m_objStream.AsInteger);                 // ADOType2ShortType(Type_)
        if lst[i].Datatype = etUndefined then
          lst[i].Datatype := ADOType2ShortType(lst[i].Type_, lst[i].Precision, lst[i].Scale);
        if sCols <> ksEMPTY then
        begin
          sCols := sCols + ', ';
          sPrms := sPrms + ', ';
        end;
        sCols := sCols + Format('%s%s%s', [Item(parColumnDelimiter, ',', 0), lst[i].Name, Item(parColumnDelimiter, ',', 1)]);
        sPrms := sPrms + Format(':p%d', [i + 1]);
      end;
      //
      // 3. Import data
      m_objConnection.BeginTrans;
      for i := 0 to result.iRecTotal - 1 do
      begin
        try
          // 3.2. Set query
          sSQL := Format('insert into %s (%s) values (%s)', [value, sCols, sPrms]);
          cmd.Text := sSQL;
          //
          for j := 0 to L - 1 do
          begin
            s := Format('p%d', [j + 1]);
            case lst[j].Datatype of
              etInteger:
                cmd.ParamAsInteger[s] := m_objStream.AsInteger;
              etAnsiString:
                cmd.ParamAsString[s] := String(m_objStream.AsAnsiString);
              etUnicodeString:
                cmd.ParamAsString[s] := m_objStream.AsUnicodeString;
              etLongString:
                cmd.ParamAsCLOB[s] := String(m_objStream.AsAnsiString);
              etDate:
                cmd.ParamAsDate[s] := m_objStream.AsDate;
              etBoolean:
                cmd.ParamAsBoolean[s] := m_objStream.AsBoolean;
              etFloat:
                cmd.ParamAsFloat[s] := m_objStream.AsFloat;
              etBinary:
                cmd.ParamAsBinary[s] := m_objStream.AsBinary;
            end;
          end;
          // 3.3 Execute!
          cmd.Execute;
          inc(result.iRecImported);
        except
          on E: Exception do
          begin
            sErr := ksEMPTY;
            for j := 0 to L - 1 do
            begin
              s := Format('p%d', [j + 1]);
              t := ksEMPTY;
              case lst[j].Datatype of
                etInteger:
                  t := inttostr(cmd.ParamAsInteger[s]);
                etAnsiString, etLongString, etUnicodeString {ParamAsCLOB}:
                  t := cmd.ParamAsString[s];
                etDate:
                  t := FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', cmd.ParamAsDate[s]);
                etBoolean:
                  t := kasBOOL[cmd.ParamAsBoolean[s]];
                etFloat:
                  t := Format('%f', [cmd.ParamAsFloat[s]]);
                etBinary:
                  t := '[binary]';
              end;
              sErr := sErr + Format('\n%s: %s', [s, t]);
            end;
            sErr := E.Message + ksCR + sSQL + ksCR + sErr;
            s := TextToText(Format('[%s]\n%s\n%s\n', [value, sErr, GetADOError]));
            if s <> ksEMPTY then
              strmLog.Write(s[1], length(s));
            result.bSuccess := FALSE;
          end;
        end;
        // 3.4 Commit point?
        if result.iRecImported mod m_iCommitPoint = 0 then
        begin
          m_objConnection.CommitTrans;
          m_objConnection.BeginTrans;
          if Assigned(m_hdlResult) then
            m_hdlResult(result.iRecImported);
        end;
      end;
      m_objConnection.CommitTrans;
    finally
      cmd.free;
    end;
    if Assigned(m_hdlResult) then
      m_hdlResult(result.iRecImported);
    // Done.
    with result do
      bSuccess := iRecImported = iRecTotal;
  end;
end;
{$ENDIF}

// TcExecute
//   GetError
//
function TcExecute.GetError: String;
begin
  result := m_sError;
  m_sError := ksEMPTY;
end;

// TcExecute
//   SetCommitPoint
//
procedure TcExecute.SetCommitPoint(value: longint);
begin
  if value <> kiUNDEFINED then
    m_iCommitPoint := value;
end;

// TcExecute
//   GetADOError
//
function TcExecute.GetADOError: String;
var
  i: longint;
  e: ADODB_TLB.Error;
begin
  result := ksEMPTY;
  if m_objConnection <> nil then
  begin
    for i := 0 to m_objConnection.adoConnection.Errors.Count - 1 do
    begin
      e := m_objConnection.adoConnection.Errors.Item[i];
      result := result + e.Description + ksCR;
    end;
    m_objConnection.adoConnection.Errors.Clear;
  end;
end;

// TcExecute
//   onStop
//
procedure TcExecute.onStop(Sender: TObject);
begin
  m_bStop := TRUE;
  if (m_eState = eesExecute) and m_bIsAsynchronous then
    (m_objConnection.Connection.DefaultInterface as _Connection).Cancel;
  Screen.Cursor := crDefault;
end;

// TcExecute
//   onPause
//
procedure TcExecute.onPause(Sender: TObject);
begin
  //
end;

// TcExecute
//   onRestart
//
procedure TcExecute.onRestart(Sender: TObject);
begin
  //
end;

// TcExecute
//   SetFieldWidth
//
procedure TcExecute.SetFieldWidth(Name: String; value: longint);
var
  L: longint;
begin
  L := m_lstWidth.IndexOf(Name);
  if L = kiUNDEFINED then
    L := m_lstWidth.Add(Name);
  m_lstWidth.Objects[L] := pointer(value);
end;

// TcExecute
//   SetError
//
procedure TcExecute.SetError(value: String);
begin
  m_sError := m_sError + value;
end;

// TcExecute
//   SetOracleErrorException (1)
//
procedure TcExecute.SetOracleErrorException(e: Exception; value: String);
begin
  SetError(trim(TextToText(Format('Error Executing [%s]\n%s', [value, e.Message]))));
end;

// TcExecute
//   SetOracleErrorException (2)
//
procedure TcExecute.SetOracleErrorException(e: EOracleError; value: String; parQuery: TOracleQuery);
var
  s: String;
begin
  SetOracleErrorException(e, value);
  if parQuery.ErrorLine <> 0 then
  begin
    s := GetLine(value, parQuery.ErrorLine);
    system.Insert('^', s, parQuery.ErrorPosition);
    SetError(TextToText(Format('\nLine %d, Column %d: [%s]', [parQuery.ErrorLine, parQuery.ErrorPosition, trim(s)])));
  end;
end;

// TcExecute
//   ThreadedExecute
//
function TcExecute.ThreadedExecute(value: String): TcRecordSet;
var
  p: TcExecuteThreaded;
begin
  result := nil;
  p := nil;
  try
    try
      p := TcExecuteThreaded.Create(True); { create suspended – secondprocess does not run yet }
      p.Initialize;
      p.Priority := tpNormal;
      p.objExecute := Self;
      p.SQL := value;
      p.Start; { now run the thread }
      while not p.bExecutionState do
      begin
        //Sleep(20);
        Application.ProcessMessages;
      end;
      result := p.m_objResult;
    except
      //
    end;
  finally
    p.Free;
  end;
end;

//
// TcCommand
//

// TcCommand
//   Create
//
constructor TcCommand.Create;
begin
  inherited Create;
  m_objConnection := nil;
  m_objCommand := nil;
  m_sSQL := ksEMPTY;
  m_objParameters := TcCollection.Create(nil);
  m_eType := eadoctSQL;
end;

// TcCommand
//   Destroy
//
destructor TcCommand.Destroy;
begin
  m_objCommand := nil;
  m_objParameters.free;
  m_objConnection := nil;
  inherited Destroy;
end;

// TcCommand
//   SetConnection
//
procedure TcCommand.SetConnection(const Value: TcConnection);
begin
  m_objConnection := value;
  if m_objCommand <> nil then
    m_objCommand := nil;
  m_objCommand := CreateCOMObject(CLASS_Command) as _Command;
  m_objCommand.Set_ActiveConnection(m_objConnection.adoConnection);
end;

// TcCommand
//   SetSQL
//
procedure TcCommand.SetSQL(Index: Integer; const Value: String);
var
  i:longint;
  s: String;
begin
  s := Value;
  for i := 1 to length(s) do
    if ord(s[i]) < 32 then
      s[i] := ' ';
  m_sSQL := s;
  m_objParameters.Clear;
  m_eType := TeCommandType(Index);
end;

// TcCommand
//   GetParameter
//
function TcCommand.GetParameter(const Name: String): TcCommandParameter;
begin
  result := m_objParameters.Find(uppercase(Name)) as TcCommandParameter;
end;

// TcCommand
//   SetParameter
//
procedure TcCommand.SetParameter(const Name: String; const iparType: longint; const Value: OLEVariant);
var
  p: TcCommandParameter;
begin
  p := GetParameter(Name);
  if p = nil then
  begin
    p := TcCommandParameter.Create(nil);
    p.sName := uppercase(Name);
    p.Direction := adParamInput;
    m_objParameters.Add(p);
  end;
  p.Datatype := iparType;
  p.Value := Value;
end;

// TcCommand
//   SetParamAsDate
//
procedure TcCommand.SetParamAsDate(const Name: String; const Value: TDateTime);
begin
  SetParameter(Name, adDate, Value);
end;

// TcCommand
//   SetParamAsString
//
procedure TcCommand.SetParamAsString(const Name: String; const Value: String);
begin
  SetParameter(Name, adBSTR, Value);
end;

// TcCommand
//   SetParamAsInteger
//
procedure TcCommand.SetParamAsInteger(const Name: String; const Value: longint);
begin
  SetParameter(Name, adInteger, Value);
end;

// TcCommand
//   SetParamAsCLOB
//
procedure TcCommand.SetParamAsCLOB(const Name: String; const Value: String);
begin
  SetParameter(Name, adLongVarChar, Value);
end;

// TcCommand
//   SetParamAsBoolean
//
procedure TcCommand.SetParamAsBoolean(const Name: String; const Value: boolean);
begin
  SetParameter(Name, adBoolean, Value);
end;

// TcCommand
//   SetParamAsFloat
//
procedure TcCommand.SetParamAsFloat(const Name: String; const Value: double);
begin
  SetParameter(Name, adDouble, Value);
end;

// TcCommand
//   SetParamAsBinary
//
procedure TcCommand.SetParamAsBinary(const Name: String; const Value: OLEVariant);
begin
  SetParameter(Name, adBinary, Value);
end;

// TcCommand
//   Execute
//
function TcCommand.Execute: TcRecordSet;
var
  i: longint;
(*
  cp: _Parameter;
  p: TcCommandParameter;
*)
begin
  result := nil;
  if m_objConnection.Connected then
  begin
    if m_objCommand.Parameters.Count > 0 then
    begin
      for i := m_objCommand.Parameters.Count - 1 downto 0 do
        m_objCommand.Parameters.Delete(i);
    end;
    case m_eType of
      //
      // SQL Statement:
      eadoctSQL:
      begin
        (*WOOPS
        m_objCommand.CommandType := adCmdText;
        m_objCommand.Set_CommandText(WideString(ParseSQL(m_sSQL))); // Parameters are also set here!
        try
          result := m_objCommand.Execute(v, EmptyParam, adCmdText);
        except
          on E: Exception do
          begin
            result := nil;
            raise;
          end;
        end;
        *)
      end;
      //
      // Package Statement:
      eadoctPackage:
        begin
          (*WOOPS
          m_objCommand.CommandType := adCmdStoredProc;
          m_objCommand.Set_CommandText(m_sSQL);
          for i := 0 to m_objParameters.Count - 1 do
          begin
            p := m_objParameters[i] as TcCommandParameter;
            cp := m_objCommand.CreateParameter(p.sName, p.Datatype, p.Direction, varDataSize(p.Value), p.Value);
            m_objCommand.Parameters.Append(cp);
          end;
          result := m_objCommand.Execute(v, EmptyParam, adCmdStoredProc);
          for i := 0 to m_objParameters.Count - 1 do
          begin
            p := m_objParameters[i] as TcCommandParameter;
            if p.Direction in [adParamOutput, adParamInputOutput] then
            begin
              cp := m_objCommand.Parameters.item[p.sName];
              if cp <> nil then
                p.Value := cp.Value;
            end;
          end;
          *)
        end;
    end;
  end;
end;

(*
// TcCommand
//   MakeParameter
//
procedure TcCommand.MakeParameter(const Name: String);
var
  cp: _Parameter;
  p: TcCommandParameter;
const
  BLOCK_SIZE = 1000;
begin
  p := m_objParameters.Find(uppercase(Name)) as TcCommandParameter;
  if p <> nil then
  begin
    if p.Datatype <> adLongVarChar then
    begin
      if (p.Datatype = adBSTR) and (varDataSize(p.Value) = 0) then
        cp := m_objCommand.CreateParameter(Name, p.Datatype, p.Direction, 0, null)
      else
        cp := m_objCommand.CreateParameter(Name, p.Datatype, p.Direction, varDataSize(p.Value), p.Value);
    end
    else
    begin
      if (varDataSize(p.Value) = 0) then
        cp := m_objCommand.CreateParameter(Name, adBSTR, p.Direction, 0, null)
      else
        cp := m_objCommand.CreateParameter(Name, adBSTR, p.Direction, varDataSize(p.Value), p.Value);
    end;
  end
  else
    cp := m_objCommand.CreateParameter(Name, adBSTR, adParamInput, 0, null);
  m_objCommand.Parameters.Append(cp);
end;
*)

(*
// TcCommand
//   ParseSQL
//
function TcCommand.ParseSQL(const SQL: string): string;
const
  Literals = ['''', '"', '`'];
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function NameDelimiter: Boolean;
  begin
    Result := CurChar in [' ', ',', ';', ')', #13, #10];
  end;

  function IsLiteral: Boolean;
  begin
    Result := CurChar in Literals;
  end;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar;
    begin
      if TempBuf^ in Literals then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] in Literals then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := ksEMPTY;
    try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  Result := SQL;
  Value := PChar(Result);

  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
    CurChar := CurPos^;
    if (CurChar = ':') and not Literal and ((CurPos + 1)^ <> ':') then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
        CurChar := CurPos^;
        if IsLiteral then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);

      MakeParameter(Name);

      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = ':') and not Literal and ((CurPos + 1)^ = ':') then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;
*)

// TcCommand
//   GetParameters
//
function TcCommand.GetParameters: Parameters;
begin
  result := nil;
  if m_objCommand <> nil then
    result := m_objCommand.Parameters;
end;

// TcCommand
//   SetParamDir
//
procedure TcCommand.SetParamDir(const Name: String; const Value: ParameterDirectionEnum);
var
  p: TcCommandParameter;
begin
  p := GetParameter(Name);
  if p <> nil then
    p.Direction := Value;
end;

// TcCommand
//   GetParamAsInteger
//
function TcCommand.GetParamAsInteger(const Name: String): longint;
var
  p: TcCommandParameter;
begin
  result := 0;
  p := GetParameter(Name);
  if p <> nil then
    result := strtointdef(VarToStr(p.Value), 0);
end;

// TcCommand
//   GetParamAsString
//
function TcCommand.GetParamAsString(const Name: String): String;
var
  p: TcCommandParameter;
begin
  p := GetParameter(Name);
  if p <> nil then
    result := VarToStr(p.Value);
end;

// TcCommand
//   GetParamAsDate
//
function TcCommand.GetParamAsDate(const Name: String): TDateTime;
var
  p: TcCommandParameter;
begin
  result := 0.0;
  p := GetParameter(Name);
  if p <> nil then
    result := VarToDateTime(p.Value);
end;

// TcCommand
//   GetParamAsBoolean
//
function TcCommand.GetParamAsBoolean(const Name: String): boolean;
var
  p: TcCommandParameter;
begin
  result := FALSE;
  p := GetParameter(Name);
  if p <> nil then
    result := p.Value = TRUE;
end;

// TcCommand
//   GetParamAsFloat
//
function TcCommand.GetParamAsFloat(const Name: String): double;
var
  p: TcCommandParameter;
  c: longint;
begin
  result := 0.0;
  p := GetParameter(Name);
  if p <> nil then
    val(VarToStr(p.Value), result, c);
end;

// TcCommand
//   GetParamAsBinary
//
function TcCommand.GetParamAsBinary(const Name: String): OLEVariant;
begin
  result := unassigned;
end;

// TcCommand
//   GetParamDir
//
function TcCommand.GetParamDir(const Name: String): ParameterDirectionEnum;
var
  p: TcCommandParameter;
begin
  result := adParamUnknown;
  p := GetParameter(Name);
  if p <> nil then
    result := p.Direction;
end;

//
// TcRecordSet
//

// TcRecordSet
//   Create
//
constructor TcRecordSet.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_eMode := ecmMDAC;
  m_objRecordSet := nil;
  m_objOracleQuery := nil;
  m_bIsUpdatable := FALSE;
  m_eFetchType := eftGetRows;
end;

// TcRecordSet
//   Destroy
//
destructor TcRecordSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

// TcRecordSet
//   Clear
//
procedure TcRecordSet.Clear;
begin
  m_eMode := ecmMDAC;
  m_objRecordSet := nil;
  m_objOracleQuery.Free;
  m_objOracleQuery := nil;
end;

// TcRecordSet
//   GetIsOpen
//
function TcRecordSet.GetIsOpen: boolean;
begin
  case m_eMode of
    ecmMDAC:
      result := (m_objRecordSet <> nil) and (m_objRecordSet.State = adStateOpen);
    ecmOracle:
      result := TRUE;
    else
      result := FALSE;
  end;
end;

// TcRecordSet
//   GetEOF
//
function TcRecordSet.GetEOF: boolean;
begin
  case m_eMode of
    ecmMDAC:
      result := m_objRecordSet.EOF and (vartype(m_objArray) in [varEmpty, varNull]);
    ecmOracle:
      result := m_objOracleQuery.EOF;
    else
      result := FALSE;
  end;
end;

// TcRecordSet
//   FieldCount
//
function TcRecordSet.FieldCount: longint;
begin
  case m_eMode of
    ecmMDAC:
      result := m_objRecordSet.Fields.Count;
    ecmOracle:
      result := m_objOracleQuery.FieldCount;
    else
      result := 0;
  end;
end;

// TcRecordSet
//   FieldName
//
function TcRecordSet.FieldName(Index: longint): String;
begin
  case m_eMode of
    ecmMDAC:
      result := m_objRecordSet.Fields[Index].Name;
    ecmOracle:
      result := m_objOracleQuery.FieldName(Index);
    else
      result := ksEMPTY;
  end;
end;

// TcRecordSet
//   FieldIndex
//
function TcRecordSet.FieldIndex(value: string): longint;
var
  i: longint;
begin
  result := kiUNDEFINED;
  case m_eMode of
    ecmMDAC:
      for i := 0 to m_objRecordSet.Fields.Count - 1 do
        if AnsiCompareText(m_objRecordSet.Fields[i].Name, value) = 0 then
        begin
          result := i;
          break;
        end;
    ecmOracle:
      result := m_objOracleQuery.FieldIndex(value)
  end;
end;

// TcRecordSet
//   MoveNext
//
procedure TcRecordSet.MoveNext;
var
  i: longint;
  aStr: array of string;
begin
  case m_eMode of
    ecmMDAC:
      begin
        m_objArray := null;
        if not m_objRecordSet.EOF then
          case m_eFetchType of
            eftGetRows:
              m_objArray := m_objRecordSet.GetRows(1, EmptyParam, EmptyParam);
            eftMoveNext:
            begin
              SetLength(aStr, m_objRecordSet.Fields.Count);
              for i := 0 to m_objRecordSet.Fields.Count - 1 do
                aStr[i] := FieldToString(m_objRecordSet.Fields[i]);
              m_objArray := aStr;
              aStr := nil;
              m_objRecordSet.MoveNext;
            end;
          end;
      end;
    ecmOracle:
      m_objOracleQuery.Next;
  end;
end;

// TcRecordset
//   MoveFirst
//
function TcRecordset.MoveFirst: boolean;
begin
  result := FALSE;
  try
    case m_eMode of
      ecmMDAC:
        begin
          result := GetIsOpen and not m_objRecordSet.EOF;
          if result then
            MoveNext;
        end;
      ecmOracle:
        result := TRUE;
    end;
  except
    result := FALSE;
  end;
end;

// TcRecordSet
//   Close
//
procedure TcRecordSet.Close;
begin
  case m_eMode of
    ecmMDAC:
      m_objRecordSet.Close;
    ecmOracle:
      m_objOracleQuery.Close;
  end;
end;

// TcRecordSet
//   Fields (1)
//
function TcRecordSet.Fields(Index: longint): Variant;
begin
  result := GetAsString(Index);
end;

// TcRecordSet
//   Fields (2)
//
function TcRecordSet.Fields(Index: String): Variant;
begin
  case m_eMode of
    ecmMDAC:
      result := GetFieldValue(Index);
    ecmOracle:
      result := m_objOracleQuery.Field(Index);
    else
      result := ksEMPTY;
  end;
end;

// TcRecordSet
//   GetAsString
//
function TcRecordSet.GetAsString(Index: longint): string;
begin
  case m_eMode of
    ecmMDAC:
      result := VarToStr(GetFieldValue(Index));
    ecmOracle:
      result := m_objOracleQuery.Field(Index);
    else
      result := ksEMPTY;
  end;
end;

// TcRecordSet
//   GetFieldValue (1)
//
function TcRecordSet.GetFieldValue(Index: longint): OLEVariant;
begin
  result := ksEMPTY;
  case m_eMode of
    ecmMDAC:
      if VarArrayDimCount(m_objArray) = 2 then
        result := VarArrayGet(m_objArray, [Index, 0])
      else if VarArrayDimCount(m_objArray) = 1 then
        result := VarArrayGet(m_objArray, [Index]);
    ecmOracle:
      result := m_objOracleQuery.Field(Index);
  end;
end;

// TcRecordSet
//   GetFieldValue (2)
//
function TcRecordSet.GetFieldValue(Index: String): OLEVariant;
var
  L: longint;
begin
  result := ksEMPTY;
  case m_eMode of
    ecmMDAC:
      begin
        L := FieldIndex(Index);
        if (L <> kiUNDEFINED) and (VarArrayDimCount(m_objArray) = 2) then
          result := VarArrayGet(m_objArray, [L, 0])
        else if (L <> kiUNDEFINED) and (VarArrayDimCount(m_objArray) = 1) then
          result := VarArrayGet(m_objArray, [L]);
      end;
    ecmOracle:
      result := m_objOracleQuery.Field(Index);
  end;
end;

// TcRecordSet
//   FieldIsNull (1)
//
function TcRecordSet.FieldIsNull(Index: longint): boolean;
begin
  result := VarIsNull(GetFieldValue(Index));
end;

// TcRecordSet
//   FieldIsNull (2)
//
function TcRecordSet.FieldIsNull(Index: String): boolean;
begin
  result := VarIsNull(GetFieldValue(Index));
end;

// TcRecordSet
//   HasField
//
function TcRecordSet.HasField(value: String): boolean;
begin
  case m_eMode of
    ecmMDAC:
      result := FieldIndex(value) <> kiUNDEFINED;
    ecmOracle:
      result := m_objOracleQuery.FieldIndex(value) <> kiUNDEFINED;
    else
      result := FALSE;
  end;
end;

// TcRecordSet
//   GetErrors
//
function TcRecordSet.GetErrors: String;
var
  i: longint;
  e: OLEVariant; // ADODB_TLB.Error;
begin
  case m_eMode of
    ecmMDAC:
    begin
      result := ksEMPTY;
      for i := 0 to m_objRecordset.Get_ActiveConnection.Errors.Count - 1 do
      begin
        e := m_objRecordset.Get_ActiveConnection.Errors.Item[i];
        result := result + e.Description + ksCR;
      end;
    end;
    ecmOracle:
      result := ksEMPTY;
    else
      result := ksEMPTY;
  end;
end;

// TcRecordSet
//   FieldSize
//
function TcRecordSet.FieldSize(value: longint): longint;

  // Tools
  //   GetTypeLength
  //
  function GetTypeLength(value: DataTypeEnum): longint;
  const
    kiaTYPES: array[1..38] of DataTypeEnum =
      (adEmpty, adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt,
       adUnsignedSmallInt, adUnsignedInt, adUnsignedBigInt, adSingle, adDouble,
       adCurrency, adDecimal, adNumeric, adBoolean, adError, adUserDefined,
       adVariant, adIDispatch, adIUnknown, adGUID, adDate, adDBDate, adDBTime,
       adDBTimeStamp, adBSTR, adChar, adVarChar, adLongVarChar, adWChar,
       adVarWChar, adLongVarWChar, adBinary, adVarBinary, adLongVarBinary,
       adChapter, adFileTime, adVarNumeric);
    kiaTYPELENGTHS: array[1..38] of longint =
      (0, 4, 8, 10, 10, 4,
       6, 6, 10, 10, 10,
       10, 10, 10, 5, -1, -1,
       -1, -1, -1, -1, 24, 24, 12,
       20, -1, -1, -1, -1, -1,
       -1, -1, -1, -1, -1,
       -1, -1, -1);
  var
    i: longint;
  begin
    result := kiUNDEFINED;
    for i := low(kiaTYPES) to high(kiaTYPES) do
      if value = kiaTYPES[i] then
      begin
        result := kiaTYPELENGTHS[i];
        break;
      end;
  end;

begin
  result := 0;
  case m_eMode of
    ecmMDAC:
      result := max(max(GetTypeLength(m_objRecordSet.Fields[value].Type_), m_objRecordSet.Fields[value].DefinedSize), length(m_objRecordSet.Fields[value].Name));
    ecmOracle:
      case m_objOracleQuery.FieldType(value) of
        otInteger:
          result := GetTypeLength(adInteger);
        otFloat:
          result := GetTypeLength(adDouble);
        otString:
          result := m_objOracleQuery.FieldSize(value);
        otDate, otTimeStamp:
          result := 20;
        otPLSQLString, otChar, otLongRaw, otCursor, otCLOB, otBLOB, otBFile, otReference, otObject, otSubst:
          result := m_objOracleQuery.FieldSize(value);
      end;
  end;
end;

// TcRecordSet
//   RecordAffected
//
function TcRecordSet.RecordAffected: longint;
begin
  result := kiUNDEFINED;
  case m_eMode of
    ecmMDAC:
      result := kiUNDEFINED;
    ecmOracle:
      result := m_objOracleQuery.RowsProcessed;
  end;
end;

// TcRecordSet
//   FieldType
//
function TcRecordSet.FieldType(Index: longint): TeDatatype;

  // Tool
  //   ADOType2ShortType
  //
  function ADOType2ShortType(value: DataTypeEnum; Precision, NumericScale: longint): TeDatatype;
  begin
    result := etUndefined;
    case value of
      adEmpty, adError, adUserDefined, adVariant, adIDispatch, adIUnknown, adGUID,
      adChapter, adFileTime, adPropVariant, adVarNumeric, adArray:
        result := etUndefined;
      adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt, adUnsignedSmallInt,
      adUnsignedInt, adUnsignedBigInt:
        result := etInteger;
      adSingle, adDouble, adCurrency:
        result := etFloat;
      adDecimal, adNumeric:
        if (Precision <= 10) and (NumericScale = 0) then
          result := etInteger
        else
          result := etFloat;
      adBoolean:
        result := etBoolean;
      adDate, adDBDate, adDBTime, adDBTimeStamp:
        result := etDate;
      adBSTR, adChar, adVarChar:
        result := etAnsiString;
      adLongVarChar, adWChar, adVarWChar, adLongVarWChar:
        result := etLongString;
      adBinary, adVarBinary, adLongVarBinary:
        result := etBinary;
    end;
  end;

begin
  result := etUndefined;
  case m_eMode of
    ecmMDAC:
      result := ADOType2ShortType(m_objRecordSet.Fields[Index].Type_, m_objRecordSet.Fields[Index].Precision, m_objRecordSet.Fields[Index].NumericScale);
    ecmOracle:
      case m_objOracleQuery.FieldType(Index) of
        otInteger:
          result := etInteger;
        otFloat:
          result := etFloat;
        otString, otChar, otPLSQLString:
          result := etAnsiString;
        otDate, otTimeStamp:
          result := etDate;
        otLongRaw, otBLOB:
          result := etBinary;
        otCLOB:
          result := etLongString;
        otCursor, otBFile, otReference, otObject, otSubst:
          result := etUndefined;
      end;
  end;
end;

// TcRecordSet
//   Move
//
procedure TcRecordSet.Move(NumRecords: Integer; Start: OleVariant);
begin
  case m_eMode of
    ecmMDAC:
      m_objRecordSet.Move(NumRecords, Start);
    ecmOracle:
      ; // m_objOracleQuery.MoveTo(NumRecords);
  end;
end;

// TcRecordSet
//   ReQuery
//
procedure TcRecordSet.ReQuery;
begin
  case m_eMode of
    ecmMDAC:
      begin
        m_objRecordSet.Requery(0);
        m_objRecordSet.MoveFirst;
      end;
    ecmOracle:
      begin
        m_objOracleQuery.Close;
        m_objOracleQuery.Execute;
      end;
  end;
end;

// TcRecordSet
//   GetRowCount
//
function TcRecordSet.GetRowCount: longint;
begin
  result := kiUNDEFINED;
  case m_eMode of
    ecmMDAC:
      result := kiUNDEFINED;
    ecmOracle:
      result := m_objOracleQuery.RowCount;
  end;
end;

// TcRecordSet
//   Update
//
function TcRecordSet.Update(sFieldName, sFieldValue: String): boolean;
begin
  result := FALSE;
  case m_eMode of
    ecmMDAC:
      begin
        try
          m_objRecordSet.Update(sFieldName, sFieldValue);
          result := TRUE;
        except
          on E:Exception do
            Application.MessageBox(PChar(e.Message), 'Error', MB_OK + MB_ICONEXCLAMATION);
        end;
      end;
    ecmOracle:
      result := FALSE;
  end;
end;

//
// TcExecuteThreaded
//

// TcExecuteThreaded
//   Create
//
procedure TcExecuteThreaded.Initialize;
begin
  m_bExecutionState := FALSE;
  m_objExecute := nil;
  m_sSQL := ksEMPTY;
  m_objResult := nil;
end;

// TcExecuteThreaded
//   Execute
//
procedure TcExecuteThreaded.Execute;
begin
  m_objResult := nil;
  try
    m_bExecutionState := FALSE;
    if m_objExecute <> nil then
    try
      m_objResult := m_objExecute.Execute(m_sSQL);
    except
      on E: Exception do
        m_objExecute.SetOracleErrorException(E, m_sSQL);
    end;
  finally
    m_bExecutionState := TRUE;
  end;
end;

end.
