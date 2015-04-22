unit StatementLib;

interface

uses
  daGlobals,
  Classes,         // TStringList
  Controls,
  ConnectionLib,   // TcConnection
  daObjectLib,     // TcObject
  SynEdit;         // TSynEdit

type
  TeParseType = (estSemicolon, estParagraph, estParsed);
  TcPartOption = (epoPrimaryKey, epoForeignKey, epoOrderDesc);
  TcPartOptionSet = set of TcPartOption;
  TcStatementOption = (esoOuterJoinWhere, esoOuterJoinFrom, esoLeftOuter, esoRightOuter);
  TcStatementOptionSet = set of TcStatementOption;

  TcStatement = class;
  TcStatementPart = class;
  TcLocation = class;
  TcStatementPartCondition = class;

  TrStmtPos = record
    SQL: String;
    iStart, iEnd: longint;
    iLineStart, iLineEnd: longint;
  end;

  TcCustomPart = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  *
  * Description: TcCustomPart is the base object for TcStatementPart and TcSchemaObject.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 03/03/03 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_sTable: String;
    m_sColumn: String;
    m_esOptions: TcPartOptionSet;
    m_ePartType: TePartType;

  private
    // Private members
    //
    //procedure   LocalClear;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    procedure   Clear; override;                                                // Clear the object
    procedure   Copy(value: TcObject); override;
    function    Compare(value: TcObject): boolean; override;
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Find(sparTable, sparColumn: String): TcObject; overload; virtual;
    function    Find(parType: TePartTypeSet; value: String): TcObject; overload; virtual;

  public
    // Public Properties
    //
    property    sTable: String                            read m_sTable         write m_sTable;
    property    sColumn: String                           read m_sColumn        write m_sColumn;
    property    ePartType: TePartType                     read m_ePartType      write m_ePartType;
    property    Options: TcPartOptionSet                  read m_esOptions      write m_esOptions;
  end;

  TcStatementList = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcStatementList is the streaming handler for providing and
  *              storing several SQL statements from one stream
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 03/12/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objQuery: TcObject;
    m_lstErrors: TStringList;
    m_iSQLCount: longint;

  private
    // Private members
    //
    procedure   SetError(value: String);
    function    GetError: String;
    procedure   LocalClear;
    function    GetSQLCount: longint;

  published
    // Published declarations
    //
    function    GetIsEmpty: boolean; override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    destructor  Destroy; override;                                              // Standard Destructor
    procedure   Clear; override;
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    StatementByPosition(value: longint): TcStatement;
    function    Parse(eparMode: TeParseType; value: String): boolean;
    function    GetCurrentStatement(eparType: TeParseType; value: TSynEdit): TrStmtPos;

  public
    // Public Properties
    //
    property    Error: String                             read GetError         write SetError;
    property    Query: TcObject                           read m_objQuery       write m_objQuery;
    property    SQLCount: longint                         read GetSQLCount;
  end;

  TcStatement = class(TcCustomPart)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcStatement is the streaming handler for providing several
  *              SQL statements from one stream
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 03/12/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_rPos: TrStmtPos;
    m_sTrace: String;
    m_bDistinct: boolean;
    m_eOptions: TcStatementOptionSet;
    m_datDuration: TDateTime;
    m_bIsValid: boolean;
    m_iSQLNumber: longint;

  private
    // Private members
    //
    function    GetSQL: String;
    procedure   SetSQL(value: String);
    function    GetPart(eparTypeSet: TePartTypeSet; Index: longint): TcStatementPart;
    function    GetPartCount(eparTypeSet: TePartTypeSet): longint;
    procedure   SetSchema(value: TcObject);
    function    GetOriginalSQL: String;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    destructor  Destroy; override;                                              // Standard Destructor
    procedure   Clear; override;
    procedure   Copy(value: TcObject); override;
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Add(parObject: TcObject): longint; override;
    function    Delete(parObject: TcObject): longint; override;
    function    Parse(eparMode: TeParseType; value: String): boolean;
    function    Find(eparPartType: TePartTypeSet; sparTableName, value: String): TcObject; overload; virtual;
    function    Edit(eparFormSet: TcObject): boolean;
    function    Build(eparFormSet: TcObject): boolean;
    function    Check: boolean;
    function    FindTableByAlias(value: String): TcStatementPart;
    function    AddCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: TcObject): TcStatementPartCondition; overload; virtual;
    function    AddCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: String): TcStatementPartCondition; overload; virtual;
    procedure   EstablishRelation(value: TcStatementPart);

  public
    // Public Properties
    //
    property    rPos: TrStmtPos                         read m_rPos             write m_rPos;
    property    SQL: String                             read GetSQL             write SetSQL;
    property    sTrace: String                          read m_sTrace           write m_sTrace;
    property    Part[eparTypeSet: TePartTypeSet; Index: longint]: TcStatementPart read GetPart;
    property    PartCount[eparTypeSet: TePartTypeSet]: longint read GetPartCount;
    property    bDistinct: boolean                      read m_bDistinct        write m_bDistinct;
    property    Schema: TcObject                                                write SetSchema;
    property    eOptions: TcStatementOptionSet          read m_eOptions         write m_eOptions;
    property    datDuration: TDateTime                  read m_datDuration      write m_datDuration;
    property    bIsValid: boolean                       read m_bIsValid         write m_bIsValid;
    property    OriginalSQL: String                     read GetOriginalSQL;
    property    iSQLNumber: longint                     read m_iSQLNumber       write m_iSQLNumber;
  end;

  TcStatementPart = class(TcCustomPart)
 {******************************************************************************
  * Author: Regis Charlot
  *
  *
  * Description: TcStatementPart is the Statement section object.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 01/21/03 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_sAlias: String;
    m_sPrefix: String;
    m_objPart: TcObject;
    m_iColumnWidth: longint;
    m_objControl: TControl;
    m_objLocation: TcLocation;
    m_sRenamedAs: String;
    m_ePartSide: TePartSide;

  private
    // Private members
    //
    // function    GetColumnIndex: longint;
    function    GetImageIndex: longint;
    procedure   LocalClear;
    function    GetPartIndex: longint;
    procedure   SetSchema(value: TcObject);

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;
    function    GetTag: longint; override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    destructor  Destroy; override;                                              // Standard Destructor
    procedure   Clear; override;                                                // Clear the object
    procedure   Copy(value: TcObject); override;
    function    Compare(value: TcObject): boolean; override;
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    Find(eparPartType: TePartTypeSet; sparTableName, value: String): TcObject; overload; virtual;

  public
    // Public Properties
    //
    property    sAlias: String                          read m_sAlias           write m_sAlias;
    property    sPrefix: String                         read m_sPrefix          write m_sPrefix;
    property    objPart: TcObject                       read m_objPart          write m_objPart;
    property    iColumnWidth: longint                   read m_iColumnWidth     write m_iColumnWidth;
    property    Control: TControl                       read m_objControl       write m_objControl;
    property    Location: TcLocation                    read m_objLocation      write m_objLocation;
    property    ImageIndex: longint                     read GetImageIndex;
    property    PartIndex: longint                      read GetPartIndex;
    property    sRenamedAs: String                      read m_sRenamedAs       write m_sRenamedAs;
    property    ePartSide: TePartSide                   read m_ePartSide        write m_ePartSide;
    property    Schema: TcObject                                                write SetSchema;
  end;

  TcStatementPartCondition = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcStatementPartCondition is the Statement Part Where Clause
  *              encapsulation.
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/08/03 Regis Created
  ******************************************************************************}
  private
    // Private members
    //
    m_eConditionOperator: TeConditionOperator;
    m_objLHS: TcStatementPart;
    m_objRHS: TcStatementPart;
    m_eRHSType: TeOperatorRHS;
    m_sRHS: String;
    m_objControl: TControl;
    m_iRHS, m_iLHS: longint;
    m_eOptions: TcStatementOptionSet;

  private
    // Private members
    //
    function    GetControl(Index: longint): TControl;
    procedure   SetSchema(value: TcObject);

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    destructor  Destroy; override;                                              // Standard Destructor
    //   Clear
    procedure   Copy(value: TcObject); override;
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    SetCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: TcObject): TcStatementPartCondition; overload; virtual;
    function    SetCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: String): TcStatementPartCondition; overload; virtual;

  public
    // Public Properties
    //
    property    eConditionOperator: TeConditionOperator read m_eConditionOperator write m_eConditionOperator;
    property    RHS: TcStatementPart                    read m_objRHS;
    property    LHS: TcStatementPart                    read m_objLHS;
    property    eRHSType: TeOperatorRHS                 read m_eRHSType         write m_eRHSType;
    property    sRHS: String                            read m_sRHS             write m_sRHS;
    property    Control: TControl                       read m_objControl       write m_objControl;
    property    Control_LHS: TControl index 0           read GetControl;
    property    Control_RHS: TControl index 1           read GetControl;
    property    iLHS: longint                           read m_iLHS             write m_iLHS;
    property    iRHS: longint                           read m_iRHS             write m_iRHS;
    property    Schema: TcObject                                                write SetSchema;
    property    eOptions: TcStatementOptionSet          read m_eOptions         write m_eOptions;
  end;

  TcLocation = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcLocation is the control position/handling class.
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/18/03 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_iTop: longint;
    m_iLeft: longint;
    m_iHeight: longint;
    m_iWidth: longint;

  published
    // Published declarations
    //
    function    GetIsEmpty: boolean; override;
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    //   Destroy
    procedure   Clear; override;
    procedure   Copy(value: TcObject); override;
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    procedure   SetToControl(value: TControl);
    procedure   GetFromControl(value: TControl);

  public
    // Public Properties
    //
    property    Top: longint                              read m_iTop           write m_iTop;
    property    Left: longint                             read m_iLeft          write m_iLeft;
    property    Height: longint                           read m_iHeight        write m_iHeight;
    property    Width: longint                            read m_iWidth         write m_iWidth;
  end;

  TcSchemaObject = class;

  TcSchema = class(TcCustomPart)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcSchema is the grammar object for applying any type of grammar.
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/09/02 Regis Created
  * 2/28/03  Regis Scraped Grammar Idea.
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objConnection: TcConnection;
    m_bInitialized: boolean;
    m_objMetaData: TcObject;

  private
    // Private declarations
    //
    procedure   SetConnection(value: TcConnection);
    procedure   SetMetaData(value: TcObject);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy
    procedure   Clear; override;                                                // Clear the object
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Initialize: boolean;

  public
    // Public Properties
    //
    property    Connection: TcConnection                  read m_objConnection  write SetConnection;
    property    MetaData: TcObject                        read m_objMetaData    write SetMetaData;
    property    Initialized: boolean                      read m_bInitialized;
  end;

  TcSchemaObject = class(TcCustomPart)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcSchemaObject is the base grammar object for macro (i.e.
  *              database object) storage.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/14/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_sSchema: String;
    m_objForeignKey: TcSchemaObject;

  private
    // Private members
    //
    function    GetImageIndex: longint;

  published
    // Published declarations
    //
    function    GetTag: longint; override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    //   Destroy
    procedure   Clear; override;                                                // Clear the object
    //   Copy
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom

  public
    // Public Properties
    //
    property    sSchema: String                           read m_sSchema        write m_sSchema;
    property    ForeignKey: TcSchemaObject                read m_objForeignKey  write m_objForeignKey;
    property    ImageIndex: longint                       read GetImageIndex;
  end;

implementation

uses
  ADODB_TLB,
  Forms,
  ComObj,
  Windows,
  Variants,
  DataLib,          // TcMetaData
  sysUtils,
  FormLib,          // TcFormSet
  daResourceStrings,
  daStreamLib,        // TcTokenStream
  ExecuteLib,       // TcExecute
  PanelLib,         // TvPanel
  frmStatementBuild,// TfrmStatementBuild
  frmTextEdit;      // TfrmTextEdit

//
// TcStatementList
//

// TcStatementList
//   Constructor
//
constructor TcStatementList.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objQuery := nil;
  m_lstErrors := TStringList.Create;
  m_iSQLCount := 0;
  LocalClear;
end;

// TcStatementList
//   Destructor
//
destructor TcStatementList.Destroy;
begin
  Clear;
  m_lstErrors.free;
  inherited Destroy;
end;

// TcStatementList
//   Clear
//
procedure TcStatementList.Clear;
begin
  inherited Clear;
  LocalClear;
end;

// TcStatementList
//   LocalClear
//
procedure TcStatementList.LocalClear;
begin
  m_lstErrors.Clear;
end;

// TcStatementList
//   Parse
//
function TcStatementList.Parse(eparMode: TeParseType; value: String): boolean;

  // Tool
  //   AddStatement
  //
  function AddStatement(var iPtrStart: longint; iPtrCur: longint; var iLineStart: longint; iLineCur: longint): TcStatement;
  var
    r: TrStmtPos;
  begin
    // Create result
    result := TcStatement.Create(self);
    Add(result);
    r.SQL := system.copy(value, iPtrStart + 1, iPtrCur - iPtrStart);
    r.iStart := iPtrStart;
    r.iEnd := iPtrCur - 1;
    r.iLineStart := iLineStart;
    r.iLineEnd := iLineCur;
    // Adjust start line.
    if r.iLineStart <> r.iLineEnd then
      while (r.SQL <> ksEMPTY) and not CharInSet(r.SQL[1], [#33 .. #126]) do
      begin
        inc(r.iStart);
        if (r.SQL[1] = kcLF) then
          inc(r.iLineStart);
        system.delete(r.SQL, 1, 1);
      end;
    // Adjust end line.
    (*
    L := length(r.SQL);
    if r.iLineStart <> r.iLineEnd then
      while (L >= 1) and not (r.SQL[L] in [#33 .. #126]) do
      begin
        dec(r.iEnd);
        if (r.SQL[L] = kcLF) then
          dec(r.iLineEnd);
        dec(L);
      end;
    *)
    // Assign Result
    result.rPos := r;
    // Adjust Ptr and Line Start
    iPtrStart := iPtrCur + 1;
    iLineStart := iLineCur;
  end;

var
  iPtrStart, iPtrCur, iLineStart, iLineCur: longint;
  c: char;
const
  kaeMAP: array[TeParseType] of TvKeyword =
    (_SEMICOLON, _PARAGRAPH, _UNDEFINED);
begin
  Clear;
  result := FALSE;
  sValue := value;
  if value <> ksEMPTY then
  begin
    iPtrStart := 0;
    iPtrCur := 0;
    iLineStart := 0;
    iLineCur := 0;
    while iPtrCur < length(value) do
    begin
      // semicolon?
      if value[iPtrCur + 1] = _TOKENS[kaeMAP[eparMode]] then
        AddStatement(iPtrStart, iPtrCur, iLineStart, iLineCur)
      // Single/Double Quote?
      else if CharInSet(value[iPtrCur + 1], [kcDBLQUOTE, kcSGLQUOTE]) then
      begin
        c := value[iPtrCur + 1];
        repeat
          inc(iPtrCur);
          if value[iPtrCur + 1] = kcLF then
            inc(iLineCur);
        until (value[iPtrCur + 1] = c) or (iPtrCur >= length(value));
      end
      // Linefeed?
      else if value[iPtrCur + 1] = kcLF then
        inc(iLineCur);
      //
      inc(iPtrCur);
    end;
    if iPtrStart <> iPtrCur then
      AddStatement(iPtrStart, iPtrCur, iLineStart, iLineCur);
    result := count <> 0;
  end;
end;

// TcStatementList
//   StatementByPosition
//
function TcStatementList.StatementByPosition(value: longint): TcStatement;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and
       (Objects[i] is TcStatement) and
       (value <= (Objects[i] as TcStatement).rPos.iEnd) then
    begin
      result := Objects[i] as TcStatement;
      break;
    end;
  if (result = nil) and (count > 0) then
    result := Objects[count - 1] as TcStatement;
end;

// TcStatementList
//   SetError
//
procedure TcStatementList.SetError(value: String);
begin
  m_lstErrors.Add(value);
end;

// TcStatementList
//   GetError
//
function TcStatementList.GetError: String;
begin
  result := m_lstErrors.Text;
end;

// TcStatementList
//   IsEmpty
//
function TcStatementList.GetIsEmpty: boolean;
begin
  result := (inherited GetIsEmpty) and (sValue <> ksEMPTY);
end;

// TcStatementList
//   GetSQLCount
//
function TcStatementList.GetSQLCount: longint;
begin
  inc(m_iSQLCount);
  result := m_iSQLCount;
end;

// TcStatementList
//   GetCurrentStatement
//
function TcStatementList.GetCurrentStatement(eparType: TeParseType; value: TSynEdit): TrStmtPos;
var
  q: TcStatement;
  i, L: longint;
  s: String;
begin
  result.SQL := ksEMPTY;
  result.iStart := 0;
  result.iEnd := 0;
  result.iLineStart := 0;
  result.iLineEnd := 0;
  //
  L := value.SelStart;
  if value.SelLength < 0 then
    inc(L, value.SelLength);
  if value.SelText <> ksEMPTY then
  begin
    result.SQL := value.SelText;
    s := value.Lines.Text;
    for i := 1 to L do
      if s[i] = kcLF then
        inc(result.iLineStart);
    result.iLineEnd := result.iLineStart;
    for i := 1 to length(result.SQL) do
      if result.SQL[i] = kcLF then
        inc(result.iLineEnd);
    result.iStart := value.SelStart;
    result.iEnd := value.SelStart + value.SelLength;
  end
  else
  try
    q := nil;
    if Parse(eparType, value.Lines.Text) then
      q := StatementByPosition(L - 1);
    if q <> nil then
      result := q.rPos;
  except
    //
  end;
end;

//
// TcStatement
//

// TcStatement
//   Constructor
//
constructor TcStatement.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  Clear;
end;

// TcStatement
//   Destructor
//
destructor TcStatement.Destroy;
begin
  Clear;
  inherited Destroy;
end;

// TcStatement
//   Clear
//
procedure TcStatement.Clear;
begin
  inherited Clear;
  m_rPos.iStart := kiUNDEFINED;
  m_rPos.iEnd := kiUNDEFINED;
  m_rPos.iLineStart := kiUNDEFINED;
  m_rPos.iLineEnd := kiUNDEFINED;
  m_rPos.SQL := ksEMPTY;
  m_sTrace := ksEMPTY;
  m_bDistinct := FALSE;
  m_eOptions := [];
  m_datDuration := 0;
  m_bIsValid := TRUE;
  m_iSQLNumber := kiUNDEFINED;
end;

// TcStatement
//   Copy
//
procedure TcStatement.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcStatement) then
  begin
    m_rPos          := (value as TcStatement).m_rPos;
    m_sTrace        := (value as TcStatement).m_sTrace;
    m_bDistinct     := (value as TcStatement).m_bDistinct;
    m_eOptions      := (value as TcStatement).m_eOptions;
    m_datDuration   := (value as TcStatement).m_datDuration;
    m_bIsValid      := (value as TcStatement).m_bIsValid;
    m_iSQLNumber    := (value as TcStatement).m_iSQLNumber;
  end;
end;

// TcStatement
//   Add
//
function TcStatement.Add(parObject: TcObject): longint;

  function GetAlias: String;
  var
    i, j, L: longint;
    b: boolean;
  begin
    L := 0;
    for j := 0 to count do
    begin
      b := FALSE;
      result := Format('t%d', [L]);
      for i := 0 to count - 1 do
        if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType in kesTABLESET) and ((Objects[i] as TcStatementPart).sAlias = result) then
          b := TRUE;
      if not b then
        break
      else
        inc(L);
    end;
  end;

var
  p: TcStatementPart;
begin
  if (parObject <> nil) and (parObject is TcSchemaObject) then
  begin
    p := TcStatementPart.Create(self);
    result := Add(p);
    p.Copy(parObject);
    if p.ePartType in kesCOLUMNSET then
      p.sPrefix := (parObject as TcSchemaObject).sTable;
    if p.ePartType in kesTABLESET then
      p.sAlias := GetAlias;
  end
  else
    result := inherited Add(parObject);
end;

// TcStatement
//   Check
//
function TcStatement.Check: boolean;
var
  i, j: longint;
  p: TcSchemaObject;
  r, q: TcStatementPart;
begin

  //
  // A. Check for '*' as a column
  //
  for i := Count - 1 downto 0 do
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) then
    begin
      r := Objects[i] as TcStatementPart;
      if (r.ePartType in kesCOLUMNSET) and ((pos(krsSTAR, r.sValue) <> 0) or (pos(krsSTAR, r.sName) <> 0)) then
      begin
        p := nil;
        if r.objPart = nil then
        begin
          q := FindTableByAlias(r.sPrefix);
          if (q <> nil) and (q.objPart <> nil) then
            p := q.objPart as TcSchemaObject;
        end
        else if (r.objPart <> nil) and (r.objPart is TcSchemaObject) then
          p := r.objPart as TcSchemaObject;
        if (p <> nil) and (p.ePartType in kesCOLUMNSET) then
          p := p.parent as TcSchemaObject;
        if (p <> nil) and (p.ePartType in kesTABLESET) then
        begin
          // a. Create new rows
          for j := p.count - 1 downto 0 do
            if p[j].sName <> krsSTAR then
            begin
              q := TcStatementPart.Create(self);
              q.Copy(p[j]);
              q.sPrefix := r.sPrefix;
              if i < Count - 1 then
                Insert(i, q)
              else
                Add(q);
            end;
          // b. Delete '*' column
          Delete(r);
        end;
      end;
    end;

  //
  // Done.
  //
  result := TRUE;
end;

// TcStatement
//   Delete
//
function TcStatement.Delete(parObject: TcObject): longint;
var
  p: TcObject;
begin
  result := kiUNDEFINED;
  if (parObject <> nil) and (parObject is TcSchemaObject) then
  begin
    p := Find([(parObject as TcSchemaObject).ePartType], (parObject as TcSchemaObject).sTable, parObject.sName);
    if p <> nil then
      result := Delete(p);
  end
  else
   result := inherited Delete(parObject);
end;

// TcStatement
//   GetSQL
//
function TcStatement.GetSQL: String;

  function IsOuterRHS(value: TcStatementPart): boolean;
  var
    i: longint;
  begin
    result := FALSE;
    for i := 0 to count - 1 do
      if (Objects[i] <> nil) and
         (Objects[i] is TcStatementPartCondition) and
         ((Objects[i] as TcStatementPartCondition).RHS.sTable = value.sTable) and
         ((Objects[i] as TcStatementPartCondition).RHS.sPrefix = value.sAlias) and
         ((Objects[i] as TcStatementPartCondition).eOptions * [esoLeftOuter, esoRightOuter] <> []) then
      begin
        result := TRUE;
        break;
      end;
  end;

  function GetTextByParts(ValueSet: TePartTypeSet): String;
  var
    i, j: longint;
  begin
    result := ksEMPTY;
    //
    // SQL Text Generation Oracle-Style
    // >> Outer joins
    if esoOuterJoinWhere in m_eOptions then
    begin
      for i := 0 to Count - 1 do
        if (Objects[i] <> nil) and
           (
             ((Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType in ValueSet)) or
             ((Objects[i] is TcStatementPartCondition) and (epWhere in ValueSet))
           ) then
        begin
          if result <> ksEMPTY then
          begin
            if Objects[i] is TcStatementPartCondition then
              result := result + ' and ' + ksCR
            else
              result := result + ', ' + ksCR;
          end;
          result := result + '  ' + Objects[i].Text;
          // Ordering
          if (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType = epOrder) then
          begin
            if epoOrderDesc in (Objects[i] as TcStatementPart).Options then
              result := result + ' desc'
            else
              result := result + ' asc';
          end;
        end;
    end
    else
    //
    // SQL Text Generation Ansi-Style
    // >> Outer joins
    begin
      for i := 0 to Count - 1 do
        if (Objects[i] <> nil) and
           (
             (
               (Objects[i] is TcStatementPart) and
               ((Objects[i] as TcStatementPart).ePartType in ValueSet)
             ) or
             (
               (Objects[i] is TcStatementPartCondition) and
               (epWhere in ValueSet) and
               ((Objects[i] as TcStatementPartCondition).eOptions * [esoLeftOuter, esoRightOuter] = [])
             )
           ) then
        begin
          if not // Weed out tables that are referenced as outer join tables
            (
              (Objects[i] is TcStatementPart) and
              ((Objects[i] as TcStatementPart).ePartType in kesTABLESET) and
              IsOuterRHS(Objects[i] as TcStatementPart)
            ) then
          begin
            if result <> ksEMPTY then
            begin
              if Objects[i] is TcStatementPartCondition then
                result := result + ' and ' + ksCR
              else
                result := result + ', ' + ksCR;
            end;
            result := result + '  ' + Objects[i].Text;
            // Ordering
            if (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType = epOrder) then
            begin
              if epoOrderDesc in (Objects[i] as TcStatementPart).Options then
                result := result + ' desc'
              else
                result := result + ' asc';
            end;
            //
            // Object is Table: check for Outer joins
            if (esoOuterJoinFrom in m_eOptions) and
               (Objects[i] is TcStatementPart) and
               ((Objects[i] as TcStatementPart).ePartType in kesTABLESET) then
              for j := 0 to Count - 1 do
                if (Objects[j] <> nil) and
                   (Objects[j] is TcStatementPartCondition) and
                   ([esoLeftOuter, esoRightOuter] * (Objects[j] as TcStatementPartCondition).eOptions <> []) and
                   (Objects[i].Text = Format('%s %s', [(Objects[j] as TcStatementPartCondition).LHS.sTable, (Objects[j] as TcStatementPartCondition).LHS.sPrefix])) then
                  result := result + Objects[j].Text;
          end;
        end;
      end;
  end;

begin
  result := ksEMPTY;
  if Count <> 0 then
  begin
    // Text Generation
    if (GetTextByParts(kesCOLUMNSET) <> ksEMPTY) and (GetTextByParts(kesTABLESET) <> ksEMPTY) then
    begin
      result := 'select';
      if m_bDistinct then
        result := result + ' distinct';
      result := result + ksCR + GetTextByParts(kesCOLUMNSET) + ksCR +
                'from ' + ksCR + GetTextByParts(kesTABLESET);
      if GetTextByParts([epWhere]) <> ksEMPTY then
        result := result + ksCR + 'where' + ksCR + GetTextByParts([epWhere]);
      if GetTextByParts([epOrder]) <> ksEMPTY then
        result := result + ksCR + 'order by' + ksCR + GetTextByParts([epOrder]);
    end;
  end
  else
    result := m_rPos.SQL;
end;

// TcStatement
//   GetSQL
//
procedure TcStatement.SetSQL(value: String);
begin
  Clear;
  m_rPos.SQL := value;
end;

// TcStatement
//   Parse
//
function TcStatement.Parse(eparMode: TeParseType; value: String): boolean;
var
  strm: TcTokenStream;
  s: String;
  L: longint;
begin
  result := FALSE;
  sValue := value;
  case eparMode of
    estParsed:
      ;
    estSemicolon:
    begin
      strm := nil;
      if value <> ksEMPTY then
      try
        strm := StringToStream(value);
        strm.SingleQuoteMarker := TRUE;
        while not strm.EOS do
        begin
          s := ksEMPTY;
          L := strm.iTokenStart;
          while not (not strm.Token.HasQuote and (strm.Token.ID = _SEMICOLON)) and not strm.EOS do
          begin
            if strm.Token.HasPreceedingSpace then
              s := s + ' ';
            if strm.Token.HasQuote then
              s := s + strm.Token.QuoteSymbol + strm.Token.Value + strm.Token.QuoteSymbol
            else
              s := s + strm.Token.Value;
            strm.Match(_WILDCARD);
          end;
          if not strm.EOS and (strm.Token.ID = _SEMICOLON) then
            strm.Match(_SEMICOLON); // _SEMICOLON
          result := trim(s) <> ksEMPTY;
          if result then
          begin
            m_rPos.iStart := L;
            m_rPos.iEnd := strm.Position;
            m_rPos.SQL := trim(s);
          end;
        end;
      finally
        strm.Free;
      end;
    end;
  end;
end;

// TcStatement
//   Find
//
function TcStatement.Find(eparPartType: TePartTypeSet; sparTableName, Value: String): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) then
    begin
      result := (Objects[i] as TcStatementPart).Find(eparPartType, sparTableName, Value);
      if result <> nil then
        break;
    end;
  end;
end;

// TcStatement
//   Edit
//
function TcStatement.Edit(eparFormSet: TcObject): boolean;
var
  s: String;
begin
  s := GetSQL;
  result := EditText(eparFormSet as TcFormSet, s);
  if result then
    SetSQL(s);
end;

// TcStatement
//   Build
//
function TcStatement.Build(eparFormSet: TcObject): boolean;
var
  frm: TfrmStatementBuild;
begin
  frm := nil;
  try
    frm := TfrmStatementBuild.Create(Application.MainForm.ActiveMDIChild);
    frm.FormSet := eparFormSet as TcFormSet;
    frm.Statement := self;
    result := frm.ShowModal = mrOK;
  finally
    frm.release;
  end;
end;

// TcStatement
//   FindTableByAlias
//
function TcStatement.FindTableByAlias(value: String): TcStatementPart;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType in kesTABLESET) and ((Objects[i] as TcStatementPart).sAlias = value) then
    begin
      result := Objects[i] as TcStatementPart;
      break;
    end;
end;

// TcStatement
//   GetPart
//
function TcStatement.GetPart(eparTypeSet: TePartTypeSet; Index: longint): TcStatementPart;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType in eparTypeSet) then
    begin
      dec(Index);
      if Index < 0 then
      begin
        result := Objects[i] as TcStatementPart;
        break;
      end;
    end;
end;

// TcStatement
//   GetPart
//
function TcStatement.GetPartCount(eparTypeSet: TePartTypeSet): longint;
var
  i: longint;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) and ((Objects[i] as TcStatementPart).ePartType in eparTypeSet) then
      inc(result);
end;

// TcStatement
//   AddCondition (1)
//
function TcStatement.AddCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: TcObject): TcStatementPartCondition;
begin
  result := TcStatementPartCondition.Create(self);
  Add(result);
  result.SetCondition(parLHS, parOperator, parRHS);
end;

// TcStatement
//   AddCondition (2)
//
function TcStatement.AddCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: String): TcStatementPartCondition;
begin
  result := TcStatementPartCondition.Create(self);
  Add(result);
  result.SetCondition(parLHS, parOperator, parRHS);
end;

// TcStatement
//   EstablishRelation
//
procedure TcStatement.EstablishRelation(value: TcStatementPart);
var
  i, j: longint;
  s: TcSchemaObject;
  q, r: TcObject;
  c: TcStatementPartCondition;
begin
  if (value <> nil) and (value.ePartType in kesTABLESET) and (value.objPart <> nil) and (value.objPart is TcSchemaObject) then
  begin
    //
    // From the current object, check foreign keys.
    //
    s := value.objPart as TcSchemaObject;
    for j := 0 to s.count - 1 do
      if (s[j] <> nil) and
         (s[j] is TcSchemaObject) and
         ((s[j] as TcSchemaObject).ePartType in kesCOLUMNSET) and
         ((s[j] as TcSchemaObject).ForeignKey <> nil) then
      begin
        q := (s[j] as TcSchemaObject).ForeignKey;
        r := nil;
        if (q <> nil) and (q is TcSchemaObject) then
          r := Find((q as TcSchemaObject).sTable, (q as TcSchemaObject).sColumn);
        if (r <> nil) and
           (r.Parent <> nil) and
           (r.Parent is TCStatementPart) and
           ((r.Parent as TCStatementPart).sAlias <> value.sAlias) then
        begin
          c := AddCondition(s[j], ecoEqual, r);
          c.LHS.sPrefix := value.sAlias;
          c.LHS.sTable := value.sTable;
          c.iLHS := j;
          c.RHS.sPrefix := (r.Parent as TCStatementPart).sAlias;
          c.RHS.sTable := (r.Parent as TCStatementPart).sTable;
          c.RHS.ePartType := epColumn;
          c.iRHS := r.ParentIndex;
          break;
        end;
      end;
    //
    // From all other objects, check for this object's primary key.
    //
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) and (Objects[i] <> value) then
      begin
        s := (Objects[i] as TcStatementPart).objPart as TcSchemaObject;
        for j := 0 to s.count - 1 do
          if (s[j] <> nil) and
             (s[j] is TcSchemaObject) and
             ((s[j] as TcSchemaObject).ePartType in kesCOLUMNSET) and
             ((s[j] as TcSchemaObject).ForeignKey <> nil) then
          begin
            q := (s[j] as TcSchemaObject).ForeignKey;
            r := nil;
            if (q <> nil) and (q is TcSchemaObject) and ((q as TcSchemaObject).sTable = value.sTable) then
              r := value.Find(kesCOLUMNSET, (q as TcSchemaObject).sColumn);
            if (r <> nil) and
               (r.Parent <> nil) and
               (r.Parent is TCStatementPart) and
               ((Objects[i] as TcStatementPart).sAlias <> value.sAlias) then
            begin
              c := AddCondition(s[j], ecoEqual, r);
              c.LHS.sPrefix := (Objects[i] as TcStatementPart).sAlias;
              c.LHS.sTable := s.sTable;
              c.iLHS := j;
              c.RHS.sPrefix := value.sAlias;
              c.RHS.sTable := value.sTable;
              c.RHS.ePartType := epColumn;
              c.iRHS := r.ParentIndex;
              break;
            end;
          end;
      end;
  end;
end;

// TcStatement
//   GetXML
//
function TcStatement.GetXML: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) then
      result := result + Objects[i].XML;
  if result <> ksEMPTY then
    result := Format('<%s %s="%s">', [krsXML_STATEMENT, krsXML_DISTINCT, kasBOOL[m_bDistinct]]) + result + Format('</%s>', [krsXML_STATEMENT]);
end;

// TcStatement
//   SetXML
//
procedure TcStatement.SetXML(value: String);
var
  p, xPE, t: OLEVariant;
  q: TcObject;
  i: longint;
begin
  Clear;
  if trim(value) <> ksEMPTY then
  try
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) and (p.childNodes.Length = 1) then
      begin
        t := p.childNodes.Item[0];
        // Distinct
        m_bDistinct := GetXMLAttribute(t, krsXML_DISTINCT) = krsTRUE;
        //
        // Statement Part Elements
        for i := 0 to t.childNodes.Length - 1 do
        begin
          q := nil;
          if AnsiCompareText(t.childNodes.item[i].nodeName, krsXML_STATEMENTPART) = 0 then
            q := TcStatementPart.Create(self)
          else if AnsiCompareText(t.childNodes.item[i].nodeName, krsXML_STATEMENTPARTCONDITION) = 0 then
            q := TcStatementPartCondition.Create(self);
          if q <> nil then
          begin
            add(q);
            q.XML := t.childNodes.item[i].XML;
          end;
        end;
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create('''Statement'' XML document failed to load.' + ksCR + Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR + trim(xPE.srcText));
      end;
    except
      on E: Exception do
      begin
        Clear;
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
      end;
    end;
  finally
    p := unassigned;
  end;
end;

// TcStatement
//   SetSchema
//
procedure TcStatement.SetSchema(value: TcObject);
var
  i: longint;
begin
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcStatementPart) then
      (Objects[i] as TcStatementPart).Schema := value
    else if (Objects[i] <> nil) and (Objects[i] is TcStatementPartCondition) then
      (Objects[i] as TcStatementPartCondition).Schema := value;
end;

// TcStatement
//   GetOriginalSQL
//
function TcStatement.GetOriginalSQL: String;
begin
  result := ksEMPTY;
  if parent <> nil then
    result := system.Copy(parent.sValue, m_rPos.iStart + 1, m_rPos.iEnd - m_rPos.iStart);
end;

//
// TcStatementPart
//

// TcStatementPart
//   Constructor
//
constructor TcStatementPart.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objLocation := TcLocation.Create(self);
  LocalClear;
end;

// TcStatementPart
//   Destroy
//
destructor TcStatementPart.Destroy;
begin
  if (m_objControl <> nil) and (m_objControl is TvPanel) then
    (m_objControl as TvPanel).Data := nil;
  m_objLocation.free;
  m_objLocation := nil;
  inherited Destroy;
end;

// TcStatementPart
//   Clear
//
procedure TcStatementPart.Clear;
begin
  inherited Clear;
  LocalClear;
end;

// TcStatementPart
//   LocalClear
//
procedure TcStatementPart.LocalClear;
begin
  m_sAlias := ksEMPTY;
  m_sPrefix := ksEMPTY;
  m_objPart := nil;
  m_iColumnWidth := kiUNDEFINED;
  m_objControl := nil;
  m_objLocation.Clear;
  m_sRenamedAs := ksEMPTY;
  m_ePartSide := epsNone;
end;

// TcStatementPart
//   Copy
//
procedure TcStatementPart.Copy(value: TcObject);
begin
  inherited Copy(value);
  iColumnWidth := kiUNDEFINED;
  if (value <> nil) and (value is TcStatementPart) then
  begin
    m_sAlias       := (value as TcStatementPart).sAlias;
    m_sPrefix      := (value as TcStatementPart).sPrefix;
    m_objPart      := (value as TcStatementPart).objPart;
    m_iColumnWidth := (value as TcStatementPart).iColumnWidth;
    m_objLocation.Copy((value as TcStatementPart).Location);
    m_sRenamedAs   := (value as TcStatementPart).m_sRenamedAs;
    m_ePartSide    := (value as TcStatementPart).m_ePartSide;
    m_objControl   := nil; // Important!
  end
  else if (value <> nil) and (value is TcSchemaObject) then
    m_objPart := value;
end;

// TcStatementPart
//   Compare method
//
function TcStatementPart.Compare(value: TcObject): boolean;
begin
  result := inherited Compare(value);
  if result and (value <> nil) and (value is TcStatementPart) then
    result := result and
              (m_sAlias     = (value as TcStatementPart).sAlias) and
              (m_sPrefix    = (value as TcStatementPart).m_sPrefix) and
              (m_sRenamedAs = (value as TcStatementPart).m_sRenamedAs);
end;

// TcStatementPart
//   Text
//
function TcStatementPart.Text: String;
begin
  result := m_sRenamedAs;
  if result = ksEMPTY then
  begin
    if m_sPrefix <> ksEMPTY then
      result := result + m_sPrefix + '.';
    if pos(' ', sName) <> 0 then
      result := result + '"' + sName + '"'
    else
      result := result + sName;
    if sAlias <> ksEMPTY then
    begin
      if m_ePartType in kesCOLUMNSET then
        result := result + ' as';
      result := result + ' ' + sAlias;
    end;
  end;
end;

// TcStatementPart
//   GetImageIndex
//
function TcStatementPart.GetImageIndex: longint;
begin
  result := kiUNDEFINED;
  case ePartType of
    epColumn:
      begin
        if epoForeignKey in m_esOptions then
          result := 0;
        if epoPrimaryKey in m_esOptions then
          result := 1;
      end;
    epOrder:
      result := longint(epoOrderDesc in m_esOptions);
  end;
end;

// TcStatementPart
//   GetPartIndex
//
function TcStatementPart.GetPartIndex: longint;
var
  i: longint;
begin
  result := kiUNDEFINED;
  if parent <> nil then
    for i := 0 to parent.count - 1 do
    begin
      if (parent[i] <> nil) and (parent[i] is TcStatementPart) and ((parent[i] as TcStatementPart).ePartType = m_ePartType) then
        inc(result);
      if parent[i] = self then
        break;
    end;
end;

// TcStatementPart
//   GetXML
//
function TcStatementPart.GetXML: String;
begin
  result := Format('<%s', [krsXML_STATEMENTPART]);
  if m_ePartSide <> epsNone then
    result := result + Format(' %s="%d"', [krsXML_PARTSIDE, longint(m_ePartSide)]);
  if sName <> ksEMPTY then
    result := result + Format(' %s="%s"', [krsNAME, sName]);
  result := result + Format(' %s="%d"', [krsXML_PARTTYPE, longint(m_ePartType)]);
  if m_sAlias <> ksEMPTY then
    result := result + Format(' %s="%s"', [krsXML_ALIAS, m_sAlias]);
  if m_sPrefix <> ksEMPTY then
    result := result + Format(' %s="%s"', [krsXML_PREFIX, m_sPrefix]);
  if m_iColumnWidth > 0 then
    result := result + Format(' %s="%d"', [krsXML_COLUMNWIDTH, m_iColumnWidth]);
  if m_sRenamedAs <> ksEMPTY then
    result := result + Format(' %s="%s"', [krsXML_RENAMEDAS, m_sRenamedAs]);
  if (m_objLocation.Top <> 0) or (m_objLocation.Left <> 0) or (m_objLocation.Height <> 0) or (m_objLocation.Width <> 0) then
    result := result + format(' %s="%d" %s="%d" %s="%d" %s="%d"', [krsTOP, m_objLocation.Top, krsLEFT, m_objLocation.Left, krsHEIGHT, m_objLocation.Height, krsWIDTH, m_objLocation.Width]);
  result := result + Format(' %s="%s" %s="%s"/>', [krsXML_SCHEMATABLE, sTable, krsXML_SCHEMACOLUMN, sColumn]);
end;

// TcStatementPart
//   SetXML
//
procedure TcStatementPart.SetXML(value: String);
var
  p, xPE, q: OLEVariant;
begin
  Clear;
  if trim(value) <> ksEMPTY then
  try
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) and (p.childNodes.length = 1) then
      begin
        q := p.childNodes.item[0];
        //
        // Attributes
        // Name
        sName := GetXMLAttribute(q, krsNAME);
        // Part Side
        m_ePartSide := TePartSide(strtointdef(GetXMLAttribute(q, krsXML_PARTSIDE), 0));
        // Alias
        m_sAlias := GetXMLAttribute(q, krsXML_ALIAS);
        // Part Type
        ePartType := TePartType(strtointdef(GetXMLAttribute(q, krsXML_PARTTYPE), 0));
        // Prefix
        m_sPrefix := GetXMLAttribute(q, krsXML_PREFIX);
        // ColumnWidth
        m_iColumnWidth := strtointdef(GetXMLAttribute(q, krsXML_COLUMNWIDTH), 0);
        // RenamedAs
        m_sRenamedAs := GetXMLAttribute(q, krsXML_RENAMEDAS);
        // Location
        m_objLocation.Top := strtointdef(GetXMLAttribute(q, krsTOP), 0);
        m_objLocation.Left := strtointdef(GetXMLAttribute(q, krsLEFT), 0);
        m_objLocation.Width := strtointdef(GetXMLAttribute(q, krsWIDTH), 0);
        m_objLocation.Height := strtointdef(GetXMLAttribute(q, krsHEIGHT), 0);
        // Schema Table & Column
        sTable := GetXMLAttribute(q, krsXML_SCHEMATABLE);
        sColumn := GetXMLAttribute(q, krsXML_SCHEMACOLUMN);
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create('''Statement Part'' XML document failed to load.' + ksCR + Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR + trim(xPE.srcText));
      end;
    except
      on E: Exception do
      begin
        Clear;
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
      end;
    end;
  finally
    p := unassigned;
  end;
end;

// TcStatementPart
//   SetSchema
//
procedure TcStatementPart.SetSchema(value: TcObject);
begin
  m_objPart := nil;
  if (sTable <> ksEMPTY) and (value <> nil) and (value is TcCustomPart) then
    m_objPart := (value as TcCustomPart).Find(sTable, sColumn);
end;

// TcStatementPart
//   Find
//
function TcStatementPart.Find(eparPartType: TePartTypeSet; sparTableName, Value: String): TcObject;
var
  i: longint;
begin
  result := nil;
  if (m_ePartType in eparPartType) and (sTable = sparTableName) and (sName = value) then
    result := self
  else if (sTable = sparTableName) then
    for i := 0 to count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcSchemaObject) and (Objects[i].sName = Value) then
      begin
        result := Objects[i];
        break;
      end;
end;

// TcStatementPart
//   GetTag
//
function TcStatementPart.GetTag: longint;
begin
  result := GetImageIndex;
end;

//
// TcStatementPartCondition
//

// TcStatementPartCondition
//   Create
//
constructor TcStatementPartCondition.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objLHS := TcStatementPart.Create(self);
  m_objLHS.ePartSide := epsLHS;
  m_objRHS := TcStatementPart.Create(self);
  m_objRHS.ePartSide := epsRHS;
  m_eConditionOperator := ecoEqual;
  m_eRHSType := eorColumn;
  m_objControl := nil;
  m_iRHS := kiUNDEFINED;
  m_iLHS := kiUNDEFINED;
  m_eOptions := [];
  if (parParent <> nil) and (parParent is TcStatement) then
    m_eOptions := (parParent as TcStatement).eOptions;
end;

// TcStatementPartCondition
//   Destroy
//
destructor TcStatementPartCondition.Destroy;
begin
  m_objLHS.free;
  m_objLHS := nil;
  m_objRHS.free;
  m_objRHS := nil;
  inherited Destroy;
end;

// TcStatementPartCondition
//   Copy
//
procedure TcStatementPartCondition.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcStatementPartCondition) then
  begin
    m_eConditionOperator := (value as TcStatementPartCondition).eConditionOperator;
    m_eRHSType           := (value as TcStatementPartCondition).eRHSType;
    m_sRHS               := (value as TcStatementPartCondition).sRHS;
    m_iRHS               := (value as TcStatementPartCondition).iRHS;
    m_iLHS               := (value as TcStatementPartCondition).iLHS;
    m_eOptions           := (value as TcStatementPartCondition).eOptions;
    m_objLHS.Copy((value as TcStatementPartCondition).m_objLHS);
    m_objRHS.Copy((value as TcStatementPartCondition).m_objRHS);
  end;
end;

// TcStatementPartCondition
//   Text
//
function TcStatementPartCondition.Text: String;
begin
  //
  // Where clause, for either regular where clause, or Oracle-type outer join clause
  if (((esoLeftOuter in m_eOptions) or (esoRightOuter in m_eOptions)) and (esoOuterJoinWhere in m_eOptions)) or
     (not (esoLeftOuter in m_eOptions) and not (esoRightOuter in m_eOptions)) then
  begin
    result := m_objLHS.Text;
    if [esoLeftOuter, esoOuterJoinWhere] * m_eOptions = [esoLeftOuter, esoOuterJoinWhere] then
      result := result + ' (+)';
    result := result + format(' %S ', [kasCONDITIONOPERATORS[m_eConditionOperator]]);
    case m_eRHSType of
      eorColumn:
      begin
        result := result + m_objRHS.Text;
        if [esoRightOuter, esoOuterJoinWhere] * m_eOptions = [esoRightOuter, esoOuterJoinWhere] then
          result := result + ' (+)';
      end;
      eorValue:
        if not (m_eConditionOperator in [ecoIsNull, ecoIsNotNull]) then
        begin
          if IsNumber(m_sRHS) or IsReal(m_sRHS) then
            result := result + m_sRHS
          else
            result := result + '''' + m_sRHS + '''';
        end;
    end;
  end
  else
  //
  // outer join Where clause for Ansi type join
  begin
    result := ksEMPTY;
    if esoLeftOuter in m_eOptions then
      result := result + ' left outer'
    else if esoRightOuter in m_eOptions then
      result := result + ' right outer';
    result := result + ' join ' + Format('%s %s', [m_objRHS.sTable, m_objRHS.sPrefix]) + ' on ' + m_objLHS.Text + ' = ' + m_objRHS.Text;
  end;
end;

// TcStatementPartCondition
//   GetControl
//
function TcStatementPartCondition.GetControl(Index: longint): TControl;
var
  p: TcStatementPart;
begin
  result := nil;
  p := nil;
  case Index of
    0: // LHS
      p := m_objLHS;
    1: // RHS
      if m_eRHSType = eorColumn then
        p := m_objRHS;
  end;
  if (p <> nil) and (p.ePartType in kesCOLUMNSET) and (p.TopParent <> nil) and (p.TopParent is TcStatement) then
    p := (p.TopParent as TcStatement).FindTableByAlias(p.sPrefix);
  if p <> nil then
    result := p.Control;
end;

// TcStatementPartCondition
//   SetCondition (1)
//
function TcStatementPartCondition.SetCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: TcObject): TcStatementPartCondition;
begin
  // Left Hand Site
  m_objLHS.Clear;
  if parLHS <> nil then
    m_objLHS.Copy(parLHS);
  m_objLHS.ePartSide := epsLHS;
  // Operator
  m_eConditionOperator := parOperator;
  // Right Hand Side
  m_eRHSType := eorColumn;
  m_objRHS.Clear;
  if parRHS <> nil then
    m_objRHS.Copy(parRHS);
  m_objRHS.ePartSide := epsRHS;
  // Column Indexes
  if parLHS <> nil then
    m_iLHS := parLHS.ParentIndex;
  m_iRHS := parRHS.ParentIndex;
  result := self;
end;

// TcStatementPartCondition
//   SetCondition (2)
//
function TcStatementPartCondition.SetCondition(parLHS: TcObject; parOperator: TeConditionOperator; parRHS: String): TcStatementPartCondition;
begin
    // Left Hand Site
  m_objLHS.Clear;
  if parLHS <> nil then
    m_objLHS.Copy(parLHS);
  m_objLHS.ePartSide := epsLHS;
  // Operator
  m_eConditionOperator := parOperator;
    // Right Hand Side
  m_eRHSType := eorValue;
  m_sRHS := parRHS;
  // Column Indexes
  if parLHS <> nil then
    m_iLHS := parLHS.ParentIndex;
  m_iRHS := kiUNDEFINED;
  result := self;
end;

// TcStatementPartCondition
//   GetXML
//
function TcStatementPartCondition.GetXML: String;
begin
  result := Format('<%s ', [krsXML_STATEMENTPARTCONDITION]) +
            Format(' %s="%d"', [krsXML_OPERATOR, longint(m_eConditionOperator)]) +
            Format(' %s="%d"', [krsXML_RHS_TYPE, longint(m_eRHSType)]) +
            Format(' %s="%s"', [krsXML_RHS_VALUE, m_sRHS]) +
            Format(' %s="%s"', [krsXML_RHS_OUTER, kasBOOL[esoRightOuter in m_eOptions]]) +
            Format(' %s="%s"', [krsXML_LHS_OUTER, kasBOOL[esoLeftOuter in m_eOptions]]) +
            '>' +
            m_objLHS.XML;
  if m_eRHSType = eorColumn then
    result := result + m_objRHS.XML;
  result := result + Format('</%s>', [krsXML_STATEMENTPARTCONDITION]);
end;

// TcStatementPartCondition
//   SetXML
//
procedure TcStatementPartCondition.SetXML(value: String);
var
  p, xPE, q: OLEVariant;
  i: longint;
begin
  Clear;
  if trim(value) <> ksEMPTY then
  try
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) and (p.childNodes.Length = 1) then
      begin
        q := p.childNodes.item[0];
        // Condition Operator
        m_eConditionOperator := TeConditionOperator(strtointdef(GetXMLAttribute(q, krsXML_OPERATOR), longint(ecoEqual)));
        // LHS Index
        m_iLHS := kiUNDEFINED;
        // RHS Index
        m_iRHS := kiUNDEFINED;
        // RHS Type
        m_eRHSType := TeOperatorRHS(strtointdef(GetXMLAttribute(q, krsXML_RHS_TYPE), longint(eorValue)));
        // RHS Value
        m_sRHS := GetXMLAttribute(q, krsXML_RHS_VALUE);
        // RHS Outer
        if GetXMLAttribute(q, krsXML_RHS_OUTER) = krsTRUE then
          m_eOptions := m_eOptions + [esoRightOuter];
        // LHS Outer
        if GetXMLAttribute(q, krsXML_LHS_OUTER) = krsTRUE then
          m_eOptions := m_eOptions + [esoLeftOuter];
        //
        // Child Nodes
        for i := 0 to q.childNodes.length - 1 do
          if AnsiCompareText(q.childNodes.Item[i].nodeName, krsXML_STATEMENTPART) = 0 then
          begin
            if GetXMLAttribute(q.childNodes.Item[i], krsXML_PARTSIDE) = '1' then
              m_objRHS.XML := q.childNodes.Item[i].XML
            else if GetXMLAttribute(q.childNodes.Item[i], krsXML_PARTSIDE) = '2' then
              m_objLHS.XML := q.childNodes.Item[i].XML;
          end;
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create('''Statement Part Condition'' XML document failed to load.' + ksCR + Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR + trim(xPE.srcText));
      end;
    except
      on E: Exception do
      begin
        Clear;
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
      end;
    end;
  finally
    p := unassigned;
  end;
end;

// TcStatementPartCondition
//   SetSchema
//
procedure TcStatementPartCondition.SetSchema(value: TcObject);
begin
  m_objLHS.Schema := value;
  m_objRHS.Schema := value;
  // Indexes
  if m_objLHS.objPart <> nil then
    m_iLHS := m_objLHS.objPart.ParentIndex;
  if m_objRHS.objPart <> nil then
    m_iRHS := m_objRHS.objPart.ParentIndex;
end;

//
// TcLocation
//

// TcLocation
//   Constructor
//
constructor TcLocation.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  Clear;
end;

// TcLocation
//   Clear
//
procedure TcLocation.Clear;
begin
  inherited Clear;
  m_iTop := 0;
  m_iLeft := 0;
  m_iHeight := 0;
  m_iWidth := 0;
end;

// TcLocation
//   Copy
//
procedure TcLocation.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcLocation) then
  begin
    m_iTop    := (value as TcLocation).Top;
    m_iLeft   := (value as TcLocation).Left;
    m_iHeight := (value as TcLocation).Height;
    m_iWidth  := (value as TcLocation).Width;
  end;
end;

// TcLocation
//   GetIsEmpty
//
function TcLocation.GetIsEmpty: boolean;
begin
  result := (m_iTop = 0) and (m_iLeft = 0) and (m_iHeight = 0) and (m_iWidth = 0);
end;

// TcLocation
//   GetXML
//
function TcLocation.GetXML: String;
begin
  result := format('<LOCATION %s="%d" %s="%d" %s="%d" %s="%d" />', [krsTOP, m_iTop, krsLEFT, m_iLeft, krsHEIGHT, m_iHeight, krsWIDTH, m_iWidth]);
end;

// TcLocation
//   SetXML
//
procedure TcLocation.SetXML(value: String);
var
  p, xPE: OLEVariant;
begin
  Clear;
  if trim(value) <> ksEMPTY then
  try
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) then
      begin
        m_iTop := strtointdef(GetXMLAttribute(p, krsTOP), 0);
        m_iLeft := strtointdef(GetXMLAttribute(p, krsLEFT), 0);
        m_iHeight := strtointdef(GetXMLAttribute(p, krsHEIGHT), 0);
        m_iWidth := strtointdef(GetXMLAttribute(p, krsWIDTH), 0);
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create('''Location'' XML document failed to load.' + ksCR + Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR + trim(xPE.srcText));
      end;
    except
      on E: Exception do
      begin
        Clear;
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
      end;
    end;
  finally
    p := unassigned;
  end;
end;

// TcLocation
//   SetToControl
//
procedure TcLocation.SetToControl(value: TControl);
begin
  if (value <> nil) and (value is TControl) then
  begin
    value.Top    := m_iTop;
    value.Left   := m_iLeft;
    value.Height := m_iHeight;
    value.Width  := m_iWidth;
  end;
end;

// TcLocation
//   GetFromControl
//
procedure TcLocation.GetFromControl(value: TControl);
begin
  if (value <> nil) and (value is TControl) then
  begin
    m_iTop    := value.Top;
    m_iLeft   := value.Left;
    m_iHeight := value.Height;
    m_iWidth  := value.Width;
  end;
end;

//
// TcSchema
//

// TcSchema
//   Constructor
//
constructor TcSchema.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objConnection := nil;
  m_objMetaData := nil;
  Clear;
end;

// TcSchema
//   Clear
//
procedure TcSchema.Clear;
begin
  inherited Clear;
  m_bInitialized := FALSE;
end;

// TcSchema
//   Initialize
//
function TcSchema.Initialize: boolean;
var
  e: TcExecute;
  rs: TcRecordSet;
  p, q: TcSchemaObject;
  s: String;
  m: TcObject;
begin
  result := FALSE;
  e := nil;
  if (m_objConnection <> nil) and (m_objMetaData <> nil) and (m_objMetaData is TcMetaData) then
  try
    Screen.Cursor := crAppStart;
    e := TcExecute.Create(nil);
    e.Connection := m_objConnection;
    try
      //
      // Table Objects
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsTABLELIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaTables, Unassigned, 'Tables');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            p := TcSchemaObject.Create(self);
            Add(p);
            if not rs.FieldIsNull('TABLE_CATALOG') then
              p.sSchema := VarToStr(rs.Fields('TABLE_CATALOG'))
            else if not rs.FieldIsNull('TABLE_SCHEMA') then
              p.sSchema := VarToStr(rs.Fields('TABLE_SCHEMA'));
            p.sTable := VarToStr(rs.Fields('TABLE_NAME'));
            p.sName := p.sTable;
            s := VarToStr(rs.Fields('TABLE_TYPE'));
            if (s = 'TABLE') or (s = 'SYSTEM TABLE') then
              p.ePartType := epTable
            else if (s = 'VIEW') or (s = 'SYSTEM VIEW') then
              p.ePartType := epView
            else if (s = 'SYNONYM') then
              p.ePartType := epSynonym;
            // create ddefault '*' column
            q := TcSchemaObject.Create(p);
            p.add(q);
            q.sTable := p.sTable;
            q.sColumn := krsSTAR;
            q.sName := q.sColumn;
            q.ePartType := epColumn;
            //
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
      //
      // Column Objects
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsCOLUMNLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaColumns, Unassigned, 'Table Columns');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            q := Find(VarToStr(rs.Fields('TABLE_NAME')), ksEMPTY) as TcSchemaObject;
            if q <> nil then
            begin
              p := TcSchemaObject.Create(q);
              q.add(p);
              p.sTable := VarToStr(rs.Fields('TABLE_NAME'));
              p.sColumn := VarToStr(rs.Fields('COLUMN_NAME'));
              p.sName := p.sColumn;
              p.ePartType := epColumn;
            end;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.Free;
      end;
      //
      // Primary Key
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsPRIMARYKEYLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaPrimaryKeys, Unassigned, 'Primary Keys');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            p := Find(VarToStr(rs.Fields('TABLE_NAME')), VarToStr(rs.Fields('COLUMN_NAME'))) as TcSchemaObject;
            if (p <> nil) and (p is TcSchemaObject) and ((p as TcSchemaObject).ePartType = epColumn) then
              p.Options := p.Options + [epoPrimaryKey];
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.Free;
      end;
      //
      // Foreign Key
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsFOREIGNKEYLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaForeignKeys, Unassigned, 'Foreign Keys');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            // Foreign Key part
            p := Find(VarToStr(rs.Fields('FK_TABLE_NAME')), VarToStr(rs.Fields('FK_COLUMN_NAME'))) as TcSchemaObject;
            if p <> nil then
            begin
              // Primary Key part
              q := Find(VarToStr(rs.Fields('PK_TABLE_NAME')), VarToStr(rs.Fields('PK_COLUMN_NAME'))) as TcSchemaObject;
              if q <> nil then
              begin
                if not (epoForeignKey in p.Options) then
                begin
                  p.Options := p.Options + [epoForeignKey];
                  p.ForeignKey := q;
                end
                else
                  p.ForeignKey := nil;
              end;
            end;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.Free;
      end;
      (*
      //
      // Index Objects
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsINDEXLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaTables, Unassigned, 'Indexes');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            p := TcSchemaObject.Create(self);
            Add(p);
            p.sSchema := VarToStr(rs.Fields('TABLE_SCHEMA'));
            p.sTable := VarToStr(rs.Fields('TABLE_NAME'));
            p.sName := VarToStr(rs.Fields('INDEX_NAME'));
            p.ePartType := epIndex;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
      //
      // Index Column Objects
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsINDEXCOLUMNLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue)
        else
          rs := e.OpenSchema(adSchemaColumns, Unassigned, 'Index Columns');
        if rs <> nil then
        begin
          while not rs.EOF do
          begin
            q := Find(VarToStr(rs.Fields('INDEX_NAME')), ksEMPTY) as TcSchemaObject;
            if q <> nil then
            begin
              p := TcSchemaObject.Create(q);
              q.add(p);
              p.sTable := VarToStr(rs.Fields('INDEX_NAME'));
              p.sColumn := VarToStr(rs.Fields('COLUMN_NAME'));
              p.sName := p.sColumn;
              p.ePartType := epColumn;
            end;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.Free;
      end;
      *)
      //
      // Done
      result := TRUE;
    except
      //
    end;
  finally
    e.free;
    Screen.Cursor := crDefault;
  end;
  m_bInitialized := TRUE;
end;

// TcSchema
//   SetConnection
//
procedure TcSchema.SetConnection(value: TcConnection);
begin
  m_objConnection := value;
end;

// TcSchema
//   SetMetaData
//
procedure TcSchema.SetMetaData(value: TcObject);
begin
  m_objMetaData := value;
end;

//
// TcSchemaObject
//

// TcSchemaObject
//   Constructor
//
constructor TcSchemaObject.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  Clear;
end;

// TcSchemaObject
//   Clear
//
procedure TcSchemaObject.Clear;
begin
  inherited Clear;
  m_sSchema := ksEMPTY;
  m_sTable := ksEMPTY;
  m_sColumn := ksEMPTY;
  m_ePartType := epUndefined;
  m_esOptions := [];
  m_objForeignKey := nil;
end;

// TcSchemaObject
//   Text method
//
function TcSchemaObject.Text: String;
begin
  result := sName;
end;

// TcSchemaObject
//   GetImageIndex
//
function TcSchemaObject.GetImageIndex: longint;
begin
  result := kiUNDEFINED;
  if epoForeignKey in m_esOptions then
    result := 0;
  if epoPrimaryKey in m_esOptions then
    result := 1;
end;

// TcSchemaObject
//   GetTag
//
function TcSchemaObject.GetTag: longint;
begin
  result := GetImageIndex;
end;

//
// TcCustomPart
//

// TcCustomPart
//   Clear
//
procedure TcCustomPart.Clear;
begin
  inherited Clear;
  m_sTable := ksEMPTY;
  m_sColumn := ksEMPTY;
  m_esOptions := [];
  m_ePartType := epUndefined;
end;

// TcCustomPart
//   Copy
//
procedure TcCustomPart.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcCustomPart) then
  begin
    m_sTable    := (value as TcCustomPart).m_sTable;
    m_sColumn   := (value as TcCustomPart).m_sColumn;
    m_esOptions := (value as TcCustomPart).m_esOptions;
    m_ePartType := (value as TcCustomPart).m_ePartType;
  end;
end;

// TcCustomPart
//   Compare
//
function TcCustomPart.Compare(value: TcObject): boolean;
begin
  result := inherited Compare(value);
  if (value <> nil) and (value is TcCustomPart) then
    result := result and
              (m_sTable    = (value as TcCustomPart).m_sTable) and
              (m_sColumn   = (value as TcCustomPart).m_sColumn) and
              // (m_esOptions = (value as TcCustomPart).m_esOptions) and
              (m_ePartType = (value as TcCustomPart).m_ePartType);
end;

// TcCustomPart
//   Find
//
function TcCustomPart.Find(parType: TePartTypeSet; value: String): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and
       (Objects[i] is TcCustomPart) and
       ((Objects[i] as TcCustomPart).ePartType in parType) and
       (AnsiCompareText(Objects[i].sName, value) = 0) then
    begin
      result := Objects[i];
      break;
    end;
end;

// TcCustomPart
//   Find
//
function TcCustomPart.Find(sparTable, sparColumn: String): TcObject;
begin
  result := nil;
  if (sparTable <> ksEMPTY) then
    result := Find(kesTABLESET, sparTable);
  if (result <> nil) and (sparColumn <> ksEMPTY) and (result is TcCustomPart) then
    result := (result as TcCustomPart).Find(kesCOLUMNSET, sparColumn);
end;

end.


