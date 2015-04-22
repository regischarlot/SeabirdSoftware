unit GrammarLib;

interface

uses
  Classes,       // TStringList
  daGlobals,
  daObjectLib,     // TcObject
  ConnectionLib, // TcConnection
  StatementLib,  // TcStatementList, TcStatement
  daStreamLib;     // TcTokenStream

type
  TrResult = record
    Result: boolean;
    Depth: longint;
  end;

  TcGrammarMacro = class;

  TcGrammarItem = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcGrammarItem is the base grammar object.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/09/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_eKeyword: TvKeyword;
    m_iStart: longint;
    m_iLineNumber, m_iColumnNumber: longint;
    m_objReference: TcObject;

  private
    // Private declarations
    //
    function    NextEquivalentToken(parStream: TcTokenStream; parID: longint; parDepth: longint; parPath: String): TrResult;
    function    NextPossibleToken(parStream: TcTokenStream): String;
    procedure   AddToken(parStream: TcTokenStream; parObject: TcGrammarItem); virtual;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    procedure   Clear; override;                                                // Clear the object
    //   Copy
    //   Compare
    function    Text: string; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    Find(eparKeyword: TvKeyword; value: String): TcObject; overload; virtual;
    function    ParseExpression(parStream: TcTokenStream; parID: longint; parStatement: TcStatement): boolean;
    function    IsReserved(value: string): boolean; virtual;

  public
    // Public Properties
    //
    property    eKeyword: TvKeyword                       read m_eKeyword       write m_eKeyword;
    property    iStart: longint                           read m_iStart         write m_iStart;
    property    iLineNumber: longint                      read m_iLineNumber    write m_iLineNumber;
    property    iColumnNumber: longint                    read m_iColumnNumber  write m_iColumnNumber;
    property    objReference: TcObject                    read m_objReference;
  end;

  TcGrammar = class(TcGrammarItem)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcGrammar is the grammar object for applying any type of grammar.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/09/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objConnection: TcConnection;
    m_bInitialized: boolean;
    m_lstErrors: TStringList;
    m_bIsValid: boolean;
    m_sExpression: String;
    m_sTrace: String;
    m_sReservedkeywords: string;
    m_objMetaData: TcObject;
    m_objMacros: TcGrammarMacro;
    m_sTableListStmt: String;
    m_sColumnListStmt: String;
    m_sPrimaryKeyStmt: String;
    m_sForeignKeyStmt: String;

  private
    // Private declarations
    //
    procedure   SetError(value: String);
    function    GetError: String;
    procedure   SetConnection(value: TcConnection);
    procedure   AddToken(parStream: TcTokenStream; parObject: TcGrammarItem); override;
    procedure   SetMetaData(value: TcObject);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear the object
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Initialize(value: String): boolean;
    function    Parse(parObject: TcObject; value: string): boolean;
    function    Find(eparPartType: TePartType; value: String): TcObject; overload; virtual;
    function    IsReserved(value: string): boolean; override;

  public
    // Public Properties
    //
    property    Connection: TcConnection                  read m_objConnection  write SetConnection;
    property    MetaData: TcObject                        read m_objMetaData    write SetMetaData;
    property    Initialized: boolean                      read m_bInitialized;
    property    Error: String                             read GetError         write SetError;
    property    IsValid: boolean                          read m_bIsValid;
    property    Macros: TcGrammarMacro                    read m_objMacros;
  end;

  TcGrammarMacro = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcGrammarMacro is the base grammar object for macro (i.e.
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
    m_sTable: String;
    m_sColumn: String;
    m_ePartType: TePartType;
    m_esOptions: TcPartOptionSet;
    m_objForeignKey: TcGrammarMacro;

  private
    // Private members
    //
    function    GetImageIndex: longint;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
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
    property    sTable: String                            read m_sTable         write m_sTable;
    property    sColumn: String                           read m_sColumn        write m_sColumn;
    property    ePartType: TePartType                     read m_ePartType      write m_ePartType;
    property    Options: TcPartOptionSet                  read m_esOptions      write m_esOptions;
    property    ForeignKey: TcGrammarMacro                read m_objForeignKey  write m_objForeignKey;
    property    ImageIndex: longint                       read GetImageIndex;
  end;

implementation

uses
  sysUtils,
  Math, // Min()
  daResourceStrings,
  ExecuteLib,
  ADODB_TLB,
  Dialogs,
  DataLib;

const
  kiPATHMAXSIZE = 1000;
  kiLR          = 2;

//
// Tools
//

// Tool
//   StringToPartType
//
function StringToPartType(value: String): TePartType;
var
  e: TePartType;
const
  kasPARTTYPE: array[TePartType] of String =
    ('', '#column', '#schema', '#sequence', '#snapshot', '#table', '#view', '#alias', '#synonym', '#prefix', '#where');
begin
  result := epUndefined;
  for e := low(TePartType) to high(TePartType) do
    if AnsiCompareText(value, kasPARTTYPE[e]) = 0 then
    begin
      result := e;
      break;
    end;
end;

// Tool
//   rResult
//
function rResult(bvalue: boolean; lvalue: longint): TrResult;
begin
  result.Result := bvalue;
  result.Depth := lvalue;
end;

//
// TcGrammarItem
//

// TcGrammarItem
//   Clear
//
procedure TcGrammarItem.Clear;
begin
  inherited Clear;
  m_eKeyword := _UNDEFINED;
  m_iStart := kiUNDEFINED;
  m_iLineNumber := kiUNDEFINED;
  m_iColumnNumber := kiUNDEFINED;
  m_objReference := nil;
end;

// TcGrammarItem
//   Text
//
function TcGrammarItem.Text: String;
var
  i: longint;
begin
  // Generate text for *this
  result := inttostr(Tag) + RepeatStr(Depth, '. ') + _TOKENS[m_eKeyword];
  if AnsiCompareText(sValue, _TOKENS[m_eKeyword]) <> 0 then
  begin
    if _TOKENS[m_eKeyword] <> ksEMPTY then
      result := result + ' ';
    result := result + sValue;
  end;
  if parent <> nil then
    result := result + format(' %d', [parent.Tag]);
  if m_objReference <> nil then
    result := result + Format(' <%d>', [m_objReference.Tag]);
  result := result + ksCR;
  // Traverse Children
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
      result := result + (Objects[i] as TcGrammarItem).Text;
end;

// TcGrammarItem
//   Find
//
function TcGrammarItem.Find(eparKeyword: TvKeyword; value: String): TcObject;
var
  i: longint;
begin
  result := nil;
  if (m_eKeyword = eparKeyword) and (AnsiCompareText(sValue, value) = 0) then
    result := self
  else for i := 0 to count - 1 do
    if (result = nil) and (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
      result := (Objects[i] as TcGrammarItem).Find(eparKeyword, value);
end;

// TcGrammarItem
//   ParseExpression
//
function TcGrammarItem.ParseExpression(parStream: TcTokenStream; parID: longint; parStatement: TcStatement): boolean;

  procedure RaiseError(Token: TcToken; value: String);
  var
    s: String;
  begin
    s := GetLine(StreamToString(parStream), Token.Line) + ksCR;
    system.insert('^', s, Token.Col + 1);
    raise Exception.Create(Format('%sError Line %d, Column %d: ', [s, Token.Line, Token.Col]) + Value);
  end;

var
  i, L, M: longint;
  e: TePartType;
  SP: TcStatementPart;
  p: TcObject;
begin
  with parStream do
  begin
    result := FALSE;
    case eKeyword of
      // String & Number
      _STRING, _NUMBER, _REAL:
        if AnsiCompareText(sValue, Token.Value) <> 0 then
          RaiseError(Token, Format('''%s'' expected.', [sValue]))
        else
        begin
          AddToken(parStream, self);
          result := Match(Token.ID);
        end;
      // Literal String (i.e. any string) expected
      _LITERAL_STRING:
        begin
          AddToken(parStream, self);
          result := Match(_STRING);
        end;
      // Literal Number (i.e. any number) expected
      _LITERAL_NUMBER:
        begin
          AddToken(parStream, self);
          result := Match(_NUMBER);
        end;
      // Literal Floating point (i.e. any number) expected
      _LITERAL_REAL:
        begin
          AddToken(parStream, self);
          result := Match(_REAL);
        end;
      // Term: Reference to another grammar expression.
      _TERM:
      begin
        if (m_objReference = nil) or ((m_objReference <> nil) and not (m_objReference is TcGrammarItem)) then
          RaiseError(Token, Format('''%s'' is an invalid reference.', [sValue]))
        else
          result := (m_objReference as TcGrammarItem).ParseExpression(parStream, m_objReference.Tag, parStatement);
      end;
      // {..}
      _LCURLY:
        if count > 0 then
        repeat
          if NextEquivalentToken(parStream, Tag, 1, ksEMPTY).Result then
            for i := 0 to count - 1 do
              if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
                result := (Objects[i] as TcGrammarItem).ParseExpression(parStream, Objects[i].Tag, parStatement) or result;
        until not NextEquivalentToken(parStream, Tag, 1, ksEMPTY).Result
        else
          result := TRUE;
      // [..]
      _LBRACKET:
        if (count > 0) and (Objects[0] <> nil) and NextEquivalentToken(parStream, Tag, kiLR, ksEMPTY).Result then
        begin
          for i := 0 to count - 1 do
            if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
              result := (Objects[i] as TcGrammarItem).ParseExpression(parStream, Objects[i].Tag, parStatement) or result;
        end
        else
          result := TRUE;
      // (..)
      _LPAREN, _ITEM:
        for i := 0 to count - 1 do
          if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
            result := (Objects[i] as TcGrammarItem).ParseExpression(parStream, Objects[i].Tag, parStatement) or result;
      // |
      _VBAR:
      begin
        M := kiUNDEFINED;
        for i := 0 to count - 1 do
          if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) and (Objects[i] as TcGrammarItem).NextEquivalentToken(parStream, Objects[i].Tag, kiLR, ksEMPTY).Result then
          begin
            M := i;
            break
          end;
        // OK. So we must have some result.
        if M = kiUNDEFINED then
          RaiseError(Token, Format('''%s'' should be one of %s', [Token.Value, NextPossibleToken(parStream)]))
        else
          result := (Objects[M] as TcGrammarItem).ParseExpression(parStream, Objects[M].Tag, parStatement);
      end;
      // Macro string: '#column' | '#schema' | '#sequence' | '#snapshot' | '#table' | '#view' | '#alias' | '#synonym' | '#prefix'
      _MACRO:
      begin
        e := StringToPartType(sValue);
        if e in [epColumn, epTable, epView, epSynonym] then
        begin
          p := nil;
          if (Token.ID = _STRING) and (e <> epUndefined) and (TopParent <> nil) and (TopParent is TcGrammar) then
            p := (TopParent as TcGrammar).Find(e, Token.Value);
          if p <> nil then
          begin
            if parStatement.CurrentPragma <> nil then
            begin
              parStatement.CurrentPragma.sName := p.sName;
              parStatement.CurrentPragma.ePartType := (p as TcGrammarMacro).ePartType;
              parStatement.CurrentPragma.objPart := p;
            end;
            AddToken(parStream, self);
            result := Match(_STRING);
          end
          else
            RaiseError(Token, Format('''%s'' is not a %s object', [Token.Value, sValue]));
        end
        else
        begin
          AddToken(parStream, self);
          if (e = epAlias) and (parStatement.CurrentPragma <> nil) then
            parStatement.CurrentPragma.sAlias := Token.Value
          else if (e = epPrefix) and (parStatement.CurrentPragma <> nil) then
            parStatement.CurrentPragma.sPrefix:= Token.Value;
          result := Match(_STRING);
        end
      end;
      // Pragma; COLUMN, TABLE
      _PRAGMA:
      begin
        SP := TcStatementPart.Create(nil);
        SP.ePartType := StringToPartType('#' + lowercase(sValue));
        parStatement.CurrentPragma := SP;
        L := iTokenStart;
        result := (count > 0) and
                  (Objects[0] <> nil) and (Objects[0] is TcGrammarItem) and
                  (Objects[0] as TcGrammarItem).ParseExpression(parStream, parID, parStatement);
        if result then
        begin
          SP.sValue := parStream.SubStr(L, iTokenStart - L);
          parStatement.Add(SP);
        end
        else
          SP.Free;
        parStatement.CurrentPragma := nil;
      end;
    else
      RaiseError(Token, Format('Token ID: %d, Value: ''%s'' is Unexpected.', [longint(Token.ID), Token.Value]));
    end;
  end;
end;

// TcGrammarItem
//   NextEquivalentToken
//
function TcGrammarItem.NextEquivalentToken(parStream: TcTokenStream; parID: longint; parDepth: longint; parPath: String): TrResult;
var
  i: longint;
  b: TrResult;
  p: TcGrammarItem;
  e: TePartType;
  ts: TcTokenState;
  s: String;
begin
  result := rResult(FALSE, 0);
  // Check for recursions
  if length(parPath) > kiPATHMAXSIZE then
    exit;
  if pos(Format('/%d/', [parID]), '/' + parPath + '/') > 0 then
  begin
    s := Item(parPath, '/', ItemCount(parPath, '/') - 1);
    for i := 1 to ItemCount(parPath, '/') - 1 do
      if (Item(parPath, '/', i) = inttostr(parID)) and (Item(parPath, '/', i - 1) = s) then
        exit;
  end;
  if parPath <> ksEMPTY then
    parPath := parPath + '/';
  parPath := parPath + inttostr(parID);
  // Proceed with check
  with parStream do
    case eKeyword of
      // String & Number
      _STRING, _NUMBER, _REAL:
        result := rResult(AnsiCompareText(sValue, Token.Value) = 0, 1);
      // Literal String (i.e. any string) expected
      _LITERAL_STRING:
        result := rResult((Token.ID = _STRING) and (Token.HasQuote or not IsReserved(Token.Value)), 1);
      // Literal Number (i.e. any number) expected
      _LITERAL_NUMBER:
        result := rResult(Token.ID = _NUMBER, 1);
      // Literal Floating point (i.e. any number) expected
      _LITERAL_REAL:
        result := rResult(Token.ID = _REAL, 1);
      // Term: Reference to another grammar expression.
      _TERM:
        if (m_objReference <> nil) and (m_objReference is TcGrammarItem) then
          result := (m_objReference as TcGrammarItem).NextEquivalentToken(parStream, m_objReference.Tag, parDepth, parPath);
      // Item, [..], {..}, (..)
      _ITEM, _LBRACKET, _LCURLY, _LPAREN:
      begin
        ts := parStream.State;
        for i := 0 to count - 1 do
          if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
          begin
            p := Objects[i] as TcGrammarItem;
            b := p.NextEquivalentToken(parStream, p.Tag, parDepth - result.Depth, parPath);
            if b.Result then
              Match(_WILDCARD);
            result.Result := b.Result or (not b.Result and (p.eKeyword in [_LBRACKET, _LCURLY] ));
            if b.Result then
              inc(result.Depth, b.Depth);
            if (not result.Result and (p.eKeyword <> _LBRACKET)) or (result.Depth >= parDepth) then
              break;
          end;
        parStream.State := ts;
      end;
      // |
      _VBAR:
        for i := 0 to count - 1 do
          if (Objects[i] <> nil) and (Objects[i] is TcGrammarItem) then
          begin
            result := (Objects[i] as TcGrammarItem).NextEquivalentToken(parStream, Objects[i].Tag, parDepth, parPath);
            if result.Result then
              break;
          end;
      // Macro string: '#column' | '#schema' | '#sequence' | '#snapshot' | '#table' | '#view' | '#alias' | '#synonym' | '#prefix'
      _MACRO:
      begin
        e := StringToPartType(sValue);
        if (e in [epColumn, epTable, epView, epSynonym]) and (Token.ID = _STRING) and (e <> epUndefined) and (TopParent <> nil) and (TopParent is TcGrammar) then
          result := rResult((TopParent as TcGrammar).Find(e, Token.Value) <> nil, 1)
        else if (e in [epAlias, epPrefix]) then
          result := rResult((Token.ID = _STRING) and
                            (Token.Value <> ksEMPTY) and
                            (Token.Value[1] in ['a' .. 'z', 'A' .. 'Z', '_']) and
                            (not IsReserved(Token.Value)),
                            1);
        // Missing: emtSchema, emtSequence, emtSnapshot
      end;
      _PRAGMA:
      begin
        if (count > 0) and (Objects[0] <> nil) and (Objects[0] is TcGrammarItem) then
          result := (Objects[0] as TcGrammarItem).NextEquivalentToken(parStream, parID, parDepth, parPath);
      end;
    end;
end;

// TcGrammarItem
//   NextPossibleToken
//
function TcGrammarItem.NextPossibleToken(parStream: TcTokenStream): String;
begin
  result := ksEMPTY;
end;

// TcGrammarItem
//   AddToken
//
procedure TcGrammarItem.AddToken(parStream: TcTokenStream; parObject: TcGrammarItem);
begin
  if (TopParent <> nil) and (TopParent is TcGrammar) then
    (TopParent as TcGrammar).AddToken(parStream, parObject);
end;

// TcGrammarItem
//   IsReserved
//
function TcGrammarItem.IsReserved(value: string): boolean;
begin
  result := FALSE;
  if (TopParent <> nil) and (TopParent is TcGrammar) then
    result := (TopParent as TcGrammar).IsReserved(value);
end;

//
// TcGrammar
//

// TcGrammar
//   Constructor
//
constructor TcGrammar.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objConnection := nil;
  m_lstErrors := TStringList.Create;
  m_sReservedkeywords := ksEMPTY;
  m_sTableListStmt := ksEMPTY;
  m_sColumnListStmt := ksEMPTY;
  m_sPrimaryKeyStmt := ksEMPTY;
  m_sForeignKeyStmt := ksEMPTY;
  m_objMetaData := nil;
  m_objMacros := TcGrammarMacro.Create(self);
  Clear;
end;

// TcGrammar
//   Destructor
//
destructor TcGrammar.Destroy;
begin
  Clear;
  m_lstErrors.free;
  m_objMacros.Free;
  inherited Destroy;
end;

// TcGrammar
//   Clear
//
procedure TcGrammar.Clear;
begin
  inherited Clear;
  m_lstErrors.Clear;
  m_bInitialized := FALSE;
  m_bIsValid := FALSE;
  m_sExpression := ksEMPTY;
  m_sTrace := ksEMPTY;
end;

// TcGrammar
//   Initialize
//      grammar      -> { term '->' { expr } ';' }
//      term         -> '@' string
//      expr         -> item { '|' item }
//      item         -> number |
//                      string |
//                      keyword |
//                      term |
//                      '[' expr { expr } ']' |
//                      '{' expr { expr } '}' |
//                      '(' expr { expr } ')' |
//                      pragma
//      keyword      -> '#column' | '#schema' | '#sequence' | '#snapshot' | '#table' | '#view' | '#alias' | '#synonym' | '#prefix'
//      pragma       -> '(*' string '*)' item
//
function TcGrammar.Initialize(value: String): boolean;

  procedure Read_Tables_Columns;
  var
    e: TcExecute;
    rs: RecordSet;
    p, q, r, t: TcGrammarMacro;
    s: String;
  begin
    if m_objMacros <> nil then
    begin
      e := nil;
      m_objMacros.Clear;
      if m_objConnection <> nil then
      try
        e := TcExecute.Create(nil);
        e.Connection := m_objConnection;
        try
          //
          // Table Objects
          if m_sTableListStmt <> ksEMPTY then
            rs := e.Execute(m_sTableListStmt)
          else
            rs := e.OpenSchema(adSchemaTables);
          if rs <> nil then
          begin
            while not rs.EOF do
            begin
              p := TcGrammarMacro.Create(self);
              m_objMacros.Add(p);
              if not FieldIsNull(rs.Fields['TABLE_CATALOG']) then
                p.sSchema := FieldToString(rs.Fields['TABLE_CATALOG'])
              else if not FieldIsNull(rs.Fields['TABLE_SCHEMA']) then
                p.sSchema := FieldToString(rs.Fields['TABLE_SCHEMA']);
              p.sTable := FieldToString(rs.Fields['TABLE_NAME']);
              p.sName := p.sTable;
              s := FieldToString(rs.Fields['TABLE_TYPE']);
              if (s = 'TABLE') or (s = 'SYSTEM TABLE') then
                p.ePartType := epTable
              else if (s = 'VIEW') or (s = 'SYSTEM VIEW') then
                p.ePartType := epView
              else if (s = 'SYNONYM') then
                p.ePartType := epSynonym;
              // create ddefault '*' column
              q := TcGrammarMacro.Create(p);
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
          rs := nil;
          //
          // Column Objects
          if m_sColumnListStmt <> ksEMPTY then
            rs := e.Execute(m_sColumnListStmt)
          else
            rs := e.OpenSchema(adSchemaColumns);
          if rs <> nil then
          begin
            while not rs.EOF do
            begin
              q := m_objMacros.Find(FieldToString(rs.Fields['TABLE_NAME'])) as TcGrammarMacro;
              if q <> nil then
              begin
                p := TcGrammarMacro.Create(q);
                q.add(p);
                p.sTable := FieldToString(rs.Fields['TABLE_NAME']);
                p.sColumn := FieldToString(rs.Fields['COLUMN_NAME']);
                p.sName := p.sColumn;
                p.ePartType := epColumn;
              end;
              rs.MoveNext;
            end;
            rs.Close;
          end;
          rs := nil;
          //
          // Primary Key
          if m_sPrimaryKeyStmt <> ksEMPTY then
            rs := e.Execute(m_sPrimaryKeyStmt)
          else
            rs := e.OpenSchema(adSchemaPrimaryKeys);
          if rs <> nil then
          begin
            while not rs.EOF do
            begin
              q := m_objMacros.Find(FieldToString(rs.Fields['TABLE_NAME'])) as TcGrammarMacro;
              if q <> nil then
              begin
                p := q.Find(FieldToString(rs.Fields['COLUMN_NAME'])) as TcGrammarMacro;
                if (p <> nil) and (p is TcGrammarMacro) and ((p as TcGrammarMacro).ePartType = epColumn) then
                  p.Options := p.Options + [epoPrimaryKey];
              end;
              rs.MoveNext;
            end;
            rs.Close;
          end;
          rs := nil;
          //
          // Foreign Key
          if m_sForeignKeyStmt <> ksEMPTY then
            rs := e.Execute(m_sForeignKeyStmt)
          else
            rs := e.OpenSchema(adSchemaForeignKeys);
          if rs <> nil then
          begin
            while not rs.EOF do
            begin
              // Foreign Key part
              t := m_objMacros.Find(FieldToString(rs.Fields['PK_TABLE_NAME'])) as TcGrammarMacro;
              if t <> nil then
              begin
                r := t.Find(FieldToString(rs.Fields['PK_COLUMN_NAME'])) as TcGrammarMacro;
                if (r <> nil) and (r.ePartType = epColumn) and not (epoForeignKey in r.Options) then
                begin
                  // Primary Key part
                  p := m_objMacros.Find(FieldToString(rs.Fields['PK_TABLE_NAME'])) as TcGrammarMacro;
                  if p <> nil then
                  begin
                    q := p.Find(FieldToString(rs.Fields['PK_COLUMN_NAME'])) as TcGrammarMacro;
                    if q <> nil then
                    begin
                      r.Options := r.Options + [epoForeignKey];
                      r.ForeignKey := q;
                    end;
                  end;
                end;
              end;
              rs.MoveNext;
            end;
            rs.Close;
          end;
          rs := nil;
        except
          on E: Exception do
            SetError(E.Message);
        end;
      finally
        e.free;
      end;
    end;
  end;

  function gCreate(parStream: TcTokenStream; parObject: TcGrammarItem; parTitle: String): TcGrammarItem;
  begin
    result := TcGrammarItem.Create(parObject);
    parObject.Add(result);
    result.sValue := parStream.Token.Value;
    if parTitle = ksEMPTY then
      parTitle := result.sValue;
    result.sName := parTitle;
    result.iStart := parStream.iTokenStart;
    result.eKeyword := parStream.Token.ID;
    result.iLineNumber := parStream.Token.Line;
    result.iColumnNumber := parStream.Token.Col;
  end;

  function Expr(parStream: TcTokenStream; parObject: TcGrammarItem; parTitle: String): TcGrammarItem; forward;

  // item -> number |
  //         string |
  //         keyword |
  //         term |
  //         '[' expr { expr } ']' |
  //         '{' expr { expr } '}' |
  //         '(' expr { expr } ')' |
  //         pragma
  // pragma -> '(*' string '*)' item

  function item(parStream: TcTokenStream; parObject: TcGrammarItem; parTitle: String): TcGrammarItem;
  var
    e: TvKeyword;
  begin
    with parStream do
      case Token.ID of
        // number | string | keyword | term
        _NUMBER, _REAL, _STRING:
        begin
          result := gCreate(parStream, parObject, parTitle);
          if (AnsiCompareText(Token.Value, 'string') = 0) and (Token.ID = _STRING) then
            result.eKeyword := _LITERAL_STRING
          else if (AnsiCompareText(Token.Value, 'number') = 0) and (Token.ID = _STRING) then
            result.eKeyword := _LITERAL_NUMBER
          else if (Token.Value <> ksEMPTY) and (Token.ID = _STRING) and (Token.Value[1] = '@') and not Token.HasQuote then
            result.eKeyword := _TERM
          else if (Token.Value <> ksEMPTY) and (Token.ID = _STRING) and (Token.Value[1] = '#') and not Token.HasQuote then
            result.eKeyword := _MACRO;
          Match(Token.ID);
        end;
        // '[' expr { expr } ']'
        // '{' expr { expr } '}'
        // '(' expr { expr } ')'
        _LBRACKET, _LPAREN, _LCURLY:
          begin
            e := Token.ID;
            result := gCreate(parStream, parObject, parTitle);
            Match(e);
            case e of
              _LBRACKET:
                e := _RBRACKET;
              _LPAREN:
                e := _RPAREN;
              _LCURLY:
                e := _RCURLY;
            end;
            while not EOS and (Token.ID <> e) do
              Expr(parStream, result, parTitle);
            Match(e);
          end;
        // Pragma
        _PRAGMA:
          begin
            Match(_PRAGMA);
            result := gCreate(parStream, parObject, parTitle);
            result.eKeyword := _PRAGMA;
            result.sValue := uppercase(Token.Value);
            Match(_STRING);
            Item(parStream, result, parTitle);
          end;
      else // Unexpected input!!
        raise Exception.Create(Error(Format(krsUNEXPECTEDTOKEN, [Token.Value])));
      end;
  end;

  // expr -> item { '|' item }
  function Expr(parStream: TcTokenStream; parObject: TcGrammarItem; parTitle: String): TcGrammarItem;
  var
    p: TcGrammarItem;
  begin
    with parStream do
    begin
      result := item(parStream, parObject, parTitle);
      if (result <> nil) and (Token.ID = _VBAR) and not EOS then
      begin
        // create top token
        p := gCreate(parStream, parObject, parTitle);
        // move the object created from item() below p
        result.Parent.Delete(result, FALSE);
        result.Parent := p;
        p.Add(result);
        //
        result := p;
        while (Token.ID = _VBAR) and not EOS do
        begin
          Match(_VBAR);
          item(parStream, result, parTitle);
        end;
      end
    end;
  end;

  procedure SetReferences(parObject: TcGrammarItem);
  var
    i: longint;
  begin
    // Process reference for *this
    if (parObject.eKeyword = _TERM) then
    begin
      parObject.m_objReference := Find(_ITEM, parObject.sValue);
      with parObject do
        if m_objReference = nil then
          SetError(Format('Line %d, Col %d: Reference to Object ''%s'' does not resolve to an expression', [m_iLineNumber, m_iColumnNumber, sValue]));
    end;
    // Traverse Children
    for i := 0 to parObject.count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcGrammarItem) then
        SetReferences(parObject[i] as TcGrammarItem);
  end;

var
  parStream: TcTokenStream;
  p: TcGrammarItem;
begin
  result := TRUE;
  sValue := value;
  Clear;
  //
  // A. Set Table & Column Lists
  Read_Tables_Columns;
  //
  // B. Parse Input
  parStream := nil;
  try
    //
    // A. Parse input
    parStream := StringToStream(trim(value));
    try
      with parStream do while result and not EOS do
      begin
        // grammar -> { term '->' { expr } ';' }
        // >> Term
        p := gCreate(parStream, self, ksEMPTY);
        p.eKeyword := _ITEM;
        Match(_STRING);
        // >> '->'
        Match(_IS);
        // >> { expr } ';'
        while not EOS and (Token.ID <> _SEMICOLON) do
          result := Expr(parStream, p, p.sValue) <> nil;
        if result then
          parStream.Match(_SEMICOLON);
      end;
    except
      on E:Exception do
        SetError(E.Message);
    end;
    //
    // B. Set References
    SetReferences(self);
    //
    // C. We are done.
    m_bIsValid := m_lstErrors.Count = 0;
  finally
    parStream.free;
  end;
  m_bInitialized := TRUE;
end;

// TcGrammar
//   SetError
//
procedure TcGrammar.SetError(value: String);
begin
  m_lstErrors.Add(value);
end;

// TcGrammar
//   GetError
//
function TcGrammar.GetError: String;
begin
  result := m_lstErrors.Text;
end;

// TcGrammar
//   Parse
//
function TcGrammar.Parse(parObject: TcObject; value: string): boolean;
var
  strm: TcTokenStream;
  p: TcObject;
  s: TcStatement;
  L: longint;
begin
  result := FALSE;
  if (parObject <> nil) and (value <> ksEMPTY) then
  begin
    parObject.sValue := value;
    strm := nil;
    try
      strm := TcTokenStream.Create;
      strm.AsStringTokens := [_SEMICOLON, _COMMA, _PERIOD, _COLON, _MULT, _DIV, _ADD, _SUBSTRACT, _QUESTIONMARK, _CARET, _LOWER, _LOWEREQUAL, _NOTEQUAL, _GREATER, _GREATEREQUAL, _EQUAL, _NOT, _IN, _AND, _OR, _LIKE];
      strm.AsValue := value;
      try
        while not strm.EOS do
        begin
          result := FALSE;
          p := Find(_ITEM, '@' + strm.Token.Value);
          if not ((p <> nil) and (p is TcGrammarItem)) then
          begin
            if parObject is TcStatementList then
              (parObject as TcStatementList).Error := Format('Unknown grammar for statement starting with ''%s''.', [strm.Token.Value]);
            break;
          end;
          m_sExpression := ksEMPTY;
          m_sTrace := ksEMPTY;
          // Get a statement object
          s := TcStatement.Create(parObject);
          L := strm.iTokenStart;
          // Get parsing happening
          if (p as TcGrammarItem).ParseExpression(strm, p.Tag, s) then
          begin
            // Set the statement object
            s.iStart := L;
            s.iEnd := strm.iTokenStart;
            s.SQL[estRaw] := system.copy(value, s.iStart + 1, s.iEnd - s.iStart);    // Raw SQL
            s.SQL[estParsed] := m_sExpression;                                       // Parsed SQL
            s.sTrace := m_sTrace;
            // Add it / Set it to parObject
            if parObject is TcStatement then
            begin
              parObject.Copy(s);
              s.Free;
            end
            else if parObject is TcStatementList then
              parObject.Add(s);
          end
          else
          begin
            s.Free;
            break;
          end;
          if parObject is TcStatement then
            break;
        end;
        result := count <> 0;
      except
        on E:Exception do
          if parObject is TcStatementList then
            (parObject as TcStatementList).Error := E.Message;
      end;
    finally
      strm.Free;
    end;
  end;
end;

// TcGrammar
//   SetError
//
procedure TcGrammar.SetConnection(value: TcConnection);
begin
  m_objConnection := value;
end;

// TcGrammar
//   Parse
//
function TcGrammar.Find(eparPartType: TePartType; value: String): TcObject;

  function LocalFind(parObject: TcGrammarMacro): TcObject;
  var
    i: longint;
  begin
    result := nil;
    if (parObject.ePartType = eparPartType) and (AnsiCompareText(parObject.sName, value) = 0) then
      result := parObject
    else for i := 0 to parObject.count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcGrammarMacro) then
      begin
        result := LocalFind(parObject[i] as TcGrammarMacro);
        if result <> nil then
          break;
      end;
  end;

begin
  result := LocalFind(m_objMacros);
end;

// TcGrammar
//   AddToken
//
procedure TcGrammar.AddToken(parStream: TcTokenStream; parObject: TcGrammarItem);
begin
  with parStream do
    if not EOS then
    begin
      if Token.HasPreceedingSpace and (m_sExpression <> ksEMPTY) then
        m_sExpression := m_sExpression + ' ';
      if Token.HasQuote then
        m_sExpression := m_sExpression + Token.QuoteSymbol + Token.Value + Token.QuoteSymbol
      else
        m_sExpression := m_sExpression + Token.Value;
    end;
  m_sTrace := m_sTrace + Format('[%s %d] ', [parStream.Token.Value, parObject.Tag]);
end;

// TcGrammar
//   IsReserved
//
function TcGrammar.IsReserved(value: string): boolean;
begin
  result := pos(',' + lowercase(value) + ',', ',' + lowercase(m_sReservedkeywords) + ',') <> 0;
end;

// TcGrammar
//   SetMetaData
//
procedure TcGrammar.SetMetaData(value: TcObject);
var
  i: longint;
  s: String;
begin
  m_objMetaData := value;
  if (value <> nil) and (value is TcMetaData) then
  begin
    // Reserved keywords
    s := (value as TcMetaData).Attribute[krsRESERVEDKEYWORDS];
    m_sReservedkeywords := ksEMPTY;
    for i := 1 to length(s) do
      if s[i] in [#33 .. #126] then
        m_sReservedkeywords := m_sReservedkeywords + s[i];
    // Table List Statement
    m_sTableListStmt := (value as TcMetaData).Attribute[krsTABLELIST];
    // Column List Statement
    m_sColumnListStmt := (value as TcMetaData).Attribute[krsCOLUMNLIST];
    // Primary Key Statement
    m_sPrimaryKeyStmt := (value as TcMetaData).Attribute[krsPRIMARYKEYLIST];
    // Foreign Key Statement
    m_sForeignKeyStmt := (value as TcMetaData).Attribute[krsFOREIGNKEYLIST];
  end;
end;

//
// TcGrammarMacro
//

// TcGrammarMacro
//   Clear
//
procedure TcGrammarMacro.Clear;
begin
  inherited Clear;
  m_sSchema := ksEMPTY;
  m_sTable := ksEMPTY;
  m_sColumn := ksEMPTY;
  m_ePartType := epUndefined;
  m_esOptions := [];
  m_objForeignKey := nil;
end;

// TcGrammarMacro
//   Text method
//
function TcGrammarMacro.Text: String;
begin
  result := sName;
end;

// TcGrammarMacro
//   GetImageIndex
//
function TcGrammarMacro.GetImageIndex: longint;
begin
  result := kiUNDEFINED;
  if epoForeignKey in m_esOptions then
    result := 1
  else if epoPrimaryKey in m_esOptions then
    result := 0;
end;

end.
