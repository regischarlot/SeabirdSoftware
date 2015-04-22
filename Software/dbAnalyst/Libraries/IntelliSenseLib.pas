unit IntelliSenseLib;

interface

uses
  sysUtils,
  daGlobals,
  daStreamLib,
  daObjectLib,
  daResourceStrings,
  ADODB_TLB,
  Classes,
  ConnectionLib,
  ExecuteLib,
  Forms,
  Types,
  SynEdit,
  StdCtrls,
  ExtCtrls,
  Graphics,
  Messages,
  Menus,
  Controls,
  PreferenceLib;

type
  TisTableList = class;
  TisIndexList = class;
  TIntelliSenseControl = class;
  TeKeyUpDown = (ekDown, ekUp);
  TeMacroType = (emtUndefined, emtTable, emtField, emtWhere);

  TisStatementException = class(TcObject)
  private
    // Private members
    //
    m_rToken: TcToken;
    m_esExpected: TeTokenTypeSet;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public properties
    //
    property   ErrorMessage: String     read GetName           write SetName;
    property   Token: TcToken           read m_rToken          write m_rToken;
    property   Expected: TeTokenTypeSet read m_esExpected      write m_esExpected;
  end;

  TisBaseStatement = class(TcObject)
  private
    // Private members
    //
    m_eTokenType: TeTokenType;
    m_sAlias: String;
    m_sPrefix: String;
    m_eMacroType: TeMacroType;

  private
    // Private members
    //
    procedure   LocalClear;
    function    GetAlias: String;
    function    ChildOf(value: TeTokenType; parButNot: TeTokenTypeSet): boolean;
    function    GetColumnPrefix: string;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public properties
    //
    property   sAlias: String                             read m_sAlias                 write m_sAlias;
    property   sPrefix: String                            read m_sPrefix                write m_sPrefix;
    property   eTokenType: TeTokenType                    read m_eTokenType             write m_eTokenType;
    property   eMacroType: TeMacroType                    read m_eMacroType             write m_eMacroType;
    property   Alias: String                              read GetAlias;
  end;

  TisStatement = class(TisBaseStatement)
  private
    // Private members
    //
    m_rToken: TcToken;
    m_iStart, m_iEnd: longint;
    m_lstAnalysis_Table: TcCollection;
    m_lstAnalysis_Column: TcCollection;
    m_lstErrors: TcCollection;
    m_esExpected: TeTokenTypeSet;
    m_objTableList: TisTableList;
    m_objIndexList: TisIndexList;
    m_objIntelliSenseControl: TIntelliSenseControl;
    m_objPreferences: TcPreferenceList;
    m_sProperties: String;

  private
    // Private members
    //
    procedure   SetStartEnd(iOffset: longint);
    procedure   ExtendToken(value: TcToken);
    procedure   SetTokenType(value: TcToken; parMode: TeTokenType);
    procedure   SetErrors(value: String); overload;
    procedure   SetErrors(value: String; Token: TcToken; esExpected: TeTokenTypeSet); overload;
    function    GetErrors: String;
    procedure   ClearErrors;
    function    GetTableList: TisTableList;
    procedure   SetTableList(value: TisTableList);
    function    GetIndexList: TisIndexList;
    procedure   SetIndexList(value: TisIndexList);
    function    Parse(value: string): boolean; overload; virtual;
    function    Parse(value: TcTokenStream; var stmtParent: TisStatement): boolean; overload; virtual;
    function    PosToStatementPart(value: longint): TisStatement;
    procedure   Analyze;
    function    ExpectedNext(value: String; var stmtParent: TisStatement; iOffset: longint): TeTokenTypeSet;
    function    ParentByTokenType(parTokenType: TeTokenTypeSet): TisStatement;
    procedure   SetToken(value: TcToken);
    procedure   LocalClear;
    function    GetPreferences: TcPreferenceList;
    procedure   SetPreferences(value: TcPreferenceList);
    procedure   AddObjectReference(parMacroType: TeMacroType; parType: TeTokenType; sparName, sparPrefix, sparAlias: String); overload;
    procedure   AddObjectReference(parMacroType: TeMacroType; value: TisStatement); overload;

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
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    Add(parObject: TcObject): longint; override;
    function    SendToObject(value: TObject): TObject; override;
    procedure   ForeCast(value: String; iOffset: longint);
    function    GetHTML(value: TecContentTypeSet; var bIssueRaised: boolean): String;

  public
    // Public properties
    //
    property   Token: TcToken                             read m_rToken                 write SetToken;
    property   iStart: longint                            read m_iStart                 write m_iStart;
    property   iEnd: longint                              read m_iEnd                   write m_iEnd;
    property   Errors: String                             read GetErrors                write SetErrors;
    property   TableList: TisTableList                    read GetTableList             write SetTableList;
    property   IndexList: TisIndexList                    read GetIndexList             write SetIndexList;
    property   IntelliSenseControl: TIntelliSenseControl  read m_objIntelliSenseControl write m_objIntelliSenseControl;
    property   PreferenceList: TcPreferenceList           read GetPreferences           write SetPreferences;
    property   sProperties: String                        read m_sProperties            write m_sProperties;
  end;

  TisTableList = class(TcObject)
  private
    // Private members
    //
    m_lstColumns: TcBag;
    m_objMetaData: TcObject;

   private
    // Private declarations
    //
    procedure   SetMetaData(value: TcObject);

 public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load(value: TcConnection): boolean; overload; virtual;
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public properties
    //
    property    Columns: TcBag          read m_lstColumns     write m_lstColumns;
    property    MetaData: TcObject      read m_objMetaData    write SetMetaData;
  end;

  TisIndexList = class(TisTableList)
  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load(value: TcConnection): boolean; override;
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    GetMatchingIndexes(parTable: String; parColumns: TStringList): TcBag;

  public
    // Public properties
    //
    property    Columns;
    property    MetaData;
  end;

  TisTable = class(TcObject);
  TisColumn = class(TcObject);

  TisIndex = class(TcObject)
  private
    // Private members
    //
    m_sTablename: String;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public properties
    //
    property    sTablename: String read m_sTablename write m_sTablename;
  end;

  TeisContent = (eisNone, eisContext, eisObjectList, eisPseudo, eisText, eisInfoColumns);

  TIntelliSenseControl = class(TCustomListBox)
  private
    // Private members
    //
    m_lstTables: TisTableList;
    m_lstIndexes: TisTableList;
    m_iCaretPos: longint;
    m_objControl: TSynEdit;
    m_tmTimer: TTimer;
    m_bIsTimed: boolean;
    m_lstContent: TcCollection;
    m_popList: TPopupMenu;
    m_mnuSelect: TMenuItem;
    m_mnuTableColumns: TMenuItem;
    m_sPseudoColumns: String;

  private
    // Private declarations
    //
    procedure   GetEntry;
    function    GetIndex(Value: string): longint;
    procedure   OnIntelliSenseControl_Click(Sender: TObject);
    procedure   OnIntelliSenseControl_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   OnIntelliSenseControl_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   SetSelStart(value: longint);
    function    GetSelStart: longint;
    procedure   SendToObject(value: TcBag; eparContent: TeisContent); overload;
    procedure   SendToObject(value: String; eparContent: TeisContent); overload;
    function    SetEntry(bDismiss: boolean): boolean; overload;
    function    SetEntry(value: String; bDismiss: boolean): boolean; overload;
    procedure   SetTimer(value: boolean);
    procedure   OnTimerTrigger(sender: TObject);
    procedure   SetBackgroundColor(value: TColor);
    procedure   SetTimerValue(value: longint);
    procedure   WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    function    CreateMenuItem(parMenu: TMenuItem; sCaption: String; dDefault, bChecked, bEnabled: boolean; objHandle: TNotifyEvent): TMenuItem;
    procedure   OnAddAllColumns(Sender: TObject);
    procedure   SetPseudoColumns(value: String);
    function    GetCanSense: boolean;

  protected
    // Protected declarations
    //
    procedure   DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    //
    // 2. Custom
    function    Display: boolean; overload;
    procedure   Display(parType: TeTokenType; parValue: String; parCurrentList: TcBag); overload;
    procedure   FilterKeyPress(var Key: Word; Shift: TShiftState; KeyState: TeKeyUpDown);
    procedure   Close(Sender: TObject);
    function    CanDisplay(Key: word): boolean;
    function    ObjectStructure_ColumnList(value: String): boolean;

  public
    // Public properties
    //
    property   Tables: TisTableList    read m_lstTables      write m_lstTables;
    property   Indexes: TisTableList   read m_lstIndexes     write m_lstIndexes;
    property   iCaretPos: longint       read m_iCaretPos      write m_iCaretPos;
    property   SelStart: longint        read GetSelStart      write SetSelStart;
    property   Control: TSynEdit        read m_objControl     write m_objControl;
    property   IsTimed: boolean         read m_bIsTimed       write m_bIsTimed;
    property   TimerValue: longint                            write SetTimerValue;
    property   BackgroundColor: TColor                        write SetBackgroundColor;
    property   sPseudoColumns: String   read m_sPseudoColumns write SetPseudoColumns;
    property   CanSense: boolean        read GetCanSense;
  end;

  TcIntelliSenseItem = class(TcObject)
  private
    // Private members
    //
    m_eisType: TeisContent;
    m_objData: TcObject;

 public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public properties
    //
    property    eisType: TeisContent    read m_eisType        write m_eisType;
    property    Data: TcObject          read m_objData        write m_objData;
  end;

implementation

uses
  Variants,
  Windows,
  strUtils,
  comctrls,
  math,
  DataLib;

const
  kaeROOTS: TeTokenTypeSet =
    [ettSelect, ettInsert, ettUpdate, ettDelete, ettDescribe, ettDrop, ettTruncate];
  kaeBUTNOT_FIELD: TeTokenTypeSet =
    [ettCase, ettOperation, ettUnion, ettIntersect, ettMinus, ettParenthese, ettBracket];
  kaeBUTNOT_WHERE: TeTokenTypeSet =
    [ettCase, ettUnion, ettIntersect, ettMinus];
  krsDEFAULTPSEUDO      = '*,null';
  kasMACROTYPE: array[TeMacroType] of String =
    (ksEMPTY, 'Table', 'Field', 'Where');


// Tool
//   StringToTokenType
//
function StringToTokenType(value: String): TeTokenType;
var
  e: TeTokenType;
const
  kasTOKENTYPE: array[TeTokenType] of String =
    (ksEMPTY, 'Select', '', 'From', 'Set', '', 'Where',
     'Group', 'Order', 'As', '', '', '',
     '', '', 'Union', 'Intersect', 'Minus', '(',
     '',
     '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
     '', '', '', '', '');
begin
  result := low(TeTokenType);
  for e := succ(low(TeTokenType)) to high(TeTokenType) do
    if (AnsiCompareText(kasTOKENTYPE[e], value) = 0) and (kasTOKENTYPE[e] <> ksEMPTY) then
    begin
      result := e;
      break;
    end;
end;

// Tool
//   TokenTypeSetToString
//
function TokenTypeSetToString(value: TeTokenTypeSet): String;
var
  e: TeTokenType;
begin
  result := ksEMPTY;
  for e := low(TeTokenType) to high(TeTokenType) do
    if e in value then
    begin
      if result <> ksEMPTY then
        result := result + ', ';
      result := result + kasTOKENTYPE[e];
    end;
end;

//
// TisBaseStatement
//

// TisBaseStatement
//   Create
//
constructor TisBaseStatement.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  LocalClear;
end;

// TisBaseStatement
//   Clear
//
procedure TisBaseStatement.Clear;
begin
  LocalClear;
  inherited Clear;
end;

// TisBaseStatement
//   LocalClear
//
procedure TisBaseStatement.LocalClear;
begin
  sName := ksEMPTY;
  m_eTokenType := ettUndefined;
  m_sAlias := ksEMPTY;
  m_sPrefix := ksEMPTY;
  m_eMacroType := emtUndefined;
end;

// TisBaseStatement
//   Text
//
function TisBaseStatement.Text: String;
begin
  result := sName + GetAlias;
  if m_sPrefix <> ksEMPTY then
    result := Format('%s.', [m_sPrefix]) + result;
end;

// TisBaseStatement
//   GetAlias
//
function TisBaseStatement.GetAlias: String;
begin
  result := ksEMPTY;
  if m_sAlias <> ksEMPTY then
  begin
    if m_eTokenType <> ettTable then
      result := result + ' <span style="font-weight:bolder;">as</span>';
    result := result + ' ' + m_sAlias;
  end;
end;

// TisBaseStatement
//   ChildOf
//
function TisBaseStatement.ChildOf(value: TeTokenType; parButNot: TeTokenTypeSet): boolean;
begin
  result := FALSE;
  if m_eTokenType = value then
    result := TRUE
  else if m_eTokenType in kaeROOTS then
    result := FALSE
  else if (Parent <> nil) and (Parent is TisBaseStatement) and not ((Parent as TisBaseStatement).eTokenType in parButNot) then
    result := (Parent as TisStatement).ChildOf(value, parButNot);
end;

// TisBaseStatement
//   GetColumnPrefix
//
function TisBaseStatement.GetColumnPrefix: string;
begin
  result := ksEMPTY;
  if m_sAlias <> ksEMPTY then
    result := m_sAlias + '.';
end;

//
// TisStatement
//

// TisStatement
//   Create
//
constructor TisStatement.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_lstAnalysis_Table := TcCollection.Create(nil);
  m_lstAnalysis_Column := TcCollection.Create(nil);
  m_lstErrors := nil;
  m_objTableList := nil;
  m_objIndexList := nil;
  m_objIntelliSenseControl := nil;
  m_objPreferences := nil;
  m_sProperties := ksEMPTY;
end;

// TisStatement
//   Destroy
//
destructor TisStatement.Destroy;
begin
  LocalClear;
  FreeAndNil(m_lstAnalysis_Table);
  FreeAndNil(m_lstAnalysis_Column);
  FreeAndNil(m_lstErrors);
  inherited Destroy;
end;

// TisStatement
//   Clear
//
procedure TisStatement.Clear;
begin
  LocalClear;
  inherited Clear;
end;

// TisStatement
//   LocalClear
//
procedure TisStatement.LocalClear;
begin
  m_lstAnalysis_Table.Clear;
  m_lstAnalysis_Column.Clear;
  if m_lstErrors <> nil then
    m_lstErrors.Clear;
end;

// TisStatement
//   Add
//
function TisStatement.Add(parObject: TcObject): longint;
begin
  result := IndexOf(parObject);
  if result = kiUNDEFINED then
    result := inherited Add(parObject);
end;

// TisStatement
//   SendToObject
//
function TisStatement.SendToObject(value: TObject): TObject;
var
  i: longint;
begin
  result := nil;
  if (value <> nil) and (value is TTreeView) then
  begin
    (value as TTreeView).Items.BeginUpdate;
    (value as TTreeView).Items.Clear;
    result := (value as TTreeView).Items.AddChildObject(nil, Text, self);
    SendToObject(result);
    (value as TTreeView).FullExpand;
    (value as TTreeView).Items.EndUpdate;
  end
  else if (value <> nil) and (value is TTreeNode) then
  begin
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TisStatement) then
      begin
        result := ((value as TTreeNode).TreeView as TTreeView).Items.AddChildObject(value as TTreeNode, Objects[i].Text, Objects[i]);
        Objects[i].SendToObject(result);
      end;
  end;
end;

// TisStatement
//   ExpectedNext
//
function TisStatement.ExpectedNext(value: String; var stmtParent: TisStatement; iOffset: longint): TeTokenTypeSet;
var
  strm: TcTokenStream;
begin
  result := [];
  strm := nil;
  stmtParent := nil;
  try
    strm := StringToStream(value);
    strm.SingleQuoteMarker := TRUE;
    try
      if not strm.EOS then
        Parse(strm, stmtParent);
    except
      on E:ParsingException do
      begin
        result := E.Expected;
        stmtParent := TisStatement(E.ParsedObject);
      end;
    end;
  finally
    strm.free;
  end;
  SetStartEnd(iOffset);
  Analyze;
end;

// TisStatement
//   Parse (1)
//
function TisStatement.Parse(value: string): boolean;
var
  strm: TcTokenStream;
  p: TisStatement;
begin
  result := FALSE;
  strm := nil;
  try
    strm := StringToStream(value);
    strm.SingleQuoteMarker := TRUE;
    try
      if not strm.EOS then
        result := Parse(strm, p);
    except
      on E:ParsingException do
        result := FALSE;
    end;
  finally
    strm.free;
  end;
  SetStartEnd(0);
  Analyze;
end;

// TisStatement
//   Parse (1)
//
function TisStatement.Parse(value: TcTokenStream; var stmtParent: TisStatement): boolean;

  // Boolean Expression
  //   bExpr          -> bterm { 'or' bterm }
  //   bTerm          -> bfactor { 'and' bfactor }
  //   bFactor        -> 'not' bfactor | expr bfactor2 | '(' bExpr ')'
  //   bFactor2       -> relop expr | 'in' '(' expr { ',' expr } ')'
  //   relop          -> '<' | '<=' | '<>' | '>' | '>=' | '='
  //
  // Expression
  //   Expr           -> term { addop term }
  //   Term           -> factor { mulop factor }
  //   Factor         -> number | '(' bExpr ')' | 'not' factor | function
  //   Addop          -> '+' | '-' | '||' | '|'
  //   Mulop          -> '*' | '/' | '&'
  //   Function       -> string '(' [ bExpr { ',' bExpr } ]

  // Tool
  //   IsMulOp
  //     Mulop -> '*' | '/' | '&'
  //
  function IsMulOp(value: TvKeyword): boolean;
  begin
    result := value in [_MULT, _DIV, _B_AND];
  end;

  // Tool
  //   IsAddOp
  //     Addop -> '+' | '-' | '||' | '|'
  //
  function IsAddOp(value: TvKeyword): boolean;
  begin
    result := value in [_ADD, _SUBSTRACT, _CONCAT, _B_OR];
  end;

  // Tool
  //   IsRelOp
  //
  function IsRelOp(value: TvKeyword): boolean;
  begin
    result := value in [_LOWER, _LOWEREQUAL, _NOTEQUAL, _GREATER, _GREATEREQUAL, _EQUAL, _LIKE, __NOTLIKE, _IN, __NOTIN, _IS, __ISNOT];
  end;

  // Forwards
  //
  function Expr(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; forward;
  function bExpr(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; forward; // bExpr -> bterm { 'or' bterm }
  procedure ProcessAlias(strm: TcTokenStream; value: TisStatement; parToken: TeTokenType); forward;

  // Tool
  //   InsertParent (2)
  //
  function InsertParent(parStream: TcTokenStream; parTokenID: TvKeyword; parNode: TisStatement; parType: TeTokenType; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; overload;
  begin
    result := TisStatement.Create(parNode.Parent);
    parNode.Parent.Add(result);
    parNode.Remove;
    result.Add(parNode);
    parNode.Parent := result;
    result.sName := parStream.Token.value;
    result.Token := parStream.Token;
    result.eTokenType := parType;
    if parType = ettUndefined then
      result.SetTokenType(parStream.Token, parMode);
    m_esExpected := parExpected;
    parStream.Match(parTokenID);
  end;

  // Tool
  //   InsertParent (1)
  //
  function InsertParent(parStream: TcTokenStream; parNode: TisStatement; parType: TeTokenType; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; overload;
  begin
    result := InsertParent(parStream, parStream.Token.ID, parNode, parType, parExpected, parMode);
  end;

  // Tool
  //   ProcessSet
  //
  function ProcessSet(parStream: TcTokenStream; res: TisStatement; parType: TeTokenType; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement;
  begin
    result := InsertParent(parStream, res, parType, parExpected, parMode);
    bExpr(parStream, result, parExpected, parMode);
  end;

  // Tool
  //   CreateItem
  //
  function CreateItem(parParent: TisStatement; parToken: TcToken; parType: TeTokenType; parExpected: TeTokenTypeSet; parMode: TeTokenType; parbMatch: boolean): TisStatement; overload;
  begin
    result := TisStatement.Create(parParent);
    parParent.Add(result);
    result.sName := parToken.value;
    result.Token := parToken;
    result.eTokenType := parType;
    if parType = ettUndefined then
      result.SetTokenType(parToken, parMode);
    m_esExpected := parExpected;
    if parbMatch then
      value.Match(parToken.ID, parExpected);
  end;

  // bfactor -> '(' bExpr ')' | 'not' bfactor | expr bfactor2
  // bfactor2 -> relop expr | 'in' '(' expr { ',' expr } ')'
  function bFactor(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement;
  begin
    with parStream do
      case Token.ID of
        _NOT:
          begin
            result := CreateItem(parParent, Value.Token, ettOperation, [], parMode, TRUE);
            bFactor(parStream, result, [ettNumber, ettColumn, ettString], parMode);
          end;
        else
        begin
          result := Expr(parStream, parParent, parExpected, parMode);   // The next expression could just be 'if variable then [..]',
          if IsRelOP(Token.ID) then      // thus avoiding any more processing. The p assignment resolves that.
          begin
            result := InsertParent(parStream, result, ettOperation, parExpected, parMode);
            if result.Token.ID in [_IN, __NOTIN] then
            begin
              result := CreateItem(result, Value.Token, ettParenthese, [], parMode, FALSE);
              Match(_LPAREN);
              Expr(parStream, result, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
              while Token.ID = _COMMA do
              begin
                Match(_COMMA);
                Expr(parStream, result, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
              end;
              Match(_RPAREN);
              // Alias
              ProcessAlias(parStream, result, ettParenthese);
            end
            else
              Expr(parStream, result, [ettNumber, ettString, ettColumn, ettFunction], parMode);
          end;
        end
      end;
  end;

  // Tool
  //   bTerm
  //
  function bTerm(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; // bterm -> bfactor { 'and' bfactor }
  begin
    with parStream do
    begin
      result := bFactor(parStream, parParent, parExpected, parMode);
      while Token.ID = _AND do
      begin
        result := InsertParent(parStream, result, ettOperation, parExpected, parMode);
        result := bFactor(parStream, result, parExpected, parMode);
      end;
    end;
  end;

  // Tool
  //   bExpr
  //
  function bExpr(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement; // bExpr -> bterm { 'or' bterm }
  begin
    with parStream do
    begin
      result := bTerm(parStream, parParent, parExpected, parMode);
      while Token.ID = _OR do
      begin
        result := InsertParent(parStream, result, ettOperation, parExpected, parMode);
        bTerm(parStream, result, parExpected, parMode);
      end;
    end;
  end;

  // Tool
  //   Factor
  //
  function Factor(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement;

    function NoValue(value: TcToken): TcToken;
    begin
      result := value;
      result.Value := ksEMPTY;
    end;

  var
    p, q, r: TisStatement;
    b: boolean;
  begin
    with parStream do
    begin
      case Token.ID of
        _NUMBER, _REAL:
          begin
            result := CreateItem(parParent, Token, ettNumber, parExpected, parMode, TRUE);
            ProcessAlias(value, result, ettUndefined);
          end;
        _LPAREN:
          begin
            result := CreateItem(parParent, Value.Token, ettParenthese, [], parMode, FALSE);
            Match(_LPAREN); // woops
            p := bExpr(parStream, result, [ettInlineView, ettString, ettNumber, ettFunction], parMode);
            Match(_RPAREN);
            // Alias
            ProcessAlias(parStream, result, ettParenthese);
            // Set Operations
            if parStream.Token.ID in [__MINUS, __INTERSECT, __UNION] then
              result := ProcessSet(parStream, p, ettOperation, [ettInlineView], parMode);
          end;
        _NOT:
          begin
            result := CreateItem(parParent, Value.Token, ettOperation, [], parMode, TRUE);
            Factor(parStream, result, [ettString, ettNumber, ettColumn, ettFunction], parMode);
          end;
        // Case when then?
        __CASE:
          begin
            result := CreateItem(parParent, Value.Token, ettCase, parExpected, parMode, TRUE);
            // First Expression
            if value.Token.ID <> __WHEN then
            begin
              p := CreateItem(result, NoValue(Value.Token), ettField, [], parMode, TRUE);
              bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
            end;
            // c. When Statements
            while value.Token.ID = __WHEN do
            begin
              // When boolean Expression
              p := CreateItem(result, NoValue(Value.Token), ettWhen, [], parMode, TRUE);
              bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
              // Then declaration
              p := CreateItem(result, NoValue(Value.Token), ettThen, [], parMode, TRUE);
              bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
            end;
            // d. Else Statement?
            if value.Token.ID = __ELSE then
            begin
              p := CreateItem(result, Value.Token, ettElse, [], parMode, TRUE);
              bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
            end;
            // e. End Statement
            CreateItem(result, Value.Token, ettEnd, [], parMode, TRUE);
            ProcessAlias(value, result, ettUndefined);
          end;
        // Bracketed column / Table
        _LBRACKET:
          begin
            result := CreateItem(parParent, Value.Token, ettBracket, [], parMode, FALSE);
            Match(_LBRACKET); // woops
            bExpr(parStream, result, [ettString], parMode);
            Match(_RBRACKET);
            // Alias
            ProcessAlias(parStream, result, ettBracket);
          end;
        _STRING, _MULT, __LOWER, __UPPER:
          begin
            result := CreateItem(parParent, Value.Token, ettUndefined, parExpected, parMode, TRUE);
            if result.Token.HasQuote then
              result.m_eTokenType := ettString
            else
              result.m_eTokenType := parMode;
            // Is this a prefix?
            while value.Token.ID = _PERIOD do
            begin
              if result.sPrefix <> ksEMPTY then
                result.sPrefix := result.sPrefix + '.';
              result.sPrefix := result.sPrefix + result.sName;
              Value.Match(_PERIOD);
              result.sName := value.Token.Value;
              result.ExtendToken(value.Token);
              result.SetTokenType(value.Token, parMode);
              if value.Token.ID = _MULT then
                value.Match(_MULT, [parMode])
              else
                value.Match(_STRING, [parMode]);
            end;
            // Function?
            if (value.Token.ID = _LPAREN) then
            begin
              p := CreateItem(result, Value.Token, ettParenthese, [], parMode, FALSE);
              value.Match(_LPAREN);
              result.eTokenType := ettFunction;
              if value.Token.ID <> _RPAREN then
              begin
                bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
                while (value.Token.ID = _COMMA) and not value.EOS do
                begin
                  value.Match(_COMMA);
                  bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], parMode);
                end;
              end;
              value.Match(_RPAREN);
            end;
            ProcessAlias(value, result, ettUndefined);
          end;
        //
        // Select Statement
        //
        __SELECT:
          begin
            result := CreateItem(parParent, NoValue(Value.Token), ettSelect, parExpected, parMode, TRUE);
            // Select Distinct
            if value.Token.ID = __DISTINCT then
              CreateItem(result, value.Token, ettProperty, [], parMode, TRUE);
            //
            // Select: Fields
            p := CreateItem(result, NoValue(Value.Token), ettField, [], ettColumn, FALSE);
            bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            while not value.EOS and (value.Token.ID = _COMMA) do
            begin
              value.Match(_COMMA);
              bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            end;
            //
            // Select: From Clause
            if not value.EOS then
            begin
              p := CreateItem(result, NoValue(Value.Token), ettFrom, [ettFrom], parMode, TRUE);
              repeat
                //
                // a. table declaration
                 bExpr(value, p, [ettTable, ettInlineView], ettTable);
                //
                // b. inner/outer join
                //      SELECT * FROM employee CROSS JOIN department
                //      SELECT * FROM employee INNER JOIN department ON employee.DepartmentID = department.DepartmentID
                //      SELECT * FROM employee LEFT JOIN department ON employee.DepartmentID = department.DepartmentID
                //      SELECT * FROM employee LEFT OUTER JOIN department ON department.DepartmentID = employee.DepartmentID
                //      SELECT * FROM employee NATURAL JOIN department
                //      SELECT * FROM employee RIGHT JOIN department ON employee.DepartmentID = department.DepartmentID
                //      SELECT * FROM employee RIGHT OUTER JOIN department ON employee.DepartmentID = department.DepartmentID
                while (value.Token.ID in [__CROSS, _INNER, __LEFT, __NATURAL, __RIGHT, __JOIN, __OUTER]) do
                begin
                  q := CreateItem(p, NoValue(Value.Token), ettJoin, [ettJoin], parMode, FALSE);
                  if value.Token.ID in [__CROSS, _INNER, __LEFT, __NATURAL, __RIGHT, __OUTER] then
                    Value.Match(value.Token.ID);
                  if value.Token.ID in [__CROSS, _INNER, __LEFT, __NATURAL, __RIGHT, __OUTER] then
                    Value.Match(value.Token.ID);
                  Value.Match(__JOIN);
                  bExpr(value, q, [ettTable, ettInlineView], ettTable);
                  if value.Token.ID = __ON then
                  begin
                    r := CreateItem(q, NoValue(Value.Token), ettOn, [ettOn], parMode, TRUE);
                    bExpr(value, r, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
                  end;
                end;
                //
                // c. next table.
                b := not value.EOS and (value.Token.ID = _COMMA);
                if b then
                  value.Match(_COMMA);
              until not b;
              //
              // Select: Where Clause
              if value.Token.ID = __WHERE then
              begin
                p := CreateItem(result, NoValue(value.Token), ettWhere, [ettWhere], parMode, TRUE);
                bExpr(value, p, [ettColumn, ettNumber, ettString, ettFunction], ettColumn);
              end;
              //
              // Select: Group By Clause
              if value.Token.ID = __GROUP then
              begin
                value.Match(__GROUP);
                value.Match(__BY);
                p := CreateItem(result, NoValue(value.Token), ettGroupBy, parExpected, parMode, FALSE);
                bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
                while not value.EOS and (value.Token.ID = _COMMA) do
                begin
                  value.Match(_COMMA);
                  bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
                end;
              end;
              //
              // Select: Order By Clause
              if value.Token.ID = __ORDER then
              begin
                p := CreateItem(result, NoValue(value.Token), ettOrderby, parExpected, parMode, TRUE);
                value.Match(__BY);
                q := bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
                while value.Token.ID in [__ASC, __DESC, __NULLS, __LAST] do
                begin
                  q.sProperties := q.sProperties + value.Token.PreceedingSpaces + value.Token.Value;
                  value.Match(value.Token.ID);
                end;
                while not value.EOS and (value.Token.ID = _COMMA) do
                begin
                  value.Match(_COMMA);
                  q := bExpr(value, p, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
                  while value.Token.ID in [__ASC, __DESC, __NULLS, __LAST] do
                  begin
                    q.sProperties := q.sProperties + value.Token.PreceedingSpaces + value.Token.Value;
                    value.Match(value.Token.ID);
                  end;
                end;
              end;
            end;
            //
            // Set Operations
            if parStream.Token.ID in [__MINUS, __INTERSECT, __UNION] then
              result := ProcessSet(parStream, result, ettOperation, [ettInlineView], parMode);
          end;
        //
        // Update Statement
        //
        __UPDATE:
          begin
            // Update
            result := CreateItem(parParent, NoValue(Value.Token), ettUpdate, parExpected, parMode, TRUE);
            // Table Statement
            p := CreateItem(result, NoValue(Value.Token), ettDataset, [], parMode, FALSE);
            bExpr(value, p, [ettField], ettTable);
            // Set Statement
            p := CreateItem(result, NoValue(Value.Token), ettSet, [], parMode, TRUE);
            q := Factor(value, p, [ettColumn], ettColumn);
            q := InsertParent(parStream, _EQUAL, q, ettOperation, parExpected, parMode);
            bExpr(value, q, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            while not value.EOS and (value.Token.ID = _COMMA) do
            begin
              value.Match(_COMMA);
              // Column = value
              q := Factor(value, p, [ettColumn], ettColumn);
              q := InsertParent(parStream, _EQUAL, q, ettOperation, parExpected, parMode);
              bExpr(value, q, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            end;
            //
            // Where Clause
            if value.Token.ID = __WHERE then
            begin
              p := CreateItem(result, NoValue(value.Token), ettWhere, [ettWhere], parMode, TRUE);
              bExpr(value, p, [ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            end;
          end;
        //
        // Insert Statement
        //
        __INSERT:
          begin
            // Insert into
            result := CreateItem(parParent, NoValue(Value.Token), ettInsert, parExpected, parMode, TRUE);
            // Table
            p := CreateItem(result, NoValue(Value.Token), ettInto, [ettInto], parMode, TRUE);
            bExpr(value, p, [ettTable], ettTable);
            //
            // Named Columns?
            if Value.Token.ID = _LPAREN then
            begin
              Match(_LPAREN);
              p := CreateItem(result, Value.Token, ettParenthese, [], parMode, FALSE);
              bExpr(value, p, [ettColumn], ettColumn);
              while not value.EOS and (value.Token.ID = _COMMA) do
              begin
                value.Match(_COMMA);
                bExpr(value, p, [ettColumn], ettColumn);
              end;
              Match(_RPAREN);
            end;
            //
            // Values?
            if Value.Token.ID = __VALUES then
            begin
              p := CreateItem(result, NoValue(value.Token), ettValues, parExpected, parMode, TRUE);
              q := CreateItem(p, Value.Token, ettParenthese, [], parMode, FALSE);
              Match(_LPAREN);
              bExpr(value, q, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
              while not value.EOS and (value.Token.ID = _COMMA) do
              begin
                value.Match(_COMMA);
                bExpr(value, q, [ettInlineView, ettColumn, ettNumber, ettString, ettFunction], ettColumn);
              end;
              Match(_RPAREN);
            end
            else
              //
              // else, Select Statement
              bExpr(value, result, [ettInlineView], ettColumn);
        end;
        //
        // Delete Statement
        //
        __DELETE:
          begin
            // Insert into
            result := CreateItem(parParent, NoValue(Value.Token), ettDelete, parExpected, parMode, TRUE);
            if Value.Token.ID = __FROM then
              Match(__FROM);
            // Table
            p := CreateItem(result, NoValue(Value.Token), ettFrom, [ettFrom], parMode, FALSE);
            bExpr(value, p, [ettTable], ettTable);
            // Where Clause
            if value.Token.ID = __WHERE then
            begin
              p := CreateItem(result, NoValue(value.Token), ettWhere, [ettWhere], parMode, TRUE);
              bExpr(value, p, [ettColumn, ettNumber, ettString, ettFunction], ettColumn);
            end;
          end;
        //
        // Delete Statement
        //
        __DESC, __DESCRIBE:
          begin
            // Decsribe Statement
            result := CreateItem(parParent, NoValue(Value.Token), ettDescribe, parExpected, parMode, TRUE);
            // Table
            p := CreateItem(result, NoValue(Value.Token), ettFrom, [ettInto], parMode, TRUE);
            bExpr(value, p, [ettTable], ettTable);
          end;

        //
        // Drop Statement
        //
        __DROP:
          begin
            // Drop Statement
            result := CreateItem(parParent, NoValue(Value.Token), ettDrop, parExpected, parMode, TRUE);
            Match(_WILDCARD);
            // Table
            p := CreateItem(result, NoValue(Value.Token), ettFrom, [ettDataset], parMode, TRUE);
            bExpr(value, p, [ettTable], ettTable);
          end;

        //
        // Truncate Statement
        //
        __TRUNCATE:
          begin
            // Decsribe Statement
            result := CreateItem(parParent, NoValue(Value.Token), ettTruncate, parExpected, parMode, TRUE);
            Match(_WILDCARD);
            // Table
            p := CreateItem(result, NoValue(Value.Token), ettFrom, [ettDataset], parMode, TRUE);
            bExpr(value, p, [ettTable], ettTable);
          end;

      //
      // Unexpected input!!
      else
        raise ParsingException.Create(Error(Format(krsUNEXPECTEDTOKEN, [NullIf(Token.Value <> ksEMPTY, Token.Value, _TOKENS[Token.ID])])), parStream.Token, parExpected, parParent);
    end;
    end;
  end;

  // Tool
  //   Term
  //
  function Term(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement;
  begin
    with parStream do
    begin
      result := Factor(parStream, parParent, parExpected, parMode);
      while IsMulOp(Token.ID) do
      begin
        result := InsertParent(parStream, result, ettOperation, parExpected, parMode);
        Factor(parStream, result, parExpected, parMode);
      end;
    end;
  end;

  // Tool
  //   Expr
  //
  function Expr(parStream: TcTokenStream; parParent: TisStatement; parExpected: TeTokenTypeSet; parMode: TeTokenType): TisStatement;
  begin
    with parStream do
    begin
      result := Term(parStream, parParent, parExpected, parMode);
      while IsAddOp(Token.ID) do
      begin
        result := InsertParent(parStream, result, ettOperation, parExpected, parMode);
        Term(parStream, result, parExpected, parMode);
      end;
    end;
  end;

  // Tool
  //   ProcessAlias
  //
  procedure ProcessAlias(strm: TcTokenStream; value: TisStatement; parToken: TeTokenType);
  var
    r: TisStatement;
  const
    kaePROPERPARENT = [ettField, ettFrom, ettWhere];
  begin
    if ( not (strm.Token.ID in [_COMMA, _RPAREN, _RBRACKET, __THEN, __ELSE, __END, _SEMICOLON,
                                __SET, _IS, __ISNOT, __WHEN, __CROSS, _INNER, __LEFT, __NATURAL,
                                __RIGHT, __JOIN, __ON, _NOT, __VALUES, __ASC, __DESC]) and
         not (strm.Token.ID in [_AND, _OR, _B_AND, _B_OR]) and
         not IsMulOp(strm.Token.ID) and
         not IsAddOp(strm.Token.ID) and
         not IsRelOp(strm.Token.ID) and
         (StringToTokenType(strm.Token.Value) = ettUndefined)
        ) or
       (strm.Token.ID = __AS) then
    begin
      if strm.Token.ID = __AS then
        strm.Match(__AS);
      // Find a proper parent
      r := value;
      while (r <> nil) and
            (r.parent <> nil) and
            (r.Parent is TisStatement) and
            not ((r.Parent as TisStatement).eTokenType in kaePROPERPARENT) do
        r := r.Parent as TisStatement;
      // Set Alias
      if r <> nil then
      begin
        r.sAlias := strm.Token.Value;
        r.ExtendToken(strm.Token);
      end;
      strm.Match(_WILDCARD, [ettString]);
    end;
  end;

var
  p: TisStatement;
begin
  ClearErrors;
  p := TisStatement.Create(nil);
  Add(p);
  result := bExpr(value, p, [], ettColumn) <> nil;
end;

// TisStatement
//   SetStartEnd
//
procedure TisStatement.SetStartEnd(iOffset: longint);
var
  i: longint;
begin
  if count = 0 then
  begin
    m_iStart := iOffset + m_rToken.Pos;
    m_iEnd := m_iStart + m_rToken.Len - 1;
  end
  else
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TisStatement) then
      begin
        (Objects[i] as TisStatement).SetStartEnd(iOffset);
        if i = 0 then
          m_iStart := iOffset + min(m_rToken.Pos, (Objects[i] as TisStatement).iStart);
        if i = count - 1 then
          m_iEnd := max(m_iStart + m_rToken.Len - 1, (Objects[i] as TisStatement).iEnd);
      end;
end;

// TisStatement
//   SetStartEnd
//
procedure TisStatement.ExtendToken(value: TcToken);
begin
  m_rToken.Len := value.Pos - m_rToken.Pos + value.Len;
end;

// TisStatement
//   PosToStatementPart
//
function TisStatement.PosToStatementPart(value: longint): TisStatement;
var
  i: longint;
  p: TisStatement;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TisStatement) and ((Objects[i] as TisStatement).iStart <= value) and ((Objects[i] as TisStatement).iEnd >= value) then
    begin
      result := Objects[i] as TisStatement;
      p := result.PosToStatementPart(value);
      if p <> nil then
        result := p;
      break;
    end;
end;

// TisStatement
//   SetTokenType
//
procedure TisStatement.SetTokenType(value: TcToken; parMode: TeTokenType);
begin
  if IsNumber(Value.Value) then
    m_eTokenType := ettNumber
  else if IsReal(value.Value) then
    m_eTokenType := ettReal
  else if value.HasQuote then
  begin
    if value.QuoteSymbol = kcSGLQUOTE then
      m_eTokenType := ettString
    else if value.QuoteSymbol = kcDBLQUOTE then
    begin
      m_eTokenType := ettColumn;
      if (parent <> nil) and (parent is TisStatement) and ((parent as TisStatement).eTokenType = ettFrom) then
        m_eTokenType := ettDataset;
    end
  end
  else if AnsiCompareText(value.Value, 'null') = 0 then
    m_eTokenType := ettNull
  else
    (*
    m_eTokenType := ettColumn;
    if (parent <> nil) and (parent is TisStatement) and ((parent as TisStatement).eTokenType in [ettFrom, ettInto, ettJoin]) then
      m_eTokenType := ettDataset;
    *)
    m_eTokenType := parMode;
end;

// TisStatement
//   SetErrors (1)
//
procedure TisStatement.SetErrors(value: String);
begin
  SetErrors(value, EmptyToken(), []);
end;

// TisStatement
//   SetErrors (2)
//
procedure TisStatement.SetErrors(value: String; Token: TcToken; esExpected: TeTokenTypeSet);
var
  p: TisStatementException;
begin
  if (parent <> nil) and (parent is TisStatement) and (parent <> self) then
    (parent as TisStatement).SetErrors(value, Token, esExpected)
  else
  begin
    if m_lstErrors = nil then
      m_lstErrors := TcCollection.Create(nil);
    if m_lstErrors.Find(value) = nil then
    begin
      p := TisStatementException.Create(nil);
      m_lstErrors.Add(p);
      p.Token := Token;
      p.sName := value;
      p.Expected := esExpected;
    end;
  end;
end;

// TisStatement
//   GetErrors
//
function TisStatement.GetErrors: String;
begin
  result := ksEMPTY;
  if (parent <> nil) and (parent is TisStatement) and (parent <> self) then
    result := (parent as TisStatement).GetErrors
  else if m_lstErrors <> nil then
    result := m_lstErrors.Text;
end;

// TisStatement
//   ClearErrors
//
procedure TisStatement.ClearErrors;
begin
  FreeAndNil(m_lstErrors);
  if (parent <> nil) and (parent is TisStatement) and (parent <> self) then
    (parent as TisStatement).ClearErrors;
end;

// TisStatement
//   Analyze
//
procedure TisStatement.Analyze;

  // Tool
  //   AddFieldItemToParent
  //
  procedure AddFieldItemToParent(pRoot: TisStatement; parFieldItem: TisStatement);
  var
    p, r: TisStatement;
    s: String;
  begin
    if (pRoot.Parent <> nil) and (pRoot.Parent is TisStatement) and ((pRoot.Parent as TisStatement).eTokenType = ettParenthese) then
    begin
      p := pRoot.Parent as TisStatement;
      r := pRoot.ParentByTokenType(kaeROOTS);
      if r <> nil then
      begin
        s := parFieldItem.sName;
        if parFieldItem.sAlias <> ksEMPTY then
          s := parFieldItem.sAlias;
        r.AddObjectReference(emtField, ettColumn, s, p.sAlias, ksEMPTY);
      end;
    end;
  end;

  // Tool
  //   SubAnalysis
  //
  procedure SubAnalysis(value: TisStatement);
  var
    i: longint;
    p: TisStatement;
    e: TeMacroType;
  begin
    // a. Root object
    if value.eTokenType in kaeROOTS then
    begin
      value.m_lstAnalysis_Column.Clear;
      value.m_lstAnalysis_Table.Clear;
    end
    // b. Table..
    else if (value.eTokenType = ettTable) or
            ( (value.eTokenType in [ettParenthese]) and
              (value.sAlias <> ksEMPTY) and
              (value.Parent <> nil) and
              (value.Parent is TisStatement) and
              ((value.Parent as TisStatement).eTokenType = ettFrom)
            ) then
    begin
      value.eMacroType := emtTable;
      p := value.ParentByTokenType(kaeROOTS);
      if p <> nil then
        p.AddObjectReference(emtTable, value);
    end
    // c. Column..
    else if (value.eTokenType = ettColumn) or
            ((value.eTokenType in [ettFunction, ettNumber, ettString, ettCase, ettOperation,
                                   ettUnion, ettIntersect, ettMinus, ettParenthese, ettInlineView,
                                   ettReference, ettReal, ettNull]) and (value.sAlias <> ksEMPTY)) then
    begin
      p := value.ParentByTokenType(kaeROOTS);
      if p <> nil then
      begin
        e := emtUndefined;
        if value.ChildOf(ettField, kaeBUTNOT_FIELD) then
          e := emtField
        else if value.ChildOf(ettWhere, kaeBUTNOT_WHERE) then
          e := emtWhere;
        value.eMacroType := e;
        p.AddObjectReference(e, value);
        if e = emtField then
          AddFieldItemToParent(p, value);
      end;
    end;
    // Explore children
    for i := 0 to value.Count - 1 do
      if (value[i] <> nil) and (value[i] is TisStatement) then
        SubAnalysis(value[i] as TisStatement);
  end;

begin
  SubAnalysis(self);
end;

// TisStatement
//   ParentByTokenType
//
function TisStatement.ParentByTokenType(parTokenType: TeTokenTypeSet): TisStatement;
var
  p: TisStatement;
begin
  result := nil;
  if (parent <> nil) and (parent is TisStatement) then
  begin
    p := parent as TisStatement;
    if p.eTokenType in parTokenType then
      result := p
    else
      result := p.ParentByTokenType(parTokenType);
  end;
end;

// TisStatement
//   GetTableList
//
function TisStatement.GetTableList: TisTableList;
begin
  result := nil;
  if (TopParent <> nil) and (TopParent is TisStatement) then
    result := (TopParent as TisStatement).m_objTableList;
end;

// TisStatement
//   SetTableList
//
procedure TisStatement.SetTableList(value: TisTableList);
begin
  if (TopParent <> nil) and (TopParent is TisStatement) then
    (TopParent as TisStatement).m_objTableList := value;
end;

// TisStatement
//   GetIndexList
//
function TisStatement.GetIndexList: TisIndexList;
begin
  result := nil;
  if (TopParent <> nil) and (TopParent is TisStatement) then
    result := (TopParent as TisStatement).m_objIndexList;
end;

// TisStatement
//   SetIndexList
//
procedure TisStatement.SetIndexList(value: TisIndexList);
begin
  if (TopParent <> nil) and (TopParent is TisStatement) then
    (TopParent as TisStatement).m_objIndexList := value;
end;

// TisStatement
//   ForeCast
//
procedure TisStatement.ForeCast(value: String; iOffset: longint);
var
  e: TeTokenTypeSet;
  p: TisStatement;
  c, d: TcObject;
  lst: TcCollection;
  bag: TcBag;
  i, j: longint;
  t: TisBaseStatement;
begin
  Clear;
  lst := nil;
  if value <> ksEMPTY then
  try
    if Assigned(m_objIntelliSenseControl) then
    try
      lst := TcCollection.Create(nil);
      e := ExpectedNext(value, p, iOffset);
      if e <> [] then
      begin
        if p <> nil then
          p := p.ParentByTokenType(kaeROOTS);
        if p <> nil then
        begin
          //
          // Find all the related tables
          if ettTable in e then
          begin
            lst.Copy(p.m_lstAnalysis_Table);
            for i := 0 to p.m_lstAnalysis_Column.count - 1 do
            try
              bag := m_objIntelliSenseControl.Tables.Columns.FindAll(p.m_lstAnalysis_Column[i].sName);
              for j := 0 to bag.Count - 1 do
                if (bag[j] <> nil) and (bag[j].Parent <> nil) and (lst.Find(bag[j].Parent.sName) = nil) then
                begin
                  d := bag[j].Parent.CreateSame(nil);
                  d.Copy(bag[j].Parent);
                  lst.Add(d);
                end;
            finally
              FreeAndNil(bag);
            end;
            if lst.count > 0 then
              lst.QuickSort(0, lst.count - 1);
          end
          //
          // Find all the related columns
          else if ettColumn in e then
          begin
            lst.copy(p.m_lstAnalysis_Column);
            for i := 0 to p.m_lstAnalysis_Table.count - 1 do
            begin
              t := p.m_lstAnalysis_Table[i] as TisBaseStatement;
              c := m_objIntelliSenseControl.Tables.Find(t.sName);
              if c <> nil then
                for j := 0 to c.Count - 1 do
                begin
                  d := c[j].CreateSame(nil);
                  d.Parent := c;
                  d.Copy(c[j]);
                  d.sName := t.GetColumnPrefix + d.sName;
                  lst.Add(d);
                end;
            end;
            if lst.count > 0 then
              lst.QuickSort(0, lst.count - 1);
          end;
        end;
        if ettTable in e then
          m_objIntelliSenseControl.Display(ettTable, ksEMPTY, lst)
        else if ettColumn in e then
          m_objIntelliSenseControl.Display(ettColumn, ksEMPTY, lst);
      end;
      m_objIntelliSenseControl.GetEntry;
    except
      on E: ParsingException do
        m_objIntelliSenseControl.SelStart := E.Token.Pos;
    end;
  finally
    lst.free;
  end;
end;

// TisStatement
//   SetToken
//
procedure TisStatement.SetToken(value: TcToken);
begin
  m_rToken := value;
  if sName = ksEMPTY then
    sName := value.Value;
end;

// TisStatement
//   GetHTML
//
function TisStatement.GetHTML(value: TecContentTypeSet; var bIssueRaised: boolean): String;

  (*
  // Tool
  //   GetSubHTML1
  //
  function GetSubHTML1(value: TisBaseStatement; parDepth: longint): String;
  var
    i: longint;
    s: String;
  begin
    result := Format('<div class="SmallIndent"><span style="background-color: yellow;">%s</span> %s', [kasTOKENTYPE[value.m_eTokenType], value.Text]);
    if value.sAlias <> ksEMPTY then
      result := result + Format('&nbsp;&nbsp;<span style="color:purple;">%s</span>', [value.sAlias]);
    if value.eMacroType <> emtUndefined then
      result := result + Format('&nbsp;&nbsp;<span style="color:red;">%s</span>', [kasMACROTYPE[value.eMacroType]]);
    s := ksEMPTY;
    if value.m_eTokenType in [ettField, ettFrom, ettWhere, ettGroupBy, ettOrderBy] then
      s := ', ';
    result := result + ksCR;
    for i := 0 to value.Count - 1 do
      if (value[i] <> nil) and (value[i] is TisBaseStatement) then
        result := result + GetSubHTML1(value[i] as TisBaseStatement, parDepth + 1);
    result := result + '</div>';
  end;
  *)

  // Tool
  //   GetSubHTML2
  //
  function GetSubHTML2(value: TisStatement; parDepth: longint; parCR: boolean; parSep: String): String;

    // Tool
    //   GetKeyword (1)
    //
    function GetKeyword(value: TisStatement; p: TcPreferenceList; sColorType: String): String; overload;
    begin
      result := kasTOKENTYPE[value.m_eTokenType];
      if p <> nil then
        result := Format('<span style="color:#%s;">%s</span>', [ColorToHex(p.Color[sColorType]), result]);
    end;

    // Tool
    //   GetKeyword (2)
    //
    function GetKeyword(value: string; p: TcPreferenceList; sColorType: String): String; overload;
    begin
      result := value;
      if p <> nil then
        result := Format('<span style="color:#%s;">%s</span>', [ColorToHex(p.Color[sColorType]), result]);
    end;

    (*
    // Tool
    //   getAnalysis
    //
    function getAnalysis(value: TisStatement): String;
    var
      i: longint;
      p: TcObject;
    begin
      result := ksEMPTY;
      for i := 0 to value.m_lstAnalysis_Table.Count - 1 do
        if value.m_lstAnalysis_Table[i] is TisBaseStatement then
        result := result + format('[Table] %s %s<br>', [kasTOKENTYPE[(value.m_lstAnalysis_Table[i] as TisBaseStatement).eTokenType], value.m_lstAnalysis_Table[i].Text]);
      for i := 0 to value.m_lstAnalysis_Column.Count - 1 do
      begin
        p := value.m_lstAnalysis_Column[i];
        if p is TisBaseStatement then
        begin
          result := result + format('[Field] %s %s', [kasTOKENTYPE[(p as TisBaseStatement).eTokenType], p.Text]);
          if (p as TisBaseStatement).eMacroType <> emtUndefined then
            result := result + Format('&nbsp;&nbsp;<span style="color:red;">%s</span>', [kasMACROTYPE[(p as TisBaseStatement).eMacroType]]);
          result := result + '<br>';
        end;
      end;
    end;
    *)

    // Tool
    //   getAnalysis2
    //
    function getAnalysis2(value: TisStatement): String;
    var
      i, j, k: longint;
      p, q: TisBaseStatement;
      lst: TStringList;
      il: TisIndexList;
      lst2: TcBag;
    begin
      result := ksEMPTY;
      k := 1;
      il := GetIndexList;
      if il <> nil then
        for i := 0 to value.m_lstAnalysis_Table.Count - 1 do
        begin
          p := value.m_lstAnalysis_Table[i] as TisBaseStatement;
          lst := nil;
          try
            lst := TStringList.Create;
            if p.eTokenType <> ettParenthese then
              for j := 0 to value.m_lstAnalysis_Column.Count - 1 do
              begin
                q := value.m_lstAnalysis_Column[j] as TisBaseStatement;
                if (q.eMacroType = emtWhere) and (AnsiCompareText(p.sAlias, q.sPrefix) = 0) then
                  lst.add(q.sName);
              end;
            if lst.Count > 0 then
            begin
              result := result + Format('<div class="SmallFont">%d. Table %s, Indexing on %s', [k, p.sName, lst.CommaText]) + ksCR;
              lst2 := nil;
              try
                lst2 := il.GetMatchingIndexes(p.sName, lst);
                if lst2.Count = 0 then
                  result := result +
                            '<div class="Indent"><span style="color:red;">No matching index found:</span>' + ksCR +
                            '&nbsp;' + ksCR +
                            Format('<span class="code">create index %s_skx on %s (%s);</span>', [p.sName, p.sName, lst.CommaText]) + ksCR +
                            '</div>'
                else
                  for j := 0 to lst2.Count - 1 do
                    result := result + Format('<div class="Indent">Possible Index: %s</div>', [lst2[j].Text]) + ksCR;
              finally
                lst2.free;
              end;
              result := result + '</div>';
              inc(k);
            end;
          finally
            lst.free;
          end;
        end;
    end;

    // Tool
    //   getSub
    //
    function getSub(value: TisStatement; sSep: String; parDepth: longint; parStyle: String; parCR: boolean): String;
    var
      i: longint;
      s: String;
    begin
      result := ksEMPTY;
      if sSep = ksEMPTY then
        sSep := ', ';
      for i := 0 to value.Count - 1 do
        if (value[i] <> nil) and (value[i] is TisStatement) then
        begin
          s := GetSubHTML2(value[i] as TisStatement, parDepth, parCR, sSep);
          if (value.m_eTokenType in [ettOperation]) and (value.Token.ID in [_NOT]) then
            s := sSep + s
          else if (value.m_eTokenType in [ettField, ettFrom, ettWhere, ettGroupBy, ettOrderBy, ettFunction, ettParenthese, ettOperation, ettSet]) and
             (i < value.Count - 1) then
            s := s + sSep;
          if parStyle <> ksEMPTY then
            result := result + Format(parStyle, [s])
          else
            result := result + s;
        end;
    end;

  var
    p: TcPreferenceList;
    s: String;
  begin
    p := GetPreferences;
    case value.m_eTokenType of
      ettUndefined:
        result := GetSub(value, ksEMPTY, parDepth, ksEMPTY, FALSE);

      ettFunction:
        result := GetKeyword(value.sName, p, krsPREF_SYNTAXFUNCTION) + GetSub(value, ', ', parDepth, ksEMPTY, FALSE) + value.Alias;

      ettOperation:
      begin
        s := ksEMPTY;
        if parCR and (value.Token.ID in [_AND, _OR]) then
          s := s + '<br>';
        result := GetSub(value, Format(' <span style="font-weight:bolder;">%s</span> %s', [value.sName, s]), parDepth, ksEMPTY, parCR and not (value.Token.ID in [_IN, __NOTIN])) + value.Alias;
      end;

      ettColumn, ettTable:
        result := value.Text;

      ettSelect, ettInsert, ettUpdate, ettDelete, ettDescribe, ettDrop, ettTruncate:
      begin
        result := GetKeyword(value, p, krsPREF_SYNTAXKEYWORD);
        if value.m_eTokenType = ettInsert then
          parSep := ',<br>';
        result := '<div class="BarIndent">' + result + ' ' + GetSub(value, parSep, parDepth + 1, ksEMPTY, parCR or (value.m_eTokenType in [ettInsert])) + value.Alias;
        s := getAnalysis2(value);
        bIssueRaised := bIssueRaised or (s <> ksEMPTY);
             //'<br>' + getAnalysis(value);
        if s <> ksEMPTY then
          result := result + Format('<div class="Note">' +
                                    '  <table border="0">' +
                                    '    <tr>' +
                                    '      <td valign="top"><img src="%sImages/Exclamation.gif" border="0"/></td>' +
                                    '      <td class="Note">%s</td>' +
                                    '    </tr>' +
                                    '  </table>' +
                                    '</div>', [GetFilePath(Application.ExeName), s]) + ksCR;
        result := result + '</div>' + ksCR;
      end;

      ettFrom, ettGroupBy, ettOrderBy, ettWhere, ettSet:
        result := GetKeyword(value, p, krsPREF_SYNTAXKEYWORD) + ' ' + GetSub(value, ', ', parDepth, '<div class="BarIndent2">%s</div>',
                  (value.m_eTokenType in [ettWhere]) and not (value.m_eTokenType in [ettInto]));

      ettElse, ettJoin, ettOn:
        result := GetKeyword(value, p, krsPREF_SYNTAXKEYWORD)+ ' ' + GetSub(value, ksEMPTY, parDepth, ksEMPTY, parCR);

      ettInto:
        result := GetKeyword(value, p, krsPREF_SYNTAXKEYWORD) + ' ' + GetSub(value, ksEMPTY, parDepth, ksEMPTY, FALSE) + '<br>';

      ettValues:
        result := '<br>' + GetKeyword(value, p, krsPREF_SYNTAXKEYWORD)+ '<br>' + GetSub(value, ',<br>', parDepth, ksEMPTY, TRUE);

      ettParenthese:
      begin
        result := GetSub(value, parSep, parDepth, ksEMPTY, parCR);
        if parCR then
          result := Format('<div class="BarIndent2">%s</div>', [result]);
        result := Format('(%s)', [result]) + value.Alias;
      end;

      ettBracket:
        result := Format('[%s]', [GetSub(value, ksEMPTY, parDepth, ksEMPTY, parCR)]) + value.Alias;

      ettUnion, ettIntersect, ettMinus:
        result := GetSub(value, GetKeyword(value, p, krsPREF_SYNTAXKEYWORD) + ' ', parDepth, ksEMPTY, parCR) + value.Alias;

      ettCase, ettWhen, ettThen:
        result := GetKeyword(value, p, krsPREF_SYNTAXKEYWORD) + ' ' + GetSub(value, ksEMPTY, parDepth, ksEMPTY, parCR) + ' ' + value.Alias;

      ettEnd:
        result := ' ' + GetKeyword(value.sName, p, krsPREF_SYNTAXKEYWORD) + ' ';

      ettString, ettNumber, ettReal, ettNull:
      begin
        result := value.sName;
        if value.m_eTokenType = ettString then
          result := Format('''%s''', [result]);
        if p <> nil then
          result := Format('<span style="color:#%s;">%s</span>', [ColorToHex(p.Color[krsPREF_SYNTAXMARK]), result]);
        result := result + value.Alias;
      end;

      ettField, ettDataset:
        result := GetSub(value, ksEMPTY, parDepth, '<div class="BarIndent2">%s</div>', parCR) + ' ';

      ettAs, ettProperty, ettInlineView, ettReference:
        ;

    else
      result := GetSub(value, ksEMPTY, parDepth, ksEMPTY, parCR);
    end;
    result := result + value.sProperties;
  end;

begin
  bIssueRaised := FALSE;
  result := '<html>' + ksCR +
            '  <head>' + ksCR +
	 					'    <style type="text/css"> ' + ksCR +
						'    	<!-- ' + ksCR +
            '       a {font-size:8.0pt; font-family:"Arial";} ' + ksCR +
            '       span.Code {font-size:8.0pt; font-family:"Courier New";} ' + ksCR +
            '       div.BarIndent ' + ksCR +
            '       { ' + ksCR +
            '       	margin-left:4px; ' + ksCR +
            '       	margin-top:5px; ' + ksCR +
            '       	margin-bottom:5px; ' + ksCR +
            '       	border-left-width: 1px; ' + ksCR +
            '       	border-left-style: dotted; ' + ksCR +
            '       	border-left-color: #909090; ' + ksCR +
            '       	padding-left:5px; ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
           // '         word-wrap: break-word; ' + ksCR +
            '       } ' + ksCR +
            '       div.Indent ' + ksCR +
            '       { ' + ksCR +
            '       	margin-left:4px; ' + ksCR +
            '       	margin-top:0px; ' + ksCR +
            '       	margin-bottom:0px; ' + ksCR +
            '       	padding-left:5px; ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
            //'         word-wrap: break-word; ' + ksCR +
            '       } ' + ksCR +
            '       div.SmallFont ' + ksCR +
            '       { ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
            //'         word-wrap: break-word; ' + ksCR +
            '       } ' + ksCR +
            '       div.SmallIndent ' + ksCR +
            '       { ' + ksCR +
            '       	margin-left:5px; ' + ksCR +
            '       	border-left-width: 1px; ' + ksCR +
            '       	border-left-style: solid; ' + ksCR +
            '       	border-left-color: red; ' + ksCR +
            '       	padding-left:5px; ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
            //'         word-wrap: break-word; ' + ksCR +
            '       } ' + ksCR +
			      '       div.BarIndent2 ' + ksCR +
			      '       { ' + ksCR +
			      '       	margin-left:20px; ' + ksCR +
            //'         word-wrap: break-word; ' + ksCR +
			      '       } ' + ksCR +
            '       div.Note ' + ksCR +
            '       { ' + ksCR +
            '       	margin:5px; ' + ksCR +
            '       	padding: 5px; ' + ksCR +
            '       	background-color:#e7e6f2; ' + ksCR +
            '       	border-width: 1px; ' + ksCR +
            '       	border-style: dotted; ' + ksCR +
            '       	border-color: #909090; ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
            //'         word-wrap: break-word; ' + ksCR +
            '       } ' + ksCR +
            '       td.Note ' + ksCR +
            '       { ' + ksCR +
            '       	font-size:8.0pt; ' + ksCR +
            '       	font-family:"Arial"; ' + ksCR +
            '       } ' + ksCR +
            '    	--> ' + ksCR +
						'    </style> ' + ksCR +
            '  </head>' + ksCR +
            '  <body>' + ksCR +
            GetSubHTML2(self, 0, FALSE, ksEMPTY) + ksCR +
            //'<br><br>' + GetSubHTML1(self, 0) + ksCR +
            '  </body>' + ksCR +
            '</html>';
end;

// TisStatement
//   GetPreferences
//
function TisStatement.GetPreferences: TcPreferenceList;
begin
  result := m_objPreferences;
  if (parent <> nil) and (parent is TisStatement) then
    result := (parent as TisStatement).GetPreferences;
end;

// TisStatement
//   SetPreferences
//
procedure TisStatement.SetPreferences(value: TcPreferenceList);
begin
  if (parent <> nil) and (parent is TisStatement) then
    (parent as TisStatement).SetPreferences(value)
  else
    m_objPreferences := value;
end;

// TisStatement
//   AddObjectReference (1)
//
procedure TisStatement.AddObjectReference(parMacroType: TeMacroType; parType: TeTokenType; sparName, sparPrefix, sparAlias: String);

  function Signature(sparName, sparPrefix, sparAlias: String): string;
  begin
    result := sparName;
    if sparPrefix <> ksEMPTY then
      result := sparPrefix + '.' + result;
    if sparAlias <> ksEMPTY then
      result := sparAlias;
  end;

var
  p: TcBag;
  q: TisBaseStatement;
  i, L: longint;
  s1, s2: String;
begin
  if parMacroType = emtTable then
    p := m_lstAnalysis_Table
  else
    P := m_lstAnalysis_Column;
  if (sparName <> _TOKENS[_MULT]) and (p <> nil) then
  begin
    L := kiUNDEFINED;
    for i := 0 to p.Count - 1 do
      if (p[i] <> nil) and (p[i] is TisBaseStatement) then
      begin
        s1 := Signature(p[i].sName, (p[i] as TisBaseStatement).m_sPrefix, (p[i] as TisBaseStatement).m_sAlias);
        s2 := Signature(sparName, sparPrefix, sparAlias);
        if ((AnsiCompareText(s1, s2) = 0) or (AnsiCompareText(p[i].sName, s2) = 0) or (AnsiCompareText(s1, sparName) = 0)) and
           ((p[i] as TisBaseStatement).eMacroType = parMacroType) then
          L := i;
        if L <> kiUNDEFINED then
          break;
      end;
    if L = kiUNDEFINED then
    begin
      // Add Entry.
      q := TisBaseStatement.Create(nil);
      q.sName := sparName;
      q.sAlias := sparAlias;
      q.sPrefix := sparPrefix;
      q.eMacroType := parMacroType;
      q.eTokenType := parType;
      p.Add(q);
    end;
  end;
end;

// TisStatement
//   AddObjectReference (2)
//
procedure TisStatement.AddObjectReference(parMacroType: TeMacroType; value: TisStatement);
begin
  AddObjectReference(parMacroType, value.eTokenType, value.sName, value.sPrefix, value.sAlias);
end;

//
// TisIndex
//

// TisIndex
//   Text
//
function TisIndex.Text: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to Count - 1 do
  begin
    if result <> ksEMPTY then
      result := result + ', ';
    result := result + Objects[i].sName;
  end;
  result := format('index %s on %s (%s)', [sName, m_sTableName, result]);
end;

//
// TisTableList
//

// TisTableList
//   Create
//
constructor TisTableList.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_lstColumns := TcBag.Create(nil);
  m_objMetaData := nil;
end;

// TisTableList
//   Destroy
//
destructor TisTableList.Destroy;
begin
  FreeAndNil(m_lstColumns);
  inherited Destroy;
end;

// TisTableList
//   Load
//
function TisTableList.Load(value: TcConnection): boolean;
var
  e: TcExecute;
  rs: TcRecordSet;
  t: TisTable;
  c: TisColumn;
  m: TcObject;
begin
  //
  // A. Tables
  e := nil;
  if m_objMetaData <> nil then
  try
    e := TcExecute.Create(nil);
    e.Connection := value;
    try
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsTABLELIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue, 'Tables')
        else
          rs := e.OpenSchema(adSchemaTables, Unassigned, 'Tables');
        if (e.Error = ksEMPTY) and (rs <> nil) and rs.IsOpen and not rs.EOF then
        begin
          while not rs.EOF do
          begin
            t := TisTable.Create(nil);
            add(t);
            t.sName := VarToStr(rs.Fields('TABLE_NAME'));
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
      // Sort tables
      if Count > 0 then
        QuickSort(0, Count - 1);
    except
      on E: Exception do
        //
    end;
  finally
    e.free;
  end;
  //
  // B. Columns
  e := nil;
  try
    e := TcExecute.Create(nil);
    e.Connection := value;
    try
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsCOLUMNLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue, 'Table Columns')
        else
          rs := e.OpenSchema(adSchemaColumns, Unassigned, 'Table Columns');
        if (e.Error = ksEMPTY) and (rs <> nil) and rs.IsOpen and not rs.EOF then
        begin
          while not rs.EOF do
          begin
            t := Find(VarToStr(rs.Fields('TABLE_NAME')), 1) as TisTable;
            if t <> nil then
            begin
              c := TisColumn.Create(t);
              t.Add(c);
              c.sName := VarToStr(rs.Fields('COLUMN_NAME'));
              m_lstColumns.Add(c);
            end;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
      //
      // Sort Columns
      if m_lstColumns.Count > 0 then
        m_lstColumns.QuickSort(0, m_lstColumns.Count - 1);
    except
      on E: Exception do
        //
    end;
  finally
    e.free;
  end;
  // Done..!
  result := (Count > 0) and (m_lstColumns.Count > 0);
end;

// TisTableList
//   SetMetaData
//
procedure TisTableList.SetMetaData(value: TcObject);
begin
  m_objMetaData := value;
end;

//
// TisIndexList
//

// TisIndexList
//   Load
//
function TisIndexList.Load(value: TcConnection): boolean;
var
  e: TcExecute;
  rs: TcRecordSet;
  c: TisColumn;
  m: TcObject;
  ind: TcObject;
begin
  //
  // A. Index
  e := nil;
  if m_objMetaData <> nil then
  try
    e := TcExecute.Create(nil);
    e.Connection := value;
    try
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsINDEXLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue, 'Indexes')
        else
          rs := e.OpenSchema(adSchemaIndexes, Unassigned, 'Indexes');
        if (e.Error = ksEMPTY) and (rs <> nil) and rs.IsOpen and not rs.EOF then
        begin
          while not rs.EOF do
          begin
            ind := TisIndex.Create(nil);
            add(ind);
            ind.sName := VarToStr(rs.Fields('INDEX_NAME'));
            (ind as TisIndex).sTableName := VarToStr(rs.Fields('TABLE_NAME'));
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
      // Sort tables
      if Count > 0 then
        QuickSort(0, Count - 1);
    except
      on E: Exception do
        //
    end;
  finally
    e.free;
  end;
  //
  // B. Index Columns
  e := nil;
  try
    e := TcExecute.Create(nil);
    e.Connection := value;
    try
      rs := nil;
      try
        m := (m_objMetaData as TcMetaData).Find(enQuery, krsINDEXCOLUMNLIST);
        if (m <> nil) and (m.sValue <> ksEMPTY) then
          rs := e.Execute(m.sValue, 'Index Columns')
        else
          rs := e.OpenSchema(adSchemaColumns, Unassigned, 'Index Columns');
        if (e.Error = ksEMPTY) and (rs <> nil) and rs.IsOpen and not rs.EOF then
        begin
          while not rs.EOF do
          begin
            ind := Find(VarToStr(rs.Fields('INDEX_NAME')), 1);
            if (ind <> nil) and (ind is TisIndex) then
            begin
              c := TisColumn.Create(ind);
              ind.Add(c);
              c.sName := VarToStr(rs.Fields('COLUMN_NAME'));
              m_lstColumns.Add(c);
            end;
            rs.MoveNext;
          end;
          rs.Close;
        end;
      finally
        rs.free;
      end;
    except
      on E: Exception do
        //
    end;
  finally
    e.free;
  end;
  // Done..!
  result := (Count > 0) and (m_lstColumns.Count > 0);
end;

// TisIndexList
//   GetMatchingIndexes
//
function TisIndexList.GetMatchingIndexes(parTable: String; parColumns: TStringList): TcBag;
var
  i, j: longint;
  p: TisIndex;
  b: boolean;
begin
  result := TcBag.Create(nil);
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TisIndex) then
    begin
      p := Objects[i] as TisIndex;
      b := FALSE;
      if (AnsiCompareText(p.sTableName, parTable) = 0) and (p.count > 0) then
        for j := 0 to parColumns.Count - 1 do
        begin
          b := b or (AnsiCompareText(p[0].sName, parColumns[j]) = 0);
          if b then
            break;
        end;
      if b then
        result.add(p);
    end;
end;

//
// TIntelliSenseControl
//

// TIntelliSenseControl
//   FormCreate
//
constructor TIntelliSenseControl.Create(AOwner: TComponent);
//var
  //e: TeisContent;
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  m_lstTables := nil;
  m_lstIndexes := nil;
  m_iCaretPos := kiUNDEFINED;
  m_objControl := nil;
  OnClick := OnIntelliSenseControl_Click;
  OnDblClick := Close;
  OnMouseDown := OnIntelliSenseControl_MouseDown;
  OnKeyDown := OnIntelliSenseControl_KeyDown;
  Width := 240;
  Height := 200;
  Visible := FALSE;
  //
  m_lstContent := TcCollection.Create(nil);
  // Timer
  m_tmTimer := TTimer.Create(self);
  m_tmTimer.OnTimer := OnTimerTrigger;
  m_bIsTimed := TRUE;
  SetTimerValue(kiINTELLLISENSECONTROL_TIMER);
  SetTimer(FALSE);
  // Popup
  m_popList := TPopupMenu.Create(self);
  PopupMenu := m_popList;
  m_mnuSelect := CreateMenuItem(m_popList.Items, '&Select', TRUE, FALSE, TRUE, OnIntelliSenseControl_Click);
  m_mnuTableColumns := CreateMenuItem(m_popList.Items, '&Add all table columns', FALSE, FALSE, TRUE, OnAddAllColumns);
  m_sPseudoColumns := krsDEFAULTPSEUDO;
end;

// TIntelliSenseControl
//   Destroy
//
destructor TIntelliSenseControl.Destroy;
begin
  m_lstContent.free;
  inherited Destroy;
end;

// TIntelliSenseControl
//   DrawItem
//
procedure TIntelliSenseControl.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  kclCOLOR      = clHotLight;
  kasCONTENT: array[TeisContent] of string =
    (ksEMPTY, 'Context', 'Object List', 'Pseudo Columns', 'Text', 'Column Info');
  kiLEFT        = 10;
  kiSPACE       = 2;
  kiRIGHT       = 5;
  kiINTERSPACE  = 2;

  procedure vLine(value: TeisContent; iTop, iBottom, iRight: longint);
  begin
    Canvas.Pen.Color := kclCOLOR;
    Canvas.MoveTo(iRight - kiRIGHT, iTop);
    Canvas.LineTo(iRight - kiRIGHT, iBottom + 1);
    if value in [eisObjectList, eisPseudo, eisText] then
    begin
      Canvas.Pen.Color := kclCOLOR;
      Canvas.MoveTo(iRight - kiRIGHT - kiINTERSPACE, iTop);
      Canvas.LineTo(iRight - kiRIGHT - kiINTERSPACE, iBottom + 1);
    end;
    if value in [eisPseudo, eisText] then
    begin
      Canvas.Pen.Color := kclCOLOR;
      Canvas.MoveTo(iRight - kiRIGHT - 2 * kiINTERSPACE, iTop);
      Canvas.LineTo(iRight - kiRIGHT - 2 * kiINTERSPACE, iBottom + 1);
    end;
  end;

var
  L: longint;
  s: TSize;
  t: String;
  r: TRect;
  p: TcIntelliSenseItem;
  eb, ea: TeisContent;
begin
  inherited DrawItem(Index, Rect, State);
  // Before entry content type
  eb := eisNone;
  if Index > 0 then
  begin
    p := TcIntelliSenseItem(Items.Objects[Index - 1]);
    if p <> nil then
      eb := p.eisType;
  end;
  // After entry content type
  p := TcIntelliSenseItem(Items.Objects[Index]);
  ea := p.eisType;
  if Index < count - 1 then
  begin
    p := TcIntelliSenseItem(Items.Objects[Index + 1]);
    if p <> nil then
      ea := p.eisType;
  end;
  //
  p := TcIntelliSenseItem(Items.Objects[Index]);
  // Text or Line?
  if (p.eisType <> eb) and (Index <> count - 1) then
  // Text
  begin
    t := ksEMPTY;
    if p <> nil then
      t := kasCONTENT[p.eisType];
    s := Canvas.TextExtent(t);
    r := Rect;
    r.Left := max(r.Right - s.cx - kiLEFT - kiSPACE, r.Left);
    Canvas.Font.Color := kclCOLOR;
    Canvas.TextRect(r, r.Left, r.Top + 1, t);
    L := (Rect.Top + Rect.Bottom) div 2;
    Canvas.Pen.Color := kclCOLOR;
    Canvas.MoveTo(Rect.Right - kiLEFT, L);
    Canvas.LineTo(Rect.Right - kiRIGHT, L);
    vLine(p.eisType, L, Rect.Bottom, Rect.Right)
  end
  else if (p.eisType = ea) and (Index <> count - 1) then
    // Vertical Line
    vLine(p.eisType, Rect.Top, Rect.Bottom, Rect.Right)
  else
  // End Bracket
  begin
    Canvas.Pen.Color := kclCOLOR;
    L := Rect.Bottom - 3;
    vLine(p.eisType, Rect.Top, L, Rect.Right);
    Canvas.MoveTo(Rect.Right - kiLEFT, L);
    Canvas.LineTo(Rect.Right - kiRIGHT, L);
  end;
end;

// TIntelliSenseControl
//   Close
//
procedure TIntelliSenseControl.Close(sender: TObject);
begin
  if Visible then
  begin
    Hide;
    if Assigned(m_objControl) then
      m_objControl.SetFocus;
  end;
end;

// TIntelliSenseControl
//   Display (1)
//
function TIntelliSenseControl.Display: boolean;
var
  pt: TPoint;
begin
  result := Assigned(m_objControl);
  if result then
  begin
    with m_objControl do
      pt := RowColumnToPixels(BufferToDisplayPos(WordStart));
    pt.X := pt.X + m_objControl.Left;
    pt.Y := pt.Y + m_objControl.LineHeight + m_objControl.Top;
    Top := pt.Y;
    Left := max(min(pt.X, m_objControl.Width - Width), 0);
    if not Visible then
    begin
      Visible := TRUE;
      SetTimer(TRUE);
    end;
  end;
end;

// TIntelliSenseControl
//   Display (2)
//
procedure TIntelliSenseControl.Display(parType: TeTokenType; parValue: String; parCurrentList: TcBag);
begin
  //
  m_iCaretPos := GetSelStart;
  if Assigned(m_objControl) then
    m_iCaretPos := m_objControl.RowColToCharIndex(m_objControl.WordStart);
  //
  try
    m_lstContent.Clear;
    Items.BeginUpdate;
    Items.Clear;
    // Column & Table List
    if (parCurrentList <> nil) and (parCurrentList.Count > 0) then
      SendToObject(parCurrentList, eisContext);
    // Directory
    if parType = ettTable then
      SendToObject(m_lstTables, eisObjectList)
    else if parType = ettColumn then
      SendToObject(m_lstTables.Columns, eisObjectList);
    // Pseudo Columns
    if m_sPseudoColumns <> ksEMPTY then
      SendToObject(m_sPseudoColumns, eisPseudo);
    //
    ItemIndex := kiUNDEFINED;
  finally
    Items.EndUpdate;
  end;
  Display;
end;

// TIntelliSenseControl
//   SendToObject method (1)
//
procedure TIntelliSenseControl.SendToObject(value: TcBag; eparContent: TeisContent);
var
  i: longint;
  p: TcIntelliSenseItem;
begin
  if value <> nil then
    for i := 0 to value.count - 1 do
      if (value[i] <> nil) and (value[i].sName <> ksEMPTY) and ((i = 0) or ((i > 0) and (AnsiCompareText(value[i].sName, value[i - 1].sName) <> 0))) then
      begin
        p := TcIntelliSenseItem.Create(nil);
        m_lstContent.Add(p);
        p.sName := value[i].sName;
        p.Data := value[i];
        p.eisType := eparContent;
        Items.AddObject(p.sName, p);
      end;
end;

// TIntelliSenseControl
//   SendToObject method (2)
//
procedure TIntelliSenseControl.SendToObject(value: String; eparContent: TeisContent);
var
  i: longint;
  p: TcIntelliSenseItem;
begin
  if value <> ksEMPTY then
    for i := 0 to ItemCount(value, ',') - 1 do
    begin
      p := TcIntelliSenseItem.Create(nil);
      m_lstContent.Add(p);
      p.sName := Item(value, ',', i);
      p.Data := nil;
      p.eisType := eparContent;
      Items.AddObject(p.sName, p);
    end;
end;

// TIntelliSenseControl
//   GetIndex
//
function TIntelliSenseControl.GetIndex(Value: string): longint;
var
  i: longint;
  p: TcIntelliSenseItem;
begin
  result := kiUNDEFINED;
  for i := 0 to m_lstContent.Count - 1 do
    if (m_lstContent[i] <> nil) and (m_lstContent[i] is TcIntelliSenseItem) then
    begin
      p := (m_lstContent[i] as TcIntelliSenseItem);
      if AnsiCompareText(Value, system.copy(p.sName, 1, length(value))) = 0 then
      begin
        result := i;
        break;
      end;
    end;
end;

// TIntelliSenseControl
//   OnIntelliSenseControl_Click
//
procedure TIntelliSenseControl.OnIntelliSenseControl_Click(Sender: TObject);
begin
  SetEntry(TRUE);
end;

// TIntelliSenseControl
//   GetEntry
//
procedure TIntelliSenseControl.GetEntry;
var
  s: String;
begin
  if visible and Assigned(m_objControl) and (m_objControl.SelStart >= m_iCaretPos) then
  begin
    s := trim(system.copy(m_objControl.Text, m_iCaretPos + 1, m_objControl.SelStart - m_iCaretPos));
    if trim(s) = ksEMPTY then
      ItemIndex := kiUNDEFINED
    else
      ItemIndex := GetIndex(s);
  end;
end;

// TIntelliSenseControl
//   SetEntry (1)
//
function TIntelliSenseControl.SetEntry(value: String; bDismiss: boolean): boolean;
var
  L: longint;
begin
  result := Visible and Assigned(m_objControl) and (value <> ksEMPTY); 
  if result then
  begin
    L := m_objControl.SelStart;
    m_objControl.SelStart := m_iCaretPos;
    m_objControl.SelLength := L - m_iCaretPos;
    m_objControl.SelText := Value;
    m_objControl.SelStart := m_iCaretPos + length(Value);
    if bDismiss then
      Close(nil);
  end;
end;

// TIntelliSenseControl
//   SetEntry (2)
//
function TIntelliSenseControl.SetEntry(bDismiss: boolean): boolean;
var
  p: TcIntelliSenseItem;
begin
  result := FALSE;
  if ItemIndex <> kiUNDEFINED then
  begin
    p := TcIntelliSenseItem(Items.Objects[ItemIndex]);
    if p.eisType <> eisInfoColumns then
      result := SetEntry(trim(Items[ItemIndex]), bDismiss);
  end;
end;

// TIntelliSenseControl
//   FilterKeyPress
//
procedure TIntelliSenseControl.FilterKeyPress(var Key: Word; Shift: TShiftState; KeyState: TeKeyUpDown);
begin
  //
  // Is window already visible?
  if visible then
  begin
    case Key of
      VK_ESCAPE, VK_CONTROL:
        if KeyState = ekDown then
        begin
          Close(nil);
          Key := 0;
        end;
      VK_LEFT, VK_RIGHT, VK_TAB:
        if visible then
          Close(nil);
      VK_RETURN:
        begin
          if (KeyState = ekDown) and (ItemIndex > kiUNDEFINED) then
          begin
            SetEntry(TRUE);
            Key := 0;
          end;
          Close(nil);
        end;
      VK_UP:
        if (KeyState = ekDown) and (ItemIndex > 0) then
        begin
          ItemIndex := ItemIndex - 1;
          Key := 0;
        end;
      VK_DOWN:
        if (KeyState = ekDown) and (ItemIndex < Items.Count - 1) then
        begin
          ItemIndex := ItemIndex + 1;
          Key := 0;
        end;
      VK_PRIOR:
        if (KeyState = ekDown) then
        begin
          ItemIndex := max(0, ItemIndex - (m_objControl.Height div m_objControl.LineHeight));
          Key := 0;
        end;
      VK_NEXT:
        if (KeyState = ekDown) then
        begin
          ItemIndex := min(Items.Count - 1, ItemIndex + (m_objControl.Height div m_objControl.LineHeight));
          Key := 0;
        end;

      //
      // Alpha Keyboard entry
      else if KeyState = ekUp then
        GetEntry;
    end;
    if (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) or
       (ItemIndex <> kiUNDEFINED) then
      SetTimer(FALSE);
  end;
end;

// TIntelliSenseControl
//   CanDisplay
//
function TIntelliSenseControl.CanDisplay(Key: word): boolean;
begin
  if Key in [VK_ESCAPE, VK_CONTROL, VK_TAB] then // VK_LEFT, VK_RIGHT,
    result := FALSE
  else
    result := TRUE;
end;

// TIntelliSenseControl
//   OnIntelliSenseControl_KeyDown
//
procedure TIntelliSenseControl.OnIntelliSenseControl_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FilterKeyPress(Key, Shift, ekDown);
end;

// TIntelliSenseControl
//   SetSelStart
//
procedure TIntelliSenseControl.SetSelStart(value: longint);
begin
  if Assigned(m_objControl) then
    m_objControl.SelStart := value;
end;

// TIntelliSenseControl
//   GetSelStart
//
function TIntelliSenseControl.GetSelStart: longint;
begin
  result := kiUNDEFINED;
  if Assigned(m_objControl) then
    result := m_objControl.SelStart;
end;

// TIntelliSenseControl
//   SetTimer
//
procedure TIntelliSenseControl.SetTimer(value: boolean);
begin
  if m_bIsTimed then
  begin
    if m_tmTimer.Enabled and value then
      m_tmTimer.Enabled := FALSE;
    m_tmTimer.Enabled := value;
  end
  else
    m_tmTimer.Enabled := FALSE;
end;

// TIntelliSenseControl
//   OnTimerTrigger
//
procedure TIntelliSenseControl.OnTimerTrigger(sender: TObject);
begin
  Close(Sender);
end;

// TIntelliSenseControl
//   SetBackgroundColor
//
procedure TIntelliSenseControl.SetBackgroundColor(value: TColor);
begin
  Color := value;
end;

// TIntelliSenseControl
//   SetBackgroundColor
//
procedure TIntelliSenseControl.SetTimerValue(value: longint);
begin
  m_tmTimer.Interval := value * 1000;
end;

// TIntelliSenseControl
//   TIntelliSenseControl
//
procedure TIntelliSenseControl.WMVScroll(var Msg: TWMScroll);
begin
  SetTimer(FALSE);
  inherited;
end;

// TIntelliSenseControl
//   CreateMenuItem
//
function TIntelliSenseControl.CreateMenuItem(parMenu: TMenuItem; sCaption: String; dDefault, bChecked, bEnabled: boolean; objHandle: TNotifyEvent): TMenuItem;
begin
  result := TMenuItem.Create(parMenu);
  parMenu.Add(result);
  result.Caption := sCaption;
  result.onClick := objHandle;
  result.Default := dDefault;
  result.Checked := bChecked;
  result.Enabled:= bEnabled;
end;

// TIntelliSenseControl
//   OnIntelliSenseControl_MouseDown
//
procedure TIntelliSenseControl.OnIntelliSenseControl_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  L: longint;
  p: TcIntelliSenseItem;
begin
  if Button = mbRight then
  begin
    L := ItemAtPos(Point(X, Y), TRUE);
    if L <> kiUNDEFINED then
      ItemIndex := L;
  end;
  p := nil;
  if (ItemIndex <> kiUNDEFINED) and (Items.Objects[ItemIndex] <> nil) then
    p := TcIntelliSenseItem(Items.Objects[ItemIndex]);
  m_mnuTableColumns.Enabled := (p <> nil) and (p.Data <> nil) and (p.Data.Parent <> nil) and (p.Data.Parent.Count > 0);
end;

// TIntelliSenseControl
//   OnAddAllColumns
//
procedure TIntelliSenseControl.OnAddAllColumns(Sender: TObject);
var
  i: longint;
  p: TcIntelliSenseItem;
  s: String;
begin
  if (ItemIndex <> kiUNDEFINED) and (Items.Objects[ItemIndex] <> nil) then
  begin
    p := TcIntelliSenseItem(Items.Objects[ItemIndex]);
    if (p <> nil) and (p.Data <> nil) and (p.Data.Parent <> nil) and (p.Data.Parent.Count > 0) then
    begin
      s := ksEMPTY;
      for i := 0 to p.Data.Parent.Count - 1 do
      begin
        if s <> ksEMPTY then
          s := s + ', ';
        s := s + p.Data.Parent[i].sName;
      end;
      SetEntry(s, TRUE);
    end;
  end;
end;

// TIntelliSenseControl
//   OnAddAllColumns
//
procedure TIntelliSenseControl.SetPseudoColumns(value: String);
begin
  m_sPseudoColumns := krsDEFAULTPSEUDO;
  if value <> ksEMPTY then
    m_sPseudoColumns := m_sPseudoColumns + ',' + value;
end;

// TIntelliSenseControl
//   CreateMenuItem
//
function TIntelliSenseControl.GetCanSense: boolean;
begin
  result := not visible or (visible and (ItemIndex = kiUNDEFINED));
end;

// TIntelliSenseControl
//   ObjectStructure_ColumnList
//
function TIntelliSenseControl.ObjectStructure_ColumnList(value: String): boolean;
var
  p: TcObject;
begin
  try
    m_lstContent.Clear;
    Items.BeginUpdate;
    Items.Clear;
    p := m_lstTables.Find(value);
    if p <> nil then
      SendToObject(p, eisInfoColumns);
    //
    ItemIndex := kiUNDEFINED;
  finally
    Items.EndUpdate;
  end;
  result := Items.Count > 0;
  if result then
    Display
  else
   Application.MessageBox(PChar(Format('Object ''%s'' not found.', [value])), krsINFORMATION, MB_OK + MB_ICONINFORMATION);
end;

end.

