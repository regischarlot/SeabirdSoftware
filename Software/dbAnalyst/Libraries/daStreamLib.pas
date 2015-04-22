unit daStreamLib;

(******************************************************************************
 * Filename: daStreamLib.PAS
 *
 * Author: Regis Charlot
 *
 * Created: April 26, 1997
 *
 * Description: daStreamLib is the unit assuming all streaming for this project.
 *
 * Classes defined:
 *   TcTokenStream - TcTokenStream Class. TcTokenStream is the unit tokenizing
 *                   an input stream;
 *   TcDataStream  - TcDataStream Class. TcDataStream gets in & out data to &
 *                   from a stream
 *
 * Public procedures / functions:
 *   None
 *
 * Database objects used:
 *   None
 *
 * Notes:
 *   None
 *
 * Revision History:
 * Date     Who   Reason
 * -------- ----- ------------------------------------------------------------
 * 09/23/01 Regis Expanded for Data storage
 *
 ******************************************************************************)

interface

uses
  Windows,
  sysUtils,
  ADODB_TLB,
  AxCtrls,
  Classes,
  daGlobals;

type
  TcToken = record
    ID: TvKeyword;
    Value: String;
    Line, Col: longint;
    Pos, Len: longint;
    HasQuote: boolean;
    HasPreceedingSpace: boolean;
    QuoteSymbol: Char;
    PreceedingSpaces: String;
  end;

  TcTokenState = record
    m_bEOS: boolean;
    m_iKeywordStart, m_iTokenStart: longint;
    m_rToken: TcToken;
    m_iLine: longint;
    m_iPositionAtLine: longint;
    m_Position: longint;
    m_esAsStringTokens: TvKeywordSet;
    m_bSglQuoteMarker: boolean;
    m_esExpected: TeTokenTypeSet;
  end;

  ParsingException = class(Exception)
  private
    // Private declarations
    //
    FErrorMessage: WideString;
    FToken: TcToken;
    FExpected: TeTokenTypeSet;
    FParsedObject: TObject;

  public
    // Public declarations
    //
    constructor Create(const ErrorMessage: WideString; rToken: TcToken; esExpected: TeTokenTypeSet; oParsedObject: TObject); overload;

  public
    // Public declarations
    //
    property    Message: WideString                     read FErrorMessage;
    property    Token: TcToken                          read FToken;
    property    Expected: TeTokenTypeSet                read FExpected;
    property    ParsedObject: TObject                   read FParsedObject;
  end;

  TeStreamMarker = (emUndefined, emStart, emEnd, emString, emInteger);
  TeObjectMarker = (eomUndefined, eomException, eomObject, eomMetaData, eomData);

  TcTokenStream = class(TMemoryStream)
 {*****************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *   Tokenizing Class, supporting all parsing operations
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 09/03/00 Regis Created
  * 02/12/05 Regis Added Binary Streaming
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_bEOS: boolean;
    m_iKeywordStart, m_iTokenStart: longint;
    m_rToken: TcToken;
    m_iLine: longint;
    m_iPositionAtLine: longint;
    m_esAsStringTokens: TvKeywordSet;
    m_bSglQuoteMarker: boolean;
    m_esExpected: TeTokenTypeSet;
    m_iStringSize: longint;

  private
    // Private declarations
    //
    // Token Functions
    function    GetNextToken: TcToken;
    function    HashKeyword(sparValue: String): TvKeyword;
    function    GetState: TcTokenState;
    procedure   SetState(Value: TcTokenState);
    function    GetErrorText: String;
    function    PeekNextToken: TcToken;
    procedure   SetAsValue(Value: String);
    // Binary Functions
    function    GetMarker(Item: TeStreamMarker): TeObjectMarker;
    procedure   SetMarker(Item: TeStreamMarker; parValue: TeObjectMarker);
    function    GetAsInteger: longint;
    function    GetAsString: String;
    procedure   SetAsInteger(Value: longint);
    procedure   SetAsString(Value: String);

  public
    // Public declarations
    //
    constructor Create; virtual;
    procedure   Reset; virtual;
    function    Match(eparKeyword: TvKeyword): boolean; overload;
    function    Match(eparKeyword: TvKeyword; parExpected: TeTokenTypeSet): boolean; overload;
    function    Match(eparKeyword: String; parExpected: TeTokenTypeSet): boolean; overload;
    function    Error(sparString: String): String;
    function    CurrentLine: String;
    function    SubStr(iparStart, iparLength: longint): String;
    // Binary functions
    function    MatchMarker(Item: TeStreamMarker; parValue: TeObjectMarker): boolean;
    function    NextObjectMarker: TeObjectMarker;
    function    NextStreamMarker: TeStreamMarker;

  public
    // Public declarations
    //
    property    EOS: boolean                            read m_bEOS;
    property    Token: TcToken                          read m_rToken;
    property    iTokenStart: longint                    read m_iTokenStart;
    property    State: TcTokenState                     read GetState           write SetState;
    property    AsStringTokens: TvKeywordSet            read m_esAsStringTokens write m_esAsStringTokens;
    property    SingleQuoteMarker: boolean              read m_bSglQuoteMarker  write m_bSglQuoteMarker;
    // Binary functions
    property    AsInteger: longint                      read GetAsInteger       write SetAsInteger;
    property    AsString: String                        read GetAsString        write SetAsString;
    property    Marker[Item: TeStreamMarker]: TeObjectMarker read GetMarker     write SetMarker;
    property    StringSize: longint                     read m_iStringSize;
    property    AsValue: String                                                 write SetAsValue;
  end;

  TcDataStream = class(TOLEStream)
 {*****************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *   TcDataStream gets in & out data to & from a stream
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 09/23/01 Regis
  ******************************************************************************}
  private
    // Private declarations
    //
    function    GetMarker: word;
    function    GetAsInteger: longint;
    function    GetAsUnicodeString: String;
    function    GetAsAnsiString: AnsiString;
    function    GetAsUndefined: String;
    function    GetAsDate: TDateTime;
    function    GetAsBoolean: boolean;
    function    GetAsFloat: Double;
    function    GetAsBinary: OLEVariant;
    procedure   SetMarker(Value: word);
    procedure   SetAsInteger(Value: longint);
    procedure   SetAsUnicodeString(Value: String);
    procedure   SetAsAnsiString(Value: AnsiString);
    procedure   SetAsUndefined(Value: String);
    procedure   SetAsDate(Value: TDateTime);
    procedure   SetAsBoolean(Value: boolean);
    procedure   SetAsFloat(Value: Double);
    procedure   SetAsField(objField: Field20);
    procedure   SetAsVariant(Value: OLEVariant);
    procedure   SetAsBinary(Value: OLEVariant);
    procedure   SetAsTypedValue(index: TeDatatype; objVariant: OLEVariant);

  public
    // Public declarations
    //
    procedure   Reset; virtual;
    procedure   AddParameter(objCommand: Command; sparName: String; parDatatype: TeDatatype);

  public
    // Public Properties
    //
    property    AsInteger: longint                      read GetAsInteger       write SetAsInteger;
    property    AsUnicodeString: String                 read GetAsUnicodeString write SetAsUnicodeString;
    property    AsAnsiString: AnsiString                read GetAsAnsiString    write SetAsAnsiString;
    property    AsUndefined: String                     read GetAsUndefined     write SetAsUndefined;
    property    AsDate: TDateTime                       read GetAsDate          write SetAsDate;
    property    AsBoolean: boolean                      read GetAsBoolean       write SetAsBoolean;
    property    AsFloat: double                         read GetAsFloat         write SetAsFloat;
    property    Marker: word                            read GetMarker          write SetMarker;
    property    AsField: Field20                                                write SetAsField;
    property    AsVariant: OLEVariant                                           write SetAsVariant;
    property    AsTypedValue[Index: TeDatatype]: OLEVariant                     write SetAsTypedValue;
    property    AsBinary: OLEVariant                    read GetAsBinary        write SetAsBinary;
  end;

//
// Tools
//

function StringToStream(Value: String): TcTokenStream;
function EmptyToken: TcToken;
function TokenTypeSetToString(Value: TeTokenTypeSet): String;

implementation

uses
  Variants,
  Math, // min, max
  daResourceStrings;

//
// TcTokenStream
//

// TcTokenStream
//   Create
//
constructor TcTokenStream.Create;
begin
  inherited Create;
  m_esAsStringTokens := [];
  m_bSglQuoteMarker := FALSE;
  m_iStringSize := 0;
  Reset;
end;

// TcTokenStream
//   Reset
//
procedure TcTokenStream.Reset;
begin
  Position := 0;
  m_iKeywordStart := 0;
  m_iTokenStart := 0;
  m_bEOS := m_iStringSize = 0;
  m_iLine := 1;
  m_iPositionAtLine := Position;
  // Initialize token stream as on first token
  if not m_bEOS then
    Match(_WILDCARD);
end;

// TcTokenStream
//   GetNextToken
//
function TcTokenStream.GetNextToken: TcToken;

  // IsSpace
  //   Return TRUE if char is space, or CR/LF
  //
  function IsSpace(Value: Char): boolean;
  begin
    result := CharInSet(Value, [kcSPACE, kcCR, kcLF]);
  end;

  // GetCol
  //   Get the current column
  //
  function GetCol(iparIndex: longint): longint;
  var
    i: longint;
  begin
    result := 0;
    for i := m_iPositionAtLine + 1 to iparIndex do
      if CharInSet(PChar(Memory)[i], [#32 .. #127, kcPARAGRAPH]) then
        inc(result);
  end;

  // GetCharAt
  //   Return the character at position 'index'.
  //
  function GetCharAt(iparIndex: longint): Char;
  begin
    result := PChar(Memory)[iparIndex];
  end;

  // SetPosition
  //   Readjust the stream position index. Also sets the line & Col indexes.
  //
  procedure SetPosition(iparIndex: longint);
  var
    i: longint;
  begin
    // Increment position index
    if iparIndex > Position then
    begin
      for i := Position + 1 to iparIndex do
        if GetCharAt(i) = kcLF then
        begin
          m_iPositionAtLine := Position;
          inc(m_iLine);
        end;
    end
    // Decrement position index
    else if iparIndex < Position then
      for i := Position downto iparIndex do
        if GetCharAt(i) = kcLF then
        begin
          m_iPositionAtLine := Position;
          dec(m_iLine);
        end;
    // Eventually set the position index
    Position := iparIndex;
  end;

  // SetTokenStart
  //   Readjust the token start stream position index.
  //
  procedure SetTokenStart(iparIndex: longint);
  begin
    m_iKeywordStart := iparIndex;
    m_rToken.Line := m_iLine;
    m_rToken.Col := GetCol(iparIndex);
    m_rToken.Pos := m_iKeywordStart;
    m_rToken.Len := Position - m_rToken.Pos;
  end;

  // GetNextChar
  //   Return next char in stream pipe. This does not increment the stream position index.
  //
  function GetNextChar(cparChar: PChar): Char;
  begin
    result := kcSPACE;
    if Position < m_iStringSize then
      result := PChar(Memory)[Position];
    if cparChar <> nil then
      cparChar^ := result;
    if (result < #32) and not CharInSet(result, [kcPARAGRAPH, kcCR, kcLF]) then
      result := kcSPACE;
  end;

  // GetChar
  //   Return next char in stream pipe. This increments the stream position index.
  //
  function GetChar(cparChar: PChar): Char;
  begin
    // Get the next char
    result := GetNextChar(cparChar);
    // Set flags
    SetPosition(Position + 1);
    m_bEOS := Position > m_iStringSize;
  end;

var
  c, e, cNext: Char;
  i: longint;
  b: boolean;
  t: TcToken;
begin
  result.ID := _UNDEFINED;
  result.Value := ksEMPTY;
  result.HasQuote := FALSE;
  result.HasPreceedingSpace := FALSE;
  result.QuoteSymbol := #0;
  result.PreceedingSpaces := ksEMPTY;
  // 1.
  // Loop until next non-Space character. Position stream index to this character
  // and check if the nect input is not a comment. If this is a comment, then
  // skip comment, and repeat Step 1.
  SetTokenStart(Position);
  m_iTokenStart := m_iKeywordStart;
  m_bEOS := Position >= m_iStringSize;
  if not m_bEOS then
  repeat
    repeat
      c := GetChar(nil);
      result.HasPreceedingSpace := result.HasPreceedingSpace or IsSpace(c);
      if IsSpace(c) then
        result.PreceedingSpaces := result.PreceedingSpaces + c;
    until (Position >= m_iStringSize) or not IsSpace(c);
    SetTokenStart(Position - 1);
    // Is there a comment next ?
    // 1. double slash comment
    b := (c = kcSLASH) and (GetNextChar(nil) = kcSLASH);
    if b then
      repeat
        GetChar(@c)
      until m_bEOS or (c = kcCR) or (c = kcLF)
    // 2. double dash comment
    else
    begin
      b := (c = kcDASH) and (GetNextChar(nil) = kcDASH);
      if b then
        repeat
          GetChar(@c)
        until m_bEOS or (c = kcCR) or (c = kcLF)
    end;
  until m_bEOS or not b;
  // 2.
  // Stop here if we met an end of stream
  if m_bEOS and (m_iKeywordStart = Position) then
  begin
    result := m_rToken;
    result.ID := _EOS;
    result.Value := ksEMPTY;
    result.HasPreceedingSpace := IsSpace(GetCharAt(Position));
    exit;
  end;
  // 3.
  // We met a quote, therefore a string is expected. Blindly copy all following
  // characters until another quotae is met. If an EOL is met, then the string
  // does not have an end, and therefore raise an exception.
  SetTokenStart(Position - 1);
  if (c = kcDBLQUOTE) or (m_bSglQuoteMarker and (c = kcSGLQUOTE)) then
  begin
    result.ID := _STRING;
    result.HasQuote := TRUE;
    result.QuoteSymbol := c;
    repeat
      cNext := kcSPACE;
      c := GetChar(@e);
      if c <> result.QuoteSymbol then
        result.Value := result.Value + c
      else { = kcQUOTE }
      begin
        cNext := GetNextChar(nil);
        if cNext = result.QuoteSymbol then
        begin
          result.Value := result.Value + cNext + cNext; // Single or Double Quotes must be doubled when reproducing
          c := GetChar(@e);
        end; { if cNext = kcQUOTE }
      end; { else kcQUOTE }
    until m_bEOS or ((c = result.QuoteSymbol) and (cNext <> result.QuoteSymbol));
    if (e = kcCR) or (e = kcLF) or m_bEOS then
      raise ParsingException.Create(Error(krsUNTERMINATEDSTRING), result, [], nil);
  end
  else
  // 3.
  // Normal processing (other than commnent and quoted string processing)
  begin
    // Recognize end of token
    SetPosition(m_iKeywordStart);
    repeat
      c := GetChar(nil);
    until m_bEOS or                                          // Is it the end of the file ?
          (IsDelimitor(c) and (c <> '-') and (c <> '.')) or  // Is it a delimitor other than '-' & '.' ?
          IsSpace(c) or                                      // Is it a space ?
          ((c = '.') and (( pos(GetCharAt(Position - 2), '0123456789') = 0 ) or ( pos(GetNextChar(nil), '0123456789') = 0 ))) or
          ((c = '-') and (pos(GetNextChar(nil), '0123456789') = 0)); // Is it a '-', not being part of a number ?
    // Copy stream portion to token
    i := Position - 1;
    // Special case for double character operators.
    if ((GetCharAt(m_iKeywordStart) = '>') and (GetCharAt(m_iKeywordStart + 1) = '=')) or
       ((GetCharAt(m_iKeywordStart) = '!') and (GetCharAt(m_iKeywordStart + 1) = '=')) or
       ((GetCharAt(m_iKeywordStart) = '<') and (GetCharAt(m_iKeywordStart + 1) = '>')) or
       ((GetCharAt(m_iKeywordStart) = '<') and (GetCharAt(m_iKeywordStart + 1) = '=')) or
       ((GetCharAt(m_iKeywordStart) = ':') and (GetCharAt(m_iKeywordStart + 1) = '=')) or
       ((GetCharAt(m_iKeywordStart) = '-') and (GetCharAt(m_iKeywordStart + 1) = '>')) or
       //((GetCharAt(m_iKeywordStart) = '(') and (GetCharAt(m_iKeywordStart + 1) = '*')) or // NOT APPLICABLE TO SQL
       //((GetCharAt(m_iKeywordStart) = '*') and (GetCharAt(m_iKeywordStart + 1) = ')')) or // NOT APPLICABLE TO SQL
       ((GetCharAt(m_iKeywordStart) = ':') and (GetCharAt(m_iKeywordStart + 1) = ':')) then
    begin
      SetPosition(m_iKeywordStart);
      result.Value := GetChar(nil);
      result.Value := result.Value + GetChar(nil);
      if result.Value = '<>' then
        result.Value := '!=';
    end
    //
    else
    begin
      SetPosition(m_iKeywordStart);
      repeat
        result.Value := result.Value + GetChar(nil);
      until i <= Position;
    end;
    // Recognize token
    // 0. Is this an empty token at the end of the stream?
    if (Position >= m_iStringSize) and (result.Value <> ksEMPTY) and (trim(result.Value) = ksEMPTY) then
    begin
      m_bEOS := TRUE;
      result.ID := _EOS;
      result.Value := ksEMPTY;
      result.HasPreceedingSpace := TRUE;
    end
    // 1. Could it be a number ?
    else if IsNumber(result.Value) then
      result.ID := _NUMBER
    // 2. Could it be a Real?
    else if IsReal(result.Value) then
      result.ID := _REAL
    // 3. OK. Now let's scan known keywords
    else with result do
    begin
      ID := HashKeyword(Value);
      if ID = _UNDEFINED then
      begin
        ID := _STRING;
        result.Value := trim(result.Value);
      end;
    end;
  end;
  result.Line := m_rToken.Line;
  result.Col := m_rToken.Col;
  result.Pos := m_rToken.Pos;
  result.Len := Position - result.Pos;
  if result.ID in m_esAsStringTokens then
    result.ID := _STRING;
  // Not?
  if (result.ID = _NOT) then
  begin
    t := PeekNextToken;
    if t.ID = _LIKE then
    begin
      result.Value := _TOKENS[__NOTLIKE];
      result.ID := __NOTLIKE;
      result.Len := t.Pos - result.Pos + t.Len;
      Position := t.Pos + t.Len;
    end
    else if t.ID = _IN then
    begin
      result.Value := _TOKENS[__NOTIN];
      result.ID := __NOTIN;
      result.Len := t.Pos - result.Pos + t.Len;
      Position := t.Pos + t.Len;
    end;
  end
  // Is?
  else if (result.ID = _IS) then
  begin
    t := PeekNextToken;
    if t.ID = _NOT then
    begin
      result.Value := _TOKENS[__ISNOT];
      result.ID := __ISNOT;
      result.Len := t.Pos - result.Pos + t.Len;
      Position := t.Pos + t.Len;
    end;
  end;
end;

// TcTokenStream
//   GetErrorText
//
function TcTokenStream.GetErrorText: String;
begin
  result := GetLine(StreamToString(self, ecsUnicode), Token.Line);
  insert('^', result, Token.Col + 1);
end;

// TcTokenStream
//   Match (1)
//
function TcTokenStream.Match(eparKeyword: TvKeyword; parExpected: TeTokenTypeSet): boolean;
var
  s: String;
begin
  m_esExpected := parExpected;
  result := (eparKeyword = _WILDCARD) or (Token.ID = eparKeyword);
  s := ksEMPTY;
  if not result then
  begin
    if m_esExpected = [] then
      case eparKeyword of
        _LCURLY:
          s := krsEXPECTEDLEFTCURLY;
        _RCURLY:
          s := krsEXPECTEDRIGHTCURLY;
        _SEMICOLON:
          s := krsEXPECTEDSEMICOLON;
        _COMMA:
          s := krsEXPECTEDCOMMA;
        _EQUAL:
          s := krsEXPECTEDEQUAL;
        _EOS:
          s := krsEXPECTEDEOS;
        _COLON:
          s := krsEXPECTEDCOLON;
      else
        s := Format(krsEXPECTEDTOKEN, [_TOKENS[eparKeyword]]);
      end
    else
      s := Format(krsEXPECTEDTOKEN, [TokenTypeSetToString(m_esExpected)]);
    raise ParsingException.Create(GetErrorText + ksCR + Error(s), m_rToken, parExpected, nil);
  end;
  m_rToken := GetNextToken;
end;

// TcTokenStream
//   Match (2)
//
function TcTokenStream.Match(eparKeyword: TvKeyword): boolean;
begin
  result := Match(eparKeyword, []);
end;

// TcTokenStream
//   Match (3)
//
function TcTokenStream.Match(eparKeyword: String; parExpected: TeTokenTypeSet): boolean;
var
  s: String;
begin
  m_esExpected := parExpected;
  result := (eparKeyword = ksEMPTY) or (AnsiCompareText(Token.Value, eparKeyword) = 0);
  s := ksEMPTY;
  if not result then
  begin
    if m_esExpected = [] then
      s := Format(krsEXPECTEDTOKEN, [eparKeyword])
    else
      s := Format(krsEXPECTEDTOKEN, [TokenTypeSetToString(m_esExpected)]);
    raise ParsingException.Create(GetErrorText + ksCR + Error(s), m_rToken, parExpected, nil);
  end;
  m_rToken := GetNextToken;
end;

// TcTokenStream
//   Error
//
function TcTokenStream.Error(sparString: String): String;
begin
  result := Format(krsERRORMASK, [m_rToken.Value, m_rToken.Line, m_rToken.Col, sparString]);
end;

// TcTokenStream
//   HashKeyword
//
function TcTokenStream.HashKeyword(sparValue: String): TvKeyword;
var
  t: TvKeyword;
begin
  result := _UNDEFINED;
  // Check for a regular keyword
  for t := _LBRACKET to high(TvKeyword) do
    if CompareText(sparValue, _TOKENS[t]) = 0 then
    begin
      result := t;
      break;
    end;
end;

// TcTokenStream
//   CurrentLine
//
function TcTokenStream.CurrentLine: String;
var
  i: longint;
begin
  i := m_iPositionAtLine + 2;
  result := ksEMPTY;
  while (i < m_iStringSize) and (PChar(Memory)[i] <> kcCR) do
  begin
    if CharInSet(PChar(Memory)[i], [#32 .. #127, kcPARAGRAPH]) then
      result := result + PChar(Memory)[i];
    inc(i);
  end;
end;

// TcTokenStream
//   SubStr
//
function TcTokenStream.SubStr(iparStart, iparLength: longint): String;
var
  L: longint;
  p: PChar;
begin
  result := ksEMPTY;
  if iparStart < m_iStringSize then
  begin
    L := min(m_iStringSize - iparStart + 1, iparLength);
    p := nil;
    try
      p := StrAlloc(L + 1);
      strLCopy(p, @(PChar(Memory)[iparStart]), L);
      p[L] := #0;
      result := String(p);
    finally
      strDispose(p);
    end;
  end;
end;

// TcTokenStream
//   GetState
//
function TcTokenStream.GetState: TcTokenState;
begin
  result.m_bEOS             := m_bEOS;
  result.m_iKeywordStart    := m_iKeywordStart;
  result.m_iTokenStart      := m_iTokenStart;
  result.m_rToken           := m_rToken;
  result.m_iLine            := m_iLine;
  result.m_iPositionAtLine  := m_iPositionAtLine;
  result.m_Position         := Position;
  result.m_esAsStringTokens := m_esAsStringTokens;
  result.m_bSglQuoteMarker  := m_bSglQuoteMarker;
  result.m_esExpected       := m_esExpected;
end;

// TcTokenStream
//   SetState
//
procedure TcTokenStream.SetState(Value: TcTokenState);
begin
  m_bEOS                := value.m_bEOS;
  m_iKeywordStart       := value.m_iKeywordStart;
  m_iTokenStart         := value.m_iTokenStart;
  m_rToken              := value.m_rToken;
  m_iLine               := value.m_iLine;
  m_iPositionAtLine     := value.m_iPositionAtLine;
  Position              := value.m_Position;
  m_esAsStringTokens    := value.m_esAsStringTokens;
  m_bSglQuoteMarker     := value.m_bSglQuoteMarker;
  m_esExpected          := value.m_esExpected;
end;

// TcTokenStream
//   SetAsInteger
//
procedure TcTokenStream.SetAsInteger(Value: longint);
begin
  SetMarker(emInteger, eomUndefined);
  Write(Value, sizeof(longint));
end;

// TcTokenStream
//   GetAsInteger
//
function TcTokenStream.GetAsInteger: longint;
begin
  result := 0;
  if GetMarker(emInteger) <> eomException then
    read(result, sizeof(result));
end;

// TcTokenStream
//   SetAsString
//
procedure TcTokenStream.SetAsString(Value: String);
var
  L: longint;
begin
  SetMarker(emString, eomUndefined);
  L := length(Value);
  Write(L, sizeof(L));
  if Value <> ksEMPTY then
    Write(Value[1], L);
end;

// TcTokenStream
//   GetAsString
//
function TcTokenStream.GetAsString: String;
var
  L: longint;
  p: PChar;
begin
  result := ksEMPTY;
  if GetMarker(emString) = eomException then
    exit;
  // Read the length of Comment
  Read(L, sizeof(L));
  p := nil;
  if L <> 0 then
  try
    p := StrAlloc(L + 1);
    // Comment read as a pascal String
    Read(p^, L);
    p[L] := #0;
    result := String(p);
  finally
    strDispose(p);
  end;
end;

// TcTokenStream
//   MatchMarker
//
function TcTokenStream.MatchMarker(Item: TeStreamMarker; parValue: TeObjectMarker): boolean;
begin
  result := parValue = GetMarker(Item);
end;

// TcTokenStream
//   GetMarker
//
function TcTokenStream.GetMarker(Item: TeStreamMarker): TeObjectMarker;
var
  smMarker: TeStreamMarker;
  p: longint;
begin
  p := Position;
  read(smMarker, sizeof(TeStreamMarker));
  if Item <> smMarker then
  begin
    Position := p;
    result := eomException;
  end
  else
    read(result, sizeof(TeObjectMarker));
end;

// TcTokenStream
//   SetMarker
//
procedure TcTokenStream.SetMarker(Item: TeStreamMarker; parValue: TeObjectMarker);
begin
  Write(Item, sizeof(Item));
  Write(parValue, sizeof(parValue));
end;

// TcTokenStream
//   NextObjectMarker
//
function TcTokenStream.NextObjectMarker: TeObjectMarker;
var
  smMarker: TeStreamMarker;
  p: longint;
begin
  p := Position;
  read(smMarker, sizeof(TeStreamMarker));
  read(result, sizeof(TeObjectMarker));
  Position := p;
end;

// TcTokenStream
//   NextStreamMarker
//
function TcTokenStream.NextStreamMarker: TeStreamMarker;
var
  p: longint;
begin
  p := Position;
  read(result, sizeof(TeStreamMarker));
  Position := p;
end;

// TcTokenStream
//   PeekNextToken
//
function TcTokenStream.PeekNextToken: TcToken;
var
  s: TcTokenState;
begin
  s := GetState;
  Match(_WILDCARD);
  result := m_rToken;
  SetState(s);
end;

// TcTokenStream
//   SetAsValue
//
procedure TcTokenStream.SetAsValue(Value: String);
begin
  Clear;
  m_iStringSize := length(value);
  value := Value + #0;
  if Value <> ksEMPTY then
    Write(Value[1], Length(Value) * 2);
  Reset;
end;

//
// TcDataStream
//

// TcDataStream
//   GetMarker
//
function TcDataStream.GetMarker: word;
begin
  read(result, sizeof(word));
end;

// TcDataStream
//   SetMarker
//
procedure TcDataStream.SetMarker(Value: word);
begin
  Write(Value, sizeof(word));
end;

// TcDataStream
//   Reset
//
procedure TcDataStream.Reset;
begin
  Position := 0;
end;

// TcDataStream
//   SetAsInteger
//
procedure TcDataStream.SetAsInteger(Value: longint);
begin
  SetMarker(longint(etInteger));
  Write(Value, sizeof(longint));
end;

// TcDataStream
//   GetAsInteger
//
function TcDataStream.GetAsInteger: longint;
begin
  result := 0;
  if GetMarker = longint(etInteger) then
    read(result, sizeof(longint));
end;

// TcDataStream
//   SetAsAnsiString
//
procedure TcDataStream.SetAsAnsiString(Value: AnsiString);
var
  L: longint;
begin
  SetMarker(longint(etAnsiString));
  L := length(Value);
  Write(L, sizeof(longint));
  if L > 0 then
    Write(Value[1], L);
end;

// TcDataStream
//   GetAsAnsiString
//
function TcDataStream.GetAsAnsiString: AnsiString;
var
  L: longint;
begin
  result := ksEMPTY;
  if GetMarker = longint(etAnsiString) then
  begin
    read(L, sizeof(longint));
    if L > 0 then
    begin
      SetLength(result, L);
      ReadBuffer(Pointer(result)^, L);
    end;
  end;
end;

// TcDataStream
//   SetAsUnicodeString
//
procedure TcDataStream.SetAsUnicodeString(Value: String);
var
  L: longint;
begin
  SetMarker(longint(etUnicodeString));
  L := length(Value);
  Write(L, sizeof(longint));
  if L > 0 then
    Write(Value[1], L * 2);
end;

// TcDataStream
//   GetAsUnicodeString
//
function TcDataStream.GetAsUnicodeString: String;
var
  L: longint;
begin
  result := ksEMPTY;
  if GetMarker = longint(etUnicodeString) then
  begin
    read(L, sizeof(longint));
    if L > 0 then
    begin
      SetLength(result, L);
      ReadBuffer(Pointer(result)^, L * 2);
    end;
  end;
end;

// TcDataStream
//   SetAsUndefined
//
procedure TcDataStream.SetAsUndefined(Value: String);
begin
  SetMarker(longint(etUndefined));
end;

// TcDataStream
//   GetAsUndefined
//
function TcDataStream.GetAsUndefined: String;
begin
  GetMarker;
  result := ksEMPTY;
end;

// TcDataStream
//   SetAsDate
//
procedure TcDataStream.SetAsDate(Value: TDateTime);
begin
  SetMarker(longint(etDate));
  Write(Value, sizeof(TDateTime));
end;

// TcDataStream
//   GetAsDate
//
function TcDataStream.GetAsDate: TDateTime;
begin
  result := 0.0;
  if GetMarker = longint(etDate) then
    read(result, sizeof(TDateTime));
end;

// TcDataStream
//   SetAsBoolean
//
procedure TcDataStream.SetAsBoolean(Value: boolean);
begin
  SetMarker(longint(etBoolean));
  Write(Value, sizeof(boolean));
end;

// TcDataStream
//   GetAsBoolean
//
function TcDataStream.GetAsBoolean: boolean;
begin
  result := FALSE;
  if GetMarker = longint(etBoolean) then
    read(result, sizeof(boolean));
end;

// TcDataStream
//   SetAsFloat
//
procedure TcDataStream.SetAsFloat(Value: Double);
begin
  SetMarker(longint(etFloat));
  Write(Value, sizeof(Double));
end;

// TcDataStream
//   GetAsFloat
//
function TcDataStream.GetAsFloat: Double;
begin
  result := 0.0;
  if GetMarker = longint(etFloat) then
    read(result, sizeof(Double));
end;

// TcDataStream
//   SetAsField
//
procedure TcDataStream.SetAsField(objField: Field20);
var
  L: longint;
  s: String;
  f: Double;
  b: boolean;
  d: TDateTime;
begin
  if VarIsNull(objField.Value) then
    SetMarker(longint(etNull))
  else
  begin
    SetMarker(longint(ADOType2ShortType(objField.Type_, objField.Precision, objField.NumericScale)));
    case ADOType2ShortType(objField.Type_, objField.Precision, objField.NumericScale) of
      etUndefined:
        ;
      etAnsiString, etLongString:
      begin
        s := FieldToString(objField);
        L := length(s);
        Write(L, sizeof(longint));
        if L > 0 then
          Write(s[1], L);
      end;
      etInteger:
      begin
        L := FieldToInteger(objField);
        Write(L, sizeof(longint));
      end;
      etDate:
      begin
        d := FieldToDate(objField);
        Write(d, sizeof(TDateTime));
      end;
      etBoolean:
      begin
        b := FieldToBoolean(objField);
        Write(b, sizeof(boolean));
      end;
      etFloat:
      begin
        f := FieldToReal(objField);
        Write(f, sizeof(double));
      end;
    end;
  end;
end;

// TcDataStream
//   SetAsVariant
//
procedure TcDataStream.SetAsVariant(Value: OLEVariant);
begin
  if VarIsNull(Value) then
    SetMarker(longint(etNull))
  else
    SetAsTypedValue(VariantTypeToDatatype(VarType(Value)), Value);
end;

// TcDataStream
//   SetAsTypedValue
//
procedure TcDataStream.SetAsTypedValue(index: TeDatatype; objVariant: OLEVariant);
var
  L: longint;
  f: Double;
begin
  if VarIsNull(objVariant) then
    SetMarker(longint(etNull))
  else
    case index of
      etUndefined, etNull:
        SetMarker(longint(etNull));
      etAnsiString:
        SetAsAnsiString(AnsiString(VarToStr(objVariant)));
      etLongString:
        SetAsUnicodeString(VarToStr(objVariant));
      etUnicodeString:
        SetAsUnicodeString(VarToStr(objVariant));
      etInteger:
        SetAsInteger(StrToIntDef(VarToStr(objVariant), 0));
      etDate:
        SetAsDate(VarToDateTime(objVariant));
      etBoolean:
        SetAsBoolean(objVariant = TRUE);
      etFloat:
        begin
          Val(VarToStr(objVariant), f, L);
          SetAsFloat(f);
        end;
      etBinary:
        SetAsBinary(objVariant);
    end;
end;

// TcDataStream
//   SetAsBinary
//
procedure TcDataStream.SetAsBinary(Value: OLEVariant);
begin
  SetMarker(longint(etBinary));
  // Woops
end;

// TcDataStream
//   GetAsBinary
//
function TcDataStream.GetAsBinary: OLEVariant;
begin
  GetMarker;
  // Woops
end;

// TcDataStream
//   AddParameter
//
procedure TcDataStream.AddParameter(objCommand: Command; sparName: String; parDatatype: TeDatatype);

  function VarDataSize(const Value: OLEVariant): Integer;
  begin
    if VarIsNull(Value) then
      result := 0
    else if VarIsArray(Value) then
      result := VarArrayHighBound(Value, 1) + 1
    else if TVarData(Value).VType = varOleStr then
      result := length(PWideString(@TVarData(Value).VOleStr)^)
    else
      result := sizeof(OLEVariant);
  end;

const
  BLOCK_SIZE = 1000;
  keaDATATYPE: array [TeDatatype] of DataTypeEnum =
    (adEmpty, adEmpty, adBSTR, adInteger, adDate, adBoolean, adDecimal, adLongVarChar, adVarBinary, adBSTR);
var
  m: TeDatatype;
  L: longint;
  s: String;
  f: Double;
  b: boolean;
  d: TDateTime;
  v: OLEVariant;
  p: PChar;
  cp: Parameter;
begin
  m := TeDatatype(GetMarker);
  v := null;
  case m of
    etNull, etUndefined:
      ;
    etAnsiString, etLongString:
      begin
        s := ksEMPTY;
        read(L, sizeof(longint));
        p := nil;
        if L > 0 then
          try
            p := StrAlloc(L + 1);
            Read(p^, L);
            p[L] := #0;
            s := String(p);
          finally
            strDispose(p);
          end;
        v := s;
      end;
    etInteger:
      begin
        read(L, sizeof(longint));
        v := L;
      end;
    etDate:
      begin
        read(d, sizeof(TDateTime));
        v := d;
      end;
    etBoolean:
      begin
        read(b, sizeof(boolean));
        v := b;
      end;
    etFloat:
      begin
        read(f, sizeof(Double));
        v := f;
      end;
  end;
  cp := objCommand.CreateParameter(sparName, keaDATATYPE[parDatatype], adParamInput, VarDataSize(v), v);
  objCommand.Parameters.Append(cp);
end;

//
// Tools
//

// Tools
//   StringToStream
//
function StringToStream(Value: String): TcTokenStream;
begin
  result := TcTokenStream.Create;
  result.AsValue := value;
end;

// Tools
//   EmptyToken
//
function EmptyToken: TcToken;
begin
  result.ID := _UNDEFINED;
  result.Value := ksEMPTY;
  result.Line := 0;
  result.Col := 0;
  result.Pos := 0;
  result.Len := 0;
  result.HasQuote := FALSE;
  result.HasPreceedingSpace := FALSE;
  result.QuoteSymbol := #0;
  result.PreceedingSpaces := ' ';
end;

// Tools
//   TokenTypeSetToString
//
function TokenTypeSetToString(Value: TeTokenTypeSet): String;
var
  e: TeTokenType;
  lst: TStringList;
  i: longint;
begin
  result := ksEMPTY;
  lst := nil;
  try
    lst := TStringList.Create;
    // Compute List
    for e := low(TeTokenType) to high(TeTokenType) do
      if e in Value then
        lst.Add(kasTOKENTYPE[e]);
    // Build String
    for i := 0 to lst.Count - 1 do
    begin
      if (i > 0) and (i < lst.Count - 1) then
        result := result + ', '
      else if (i = lst.Count - 1) then
        result := result + ' or ';
      result := result + lst[i];
    end;
  finally
    lst.free;
  end;
end;

//
// ParsingException
//

// ParsingException
//   Create
//
constructor ParsingException.Create(const ErrorMessage: WideString; rToken: TcToken; esExpected: TeTokenTypeSet; oParsedObject: TObject);
begin
  FErrorMessage := ErrorMessage;
  inherited Create(FErrorMessage);
  FToken := rToken;
  FExpected := esExpected;
  FParsedObject := oParsedObject;
end;

end.
