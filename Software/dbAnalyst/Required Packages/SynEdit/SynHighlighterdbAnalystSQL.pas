{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSQL.pas, released 2000-04-21.
The Original Code is based on the wmSQLSyn.pas and wmSybaseSyn.pas files from
the mwEdit component suite by Martin Waldenburg and other developers, the
Initial Author of these files is Willo van der Merwe. Initial Author of
SynHighlighterSQL.pas is Michael Hieke.
Portions created by Willo van der Merwe are Copyright 1999 Willo van der Merwe.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterSQL.pas,v 1.39.2.14 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(SQL highlighter for SynEdit with support for different dialects.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2000-11-16)
The SynHighlighterSQL implements a highlighter for SQL for the SynEdit projects.
Different SQL dialects can be selected via the Dialect property.
}

{$IFNDEF QSYNHIGHLIGHTERSQL}
unit SynHighlighterdbAnalystSQL;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Types,
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
  QSynUnicode,
{$ELSE}
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDatatype, tkDefaultPackage, tkException,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkPLSQL,
    tkSQLPlus, tkString, tkSymbol, tkTableName, tkUnknown, tkVariable,
    tkConditionalComment, tkDelimitedIdentifier);

  TRangeState = (rsUnknown, rsComment, rsString, rsConditionalComment);

  TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle,
    sqlSybase, sqlIngres, sqlMSSQL2K, sqlPostgres);

type
  TSyndbAnalystSQLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    fTableNames: TUnicodeStrings;
    fDialect: TSQLDialect;
    fCommentAttri: TSynHighlighterAttributes;
    fConditionalCommentAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fDefaultPackageAttri: TSynHighlighterAttributes;
    fDelimitedIdentifierAttri: TSynHighlighterAttributes;
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPLSQLAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSQLPlusAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTableNameAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fStringList: TUnicodeStringList;
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    procedure SetDialect(Value: TSQLDialect);
    procedure SetTableNames(const Value: TUnicodeStrings);
    procedure TableNamesChanged(Sender: TObject);
    procedure InitializeKeywordLists;
    procedure PutTableNamesInKeywordList;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure HashProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure QuoteProc;
    procedure BacktickProc;
    procedure BracketProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    procedure AnsiCProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetWordList(value: TtkTokenKind; WordList: string);
    procedure SetHilightStyle(value: TtkTokenKind; parStyle: TFontStyles; parColor: TColor);
    function GetAttri(value: TtkTokenKind): TSynHighlighterAttributes;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property ConditionalCommentAttri: TSynHighlighterAttributes
      read fConditionalCommentAttri write fConditionalCommentAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri
      write fDataTypeAttri;
    property DefaultPackageAttri: TSynHighlighterAttributes
      read fDefaultPackageAttri write fDefaultPackageAttri;
    property DelimitedIdentifierAttri: TSynHighlighterAttributes
      read fDelimitedIdentifierAttri write fDelimitedIdentifierAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri
      write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PLSQLAttri: TSynHighlighterAttributes read fPLSQLAttri
      write fPLSQLAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SQLPlusAttri: TSynHighlighterAttributes read fSQLPlusAttri
      write fSQLPlusAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TableNameAttri: TSynHighlighterAttributes read fTableNameAttri
      write fTableNameAttri;
    property TableNames: TUnicodeStrings read fTableNames write SetTableNames;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
    property SQLDialect: TSQLDialect read fDialect write SetDialect
      default sqlStandard;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

function TSyndbAnalystSQLSyn.HashKey(Str: PWideChar): Integer;
var
  FoundDoubleMinus: Boolean;

  function GetOrd: Integer;
  begin
    case Str^ of
      '_': Result := 1;
      'a'..'z': Result := 2 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 2 + Ord(Str^) - Ord('A');
      '@':
        if fDialect in [sqlMSSQL7, sqlMSSQL2K] then
          Result := 24
        else
          Result := 0;
      else Result := 0;
    end;
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    FoundDoubleMinus := (Str^ = '-') and ((Str + 1)^ = '-');
    if FoundDoubleMinus then Break;
{$IFOPT Q-}
    Result := 2 * Result + GetOrd;
{$ELSE}
    Result := (2 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    inc(Str);
  end;
  Result := Result and $FF; // 255
  fStringLen := Str - fToIdent;
end;

function TSyndbAnalystSQLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSyndbAnalystSQLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStringList := TUnicodeStringList.Create;
  fCaseSensitive := False;

  fKeywords := TSynHashEntryList.Create;
  fTableNames := TUnicodeStringList.Create;
  TUnicodeStringList(fTableNames).OnChange := TableNamesChanged;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fConditionalCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrConditionalComment, SYNS_FriendlyAttrConditionalComment);
  fConditionalCommentAttri.Style := [fsItalic];
  AddAttribute(fConditionalCommentAttri);

  fDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
  fDefaultPackageAttri :=
    TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage, SYNS_FriendlyAttrDefaultPackage);
  fDefaultPackageAttri.Style := [fsBold];
  AddAttribute(fDefaultPackageAttri);
  fDelimitedIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrDelimitedIdentifier, SYNS_FriendlyAttrDelimitedIdentifier);
  AddAttribute(fDelimitedIdentifierAttri);
  fExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException, SYNS_FriendlyAttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fPLSQLAttri := TSynHighlighterAttributes.Create(SYNS_AttrPLSQL, SYNS_FriendlyAttrPLSQL);
  fPLSQLAttri.Style := [fsBold];
  AddAttribute(fPLSQLAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fSQLPlusAttri:=TSynHighlighterAttributes.Create(SYNS_AttrSQLPlus, SYNS_FriendlyAttrSQLPlus);
  fSQLPlusAttri.Style := [fsBold];
  AddAttribute(fSQLPlusAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring, SYNS_FriendlyAttrstring);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fTableNameAttri := TSynHighlighterAttributes.Create(SYNS_AttrTableName, SYNS_FriendlyAttrTableName);
  AddAttribute(fTableNameAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
  fDialect := sqlStandard;
  InitializeKeywordLists;
end;

destructor TSyndbAnalystSQLSyn.Destroy;
begin
  fKeywords.Free;
  fTableNames.Free;
  fStringList.Free;
  inherited Destroy;
end;

procedure TSyndbAnalystSQLSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if (Source is TSyndbAnalystSQLSyn) then
    SQLDialect := TSyndbAnalystSQLSyn(Source).SQLDialect;
end;

procedure TSyndbAnalystSQLSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '&']) then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.AsciiCharProc;
begin
  // Oracle SQL allows strings to go over multiple lines
  if fLine[Run] = #0 then
    NullProc
  else begin
    fTokenID := tkString;
    // else it's end of multiline string
    if SQLDialect <> sqlMySql then
    begin
      if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then
      begin
        fRange := rsString;
        repeat
          Inc(Run);
        until IsLineEnd(Run) or (fLine[Run] = #39);
      end;
      if fLine[Run] = #39 then
      begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end
    else
    begin
      if (Run > 0) or (fRange <> rsString) or
        ((fLine[Run] <> #39) and (fLine[Run - 1] <> '\')) then
      begin
        fRange := rsString;
        repeat
          if (fLine[Run] <> '\') and (fLine[Run + 1] = #39) then
          begin
            Inc(Run);
            break;
          end;
          Inc(Run);
        until IsLineEnd(Run);
      end;
      if (fLine[Run] = #39) and not(fLine[Run-1] = '\') then
      begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end;
  end;
end;

procedure TSyndbAnalystSQLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.IdentProc;
var
  FoundDoubleMinus: Boolean;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  if fTokenID = tkComment then
  begin
    while not IsLineEnd(Run) do
      Inc(Run);
  end
  else
    while IsIdentChar(fLine[Run]) do
    begin
      FoundDoubleMinus := (fLine[Run] = '-') and (fLine[Run + 1] = '-');
      if FoundDoubleMinus then Break;
      inc(Run);
    end;
end;

procedure TSyndbAnalystSQLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSyndbAnalystSQLSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then Inc(Run);
         end;
  end;
end;

procedure TSyndbAnalystSQLSyn.MinusProc;
begin
  Inc(Run);
  if fLine[Run] = '-' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSyndbAnalystSQLSyn.HashProc;
begin
  if SQLDialect = sqlMySql then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    Inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSyndbAnalystSQLSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSyndbAnalystSQLSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSyndbAnalystSQLSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '|']) then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '+']) then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        if (SQLDialect = sqlMySql) and (fLine[Run + 1] = '!') then
        begin
          fRange := rsConditionalComment;
          fTokenID := tkConditionalComment;
        end
        else
        begin
          fRange := rsComment;
          fTokenID := tkComment;
        end;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until IsLineEnd(Run);
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSyndbAnalystSQLSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSyndbAnalystSQLSyn.QuoteProc;
begin
  fTokenID := tkDelimitedIdentifier;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = #34 then
    begin
      Inc(Run);
      if fLine[Run] <> #34 then
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSyndbAnalystSQLSyn.BacktickProc;
begin
  if SQLDialect = sqlMySql then
  begin
    fTokenID := tkDelimitedIdentifier;
    Inc(Run);
    while not IsLineEnd(Run) do
    begin
      if fLine[Run] = '`' then
      begin
        Inc(Run);
        if fLine[Run] <> '`' then
          Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSyndbAnalystSQLSyn.BracketProc;
begin
  if SQLDialect in [sqlMSSQL7, sqlMSSQL2K] then
  begin
    fTokenID := tkDelimitedIdentifier;
    Inc(Run);
    while not IsLineEnd(Run) do
    begin
      if fLine[Run] = ']' then
      begin
        Inc(Run);
        if fLine[Run] <> ']' then
          Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSyndbAnalystSQLSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSyndbAnalystSQLSyn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSyndbAnalystSQLSyn.VariableProc;
var
  i: integer;
  FoundDoubleMinus: Boolean;
begin
  // MS SQL Server uses @@ to indicate system functions/variables
  if (SQLDialect in [sqlMSSQL7, sqlMSSQL2K]) and (fLine[Run] = '@') and (fLine[Run + 1] = '@') then
    IdentProc
  else if (SQLDialect in [sqlMySql, sqlOracle]) and (fLine[Run] = '@') then
    SymbolProc
  // Oracle uses the ':' character to indicate bind variables
  // Ingres II also uses the ':' character to indicate variables
  else if not (SQLDialect in [sqlOracle, sqlIngres]) and (fLine[Run] = ':') then
    SymbolProc
  else
  begin
    fTokenID := tkVariable;
    i := Run;
    repeat
      FoundDoubleMinus := (fLine[i] = '-') and (fLine[i + 1] = '-');
      if FoundDoubleMinus then Break;
      Inc(i);
    until not IsIdentChar(fLine[i]);
    Run := i;
  end;
end;

procedure TSyndbAnalystSQLSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSyndbAnalystSQLSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
    begin
      if fRange = rsConditionalComment then
        fTokenID := tkConditionalComment
      else
        fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
        begin
          fRange := rsUnknown;
          Inc(Run, 2);
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

function TSyndbAnalystSQLSyn.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PWideChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkPLSQL,
    tkDefaultPackage];
end;

procedure TSyndbAnalystSQLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment, rsConditionalComment:
      AnsiCProc;
    rsString:
      AsciiCharProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #39: AsciiCharProc;
      '=': EqualProc;
      '>': GreaterProc;
      '<': LowerProc;
      '-': MinusProc;
      '#': HashProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '/': SlashProc;
      '&': AndSymbolProc;
      #34: QuoteProc;
      '`': BacktickProc;
      '[': BracketProc;
      ':', '@': VariableProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '^', '%', '*', '!': SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', ']', '~': SymbolProc;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSyndbAnalystSQLSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSyndbAnalystSQLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSyndbAnalystSQLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSyndbAnalystSQLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSyndbAnalystSQLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkConditionalComment: Result := fConditionalCommentAttri;
    tkDatatype: Result := fDataTypeAttri;
    tkDefaultPackage: Result := fDefaultPackageAttri;
    tkDelimitedIdentifier: Result := fDelimitedIdentifierAttri;
    tkException: Result := fExceptionAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPLSQL: Result := fPLSQLAttri;
    tkSpace: Result := fSpaceAttri;
    tkSQLPlus: Result := fSQLPlusAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTableName: Result := fTableNameAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSyndbAnalystSQLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSyndbAnalystSQLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSyndbAnalystSQLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSyndbAnalystSQLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSQL;
end;

function TSyndbAnalystSQLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    'a'..'z', 'A'..'Z', '0'..'9', '_':
      Result := True;
    '-':
      Result := fDialect = sqlStandard;
    '#', '$':                          // TODO: check this case, ANSI code wasn't clear here if this is exclusively Oracle
      Result := fDialect = sqlOracle;
    '@':
      Result := fDialect in [sqlMSSQL7, sqlMSSQL2K];
    else
      Result := False;
  end;
end;

class function TSyndbAnalystSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSyndbAnalystSQLSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Integer;
begin
  AKeyword := SynWideLowerCase(AKeyword);
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSyndbAnalystSQLSyn.SetTableNames(const Value: TUnicodeStrings);
begin
  fTableNames.Assign(Value);
end;

procedure TSyndbAnalystSQLSyn.TableNamesChanged(Sender: TObject);
begin
  //InitializeKeywordLists;
end;

procedure TSyndbAnalystSQLSyn.PutTableNamesInKeywordList;
var
  i: Integer;
  Entry: TSynHashEntry;
begin
  for i := 0 to fTableNames.Count - 1 do
  begin
    Entry := fKeywords[HashKey(PWideChar(fTableNames[i]))];
    while Assigned(Entry) do
    begin
      if SynWideLowerCase(Entry.Keyword) = SynWideLowerCase(fTableNames[i]) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fTableNames[i], Ord(tkTableName));
  end;
end;

procedure TSyndbAnalystSQLSyn.InitializeKeywordLists;
var
  I: Integer;
begin
  fKeywords.Clear;

  for I := 0 to Ord(High(TtkTokenKind)) - 1 do
    EnumerateKeywords(I, GetKeywords(I), IsIdentChar, DoAddKeyword);

  PutTableNamesInKeywordList;
  DefHighlightChange(Self);
end;

procedure TSyndbAnalystSQLSyn.SetDialect(Value: TSQLDialect);
begin
  if (Value <> fDialect) then
  begin
    fDialect := Value;
    InitializeKeywordLists;
  end;
end;

function TSyndbAnalystSQLSyn.GetSampleSource: UnicodeString;
begin
  Result := '';
  case fDialect of
    sqlPostgres:
      Result := '-- ANSI SQL sample source'#13#10 +
        'SELECT *'#13#10 +
        'FROM planets'#13#10 +
        'WHERE diameter < 13000'#13#10 +
        '  AND name <> ''Earth''';
    sqlStandard:
      Result := '-- ANSI SQL sample source'#13#10 +
        'SELECT *'#13#10 +
        'FROM planets'#13#10 +
        'WHERE diameter < 13000'#13#10 +
        '  AND name <> ''Earth''';
    sqlInterbase6:
      Result := '/* Interbase sample source */'#13#10 +
        'SET TERM !! ;'#13#10 +
        #13#10 +
        'CREATE PROCEDURE HelloWorld(P_MSG VARCHAR(80)) AS'#13#10 +
        'BEGIN'#13#10 +
        '  EXECUTE PROCEDURE WRITELN(:P_MSG);'#13#10 +
        'END !!'#13#10 +
        #13#10 +
        'SET TERM ; !!';
    sqlMySQL:
      Result := '/* MySQL sample source*/'#13#10 +
        'SET @variable = 1;'#13#10 +
        #13#10 +
        'CREATE /*!32302 TEMPORARY */ TABLE t (a INT);'#13#10 +
        #13#10 +
        'CREATE TABLE sample ('#13#10 +
        '        id INT NOT NULL,'#13#10 +
        '        first_name CHAR(30) NOT NULL,'#13#10 +
        '        PRIMARY KEY (id),'#13#10 +
        '        INDEX name (first_name));'#13#10 +
        #13#10 +
        'SELECT DATE_ADD(''1997-12-31 23:59:59'','#13#10 +
        '        INTERVAL 1 SECOND);'#13#10 +
        #13#10 +
        '# End of sample';
    sqlOracle:
      Result := 'PROMPT Oracle sample source'#13#10 +
        'declare'#13#10 +
        '  x varchar2(2000);'#13#10 +
        'begin   -- Show some text here'#13#10 +
        '  select to_char(count(*)) into x'#13#10 +
        '  from tab;'#13#10 +
        #13#10 +
        '  dbms_output.put_line(''Hello World: '' || x);'#13#10 +
        'exception'#13#10 +
        '  when others then'#13#10 +
        '    null;'#13#10 +
        'end;';
    sqlSybase:
      Result := '/* SyBase example source */'#13#10 +
        'declare @Integer        int'#13#10 +
        #13#10 +
        '/* Good for positive numbers only. */'#13#10 +
        'select @Integer = 1000'#13#10 +
        #13#10 +
        'select "Positives Only" ='#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, @Integer),12)'#13#10 +
        #13#10 +
        '/* Good for positive and negative numbers. */'#13#10 +
        'select @Integer = -1000'#13#10 +
        #13#10 +
        'select "Both Signs" ='#13#10 +
        '  substring( "- +", (sign(@Integer) + 2), 1) +'#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, abs(@Integer)),12)'#13#10 +
        #13#10 +
        'select @Integer = 1000'#13#10 +
        #13#10 +
        'select "Both Signs" ='#13#10 +
        '  substring( "- +", (sign(@Integer) + 2), 1) +'#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, abs(@Integer)),12)'#13#10 +
        #13#10 +
        'go';
    sqlIngres:
      Result := '/* Ingres example source */'#13#10 +
        'DELETE'#13#10 +
        'FROM t1'#13#10 +
        'WHERE EXISTS'#13#10 +
        '(SELECT t2.column1, t2.column2'#13#10 +
        'FROM t2'#13#10 +
        'WHERE t1.column1 = t2.column1 and'#13#10 +
        't1.column2 = t2.column2)';
    sqlMSSQL7:
      Result := '/* SQL Server 7 example source */'#13#10 +
        'SET QUOTED_IDENTIFIER ON'#13#10 +
        'GO'#13#10 +
        'SET ANSI_NULLS OFF'#13#10 +
        'GO'#13#10 +
        #13#10 +
        '/* Object:  Stored Procedure dbo.sp_PPQInsertOrder */'#13#10 +
        'CREATE PROCEDURE sp_PPQInsertOrder'#13#10 +
        '  @Name    varchar(25),'#13#10 +
        '  @Address varchar(255),'#13#10 +
        '  @ZipCode varchar(15)'#13#10 +
        'AS'#13#10 +
        '  INSERT INTO PPQOrders(Name, Address, ZipCode, OrderDate)'#13#10 +
        '  VALUES (@Name, @Address, @ZipCode, GetDate())'#13#10 +
        #13#10 +
        '  SELECT SCOPE_IDENTITY()'#13#10 +
        'GO';
    sqlMSSQL2K:
      Result := '/* SQL Server2000 example source */'#13#10 +
        'SET QUOTED_IDENTIFIER ON'#13#10 +
        'GO'#13#10 +
        'SET ANSI_NULLS OFF'#13#10 +
        'GO'#13#10 +
        #13#10 +
        '/* Object:  Stored Procedure dbo.sp_PPQInsertOrder */'#13#10 +
        'CREATE PROCEDURE sp_PPQInsertOrder'#13#10 +
        '  @Name    varchar(25),'#13#10 +
        '  @Address varchar(255),'#13#10 +
        '  @ZipCode varchar(15)'#13#10 +
        'AS'#13#10 +
        '  INSERT INTO PPQOrders(Name, Address, ZipCode, OrderDate)'#13#10 +
        '  VALUES (@Name, @Address, @ZipCode, GetDate())'#13#10 +
        #13#10 +
        '  SELECT SCOPE_IDENTITY()'#13#10 +
        'GO';
  end;
end;

class function TSyndbAnalystSQLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSQL;
end;

function TSyndbAnalystSQLSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  result := '';
  if longint(TokenKind) < fStringList.Count then
    Result := fStringList[longint(TokenKind)];
end;

procedure TSyndbAnalystSQLSyn.SetWordList(value: TtkTokenKind; WordList: string);
begin
  while longint(value) >= fStringList.Count do
    fStringList.Add('');
  fStringList[longint(value)] := WordList;
  InitializeKeywordLists;
end;

procedure TSyndbAnalystSQLSyn.SetHilightStyle(value: TtkTokenKind; parStyle: TFontStyles; parColor: TColor);
var
  f: TSynHighlighterAttributes;
begin
  f := GetAttri(value);
  if f <> nil then
  begin
    f.Style := parStyle;
    f.Foreground := parColor;
  end;
end;

function TSyndbAnalystSQLSyn.GetAttri(value: TtkTokenKind): TSynHighlighterAttributes;
begin
  result := nil;
  case value of
    tkComment:             result := fCommentAttri;
    tkDatatype:            result := fDataTypeAttri;
    tkDefaultPackage:      result := fDefaultPackageAttri;
    tkException:           result := fExceptionAttri;
    tkFunction:            result := fFunctionAttri;
    tkIdentifier:          result := nil;
    tkKey:                 result := fKeyAttri;
    tkNull:                result := nil;
    tkNumber:              result := fNumberAttri;
    tkSpace:               result := fSpaceAttri;
    tkPLSQL:               result := fPLSQLAttri;
    tkSQLPlus:             result := fSQLPlusAttri;
    tkString:              result := fStringAttri;
    tkSymbol:              result := fSymbolAttri;
    tkTableName:           result := fTableNameAttri;
    tkUnknown:             result := nil;
    tkVariable:            result := fVariableAttri;
    tkConditionalComment:  result := fConditionalCommentAttri;
    tkDelimitedIdentifier: result := fDelimitedIdentifierAttri;
  end;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSyndbAnalystSQLSyn);
{$ENDIF}
end.

