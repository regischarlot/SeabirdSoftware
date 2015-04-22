unit HTMLColorLib;

interface

uses
  Windows,
  Types,
  Classes,
  DataLib,
  daObjectLib,
  daGlobals,
  Graphics,
  PreferenceLib;

type
  TrBand = record
    iStart, iEnd: longint;
    eSyntaxHilite: TeSyntaxHiliteType;
  end;
  TrBandArray = array of TrBand;

  TcHTMLColorParser = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *         Intelligent Medical Objects, Inc.
  *
  * Description: TcCustomDataStream is the base class for lexicon searches
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/31/08 Regis Created
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_sKeywords: array[TeSyntaxHiliteType] of String;
    m_objMetaData: TcMetaData;
    m_objPreferences: TcPreferenceList;

  private
    // Private method
    //
    procedure   LocalClear;
    procedure   SetMetaData(value: TcMetaData);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(Parent: TcObject); override;                             // Standard Contructor
    destructor  Destroy; override;                                              // Standard Destructor
    procedure   Clear; override;
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    Colorize(value: String): String;

  public
    // Public proeprties
    //
    property    MetaData: TcMetaData                                            write SetMetaData;
    property    Preferences: TcPreferenceList                                   write m_objPreferences;
  end;

implementation

uses
  Math,
  FormLib,
  daResourceStrings,
  sysUtils,
  strUtils;

//
// TcHTMLColorParser
//

// TcHTMLColorParser
//   Create
//
constructor TcHTMLColorParser.Create(Parent: TcObject);
begin
  inherited Create(Parent);
  m_objMetaData := nil;
  m_objPreferences := nil;
  LocalClear;
end;

// TcHTMLColorParser
//   Destroy
//
destructor TcHTMLColorParser.Destroy;
begin
  LocalClear;
  inherited Destroy;
end;

// TcHTMLColorParser
//   LocalClear
//
procedure TcHTMLColorParser.LocalClear;
begin

end;

// TcHTMLColorParser
//   Clear
//
procedure TcHTMLColorParser.Clear;
begin
  inherited Clear;
  LocalClear;
end;

// TcHTMLColorParser
//   SetMetaData
//
procedure TcHTMLColorParser.SetMetaData(value: TcMetaData);
var
  e: TeSyntaxHiliteType;
  i: longint;
  p: TcMetaData;
  s: String;
begin
  m_objMetaData := value;
  p := nil;
  if m_objMetaData <> nil then
    for i := 0 to m_objMetaData.Count - 1 do
      if (m_objMetaData[i] <> nil) and (m_objMetaData[i] is TcMetaData) and (m_objMetaData[i].eType = enSyntaxHilighting) then
      begin
        p := m_objMetaData[i] as TcMetaData;
        break;
      end;
  //
  if p <> nil then
  begin
    for i := 0 to p.Count - 1 do
      if (p[i] <> nil) and (p[i] is TcMetaData) and (p[i].eType = enKeyword) then
      begin
        e := StringToSyntaxHiliteType((p[i] as TcMetaData).Attribute[krsTYPE]);
        s := AnsiReplaceText(p[i].sValue, ',', kcSPACE);
        s := AnsiReplaceText(s, kcTAB, kcSPACE);
        s := AnsiReplaceText(s, kcCR, kcSPACE);
        s := AnsiReplaceText(s, kcLF, kcSPACE);
        m_sKeywords[e] := kcSPACE + lowercase(s) + kcSPACE;
      end;
  end;
end;

// TcHTMLColorParser
//   Colorize
//
function TcHTMLColorParser.Colorize(value: String): String;

const
  kasPREFERENCES: array[TeSyntaxHiliteType] of String =
    (krsPREF_SYNTAXKEYWORD, krsPREF_SYNTAXDATATYPE, krsPREF_SYNTAXFUNCTION, krsPREF_SYNTAXMARK, krsPREF_SYNTAXCOMMENT, krsPREF_SYNTAXPACKAGE, krsPREF_SYNTAXEXCEPTION, krsPREF_SYNTAXMARK, krsPREF_SYNTAXMARK);
  kasSEPARATORS: string =
    ' (),./\:;<>[]{}=-@{}<>' + kcTAB + ksCR;
  kcCOMMENT = #7;

  // Tool
  //   AddArr
  //
  function AddArr(var value: TrBandArray; iparStart, iparEnd: longint; eparSyntaxHilite: TeSyntaxHiliteType): longint;
  var
    b: TrBand;
  begin
    b.iStart := iparStart;
    b.iEnd := iparEnd;
    b.eSyntaxHilite := eparSyntaxHilite;
    result := length(value);
    SetLength(value, result + 1);
    value[result] := b;
  end;

  // Tool
  //   IsSpace
  //
  function IsSpace(value: char): boolean;
  begin
    result := pos(value, kasSEPARATORS) > 0;
  end;

  // Tool
  //   IsLineComment
  //
  function IsLineComment(value: string; Index: longint): boolean;
  begin
    result := (value[Index] = '-') and (Index < length(value)) and  (value[Index + 1] = '-');
  end;

var
  s, k: String;
  i, L, N: longint;
  e: TeSyntaxHiliteType;
  b: TrBand;
  arr: TrBandArray;
  col: TColor;
  c: Char;
begin
  result := value;
  arr := nil;
  try
    s := value;
    // c. Tokenize
    L := 1;
    while L < length(s) do
    begin
      if IsSpace(s[L]) and not IsLineComment(s, L) then
        inc(L)
      else
      begin
        N := L;
        // Comment
        if IsLineComment(s, L) then
        begin
          repeat
            inc(N);
          until (N > length(s)) or CharInSet(s[N], [kcLF, kcCR]);
          AddArr(arr, L, N, eshtComment)
        end
        // Text parsing
        else
        begin
          c := kcSPACE;
          if CharInSet(s[N], [kcDBLQUOTE, kcSGLQUOTE]) then
            c := s[N];
          repeat
            inc(N);
          until (N > length(s)) or (((c <> kcSPACE) and (s[N] = c)) or ((c = kcSPACE) and IsSpace(s[N])));
          if CharInSet(s[N], [kcDBLQUOTE, kcSGLQUOTE]) then
            inc(N);
          k := system.copy(s, L, N - L);
          if c = kcSGLQUOTE then
            AddArr(arr, L, N, eshtString)
          else if IsNumber(k) then
            AddArr(arr, L, N, eshtNumber)
          else if c <> kcDBLQUOTE then
            for e := low(TeSyntaxHiliteType) to high(TeSyntaxHiliteType) do
              if pos(kcSPACE + lowercase(k) + kcSPACE, kcSPACE + m_sKeywords[e] + kcSPACE) > 0 then
              begin
                AddArr(arr, L, N, e);
                break;
              end;
        end;
        L := N;
      end;
    end;
    // c. Colorize
    s := value;
    for i := length(arr) - 1 downto 0 do
    begin
      b := arr[i];
      system.insert('</span>', s, b.iEnd);
      col := clRed;
      if m_objPreferences <> nil then
        col := m_objPreferences.Color[kasPREFERENCES[b.eSyntaxHilite]];
      system.insert(Format('<span style="color:#%s">', [ColorToHex(col)]), s, b.iStart);
    end;
    result := s;
  finally
    if arr <> nil then
    begin
      SetLength(arr, 0);
      arr := nil;
    end;
  end;
end;

end.
