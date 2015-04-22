unit SynEditSearchReplaceLib;

interface

uses
  Windows,
  daGlobals,
  daObjectLib,
  Classes,
  SynEdit,
  Controls,
  frmSynEditReplaceText,
  frmSynEditSearchText,
  frmSynEditConfirmReplace,
  SynEditTypes,
  SynEditMiscProcs,
  SynEditRegexSearch,
  SynEditSearch;

type
  TcSynEditSearchReplace = class(TWinControl)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 01/01/06 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_fSearchFromCaret: boolean;
    m_objSynEditSearch: TSynEditSearch;
    m_objSynEditRegexSearch: TSynEditRegexSearch;
    m_frmConfirmReplace: TConfirmReplaceDialog;
    m_synEditor: TSynEdit;
    m_bSearchBackwards: boolean;
    m_bSearchCaseSensitive: boolean;
    m_bSearchFromCaret: boolean;
    m_bSearchSelectionOnly: boolean;
    m_bSearchTextAtCaret: boolean;
    m_bSearchWholeWords: boolean;
    m_bSearchRegex: boolean;
    m_sSearchText: string;
    m_sSearchTextHistory: string;
    m_sReplaceText: string;
    m_sReplaceTextHistory: string;
    m_bCanFindNext: boolean;
    m_bCanFindPrev: boolean;

  private
    // Private methods
    //
    procedure   SetStatusBar(value: String);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;                          // Standard Constructor, virtual
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
    procedure SynEditorReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var Action: TSynReplaceAction);
    function  CanFindNext: boolean;
    function  CanFindPrev: boolean;
    procedure DoSearchReplaceText(AOwner: TComponent; AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AOwner: TComponent; AReplace: boolean);

  public
    // Public Properties
    //
    property bSearchBackwards: boolean      read m_bSearchBackwards        write m_bSearchBackwards;
    property bSearchCaseSensitive: boolean  read m_bSearchCaseSensitive    write m_bSearchCaseSensitive;
    property bSearchFromCaret: boolean      read m_bSearchFromCaret        write m_bSearchFromCaret;
    property bSearchSelectionOnly: boolean  read m_bSearchSelectionOnly    write m_bSearchSelectionOnly;
    property bSearchTextAtCaret: boolean    read m_bSearchTextAtCaret      write m_bSearchTextAtCaret;
    property bSearchWholeWords: boolean     read m_bSearchWholeWords       write m_bSearchWholeWords;
    property bSearchRegex: boolean          read m_bSearchRegex            write m_bSearchRegex;
    property sSearchText: string            read m_sSearchText             write m_sSearchText;
    property sSearchTextHistory: string     read m_sSearchTextHistory      write m_sSearchTextHistory;
    property sReplaceText: string           read m_sReplaceText            write m_sReplaceText;
    property sReplaceTextHistory: string    read m_sReplaceTextHistory     write m_sReplaceTextHistory;
  end;

implementation

uses
  daResourceStrings,
  sysUtils,
  Dialogs,
  Math;

resourcestring
  STextNotFound = 'Text not found';
  SNoSelectionAvailable = 'The is no selection available, search whole text?';

//
// TcSynEditSearchReplace
//

// TcSynEditSearchReplace
//   Create
//
constructor TcSynEditSearchReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  m_objSynEditSearch := TSynEditSearch.Create(AOwner);
  m_objSynEditRegexSearch := TSynEditRegexSearch.Create(AOwner);
  m_frmConfirmReplace := nil;
  m_bCanFindNext := FALSE;
  m_bCanFindPrev := FALSE;
end;

// TcSynEditSearchReplace
//   SynEditorReplaceText
//
procedure TcSynEditSearchReplace.SynEditorReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if (Sender <> nil) and (Sender is TSynEdit) then
  begin
    m_synEditor := Sender as TSynEdit;
    if ASearch = AReplace then
      Action := raSkip
    else
    begin
      APos := m_synEditor.ClientToScreen(m_synEditor.RowColumnToPixels(m_synEditor.BufferToDisplayPos(BufferCoord(Column, Line))));
      EditRect := ClientRect;
      EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
      EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);
      if m_frmConfirmReplace = nil then
        m_frmConfirmReplace := TConfirmReplaceDialog.Create(Parent);
      m_frmConfirmReplace.PrepareShow(EditRect, APos.X, APos.Y, APos.Y + m_synEditor.LineHeight, ASearch);
      case m_frmConfirmReplace.ShowModal of
        mrYes:
          Action := raReplace;
        mrYesToAll:
          Action := raReplaceAll;
        mrNo:
          Action := raSkip;
        else
          Action := raCancel;
      end;
    end;
  end;
end;

// TcSynEditSearchReplace
//   CanFindNext
//
function TcSynEditSearchReplace.CanFindNext: boolean;
begin
  Result := (m_synEditor <> nil) and (sSearchText <> ksEMPTY) and m_bCanFindNext;
end;

// TcSynEditSearchReplace
//   CanFindPrev
//
function TcSynEditSearchReplace.CanFindPrev: boolean;
begin
  result := m_bCanFindPrev;
end;

// TcSynEditSearchReplace
//   ShowSearchReplaceDialog
//
procedure TcSynEditSearchReplace.ShowSearchReplaceDialog(AOwner: TComponent; AReplace: boolean);
var
  dlg: TTextSearchDialog;
begin
  if (AOwner <> nil) and (AOwner is TSynEdit) then
  begin
    m_synEditor := AOwner as TSynEdit;
    SetStatusBar(ksEMPTY);
    m_bCanFindNext := TRUE;
    m_bCanFindPrev := TRUE;
    if AReplace then
      dlg := TTextReplaceDialog.Create(Self)
    else
      dlg := TTextSearchDialog.Create(Self);
    with dlg do
    try
      // Selected Text?
      if (AOwner <> nil) and (AOwner is TSynEdit) then
        sSearchText := (AOwner as TSynEdit).SelText;
      // assign search options
      SearchBackwards := bSearchBackwards;
      SearchCaseSensitive := bSearchCaseSensitive;
      SearchFromCursor := bSearchFromCaret;
      SearchInSelectionOnly := bSearchSelectionOnly;
      // start with last search text
      SearchText := sSearchText;
      if bSearchTextAtCaret then
      begin
        // if something is selected search for that text
        if m_synEditor.SelAvail and (m_synEditor.BlockBegin.Line = m_synEditor.BlockEnd.Line) then
          SearchText := m_synEditor.SelText
        else
          SearchText := m_synEditor.GetWordAtRowCol(m_synEditor.CaretXY);
      end;
      SearchTextHistory := sSearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do
      begin
        ReplaceText := sReplaceText;
        ReplaceTextHistory := sReplaceTextHistory;
      end;
      SearchWholeWords := bSearchWholeWords;
      if ShowModal = mrOK then
      begin
        bSearchSelectionOnly := SearchInSelectionOnly;
        bSearchBackwards := SearchBackwards;
        bSearchCaseSensitive := SearchCaseSensitive;
        bSearchFromCaret := SearchFromCursor;
        bSearchWholeWords := SearchWholeWords;
        bSearchRegex := SearchRegularExpression;
        sSearchText := SearchText;
        sSearchTextHistory := SearchTextHistory;
        if AReplace then with dlg as TTextReplaceDialog do
        begin
          sReplaceText := ReplaceText;
          sReplaceTextHistory := ReplaceTextHistory;
        end;
        m_fSearchFromCaret := bSearchFromCaret;
        if sSearchText <> ksEMPTY then
        begin
          DoSearchReplaceText(m_synEditor, AReplace, bSearchBackwards);
          m_fSearchFromCaret := TRUE;
        end;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

// TcSynEditSearchReplace
//   DoSearchReplaceText
//
procedure TcSynEditSearchReplace.DoSearchReplaceText(AOwner: TComponent; AReplace: boolean; ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  if (AOwner <> nil) and (AOwner is TSynEdit) then
  begin
    m_synEditor := AOwner as TSynEdit;
    SetStatusbar(ksEMPTY);
    if AReplace then
      Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
    else
      Options := [];
    if ABackwards then
      Include(Options, ssoBackwards);
    if bSearchCaseSensitive then
      Include(Options, ssoMatchCase);
    if not m_fSearchFromCaret then
      Include(Options, ssoEntireScope);
    if bSearchSelectionOnly then
    begin
      if (not m_synEditor.SelAvail) or SameText(m_synEditor.SelText, sSearchText) then
      begin
        if MessageDlg(SNoSelectionAvailable, mtWarning, [mbYes, mbNo], 0) = mrYes then
          bSearchSelectionOnly := False
        else
          Exit;
      end
      else
        Include(Options, ssoSelectedOnly);
    end;
    if bSearchWholeWords then
      Include(Options, ssoWholeWord);
    if bSearchRegex then
      m_synEditor.SearchEngine := m_objSynEditRegexSearch
    else
      m_synEditor.SearchEngine := m_objSynEditSearch;
    m_bCanFindNext := TRUE;
    m_bCanFindPrev := TRUE;
    if m_synEditor.SearchReplace(sSearchText, sReplaceText, Options) = 0 then
    begin
      MessageBeep(MB_ICONASTERISK);
      SetStatusBar(STextNotFound);
      if ssoBackwards in Options then
        m_synEditor.BlockEnd := m_synEditor.BlockBegin
      else
        m_synEditor.BlockBegin := m_synEditor.BlockEnd;
      m_synEditor.CaretXY := m_synEditor.BlockBegin;
      m_bCanFindNext := ssoBackwards in Options;
      m_bCanFindPrev := not m_bCanFindNext;
    end;
    if m_frmConfirmReplace <> nil then
    begin
      m_frmConfirmReplace.Free;
      m_frmConfirmReplace := nil;
    end;
  end;
end;

// TcSynEditSearchReplace
//   SetStatusBar
//
procedure TcSynEditSearchReplace.SetStatusBar(value: String);
begin

end;

//
//   Initialization Section
//
initialization

end.
