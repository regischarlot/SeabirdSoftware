unit Grid;

interface

uses
  Windows, daObjectLib, Classes, ComCtrls, Messages, Controls, Types, CommCtrl,
  Graphics, daGlobals, StdCtrls, ExtCtrls, Buttons, ImgList;

const
  kiINITIALROWS         = 20;
  LVM_FIRST             = $1000;      { ListView messages }
  LVM_GETTOPINDEX       = LVM_FIRST + 39;
  LVM_REDRAWITEMS       = LVM_FIRST + 21;
  LVM_GETCOUNTPERPAGE   = LVM_FIRST + 40;
  ksCR                  = #$D#$A;
  ksEMPTY               = '';
  kiUNDEFINED           = -1;

type
  TGridResizeEvent = procedure(Sender: TCustomListview; columnindex: Integer; columnwidth: Integer) of object;
  TGridMessage = procedure(Sender: TObject; value: String) of object;
  TeGridDatatype = (dtUndefined, dtString, dtLongString, dtNumber, dtBoolean, dtDictionary);
  TGrid = class;
  TGridBooleanArray = array[boolean] of longint;
  TeGridState = (egsRW, egsPreviousRO, egsRO);

  TGridComboBoxButton = class(TSpeedButton)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, LLC
  *
  * Description:
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/23/03 Regis Created
  ******************************************************************************}
  protected
    // protected methods
    //
    procedure Paint; override;
  end;

  TGridComboBox = class(TCustomPanel)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, LLC
  *
  * Description:
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/23/03 Regis Created
  ******************************************************************************}
  private
    // private members
    //
    FButton: TGridComboBoxButton;
    FEdit: TEdit;
    FListBox: TListBox;

  private
    // private methods
    //
    function    GetDroppedDown: boolean;
    procedure   SetColor(value: TColor);
    function    GetText: String;
    procedure   SetText(value: String);
    procedure   DropDown(Sender: TObject);
    function    GetItems: TStrings;
    procedure   SetItems(value: TStrings);
    procedure   EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   ListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function    GetCharCase: TEditCharCase;
    procedure   SetCharCase(value: TEditCharCase);

  public
    // Public methods
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    //
    // 2. Custom
    procedure   SetFocus; override;

  public
    // Public properties
    //
    property    DroppedDown: boolean                            read GetDroppedDown;
    property    Color: TColor                                                                   write SetColor;
    property    Items: TStrings                                 read GetItems                   write SetItems;
    property    Text: String                                    read GetText                    write SetText;
    property    CharCase: TEditCharCase                         read GetCharCase                write SetCharCase;
  end;

  TGridCheckBox = class(TCustomControl)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, LLC
  *
  * Description:
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/23/03 Regis Created
  ******************************************************************************}
  private
    // private members
    //
    FChecked: boolean;
    FImages: TCustomImageList;
    FStateImages: TGridBooleanArray;

  private
    // private methods
    //
    procedure   SetChecked(value: boolean);
    procedure   OnMouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnCheckBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  protected
    // protected methods
    //
    procedure   Paint; override;

  public
    // Public methods
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    //
    // 2. Custom

  public
    // Public properties
    //
    property    Checked: boolean                                read FChecked                   write SetChecked;
    property    Images: TCustomImageList                        read FImages                    write FImages;
    property    StateImages: TGridBooleanArray                  read FStateImages               write FStateImages;
  end;

  TGridColumn = class;

  TGrid = class(TCustomListView)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, LLC
  *
  * Description:
  *
  * Inheritance: TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 09/02/02 Regis Created
  ******************************************************************************}
  private
    // private members
    //
    m_lstColumns: TcObject;
    m_pCell: TPoint;
    m_FBeginColumnResizeEvent: TGridResizeEvent;
    m_FEndColumnResizeEvent: TGridResizeEvent;
    m_FColumnResizeEvent: TGridResizeEvent;
    m_FDisplayMessage: TGridMessage;
    m_iSelectedColumn: longint;
    m_colBackgroundColor: TColor;
    m_colSelectedColumnColor: TColor;
    m_colSelection: TColor;
    m_colCurrentLineColor: TColor;
    m_lstCellRows: TcObject;
    m_objCombo: TGridComboBox;
    m_objEdit: TEdit;
    m_objCheckBox: TGridCheckBox;
    m_bColumnSort: boolean;
    FImages: TCustomImageList;
    FOnChangeValue: TNotifyEvent;
    FEditState: boolean;
    FItemHeight: longint;
    FGridState: TeGridState;

  private
    // private methods
    //
    procedure   dbCustomDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    procedure   OnMouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function    CellMessage: String;
    procedure   onColumnClickEvent(Sender: TObject; Column: TListColumn);
    procedure   SetBackgroundColor(value: TColor);
    procedure   SetSelectedColumnColor(value: TColor);
    procedure   SetSelectionColor(value: TColor);
    procedure   SetCurrentLineColor(value: TColor);
    procedure   onContextPopupEvent(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    function    GetColumn(Index: longint): TGridColumn;
    procedure   SetColumn(Index: longint);
    procedure   SetCell(iRow, iColumn: longint; value: String);
    function    GetCell(iRow, iColumn: longint): String;
    function    GetAsString(iRow, iColumn: longint): String;
    procedure   SetAsString(iRow, iColumn: longint; value: String);
    function    GetAsLongint(iRow, iColumn: longint): longint;
    procedure   SetAsLongint(iRow, iColumn: longint; value: longint);
    function    GetAsBoolean(iRow, iColumn: longint): boolean;
    procedure   SetAsBoolean(iRow, iColumn: longint; value: boolean);
    procedure   SetColumnValues(Index: longint; value: TStrings);
    function    GetRect: TRect;
    procedure   OnFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   onMoveField(bActivate, bHide: boolean);
    procedure   SetImages(value: TCustomImageList);
    function    GetColumnCount: longint;
    function    GetData(Index: longint): TcObject;
    procedure   SetData(Index: longint; value: TcObject);
    procedure   OnGridKeyPress(Sender: TObject; var Key: Char);
    function    GetUsableRect: TRect;
    procedure   WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure   WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure   UpdateFieldPosition;
    function    GetCheckAllRequired: boolean;

  protected
    // private protected
    //
    procedure   DoBeginColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure   DoEndColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure   DoColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure   WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    function    FindColumnIndex(pHeader: pNMHdr): Integer;
    function    FindColumnWidth(pHeader: pNMHdr): Integer;
    procedure   CreateWnd; override;
    procedure   DoEnter; override;
    procedure   DoExit; override;
    procedure   SetItemIndex(const Value: Integer); override;

  public
    // Public methods
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; override;                                                // Clear base method
    //
    // 2. Custom
    function    CellAt(Sender: TObject; X, Y: Integer): TPoint;
    procedure   ClearColumns;
    function    CheckRequiredFields(Index: longint; bMessage: boolean): boolean;
    procedure   DeleteSelected; override;

  public
    // Public properties
    //
    property    ColumnCount: longint                            read GetColumnCount;
    property    Column[Index: longint]: TGridColumn             read GetColumn;
    property    AsString[iRow, iColumn: longint]: String        read GetAsString                write SetAsString;
    property    AsInteger[iRow, iColumn: longint]: longint      read GetAsLongint               write SetAsLongint;
    property    AsBoolean[iRow, iColumn: longint]: boolean      read GetAsBoolean               write SetAsBoolean;
    property    ColumnValues[Index: longint]: TStrings                                          write SetColumnValues;
    property    Data[Index: longint]: TcObject                  read GetData                    write SetData;
    property    EditState: boolean                              read FEditState                 write FEditState;
    property    UsableRect: TRect                               read GetUsableRect;
    property    CheckAllRequired: boolean                       read GetCheckAllRequired;
    property    GridState: TeGridState                          read FGridState                 write FGridState;

  published
    // Published properties
    //
    property    OnBeginColumnResize: TGridResizeEvent           read m_FBeginColumnResizeEvent  write m_FBeginColumnResizeEvent;
    property    OnEndColumnResize: TGridResizeEvent             read m_FEndColumnResizeEvent    write m_FEndColumnResizeEvent;
    property    OnColumnResize: TGridResizeEvent                read m_FColumnResizeEvent       write m_FColumnResizeEvent;
    property    OnDisplayMessage: TGridMessage                  read m_FDisplayMessage          write m_FDisplayMessage;
    property    BackgroundColor: TColor                         read m_colBackgroundColor       write SetBackgroundColor;
    property    SelectedColumnColor: TColor                     read m_colSelectedColumnColor   write SetSelectedColumnColor;
    property    SelectionColor: TColor                          read m_colSelection             write SetSelectionColor;
    property    CurrentLineColor: TColor                        read m_colCurrentLineColor      write SetCurrentLineColor;
    property    Coordinates: TPoint                             read m_pCell;
    property    ColumnSort: boolean                             read m_bColumnSort              write m_bColumnSort;
    property    Images: TCustomImageList                        read FImages                    write SetImages;
    property    OnChangeValue: TNotifyEvent                     read FOnChangeValue             write FOnChangeValue;
    property    Action;
    property    Align;
    property    AllocBy;
    property    Anchors;
    property    BevelEdges;
    property    BevelInner;
    property    BevelOuter;
    property    BevelKind default bkNone;
    property    BevelWidth;
    property    BiDiMode;
    property    BorderStyle;
    property    BorderWidth;
    property    Checkboxes;
    property    ColumnClick;
    property    Constraints;
    property    Ctl3D;
    property    DragCursor;
    property    DragKind;
    property    DragMode;
    property    Enabled;
    property    Font;
    property    FlatScrollBars;
    property    FullDrag;
    property    GridLines;
    property    HideSelection;
    property    HotTrack;
    property    HotTrackStyles;
    property    HoverTime;
    property    IconOptions;
    property    Items;
    property    MultiSelect;
    property    OwnerData;
    property    OwnerDraw;
    property    RowSelect;
    property    ParentBiDiMode;
    property    ParentColor default False;
    property    ParentFont;
    property    ParentShowHint;
    property    PopupMenu;
    property    ShowColumnHeaders;
    property    ShowWorkAreas;
    property    ShowHint;
    property    SortType;
    property    TabOrder;
    property    TabStop default True;
    property    ViewStyle;
    property    Visible;
    property    OnAdvancedCustomDraw;
    property    OnAdvancedCustomDrawItem;
    property    OnAdvancedCustomDrawSubItem;
    property    OnChange;
    property    OnChanging;
    property    OnClick;
    property    OnColumnClick;
    property    OnColumnDragged;
    property    OnColumnRightClick;
    property    OnCompare;
    property    OnContextPopup;
    property    OnCustomDraw;
    property    OnCustomDrawSubItem;
    property    OnData;
    property    OnDataFind;
    property    OnDataHint;
    property    OnDataStateChange;
    property    OnDblClick;
    property    OnDeletion;
    property    OnEdited;
    property    OnEditing;
    property    OnEndDock;
    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnGetImageIndex;
    property    OnGetSubItemImage;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnInfoTip;
    property    OnInsert;
    property    OnKeyPress;
    property    OnKeyUp;
    property    OnMouseMove;
    property    OnResize;
    property    OnSelectItem;
    property    OnStartDock;
    property    OnStartDrag;
  end;

  TGridColumn = class(TcObject)
  private
    // private members
    //
    FWidth: longint;
    FeDatatype: TeGridDatatype;
    FValues: TStringList;
    FStateImages: TGridBooleanArray;
    FGrid: TGrid;
    FIsReadOnly: boolean;
    FMetaData: TcObject;
    FMaxLength: longint;
    FFilter: String;
    FRequired: boolean;
    FCharCase: TEditCharCase;
    FTRUE, FFALSE: String;
    FabBOOLEAN: array[boolean] of String;

  private
    // private methods
    //
    procedure   SetWidth(value: longint);
    procedure   SetCaption(value: string);
    procedure   SetStateImages(value: TGridBooleanArray);
    procedure   SetStateImage(index: boolean; value: longint);
    function    GetStateImage(index: boolean): longint;
    function    GetBooleanValues: String;
    procedure   SetBooleanValues(value: String);
    function    GetBooleanState(value: boolean): String;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    procedure   Assign(value: TStrings);

  public
    // Public properties
    //
    property    Width: longint                                  read FWidth                     write SetWidth;
    property    Caption: String                                                                 write SetCaption;
    property    StateImages: TGridBooleanArray                  read FStateImages               write SetStateImages;
    property    StateImage[Index: boolean]: longint             read GetStateImage              write SetStateImage;
    property    Datatype: TeGridDatatype                        read FeDatatype                 write FeDatatype;
    property    IsReadOnly: boolean                             read FIsReadOnly                write FIsReadOnly;
    property    MetaData: TcObject                              read FMetaData                  write FMetaData;
    property    MaxLength: longint                              read FMaxLength                 write FMaxLength;
    property    Filter: String                                  read FFilter                    write FFilter;
    property    Required: boolean                               read FRequired                  write FRequired;
    property    CharCase: TEditCharCase                         read FCharCase                  write FCharCase;
    property    BooleanValues: String                           read GetBooleanValues           write SetBooleanValues;
    property    BooleanState[value: boolean]: String            read GetBooleanState;
  end;

procedure Register;

function StringToGridDatatype(value: String): TeGridDatatype;
function StringToGridState(value: String): TeGridState;

implementation

uses
  ComObj,
  sysUtils,
  StrUtils,
  Variants,
  Math,
{$IFNDEF PACKAGE}
  DataLib,
{$ENDIF}
  Forms,
  daStreamLib,
  daResourceStrings;

procedure Register;
begin
  RegisterComponents('Seabird', [TGrid]);
end;  {Register}

const
  kcolGrid         = $C0C0C0;
  kcolGrid2        = $B0B0B0;
  kcolSelectedRect = clBlack;

//
// TGrid
//

// TGrid
//   Create
//
constructor TGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Members
  m_lstColumns := TcObject.Create(nil);
  // Visual Properties
  BevelOuter := bvNone;
  HideSelection := False;
  inherited ReadOnly := True;
  RowSelect := True;
  ViewStyle := vsReport;
  ShowHint := FALSE;
  OwnerDraw := TRUE;
  OnDrawItem := dbCustomDrawItem;
  OnColumnClick := onColumnClickEvent;
  OnMouseDown := OnMouseDownClick;
  m_pCell := point(kiUNDEFINED, kiUNDEFINED);
  onKeyDown := OnListViewKeyDown;
  m_FBeginColumnResizeEvent := nil;
  m_FEndColumnResizeEvent := nil;
  m_FColumnResizeEvent := nil;
  m_FDisplayMessage := nil;
  m_colBackgroundColor := clWindow;
  m_colSelectedColumnColor := clCaptionText;
  m_colCurrentLineColor := clMenuBar;
  m_colSelection := clMenuHighlight;
  FOnChangeValue := nil;
  AllocBy := kiINITIALROWS;
  OnContextPopup := onContextPopupEvent;
  m_lstCellRows := TcObject.Create(nil);
  m_bColumnSort := TRUE;
  FEditState := FALSE;
  FImages := nil;
  FItemHeight := 0;
  FGridState := egsRW;
  //
  m_objCombo := TGridComboBox.Create(self);
  m_objCombo.Parent := self;
  m_objCombo.visible := FALSE;
  m_objCombo.Height := 1;
  m_objCombo.OnKeyDown := OnFieldKeyDown;
  m_objCombo.OnKeyPress := OnGridKeyPress;
  //
  m_objEdit := TEdit.Create(self);
  m_objEdit.Parent := self;
  m_objEdit.visible := FALSE;
  m_objEdit.Height := 1;
  m_objEdit.OnKeyDown := OnFieldKeyDown;
  m_objEdit.BorderStyle := bsNone;
  m_objEdit.OnKeyPress := OnGridKeyPress;
  //
  m_objCheckBox := TGridCheckBox.Create(self);
  m_objCheckBox.Parent := self;
  m_objCheckBox.visible := FALSE;
  m_objCheckBox.Height := 1;
end;

// TGrid
//   Destroy
//
destructor TGrid.Destroy;
begin
  m_lstColumns.Free;
  m_lstCellRows.Free;
  inherited Destroy;
end;

// TGrid
//   Clear
//
procedure TGrid.Clear;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
end;

// TGrid
//   dbCustomDrawItem
//
procedure TGrid.dbCustomDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
(*
  List views receive several other custom draw events, including OnCustomDraw, OnCustomDrawItem,
  OnCustomDrawSubItem, OnAdvancedCustomDraw, OnAdvancedCustomDrawItem, and OnAdvancedCustomDrawSubItem.
  These other events, unlike OnDrawItem, occur regardless of the value of the OwnerDraw property.
  They provide slightly different information about the state of the item to be drawn, and the OnAdvancedXXX
  events occur at more points during the paint process.
*)
  procedure DrawText(r: TRect; sText: String; FBitmap: TBitmap);
  begin
    if FBitmap <> nil then
    begin
      Canvas.FillRect(Types.Rect(r.left + 1, r.top + 1, r.right, r.bottom));
      Canvas.Draw(r.left + (r.right - r.left + 1 - FBitmap.Width) div 2, r.top + (r.bottom - r.top + 1 - FBitmap.Height) div 2, FBitmap);
    end
    else
      Canvas.TextRect(Types.Rect(r.left + 1, r.top + 1, r.right, r.bottom), r.Left + 2, r.Top, sText);
  end;

  procedure DrawFrame(r: TRect; bLast: boolean);
  begin
    if not bLast then
      Canvas.FrameRect(Types.Rect(r.left, r.top, r.right + 1, r.bottom + 1))
    else
      Canvas.FrameRect(Types.Rect(r.left, r.top, r.right, r.bottom + 1));
  end;

  function GetDrawColor(i: longint): TColor;
  begin
    if m_bColumnSort and (i = m_iSelectedColumn) and (m_pCell.Y <> Item.Index) then
      // Color for Sort Column
      result := m_colSelectedColumnColor
    else if m_pCell.Y = Item.Index then
      // Color for current line
      result := m_colCurrentLineColor
    else
      // Color for Normal Background
      result := m_colBackgroundColor;
  end;

var
  i, j, L: longint;
  p: TListColumn;
  s: String;
  r: TRect;
  b: boolean;
  FBitmap: TBitmap;
  c: TGridColumn;
begin
  FItemHeight := max(FItemHeight, Rect.Bottom - Rect.Top + 1);
  if (Sender <> nil) and (Sender is TGrid) then
  begin
    // Background Color
    b := m_pCell.Y = Item.Index;
    Canvas.Font.Color := clDefault;
    L := 0;
    for i := 0 to Columns.Count - 1 do
    begin
      c := GetColumn(i);
      p := Columns[i];
      s := ksEMPTY;
      if i = 0 then
        s := String(Item.Caption)
      else
      begin
        if i - 1 > Item.SubItems.Count - 1 then
          while i - 1 > Item.SubItems.Count - 1 do
            Item.SubItems.Add(ksEMPTY);
        s := String(Item.SubItems[i - 1]);
      end;
      for j := 1 to length(s) do
        if s[j] = #0 then
          s[j] := ' ';
      r := Rect;
      r.Left := L + Rect.Left;
      if p.Width > 0 then
        r.Right := r.Left + p.Width;
      FBitmap := nil;
      if r.Right > 0 then
      try
        if (c.Datatype = dtBoolean) and (FImages <> nil) then
        begin
          FBitmap := TBitmap.Create;
          FImages.GetBitmap(c.StateImages[AnsiCompareText(s, c.BooleanState[TRUE]) = 0], FBitmap);
          FImages.BkColor := GetDrawColor(i);
          FBitmap.Transparent := TRUE;
        end;
        //
        // Current Cell
        if b and (m_pCell.X = i) then
        begin
          // Color for selected cell
          Canvas.Brush.Color := m_colSelection;
          DrawText(r, s, FBitmap);
          Canvas.Brush.Color := kcolGrid2;
          DrawFrame(r, i = Columns.Count - 1);
        end
        else
        begin
          Canvas.Brush.Color := GetDrawColor(i);
          if (i <> m_iSelectedColumn) or not m_bColumnSort then
          begin
            DrawText(r, s, FBitmap);
            Canvas.Brush.Color := kcolGrid;
            DrawFrame(r, i = Columns.Count - 1);
          end
          else
          begin
            DrawText(r, s, FBitmap);
            DrawFrame(r, i = Columns.Count - 1);
          end;
        end;
      finally
        FBitmap.Free;
      end;
      L := L + Columns[i].Width;
    end;
  end;
end;

// TGrid
//   GetRect
//
function TGrid.GetRect: TRect;
var
  i, L: longint;
  p: TListColumn;
  r: TRect;
begin
  result := Rect(0, 0, 0, 0);
  if (m_pCell.X > kiUNDEFINED) and (m_pCell.X < Columns.Count) and (m_pCell.Y > kiUNDEFINED) and (m_pCell.Y < Items.Count) then
  begin
    r := Items[m_pCell.Y].DisplayRect(drBounds);
    L := r.Left;
    for i := 0 to m_pCell.X - 1 do
    begin
      p := Columns[i];
      inc(L, p.Width);
    end;
    result.Left := L;
    p := Columns[m_pCell.X];
    result.Right := result.Left + p.Width;
    result.Top := r.Top;
    result.Bottom := r.Bottom;
  end;
end;

// TGrid
//   OnMouseDownClick
//
procedure TGrid.OnMouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  d, L: TPoint;
  c: TGridColumn;
  b: boolean;
begin
  d := m_pCell;
  L := CellAt(Sender, X, Y);
  b := TRUE;
  if (L.Y <> kiUNDEFINED) and (L.Y <> d.Y) and (d.Y <> kiUNDEFINED) then
  begin
    onMoveField(FALSE, FALSE);
    b := CheckRequiredFields(L.Y, TRUE);
  end;
  if not b then
    onMoveField(TRUE, FALSE)
  else
  begin
    onMoveField(FALSE, TRUE);
    if (L.X <> kiUNDEFINED) and (L.Y <> kiUNDEFINED) then
    begin
      m_pCell := L;
      // Cell is check box?
      if (m_pCell.X >= 0) and (m_pCell.X < Items.Count) then
      begin
        c := GetColumn(m_pCell.X);
        if (c <> nil) and (c.Datatype = dtBoolean) and not c.IsReadOnly then
          AsString[m_pCell.Y, m_pCell.X] := c.BooleanState[AnsiCompareText(AsString[m_pCell.Y, m_pCell.X], c.BooleanState[TRUE]) <> 0];
      end;
      // Redraw
      if (m_pCell.Y <> d.Y) and (d.Y >= 0) and (d.Y < Items.Count) then
        dbCustomDrawItem(Sender as TCustomListView, (Sender as TGrid).Items[d.Y], (Sender as TGrid).Items[d.Y].DisplayRect(drBounds), []);
      dbCustomDrawItem(Sender as TCustomListView, (Sender as TGrid).Items[m_pCell.Y], (Sender as TGrid).Items[m_pCell.Y].DisplayRect(drBounds), []);
      onMoveField(TRUE, TRUE);
      if Assigned(m_FDisplayMessage) then
        m_FDisplayMessage(Sender, CellMessage);
    end;
  end;
end;

// TGrid
//   OnListViewKeyDown
//
procedure TGrid.OnListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  L, i: longint;
  d: TPoint;
  c: TGridColumn;
begin
  // Prevent Flashing on same key
  if not (Key in [36, 33, 35, 34, 40, 37, 39]) or
     ((m_pCell.Y > 0) and (Key in [36, 33])) or                         // Home, Page Up, Up
     ((m_pCell.Y < Items.Count - 1) and (Key in [35, 34, 40])) or       // End, Page Down, Down
     ((m_pCell.X > 0) and (Key in [37])) or                             // Left
     ((m_pCell.X < Columns.Count - 1) and (Key in [39])) then           // Right
  // Provent key
  begin
    c := GetColumn(m_pCell.X);
    //onMoveField(Key <> 32, FALSE);
    if (Key in [36, 35, 33, 34, 38, 40]) and // Home, End, Page Up, Page Down, Up, Down
       (m_pCell.Y > kiUNDEFINED) and (m_pCell.Y < Items.Count) and
       not CheckRequiredFields(m_pCell.Y, TRUE) then
      onMoveField(TRUE, FALSE)
    else
    try
      if (Key = 32) and (c <> nil) and (c.Datatype = dtBoolean) then
        m_objCheckBox.Checked := not m_objCheckBox.Checked;
      if (Key in [33, 34, 35, 36, 37, 38, 39, 40, 32]) then
        onMoveField(key = 32, key <> 32);
      d := m_pCell;
      if (Key = 36) then                                            // Home
        m_pCell.Y := 0
      else if (Key = 35) then                                       // End
        m_pCell.Y := Items.Count - 1
      else if (Key = 33) then                                       // Page Up
        m_pCell.Y := max(m_pCell.Y - VisibleRowCount, 0)
      else if (Key = 34) then                                       // Page Down
        m_pCell.Y := min(m_pCell.Y + VisibleRowCount, Items.Count - 1)
      else if (Key = 38) and (m_pCell.Y > 0) then                   // Up
        m_pCell.Y := min(m_pCell.Y - 1, Items.Count - 1)
      else if (Key = 40) and (m_pCell.Y < Items.Count - 1) then     // Down
        m_pCell.Y := max(m_pCell.Y + 1, 0)
      else if (Key = 39) and (m_pCell.X < Columns.Count - 1) then   // Right
        m_pCell.X := m_pCell.X + 1
      else if (Key = 37) and (m_pCell.X > 0) then                   // Left
        m_pCell.X := m_pCell.X - 1;
      if (Sender is TGrid) and
         (Key in [33, 34, 35, 36, 37, 38, 39, 40, 32]) and
         (m_pCell.Y >= 0) and
         (m_pCell.Y < Items.Count) then
      begin
        if (m_pCell.Y <> d.Y) and (d.Y >= 0) and (d.Y < Items.Count) then
          dbCustomDrawItem(Sender as TCustomListView, (Sender as TGrid).Items[d.Y], (Sender as TGrid).Items[d.Y].DisplayRect(drBounds), []);
        dbCustomDrawItem(Sender as TCustomListView, (Sender as TGrid).Items[m_pCell.Y], (Sender as TGrid).Items[m_pCell.Y].DisplayRect(drBounds), []);
        // Check for vertical scroll
        if Key in [33, 34, 35, 36, 38, 40] then
        begin
          Selected := Items[m_pCell.Y];
          Items[m_pCell.Y].MakeVisible(FALSE);
          if Assigned(onChange) then
            OnChange(Sender, Selected, ctState);
        end;
        onMoveField(TRUE, Key <> 32);
        // Check for horizontal scroll
        if Key in [39, 37] then
        begin
          d.X := GetScrollPos(Handle, SB_HORZ);
          L := 0;
          for i := 0 to m_pCell.X - 1 do
            inc(L, Columns[i].Width);
          if (L >= Width + d.X) then
            Scroll(L - (Width + d.X) + abs(Columns[m_pCell.X].Width), 0)
          else if (L < d.X) then
            Scroll(L - d.x, 0);
        end;
        // ListView_Scroll(hwndLV: HWnd; DX, DY: Integer): Bool;
        Key := 0;
        if Assigned(m_FDisplayMessage) then
          m_FDisplayMessage(Sender, CellMessage);
      end;
    except
      //
    end;
  end;
end;

// TGrid
//   OnFieldKeyDown
//
procedure TGrid.OnFieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (
       not (ssAlt in Shift) and
       (
         (Key = 13) or   // Enter
         (Key = 36) or   // Home
         (Key = 35) or   // End
         (Key = 33) or   // Page Up
         (Key = 34) or   // Page Down
         (Key = 38) or   // Up
         (Key = 40) or   // Down
         (Key = 39) or   // Right
         (Key = 37)      // Left
       ) and
       ( not (Sender is TGridComboBox) or
         (
           (Sender is TGridComboBox) and
           not (Sender as TGridComboBox).DroppedDown
         )
       )
     ) then
    OnListViewKeyDown(Self, Key, Shift);
end;

// TGrid
//   onMoveField
//
procedure TGrid.onMoveField(bActivate, bHide: boolean);

  function LocalIsRW(parcolumn: TGridColumn; value: TcObject): boolean;
{$IFNDEF PACKAGE}
  var
    p: TcObject;
{$ENDIF}
  begin
    result := FALSE;
{$IFNDEF PACKAGE}
    if (parcolumn <> nil) and (parColumn.MetaData <> nil) and (value <> nil) and (value is TcData) then
    begin
      result := (value as TcData).State = edsAdded;
      if result and ((value as TcData).MetaData <> nil) then
      begin
        p := (value as TcData).MetaData.Find(parColumn.MetaData.sName);
        if (p <> nil) and (p is TcMetaData) then
          result := (not (p as TcMetaData).HasAttribute[krsREADONLY]) or
                    (AnsiCompareText((p as TcMetaData).Attribute[krsREADONLY], krsFALSE) = 0) or
                    (AnsiCompareText((p as TcMetaData).Attribute[krsREADONLY], krsMODIFY) = 0);
      end;
    end;
{$ENDIF}
  end;

{$IFNDEF PACKAGE}
var
  r: TRect;
  p: TGridColumn;
  d, f: TcObject;
  b, bRW: boolean;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  p := GetColumn(m_pCell.X);
  d := GetData(m_pCell.Y);
  bRW := (FGridState = egsRW) or ((FGridState = egsPreviousRO) and (d <> nil) and (d is TcData) and ((d as TcData).State = edsAdded));
  if bActivate and bRW then
  begin
    r := GetRect;
    if not ((r.Left = 0) and (r.Top = 0) and (r.Right = 0) and (r.Bottom = 0)) and (p <> nil) and (not p.IsReadOnly or LocalIsRW(p, d)) then
      case p.Datatype of
        dtString, dtNumber:
        begin
          m_objEdit.Top := r.Top + 1;
          m_objEdit.Left := r.Left + 1;
          m_objEdit.Height := r.Bottom - r.Top - 1;
          m_objEdit.Width := r.Right - r.Left - 1;
          m_objEdit.Text := AsString[m_pCell.Y, m_pCell.X];
          m_objEdit.Color := m_colSelection;
          m_objEdit.Visible := TRUE;
          m_objEdit.MaxLength := p.MaxLength;
          m_objEdit.CharCase := p.CharCase;
          m_objEdit.SetFocus;
        end;
        dtDictionary:
        begin
          m_objCombo.Top := r.Top + 1;
          m_objCombo.Left := r.Left + 1;
          m_objCombo.Height := r.Bottom - r.Top - 1;
          m_objCombo.Width := r.Right - r.Left - 1;
          m_objCombo.Color := m_colSelection;
          m_objCombo.CharCase := p.CharCase;
          if p <> nil then
            m_objCombo.Items.Assign(p.FValues);
          m_objCombo.Text := AsString[m_pCell.Y, m_pCell.X];
          m_objCombo.Visible := TRUE;
          m_objCombo.SetFocus;
        end;
        dtBoolean:
        begin
          if bHide then
          begin
            m_objCheckBox.Top := r.Top + 1;
            m_objCheckBox.Left := r.Left + 1;
            m_objCheckBox.Height := r.Bottom - r.Top - 1;
            m_objCheckBox.Width := r.Right - r.Left - 1;
            m_objCheckBox.Color := m_colSelection;
            if p <> nil then
            begin
              m_objCheckBox.StateImages := p.StateImages;
              m_objCheckBox.Checked := AnsiCompareText(AsString[m_pCell.Y, m_pCell.X], p.BooleanState[TRUE]) = 0;
            end;
          end;
          m_objCheckBox.Visible := TRUE;
          {m_objCheckBox.}SetFocus;
        end;
      end;
    FEditState := TRUE;
  end
  else if not bActivate and bRW then
  begin
    if (p <> nil) and (not p.IsReadOnly or LocalIsRW(p, d)) and (m_pCell.Y < Items.Count) then
    begin
      case p.Datatype of
        dtString, dtNumber:
        begin
          m_objEdit.Visible := FALSE;
          AsString[m_pCell.Y, m_pCell.X] := m_objEdit.Text;
        end;
        dtDictionary:
        begin
          m_objCombo.visible := FALSE;
          AsString[m_pCell.Y, m_pCell.X] := m_objCombo.Text;
        end;
        dtBoolean:
        begin
          m_objCheckBox.Hide;
          AsString[m_pCell.Y, m_pCell.X] := p.BooleanState[m_objCheckBox.Checked];
        end;
      end;
      // Set External Value
      if (d <> nil) then
      begin
        f := d.Child(p.MetaData);
        if f <> nil then
        begin
          b := f.sValue <> AsString[m_pCell.Y, m_pCell.X];
          f.sValue := AsString[m_pCell.Y, m_pCell.X];
          if b and Assigned(FOnChangeValue) then
            FOnChangeValue(self);
        end;
      end;
    end;
    FEditState := FALSE;
  end;
  if (p <> nil) and p.IsReadOnly and not bHide then
    SetFocus;
{$ENDIF}
end;

// TGrid
//   DoBeginColumnResize
//
procedure TGrid.DoBeginColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(m_FBeginColumnResizeEvent) then
    m_FBeginColumnResizeEvent(Self, columnindex, columnwidth);
end;

// TGrid
//   DoEndColumnResize
//
procedure TGrid.DoEndColumnResize(columnindex, columnwidth: Integer);
var
  p: TGridColumn;
begin
  onMoveField(FALSE, TRUE);
  // Cascade Event;
  if Assigned(m_FEndColumnResizeEvent) then
    m_FEndColumnResizeEvent(Self, columnindex, columnwidth);
  // Reassign to memory column widths
  p := GetColumn(columnindex);
  if p <> nil then
    p.Width := columnWidth;
  Invalidate;
end;

// TGrid
//   DoColumnResize
//
procedure TGrid.DoColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(m_FColumnResizeEvent) then
    m_FColumnResizeEvent(Self, columnindex, columnwidth);
end;

// TGrid
//   FindColumnIndex
//
function TGrid.FindColumnIndex(pHeader: pNMHdr): Integer;
var
  hwndHeader: HWND;
  iteminfo: THdItem;
  ItemIndex: Integer;
  buf: array [0..128] of Char;
begin
  Result := kiUNDEFINED;
  hwndHeader := pHeader^.hwndFrom;
  ItemIndex := pHDNotify(pHeader)^.Item;
  FillChar(iteminfo, SizeOf(iteminfo), 0);
  iteminfo.Mask := HDI_TEXT;
  iteminfo.pszText := buf;
  iteminfo.cchTextMax := SizeOf(buf) - 1;
  Header_GetItem(hwndHeader, ItemIndex, iteminfo);
  if CompareStr(Columns[ItemIndex].Caption, iteminfo.pszText) = 0 then
    Result := ItemIndex
  else
  begin
    for ItemIndex := 0 to Columns.Count - 1 do
      if CompareStr(Columns[ItemIndex].Caption, iteminfo.pszText) = 0 then
      begin
        Result := ItemIndex;
        Break;
      end;
  end;
end;

// TGrid
//   WMNotify
//
procedure TGrid.WMNotify(var Msg: TWMNotify);
begin
  inherited;
  case Msg.NMHdr^.code of
    HDN_ENDTRACK, HDN_DIVIDERDBLCLICK:
      DoEndColumnResize(FindColumnIndex(Msg.NMHdr), FindColumnWidth(Msg.NMHdr));
    HDN_BEGINTRACK:
      DoBeginColumnResize(FindColumnIndex(Msg.NMHdr), FindColumnWidth(Msg.NMHdr));
    HDN_TRACK:
      DoColumnResize(FindColumnIndex(Msg.NMHdr), FindColumnWidth(Msg.NMHdr));
  end;
end;

// TGrid
//   CreateWnd
//
procedure TGrid.CreateWnd;
var
  wnd: HWND;
begin
  inherited;
  wnd := GetWindow(Handle, GW_CHILD);
  SetWindowLong(wnd, GWL_STYLE,
    GetWindowLong(wnd, GWL_STYLE) and not HDS_FULLDRAG);
end;

// TGrid
//   FindColumnWidth
//
function TGrid.FindColumnWidth(pHeader: pNMHdr): Integer;
begin
  Result := kiUNDEFINED;
  if Assigned(PHDNotify(pHeader)^.pItem) and
    ((PHDNotify(pHeader)^.pItem^.mask and HDI_WIDTH) <> 0) then
    Result := PHDNotify(pHeader)^.pItem^.cxy;
end;

// TGrid
//   CellMessage
//
function TGrid.CellMessage: String;
begin
  result := Format(' Line %d, Column %d', [m_pCell.Y + 1, m_pCell.X + 1]);
end;

// TGrid
//   ListViewSort
//
function ListViewSort(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
var
  D: boolean;
begin
  D := (Item1.ListView as TGrid).m_lstColumns[lParam].Tag = longint(etInteger);
  if lParam > 0 then
  begin
    if (Item1.ListView as TGrid).Columns[lParam].Tag > 0 then
    begin
      if D then
        result := strtointdef(Item1.SubItems[lParam - 1], kiUNDEFINED) - strtointdef(Item2.SubItems[lParam - 1], kiUNDEFINED)
      else
        result := CompareText(Item1.SubItems[lParam - 1], Item2.SubItems[lParam - 1]);
    end
    else
    begin
      if D then
        result := strtointdef(Item2.SubItems[lParam - 1], kiUNDEFINED) - strtointdef(Item1.SubItems[lParam - 1], kiUNDEFINED)
      else
        result := CompareText(Item2.SubItems[lParam - 1], Item1.SubItems[lParam - 1]);
    end;
  end
  else
  begin
    if (Item1.ListView as TGrid).Columns[lParam].Tag > 0 then
    begin
      if D then
        result := strtointdef(Item1.Caption, kiUNDEFINED) - strtointdef(Item2.Caption, kiUNDEFINED)
      else
        result := CompareText(Item1.Caption, Item2.Caption);
    end
    else
    begin
      if D then
        result := strtointdef(Item2.Caption, kiUNDEFINED) - strtointdef(Item1.Caption, kiUNDEFINED)
      else
        result := CompareText(Item2.Caption, Item1.Caption);
    end;
  end
end;

// TGrid
//   onColumnClickEvent
//
procedure TGrid.onColumnClickEvent(Sender: TObject; Column: TListColumn);
begin
  if m_bColumnSort then
  begin
    onMoveField(FALSE, TRUE);
    m_iSelectedColumn := Column.Index;
    CustomSort(@ListViewSort, Column.Index);
    Column.Tag := -Column.Tag;
  end;
end;

// TGrid
//   SetBackgroundColor
//
procedure TGrid.SetBackgroundColor(value: TColor);
begin
  m_colBackgroundColor := value;
  Color := m_colBackgroundColor;
end;

// TGrid
//   SetSelectedColumnColor
//
procedure TGrid.SetSelectedColumnColor(value: TColor);
begin
  m_colSelectedColumnColor := value;
end;

// TGrid
//   SetSelectionColor
//
procedure TGrid.SetSelectionColor(value: TColor);
begin
  m_colSelection := value;
end;

// TGrid
//   SetCurrentLineColor
//
procedure TGrid.SetCurrentLineColor(value: TColor);
begin
  m_colCurrentLineColor := value;
end;

// TGrid
//   ClearColumns
//
procedure TGrid.ClearColumns;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  Columns.BeginUpdate;
  Columns.Clear;
  Columns.EndUpdate;
  m_lstColumns.Clear;
end;

// TGrid
//   CellAt
//
function TGrid.CellAt(Sender: TObject; X, Y: Integer): TPoint;
var
  p: TListItem;
  c: TListColumn;
  L, i, w: longint;
begin
  result := point(kiUNDEFINED, kiUNDEFINED);
  p := GetItemAt(X, Y);
  if p <> nil then
  begin
    L := p.DisplayRect(drBounds).Left;
    for i := 0 to Columns.Count - 1 do
    begin
      c := Columns[i];
      w := c.Width;
      if c.AutoSize then
        w := abs(w);
      if (X >= L) and (X < L + w) then
      begin
        result.X := i;
        result.Y := p.Index;
        break;
      end;
      L := L + c.Width;
    end;
  end;
end;

// TGrid
//   onContextPopupEvent
//
procedure TGrid.onContextPopupEvent(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  OnMouseDownClick(Sender, mbLeft, [], MousePos.X, MousePos.Y);
  // still popup context popup menu.
  Handled := False;
end;

// TGrid
//   GetColumnCount
//
function TGrid.GetColumnCount: longint;
begin
  result := m_lstColumns.Count;
end;

// TGrid
//   GetColumn
//
function TGrid.GetColumn(Index: longint): TGridColumn;
var
  c: TGridColumn;
begin
  result := nil;
  while Index > m_lstColumns.Count - 1 do
  begin
    c := TGridColumn.Create(m_lstColumns);
    c.FGrid := self;
    m_lstColumns.Add(c);
  end;
  if (Index >= 0) and (Index < m_lstColumns.Count) then
    result := m_lstColumns[Index] as TGridColumn;
end;

// TGrid
//   SetColumn
//
procedure TGrid.SetColumn(Index: longint);
var
  p: TListColumn;
  c: TGridColumn;
  i: longint;
begin
  try
    Columns.BeginUpdate;
    // adjusr grid columns to Index
    while (Index > Columns.Count - 1) or (m_lstColumns.count > columns.count) do
      Columns.Add;
    // reset all column widths
    for i := 0 to m_lstColumns.count - 1 do
    begin
      c := GetColumn(i);
      p := Columns[i];
      if (c <> nil) and (p <> nil) then
      begin
        p.Caption := c.sName;
        p.Width := c.Width;
        p.AutoSize := i = m_lstColumns.Count - 1;
        if p.AutoSize and (i > 0) then
        begin
          p := Columns[i - 1];
          if (p <> nil) and p.AutoSize then
            p.AutoSize := FALSE;
        end;
      end;
    end;
  finally
    Columns.EndUpdate;
  end;
end;

// TGrid
//   SetCell
//
procedure TGrid.SetCell(iRow, iColumn: longint; value: String);
var
  p: TListItem;
begin
  if (iRow > kiUNDEFINED) and (iColumn > kiUNDEFINED) then
  begin
    while iRow > Items.Count - 1 do
      Items.Add;
    p := Items[iRow];
    if iColumn = 0 then
      p.Caption := value
    else
    begin
      while p.SubItems.Count - 1 < iColumn - 1 do
        p.SubItems.Add(ksEMPTY);
      p.SubItems[iColumn - 1] := value;
    end;
  end;
end;

// TGrid
//   GetCell
//
function TGrid.GetCell(iRow, iColumn: longint): String;
var
  p: TListItem;
begin
  result := ksEMPTY;
  if (iRow > kiUNDEFINED) and (iColumn > kiUNDEFINED) then
  begin
    while iRow > Items.Count - 1 do
      Items.Add;
    p := Items[iRow];
    if iColumn = 0 then
      result := p.Caption
    else
    begin
      while p.SubItems.Count - 1 < iColumn - 1 do
        p.SubItems.Add(ksEMPTY);
      result := p.SubItems[iColumn - 1];
    end;
  end;
end;

// TGrid
//   GetAsString
//
function TGrid.GetAsString(iRow, iColumn: longint): String;
begin
  result := GetCell(iRow, iColumn);
end;

// TGrid
//   SetAsString
//
procedure TGrid.SetAsString(iRow, iColumn: longint; value: String);
begin
  SetCell(iRow, iColumn, value);
end;

// TGrid
//   GetAsLongint
//
function TGrid.GetAsLongint(iRow, iColumn: longint): longint;
begin
  result := strtointdef(GetCell(iRow, iColumn), 0);
end;

// TGrid
//   SetAsLongint
//
procedure TGrid.SetAsLongint(iRow, iColumn: longint; value: longint);
begin
  SetCell(iRow, iColumn, inttostr(value));
end;

// TGrid
//   GetAsBoolean
//
function TGrid.GetAsBoolean(iRow, iColumn: longint): boolean;
begin
  result := FALSE;
end;

// TGrid
//   SetAsBoolean
//
procedure TGrid.SetAsBoolean(iRow, iColumn: longint; value: boolean);
begin
  //
end;

// TGrid
//   SetColumnValues
//
procedure TGrid.SetColumnValues(Index: longint; value: TStrings);
var
  p: TGridColumn;
begin
  p := GetColumn(Index);
  if p <> nil then
    p.Assign(value);
end;

// TGrid
//   SetImages
//
procedure TGrid.SetImages(value: TCustomImageList);
begin
  FImages := value;
  m_objCheckBox.Images := value
end;

// TGrid
//   GetData
//
function TGrid.GetData(Index: longint): TcObject;
begin
  result := nil;
  if (Index >= 0) and (Index < Items.Count) then
    result := TcObject(Items[Index].Data);
end;

// TGrid
//   SetData
//
procedure TGrid.SetData(Index: longint; value: TcObject);
begin
  if (Index >= 0) and (Index < Items.Count) then
    Items[Index].Data := value;
end;

// TGrid
//   SetData
//
procedure TGrid.DoEnter;
begin
  onMoveField(TRUE, FALSE);
  inherited DoEnter;
end;

// TGrid
//   SetData
//
procedure TGrid.DoExit;
begin
  onMoveField(FALSE, TRUE);
  inherited DoExit;
end;

// TGrid
//   OnGridKeyPress
//
procedure TGrid.OnGridKeyPress(Sender: TObject; var Key: Char);
var
  p: TGridColumn;
begin
  p := GetColumn(m_pCell.X);
  if (p <> nil) and (p.Filter <> ksEMPTY) and (Key > #31) and not AnsiContainsText(p.Filter, key) then
    Key := #0;
end;

// TGrid
//   CheckRequiredFields
//
function TGrid.CheckRequiredFields(Index: longint; bMessage: boolean): boolean;
var
  i: longint;
  s: String;
begin
  result := TRUE;
  if (Index <> kiUNDEFINED) then
    for i := 0 to ColumnCount - 1 do
      if GetColumn(i).Required then
      begin
        result := result and (GetAsString(Index, i) <> ksEMPTY);
        if s <> ksEMPTY then
          s := s + ', ';
        s := s + Format('''%s''', [GetColumn(i).sName]);
      end;
  if not result and bMessage then
    Application.MessageBox(PChar(Format('Values for the following columns are required: %s', [s])), 'Error', MB_OK + MB_ICONEXCLAMATION);
end;

// TGrid
//   SetItemIndex method
//
procedure TGrid.SetItemIndex(const Value: Integer);
begin
  onMoveField(FALSE, FALSE);
  inherited SetItemIndex(value);
  m_pCell := point(0, value);
  onMoveField(TRUE, FALSE);
end;

// TGrid
//   DeleteSelected method
//
procedure TGrid.DeleteSelected;
begin
  onMoveField(FALSE, FALSE);
  inherited DeleteSelected;
end;

// TGrid
//   GetUsableRect method
//
function TGrid.GetUsableRect: TRect;
begin
  result.left := 0;
  result.Top := TopItem.Top;
  result.Bottom := result.Top + VisibleRowCount * FItemHeight;
  result.Right := width;
end;

// TGrid
//   WMVScroll method
//
procedure TGrid.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  UpdateFieldPosition;
end;

// TGrid
//   WMHScroll method
//
procedure TGrid.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  UpdateFieldPosition;
end;

// TGrid
//   UpdateFieldPosition method
//
procedure TGrid.UpdateFieldPosition;
var
  r: TRect;
begin
  inherited;
  r := GetRect;
  if m_objEdit.Visible then
  begin
    m_objEdit.Left := r.Left;
    m_objEdit.Top := r.Top;
  end
  else if m_objCombo.Visible then
  begin
    m_objCombo.Left := r.Left;
    m_objCombo.Top := r.Top;
  end
  else if m_objCheckBox.Visible then
  begin
    m_objCheckBox.Left := r.Left;
    m_objCheckBox.Top := r.Top;
  end
end;

// TGrid
//   GetCheckAllRequired
//
function TGrid.GetCheckAllRequired: boolean;
var
  i: longint;
begin
  result := TRUE;
  for i := 0 to Items.Count - 1 do
  begin
    result := result and CheckRequiredFields(i, FALSE);
    if not result then
      break;
  end;
end;

//
// TGridColumn
//

// TGridColumn
//   Create
//
constructor TGridColumn.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  FValues := TStringList.Create;
  FGrid := nil;
  FIsReadOnly := FALSE;
  FMetaData := nil;
  FMaxLength := 0;
  FCharCase := ecNormal;
  SetBooleanValues('Yes,No');
  Clear;
end;

// TGridColumn
//   Destroy
//
destructor TGridColumn.Destroy;
begin
  FValues.free;
  inherited Destroy;
end;

// TGridColumn
//   Clear
//
procedure TGridColumn.Clear;
begin
  inherited Clear;
  FWidth := 100;
  FeDatatype := dtString;
  FValues.Clear;
  FStateImages[TRUE] := kiUNDEFINED;
  FStateImages[FALSE] := kiUNDEFINED;
  FFilter := ksEMPTY;
  FRequired := FALSE;
end;

// TGridColumn
//   Assign
//
procedure TGridColumn.Assign(value: TStrings);
begin
  FValues.Assign(value);
end;

// TGridColumn
//   SetWidth
//
procedure TGridColumn.SetWidth(value: longint);
begin
  if value > 0 then
    FWidth := value;
  if FGrid <> nil then
    FGrid.SetColumn(ParentIndex);
end;

// TGridColumn
//   SetCaption
//
procedure TGridColumn.SetCaption(value: string);
begin
  sName := value;
  if FGrid <> nil then
    FGrid.SetColumn(ParentIndex);
end;

// TGridColumn
//   SetStateImages
//
procedure TGridColumn.SetStateImages(value: TGridBooleanArray);
begin
  FStateImages := value;
  if FGrid <> nil then
    FGrid.SetColumn(ParentIndex);
end;

// TGridColumn
//   SetStateImage
//
procedure TGridColumn.SetStateImage(index: boolean; value: longint);
begin
  FStateImages[Index] := value;
  if FGrid <> nil then
    FGrid.SetColumn(ParentIndex);
end;

// TGridColumn
//   GetStateImage
//
function TGridColumn.GetStateImage(index: boolean): longint;
begin
  result := FStateImages[Index];
end;

// TGridColumn
//   GetBooleanValues
//
function TGridColumn.GetBooleanValues: String;
begin
  result := Format('%s,%s', [FTRUE, FFALSE]);
end;

// TGridColumn
//   GetBooleanState
//
procedure TGridColumn.SetBooleanValues(value: String);
begin
  if value <> ksEMPTY then
  begin
    FTRUE := Item(value, ',', 0);
    FFALSE := Item(value, ',', 1);
    FabBOOLEAN[TRUE] := FTRUE;
    FabBOOLEAN[FALSE] := FFALSE;
  end;
end;

// TGridColumn
//   GetBooleanState
//
function TGridColumn.GetBooleanState(value: boolean): String;
begin
  result := FabBOOLEAN[value];
end;

//
// TGridComboBox
//

// TGridComboBox
//   Create
//
constructor TGridComboBox.Create(AOwner: TComponent);
const
  kiWIDTH = 147;
  kiHEIGHT = 16;
  kiBUTTONWIDTH = 11;
begin
  inherited Create(AOwner);
  Width := kiWIDTH;
  Height := kiHEIGHT;
  BevelOuter := bvNone;
  //
  // Edit field
  FEdit := TEdit.Create(self);
  FEdit.Parent := self;
  FEdit.Left := 0;
  FEdit.Top := 0;
  FEdit.Width := kiWIDTH - (kiBUTTONWIDTH + 1);
  FEdit.Height := kiHEIGHT;
  FEdit.Anchors := [akLeft, akTop, akRight, akBottom];
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  //
  // Button
  FButton := TGridComboBoxButton.Create(self);
  FButton.Parent := self;
  FButton.Left := kiWIDTH - (kiBUTTONWIDTH + 1);
  FButton.Top := 0;
  FButton.Width := kiBUTTONWIDTH;
  FButton.Height := kiHEIGHT;
  FButton.Align := alRight;
  FButton.OnClick := DropDown;
  //
  // Listbox
  FListBox := TListBox.Create(self);
  FListBox.Parent := self;
  FListBox.BevelInner := bvNone;
  FListBox.BevelOuter := bvNone;
  FListBox.Ctl3D := FALSE;
  FListBox.ParentCtl3D := FALSE;
  FListBox.Visible := FALSE;
  FListBox.OnMouseMove := ListBoxMouseMove;
  FListBox.OnMouseUp := ListBoxMouseUp;
  FListBox.OnExit := DropDown;
  FListBox.OnKeyUp := ListBoxKeyUp;
end;

// TGridComboBox
//   GetDroppedDown
//
function TGridComboBox.GetDroppedDown: boolean;
begin
  result := FListbox.Visible;
end;

// TGridComboBox
//   SetColor
//
procedure TGridComboBox.SetColor(value: TColor);
begin
  FEdit.Color := value;
end;

// TGridComboBox
//   SetFocus
//
procedure TGridComboBox.SetFocus;
begin
  FEdit.SetFocus;
end;

// TGridComboBox
//   GetStyle
//
function TGridComboBox.GetText: String;
begin
  result := FEdit.Text;
end;

// TGridComboBox
//   SetFocus
//
procedure TGridComboBox.SetText(value: String);
begin
  FEdit.Text := value;
end;

// TGridComboBox
//   EditKeyDown
//
procedure TGridComboBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssAlt in Shift) and (Key = 40) then // Down
    DropDown(Sender)
  else if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

// TGridComboBox
//   DropDown
//
procedure TGridComboBox.DropDown(Sender: TObject);
var
  T1, H1, T2, H2: longint;
  r: TRect;
begin
  if not GetDroppedDown and (parent <> nil) and (FListBox.Items.Count > 0) then
  begin
    FListBox.Parent := Parent;
    FListBox.Left := Left;
    FListBox.Width := Width;
    // Standing below ?
    if Parent is TGrid then
      r := (parent as TGrid).UsableRect
    else
      r := Clientrect;
    T1 := Top + Height;
    H1 := FListBox.ItemHeight * min(8, FListBox.Items.Count);
    if H1 + T1 > r.Bottom then
      H1 := r.Bottom - T1;
    // Standing above ?
    H2 := FListBox.ItemHeight * min(8, FListBox.Items.Count);
    T2 := Top - H2;
    if T2 < r.Top then
    begin
      H2 := H2 + T2 - r.Top;
      T2 := R.Top;
    end;
    // Chose where it will stand.
    if H1 > H2 then
    begin
      FListBox.Height := H1;
      FListBox.Top := T1;
    end
    else
    begin
      FListBox.Height := H2;
      FListBox.Top := T2;
    end;
    //
    with FListBox do
      ItemIndex := Items.IndexOf(GetText);
    FListBox.Color := FEdit.Color;
    FListBox.Show;
    FListBox.SetFocus;
  end
  else
  begin
    FListBox.Hide;
    FEdit.SetFocus;
  end;
end;

// TGridComboBox
//   GetItems
//
function TGridComboBox.GetItems: TStrings;
begin
  result := FListBox.Items;
end;

// TGridComboBox
//   SetItems
//
procedure TGridComboBox.SetItems(value: TStrings);
begin
  FListBox.Items.Assign(value);
end;

// TGridComboBox
//   ListBoxMouseMove
//
procedure TGridComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  with FListBox do
    ItemIndex := ItemAtPos(Point(X, Y), FALSE);
end;

// TGridComboBox
//   ListBoxMouseUp
//
procedure TGridComboBox.ListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with FListBox do
    if (ItemIndex > kiUNDEFINED) and (ItemIndex < Items.Count) then
    begin
      SetText(Items[ItemIndex]);
      DropDown(Sender);
    end;
end;

// TGridComboBox
//   ListBoxKeyUp
//
procedure TGridComboBox.ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with FListBox do
    if (Key = 13) and (ItemIndex > kiUNDEFINED) and (ItemIndex < Items.Count) then // 27=Esc
      SetText(Items[ItemIndex]);
  if (Key = 13) or (Key = 27) then // 13=Enter, 27=Esc
    DropDown(Sender);
end;

// TGridComboBox
//   GetItems
//
function TGridComboBox.GetCharCase: TEditCharCase;
begin
  result := FEdit.CharCase;
end;

// TGridComboBox
//   ListBoxKeyUp
//
procedure TGridComboBox.SetCharCase(value: TEditCharCase);
begin
  FEdit.CharCase := value;
end;

//
// TGridComboBoxButton
//

// TGridComboBoxButton
//   Paint
//
procedure TGridComboBoxButton.Paint;
var
  H, W: longint;
begin
  inherited Paint;
  H := Height div 2;
  W := width div 2;
  Canvas.Pen.Color := clBlack;
  Canvas.Polyline([Point(W - 2, H - 1), Point(W + 3, H - 1)]);
  Canvas.Polyline([Point(W - 1, H    ), Point(W + 2, H    )]);
  Canvas.Polyline([Point(W    , H + 1), Point(W + 1, H + 1)]);
end;

//
// TGridCheckBox
//

// TGridCheckBox
//   Create
//
constructor TGridCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  SetChecked(FALSE);
  OnMouseDown := OnMouseDownClick;
  OnKeyDown := OnCheckBoxKeyDown;
  FImages := nil;
  FStateImages[FALSE] := kiUNDEFINED;
  FStateImages[TRUE] := kiUNDEFINED;
end;

// TGridCheckBox
//   SetChecked
//
procedure TGridCheckBox.SetChecked(value: boolean);
begin
  FChecked := value;
  Invalidate;
end;

// TGridCheckBox
//   Paint
//
procedure TGridCheckBox.Paint;
var
  FBitmap: TBitmap;
  r: TRect;
begin
  FBitmap := nil;
  if parent <> nil then
  try
    FBitmap := TBitmap.Create;
    if FImages <> nil then
      FImages.GetBitmap(FStateImages[FChecked], FBitmap);
    FBitmap.Transparent := TRUE;
    Canvas.Brush.Color := Color;
    r := Rect((Width - FBitmap.Width) div 2, (Height - FBitmap.Height) div 2, (Width + FBitmap.Width) div 2, (Height + FBitmap.Height) div 2);
    Canvas.FillRect(r);
    Canvas.Draw((Width - FBitmap.Width) div 2, (Height - FBitmap.Height) div 2, FBitmap);
  finally
    FBitmap.free;
  end;
end;

// TGridCheckBox
//   OnMouseDownClick
//
procedure TGridCheckBox.OnMouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (parent <> nil) and (parent is TGrid) and Assigned((parent as TGrid).OnMouseDown) then
    (parent as TGrid).OnMouseDown(parent, Button, Shift, X + Left, Y + Top);
end;

// TGridCheckBox
//   OnCheckBoxKeyDown
//
procedure TGridCheckBox.OnCheckBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (parent <> nil) and (Parent is TGrid) and assigned((Parent as TGrid).OnKeyDown) then
    (Parent as TGrid).OnKeyDown(Sender, Key, Shift);
end;

// Tool
//   StringToGridDatatype
//
function StringToGridDatatype(value: String): TeGridDatatype;
var
  e: TeGridDatatype;
const
  kasDATATYPE: array[TeGridDatatype] of string =
    (ksEMPTY, 'String', 'longstring', 'Number', 'boolean', 'dictionary');
begin
  result := low(TeGridDatatype);
  for e := succ(low(TeGridDatatype)) to high(TeGridDatatype) do
    if AnsiCompareText(value, kasDATATYPE[e]) = 0 then
    begin
      result := e;
      break;
    end;
end;

// Tool
//   StringToGridState
//
function StringToGridState(value: String): TeGridState;
var
  e: TeGridState;
const
  kasGRIDSTATE: array[TeGridState] of string =
    (krsFALSE, krsPREVIOUS, krsTRUE);
begin
  result := low(TeGridState);
  for e := succ(low(TeGridState)) to high(TeGridState) do
    if AnsiCompareText(value, kasGRIDSTATE[e]) = 0 then
    begin
      result := e;
      break;
    end;
end;

end.





