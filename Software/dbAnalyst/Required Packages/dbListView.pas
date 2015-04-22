unit dbListView;

interface

uses
  Windows,
  daObjectLib,
  daGlobals,
  ADODB_TLB,
  Classes,
  ComCtrls,
  Messages,
  Controls,
  Types,
  CommCtrl,
{$IFNDEF PACKAGE}
  ExecuteLib,
  ConnectionLib,
{$ENDIF}
  Graphics;

const
  kiINITIALROWS         = 200;
  LVM_FIRST             = $1000;      { ListView messages }
  LVM_GETTOPINDEX       = LVM_FIRST + 39;
  LVM_REDRAWITEMS       = LVM_FIRST + 21;
  LVM_GETCOUNTPERPAGE   = LVM_FIRST + 40;
  ksCR                  = #$D#$A;
  ksEMPTY               = '';
  kiUNDEFINED           = -1;
  krsNULL               = '(null)';
  krsINFORMATION        = 'Information';
  krsOPENSCHEMA         = 'OpenSchema';
  krsSTATUS_COLLINE     = 'Line %d, Col %d';
  krsNULLFIELD          = 'Ø';

type
  TDBListViewnResizeEvent = procedure(Sender: TCustomListview; columnindex: Integer; columnwidth: Integer) of object;
  TDBListViewMessage = procedure(Sender: TObject; value: String) of object;
  TeColumnOrigin = (ecoADO, ecoUser);
  TeFeatureSupport = (efsEdit);

  TcDBListViewColumn = class(TcObject)
  private
    // private members
    //
    m_eOrigin: TeColumnOrigin;
    m_iWidth: longint;
    m_eDatatype: TeDatatype;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom

  public
    // Public properties
    //
    property    eOrigin: TeColumnOrigin                         read m_eOrigin                  write m_eOrigin;
    property    iWidth: longint                                 read m_iWidth                   write m_iWidth;
    property    eDatatype: TeDatatype                           read m_eDatatype                write m_eDatatype;
  end;

  TDBListView = class(TCustomListView)
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
    m_lstColumns: TcCollection;
    m_pCell: TPoint;
    m_FBeginColumnResizeEvent: TDBListViewnResizeEvent;
    m_FEndColumnResizeEvent: TDBListViewnResizeEvent;
    m_FColumnResizeEvent: TDBListViewnResizeEvent;
    m_FDisplayMessage: TDBListViewMessage;
    m_rectDetail: TRect;
    m_iSelectedColumn: longint;
    m_colBackgroundColor: TColor;
    m_colSelectedColumnColor: TColor;
    m_colSelection: TColor;
    m_colCurrentLineColor: TColor;
    m_colNullCell: TColor;
    m_sSQL: String;
    m_bIsAsynchronous: boolean;
    m_bStop: boolean;
    m_bAcceptDblClick: boolean;
    m_esOptions: TDBListViewOptions;
    m_bIsReadOnly: boolean;
    m_lstErrors: TStringList;
    m_FDisplayErrors: TDBListViewMessage;
{$IFNDEF PACKAGE}
    m_objConnection: TcConnection;
    m_objRS: TcRecordSet;
    m_bExecuteComplete: boolean;
{$ENDIF}

  private
    // private methods
    //
    procedure   SetSQL(value: String);
    function    GetNextBlock: boolean;
    procedure   WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure   dbCustomDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    procedure   MouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   ListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   CheckForNextBlock;
    procedure   ListDoubleClick(Sender: TObject);
    function    CellMessage: String;
{$IFNDEF PACKAGE}
    procedure   SetConnection(value: TcConnection);
{$ENDIF}
    procedure   onColumnClickEvent(Sender: TObject; Column: TListColumn);
    function    GetCellText: String;
    procedure   SetBackgroundColor(value: TColor);
    procedure   SetSelectedColumnColor(value: TColor);
    procedure   SetSelectionColor(value: TColor);
    procedure   SetCurrentLineColor(value: TColor);
    procedure   SetNullCellColor(value: TColor);
    procedure   SetIsReadOnly(value: boolean);
    function    GetIsReadOnly: boolean;
    procedure   SetColumnWidth(Index, value: longint);
    function    GetColumnWidth(Index: longint): longint;
    procedure   SetColumnCaption(Index: longint; value: string);
    function    GetColumnCaption(Index: longint): string;
    procedure   onContextPopupEvent(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    function    GetTextAtCell: String;
    procedure   SetTextAtCell(value: String);
    // procedure   onExecuteComplete(Sender: TObject; RecordsAffected: Integer; var pError: OleVariant; var adStatus: OleVariant; var pCommand: OleVariant; var pRecordset: OleVariant; var pConnection: OleVariant);
    function    GetColumn(Index: longint): TcDBListViewColumn;
    function    GetSupport(Index: TeFeatureSupport): boolean;
    function    GetCanClipboard: boolean;
    procedure   SetError(value: String);
    function    GetErrors: String;

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

  public
    // Public methods
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; override;                                                // Clear base method
    //
    // 2. Custom
    procedure   Close;
    procedure   Refresh;
    function    CellAt(Sender: TObject; X, Y: Integer): TPoint;
    procedure   onStop(Sender: TObject);
    procedure   onDeActivate(Sender: TObject);
    procedure   ClearColumns;
    procedure   SendDisplayMessage(sender: TObject);

  public
    // Public properties
    //
    property    SQL: String                                     read m_sSQL                     write SetSQL;
{$IFNDEF PACKAGE}
    property    Connection: TcConnection                        read m_objConnection            write SetConnection;
{$ENDIF}
    property    ColumnWidth[Index: longint]: longint            read GetColumnWidth             write SetColumnWidth;
    property    ColumnCaption[Index: longint]: String           read GetColumnCaption           write SetColumnCaption;
    property    Stopped: boolean                                read m_bStop;
    property    Support[Index: TeFeatureSupport]: boolean       read GetSupport;

  published
    // Published properties
    //
    property    OnBeginColumnResize: TDBListViewnResizeEvent    read m_FBeginColumnResizeEvent  write m_FBeginColumnResizeEvent;
    property    OnEndColumnResize: TDBListViewnResizeEvent      read m_FEndColumnResizeEvent    write m_FEndColumnResizeEvent;
    property    OnColumnResize: TDBListViewnResizeEvent         read m_FColumnResizeEvent       write m_FColumnResizeEvent;
    property    OnDisplayMessage: TDBListViewMessage            read m_FDisplayMessage          write m_FDisplayMessage;
    property    CellText: String                                read GetCellText;
    property    BackgroundColor: TColor                         read m_colBackgroundColor       write SetBackgroundColor;
    property    SelectedColumnColor: TColor                     read m_colSelectedColumnColor   write SetSelectedColumnColor;
    property    SelectionColor: TColor                          read m_colSelection             write SetSelectionColor;
    property    CurrentLineColor: TColor                        read m_colCurrentLineColor      write SetCurrentLineColor;
    property    NullCellColor: TColor                           read m_colNullCell              write SetNullCellColor;
    property    IsReadOnly: boolean                             read GetIsReadOnly              write SetIsReadOnly;
    property    Coordinates: TPoint                             read m_pCell;
    property    TextAtCell: String                              read GetTextAtCell              write SetTextAtCell;
    property    IsAsynchronous: boolean                         read m_bIsAsynchronous          write m_bIsAsynchronous;
    property    CanClipboard: boolean                           read GetCanClipboard;
    property    Errors: String                                  read GetErrors;
    property    OnDisplayErrors: TDBListViewMessage             read m_FDisplayErrors           write m_FDisplayErrors;
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
    property    Columns;
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
    property    LargeImages;
    property    MultiSelect;
    property    OwnerData;
    property    OwnerDraw;
    property    ReadOnly default False;
    property    RowSelect;
    property    ParentBiDiMode;
    property    ParentColor default False;
    property    ParentFont;
    property    ParentShowHint;
    property    PopupMenu;
    property    ShowColumnHeaders;
    property    ShowWorkAreas;
    property    ShowHint;
    property    SmallImages;
    property    SortType;
    property    StateImages;
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

procedure Register;

implementation

uses
  ComObj,
  sysUtils,
  StrUtils,
  Variants,
  Math,
  Forms,
  dbListViewEdit,
  daStreamLib;

procedure Register;
begin
  RegisterComponents('Seabird', [TDBListView]);
end;  {Register}

function GetTypeLength(value: DataTypeEnum): longint; forward;

//
// TDBListView
//

// TDBListView
//   Create
//
constructor TDBListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Members
  m_lstColumns := TcCollection.Create(nil);
{$IFNDEF PACKAGE}
  m_objConnection := nil;
  m_objRS := TcRecordSet.Create(nil);
{$ENDIF}
  m_bIsReadOnly := TRUE;
  // Visual Properties
  BevelOuter := bvNone;
  HideSelection := False;
  ReadOnly := True;
  RowSelect := True;
  ViewStyle := vsReport;
  ShowHint := FALSE;
  OwnerDraw := TRUE;
  OnDrawItem := dbCustomDrawItem;
  OnColumnClick := onColumnClickEvent;
  OnMouseDown := MouseDownClick;
  m_pCell := point(kiUNDEFINED, kiUNDEFINED);
  onKeyDown := ListViewKeyDown;
  OnDblClick := ListDoubleClick;
  m_FBeginColumnResizeEvent := nil;
  m_FEndColumnResizeEvent := nil;
  m_FColumnResizeEvent := nil;
  m_FDisplayMessage := nil;
  m_rectDetail := rect(0, 0, 400, 300);
  m_colBackgroundColor := clWindow;
  m_colSelectedColumnColor := clCaptionText;
  m_colCurrentLineColor := clGray;
  m_colSelection := $C0C0C0;
  m_colNullCell := clCream;
  AllocBy := kiINITIALROWS;
  OnContextPopup := onContextPopupEvent;
  m_sSQL := ksEMPTY;
  m_bStop := FALSE;
  m_bAcceptDblClick := FALSE;
  m_esOptions := [edboWrap];
  m_lstErrors := TStringList.Create;
  m_FDisplayErrors := nil;
end;

// TDBListView
//   Destroy
//
destructor TDBListView.Destroy;
begin
  m_lstColumns.Free;
{$IFNDEF PACKAGE}
  m_objRS.Free;
  m_objConnection := nil;
{$ENDIF}
  m_lstErrors.Free;
  inherited Destroy;
end;

// TDBListView
//   Clear
//
procedure TDBListView.Clear;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  Columns.BeginUpdate;
  Columns.Clear;
  Columns.EndUpdate;
  SetScrollPos(Handle, SB_HORZ, 0, TRUE);
  Close;
end;

// TDBListView
//   Close
//
procedure TDBListView.Close;
begin
{$IFNDEF PACKAGE}
  if m_objRS.IsOpen then
    m_objRS.Close;
{$ENDIF}
  m_lstErrors.Clear;
end;

// TDBListView
//   SetSQL
//
procedure TDBListView.SetSQL(value: String);
{$IFNDEF PACKAGE}
var
  i, L: longint;
  c: TListColumn;
  s: String;
  p: TcDBListViewColumn;
  strm: TcTokenStream;
  ex: TcExecute;
  rs: TcRecordSet;
  b: boolean;
{$ENDIF}
const
  kiMAXSIZE = 40;
begin
{$IFNDEF PACKAGE}
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  // Clean up first
  Close;
  m_lstErrors.Clear;
  if Assigned(m_FDisplayErrors) then
    m_FDisplayErrors(self, ksEMPTY);
  // Create Record Set
  if m_objConnection <> nil then
  try
    m_iSelectedColumn := kiUNDEFINED;
    m_pCell := point(kiUNDEFINED, kiUNDEFINED);
    m_bExecuteComplete := FALSE;
    m_bStop := FALSE;

    ex := nil;
    try
      rs := nil;
      ex := TcExecute.Create(nil);
      ex.Connection := m_objConnection;
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
          rs := ex.OpenSchema(L, ksEMPTY, 'Grid Display');
      end
      else
      begin
        //
        // Standard Command
        ex.IsReadOnly := m_bIsReadOnly;
        rs := ex.Execute(value);
        //
        // Errors?
        s := ex.Error;
        if s <> ksEMPTY then
          Application.MessageBox(PChar(s), 'Error', MB_OK + MB_ICONEXCLAMATION);
      end;
    finally
      ex.free;
    end;

    if (rs <> nil) and rs.IsOpen then
    begin
      if (m_objRS <> nil) then
      begin
        if m_objRS.IsOpen then
          m_objRS.Close;
        m_objRS.Free;
      end;
      m_objRS := rs;
    end;

    if m_objRS.IsOpen then
    begin
      // Check if we should rebuild the columns
      b := Columns.Count = m_objRS.FieldCount;
      if b then
        for i := 0 to m_objRS.FieldCount - 1 do
        begin
          b := GetColumn(i).sName = m_objRS.FieldName(i);
          if not b then
            break;
        end;
      // Rebuild columns if necessary
      if not b then
      try
        Items.BeginUpdate;
        Items.Clear;
        Columns.BeginUpdate;
        Columns.Clear;
        try
          for i := 0 to m_objRS.FieldCount - 1 do
          begin
            p := GetColumn(i);
            if (p.eOrigin = ecoADO) or (p.sName = ksEMPTY) then
            begin
              p.sName := m_objRS.FieldName(i);
              p.eDatatype := m_objRS.FieldType(i);
            end;
            if p.iWidth = kiUNDEFINED then
              p.iWidth := max(min(m_objRS.FieldSize(i), 50) * Canvas.TextWidth('H'), Canvas.TextWidth(p.sName));
            c := nil;
            if Columns.Count > i then
              c := Columns[i]
            else while i > Columns.Count - 1 do
              c := Columns.Add;
            if c <> nil then
            begin
              c.Caption := p.sName;
              c.Width := p.iWidth;
              c.AutoSize := i = m_objRS.FieldCount - 1;
              c.Tag := 1;
            end;
          end;
        except
          on E:Exception do
            SetError(E.Message);
        end;
      finally
        Columns.EndUpdate;
        Items.EndUpdate;
      end;
      GetNextBlock;
    end;
  except
    on E: Exception do
      application.MessageBox(PChar('An error occured while executing the SQL Statement:' + ksCR + m_objRS.Errors), 'SQL Error', MB_OK + MB_ICONEXCLAMATION);
  end;
{$ENDIF}
  m_sSQL := value;
end;

(*
// TDBListView
//   onExecuteComplete
//
procedure TDBListView.onExecuteComplete(Sender: TObject; RecordsAffected: Integer; var pError: OleVariant; var adStatus: OleVariant; var pCommand: OleVariant; var pRecordset: OleVariant; var pConnection: OleVariant);
begin
  m_bExecuteComplete := TRUE;
end;
*)

// TDBListView
//   GetNextBlock
//
function TDBListView.GetNextBlock: boolean;
{$IFNDEF PACKAGE}
var
  i, j: longint;
  r: TListItem;
  s, sErr: String;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  result := m_objRS.IsOpen and not m_objRS.EOF;
  if result then
  try
    screen.cursor := crAppStart;
    Items.BeginUpdate;
    sErr := GetErrors;
    if Items.Count <> 0 then
      m_objRS.Move(Items.Count, adBookmarkFirst);
    if not m_objRS.EOF then
    begin
      j := 0;
      while (j < kiINITIALROWS) and not m_objRS.EOF do
      begin
        r := Items.Add;
        for i := 0 to m_objRS.FieldCount - 1 do
        begin
          try
            if m_objRS.FieldIsNull(i) then
               s := krsNULLFIELD
            else
              s := m_objRS.AsString[i];
          except
            on E:Exception do
              SetError(Format('Line %d, Col %d: %s', [r.Index + 1, i, E.Message]));
          end;
          if i = 0 then
            r.Caption := s
          else
            r.SubItems.Add(s);
        end;
        m_objRS.MoveNext;
        inc(j);
      end;
    end;
    // Error Handling
    if (GetErrors <> sErr) and Assigned(m_FDisplayErrors) then
      m_FDisplayErrors(self, GetErrors);
  finally
    Items.EndUpdate;
    screen.cursor := crDefault;
  end;
{$ELSE}
  result := FALSE;
{$ENDIF}
end;

// TDBListView
//   Refresh
//
procedure TDBListView.Refresh;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
{$IFNDEF PACKAGE}
  if m_objRS.IsOpen then
    m_objRS.ReQuery;
  GetNextBlock;
{$ENDIF}
end;

// TDBListView
//   WMVScroll
//
procedure TDBListView.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  CheckForNextBlock;
end;

// TDBListView
//   CheckForNextBlock
//
procedure TDBListView.CheckForNextBlock;
var
  L: longint;
begin
  L := SendMessage(Handle, LVM_GETTOPINDEX, 0, 0);
  if Items.Count - L < kiINITIALROWS then
    GetNextBlock;
end;

// TDBListView
//   dbCustomDrawItem
//
procedure TDBListView.dbCustomDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
(*
  List views receive several other custom draw events, including OnCustomDraw, OnCustomDrawItem,
  OnCustomDrawSubItem, OnAdvancedCustomDraw, OnAdvancedCustomDrawItem, and OnAdvancedCustomDrawSubItem.
  These other events, unlike OnDrawItem, occur regardless of the value of the OwnerDraw property.
  They provide slightly different information about the state of the item to be drawn, and the OnAdvancedXXX
  events occur at more points during the paint process.
*)
const
  kcolGrid         = $C0C0C0;
  kcolGrid2        = $B0B0B0;
  kcolSelectedRect = clBlack;
var
  i, j, L, H: longint;
  p: TListColumn;
  s: String;
  r: TRect;
  b, err: boolean;
  // c: TcDBListViewColumn;
begin
  if (Sender <> nil) and (Sender is TDBListView) then
  begin
    // Background Color
    b := m_pCell.Y = Item.Index;
    H := max(((Rect.Bottom - Rect.Top) - abs(Sender.Canvas.Font.Height)) div 2, 0);
    Sender.Canvas.Font.Color := clDefault;
    L := 0;
    for i := 0 to (Sender as TDBListView).Columns.Count - 1 do
    begin
      // c := GetColumn(i);
      p := (Sender as TDBListView).Columns[i];
      err := FALSE;
      if i = 0 then
        s := String(Item.Caption)
      else if i - 1 < Item.SubItems.Count then
        s := String(Item.SubItems[i - 1])
      else
      begin
        err := TRUE;
        s := 'Display Error!';
      end;
      for j := 1 to length(s) do
        if s[j] = #0 then
          s[j] := ' ';
      r := Rect;
      r.Left := L + Rect.Left;
      if p.Width > 0 then
        r.Right := r.Left + p.Width;
      if r.Right > 0 then
      begin
        //
        // Current Cell
        if b and (m_pCell.X = i) then
        begin
          // Color for selected cell
          if err then
            Sender.Canvas.Brush.Color := clRed
          else
            Sender.Canvas.Brush.Color := m_colSelection;
          Sender.Canvas.TextRect(Types.Rect(r.left + 1, r.top + 1, r.right, r.bottom), r.Left + 2, r.Top + H, s);
          Sender.Canvas.Brush.Color := kcolGrid2;
          Sender.Canvas.FrameRect(Types.Rect(r.left, r.top, r.right + 1, r.bottom + 1));
        end
        else
        begin
          // error condition: cannot retrieve value
          if err then
            Sender.Canvas.Brush.Color := clRed
          // Color for Sort Column
          else if (i = m_iSelectedColumn) and (m_pCell.Y <> Item.Index) then
            Sender.Canvas.Brush.Color := m_colSelectedColumnColor
          // Color for current line
          else if m_pCell.Y = Item.Index then
            Sender.Canvas.Brush.Color := m_colCurrentLineColor
          // Color for null cell
          else if s = krsNULLFIELD then
            Sender.Canvas.Brush.Color := m_colNullCell
          // Color for Normal Background
          else
            Sender.Canvas.Brush.Color := m_colBackgroundColor;
          if i <> m_iSelectedColumn then
          begin
            Sender.Canvas.TextRect(Types.Rect(r.left + 1, r.top + 1, r.right, r.bottom), r.Left + 2, r.Top + H, s);
            Sender.Canvas.Brush.Color := kcolGrid;
            Sender.Canvas.FrameRect(Types.Rect(r.left, r.top, r.right + 1, r.bottom + 1));
          end
          else
          begin
            Sender.Canvas.TextRect(Types.Rect(r.left, r.top, r.right, r.bottom), r.Left + 2, r.Top + H, s);
            Sender.Canvas.FrameRect(Types.Rect(r.left, r.top, r.right + 1, r.bottom + 1));
          end;
        end;
      end;
      L := L + (Sender as TDBListView).Columns[i].Width;
    end;
  end;
end;

// TDBListView
//   MouseDownClick
//
procedure TDBListView.MouseDownClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  d, L: TPoint;
begin
  d := m_pCell;
  L := CellAt(Sender, X, Y);
  if (L.X <> kiUNDEFINED) and (L.Y <> kiUNDEFINED) then
  begin
    m_pCell := L;
    if (m_pCell.Y <> d.Y) and (d.Y >= 0) and (d.Y < Items.Count) then
      dbCustomDrawItem(Sender as TCustomListView, (Sender as TDBListView).Items[d.Y], (Sender as TDBListView).Items[d.Y].DisplayRect(drBounds), []);
    dbCustomDrawItem(Sender as TCustomListView, (Sender as TDBListView).Items[m_pCell.Y], (Sender as TDBListView).Items[m_pCell.Y].DisplayRect(drBounds), []);
    SendDisplayMessage(self);
  end;
  m_bAcceptDblClick := TRUE;
end;

// TDBListView
//   ListViewKeyDown
//
procedure TDBListView.ListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  L, i: longint;
  d: TPoint;
begin
  try
    d := m_pCell;
    if (Key = 13) then                                            // Enter
      ListDoubleClick(sender)
    else if (Key = 36) then                                       // Home
      m_pCell.Y := 0
    else if (Key = 35) then                                       // End
    begin
      repeat until not GetNextBlock;
      m_pCell.Y := Items.Count - 1
    end
    else if (Key = 33) then                                       // Page Up
      m_pCell.Y := max(m_pCell.Y - VisibleRowCount, 0)
    else if (Key = 34) then                                       // Page Down
      m_pCell.Y := min(m_pCell.Y + VisibleRowCount, Items.Count - 1)
    else if (Key = 38) and (m_pCell.Y > 0) then                   // Up
      m_pCell.Y := m_pCell.Y - 1
    else if (Key = 40) and (m_pCell.Y < Items.Count - 1) then     // Down
      m_pCell.Y := m_pCell.Y + 1
    else if (Key = 39) and (m_pCell.X < Columns.Count - 1) then   // Right
      m_pCell.X := m_pCell.X + 1
    else if (Key = 37) and (m_pCell.X > 0) then                   // Left
      m_pCell.X := m_pCell.X - 1;
    if (Key in [33, 34, 35, 36, 37, 38, 39, 40]) and
       (m_pCell.Y >= 0) and
       (m_pCell.Y < (Sender as TDBListView).Items.Count) then
    begin
      if (m_pCell.Y <> d.Y) and (d.Y >= 0) and (d.Y < Items.Count) then
        dbCustomDrawItem(Sender as TCustomListView, (Sender as TDBListView).Items[d.Y], (Sender as TDBListView).Items[d.Y].DisplayRect(drBounds), []);
      dbCustomDrawItem(Sender as TCustomListView, (Sender as TDBListView).Items[m_pCell.Y], (Sender as TDBListView).Items[m_pCell.Y].DisplayRect(drBounds), []);
      // Check for vertical scroll
      if Key in [33, 34, 35, 36, 38, 40] then
      begin
        Selected := Items[m_pCell.Y];
        Items[m_pCell.Y].MakeVisible(FALSE);
        if Assigned(onChange) then
          OnChange(Sender, Selected, ctState);
      end;
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
      CheckForNextBlock;
      Key := 0;
      SendDisplayMessage(self);
    end;
  except
    on E:Exception do
      SetError(E.Message);
  end;
end;

// TDBListView
//   SendDisplayMessage
//
procedure TdbListView.SendDisplayMessage(sender: TObject);
begin
  if Assigned(m_FDisplayMessage) then
    m_FDisplayMessage(Sender, CellMessage);
end;

// TDBListView
//   DoBeginColumnResize
//
procedure TdbListView.DoBeginColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(m_FBeginColumnResizeEvent) then
    m_FBeginColumnResizeEvent(Self, columnindex, columnwidth);
end;

// TDBListView
//   DoEndColumnResize
//
procedure TdbListView.DoEndColumnResize(columnindex, columnwidth: Integer);
var
  p: TcDBListViewColumn;
  i: longint;
begin
  Invalidate;
  for i := 0 to Columns.Count - 1 do
  begin
    p := GetColumn(i);
    p.iWidth := Columns[i].Width;
    p.eOrigin := ecoUser;
  end;
  if Assigned(m_FEndColumnResizeEvent) then
    m_FEndColumnResizeEvent(Self, columnindex, columnwidth);
end;

// TDBListView
//   DoColumnResize
//
procedure TdbListView.DoColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(m_FColumnResizeEvent) then
    m_FColumnResizeEvent(Self, columnindex, columnwidth);
end;

// TDBListView
//   FindColumnIndex
//
function TdbListView.FindColumnIndex(pHeader: pNMHdr): Integer;
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

// TDBListView
//   WMNotify
//
procedure TdbListView.WMNotify(var Msg: TWMNotify);
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

// TDBListView
//   CreateWnd
//
procedure TdbListView.CreateWnd;
var
  wnd: HWND;
begin
  inherited;
  wnd := GetWindow(Handle, GW_CHILD);
  SetWindowLong(wnd, GWL_STYLE,
    GetWindowLong(wnd, GWL_STYLE) and not HDS_FULLDRAG);
end;

// TDBListView
//   FindColumnWidth
//
function TdbListView.FindColumnWidth(pHeader: pNMHdr): Integer;
begin
  Result := kiUNDEFINED;
  if Assigned(PHDNotify(pHeader)^.pItem) and
    ((PHDNotify(pHeader)^.pItem^.mask and HDI_WIDTH) <> 0) then
    Result := PHDNotify(pHeader)^.pItem^.cxy;
end;

// TDBListView
//   FindColumnWidth
//
procedure TdbListView.ListDoubleClick(Sender: TObject);
{$IFNDEF PACKAGE}
var
  frm: TfrmdbListViewEdit;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  frm := nil;
  if m_bAcceptDblClick and (m_pCell.X <> kiUNDEFINED) and (m_pCell.Y <> kiUNDEFINED) then
  try
    frm := TfrmdbListViewEdit.Create(self);
    frm.Options := m_esOptions;
    if m_bIsReadOnly then
      frm.Options := frm.Options + [edboReadOnly]
    else
      frm.Options := frm.Options - [edboReadOnly];
    frm.caption := Format('Value for %s', [CellMessage]);
    frm.Cell := m_pCell;
    frm.Width := m_rectDetail.Right - m_rectDetail.Left;
    frm.Height := m_rectDetail.Bottom - m_rectDetail.Top;
    if (m_rectDetail.Top = 0) and (m_rectDetail.Left = 0) then
      frm.Position := poMainFormCenter
    else
    begin
      frm.Position := poDesigned;
      frm.Top := m_rectDetail.Top;
      frm.Left := m_rectDetail.Left;
    end;
    if TextAtCell <> krsNULL then
      frm.Value := TextAtCell
    else
      frm.Value := ksEMPTY;
    //
    // Try Editing the value
    if (frm.ShowModal = mrOK) and not m_bIsReadOnly and (frm.Value <> TextAtCell) and (m_objRS <> nil) then
    try
      m_objConnection.BeginTrans;
      m_objRS.Move(m_pCell.Y, adBookmarkFirst);
      m_objRS.Update(m_objRS.FieldName(m_pCell.X), frm.Value);
      m_objConnection.CommitTrans;
      TextAtCell := frm.Value;
    except
      on E: Exception do
      begin
        m_objConnection.RollbackTrans;
        Application.MessageBox(PChar(Format('Update Failed: %s', [E.Message])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
        SetSQL(m_sSQL); // The grid has to be refreshed because some providers do not clear the error up.
      end;
    end;
  finally
    m_rectDetail.Left := frm.Left;
    m_rectDetail.Top := frm.Top;
    m_rectDetail.Right := m_rectDetail.Left + frm.Width;
    m_rectDetail.Bottom := m_rectDetail.Top + frm.Height;
    m_esOptions := frm.Options;
    frm.release;
  end;
{$ENDIF}
end;

// TDBListView
//   CellMessage
//
function TDBListView.CellMessage: String;
begin
  result := Format(krsSTATUS_COLLINE, [m_pCell.Y + 1, m_pCell.X + 1]);
end;

{$IFNDEF PACKAGE}
// TDBListView
//   SetConnection
//
procedure TDBListView.SetConnection(value: TcConnection);
begin
  m_objConnection := value;
end;
{$ENDIF}

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

// TDBListView
//   ListViewSort
//
function ListViewSort(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
var
  D: boolean;
begin
  D := (Item1.ListView as TdbListView).m_lstColumns[lParam].Tag = longint(etInteger);
  if lParam > 0 then
  begin
    if (Item1.ListView as TdbListView).Columns[lParam].Tag > 0 then
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
    if (Item1.ListView as TdbListView).Columns[lParam].Tag > 0 then
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

// TDBListView
//   onColumnClickEvent
//
procedure TDBListView.onColumnClickEvent(Sender: TObject; Column: TListColumn);
begin
  m_iSelectedColumn := Column.Index;
  CustomSort(@ListViewSort, Column.Index);
  Column.Tag := -Column.Tag;
end;

// TDBListView
//   GetCellText
//
function TDBListView.GetCellText: String;
begin
  result := ksEMPTY;
  if (m_pCell.X <> kiUNDEFINED) and (m_pCell.Y <> kiUNDEFINED) then
  begin
    if m_pCell.X = 0 then
      result := Items[m_pCell.Y].Caption
    else
      result := Items[m_pCell.Y].SubItems[m_pCell.X - 1];
  end;
end;

// TDBListView
//   SetBackgroundColor
//
procedure TDBListView.SetBackgroundColor(value: TColor);
begin
  m_colBackgroundColor := value;
  Color := m_colBackgroundColor;
end;

// TDBListView
//   SetSelectedColumnColor
//
procedure TDBListView.SetSelectedColumnColor(value: TColor);
begin
  m_colSelectedColumnColor := value;
end;

// TDBListView
//   SetSelectionColor
//
procedure TDBListView.SetSelectionColor(value: TColor);
begin
  m_colSelection := value;
end;

// TDBListView
//   SetCurrentLineColor
//
procedure TDBListView.SetCurrentLineColor(value: TColor);
begin
  m_colCurrentLineColor := value;
end;

// TDBListView
//   SetNullCellColor
//
procedure TDBListView.SetNullCellColor(value: TColor);
begin
  m_colNullCell := value;
end;

// TDBListView
//   SetIsReadOnly
//
procedure TDBListView.SetIsReadOnly(value: boolean);
begin
  // Note: Might not be able to set the CursorLocation to adUseClient and the LockType to adLockOptimistic (?)
  m_bIsReadOnly := value;
end;

// TDBListView
//   SetIsReadOnly
//
function TDBListView.GetIsReadOnly: boolean;
begin
  result := m_bIsReadOnly;
end;

// TDBListView
//   SetColumnWidth
//
procedure TDBListView.SetColumnWidth(Index, value: longint);
var
  p: TcDBListViewColumn;
begin
  p := GetColumn(Index);
  p.iWidth := value * Canvas.TextWidth('H');
  p.eOrigin := ecoUser;
end;

// TDBListView
//   GetColumnWidth
//
function TDBListView.GetColumnWidth(Index: longint): longint;
var
  p: TcDBListViewColumn;
begin
  p := GetColumn(Index);
  result := p.iWidth;
end;

// TDBListView
//   SetColumnCaption
//
procedure TDBListView.SetColumnCaption(Index: longint; value: string);
var
  p: TcDBListViewColumn;
begin
  p := GetColumn(Index);
  p.sName := value;
end;

// TDBListView
//   GetColumnCaption
//
function TDBListView.GetColumnCaption(Index: longint): string;
var
  p: TcDBListViewColumn;
begin
  p := GetColumn(Index);
  result := p.sName;
end;

// TDBListView
//   ClearColumns
//
procedure TDBListView.ClearColumns;
begin
  m_lstColumns.Clear;
end;

// TDBListView
//   CellAt
//
function TDBListView.CellAt(Sender: TObject; X, Y: Integer): TPoint;
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

// TDBListView
//   onContextPopupEvent
//
procedure TDBListView.onContextPopupEvent(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  MouseDownClick(Sender, mbLeft, [], MousePos.X, MousePos.Y);
  // still popup context popup menu.
  Handled := False;
end;

// TDBListView
//   GetTextAtCell
//
function TdbListView.GetTextAtCell: String;
begin
  result := ksEMPTY;
  if (m_pCell.X <> kiUNDEFINED) and (m_pCell.Y <> kiUNDEFINED) then
  begin
    if m_pCell.X = 0 then
      result := Items[m_pCell.Y].Caption
    else
      result := Items[m_pCell.Y].SubItems[m_pCell.X - 1];
  end;
end;

// TDBListView
//   SetTextAtCell
//
procedure TdbListView.SetTextAtCell(value: String);
begin
  if (m_pCell.X <> kiUNDEFINED) and (m_pCell.Y <> kiUNDEFINED) then
  begin
    if m_pCell.X = 0 then
      Items[m_pCell.Y].Caption := value
    else
      Items[m_pCell.Y].SubItems[m_pCell.X - 1] := value;
  end;
end;

// TDBListView
//   onStop
//
procedure TDBListView.onStop(Sender: TObject);
begin
  m_bStop := TRUE;
  Screen.Cursor := crDefault;
end;

// TDBListView
//   onDeActivate
//
procedure TDBListView.onDeActivate(Sender: TObject);
begin
  m_bAcceptDblClick := FALSE;
end;

// TDBListView
//   GetColumn
//
function TdbListView.GetColumn(Index: longint): TcDBListViewColumn;
begin
  while Index > m_lstColumns.Count - 1 do
    m_lstColumns.Add(TcDBListViewColumn.Create(nil));
  result := m_lstColumns[Index] as TcDBListViewColumn;
end;

// TDBListView
//   GetSupport
//
function TdbListView.GetSupport(Index: TeFeatureSupport): boolean;
begin
  result := FALSE;
{$IFNDEF PACKAGE}
  case Index of
    efsEdit:
      result := (m_objConnection <> nil) and (m_objConnection.eMode = ecmMDAC);
  end;
{$ENDIF}
end;

// TDBListView
//   GetCanClipboard
//
function TdbListView.GetCanClipboard: boolean;
begin
  result := (m_pCell.X <> kiUNDEFINED) and (m_pCell.Y <> kiUNDEFINED);
end;

// TDBListView
//   SetError
//
procedure TDBListView.SetError(value: String);
begin
  m_lstErrors.Add(value);
end;

// TDBListView
//   GetErrors
//
function TdbListView.GetErrors: String;
begin
  result := m_lstErrors.Text;
end;

//
// TcDBListViewColumn
//

// TcDBListViewColumn
//   Create
//
constructor TcDBListViewColumn.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  Clear;
end;

// TcDBListViewColumn
//   Create
//
procedure TcDBListViewColumn.Clear;
begin
  inherited Clear;
  m_eOrigin := ecoADO;
  m_iWidth := kiUNDEFINED;
  m_eDatatype := etUndefined;
end;

end.





