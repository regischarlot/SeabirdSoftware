unit ImageListBoxLib;

interface

uses
  StdCtrls,
  Controls,
  Messages,
  Types,
  Classes;

type
  TVScrollEventType = (vsLineUp, vsLineDown, vsPageUp, vsPageDown, vsThumbPos, vsThumbTrack, vsTop, vsBottom, vsEndScroll);
  TVScrollEvent = procedure (Sender: TObject; pos: SmallInt; EventType: TVScrollEventType) of object;

  //
  // TImageListBox
  //
  TImageListBox = class(TCustomListBox)
  private
    // Private members
    //
    m_onVScroll: TVScrollEvent;
    m_imgList: TImageList;

  private
    // Private methods
    //
    procedure   WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure   onPanelDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    function    GetSelections: TList;
    procedure   SetSelections(value: TList);

  protected
    // Protected methods
    //
    procedure   VScroll(Pos: integer; EventType: TVScrollEventType); virtual;

  public
    // Public declarations
    //
    constructor Create(AOwner: TComponent); override;
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom

  published
    // published properties
    //
    property    Style;
    property    AutoComplete;
    property    Align;
    property    Anchors;
    property    BevelEdges;
    property    BevelInner;
    property    BevelKind default bkNone;
    property    BevelOuter;
    property    BiDiMode;
    property    BorderStyle;
    property    Color;
    property    Columns;
    property    Constraints;
    property    Ctl3D;
    property    DragCursor;
    property    DragKind;
    property    DragMode;
    property    Enabled;
    property    ExtendedSelect;
    property    Font;
    property    ImeMode;
    property    ImeName;
    property    IntegralHeight;
    property    ItemHeight;
    property    Items;
    property    MultiSelect;
    property    ParentBiDiMode;
    property    ParentColor;
    property    ParentCtl3D;
    property    ParentFont;
    property    ParentShowHint;
    property    PopupMenu;
    property    ScrollWidth;
    property    ShowHint;
    property    Sorted;
    property    TabOrder;
    property    TabStop;
    property    TabWidth;
    property    Visible;
    property    OnClick;
    property    OnContextPopup;
    property    OnData;
    property    OnDataFind;
    property    OnDataObject;
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnDrawItem;
    property    OnEndDock;
    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnKeyDown;
    property    OnKeyPress;
    property    OnKeyUp;
    property    OnMeasureItem;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDock;
    property    OnStartDrag;
    property    onVerticalScroll: TvScrollEvent read m_onVScroll                write m_onVScroll;
    property    ImageList: TImageList           read m_imgList                  write m_imgList;
    property    Selections: TList               read GetSelections              write SetSelections;
  end;

procedure Register;

implementation

uses
  Windows,
  daObjectLib,
  daGlobals,
  sysUtils;

procedure Register;
begin
  RegisterComponents('Seabird', [TImageListBox]);
end;  {Register}

const
  ksEMPTY = '';
  kiUNDEFINED = -1;

//
// TImageListBox
//

// TImageListBox
//   Create
//
constructor TImageListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  m_onVScroll := nil;
  Style := lbOwnerDrawFixed;
  m_imgList := nil;
  onDrawItem := onPanelDrawItem;
end;

// TImageListBox
//   WMVScroll
//
procedure TImageListBox.WMVScroll(var message: TWMVScroll);
var
  e: TVScrollEventType;
begin
  inherited;
  e := TVScrollEventType(Message.ScrollCode);
  if e in [vsThumbPos, vsThumbTrack] then
    VScroll(Message.Pos, e)
  else
    VScroll(GetScrollPos(Handle, SB_VERT), e);
end;

// TImageListBox
//   VScroll
//
procedure TImageListBox.VScroll(Pos: integer; EventType: TVScrollEventType);
begin
  if assigned(m_onVScroll) then
    m_onVScroll(Parent, Pos, EventType); // Parent is TvPanel
end;

// TImageListBox
//   onPanelDrawItem
//
procedure TImageListBox.onPanelDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  L, B: longint;
  p: TObject;
begin
  // Clear up space
  Canvas.FillRect(Rect);
  // Image
  L := 0;
  if m_imgList <> nil then
  begin
    L := m_imgList.Width;
    p := TcObject((Control as TCustomListBox).Items.Objects[Index]);
    B := kiUNDEFINED;
    if (p <> nil) and (p is TcObject) then
      B := (p as TcObject).Tag;
    if B <> kiUNDEFINED then
      m_imgList.Draw(Canvas, Rect.Left + 1, Rect.Top, B);
  end;
  // display the text
  inc(Rect.Left, L + 3);
  Canvas.TextRect(Rect, Rect.Left, Rect.Top + (ItemHeight - (Rect.Bottom - Rect.Top)) div 2, (Control as TCustomListBox).Items[Index]);
end;

// TImageListBox
//   GetSelections
//
function TImageListBox.GetSelections: TList;
var
  i: longint;
begin
  result := TList.Create;
  for i := 0 to Items.Count - 1 do
    if Selected[i] then
      result.Add(Items.Objects[i]);
end;

// TImageListBox
//   SetSelections
//
procedure TImageListBox.SetSelections(value: TList);
var
  i, L: longint;
begin
  for i := 0 to value.Count - 1 do
  begin
    L := Items.IndexOfObject(value[i]);
    if L <> kiUNDEFINED then
      Selected[L] := TRUE;
  end;
end;

end.
