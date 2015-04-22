unit SourceListBox;

interface

uses
  Windows, Classes, SysUtils, StdCtrls, daObjectLib, Controls,
  Graphics, ImgList;

type
  TSourceListBoxItem = class;

  TSourceListBox = class(TCustomListBox)
  private
    FList: TcCollection;
    FImageList1: TImageList;
    FImageList2: TImageList;
    FSourceColor: TColor;

  private
    function    GetLine(value: string; Index: longint): String;
    function    GetLineCount(Value: string): longint;
    procedure   OnsbsListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure   OnsbsListBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure   SetLineEnabled(Index: longint; value: boolean);
    function    GetLineEnabled(Index: longint): boolean;
    procedure   SetLineIcon(Index, IconNum: longint; value: longint);
    function    GetLineIcon(Index, IconNum: longint): longint;
    function    GetPointer(Index: longint): TSourceListBoxItem;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

  public
    property    LineEnabled[Index: longint]: boolean read GetLineEnabled write SetLineEnabled;
    property    LineIcon[Index, IconNum: longint]: longint read GetLineIcon write SetLineIcon;
    procedure   Clear; override;

  published
    property    ImageList1: TImageList read FImageList1 write FImageList1;
    property    ImageList2: TImageList read FImageList2 write FImageList2;
    property    SourceColor: TColor read FSourceColor write FSourceColor;
    property    Style;
    property    AutoComplete;
    property    AutoCompleteDelay;
    property    Align;
    property    Anchors;
    property    BevelEdges;
    property    BevelInner;
    property    BevelKind default bkNone;
    property    BevelOuter;
    property    BevelWidth;
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
    property    OnMouseActivate;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseLeave;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDock;
    property    OnStartDrag;
  end;

  TSourceListBoxItem = class(TcObject)
  private
    m_iIcon: array[0 .. 1] of longint;
    m_bEnabled: boolean;
  private
    procedure   SetIcon(Index: longint; value: longint);
    function    GetIcon(Index: longint): longint;
  public
    constructor Create(parParent: TcObject); override;
  public
    property    iIcon[Index: longint]: longint read GetIcon write SetIcon;
    property    bEnabled: boolean read m_bEnabled write m_bEnabled;
  end;

procedure Register;

implementation

uses
  strUtils,
  Math;

const
  kcCR = #13;
  kcLF = #10;
  ksEMPTY = '';
  kiUNDEFINED = -1;
  kiICONBORDER_LEFT = 4;
  kiICONBORDER_TOP = 4;
  kiICONBORDER_BOTTOM = 3;
  kriLINEMAX = 10;

procedure Register;
begin
  RegisterComponents('Seabird', [TSourceListBox]);
end;  {Register}

//  TSourceListBox
//    Create
//
constructor TSourceListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TcCollection.Create(nil);
  Style := lbOwnerDrawVariable;
  OnDrawItem := OnsbsListBoxDrawItem;
  OnMeasureItem := OnsbsListBoxMeasureItem;
  FImageList1 := nil;
  FImageList2 := nil;
  FSourceColor := clHighlightText;
end;

//  TSourceListBox
//    Destroy
//
destructor TSourceListBox.Destroy;
begin
  FList.Free;
  inherited destroy;
end;

// Tool
//   GetLineCount
//
function TSourceListBox.GetLineCount(Value: string): longint;
var
  i: longint;
begin
  result := 0;
  if value <> ksEMPTY then
    result := result + 1;
  for i := 1 to length(value) do
    if value[i] = kcLF then
      result := result + 1;
end;

//  TSourceListBox
//    Line
//
function TSourceListBox.GetLine(value: string; Index: longint): String;
var
  L, i: longint;
begin
  result := ksEMPTY;
  if value <> ksEMPTY then
  begin
    L := 1;
    if index > 0 then
      while (index > 0) and (L < length(value)) do
      begin
        if value[L] = kcLF then
          dec(index);
        inc(L);
      end;
    i := pos(kcLF, system.copy(value, L, length(value)));
    if i = 0 then
      i := length(value) + 1;
    result := system.copy(value, L, i - 1);
  end;
end;

//  TSourceListBox
//    OnsbsListBoxDrawItem
//
procedure TSourceListBox.OnsbsListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  p: TSourceListBoxItem;
  r: TRect;
  i, LH, FS: longint;
  s: String;
  c, bc: TColor;
begin
  p := GetPointer(Index);
  // Background
  c := Canvas.Font.Color;
  bc := Canvas.Brush.Color;
  Canvas.FillRect(Rect);
  // Icons
  if (FImageList1 <> nil) and (p.iIcon[0] <> kiUNDEFINED) then
    FImageList1.Draw(Canvas, Rect.Left + kiICONBORDER_LEFT, Rect.Top + kiICONBORDER_TOP, p.iIcon[0], dsTransparent, itImage, p.bEnabled);
  if (FImageList2 <> nil) and (p.iIcon[1] <> kiUNDEFINED) then
    FImageList2.Draw(Canvas, Rect.Left + kiICONBORDER_LEFT, Rect.Top + kiICONBORDER_TOP + FImageList1.Height + kiICONBORDER_TOP, p.iIcon[1], dsTransparent, itImage, TRUE);
  // Description Line
  LH := Canvas.TextHeight('W');
  FS := Canvas.Font.Size;
  if Items[Index] <> ksEMPTY then
    for i := 0 to min(GetLineCount(Items[Index]) - 1, kriLINEMAX - 1) do
    begin
      s := GetLine(Items[Index], i);
      r := Rect;
      r.Left := r.Left + 24 + 2;
      r.Top := Rect.Top + i * LH + 2;
      if i > 1 then
        r.Top := r.Top + 3;
      r.Bottom := r.Top + LH + 2;
      if r.Bottom > Rect.Bottom then
        r.Bottom := Rect.Bottom;
      if (r.Top >= Rect.Top) and (r.Top < Rect.Bottom) and (r.Top < r.Bottom) then
      begin
        if not p.bEnabled then
          Canvas.Font.Color := clInactiveCaptionText
        else if i = 0 then
          Canvas.Font.Color := Font.Color
        else if i = 1 then
          Canvas.Font.Color := clMedGray
        else if i > 1 then
          Canvas.Font.Color := FSourceColor;
        if i > 1 then
          Canvas.Font.Name := 'Courier New';
        if i = 1 then
          Canvas.Font.Size := FS - 3
        else
          Canvas.Font.Size := FS;
        Canvas.Brush.Color := bc;
        if i = 0 then
          Canvas.Font.Style := [fsBold]
        else
          Canvas.Font.Style := [];
        Canvas.TextRect(r, r.Left, r.Top, trim(s));
      end;
    end;
  Canvas.Font.Color := c;
  Canvas.Font.Name := Font.Name;
  // Bottom Line
  Canvas.Pen.Color := clActiveBorder;
  Canvas.Pen.Style := psDot;
  Canvas.PenPos := Point(Rect.Left + 4, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Right - 4, Rect.Bottom - 1);
end;

//  TSourceListBox
//    OnsbsListBoxMeasureItem
//
procedure TSourceListBox.OnsbsListBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
var
  L: longint;
begin
  if Index < Items.Count then
  begin
    Height := min(GetLineCount(Items[Index]), kriLINEMAX) * Canvas.TextHeight('W') + kiICONBORDER_TOP + kiICONBORDER_BOTTOM;
    L := 0;
    if FImageList1 <> nil then
      L := L + kiICONBORDER_TOP + FImageList1.Height;
    if FImageList2 <> nil then
      L := L + kiICONBORDER_TOP + FImageList2.Height;
    Height := min(max(Height, L + kiICONBORDER_BOTTOM), self.Height);
  end;
end;

//  TSourceListBox
//    SetLineEnabled
//
procedure TSourceListBox.SetLineEnabled(Index: longint; Value: boolean);
var
  p: TSourceListBoxItem;
begin
  p := GetPointer(Index);
  if p <> nil then
  begin
    p.bEnabled := value;
    Invalidate;
  end;
end;

//  TSourceListBox
//    GetLineEnabled
//
function TSourceListBox.GetLineEnabled(Index: longint): boolean;
var
  p: TSourceListBoxItem;
begin
  result := TRUE;
  p := GetPointer(Index);
  if p <> nil then
    result := p.bEnabled;
end;

//  TSourceListBox
//    SetLineIcon
//
procedure TSourceListBox.SetLineIcon(Index, IconNum: longint; value: longint);
var
  p: TSourceListBoxItem;
begin
  p := GetPointer(Index);
  if p <> nil then
  begin
    if IconNum = 0 then
      p.iIcon[0] := value
    else if IconNum = 1 then
      p.iIcon[1] := value;
    Invalidate;
  end;
end;

//  TSourceListBox
//    GetLineIcon
//
function TSourceListBox.GetLineIcon(Index, IconNum: longint): longint;
var
  p: TSourceListBoxItem;
begin
  result := kiUNDEFINED;
  p := GetPointer(Index);
  if p <> nil then
  begin
    if IconNum = 0 then
      result := p.iIcon[0]
    else if IconNum = 1 then
      result := p.iIcon[1];
  end;
end;

//  TSourceListBox
//    GetPointer
//
function TSourceListBox.GetPointer(Index: longint): TSourceListBoxItem;
begin
  result := nil;
  if Index < Items.Count then
  begin
    while Index >= FList.Count do
      FList.Add(TSourceListBoxItem.Create(nil));
    result := FList[Index] as TSourceListBoxItem;
  end;
end;

//  TSourceListBox
//    Clear
//
procedure TSourceListBox.Clear;
begin
  inherited Clear;
  FList.Clear;
end;

//
//  TSourceListBoxItem
//

//  TSourceListBoxItem
//    Create
//
constructor TSourceListBoxItem.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_iIcon[0] := kiUNDEFINED;
  m_iIcon[1] := kiUNDEFINED;
  m_bEnabled := TRUE;
end;

//  TSourceListBoxItem
//    SetIcon
//
procedure TSourceListBoxItem.SetIcon(Index: longint; value: longint);
begin
  m_iIcon[Index] := value;
end;

//  TSourceListBoxItem
//    GetIcon
//
function TSourceListBoxItem.GetIcon(Index: longint): longint;
begin
  result := m_iIcon[Index];
end;

end.
