unit PanelLib;

interface

uses
  Classes,
  Controls,
  ExtCtrls,
  StatementLib,
  Graphics,
  ImageListBoxLib; // TvImageListBo

type
  //
  // TvCustomPanel
  //
  TvCustomPanel = class(TCustomPanel)
  private
    // Private members
    //
    m_colBorder: TColor;
    m_iAngle: longint;

  protected
    // protected methods
    //
    procedure   Paint; override;
    function    GetColor(index: longint): TColor; virtual;
    procedure   SetColor(index: longint; value: TColor); virtual;

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

  public
    // Public properties
    //
    property    Color: TColor       index 0     read GetColor                   write SetColor;
    property    colBorder: TColor   index 1     read GetColor                   write SetColor;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
  end;

  //
  // TvPanel
  //
  TvPanel = class(TvCustomPanel)
  private
    // Private members
    //
    m_pnlTitle: TPanel;
    m_lstColumns: TImageListBox;
    m_objData: TcStatementPart;
    m_ptX, m_ptY: longint;
    m_bMouseDown: boolean;
    m_onChangeLocation: TNotifyEvent;
    m_onScroll: TNotifyEvent;
    m_iDeltaX, m_iDeltaY: longint;

  private
    // Private methods
    //
    procedure   SetData(value: TcStatementPart);
    procedure   onTitleMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   SetEndDrag(value: TEndDragEvent);
    procedure   SetDragOver(value: TDragOverEvent);
    procedure   onColumnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   DoHeaderColor(Sender: TObject);
    procedure   onPanelMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onListBoxScroll(Sender: TObject; pos: SmallInt; EventType: TVScrollEventType);
    procedure   SetImageList(value: TImageList);

  protected
    // protected methods
    //
    procedure   SetColor(index: longint; value: TColor); override;
    procedure   SetCaption(value: String);

  public
    // Public declarations
    //
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    Procedure   AddColumn(value: String; pointer: TObject);
    function    Blub(Index: longint): longint;

  public
    // Public properties
    //
    property    Parent;
    property    Caption: String                                                 write SetCaption;
    property    Data: TcStatementPart           read m_objData                  write SetData;
    property    hdlDragOver: TDragOverEvent                                     write SetDragOver;
    property    hdlEndDrag: TEndDragEvent                                       write SetEndDrag;
    property    onChangeLocation: TNotifyEvent  read m_onChangeLocation         write m_onChangeLocation;
    property    onScroll: TNotifyEvent          read m_onScroll                 write m_onScroll;
    property    ImageList: TImageList                                           write SetImageList;

  end;

implementation

uses
  daGlobals,
  Forms,
  Math,
  Types;

type
  TeSection = (esLow, esMiddle, esHigh);

const
  kabHEADERCOLOR: array[boolean] of TColor =
    (clGradientInactiveCaption, clGradientActiveCaption);

//
// TvCustomPanel
//

// TvCustomPanel
//   Create
//
constructor TvCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  m_colBorder := color;
  m_iAngle := 8;
  DoubleBuffered := GetWinVersion >= wvWinXP;
end;

// TvCustomPanel
//   Paint
//
procedure TvCustomPanel.Paint;
var
  r: TRect;
begin
  r := GetClientRect;
  Canvas.Brush.Color := color;
  Canvas.Pen.Color := color;
  Canvas.Rectangle(r);
  Canvas.Brush.Color := color;
  Canvas.Pen.Color := m_colBorder;
  Canvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, m_iAngle, m_iAngle);
end;

// TvCustomPanel
//   SetColor
//
function TvCustomPanel.GetColor(index: longint): TColor;
begin
  result := inherited Color;
  case index of
    0:
      result := inherited Color;
    1:
      result := m_colBorder;
  end;
end;

// TvCustomPanel
//   SetColor
//
procedure TvCustomPanel.SetColor(index: longint; value: TColor);
begin
  case index of
    0:
      inherited  Color := value;
    1:
      m_colBorder := value;
  end;
end;

//
// TvPanel
//

// TvPanel
//   Create
//
constructor TvPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  Width := 120;
  height := 80;
  Caption := ksEMPTY;
  BevelInner := bvSpace;
  BevelOuter := bvSpace;
  BorderWidth := 2;
  OnMouseDown := onMouseDownHandler;
  OnMouseMove := onPanelMouseMoveHandler;
  OnMouseUp := onMouseUpHandler;
  // Title
  m_pnlTitle := TPanel.Create(self);
  m_pnlTitle.Parent := self;
  m_pnlTitle.Height := Canvas.TextHeight('L') + 2;
  m_pnlTitle.Align := alTop;
  m_pnlTitle.BevelOuter := bvNone;
  m_pnlTitle.Alignment := taLeftJustify;
  m_pnlTitle.OnMouseDown := onMouseDownHandler;
  m_pnlTitle.OnMouseMove := onTitleMouseMoveHandler;
  m_pnlTitle.OnMouseUp := onMouseUpHandler;
  m_pnlTitle.DoubleBuffered := GetWinVersion >= wvWinXP;
  // Columns
  m_lstColumns := TImageListBox.Create(self);
  m_lstColumns.Parent := self;
  m_lstColumns.Align := alClient;
  m_lstColumns.BevelInner := bvNone;
  m_lstColumns.BorderStyle := bsNone;
  m_lstColumns.onMouseDown := onColumnMouseDown;
  m_lstColumns.onVerticalScroll := onListBoxScroll;
  m_lstColumns.DoubleBuffered := GetWinVersion >= wvWinXP;
  //
  colBorder := clBtnHighlight;
  Color := kabHEADERCOLOR[FALSE];
  //
  m_objData := nil;
  m_bMouseDown := FALSE;
  m_onChangeLocation := nil;
  m_onScroll := nil;
end;

// TvPanel
//   Destroy
//
destructor TvPanel.Destroy;
begin
  m_onChangeLocation := nil;
  m_onScroll := nil;
  inherited Destroy;
end;

// TvPanel
//   SetStatementPart
//
procedure TvPanel.SetData(value: TcStatementPart);
begin
  m_objData := value;
  m_lstColumns.Tag := longint(value);
end;

// TvPanel
//   SetCaption
//
procedure TvPanel.SetCaption(value: String);
begin
  m_pnlTitle.Caption := ' ' + value;
end;

// TvPanel
//   AddColumn
//
procedure TvPanel.AddColumn(value: String; pointer: TObject);
begin
  m_lstColumns.Items.AddObject(value, pointer);
end;

// TvPanel
//   onColumnMouseDown
//
procedure TvPanel.onColumnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    (Sender as TControl).BeginDrag(FALSE);
  DoHeaderColor(Sender);
end;

// TvPanel
//   SetEndDrag
//
procedure TvPanel.SetEndDrag(value: TEndDragEvent);
begin
  m_lstColumns.OnEndDrag := value;
end;

// TvPanel
//   SetDragOver
//
procedure TvPanel.SetDragOver(value: TDragOverEvent);
begin
  m_lstColumns.OnDragOver := value;
end;

// TvPanel
//   DoHeaderColor
//
procedure TvPanel.DoHeaderColor(Sender: TObject);
var
  p: TWinControl;
  i: longint;
begin
  // Get a TvCustomPanel
  while (Sender <> nil) and (Sender is TControl) and not (Sender is TvPanel) and ((Sender as TControl).Parent <> nil) and ((Sender as TControl).Parent is TControl) do
    Sender := (Sender as TControl).Parent;
  // Highlight correct panel
  if (Sender <> nil) and (Sender is TvPanel) and ((Sender as TvPanel).Parent <> nil) then
  begin
    p := (Sender as TvCustomPanel).Parent as TWinControl;
    for i := 0 to p.ComponentCount - 1 do
      if (p.Components[i] <> nil) and (p.Components[i] is TvPanel) then
        (p.Components[i] as TvPanel).Color := kabHEADERCOLOR[p.Components[i] = Sender];
  end;
end;

// TvPanel
//   SetColor
//
procedure TvPanel.SetColor(index: longint; value: TColor);
begin
  inherited SetColor(index, value);
  m_pnlTitle.Color := value;
end;

// TvPanel
//   onMouseUpHandler
//
procedure TvPanel.onMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  m_bMouseDown := FALSE;
end;

// TvPanel
//   onMouseDownHandler
//
procedure TvPanel.onMouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    m_bMouseDown := TRUE;
    m_ptX := X;
    m_ptY := Y;
    m_iDeltaX := Width - X;
    m_iDeltaY := Height - Y;
    DoHeaderColor(self);
  end;
end;

// TvPanel
//   onTitleMouseMoveHandler
//
procedure TvPanel.onTitleMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if m_bMouseDown then
  begin
    Top := max(0, Top + Y - m_ptY);
    Left := max(0, Left + X - m_ptX);
    if assigned(onChangeLocation) then
      onChangeLocation(self);
  end;
end;

// TvPanel
//   onPanelMouseMoveHandler
//
procedure TvPanel.onPanelMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  kacCURSOR: array[TeSection, TeSection] of TCursor =
    ((crSizeNWSE, crSizeWE, crSizeNESW),
     (crSizeNS, crDefault, crSizeNS),
     (crSizeNESW, crSizeWE, crSizeNWSE));
  kiFACTOR = 0.2;
var
  eX, eY: TeSection;
begin
  with sender as TWinControl do
  begin
    if X <= round(Width * kiFACTOR) then
      eX := esLow
    else if X >= round(Width * (1 - kiFACTOR)) then
      eX := esHigh
    else
      eX := esMiddle;
    if Y <= round(Height * kiFACTOR) then
      eY := esLow
    else if Y >= round(Height * (1 - kiFACTOR)) then
      eY := esHigh
    else
      eY := esMiddle;
    Cursor := kacCURSOR[eX, eY];

    if m_bMouseDown then
    begin
      case eX of
        esLow:
        begin
          Width := Width - (X - m_ptX);
          Left := Left + (X - m_ptX);
          case eY of
            esLow: // Top Left Corner
              begin
                Height := Height - (Y - m_ptY); // OK
                Top := Top + (Y - m_ptY); // ok
              end;
            esHigh: // Bottom Left Corner
              Height := (Y + m_iDeltaY);  // OK
          end;
        end;
        esMiddle:
        case eY of
          esLow: // Top Side
          begin
            Height := Height - (Y - m_ptY); // OK
            Top := Top + (Y - m_ptY); // OK
          end;
          esHigh: // Bottom Side
            Height := (Y + m_iDeltaY);  // OK
        end;
        esHigh:
        begin
          Width := X + m_iDeltaX;
          case eY of
            esLow: // Top Right Corner
            begin
              Height := Height - (Y - m_ptY); // OK
              Top := Top + (Y - m_ptY); // OK
            end;
            esHigh: // Bottom Right Corner
              Height := (Y + m_iDeltaY);  // OK
          end;
        end;
      end;
      if assigned(onChangeLocation) then
        onChangeLocation(self);
    end;
  end;
end;

// TvPanel
//   Blub
//
function TvPanel.Blub(Index: longint): longint;
var
  r1, r2: TRect;
  L: longint;
begin
  result := BorderWidth + m_pnlTitle.Height;
  if Index <> kiUNDEFINED then
  begin
    r1 := m_lstColumns.ItemRect(Index);
    r2 := m_lstColumns.ItemRect(m_lstColumns.TopIndex);
    L := r1.Bottom - r1.Top;
    r1.Top := r1.Top - r2.Top;
    r1.Bottom := r1.Top + L;
    if r1.Top < 0 then
      result := result + 0
    else if r1.Bottom > m_lstColumns.Height then
      result := result + m_lstColumns.Height
    else
      result := result + (r1.Top + r1.Bottom) div 2 + 4;
  end;
end;

// TvPanel
//   onListBoxScroll
//
procedure TvPanel.onListBoxScroll(Sender: TObject; pos: SmallInt; EventType: TVScrollEventType);
begin
  if assigned(m_onScroll) then
    m_onScroll(Sender);
end;

// TvPanel
//   SetImageList
//
procedure TvPanel.SetImageList(value: TImageList);
begin
  m_lstColumns.ImageList := value;
end;

end.
