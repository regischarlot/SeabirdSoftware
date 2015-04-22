unit Progress;

interface

uses
  Classes,
  ExtCtrls,
  Graphics;

type
  TeProgressMode = (epmNone, epmExecute, epmScript);

  TProgress = class(TCustomPanel)
  {******************************************************************************
  * Author: Regis Charlot
  *         seabird Software, LLC
  *
  * Description: wrapper for a Progress bar
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_iLineHeight, m_iStart, m_iEnd, m_iTopOrigin: longint;
    m_colBrush: array[TeProgressMode] of TColor;
    m_eMode: TeProgressMode;

  protected
    // protected declarations
    //
    procedure   Paint; override;
    function    GetBrush(index: TeProgressMode): TColor;
    procedure   SetBrush(index: TeProgressMode; value: TColor);

  public
    // Public declarations
    //
    constructor Create(AOwner: TComponent); override;

  public
    // Public declarations
    //
    property iLineHeight: longint     read m_iLineHeight  write m_iLineHeight;
    property iStart: longint          read m_iStart       write m_iStart;
    property iEnd: longint            read m_iEnd         write m_iEnd;
    property iTopOrigin: longint      read m_iTopOrigin   write m_iTopOrigin;
    property Brush[index: TeProgressMode]: TColor read GetBrush write SetBrush;
    property eMode: TeProgressMode    read m_eMode        write m_eMode;

  published
    // published declarations
    //
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Seabird', [TProgress]);
end;  {Register}

// TProgress
//   Create
//
constructor TProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  m_iLineHeight := 0;
  m_iStart := -1;
  m_iEnd := -1;
  m_iTopOrigin := 0;
  m_colBrush[epmNone] := clWindow;
  m_colBrush[epmExecute] := $3CC7FF;
  m_colBrush[epmScript] := $89FF15;
end;

// TProgress
//   Paint
//
procedure TProgress.Paint;
var
  L: longint;
begin
  inherited Paint;
  try
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    L := ((m_iStart - m_iTopOrigin + 1) * m_iLineHeight);
    Canvas.Pen.Color := m_colBrush[m_eMode];
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.Rectangle(rect(0, L, Width, L + (abs(m_iEnd - m_iStart) + 1) * m_iLineHeight));
  except
    //
  end;
end;

// TProgress
//   GetBrush
//
function TProgress.GetBrush(index: TeProgressMode): TColor;
begin
  result := m_colBrush[index];
end;

// TProgress
//   SetBrush
//
procedure TProgress.SetBrush(index: TeProgressMode; value: TColor);
begin
  m_colBrush[index] := value;
end;

end.



