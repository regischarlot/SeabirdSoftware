unit EdgeLib;

interface

uses
  Classes,
  Types,
  ExtCtrls,
  Controls,
  StatementLib;

type
  //
  // TvEdge
  //
  TvEdge = class(TPaintBox)
  private
    // Private members
    //
    m_objControlLHS: TControl;
    m_objControlRHS: TControl;
    m_iLHS, m_iRHS: longint;
    m_iPreviousL1, m_iPreviousL2, m_iPreviousMode: longint;

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
    procedure   PaintEdge(Sender: TObject);

  public
    // Public properties
    //
    property    ControlLHS: TControl read m_objControlLHS write m_objControlLHS;
    property    ControlRHS: TControl read m_objControlRHS write m_objControlRHS;
    property    iLHS: longint read m_iLHS write m_iLHS;
    property    iRHS: longint read m_iRHS write m_iRHS;
  end;

implementation

uses
  Math,
  daObjectLib,
  sysUTils,
  Graphics,
  PanelLib,
  daResourceStrings;

//
// TvEdge
//

// TvEdge
//   SetCoordinates
//
constructor TvEdge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  onPaint := PaintEdge;
  m_objControlLHS := nil;
  m_objControlRHS := nil;
  m_iLHS := kiUNDEFINED;
  m_iRHS := kiUNDEFINED;
  m_iPreviousL1 := kiUNDEFINED;
  m_iPreviousL2 := kiUNDEFINED;
  m_iPreviousMode := kiUNDEFINED;
end;

// TvEdge
//   PaintEdge
//
procedure TvEdge.PaintEdge(Sender: TObject);

  procedure SetLine(P1x, P1y, P2x, P2y, iMode: longint; parColor: TColor);

    procedure Ball(X, Y: longint);
    begin
      case iMode of
        1: // Horizontal
        begin
          Canvas.MoveTo(X, Y - 1);
          Canvas.LineTo(X + 4, Y - 1);
          Canvas.MoveTo(X, Y + 1);
          Canvas.LineTo(X + 4, Y + 1);
          Canvas.MoveTo(X + 1, Y - 2);
          Canvas.LineTo(X + 3, Y - 2);
          Canvas.MoveTo(X + 1, Y + 2);
          Canvas.LineTo(X + 3, Y + 2);
        end;
        2: // Vertical
        begin
          Canvas.MoveTo(X - 1, Y);
          Canvas.LineTo(X + 2, Y);
          Canvas.MoveTo(X - 2, Y + 1);
          Canvas.LineTo(X + 3, Y + 1);
          Canvas.MoveTo(X - 2, Y + 2);
          Canvas.LineTo(X + 3, Y + 2);
          Canvas.MoveTo(X - 1, Y + 3);
          Canvas.LineTo(X + 2, Y + 3);
        end;
      end;
    end;

  begin
    Canvas.Pen.Color := parColor;
    Canvas.Pen.Mode := pmCopy;
    case iMode of
      1:
      begin
        Canvas.MoveTo(P1x, P1y);
        Canvas.LineTo(P2x div 2, P1y);
        Canvas.LineTo(P2x div 2, P2y);
        Canvas.LineTo(P2x, P2y);
        Ball(P1x, P1y);
        Ball(P2x - 4, P2y);
        m_iPreviousL1 := P1y;
        m_iPreviousL2 := P2y;
      end;
      2:
      begin
        Canvas.MoveTo(P1x, P1y);
        Canvas.LineTo(P1x, P2y div 2);
        Canvas.LineTo(P2x, P2y div 2);
        Canvas.LineTo(P2x, P2y);
        Ball(P1x, P1y);
        Ball(P2x, P2y - 4);
        m_iPreviousL1 := P1x;
        m_iPreviousL2 := P2x;
      end;
    end;
    m_iPreviousMode := iMode;
  end;

var
  c1, c2: TControl;
  L1, L2: longint;
begin



  c1 := m_objControlLHS;
  c2 := m_objControlRHS;

  if (c1 <> nil) and (c2 <> nil) then
  begin
    if (m_iPreviousL1 <> kiUNDEFINED) and (m_iPreviousL2 <> kiUNDEFINED) then
    begin
      if m_iPreviousMode = 1 then
        SetLine(0, m_iPreviousL1, width, m_iPreviousL2, 1, clBtnFace)
      else
        SetLine(m_iPreviousL1, 0, m_iPreviousL2, Height, 2, clBtnFace)
    end;
    //
    // Side to Side
    if (c2.Left >= (c1.Left + c1.Width)) or (c1.Left >= (c2.Left + c2.Width)) then
    begin
      // Component Size
      Top := min(c1.Top, c2.Top);
      Height := max(c1.Top + c1.Height, c2.Top + c2.Height) - Top;
      Left := min(c1.Left + c1.Width, c2.Left + c2.Width);
      Width := max(c2.Left - (c1.Left + c1.Width), c1.Left - (c2.Left + c2.Width));
      // Origin computation
      L1 := 0;
      if c1 is TvPanel then
        L1 := (c1 as TvPanel).Blub(m_iLHS) + (c1.Top - Top);
      L2 := 0;
      if c2 is TvPanel then
        L2 := (c2 as TvPanel).Blub(m_iRHS) + (c2.Top - Top);
      // Line Drawing
      if c1.Left < c2.Left then
        SetLine(0, L1, Width, L2, 1, clBlack)
      else
        SetLine(0, L2, Width, L1, 1, clBlack);
    end
    else
    begin
      // Component Size
      Top := min(c1.Top + c1.Height, c2.Top + c2.Height);
      Height := max(c2.Top - (c1.Top + c1.Height), c1.Top - (c2.Top + c2.Height));
      Left := min(c1.Left, c2.Left);
      Width := max(c1.Left + c1.Width, c2.Left + c2.Width) - Left;
      // Origin computation
      L1 := (c1.Width div 2) + c1.Left - Left;
      L2 := (c2.Width div 2) + c2.Left - Left;
      // Line Drawing
      if c1.Top < c2.Top then
        SetLine(L1, 0, L2, Height, 2, clBlack)
      else
        SetLine(L2, 0, L1, Height, 2, clBlack);
    end;
  end;
end;

end.






















