{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsSplitter v1.07                                                           }
{------------------------------------------------------------------------------}
{ A descendant of the TSplitter component (D3, C3, & D4) that adds a           }
{ "maximize - restore" button.  This mimics the behavior of the splitter in    }
{ Netscape Communicator v4.5.  Clicking the button moves the splitter to its   }
{ farthest extreme.  Clicking again returns it to the last position.           }
{                                                                              }
{ Copyright 1999, Brad Stowers.  All Rights Reserved.                          }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TdfsColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See DFSSplitter.txt for notes, known issues, and revision history.           }
{------------------------------------------------------------------------------}
{ Date last modified:  September 13, 1999                                      }
{------------------------------------------------------------------------------}

unit dfsSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsSplitter v1.07';
  MOVEMENT_TOLERANCE = 5; // See WMLButtonUp message handler.
  DEF_BUTTON_HIGHLIGHT_COLOR = $00FFCFCF; // RGB(207,207,255)

type
  TButtonWidthType = (btwPixels, btwPercentage);

  TdfsSplitter = class(TSplitter)
  private
    FShowButton: boolean;
    FButtonWidthType: TButtonWidthType;
    FButtonWidth: integer;
    FOnMaximize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FMaximized: boolean;
    // Internal use for "restoring" from "maximized" state
    FRestorePos: integer;
    // For internal use to avoid calling GetButtonRect when not necessary
    FLastKnownButtonRect: TRect;
    // Internal use to avoid unecessary painting
    FIsHighlighted: boolean;
    // Internal for detecting real clicks
    FGotMouseDown: boolean;
    FButtonColor: TColor;
    FButtonHighlightColor: TColor;
    FArrowColor: TColor;
    FTextureColor1: TColor;
    FTextureColor2: TColor;
    FAutoHighlightColor : boolean;
    procedure SetShowButton(const Value: boolean);
    procedure SetButtonWidthType(const Value: TButtonWidthType);
    procedure SetButtonWidth(const Value: integer);
    function GetButtonRect: TRect;
    procedure SetMaximized(const Value: boolean);
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    procedure SetArrowColor(const Value: TColor);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonHighlightColor(const Value: TColor);
    procedure SetTextureColor1(const Value: TColor);
    procedure SetTextureColor2(const Value: TColor);
    procedure SetAutoHighLightColor(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Val: string);
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMMouseEnter(var Msg: TWMMouse); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TWMMouse); message CM_MOUSELEAVE;
  protected
    // Internal use for moving splitter position with FindControl and
    // UpdateControlSize
    FControl: TControl;
    FDownPos: TPoint;

    procedure Paint; override;
    {$IFDEF DFS_COMPILER_4_UP}
    function DoCanResize(var NewSize: integer): boolean; override;
    {$ENDIF}
    procedure PaintButton(Highlight: boolean); dynamic;
    function DrawArrow(ACanvas: TCanvas; AvailableRect: TRect; Offset: integer;
       ArrowSize: integer; Color: TColor): integer; dynamic;
    function ButtonHitTest(X, Y: integer): boolean; dynamic;
    procedure DoMaximize; dynamic;
    procedure DoRestore; dynamic;
    procedure FindControl; dynamic;
    procedure UpdateControlSize(NewSize: integer); dynamic;
    function GrabBarColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;

    property ButtonRect: TRect
       read GetButtonRect;
    property Maximized: boolean
       read FMaximized
       write SetMaximized;
    property RestorePos: integer
       read FRestorePos
       write FRestorePos;
  published
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;
    property ButtonWidthType: TButtonWidthType
       read FButtonWidthType
       write SetButtonWidthType
       default btwPixels;
    property ButtonWidth: integer
       read FButtonWidth
       write SetButtonWidth
       default 100;
    property ShowButton: boolean
       read FShowButton
       write SetShowButton
       default TRUE;
    property ButtonColor: TColor
       read FButtonColor
       write SetButtonColor
       default clBtnFace;
    property ArrowColor: TColor
       read FArrowColor
       write SetArrowColor
       default clNavy;
    property ButtonHighlightColor: TColor
       read FButtonHighlightColor
       write SetButtonHighlightColor
       default DEF_BUTTON_HIGHLIGHT_COLOR;
    property AutoHighlightColor: Boolean
       read FAutoHighlightColor
       write SetAutoHighlightColor
       default FALSE;
    property TextureColor1: TColor
       read FTextureColor1
       write SetTextureColor1
       default clWhite;
    property TextureColor2: TColor
       read FTextureColor2
       write SetTextureColor2
       default clNavy;
    property Align: TAlign // Need to know when it changes to redraw arrows
       read GetAlign
       write SetAlign;
    property Width
       default 10;  // it looks best with 10
    property Beveled
       default FALSE; // it looks best without the bevel
    property Enabled;

    property OnMaximize: TNotifyEvent
       read FOnMaximize
       write FOnMaximize;
    property OnRestore: TNotifyEvent
       read FOnRestore
       write FOnRestore;
  end;

implementation

{ TdfsSplitter }

constructor TdfsSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Beveled := FALSE;
  FButtonWidthType := btwPixels;
  FButtonWidth := 100;
  FShowButton := TRUE;
  SetRectEmpty(FLastKnownButtonRect);
  FIsHighlighted := FALSE;
  FGotMouseDown := FALSE;
  FControl := NIL;
  FDownPos := Point(0,0);
  FMaximized := FALSE;
  FRestorePos := 0;
  Width := 10;
  FButtonColor := clBtnFace;
  FArrowColor := clNavy;
  FButtonHighlightColor := DEF_BUTTON_HIGHLIGHT_COLOR;
  FAutoHighLightColor := FALSE;
  FTextureColor1 := clWhite;
  FTextureColor2 := clNavy;
end;

function TdfsSplitter.GrabBarColor: TColor;
var
  BeginRGB: array[0..2] of Byte;
  RGBDifference: array[0..2] of integer;
  R,G,B: Byte;
  BeginColor,
  EndColor: TColor;
  NumberOfColors: integer;

begin
  //Need to figure out how many colors available at runtime
  NumberOfColors := 256;

  BeginColor := clActiveCaption;
  EndColor := clBtnFace;

  BeginRGB[0] := GetRValue(ColorToRGB(BeginColor));
  BeginRGB[1] := GetGValue(ColorToRGB(BeginColor));
  BeginRGB[2] := GetBValue(ColorToRGB(BeginColor));

  RGBDifference[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGB[0];
  RGBDifference[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGB[1];
  RGBDifference[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGB[2];

  R := BeginRGB[0] + MulDiv (180, RGBDifference[0], NumberOfColors - 1);
  G := BeginRGB[1] + MulDiv (180, RGBDifference[1], NumberOfColors - 1);
  B := BeginRGB[2] + MulDiv (180, RGBDifference[2], NumberOfColors - 1);

  GrabBarColor := RGB (R, G, B);
end;

function TdfsSplitter.DrawArrow(ACanvas: TCanvas; AvailableRect: TRect; Offset: integer;
   ArrowSize: integer; Color: TColor): integer;
var
  x, y, q, i, j: integer;
  ArrowAlign: TAlign;
begin
  // STB Nitro drivers have a LineTo bug, so I've opted to use the slower
  // SetPixel method to draw the arrows.

  if not Odd(ArrowSize) then
    Dec(ArrowSize);
  if ArrowSize < 1 then
    ArrowSize := 1;

  if FMaximized then
  begin
    case Align of
      alLeft:   ArrowAlign := alRight;
      alRight:  ArrowAlign := alLeft;
      alTop:    ArrowAlign := alBottom;
    else //alBottom
      ArrowAlign := alTop;
    end;
  end else
    ArrowAlign := Align;
  q := ArrowSize * 2 - 1 ;
  Result := q;
  ACanvas.Pen.Color := Color;
  with AvailableRect do
  begin
    case ArrowAlign of
      alLeft:
        begin
          x := Left + ((Right - Left - ArrowSize) div 2) + 1;
          if Offset < 0 then
            y := Bottom + Offset - q
          else
            y := Top + Offset;
          for j := x + ArrowSize - 1 downto x do
          begin
            for i := y to y + q - 1 do
              ACanvas.Pixels[j, i] := Color;
            inc(y);
            dec(q,2);
          end;
        end;
      alRight:
        begin
          x := Left + ((Right - Left - ArrowSize) div 2) + 1;
          if Offset < 0 then
            y := Bottom + Offset - q
          else
            y := Top + Offset;
          for j := x to x + ArrowSize - 1 do
          begin
            for i := y to y + q - 1 do
              ACanvas.Pixels[j, i] := Color;
            inc(y);
            dec(q,2);
          end;
        end;
      alTop:
        begin
          if Offset < 0 then
            x := Right + Offset - q
          else
            x := Left + Offset;
          y := Top + ((Bottom - Top - ArrowSize) div 2) + 1;
          for i := y + ArrowSize - 1 downto y do
          begin
            for j := x to x + q - 1 do
              ACanvas.Pixels[j, i] := Color;
            inc(x);
            dec(q,2);
          end;
        end;
    else // alBottom
      if Offset < 0 then
        x := Right + Offset - q
      else
        x := Left + Offset;
      y := Top + ((Bottom - Top - ArrowSize) div 2) + 1;
      for i := y to y + ArrowSize - 1 do
      begin
        for j := x to x + q - 1 do
          ACanvas.Pixels[j, i] := Color;
        inc(x);
        dec(q,2);
      end;
    end;
  end;
end;

function TdfsSplitter.GetButtonRect: TRect;
var
  BW: integer;
begin
  // Calc the rectangle the button goes in
  if ButtonWidthType = btwPercentage then
  begin
    if Align in [alLeft, alRight] then
      BW := ClientRect.Bottom - ClientRect.Top
    else
      BW := ClientRect.Right - ClientRect.Left;
    BW := MulDiv(BW, FButtonWidth, 100);
  end else
    BW := FButtonWidth;
  if BW = 0 then
    SetRectEmpty(Result)
  else begin
    Result := ClientRect;
    if Align in [alLeft, alRight] then
    begin
      Result.Top := (ClientRect.Bottom - ClientRect.Top - BW) div 2;
      Result.Bottom := Result.Top + BW;
      InflateRect(Result, -1, 0);
    end else begin
      Result.Left := (ClientRect.Right - ClientRect.Left - BW) div 2;
      Result.Right := Result.Left + BW;
      InflateRect(Result, 0, -1);
    end;
    if Result.Top < 1 then
      Result.Top := 1;
    if Result.Left < 1 then
      Result.Left := 1;
    if Result.Bottom >= ClientRect.Bottom then
      Result.Bottom := ClientRect.Bottom - 1;
    if Result.Right >= ClientRect.Right then
      Result.Right := ClientRect.Right - 1;
    // Make smaller if it's beveled
    if Beveled then
      if Align in [alLeft, alRight] then
        InflateRect(Result, -3, 0)
      else
        InflateRect(Result, 0, -3);
  end;
  FLastKnownButtonRect := Result;
end;

procedure TdfsSplitter.Paint;
begin
// Exclude button rect from update region here for less flicker.
  inherited Paint;

// Don't paint while being moved unless ResizeStyle = rsUpdate!!!
// Make rect smaller if Beveled is true.
  PaintButton(FIsHighlighted);
end;

{$IFDEF DFS_COMPILER_4_UP}
function TdfsSplitter.DoCanResize(var NewSize: integer): boolean;
begin
  Result := inherited DoCanResize(NewSize);
  // D4 version has a bug that causes it to not honor MinSize, which causes a
  // really nasty problem.
  if Result and (NewSize < MinSize) then
    NewSize := MinSize;
end;
{$ENDIF}

procedure TdfsSplitter.PaintButton(Highlight: boolean);
const
  TEXTURE_SIZE = 3;
var
  BtnRect: TRect;
  BW: integer;
  TextureBmp: TBitmap;
  x, y: integer;
  RW, RH: integer;
  OffscreenBmp: TBitmap;
begin
  if (not FShowButton) or (not Enabled) or (GetParentForm(Self) = NIL) then
    exit;

  if FAutoHighLightColor then
    FButtonHighlightColor:=GrabBarColor;
     
  BtnRect := ButtonRect; // So we don't repeatedly call GetButtonRect
  if IsRectEmpty(BtnRect) then
    exit; // nothing to draw

  OffscreenBmp := TBitmap.Create;
  try
    OffsetRect(BtnRect, -BtnRect.Left, -BtnRect.Top);
    OffscreenBmp.Width := BtnRect.Right;
    OffscreenBmp.Height := BtnRect.Bottom;

    // Draw basic button
    OffscreenBmp.Canvas.Brush.Color := clGray;
    OffscreenBmp.Canvas.FrameRect(BtnRect);
    InflateRect(BtnRect, -1, -1);

    OffscreenBmp.Canvas.Pen.Color := clWhite;
    with BtnRect, OffscreenBmp.Canvas do
    begin
      // This is not going to work with the STB bug.  Have to find workaround.
      MoveTo(Left, Bottom-1);
      LineTo(Left, Top);
      LineTo(Right, Top);
    end;
    Inc(BtnRect.Left);
    Inc(BtnRect.Top);

    if Highlight then
      OffscreenBmp.Canvas.Brush.Color := ButtonHighlightColor
    else
      OffscreenBmp.Canvas.Brush.Color := ButtonColor;
    OffscreenBmp.Canvas.FillRect(BtnRect);
    FIsHighlighted := Highlight;
    Dec(BtnRect.Right);
    Dec(BtnRect.Bottom);

    // Draw the insides of the button
    with BtnRect do
    begin
      // Draw the arrows
      if Align in [alLeft, alRight] then
      begin
        InflateRect(BtnRect, 0, -4);
        BW := BtnRect.Right - BtnRect.Left;
        DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, ArrowColor);
        BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, ArrowColor);
        InflateRect(BtnRect, 0, -(BW+4));
      end else begin
        InflateRect(BtnRect, -4, 0);
        BW := BtnRect.Bottom - BtnRect.Top;
        DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, ArrowColor);
        BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, ArrowColor);
        InflateRect(BtnRect, -(BW+4), 0);
      end;

      // Draw the texture
      // Note: This is so complex because I'm trying to make as much like the
      //       Netscape splitter as possible.  They use a 3x3 texture pattern, and
      //       that's harder to tile.  If the had used an 8x8 (or smaller
      //       divisibly, i.e. 2x2 or 4x4), I could have used Brush.Bitmap and
      //       FillRect and they whole thing would have been about half the size,
      //       twice as fast, and 1/10th as complex.
      RW := BtnRect.Right - BtnRect.Left;
      RH := BtnRect.Bottom - BtnRect.Top;
      if (RW >= TEXTURE_SIZE) and (RH >= TEXTURE_SIZE) then
      begin
        TextureBmp := TBitmap.Create;
        try
          with TextureBmp do
          begin
            Width := RW;
            Height := RH;
            // Draw first square
            Canvas.Brush.Color := OffscreenBmp.Canvas.Brush.Color;
            Canvas.FillRect(Rect(0, 0, RW+1, RH+1));
            Canvas.Pixels[1,1] := TextureColor1;
            Canvas.Pixels[2,2] := TextureColor2;

            // Tile first square all the way across
            for x := 1 to ((RW div TEXTURE_SIZE) + ord(RW mod TEXTURE_SIZE > 0)) do
            begin
              Canvas.CopyRect(Bounds(x * TEXTURE_SIZE, 0, TEXTURE_SIZE,
                 TEXTURE_SIZE), Canvas, Rect(0, 0, TEXTURE_SIZE, TEXTURE_SIZE));
            end;

            // Tile first row all the way down
            for y := 1 to ((RH div TEXTURE_SIZE) + ord(RH mod TEXTURE_SIZE > 0)) do
            begin
              Canvas.CopyRect(Bounds(0, y * TEXTURE_SIZE, RW, TEXTURE_SIZE),
                 Canvas, Rect(0, 0, RW, TEXTURE_SIZE));
            end;

            // Above could be better if it reversed process when splitter was
            // taller than it was wider.  Optimized only for horizontal right now.
          end;
          // Copy texture bitmap to the screen.
          OffscreenBmp.Canvas.CopyRect(BtnRect, TextureBmp.Canvas,
             Rect(0, 0, RW, RH));
        finally
          TextureBmp.Free;
        end;
      end;
    end;
(**)
    Canvas.CopyRect(ButtonRect, OffscreenBmp.Canvas, Rect(0, 0,
       OffscreenBmp.Width, OffscreenBmp.Height));
  finally
    OffscreenBmp.Free;
  end;
end;

procedure TdfsSplitter.SetButtonWidth(const Value: integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    if (FButtonWidthType = btwPercentage) and (FButtonWidth > 100) then
      FButtonWidth := 100;
    if FButtonWidth < 0 then
      FButtonWidth := 0;
    if FShowButton then
      Invalidate;
  end;
end;

procedure TdfsSplitter.SetButtonWidthType(const Value: TButtonWidthType);
begin
  if Value <> FButtonWidthType then
  begin
    FButtonWidthType := Value;
    if (FButtonWidthType = btwPercentage) and (FButtonWidth > 100) then
      FButtonWidth := 100;
    if FShowButton then
      Invalidate;
  end;
end;

procedure TdfsSplitter.SetShowButton(const Value: boolean);
begin
  if Value <> FShowButton then
  begin
    FShowButton := Value;
    SetRectEmpty(FLastKnownButtonRect);
    Invalidate;
  end;
end;

procedure TdfsSplitter.WMMouseMove(var Msg: TWMMouseMove);
begin
  inherited;

  if ButtonHitTest(Msg.XPos, Msg.YPos) then
  begin
    if not FIsHighlighted then
      PaintButton(TRUE)
  end else
    if FIsHighlighted then
      PaintButton(FALSE);
end;

procedure TdfsSplitter.CMMouseEnter(var Msg: TWMMouse);
var
  Pos: TPoint;
begin
  inherited;

  GetCursorPos(Pos); // CM_MOUSEENTER doesn't send mouse pos.
  Pos := Self.ScreenToClient(Pos);
  if ButtonHitTest(Pos.x, Pos.y) then
  begin
    if not FIsHighlighted then
      PaintButton(TRUE)
  end else
    if FIsHighlighted then
      PaintButton(FALSE);
end;

procedure TdfsSplitter.CMMouseLeave(var Msg: TWMMouse);
begin
  inherited;

  if FIsHighlighted then
    PaintButton(FALSE);

// ????  Might be a bug...
  FGotMouseDown := FALSE;
end;

procedure TdfsSplitter.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  inherited;

  if Enabled then
  begin
    FGotMouseDown := ButtonHitTest(Msg.XPos, Msg.YPos);
    if FGotMouseDown then
    begin
      FindControl;
      FDownPos := ClientToScreen(Point(Msg.XPos, Msg.YPos));
    end;
  end;
end;

procedure TdfsSplitter.WMLButtonUp(var Msg: TWMLButtonUp);
var
  CurPos: TPoint;
begin
  inherited;

  if FGotMouseDown and ButtonHitTest(Msg.XPos, Msg.YPos) then
  begin
    CurPos := ClientToScreen(Point(Msg.XPos, Msg.YPos));
    // More than a little movement is not a click, but a regular resize.
    if ((Align in [alLeft, alRight]) and
       (Abs(FDownPos.x - CurPos.X) <= MOVEMENT_TOLERANCE)) or
       ((Align in [alTop, alBottom]) and
       (Abs(FDownPos.y - CurPos.Y) <= MOVEMENT_TOLERANCE)) then
    begin
      StopSizing;
      Maximized := not Maximized;
    end
  end;
  FGotMouseDown := FALSE;
  Invalidate;
end;

function TdfsSplitter.ButtonHitTest(X, Y: integer): boolean;
begin
  // We use FLastKnownButtonRect here so that we don't have to recalculate the
  // button rect with GetButtonRect every time the mouse moved.  That would be
  // EXTREMELY inefficient.
  Result := PtInRect(FLastKnownButtonRect, Point(X, Y));
  if Align in [alLeft, alRight] then
  begin
    if (Y >= FLastKnownButtonRect.Top) and (Y <= FLastKnownButtonRect.Bottom) then
      Cursor := crArrow
    else
      Cursor := crHSplit;
  end else begin
    if (X >= FLastKnownButtonRect.Left) and (X <= FLastKnownButtonRect.Right) then
      Cursor := crArrow
    else
      Cursor := crVSplit;
  end;
end;

procedure TdfsSplitter.DoMaximize;
begin
  if assigned(FOnMaximize) then
    FOnMaximize(Self);
end;


procedure TdfsSplitter.DoRestore;
begin
  if assigned(FOnRestore) then
    FOnRestore(Self);
end;


procedure TdfsSplitter.SetMaximized(const Value: boolean);
begin
  if Value <> FMaximized then
  begin
    FindControl;
    if FControl = NIL then
      exit;

    FMaximized := Value;
    if FMaximized then
    begin
      case Align of
        alLeft,
        alRight:  FRestorePos := FControl.Width;
        alTop,
        alBottom: FRestorePos := FControl.Height;
      else
        exit;
      end;
      UpdateControlSize(MinSize);
      DoMaximize;
    end else begin
      UpdateControlSize(FRestorePos);
      DoRestore;
    end;
  end;
end;

function TdfsSplitter.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TdfsSplitter.SetAlign(Value: TAlign);
begin
  inherited Align := Value;

  Invalidate; // Direction changing, redraw arrows.
  {$IFNDEF DFS_COMPILER_4_UP}
  // D4 does this already
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
  {$ENDIF}
end;


procedure TdfsSplitter.FindControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  FControl := NIL;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    FControl := Parent.Controls[I];
    if FControl.Visible and FControl.Enabled then
    begin
      R := FControl.BoundsRect;
      if (R.Right - R.Left) = 0 then
        Dec(R.Left);
      if (R.Bottom - R.Top) = 0 then
        Dec(R.Top);
      if PtInRect(R, P) then
        Exit;
    end;
  end;
  FControl := NIL;
end;


procedure TdfsSplitter.UpdateControlSize(NewSize: integer);
begin
  if (FControl <> NIL) then
  begin
    case Align of
      alLeft: FControl.Width := NewSize;
      alTop: FControl.Height := NewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - NewSize);
            FControl.Width := NewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - NewSize);
            FControl.Height := NewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
  end;
end;

procedure TdfsSplitter.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Invalidate;
  end;
end;

procedure TdfsSplitter.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

procedure TdfsSplitter.SetButtonHighlightColor(const Value: TColor);
begin
  if FButtonHighlightColor <> Value then
  begin
    FButtonHighlightColor := Value;
    Invalidate;
  end;
end;

procedure TdfsSplitter.SetAutoHighlightColor(const Value: boolean);
begin
  if FAutoHighLightColor <> Value then
  begin
    FAutoHighLightColor := Value;
    if FAutoHighLightColor then
      FButtonHighLightColor := GrabBarColor
    else
      FButtonHighLightColor := DEF_BUTTON_HIGHLIGHT_COLOR;
    Invalidate;
  end;
end;

procedure TdfsSplitter.SetTextureColor1(const Value: TColor);
begin
  if FTextureColor1 <> Value then
  begin
    FTextureColor1 := Value;
    Invalidate;
  end;
end;

procedure TdfsSplitter.SetTextureColor2(const Value: TColor);
begin
  if FTextureColor2 <> Value then
  begin
    FTextureColor2 := Value;
    Invalidate;
  end;
end;

function TdfsSplitter.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsSplitter.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;


end.

