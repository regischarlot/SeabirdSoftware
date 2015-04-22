unit PreferencePanel;

interface

uses
  Windows,
  Classes,
  Controls,
  ExtCtrls,
  Buttons,
  Graphics;

type
  TeColorMode = (ectColor, ectFont);
  TcCanvasPanel = class(TCustomPanel);

  TPreferencePanel = class(TCustomPanel)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, Inc.
  *
  * Description:
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 05/13/01 Regis Created
  ******************************************************************************}
  private
    // Private members
    //
    FButton: TSpeedButton;
    FColor: TColor;
    FImage: TImage;
    FMode: TeColorMode;
    FFont: TFont;
    FCaption: TCaption;

  private
    // Private Methods
    //
    procedure   SetColor(value: TColor);
    function    GetColor: TColor;
    procedure   FPanelClick(Sender: TObject);
    procedure   SetImage(value: TImage);
    procedure   SetMode(value: TeColorMode);
    procedure   SetFont(value: TFont);
    procedure   PanelResize(Sender: TObject);
    function    GetPanelPos: longint;

  protected
    // Protected Methods
    //
    procedure   Paint; override;

  public
    // Public Methods
    //
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

  published
    // Public properties
    //
    property    Top;
    property    Left;
    property    Height;
    property    Width;
    property    Color: TColor             read GetColor          write SetColor;
    property    Image: TImage             read FImage            write SetImage;
    property    Mode: TeColorMode         read FMode             write SetMode;
    property    Font: TFont               read FFont             write SetFont;
    property    Caption: TCaption         read FCaption          write FCaption;
  end;

procedure Register;

implementation

uses
  Math,
  sysUtils,
  Dialogs;

procedure Register;
begin
  RegisterComponents('Seabird', [TPreferencePanel]);
end;  {Register}

const
  ksEMPTY = '';
  kiPANELWIDTH = 100;

//
// TPreferencePanel
//

// TPreferencePanel
//   Create
//
constructor TPreferencePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := nil;
  FFont := TFont.Create;
  BevelOuter := bvNone;
  Width := 150;
  Height := 24;
  OnClick := FPanelClick;
  //BevelOuter := bvRaised;
  Caption := ksEMPTY;
  onResize := PanelResize;
  // Button
  FButton := TSpeedButton.Create(self);
  FButton.parent := self;
  FButton.Flat := TRUE;
  FButton.OnClick := FPanelClick;
  PanelResize(nil);
  FMode := ectColor;
  FCaption := 'caption';
  SetColor(clBlack);
end;

// TPreferencePanel
//   Destroy
//
destructor TPreferencePanel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

// TPreferencePanel
//   PanelResize
//
procedure TPreferencePanel.PanelResize(Sender: TObject);
begin
  // Button
  FButton.Left := max(Width - Height, 0);
  FButton.Top := 0;
  FButton.Height := Height;
  FButton.Width := Height;
  //
end;

// TPreferencePanel
//   FPanelClick
//
procedure TPreferencePanel.FPanelClick(Sender: TObject);
var
  p: TCommonDialog;
begin
  p := nil;
  case FMode of
    ectColor:
    try
      p := TColorDialog.Create(self);
      (p as TColorDialog).Color := FColor;
      if (p as TColorDialog).Execute then
        SetColor((p as TColorDialog).Color);
    finally
      p.free;
    end;
    ectFont:
    try
      p := TFontDialog.Create(self);
      (p as TFontDialog).Font.Assign(FFont);
      if (p as TFontDialog).Execute then
        SetFont((p as TFontDialog).Font);
    finally
      p.free;
    end;
  end;
end;

// TPreferencePanel
//   GetColor
//
function TPreferencePanel.GetColor: TColor;
begin
  result := FColor;
end;

// TPreferencePanel
//   SetImage
//
procedure TPreferencePanel.SetImage(value: TImage);
begin
  FImage := value;
  if (value <> nil) and (value.Picture <> nil) then
    FButton.Glyph.Assign(value.Picture.Bitmap);
end;

// TPreferencePanel
//   SetMode
//
procedure TPreferencePanel.SetMode(value: TeColorMode);
begin
  FMode := value;
  case FMode of
    ectColor:
      SetColor(FColor);
    ectFont:
      SetFont(FFont);
  end;
end;

// TPreferencePanel
//   SetColor
//
procedure TPreferencePanel.SetColor(value: TColor);
begin
  FColor := value;
  Invalidate;
end;

// TPreferencePanel
//   Paint
//
procedure TPreferencePanel.Paint;
var
  L, H: longint;
  r1, r2: TRect;
  bc: TColor;
begin
  inherited Paint;
  if (FCaption <> ksEMPTY) and (GetPanelPos > 0) then
  begin
    bc := Canvas.Brush.Color;
    Canvas.Brush.Color := inherited Color;
    H := Canvas.TextHeight(FCaption);
    L := min(Canvas.TextWidth(FCaption), GetPanelPos);
    r1 := Rect(0, (Height - H) div 2, GetPanelPos, (Height + H) div 2);
    r2 := Rect(GetPanelPos, 1, GetPanelPos + kiPANELWIDTH, Height - 2);
    //
    if GetPanelPos > 0 then
    begin
      Canvas.TextRect(r1, 2, r1.top, FCaption);
      Canvas.Pen.Color := clGray;
      Canvas.Pen.Style := psDot;
      Canvas.MoveTo(L + 6, r1.top + Canvas.TextHeight(FCaption) - 2);
      Canvas.LineTo(r2.Left - 6, r1.top + Canvas.TextHeight(FCaption) - 2);
    end;
    //
    case FMode of
      ectColor:
      begin
        Canvas.Brush.Color := FColor;
        Canvas.FillRect(r2);
      end;
      ectFont:
      begin
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(r2);
        Canvas.Pen.Color := clBlack;
        Canvas.Font.Assign(FFont);
        Canvas.TextRect(r2, r2.Left + 3, r2.Top + 1, FFont.Name);
      end;
    end;
    Canvas.Brush.Color := clGray;
    Canvas.FrameRect(r2);

    Canvas.Brush.Color := bc;
  end;
end;

// TPreferencePanel
//   SetFont
//
procedure TPreferencePanel.SetFont(value: TFont);
begin
  FFont.Assign(value);
  FColor := clWindow;
  Invalidate;
end;

// TPreferencePanel
//   GetPanelPos
//
function TPreferencePanel.GetPanelPos: longint;
begin
  result := max(0, width - (kiPANELWIDTH + Height));
end;

end.


