unit CustomControls;

interface

uses
  Windows,
  Classes,
  Dialogs,
  Graphics,
  Controls,
  stdCtrls,
  extCtrls;

type

  //
  // 1. Preference Object: TDisplayPreference
  //
  TDisplayType = (dtValue, dtColor, dtFont);

  TDisplayPreference = class(TCustomPanel)
 {******************************************************************************
  * Author: Regis Charlot
  *         Seabird Software, Inc.
  *
  * Description: the TDisplayPreference component displays a label, and either
  *              a 'test' font area or a color area, as well as a button. Clicking
  *              on the display area or the button brings up a font/color dialog.
  *              The DisplayType property is the driving factor.
  *
  * Inheritance: TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/06/01 Regis Created (Bitter End Yatch Club)
  ******************************************************************************}
  private
    // Private members
    //
    FLabel: TLabel;
    FPanel: TPanel;
    FColor: TColor;
    FDefaultColor: TColor;
    FFont: TFont;
    FCaption: String;
    FDisplayType: TDisplayType;

  private
    // Private Methods
    //
    procedure FPanelClick(Sender: TObject);
    procedure SetCaption(value: String);
    procedure SetColor(value: TColor);
    procedure SetDefaultColor(value: TColor);
    procedure SetFont(value: TFont);
    procedure SetDisplayType(value: TDisplayType);

  public
    // Public Methods
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    // Public properties
    //
    property Top;
    property Left;
    property Height;
    property Width;
    property Color: TColor             read FColor            write SetColor;
    property Font: TFont               read FFont             write SetFont;
    property Caption: String           read FCaption          write SetCaption;
    property DisplayType: TDisplayType read FDisplayType      write SetDisplayType;
    property DefaultColor: TColor      read FDefaultColor     write SetDefaultColor   default clWhite;
  end;

implementation

uses
  sysUtils,
  daResourceStrings;

constructor TDisplayPreference.Create(AOwner: TComponent);
var
  b: TButton;
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Width := 354;
  Height := 22;
  FCaption := 'Caption';
  FColor := clPurple;
  FFont := TFont.Create;
  BorderWidth := 1;
  // Label
  FLabel := TLabel.Create(self);
  FLabel.Parent := self;
  FLabel.Caption := FCaption;
  FLabel.AutoSize := False;
  FLabel.Left := 10;
  FLabel.Top := 4;
  FLabel.Height := 14;
  FLabel.Width := 200;
  FLabel.Anchors := [akTop, akLeft, akRight];
  // Font panel
  FPanel := TPanel.Create(self);
  FPanel.parent := self;
  FPanel.Left := 230;
  FPanel.Top := 1;
  FPanel.Height := 20;
  FPanel.Width := 100;
  FPanel.Anchors := [akTop, akRight];
  FPanel.BevelOuter := bvLowered;
  FPanel.Alignment := taLeftJustify;
  FPanel.onClick := FPanelClick;
  FPanel.BorderWidth := 2;
  // Button
  b := TButton.Create(self);
  b.Parent := self;
  b.Left := 332;
  b.Top := 1;
  b.Height := 20;
  b.Width := 20;
  b.Anchors := [akTop, akRight];
  b.CAption := '...';
  b.onClick := FPanelClick;
  //
  SetDisplayType(dtColor);
end;

destructor TDisplayPreference.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TDisplayPreference.FPanelClick(Sender: TObject);
var
  p: TCommonDialog;
begin
  p := nil;
  try
    case FDisplayType of
      dtColor:
        begin
          p := TColorDialog.Create(self);
          (p as TColorDialog).Color := FColor;
          if (p as TColorDialog).Execute then
            SetColor((p as TColorDialog).Color);
        end;
      dtFont:
        begin
          p := TFontDialog.Create(self);
          (p as TFontDialog).Font.Assign(FFont);
          if (p as TFontDialog).Execute then
            SetFont((p as TFontDialog).Font);
        end;
    end;
  finally
    p.free;
  end;
end;

procedure TDisplayPreference.SetCaption(value: String);
begin
  FCaption := value;
  FLabel.Caption := FCaption;
end;

procedure TDisplayPreference.SetColor(value: TColor);
begin
  FColor := value;
  if FDisplayType = dtColor then
    FPanel.Color := value;
end;

procedure TDisplayPreference.SetDefaultColor(value: TColor);
begin
  FDefaultColor := value;
end;

procedure TDisplayPreference.SetFont(value: TFont);
begin
  FFont.Assign(value);
  if FDisplayType = dtFont then
    FPanel.Font.Assign(value);
end;

procedure TDisplayPreference.SetDisplayType(value: TDisplayType);
begin
  FDisplayType := value;
  case FDisplayType of
    dtColor:
      begin
        FPanel.Caption := '';
        FPanel.Color := FColor;
      end;
    dtFont:
      begin
        FPanel.Caption := 'Test';
        FPanel.Color := clBtnFace;
      end;
  end;
end;

end.


