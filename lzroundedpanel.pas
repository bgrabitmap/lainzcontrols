{
  LZRoundedPanel
  by Lainz

  Last modified: 2020-09-06 20:45 GMT-3

  Changelog:
  - 2020-09-06: Initial version with color and style properties and paint event.
}
unit LZRoundedPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, Math;

type
  TLZRoundedPanel = class;

  // Event to draw before the image is sent to canvas
  TLZRoundedImagePaintEvent = procedure(const Sender: TLZRoundedPanel;
    const Bitmap: TBGRABitmap) of object;

  // Control that draws a panel with a rounded border

  { TLZRoundedPanel }

  TLZRoundedPanel = class(TCustomControl)
  private
    FBorderWidth: single;
    FFillColor: TColor;
    FOnPaintEvent: TLZRoundedImagePaintEvent;
    FPenColor: TColor;
    FRounding: single;
    FBorderStyle: TRoundRectangleOptions;
    procedure SetBorderWidth(AValue: single);
    procedure SetFillColor(AValue: TColor);
    procedure SetPenColor(AValue: TColor);
    procedure SetRounding(AValue: single);
    procedure SetCustomBorderStyle(AValue: TRoundRectangleOptions);
  protected
    // The color of the canvas
    procedure SetColor(Value: TColor); override;
    // The caption
    procedure RealSetText(const AValue: TCaption); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    // Rounding of the borders
    property Rounding: single read FRounding write SetRounding;
    // Color of the border
    property PenColor: TColor read FPenColor write SetPenColor;
    // Color of the panel
    property FillColor: TColor read FFillColor write SetFillColor;
    // Thickness of the border
    property BorderWidth: single read FBorderWidth write SetBorderWidth;
    // The style of the rounded rectangle
    property BorderStyle: TRoundRectangleOptions
      read FBorderStyle write SetCustomBorderStyle;
    // You can paint before text is drawn and before the bitmap is drawn on canvas
    property OnPaintEvent: TLZRoundedImagePaintEvent
      read FOnPaintEvent write FOnPaintEvent;
  published
    property Caption;
    property Color;
    property Font;
    property Align;
    property Anchors;
    property ChildSizing;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lainz Controls', [TLZRoundedPanel]);
end;

{ TLZRoundedPanel }

procedure TLZRoundedPanel.SetRounding(AValue: single);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;
  Invalidate;
end;

procedure TLZRoundedPanel.SetCustomBorderStyle(AValue: TRoundRectangleOptions);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TLZRoundedPanel.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;
  Invalidate;
end;

procedure TLZRoundedPanel.SetBorderWidth(AValue: single);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;
  Invalidate;
end;

procedure TLZRoundedPanel.SetPenColor(AValue: TColor);
begin
  if FPenColor = AValue then
    Exit;
  FPenColor := AValue;
  Invalidate;
end;

procedure TLZRoundedPanel.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  Invalidate;
end;

procedure TLZRoundedPanel.RealSetText(const AValue: TCaption);
begin
  inherited RealSetText(AValue);
  Invalidate;
end;

constructor TLZRoundedPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csParentBackground];
  FPenColor := clSilver;
  FFillColor := clWhite;
  FRounding := 10;
  FBorderWidth := 1;
end;

procedure TLZRoundedPanel.Paint;
var
  bgra: TBGRABitmap;
  textStyle: TTextStyle;
  offset: single;
begin
  bgra := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  if (trunc(FBorderWidth) mod 2 = 0) and (frac(FBorderWidth)=0) then
    offset := 0.5
  else
    offset := 0;
  try
    // Colors, BorderWidth, Fill and Pen color, BorderStyle
    bgra.RoundRectAntialias(trunc(FBorderWidth / 2)-offset, trunc(FBorderWidth / 2)-offset, trunc(Width -
      (FBorderWidth / 2))-offset, trunc(Height - (FBorderWidth / 2))-offset, FRounding, FRounding,
      FPenColor, FBorderWidth, FFillColor, FBorderStyle);
    // OnPaintEvent
    if Assigned(FOnPaintEvent) then
      FOnPaintEvent(Self, bgra);
    bgra.Draw(Canvas, 0, 0, False);
  finally
    bgra.Free;
  end;
  // Caption
  if Caption <> '' then
  begin
    textStyle.Alignment := taCenter;
    textStyle.Layout := tlCenter;
    textStyle.Opaque := False;
    textStyle.SystemFont := False;
    textStyle.Wordbreak := True;
    textStyle.SingleLine := False;
    // Add some margins
    Canvas.TextRect(Rect(5, 5, Width - 5, Height - 5), 0, 0, Caption, textStyle);
  end;
end;

end.
