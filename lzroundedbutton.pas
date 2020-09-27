{
  LZRoundedButton
  by Lainz

  Last modified: 2020-09-06 20:45 GMT-3

  Changelog:
  - 2020-09-06: Initial version with color and style properties.
}
unit LZRoundedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TLZRoundedButton = class;
  TLZRoundedButtonStyle = class;
  TLZRoundedButtonPropertyChange = procedure(Sender: TLZRoundedButtonStyle) of object;
  TLZRoundedButtonState = (bsNormal, bsHover, bsActive, bsDisabled);

  { TLZRoundedButtonStyle }

  TLZRoundedButtonStyle = class(TPersistent)
  private
    FBorderStyle: TRoundRectangleOptions;
    FBorderWidth: single;
    FControl: TLZRoundedButton;
    FFillColor: TColor;
    FOnChange: TLZRoundedButtonPropertyChange;
    FPenColor: TColor;
    FRounding: single;
    procedure SetBorderWidth(AValue: single);
    procedure SetCustomBorderStyle(AValue: TRoundRectangleOptions);
    procedure SetFillColor(AValue: TColor);
    procedure SetPenColor(AValue: TColor);
    procedure SetRounding(AValue: single);
  public
    constructor Create(AControl: TLZRoundedButton);
    // The owner button
    property Control: TLZRoundedButton read FControl;
    // The change style event
    property OnChange: TLZRoundedButtonPropertyChange read FOnChange write FOnChange;
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
  end;

  { TLZRoundedButton }

  TLZRoundedButton = class(TGraphicControl)
  private
    FBGRA: TBGRABitmap;
    FModalResult: TModalResult;
    FState: TLZRoundedButtonState;
    FStyleActive: TLZRoundedButtonStyle;
    FStyleDisabled: TLZRoundedButtonStyle;
    FStyleHover: TLZRoundedButtonStyle;
    FStyleNormal: TLZRoundedButtonStyle;
    procedure OnChangeStyle(Sender: TLZRoundedButtonStyle);
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove({%H-}x, {%H-}y: integer); virtual;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
    property State: TLZRoundedButtonState read FState;
    property StyleNormal: TLZRoundedButtonStyle read FStyleNormal write FStyleNormal;
    property StyleHover: TLZRoundedButtonStyle read FStyleHover write FStyleHover;
    property StyleActive: TLZRoundedButtonStyle read FStyleActive write FStyleActive;
    property StyleDisabled: TLZRoundedButtonStyle
      read FStyleDisabled write FStyleDisabled;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lainz Controls', [TLZRoundedButton]);
end;

{ TLZRoundedButtonStyle }

procedure TLZRoundedButtonStyle.SetBorderWidth(AValue: single);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLZRoundedButtonStyle.SetCustomBorderStyle(AValue: TRoundRectangleOptions);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLZRoundedButtonStyle.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLZRoundedButtonStyle.SetPenColor(AValue: TColor);
begin
  if FPenColor = AValue then
    Exit;
  FPenColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLZRoundedButtonStyle.SetRounding(AValue: single);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TLZRoundedButtonStyle.Create(AControl: TLZRoundedButton);
begin
  inherited Create;
  FControl := AControl;
  FPenColor := clSilver;
  FFillColor := clWhite;
  FRounding := 10;
  FBorderWidth := 1;
end;

{ TLZRoundedButton }

procedure TLZRoundedButton.OnChangeStyle(Sender: TLZRoundedButtonStyle);
begin
  Invalidate;
end;

procedure TLZRoundedButton.DoClick;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
end;

procedure TLZRoundedButton.DoMouseDown;
begin
  FState := bsActive;
  Invalidate;
end;

procedure TLZRoundedButton.DoMouseUp;
var
  NewState: TLZRoundedButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := bsHover
  else
    NewState := bsNormal;

  FState := NewState;

  Invalidate;
end;

procedure TLZRoundedButton.DoMouseEnter;
begin
  FState := bsHover;
  Invalidate;
end;

procedure TLZRoundedButton.DoMouseLeave;
begin
  FState := bsNormal;
  Invalidate;
end;

procedure TLZRoundedButton.DoMouseMove(x, y: integer);
begin
  inherited;
end;

procedure TLZRoundedButton.Paint;
var
  textStyle: TTextStyle;
begin
  inherited Paint;
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height) then
    FBGRA.SetSize(ClientWidth, ClientHeight);
  FBGRA.FillTransparent;
  // Colors, BorderWidth, Fill and Pen color, BorderStyle
  case FState of
    bsNormal: FBGRA.RoundRectAntialias(trunc(FStyleNormal.BorderWidth / 2),
        trunc(FStyleNormal.FBorderWidth / 2),
        trunc(Width - 1 - (FStyleNormal.FBorderWidth / 2)), trunc(Height - 1 -
        (FStyleNormal.FBorderWidth / 2)),
        FStyleNormal.Rounding, FStyleNormal.Rounding,
        FStyleNormal.PenColor, FStyleNormal.BorderWidth, FStyleNormal.FillColor,
        FStyleNormal.BorderStyle);

    bsHover: FBGRA.RoundRectAntialias(trunc(FStyleHover.BorderWidth / 2),
        trunc(FStyleHover.FBorderWidth / 2),
        trunc(Width - 1 - (FStyleHover.FBorderWidth / 2)), trunc(Height - 1 -
        (FStyleHover.FBorderWidth / 2)),
        FStyleHover.Rounding, FStyleHover.Rounding,
        FStyleHover.PenColor, FStyleHover.BorderWidth, FStyleHover.FillColor,
        FStyleHover.BorderStyle);

    bsActive: FBGRA.RoundRectAntialias(trunc(FStyleActive.BorderWidth /
        2), trunc(FStyleActive.FBorderWidth / 2),
        trunc(Width - 1 - (FStyleActive.FBorderWidth / 2)), trunc(Height - 1 -
        (FStyleActive.FBorderWidth / 2)),
        FStyleActive.Rounding, FStyleActive.Rounding,
        FStyleActive.PenColor, FStyleActive.BorderWidth, FStyleActive.FillColor,
        FStyleActive.BorderStyle);

    bsDisabled: FBGRA.RoundRectAntialias(trunc(FStyleDisabled.BorderWidth /
        2), trunc(FStyleDisabled.FBorderWidth / 2),
        trunc(Width - 1 - (FStyleDisabled.FBorderWidth / 2)), trunc(Height - 1 -
        (FStyleDisabled.FBorderWidth / 2)),
        FStyleDisabled.Rounding, FStyleDisabled.Rounding,
        FStyleDisabled.PenColor, FStyleDisabled.BorderWidth, FStyleDisabled.FillColor,
        FStyleDisabled.BorderStyle);
  end;

  FBGRA.Draw(Canvas, 0, 0, False);
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

procedure TLZRoundedButton.Click;
begin
  DoClick;
  inherited Click;
end;

procedure TLZRoundedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    DoMouseDown;
end;

procedure TLZRoundedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp;
end;

procedure TLZRoundedButton.MouseEnter;
begin
  inherited MouseEnter;
  DoMouseEnter;
end;

procedure TLZRoundedButton.MouseLeave;
begin
  inherited MouseLeave;
  DoMouseLeave;
end;

procedure TLZRoundedButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  DoMouseMove(X, Y);
end;

procedure TLZRoundedButton.SetEnabled(Value: boolean);
begin
  if Value then
    FState := bsNormal
  else
    FState := bsDisabled;
  inherited SetEnabled(Value);
end;

procedure TLZRoundedButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

constructor TLZRoundedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := False;
  FBGRA := TBGRABitmap.Create(Width, Height);
  FStyleNormal := TLZRoundedButtonStyle.Create(Self);
  FStyleNormal.OnChange := @OnChangeStyle;
  FStyleHover := TLZRoundedButtonStyle.Create(Self);
  FStyleHover.OnChange := @OnChangeStyle;
  FStyleActive := TLZRoundedButtonStyle.Create(Self);
  FStyleActive.OnChange := @OnChangeStyle;
  FStyleDisabled := TLZRoundedButtonStyle.Create(Self);
  FStyleDisabled.OnChange := @OnChangeStyle;
  FStyleHover.PenColor := $00C08000;
  FStyleActive.PenColor := $00C08000;
  FStyleActive.FillColor := $00DDDDDD;
  FStyleDisabled.PenColor := clGray;
  FStyleDisabled.FillColor := clSilver;
end;

destructor TLZRoundedButton.Destroy;
begin
  FBGRA.Free;
  FStyleNormal.Free;
  FStyleHover.Free;
  FStyleActive.Free;
  FStyleDisabled.Free;
  inherited Destroy;
end;

end.
