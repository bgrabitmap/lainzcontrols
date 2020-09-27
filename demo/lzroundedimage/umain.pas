{
  LZRoundedImage Demo
  by Lainz

  Last modified: 2020-09-06 17:01 GMT-3

  Changelog:
  - 2020-09-06: Showcasing the component. Using all the style variations and the
                OnPaintEvent.
}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LZRoundedImage, BGRABitmap, BGRABitmapTypes;

type

  { TfrmDemo }

  TfrmDemo = class(TForm)
    LZRoundedImage1: TLZRoundedImage;
    LZRoundedImage2: TLZRoundedImage;
    LZRoundedImage3: TLZRoundedImage;
    procedure LZRoundedImage1Click(Sender: TObject);
    procedure LZRoundedImage1MouseEnter(Sender: TObject);
    procedure LZRoundedImage1MouseLeave(Sender: TObject);
    procedure LZRoundedImage1PaintEvent(const Sender: TLZRoundedImage;
      const Bitmap: TBGRABitmap);
  private
    hoveredImage: TLZRoundedImage;
  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.lfm}

{ TfrmDemo }

procedure TfrmDemo.LZRoundedImage1PaintEvent(const Sender: TLZRoundedImage;
  const Bitmap: TBGRABitmap);
begin
  // If the image is hovered
  if hoveredImage = Sender then
  begin
    // Add a white light to the entire image area
    Bitmap.Fill(BGRA(255, 255, 255, 100), dmDrawWithTransparency);
    // Draw a border
    Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, clGray);
  end;
end;

procedure TfrmDemo.LZRoundedImage1MouseEnter(Sender: TObject);
begin
  // Set hovered image
  hoveredImage := TLZRoundedImage(Sender);
  // Repaint form
  TLZRoundedImage(Sender).Invalidate;
end;

procedure TfrmDemo.LZRoundedImage1Click(Sender: TObject);
begin
  // Show the hint of the control
  ShowMessage(TLZRoundedImage(Sender).Hint);
end;

procedure TfrmDemo.LZRoundedImage1MouseLeave(Sender: TObject);
begin
  // Remove hovered image
  hoveredImage := nil;
  // Repaint form
  TLZRoundedImage(Sender).Invalidate;
end;

end.

