unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LZRoundedPanel,
  LZRoundedButton;

type

  { TfrmDemo }

  TfrmDemo = class(TForm)
    LZRoundedButton1: TLZRoundedButton;
    LZRoundedButton2: TLZRoundedButton;
    LZRoundedButton3: TLZRoundedButton;
    LZRoundedPanel1: TLZRoundedPanel;
    procedure LZRoundedButton1Click(Sender: TObject);
    procedure LZRoundedButton2Click(Sender: TObject);
    procedure LZRoundedButton3Click(Sender: TObject);
  private

  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.lfm}

{ TfrmDemo }

procedure TfrmDemo.LZRoundedButton3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmDemo.LZRoundedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmDemo.LZRoundedButton2Click(Sender: TObject);
begin
  ShowMessage(':)');
end;

end.

