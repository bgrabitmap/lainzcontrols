{
  LZRoundedPanel Demo
  by Lainz

  Last modified: 2020-09-06 20:45 GMT-3

  Changelog:
  - 2020-09-06: Initial version.
}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LZRoundedPanel;

type

  { TfrmDemo }

  TfrmDemo = class(TForm)
    LZRoundedPanel1: TLZRoundedPanel;
    LZRoundedPanel2: TLZRoundedPanel;
    LZRoundedPanel3: TLZRoundedPanel;
    procedure LZRoundedPanel1Click(Sender: TObject);
    procedure LZRoundedPanel3Click(Sender: TObject);
  private

  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.lfm}

{ TfrmDemo }

procedure TfrmDemo.LZRoundedPanel1Click(Sender: TObject);
begin
  LZRoundedPanel1.BorderWidth := 5;
  LZRoundedPanel3.BorderWidth := 1;
  LZRoundedPanel2.Caption := 'Animals (also called Metazoa) are multicellular eukaryotic organisms that form the biological kingdom Animalia';
end;

procedure TfrmDemo.LZRoundedPanel3Click(Sender: TObject);
begin
  LZRoundedPanel3.BorderWidth := 5;
  LZRoundedPanel1.BorderWidth := 1;
  LZRoundedPanel2.Caption := 'Plants are mainly multicellular organisms predominantly photosynthetic eukaryotes of the kingdom Plantae';
end;

end.

