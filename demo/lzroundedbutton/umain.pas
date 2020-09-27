{
  LZRoundedButton Demo
  by Lainz

  Last modified: 2020-09-06 20:45 GMT-3

  Changelog:
  - 2020-09-06: Initial version.
}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LZRoundedButton;

type

  { TfrmDemo }

  TfrmDemo = class(TForm)
    LZRoundedButton1: TLZRoundedButton;
    LZRoundedButton2: TLZRoundedButton;
  private

  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.lfm}

end.

