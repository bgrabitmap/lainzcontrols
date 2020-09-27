{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LainzControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  LZRoundedImage, LZRoundedPanel, LZRoundedButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LZRoundedImage', @LZRoundedImage.Register);
  RegisterUnit('LZRoundedPanel', @LZRoundedPanel.Register);
  RegisterUnit('LZRoundedButton', @LZRoundedButton.Register);
end;

initialization
  RegisterPackage('LainzControls', @Register);
end.
