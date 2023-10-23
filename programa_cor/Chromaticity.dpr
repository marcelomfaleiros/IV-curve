program Chromaticity;

uses
  Forms,
  ScreenChromaticity in 'ScreenChromaticity.pas' {FormChromaticity},
  ColorMatchingFunctions in 'ColorMatchingFunctions.PAS',
  MapWorldToPixel in 'MapWorldToPixel.pas',
  XYZtoRGBconversion in 'XYZtoRGBconversion.pas',
  ScreenCIEWrite in 'ScreenCIEWrite.pas' {FormWriteCIEFile},
  CIEChromaticity in 'CIEChromaticity.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormChromaticity, FormChromaticity);
  Application.CreateForm(TFormWriteCIEFile, FormWriteCIEFile);
  Application.Run;
end.
