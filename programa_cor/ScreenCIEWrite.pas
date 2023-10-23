unit ScreenCIEWrite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, ExtDlgs;

type
  TFormWriteCIEFile = class(TForm)
    SpinEditCIESize: TSpinEdit;
    LabelPixelsPerSide: TLabel;
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    ComboBoxCIE: TComboBox;
    SavePictureDialog: TSavePictureDialog;
    procedure BitBtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWriteCIEFile: TFormWriteCIEFile;

implementation
{$R *.DFM}

  uses
    ScreenChromaticity,       // FormChromaticity
    ColorMatchingFunctions,   // TReal
    JPEG,                     // TJPEGImage
    CIEChromaticity;          // CIE1931xyChart

procedure TFormWriteCIEFile.BitBtnOKClick(Sender: TObject);
  VAR
    Bitmap   :  TBitmap;
    Extension:  STRING;
    Gamma    :  TReal;
    JPEGImage:  TJPEGImage;
    Margin   :  INTEGER;
    Title    :  STRING;
begin
  CASE ComboBoxCIE.ItemIndex OF
    0:  BEGIN
          SavePictureDialog.FileName := 'CIE1931';
          Title := 'CIE 1931 Chromaticity Diagram'
        END;

    1:  BEGIN
          SavePictureDialog.FileName := 'CIE1960';
          Title := 'CIE 1960 Chromaticity Diagram'
        END;

    2:  BEGIN
          SavePictureDialog.FileName := 'CIE1976';
          Title := 'CIE 1976 Chromaticity Diagram'
        END;

    ELSE
      SavePictureDialog.FileName := '';
      Title := ''
  END;

  IF   SavePictureDialog.Execute
  THEN BEGIN
    TRY
      Gamma := StrToFloat(FormChromaticity.MaskEditGamma.Text)
    EXCEPT
      ON E: EConvertError DO
        Gamma := 1.0;
    END;

    Screen.Cursor := crHourGlass;
    TRY
      CASE ComboBoxCIE.ItemIndex OF
       0:  Bitmap := CIE1931xyChart(
                          FormChromaticity.CIEStandardObserver,
                          FormChromaticity.ColorSystem,
                          Gamma,
                          clBtnFace,
                          SpinEditCIESize.Value,
                          NOT FormChromaticity.CheckBoxMonitorGamut.Checked,
                          FormChromaticity.CheckBoxGridLines.Checked,
                          FormChromaticity.CheckBoxWavelengthValues.Checked);

       1:  Bitmap := CIEuvChart(
                          CIEChart1960,
                          FormChromaticity.CIEStandardObserver,
                          FormChromaticity.ColorSystem,
                          Gamma,
                          clBtnFace,
                          SpinEditCIESize.Value,
                          NOT FormChromaticity.CheckBoxMonitorGamut.Checked,
                          FormChromaticity.CheckBoxGridLines.Checked,
                          FormChromaticity.CheckBoxWavelengthValues.Checked);

       2:  Bitmap := CIEuvChart(
                          CIEChart1976,
                          FormChromaticity.CIEStandardObserver,
                          FormChromaticity.ColorSystem,
                          Gamma,
                          clBtnFace,
                          SpinEditCIESize.Value,
                          NOT FormChromaticity.CheckBoxMonitorGamut.Checked,
                          FormChromaticity.CheckBoxGridLines.Checked,
                          FormChromaticity.CheckBoxWavelengthValues.Checked);

        ELSE
          Bitmap := NIL   // should never happen; avoid compiler warning
      END;

      // Put "Chromaticity Diagram" Label on Bitmap
      Margin := MulDiv(Bitmap.Height, 1, 100);
      Bitmap.Canvas.Font.Name := 'Arial';
      Bitmap.Canvas.Font.Height := MulDiv(Bitmap.Height, 4, 100);
      Bitmap.Canvas.Font.Color := clWhite;
      Bitmap.Canvas.TextOut(Bitmap.Width -    // upper-right corner
                            Bitmap.Canvas.TextWidth(Title) - Margin,
                            Margin,
                            Title);

      // Show "Color System" only when gamut is displayed
      IF   FormChromaticity.CheckBoxMonitorGamut.Checked
      THEN BEGIN
        Title := FormChromaticity.ComboBoxColorSystem.Text + ' Gamut';
        Bitmap.Canvas.TextOut(Bitmap.Width -    // upper-right corner
                              Bitmap.Canvas.TextWidth(Title) - Margin,
                              Margin +
                              Bitmap.Canvas.TextHeight('X'),
                              Title)
      END;

      IF   FormChromaticity.RadioGroupStandardObserver.ItemIndex = 0
      THEN Title := '1931 2-degree Observer'
      ELSE Title := '1964 10-degree Observer';
      Bitmap.Canvas.TextOut(Bitmap.Width -    // lower-right corner
                            Bitmap.Canvas.TextWidth(Title) - 4*Margin,
                            Bitmap.Height -
                            5*Bitmap.Canvas.TextHeight(Title) - Margin,
                            Title);

      // Plug web site
      Title := 'efg''s Computer Lab';
      Bitmap.Canvas.Font.Color := clBlue;
      Bitmap.Canvas.Font.Style := [fsItalic];
      Bitmap.Canvas.TextOut(Bitmap.Width -    // lower-right corner
                            Bitmap.Canvas.TextWidth(Title) - 8*Margin,
                            Bitmap.Height -
                            3*Bitmap.Canvas.TextHeight(Title) - Margin,
                            Title);

      Title := 'www.efg2.com/lab';
      Bitmap.Canvas.Font.Style := [];
      Bitmap.Canvas.TextOut(Bitmap.Width -    // lower-right corner
                            Bitmap.Canvas.TextWidth(Title) - 8*Margin,
                            Bitmap.Height -
                            2*Bitmap.Canvas.TextHeight(Title) - Margin,
                            Title);

      // Write to Disk
      TRY
        Extension := UpperCase( ExtractFileExt(SavePictureDialog.Filename) );
        IF   Extension = '.BMP'
        THEN Bitmap.SaveToFile(SavePictureDialog.Filename)
        ELSE BEGIN
          IF   Extension = '.JPG'
          THEN BEGIN
            JPEGImage := TJPEGImage.Create;
            TRY
              JPEGImage.CompressionQuality := 80;  // keep high because of text
              JPEGImage.Assign(Bitmap);
              JPEGImage.SaveToFile(SavePictureDialog.Filename)
            FINALLY
              JPEGImage.Free
            END
          END
          ELSE ShowMessage('Cannot write "' + Extension + '" file type.')
        END
      FINALLY
        Bitmap.Free
      END
    FINALLY
      Screen.Cursor := crDefault
    END
  END
end;

end.
