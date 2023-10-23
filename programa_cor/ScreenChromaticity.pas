unit ScreenChromaticity;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, StdCtrls, ComCtrls,
  ColorMatchingFunctions, Spin,
  XYZtoRGBConversion, Mask, ExtDlgs;   // TColorSystem, XYZtoRGB, Clamp

type
  TFormChromaticity = class(TForm)
    PageControlCharts: TPageControl;
    TabSheetStandardObserver: TTabSheet;
    TabSheetColorMatchingFunctions: TTabSheet;
    TabSheetCIE1931: TTabSheet;
    TabSheetCIE1960: TTabSheet;
    TabSheetCIE1976: TTabSheet;
    RadioGroupStandardObserver: TRadioGroup;
    ChartColorMatchingFunctions: TChart;
    Series_xBar: TLineSeries;
    Series_yBar: TLineSeries;
    Series_zBar: TLineSeries;
    ButtonPrintColorMatch: TButton;
    LabelViewingDistance: TLabel;
    SpinEditViewingDistanceMillimeters: TSpinEdit;
    LabelCalibrationData: TLabel;
    LabelScreenPixels: TLabel;
    SpinEditScreenDiagonalMillimeters: TSpinEdit;
    ImageStandardObserver: TImage;
    LabelFieldOfViewCircle: TLabel;
    LabelScreenMillimeters: TLabel;
    LabelCircleDiameter: TLabel;
    LabelScreenDiagonal: TLabel;
    LabelSquarePixels: TLabel;
    MemoStandardObserver: TMemo;
    LabelObserverNotes: TLabel;
    ImageChart1931: TImage;
    ImageChart1960: TImage;
    ImageChart1976: TImage;
    LabelLab1: TLabel;
    LabelLab2: TLabel;
    CheckBoxMonitorGamut: TCheckBox;
    ComboBoxColorSystem: TComboBox;
    LabelColorSystem: TLabel;
    SpinButtonGamma: TSpinButton;
    MaskEditGamma: TMaskEdit;
    LabelGamma: TLabel;
    CheckBoxWavelengthValues: TCheckBox;
    ButtonWrite1931: TButton;
    ButtonSaveColorMatch: TButton;
    SavePictureDialog: TSavePictureDialog;
    ButtonWrite1960: TButton;
    ButtonWrite1976: TButton;
    CheckBoxGridLines: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupStandardObserverClick(Sender: TObject);
    procedure ButtonPrintColorMatchClick(Sender: TObject);
    procedure SpinEditFieldOfViewUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChartColorMatchingFunctionsAfterDraw(Sender: TObject);
    procedure CheckBoxMonitorGamutClick(Sender: TObject);
    procedure ComboBoxColorSystemChange(Sender: TObject);
    procedure SpinButtonGammaUpClick(Sender: TObject);
    procedure SpinButtonGammaDownClick(Sender: TObject);
    procedure MaskEditGammaChange(Sender: TObject);
    procedure LabelLab2Click(Sender: TObject);
    procedure CheckBoxWavelengthValuesClick(Sender: TObject);
    procedure ButtonWrite1931Click(Sender: TObject);
    procedure ButtonSaveColorMatchClick(Sender: TObject);
    procedure PageControlChartsChange(Sender: TObject);
  private
    PROCEDURE UpdateEverything;
    PROCEDURE ShowStandardObserverFieldOfView (CONST UpdateNotes:  BOOLEAN);
    PROCEDURE PlotColorMatchingFunctions;
    PROCEDURE PlotChromaticityCharts (CONST ShowFullChart:  BOOLEAN);
  public
    CIEStandardObserver:  TCIEStandardObserver;
    ColorSystem        :  TColorSystem;
  end;

var
  FormChromaticity: TFormChromaticity;

implementation
{$R *.DFM}

  USES
    IniFiles,           // TIniFile
    MapWorldToPixel,    // TRealRect, TSimplePantograph
    Math,               // Power, MaxValue, ArcTan2
    CIEChromaticity,    // CIE1931xyChart, CIEuvChart    
    ShellAPI,           // ShellExecute
{$IFNDEF VER100}        // Delphi 4 and later but not Delphi 3
    TeCanvas,           // TCanvas3D
{$ENDIF}
    ScreenCIEWrite;

  CONST
    KeywordObserver        = 'Observer';
    KeywordObserverIndex   = 'Index';
    KeywordViewingDistance = 'ViewingDistanceMillimeters';
    KeywordScreenDiagonal  = 'ScreenDiagonalMillimeters';


// ========================================================================

procedure TFormChromaticity.FormCreate(Sender: TObject);
  VAR
    Filename:  STRING;
    Index   :  INTEGER;
    IniFile :  TIniFile;
begin
  // Set active tabsheet in case not set correctly in design mode
  PageControlCharts.ActivePage := TabSheetCIE1931;

{$IFNDEF VER100}  // not D3
  // Needed to make D4 and later work the same way as D3
  ChartColorMatchingFunctions.BackColor := clWhite;
{$ENDIF}  

  ColorSystem := DellPhosphors;

  // Save new values
  Filename := ChangeFileExt(ParamStr(0), '.INI');
  IniFile := TIniFile.Create(Filename);
  TRY
    Index := IniFile.ReadInteger(KeywordObserver, KeywordObserverIndex, 0);
    IF   Index = 0
    THEN CIEStandardObserver := CIEStandardObserver1931
    ELSE CIEStandardObserver := CIEStandardObserver1964;

    RadioGroupStandardObserver.ItemIndex := Index;  // also triggers update

    SpinEditViewingDistanceMillimeters.Value :=
         IniFile.ReadInteger(KeywordObserver, KeywordViewingDistance, 450);

    SpinEditScreenDiagonalMillimeters.Value :=
         IniFile.ReadInteger(KeywordObserver, KeywordScreenDiagonal, 305);

  FINALLY
    IniFile.Free
  END;

  UpdateEverything
end;


PROCEDURE TFormChromaticity.UpdateEverything;
BEGIN
  ShowStandardObserverFieldOfView (TRUE {Update Notes});
  PlotColorMatchingFunctions;
  PlotChromaticityCharts (NOT CheckBoxMonitorGamut.Checked);
END {UpdateEverything};


PROCEDURE TFormChromaticity.ShowStandardObserverFieldOfView (CONST UpdateNotes:  BOOLEAN);
  VAR
    AngleDegrees         :  DOUBLE;
    AngleRadians         :  DOUBLE;
    Bitmap               :  TBitmap;
    DiagonalPixels       :  DOUBLE;
    DiagonalMillimeters  :  DOUBLE;
    DiameterMillimeters  :  DOUBLE;
    DiameterPixels       :  DOUBLE;
    iCenter              :  Integer;
    iLength              :  Integer;
    jCenter              :  Integer;
    LineLengthMillimeters:  INTEGER;
    PixelsPerMillimeter  :  DOUBLE;
    RadiusPixels         :  Integer;
    s                    :  STRING;
BEGIN
  TRY
    DiagonalMillimeters := SpinEditScreenDiagonalMillimeters.Value
    EXCEPT
      ON EConvertError DO
        DiagonalMillimeters := 0.0
  END;

  // Ignore unrealistic screen diagonal values.  This can only happen
  // during keyboard entry of a value.
  IF   DiagonalMillimeters <= SpinEditScreenDiagonalMillimeters.MinValue
  THEN BEGIN
    MessageBeep(0);
    LabelCircleDiameter.Caption := '';
    LabelScreenMillimeters.Caption := ''
  END
  ELSE BEGIN
    // Use UpdateNotes = FALSE to avoid flicker
    IF   UpdateNotes
    THEN MemoStandardObserver.Lines.Clear;

    AngleDegrees := 2.0;  // default to avoid warning

    CASE CIEStandardObserver OF
      CIEStandardObserver1931:
        BEGIN
          LabelFieldOfViewCircle.Caption :=  '2-degree Field of View Circle';
          AngleDegrees := 2.0;
          IF   UpdateNotes
          THEN BEGIN
            MemoStandardObserver.Lines.Add('The experiments leading to the 1931 ' +
              'standard observer were performed using only the fovea, which covers ' +
              'about a 2-degree angle of vision.');

            MemoStandardObserver.Lines.Add('');
            MemoStandardObserver.Lines.Add('According to [Foley96, p. 580], "the ' +
              ' original 1931 tabulation is normally used in work relevant to ' +
              ' computer graphics.')
          END
        END;

      CIEStandardObserver1964:
        BEGIN
          LabelFieldOfViewCircle.Caption := '10-degree Field of View Circle';
          AngleDegrees := 10.0;
          IF   UpdateNotes
          THEN BEGIN
            MemoStandardObserver.Lines.Add('The 1964 supplementary standard observer ' +
              'was based on color-matching experiments using a 10-degree area on the ' +
              'retina.  The observers were instructed to ignore the central 2-degree ' +
              'spot.');

            MemoStandardObserver.Lines.Add('');
            MemoStandardObserver.Lines.Add('The supplementary 1964 standard '+
              'observer is recommended when visual perception of more than about ' +
              '4-degrees is desired.');

            MemoStandardObserver.Lines.Add('');
            MemoStandardObserver.Lines.Add('According to [Foley96, p. 580], the ' +
              ' later 1964 tabulation is not generally useful in computer graphics ' +
              ' since it emphasizes larger areas of constant color than are ' +
              ' normally found in graphics.')
          END
        END
    END;

    IF   UpdateNotes
    THEN BEGIN
      MemoStandardObserver.Lines.Add('');
      MemoStandardObserver.Lines.Add('CIE standard observers are averages based on ' +
         'experiments with small numbers (~15-20) of people with normal color vision. ' +
         'No real observer is probably exactly like the CIE standard observer.');

      MemoStandardObserver.Lines.Add('');
      MemoStandardObserver.Lines.Add('See [Judd75, pp. 153-157] or [Billmeyer81, pp.42-45]');
      MemoStandardObserver.SelStart := 1;
      MemoStandardObserver.SelLength := 1
    END;

    DiagonalPixels := SQRT( SQR(Screen.Width) + SQR(Screen.Height) );
    PixelsPerMillimeter := DiagonalPixels / SpinEditScreenDiagonalMillimeters.Value;

    LabelScreenPixels.Caption :=
      Format('%d pixels wide by %d pixels high',
             [Screen.Width, Screen.Height]);
    LabelScreenMillimeters.Caption :=
      Format('%.1f mm wide by %.1f mm high',
           [Screen.Width /PixelsPerMillimeter,
            Screen.Height/PixelsPerMillimeter]);

    AngleRadians := AngleDegrees * PI / 180.0;
    TRY
    DiameterMillimeters := 2 * SpinEditViewingDistanceMillimeters.Value *
                           ARCTAN(AngleRadians / 2.0);
    EXCEPT
      ON EConvertError DO DiameterMillimeters := 0 
    END;
    
    DiameterPixels := DiameterMillimeters * PixelsPerMillimeter;

    LabelCircleDiameter.Caption :=
      Format('%.1f mm diameter (%.1f pixels)',
             [DiameterMillimeters, DiameterPixels]);

    Bitmap := TBitmap.Create;
    TRY
      Bitmap.Width  := ImageStandardObserver.Width;
      Bitmap.Height := ImageStandardObserver.Height;
      Bitmap.PixelFormat := pf24bit;

      Bitmap.Canvas.Brush.Color := clBtnFace;
      Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);

      iCenter      := Bitmap.Width  DIV 2;
      jCenter      := Bitmap.Height DIV 2;
      RadiusPixels := ROUND(DiameterPixels/2);

      Bitmap.Canvas.Brush.Color := clBlue;
      Bitmap.Canvas.Pen.Color := clBlack;
      Bitmap.Canvas.Ellipse(iCenter-RadiusPixels, jCenter-RadiusPixels,
                            iCenter+RadiusPixels, jCenter+RadiusPixels);
      IF   iCenter-RadiusPixels < 0
      THEN BEGIN
        Bitmap.Canvas.Font.Height := MulDiv(Bitmap.Height, 6, 100);
        Bitmap.Canvas.Font.Color  := clYellow;
        Bitmap.Canvas.Font.Name := 'Arial';
        Bitmap.Canvas.Font.Style := [fsBold];
        s := 'Field of View Circle Truncated';
        Bitmap.Canvas.TextOut(iCenter - Bitmap.Canvas.TextWidth(s) DIV 2,
                              jCenter, s)
      END;

      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.Pen.Color := clRed;
      Bitmap.Canvas.Pen.Width := 2;
      Bitmap.Canvas.MoveTo(20,0);
      LineLengthMillimeters := 10;
      iLength := ROUND(LineLengthMillimeters * PixelsPerMillimeter);
      Bitmap.Canvas.LineTo(20+iLength, 0);

      Bitmap.Canvas.Font.Height := MulDiv(Bitmap.Height, 4, 100);
      Bitmap.Canvas.Font.Color  := clRed;
      Bitmap.Canvas.Font.Name := 'Arial';
      Bitmap.Canvas.Font.Style := [];
      Bitmap.Canvas.TextOut(20, 5, IntToStr(LineLengthMillimeters) + ' mm');

      ImageStandardObserver.Picture.Graphic := Bitmap;
    FINALLY
      Bitmap.Free
    END

  END
END {ShowStandardObserverFieldOfView};


PROCEDURE TFormChromaticity.PlotColorMatchingFunctions;
  VAR
    ColorMatchTriple:  TColorMatchTriple;
    ChartTitle      :  STRING;

  PROCEDURE PlotGraphs(ChartTitle:  STRING);
    VAR
      Wavelength     :  TWavelength;
  BEGIN
    ChartColorMatchingFunctions.Title.Text.Clear;
    ChartColorMatchingFunctions.Title.Text.Add(ChartTitle);

    Series_xBar.Clear;
    Series_yBar.Clear;
    Series_zBar.Clear;

    FOR Wavelength := Low(TWavelength) TO High(TWavelength) DO
    BEGIN
      ColorMatchTriple := GetColorMatchTriple(CIEStandardObserver, Wavelength);
      Series_xBar.AddXY(wavelength, ColorMatchTriple.xBar, '', clTeeColor);
      Series_yBar.AddXY(wavelength, ColorMatchTriple.yBar, '', clTeeColor);
      Series_zBar.AddXY(wavelength, ColorMatchTriple.zBar, '', clTeeColor)
    END

  END {PlotColorMatchingFunctions};

BEGIN
  Screen.Cursor := crHourGlass;
  TRY
    CASE  CIEStandardObserver OF
      CIEStandardObserver1931:
          ChartTitle := 'CIE 1931 Color Matching Functions (2-degree observer)';

      CIEStandardObserver1964:
          ChartTitle := 'CIE 1964 Color Matching Functions (10-degree observer)'
    END;

    PlotGraphs(ChartTitle);
  FINALLY
    Screen.Cursor := crDefault
  END
END {CreateChromaticityCharts};


procedure TFormChromaticity.RadioGroupStandardObserverClick(Sender: TObject);
begin
  CASE RadioGroupStandardObserver.ItemIndex OF
    0:  CIEStandardObserver := CIEStandardObserver1931;
    1:  CIEStandardObserver := CIEStandardObserver1964
  END;
  UpdateEverything
end;


procedure TFormChromaticity.ButtonPrintColorMatchClick(Sender: TObject);
begin
  ChartColorMatchingFunctions.PrintLandscape
end;


procedure TFormChromaticity.SpinEditFieldOfViewUpdate(Sender: TObject);
begin
  CASE RadioGroupStandardObserver.ItemIndex OF
    0:  CIEStandardObserver := CIEStandardObserver1931;
    1:  CIEStandardObserver := CIEStandardObserver1964
  END;

  ShowStandardObserverFieldOfView (FALSE {Update Notes -- avoid flicker})
end;


procedure TFormChromaticity.FormDestroy(Sender: TObject);
  VAR
    Filename:  STRING;
    IniFile :  TIniFile;
begin
  // Save new values
  Filename := ChangeFileExt(ParamStr(0), '.INI');
  IniFile := TIniFile.Create(Filename);
  TRY
    IniFile.WriteInteger(KeywordObserver,
                         KeywordObserverIndex,
                         RadioGroupStandardObserver.ItemIndex);
    IniFile.WriteInteger(KeywordObserver,
                         KeywordViewingDistance,
                         SpinEditViewingDistanceMillimeters.Value);
    IniFile.WriteInteger(KeywordObserver,
                         KeywordScreenDiagonal,
                         SpinEditScreenDiagonalMillimeters.Value)
  FINALLY
    IniFile.Free
  END;
  // Flush INI cache
  WritePrivateProfileString(NIL, NIL, NIL, pChar(Filename))
end;



PROCEDURE TFormChromaticity.PlotChromaticityCharts (CONST ShowFullChart:  BOOLEAN);
  VAR
    Bitmap:  TBitmap;
    Gamma :  TReal;
BEGIN
  TRY
    Gamma := StrToFloat(MaskEditGamma.Text)
  EXCEPT
    ON E: EConvertError DO
      Gamma := 1.0;
  END;

  Bitmap := CIE1931xyChart(CIEStandardObserver, ColorSystem, Gamma, clBtnFace,
                           ImageChart1931.Width, ShowFullChart,
                           CheckBoxGridLines.Checked,
                           CheckBoxWavelengthValues.Checked);
  TRY
    ImageChart1931.Picture.Graphic := Bitmap
  FINALLY
    Bitmap.Free
  END;

  Bitmap := CIEuvChart(CIEChart1960, CIEStandardObserver, ColorSystem, Gamma,
                       clBtnFace, ImageChart1960.Width, ShowFullChart,
                       CheckBoxGridLines.Checked,
                       CheckBoxWavelengthValues.Checked);
  TRY
    ImageChart1960.Picture.Graphic := Bitmap
  FINALLY
    Bitmap.Free
  END;

  Bitmap := CIEuvChart(CIEChart1976, CIEStandardObserver, ColorSystem, Gamma,
                       clBtnFace, ImageChart1976.Width, ShowFullChart,
                       CheckBoxGridLines.Checked,
                       CheckBoxWavelengthValues.Checked);
  TRY
    ImageChart1976.Picture.Graphic := Bitmap
  FINALLY
    Bitmap.Free
  END
END {PlotChromaticityCharts};


procedure TFormChromaticity.ChartColorMatchingFunctionsAfterDraw(
  Sender: TObject);

{$IFDEF VER100}   // Delphi 3
  PROCEDURE PlotLetterBar (CONST Canvas:  TCanvas;
{$ELSE}           // Delphi 4 and later
  PROCEDURE PlotLetterBar (CONST Canvas:  TCanvas3D;
{$ENDIF}
                           CONST s:  STRING;
                           CONST color:  TColor;
                           CONST Wavelength:  INTEGER;
                           CONST Series:  TLineSeries);
    VAR
      i    :  INTEGER;
      index:  INTEGER;
      j    :  INTEGER;
  BEGIN
    Canvas.Font.Color := color;
    index := Wavelength - Low(TWavelength);
    i := Series.CalcXPOS(index);
    j := Series.CalcYPOS(index);
    INC(j, MulDiv(Canvas.TextHeight(s), 1, 5));
    Canvas.TextOut(i,j, s);
    Canvas.Pen.Color := color;
    Canvas.Pen.Width := 2;
    INC(j, MulDiv(Canvas.TextHeight(s), 1, 8));
    Canvas.MoveTo(i,j);
    Canvas.LineTo(i + Canvas.TextWidth(s), j)
  END {PlotLetterBar};

begin
  WITH ChartColorMatchingFunctions DO
  BEGIN
    Canvas.Brush.Style := bsClear;

    // Strange:  In D3, I used
    //    Canvas.ClipRect.Bottom - Canvas.ClipRect.Top,
    // for the height of the drawing area.  This will not
    // compile in D4 or D5, so use the TChart's height
    // instead:
    Canvas.Font.Height := MulDiv(Height, 5, 100);

    Canvas.Font.Style := [fsBold];

    // Customize peak labels
    IF   RadioGroupStandardObserver.ItemIndex = 0
    THEN BEGIN  // 1931
      PlotLetterBar (Canvas, 'x', clRed,  595, Series_xBar);
      PlotLetterBar (Canvas, 'y', clLime, 550, Series_yBar);
      PlotLetterBar (Canvas, 'z', clBlue, 443, Series_zBar)
    END
    ELSE BEGIN  // 1964
      PlotLetterBar (Canvas, 'x', clRed,  591, Series_xBar);
      PlotLetterBar (Canvas, 'y', clLime, 549, Series_yBar);
      PlotLetterBar (Canvas, 'z', clBlue, 442, Series_zBar)
    END
  END
end;


procedure TFormChromaticity.CheckBoxMonitorGamutClick(Sender: TObject);
begin
  UpdateEverything
end;


procedure TFormChromaticity.ComboBoxColorSystemChange(Sender: TObject);
begin
  ColorSystem := DellPhosphors;  // Default to avoid compiler initialization warning

  CASE ComboBoxColorSystem.ItemIndex OF
    0:  ColorSystem := DellPhosphors;
    1:  ColorSystem := NTSCsystem;
    2:  ColorSystem := EBUsystem;
    3:  ColorSystem := SMPTEsystem;
    4:  ColorSystem := ShortPersistencePhosphors;
    5:  COlorSystem := LongPersistencePhosphors;
  END;

  UpdateEverything
end;

procedure TFormChromaticity.SpinButtonGammaUpClick(Sender: TObject);
begin
  MaskEditGamma.Text := Format('%4.2f', [ StrToFloat(MaskEditGamma.Text) + 0.05 ])
end;

procedure TFormChromaticity.SpinButtonGammaDownClick(Sender: TObject);
begin
  MaskEditGamma.Text := Format('%4.2f', [ StrToFloat(MaskEditGamma.Text) - 0.05 ])
end;

procedure TFormChromaticity.MaskEditGammaChange(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  TRY
    UpdateEverything
  FINALLY
    Screen.Cursor := crDefault
  END
end;

procedure TFormChromaticity.LabelLab2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', pchar('http://www.efg2.com/lab'),
               NIL, NIL, SW_NORMAL)
end;

procedure TFormChromaticity.CheckBoxWavelengthValuesClick(Sender: TObject);
begin
  UpdateEverything
end;

procedure TFormChromaticity.ButtonWrite1931Click(Sender: TObject);
begin
  FormWriteCIEFile.ComboBoxCIE.ItemIndex := (Sender AS TButton).Tag;
  FormWriteCIEFile.ShowModal
end;

procedure TFormChromaticity.ButtonSaveColorMatchClick(Sender: TObject);
  VAR
    Extension:  STRING;
begin
  IF   RadioGroupStandardObserver.ItemIndex = 0
  THEN SavePictureDialog.Filename := 'CIE1931ColorMatch'
  ELSE SavePictureDialog.Filename := 'CIE1964ColorMatch';

  IF   SavePictureDialog.Execute
  THEN BEGIN
    Extension := UpperCase( ExtractFileExt(SavePictureDialog.Filename) );
    IF   Extension = '.BMP'
    THEN ChartColorMatchingFunctions.SaveToBitmapFile(SavePictureDialog.Filename)
    ELSE
      IF   Extension = '.WMF'
      THEN ChartColorMatchingFunctions.SaveToMetafile(SavePictureDialog.Filename)
      ELSE
        IF   Extension = '.EMF'
        THEN ChartColorMatchingFunctions.SaveToMetafileEnh(SavePictureDialog.Filename)
        ELSE ShowMessage('Cannot handle filetype "' + Extension + '"')
  END
end;

// Somewhat of a kludge to hide controls that are only relevant to
// some -- but not all -- of the tabs on the TPageControl
procedure TFormChromaticity.PageControlChartsChange(Sender: TObject);
  VAR
    ShowCIE:  BOOLEAN;
begin
  ShowCIE := (PageControlCharts.ActivePage = TabSheetCIE1931) OR
             (PageControlCharts.ActivePage = TabSheetCIE1960) OR
             (PageControlCharts.ActivePage = TabSheetCIE1976);

  ComboBoxColorSystem.Visible      := ShowCIE;
  LabelColorSystem.Visible         := ShowCIE;

  LabelGamma.Visible               := ShowCIE;
  MaskEditGamma.Visible            := ShowCIE;
  SpinButtonGamma.Visible          := ShowCIE;

  CheckBoxMonitorGamut.Visible     := ShowCIE;
  CheckBoxGridLines.Visible        := ShowCIE;
  CheckBoxWavelengthValues.Visible := ShowCIE
end;

end.
