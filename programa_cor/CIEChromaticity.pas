UNIT CIEChromaticity;

INTERFACE

  USES
    Windows,                   // TRect
    Graphics,                  // TBitmap;
    XYZtoRGBconversion,        // TColorSystem, XYZtoRGB, InsideGamut
    ColorMatchingFunctions;    // TCIEStandardObserver, xyChromaticityCoordinates



  FUNCTION CIE1931xyChart(CONST CIEStandardObserver:  TCIEStandardObserver;
                          CONST ColorSystem    :  TColorSystem;
                          CONST Gamma          :  TReal;
                          CONST BackgroundColor:  TColor;
                          CONST ChartSize      :  INTEGER;
                          CONST ShowFullChart  :  BOOLEAN;
                          CONST ShowGridLines  :  BOOLEAN;
                          CONST ShowWavelengthValues:  BOOLEAN):  TBitmap;

  FUNCTION CIEuvChart (CONST CIEChart:  TCIEChart;
                       CONST CIEStandardObserver:  TCIEStandardObserver;
                       CONST ColorSystem    :  TColorSystem;
                       CONST Gamma          :  TReal;
                       CONST BackgroundColor:  TColor;
                       CONST ChartSize      :  INTEGER;
                       CONST ShowFullChart  :  BOOLEAN;
                       CONST ShowGridLines  :  BOOLEAN;
                       CONST ShowWavelengthValues:  BOOLEAN):  TBitmap;

  PROCEDURE ShowAnnotation(CONST Canvas:  TCanvas;
                           CONST iText,   jText  :  INTEGER;
                           CONST iTarget, jTarget:  INTEGER;
                           CONST s:  STRING);

IMPLEMENTATION

  USES
    Classes,           // Rect
    Math,              // MaxValue
    SysUtils,          // IntToStr
    MapWorldToPixel;   // TRealRect, MapRealToPixel, MoveTo, LineTo

  CONST
    PixelCountMax          = 32768;

  TYPE
    TRGBTripleArray    = ARRAY[0..PixelCountMax-1] OF TRGBTriple;
    pRGBTripleArray    = ^TRGBTripleArray;

  FUNCTION CIE1931xyChart(CONST CIEStandardObserver:  TCIEStandardObserver;
                          CONST ColorSystem    :  TColorSystem;
                          CONST Gamma          :  TReal;
                          CONST BackgroundColor:  TColor;
                          CONST ChartSize      :  INTEGER;
                          CONST ShowFullChart  :  BOOLEAN;
                          CONST ShowGridLines  :  BOOLEAN;
                          CONST ShowWavelengthValues:  BOOLEAN):  TBitmap;

    CONST
      xMax = 0.84;
      xMin = 0.00;
      yMax = 0.84;
      yMin = 0.00;

    VAR
      CIE1931xyRect     :  TRealRect;
      factor            :  TReal;
      i                 :  INTEGER;
      iDelta            :  INTEGER;
      iText             :  INTEGER;
      j                 :  INTEGER;
      jDelta            :  INTEGER;
      jText             :  INTEGER;
      PixelRect         :  TRect;
      R,G,B             :  TFloat;
      row               :  pRGBTripleArray;
      s                 :  STRING;
      SideLeft          :  INTEGER;
      SideRight         :  INTEGER;
      SimplePantograph  :  TSimplePantograph;
      PerpendicularSlope:  TReal;
      TickLength        :  TReal;
      Wavelength        :  INTEGER;
      x                 :  TReal;
      xDelta            :  TReal;
      xNormalDelta      :  TReal;
      xNormalUnitDelta  :  TReal;
      xOld              :  TReal;
      y                 :  TReal;
      yDelta            :  TReal;
      yNormalDelta      :  TReal;
      yNormalUnitDelta  :  TReal;
      yUnitDelta        :  TReal;
      yOld              :  TReal;
      z                 :  TReal;

    PROCEDURE PlotPoint;
      VAR
        maxRGB:  TFloat;
    BEGIN
      R := Clamp(R, 0.0, 1.0);
      G := Clamp(G, 0.0, 1.0);
      B := Clamp(B, 0.0, 1.0);
      maxRGB := MaxValue( [R, G, B] );

      WITH Row[i] DO
      BEGIN
        IF   ABS(Gamma - 1.00) < 0.0001
        THEN BEGIN
          rgbtRed   := ROUND(255 * R / maxRGB);
          rgbtGreen := ROUND(255 * G / maxRGB);
          rgbtBlue  := ROUND(255 * B / maxRGB)
        END
        ELSE BEGIN
          rgbtRed   := ROUND(255* Power( R/maxRGB, Gamma) );
          rgbtGreen := ROUND(255* Power( G/maxRGB, Gamma) );
          rgbtBlue  := ROUND(255* Power( B/maxRGB, Gamma) )
        END
      END
    END {PlotPoint};

  BEGIN
    RESULT := TBitmap.Create;
    RESULT.Width  := ChartSize;     // create only a square bitmap for now
    RESULT.Height := ChartSize;
    RESULT.PixelFormat := pf24bit;  // avoid using palettes

    RESULT.Canvas.Brush.Color := clBlack;
    RESULT.Canvas.FillRect(RESULT.Canvas.ClipRect);

    iDelta := RESULT.Canvas.ClipRect.Right  - RESULT.Canvas.ClipRect.Left;
    jDelta := RESULT.Canvas.ClipRect.Bottom - RESULT.Canvas.ClipRect.Top;
    PixelRect := Rect(RESULT.Canvas.ClipRect.Left  + MulDiv(iDelta, 5, 100),
                      RESULT.Canvas.ClipRect.Top   + MulDiv(jDelta, 5, 100),
                      RESULT.Canvas.ClipRect.Left  + MulDiv(iDelta, 95,100),
                      RESULT.Canvas.ClipRect.Top   + MulDiv(jDelta, 95,100));

    CIE1931xyRect := RealRect(xMin, yMin, xMax, yMax);

    SimplePantograph := TSimplePantograph.Create(PixelRect, CIE1931xyRect);
    TRY
      RESULT.Canvas.Pen.Color := clRed;

      // Plot wavelengths only from 360 to 730 nm to avoid "hook" in
      // 1964 data above 740 nm

      // Plot outline of chromaticity chart
      wavelength := 360;   // nm
      xyChromaticityCoordinates(CIEStandardObserver, Wavelength, x,y);
      SimplePantograph.MoveTo(RESULT.Canvas, x,y);

      FOR wavelength := 361 TO 730 DO   // nm
      BEGIN
        xyChromaticityCoordinates(CIEStandardObserver, Wavelength, x,y);
        SimplePantograph.LineTo(RESULT.Canvas, x,y)
      END;

      Wavelength := 360;   // nm
      xyChromaticityCoordinates(CIEStandardObserver, Wavelength, x,y);
      SimplePantograph.LineTo(RESULT.Canvas, x,y);

      // Fill in each scanline inside outline
      FOR j := 0 TO RESULT.Height-1 DO
      BEGIN
        Row := RESULT.Scanline[j];

        SideLeft := 0;
        WHILE (SideLeft < RESULT.Width) AND
              (Row[SideLeft].rgbtRed <> 255) DO
          INC(SideLeft);

        SideRight := RESULT.Width-1;
        WHILE (SideRight > 0) AND
              (Row[SideRight].rgbtRed <> 255) DO
          DEC(SideRight);

        FOR i := SideLeft TO SideRight DO
        BEGIN
          SimplePantograph.MapPixelToReal(i,j, x,y);

  //      A color, C, can be defined using CIE color primaries X, Y, and Z:
  //
  //      C = X + Y + Z
  //
  //      The chromaticity coordinates (x,y,z) are defined as
  //
  //      x = X/C
  //      y = Y/C
  //      z = Z/C
  //
  //      For the purposes of this diagram, assume C = 1.0, so for this case
  //      x = X, y = Y and z = Z

          z := 1.0 - x - y;

          // It's a little confusing, but (x,y,z) (i.e., chromaticity values)
          // are the same as (X,Y,Z) (i.e., imaginary additive primaries) here.
          XYZtoRGB(ColorSystem, x, y, z, R, G, B);

          IF   ShowFullChart      OR
               InsideGamut(R,G,B)
          THEN PlotPoint

        END;

        // If we're not showing the full chart, make another pass through to
        // "fix" the outline of the horseshoe to be either a spectral color,
        // or a "line of purple" color.  This is somewhat of a kludge, but
        // optimization isn't really necessary.
        IF   NOT ShowFullChart
        THEN BEGIN
          // point(s) on left side (usually only one)
          i := SideLeft;
          WHILE (i < SideRight) AND (Row[i].rgbtRed = 255) DO
          BEGIN
            SimplePantograph.MapPixelToReal(i,j, x,y);
            z := 1.0 - x - y;
            XYZtoRGB(ColorSystem, x, y, z, R, G, B);
            PlotPoint;
            INC (i)
          END;

          // point(s) on right side (usually only one)
          i := SideRight;
          WHILE (i > 0) AND (Row[i].rgbtRed = 255) DO
          BEGIN
            SimplePantograph.MapPixelToReal(i,j, x,y);
            z := 1.0 - x - y;
            XYZtoRGB(ColorSystem, x, y, z, R, G, B);
            PlotPoint;
            DEC (i)
          END;

        END
      END;

      // Gridlines are "under" Wavelength Values
      IF   ShowGridLines
      THEN BEGIN
        RESULT.Canvas.Font.Name := 'Arial Narrow';
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        RESULT.Canvas.Font.Color := clDkGray;

        RESULT.Canvas.Pen.Width := 1;
        RESULT.Canvas.Pen.Color := clDkGray;
        RESULT.Canvas.Pen.Style := psDot;
        RESULT.Canvas.Brush.Style := bsClear;

        FOR i := 0 TO TRUNC( (xMax - xMin)/0.10 ) DO
        BEGIN
          x := 0.1*i;
          SimplePantograph.MoveTo(RESULT.Canvas, x, yMin);
          SimplePantograph.LineTo(RESULT.Canvas, x, yMax);

          s := Format('%.1f', [0.1*i]);
          SimplePantograph.MapRealToPixel(x,yMin, iText,jText);
          RESULT.Canvas.TextOut(iText-RESULT.Canvas.TextWidth(s) DIV 2,
                                jText, s);
        END;

        // Label x Axis
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 6,100);
        RESULT.Canvas.TextOut(RESULT.Width -
                              3*RESULT.Canvas.TextWidth('X') DIV 2,
                              jText - RESULT.Canvas.TextHeight('X') DIV 2, 'x');

        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        FOR j := 0 TO TRUNC( (yMax - yMin)/0.10 ) DO
        BEGIN
          y := 0.1*j;
          SimplePantograph.MoveTo(RESULT.Canvas, xMin, 0.1*j);
          SimplePantograph.LineTo(RESULT.Canvas, xMax, 0.1*j);

          s := Format('%.1f', [0.1*j]);
          SimplePantograph.MapRealToPixel(xMin,y, iText,jText);

          IF   j = 0  // avoid overwrite at origin
          THEN jText := jText-3*RESULT.Canvas.TextHeight(s) DIV 4
          ELSE jText := jText-RESULT.Canvas.TextHeight(s) DIV 2;

          RESULT.Canvas.TextOut(iText-RESULT.Canvas.TextWidth(s),
                                jText, s);

        END;

        // Label y Axis
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 6,100);
        RESULT.Canvas.TextOut(RESULT.Canvas.TextWidth('X')  DIV 2,
                              0,'y')
      END;

      IF   ShowWavelengthValues
      THEN BEGIN
        TickLength := 0.02 * 0.84;

        // Show wavelength values
        wavelength := 419;   // nm
        xyChromaticityCoordinates(CIEStandardObserver, Wavelength, xOld,yOld);

        RESULT.Canvas.Pen.Width := 1;
        RESULT.Canvas.Pen.Style := psSolid;
        RESULT.Canvas.Font.Name := 'Arial Narrow';
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        RESULT.Canvas.Font.Color := clWhite;
        RESULT.Canvas.Brush.Style := bsClear;

        FOR wavelength := 420 TO 680 DO   // nm
        BEGIN
          xyChromaticityCoordinates(CIEStandardObserver, Wavelength, x,y);

          RESULT.Canvas.Pen.Color := clWhite;
          factor := 0;
          IF   wavelength MOD 10 = 0
          THEN factor := 1.0
          ELSE
            IF   wavelength MOD 5 = 0
            THEN factor := 0.5;

          // Kludge to get rid of too many tick marks
          IF   (wavelength > 420) AND (wavelength < 450)
          THEN factor := 0.0;

          IF   (wavelength > 630) AND (wavelength < 680)
          THEN factor := 0.0;

          IF   factor > 0
          THEN BEGIN
            xDelta := x - xOld;
            yDelta := y - yOld;

            xNormalDelta :=  yDelta;
            yNormalDelta := -xDelta;

            xNormalUnitDelta := xNormalDelta / SQRT( SQR(xNormalDelta) + SQR(yNormalDelta) );
            yNormalUnitDelta := yNormalDelta / SQRt( SQR(xNormalDelta) + SQR(yNormalDelta) );

            IF   (wavelength MOD 10 = 0)  AND  // every 10th nm
                 (wavelength DIV 10 IN         // divide by 10 to use 0..255 set
                  ([42..68] - [43,44,45, 50, 62,64..67]))   // avoid overwrites
            THEN BEGIN
              // Show wavelength annotation
              SimplePantograph.MapRealToPixel(x,y, i,j);
              SimplePantograph.MapRealToPixel(x-factor*TickLength*xNormalUnitDelta,
                                              y-factor*TickLength*yNormalUnitDelta,
                                              iText, jText);
               ShowAnnotation(RESULT.Canvas, iText,jText, i,j, IntToStr(wavelength))
            END
            ELSE BEGIN
  //          SimplePantograph.MoveTo(RESULT.Canvas, x,y);
  //          SimplePantograph.LineTo(RESULT.Canvas, x+factor*TickLength*xNormalUnitDelta,
  //                                                 y+factor*TickLength*yNormalUnitDelta);

              SimplePantograph.MoveTo(RESULT.Canvas, x,y);
              SimplePantograph.LineTo(RESULT.Canvas, x-factor*TickLength*xNormalUnitDelta,
                                                     y-factor*TickLength*yNormalUnitDelta);
            END
          END;

          xOld := x;
          yOld := y;
        END
      END

    FINALLY
      SimplePantograph.Free
    END

  END {CIE1931Chart};


  // 1960 uv Chart or 1976 u'v' Chart
  FUNCTION CIEuvChart (CONST CIEChart:  TCIEChart;
                       CONST CIEStandardObserver:  TCIEStandardObserver;
                       CONST ColorSystem    :  TColorSystem;
                       CONST Gamma          :  TReal;
                       CONST BackgroundColor:  TColor;
                       CONST ChartSize      :  INTEGER;
                       CONST ShowFullChart  :  BOOLEAN;
                       CONST ShowGridLines  :  BOOLEAN;
                       CONST ShowWavelengthValues:  BOOLEAN):  TBitmap;

    CONST
      xMax = 0.60;
      xMin = 0.00;
      yMax = 0.60;
      yMin = 0.00;

    VAR
      CIE1960uvRect     :  TRealRect;
      denominator       :  TReal;
      factor            :  TReal;
      i                 :  INTEGER;
      iDelta            :  INTEGER;
      iText             :  INTEGER;
      j                 :  INTEGER;
      jDelta            :  INTEGER;
      jText             :  INTEGER;
      maxRGB            :  TFloat;
      PerpendicularSlope:  TReal;
      PixelRect         :  TRect;
      R,G,B             :  TFloat;
      row               :  pRGBTripleArray;
      s                 :  STRING;
      SideLeft          :  INTEGER;
      SideRight         :  INTEGER;
      SimplePantograph  :  TSimplePantograph;
      TickLength        :  TReal;
      wavelength        :  INTEGER;
      u                 :  TReal;
      v                 :  TReal;
      x                 :  TReal;
      xDelta            :  TReal;
      xNormalDelta      :  TReal;
      xNormalUnitDelta  :  TReal;
      xOld              :  TReal;
      y                 :  TReal;
      yDelta            :  TReal;
      yNormalDelta      :  TReal;
      yNormalUnitDelta  :  TReal;
      yOld              :  TReal;
      z                 :  TReal;

    PROCEDURE PlotPoint;
      VAR
        maxRGB:  TFloat;
    BEGIN
      R := Clamp(R, 0.0, 1.0);
      G := Clamp(G, 0.0, 1.0);
      B := Clamp(B, 0.0, 1.0);
      maxRGB := MaxValue( [R, G, B] );

      WITH Row[i] DO
      BEGIN
        rgbtRed   := ROUND(255* Power( R/maxRGB, Gamma) );
        rgbtGreen := ROUND(255* Power( G/maxRGB, Gamma) );
        rgbtBlue  := ROUND(255* Power( B/maxRGB, Gamma) );
      END
    END {PlotPoint};

  BEGIN
    RESULT := TBitmap.Create;
    RESULT.Width  := ChartSize;      // create only a square bitmap for now
    RESULT.Height := ChartSize;
    RESULT.PixelFormat := pf24bit;   // avoid using of palettes

    RESULT.Canvas.Brush.Color := clBlack;
    RESULT.Canvas.FillRect(RESULT.Canvas.ClipRect);

    iDelta := RESULT.Canvas.ClipRect.Right  - RESULT.Canvas.ClipRect.Left;
    jDelta := RESULT.Canvas.ClipRect.Bottom - RESULT.Canvas.ClipRect.Top;
    PixelRect := Rect(RESULT.Canvas.ClipRect.Left  + MulDiv(iDelta, 5, 100),
                      RESULT.Canvas.ClipRect.Top   + MulDiv(jDelta, 5, 100),
                      RESULT.Canvas.ClipRect.Left  + MulDiv(iDelta, 95,100),
                      RESULT.Canvas.ClipRect.Top   + MulDiv(jDelta, 95,100));


    CIE1960uvRect := RealRect(0.0, 0.0, 0.60, 0.60);

    SimplePantograph := TSimplePantograph.Create(PixelRect, CIE1960uvRect);
    TRY
      RESULT.Canvas.Pen.Color := clRed;

      // Plot wavelengths only from 360 to 730 nm to avoid "hook" in
      // 1964 data above 740 nm

      // Plot outline of chromaticity chart
      Wavelength := 360;   // nm
      uvChromaticityCoordinates(CIEChart, CIEStandardObserver, Wavelength, u,v);
      SimplePantograph.MoveTo(RESULT.Canvas, u,v);

      FOR Wavelength := 361 TO 730 DO   // nm
      BEGIN
        uvChromaticityCoordinates(CIEChart, CIEStandardObserver, Wavelength, u,v);
        SimplePantograph.LineTo(RESULT.Canvas, u,v)
      END;

      // Avoid compiler warning about initialization
      x := 0;
      y := 0;

      Wavelength := 360;   // nm
      uvChromaticityCoordinates(CIEChart, CIEStandardObserver, Wavelength, u,v);
      SimplePantograph.LineTo(RESULT.Canvas, u,v);

     // Fill in each scanline inside outline
      FOR j := 0 TO RESULT.Height-1 DO
      BEGIN
        Row := RESULT.Scanline[j];

        SideLeft := 0;
        WHILE (SideLeft < RESULT.Width) AND
              (Row[SideLeft].rgbtRed = 0) DO
          INC(SideLeft);

        SideRight := RESULT.Width-1;
        WHILE (SideRight > 0) AND
              (Row[SideRight].rgbtREd = 0) DO
          DEC(SideRight);

        FOR i := SideLeft TO SideRight DO
        BEGIN
          SimplePantograph.MapPixelToReal(i,j, u,v);

          CASE CIEChart OF
            CIEChart1960:
              BEGIN
                // "Color Theory and Its Application in Art and Design"
                // George A. Agoston, Springer-Verlag, p. 240, 1987
                //
                // "Color in Business, Science and Industry" (3rd edition)
                // Deane B. Judd and Gunter Wyszecki, John Wiley, p. 296, 1975
                // for inverse of these functions
                //
                denominator :=  2*u - 8*v + 4;
                x := 3*u / denominator;
                y := 2*v / denominator
              END;

            CIEChart1976:
              BEGIN
                // Here (u,v) is really (u',v')
                // "Principles of Color Technology" (2nd edition)
                // Fred W. Billmeyer, Jr. and Max Saltzman
                // John Wiley, p. 58, 1981
                denominator := 18*u - 48*v + 36;
                x := 27*u / denominator;
                y := 12*v / denominator;
              END;
          END;

          z := 1.0 - x - y;

          // See comments above in CIE1931Chart as to why x=X, y=Y and z=Z here
          XYZtoRGB(ColorSystem, x, y, z, R, G, B);

          IF   ShowFullChart OR InsideGamut(R,G,B)
          THEN BEGIN

            R := Clamp(R, 0.0, 1.0);
            G := Clamp(G, 0.0, 1.0);
            B := Clamp(B, 0.0, 1.0);
            maxRGB := MaxValue( [R, G, B] );

            WITH Row[i] DO
            BEGIN
              rgbtRed   := ROUND(255* Power( R/maxRGB, Gamma) );
              rgbtGreen := ROUND(255* Power( G/maxRGB, Gamma) );
              rgbtBlue  := ROUND(255* Power( B/maxRGB, Gamma) );
            END

          END
        END;

        // If we're not showing the full chart, make another pass through to
        // "fix" the outline of the horseshoe to be either a spectral color,
        // or a "line of purple" color.  This is somewhat of a kludge, but
        // optimization isn't really necessary.
        IF   NOT ShowFullChart
        THEN BEGIN
          // point(s) on left side (usually only one)
          i := SideLeft;
          WHILE (i < SideRight) AND (Row[i].rgbtRed > 0) DO
          BEGIN
            SimplePantograph.MapPixelToReal(i,j, u,v);

            CASE CIEChart OF
              CIEChart1960:
                BEGIN
                  // "Color Theory and Its Application in Art and Design"
                  // George A. Agoston, Springer-Verlag, p. 240, 1987
                  //
                  // "Color in Business, Science and Industry" (3rd edition)
                  // Deane B. Judd and Gunter Wyszecki, John Wiley, p. 296, 1975
                  // for inverse of these functions
                  //
                  denominator :=  2*u - 8*v + 4;
                  x := 3*u / denominator;
                  y := 2*v / denominator
                END;

              CIEChart1976:
                BEGIN
                  // Here (u,v) is really (u',v')
                  // "Principles of Color Technology" (2nd edition)
                  // Fred W. Billmeyer, Jr. and Max Saltzman
                  // John Wiley, p. 58, 1981
                  denominator := 18*u - 48*v + 36;
                  x := 27*u / denominator;
                  y := 12*v / denominator
                END;
            END;

            z := 1.0 - x - y;
            XYZtoRGB(ColorSystem, x, y, z, R, G, B);
            PlotPoint;
            INC (i)
          END;

          // point(s) on right side (usually only one)
          i := SideRight;
          WHILE (i > 0) AND (Row[i].rgbtRed > 0) DO
          BEGIN
            SimplePantograph.MapPixelToReal(i,j, u,v);

            CASE CIEChart OF
              CIEChart1960:
                BEGIN
                  // "Color Theory and Its Application in Art and Design"
                  // George A. Agoston, Springer-Verlag, p. 240, 1987
                  //
                  // "Color in Business, Science and Industry" (3rd edition)
                  // Deane B. Judd and Gunter Wyszecki, John Wiley, p. 296, 1975
                  // for inverse of these functions
                  //
                  denominator :=  2*u - 8*v + 4;
                  x := 3*u / denominator;
                  y := 2*v / denominator
                END;

              CIEChart1976:
                BEGIN
                  // Here (u,v) is really (u',v')
                  // "Principles of Color Technology" (2nd edition)
                  // Fred W. Billmeyer, Jr. and Max Saltzman
                  // John Wiley, p. 58, 1981
                  x := 27*u / (18*u - 48*v + 36);
                  y := 12*v / (18*u - 48*v + 36);
                END;
            END;

            z := 1.0 - x - y;
            XYZtoRGB(ColorSystem, x, y, z, R, G, B);
            PlotPoint;
            DEC (i)
          END;

        END

      END;

      // Gridlines are "under" Wavelength Values
      IF   ShowGridLines
      THEN BEGIN
        RESULT.Canvas.Font.Name := 'Arial Narrow';
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        RESULT.Canvas.Font.Color := clDkGray;

        RESULT.Canvas.Pen.Width := 1;
        RESULT.Canvas.Pen.Color := clDkGray;
        RESULT.Canvas.Pen.Style := psDot;
        RESULT.Canvas.Brush.Style := bsClear;

        FOR i := 0 TO TRUNC( (xMax - xMin)/0.10 ) DO
        BEGIN
          x := 0.1*i;
          SimplePantograph.MoveTo(RESULT.Canvas, x, yMin);
          SimplePantograph.LineTo(RESULT.Canvas, x, yMax);

          s := Format('%.1f', [0.1*i]);
          SimplePantograph.MapRealToPixel(x,yMin, iText,jText);
          RESULT.Canvas.TextOut(iText-RESULT.Canvas.TextWidth(s) DIV 2,
                                jText, s);
        END;

        // Label x Axis
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 6,100);
        IF   CIEChart = CIEChart1960
        THEN s := 'u'
        ELSE s := 'u''';

        SimplePantograph.MapRealToPixel(xMax,yMin, iText,jText);
        RESULT.Canvas.TextOut(iText + RESULT.Canvas.TextWidth('x') DIV 2,
                              jText - 3*RESULT.Canvas.TextHeight(s) DIV 4, s);

        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        FOR j := 0 TO TRUNC( (yMax - yMin)/0.10 ) DO
        BEGIN
          y := 0.1*j;
          SimplePantograph.MoveTo(RESULT.Canvas, xMin, 0.1*j);
          SimplePantograph.LineTo(RESULT.Canvas, xMax, 0.1*j);

          s := Format('%.1f', [0.1*j]);
          SimplePantograph.MapRealToPixel(xMin,y, iText,jText);
          IF   j = 0  // avoid overwrite at origin
          THEN jText := jText-3*RESULT.Canvas.TextHeight(s) DIV 4
          ELSE jText := jText-RESULT.Canvas.TextHeight(s) DIV 2;
          RESULT.Canvas.TextOut(iText-RESULT.Canvas.TextWidth(s),
                                jText, s);

        END;

        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 6,100);
        IF   CIEChart = CIEChart1960
        THEN s := 'v'
        ELSE s := 'v''';

        SimplePantograph.MapRealToPixel(xMin,yMax, iText,jText);
        RESULT.Canvas.TextOut(iText,
                              jText - RESULT.Canvas.TextHeight(s), s)
      END;

      IF   ShowWavelengthValues
      THEN BEGIN
        TickLength := 0.02 * 0.84;

        // Show wavelength values
        wavelength := 419;   // nm
        uvChromaticityCoordinates(CIEChart, CIEStandardObserver, Wavelength, xOld,yOld);

        RESULT.Canvas.Pen.Width := 1;
        RESULT.Canvas.Pen.Style := psSolid;
        RESULT.Canvas.Font.Name := 'Arial Narrow';
        RESULT.Canvas.Font.Height := MulDiv(Result.Height, 4,100);
        RESULT.Canvas.Font.Color := clWhite;
        RESULT.Canvas.Brush.Style := bsClear;

        FOR wavelength := 420 TO 680 DO   // nm
        BEGIN
          uvChromaticityCoordinates(CIEChart, CIEStandardObserver, Wavelength, x,y);

          RESULT.Canvas.Pen.Color := clWhite;
          factor := 0;
          IF   wavelength MOD 10 = 0
          THEN factor := 1.0
          ELSE
            IF   wavelength MOD 5 = 0
            THEN factor := 0.5;

          // Kludge to get rid of too many tick marks
//          IF   (wavelength > 420) AND (wavelength < 440)
//          THEN factor := 0.0;

          IF   (wavelength > 640) AND (wavelength < 680)
          THEN factor := 0.0;

          IF   factor > 0
          THEN BEGIN
            xDelta := x - xOld;
            yDelta := y - yOld;

            xNormalDelta :=  yDelta;
            yNormalDelta := -xDelta;

            xNormalUnitDelta := xNormalDelta / SQRT( SQR(xNormalDelta) + SQR(yNormalDelta) );
            yNormalUnitDelta := yNormalDelta / SQRt( SQR(xNormalDelta) + SQR(yNormalDelta) );

            IF   (wavelength MOD 10 = 0)  AND  // every 10th nm
                 (wavelength DIV 10 IN         // divide by 10 to use 0..255 set
                  ([42..68] - [43, 50,51, 63, 65..67]))   // avoid overwrites
            THEN BEGIN
              // Show wavelength annotation
              SimplePantograph.MapRealToPixel(x,y, i,j);
              SimplePantograph.MapRealToPixel(x-factor*TickLength*xNormalUnitDelta,
                                              y-factor*TickLength*yNormalUnitDelta,
                                              iText, jText);
               ShowAnnotation(RESULT.Canvas, iText,jText, i,j, IntToStr(wavelength))
            END
            ELSE BEGIN
  //          SimplePantograph.MoveTo(RESULT.Canvas, x,y);
  //          SimplePantograph.LineTo(RESULT.Canvas, x+factor*TickLength*xNormalUnitDelta,
  //                                                 y+factor*TickLength*yNormalUnitDelta);

              SimplePantograph.MoveTo(RESULT.Canvas, x,y);
              SimplePantograph.LineTo(RESULT.Canvas, x-factor*TickLength*xNormalUnitDelta,
                                                     y-factor*TickLength*yNormalUnitDelta);
            END
          END;

          xOld := x;
          yOld := y;
        END
      END

    FINALLY
      SimplePantograph.Free
    END

  END {CIEuvChart};


  // Draw Text in a "smart" way at the end of a line
  PROCEDURE ShowAnnotation(CONST Canvas:  TCanvas;
                           CONST iText,   jText  :  INTEGER;
                           CONST iTarget, jTarget:  INTEGER;
                           CONST s:  STRING);
    VAR
      angle :  Double;
      i     :  INTEGER;
      index :  INTEGER;
      j     :  INTEGER;
      width :  INTEGER;
      height:  INTEGER;
  BEGIN
    // Given points(iText,jText) and (iTarget,jTarget).  The angle between
    // the x-axis and the vector (iTarget-iText, jTarget-jText) ranges in the
    // open interval [0.0, 360.0) degrees.  Note:  Normally, ArcTan2 returns
    // a value from -PI to PI.
    angle := 180 * (1  + ArcTan2(jText-jTarget, iTarget-iText) / PI);
    IF   angle >= 360.0
    THEN angle := angle - 360.0;

    index := TRUNC( (angle + 22.5) / 45.0);

    Canvas.MoveTo(iText,jText);
    Canvas.LineTo(iTarget, jTarget);

    width  := Canvas.TextWidth(s);
    height := Canvas.TextHeight(s);

    CASE  index OF
      0,8:
          BEGIN  // East
            i := iText;
            j := jText - height DIV 2
          END;

      1:  BEGIN  //NorthEast
            i := iText;
            j := jText - height;
          END;

      2:  BEGIN  // North
            i := iText - width DIV 2;
            j := jText - height;
          END;

      3:  BEGIN  // NorthWest
            i := iText - width;
            j := jText - height;
          END;

      4:  BEGIN  // West
            i := iText - width;
            j := jText - height DIV 2
          END;

      5:  BEGIN  // SouthWest
            i := iText - width;
            j := jText
          END;

      6:  BEGIN  // South
            i := iText - width DIV 2;
            j := jText
          END;

      7:  BEGIN  // SouthEast
            i := iText;
            j := jText
          END;

      ELSE
       i := iText;
       j := jText;
    END;

    // Draw Text in a "smart" way at end of line
    Canvas.TextOut(i, j, s)
  END;


END.
