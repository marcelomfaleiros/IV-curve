// Create "real" analogs.
//   TRealPoint instead of TPoint
//   TRealRect instead of TRect
//   RealPoint instead of Point
//   RealRect instead of Rect
//
//  efg, January 1999.

UNIT MapWorldToPixel;

INTERFACE

  USES
    WinTypes,    // TRect, TPoint
    Graphics;    // TCanvas

  // Define "Real" analogs to TPoint and TRect
  TYPE
    TReal = Double;

    TRealPoint =
    RECORD
      x:  TReal;
      y:  TReal
    END;

    TRealRect =
    RECORD
      CASE Integer OF
        0:  (Left, Top, Right, Bottom:  TReal);
        1:  (TopLeft, BottomRight:  TRealPoint)
    END;

    TSimplePantograph =
    CLASS(TObject)
      PRIVATE
        FPixelRect:  TRect;
        FRealRect :  TRealRect;

        FxDelta   :  TReal;
        FyDelta   :  TReal;
        FiDelta   :  INTEGER;
        FjDelta   :  INTEGER;

        FiDeltaOverxDelta:  TReal;
        FjDeltaOveryDelta:  TReal;

        FxDeltaOveriDelta:  TReal;
        FyDeltaOverjDelta:  TReal;


      PUBLIC
        CONSTRUCTOR Create(PixelRect:  TRect; RealRect:  TRealRect);

        // Could make these functions but don't always want to use .x and .y
        // to access fields.
        PROCEDURE MapRealToPixel(CONST x,y:  TReal;   VAR i,j:  INTEGER);
        PROCEDURE MapPixelToReal(CONST i,j:  INTEGER; VAR x,y:  TReal);

        PROCEDURE MoveTo(CONST Canvas:  TCanvas; CONST x,y:  TReal);
        PROCEDURE LineTo(CONST Canvas:  TCanvas; CONST x,y:  TReal);
    END;

    FUNCTION RealPoint(CONST aX, aY:  DOUBLE):  TRealPoint;
    FUNCTION RealRect(CONST aLeft, aTop, aRight, aBottom:  DOUBLE):  TRealRect;


IMPLEMENTATION

  USES
    SysUtils;  // Exception

  TYPE
    ESimplePantographError = CLASS(Exception);

  FUNCTION RealPoint(CONST aX, aY:  DOUBLE):  TRealPoint;
  BEGIN
    WITH RESULT DO
    BEGIN
      X := aX;
      Y := aY
    END
  END {RealRect};

  FUNCTION RealRect(CONST aLeft, aTop, aRight, aBottom:  DOUBLE):  TRealRect;
  BEGIN
    WITH RESULT DO
    BEGIN
      Left   := aLeft;
      Top    := aBottom;    // Switch top and bottom in "world coordinates"
      Right  := aRight;
      Bottom := aTop        // Switch top and bottom in "world coordinates"
    END
  END {RealRect};


  CONSTRUCTOR TSimplePantograph.Create(PixelRect:  TRect; RealRect:  TRealRect);
  BEGIN
    FPixelRect := PixelRect;
    FRealRect  := RealRect;

    FiDelta := PixelRect.Right  - PixelRect.Left;
    FjDelta := PixelRect.Bottom - PixelRect.Top;

    FxDelta := RealRect.Right   - RealRect.Left;
    FyDelta := RealRect.Top     - RealRect.Bottom;

    IF   (FiDelta = 0)   OR (FjDelta = 0)
    THEN RAISE ESimplePantographError.Create('Invalid Rectangle');

    IF   (FxDelta = 0.0) OR (FyDelta = 0.0)
    THEN RAISE ESimplePantographError.Create('Invalid Real Rectangle');

    FxDeltaOveriDelta := FxDelta / FiDelta;
    FyDeltaOverjDelta := FyDelta / FjDelta;

    FiDeltaOverxDelta := FiDelta / FxDelta;
    FjDeltaOveryDelta := FjDelta / FyDelta
  END {Create};


  PROCEDURE TSimplePantograph.MapRealToPixel(CONST x,y:  TReal;  VAR i,j:  INTEGER);
  BEGIN
    i := ROUND( FPixelRect.Left   + (x - FRealRect.Left) * FiDeltaOverxDelta);
    j := ROUND( FPixelRect.Top    - (y - FRealRect.Top)  * FjDeltaOveryDelta)
  END {MapRealToPixel};


  PROCEDURE TSimplePantograph.MapPixelToReal(CONST i,j:  INTEGER; VAR x,y:  TReal);
  BEGIN
    x := FRealRect.Left + (i - FPixelRect.Left) * FxDeltaOveriDelta;
    y := FRealRect.Top  - (j - FPixelRect.Top)  * FyDeltaOverjDelta
  END {MapPixelToReal};


  PROCEDURE TSimplePantograph.MoveTo(CONST Canvas:  TCanvas; CONST x,y:  TReal);
    VAR
      i,j:  INTEGER;
  BEGIN
    MapRealToPixel(x,y, i,j);
    Canvas.MoveTo(i,j)
  END {MoveTo};


  PROCEDURE TSimplePantograph.LineTo(CONST Canvas:  TCanvas; CONST x,y:  TReal);
    VAR
      i,j:  INTEGER;
  BEGIN
    MapRealToPixel(x,y, i,j);
    Canvas.LineTo(i,j)
  END {LineTo};


END.
