;===========================================================================
;                                                                          =
;                                   MINX                                   =
;                                                                          =
;===========================================================================
;                                                                          =
;                         Jet Propulsion Laboratory                        =
;                                   MISR                                   =
;                                                                          =
;         Copyright 2007-2019, California Institute of Technology.         =
;                           ALL RIGHTS RESERVED.                           =
;                 U.S. Government Sponsorship acknowledged.                =
;                                                                          =
;===========================================================================

;***************************************************************************
PRO SetConfigValues, Auto, wTopWorkBase, CurFrame, OrbitNum
;***************************************************************************
; Set configurable parameter values. AirMISR option removed.
; CORR_OP_WIDTH / 2 is the maximum pixel offset allowed; this effectively
;     eliminates most clouds from inclusion in registration correction
; REF_WNDW_SIZE + 1 is the real correlation operator size
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON config, Cancel

   !SAV.HtWind.REF_WNDW_SIZE = 12
   !SAV.HtWind.SLOPE_TUNING_FCTR = 10.0
   !SAV.HtWind.MEANABSDEV_THRESH = 0.025
   !VAR.CurrFiles.PRINT_FILENAME = ''

   CASE CurFrame OF
      0    : !SAV.HtWind.CORR_OP_WIDTH = 11 ; Correlation window width
      1    : !SAV.HtWind.CORR_OP_WIDTH = 9  ; should be odd and larger for
      2    : !SAV.HtWind.CORR_OP_WIDTH = 7  ; the D cameras to accommodate
      3    : !SAV.HtWind.CORR_OP_WIDTH = 5  ; larger coregistration errors.
      4    : !SAV.HtWind.CORR_OP_WIDTH = 5  
      5    : !SAV.HtWind.CORR_OP_WIDTH = 5  ; These are used only for the
      6    : !SAV.HtWind.CORR_OP_WIDTH = 5  ; correlation of surfaces when
      7    : !SAV.HtWind.CORR_OP_WIDTH = 5  ; performing registration error
      8    : !SAV.HtWind.CORR_OP_WIDTH = 7  ; correction.
      9    : !SAV.HtWind.CORR_OP_WIDTH = 9
      ELSE : !SAV.HtWind.CORR_OP_WIDTH = 11 ; THESE DETERMINE MAX OFFSET
   ENDCASE

END ; SetConfigValues

;***************************************************************************
PRO CorrelatePatch, Curframe, AnCam, Ialong, Icross, RefWndwHalfSize, $
                    CorrMatrix, Offsets, SurfaceFit, MaxCorr, MeanAbsDev, $
                    MinSlope, FineSlopeThresh, CorrFailed
;***************************************************************************
; Does actual camera correlation for registration correction and tests for
; success.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON config, Cancel

COMPILE_OPT IDL2

   MaxCorr = !KON.Misc.BADVALUE_REAL
   MeanAbsDev = !KON.Misc.BADVALUE_REAL

   SLOPE_CONST1 = 0.0003
   MIN_GOOD_CORR = 0.65

   ;------------------------------------------------------------------------
   ; Half the CORR_OP_WIDTH (correlation operator) rounded down.
   ;------------------------------------------------------------------------

   corr_op_half_width = !SAV.HtWind.CORR_OP_WIDTH / 2

   ;------------------------------------------------------------------------
   ; Compute the coordinates of the correlation window.
   ;------------------------------------------------------------------------

   c1 = Icross - RefWndwHalfSize
   c2 = Icross + RefWndwHalfSize
   a1 = Ialong - RefWndwHalfSize
   a2 = Ialong + RefWndwHalfSize

   num_corr_sq = FLOAT(c2 - c1 + 1) * FLOAT(a2 - a1 + 1)

   CorrFailed = 0

   ;------------------------------------------------------------------------
   ; Make a copy of the correlation window for the reference camera.
   ;------------------------------------------------------------------------
   
   ref_wndw = DOUBLE(GetRawImage(c1, c2, a1, a2, 0, AnCam, $
                     !KON.Instr.HI_RES_PIX_SIZE, $
                     !KON.Misc.INTERP_TYPE_SAMP))
      
   ;------------------------------------------------------------------------
   ; Skip a window if there aren't enough valid data points. 
   ;------------------------------------------------------------------------

   badndx = WHERE(ref_wndw LE 0 OR $
                  GetRawImage(c1, c2, a1, a2, 0, Curframe-1, $
                              !KON.Instr.HI_RES_PIX_SIZE, $
                              !KON.Misc.INTERP_TYPE_SAMP) LE 0, numbad)
   badndx = 0

   IF (numbad GT (1.0-!SAV.HtWind.FRACTION_VALID) * num_corr_sq) THEN BEGIN
      CorrFailed = 5
      GOTO, cleanup8
   ENDIF 

   ;------------------------------------------------------------------------
   ; Fill the correlation matrix by cross-multiplying the camera windows
   ; as they are shifted with respect to each other over the range of +/-
   ; corr_op_half_width 2D positions.
   ;------------------------------------------------------------------------

   COW_sq_inv = 1.0 / DOUBLE(num_corr_sq)
   ref_t = ref_wndw - COW_sq_inv * TOTAL(ref_wndw)
   ref_ref = TOTAL(ref_t * ref_t)

   pix_ndx = INDGEN(!SAV.HtWind.CORR_OP_WIDTH)
   a3 = a1 + pix_ndx - corr_op_half_width
   a4 = a2 + pix_ndx - corr_op_half_width
   c3 = c1 + pix_ndx - corr_op_half_width
   c4 = c2 + pix_ndx - corr_op_half_width

   FOR ia1 = 0, !SAV.HtWind.CORR_OP_WIDTH-1 DO BEGIN
      FOR ic1 = 0, !SAV.HtWind.CORR_OP_WIDTH-1 DO BEGIN

         ;------------------------------------------------------------------
         ; Make a copy of the correlation window for the comparison camera.
         ;------------------------------------------------------------------
   
         cmp_wndw = DOUBLE(GetRawImage(c3[ic1], c4[ic1], a3[ia1], a4[ia1], $
                           0, Curframe-1, !KON.Instr.HI_RES_PIX_SIZE, $
                           !KON.Misc.INTERP_TYPE_SAMP))

         ;------------------------------------------------------------------
         ; Compute Pearson correlation coefficient metric in optimized form.
         ;------------------------------------------------------------------

         cmp_t = cmp_wndw - COW_sq_inv * TOTAL(cmp_wndw)
         ref_cmp = ref_t * cmp_t
         cmp_cmp = cmp_t * cmp_t
         CorrMatrix[ic1,ia1] = TOTAL(ref_cmp) / SQRT(ref_ref * TOTAL(cmp_cmp))
      ENDFOR      
   ENDFOR      

   goodndx = 0
   pix_ndx = 0
   a3 = 0
   a4 = 0
   c3 = 0
   c4 = 0
   
   ;------------------------------------------------------------------------
   ; Reject any grid point if correlation wasn't successful at every point.
   ;------------------------------------------------------------------------

   stddev_ndx = WHERE(CorrMatrix NE !KON.Misc.BADVALUE_REAL, ngood)
   IF (ngood NE !SAV.HtWind.CORR_OP_WIDTH * $
                !SAV.HtWind.CORR_OP_WIDTH) THEN BEGIN
      CorrFailed = 6
      GOTO, cleanup8
   ENDIF

   ;------------------------------------------------------------------------
   ; For each center, fit a surface and interpolate to a smaller grid size
   ; to find the fractional index where the greatest correlation is present.
   ; The fitted output grid has higher resolution than input. Bi-cubic
   ; interpolation is much faster than and almost as good as thin-plate
   ; spline interpolation.
   ;------------------------------------------------------------------------

   outwidth = !SAV.HtWind.CORR_OP_WIDTH * !SAV.HtWind.INTERPOLATE_FCTR

   XX = (FINDGEN(outwidth) - !SAV.HtWind.INTERPOLATE_FCTR * 0.5 + 0.5) / $
        !SAV.HtWind.INTERPOLATE_FCTR
   SurfaceFit = INTERPOLATE(CorrMatrix, XX, XX, /GRID, CUBIC=(-0.5))
   XX = 0

   ;------------------------------------------------------------------------
   ; Find the maximum of the fitted surface and convert its indices into
   ; fractional indices. Take into account that the center of the correlation
   ; matrix is half a fine pixel greater than its index and half a coarse
   ; pixel greater than its index.
   ;------------------------------------------------------------------------   

   MaxCorr = MAX(SurfaceFit, maxndx)
   index_2d = ARRAY_INDICES(SurfaceFit, maxndx)
   Offsets = (0.5 + index_2d) / $
              FLOAT(!SAV.HtWind.INTERPOLATE_FCTR) - (0.5 + corr_op_half_width)

   ;------------------------------------------------------------------------
   ; Find the peakedness and monotonicity of slopes on each of 4 sides of
   ; the maximum point for the interpolated surface, and use the results to
   ; filter out unwanted points.
   ;------------------------------------------------------------------------

   IF (MaxCorr GT MIN_GOOD_CORR AND MaxCorr LT 1.0) THEN BEGIN
      CorrelationSlopeTest, SLOPE_CONST1, FineSlopeThresh, SurfaceFit, $
                            index_2d, MinSlope, CorrFailed
      IF (CorrFailed NE 0) THEN GOTO, cleanup8
   ENDIF ELSE BEGIN
      CorrFailed = 1
      GOTO, cleanup8
   ENDELSE

   ;------------------------------------------------------------------------
   ; Find the maximum absolute deviation of the correlation surface and
   ; reject the grid point if it is too large.
   ;------------------------------------------------------------------------

   half_npts = !SAV.HtWind.INTERPOLATE_FCTR

   x1 = index_2d[0] - half_npts
   x2 = index_2d[0] + half_npts
   IF (x1 LT 0) THEN BEGIN
      x2 -= x1
      x1  = 0
   ENDIF
   IF (x2 GT (outwidth - 1)) THEN BEGIN
      x1 -= (x2 - outwidth + 1)
      x2  = outwidth - 1
   ENDIF

   y1 = index_2d[1] - half_npts
   y2 = index_2d[1] + half_npts
   IF (y1 LT 0) THEN BEGIN
      y2 -= y1
      y1  = 0
   ENDIF
   IF (y2 GT (outwidth - 1)) THEN BEGIN
      y1 -= (y2 - outwidth + 1)
      y2  = outwidth - 1
   ENDIF

   temp_corr = SurfaceFit[x1:x2, y1:y2]
   MeanAbsDev = TOTAL(ABS(temp_corr - MaxCorr)) / N_ELEMENTS(temp_corr)
   temp_corr = 0

   IF (MeanAbsDev LT !SAV.HtWind.MEANABSDEV_THRESH) THEN BEGIN
      CorrFailed = 4
   ENDIF

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

cleanup8:
   ref_wndw = 0
   ref1_wndw = 0
   cmp_wndw = 0
   cmp1_wndw = 0
   combo_wndw = 0
   stddev_ndx = 0
   SurfaceFit = 0

END  ;  CorrelatePatch

;***************************************************************************
PRO CorrelatePlume, iCam, CorrWndw, PassNum, RefWndw, CmpWndw, $
                    CmpPtCenter, CorrOpWidth, CmpWndwSize, DrawCorrs, $
                    CorrMatrix, Offsets, MaxCorr, CorrFailed, Retval
;***************************************************************************
; Does actual correlation of clouds/plumes and tests for success.
;---------------------------------------------------------------------------
   
   COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
   COMMON config, Cancel
   
   COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
   ;------------------------------------------------------------------------
   ; Set constants.
   ;------------------------------------------------------------------------
   
   Retval = -1
   CorrFailed = 0

   MIN_GOOD_CORR1 = (!SAV.Digitize.RELAX_THRESH EQ 1) ? 0.50 : 0.65
   MIN_GOOD_CORR2 = (!SAV.Digitize.RELAX_THRESH EQ 1) ? 0.50 : 0.65
   
   MaxCorr = !KON.Misc.BADVALUE_REAL
   
   ;------------------------------------------------------------------------
   ; Half the correlation operator rounded down.
   ;------------------------------------------------------------------------
   
   corr_op_half_width = CorrOpWidth / 2
   
   COW_sq = FLOAT(CorrOpWidth[0]) * CorrOpWidth[1]
   COW_sq_inv = 1.0 / COW_sq

   ;------------------------------------------------------------------------
   ; Initialize the correlation window patch coordinates. Then compute some
   ; fixed values for the reference window. Also find where there are any
   ; invalid data points (NAN). This occurs when there are terrain-obscured
   ; holes in the image, but generally only for oblique cameras.
   ;------------------------------------------------------------------------
   
   abeg = corr_op_half_width[1]
   aend = CmpWndwSize[1] - 1 - corr_op_half_width[1]
   cbeg = corr_op_half_width[0]
   cend = CmpWndwSize[0] - 1 - corr_op_half_width[0]
   
   ref_t = RefWndw - (COW_sq_inv * TOTAL(RefWndw, /NAN))
   total_ref_ref = TOTAL(ref_t * ref_t, /NAN)
      
   badndx = WHERE(FINITE(CmpWndw, /NAN), has_bad)
   badndx = 0

   ;------------------------------------------------------------------------
   ; Loop over the possible match positions in the comparison window. These
   ; loops are a major limiting factor in retrieval speed and have been
   ; optimized as much as possible when using the cross-correlation metric.
   ; Try using a much faster metric - see the MISR standard TC_CLOUD product.
   ;------------------------------------------------------------------------
   
   FOR ia1 = abeg, aend DO BEGIN
      FOR ic1 = cbeg, cend DO BEGIN
      
         ;------------------------------------------------------------------
         ; Get region comparable to patch in the comparison window. Eliminate
         ; any points in BOTH patches that are empty in the comparison window.
         ;------------------------------------------------------------------
  
         cmp_wndw = CmpWndw[ic1-cbeg:ic1+cbeg, ia1-abeg:ia1+abeg]

         IF (has_bad) THEN BEGIN
            goodndx = WHERE(~FINITE(cmp_wndw, /NAN) AND $
                            ~FINITE(RefWndw,  /NAN), numgood, $
                            NCOMPLEMENT=numbad)

            IF (numgood EQ 0) THEN BEGIN
               CorrMatrix[ic1,ia1] = 0.0
               CONTINUE
            ENDIF            
            
            IF (numbad GT 0 AND numgood GT 0) THEN BEGIN
               COW_sq_inv = 1.0 / (COW_sq - numbad)
               cmp_wndw = cmp_wndw[goodndx]
               ref_t = RefWndw[goodndx] - $
                       (COW_sq_inv * TOTAL(RefWndw[goodndx]))
               total_ref_ref = TOTAL(ref_t * ref_t)
            ENDIF
         ENDIF
         
         ;------------------------------------------------------------------
         ; PERFORM IMAGE MATCHING. Use IDL array processing function TOTAL.
         ;------------------------------------------------------------------
         
         cmp_t = cmp_wndw - (COW_sq_inv * TOTAL(cmp_wndw, /NAN))
         total_ref_cmp = TOTAL(ref_t * cmp_t, /NAN)
         total_cmp_cmp = TOTAL(cmp_t * cmp_t, /NAN)
         
         CorrMatrix[ic1,ia1] = FLOAT(total_ref_cmp) / $
                               SQRT(total_ref_ref * total_cmp_cmp)
      ENDFOR
   ENDFOR
   
   ref_t = 0
   cmp_t = 0
   cmp_wndw = 0
   total_ref_ref = 0
   total_ref_cmp = 0
   total_cmp_cmp = 0
   
   ;------------------------------------------------------------------------
   ; Reject a grid point if ANY point in the center of the correlation
   ; matrix is bad.
   ;------------------------------------------------------------------------
   
   p_cross = (cend - cbeg + 1)
   p_along = (aend - abeg + 1)
   
   goodndx = WHERE(CorrMatrix GT -999.0, ngood)
   
   IF (ngood NE (p_cross * p_along)) THEN BEGIN
      CorrFailed = 6
      RETURN
   ENDIF
   
   ;------------------------------------------------------------------------
   ; If this is the first pass, then we just find the pixel at the maximum
   ; of correlation matrix rather than interpolating. If offset results are
   ; on the edge of the pass 1 matrix, it's better to let them through and
   ; have pass 2 eliminate them.
   ;------------------------------------------------------------------------
   
   IF (PassNum EQ 1) THEN BEGIN
      frac_pix = 1.0
      SurfaceFit = CorrMatrix[cbeg:cend, abeg:aend]
      
      index_2d = [-99999., -99999.]
      MaxCorr = MAX(SurfaceFit, maxndx)
      index_2d = ARRAY_INDICES(SurfaceFit, maxndx)
      offset_pair = FLOAT(index_2d) + [cbeg, abeg] - CmpPtCenter
 
      ;---------------------------------------------------------------------
      ; If the correlation is poor, eliminate the camera.
      ;---------------------------------------------------------------------
      
      IF (MaxCorr LT MIN_GOOD_CORR1) THEN BEGIN
         CorrFailed = 3
         GOTO, get_clean
      ENDIF
      
   ;---------------------------------------------------------------------------
   ; If this is the second pass, then interpolate the correlation matrix to
   ; subpixel resolution and find the disparity at maximum correlation.
   ;---------------------------------------------------------------------------
      
   ENDIF ELSE BEGIN
   
      ;---------------------------------------------------------------------
      ; INTERPOLATE TO SUB-PIXEL RESOLUTION
      ; For each center, fit a surface and interpolate to find fractional
      ; index where the greatest correlation is present. The fitted output
      ; grid has higher resolution than input. CorrMatrix in; SurfaceFit out.
      ;
      ; Set the size of the window around the best fit pixel for doing
      ; sub-pixel interpolation. This has a significant effect on speed.
      ;
      ; HALF_PASS_2  WindowSize  Comments
      ;      1           5       noticeably poorer coverage
      ;      2           7       nearly as good as 3 and twice as fast
      ;      3           9       best results
      ;---------------------------------------------------------------------
      
      HALF_PASS_2 = 3
      
      csize = SIZE(CorrMatrix)
      
      c1beg = csize[1] / 2 - HALF_PASS_2 - 1
      c1end = csize[1] / 2 + HALF_PASS_2 + 1
      a1beg = csize[2] / 2 - HALF_PASS_2 - 1
      a1end = csize[2] / 2 + HALF_PASS_2 + 1
      
      p1_cross = c1end - c1beg + 1
      p1_along = a1end - a1beg + 1
      
      ;---------------------------------------------------------------------
      ; Bi-cubic interpolation is much faster than and almost as good as
      ; thin-plate spline interpolation.
      ;---------------------------------------------------------------------
      
      nx = p1_cross * !SAV.HtWind.INTERPOLATE_FCTR
      ny = p1_along * !SAV.HtWind.INTERPOLATE_FCTR
      
      SurfaceFit = CONGRID(CorrMatrix[c1beg:c1end,a1beg:a1end], nx, ny, $
                           CUBIC=-0.5, /MINUS_ONE)
         
      MaxCorr = MAX(SurfaceFit, maxndx)
      index_2d = ARRAY_INDICES(SurfaceFit, maxndx)
      offset_pair = (0.5 + index_2d) / FLOAT(!SAV.HtWind.INTERPOLATE_FCTR) - $
                     0.5 + [c1beg, a1beg] - CmpPtCenter

      ;---------------------------------------------------------------------
      ; If offset results are on the edge of the matrix, eliminate them.
      ;---------------------------------------------------------------------
      
      IF (index_2d[0] LE 0 OR index_2d[0] GE nx-1 OR $
          index_2d[1] LE 0 OR index_2d[1] GE ny-1) THEN BEGIN
         CorrFailed = 4
         GOTO, get_clean
      ENDIF

      ;---------------------------------------------------------------------
      ; If the correlation is poor, eliminate the camera.
      ;---------------------------------------------------------------------

      IF (MaxCorr LT MIN_GOOD_CORR2) THEN BEGIN
         CorrFailed = 5
         GOTO, get_clean
      ENDIF
      
   ENDELSE
   
   ;------------------------------------------------------------------------
   ; If requested, show the correlation surface.
   ;------------------------------------------------------------------------
   
   IF (DrawCorrs EQ 1) THEN BEGIN
   
      SCR_SIZE = [!KON.Misc.ScreenX, !KON.Misc.ScreenY]
      x_size = SCR_SIZE[0] * 0.25
      y_size = SCR_SIZE[1] * 0.35
      
      IF (PassNum EQ 1) THEN BEGIN
         x_pos = SCR_SIZE[0]/2 - x_size * 1.5
         y_pos = SCR_SIZE[1]/2 - y_size / 2 + 100
         wndw_ndx = 28
      ENDIF ELSE BEGIN
         x_pos = SCR_SIZE[0]/2 + x_size * 0.5
         y_pos = SCR_SIZE[1]/2 - y_size / 2 - 100
         wndw_ndx = 29
      ENDELSE
      
      xysize = SIZE(SurfaceFit)
      
      WINDOW, wndw_ndx, XPOS=x_pos, YPOS=y_pos, XSIZE=x_size, YSIZE=y_size
      CorrWndw = !D.WINDOW
      
      SURFACE, SurfaceFit, AX=25, AZ=20, $ ; ZAXIS=-1, $
         XRANGE=[0,xysize[1]], YRANGE=[0,xysize[2]], $
         MIN_VALUE=MIN(SurfaceFit), MAX_VALUE=MAX(SurfaceFit), $
         XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
         BACKGROUND=16777215, COLOR=0, BOTTOM=255
         
      IF (PassNum EQ 2) THEN BEGIN
         XYOUTS,0.2,0.95, 'Corr. Coeff. = ' + $
            STRING(FORMAT='(F9.4)',MaxCorr), /NORMAL, COLOR=0
         IF (CorrFailed NE 0) THEN BEGIN
            XYOUTS,0.2,0.93, 'corr failed = ' + STRTRIM(CorrFailed,2), $
               COLOR=255, /NORMAL
         ENDIF ELSE BEGIN
            XYOUTS,0.2,0.93, 'corr succeeded', COLOR=65280, /NORMAL
         ENDELSE
      ENDIF
      
      rtrn = DIALOG_MESSAGE('Continue', /INFO, /CENTER)
      
   ENDIF
   
   ;------------------------------------------------------------------------
   ; If successful, save results.
   ;------------------------------------------------------------------------
   
   IF (CorrFailed EQ 0) THEN BEGIN
      Offsets = offset_pair
      Retval = 0
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------
   
   get_clean:
   ndxs = 0
   index_2d = 0
   badndx = 0
   goodndx = 0
   temp_corr = 0
   SurfaceFit = 0
   offset_pair = 0
   
END  ;  CorrelatePlume

;***************************************************************************
PRO CorrelationSlopeTest, slope_const, fine_slope_thresh, surface_fit, $
                          xy_maxndx, min_slope, corr_failed
;***************************************************************************
; Find the peakedness and monotonicity of slopes on each of 4 sides of the
; maximum point for the interpolated surface:
; 
; 1) Compute the slopes for each of the consecutive, adjacent pixel pairs on
;    either side of the maximum to a distance of 'NUM_FINE_PIX' from the max.
; 2) If the magnitude of any slope is less than the magnitude of the slope
;    adjacent to it in the direction of the max, or if the sign of the slope
;    changes, reject the point.
; 3) Find the minimum slope in the ('NUM_FINE_PIX' - 1) * 4 set. If it is
;    smaller than a dynamic threshold, 'fine_slope_thresh', reject the
;    point.  Do not consider the slopes of the 4 pairs that include the
;    maximum pixel.
;
; Then slopes can be computed up to 'NUM_FINE_PIX' fine pixels from the
; edges of the interpolated correlation matrix without fear of missing a
; peak.  Any peak located within 'NUM_FINE_PIX' pixels of the edge will not
; be tested but should be rejected.
;
; slope_const - A critical factor in determining whether points get rejected
;               based on slope; this is very different for BRF (0.0003) and
;               radiance (0.01).
;---------------------------------------------------------------------------

COMMON config, Cancel

COMPILE_OPT IDL2

   NUM_FINE_PIX = 5
   DBG_PRNT = 0          ; Set to 0 to suppress the print statements.

   first_pixel = 1
   n_pixels = NUM_FINE_PIX - first_pixel
   min_slope = !KON.Misc.LARGE_POS_NUM * 10.0
   fine_slope_thresh = !KON.Misc.LARGE_POS_NUM * 10.0

   ;------------------------------------------------------------------------
   ; Edge test.
   ;------------------------------------------------------------------------

   IF ( xy_maxndx[1] LE NUM_FINE_PIX ) OR $
      ( N_ELEMENTS(surface_fit[0,*]) - 1 - xy_maxndx[1] LE $
        NUM_FINE_PIX ) OR $
      ( xy_maxndx[0] LE NUM_FINE_PIX ) OR $
      ( N_ELEMENTS(surface_fit[*,0]) - 1 - xy_maxndx[0] LE $
        NUM_FINE_PIX ) $
   THEN corr_failed = 1

   ;------------------------------------------------------------------------
   ; Do not proceed if edge test failed.
   ;------------------------------------------------------------------------

   IF (corr_failed EQ 0) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Slope test.  Test one pair of slopes across 3 pixels on each pass of
      ; the FOR loop.  Step by 1 pixel and repeat loop.  (This could be
      ; programmed in a more efficient manner but the gains would be minimal
      ; since not very much computing is going on.)
      ;---------------------------------------------------------------------

      FOR x_pixel = first_pixel, NUM_FINE_PIX-3 DO BEGIN
         FOR y_pixel = first_pixel, NUM_FINE_PIX-3 DO BEGIN
            FOR step = 0, n_pixels-2 DO BEGIN

               ;------------------------------------------------------------
               ; North compass direction (increasing y).
               ;------------------------------------------------------------

               x_ndx = xy_maxndx[0]
               y_ndx = xy_maxndx[1] + y_pixel + step
               pix1 = surface_fit[x_ndx,y_ndx]
               pix2 = surface_fit[x_ndx,(y_ndx += 1)]
               pix3 = surface_fit[x_ndx,(y_ndx += 1)]
               slope1 = pix1 - pix2
               slope2 = pix2 - pix3

               IF ( slope1 LT 0 ) NE ( slope2 LT 0 ) THEN BEGIN
                  corr_failed = 2
                  IF (DBG_PRNT) THEN PRINT,slope1,slope2,pix1,pix2,pix3
               ENDIF

               min_slope = min_slope < ABS( slope1 ) < ABS( slope2 )
               IF (DBG_PRNT) THEN PRINT,min_slope,slope1,slope2,pix1,pix2,pix3

               ;------------------------------------------------------------
               ; East compass direction (increasing x).
               ;------------------------------------------------------------

               x_ndx = xy_maxndx[0] + x_pixel + step
               y_ndx = xy_maxndx[1]
               pix1 = surface_fit[x_ndx,y_ndx]
               pix2 = surface_fit[(x_ndx += 1),y_ndx]
               pix3 = surface_fit[(x_ndx += 1),y_ndx]
               slope1 = pix1 - pix2
               slope2 = pix2 - pix3

               IF ( slope1 LT 0 ) NE ( slope2 LT 0 ) THEN BEGIN
                  IF (DBG_PRNT) THEN PRINT,slope1,slope2,pix1,pix2,pix3
                  corr_failed = 2
               ENDIF

               min_slope = min_slope < ABS( slope1 ) < ABS( slope2 )
               IF (DBG_PRNT) THEN PRINT,min_slope,slope1,slope2,pix1,pix2,pix3

               ;------------------------------------------------------------
               ; South compass direction (decreasing y).
               ;------------------------------------------------------------

               x_ndx = xy_maxndx[0]
               y_ndx = xy_maxndx[1] - y_pixel - step
               pix1 = surface_fit[x_ndx,y_ndx]
               pix2 = surface_fit[x_ndx,(y_ndx -= 1)]
               pix3 = surface_fit[x_ndx,(y_ndx -= 1)]
               slope1 = pix1 - pix2
               slope2 = pix2 - pix3

               IF ( slope1 LT 0 ) NE ( slope2 LT 0 ) THEN BEGIN
                  corr_failed = 2
                  IF (DBG_PRNT) THEN PRINT,slope1,slope2,pix1,pix2,pix3
               ENDIF

               min_slope = min_slope < ABS( slope1 ) < ABS( slope2 )
               IF (DBG_PRNT) THEN PRINT,min_slope,slope1,slope2,pix1,pix2,pix3

               ;------------------------------------------------------------
               ; West compass direction (decreasing x).
               ;------------------------------------------------------------

               x_ndx = xy_maxndx[0] - x_pixel - step
               y_ndx = xy_maxndx[1]
               pix1 = surface_fit[x_ndx,y_ndx]
               pix2 = surface_fit[(x_ndx -= 1),y_ndx]
               pix3 = surface_fit[(x_ndx -= 1),y_ndx]
               slope1 = pix1 - pix2
               slope2 = pix2 - pix3

               IF ( slope1 LT 0 ) NE ( slope2 LT 0 ) THEN BEGIN
                  corr_failed = 2
                  IF (DBG_PRNT) THEN PRINT,slope1,slope2,pix1,pix2,pix3
               ENDIF

               min_slope = min_slope < ABS( slope1 ) < ABS( slope2 )
               IF (DBG_PRNT) THEN PRINT,min_slope,slope1,slope2,pix1,pix2,pix3

            ENDFOR
         ENDFOR
      ENDFOR

      minz = MIN(surface_fit,ndx_min,MAX=maxz,SUBSCRIPT_MAX=ndx_max)
      ndxs_min = ARRAY_INDICES(surface_fit,ndx_min)
      ndxs_max = ARRAY_INDICES(surface_fit,ndx_max)
      dist = SQRT((ndxs_max[0]-ndxs_min[0])*(ndxs_max[0]-ndxs_min[0]) + $
                  (ndxs_max[1]-ndxs_min[1])*(ndxs_max[1]-ndxs_min[1]))

      fine_slope_thresh = slope_const > ((maxz - minz) / $
                                         (dist * !SAV.HtWind.SLOPE_TUNING_FCTR))

      IF (min_slope LT fine_slope_thresh) THEN BEGIN
         corr_failed = 3
         IF (DBG_PRNT) THEN PRINT, min_slope, fine_slope_thresh
      ENDIF

   ENDIF

END  ;  CorrelationSlopeTest

;***************************************************************************
PRO SmoothOffsets, State, Curframe, GridSpace, NxyGrid, MinXGrid, $
                   MaxXGrid, MinYGrid, MaxYGrid, NxyMatch, MinXMatch, $
                   MaxXMatch, MinYMatch, MaxYMatch, NumEdgePts, $
                   Xi, Yi, Xo, Yo, XoNew, YoNew
;***************************************************************************
; Function smooths the along and across offsets from coregistration. Xi, Yi
; are the pixel coordinates in comparison camera where data came from;
; Xo, Yo are the pixel coordinates in comparison camera where data are to be
; moved to; the difference is the offset.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON config, Cancel

   OUTLIER_TUNING_FCTR_MAX = 21.0

   wndw_save = !D.WINDOW
   XoNew = 0
   YoNew = 0

   ;------------------------------------------------------------------------
   ; If a fatal error occurs, detect it, skip it and continue.
   ;------------------------------------------------------------------------

   CATCH, error_status
   IF error_status NE 0 THEN BEGIN
      CATCH, /CANCEL
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Set up parameters and triangulate the Xi,Yi locations.
   ;------------------------------------------------------------------------

   nx_grid = FLOOR((MaxXGrid - MinXGrid) / GridSpace + 1)
   ny_grid = FLOOR((MaxYGrid - MinYGrid) / GridSpace + 1)

   TRIANGULATE, Xi, Yi, triangles, bound

   ;------------------------------------------------------------------------
   ; Grid the input points onto a regular grid for the across and along 
   ; offsets and display.
   ;------------------------------------------------------------------------

   off_surf_cross = TRIGRID(Xi, Yi, Xi-Xo, triangles, [GridSpace,GridSpace], $
                            [MinXGrid,MinYGrid,MaxXGrid,MaxYGrid], $
                            NX=nx_grid, NY=ny_grid, XGRID=xgrid, YGRID=ygrid)

   off_surf_along = TRIGRID(Xi, Yi, Yi-Yo, triangles, [GridSpace,GridSpace], $
                            [MinXGrid,MinYGrid,MaxXGrid,MaxYGrid], $
                            NX=nx_grid, NY=ny_grid)

   ;------------------------------------------------------------------------
   ; If requested, draw surfaces representing various stages in the smoothing
   ; process. Repeat SURFACE command 4 times to draw Zaxis at all 4 corners.
   ;------------------------------------------------------------------------

   IF (!VAR.DEBUG_FLAG AND !SAV.HtWind.SHOW_SMOOTHING EQ 1) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Draw the raw across misregistration offsets for this camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=50, YPOS=700, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
                    ' - Raw Coregistration Offsets - Across Track'
      new_wndw1 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_cross)) < (-2.)
      zmax =  (MAX(off_surf_cross)) > 2.

      SURFACE, REVERSE(off_surf_cross,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_cross,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255

      ;---------------------------------------------------------------------
      ; Draw the raw along misregistration offsets for this camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=650, YPOS=700, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
                    ' - Raw Coregistration Offsets - Along Track'
      new_wndw2 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_along)) < (-2.)
      zmax = CEIL (MAX(off_surf_along)) > 2.

      SURFACE, REVERSE(off_surf_along,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_along,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax],$
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255
   ENDIF

   ;------------------------------------------------------------------------
   ; Smooth the offset across and along surfaces. These are used as a base
   ; from which outliers are removed.
   ;------------------------------------------------------------------------

   off_surf_cross = SMOOTH(off_surf_cross, !SAV.HtWind.SMOOTH_FLTR_SIZE_P1, $
                           /EDGE_TRUNCATE)
   off_surf_along = SMOOTH(off_surf_along, !SAV.HtWind.SMOOTH_FLTR_SIZE_P1, $
                           /EDGE_TRUNCATE)

   IF (!VAR.DEBUG_FLAG AND !SAV.HtWind.SHOW_SMOOTHING EQ 1) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Draw smoothed across misregistration offsets for this camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=50, YPOS=375, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
              ' - Smoothed Raw Offsets for Finding Outliers - Across Track'
      new_wndw3 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_cross)) < (-2.)
      zmax = CEIL (MAX(off_surf_cross)) > 2.

      SURFACE, REVERSE(off_surf_cross,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_cross,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax],$
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255

      ;---------------------------------------------------------------------
      ; Draw smoothed along misregistration offsets for this camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=650, YPOS=375, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
              ' - Smoothed Raw Offsets for Finding Outliers - Along Track'
      new_wndw4 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_along)) < (-2.)
      zmax = CEIL (MAX(off_surf_along)) >   2.

      SURFACE, REVERSE(off_surf_along,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_along,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax],$
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255
   ENDIF

   ;------------------------------------------------------------------------
   ; Recover smoothed values at the input points.
   ;------------------------------------------------------------------------

   Xo_sm = FLTARR((SIZE(Xo))[1])
   Yo_sm = FLTARR((SIZE(Xo))[1])

   FOR ipts = 0, NxyGrid-1 DO BEGIN
      xndx = WHERE(ABS(xgrid-Xi[ipts]) LT 2, xnumgood)
      yndx = WHERE(ABS(ygrid-Yi[ipts]) LT 2, ynumgood)

      IF (xnumgood EQ 1 AND ynumgood EQ 1) THEN BEGIN
         Xo_sm[ipts] = Xi[ipts] - off_surf_cross[xndx,yndx]
         Yo_sm[ipts] = Yi[ipts] - off_surf_along[xndx,yndx]
       ENDIF
   ENDFOR

   xndx = 0
   yndx = 0

   ;------------------------------------------------------------------------
   ; Filter out any points that exceed a reasonable distance from the
   ; smoothed surface. Move them to the smoothed surface. Leave non-outlier
   ; points at the end of the arrays and == 0.0.
   ;------------------------------------------------------------------------

   IF (!SAV.HtWind.OUTLIER_TUNING_FCTR LT OUTLIER_TUNING_FCTR_MAX) THEN BEGIN

      outlier_thresh_cross = 0.3 > (!SAV.HtWind.OUTLIER_TUNING_FCTR * $
                                    STDDEV(Xi-Xo))
      outlier_thresh_along = 0.3 > (!SAV.HtWind.OUTLIER_TUNING_FCTR * $
                                    STDDEV(Yi-Yo))

      ndxgood = WHERE(((ABS(Xo-Xo_sm) LT outlier_thresh_cross) AND $
                       (ABS(Yo-Yo_sm) LT outlier_thresh_along)) OR $
                       (Xo_sm EQ 0.0 AND Yo_sm EQ 0.0), numgood)

      IF (numgood GT NumEdgePts) THEN BEGIN
         Xi = Xi[ndxgood]
         Yi = Yi[ndxgood]
         Xo = Xo[ndxgood]
         Yo = Yo[ndxgood]
         NxyMatch = numgood - NumEdgePts
      ENDIF

   ENDIF

   off_surf_cross = 0
   off_surf_along = 0
   Xo_sm = 0
   Yo_sm = 0
   ndxgood = 0
   nx_grid = 0
   ny_grid = 0

   ;------------------------------------------------------------------------
   ; Next regrid and smooth the outlier-free surfaces.
   ;------------------------------------------------------------------------

   nx_grid = FLOOR((MaxXMatch - MinXMatch) / GridSpace + 1)
   ny_grid = FLOOR((MaxYMatch - MinYMatch) / GridSpace + 1)

   IF (NxyMatch LT 4) THEN BEGIN
      CATCH, /CANCEL
      nx_grid = 0
      ny_grid = 0     
      RETURN
   ENDIF

   TRIANGULATE, Xi[0:NxyMatch-1], Yi[0:NxyMatch-1], triangles, bound

   off_surf_cross = TRIGRID(Xi[0:NxyMatch-1], Yi[0:NxyMatch-1], $
                            Xi[0:NxyMatch-1]-Xo[0:NxyMatch-1], $
                            triangles, [GridSpace,GridSpace], $
                            [MinXMatch,MinYMatch,MaxXMatch,MaxYMatch], $
                            NX=nx_grid, NY=ny_grid, $
                            XGRID=xgrid, YGRID=ygrid)

   IF (!SAV.HtWind.SMOOTH_FLTR_SIZE_P2 GE 3) THEN $
      off_surf_cross = SMOOTH(off_surf_cross, !SAV.HtWind.SMOOTH_FLTR_SIZE_P2, $
                              /EDGE_TRUNCATE)

   off_surf_along = TRIGRID(Xi[0:NxyMatch-1], Yi[0:NxyMatch-1], $
                            Yi[0:NxyMatch-1]-Yo[0:NxyMatch-1], $
                            triangles, [GridSpace,GridSpace], $
                            [MinXMatch,MinYMatch,MaxXMatch,MaxYMatch], $
                            NX=nx_grid, NY=ny_grid)

   IF (!SAV.HtWind.SMOOTH_FLTR_SIZE_P2 GE 3) THEN $
      off_surf_along = SMOOTH(off_surf_along, !SAV.HtWind.SMOOTH_FLTR_SIZE_P2, $
                              /EDGE_TRUNCATE)

   ;------------------------------------------------------------------------
   ; Recover smoothed values at the input points again.
   ;------------------------------------------------------------------------

   Xo_sm = FLTARR((SIZE(Xo))[1])
   Yo_sm = FLTARR((SIZE(Xo))[1])

   FOR ipts = 0, NxyMatch-1 DO BEGIN
      xndx = WHERE(ABS(xgrid-Xi[ipts]) LT 2, xnumgood)
      yndx = WHERE(ABS(ygrid-Yi[ipts]) LT 2, ynumgood)

      IF (xnumgood EQ 1 AND ynumgood EQ 1) THEN BEGIN
         Xo_sm[ipts] = Xi[ipts] - off_surf_cross[xndx,yndx]
         Yo_sm[ipts] = Yi[ipts] - off_surf_along[xndx,yndx]
      ENDIF
   ENDFOR

   xndx = 0
   yndx = 0

   ;------------------------------------------------------------------------
   ; Get the points on the MISR data edges and add to the list.
   ; NOTE - FillWithNearest increments the value of NxyMatch on return.
   ;------------------------------------------------------------------------

   nxy1 = NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch
   FillWithNearest, nxy1, NumEdgePts, State.sizey-1, Xi, Yi, Xo_sm, Yo_sm, NxyMatch

   ;------------------------------------------------------------------------
   ; Display the residual across and along registration offsets to be
   ; removed if requested.
   ;------------------------------------------------------------------------

   IF (!VAR.DEBUG_FLAG AND !SAV.HtWind.SHOW_SMOOTHING EQ 1) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Regrid the outlier-free surfaces a final time for display only. Use
      ; the corrected locations as the references.
      ;---------------------------------------------------------------------

      nx_grid = FLOOR((MaxXMatch - MinXMatch) / GridSpace + 1)
      ny_grid = FLOOR((MaxYMatch - MinYMatch) / GridSpace + 1)

      TRIANGULATE, Xi, Yi, triangles, bound

      off_surf_cross = TRIGRID(Xi, Yi, Xi-Xo_sm, triangles, $
                               [GridSpace,GridSpace], $
                               [MinXMatch,MinYMatch,MaxXMatch,MaxYMatch], $
                               NX=nx_grid, NY=ny_grid, $
                               XGRID=xgrid, YGRID=ygrid)

      off_surf_along = TRIGRID(Xi, Yi, Yi-Yo_sm, triangles, $
                               [GridSpace,GridSpace], $
                               [MinXMatch,MinYMatch,MaxXMatch,MaxYMatch], $
                               NX=nx_grid, NY=ny_grid)

      ;---------------------------------------------------------------------
      ; Draw smoothed, outlier-free across misregistration offsets for this
      ; camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=50, YPOS=50, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
              ' - Residual Outlier-Free Offsets to Remove - Across Track'
      new_wndw5 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_cross)) < (-2.)
      zmax = CEIL (MAX(off_surf_cross)) >   2.

      SURFACE, REVERSE(off_surf_cross,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_cross,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255

      ;---------------------------------------------------------------------
      ; Draw smoothed, outlier-free along misregistration offsets for this
      ; camera.
      ;---------------------------------------------------------------------

      WINDOW, XPOS=650, YPOS=50, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='An-' + !KON.Instr.camera_names[Curframe] + $
              ' - Residual Outlier-Free Offsets to Remove - Along Track'
      new_wndw6 = !D.WINDOW

      zmin = FLOOR(MIN(off_surf_along)) < (-2.)
      zmax = CEIL (MAX(off_surf_along)) >   2.

      SURFACE, REVERSE(off_surf_along,2), $
               AX=35, AZ=25, XSTYLE=1, YSTYLE=1,ZSTYLE=1,  ZAXIS=1, $
               XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               MIN_VALUE=zmin_along, MAX_VALUE=zmax_along, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(off_surf_along,2), /NOERASE, $
                  AX=35, AZ=25, XSTYLE=1, YSTYLE=1,ZSTYLE=1,  ZAXIS=irpt, $
                  XRANGE=[1,nx_grid], YRANGE=[1,ny_grid], ZRANGE=[zmin,zmax], $
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255
   ENDIF

   ;------------------------------------------------------------------------
   ; Cancel the error detection.
   ;------------------------------------------------------------------------

   CATCH, /CANCEL

   ;------------------------------------------------------------------------
   ; Compute what the corrected X and Y positions will be after correction.
   ; Then compute what the offsets will look like after correction,
   ; neglecting the points that were removed as outliers. This last is only
   ; for display purposes in the caller.
   ;------------------------------------------------------------------------

   XoNew = Xo - Xo_sm
   YoNew = Yo - Yo_sm

   Xo = Xo_sm
   Yo = Yo_sm

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   off_surf_cross = 1
   off_surf_along = 1
   bound = 0
   Xo_sm = 0
   Yo_sm = 0
   triangles = 0
   ndxgood = 0
   ndxbad = 0

   IF (!VAR.DEBUG_FLAG AND !SAV.HtWind.SHOW_SMOOTHING EQ 1) THEN BEGIN
      mssg = $
         ['To delete these 6 plot windows but show more plots, click "Yes".',$
          'To delete these windows and suppress all plots of this type', $
          'during your current MINX session, click "No".']
      ival = DIALOG_MESSAGE(mssg, /QUESTION)
      IF (STRUPCASE(ival) EQ 'NO') THEN !SAV.HtWind.SHOW_SMOOTHING = 0

      SafeWDELETE, new_wndw1, didit
      SafeWDELETE, new_wndw2, didit
      SafeWDELETE, new_wndw3, didit
      SafeWDELETE, new_wndw4, didit
      SafeWDELETE, new_wndw5, didit
      SafeWDELETE, new_wndw6, didit
      SafeWSET, wndw_save, didit
   ENDIF

END  ;  SmoothOffsets

;***************************************************************************
PRO FillWithNearest, Num_xy, NumEdgePts, MaxY, Xi, Yi, Xo, Yo, Nxy
;***************************************************************************
; Function finds the location from among the valid data points that is
; nearest to the passed point and copies its offset.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

   min_dist = !KON.Misc.LARGE_POS_NUM
   min_ndx  = !KON.Misc.LARGE_NEG_NUM

   ;------------------------------------------------------------------------
   ; Find the point that is nearest to the passed point.
   ;------------------------------------------------------------------------

   dx = Xi[Nxy] - Xi[0:Num_xy-1]
   dy = Yi[Nxy] - Yi[0:Num_xy-1]
   sqrt_x2y2 = dx * dx + dy * dy

   sqrt_x2y2 = SQRT(sqrt_x2y2)
   min_dist = MIN(sqrt_x2y2, min_ndx)

   sqrt_x2y2 = 0

   ;------------------------------------------------------------------------
   ; If we found a point, then copy its offset into the passed point.
   ;------------------------------------------------------------------------

   IF (min_ndx GE 0) THEN BEGIN

      Xo[Nxy] = Xi[Nxy] - Xi[min_ndx] + Xo[min_ndx]
      Yo[Nxy] = Yi[Nxy] - Yi[min_ndx] + Yo[min_ndx]

      IF (Yo[Nxy] LT 0.0) THEN Yo[Nxy] = 0.0
      IF (Yo[Nxy] GT MaxY) THEN Yo[Nxy] = MaxY

   ENDIF

   Nxy = Nxy + 1

END  ;  FillWithNearest

;***************************************************************************
PRO GetWarpPoints, Auto, FirstPass, State, Curframe, Nxy, $
                   DoQuintic, Xi, Yi, Xo, Yo, Retval, whichorbit
;***************************************************************************
; Function uses cross-correlation to determine, at various control pixels in
; the camera image, to what nearby pixel the control pixel would need to be
; moved to minimize the camera co-registration error relative to the nadir
; camera. Use the hi-resolution red band.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct
COMMON config, Cancel
COMMON agp_data, Land_Water_Mask, Terrain_Hts

   MIN_REQ_MATCH_RATIO = 0.015
   DRAW_CORR_PLOTS = 0

   Cancel = 0
   AutoCam = Auto

   NumEdgePts  = 10
   NumNewSurfX = 64
   NumNewSurfY = 32
   display_every_nth = 10
   
   SetConfigValues, AutoCam, State.wTopWorkBase, CurFrame, $
                    CoordStruct.(whichorbit).OrbitNum

   IF (~ FirstPass) THEN !SAV.HtWind.CORR_OP_WIDTH += 4

   IF ( Cancel EQ 1 ) THEN BEGIN
      Retval = -2
      RETURN
   ENDIF

   WIDGET_CONTROL, /HOURGLASS

   Retval = -1                 ; returned status code
   Nxy = 0                     ; number of successful points returned

   ;------------------------------------------------------------------------
   ; Determine reference camera. It will be the An of orbit 1 if there is 1
   ; orbit or if we're correcting the An of a 2nd orbit. Otherwise it will
   ; be the An of the 2nd orbit.
   ;------------------------------------------------------------------------

   IF (Curframe LE 9 OR Curframe EQ 14) THEN BEGIN
      AnCam = 4
   ENDIF ELSE BEGIN
      AnCam = 13
   ENDELSE

   ;------------------------------------------------------------------------
   ; If requested, create a file to save the offsets by grid location.
   ;------------------------------------------------------------------------

   IF (!VAR.CurrFiles.PRINT_FILENAME NE '') THEN BEGIN
      OPENW,  unit, !VAR.CurrFiles.PRINT_FILENAME, /GET_LUN
      PRINTF, unit, 'Block Line Sample LOffset SOffset NumCam MchCond'
      PRINTF, unit, '----- ---- ------ ------- ------- ------ -------'
      wndw_coord = FLTARR(2,1)
   ENDIF

   ;------------------------------------------------------------------------
   ; Find the first and last rows in this camera's swath that have valid
   ; data by scanning the pixels from a column in the center of the swath.
   ;------------------------------------------------------------------------

   ndxs = WHERE(GetRawImage(State.sizex/2, State.sizex/2, 0, $
                State.sizey-1, 2, Curframe-1, !KON.Instr.HI_RES_PIX_SIZE, $
                !KON.Misc.INTERP_TYPE_SAMP) GT 0, numndxs)

   IF (numndxs EQ 0) THEN BEGIN
      ndxs = 0
      RETURN
   ENDIF
   
   beg_y = ndxs[0]
   end_y = ndxs[numndxs-1]
   
   ndxs = 0

   ;------------------------------------------------------------------------
   ; Find the first and last columns in this camera's swath that have valid
   ; data using the valid rows discovered above. These define the area in
   ; which matches are attempted. Do this over 25 rows of data on the top
   ; and bottom so a 2nd pass of corrections doesn't find null values there
   ; in case the image has been squeezed in.
   ;------------------------------------------------------------------------
   
   minx1 = MIN(WHERE(GetRawImage(0, State.sizex-1, beg_y, beg_y+25, 2, $
                     Curframe-1, !KON.Instr.HI_RES_PIX_SIZE, $
                     !KON.Misc.INTERP_TYPE_SAMP) GT 0), MAX=maxx1)
   minx2 = MIN(WHERE(GetRawImage(0, State.sizex-1, end_y-25, end_y, 2, $
                     Curframe-1, !KON.Instr.HI_RES_PIX_SIZE, $
                     !KON.Misc.INTERP_TYPE_SAMP) GT 0), MAX=maxx2)

   min_x = minx1 < minx2
   max_x = (ARRAY_INDICES([State.sizex,State.sizey,3], maxx1, /DIMENSIONS))[0] > $
           (ARRAY_INDICES([State.sizex,State.sizey,3], maxx2, /DIMENSIONS))[0]

   along_beg_grid = !SAV.HtWind.CORR_WNDW_SEP

   along_end_grid = State.sizey / !SAV.HtWind.CORR_WNDW_SEP * $
                    !SAV.HtWind.CORR_WNDW_SEP
   IF (along_end_grid GE State.sizey-1) THEN $
      along_end_grid -= !SAV.HtWind.CORR_WNDW_SEP

   cross_beg_grid = min_x / !SAV.HtWind.CORR_WNDW_SEP * $
                    !SAV.HtWind.CORR_WNDW_SEP
   IF (cross_beg_grid LT min_x) THEN $
      cross_beg_grid += !SAV.HtWind.CORR_WNDW_SEP

   cross_end_grid = max_x / !SAV.HtWind.CORR_WNDW_SEP * $
                    !SAV.HtWind.CORR_WNDW_SEP
   IF (cross_end_grid GT max_x) THEN $
      cross_end_grid -= !SAV.HtWind.CORR_WNDW_SEP

   nnx = (cross_end_grid - cross_beg_grid) / !SAV.HtWind.CORR_WNDW_SEP + 1
   nny = (along_end_grid - along_beg_grid) / !SAV.HtWind.CORR_WNDW_SEP + 1

   NxyGrid0 = nnx * nny

   Xi = FLTARR(NxyGrid0)
   Yi = FLTARR(NxyGrid0)
   Xo = FLTARR(NxyGrid0)
   Yo = FLTARR(NxyGrid0)

   NxyGrid = 0

   corr_matrix = FLTARR(!SAV.HtWind.CORR_OP_WIDTH, $
                        !SAV.HtWind.CORR_OP_WIDTH) + !KON.Misc.BADVALUE_REAL
   corr_num = 0
   row_num  = 0
   xsave    = -999

   ;------------------------------------------------------------------------
   ; Loop over the potential set of input x,y pixel coordinates to use as
   ; centers for the cross-correlation windows in reference camera (An) and
   ; comparison camera. Loop in reverse order for along direction, because
   ; screen pixels start at the bottom, while we need increasing order for
   ; MISR pixels in offset file.
   ;------------------------------------------------------------------------

   FOR ialong = along_end_grid, $
                along_beg_grid, -!SAV.HtWind.CORR_WNDW_SEP DO BEGIN
      FOR icross = cross_beg_grid, $
                   cross_end_grid, !SAV.HtWind.CORR_WNDW_SEP DO BEGIN

         offsets = [0.0, 0.0]

         ;------------------------------------------------------------------
         ; If this is a mis-registration correction for height determination,
         ; only use points over land.
         ;------------------------------------------------------------------

         IF (!SAV.HtWind.CORR_TARGET GE 1 AND $
             !SAV.HtWind.CORR_TARGET LE 3) THEN BEGIN
            agp_cross = ((icross + 2) / 4) < (State.sizex / 4 - 1)
            agp_along = ((ialong + 2) / 4) < (State.sizey / 4 - 1)

            ; Bypass next test if it's essential to use sea ice
            ; to match cameras during mis-registration correction.
            ;   0 = Shallow Ocean, 1 = Land, 2 = Coastline,
            ;   3 = Shallow Inland Water, 4 = Ephemeral Water,
            ;   5 = Deep Inland Water, 6 = Deep Ocean

            IF (Land_Water_Mask[agp_cross, agp_along] EQ 0 OR $
                Land_Water_Mask[agp_cross, agp_along] EQ 3 OR $
                Land_Water_Mask[agp_cross, agp_along] EQ 5 OR $
                Land_Water_Mask[agp_cross, agp_along] EQ 6) THEN CONTINUE
         ENDIF

         corr_num = corr_num + 1

         ;------------------------------------------------------------------
         ; Correlate to find the best fitting offsets and fit a finer grid
         ; to the correlation surface.
         ;------------------------------------------------------------------

         CorrelatePatch, Curframe, AnCam, ialong, icross, $
                         !SAV.HtWind.REF_WNDW_SIZE / 2, corr_matrix, $
                         offsets, surface_fit, max_corr, mean_abs_dev, $
                         min_slope, fine_slope_thresh, corr_failed

         ;------------------------------------------------------------------
         ; If requested, display the correlation surfaces.
         ;------------------------------------------------------------------

         IF (DRAW_CORR_PLOTS EQ 1 AND $
             corr_num MOD display_every_nth EQ 0) THEN BEGIN

            wndw_save = !D.WINDOW
            WINDOW, XPOS=650, YPOS=450, /FREE
            new_wndw = !D.WINDOW

            SURFACE, surface_fit, AX=15, XCHARSIZE=2, YCHARSIZE=2, $
                     ZCHARSIZE=2

            XYOUTS, 0.2, 0.95, 'Pixel x/y    = ' + $
                    STRTRIM(STRING(icross),2) + '/' + $
                    STRTRIM(STRING(ialong),2), /NORMAL
            XYOUTS, 0.2, 0.89, 'Max.Abs.Dev. = ' + $
                    STRTRIM(mean_abs_dev,2), /NORMAL
            XYOUTS, 0.2, 0.87, 'MAD Thresh.  = ' + $
                    STRTRIM(!SAV.HtWind.MEANABSDEV_THRESH,2), /NORMAL
            XYOUTS, 0.2, 0.85, 'Min. Slope   = ' + $
                    STRTRIM(min_slope,2), /NORMAL
            XYOUTS, 0.2, 0.83, 'Slope Thresh.= ' + $
                    STRTRIM(fine_slope_thresh,2), /NORMAL
            IF (corr_failed NE 0) THEN BEGIN
               XYOUTS,0.2,0.81, 'corr failed = ' + STRTRIM(corr_failed,2), $
               COLOR=255, /NORMAL
            ENDIF ELSE BEGIN
               XYOUTS,0.2,0.81, 'corr succeeded', COLOR=65280, /NORMAL
            ENDELSE

            IF (Auto NE 1) THEN BEGIN
               ival = DIALOG_MESSAGE('Press OK to continue.', /INFORMATION, $
                                     /CENTER)
            ENDIF ELSE BEGIN
               WAIT, 1
            ENDELSE

            SafeWDELETE, new_wndw, didit
            SafeWSET, wndw_save, didit
         ENDIF

         ;------------------------------------------------------------------
         ; If requested, print the grid point location and the fractional
         ; along and across offsets.
         ;------------------------------------------------------------------

         IF (!VAR.CurrFiles.PRINT_FILENAME NE '') THEN BEGIN
            wndw_coord[0,0] = icross
            wndw_coord[1,0] = ialong
            WndwCrdToMisrCrd, State.Curframe, wndw_coord, misr_coord, $
                              Retval1

            PRINTF, unit, FORMAT='(i4,i5,i7,f8.1,f8.1,a6,i7)', $
                       FIX(misr_coord[2,0]), FIX(misr_coord[1,0]), $
                       FIX(misr_coord[0,0]), offsets[1], offsets[0], $
                       '    NA', (corr_failed GT 0) ? 0 : 1
         ENDIF

         IF (corr_failed NE 0) THEN CONTINUE

         ;------------------------------------------------------------------
         ; Increment the count of successful correlations and fill in the
         ; values of the starting correlation centers and where they should
         ; be moved to.
         ;------------------------------------------------------------------

         Xi[NxyGrid] = icross
         Yi[NxyGrid] = ialong
         Xo[NxyGrid] = FLOAT(icross) - offsets[0]
         Yo[NxyGrid] = FLOAT(ialong) - offsets[1]

         NxyGrid = NxyGrid + 1
      ENDFOR
   ENDFOR

   IF (!VAR.CurrFiles.PRINT_FILENAME NE '') THEN FREE_LUN, unit

   Nxy = NxyGrid
   IF (NxyGrid / FLOAT(NxyGrid0) LT MIN_REQ_MATCH_RATIO) THEN BEGIN
      corr_matrix = 0
      wndw_coord = 0
      surface_fit = 0
      Xi = 0
      Yi = 0
      Xo = 0
      Yo = 0
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Truncate the arrays after the last good point. Add points on the edge
   ; of the valid data area (the MISR parallelogram).
   ;------------------------------------------------------------------------

   Xi = Xi[0:NxyGrid-1]
   Yi = Yi[0:NxyGrid-1]
   Xo = Xo[0:NxyGrid-1]
   Yo = Yo[0:NxyGrid-1]

   ;------------------------------------------------------------------------
   ; Find the beginning and ending across coordinates for the regrid and
   ; final interpolation process. These create a grid slightly larger than
   ; the matcher grid, so it includes the entire valid data area. Do this so
   ; there are points on the very edge of the data area that we can copy the
   ; nearest good correlation values to.
   ;------------------------------------------------------------------------

   along_beg_match = 0.0
   along_end_match = State.sizey - 1.0
   cross_beg_match = (cross_beg_grid - !SAV.HtWind.CORR_WNDW_SEP) > 0
   cross_end_match = (cross_end_grid + !SAV.HtWind.CORR_WNDW_SEP)
   IF (cross_end_match GT State.sizex-1) THEN $
      cross_end_match -= !SAV.HtWind.CORR_WNDW_SEP

   xy = FLTARR(NumEdgePts)
   Xi = [Xi,xy]
   Yi = [Yi,xy]
   Xo = [Xo,xy]
   Yo = [Yo,xy]
   xy = 0

   NxyMatch = NxyGrid

   Xi[NxyMatch] = cross_beg_match
   Yi[NxyMatch] = along_beg_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_beg_match + nnx / 3 * !SAV.HtWind.CORR_WNDW_SEP
   Yi[NxyMatch] = along_beg_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_beg_match + 2 * nnx / 3 * !SAV.HtWind.CORR_WNDW_SEP
   Yi[NxyMatch] = along_beg_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_end_match
   Yi[NxyMatch] = along_beg_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch

   Xi[NxyMatch] = cross_beg_match
   Yi[NxyMatch] = along_end_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_beg_match + nnx / 3 * !SAV.HtWind.CORR_WNDW_SEP
   Yi[NxyMatch] = along_end_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_beg_match + 2 * nnx / 3 * !SAV.HtWind.CORR_WNDW_SEP
   Yi[NxyMatch] = along_end_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_end_match
   Yi[NxyMatch] = along_end_match
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch

   Xi[NxyMatch] = cross_beg_match
   Yi[NxyMatch] = nny / 2 * !SAV.HtWind.CORR_WNDW_SEP
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch
   Xi[NxyMatch] = cross_end_match
   Yi[NxyMatch] = nny / 2 * !SAV.HtWind.CORR_WNDW_SEP
   FillWithNearest, NxyGrid, NumEdgePts, State.sizey-1, Xi, Yi, Xo, Yo, NxyMatch

   ;------------------------------------------------------------------------
   ; Apply smoothing filters to the remaining data points. Use it to filter
   ; out any points that exceed a reasonable distance from the smoothed
   ; surface. The returned values Xo, Yo are new pixel coordinates where the
   ; image is to be warped to.
   ;------------------------------------------------------------------------

   SmoothOffsets, State, Curframe, !SAV.HtWind.CORR_WNDW_SEP, $
                  NxyGrid, cross_beg_grid, cross_end_grid, $
                           along_beg_grid, along_end_grid, $
                  NxyMatch, cross_beg_match, cross_end_match, $
                            along_beg_match, along_end_match, $
                  NumEdgePts, Xi, Yi, Xo, Yo, XoNew, YoNew

   ;------------------------------------------------------------------------
   ; If requested, display a fit to all the successful offset points for both
   ; across and along. Repeat SURFACE command 4 times to draw Zaxis at all 4
   ; corners.
   ;------------------------------------------------------------------------

   IF (!VAR.DEBUG_FLAG AND !SAV.HtWind.SHOW_OFFSET_SURF EQ 1) THEN BEGIN
      wndw_save = !D.WINDOW

      WINDOW, XPOS=50, YPOS=50, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='Final An-' + !KON.Instr.camera_names[Curframe] + $
                 ' - Coreg. Offsets Remaining after Correction - Across Track'
      new_wndw1 = !D.WINDOW

      TRIANGULATE, Xi, Yi, triangles

      IF (DoQuintic EQ 1) THEN BEGIN
         new_surface = TRIGRID(Xi, Yi, XoNew, triangles, $
                               NX=NumNewSurfX, NY=NumNewSurfY, /QUINTIC)
      ENDIF ELSE BEGIN
         new_surface = TRIGRID(Xi, Yi, XoNew, triangles, $
                               NX=NumNewSurfX, NY=NumNewSurfY)
      ENDELSE

      zmin = FLOOR(MIN(Xi-Xo)) < (-2.)
      zmax = CEIL (MAX(Xi-Xo)) >   2.

      SURFACE, REVERSE(new_surface,2), AX=35, AZ=25, $
               XRANGE=[0,NumNewSurfX], YRANGE=[0,NumNewSurfY], $
               ZRANGE=[zmin,zmax], $
               XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(new_surface,2), AX=35, AZ=25, /NOERASE, $
                  XRANGE=[0,NumNewSurfX], YRANGE=[0,NumNewSurfY], $
                  ZRANGE=[zmin,zmax], $
                  XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255

      WINDOW, XPOS=650, YPOS=50, XSIZE=550, YSIZE=280, /FREE, $
              TITLE='Final An-' + !KON.Instr.camera_names[Curframe] + $
                  ' - Coreg. Offsets Remaining after Correction - Along Track'
      new_wndw2 = !D.WINDOW

      TRIANGULATE, Xi, Yi, triangles

      IF (DoQuintic EQ 1) THEN BEGIN
         new_surface = TRIGRID(Xi, Yi, YoNew, triangles, $
                               NX=NumNewSurfX, NY=NumNewSurfY, /QUINTIC)
      ENDIF ELSE BEGIN
         new_surface = TRIGRID(Xi, Yi, YoNew, triangles, $
                               NX=NumNewSurfX, NY=NumNewSurfY)
      ENDELSE

      zmin = FLOOR(MIN(Yi-Yo)) < (-2.)
      zmax = CEIL (MAX(Yi-Yo)) >   2.

      SURFACE, REVERSE(new_surface,2), AX=35, AZ=25, $
               XRANGE=[0,NumNewSurfX], YRANGE=[0,NumNewSurfY], $
               ZRANGE=[zmin,zmax], $
               XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=1, $
               XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
               XTITLE='Sample Direction', YTITLE='Line Direction', $
               BACKGROUND=16777215, COLOR=0, BOTTOM=255
      FOR irpt=2,4 DO $
         SURFACE, REVERSE(new_surface,2), AX=35, AZ=25, /NOERASE, $
                  XRANGE=[0,NumNewSurfX], YRANGE=[0,NumNewSurfY], $
                  ZRANGE=[zmin,zmax], $
                  XSTYLE=1, YSTYLE=1, ZSTYLE=1, ZAXIS=irpt, $
                  XCHARSIZE=3, YCHARSIZE=3, ZCHARSIZE=3, $
                  BACKGROUND=16777215, COLOR=0, BOTTOM=255

      mssg = $
         ['To delete these 2 plot windows but show more plots, click "Yes".', $
          'To delete these windows and suppress all plots of this type', $
          'during your current MINX session, click "No".']
      ival = DIALOG_MESSAGE(mssg, /QUESTION)
      IF (STRUPCASE(ival) EQ 'NO') THEN !SAV.HtWind.SHOW_OFFSET_SURF = 0

      SafeWDELETE, new_wndw1, didit
      SafeWDELETE, new_wndw2, didit
      SafeWSET, wndw_save, didit

   ENDIF

   Nxy = NxyMatch

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   corr_matrix = 0
   wndw_coord = 0
   surface_fit = 0
   new_surface = 0
   triangles = 0
   XoNew = 0
   YoNew = 0

   ; There are 10 new edge points - remove them first.
   
   IF ((Nxy - 10) / FLOAT(NxyGrid0) LT MIN_REQ_MATCH_RATIO) THEN BEGIN
      Xi = 0
      Yi = 0
      Xo = 0
      Yo = 0
      RETURN  ; this returns Retval = -1
   ENDIF

   Retval = 0

END  ;  GetWarpPoints

;***************************************************************************
PRO Op_FixCoregError, Auto, FirstPass, State, Curframe, ResetAn, TargetVal, $
                      NumBand, Nxy, MeanAcross, MeanAlong, RMSdevAcross, $
                      RMSdevAlong, Retval
;***************************************************************************
; Routine warps blocks of L1B2 data to minimize the coregistration error.
; The camera's data are compared to the nadir image by cross-correlation in
; several regions.  The point of maximum correlation becomes the location
; where the correlation center is moved by the warp routine.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

   Retval = -1
   retval1 = -1
   whichorbit = (Curframe GT 9)
   !SAV.HtWind.CORR_TARGET = TargetVal

   MeanAcross   = 0.0
   MeanAlong    = 0.0
   RMSdevAcross = 0.0
   RMSDevAlong  = 0.0
   
   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   ResetSwathData

   ;------------------------------------------------------------------------
   ; Load land/water mask and digital elevation data for all the blocks that
   ; are present if not already loaded.
   ;------------------------------------------------------------------------

   LoadAGPData, State, retval2

   IF (retval2 NE 0) THEN BEGIN
      Retval = -9
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Set the font.
   ;------------------------------------------------------------------------
   
   old_font = GetFontInfo(0)
   SetFontInfo, {Type:!KON.FontTyp.FONT_VECTOR, Size:'12', Face:'bold'}
   
   ;------------------------------------------------------------------------
   ; Derive the vectors that specify the x and y control points for the
   ; input and warped output images.
   ;------------------------------------------------------------------------

   GetWarpPoints, Auto, FirstPass, State, Curframe, Nxy, 0, Xi, Yi, Xo, Yo, $
                  retval1, whichorbit

   IF (retval1 EQ -1) THEN BEGIN
      IF (FirstPass) THEN BEGIN
         SetFontInfo, old_font
         IF (Auto EQ 0) THEN BEGIN
            FirstPass = 0
            GetWarpPoints, Auto, FirstPass, State, Curframe, Nxy, $
                           0, Xi, Yi, Xo, Yo, retval1, whichorbit
            IF (retval1 EQ -1) THEN BEGIN
               cur_frame = (Curframe LE 9) ? Curframe : Curframe - 9
               camname = !KON.Instr.CAM_NAMES[cur_frame-1]
               ival = DIALOG_MESSAGE('Could not co-register camera ' + $
                                      camname + '. ', /INFORMATION, /CENTER)
               RETURN
            ENDIF
         ENDIF ELSE BEGIN
            Retval = -2
            RETURN
         ENDELSE
      ENDIF ELSE BEGIN
         cur_frame = (Curframe LE 9) ? Curframe : Curframe - 9
         camname = !KON.Instr.CAM_NAMES[cur_frame-1]
         ival = DIALOG_MESSAGE('Could not co-register camera ' + $
                               camname + '. ', /INFORMATION, /CENTER)
         SetFontInfo, old_font
         RETURN
      ENDELSE 
   ENDIF

   IF (retval1 EQ -2) THEN BEGIN
      ival = DIALOG_MESSAGE('Cancelling co-registration correction.', $
                             /INFORMATION, /CENTER)
      SetFontInfo, old_font
      Retval = -2
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Copy the current camera BRFs to the work window storage array at high
   ; resolution in all channels. 
   ;------------------------------------------------------------------------
   
   FOR iband=0,NumBand-2 DO $
      WorkImage[*,*,iband] = GetRawImage(-1, -1, -1, -1, iband, Curframe-1, $
                                         !KON.Instr.HI_RES_PIX_SIZE, $
                                         !KON.Misc.INTERP_TYPE_SAMP)
   
   ;------------------------------------------------------------------------
   ; Copy the camera image to the work window (this avoids rescaling the
   ; BRFs) and post the warping offsets applied at the actual warp point
   ; positions. Then display the work window.
   ;------------------------------------------------------------------------
   
   IF (Auto EQ 0) THEN BEGIN
      State.curframe = Curframe
      SafeWSET, (*State.pwinHdl)[State.curframe], didit
   ENDIF

   tempImage = TVRD(ORDER=1, TRUE=((NumBand GE 3) ? 3 : 0))
   save_wndw = !D.WINDOW
   SafeWSET, (*State.pwinHdl)[!VAR.WORK_WNDW], didit
   State.curframe = !VAR.WORK_WNDW
   
   TV, tempImage, ORDER=1, TRUE=((NumBand GE 3) ? 3 : 0)
   
   PLOT, Xo, State.sizey-Yo, PSYM=7, COLOR=255, SYMSIZE=0.5, $
         POSITION=[0,0,State.sizex-1,State.sizey-1], XSTYLE=5, YSTYLE=5, $
         XRANGE=[0,State.sizex-1], YRANGE=[0,State.sizey-1], /NOERASE

   xdif = STRING(Xi-Xo, FORMAT='(F4.1)')
   ydif = STRING(Yi-Yo, FORMAT='(F4.1)')
   XYOUTS, Xo-3, State.sizey-Yo+5, xdif + '/' + ydif, ALIGNMENT=0.5, $
           CHARSIZE=0.7, COLOR=255

   SafeWSET, save_wndw, didit
   RedrawWindow, State, !VAR.WORK_WNDW

   xdif = 0
   ydif = 0

   ;------------------------------------------------------------------------
   ; Compute the mean and RMS differences of all offsets across and along.
   ; Display to nearest 1/10th pixel. Show statistics in a message box.
   ;------------------------------------------------------------------------

   MeanAcross = ROUND(MEAN(Xi[0:Nxy-1]-Xo[0:Nxy-1]) * 100.0) * 0.01
   MeanAlong  = ROUND(MEAN(Yi[0:Nxy-1]-Yo[0:Nxy-1]) * 100.0) * 0.01

   RMSdevAcross = SQRT(MEAN((Xi[0:Nxy-1]-Xo[0:Nxy-1])^2))
   RMSdevAlong  = SQRT(MEAN((Yi[0:Nxy-1]-Yo[0:Nxy-1])^2))

   IF (Auto EQ 0) THEN BEGIN
      msg_str = STRARR(4)
      msg_str[0] = '                         ' + $
                   'Pixel Offsets for Registration Correction'
      msg_str[1] = ' '
      msg_str[2] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2,A)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' cross-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAcross, ' ; RMS correction = ', $
           RMSdevAcross, '   ')
      msg_str[3] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2,A)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' along-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAlong, ' ; RMS correction = ', $
           RMSdevAlong, '   ')
      rval = DIALOG_MESSAGE(msg_str, /INFORMATION, /CENTER)
      msg_str = 0
   ENDIF

   ;------------------------------------------------------------------------
   ; If the user wants to keep the warped image, copy it to the appropriate
   ; place after increasing the resolution.
   ;------------------------------------------------------------------------

   iyesno = 'Yes'
   IF (Auto NE 1) THEN $
      iyesno = DIALOG_MESSAGE('Do you wish to accept this as the new image?', $
                              /QUESTION, /CENTER)

   IF (STRUPCASE(iyesno) EQ 'YES') THEN BEGIN
      num_bands = (NumBand EQ 4) ? 4 : 1

      FOR iband=0,num_bands-1 DO BEGIN
         
         size_y = (SIZE(*!VAR.RawImages[iband,Curframe-1]))[2] / $
                  CoordStruct.(whichorbit).NumBlk
         mult = (size_y EQ 128) ? 0.25 : 1.0
         native_res = (size_y EQ 128) ? !KON.Instr.LO_RES_PIX_SIZE : $
                                        !KON.Instr.HI_RES_PIX_SIZE

         ;------------------------------------------------------------------
         ; Get the BRF values for this channel at native resolution.
         ;------------------------------------------------------------------

         tempBRFs = GetRawImage(-1, -1, -1, -1, iband, Curframe-1, $
                                native_res, !KON.Misc.INTERP_TYPE_SAMP)

         ;------------------------------------------------------------------
         ; To avoid producing patches of anomalous color caused by black
         ; pixels in terrain-obscured areas ("terrain holes"), fill all
         ; holes with the interpolated values derived when the image was
         ; loaded. Restore the holes afterwards.
         ;------------------------------------------------------------------

         IF (PTR_VALID(!VAR.NullPixelList[iband,Curframe-1])) THEN $
            tempBRFs[*(!VAR.NullPixelList[iband,Curframe-1])] = $
                     *(!VAR.NullValueList[iband,Curframe-1])

         tempBRFs = WARP_TRI(Xo*mult, Yo*mult, Xi*mult, Yi*mult, $
                             TEMPORARY(tempBRFs))

         IF (PTR_VALID(!VAR.NullPixelList[iband,Curframe-1])) THEN $
            tempBRFs[*(!VAR.NullPixelList[iband,Curframe-1])] = 0.0
            
         ;------------------------------------------------------------------
         ; Copy the warped (registration corrected) BRF data to their arrays.
         ; This change cannot be backed out without reloading from HDF file.
         ;------------------------------------------------------------------

         *(!VAR.RawImages[iband,Curframe-1]) = tempBRFs

         tempBRFs = 0
      ENDFOR
      
      ;---------------------------------------------------------------------
      ; Rescale the BRFs for display and copy the results to the appropriate
      ; (hidden) image window.
      ;---------------------------------------------------------------------

      PrepImageArray, 0, ((NumBand EQ 1) ? [0] : !KON.Instr.RGBbands), $
                      State.nframes-1, Curframe-1, newImage, retval

      save_wndw = !D.WINDOW
      State.curframe = Curframe
      SafeWSET, (*State.pwinHdl)[State.curframe], didit

      TV, newImage, ORDER=1, TRUE=(NumBand GE 3) ? 3 : 0
      SafeWSET, save_wndw, didit

      ;---------------------------------------------------------------------
      ; Set global flag to indicate a registration correction was applied.
      ;---------------------------------------------------------------------

      !SAV.HtWind.REGISTRATION_CORR = 1

   ENDIF

   newImage = 0
   Xo = 0
   Yo = 0
   Xi = 0
   Yi = 0
   
   ;------------------------------------------------------------------------
   ; Reset the window to An if it isn't already. Then reset parameters to
   ; indicate that something other than a Data Menu item changed the
   ; contents of the OP window.
   ;------------------------------------------------------------------------

   IF (ResetAn EQ 'Y') THEN BEGIN
      Curframe = 5
      ResetFrame, State, Curframe, 1
   ENDIF
   ResetSwathData

   SetFontInfo, old_font

   Retval = 0

END  ;  Op_FixCoregError

;***************************************************************************
PRO Op_FixCoregErrorAn, Auto, State, TargetVal, ResetAn, Curframe, NumBand, $
                        Retval
;***************************************************************************
; Routine warps the An camera of orbit 2 to match the An camera of orbit 1.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = 0
   Nxy = 0
   MeanAcross = 0.0
   MeanAlong = 0.0
   RMSdevAcross = 0.0
   RMSdevAlong = 0.0

   !SAV.HtWind.CORR_TARGET = TargetVal

   ;------------------------------------------------------------------------
   ; Determine whether a single orbit or two orbits are loaded.
   ;------------------------------------------------------------------------

   norbits = 1
   IF ((State.nframes-1) EQ 18) THEN norbits = 2

   ;------------------------------------------------------------------------
   ; If 2 orbits are present, warp the nadir camera of orbit 2 to the nadir
   ; camera of orbit 1.  
   ;------------------------------------------------------------------------

   IF (norbits LT 2) THEN BEGIN
      ival = DIALOG_MESSAGE('There is no orbit 2 to operate on', /CENTER, $
                            /INFORMATION)
      Retval = 1
      RETURN
   ENDIF

   Curframe = 14
   ResetFrame, State, Curframe, 1

   WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = !VAR.WORK_WNDW
   WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = $
                   'Camera: ' + !KON.Instr.camera_names[!VAR.WORK_WNDW]

   Op_FixCoregError, Auto, 1, State, Curframe, ResetAn, 1, NumBand, Nxy, $
                     MeanAcross, MeanAlong, RMSdevAcross, RMSdevAlong, Retval

END  ;  Op_FixCoregErrorAn

;***************************************************************************
PRO Op_FixCoregErrorMulti, State, TargetVal, ResetAn, CamMask, Curframe, $
                           NumBand, Retval
;***************************************************************************
; Routine warps all cameras for 1 or 2 orbits with reference to their nadir
; cameras.  This avoids most of the user interaction by bypassing the
; dialog boxes.
; Target means: 1 = Land; 2 = Cloud; 3 = also Cloud; 4 = Plume.
;               1, 2 and 3 only refer to misregistration correction
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

   Retval = -1
   !SAV.HtWind.CORR_TARGET = TargetVal

   ;------------------------------------------------------------------------
   ; Determine whether a single orbit or two orbits are loaded.
   ;------------------------------------------------------------------------

   norbits = 1
   IF ((State.nframes-1) EQ 18) THEN norbits = 2

   ;------------------------------------------------------------------------
   ; Warp the 8 off-nadir cameras of orbit 1 to their nadir camera.  
   ;------------------------------------------------------------------------

   cam_count = 0
   Nxy = 0
   MeanAcross = 0.0
   MeanAlong = 0.0
   RMSdevAcross = 0.0
   RMSdevAlong = 0.0

   ndxs = WHERE(CamMask[0:9] EQ 1, numndxs)
   msg_str = STRARR(numndxs*3+2)
   ndxs = 0

   FOR icam=1,9 DO BEGIN
      IF (~ CamMask[icam]) THEN CONTINUE      

      Curframe = icam
      ResetFrame, State, Curframe, 1

      WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = !VAR.WORK_WNDW
      WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = 'Camera: ' + $
                      !KON.Instr.camera_names[!VAR.WORK_WNDW]

      first_pass = 1
try_again_1to9:

      Op_FixCoregError, 1, first_pass, State, Curframe, ResetAn, $
                        TargetVal, NumBand, Nxy, MeanAcross, MeanAlong, $
                        RMSdevAcross, RMSdevAlong, Retval

      IF (Retval EQ -9) THEN BEGIN
         msg_str = 0
         RETURN
      ENDIF

      msg_str[0] = '                         ' + $
                   'Pixel Offsets for Registration Correction'
      msg_str[1] = ' '
      msg_str[cam_count*3+2] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2,A)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' cross-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAcross, ' ; RMS correction = ', $
           RMSdevAcross, '   ')
      msg_str[cam_count*3+3] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' along-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAlong, ' ; RMS correction = ', $
           RMSdevAlong, '   ')
      msg_str[cam_count*3+4] = ' '

      IF (Retval EQ -2) THEN BEGIN
         IF (~ first_pass) THEN RETURN
         first_pass = 0
         GOTO, try_again_1to9
      ENDIF

      cam_count += 1

   ENDFOR

   ;------------------------------------------------------------------------
   ; Show a summary of results in a message box.
   ;------------------------------------------------------------------------

   rval = DIALOG_MESSAGE(msg_str, /INFORMATION, /CENTER)
   msg_str = 0

   ;------------------------------------------------------------------------
   ; If 2 orbits are present, warp the nadir camera of orbit 2 to the nadir
   ; camera of orbit 1. General registration correction.
   ;------------------------------------------------------------------------

   IF (!SAV.HtWind.CORR_TARGET EQ 1) THEN BEGIN
      Op_FixCoregErrorAn, 1, State, 1, ResetAn, Curframe, NumBand, Retval
   ENDIF ELSE BEGIN
      Curframe = 5
      ResetFrame, State, Curframe, 1
   ENDELSE

   IF (Retval NE 0) THEN RETURN

   IF ((State.nframes-1) LT 18) THEN RETURN
   
   ;------------------------------------------------------------------------
   ; If 2 orbits are present, warp the 8 off-nadir cameras of orbit 2 to
   ; their nadir camera.  
   ;------------------------------------------------------------------------

   cam_count = 0
   Nxy = 0
   MeanAcross = 0.0
   MeanAlong = 0.0
   RMSdevAcross = 0.0
   RMSdevAlong = 0.0

   msg_str = STRARR(numndxs*3+2)

   FOR icam=10,18 DO BEGIN
      IF (icam EQ 14 OR ~ CamMask[icam]) THEN CONTINUE

      Curframe = icam
      ResetFrame, State, Curframe, 0

      WIDGET_CONTROL, state.wFramesWhichSlider, $
             SET_VALUE = !VAR.WORK_WNDW
      WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = $
             'Camera: ' + !KON.Instr.camera_names[!VAR.WORK_WNDW]

      first_pass = 1
try_again_10to18:
      Op_FixCoregError, 1, first_pass, State, Curframe, ResetAn, $
                        TargetVal, NumBand, Nxy, MeanAcross, MeanAlong, $
                        RMSdevAcross, RMSdevAlong, Retval

      msg_str[0] = '                         ' + $
                   'Pixel Offsets for Registration Correction'
      msg_str[1] = ' '
      msg_str[cam_count*3+2] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2,A)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' cross-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAcross, ' ; RMS correction = ', $
           RMSdevAcross, '   ')
      msg_str[cam_count*3+3] = STRING(FORMAT='(A,I4,A,F5.2,A,F5.2)', $
          'Camera ' + !KON.Instr.camera_names[Curframe] + $
          ' along-swath offset: NumPts = ', Nxy, $
          ' ; Mean correction = ', MeanAlong, ' ; RMS correction = ', $
           RMSdevAlong, '   ')
      msg_str[cam_count*3+4] = ' '

      IF (Retval EQ -2) THEN BEGIN
         IF (~ first_pass) THEN RETURN
         first_pass = 0
         GOTO, try_again_10to18
      ENDIF

      cam_count += 1

   ENDFOR      

   ;------------------------------------------------------------------------
   ; Show a summary of results in a message box.
   ;------------------------------------------------------------------------

   IF (msg_str[0] NE '') THEN $
      rval = DIALOG_MESSAGE(msg_str, /INFORMATION, /CENTER)

   Curframe = 5
   ResetFrame, State, Curframe, 1

   msg_str = 0

END  ;  Op_FixCoregErrorMulti
