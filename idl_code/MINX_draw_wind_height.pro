;===========================================================================
;                                                                          =
;                                   MINX                                   =
;                                                                          =
;===========================================================================
;                                                                          =
;                         Jet Propulsion Laboratory                        =
;                                   MISR                                   =
;                                                                          =
;         Copyright 2007-2015, California Institute of Technology.         =
;                           ALL RIGHTS RESERVED.                           =
;                 U.S. Government Sponsorship acknowledged.                =
;                                                                          =
;===========================================================================

;***************************************************************************
PRO GetHeightMinMax, ProfData, ArrayTypes, Minval, Maxval, NumPts, Status
;***************************************************************************
; Get the minimum and maximum of one or several stereo parameters. This is
; truly ugly.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Minval = -!KON.Misc.BADVALUE_REAL
Maxval =  !KON.Misc.BADVALUE_REAL
NumPts = 0
Status = 0

;---------------------------------------------------------------------------
; Find the number of subarrays passed and the index of the last subarray to
; use in computing the height range.
;---------------------------------------------------------------------------

num_array = (SIZE(ProfData))[0]
IF (num_array GT 1) THEN num_array = (SIZE(ProfData))[2]
end_ndx = (num_array EQ 1) ? 0 : 1

;---------------------------------------------------------------------------
; Get the valid heights.
;---------------------------------------------------------------------------

aerosol_hts = REFORM(ProfData[*, 0:end_ndx])
ndxs = WHERE(aerosol_hts GT -9998.0 AND aerosol_hts LT 30.0, numhts)

;---------------------------------------------------------------------------
; Exclude the top and bottom n% of the values on the assumption that they
; are outliers. Get the preliminary min/max of heights. 
;---------------------------------------------------------------------------

IF (numhts GT 1) THEN BEGIN
   aerosol_hts = aerosol_hts[ndxs]

   FracCut = ((numhts LE   10) ? 0.0   : $
              ((numhts LE   50) ? 0.025 : $
               ((numhts LE  250) ? 0.020 : $
                ((numhts LE 1000) ? 0.015 : $
                 ((numhts LE 5000) ? 0.010 : 0.005)))))

   num_cut = FracCut * numhts
   min_ndx_pass = CEIL(num_cut)
   max_ndx_pass = FLOOR(numhts - num_cut) - 1

   sort_vals = aerosol_hts[SORT(aerosol_hts)]
   Minval = sort_vals[min_ndx_pass]
   Maxval = sort_vals[max_ndx_pass]
   aerosol_hts = 0
   sort_vals = 0
ENDIF

;---------------------------------------------------------------------------
; Count the non-terrain points and adjust the minimum height down if the
; terrain is lower than the aerosol.
;---------------------------------------------------------------------------

ndxs = WHERE(ArrayTypes EQ !KON.WindObjTyp.WIND_NULL_OBJ, numndxs)

IF (numndxs EQ 1) THEN BEGIN
   terr_hts = ProfData[*, ndxs[0]]
   ndxs = WHERE(terr_hts GT -9998.0, terrht)
   Minval = Minval < MIN(terr_hts[ndxs])
ENDIF

ndxs = 0

;---------------------------------------------------------------------------
; If the minimum value is too large, quit.
;---------------------------------------------------------------------------

IF (Minval GE -!KON.Misc.BADVALUE_REAL) THEN BEGIN
   Minval = -!KON.Misc.BADVALUE_REAL
   Maxval =  !KON.Misc.BADVALUE_REAL
   NumPts = 0
   Status = -1
   RETURN
ENDIF

IF (Maxval LE !KON.Misc.BADVALUE_REAL) THEN BEGIN
   Maxval = Minval + 1.0
   NumPts = 0
   Status = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set the min and max values to numbers appropriate for displaying on map
; view with a color bar or on profile view with horizontal marker values.
; Start by adding a little room on either end for appearances.
;---------------------------------------------------------------------------

Minval *= (Minval LT 0.0) ? 1.1 : 0.9
Maxval *= (Maxval LT 0.0) ? 0.9 : 1.1

IF (Minval GE 0.0) THEN BEGIN
   Minval = (Minval LE  4.0) ? FLOOR(Minval / 0.5) * 0.5 : $
                               FLOOR(Minval / 1.0) * 1.0
ENDIF ELSE BEGIN
   Minval = (Minval GE -4.0) ? FLOOR(Minval / 0.5) * 0.5 : $
                               FLOOR(Minval / 1.0) * 1.0
ENDELSE

IF (Maxval GE 0.0) THEN BEGIN
   Maxval = (Maxval LE  4.0) ? CEIL(Maxval / 0.5) * 0.5 : $
                               CEIL(Maxval / 1.0) * 1.0
ENDIF ELSE BEGIN
   Maxval = (Maxval GE -4.0) ? CEIL(Maxval / 0.5) * 0.5 : $
                               CEIL(Maxval / 1.0) * 1.0
ENDELSE

;---------------------------------------------------------------------------
; The maximum height shouldn't be larger than the user's input value of
; maximum height above sea level in digitizing dialog.
;---------------------------------------------------------------------------

Maxval = Maxval < !SAV.Digitize.MAX_HGHT

;---------------------------------------------------------------------------
; Set the return value of number of points.
;---------------------------------------------------------------------------

NumPts = numhts

;---------------------------------------------------------------------------
; If these values are for displaying colors in map view, override any
; positive minimum height with 0.0. If for profile view, override with -0.1.
; Set the values in the color key structure. Also set the data type that was
; passed.
;---------------------------------------------------------------------------

IF (!SAV.Digitize.WIND_TYPE EQ !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN $
   !VAR.DataRgn.DATA_TYPE = !KON.DataRgn.TYPE_WINDCORR_HT

IF (!SAV.Digitize.WIND_TYPE EQ !KON.WindObjTyp.WIND_NO_DIREC_OBJ) THEN $
   !VAR.DataRgn.DATA_TYPE = !KON.DataRgn.TYPE_ZEROWIND_HT

!VAR.DataRgn.VALUE_MIN_DIG[!VAR.DataRgn.DATA_TYPE] = $
             (Minval GT 0.0) ? 0.0 : Minval

IF (Minval EQ 0.0 AND num_array NE 1) THEN Minval = -0.1
!VAR.DataRgn.VALUE_MAX_DIG[!VAR.DataRgn.DATA_TYPE] = $
             (Maxval LE Minval) ? Minval + 1.0 : Maxval

END  ;  GetHeightMinMax

;***************************************************************************
PRO GetWindMinMax, ProfData, NumStdDev, Minval, Maxval, NumPts
;***************************************************************************
; Get the minimum and maximum of wind values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = 0

Minval = -!KON.Misc.BADVALUE_REAL
Maxval =  !KON.Misc.BADVALUE_REAL
NumPts = 0

ndxs = WHERE(ProfData GT -999.0, totwnd)

;---------------------------------------------------------------------------
; Get the min/max of heights. Exclude the top and bottom X% of the values on
; the assumption that they are outliers. Count the non-terrain points.
;---------------------------------------------------------------------------

IF (totwnd GT 1) THEN BEGIN
   good_vals = ProfData[ndxs]
   Minval = MIN(good_vals)
   Maxval = MAX(good_vals)

   FracCut = ((totwnd LE   10) ? 0.0   : $
              ((totwnd LE   50) ? 0.025 : $
               ((totwnd LE  250) ? 0.020 : $
                ((totwnd LE 1000) ? 0.015 : $
                 ((totwnd LE 5000) ? 0.010 : 0.005)))))

   num_cut = FracCut * totwnd
   min_ndx_pass = CEIL(num_cut)
   max_ndx_pass = FLOOR(totwnd - num_cut) - 1

   sort_vals = good_vals[SORT(good_vals)]
   Minval = sort_vals[min_ndx_pass]
   Maxval = sort_vals[max_ndx_pass]
   good_vals = 0
   sort_vals = 0
   
   NumPts = totwnd
ENDIF

ndxs = 0

IF (Minval GE -!KON.Misc.BADVALUE_REAL) THEN BEGIN
   Minval = -1.0
   Maxval =  1.0
   NumPts = 0
   Status = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set the min and max values to numbers appropriate for displaying on map
; view with a color bar or on profile view with horizontal marker values.
; These will differ.
;---------------------------------------------------------------------------

extra = (Maxval - Minval) * 0.05
Minval -= extra
Maxval += extra

IF (Minval GE 0.0) THEN BEGIN
   Minval = (Minval LE  4.0) ? FLOOR(Minval / 0.5) * 0.5 : $
                               FLOOR(Minval / 1.0) * 1.0
ENDIF ELSE BEGIN
   Minval = (Minval GE -4.0) ? FLOOR(Minval / 0.5) * 0.5 : $
                               FLOOR(Minval / 1.0) * 1.0
ENDELSE

IF (Maxval GE 0.0) THEN BEGIN
   Maxval = (Maxval LE  4.0) ? CEIL(Maxval / 0.5) * 0.5 : $
                               CEIL(Maxval / 1.0) * 1.0
ENDIF ELSE BEGIN
   Maxval = (Maxval GE -4.0) ? CEIL(Maxval / 0.5) * 0.5 : $
                               CEIL(Maxval / 1.0) * 1.0
ENDELSE

END  ;  GetWindMinMax

;***************************************************************************
PRO ShowHeightWindProfile, State, pRgn, iBand, NumPtsLine, CompareOK, $
                           TerrainHt, PtHtsZero, PtHtsCorr, BestHtMedian, $
                           BestHtTop, PtWind_Cross, PtWindAlong, $
                           PgePtHtsZero, PgePtHtsCorr, PgeEwWind, $
                           PgeNsWind, AutoQual, Retval
;***************************************************************************
; Create a profile of the corrected heights and winds. To make the text
; presentable and allow rotated text, use TrueType fonts. To reduce the
; quality issues when drawing small characters, draw the plot very large
; using a scale factor, then scale it back down by the same amount when done.
; Allow user to opt to scale it down by less than the amount scaled up to
; allow for large presentation quality images. 
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

CompareStereo = (!SAV.Digitize.COMPARE_PGEHTS GE 1)

;---------------------------------------------------------------------------
; Copy the input profile data into a single array. Also get the min/max of
; the heights and winds.
;---------------------------------------------------------------------------

IF (CompareStereo AND CompareOK) THEN BEGIN
   prof_data =  [[PtHtsZero],    [PtHtsCorr],   [TerrainHt], $
                 [PtWind_Cross], [PtWindAlong], [PgePtHtsZero], $
                 [PgePtHtsCorr], [PgeEwWind],   [PgeNsWind]]

   GetHeightMinMax, [[PtHtsZero], [PtHtsCorr], [TerrainHt], $
                     [PgePtHtsZero], [PgePtHtsCorr]], $
                     [!KON.WindObjTyp.WIND_NO_DIREC_OBJ, $
                      !KON.WindObjTyp.WIND_USER_DIREC_OBJ, $
                      !KON.WindObjTyp.WIND_NULL_OBJ, $
                      !KON.WindObjTyp.WIND_NO_DIREC_OBJ, $
                      !KON.WindObjTyp.WIND_USER_DIREC_OBJ], $
                     min_ht, max_ht, num_ht_pts, status

   GetWindMinMax, [[PtWind_Cross], [PtWindAlong], [PgeEwWind], $
                   [PgeNsWind]], 3.0, min_wnd, max_wnd, num_wnd_pts

ENDIF ELSE BEGIN
   prof_data = [[PtHtsZero], [PtHtsCorr], [TerrainHt], $
                [PtWind_Cross], [PtWindAlong]]

   GetHeightMinMax, [[PtHtsZero], [PtHtsCorr], [TerrainHt]], $
                     [!KON.WindObjTyp.WIND_NO_DIREC_OBJ, $
                      !KON.WindObjTyp.WIND_USER_DIREC_OBJ, $
                      !KON.WindObjTyp.WIND_NULL_OBJ], $
                     min_ht, max_ht, num_ht_pts, status

   GetWindMinMax, [[PtWind_Cross], [PtWindAlong]], 3.0, min_wnd, $
                   max_wnd, num_wnd_pts
ENDELSE

min_vals = [min_ht, min_wnd]
max_vals = [max_ht, max_wnd]
num_pts  = [num_ht_pts, num_wnd_pts]

;---------------------------------------------------------------------------
; Create the x-axis data by computing the distance of each point from the
; head point. Sort the x and y data before plotting.
; At the same time accumulate the fire power as the sum of the power in all
; those pixels that have a value.
;---------------------------------------------------------------------------

x_dist = FLTARR(NumPtsLine)
pNextPt = (*pRgn).pNextLinePt

scross0 = (*((*pNextPt)).pData).somcross
salong0 = (*((*pNextPt)).pData).somalong
x_dist[0] = 0.0
fire_power = 0.0
pNextPt = (*pNextPt).pNextSib
ipt = 1

WHILE (PTR_VALID(pNextPt)) DO BEGIN
   scross = (*((*pNextPt)).pData).somcross
   salong = (*((*pNextPt)).pData).somalong
   x_dist[ipt] = SQRT((scross0-scross)*(scross0-scross) + $
                      (salong0-salong)*(salong0-salong))
   IF (!VAR.CurrFiles.Fire_Loaded) THEN BEGIN
      fpower = (*((*pNextPt).pData)).modis_data[0]
      IF (fpower GT 0.0) THEN fire_power += fpower
   ENDIF
   ipt += 1
   pNextPt = (*pNextPt).pNextSib
ENDWHILE

x_ndxs = SORT(x_dist)
x_dist = x_dist[x_ndxs]

;---------------------------------------------------------------------------
; Set up window parameters including scale factors for improving display
; quality (create large in hidden window to produce good quality TrueType
; text, then shrink). 
;---------------------------------------------------------------------------

rgn_name = (*((*pRgn).pData)).name[iBand]
orbitdate = ''
toks = STRSPLIT(CoordStruct.(0).OrbitDate, '-', /EXTRACT, COUNT=ntok)
IF (ntok EQ 3) THEN BEGIN
   months = ['Jan','Feb','March','April','May','June','July','Aug', $
             'Sept','Oct','Nov','Dec']
   month = months[FIX(toks[1])-1]
   orbitdate = month + ' ' + STRTRIM(STRING(FIX(toks[2])),2) + $
      ', ' + toks[0]
   months = 0
ENDIF
orbitdate = ' - ' + orbitdate

IF (!SAV.Digitize.PUB_QUALITY) THEN BEGIN
   titles = ['Height Profile : ' + rgn_name + orbitdate, $
             'Wind Speed Profile']
   max_med_str = ''
   scale_factor_up   = (!KON.Misc.MINX_PLATFORM EQ 2) ? 3 : 6
   scale_factor_down = (!KON.Misc.MINX_PLATFORM EQ 2) ? 2 : 4
ENDIF ELSE BEGIN
   titles = ['Height Profile : ' + rgn_name + orbitdate, $
             'Wind Speed Profile : ' + rgn_name]

   fmt_str = '(F6.3)'
   IF (BestHtMedian LT -999.0) THEN fmt_str = '(F6.0)'

   max_med_str = '!S!DMaximum / Median height estimates: ' + $
   STRING(FORMAT=fmt_str, BestHtTop) + ' / ' + $
   STRING(FORMAT=fmt_str, BestHtMedian) + ' km!N!X'
   IF (!VAR.CurrFiles.Fire_Loaded) THEN max_med_str += $
      '!S!D;    Fire power: ' + $
   STRING(FORMAT='(F8.1)', fire_power) + ' MW!N!X'
   max_med_str += '!S!D;    Retrieval Quality: ' + AutoQual + '!N!X'
   loc_lat = (*(*(*pRgn).pNextPolyPt).pData).lat
   loc_lon = (*(*(*pRgn).pNextPolyPt).pData).lon
   max_med_str += '!C!S!DDistance axis origin: ' + $
      'lat ' + STRING(FORMAT='(F7.3)', loc_lat) + ' / ' + $
      'lon ' + STRING(FORMAT='(F8.3)', loc_lon) + '!N!X'
                 
   scale_factor_up   = (!KON.Misc.MINX_PLATFORM EQ 2) ? 3 : 6
   scale_factor_down = (!KON.Misc.MINX_PLATFORM EQ 2) ? 3 : 6
ENDELSE

font_size1 = STRTRIM(STRING(12 * scale_factor_up),2)
font_size2 = STRTRIM(STRING( 8 * scale_factor_up),2)
font_size3 = STRTRIM(STRING(10 * scale_factor_up),2)

key_str = ['Zero-wind ht', 'Wind-corrected ht', 'Terrain ht', $
           'Wind across swath', 'Wind along swath', 'MISR zero-wind ht', $
           'MISR wind-corr ht', 'MISR wind EW', 'MISR wind NS']

x_axis_title = 'Distance From Initial Point (km)'
y_axis_title = ['Height ASL (km)', 'Wind Speed (m/sec)']

size_x = 600 * scale_factor_up
size_y = 780 * scale_factor_up

x_margin = [6,2]
y_margin = [5,3]

min_x = MIN(x_dist, MAX=max_x)

multi = [[0,1,2], [1,1,2]]

;                         X                    square
;                         *                   diamond
prof_symb = [[!KON.Misc.SymbID[1], !KON.Misc.SymbID[3]], $
             [!KON.Misc.SymbID[2], !KON.Misc.SymbID[5]]]

;                             red                       blue
;                            blue2                      aqua
;                            green                    magenta
;                            brown                      pink
prof_color = [[!KON.Colors.ColorVals[0], !KON.Colors.ColorVals[3], $
               !KON.Colors.ColorVals[14],!KON.Colors.ColorVals[5]], $
              [!KON.Colors.ColorVals[1], !KON.Colors.ColorVals[2], $
               !KON.Colors.ColorVals[8], !KON.Colors.ColorVals[7]]]

;---------------------------------------------------------------------------
; Create the window to draw profiles in. This call to WINDOW allocates > 100
; MByte, so be sure to clean it up when done.
;---------------------------------------------------------------------------

WINDOW, TITLE='Stereo Height and Winds', /FREE, /PIXMAP, XSIZE=size_x, $
        YSIZE=size_y

save_pixmap_wndw = !D.WINDOW

;---------------------------------------------------------------------------
; Draw 1 or 2 sets of zero-wind or zero-wind plus best-wind profiles in the
; 2 panels of the height/wind window.
;---------------------------------------------------------------------------

avg_num_major = 5
save_multi = !P.MULTI
old_font = GetFontInfo(0)

FOR iprof=0,4,3 DO BEGIN

   ;------------------------------------------------------------------------
   ; Set up the plot.
   ;------------------------------------------------------------------------

   !P.MULTI = multi[*,iprof/2]

   SetFontInfo, {Type:!KON.FontTyp.FONT_TRUTYP, Size:font_size1, Face:'bold'}

   min_y = min_vals[iprof/2]
   max_y = max_vals[iprof/2]
   y_range = max_y - min_y

   y2_range = y_range > 4.0
   int_major = CEIL(FLOAT(y2_range) / avg_num_major)
   int_major = (y_range LE 1) ? int_major / 4.0 : $
                ((y_range LE 3) ? int_major / 2.0 : int_major)

   IF ((int_major MOD 2) NE 0 AND int_major GE 3) THEN BEGIN
      IF (int_major LT 6) THEN int_major += 1
      IF (int_major GT 6) THEN int_major -= 1
   ENDIF

   num_major = y2_range / int_major
   num_minor = CEIL((int_major LE 5) ? int_major : $
                     ((int_major LE 10) ? (int_major / 2) : $
                       (int_major / 4)))
   IF ((num_minor MOD 2) NE 0 AND num_minor GE 3) THEN num_minor -= 1

   IF (iprof NE 0) THEN max_med_str = ''

   PLOT, [0.0,0.0], [0.0,0.0001], TITLE=titles[iprof/2], $
         XTITLE=x_axis_title, YTITLE=y_axis_title[iprof/2], $
         XRANGE=[min_x,max_x], YRANGE=[min_y,max_y], $
         XTHICK=scale_factor_up, YTHICK=scale_factor_up, $
         THICK=scale_factor_up, XMARGIN=x_margin, YMARGIN=y_margin, $
         XTICKLEN=1, YTICKLEN=1, XGRIDSTYLE=1, YGRIDSTYLE=1, $
         YTICKINTERVAL=int_major, YMINOR=num_minor, XSTYLE=1, $
         YSTYLE=1, COLOR=0, BACKGROUND=16777215, SUBTITLE=max_med_str

   ;------------------------------------------------------------------------
   ; Plot the data points.
   ;------------------------------------------------------------------------
            
   symthick = scale_factor_up
   IF (iprof EQ 0) THEN $
      sym_size = (num_pts[iprof/2] LT    50) ? 0.50 : $
                  ((num_pts[iprof/2] LT  200) ? 0.40 : $
                   ((num_pts[iprof/2] LT 1000) ? 0.30 : 0.25))
 
   OPLOT, x_dist, prof_data[x_ndxs,iprof], COLOR=prof_color[0,iprof/2], $
          PSYM=prof_symb[0,iprof/2], SYMSIZE=sym_size, THICK=symthick

   OPLOT, x_dist, prof_data[x_ndxs,iprof+1], COLOR=prof_color[1,iprof/2], $
          PSYM=prof_symb[1,iprof/2], SYMSIZE=sym_size, THICK=symthick

   IF (iprof EQ 0) THEN BEGIN
      OPLOT, x_dist, prof_data[x_ndxs,2], COLOR=!KON.Colors.ColorVals[1], $
             THICK=symthick+2  ;  terrain heights

      IF (CompareStereo AND CompareOK) THEN BEGIN  ;  PGE heights
         OPLOT, x_dist, prof_data[x_ndxs,5], COLOR=prof_color[2,iprof/2], $
             PSYM=prof_symb[0,iprof/2], SYMSIZE=sym_size, THICK=symthick

         OPLOT, x_dist, prof_data[x_ndxs,6], COLOR=prof_color[3,iprof/2], $
             PSYM=prof_symb[1,iprof/2], SYMSIZE=sym_size, THICK=symthick
      ENDIF
   ENDIF

   IF (iprof EQ 3 AND CompareStereo AND CompareOK) THEN BEGIN  ;  PGE winds
      OPLOT, x_dist, prof_data[x_ndxs,7], COLOR=prof_color[2,iprof/2], $
          PSYM=prof_symb[0,0], SYMSIZE=sym_size, THICK=symthick

      OPLOT, x_dist, prof_data[x_ndxs,8], COLOR=prof_color[3,iprof/2], $
          PSYM=prof_symb[1,0], SYMSIZE=sym_size, THICK=symthick
   ENDIF

   ;------------------------------------------------------------------------
   ; Draw 2 lines across the plot representing the best estimates of top and
   ; median height for the data (something like the injection height) and
   ; annotate it. Don't do for plume lines.
   ;------------------------------------------------------------------------

   SetFontInfo, {Type:!KON.FontTyp.FONT_TRUTYP, Size:font_size2, Face:'bold'}

   IF (~ !SAV.Digitize.PUB_QUALITY) THEN BEGIN

      IF (iprof EQ 0 AND BestHtMedian GE 0.0) THEN BEGIN
         OPLOT, [min_x,max_x], [BestHtMedian, BestHtMedian], $
                COLOR=!KON.Colors.ColorVals[8], LINESTYLE=5, $
                THICK=scale_factor_up
         xbeg = max_x - (max_x-min_x) / 10.
         ybeg = BestHtMedian - (max_vals[iprof/2] - min_vals[iprof/2]) / 25.
         XYOUTS, xbeg, ybeg, 'Median Top Ht', ALIGNMENT=0.5, $
                 COLOR=!KON.Colors.ColorVals[8]
      ENDIF

      IF (iprof EQ 0 AND BestHtTop GE 0.0) THEN BEGIN
         OPLOT, [min_x,max_x], [BestHtTop, BestHtTop], $
                COLOR=!KON.Colors.ColorVals[8], LINESTYLE=5, $
                THICK=scale_factor_up
         xbeg = min_x + (max_x-min_x) / 10.
         ybeg = BestHtTop - (max_vals[iprof/2] - min_vals[iprof/2]) / 25.
         XYOUTS, xbeg, ybeg, 'Maximum Top Ht', ALIGNMENT=0.5, $
                 COLOR=!KON.Colors.ColorVals[8]
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Write the key text to identify the point colors.
   ;------------------------------------------------------------------------
            
   SetFontInfo, {Type:!KON.FontTyp.FONT_TRUTYP, Size:font_size3, Face:'bold'}

   ypos1 = 0.94
   ypos2 = 0.89

   XYOUTS, .85*max_x, ypos1*(max_y-min_y)+min_y, key_str[iprof], $
           ALIGNMENT=0.5, COLOR=prof_color[0,iprof/2]

   XYOUTS, .85*max_x, ypos2*(max_y-min_y)+min_y, key_str[iprof+1], $
           ALIGNMENT=0.5, COLOR=prof_color[1,iprof/2]

   IF (iprof EQ 0) THEN BEGIN
      XYOUTS, .50*max_x, ypos1*(max_y-min_y)+min_y, key_str[2], $
              ALIGNMENT=0.5, COLOR=!KON.Colors.ColorVals[1]  ;  terrain hts

      IF (CompareStereo AND CompareOK) THEN BEGIN  ;  PGE heights
         XYOUTS, .15*max_x, ypos1*(max_y-min_y)+min_y, key_str[5], $
                 ALIGNMENT=0.5, COLOR=prof_color[2,iprof/2]
   
         XYOUTS, .15*max_x, ypos2*(max_y-min_y)+min_y, key_str[6], $
                 ALIGNMENT=0.5, COLOR=prof_color[3,iprof/2]
      ENDIF
   ENDIF

   IF (iprof EQ 3 AND CompareStereo AND CompareOK) THEN BEGIN  ;  PGE winds
      XYOUTS, .13*max_x, ypos1*(max_y-min_y)+min_y, key_str[7], $
              ALIGNMENT=0.5, COLOR=prof_color[2,iprof/2]
   
      XYOUTS, .13*max_x, ypos2*(max_y-min_y)+min_y, key_str[8], $
              ALIGNMENT=0.5, COLOR=prof_color[3,iprof/2]
   ENDIF

ENDFOR

SetFontInfo, old_font

!P.MULTI = save_multi

;---------------------------------------------------------------------------
; Scale the plot back down to a reasonable size. Then delete pixmap window
; and the image.
;---------------------------------------------------------------------------

SafeWSET, save_pixmap_wndw, didit
prof_image = TVRD(/ORDER, TRUE=1)
SafeWDELETE, save_pixmap_wndw, didit

sizes = SIZE(prof_image)
xmod = sizes[2] MOD scale_factor_down
ymod = sizes[3] MOD scale_factor_down
x_size = sizes[2] / scale_factor_down
y_size = sizes[3] / scale_factor_down

prof_image = TEMPORARY(prof_image[0:2, 0:sizes[2]-xmod-1, 0:sizes[3]-ymod-1])
prof_image = REBIN(TEMPORARY(prof_image), 3, x_size, y_size)

WINDOW, XSIZE=x_size, YSIZE=y_size, RETAIN=2, YPOS=30, /FREE, $
        XPOS=50+(x_size+30)*!SAV.Digitize.TWO_RETRIEVALS, $
        TITLE='Stereo Height and Winds'

TV, prof_image, /ORDER, TRUE=1
prof_image = 0

WndwStereoPlot[iBand] = !D.WINDOW

;---------------------------------------------------------------------------
; Save the image to file if requested.
;---------------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_HTPLOT_IMAGE) THEN BEGIN
   rgn_name = (*((*pRgn).pData)).name[iBand]
   WriteChartFile, rgn_name, 'HtWindPlot'
ENDIF

Retval = 0

END  ;  ShowHeightWindProfile

;***************************************************************************
PRO GetArrayMinMax, AryData, DefMin, DefMax, MinVal, MaxVal, Retval
;***************************************************************************
; Get the valid minimum and maximum values of an arbitrary array.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

good_ndxs = WHERE(AryData GT -999., num_good)

IF (num_good GT 0) THEN BEGIN
   max_val = MAX(AryData[good_ndxs], MIN=min_val)
   MinVal  = DefMin < min_val
   MaxVal  = DefMax > max_val
ENDIF ELSE BEGIN
   MinVal = DefMin
   MaxVal = DefMax
   RETURN
ENDELSE

good_ndxs = 0
Retval = 0

END  ;  GetArrayMinMax

;***************************************************************************
PRO FreeHistStruct, HistStruct
;***************************************************************************
; Free the data used in a histogram.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

HistStruct.DataSource = 0
HistStruct.ShowAry = 0
HistStruct.HistData = 0
HistStruct = 0

END  ;  FreeHistStruct

;***************************************************************************
PRO InitHistogram, DataType, DataName, Rotate, MaxInstance, $
                   CntrOnZero, Def_BinSize, Min_Val, Max_Val, $
                   HistStruct
;***************************************************************************
; Create a data structure to hold the histogram data. Fill some members with
; passed in data.
; NOTE - CntrOnZero is not implemented.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Compute the begin and end values for the histogram. Add a blank bin on
; either end.
;---------------------------------------------------------------------------

bin_size = Def_BinSize

bin_start = FLOOR(Min_Val / bin_size) * bin_size
IF (bin_start GT 0.0) THEN bin_start -= bin_size
bin_end   = CEIL (Max_Val / bin_size) * bin_size + bin_size
num_bins  = (bin_end - bin_start) / bin_size

;---------------------------------------------------------------------------
; If there are too many bins, increase the bin size and reduce the number of
; bins. Ensure that the bin boundarys are on multiples of the bin size.
;---------------------------------------------------------------------------

WHILE (num_bins GT 40) DO BEGIN
   bin_start -= bin_size
   bin_end += bin_size
   num_bins += 2
   IF ((bin_start MOD (bin_size * 2.0)) NE 0) THEN BEGIN
      bin_start -= bin_size
      num_bins += 1
   ENDIF
   IF ((bin_end MOD (bin_size * 2.0)) NE 0) THEN BEGIN
      bin_end += bin_size
      num_bins += 1
   ENDIF
   bin_size *= 2.
   num_bins = CEIL(num_bins / 2.)
ENDWHILE

;---------------------------------------------------------------------------
; There must be at least 5 bins.
;---------------------------------------------------------------------------

WHILE (num_bins LT 5) DO BEGIN
   bin_start -= bin_size
   bin_end += bin_size
   num_bins += 2
ENDWHILE

;---------------------------------------------------------------------------
; The range in data values must be at least 2.0 (applies to winds).
;---------------------------------------------------------------------------

IF (DataType EQ !KON.Histo.WIND_HIST) THEN BEGIN
   WHILE (bin_end - bin_start LT 2.0) DO BEGIN
      bin_start -= bin_size
      bin_end += bin_size
      num_bins += 2
   ENDWHILE
ENDIF

;---------------------------------------------------------------------------
; Create the histogram structure.
;---------------------------------------------------------------------------

HistStruct = $
  { DataType     : DataType, $     ; height, wind etc.
    DataName     : DataName, $     ; plume, cloud etc. unique name
    CntrOnZero   : CntrOnZero, $   ; 1 = bin centered @ 0; 0 = edge @ 0
    Rotate       : Rotate, $       ; 1 = bins on left axis; 0 = on bottom
    BinStart     : bin_start, $    ; value at left/bottom edge of first bin
    BinSize      : bin_size, $     ; width of each histogram bin
    BinNum       : num_bins, $     ; number of histogram bins
    MaxInstance  : MaxInstance, $  ; max number of histogram instances
    ThisInstance : 0, $            ; current histogram instance number
    MinValue     : Min_Val, $      ; minimum value in all instances
    MaxValue     : Max_Val, $      ; maximum value in all instances
    MinCount     :  99999L, $      ; minimum hist count in all instances
    MaxCount     : -99999L, $      ; maximum hist count in all instances
    DataSource   : STRARR(MaxInstance), $         ; computed or file name
    ShowAry      : INTARR(MaxInstance), $         ; 1 if hist has data
    HistData     : LONARR(Num_Bins,MaxInstance) $ ; histogram num hits array
  }

END  ;  InitHistogram

;***************************************************************************
PRO BuildHistogram, DataSource, ThisInstance, DoCorrHts, DataValues, $
                    HistStruct
;***************************************************************************
; Create a histogram from input data for the instance just computed or for
; data read from a product file. Store parameters in the appropriate instance
; of the histogram data structure.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the ShowAry flag if the histogram should be displayed.
;---------------------------------------------------------------------------

HistStruct.ShowAry[ThisInstance] = DoCorrHts

;---------------------------------------------------------------------------
; Store the source of this instance of data.
;---------------------------------------------------------------------------

HistStruct.DataSource[ThisInstance] = DataSource

;---------------------------------------------------------------------------
; Compute the starting histogram data value - the data value at the left/
; bottom edge of the first bin.
;---------------------------------------------------------------------------

hist_min = HistStruct.BinStart

;---------------------------------------------------------------------------
; Compute the number of bins in the histogram.
;---------------------------------------------------------------------------

num_bins = HistStruct.BinNum

;---------------------------------------------------------------------------
; Compute and store the the histogram and other parameters. If there are no
; valid data values, leave the histogram with default values.
;---------------------------------------------------------------------------

IF (MAX(DataValues) GT -9000.) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Compute the histogram and store in data structure. 
   ;------------------------------------------------------------------------

   hist = HISTOGRAM(DataValues, MIN=hist_min, NBINS=num_bins, $
                    BINSIZE=HistStruct.BinSize)
   HistStruct.HistData[*,ThisInstance] = hist

   ;------------------------------------------------------------------------
   ; Find the min and max of histogram bins and store in structure.
   ;------------------------------------------------------------------------

   min_count = MIN(hist, MAX=max_count)
   HistStruct.MinCount = min_count < HistStruct.MinCount
   HistStruct.MaxCount = max_count > HistStruct.MaxCount

ENDIF

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

hist = 0

END  ;  BuildHistogram

;***************************************************************************
PRO LLStoreHistogram, HistStruct, Instance, BinSize, NumBins, $
                      pCntrAry, pHistAry
;***************************************************************************
; Store histogram data in its region object.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

pCntrAry = PTR_NEW()
pHistAry = PTR_NEW()
NumBins = 0
BinSize = 0.0

;---------------------------------------------------------------------------
; Compress the histogram by removing all bins that are empty. Record the bin
; size, number of bins with data and the center value and number of hits per
; non-zero bin.
;---------------------------------------------------------------------------

ndxs = WHERE(HistStruct.HistData[*,Instance] GT 0, numndxs)
IF (numndxs LT 1) THEN RETURN

bin_cntr = FLTARR(numndxs)
num_hits = INTARR(numndxs)

NumBins = numndxs
BinSize = HistStruct.BinSize

FOR indx=0,numndxs-1 DO BEGIN
   bin_cntr[indx] = HistStruct.BinStart + $
                    HistStruct.BinSize * (ndxs[indx] + 0.5)
   num_hits[indx] = HistStruct.HistData[ndxs[indx],Instance]
ENDFOR

;---------------------------------------------------------------------------
; Store data in the appropriate region of the linked list.
;---------------------------------------------------------------------------

pCntrAry = PTR_NEW(bin_cntr)
pHistAry = PTR_NEW(num_hits)

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

ndxs = 0
bin_cntr = 0
num_hits = 0

END  ;  LLStoreHistogram

;***************************************************************************
PRO DrawBarChart, Title, Xtitle, Ytitle, Struct, BinBegNdx, BinEndNdx, $
                  NumBin, ColorAry, FmtStr, Retval
;***************************************************************************
; Draw a bar chart or histogram.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

IF (NumBin LT 1) THEN RETURN

;---------------------------------------------------------------------------
; Save a copy of histogram data. 
;---------------------------------------------------------------------------

hist = FLOAT(Struct.HistData)

;---------------------------------------------------------------------------
; Set up histogram colors.  Since up to 4 histograms are superimposed, we
; must draw them in descending order of length.  So create an artificial
; list of histogram bins and rely on the different colors per bin to keep
; the results straight. If two or more histograms have the same value, add
; or subtract a small amount from each array so they will all be visible.
;---------------------------------------------------------------------------

ColorAry = [!KON.Colors.ColorVals[3],!KON.Colors.ColorVals[1], $
            !KON.Colors.ColorVals[0],!KON.Colors.ColorVals[2]]

colors = LONARR(Struct.BinNum,Struct.MaxInstance)

ndxs = WHERE(Struct.ShowAry, numary)

IF (numary LE 1) THEN BEGIN
   FOR iary=0,Struct.MaxInstance-1 DO BEGIN
      colors[*,iary] = ColorAry[iary]
   ENDFOR
ENDIF ELSE BEGIN
   FOR ibin=BinBegNdx,BinEndNdx DO BEGIN
      vals = FLOAT(Struct.HistData[ibin,*])
      nval = N_ELEMENTS(vals)
      FOR ival1=0,nval-2 DO BEGIN
         baseval = vals[ival1]
         ichange = 0.0
         IF (baseval NE 0.0) THEN BEGIN
            FOR ival2=ival1+1,nval-1 DO BEGIN
               IF (vals[ival2] EQ baseval) THEN BEGIN
                  vals[ival2] -= ichange
                  ichange += 0.1
               ENDIF
            ENDFOR
            IF (ichange NE 0.0) THEN vals[ival1] += 0.1
         ENDIF
      ENDFOR
      ordr = REVERSE(SORT(vals))
      hist[ibin,*] = vals[ordr]
      colors[ibin,*] = ColorAry[ordr]
   ENDFOR
ENDELSE

;---------------------------------------------------------------------------
; Set up histogram bar names.
;---------------------------------------------------------------------------

barval_strlen = 1
bar_names = STRARR(NumBin+1)
val_strlen = FIX(STRMID(FmtStr, 0, 1))
num_ticks = 0

IF (Struct.Rotate EQ 1) THEN BEGIN
   Dval = 10
ENDIF ELSE BEGIN
   IF (val_strlen LE 3) THEN Dval = 12
   IF (val_strlen EQ 4) THEN Dval = 9
   IF (val_strlen GE 5) THEN Dval = 6
ENDELSE

FOR ibin=0,NumBin DO BEGIN
   IF (((NumBin GT Dval*1) AND (NumBin LE Dval*2) AND $
        (ibin MOD 2) NE 0) OR $
       ((NumBin GT Dval*2) AND (NumBin LE Dval*3) AND $
        (ibin MOD 3) NE 0) OR $
       ((NumBin GT Dval*3) AND (NumBin LE Dval*4) AND $
        (ibin MOD 4) NE 0) OR $
       ((NumBin GT Dval*4) AND $
        (ibin MOD 5) NE 0)) THEN BEGIN
      bar_names[ibin] = ' '
   ENDIF ELSE BEGIN
      IF (Struct.Rotate EQ 1) THEN BEGIN
         bin_beg = Struct.BinStart + Struct.BinSize * (BinBegNdx + ibin)
         bar_names[ibin] = STRING(FORMAT='(F'+FmtStr+')', bin_beg)
      ENDIF ELSE BEGIN
;         bin_beg = Struct.BinStart + Struct.BinSize * (BinBegNdx + ibin)
;         bar_names[ibin] = STRING(FORMAT='(I2)', bin_beg)
;      ENDELSE
         IF (Struct.BinSize GE 1) THEN BEGIN
            bin_beg = FLOOR(Struct.BinStart + Struct.BinSize * $
                            (BinBegNdx + ibin))
            bar_names[ibin] = STRTRIM(STRING(bin_beg),2)
         ENDIF ELSE BEGIN
            bin_beg = Struct.BinStart + Struct.BinSize * (BinBegNdx + ibin)
            bar_names[ibin] = STRING(FORMAT='(F'+ FmtStr + ')', bin_beg)
         ENDELSE
      ENDELSE
      num_ticks += 1
      IF (STRLEN(bar_names[ibin]) GT barval_strlen) THEN $
         barval_strlen = STRLEN(bar_names[ibin])
   ENDELSE
ENDFOR

maxbar = MAX(STRLEN(bar_names))

;---------------------------------------------------------------------------
; Plot a blank base with plot parameters set up.
;---------------------------------------------------------------------------

maxval = ((Struct.MaxCount LT -9000) OR $
          (Struct.MaxCount EQ 0)) ? 1 : Struct.MaxCount

;bin_range = Struct.BinSize * (BinEndNdx - BinBegNdx + 1)
;ndxs = WHERE(bar_names NE ' ', numndxs)
;bar_names = bar_names[ndxs]

;dummyval = INTARR(numndxs)
;dummycol = LONARR(numndxs) + !KON.Colors.ColorVals[6]

dummyval = INTARR(NumBin)
dummycol = LONARR(NumBin) + !KON.Colors.ColorVals[6]

pcolor_save   = !P.COLOR
xticklen_save = !X.TICKLEN
yticklen_save = !Y.TICKLEN
charsize_save = !P.CHARSIZE
xmargin_save  = !X.MARGIN
ymargin_save  = !Y.MARGIN

!P.COLOR = 0

;---------------------------------------------------------------------------
; Set the font to use first.
;---------------------------------------------------------------------------

old_font = GetFontInfo(0)

SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
!P.CHARSIZE = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.0

IF (Struct.Rotate EQ 1) THEN BEGIN   ;  plot heights
   !X.RANGE = [0,maxval*1.1]
   !X.GRIDSTYLE = 1    &  !X.TICKLEN = 1
   !X.MINOR = -1       &  !X.TICKS = 0
   !Y.RANGE = [0,1]

   BAR_PLOT, dummyval, TITLE=Title, XTITLE=Xtitle, $
             BACKGROUND=!KON.Colors.ColorVals[6], COLORS=dummycol, /ROTATE, $
             BAROFFSET=-0.5001, BARSPACE=0.0001, BARNAMES=bar_names

   ;-------------------------------------------------------------------------
   ; Because of a bug in rotated plots, we write the Y-axis title.
   ;-------------------------------------------------------------------------

   xpos = -(0.05 + barval_strlen * 0.03) * !X.RANGE[1]
   ypos = 0.5
   XYOUTS, xpos, ypos, Ytitle, ALIGNMENT=0.5, COLOR=0, ORIENTATION=-90.

ENDIF ELSE BEGIN                   ;  plot winds
   !Y.RANGE = [0,maxval*1.1]
   !Y.GRIDSTYLE = 1     &  !Y.TICKLEN = 1
   !Y.MINOR = -1        &  !Y.TICKS = 0
   !X.RANGE = [0,1]

   BAR_PLOT, dummyval, TITLE=Title, XTITLE=Xtitle, YTITLE=Ytitle, $
             BACKGROUND=!KON.Colors.ColorVals[6], COLORS=dummycol, $
             BAROFFSET=-0.5001, BARSPACE=0.0001, BARNAMES=bar_names
ENDELSE

;---------------------------------------------------------------------------
; Overplot the current set of bars on the blank base.  Exit now if no data
; are present.
;---------------------------------------------------------------------------

FOR ihist=0,Struct.MaxInstance-1 DO BEGIN
   IF (Struct.ShowAry[ihist] EQ 0) THEN CONTINUE
   IF (Struct.Rotate EQ 1) THEN BEGIN
      !Y.TICKLEN = 0.02  &  !Y.TICKS = 1  &  !Y.STYLE = 4
      BAR_PLOT, hist[BinBegNdx:BinEndNdx,ihist], /OVERPLOT, /OUTLINE, $
                COLORS=colors[BinBegNdx:BinEndNdx,ihist], /ROTATE, $
                BAROFFSET=0.0001, BARSPACE=0.0001
   ENDIF ELSE BEGIN
      !X.TICKLEN = 0.02  &  !X.TICKS = 1  &  !X.STYLE = 4
      BAR_PLOT, hist[BinBegNdx:BinEndNdx,ihist], /OVERPLOT, /OUTLINE, $
                COLORS=colors[BinBegNdx:BinEndNdx,ihist], $
                BAROFFSET=0.0001, BARSPACE=0.0001
   ENDELSE
ENDFOR

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

cleanup12:

SetFontInfo, old_font

hist = 0
vals = 0
ordr = 0
ndxs = 0
colors = 0
bar_names = 0
dummyval = 0
dummycol = 0

!P.COLOR = pcolor_save
!P.CHARSIZE = charsize_save
!X.MARGIN = xmargin_save
!Y.MARGIN = ymargin_save
!X.MINOR = 0       &  !Y.MINOR = 0
!X.TICKLEN = 0.02  &  !Y.TICKLEN = 0.02
!X.GRIDSTYLE = 0   &  !Y.GRIDSTYLE = 0
!X.TICKS = 0       &  !Y.TICKS = 0
!X.TICKV = 0       &  !Y.TICKV = 0

END  ;  DrawBarChart

;***************************************************************************
PRO ShowHeightWindBarChart, RgnName, iBand, ZeroHtStruct, CorrHtStruct, $
                            CWindStruct, AWindStruct, Retval
;***************************************************************************
; Construct a bar chart of heights and winds for the region.
;---------------------------------------------------------------------------

COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Get the number of window panes needed to plot the data and set up the
; window and pane sizes.
;---------------------------------------------------------------------------

num_panes = 0

ndxs = WHERE(ZeroHtStruct.ShowAry, numndx)
IF (numndx GT 0) THEN num_panes += 1
IF (num_panes EQ 0) THEN RETURN

ndxs = WHERE(CorrHtStruct.ShowAry, numndx)
IF (numndx GT 0) THEN num_panes += 1

ndxs = WHERE(CWindStruct.ShowAry, numndx1)
ndxs = WHERE(AWindStruct.ShowAry, numndx2)
IF ((numndx1 GT 0) OR (numndx2 GT 0)) THEN num_panes = 4

ndxs = 0

size_x = (num_panes LE 1) ?  350 : 550
size_y = (num_panes LE 2) ?  300 : 500

pos_x  = (num_panes LE 1) ?  500 : 300
pos_y  = (num_panes LE 2) ?  200 : 100

;---------------------------------------------------------------------------
; Set up plot annotation.
;---------------------------------------------------------------------------

titles = ['Zero-Wind Heights (above MSL)',  $
          'Wind-Corrected Hts (above MSL)', $
          'Cross-Track Wind Speed', 'Along-Track Wind Speed']

x_axis_title = ['', '', 'm/sec', 'm/sec']
y_axis_title = ['km', 'km', '', ''] 

;---------------------------------------------------------------------------
; Set the font for the title and create the window to draw profiles in.
;---------------------------------------------------------------------------

WINDOW, XSIZE=size_x, YSIZE=size_y, XPOS=pos_x, YPOS=pos_y, $
        RETAIN=2, TITLE='Stereo Histograms', /FREE

WndwStereoHist[iBand] = !D.WINDOW

;---------------------------------------------------------------------------
; Get first height bin and number of height bins.
;---------------------------------------------------------------------------

bin_beg_ndx = 0
bin_end_ndx = ZeroHtStruct.BinNum - 1
numbin = ZeroHtStruct.BinNum

IF (num_panes EQ 1 AND numbin LT 1) THEN BEGIN
   ERASE, LONG(2. ^ 24) - 1L
   maxht = ZeroHtStruct.BinNum * ZeroHtStruct.BinSize
   XYOUTS, 15, size_y/2, COLOR=255, CHARSIZE=1.4, /DEVICE, $
           'No heights detected less than ' + $
           STRING(FORMAT='(F6.3)',maxht) + ' meters'
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set up the window panes and margins.
;---------------------------------------------------------------------------

save_multi = !P.MULTI
IF (num_panes EQ 1) THEN !P.MULTI = [0,1,1]
IF (num_panes EQ 2) THEN !P.MULTI = [0,2,1]
IF (num_panes EQ 4) THEN !P.MULTI = [0,2,2]

ndxsz = WHERE(ZeroHtStruct.ShowAry EQ 1, numsource)

!X.OMARGIN = [0.5,0.5]
!Y.OMARGIN = (numsource EQ 1) ? [1.5,3] : [4,3]

!X.MARGIN = (num_panes EQ 1) ? [8,2] : [7,2]
!Y.MARGIN = [3,2]

;---------------------------------------------------------------------------
; Draw zero-wind heights.
;---------------------------------------------------------------------------

IF (num_panes GT 0) THEN $
   DrawBarChart, titles[0], x_axis_title[0], y_axis_title[0], $
                 ZeroHtStruct, bin_beg_ndx, bin_end_ndx, numbin, $
                 color_ary, '5.2', Retval

;---------------------------------------------------------------------------
; Draw wind-corrected heights.
;---------------------------------------------------------------------------

IF (num_panes GT 1) THEN $
   DrawBarChart, titles[1], x_axis_title[1], y_axis_title[1], $
                 CorrHtStruct, bin_beg_ndx, bin_end_ndx, numbin, $
                 color_ary, '5.2', Retval

IF (num_panes GT 2) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Compute first wind bin and number of wind bins.
   ;------------------------------------------------------------------------

   bin_beg_ndx = 0
   bin_end_ndx = CWindStruct.BinNum - 1
   numbin = CWindStruct.BinNum

   ;------------------------------------------------------------------------
   ; Draw wind across values.
   ;------------------------------------------------------------------------

   DrawBarChart, titles[2], x_axis_title[2], y_axis_title[2], $
                 CWindStruct, bin_beg_ndx, bin_end_ndx, numbin, $
                 color_ary, '4.1', Retval

   ;------------------------------------------------------------------------
   ; Draw wind along values.
   ;------------------------------------------------------------------------

   DrawBarChart, titles[3], x_axis_title[3], y_axis_title[3], $
                 AWindStruct, bin_beg_ndx, bin_end_ndx, numbin, $
                 color_ary, '4.1', Retval
ENDIF

;---------------------------------------------------------------------------
; Set the font for the title and write it above all the panes.
;---------------------------------------------------------------------------

old_font = GetFontInfo(0)

SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5

XYOUTS, size_x/2, size_y*0.96, 'Histograms for Region:  ' + RgnName, $
        ALIGNMENT=0.5, CHARSIZE=char_spac, COLOR=0, /DEVICE

;---------------------------------------------------------------------------
; Write a key below all the panes if there is more than 1 source.
;---------------------------------------------------------------------------

IF (numsource GT 1) THEN BEGIN
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'10', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.0

   FOR inst=0,ZeroHtStruct.MaxInstance-1 DO BEGIN
      XYOUTS, 0.05*size_x, 0.05*size_y-inst*12, $
              ZeroHtStruct.DataSource[inst], /DEVICE, $
              ALIGNMENT=0.0, COLOR=color_ary[inst], CHARSIZE=char_spac
   ENDFOR
ENDIF

SetFontInfo, old_font

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

!P.MULTI = save_multi
!X.OMARGIN = [0,0]
!Y.OMARGIN = [0,0]

;---------------------------------------------------------------------------
; Save the image to file if requested.
;---------------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_HTHIST_IMAGE) THEN BEGIN
   WriteChartFile, RgnName, 'HtWindHist'
ENDIF

Retval = 0

END  ;  ShowHeightWindBarChart

;***************************************************************************
PRO DisplayHeightWindData, State, CoordStruct, pRgn, iBand, NumPts, $
                           MisrCoords, LonLatCoords, ZeroWndHts, $
                           CorrWndHts, BestHtMedian, BestHtTop, $
                           TerrainHts, WndCross, WndAlong, AutoQual, Retval
;***************************************************************************
; Top level for constructing a bar chart of heights and winds for current
; region.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

rgn_name = (*((*pRgn).pData)).name[iBand]

DoCorrHts = !SAV.Digitize.WIND_TYPE GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ

;---------------------------------------------------------------------------
; Construct histograms (bar charts) of heights and winds for the current
; region. Save the results in newly created data structures.
;---------------------------------------------------------------------------

GetHeightMinMax, [[ZeroWndHts]], [!KON.WindObjTyp.WIND_NO_DIREC_OBJ], $
                  min_val1, max_val1, status
GetHeightMinMax, [[CorrWndHts]], [!KON.WindObjTyp.WIND_USER_DIREC_OBJ], $
                  min_val2, max_val2, status

min_val_ht = min_val1 < min_val2
max_val_ht = max_val1 > max_val2

IF (min_val_ht EQ !KON.Misc.LARGE_POS_NUM) THEN min_val_ht = 0.0
IF (max_val_ht EQ !KON.Misc.LARGE_NEG_NUM) THEN max_val_ht = 1.0

InitHistogram, !KON.Histo.HEIGHT_HIST, rgn_name, 1, 2, $
               !KON.Histo.HT_BIN_CNTR, !KON.Histo.HT_BIN_SIZE, $
               min_val_ht, max_val_ht, ZeroHtStruct
BuildHistogram, 'MINX  retrievals', 0, 1, ZeroWndHts, ZeroHtStruct

InitHistogram, !KON.Histo.HEIGHT_HIST, rgn_name, 1, 2, $
               !KON.Histo.HT_BIN_CNTR, !KON.Histo.HT_BIN_SIZE, $
               min_val_ht, max_val_ht, CorrHtStruct
BuildHistogram, 'MINX  retrievals', 0, DoCorrHts, CorrWndHts, CorrHtStruct

;*****************************************************************
;; code for testing histogram accuracy
;print,'ZeroWndHts'
;sorted = ZeroWndHts(SORT(ZeroWndHts))
;FOR i=0,N_ELEMENTS(sorted)-1 DO BEGIN
;   IF (i GT 0) THEN $
;      IF (FIX(sorted[i])/250 NE FIX(sorted[i-1])/250) THEN print, ''
;   print,'  ',sorted[i]
;ENDFOR
;*****************************************************************

GetArrayMinMax, WndCross, !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val1, max_val1, status
GetArrayMinMax, WndAlong, !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val2, max_val2, status

min_val_wnd = min_val1 < min_val2
max_val_wnd = max_val1 > max_val2

IF (min_val_wnd EQ !KON.Misc.LARGE_POS_NUM) THEN min_val_wnd = -1.0
IF (max_val_wnd EQ !KON.Misc.LARGE_NEG_NUM) THEN max_val_wnd =  1.0

InitHistogram, !KON.Histo.WIND_HIST, rgn_name, 0, 2, $
               !KON.Histo.WND_BIN_CNTR, !KON.Histo.WND_BIN_SIZE, $
               min_val_wnd, max_val_wnd, CWindStruct
BuildHistogram, 'MINX  retrievals', 0, DoCorrHts, WndCross, CWindStruct

InitHistogram, !KON.Histo.WIND_HIST, rgn_name, 0, 2, $
               !KON.Histo.WND_BIN_CNTR, !KON.Histo.WND_BIN_SIZE, $
               min_val_wnd, max_val_wnd, AWindStruct
BuildHistogram, 'MINX  retrievals', 0, DoCorrHts, WndAlong, AWindStruct

;---------------------------------------------------------------------------
; Store the histogram data in the region object of the linked list.
;---------------------------------------------------------------------------

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   LLStoreHistogram, ZeroHtStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).zht_binsize = BinSize
   (*((*pRgn).pData)).zht_numbins = NumBins
   (*((*pRgn).pData)).zeroht_cntr = pCntrAry
   (*((*pRgn).pData)).zeroht_hist = pHistAry

   LLStoreHistogram, CorrHtStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).cht_binsize = BinSize
   (*((*pRgn).pData)).cht_numbins = NumBins
   (*((*pRgn).pData)).corrht_cntr = pCntrAry
   (*((*pRgn).pData)).corrht_hist = pHistAry

   LLStoreHistogram, CWindStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).wndx_binsize = BinSize
   (*((*pRgn).pData)).wndx_numbins = NumBins
   (*((*pRgn).pData)).windx_cntr   = pCntrAry
   (*((*pRgn).pData)).windx_hist   = pHistAry

   LLStoreHistogram, AWindStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).wndy_binsize = BinSize
   (*((*pRgn).pData)).wndy_numbins = NumBins
   (*((*pRgn).pData)).windy_cntr   = pCntrAry
   (*((*pRgn).pData)).windy_hist   = pHistAry
ENDIF

;---------------------------------------------------------------------------
; If we're comparing stereo data from product files, load a file.
;---------------------------------------------------------------------------

CompareOK = 0

IF (!SAV.Digitize.COMPARE_PGEHTS GE 1) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Retrieve heights and winds from a PGE or prototype data file for
   ; comparison. User selects from a GUI list.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.COMPARE_PGEHTS EQ 1) THEN BEGIN
      LoadTCStereoData, State, stereo_file, retval1
      IF (retval1 EQ 0) THEN $
         GetPgeStereoData, State, NumPts, MisrCoords, pge_pt_hts_zero, $
                           pge_pt_hts_corr, pge_ew_wind, pge_ns_wind, $
                           stereo_file, retval
   ENDIF

   IF (!SAV.Digitize.COMPARE_PGEHTS EQ 2) THEN BEGIN
      LoadTCCloudData, State, stereo_file, retval1
      IF (retval1 EQ 0) THEN $
         GetPgeCloudData, State, NumPts, MisrCoords, pge_pt_hts_zero, $
                          pge_pt_hts_corr, pge_ew_wind, pge_ns_wind, $
                          stereo_file, retval
   ENDIF

   IF (retval1 EQ -1) THEN BEGIN
      mssg = ['Could not load MISR height/wind data']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, do3
   ENDIF
   
   IF (retval EQ -1) THEN BEGIN
      mssg = ['Could not load MISR height/wind data from:', stereo_file]
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, do2
   ENDIF
   IF (retval EQ -2) THEN GOTO, do2

   CompareOK = 1
   
   ;------------------------------------------------------------------------
   ; Append histograms of heights and winds for the current stereo product
   ; file in the data structures created above. Remove directory to leave
   ; only file name.
   ;------------------------------------------------------------------------

   ipos = STRPOS(stereo_file, !KON.Misc.Slash, /REVERSE_SEARCH)
   stereo_file = STRMID(stereo_file, ipos+1) + '  retrievals'

   BuildHistogram, stereo_file, 1, 1, pge_pt_hts_zero, ZeroHtStruct
   BuildHistogram, stereo_file, 1, DoCorrHts, pge_pt_hts_corr, CorrHtStruct
   BuildHistogram, stereo_file, 1, DoCorrHts, pge_ew_wind, CWindStruct
   BuildHistogram, stereo_file, 1, DoCorrHts, pge_ns_wind, AWindStruct

;*****************************************************************
;; code for testing histogram accuracy
;print,''
;print,'pge_pt_hts_zero'
;sorted = pge_pt_hts_zero(SORT(pge_pt_hts_zero))
;FOR i=0,N_ELEMENTS(sorted)-1 DO BEGIN
;   IF (i GT 0) THEN $
;      IF (FIX(sorted[i])/250 NE FIX(sorted[i-1])/250) THEN print, ''
;   print,'  ',sorted[i]
;ENDFOR
;print,''
;print,'pge_ew_wind'
;sorted = pge_ew_wind(SORT(pge_ew_wind))
;FOR i=0,N_ELEMENTS(sorted)-1 DO BEGIN
;   IF (i GT 0) THEN $
;      IF (sorted[i] NE sorted[i-1]) THEN print, ''
;   print,'  ',sorted[i]
;ENDFOR
;*****************************************************************

;---------------------------------------------------------------------------
; Display profiles of heights and winds for computed and MISR PGE data.
;---------------------------------------------------------------------------

do2: ShowHeightWindProfile, State, pRgn, iBand, NumPts, CompareOK, $
                            TerrainHts, ZeroWndHts, CorrWndHts, $
                            BestHtMedian, BestHtTop, WndCross, WndAlong, $
                            pge_pt_hts_zero, pge_pt_hts_corr, $
                            pge_ew_wind, pge_ns_wind, AutoQual, retval

   pge_pt_hts_zero = 0
   pge_pt_hts_corr = 0
   pge_ew_wind = 0
   pge_ns_wind = 0

ENDIF ELSE BEGIN

;---------------------------------------------------------------------------
; Display profiles of heights and winds for just computed data.
;---------------------------------------------------------------------------

   pge_pt_hts_zero = 0
   pge_pt_hts_corr = 0
   pge_ew_wind = 0
   pge_ns_wind = 0

do3: ShowHeightWindProfile, State, pRgn, iBand, NumPts, CompareOK, $
                            TerrainHts, ZeroWndHts, CorrWndHts, $
                            BestHtMedian, BestHtTop, WndCross, WndAlong, $
                            pge_pt_hts_zero, pge_pt_hts_corr, $
                            pge_ew_wind, pge_ns_wind, AutoQual, retval
ENDELSE

;---------------------------------------------------------------------------
; Display histograms (bar charts) of heights and winds for the region and
; for up to 3 stereo product files for comparison.
;---------------------------------------------------------------------------

ShowHeightWindBarChart, rgn_name, iBand, ZeroHtStruct, CorrHtStruct, $
                        CWindStruct, AWindStruct, retval

;---------------------------------------------------------------------------
; Free the histogram structure data.
;---------------------------------------------------------------------------

FreeHistStruct, ZeroHtStruct
FreeHistStruct, CorrHtStruct
FreeHistStruct, CWindStruct
FreeHistStruct, AWindStruct

Retval = 0

END  ;  DisplayHeightWindData

;***************************************************************************
PRO GetBestHeights, xCoords, yCoords, RawHeights, SmoothedHts, BestHtMean, $
                    BestHtMed, BestHtMax, Status
;***************************************************************************
; Compute a best median and best top height (injection height?) for display
; on the profle, and return them for writing to the raw data file. For these
; calculations use filtered heights, and reject a few of the highest points
; as outliers (more than 3 std devs from the median). If there aren't enough
; points to compute a top, just use the median value of the filtered data.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Constants.
;---------------------------------------------------------------------------

MIN_INPTS_THRESH = 3
FUNC_COEF = [0.286135, 2.25916, 1.24014]

;---------------------------------------------------------------------------
; Initialize.
;---------------------------------------------------------------------------

Status = -1
BestHtMean = !KON.Misc.BADVALUE_REAL
BestHtMed  = !KON.Misc.BADVALUE_REAL
BestHtMax  = !KON.Misc.BADVALUE_REAL

ndxs1 = WHERE(RawHeights  GT !KON.Misc.BADVALUE_REAL AND $
              SmoothedHts GT !KON.Misc.BADVALUE_REAL, numndxs1)
IF (numndxs1 LT MIN_INPTS_THRESH) THEN RETURN

raw_hts    = FLOAT(RawHeights[ndxs1])
smooth_hts = FLOAT(SmoothedHts[ndxs1])
BestHtMean = MEAN(smooth_hts)
BestHtMed  = MEDIAN(smooth_hts, /EVEN)
BestHtMax  = MAX(smooth_hts)

;---------------------------------------------------------------------------
; Compute the standard deviation threshold appropriate for the number of
; successfully retrieved points. Use the function below with these results
; e.g. at:  num pts input [ 1000,   200,    40,     8]
;           stddev thresh [ 2.20,  2.00,  1.80,  1.60]
;           equivalent to [0.986, 0.977, 0.964, 0.945] in cumulative prob.
;           apprx pts del [ 13.9,   4.6,   1.4,   0.4]
; This is applied only to points whose input smoothed values are larger than
; the surface fit values!
;---------------------------------------------------------------------------

stddev_thresh = FUNC_COEF[0] * ALOG10(FUNC_COEF[1] * numndxs1) + FUNC_COEF[2]

;---------------------------------------------------------------------------
; Compute best height value by removing points that are more than N standard
; deviations from the surface defined by the difference between the raw data
; and the smoothed data. The max of the remaining points is the best height.
;---------------------------------------------------------------------------

IF (numndxs1 GE MIN_INPTS_THRESH) THEN BEGIN
   surf_diff = raw_hts - smooth_hts
   
   std_dev = STDDEV(surf_diff)
   ndxs2 = WHERE(surf_diff GT (stddev_thresh * std_dev), numndxs2, $
                 COMPLEMENT=ndxs3, NCOMPLEMENT=numndxs3)
      
   IF (numndxs3 GE MIN_INPTS_THRESH) THEN BestHtMax = MAX(smooth_hts[ndxs3])
ENDIF

Status = 0

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

raw_hts = 0
smooth_hts = 0
surf_diff = 0
ndxs1 = 0
ndxs2 = 0
ndxs3 = 0
   
END  ;  GetBestHeights

;***************************************************************************
PRO DisplayRetrievedData, ImagePolyCoords, ImageLineCoords, ZDataVals, $
                          Color, SampSpacing, DrawType, Xbeg, Ybeg, $
                          Xsize, Ysize, GetHt, BestHtMedian, BestHtTop, $
                          Retval
;***************************************************************************
; Display colors corresponding to stereo heights or other retrieved
; parameters selected by the user on the camera images.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

PixelsOrContours = 1  ;  =1 -> show data values as colored pixels
                      ;  =2 -> show data values as colored contours
MIN_PTS_ORDER1 = 4

;---------------------------------------------------------------------------
; Get the spacing between digitized points for this object type measured in
; 275 m pixels.
;---------------------------------------------------------------------------

spacing = FIX(SampSpacing / !KON.Instr.HI_RES_PIX_SIZE)

;---------------------------------------------------------------------------
; Create a 3D array to contain the x/y/z data and change the origin if non-
; zero. Height data need to be converted from m to km.
;---------------------------------------------------------------------------

ndxs1 = WHERE(ZDataVals GE !VAR.DataRgn.VALUE_MIN_DLG[!VAR.DataRgn.DATA_TYPE] AND $
              ZDataVals LE !VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE], $
              numndxs1)
IF (numndxs1 LT 1) THEN RETURN

xyzdata1 = FLTARR(3,numndxs1)
xyzdata1[0,*] = ImageLineCoords[0,ndxs1]
xyzdata1[1,*] = ImageLineCoords[1,ndxs1]
xyzdata1[2,*] = ZDataVals[ndxs1]
ndxs1 = 0

;---------------------------------------------------------------------------
; Display data values as colored pixels overlain on the camera image.
;---------------------------------------------------------------------------

IF (PixelsOrContours EQ 1) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Set up the drawing parameters.
   ;------------------------------------------------------------------------

   sym_type = 8  ; user-defined symbol

   dataval_min = !VAR.DataRgn.VALUE_MIN_DLG[!VAR.DataRgn.DATA_TYPE]
   dataval_max = !VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE]

   IF (GetHt) THEN $
      dataval_max = (!VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE] LE 4.0) ? $
          CEIL(!VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE] / 0.5) * 0.5 : $
          CEIL(!VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE] / 1.0) * 1.0

   ;------------------------------------------------------------------------
   ; Loop over the pixels, drawing each in the appropriate color.
   ; 0.55 -> ndx0; 1.1 -> ndx1; 2.2 -> ndx3; 3.3 -> ndx5
   ;------------------------------------------------------------------------

   spac_coef = [0.23, 0.18, 0.18, 0.17, 0.17, 0.17]
   psymsiz = spacing * spac_coef[spacing/2-1] ; important for size of color squares
   nsymsiz = psymsiz * (-1.0)
   xval = [nsymsiz, nsymsiz, psymsiz, psymsiz, nsymsiz]
   yval = [nsymsiz, psymsiz, psymsiz, nsymsiz, nsymsiz]

   ;------------------------------------------------------------------------
   ; Compute the data value-dependent color first. Save coordinates if will
   ; display 3D data value points. Create square symbol of correct size to
   ; match sample spacing.
   ;------------------------------------------------------------------------

   coeff = !D.TABLE_SIZE / (FLOAT(dataval_max) - dataval_min)
   coltype = LONG(ROUND((xyzdata1[2,*] - dataval_min) * coeff))
   
   ndxs = WHERE(coltype LT 0, numndxs)
   IF (numndxs GT 0) THEN coltype[ndxs] = 0
   ndxs = WHERE(coltype GT (!D.TABLE_SIZE-1), numndxs)
   IF (numndxs GT 0) THEN coltype[ndxs] = (!D.TABLE_SIZE-1)
 
   xcoord = xyzdata1[0,*] - Xbeg
   ycoord = xyzdata1[1,*] - Ybeg

   ;------------------------------------------------------------------------
   ; We must loop here, because COLOR= cannot take a vector (bummer). If
   ; there are fewer than 5000 points, then loop over the pixels. If there
   ; are more than 5000 pixels, then loop over the colors. The PLOTS proc
   ; can use multiple colors, so change this to use PLOT to draw frame and
   ; use PLOTS to draw data.
   ;------------------------------------------------------------------------

   USERSYM, xval, yval, /FILL

   IF (numndxs1 LT 5000L) THEN BEGIN

      FOR ipix=0,numndxs1-1 DO $
         PLOT, [xcoord[ipix], xcoord[ipix]], [ycoord[ipix], ycoord[ipix]], $
               PSYM=sym_type, XRANGE=[0,Xsize], YRANGE=[0,Ysize], /NOERASE, $
               POSITION=[0,0,Xsize,Ysize], XSTYLE=5, YSTYLE=5, $
               COLOR=!SAV.ColorPalette[coltype[ipix]]

   ENDIF ELSE BEGIN

      FOR icol=0,255 DO BEGIN
         ndxs = WHERE(coltype EQ icol, numndxs)
         IF (numndxs GT 0) THEN $
            PLOT, [xcoord[ndxs], xcoord[ndxs]], [ycoord[ndxs], ycoord[ndxs]], $
                  PSYM=sym_type, XRANGE=[0,Xsize], YRANGE=[0,Ysize], /NOERASE, $
                  POSITION=[0,0,Xsize,Ysize], XSTYLE=5, YSTYLE=5, $
                  COLOR=!SAV.ColorPalette[icol]
      ENDFOR

   ENDELSE

;---------------------------------------------------------------------------
; Display data values as colored contour lines. Much too slow with many pts.
;---------------------------------------------------------------------------

ENDIF ELSE BEGIN

   MIN_PTS_FIT  = 9
   MIN_PTS_CNTR = 8
   MIN_DEL_CNTR = 50.
   MEDIAN_WIDTH = 5

   ;------------------------------------------------------------------------
   ; Fit a minimum curvature surface to the irregularly-spaced points and
   ; generate a regularly-spaced grid. Smooth the surface.
   ;------------------------------------------------------------------------

   IF (numndxs1 LT MIN_PTS_FIT) THEN GOTO, end_draw

   min_x = MIN(xyzdata1[0,*], MAX=max_x)
   min_y = MIN(xyzdata1[1,*], MAX=max_y)
   nx = FIX((max_x - min_x + 1) / spacing)
   ny = FIX((max_y - min_y + 1) / spacing)

   xvect = INDGEN(nx) * spacing + min_x
   yvect = INDGEN(ny) * spacing + min_y

   zgrid = MIN_CURVE_SURF(xyzdata1[2,*], xyzdata1[0,*], xyzdata1[1,*], $
                         /TPS, XOUT=xvect, YOUT=yvect)

   zsize = SIZE(zgrid)

   IF (zsize[1] GE MEDIAN_WIDTH AND zsize[2] GE MEDIAN_WIDTH) THEN $
      zgrid = MEDIAN(zgrid, MEDIAN_WIDTH, /EVEN)

   ;------------------------------------------------------------------------
   ; Use outline of the digitized plume as a region-of-interest to restrict
   ; the valid data values.
   ;------------------------------------------------------------------------

   gridobj = Obj_New('IDLanROI', ImagePolyCoords[0,*], ImagePolyCoords[1,*])

   FOR ii=0,nx-1 DO BEGIN
      xcrd = xvect[ii]
      FOR jj=0,ny-1 DO BEGIN
      ycrd = yvect[jj]
         IF (~ gridobj->ContainsPoints(xcrd,ycrd)) THEN $
            zgrid[ii,jj] = !KON.Misc.BADVALUE_REAL
      ENDFOR
   ENDFOR

   Obj_Destroy, gridobj

   xvect -= Xbeg
   yvect -= Ybeg

   ;------------------------------------------------------------------------
   ; Determine what contour values to use.
   ;------------------------------------------------------------------------

   ndxs3 = WHERE(zgrid GT 0.0, numndxs3)

   IF (numndxs3 LT MIN_PTS_CNTR) THEN BEGIN
;      PRINT, 'Too few points to contour region. Quitting.'
      GOTO, end_draw
   ENDIF

   minz = MIN(zgrid[ndxs3], MAX=maxz)
   level_beg = CEIL (minz / MIN_DEL_CNTR) * MIN_DEL_CNTR
   level_end = FLOOR(maxz / MIN_DEL_CNTR) * MIN_DEL_CNTR
   num_level = 8
   del_level = FLOAT(level_end - level_beg) / (num_level-1)
   del_level = (ROUND(del_level / MIN_DEL_CNTR) * MIN_DEL_CNTR) > MIN_DEL_CNTR 
   num_level = FLOOR((level_end - level_beg) / del_level) + 2
   levels = FLTARR(num_level)
   colors = [16777215, 14680094, 12582974, 10485854, 8388734, $
              6291614,  4194494,  2097374,      255]
   FOR ilev=0,num_level-1 DO BEGIN
      levels[ilev] = level_beg + ilev * del_level
   ENDFOR

   ndxs3 = 0

   ;------------------------------------------------------------------------
   ; Draw the contours.
   ;------------------------------------------------------------------------

   CONTOUR, zgrid, xvect, yvect, POSITION=[0,0,Xsize,Ysize], $
            XRANGE=[0,Xsize], YRANGE=[0,Ysize], /FOLLOW, $
            XMARGIN=[0,0], YMARGIN=[0,0], /DEVICE, /NOERASE, $
            XSTYLE=5, YSTYLE=5, LEVELS=levels, MIN_VALUE=0.0, $
            C_COLORS=colors
ENDELSE

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

Retval = 0

end_draw:

xyzdata1 = 0
surf = 0
surfdiff = 0
levels = 0
colors = 0

END  ;  DisplayRetrievedData

;***************************************************************************
PRO Show_3D_Structure_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON struct_3D, angle_x, angle_z, dump_data, out_dir, cancelit

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=dim3_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   cancelit = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ dim3_struct.x_angle : BEGIN
   END
   event.id EQ dim3_struct.z_angle : BEGIN
   END
   event.id EQ dim3_struct.data_dump : BEGIN
      temp = WIDGET_INFO(dim3_struct.data_dump, /BUTTON_SET)  
      IF (temp EQ 0) THEN WIDGET_CONTROL, dim3_struct.dir_out, SENSITIVE=0
      IF (temp EQ 1) THEN WIDGET_CONTROL, dim3_struct.dir_out, SENSITIVE=1
   END
   event.id EQ dim3_struct.dir_out : BEGIN
   END

   event.id EQ dim3_struct.ok_button : BEGIN
      files_bad = 0
      WIDGET_CONTROL, dim3_struct.x_angle, GET_VALUE=temp
      angle_x = temp
      WIDGET_CONTROL, dim3_struct.z_angle, GET_VALUE=temp
      angle_z = temp
      dump_data = WIDGET_INFO(dim3_struct.data_dump, /BUTTON_SET)
      IF (dump_data NE 0) THEN BEGIN
         WIDGET_CONTROL, dim3_struct.dir_out, GET_VALUE=temp
         out_dir = temp
         npos = STRPOS(out_dir, !KON.Misc.Slash, /REVERSE_SEARCH)
         out_base = STRMID(out_dir, 0, npos)
         IF (~ FILE_TEST(out_base, /DIRECTORY)) THEN BEGIN
            mssg = ['Your output directory does not exist.', $
                    'Specify a valid directory name.']
            rtrn = DIALOG_MESSAGE(mssg, /ERROR)
            files_bad = 1
         ENDIF
      ENDIF

      IF (~ files_bad) THEN BEGIN
         cancelit = 0
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDIF
   END

   event.id EQ dim3_struct.cancel_button : BEGIN
      cancelit = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=dim3_struct, /NO_COPY

END  ;  Show_3D_Structure_eh

;***************************************************************************
PRO Show_3D_Structure_gui, TopBase, Xangle, Zangle, DataDump, DirOut, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON struct_3D, angle_x, angle_z, dump_data, out_dir, cancelit

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

angle_x   = Xangle
angle_z   = Zangle
dump_data = DataDump
out_dir   = DirOut
cancelit  = 0

base0 = WIDGET_BASE(/COLUMN, TITLE='Select 3D Height Parameters', /MODAL, $
                    GROUP_LEADER=TopBase, /TLB_KILL_REQUEST_EVENTS)
base1 = WIDGET_BASE(base0, /COLUMN, /FRAME)
x_angle = CW_FIELD(base1, /STRING, /ROW, VALUE=Xangle, XSIZE=3, $
                   TITLE='New angle for rotation angle about X axis')
z_angle = CW_FIELD(base1, /STRING, /ROW, VALUE=Zangle, XSIZE=3, $
                   TITLE='New angle for rotation angle about Z axis')

base2 = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /NONEXCLUSIVE)
data_dump = WIDGET_BUTTON(base2, /ALIGN_LEFT, $
                 VALUE='Write 3D data to file for input to IDL iSurface tool')

base3 = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER)
dir_out = CW_FIELD(base3, /STRING, /COLUMN, VALUE=out_dir, XSIZE=80, $
                  TITLE='Enter full pathname where 3D output will be written')

base4 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base4, VALUE='Reshow with new angles')
cancel_button = WIDGET_BUTTON(base4, VALUE='Exit without saving')

IF (dump_data EQ 0) THEN WIDGET_CONTROL, dir_out, SENSITIVE=0

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

dim3_struct = { $
   x_angle   : x_angle, $
   z_angle   : z_angle, $
   data_dump : data_dump, $
   dir_out   : dir_out, $
   ok_button : ok_button, $
   cancel_button : cancel_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=dim3_struct, XOFFSET=700, YOFFSET=250, $
                /NO_COPY
WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'Show_3D_Structure_gui', base0, EVENT_HANDLER='Show_3D_Structure_eh'

IF (cancelit EQ 0) THEN BEGIN
   Status   = 0
   Xangle   = angle_x
   Zangle   = angle_z
   DataDump = dump_data
   DirOut   = out_dir
ENDIF ELSE BEGIN
   Status = 1
ENDELSE

END  ;  Show_3D_Structure_gui

;***************************************************************************
PRO Op_Select3DDisplayOptions, State, Orb, Blk, pNextPt, PixSamp, NumPts, $
                               PixCoords, CorrPtHts, FltrGrid
;***************************************************************************
; This is currently not called anywhere!
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; If the user has opted to do so, loop to display the 3D surface. Each time
; show a GUI to allow changing the viewing angle and allowing user to dump
; data to file for viewing in iSurface.
;---------------------------------------------------------------------------

IF (1) THEN BEGIN
   WINDOW, XPOS=10, YPOS=10, XSIZE=1200, YSIZE=700
   xvals = REFORM(SomGridX[half_filter:numx-half_filter-1, 0]) * 0.275 * $
           PixSamp
   yvals = REFORM(SomGridY[0, half_filter:numy-half_filter-1]) * 0.275 * $
           PixSamp
   angle_about_x = 30
   angle_about_z = 30
   OutDir = ''
   FOR idrawit=0,999 DO BEGIN
      DumpData = 0
      !P.BACKGROUND=!KON.Colors.ColorVals[6]

      SHADE_SURF, thin_plate, xvals, yvals, COLOR=0, CHARSIZE=3, CHARTHICK=2, $
                  ax=angle_about_x, az=angle_about_z, $
                  TITLE='Thin Plate Spline Fit to Median Filtered Heights'
                  
      Show_3D_Structure_gui, State.wAnimateBase, angle_about_x, angle_about_z, $
                             DumpData, OutDir, Status

      IF (DumpData NE 0 AND OutDir NE '') THEN BEGIN
         s_nx = STRTRIM(STRING(numx-half_filter*2),2)
         s_ny = STRTRIM(STRING(numy-half_filter*2),2)
         OPENW,  unit, OutDir, /GET_LUN
         PRINTF, unit, 'MISR orbit ' + STRTRIM(STRING(Orb),2) + ', Block ' + STRTRIM(STRING(Blk),2)
         PRINTF, unit, 'Median-filtered heights interpolated with thin-plate spline'
         PRINTF, unit, 'Format per line:  X  Y  Z - all coordinates in meters'
         PRINTF, unit, 'To run iSurface:
         PRINTF, unit, '  1) at an IDL command line, type "iSurface"'
         PRINTF, unit, '  2) select "File" -> "Open", find and double click this file name'
         PRINTF, unit, '  3) select "Fixed Width" and enter 9 in "Data starts at line:"'
         PRINTF, unit, '  4) finish entering parameters - for dimensions use x= ' + $
                     s_nx + ' and y= ' + s_ny
         FOR iy=0,numy-half_filter*2-1 DO BEGIN
            FOR ix=0,numx-half_filter*2-1 DO BEGIN
               PRINTF, unit, xvals[ix], yvals[iy], thin_plate[ix,iy]
            ENDFOR
         ENDFOR
         FREE_LUN, unit
      ENDIF
      IF (Status EQ 1) THEN BREAK
   ENDFOR
   xvals = 0
   yvals = 0
ENDIF      

END  ;  Op_Select3DDisplayOptions

;***************************************************************************
PRO Display3DHeights, RgnName, ImageCoords, TerrainHts, ZDataVals, $
                      HtType
;***************************************************************************
; Function displays a cube of height points: terrain with either zero-wind
; or wind-corrected.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Create 3D arrays to contain the x/y/z data and copy the data in.
;---------------------------------------------------------------------------

IF (HtType EQ 1) THEN BEGIN
   stype = 'zero-wind'
   scolor = [255,0,0]
   tcolor = ' (red) '
ENDIF
IF (HtType EQ 2) THEN BEGIN
   stype = 'wind-corrected'
   scolor = [0,0,255]
   tcolor = ' (blue) '
ENDIF

ndxs1 = WHERE(TerrainHts GT 0.0, numndxs1)
ndxs2 = WHERE(ZDataVals  GT 0.0, numndxs2)
numndxs12 = [numndxs1, numndxs2]

xx = FLTARR(2,MAX(numndxs12))
yy = FLTARR(2,MAX(numndxs12))
zz = FLTARR(2,MAX(numndxs12))

IF (numndxs1 GE 1) THEN BEGIN
   xx[0,0:numndxs1-1] = ImageCoords[0,ndxs1]
   yy[0,0:numndxs1-1] = ImageCoords[1,ndxs1]
   zz[0,0:numndxs1-1] = TerrainHts[ndxs1]
ENDIF

IF (numndxs2 GE 1) THEN BEGIN
   xx[1,0:numndxs2-1] = ImageCoords[0,ndxs2]
   yy[1,0:numndxs2-1] = ImageCoords[1,ndxs2]
   zz[1,0:numndxs2-1] = ZDataVals[ndxs2]
ENDIF

;---------------------------------------------------------------------------
; Set up the display parameters including data ranges.
;---------------------------------------------------------------------------

ndxsx = WHERE(xx GT 0.0, numndxsx)
delx  = MAX(xx[ndxsx]) - MIN(xx[ndxsx])
ndxsy = WHERE(yy GT 0.0, numndxsy)
dely  = MAX(yy[ndxsy]) - MIN(yy[ndxsy])
delxy = (delx > dely) * 1.2
xtrax = (delxy - delx) / 2.0
minx  = MIN(xx[ndxsx]) - xtrax
maxx  = MAX(xx[ndxsx]) + xtrax
xtray = (delxy - dely) / 2.0
miny  = MIN(yy[ndxsy]) - xtray
maxy  = MAX(yy[ndxsy]) + xtray

xrange = [minx, maxx]
yrange = [miny, maxy]

; Control vertical exaggeration.

zmax = MAX(zz[0:1,*]) * 1.2
vert_exag = ((maxx - minx) > (maxy - miny) * $
            !KON.Instr.HI_RES_PIX_SIZE) / zmax
desired_vert_exag = ((vert_exag GE 25.) ? 25.0 : ((vert_exag GE 10.) ? $
                                                  10.0 : $
                    ((vert_exag GE  5.) ?  5.0 : ((vert_exag GE  2.) ?  $
                                                  2.0 : 1.0))))
vert_exag /= desired_vert_exag
zrange = [0.0, zmax * vert_exag]

ptitle = 'Height of terrain (brown) and ' + stype + ' retrievals' + tcolor + $
         'for digitized region: ' + RgnName

; Create square prism symbols to display.

Poly1 = OBJ_NEW('IDLgrPolygon', COLOR=[127,127,0], DATA= $
          [[-1,-1,0],[-1,1,0],[1,1,0],[1,-1,0],[-1,-1,0], $
           [-1,-1,1],[1,-1,1],[1,-1,0],[-1,-1,0], $
           [-1,-1,1],[-1,1,1],[-1,1,0],[-1,-1,0], $
           [-1,1,0],[-1,1,1],[1,1,1],[1,1,0],[-1,1,0], $
           [1,1,0],[1,1,1],[1,-1,1],[1,-1,0],[1,1,0]])
Symbol1 = OBJ_NEW('IDLgrSymbol', Poly1, SIZE=[2,2,8])  

Poly2 = OBJ_NEW('IDLgrPolygon', COLOR=scolor, DATA= $
          [[-1,-1,0],[-1,1,0],[1,1,0],[1,-1,0],[-1,-1,0], $
           [-1,-1,1],[1,-1,1],[1,-1,0],[-1,-1,0], $
           [-1,-1,1],[-1,1,1],[-1,1,0],[-1,-1,0], $
           [-1,1,0],[-1,1,1],[1,1,1],[1,1,0],[-1,1,0], $
           [1,1,0],[1,1,1],[1,-1,1],[1,-1,0],[1,1,0]])
Symbol2 = OBJ_NEW('IDLgrSymbol', Poly2, SIZE=[2,2,8])  

;---------------------------------------------------------------------------
; Show rotatable 3D window containing terrain heights and stereo heights.
;---------------------------------------------------------------------------

XPLOT3D, xx, yy, zz, $
         2, numndxs12, symbols=[Symbol1,Symbol2], thicks=[1,1], $
         colors=[[255,255,255],[255,255,255]], linestyles=[6,6], $
         XRANGE=xrange, YRANGE=yrange, ZRANGE=zrange, $
         GROUP=wAminateBase, /MODAL, $
         TITLE=ptitle, $
         XTITLE='Across SOM coord (275 m pixels)', $
         YTITLE='Along SOM coord (275 m pixels)', $
         ZTITLE='Height ASL (m) - VertExag = ' + $
                STRTRIM(STRING(FIX(desired_vert_exag)),2)  

;---------------------------------------------------------------------------
; Allow user to select a different vertical exaggeration.
;---------------------------------------------------------------------------




;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

xx = 0
yy = 0
zz = 0
ndxs1 = 0
ndxs2 = 0
ndxsx = 0
ndxsy = 0
OBJ_DESTROY, Poly1
OBJ_DESTROY, Poly2
OBJ_DESTROY, Symbol1
OBJ_DESTROY, Symbol2

END  ;  Display3DHeights

;***************************************************************************
PRO PlotDataCube, x, y, z, xrange, yrange, zrange, wTopWorkBase
;***************************************************************************
; Create a 3D plot of the wind/height solution lines for cameras that succeed
; in a data cube that the user can rotate and inspect interactively.
;---------------------------------------------------------------------------

COMMON camnames, cam_name

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize parameters.
;---------------------------------------------------------------------------

   cam_color = [[  0,127,127], [  0,  0,255], [255,  0,  0], $
                [127,127,  0], [127,127,127], [  0,255,255], $
                [  0,255,  0], [127,  0,127], [255,255,  0]]
   col_name = ['Dk.Cyan','Blue','Red','Brown','Gray','Cyan', $
               'Green','Magenta','Yellow']

;---------------------------------------------------------------------------
; Get the range of data and the good cameras.
;---------------------------------------------------------------------------

   cam_good = INTARR(9)
   num_good = 0

   FOR icam = !SAV.Digitize.FIRST_CAM_USE, !SAV.Digitize.LAST_CAM_USE DO BEGIN
      del_y = [MIN(y[icam,*]),MAX(y[icam,*])]
      del_z = [MIN(z[icam,*]),MAX(z[icam,*])]
      IF ((del_y[1]-del_y[0]) NE 0.0 AND $
          (del_z[1]-del_z[0]) NE 0.0) THEN BEGIN
         cam_good[icam] = 1
         num_good += 1
      ENDIF
   ENDFOR

;---------------------------------------------------------------------------
; Prepare arrays to pass to MINX version of IDL'S XPLOT3D function.
;---------------------------------------------------------------------------

   num_lines = 2 + num_good
   num_xyz = FLTARR(num_lines)

   FOR iline=0,num_good-1 DO BEGIN
      num_xyz[iline] = (SIZE(x))[2]
   ENDFOR
   num_xyz[num_good] = 5
   num_xyz[num_good+1] = 2

   nxyzmax = (SIZE(x))[2] > 5

   xx = FLTARR(num_lines, nxyzmax)
   yy = FLTARR(num_lines, nxyzmax)
   zz = FLTARR(num_lines, nxyzmax)

   lincolor = INTARR(3,num_lines)
   linthick = INTARR(num_lines)
   linstyle = INTARR(num_lines)

;---------------------------------------------------------------------------
; Store the data for the good cameras.
;---------------------------------------------------------------------------

   igood = 0
   FOR icam = !SAV.Digitize.FIRST_CAM_USE, !SAV.Digitize.LAST_CAM_USE DO BEGIN
      IF (cam_good[icam]) THEN BEGIN
         xx[igood,*] = x[icam,*]
         yy[igood,*] = y[icam,*]
         zz[igood,*] = z[icam,*]
         lincolor[*,igood] = REFORM(cam_color[*,icam])
         linthick[igood] = 2
         linstyle[igood] = 0
         igood += 1
      ENDIF
   ENDFOR

;---------------------------------------------------------------------------
; For each camera, allow only 1 point on any edge of the cube.
;---------------------------------------------------------------------------

   FOR icam=0,igood-1 DO BEGIN
      FOR ipt1=0,nxyzmax-1 DO BEGIN
         IF (xx[icam,ipt1] NE xrange[1] AND yy[icam,ipt1] NE yrange[1] AND $
             zz[icam,ipt1] NE zrange[1]) THEN BREAK
      ENDFOR
      IF (ipt1 GT 1) THEN BEGIN
         xval = xx[icam,ipt1-1]
         yval = yy[icam,ipt1-1]
         zval = zz[icam,ipt1-1]
         FOR ipt2=0,ipt1-2 DO BEGIN
            xx[icam,ipt2] = xval
            yy[icam,ipt2] = yval
            zz[icam,ipt2] = zval
         ENDFOR
      ENDIF
   ENDFOR

   FOR icam=0,igood-1 DO BEGIN
      FOR ipt1=nxyzmax-1,0,-1 DO BEGIN
         IF (xx[icam,ipt1] NE xrange[0] AND yy[icam,ipt1] NE yrange[0] AND $
             zz[icam,ipt1] NE zrange[0]) THEN BREAK
      ENDFOR
      IF (ipt1 LT nxyzmax-2) THEN BEGIN
         xval = xx[icam,ipt1+1]
         yval = yy[icam,ipt1+1]
         zval = zz[icam,ipt1+1]
         FOR ipt2=nxyzmax-1,ipt1+2,-1 DO BEGIN
            xx[icam,ipt2] = xval
            yy[icam,ipt2] = yval
            zz[icam,ipt2] = zval
         ENDFOR
      ENDIF
   ENDFOR

;---------------------------------------------------------------------------
; Draw cube outline and heavy zero-wind line.
;---------------------------------------------------------------------------

   xx[igood,0:4] = [xrange[0], xrange[0], xrange[0], xrange[0], xrange[1]]
   yy[igood,0:4] = [yrange[0], yrange[0], yrange[1], yrange[0], yrange[0]]
   zz[igood,0:4] = [zrange[0], zrange[1], zrange[1], zrange[1], zrange[1]]
   lincolor[*,igood] = [0,0,0]
   linthick[igood] = 1
   linstyle[igood] = 0

   xx[igood+1,0:1] = [0.0,0.0]
   yy[igood+1,0:1] = [0.0,0.0]
   zz[igood+1,0:1] = [zrange[0],zrange[1]]
   lincolor[*,igood+1] = [0,0,0]
   linthick[igood+1] = 2
   linstyle[igood+1] = 0

;---------------------------------------------------------------------------
; Store lines to complete the outline of the cube.
;---------------------------------------------------------------------------

   wAminateBase = WIDGET_INFO(wTopWorkBase, /PARENT)

   camkey = ''
   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
      IF (cam_good[icam] NE 1) THEN CONTINUE
      camkey += col_name[icam] + '=' + !KON.Instr.CAM_NAMES[icam] + ',  '
   ENDFOR
   ilen = STRLEN(camkey)
   camkey = STRMID(camkey, 0, ilen-3)

   ptitle = 'Height/Wind Solution Lines by Camera;  ' + camkey

   oOrb = OBJ_NEW('orb', COLOR=[255,255, 255])  
   oOrb->Scale, .01, .01, .01  
   oSymbol = OBJ_NEW('IDLgrSymbol', oOrb)
   symbols = [oSymbol, oSymbol, oSymbol, oSymbol, oSymbol, $
              oSymbol, oSymbol, oSymbol, oSymbol]

   XPLOT3D, xx[0:igood+1,*], yy[0:igood+1,*], zz[0:igood+1,*], igood+2, $
            num_xyz, colors=lincolor, linestyles=linstyle, /MODAL, $
            symbols=symbols, thicks=linthick, GROUP=wAminateBase, $
            XRANGE=xrange, YRANGE=yrange, ZRANGE=zrange, TITLE=ptitle, $
            XTITLE='Speed Across (m/sec)', YTITLE='Speed Along (m/sec)', $
            ZTITLE='Height (m)'
  RETURN

END ;  PlotDataCube
