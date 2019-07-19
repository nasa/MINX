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
PRO GetScaleMinMax, DataAry, BadMin, BadMax, StdDevReject, DataMin, DataMax
;***************************************************************************
; Get the minimum and maximum values to use for display.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

DataMin = -!KON.Misc.BADVALUE_REAL
DataMax =  !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Filter out bad values.
;---------------------------------------------------------------------------

ndxs = WHERE(DataAry GT BadMin AND DataAry LT BadMax, numndxs)

IF (numndxs LE 0) THEN RETURN

data_vals = DataAry[ndxs]

;---------------------------------------------------------------------------
; Filter out outliers if requested.
;---------------------------------------------------------------------------

IF (StdDevReject GT 0.0) THEN BEGIN
   data_median = MEDIAN(data_vals, /EVEN)
   data_stddev = STDDEV(data_vals)
   ndxs = WHERE(ABS(data_median - data_vals) LT data_stddev * StdDevReject, $
                numndxs)
   IF (numndxs GT 0) THEN data_vals = data_vals[ndxs]
ENDIF

;---------------------------------------------------------------------------
; Find the min and max of the remaining values.
;---------------------------------------------------------------------------

DataMin = MIN(data_vals, MAX=DataMax)

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

ndxs = 0
data_vals = 0

END  ;  GetScaleMinMax

;***************************************************************************
PRO GetScaleParams, DataMin, DataMax, ScaleOverride, ScaleDelta, NumLevels
;***************************************************************************
; Find the appropriate value to use as the interval between annotations on a
; color scale or a graph's axis.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

ScaleDelta = !KON.Misc.BADVALUE_REAL
NumLevels  = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the range in data values.
;---------------------------------------------------------------------------

range = DataMax - DataMin

IF (range EQ 0.0) THEN RETURN

;---------------------------------------------------------------------------
; Get the scale difference per annotation level given the min and max values.
;---------------------------------------------------------------------------

ScaleDelta = (range LE 0.0100) ? .001 : $
              (range LE 0.0200) ? .002 : $
               (range LE 0.0500) ? .005 : $
                (range LE 0.1000) ? 0.01 : $
                 (range LE 0.2000) ? 0.02 : $
                  (range LE 0.5000) ? 0.05 : $
                   (range LE 1.0000) ? 0.10 : $
                    (range LE 2.0000) ? 0.20 : $
                     (range LE 5.0000) ? 0.50 : $
                      (range LE 10.000) ? 1.00 : $
                       (range LE 20.000) ? 2.00 : $
                        (range LE 50.000) ? 5.00 : $
                         (range LE 100.00) ? 10.0 : $
                          (range LE 200.00) ? 20.0 : $
                           (range EQ 360.00) ? 45.0 : $ ; <=
                            (range LE 500.00) ? 50.0 : $
                             (range LE 1000.0) ? 100. : $
                              (range LE 2000.0) ? 200. : $
                               (range LE 5000.0) ? 500. : $
                                (range LE 10000.) ? 1000.: $
                                 (range LE 20000.) ? 2000.: 4000.

;---------------------------------------------------------------------------
; Adjust the lower level down and the upper level up to be even multiples of
; the delta value. Make exceptions for geometric parameters.
;---------------------------------------------------------------------------

IF (ScaleOverride GT 0) THEN BEGIN
   IF (DataMin NE FIX(DataMin/ScaleDelta) * ScaleDelta) THEN BEGIN
      IF (DataMin GE 0.0) THEN BEGIN
         DataMin -= DataMin MOD ScaleDelta
      ENDIF ELSE BEGIN
         DataMin -= ScaleDelta - ABS(DataMin)
      ENDELSE
   ENDIF
   
   IF (DataMax NE FIX(DataMax/ScaleDelta) * ScaleDelta) THEN BEGIN
      IF (DataMax GE 0.0) THEN BEGIN
         DataMax += ScaleDelta - (DataMax MOD ScaleDelta)
      ENDIF ELSE BEGIN
         DataMax += ABS(DataMax) MOD ScaleDelta
      ENDELSE
   ENDIF
ENDIF
;---------------------------------------------------------------------------
; Compute the fractional and integer number of annotation values.
;---------------------------------------------------------------------------

range = DataMax - DataMin

NumLevels = CEIL(range / ScaleDelta) + 1

END  ;  GetScaleParams

;***************************************************************************
FUNCTION GetScaleUnits, DataType
;***************************************************************************
; Find the appropriate units to display with annotations on a color scale or
; a graph's axis.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; 
;---------------------------------------------------------------------------

RETURN, ScaleUnits

END  ;  GetScaleUnits

;***************************************************************************
PRO DestroyColorKey, RgnOrPrd
;***************************************************************************
; If the digitized region or product dialog box or color key windows are
; displayed, delete them and set the appropriate !VAL parameters to off.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

PLUME_TYPE = 1
SWATH_TYPE = 2

IF (RgnOrPrd EQ PLUME_TYPE) THEN BEGIN
   key_id = !VAR.DataRgn.COLORKEY_WNDW_ID
   dlg_id = !VAR.DataRgn.COLORKEY_DLG_ID
   bas_ID = -1
ENDIF ELSE BEGIN
   key_id = !VAR.DataSwath.COLORKEY_CONT_ID
   dlg_id = !VAR.DataSwath.COLORKEY_DLG_ID
   bas_ID = !VAR.DataSwath.COLORKEY_DISC_ID
ENDELSE

SafeWDELETE, key_id, DidIt
IF (dlg_id GT 0) THEN WIDGET_CONTROL, dlg_id, /DESTROY
IF (RgnOrPrd EQ SWATH_TYPE) THEN BEGIN
   IF (OBJ_VALID(!VAR.DataSwath.COLORKEY_DISC_OBJ)) THEN $
      !VAR.DataSwath.COLORKEY_DISC_OBJ.Delete
   IF (bas_ID GT 0) THEN BEGIN
      IF WIDGET_INFO(bas_ID, /VALID_ID) THEN BEGIN
         WIDGET_CONTROL, bas_ID, /DESTROY
         !VAR.DataSwath.OP_CHANGED = 1
      ENDIF
   ENDIF
ENDIF

IF (RgnOrPrd EQ PLUME_TYPE) THEN BEGIN
   !VAR.DataRgn.COLORKEY_WNDW_ID = key_id
   !VAR.DataRgn.COLORKEY_DLG_ID = -1
   !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_DO_NOT
ENDIF ELSE BEGIN
   !VAR.DataSwath.COLORKEY_CONT_ID = key_id
   !VAR.DataSwath.COLORKEY_DLG_ID = -1
   !VAR.DataSwath.COLORKEY_DISC_ID = -1
   !VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_DO_NOT
   !VAR.DataSwath.COLORKEY_DISC_OBJ = OBJ_NEW()
ENDELSE
   
END  ;  DestroyColorKey

;***************************************************************************
PRO ResetSwathData
;***************************************************************************
; If some process has changed the data in the OP window, reset the swath
; parameters that control the image of product data that the user had
; previously displayed there.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

!VAR.DataSwath.OP_CHANGED = 1
!VAR.DataSwath.DATA_TYPE = !KON.DataProd.TYPE_NONE
DestroyColorKey, 2
   
END  ;  ResetSwathData

;***************************************************************************
PRO DrawPlumeColorKey, State, ScaleOverride
;***************************************************************************
; Draw color key for plume heights displayed inside plume regions in
; animation window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set up display parameters,
;---------------------------------------------------------------------------

old_font = GetFontInfo(0)
save_wndw = !D.WINDOW
error_status = 0

;---------------------------------------------------------------------------
; Set min and max values to be used in color scale. If this adds a color key
; to the automatically captured image of a digitized region, then it can
; only be height. In that case, change default meters to km.
;---------------------------------------------------------------------------

MinValue = !VAR.DataRgn.VALUE_MIN_DLG[!VAR.DataRgn.DATA_TYPE]
MaxValue = !VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE]

IF (MaxValue LE MinValue) THEN MaxValue = MinValue + ABS(MinValue * 0.1)

;---------------------------------------------------------------------------
; Set annotation frequency and units text depending on data type and range.
;---------------------------------------------------------------------------

GetScaleParams, MinValue, MaxValue, ScaleOverride, annot_freq, num_annot_vals
units_text = !KON.DataRgn.DATA_UNITS[!VAR.DataRgn.DATA_TYPE]

;---------------------------------------------------------------------------
; Compute other parameters.
;---------------------------------------------------------------------------

line_per_color = 1
line_length = 30

annotation_lines = INTARR(num_annot_vals)

FOR iannot=0,num_annot_vals-1 DO BEGIN
   annotation_lines[iannot] = ROUND(iannot * (FLOAT(!D.TABLE_SIZE) / $
                                    (num_annot_vals - 1) * line_per_color))
   IF (annotation_lines[iannot] GE !D.TABLE_SIZE) THEN $
      annotation_lines[iannot] = !D.TABLE_SIZE - 1
ENDFOR

;---------------------------------------------------------------------------
; Set the beginning x/y coords relative to whichever window was chosen.
; First set an error catcher.
;---------------------------------------------------------------------------

CATCH, error_status
IF (error_status NE 0) THEN GOTO, key_err

ylines = line_per_color * !D.TABLE_SIZE

;---------------------------------------------------------------------------
; Set up color bar parameters for key drawn in separate window.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN BEGIN
   xbeg = !VAR.DataRgn.SIZE_X - line_length - 1
   ybeg = (((!VAR.DataRgn.SIZE_Y - line_per_color * !D.TABLE_SIZE) / 2) > 0) + 8

   NumCross = !VAR.DataRgn.SIZE_X
   NumAlong = !VAR.DataRgn.SIZE_Y

   center_x = (!VAR.DataRgn.SIZE_X + 20) / 2
ENDIF

;---------------------------------------------------------------------------
; Set up color bar parameters for key overlain on animation window image.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW OR $
    !VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_USER_SAVED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; If this is the first time it's being shown locate it near the left
   ; center of the current viewport.
   ;------------------------------------------------------------------------
    
   IF (!VAR.DataRgn.POS_X LT 0 OR !VAR.DataRgn.POS_Y LT 0) THEN BEGIN
      WIDGET_CONTROL, State.wDrawWindow, GET_DRAW_VIEW=viewport
      viewport_size = WIDGET_INFO(State.wDrawWindow, /GEOMETRY)
      !VAR.DataRgn.POS_X = viewport[0] + 150
      !VAR.DataRgn.POS_Y = viewport[1] + viewport_size.ysize / 2 + $
                           !VAR.DataRgn.SIZE_Y / 2
   ENDIF
 
   xbeg = !VAR.DataRgn.POS_X - line_length - 1
   ybeg = !VAR.DataRgn.POS_Y - line_per_color * !D.TABLE_SIZE

   NumCross = !VAR.DataRgn.POS_X
   NumAlong = !VAR.DataRgn.POS_Y

   IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
      SafeWSET, State.draw_win, didit
      NumCross += !VAR.DataRgn.SIZE_X
   ENDIF

   xcrd1 = xbeg - 50
   xcrd2 = xcrd1 + line_length * 3
   ycrd1 = ybeg - 30
   ycrd2 = ycrd1 + (!D.TABLE_SIZE - 1) * line_per_color + 45
   center_x = (xcrd1 + xcrd2) / 2

   ;------------------------------------------------------------------------
   ; Fill the background with the user's color choice and draw a box around
   ; the edge of the window if transparent background option wasn't selected.
   ;------------------------------------------------------------------------
   
   IF (!VAR.DataRgn.BKGRND_COLOR NE !KON.DataRgn.COLOR_TRANSP) THEN BEGIN
      POLYFILL, [xcrd1, xcrd1, xcrd2, xcrd2], [ycrd1, ycrd2, ycrd2, ycrd1], $
                COLOR=!VAR.DataRgn.BKGRND_COLOR
      PLOTS, [xcrd1, xcrd1, xcrd2-1, xcrd2-1, xcrd1], $
             [ycrd1+1, ycrd2, ycrd2, ycrd1+1, ycrd1+1], COLOR=0, THICK=1
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Set up color bar parameters for automatically-saved digitized region.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_SYS_SAVED) THEN BEGIN

   line_length = 15
   scale_bot = 30
   scale_top = 15

   y_ht = (!SAV.Digitize.DOCS_ON_MAPS) ? 36 : 0
   xbeg = !VAR.DataRgn.SIZE_X - line_length * 2
   ybeg = (((!VAR.DataRgn.SIZE_Y - line_per_color * !D.TABLE_SIZE - $
             y_ht - scale_top + scale_bot) / 2) > 0)

   NumCross = !VAR.DataRgn.POS_X + !VAR.DataRgn.SIZE_X
   NumAlong = !VAR.DataRgn.POS_Y + !VAR.DataRgn.SIZE_Y

   xcrd1 = xbeg - 47
   xcrd2 = xcrd1 + line_length * 5 + 2
   ycrd1 = ybeg - scale_bot
   ycrd2 = ycrd1 + (!D.TABLE_SIZE - 1) * line_per_color + scale_bot + scale_top
   center_x = (xcrd1 + xcrd2) / 2

   ;------------------------------------------------------------------------
   ; Fill the background with the user's color choice.
   ;------------------------------------------------------------------------
   
   IF (!VAR.DataRgn.BKGRND_COLOR NE !KON.DataRgn.COLOR_TRANSP) THEN $
      POLYFILL, [xcrd1, xcrd1, xcrd2, xcrd2], [ycrd1, ycrd2, ycrd2, ycrd1], $
                COLOR=!VAR.DataRgn.BKGRND_COLOR

   ;------------------------------------------------------------------------
   ; Draw a box around the edge of the window.
   ;------------------------------------------------------------------------
             
   PLOTS, [xcrd1, xcrd1, xcrd2-1, xcrd2-1, xcrd1], $
          [ycrd1+1, ycrd2, ycrd2, ycrd1+1, ycrd1+1], COLOR=0, THICK=1
ENDIF

;---------------------------------------------------------------------------
; Loop over the colors to draw.
;---------------------------------------------------------------------------

PLOT, [xbeg,xbeg+1], [ybeg,ybeg], XMARGIN=[0,0], YMARGIN=[0,0], $
      XRANGE=[0, NumCross-1], YRANGE=[0, NumAlong-1], XSTYLE=5, YSTYLE=5, $
      POSITION=[0,0,NumCross-1, NumAlong-1], /DEVICE, /NOERASE

FOR icolor=0,!D.TABLE_SIZE-1 DO BEGIN
   FOR iline=0,line_per_color-1 DO BEGIN
      ycrd = ybeg + icolor * line_per_color + iline
      OPLOT,[xbeg,xbeg+line_length],[ycrd,ycrd],COLOR=!SAV.ColorPalette[icolor]
   ENDFOR
ENDFOR

;---------------------------------------------------------------------------
; Draw a box around the color bar.
;---------------------------------------------------------------------------

yend = ybeg + !D.TABLE_SIZE - 1
PLOTS, [xbeg, xbeg+line_length, xbeg+line_length, xbeg, xbeg], $
       [ybeg, ybeg, yend, yend, ybeg], COLOR=0, THICK=1
   
;---------------------------------------------------------------------------
; Draw tick marks and write values and units.
;---------------------------------------------------------------------------

SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5
val_color = (!VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_BLACK OR $
             !VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_TRANSP) ? $
             !KON.DataRgn.COLOR_WHITE : !KON.DataRgn.COLOR_BLACK
sigfig = 0
IF (FIX(annot_freq) NE annot_freq) THEN BEGIN
   sigfig = 1
   IF (FIX(annot_freq * 10.)  NE annot_freq * 10.)  THEN sigfig = 2
   IF (FIX(annot_freq * 100.) NE annot_freq * 100.) THEN sigfig = 3
ENDIF

FOR iannot=0,num_annot_vals-1 DO BEGIN
   key_val = MinValue + annot_freq * iannot
   IF (sigfig EQ 0) THEN BEGIN
      key_str = STRTRIM(STRING(ROUND(MinValue + annot_freq * iannot)), 2)
   ENDIF ELSE BEGIN
      fmtstr = '(F5.' + ((sigfig EQ 1) ? '1)' : (sigfig EQ 2) ? '2)' : '3)')
      key_str = STRING(FORMAT=fmtstr, key_val)
   ENDELSE
   ycrd = ybeg + annotation_lines[iannot]

   OPLOT, [xbeg-5, xbeg], [ycrd, ycrd], COLOR=val_color
   XYOUTS, (xbeg-15) > 0, ycrd-2, key_str, /DEVICE, COLOR=val_color, $
           CHARSIZE=1.0, ALIGNMENT=1.0
ENDFOR

XYOUTS, center_x + 2, (ybeg-23) > 0, units_text, /DEVICE, $
        COLOR=val_color, CHARSIZE=1.1, ALIGNMENT=0.5

key_err: CATCH, /CANCEL

SetFontInfo, old_font
SafeWSET, save_wndw, didit

annotation_lines = 0

END  ;  DrawPlumeColorKey

;***************************************************************************
PRO ShowPlumeRegionOptions, State, CurFrame, LeftEdge, BottomEdge, Retval
;***************************************************************************
; Display the retrieved data color key in a separate window or in the
; animation window. We only get here if the user has selected to change the
; region display options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Here to remove the color key display.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_DO_NOT) THEN BEGIN
   key_id = !VAR.DataRgn.COLORKEY_WNDW_ID
   SafeWDELETE, key_id, DidIt
   !VAR.DataRgn.COLORKEY_WNDW_ID = key_id
ENDIF

;---------------------------------------------------------------------------
; Here to show the color key in a separate window.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Either use existing window or create a new window and set background
   ; to white, black or transparent. Then draw the color key in the window.
   ;------------------------------------------------------------------------

   SafeWSET, !VAR.DataRgn.COLORKEY_WNDW_ID, didit
   IF (didit LE 0) THEN BEGIN
      WINDOW, TITLE='', XPOS=LeftEdge, YPOS=BottomEdge, /FREE, $
              XSIZE=!VAR.DataRgn.SIZE_X + 20, YSIZE=!VAR.DataRgn.SIZE_Y
      !VAR.DataRgn.COLORKEY_WNDW_ID = !D.WINDOW
   ENDIF

   IF (!VAR.DataRgn.BKGRND_COLOR NE !KON.DataRgn.COLOR_TRANSP) THEN $
      ERASE, !VAR.DataRgn.BKGRND_COLOR

   DrawPlumeColorKey, State, 0

ENDIF

;---------------------------------------------------------------------------
; Here to show the color key overplotted on the animation window.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
 
   ;------------------------------------------------------------------------
   ; Draw the color key on the MINX animation window. If the color key is
   ; already visible in a separate window, delete that window first.
   ;------------------------------------------------------------------------

   SafeWDELETE, !VAR.DataRgn.COLORKEY_WNDW_ID, didit
   !VAR.DataRgn.COLORKEY_WNDW_ID = -1
   DrawPlumeColorKey, State, 0

ENDIF

RedrawWindow, State, CurFrame

END  ;  ShowPlumeRegionOptions

;***************************************************************************
PRO DrawSwathColorKey, State
;***************************************************************************
; Draw color key for data products displayed inside entire area of animation
; window. This is for products that have continuous values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Exit if we're not ready to display the color scale or if the current
; window is not the OP window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_DO_NOT OR (State.curframe NE 0 AND $
    !VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW)) THEN RETURN

;---------------------------------------------------------------------------
; Set up display parameters.
;---------------------------------------------------------------------------

old_font = GetFontInfo(0)
save_wndw = !D.WINDOW
error_status = 0

;---------------------------------------------------------------------------
; Get annotation parameters.
;---------------------------------------------------------------------------

data_min = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinDlg
data_max = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxDlg

GetScaleParams, data_min, data_max, 0, AnnotFreq, NumAnnotVals

line_per_color = 1
line_length = 30

annotation_lines = INTARR(NumAnnotVals)

FOR iannot=0,NumAnnotVals-1 DO BEGIN
   annotation_lines[iannot] = ROUND(iannot * (FLOAT(!D.TABLE_SIZE) / $
                                    (NumAnnotVals - 1) * line_per_color))
   IF (annotation_lines[iannot] GE !D.TABLE_SIZE) THEN $
      annotation_lines[iannot] = !D.TABLE_SIZE - 1
ENDFOR

;---------------------------------------------------------------------------
; Set the beginning x/y coords relative to whichever window was chosen.
; First set an error catcher.
;---------------------------------------------------------------------------

CATCH, error_status
IF (error_status NE 0) THEN GOTO, key_err2

ylines = line_per_color * !D.TABLE_SIZE

;---------------------------------------------------------------------------
; Set up color bar parameters for key drawn in separate window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN BEGIN
   xbeg = !VAR.DataSwath.SIZE_X - line_length
   ybeg = (((!VAR.DataSwath.SIZE_Y - line_per_color * !D.TABLE_SIZE) / 2) > 0) + 8
      
   NumCross = !VAR.DataSwath.SIZE_X
   NumAlong = !VAR.DataSwath.SIZE_Y
   
   center_x = (!VAR.DataSwath.SIZE_X + 20) / 2
   delX = 1

   ERASE, !VAR.DataSwath.BKGRND_COLOR
ENDIF

;---------------------------------------------------------------------------
; Set up color bar parameters for key overlain on animation window image.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
   
   ;------------------------------------------------------------------------
   ; If this is the first time it's being shown locate it near the left
   ; center of the current viewport.
   ;------------------------------------------------------------------------

   IF (!VAR.DataSwath.POS_X LT 0 OR !VAR.DataSwath.POS_Y LT 0) THEN BEGIN
      WIDGET_CONTROL, State.wDrawWindow, GET_DRAW_VIEW=viewport
      viewport_size = WIDGET_INFO(State.wDrawWindow, /GEOMETRY)
      !VAR.DataSwath.POS_X = viewport[0] + 150
      !VAR.DataSwath.POS_Y = viewport[1] + viewport_size.ysize / 2 + $
                             !VAR.DataSwath.SIZE_Y / 2
   ENDIF
   
   xbeg = !VAR.DataSwath.POS_X - line_length
   ybeg = !VAR.DataSwath.POS_Y - line_per_color * !D.TABLE_SIZE
   
   NumCross = !VAR.DataSwath.POS_X
   NumAlong = !VAR.DataSwath.POS_Y
   
   IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
      SafeWSET, State.draw_win, didit
      NumCross += !VAR.DataSwath.SIZE_X
   ENDIF
   
   xcrd1 = xbeg - 50
   xcrd2 = xcrd1 + line_length * 3
   ycrd1 = ybeg - 30
   ycrd2 = ycrd1 + (!D.TABLE_SIZE - 1) * line_per_color + 45
   center_x = (xcrd1 + xcrd2) / 2
   delX = 0
   
   ;------------------------------------------------------------------------
   ; Fill the background with the user's color choice and draw a box around
   ; the edge of the window if transparent background option wasn't selected.
   ;------------------------------------------------------------------------
   
   IF (!VAR.DataSwath.BKGRND_COLOR NE !KON.DataRgn.COLOR_TRANSP) THEN BEGIN
      POLYFILL, [xcrd1, xcrd1, xcrd2, xcrd2], [ycrd1, ycrd2, ycrd2, ycrd1], $
                COLOR=!VAR.DataSwath.BKGRND_COLOR, /DEVICE
      PLOTS, [xcrd1, xcrd1, xcrd2, xcrd2, xcrd1], $
             [ycrd1+1, ycrd2, ycrd2, ycrd1+1, ycrd1+1], COLOR=0, $
             THICK=1, /DEVICE
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Loop over the colors to draw.
;------------------------------------------------------------------------

PLOT, [xbeg,xbeg+1], [ybeg,ybeg], XMARGIN=[0,0], YMARGIN=[0,0], $
      XRANGE=[0, NumCross-1], YRANGE=[0, NumAlong-1], XSTYLE=5, YSTYLE=5, $
      POSITION=[0,0,NumCross-1, NumAlong-1], /DEVICE, /NOERASE

FOR icolor=0,!D.TABLE_SIZE-1 DO BEGIN
   FOR iline=0,line_per_color-1 DO BEGIN
      ycrd = ybeg + icolor * line_per_color + iline
      OPLOT,[xbeg,xbeg+line_length],[ycrd,ycrd],COLOR=!SAV.ColorPalette[icolor]
   ENDFOR
ENDFOR

;---------------------------------------------------------------------------
; Draw a box around the color bar.
;---------------------------------------------------------------------------

ycrd1 = 0 + icolor * line_per_color + iline
ycrd2 = (!D.TABLE_SIZE - 1) + icolor * line_per_color + iline

yend = ybeg + !D.TABLE_SIZE - 1
PLOTS, [xbeg, xbeg+line_length-delX, xbeg+line_length-delX, xbeg, xbeg], $
       [ybeg, ybeg, yend, yend, ybeg], COLOR=0, THICK=1
   
;------------------------------------------------------------------------
; Write the annotation.
;------------------------------------------------------------------------
    
SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5
val_color = (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_BLACK OR $
             !VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_TRANSP) ? $
             !KON.DataRgn.COLOR_WHITE : !KON.DataRgn.COLOR_BLACK

sigfig = 0
IF (FIX(AnnotFreq) NE AnnotFreq) THEN BEGIN
   sigfig = 1
   IF (FIX(AnnotFreq * 10.)  NE AnnotFreq * 10.)  THEN sigfig = 2
   IF (FIX(AnnotFreq * 100.) NE AnnotFreq * 100.) THEN sigfig = 3
ENDIF

data_min = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinDlg

FOR iannot=0,NumAnnotVals-1 DO BEGIN
   key_val = data_min + AnnotFreq * iannot
   IF (sigfig EQ 0) THEN BEGIN
      key_str = STRTRIM(STRING(ROUND(data_min + AnnotFreq * iannot)), 2)
   ENDIF ELSE BEGIN
      fmtstr = '(F5.' + ((sigfig EQ 1) ? '1)' : (sigfig EQ 2) ? '2)' : '3)')
      key_str = STRING(FORMAT=fmtstr, key_val)
   ENDELSE
   ycrd = ybeg + annotation_lines[iannot]

   OPLOT, [xbeg-5, xbeg], [ycrd, ycrd], COLOR=val_color
   XYOUTS, (xbeg-15) > 0, ycrd-2, key_str, /DEVICE, COLOR=val_color, $
           CHARSIZE=1.0, ALIGNMENT=1.0
ENDFOR

Units = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].Units

XYOUTS, center_x + 3, (ybeg-23) > 0, Units, /DEVICE, COLOR=val_color, $
        CHARSIZE=1.1, ALIGNMENT=0.5

key_err2: CATCH, /CANCEL

SetFontInfo, old_font
SafeWSET, save_wndw, didit

annotation_lines = 0

END  ;  DrawSwathColorKey

;***************************************************************************
PRO ShowMISRSwathOptions, State, LeftEdge, BottomEdge, Retval
;***************************************************************************
; Display the retrieved data color key in a separate window or in the
; animation window. We only get here if the user has selected to change the
; region display options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Here to show the color key in a separate window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Either use existing window or create a new window and set background
   ; to white, black or transparent. Then draw the color key in the window.
   ;------------------------------------------------------------------------
   
   SafeWSET, !VAR.DataSwath.COLORKEY_CONT_ID, didit
   IF (didit LE 0) THEN BEGIN
      WINDOW, TITLE='', XPOS=LeftEdge, YPOS=BottomEdge, /FREE, $
              XSIZE=!VAR.DataSwath.SIZE_X + 20, YSIZE=!VAR.DataSwath.SIZE_Y
      !VAR.DataSwath.COLORKEY_CONT_ID = !D.WINDOW
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Here to show the color key overplotted on the animation OP window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Draw the color key on the MINX OP window. If the color key is already
   ; visible in a separate window, delete that window first.
   ;------------------------------------------------------------------------
   
   SafeWDELETE, !VAR.DataSwath.COLORKEY_CONT_ID, didit
   !VAR.DataSwath.COLORKEY_CONT_ID = -1
ENDIF

RedrawWindow, State, State.curFrame
   
END  ;  ShowMISRSwathOptions

;***************************************************************************
PRO CreateSwathContinuousImage, ImageIn
;***************************************************************************
; Create the color image for the MISR swath product display.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

WorkImage[*,*,*] = 0.0

;---------------------------------------------------------------------------
; Modify colors as needed and fill color index array.
;---------------------------------------------------------------------------

color_index = $
   ROUND((FLOAT(ImageIn)-!VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinDlg) / $
   (!VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxDlg - $
    !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinDlg) * !D.TABLE_SIZE) + 1

ndxs = WHERE(color_index LT 0, numndxs)
IF (numndxs GT 0) THEN color_index[ndxs] = 0

ndxs = WHERE(color_index GT !D.TABLE_SIZE, numndxs)
IF (numndxs GT 0) THEN color_index[ndxs] = !D.TABLE_SIZE

WorkImage[*,*,0] = R_CURR[color_index]
WorkImage[*,*,1] = G_CURR[color_index]
WorkImage[*,*,2] = B_CURR[color_index]

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

ndxs = 0
color_index = 0
ImageIn = 0

END  ;  CreateSwathContinuousImage

;***************************************************************************
FUNCTION ExWidWinMouseNULLHandler, oWin, x, y, KeyMods
;***************************************************************************
; Stops our color bar window from allowing user transformations
;---------------------------------------------------------------------------
   
RETURN, 0 ; Perform default event handling

END

;***************************************************************************
PRO CreateSwathDiscreteImage, State
;***************************************************************************
; Display the map of a discrete data type in work window. Convert data
; values into color indices into RGB arrays, then create the color image and
; its color bar.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON COLORS,   R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

;---------------------------------------------------------------------------
; No need to do this if the image you want is already in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   DestroyColorKey, 1

   WorkImage[*,*,*] = 0.0
   data_type = !VAR.DataSwath.DATA_TYPE
   
   ;------------------------------------------------------------------------
   ; Load the blue-red color palette and modify it to get ideal colors.
   ;------------------------------------------------------------------------
   
   r_or = R_ORIG & g_or = G_ORIG & b_or = B_ORIG
   r_cu = R_CURR & g_cu = G_CURR & b_cu = B_CURR
   
   LOADCT, !KON.Colors.BlueRedPalette
   TVLCT, RR, GG, BB, /GET
   
   R_ORIG = r_or & G_ORIG = g_or & B_ORIG = b_or
   R_CURR = r_cu & G_CURR = g_cu & B_CURR = b_cu
 
   ;------------------------------------------------------------------------
   ; Process the land-water data type from the AGP.
   ;------------------------------------------------------------------------
   
   IF (data_type EQ !KON.DataProd.TYPE_LANDH2O_MASK) THEN BEGIN

      IF (~ !VAR.CurrFiles.AGP_Loaded) THEN RETURN
      
      rsize = SIZE(Land_Water_Mask)
      temp_image = REBIN(Land_Water_Mask, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
      
      ;---------------------------------------------------------------------
      ; Determine the color index for each pixel in the land-water map.
      ;---------------------------------------------------------------------
   
      item_color_ndx = [75,185,239,115,142,58,30]
      pixel_color_ndx = ROUND(temp_image) * 0
   
      num_items = N_ELEMENTS(item_color_ndx)
      FOR iItem=0,num_items-2 DO BEGIN
         ndxs = WHERE(temp_image EQ iItem, numndxs)
         IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = item_color_ndx[iItem]
      ENDFOR
      ndxs = WHERE(temp_image EQ $
                   !VAR.DataProd[data_type].BadVal, numndxs)
      IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = 0
      temp_image = 0
   
      ;---------------------------------------------------------------------
      ; Modify the blue-red color palette as needed.
      ;---------------------------------------------------------------------
   
      RR[0] = 31
      GG[0] = 0
      BB[0] = 127
   
      ;---------------------------------------------------------------------
      ; Set up colorbar parameters.
      ;---------------------------------------------------------------------
   
      colorbar_title = 'Land-Water Map'
      colorbar_xsize = 275
      colorbar_ysize = 200
      colorbar_items = ['Shallow Ocean', 'Land', 'Coastline', $
                        'Shallow Inland Water', 'Ephemeral Water', $
                        'Deep Inland Water', 'Deep Ocean']
   
      cb_rgb = BYTARR(3,num_items)
      cb_rgb[0,*] = [RR[75], RR[185], RR[239], RR[115], RR[142],RR[58], RR[30]]
      cb_rgb[1,*] = [GG[75], GG[185], GG[239], GG[115], GG[142],GG[58], GG[30]]
      cb_rgb[2,*] = [BB[75], BB[185], BB[239], BB[115], BB[142],BB[58], BB[30]]
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Process the biome classifiers data type from MODIS.
   ;------------------------------------------------------------------------
   
   IF (data_type EQ !KON.DataProd.TYPE_BIOME_MAP) THEN BEGIN
   
      IF (~ !VAR.CurrFiles.Biome_Loaded) THEN RETURN
   
      ;---------------------------------------------------------------------
      ; Determine the color index for each pixel in the biome map.
      ;---------------------------------------------------------------------
         
      item_color_ndx = [1,2,3,4,5,6,138,147,162,169,11,21,8,7,9,255,10,0]
      pixel_color_ndx = ROUND(Biome_swath) * 0
   
      num_items = N_ELEMENTS(item_color_ndx)
      FOR iItem=0,num_items-2 DO BEGIN
         ndxs = WHERE(Biome_swath EQ iItem, numndxs)
         IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = item_color_ndx[iItem]
      ENDFOR
      ndxs = WHERE(Biome_swath EQ $
                   !VAR.DataProd[data_type].BadVal, numndxs)
      IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = 0
   
      ;---------------------------------------------------------------------
      ; Modify the blue-red color palette as needed.
      ;---------------------------------------------------------------------
      
      RR[0]   =   0 & GG[0]   =   0 & BB[0]   =   0 ; black
      RR[1]   = 182 & GG[1]   = 235 & BB[1]   = 255 ; very light blue
      RR[2]   =   0 & GG[2]   =  95 & BB[2]   =   0 ; very dark green
      RR[3]   =   0 & GG[3]   = 130 & BB[3]   =   0 ; dark green
      RR[4]   =   0 & GG[4]   = 175 & BB[4]   =   0 ; green
      RR[5]   =   0 & GG[5]   = 215 & BB[5]   =   0 ; light green
      RR[6]   =  80 & GG[6]   = 255 & BB[6]   =  80 ; very light green
      RR[7]   = 255 & GG[7]   =   0 & BB[7]   = 255 ; magenta
      RR[8]   = 211 & GG[8]   =   0 & BB[8]   =   0 ; dark red
      RR[9]   = 255 & GG[9]   = 143 & BB[9]   =  63 ; orange red
      RR[10]  = 255 & GG[10]  = 225 & BB[10]  = 127 ; light brown
      RR[11]  = 199 & GG[11]  = 199 & BB[11]  =  54 ; green orange
      RR[255] = 255 & GG[255] = 255 & BB[255] = 255 ; white
      
      ;---------------------------------------------------------------------
      ; Set up colorbar parameters.
      ;---------------------------------------------------------------------
      
      colorbar_title = 'Biome Classification'
      colorbar_xsize = 275
      colorbar_ysize = 400
      colorbar_items = $
        ['Water',                       'Evergreen Needleleaf forest', $
         'Evergreen Broadleaf forest',  'Deciduous Needleleaf forest', $
         'Deciduous Broadleaf forest',  'Mixed forest',                $
         'Closed shrublands',           'Open shrublands',             $
         'Woody savannas',              'Savannas',                    $
         'Grasslands',                  'Permanent wetlands',          $
         'Croplands',                   'Urban and built-up',          $
         'Cropland/Natural veg mosaic', 'Snow and ice',                $
         'Barren or sparsely vegetated','Unclassified']
         
      cb_rgb = BYTARR(3,num_items)
      cb_rgb[0,*] = [RR[1],   RR[2],   RR[3],   RR[4],   RR[5],  RR[6],  $
                     RR[138], RR[147], RR[162], RR[169], RR[11], RR[21], $
                     RR[8],   RR[7],   RR[9],   RR[255], RR[10], RR[0]]
      cb_rgb[1,*] = [GG[1],   GG[2],   GG[3],   GG[4],   GG[5],  GG[6],  $
                     GG[138], GG[147], GG[162], GG[169], GG[11], GG[21], $
                     GG[8],   GG[7],   GG[9],   GG[255], GG[10], GG[0]]
      cb_rgb[2,*] = [BB[1],   BB[2],   BB[3],   BB[4],   BB[5],  BB[6],  $
                     BB[138], BB[147], BB[162], BB[169], BB[11], BB[21], $
                     BB[8],   BB[7],   BB[9],   BB[255], BB[10], BB[0]]
   ENDIF
  
   ;------------------------------------------------------------------------
   ; Process the stereo cloud mask and new cloud mask data type.
   ;------------------------------------------------------------------------
   
   IF (data_type EQ !KON.DataProd.TYPE_STER_SDCM   OR $
       data_type EQ !KON.DataProd.TYPE_CLDZ_CLDMSK OR $
       data_type EQ !KON.DataProd.TYPE_CLDC_SDCM   OR $
       data_type EQ !KON.DataProd.TYPE_CLD_CLDMSK) THEN BEGIN
 
      IF (data_type EQ !KON.DataProd.TYPE_STER_SDCM) THEN BEGIN
         rsize = SIZE(STER_SDCM)
         temp_image = REBIN(STER_SDCM, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'Stereo SDCM Cloud Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLDZ_CLDMSK) THEN BEGIN
         rsize = SIZE(CLDZ_CldMsk)
         temp_image = REBIN(CLDZ_CldMsk, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'Cloud Zero-wind Cloud Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLDC_SDCM) THEN BEGIN
         rsize = SIZE(CLDC_SDCM)
         temp_image = REBIN(CLDC_SDCM, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'Cloud SDCM Cloud Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLD_CLDMSK) THEN BEGIN
         rsize = SIZE(CLD_CldMsk)
         temp_image = REBIN(CLD_CldMsk, rsize[1] * 64, rsize[2] * 64, /SAMPLE)
         colorbar_title = 'Cloud Mask'
      ENDIF

      ;---------------------------------------------------------------------
      ; Determine the color index for each pixel in the masks.
      ;---------------------------------------------------------------------
    
      item_color_ndx = [0,133,174,215]
      pixel_color_ndx = ROUND(temp_image) * 0
    
      num_items = N_ELEMENTS(item_color_ndx)
      FOR iItem=0,num_items-1 DO BEGIN
         ndxs = WHERE(temp_image EQ iItem, numndxs)
         IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = item_color_ndx[iItem]
      ENDFOR
      temp_image = 0
    
      ;---------------------------------------------------------------------
      ; Set up colorbar parameters.
      ;---------------------------------------------------------------------
   
      colorbar_xsize = 275
      colorbar_ysize = 200
      colorbar_items = ['No Retrieval', 'Cloud High Confidence', $
                        'Cloud Low Confidence', 'Near Surface Likely']
          
      cb_rgb = BYTARR(3,num_items)
      cb_rgb[0,*] = [RR[0], RR[133], RR[174], RR[215]]
      cb_rgb[1,*] = [GG[0], GG[133], GG[174], GG[215]]
      cb_rgb[2,*] = [BB[0], BB[133], BB[174], BB[215]]
   ENDIF

   ;------------------------------------------------------------------------
   ; Process the stereo cloud mask and new cloud mask data type.
   ;------------------------------------------------------------------------
   
   IF (data_type EQ !KON.DataProd.TYPE_CLASS_SMOKE  OR $
       data_type EQ !KON.DataProd.TYPE_CLASS_DUST   OR $
       data_type EQ !KON.DataProd.TYPE_CLASS_CLOUD  OR $
       data_type EQ !KON.DataProd.TYPE_CLASS_CLEAR) THEN BEGIN
 
      IF (data_type EQ !KON.DataProd.TYPE_CLASS_SMOKE) THEN BEGIN
         rsize = SIZE(Smoke_Confid)
         temp_image = REBIN(Smoke_Confid, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'SVM Smoke Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLASS_DUST) THEN BEGIN
         rsize = SIZE(Dust_Confid)
         temp_image = REBIN(Dust_Confid, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'SVM Dust Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLASS_CLOUD) THEN BEGIN
         rsize = SIZE(Cloud_Confid)
         temp_image = REBIN(Cloud_Confid, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'SVM Cloud Mask'
      ENDIF
      IF (data_type EQ !KON.DataProd.TYPE_CLASS_CLEAR) THEN BEGIN
         rsize = SIZE(Land_Confid)
         temp_image = REBIN(Land_Confid, rsize[1] * 4, rsize[2] * 4, /SAMPLE)
         colorbar_title = 'SVM Land Mask'
      ENDIF
      
      ;---------------------------------------------------------------------
      ; Determine the color index for each pixel in the masks. First set up
      ; the colors to use.
      ;---------------------------------------------------------------------
      
      RR[0] =   0 & GG[0] =   0 & BB[0] =   0
      RR[1] =   0 & GG[1] = 196 & BB[1] =   0
      RR[2] = 127 & GG[2] = 255 & BB[2] = 127
      RR[3] = 255 & GG[3] = 127 & BB[3] = 127
      RR[4] = 196 & GG[4] =   0 & BB[4] =   0
      
      item_color_ndx = [0,1,2,3,4]
      pixel_color_ndx = ROUND(temp_image) * 0
      
      num_items = N_ELEMENTS(item_color_ndx)
      FOR iItem=0,num_items-1 DO BEGIN
         ndxs = WHERE(temp_image EQ iItem, numndxs)
         IF (numndxs GT 0) THEN pixel_color_ndx[ndxs] = item_color_ndx[iItem]
      ENDFOR
      temp_image = 0
      
      ;---------------------------------------------------------------------
      ; Set up colorbar parameters.
      ;---------------------------------------------------------------------
      
      colorbar_xsize = 260
      colorbar_ysize = 200
      colorbar_items = ['No Retrieval', 'Yes: High Confidence', $
                        'Yes: Low Confidence', 'No: Low Confidence', $
                        'No: High Confidence']
         
      cb_rgb = BYTARR(3,num_items)
      cb_rgb[0,*] = [RR[0], RR[1], RR[2], RR[3], RR[4]]
      cb_rgb[1,*] = [GG[0], GG[1], GG[2], GG[3], GG[4]]
      cb_rgb[2,*] = [BB[0], BB[1], BB[2], BB[3], BB[4]]
   ENDIF

   item_color_ndx = 0
   ndxs = 0
   
   ;------------------------------------------------------------------------
   ; Copy the pixel color indices into the work (OP) window array.
   ;------------------------------------------------------------------------
   
   WorkImage[*,*,0] = RR[pixel_color_ndx]
   WorkImage[*,*,1] = GG[pixel_color_ndx]
   WorkImage[*,*,2] = BB[pixel_color_ndx]
   
   RR = 0 & GG = 0 & BB = 0
   pixel_color_ndx = 0
   
   ;------------------------------------------------------------------------
   ; Draw colorbar in a separate window with vertical orientation and labels
   ; on the right-hand side.
   ;------------------------------------------------------------------------

   !VAR.DataSwath.COLORKEY_DISC_ID = $
      WIDGET_BASE(TITLE=colorbar_title, GROUP_LEADER=State.wAnimateBase, $
                  /FLOATING, EVENT_PRO='SwathToolbarEventHandler', $
                  /TLB_KILL_REQUEST_EVENTS)
   wDraw = WIDGET_WINDOW(!VAR.DataSwath.COLORKEY_DISC_ID, XSIZE=colorbar_xsize, $
                         YSIZE=colorbar_ysize, SENSITIVE=0, FRAME=2, $
                          MOUSE_MOTION_HANDLER='ExWidWinMouseNULLHandler', $
                          MOUSE_DOWN_HANDLER='ExWidWinMouseNULLHandler', $
                          MOUSE_UP_HANDLER='ExWidWinMouseNULLHandler', $
                          MOUSE_WHEEL_HANDLER='ExWidWinMouseNULLHandler')
   WIDGET_CONTROL, !VAR.DataSwath.COLORKEY_DISC_ID, /REALIZE

   !VAR.DataSwath.COLORKEY_DISC_OBJ = $
      COLORBAR(POSITION=[0.05,0.05,0.12,0.95], /NORMAL, RGB_TABLE=cb_rgb, $
               ORIENTATION=1, TICKNAME=colorbar_items, SUBTICKLEN=0, $
               TEXTPOS=1, FONT_NAME='HELVETICA', FONT_STYLE='Bold', $
               FONT_SIZE=12, TICKLAYOUT=1, TAPER=0, BORDER=2)

   cb_rgb = 0
ENDIF

;---------------------------------------------------------------------------
; Display the images in the work window.
;---------------------------------------------------------------------------

!VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_DO_NOT

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  CreateSwathDiscreteImage

;***************************************************************************
PRO SwathToolbarEventHandler, Event
;***************************************************************************
; Process the kill event if the user destroys it with window system button.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
   DestroyColorKey, 2
   
END  ;  SwathToolbarEventHandler

;***************************************************************************
PRO ShowAGPInWorkWndw, State
;***************************************************************************
; Display the AGP data in work window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON agp_data, Land_Water_Mask, Terrain_Hts

;---------------------------------------------------------------------------
; Create image if it is not present in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window and AGP parameters.
   ;------------------------------------------------------------------------

   WorkImage[*,*,*] = 0.0

   ;------------------------------------------------------------------------
   ; Set the min/max and scale to the full resolution of the work window.
   ; Don't interpolate these parameters.
   ;------------------------------------------------------------------------

   temp_image = Terrain_Hts
   rsize = SIZE(temp_image)
   temp_image = REBIN(temp_image, rsize[1] * 4, rsize[2] * 4, /SAMPLE)

   ;------------------------------------------------------------------------
   ; Create the color image and display in the work window.
   ;------------------------------------------------------------------------

   CreateSwathContinuousImage, temp_image
ENDIF

;------------------------------------------------------------------------
; Display the image in the work window. Only copy the image in if it was
; just created.
;------------------------------------------------------------------------

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowAGPInWorkWndw

;***************************************************************************
PRO ShowGMPInWorkWndw, State
;***************************************************************************
; Display the geometric parameters data in work window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith

DEBUG_PLT = 0 ; use 0 for production and to display interpolated camera
              ; zenith and azimuth angles; use 2 for debug to display
              ; uninterpolated angles

;---------------------------------------------------------------------------
; Create image if it is not present in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window and GMP parameters.
   ;------------------------------------------------------------------------
   
   WorkImage[*,*,*] = 0.0
   data_type = !VAR.DataSwath.DATA_TYPE
   
   IF (data_type EQ !KON.DataProd.TYPE_SUN_ZENITH) THEN $
      temp_image = Sun_Zenith
   
   IF (data_type EQ !KON.DataProd.TYPE_SUN_AZIMUTH) THEN $
      temp_image = Sun_Azimuth
   
   IF (data_type GE !KON.DataProd.TYPE_CAM_ZEN_DF AND $
       data_type LE !KON.DataProd.TYPE_CAM_ZEN_DA) THEN BEGIN
      icam = data_type - !KON.DataProd.TYPE_CAM_ZEN_DF
      temp_image = REFORM(Cam_Zenith[icam,*,*])
   ENDIF
   
   IF (data_type GE !KON.DataProd.TYPE_CAM_AZI_DF AND $
       data_type LE !KON.DataProd.TYPE_CAM_AZI_DA) THEN BEGIN
      icam = data_type - !KON.DataProd.TYPE_CAM_AZI_DF
      temp_image = REFORM(Cam_Azimuth[icam,*,*])
   ENDIF
   
   IF (data_type GE !KON.DataProd.TYPE_CAM_SCT_DF AND $
       data_type LE !KON.DataProd.TYPE_CAM_SCT_DA) THEN BEGIN
      icam = data_type - !KON.DataProd.TYPE_CAM_SCT_DF
      temp_image = REFORM(Cam_Scatter[icam,*,*])
   ENDIF
   
   IF (data_type GE !KON.DataProd.TYPE_CAM_GLT_DF AND $
       data_type LE !KON.DataProd.TYPE_CAM_GLT_DA) THEN BEGIN
      icam = data_type - !KON.DataProd.TYPE_CAM_GLT_DF
      temp_image = REFORM(Cam_Glitter[icam,*,*])
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Scale the data to the full resolution of the work window. Interpolate
   ; these parameters, because MINX interpolates when retrieving hts. Camera
   ; zenith angles have a minimum across the swath, but only the An camera
   ; needs to be handled specially before interpolation (fix_AN_zenith).
   ; Azimuth angles need to be adjusted if there is a 0=360 transition.
   ;------------------------------------------------------------------------
   
   Zen1_Azi2 = 0
   fix_AN_zenith = (data_type EQ !KON.DataProd.TYPE_CAM_ZEN_AN) ? 1 : 0
   fix_azimuth = (data_type GE !KON.DataProd.TYPE_CAM_AZI_DF AND $
                  data_type LE !KON.DataProd.TYPE_CAM_AZI_DA) ? -1 : 0
   IF (data_type EQ !KON.DataProd.TYPE_CAM_AZI_AN) THEN fix_azimuth = -2
      
   rsize = SIZE(temp_image)
   InterpolateGeomData, Zen1_Azi2, temp_image, fix_AN_zenith, fix_azimuth, $
                        DEBUG_PLT, rsize[1:2], rsize[1:2]*64, temp_image0, $
                        status
   
   ;------------------------------------------------------------------------
   ; Create the color image and display the images in the work window.
   ;------------------------------------------------------------------------
   
   CreateSwathContinuousImage, temp_image0

   ndxs = 0
   xndxs = 0
   yndxs = 0
   xyndxs = 0
   temp_image = 0
   temp_image0 = 0
ENDIF

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowGMPInWorkWndw

;***************************************************************************
PRO ShowAERInWorkWndw, State
;***************************************************************************
; Display the aerosol data in work window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid

;---------------------------------------------------------------------------
; No need to do this if the image you want is already in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window and aerosol parameters.
   ;------------------------------------------------------------------------

   WorkImage[*,*,*] = 0.0
   data_type = !VAR.DataSwath.DATA_TYPE

   IF (data_type GE !KON.DataProd.TYPE_AER_BE_TAU_B AND $
       data_type LE !KON.DataProd.TYPE_AER_BE_TAU_N) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_AER_BE_TAU_B
      temp_image = REFORM(Tau_BE_Grid[iband,*,*])
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_AER_SSA_BLU AND $
       data_type LE !KON.DataProd.TYPE_AER_SSA_NIR) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_AER_SSA_BLU
      temp_image = REFORM(Ssa_Grid[iband,*,*])
   ENDIF

   IF (data_type EQ !KON.DataProd.TYPE_AER_BE_ANGXP) THEN $
      temp_image = Angexp_BE_Grid

   IF (data_type GE !KON.DataProd.TYPE_AER_FRC_SML AND $
       data_type LE !KON.DataProd.TYPE_AER_FRC_LRG) THEN BEGIN
      item = data_type - !KON.DataProd.TYPE_AER_FRC_SML
      temp_image = REFORM(Taufrac_Grid[item,*,*])
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_AER_FRC_SPH AND $
       data_type LE !KON.DataProd.TYPE_AER_FRC_NOS) THEN BEGIN
      item = data_type - !KON.DataProd.TYPE_AER_FRC_SPH + 3
      temp_image = REFORM(Taufrac_Grid[item,*,*])
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_AER_LR_TAU_B AND $
       data_type LE !KON.DataProd.TYPE_AER_LR_TAU_N) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_AER_LR_TAU_B
      temp_image = REFORM(Tau_LR_Grid[iband,*,*])
   ENDIF

   IF (data_type EQ !KON.DataProd.TYPE_AER_LR_ANGXP) THEN $
      temp_image = Angexp_LR_Grid

   ;------------------------------------------------------------------------
   ; Scale to the full resolution of the work window. Don't interpolate.
   ;------------------------------------------------------------------------

   rsize = SIZE(temp_image)
   temp_image = REBIN(temp_image, rsize[1] * 64, rsize[2] * 64, /SAMPLE)

   ;------------------------------------------------------------------------
   ; Create the color image.
   ;------------------------------------------------------------------------

   CreateSwathContinuousImage, temp_image

   temp_image = 0
ENDIF

;---------------------------------------------------------------------------
; Display the images in the work window.
;---------------------------------------------------------------------------

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowAERInWorkWndw

;***************************************************************************
PRO ShowLandInWorkWndw, State
;***************************************************************************
; Display the aerosol data in work window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid

;---------------------------------------------------------------------------
; No need to do this if the image you want is already in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window and aerosol parameters.
   ;------------------------------------------------------------------------

   WorkImage[*,*,*] = 0.0
   data_type = !VAR.DataSwath.DATA_TYPE

   IF (data_type GE !KON.DataProd.TYPE_LND_BHR_BLU AND $
       data_type LE !KON.DataProd.TYPE_LND_BHR_NIR) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_LND_BHR_BLU
      temp_image = REFORM(BHR_Grid[iband,*,*])
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_LND_DHR_BLU AND $
       data_type LE !KON.DataProd.TYPE_LND_DHR_NIR) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_LND_DHR_BLU
      temp_image = REFORM(DHR_Grid[iband,*,*])
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_LND_NDVI) THEN $
      temp_image = REFORM(NDVI_Grid[*,*])

   IF (data_type GE !KON.DataProd.TYPE_LND_RPV1) THEN $
      temp_image = REFORM(RPV_Grid[0,*,*])

   IF (data_type GE !KON.DataProd.TYPE_LND_RPV2) THEN $
      temp_image = REFORM(RPV_Grid[1,*,*])

   IF (data_type GE !KON.DataProd.TYPE_LND_RPV3) THEN $
      temp_image = REFORM(RPV_Grid[2,*,*])

   ;------------------------------------------------------------------------
   ; Scale to the full resolution of the work window. Don't interpolate.
   ;------------------------------------------------------------------------

   rsize = SIZE(temp_image)
   temp_image = REBIN(temp_image, rsize[1] * 4, rsize[2] * 4, /SAMPLE)

   ;------------------------------------------------------------------------
   ; Create the color image.
   ;------------------------------------------------------------------------

   CreateSwathContinuousImage, temp_image

   temp_image = 0
ENDIF

;---------------------------------------------------------------------------
; Display the images in the work window.
;---------------------------------------------------------------------------

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowLandInWorkWndw

;***************************************************************************
PRO ShowStereoInWorkWndw, State, OldOrNew
;***************************************************************************
; Display the loaded stereo heights or winds in work window. These can be
; either from the old or the new stereo product.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File

units = ''
max_val = -!KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; No need to do this if the image you want is already in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window.
   ;------------------------------------------------------------------------

   WorkImage[*,*,*] = 0.0
   bad_value = !KON.Misc.BADVALUE_REAL

   ;------------------------------------------------------------------------
   ; Initialize the old stereo parameters.
   ;------------------------------------------------------------------------

   IF (OldOrNew EQ 1) THEN BEGIN
      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_STER_ZEROHT) THEN BEGIN
         temp_image = STER_ZeroHts & resize_fctr = 4 & units = 'm' & max_val = 25000.0
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_STER_CORRHT) THEN BEGIN
         temp_image = STER_CorrHts & resize_fctr = 4 & units = 'm' & max_val = 25000.0
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_STER_WNDCRS) THEN BEGIN
         temp_image = STER_WndCrossHi > STER_WndCrossLo & resize_fctr = 64 & units = 'm/s'
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_STER_WNDALG) THEN BEGIN
         temp_image = STER_WndAlongHi > STER_WndAlongLo & resize_fctr = 64 & units = 'm/s'
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Initialize the new stereo parameters.
   ;------------------------------------------------------------------------

   IF (OldOrNew EQ 2) THEN BEGIN
      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDZ_ZEROHT) THEN BEGIN
         temp_image = CLDZ_ZeroHts & resize_fctr = 4 & units = 'm' & max_val = 25000.0
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDC_CORRHT) THEN BEGIN
         temp_image = CLDC_CorrHts & resize_fctr = 4 & units = 'm' & max_val = 25000.0
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLD_MOTNHT) THEN BEGIN
         temp_image = CLD_MotionHts & resize_fctr = 64 & units = 'm' & max_val = 25000.0
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDZ_WNDCRS) THEN BEGIN
         temp_image = CLDZ_WndCross & resize_fctr = 4 & units = 'm/s'
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDC_WNDCRS) THEN BEGIN
         temp_image = CLDC_WndCross & resize_fctr = 4 & units = 'm/s'
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLD_WNDCRS) THEN BEGIN
         temp_image = CLD_WndCross & resize_fctr = 64 & units = 'm/s'
      ENDIF

      IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLD_WNDALG) THEN BEGIN
         temp_image = CLD_WndAlong & resize_fctr = 64 & units = 'm/s'
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Scale to the full resolution of the work window. Don't interpolate.
   ;------------------------------------------------------------------------

   rsize = SIZE(temp_image)
   temp_image = REBIN(temp_image, rsize[1] * resize_fctr, $
                      rsize[2] * resize_fctr, /SAMPLE)

   ;------------------------------------------------------------------------
   ; Create the color image.
   ;------------------------------------------------------------------------

   CreateSwathContinuousImage, temp_image

   temp_image = 0
ENDIF

;---------------------------------------------------------------------------
; Display the images in the work window.
;---------------------------------------------------------------------------

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

SDCM_names = ['No Retrieval', 'CloudHC', 'CloudLC', 'NearSurfaceLC', 'NearSurfaceHC']

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowStereoInWorkWndw

;***************************************************************************
PRO ShowAlbInWorkWndw, State
;***************************************************************************
; Display the aerosol data in work window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON alb_data, Local_Albedo, Expans_Albedo, Restrict_Albedo

;---------------------------------------------------------------------------
; No need to do this if the image you want is already in the work window.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.OP_CHANGED) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Initialize the work window and aerosol parameters.
   ;------------------------------------------------------------------------

   WorkImage[*,*,*] = 0.0
   data_type = !VAR.DataSwath.DATA_TYPE

   IF (data_type GE !KON.DataProd.TYPE_ALB_LOC_BLU AND $
       data_type LE !KON.DataProd.TYPE_ALB_LOC_BRD) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_ALB_LOC_BLU
      temp_image = REFORM(Local_Albedo[iband,*,*])
      rescale = 8
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_ALB_RES_BLU AND $
       data_type LE !KON.DataProd.TYPE_ALB_RES_BRD) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_ALB_RES_BLU
      temp_image = REFORM(Expans_Albedo[iband,*,*])
      rescale = 64
   ENDIF

   IF (data_type GE !KON.DataProd.TYPE_ALB_EXP_BLU AND $
       data_type LE !KON.DataProd.TYPE_ALB_EXP_BRD) THEN BEGIN
      iband = data_type - !KON.DataProd.TYPE_ALB_EXP_BLU
      temp_image = REFORM(Restrict_Albedo[iband,*,*])
      rescale = 64
   ENDIF

   ;------------------------------------------------------------------------
   ; Scale to the full resolution of the work window. Don't interpolate.
   ;------------------------------------------------------------------------

   rsize = SIZE(temp_image)
   temp_image = REBIN(temp_image, rsize[1] * rescale, rsize[2] * rescale, $
                      /SAMPLE)

   ;------------------------------------------------------------------------
   ; Create the color image.
   ;------------------------------------------------------------------------

   CreateSwathContinuousImage, temp_image

   temp_image = 0
ENDIF

;---------------------------------------------------------------------------
; Display the images in the work window.
;---------------------------------------------------------------------------

ResetFrame, State, !VAR.WORK_WNDW, 0
IF (!VAR.DataSwath.OP_CHANGED) THEN TV, WorkImage, ORDER=1, TRUE=3
ResetFrame, State, !VAR.WORK_WNDW, 1

!VAR.DataSwath.OP_CHANGED = 0

END  ;  ShowAlbInWorkWndw

;***************************************************************************
PRO TestAndLoadProducts, State, CoordStruct, ProductList, MenuItem, $
                         DataType, CurrentChoice, RtrnVal
;***************************************************************************
; When a user selects a MISR swath product to display, test whether the data
; are already loaded and if not, load them.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

RtrnVal = -1

IF (DataType LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Select the text for the Show Product button.
;---------------------------------------------------------------------------

MenuItem = !VAR.DataProd[DataType].Field

CurrentChoice = (!KON.Misc.MINX_PLATFORM EQ 1) ? $
   ' Press and Hold ' + !KON.Misc.NewLine + 'to Display' + !KON.Misc.NewLine + $
   MenuItem : 'Show ' + MenuItem

;---------------------------------------------------------------------------
; Branch to the correct group of products, based on the index number above,
; and depending on what file they're in.
;---------------------------------------------------------------------------

;------------------------------------------------------------------------
; Handle AGP parameters.
;------------------------------------------------------------------------

IF (DataType EQ !KON.DataProd.TYPE_TERRAIN_HT OR $
    DataType EQ !KON.DataProd.TYPE_LANDH2O_MASK) THEN BEGIN
   IF (~ !VAR.CurrFiles.AGP_Loaded) THEN BEGIN
      LoadAGPData, State, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load AGP data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle biome parameters.
;------------------------------------------------------------------------

IF (DataType EQ !KON.DataProd.TYPE_BIOME_MAP) THEN BEGIN
   IF (~ !VAR.CurrFiles.Biome_Loaded) THEN BEGIN
      LoadBiomeData, State, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load Biome Classification data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle geometric parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_SUN_ZENITH AND $
    DataType LE !KON.DataProd.TYPE_CAM_GLT_DA) THEN BEGIN
   IF (~ !VAR.CurrFiles.GMP_Loaded) THEN BEGIN
      LoadGMPData, State, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load Geometric Parameters data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle aerosol parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_AER_BE_TAU_B AND $
    DataType LE !KON.DataProd.TYPE_AER_FRC_NOS) THEN BEGIN
   IF (~ !VAR.CurrFiles.AE1_Loaded) THEN BEGIN
      GetPgeAerosolData, State, CoordStruct, 0, MisrCoords, AerTau_BE, $
                         AerTau_LR, AerAngexp_BE, AerAngexp_LR, AerSsa, $
                         AerTauFrac, Retval
      IF (Retval NE 0) THEN BEGIN
         mssg = 'Could not load Aerosol data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle land parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_LND_BHR_BLU AND $
    DataType LE !KON.DataProd.TYPE_LND_RPV3) THEN BEGIN
   IF (~ !VAR.CurrFiles.LND_Loaded) THEN BEGIN
      GetPgeLandData, State, CoordStruct, NumPts, MisrCoords, BHRdata, $
                      DHRdata, NDVIdata, RPVdata, LandFile, Retval
      IF (Retval NE 0) THEN BEGIN
         mssg = 'Could not load Land data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle TC_STEREO parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_STER_ZEROHT AND $
    DataType LE !KON.DataProd.TYPE_STER_WNDALG) THEN BEGIN
   IF (~ !VAR.CurrFiles.STR_Loaded) THEN BEGIN
      LoadTCStereoData, State, stereo_file, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load TC_STEREO data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle TC_CLOUD parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_CLDZ_ZEROHT AND $
    DataType LE !KON.DataProd.TYPE_CLD_CLDMSK) THEN BEGIN
   IF (~ !VAR.CurrFiles.CLD_Loaded) THEN BEGIN
      LoadTCCloudData, State, cloud_file, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load TC_CLOUD data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle classifiers parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_CLASS_SMOKE AND $
    DataType LE !KON.DataProd.TYPE_CLASS_CLEAR) THEN BEGIN
   IF (~ !VAR.CurrFiles.SVM_Loaded) THEN BEGIN
      LoadSVMData, State, retval
      IF (retval NE 0) THEN BEGIN
         mssg = 'Could not load SVM data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Handle albedo parameters.
;------------------------------------------------------------------------

IF (DataType GE !KON.DataProd.TYPE_ALB_LOC_BLU AND $
    DataType LE !KON.DataProd.TYPE_ALB_EXP_BRD) THEN BEGIN
   IF (~ !VAR.CurrFiles.ALB_Loaded) THEN BEGIN
      GetPgeAlbedoData, State, CoordStruct, 0, MisrCoords, LocalAlbedo, $
                        RestrAlbedo, ExpansAlbedo, AlbedoFile, Retval

      IF (Retval NE 0) THEN BEGIN
         mssg = 'Could not load Albedo data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Initialize variables.
;---------------------------------------------------------------------------

IF (!VAR.DataProd[DataType].MinDlg EQ  9999. AND $
    !VAR.DataProd[DataType].MaxDlg EQ -9999.) THEN BEGIN
      
   GetSwathMinMaxValues, DataType, MinVal, MaxVal

   IF (MinVal EQ 9999.0 AND MaxVal EQ -9999.0) THEN BEGIN
      mssg = ['There are no valid values in the data product named ', $
              '"' + MenuItem + '". Try something else.']
      rtrn_val = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
      RETURN
   ENDIF
   
   !VAR.DataProd[DataType].MinData = MinVal
   !VAR.DataProd[DataType].MaxData = MaxVal
   !VAR.DataProd[DataType].MinDlg  = MinVal
   !VAR.DataProd[DataType].MaxDlg  = MaxVal
ENDIF

;---------------------------------------------------------------------------
; Destroy the current color key and/or color key dialog window if it exists.
;---------------------------------------------------------------------------

IF (DataType NE !VAR.DataSwath.DATA_TYPE) THEN BEGIN
   !VAR.DataSwath.OP_CHANGED = 1
   DestroyColorKey, 2
ENDIF

!VAR.DataSwath.DATA_TYPE = DataType

RtrnVal = 0

END  ;  TestAndLoadProducts

;***************************************************************************
PRO ShowDataProducts, State
;***************************************************************************
; Here when the user clicks the button on the animation window to display a
; MISR swath data product.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data type and set the scale bar to show in the window if it has
; not already been set.
;---------------------------------------------------------------------------

DataType = !VAR.DataSwath.DATA_TYPE

IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_DO_NOT) THEN $
   !VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_ON_ANIM_WNDW

;---------------------------------------------------------------------------
; Branch to the product to display.
;---------------------------------------------------------------------------

IF (DataType EQ !KON.DataProd.TYPE_BIOME_MAP    OR $
    DataType EQ !KON.DataProd.TYPE_LANDH2O_MASK OR $
    DataType EQ !KON.DataProd.TYPE_STER_SDCM    OR $
    DataType EQ !KON.DataProd.TYPE_CLDZ_CLDMSK  OR $
    DataType EQ !KON.DataProd.TYPE_CLDC_SDCM    OR $
    DataType EQ !KON.DataProd.TYPE_CLD_CLDMSK   OR $
    DataType EQ !KON.DataProd.TYPE_CLASS_SMOKE  OR $
    DataType EQ !KON.DataProd.TYPE_CLASS_DUST   OR $
    DataType EQ !KON.DataProd.TYPE_CLASS_CLOUD  OR $
    DataType EQ !KON.DataProd.TYPE_CLASS_CLEAR) THEN BEGIN
   CreateSwathDiscreteImage, state
   RETURN
ENDIF

IF (DataType EQ !KON.DataProd.TYPE_TERRAIN_HT OR $
    DataType EQ !KON.DataProd.TYPE_LANDH2O_MASK) THEN BEGIN
   IF (!VAR.CurrFiles.AGP_Loaded) THEN ShowAGPInWorkWndw, state
   RETURN
ENDIF

IF (DataType GE !KON.DataProd.TYPE_SUN_ZENITH AND $  ; geometric parameters
    DataType LE !KON.DataProd.TYPE_CAM_GLT_DA) THEN BEGIN
   IF (!VAR.CurrFiles.GMP_Loaded) THEN ShowGMPInWorkWndw, state
   RETURN
ENDIF
   
IF (DataType GE !KON.DataProd.TYPE_AER_BE_TAU_B AND $  ; old aerosol parameters
    DataType LE !KON.DataProd.TYPE_AER_FRC_NOS) THEN BEGIN
   IF (!VAR.CurrFiles.AE1_Loaded) THEN ShowAERInWorkWndw, state
   RETURN
ENDIF

;Sebastian - new aerosol product should go here
;IF (DataType GE !KON.DataProd.TYPE_AER_BE_TAU_B AND $  ; new aerosol parameters
;    DataType LE !KON.DataProd.TYPE_AER_FRC_NOS) THEN BEGIN
;   IF (!VAR.CurrFiles.AE2_Loaded) THEN ShowAERInWorkWndw, state
;   RETURN
;ENDIF

IF (DataType GE !KON.DataProd.TYPE_LND_BHR_BLU AND $  ; land parameters
    DataType LE !KON.DataProd.TYPE_LND_RPV3) THEN BEGIN
   IF (!VAR.CurrFiles.LND_Loaded) THEN ShowLandInWorkWndw, state
   RETURN
ENDIF
   
IF (DataType GE !KON.DataProd.TYPE_STER_ZEROHT AND $  ; old stereo parameters
    DataType LE !KON.DataProd.TYPE_STER_WNDALG) THEN BEGIN
   IF (!VAR.CurrFiles.STR_Loaded) THEN ShowStereoInWorkWndw, state, 1
   RETURN
ENDIF
   
IF (DataType GE !KON.DataProd.TYPE_CLDZ_ZEROHT AND $  ; new stereo parameters
    DataType LE !KON.DataProd.TYPE_CLD_CLDMSK) THEN BEGIN
   IF (!VAR.CurrFiles.CLD_Loaded) THEN ShowStereoInWorkWndw, state, 2
   RETURN
ENDIF

;IF (DataType GE !KON.DataProd.TYPE_CLASS_SMOKE AND $  ; classifiers parameters
;    DataType LE !KON.DataProd.TYPE_CLASS_CLEAR) THEN BEGIN
;   IF (!VAR.CurrFiles.SVM_Loaded) THEN ShowSvmInWorkWndw, state
;   RETURN
;ENDIF

IF (DataType GE !KON.DataProd.TYPE_ALB_LOC_BLU AND $  ; albedo parameters
    DataType LE !KON.DataProd.TYPE_ALB_EXP_BRD) THEN BEGIN
   IF (!VAR.CurrFiles.ALB_Loaded) THEN ShowAlbInWorkWndw, state
   RETURN
ENDIF
   
END  ;  ShowDataProducts
