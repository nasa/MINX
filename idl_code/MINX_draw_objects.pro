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
PRO ResetFrame, State, Curframe, Redraw
;***************************************************************************
; Handle a request to change the camera that is displayed.
;---------------------------------------------------------------------------

   COMPILE_OPT IDL2
   
   State.curframe = Curframe
   SafeWSET, (*State.pwinHdl)[Curframe], didit
   
   WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = Curframe
   WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = $
                   'Camera: ' + !KON.Instr.camera_names[Curframe]

   IF (Redraw NE 0) THEN RedrawWindow, State, Curframe
   
END  ;  ResetFrame

;***************************************************************************
PRO RedrawWindow, State, thisframe
;***************************************************************************
; Redraw the specified window with all requested overlays.
;---------------------------------------------------------------------------
   
   COMPILE_OPT IDL2
   
   SWIN = !D.WINDOW

   SafeWSET, State.draw_win, didit
   
   State.curframe = thisframe
   WIDGET_CONTROL, State.wFramesWhichSlider, SET_VALUE = thisframe
   WIDGET_CONTROL, State.wFramesByCamLabel, SET_VALUE = $
      'Camera: ' + !KON.Instr.camera_names[thisframe]
      
   ;------------------------------------------------------------------------
   ; If the window is valid, redraw different graphics objects depending on
   ; their states.
   ;------------------------------------------------------------------------
   
   pwin = *State.pwinHdl
   DEVICE, COPY = [0, 0, State.sizex, State.sizey, 0, 0, pwin[thisframe]]
   
   IF (pwin[thisframe] GE 0) THEN BEGIN
   
      IF (thisframe GT 0) THEN BEGIN
      
         IF (State.showdots GT 0)    THEN RedrawRefDots, State, 0, 0, $
            State.sizex, State.sizey
         IF (State.showcountry GT 0) THEN RedrawGeography, State, 1
         RedrawGeography, State, 2
         IF (State.showmapclr  GT 0) THEN RedrawMapColors, State, 0, 0, $
            State.sizex, State.sizey, 0
         IF (State.showobjects GT 0) THEN RedrawObjects,   State, 0, 0, $
            State.sizex, State.sizey, 0
         IF (State.showobjects GT 0) THEN RedrawDirArrow,  State, 0, 0, $
            State.sizex, State.sizey, 0
         IF (State.showmarker GT 0)  THEN RedrawMarkerPts, State, 0, 0, $
            State.sizex, State.sizey
         IF (State.showfire   GT 0) THEN RedrawModisFires, State, 0, 0, $
            State.sizex, State.sizey
            
         ;------------------------------------------------------------------
         ; Color key options for digitized region overlay make its redraw
         ; complicated.
         ;------------------------------------------------------------------
         
         IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW AND $
             !VAR.DataRgn.MINMAX_SOURCE EQ !KON.DataRgn.MINMAX_USER) THEN BEGIN
            
            SafeWSET, !VAR.DataRgn.COLORKEY_WNDW_ID, didit
            IF (didit LE 0) THEN BEGIN
               WINDOW, TITLE='', XPOS=700, YPOS=500, /FREE, $
                  XSIZE=!VAR.DataRgn.SIZE_X + 20, YSIZE=!VAR.DataRgn.SIZE_Y
               !VAR.DataRgn.COLORKEY_WNDW_ID = !D.WINDOW
            ENDIF
            
            IF (!VAR.DataRgn.BKGRND_COLOR NE !KON.DataRgn.COLOR_TRANSP) THEN $
               ERASE, !VAR.DataRgn.BKGRND_COLOR
            DrawPlumeColorKey, State, 1
            !VAR.DataRgn.MINMAX_SOURCE = !KON.DataRgn.MINMAX_NONE
         ENDIF
         
         IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
            DrawPlumeColorKey, State, 1
         ENDIF ELSE BEGIN
            !VAR.DataRgn.MINMAX_SOURCE = !KON.DataRgn.MINMAX_NONE
         ENDELSE
      ENDIF
      
      ;---------------------------------------------------------------------
      ; Color key options for swath products overlay - also complicated.
      ;---------------------------------------------------------------------
      
      IF (!VAR.DataSwath.DATA_TYPE NE !KON.DataProd.TYPE_NONE AND $
          !VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_IN_NEW_WNDW AND $
          !VAR.DataSwath.COLORKEY_CONT_ID GE 0) THEN BEGIN
         
         SafeWSET, !VAR.DataSwath.COLORKEY_CONT_ID, didit
         IF (didit LE 0) THEN BEGIN
            WINDOW, TITLE='', XPOS=700, YPOS=500, /FREE, $
                    XSIZE=!VAR.DataSwath.SIZE_X + 20, $
                    YSIZE=!VAR.DataSwath.SIZE_Y
                    !VAR.DataSwath.COLORKEY_CONT_ID = !D.WINDOW
         ENDIF
 
         DrawSwathColorKey, State
      ENDIF
      
      IF (!VAR.DataSwath.DATA_TYPE NE !KON.DataProd.TYPE_NONE AND $
          !VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN $
         DrawSwathColorKey, State

   ENDIF
   
   ;------------------------------------------------------------------------
   ; Reset parameters.
   ;------------------------------------------------------------------------
   
clean_redraw:
   *State.pwinHdl = pwin
   pwin = 0
   EMPTY
   
   SafeWSET, SWIN, didit
   
END  ;  RedrawWindow

;***************************************************************************
PRO DrawLineSegments, DataCoords, NumPts, Xmax, Ymax, Symbol, Color, $
                      ShowLine, ObjName, Iline, Drawtype
;***************************************************************************
; Draw all the line segments contained in the list.  Also draw symbols at
; the points and post the name of the object at the head point.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; Interpret the symbol and color names.
;---------------------------------------------------------------------------

match = STRMATCH(!KON.Colors.SymbolNames, Symbol)
ndx = WHERE(match EQ 1, num_ndx)
sym_type = !KON.Misc.SymbID[0]                          ; default = '+'
IF (num_ndx EQ 1) THEN sym_type = !KON.Misc.SymbID[ndx]

match = STRMATCH(!KON.Colors.ColorNames, Color)
ndx = WHERE(match EQ 1, num_ndx)
coltype = !KON.Colors.ColorVals[0]
IF (num_ndx EQ 1) THEN coltype = !KON.Colors.ColorVals[ndx]

sym_size = 1
sym_thick = 1

;---------------------------------------------------------------------------
; Draw the next point's symbol. Then redraw the first point in the region
; with a filled octogon symbol.
;---------------------------------------------------------------------------

PLOT, DataCoords[0,0:NumPts-1],DataCoords[1,0:NumPts-1], PSYM=sym_type, $
      THICK=sym_thick, COLOR=coltype, SYMSIZE=sym_size, $
      POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], YRANGE=[0,Ymax], $
      /NOERASE, XSTYLE=5, YSTYLE=5

octogon = FINDGEN(9) * (!PI*2/8.)
USERSYM, COS(octogon), SIN(octogon), COLOR=coltype, /FILL
OPLOT, [DataCoords[0,0]],[DataCoords[1,0]], PSYM=8, SYMSIZE=sym_size

;---------------------------------------------------------------------------
; Draw the current line segment (including on direction line).
;---------------------------------------------------------------------------

IF (NumPts GT 1 AND ShowLine NE 0) THEN $
   PLOT, DataCoords[0,0:NumPts-1],DataCoords[1,0:NumPts-1], $
   POSITION=[0,0,Xmax,Ymax], LINESTYLE=2, /NOERASE, $
   SYMSIZE=sym_size, THICK=sym_thick, COLOR=coltype, $
   XRANGE=[0,Xmax], YRANGE=[0,Ymax], XSTYLE=5, YSTYLE=5
   
;---------------------------------------------------------------------------
; Post the object name angled and positioned so it covers fewer points.
;---------------------------------------------------------------------------

old_font = GetFontInfo(0)
SetFontInfo, {Type:!KON.FontTyp.FONT_VECTOR, Size:'10', Face:'bold'}

IF (NumPts GE 4 AND (Iline EQ 0 OR $
    (Iline EQ 1 AND Drawtype EQ !KON.GeomObjTyp.GEOM_LINE_OBJ))) THEN BEGIN

   IF ((DataCoords[0,0] EQ DataCoords[0,NumPts-1] AND $
        DataCoords[1,0] EQ DataCoords[1,NumPts-1]) OR $
        Drawtype EQ !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN

      cntr_x = (DataCoords[0,1] + DataCoords[0,NumPts-2]) / 2.0
      cntr_y = (DataCoords[1,1] + DataCoords[1,NumPts-2]) / 2.0
      del_y = (DataCoords[0,0] EQ cntr_x) ? 0.001 : (DataCoords[0,0]-cntr_x)
      slope  = (DataCoords[1,0] - cntr_y) / del_y
      xpos   = ROUND(10. / SQRT(1. + slope * slope)) * $
               ((cntr_x GT DataCoords[0,0]) ? (-1) : 1)
      ypos   = ROUND(ABS(xpos * slope) * $
               ((cntr_y GT DataCoords[1,0]) ? (-1) : 1))
      xpos  += DataCoords[0,0]
      ypos  += DataCoords[1,0]
      orient = ATAN(slope) * !RADEG + 90.
      IF (orient LT -90.) THEN orient += 180.
      IF (orient GT  90.) THEN orient -= 180.

      charsize = 1

      XYOUTS, xpos, ypos, ObjName, CHARSIZE=charsize, ALIGNMENT=0.5, $
              ORIENTATION=orient, COLOR=coltype
   ENDIF
ENDIF

SetFontInfo, old_font

END  ;  DrawLineSegments

;***************************************************************************
PRO RedrawRefDots, State, Xbeg, Ybeg, Xsize, Ysize
;***************************************************************************
; Redraw stationary marks on the window at uniformly spaced intervals to
; provide reference features for comparing pixel offsets.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

Xmax = Xsize - 1
Ymax = Ysize - 1

;---------------------------------------------------------------------------
; Get the number of reference dots to draw.
;---------------------------------------------------------------------------

num_x = FLOOR(State.sizex / FLOAT(State.dotdist)) + 1
num_y = FLOOR(State.sizey / FLOAT(State.dotdist)) + 1

;---------------------------------------------------------------------------
; Compute a grid where stationary dots will be displayed in the window.
;---------------------------------------------------------------------------

dot_grid_x = INTARR(num_x * num_y)
dot_grid_y = INTARR(num_x * num_y)

ivect = 0
FOR ix = 0, num_x-1 DO BEGIN
   FOR iy = 0, num_y-1 DO BEGIN
      dot_grid_x[ivect] = State.dotdist * ix
      dot_grid_y[ivect] = State.dotdist * iy
      ivect = ivect + 1
   ENDFOR
ENDFOR

goodndxs = WHERE(dot_grid_x GE Xbeg AND dot_grid_x LE (Xbeg+Xsize-1) AND $
                 dot_grid_y GE Ybeg AND dot_grid_y LE (Ybeg+Ysize-1), $
                 numgood)

IF (numgood GT 0) THEN BEGIN
   dot_grid_x = dot_grid_x[goodndxs] - Xbeg
   dot_grid_y = dot_grid_y[goodndxs] - Ybeg
ENDIF

goodndxs = 0

;---------------------------------------------------------------------------
; Display the grid of pluses in the window:
; red = 255, green = 65280, yellow = 65535, white = 16777215
;---------------------------------------------------------------------------

PLOT, dot_grid_x, dot_grid_y, COLOR=65535, PSYM=1, SYMSIZE=1.0, $
      POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], YRANGE=[0,Ymax], $
      /NOERASE, XSTYLE=5, YSTYLE=5

END  ;  RedrawRefDots

;***************************************************************************
PRO RedrawModisFires, State, Xbeg, Ybeg, Xsize, Ysize
;***************************************************************************
; Redraw the MODIS fire pixel objects read in. Follow linked list of object
; structures and plot each.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON coord_data, CoordStruct
COMMON data_structs, region_data, linept_data

whichorbit = (State.curframe GT 9)

Xmax = Xsize - 1
Ymax = Ysize - 1

;---------------------------------------------------------------------------
; Loop over all the objects.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN RETURN

pNextObj = !VAR.LinkList.pHeadObj

WHILE (PTR_VALID(pNextObj)) DO BEGIN

   ;------------------------------------------------------------------------
   ; Get the next object and use it only if it is a fire pixel.
   ;------------------------------------------------------------------------

   IF ((*pNextObj).ObjType[0] NE !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Get the color, symbol and first point and interpret the symbol and
   ; color names.
   ;------------------------------------------------------------------------

   colorx  = (*((*pNextObj).pData)).color_lin
   symbolx = (*((*pNextObj).pData)).symbol

   pNextPt = (*pNextObj).pNextLinePt

   match = STRMATCH(!KON.Colors.SymbolNames, symbolx)
   ndx = WHERE(match EQ 1, num_ndx)
   symtype = !KON.Misc.SymbID[0]
   IF (num_ndx EQ 1) THEN symtype = !KON.Misc.SymbID[ndx]

   match = STRMATCH(!KON.Colors.ColorNames, colorx)
   ndx = WHERE(match EQ 1, num_ndx)
   coltype = !KON.Colors.ColorVals[0]
   IF (num_ndx EQ 1) THEN coltype = !KON.Colors.ColorVals[ndx]

   ndx = 0

   ;------------------------------------------------------------------------
   ; Count the number of points in the line object.
   ;------------------------------------------------------------------------

   num_pts = LListCount(pNextPt)
   IF (num_pts LT 1) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Get all SOM coordinates for this object and convert them to screen
   ; coordinates. Do this if the data were read in from a file in which case
   ; device coords are not available. Always convert lat-longs to SOM when
   ; data are read in.
   ;------------------------------------------------------------------------

   misr_crds  = INTARR(3,num_pts)

   icnt = 0
   WHILE (PTR_VALID(pNextPt) AND icnt LT num_pts) DO BEGIN
      PtData = *((*pNextPt).pData)
      misr_crds[2,icnt] = PtData.block
      misr_crds[1,icnt] = PtData.along275
      misr_crds[0,icnt] = PtData.cross275
      icnt += 1
      pNextPt = (*pNextPt).pNextSib
   ENDWHILE

   IF (icnt GT 0) THEN BEGIN
      MisrCrdToWndwCrd, State.curframe, misr_crds, data_coord, 1, retval
      data_coord[0,*] -= Xbeg
      data_coord[1,*] -= Ybeg

      PLOT, data_coord[0,*], data_coord[1,*], COLOR=coltype, PSYM=symtype, $
            SYMSIZE=0.2, POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], $
            YRANGE=[0,Ymax], /NOERASE, XSTYLE=5, YSTYLE=5
   ENDIF

   misr_crds = 0
   data_coord = 0

   ;------------------------------------------------------------------------
   ; Get the pointer to the next object.
   ;------------------------------------------------------------------------

   pNextObj = (*pNextObj).pNextSib

ENDWHILE

END  ;  RedrawModisFires

;***************************************************************************
PRO RedrawModisFiresInOps, State
;***************************************************************************
; If the fire pixels were just loaded or the fire pixels button was just
; clicked on, redraw the fire pixels in the operations window (0).
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

save_wndw = !D.WINDOW
SafeWSET, (*State.pwinHdl)[!VAR.WORK_WNDW], didit

IF (CoordStruct.(0).num_band GE 3) THEN BEGIN
   workImage = TVRD(ORDER=1, TRUE=3)
   workImage[*,*,*] = 0
   TV, workImage, ORDER=1, TRUE=3
ENDIF ELSE BEGIN
   workImage = TVRD(ORDER=1, TRUE=0)
   workImage[*,*] = 0
   TV, workImage, ORDER=1, TRUE=0
ENDELSE

RedrawModisFires, State, 0, 0, State.sizex, State.sizey

SafeWSET, save_wndw, didit
WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0

ResetSwathData

END  ;  RedrawModisFiresInOps

;***************************************************************************
PRO RedrawMarkerPts, State, Xbeg, Ybeg, Xsize, Ysize
;***************************************************************************
; Redraw the Marker objects read in.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

Xmax = Xsize - 1
Ymax = Ysize - 1

;---------------------------------------------------------------------------
; Loop over all the marker objects.
;---------------------------------------------------------------------------

IF (State.showmarker EQ 0 OR !MRK.num_objects LE 0) THEN RETURN

old_font = GetFontInfo(0)
SetFontInfo, {Type:!KON.FontTyp.FONT_VECTOR, Size:'10', Face:'bold'}

FOR iobj=0,!MRK.num_objects-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Get the next marker object and determine how many points it has.
   ;------------------------------------------------------------------------

   num_pts = !MRK.num_obj_pts[iobj]

   ;------------------------------------------------------------------------
   ; Get color, symbol and 1st point and interpret symbol and color names.
   ;------------------------------------------------------------------------

   colorx  = !MRK.object_color[iobj]
   symbolx = !MRK.object_shape[iobj]

   match = STRMATCH(!KON.Colors.SymbolNames, symbolx)
   ndx = WHERE(match EQ 1, num_ndx)
   symtype = !KON.Misc.SymbID[0]
   IF (num_ndx EQ 1) THEN symtype = !KON.Misc.SymbID[ndx]

   match = STRMATCH(!KON.Colors.ColorNames, colorx)
   ndx = WHERE(match EQ 1, num_ndx)
   coltype = !KON.Colors.ColorVals[0]
   IF (num_ndx EQ 1) THEN coltype = !KON.Colors.ColorVals[ndx]

   ndx = 0

   ;------------------------------------------------------------------------
   ; Get all the SOM coordinates for this object and convert them to screen
   ; coordinates. Do this in case the data were read in from a file in which
   ; case device coords are not available. Always convert lat-longs to SOM
   ; when data are read in.
   ;------------------------------------------------------------------------

   misr_crds = INTARR(3,num_pts)

   misr_crds[0,*] = !MRK.obj_pt_275_samp[iobj,0:num_pts-1]
   misr_crds[1,*] = !MRK.obj_pt_275_line[iobj,0:num_pts-1]
   misr_crds[2,*] = !MRK.obj_pt_block[iobj,0:num_pts-1]

   IF (num_pts GT 0) THEN BEGIN

      MisrCrdToWndwCrd, State.Curframe, misr_crds, data_coord, 1, retval
      
      data_coord[0,*] -= Xbeg
      data_coord[1,*] -= Ybeg
  
      IF (num_pts GT 1) THEN symtype *= -1

      PLOT, data_coord[0,*], data_coord[1,*], COLOR=coltype, PSYM=symtype, $
            SYMSIZE=0.8, POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], $
            YRANGE=[0,Ymax], /NOERASE, XSTYLE=5, YSTYLE=5

      IF (!MRK.object_name[iobj] NE '') THEN BEGIN
         XYOUTS, data_coord[0,0], data_coord[1,0]+7, $
                 !MRK.object_name[iobj], CHARSIZE=1.0, $
                 ALIGNMENT=0.5, COLOR=coltype[0]
      ENDIF
   ENDIF

   misr_crds = 0
   data_coord = 0

ENDFOR

SetFontInfo, old_font

END  ;  RedrawMarkerPts

;***************************************************************************
PRO ScaleColorbarEventHandler, Event
;***************************************************************************
; Process the kill event if the user destroys it with window system button.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
   DestroyColorKey, 2
      
END  ;  ScaleColorbarEventHandler

;***************************************************************************
PRO DrawImageScaleBar, State
;***************************************************************************
; Draw a scale bar on the MISR image.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON scale_bar, ScaleBarBase, ScaleBarObj

;------------------------------------------------------------------------
; Initialize parameters.
;------------------------------------------------------------------------

bar_length_km  = 100.0
tick_delta_km = 10.0
bar_length_pix = (bar_length_km / !KON.Instr.HI_RES_PIX_SIZE)
bar_height_pix = 25
tick_delta_pix = 10.0 / !KON.Instr.HI_RES_PIX_SIZE
num_patches = ROUND(bar_length_km / tick_delta_km)

window_xsize = bar_length_pix * 1.111111
window_ysize = bar_height_pix * 2.0

xbeg_frac = (window_xsize - bar_length_pix) / window_xsize / 2.0
xend_frac = 1.0 - xbeg_frac
ybeg_frac = 0.005
yend_frac = bar_height_pix / window_ysize

;------------------------------------------------------------------------
; Create ticknames.
;------------------------------------------------------------------------
          
ticknames = STRING(num_patches*INDGEN(num_patches+1), FORMAT='(I0)')
         
;------------------------------------------------------------------------
; Create color values: alternating light gray - dark gray.
;------------------------------------------------------------------------

cb_rgb = BYTARR(3,num_patches)
FOR itick=0,num_patches-1,2 DO cb_rgb[0,itick] = 140
FOR itick=1,num_patches,2 DO cb_rgb[0,itick] = 220
cb_rgb[1,*] = cb_rgb[0,*]
cb_rgb[2,*] = cb_rgb[0,*]

;------------------------------------------------------------------------
; Draw colorbar in a separate window with vertical orientation and labels
; on the right-hand side.
;------------------------------------------------------------------------

ScaleBarBase = $
   WIDGET_BASE(TITLE='Kilometers', GROUP_LEADER=State.wAnimateBase, $
               XOFFSET=700, YOFFSET=600, /TLB_KILL_REQUEST_EVENTS, $
               /FLOATING, EVENT_PRO='ScaleColorbarEventHandler')
wDraw = WIDGET_WINDOW(ScaleBarBase, XSIZE=window_xsize, $
               YSIZE=window_ysize, SENSITIVE=0, FRAME=2, $
               MOUSE_MOTION_HANDLER='ExWidWinMouseNULLHandler', $
               MOUSE_DOWN_HANDLER='ExWidWinMouseNULLHandler', $
               MOUSE_UP_HANDLER='ExWidWinMouseNULLHandler', $
               MOUSE_WHEEL_HANDLER='ExWidWinMouseNULLHandler')
WIDGET_CONTROL, ScaleBarBase, /REALIZE

ScaleBarObj = COLORBAR(/NORMAL, POSITION=[xbeg_frac,ybeg_frac,xend_frac,yend_frac], $
              RGB_TABLE=cb_rgb, TICKNAME=ticknames, TAPER=0, ORIENTATION=0, $
              BORDER=1, TICKLAYOUT=1, TEXTPOS=1, FONT_SIZE=12, MAJOR=0, MINOR=0, $
              SUBTICKLEN=0.0)
cb_rgb = 0

END  ;  DrawImageScaleBar

;***************************************************************************
PRO RedrawGeography, State, Mode, LeftEdge, BottomEdge, NumAcross, NumAlong
;***************************************************************************
; Redraw the country boundaries OR a lat/lon grid.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON coord_data, CoordStruct
COMMON scale_bar, ScaleBarBase, ScaleBarObj

;---------------------------------------------------------------------------
; If the LLgrid 'show' flag has been turned off off and the scale bar window
; exists, then kill that window.
;---------------------------------------------------------------------------

IF (State.showLLgrid EQ 0) THEN BEGIN
   IF (OBJ_VALID(ScaleBarObj)) THEN ScaleBarObj.Delete

   IF (ISA(ScaleBarBase)) THEN BEGIN
      IF (ScaleBarBase GT 0) THEN BEGIN
         IF WIDGET_INFO(ScaleBarBase, /VALID_ID) THEN $
            WIDGET_CONTROL, ScaleBarBase, /DESTROY
      ENDIF
   ENDIF
   ScaleBarBase = -1
   ScaleBarObj = OBJ_NEW()
ENDIF

;---------------------------------------------------------------------------
; Exit if the 'show' flag is not on.
;---------------------------------------------------------------------------

IF (State.showcountry EQ 0 AND State.showLLgrid EQ 0) THEN RETURN

whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set the coordinates differently if saving to file, because a subset of the
; entire image may have been selected.
;---------------------------------------------------------------------------

IF (N_PARAMS() EQ 6) THEN BEGIN
  X_upperLeft  = LeftEdge
  X_LowerRight = LeftEdge + NumAcross - 1
  Y_upperLeft  = BottomEdge + NumAlong - 1
  Y_LowerRight = BottomEdge
ENDIF ELSE BEGIN
  X_upperLeft  = CoordStruct.(whichorbit).ULCwndwX
  X_LowerRight = CoordStruct.(whichorbit).LRCwndwX
  Y_upperLeft  = CoordStruct.(whichorbit).ULCwndwY
  Y_LowerRight = CoordStruct.(whichorbit).LRCwndwY
ENDELSE

;---------------------------------------------------------------------------
; Construct the lat/lon coordinates of the centers of the 4 edges of the
; image plus the center and a point just below the center.
;---------------------------------------------------------------------------

SampCntr = (X_upperLeft + X_LowerRight) / 2
LineCntr = (Y_upperLeft + Y_LowerRight) / 2

crd_left = [X_upperLeft, LineCntr]
crd_top  = [SampCntr, Y_upperLeft-1]
crd_rght = [X_LowerRight-1, LineCntr]
crd_bot  = [SampCntr, Y_LowerRight]
crd_cntr = [SampCntr, LineCntr]
crd_cntx = [SampCntr, LineCntr-50]

WndwCrd = [[crd_left], [crd_top], [crd_rght], [crd_bot], [crd_cntr], [crd_cntx]]

WndwCrdToMisrCrd,  State.Curframe, WndwCrd, MisrCrd, Retval
MisrCrdToSomCrd,   State.Curframe, MisrCrd, SomCrd,  Retval
SomCrdToLonlatCrd, State.Curframe, SomCrd,  LLcrd,   Retval

WndwCrd  = 0
MisrCrd  = 0
crd_left = 0
crd_top  = 0
crd_rght = 0
crd_bot  = 0
crd_cntr = 0
crd_cntx = 0

border_crds = [LLcrd[1,0], LLcrd[0,0], LLcrd[1,1], LLcrd[0,1], $
               LLcrd[1,2], LLcrd[0,2], LLcrd[1,3], LLcrd[0,3]]

lat_center = LLcrd[1,4]
lon_center = LLcrd[0,4]

;---------------------------------------------------------------------------
; Compute the swath angle relative to north.
;---------------------------------------------------------------------------

GetSOMandTRUEnorth, SomCrd[*,4], SomCrd[*,5], LLcrd[*,4], DirCWfromSomN, $
                    DirCWfromTruN
SomCrd = 0
LLcrd  = 0

;---------------------------------------------------------------------------
; Set map projection, scale etc. 
;---------------------------------------------------------------------------

MAP_SET, lat_center, lon_center, -DirCWfromTruN, /CYLINDRICAL, /ISOTROPIC, $
         LIMIT=border_crds, /NOBORDER, XMARGIN=[0,0], YMARGIN=[0,0], /NOERASE

border_crds = 0

IF (Mode EQ 1) THEN BEGIN
   MAP_CONTINENTS, /COASTS, /HIRES, COLOR=!KON.Colors.blue, MLINETHICK=1.5
   MAP_CONTINENTS, /RIVERS, /HIRES, COLOR=!KON.Colors.blue1, MLINETHICK=1.5
   MAP_CONTINENTS, /USA, /HIRES, COLOR=!KON.Colors.brown, MLINETHICK=1.5
   MAP_CONTINENTS, /COUNTRIES, /HIRES, COLOR=!KON.Colors.brown, MLINETHICK=2
ENDIF

IF (State.showLLgrid GT 0 AND Mode EQ 2) THEN BEGIN
   MAP_GRID, /LABEL, LATLAB=ROUND(lon_center), LONLAB=ROUND(lat_center), $
             LATDEL=0.5, LONDEL=0.5, COLOR=!KON.Colors.red, GLINETHICK=1.5, $
             LATALIGN=0.5, LONALIGN=0.5, CHARSIZE=1.5

   IF (~OBJ_VALID(ScaleBarObj)) THEN DrawImageScaleBar, State
ENDIF

END  ;  RedrawGeography

;***************************************************************************
PRO RedrawObjects, State, Xbeg, Ybeg, Xsize, Ysize, pRgn
;***************************************************************************
; Redraw the objects read in or drawn with mouse. These may be points, lines
; (connected points) or polygons (closed groups of lines). Follow the linked
; list of object structures and plot each.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

Xmax = Xsize - 1
Ymax = Ysize - 1

;---------------------------------------------------------------------------
; Check that there is a valid head node.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN RETURN

pNextObj = !VAR.LinkList.pHeadObj

;---------------------------------------------------------------------------
; Loop over the objects to be plotted.
;---------------------------------------------------------------------------

WHILE (PTR_VALID(pNextObj)) DO BEGIN

   ;-----------------------------------------------------------------------
   ; If capturing image for writing to file or flashing region before it's
   ; deleted, only display current region. 
   ;------------------------------------------------------------------------

   IF (PTR_VALID(pRgn)) THEN BEGIN
      IF (pRgn NE pNextObj) THEN BEGIN
         pNextObj = (*pNextObj).pNextSib
         CONTINUE
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Get the next object - retrieve the number of points, the symbol and
   ; object name.  Put the polygon and line head pointers in an array.
   ; If both red and blue bands were retrieved, just draw the digitized data
   ; for the red band. The band is needed only to save the region name.
   ;------------------------------------------------------------------------

   obj_type = (*pNextObj).ObjType
   IF (obj_type[0] EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   colors = [(*((*pNextObj).pData)).color_pol, (*((*pNextObj).pData)).color_lin]
   symbol =  (*((*pNextObj).pData)).symbol

   IF ((*((*pNextObj).pData)).band_type EQ !KON.BandObjTyp.RED_BAND OR $
       (*((*pNextObj).pData)).band_type EQ !KON.BandObjTyp.RB_BAND) THEN $
      show_band = 0
   IF ((*((*pNextObj).pData)).band_type EQ !KON.BandObjTyp.BLUE_BAND) THEN $
      show_band = 1
   IF ((*((*pNextObj).pData)).band_type EQ !KON.BandObjTyp.BOTH_BAND) THEN $
      show_band = (!VAR.DataRgn.SHOW_BAND_TYPE EQ 0 OR $
                   !VAR.DataRgn.SHOW_BAND_TYPE EQ 2) ? 0 : 1

   obj_names =  (*((*pNextObj).pData)).name
   obj_name =  obj_names[show_band]

   pNextPts = [(*pNextObj).pNextPolyPt, (*pNextObj).pNextLinePt]

   ;---------------------------------------------------------------------
   ; Loop over the 2 potential lines in the object (polygon and line).
   ; (To force the drawing of points internal to a region for all region
   ; types, set npoly = 1)
   ;---------------------------------------------------------------------

   npoly = 0

   IF (obj_type[1] EQ !KON.GeomObjTyp.GEOM_LINE_OBJ OR $
       !SAV.Digitize.DIG_STATE[0] EQ !KON.AerObjTyp.DIR_LINE_OBJ) THEN $
         npoly = 1

   FOR iline=0,npoly DO BEGIN

      pNextPt = pNextPts[iline]
      color = colors[iline]

      show_line = 1
      IF (iline EQ 1) THEN show_line = (*((*pNextObj).pData)).show_line
      IF (!SAV.Digitize.DIG_STATE[0] EQ !KON.AerObjTyp.DIR_LINE_OBJ) THEN $
         show_line = 1

      ;---------------------------------------------------------------------
      ; Count the number of points in the polygon or line object.
      ;---------------------------------------------------------------------

      num_pts = LListCount(pNextPt)
      IF (num_pts LT 1) THEN CONTINUE

      ;---------------------------------------------------------------------
      ; Get all the SOM coordinates for this object and convert to screen
      ; coordinates.  Do this in case the data were read in from a file in
      ; which case device coords are not available. Always convert lat-longs
      ; to SOM when data are read in.
      ;---------------------------------------------------------------------

      misr_crds = INTARR(3,num_pts)

      itotcnt = 0
      icnt = 0
      WHILE (PTR_VALID(pNextPt) AND itotcnt LT num_pts) DO BEGIN
         PtData = *((*pNextPt).pData)
         ht_data = 0.0
         IF (iline EQ 1) THEN BEGIN
            ht_data = PtData.zero_wind_ht[*]
            IF ((*(*pNextObj).pData).type GE $
                 !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN $
               ht_data = PtData.corr_wind_ht[*]
         ENDIF
         IF (iline EQ 0 OR MAX(ht_data) GT 0.0 OR $
             !SAV.Digitize.DIG_STATE[0] EQ !KON.AerObjTyp.DIR_LINE_OBJ OR $
             obj_type[1] EQ !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN
            misr_crds[2,icnt] = PtData.block
            misr_crds[1,icnt] = PtData.along275
            misr_crds[0,icnt] = PtData.cross275
            icnt += 1
         ENDIF
         itotcnt += 1
         pNextPt = (*pNextPt).pNextSib
      ENDWHILE

      IF (icnt GT 0) THEN BEGIN
         MisrCrdToWndwCrd, State.Curframe, misr_crds, data_coord, 1, retval
         data_coord[0,*] -= Xbeg
         data_coord[1,*] -= Ybeg

         goodpts = WHERE(data_coord GT 0, numgood)
         IF (numgood GE 2) THEN $
            DrawLineSegments, data_coord[0:1,goodpts], numgood/2, $
                              Xmax, Ymax, symbol, color, show_line, $
                              obj_name, iline, obj_type[1]
      ENDIF

      goodpts = 0
      misr_crds = 0
      data_coord = 0

   ENDFOR

   ;------------------------------------------------------------------------
   ; Get the pointer to the next object.
   ;------------------------------------------------------------------------

   pNextObj = (*pNextObj).pNextSib

ENDWHILE

END  ;  RedrawObjects

;***************************************************************************
PRO RedrawMapColors, State, Xbeg, Ybeg, Xsize, Ysize, pRgn
;***************************************************************************
; Overlay colored pixels in digitized regions according to data values for
; the type of data requested by user.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

Xmax = Xsize - 1
Ymax = Ysize - 1
array_elem = 0

;---------------------------------------------------------------------------
; Check that there is a valid head node.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN RETURN

;---------------------------------------------------------------------------
; Loop over the bands to be plotted. Only red, only blue or both.
;---------------------------------------------------------------------------

Band1 = (!VAR.DataRgn.SHOW_BAND_TYPE EQ 0 OR $
         !VAR.DataRgn.SHOW_BAND_TYPE EQ 2) ? 0 : 1
Band2 = (!VAR.DataRgn.SHOW_BAND_TYPE EQ 1 OR $
         !VAR.DataRgn.SHOW_BAND_TYPE EQ 2) ? 1 : 0

FOR iBand=Band1,Band2 DO BEGIN

   pNextObj = !VAR.LinkList.pHeadObj

   ;------------------------------------------------------------------------
   ; Loop over the objects to be plotted.
   ;------------------------------------------------------------------------

   WHILE (PTR_VALID(pNextObj)) DO BEGIN

      ;---------------------------------------------------------------------
      ; If capturing image for writing to file, only display current region. 
      ;---------------------------------------------------------------------

      IF (PTR_VALID(pRgn)) THEN BEGIN
         IF (pRgn NE pNextObj) THEN BEGIN
            pNextObj = (*pNextObj).pNextSib
            CONTINUE
         ENDIF
      ENDIF

      ;---------------------------------------------------------------------
      ; Get the next object.
      ;---------------------------------------------------------------------

      obj_type = (*pNextObj).ObjType
      IF (obj_type[0] EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
         pNextObj = (*pNextObj).pNextSib
         CONTINUE
      ENDIF

      pNextPts = [(*pNextObj).pNextPolyPt, (*pNextObj).pNextLinePt]
      samp_spac = (*((*pNextObj).pData)).samp_spac

      ;---------------------------------------------------------------------
      ; Get the data and redraw the data type's values as colored pixels.
      ;---------------------------------------------------------------------

      IF (!VAR.DataRgn.DATA_TYPE EQ !KON.DataRgn.TYPE_DISP_CROSS OR $
          !VAR.DataRgn.DATA_TYPE EQ !KON.DataRgn.TYPE_DISP_ALONG) THEN $
         array_elem = (!VAR.DataRgn.CAM_SEL LE 3) ? !VAR.DataRgn.CAM_SEL : $
                                                    !VAR.DataRgn.CAM_SEL + 1

      IF (!VAR.DataRgn.DATA_TYPE EQ !KON.DataRgn.TYPE_WIND_TOTAL) THEN BEGIN
         LListGetRegionData, !KON.DataRgn.TYPE_WIND_CROSS, array_elem, iBand, $
                             (*pNextObj).pNextPolyPt, CountP, ImagePolyCoords, $
                             (*pNextObj).pNextLinePt, CountL, ImageLineCoords, $
                             DataVals1
         LListGetRegionData, !KON.DataRgn.TYPE_WIND_ALONG, array_elem, iBand, $
                             (*pNextObj).pNextPolyPt, CountP, ImagePolyCoords, $
                             (*pNextObj).pNextLinePt, CountL, ImageLineCoords, $
                             DataVals2
         DataVals = DataVals1 * 0.0 + !KON.Misc.BADVALUE_REAL
         ndxs = WHERE(DataVals1 GE $
                   !VAR.DataRgn.VALUE_MIN_DIG[!KON.DataRgn.TYPE_WIND_CROSS] AND $
                   DataVals2 GE $
                   !VAR.DataRgn.VALUE_MIN_DIG[!KON.DataRgn.TYPE_WIND_ALONG], CountL)
         IF (CountL GT 0) THEN $
            DataVals = SQRT(DataVals1 * DataVals1 + DataVals2 * DataVals2)
         DataVals1 = 0
         DataVals2 = 0
      ENDIF ELSE BEGIN
         LListGetRegionData, !VAR.DataRgn.DATA_TYPE, array_elem, iBand, $
                          (*pNextObj).pNextPolyPt, CountP, ImagePolyCoords, $
                          (*pNextObj).pNextLinePt, CountL, ImageLineCoords, $
                           DataVals
      ENDELSE

      IF (CountL GT 4) THEN BEGIN
         ndxs = WHERE(DataVals NE 0.0, numndxs)
         IF (numndxs GT 0) THEN $
            DisplayRetrievedData, ImagePolyCoords, ImageLineCoords, DataVals, $
                                  !KON.Colors.ColorVals[0], samp_spac, $
                                  obj_type[2], Xbeg, Ybeg, Xsize, Ysize, 0, $
                                  BestMedian, BestMaximum, Retval
      ENDIF

      ndxs = 0
      ImagePolyCoords = 0
      ImageLineCoords = 0
      DataVals = 0

      ;------------------------------------------------------------------------
      ; Get the pointer to the next object.
      ;------------------------------------------------------------------------

      pNextObj = (*pNextObj).pNextSib

   ENDWHILE

ENDFOR

END  ;  RedrawMapColors

;***************************************************************************
PRO DrawArrowHead, EndPt, PrevPt, Xmax, Ymax, ColorVal
;***************************************************************************
; Draw an arrowhead at the end of the direction arrow.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

HeadLen  = 10
BarbDist = 6

;---------------------------------------------------------------------------
; Find a point on the line or its extension that is HeadLen pixels back from
; the end point toward the previous point.
;---------------------------------------------------------------------------

dx0 = FLOAT(EndPt[0] - PrevPt[0])
dy0 = FLOAT(EndPt[1] - PrevPt[1])

line_len = SQRT(dx0 * dx0 + dy0 * dy0)

dx1 = dx0 * (1 - HeadLen / line_len)
dy1 = dy0 * (1 - HeadLen / line_len)

HeadPt = [PrevPt[0] + dx1, PrevPt[1] + dy1]

;---------------------------------------------------------------------------
; Find equation of the line, then construct a normal line through that point.
;---------------------------------------------------------------------------

sign = ((dx0 LT 0.0 AND dy0 GE 0.0) OR $
        (dx0 GE 0.0 AND dy0 LT 0.0)) ? -1.0 : 1.0

IF (ABS(dx0) GT 0.001) THEN BEGIN
   slope = dy0 / dx0
ENDIF ELSE BEGIN
   slope = 1000.0 * sign
ENDELSE

IF (ABS(slope) GT 0.001) THEN BEGIN
   slope = 1.0 / slope * (-1.0)
ENDIF ELSE BEGIN
   slope = 1000.0 * sign * (-1.0)
ENDELSE

intercept = HeadPt[1] - slope * HeadPt[0]

;---------------------------------------------------------------------------
; On the normal line, project an equal distance in either direction to find
; the tips of the arrowhead barbs.
;---------------------------------------------------------------------------

dxb = SQRT(BarbDist * BarbDist / (1.0 + slope * slope))
dyb = BarbDist * slope

barbx = HeadPt[0] + dxb
BarbPt1 = ROUND([barbx, barbx * slope + intercept])

barbx = HeadPt[0] - dxb
BarbPt2 = ROUND([barbx, barbx * slope + intercept])

;---------------------------------------------------------------------------
; Draw the arrowhead.
;---------------------------------------------------------------------------

PLOT, [EndPt[0], BarbPt1[0]], [EndPt[1], BarbPt1[1]], COLOR=ColorVal, $
      LINESTYLE=0, POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], $
      YRANGE=[0,Ymax], /NOERASE, XSTYLE=5, YSTYLE=5

PLOT, [EndPt[0], BarbPt2[0]], [EndPt[1], BarbPt2[1]], COLOR=ColorVal, $
      LINESTYLE=0, POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], $
      YRANGE=[0,Ymax], /NOERASE, XSTYLE=5, YSTYLE=5

END  ;  DrawArrowHead

;***************************************************************************
PRO RedrawDirArrow, State, Xbeg, Ybeg, Xsize, Ysize, pRgn
;***************************************************************************
; Draw the direction arrow as an overlay if there is one.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

ColorVal = 65535

Xmax = Xsize - 1
Ymax = Ysize - 1

;---------------------------------------------------------------------------
; Loop over the objects to be plotted.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN RETURN

pNextObj = !VAR.LinkList.pHeadObj

WHILE (PTR_VALID(pNextObj)) DO BEGIN

   ;------------------------------------------------------------------------
   ; If capturing an image for writing to file, only display current region. 
   ;------------------------------------------------------------------------

   IF (PTR_VALID(pRgn)) THEN BEGIN
      IF (pRgn NE pNextObj) THEN BEGIN
         pNextObj = (*pNextObj).pNextSib
         CONTINUE
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Get the next object.
   ;------------------------------------------------------------------------

   IF (~ PTR_VALID((*((*pNextObj).pData)).direc_pts_spln)) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   misr_crds = *((*((*pNextObj).pData)).direc_pts_spln)
   num_pts = (SIZE(misr_crds))[2]

   IF (num_pts LT 2) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Convert the MISR coordinates into window coordinates.
   ;------------------------------------------------------------------------

   MisrCrdToWndwCrd, State.Curframe, misr_crds, data_coords, 1, retval
   data_coords[0,*] -= Xbeg
   data_coords[1,*] -= Ybeg

   ;------------------------------------------------------------------------
   ; Draw the center line.
   ;------------------------------------------------------------------------

   PLOT, data_coords[0,*],data_coords[1,*], COLOR=ColorVal, $
         LINESTYLE=0, POSITION=[0,0,Xmax,Ymax], XRANGE=[0,Xmax], $
         YRANGE=[0,Ymax], /NOERASE, XSTYLE=5, YSTYLE=5

   ;------------------------------------------------------------------------
   ; Draw the arrowhead.
   ;------------------------------------------------------------------------

   pts_back = (num_pts GT 50) ? 5 : ((num_pts GT 10) ? 3 : 2)
   DrawArrowHead, data_coords[*,num_pts-1], data_coords[*,num_pts-pts_back], $
                  Xmax, Ymax, ColorVal

   ;------------------------------------------------------------------------
   ; Get the pointer to the next object.
   ;------------------------------------------------------------------------

   pNextObj = (*pNextObj).pNextSib

ENDWHILE

data_coords = 0
misr_crds = 0

END  ;  RedrawDirArrow

;***************************************************************************
PRO GetUniqueName, State, ObjType, GeomType, WindType, BandType, Block, $
                   UniqueName
;***************************************************************************
; Create a unique name for the new node. Scan all existing nodes to find the
; next available free name. If the new node is for a double-band retrieval,
; both red and blue band names must be free. Since they are always paired,
; just test the red-band names in double-band retrievals.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON coord_data, CoordStruct

UniqueName = ''

whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set orbit and block strings. The block number is where the first point of
; the digitized object is located.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
WHILE (STRLEN(orbit_str) LT 6) DO orbit_str = '0' + orbit_str

blockbeg  = STRTRIM(STRING(Block),2)
WHILE (STRLEN(blockbeg) LT 3) DO blockbeg = '0' + blockbeg

;---------------------------------------------------------------------------
; Set the characters that identify the type of object this is. Concatenate
; the 4 codes.
;---------------------------------------------------------------------------

obj_type_char = !KON.AerObjTyp.Abbrv[ObjType]
IF (obj_type_char EQ '') THEN RETURN

geom_type_char = !KON.GeomObjTyp.Abbrv[GeomType]
IF (geom_type_char EQ '') THEN RETURN

wind_type_char = !KON.WindObjTyp.Abbrv[WindType]
IF (wind_type_char EQ '') THEN RETURN

band_type_char = !KON.BandObjTyp.Abbrv[BandType]
IF (band_type_char EQ '') THEN RETURN

OGWBcode = obj_type_char + geom_type_char + wind_type_char

;---------------------------------------------------------------------------
; Get a unique number for this object within the block its first point falls
; in and within its type class (exclude the band type). LListGetObjNames
; returns all names satisfying these criteria.
;---------------------------------------------------------------------------

obj_names = STRARR(999)                   ; these should be large enough
obj_num   = INTARR(999) - FIX(!KON.Misc.BADVALUE_REAL)

LListGetObjNames, OGWBcode, blockbeg, count, obj_names

OGWBcode += band_type_char

;---------------------------------------------------------------------------
; Loop over all the names. 
;---------------------------------------------------------------------------

inext = 0

FOR idnum=0,count-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Break this name into its parts separated by the dash character.
   ;------------------------------------------------------------------------

   toks = STRSPLIT(obj_names[idnum], '-', /EXTRACT, COUNT=numtok)
   IF (numtok NE 3) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Get the block component.
   ;------------------------------------------------------------------------

   blk = toks[1]
   nlen = STRLEN(blk)
   FOR ichr=0,nlen-1 DO BEGIN
      chr = BYTE(STRMID(blk, ichr, 1))
      IF (chr GE 48 AND chr LE 57) THEN BREAK
   ENDFOR
   blkval = FIX(STRMID(blk, ichr, nlen-ichr))

   ;------------------------------------------------------------------------
   ; If it's the same block, get the OGWB code.
   ;------------------------------------------------------------------------

   IF (blkval EQ Block) THEN BEGIN
      idn = toks[2]
      nlen = STRLEN(idn)
      FOR ichr=0,nlen-1 DO BEGIN
         chr = BYTE(STRMID(idn, ichr, 1))
         IF (chr GE 48 AND chr LE 57) THEN BREAK
      ENDFOR
      obj_num[inext] = FIX(STRMID(idn, ichr, nlen-ichr))
      inext += 1
   ENDIF

ENDFOR

;---------------------------------------------------------------------------
; Find the next available unique number in this block.
;---------------------------------------------------------------------------

idstr = '1'

IF (inext GT 0) THEN BEGIN
   obj_num = obj_num[SORT(obj_num)]

   FOR idnum=1,inext DO BEGIN
      IF (obj_num[idnum-1] NE idnum) THEN BREAK
   ENDFOR
   
   idstr = STRTRIM(STRING(idnum),2)
ENDIF

IF (STRLEN(idstr) LT 2) THEN idstr = '0' + idstr

;---------------------------------------------------------------------------
; Concatenate the parts of the string to form the whole name.
;---------------------------------------------------------------------------

UniqueName = 'O' + orbit_str + '-B' + blockbeg + '-' + OGWBcode + idstr

obj_names = 0
obj_num = 0

END  ;  GetUniqueName

;***************************************************************************
PRO FillObjectStruct, State, Block, ObjStr
;***************************************************************************
; Store appropriate object attributes in a new object structure.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

AerType   = !SAV.Digitize.AER_TYPE
GeomType  = !SAV.Digitize.GEOM_TYPE
WindType  = !SAV.Digitize.WIND_TYPE
BandType  = !SAV.Digitize.RETRIEVE_BAND_TYPE

;---------------------------------------------------------------------------
; Save attributes in object structure.
;---------------------------------------------------------------------------

dummy = 0
iret = InitRegionDataVals(dummy)

ObjStr.color_pol = !KON.AerObjTyp.PolyColor[AerType]
ObjStr.color_lin = !KON.GeomObjTyp.LineColor[GeomType]
ObjStr.symbol    = !KON.WindObjTyp.Symbol[WindType]
ObjStr.show_line = !KON.GeomObjTyp.Show[GeomType]
ObjStr.samp_spac = (WindType GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) ? $
                        !SAV.Digitize.SAMP_SPAC_DIR : $
                        !SAV.Digitize.SAMP_SPAC_NODIR

;---------------------------------------------------------------------------
; Create a unique name for the object. A four character code defines the
; aerosol, geometry, wind and band-type used in the retrieval. This can get
; complicated if there is a mix of single-band and double-band retrievals.
; Both members of a double-band retrieval must always use identical names
; except for the band type (these must be R and B in that order). A further
; complication is that the double-band type is indicated by a temporary code
; of Z which must be changed here to R and B. Rely on GetUniqueName to
; return a single band name to use. For a new double-band node, this will
; be the red band name. Change the name of the 2nd member of a double-band
; retrieval here.
;---------------------------------------------------------------------------

GetUniqueName, State, AerType, GeomType, WindType, BandType, Block, unique_name

IF (BandType EQ !KON.BandObjTyp.RED_BAND OR $
    BandType EQ !KON.BandObjTyp.RB_BAND) THEN $
   region_data.name[0] = unique_name

IF (BandType EQ !KON.BandObjTyp.BLUE_BAND) THEN $
   region_data.name[1] = unique_name

ipos = STRPOS(unique_name, 'Z', /REVERSE_SEARCH)
IF (ipos GT 0) THEN BEGIN
   STRPUT, unique_name, 'R', ipos
   region_data.name[0] = unique_name
   STRPUT, unique_name, 'B', ipos
   region_data.name[1] = unique_name
ENDIF

IF (!SAV.Digitize.OverrideRgnName NE '') THEN BEGIN
   IF (BandType EQ !KON.BandObjTyp.RED_BAND OR $
       BandType EQ !KON.BandObjTyp.RB_BAND) THEN $
      region_data.name[0] = !SAV.Digitize.OverrideRgnName
   IF (BandType EQ !KON.BandObjTyp.BLUE_BAND) THEN $
      region_data.name[1] = !SAV.Digitize.OverrideRgnName

   IF (BandType EQ !KON.BandObjTyp.BOTH_BAND) THEN BEGIN
      tempstr = !SAV.Digitize.OverrideRgnName
      ipos  = STRPOS(tempstr, '-', /REVERSE_SEARCH)
      iposR = STRPOS(tempstr, 'R', /REVERSE_SEARCH)
      IF (iposR GT ipos) THEN BEGIN
         region_data.name[0] = !SAV.Digitize.OverrideRgnName
         STRPUT, tempstr, 'B', iposR
         region_data.name[1] = tempstr
      ENDIF ELSE BEGIN
         iposB = STRPOS(tempstr, 'B', /REVERSE_SEARCH)
         IF (iposB GT ipos) THEN BEGIN
            region_data.name[1] = !SAV.Digitize.OverrideRgnName
            STRPUT, tempstr, 'R', iposB
            region_data.name[0] = tempstr
         ENDIF
      ENDELSE
   ENDIF
   !SAV.Digitize.OverrideRgnName = ''
ENDIF

region_data.type = AerType

END  ;  FillObjectStruct

;***************************************************************************
PRO CopyCoords, PtDataIn, PtDataOut
;***************************************************************************
; Copy all the coordinates from one point structure to another.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

PtDataOut.imagealong = PtDataIn.imagealong
PtDataOut.imagecross = PtDataIn.imagecross
PtDataOut.somalong   = PtDataIn.somalong
PtDataOut.somcross   = PtDataIn.somcross
PtDataOut.block      = PtDataIn.block
PtDataOut.along275   = PtDataIn.along275
PtDataOut.cross275   = PtDataIn.cross275
PtDataOut.lat        = PtDataIn.lat
PtDataOut.lon        = PtDataIn.lon 

END  ;  CopyCoords

;***************************************************************************
PRO GetFeaturePoint, PixRadius, NewSomCross, NewSomAlong
;***************************************************************************
; Find the pixel within an N pixel radius that has the greatest contrast in
; radiance with its neighbors. Use as a better center for stereo matching.
; Use nadir camera only.  NOTE - this function is not used.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

WT_ARY  = [[0.71, 1.0, 0.71], [1.0, 0.0, 1.0], [0.71, 1.0, 0.71]]
WT_NORM = 6.84

contrast_save = !KON.Misc.BADVALUE_REAL
cross_save = -1
along_save = -1

;---------------------------------------------------------------------------
; Get the MISR coordinates of the passed point.
;---------------------------------------------------------------------------

new_som_crds = [[NewSomCross, NewSomAlong]]
SomCrdToMisrCrd,  State.Curframe, 0, new_som_crds, misr_crds, retval
MisrCrdToWndwCrd, State.Curframe, misr_crds, wndw_crds, 1, retval

mcross = wndw_crds[0]
malong = wndw_crds[1]
malong = (SIZE(*!VAR.RawImages[0,0]))[2] - malong - 1

;---------------------------------------------------------------------------
; Loop over the pixels in the nearby region.
;---------------------------------------------------------------------------

FOR ialong=malong-PixRadius,malong+PixRadius DO BEGIN

   FOR icross=mcross-PixRadius,mcross+PixRadius DO BEGIN

      ;---------------------------------------------------------------------
      ; Get the 8 pixels adjacent to the current one.
      ;---------------------------------------------------------------------

      adjpix = GetRawImage(icross-1, icross+1, ialong-1, ialong+1, $
                           0, 4, !KON.Instr.HI_RES_PIX_SIZE, $
                           !KON.Misc.INTERP_TYPE_SAMP)

      ;---------------------------------------------------------------------
      ; Find the contrast using a weighting scheme.
      ;---------------------------------------------------------------------

      contrast = TOTAL(ABS(adjpix - GetRawImage(icross, icross, $
                       ialong, ialong, 0, 4, !KON.Instr.HI_RES_PIX_SIZE, $
                       !KON.Misc.INTERP_TYPE_SAMP)) * wt_ary / WT_NORM)

      ;---------------------------------------------------------------------
      ; Keep if it's the largest contrast.
      ;---------------------------------------------------------------------

      IF (contrast GT contrast_save) THEN BEGIN
         contrast_save = contrast
         cross_save = icross
         along_save = ialong
      ENDIF

   ENDFOR
ENDFOR

IF (contrast_save GT 0.0) THEN BEGIN
   along_save = (SIZE(*!VAR.RawImages[0,0]))[2] - along_save - 1
   WndwCrdToMisrCrd, State.Curframe, [[cross_save,along_save]], misr_crds, $
                     retval
   MisrCrdToSomCrd,  State.Curframe, misr_crds, new_som_crds, retval

   NewSomCross = new_som_crds[0]
   NewSomAlong = new_som_crds[1]
ENDIF

END  ;  GetFeaturePoint

;***************************************************************************
PRO AddPointToObject, State, Event
;***************************************************************************
; Add new point to the list of points for this object (point,line,polygon).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

IF (!SAV.Digitize.AER_TYPE EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN RETURN

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it and return so the user can try again.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN BEGIN
   ResetFrame, State, 5, 1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Convert the mouse Event to lat/long and pixel coords.
;---------------------------------------------------------------------------

WndwCrdToMisrCrd,  State.Curframe, [[Event.X,Event.Y]], misr_crds, retval
MisrCrdToSomCrd,   State.Curframe, misr_crds, som_crds, retval
SomCrdToLonlatCrd, State.Curframe, som_crds, lonlat_crds, retval

;---------------------------------------------------------------------------
; Create and fill the data structure for this point.
;---------------------------------------------------------------------------

dummy = 0
iret = InitLinePtData(dummy)

linept_data.feature_type = 0
linept_data.imagecross   = Event.X
linept_data.imagealong   = Event.Y
linept_data.somcross     = som_crds[0]
linept_data.somalong     = som_crds[1]
linept_data.cross275     = misr_crds[0]
linept_data.along275     = misr_crds[1]
linept_data.block        = misr_crds[2]
linept_data.lon          = lonlat_crds[0]
linept_data.lat          = lonlat_crds[1]

;---------------------------------------------------------------------------
; Are we drawing a polygon object? This is sort of a fudge.
;---------------------------------------------------------------------------

is_polygon = $
    (!SAV.Digitize.DIG_STATE[0] GE !KON.AerObjTyp.AER_DUST_OBJ AND $
     !SAV.Digitize.DIG_STATE[0] LE !KON.AerObjTyp.AER_XTRA2_OBJ) ? 1 : 0

;---------------------------------------------------------------------------
; If the current object is null, no objects are active, so create a new one.
; If there are no objects at all, create a linked list to contain them with
; the head in the pHeadObj structure. Always add new objects to the tail of
; the list. Save the type codes for this object in object's data structure.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pThisRgn)) THEN BEGIN
   FillObjectStruct, State, misr_crds[2], region_data
   temp_pHeadObj = !VAR.LinkList.pHeadObj
   !VAR.LinkList.pThisRgn = LListInsertTail(temp_pHeadObj, $
                              !KON.NodeObjTyp.REGION_NODE, region_data)
   !VAR.LinkList.pHeadObj = temp_pheadobj
   (*!VAR.LinkList.pThisRgn).ObjType = !SAV.Digitize.DIG_STATE
ENDIF

;---------------------------------------------------------------------------
; Add the new point to the tail of either the current polygon or current
; line object.
;---------------------------------------------------------------------------

IF is_polygon THEN BEGIN
   tmp_node = (*!VAR.LinkList.pThisRgn).pNextPolyPt
   tmp = (*!VAR.LinkList.pThisRgn).pNextPolyPt
   tmp_type = !KON.NodeObjTyp.POLYPT_NODE
ENDIF ELSE BEGIN
   tmp_node = (*!VAR.LinkList.pThisRgn).pNextLinePt
   tmp = (*!VAR.LinkList.pThisRgn).pNextLinePt
   tmp_type = !KON.NodeObjTyp.LINEPT_NODE
ENDELSE   

!VAR.LinkList.pThisPoint = LListInsertTail(tmp_node, tmp_type, linept_data)

;---------------------------------------------------------------------------
; If there are no points yet in this polygon or line, put it at the head of
; the list in pNextPolyPt position of the region structure. If the object is
; a line object, then we will create 3 additional dummy points to close the
; artificial polygon [this is a kludge].
;---------------------------------------------------------------------------

iDone = 0

IF (~ PTR_VALID(tmp)) THEN BEGIN

   DestroyColorKey, 1  ; destroy region color key windows if visible

   IF is_polygon THEN $
      (*!VAR.LinkList.pThisRgn).pNextPolyPt = !VAR.LinkList.pThisPoint $
   ELSE $
      (*!VAR.LinkList.pThisRgn).pNextLinePt = !VAR.LinkList.pThisPoint

   !VAR.LinkList.pHeadPoint = !VAR.LinkList.pThisPoint

;---------------------------------------------------------------------------
; If the new point is coincident with the first point on object, then close
; the polygon. Any additional points drawn before this line is terminated
; should be a new line object contained within the polygon object - this
; will be the direction line or the line along which height retrievals will
; be done. The first point on this new line is forced to be coincident with
; the first and last points on the polygon.
;---------------------------------------------------------------------------

ENDIF ELSE BEGIN

   IF (is_polygon AND $
       PTR_VALID((*!VAR.LinkList.pThisRgn).pNextPolyPt)) THEN BEGIN

      static_data = [(*(*!VAR.LinkList.pHeadPoint).pData).cross275, $
                     (*(*!VAR.LinkList.pHeadPoint).pData).along275, $
                     (*(*!VAR.LinkList.pHeadPoint).pData).block]

      IF (ComparePoints(static_data, $
                        *(*!VAR.LinkList.pThisPoint).pData, 4) EQ 0 OR $
          !SAV.Digitize.DIG_STATE[1] EQ !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN

         IF (!SAV.Digitize.DIG_STATE[1] EQ $
             !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN

            ; save the point just digitized that will become the 2nd point
            ; on the line where heights are to be determined

            CopyCoords, *((*!VAR.LinkList.pThisPoint).pData), linept_data

            ; make a copy of the point, set it equal to the 1st point on
            ; the region boundary, and store it in the region as the 2nd
            ; point - the 2 points will be equal and will plot as a point

            temp_pThisPoint = !VAR.LinkList.pThisPoint
            CopyCoords, *((*!VAR.LinkList.pHeadPoint).pData), $
                        *((*temp_pThisPoint).pData)

            ; make a copy of the head point of the region and save it as
            ; the head point on line where heights are to be determined

            linept2_data = linept_data
            CopyCoords, *((*!VAR.LinkList.pHeadPoint).pData), linept2_data

            tmp_node = (*!VAR.LinkList.pThisRgn).pNextLinePt
            !VAR.LinkList.pThisPoint = LListInsertTail(tmp_node, $
                          !KON.NodeObjTyp.LINEPT_NODE, linept2_data)
            (*!VAR.LinkList.pThisRgn).pNextLinePt = !VAR.LinkList.pThisPoint
            !VAR.LinkList.pHeadPoint = !VAR.LinkList.pThisPoint
            linept2_data = 0

         ENDIF ELSE BEGIN

            ; make a copy of the point, set it equal to the 1st point on
            ; the region boundary, and store it in the region as 2nd point

            temp_pThisPoint = !VAR.LinkList.pThisPoint
            CopyCoords, *((*!VAR.LinkList.pHeadPoint).pData), $
                        *((*temp_pThisPoint).pData)
            !VAR.LinkList.pThisPoint = temp_pThisPoint
            CopyCoords, *((*!VAR.LinkList.pHeadPoint).pData), linept_data

         ENDELSE

         iDone = 1

         IF (!SAV.Digitize.DIG_STATE[2] GE $
             !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN
            !SAV.Digitize.DIG_STATE[0] = !KON.AerObjTyp.DIR_LINE_OBJ
            !SAV.Digitize.DIG_STATE[1] = !KON.GeomObjTyp.GEOM_LINE_OBJ
            !SAV.Digitize.DIG_STATE[2] = !KON.WindObjTyp.WIND_USER_DIREC_OBJ
            !SAV.Digitize.DIG_STATE[3] = !KON.BandObjTyp.NULL_BAND
            iDone = 0
         ENDIF

         ; save the new point as the 1st point on the direction line or the
         ; 2nd point on the line where heights are to be determined

         tmp_node = (*!VAR.LinkList.pThisRgn).pNextLinePt
         !VAR.LinkList.pThisPoint = $
           LListInsertTail(tmp_node, !KON.NodeObjTyp.LINEPT_NODE, linept_data)

         IF (!SAV.Digitize.GEOM_TYPE NE !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN $
            (*!VAR.LinkList.pThisRgn).pNextLinePt = !VAR.LinkList.pThisPoint
      ENDIF

   ENDIF
ENDELSE

;---------------------------------------------------------------------------
; Draw line segment formed by the new point and the previous point, if there
; is one. To do this interactively, providing the ability to undraw points,
; we must redraw the entire list of objects for each new point.
;---------------------------------------------------------------------------

xmax = State.sizex - 1
ymax = State.sizey - 1

npts = 1
data_coords = INTARR(2,2)
data_coords[0:1,0] = [Event.X, Event.Y]

IF (!VAR.LinkList.PrevEventX GE 0 AND $
    !VAR.LinkList.PrevEventY GE 0) THEN BEGIN
   npts = 2
   data_coords[0:1,1] = [!VAR.LinkList.PrevEventX, !VAR.LinkList.PrevEventY]
ENDIF

SWIN = !D.WINDOW
SafeWSET, State.draw_win, didit
pwin = *State.pwinHdl
DEVICE, COPY = [0, 0, State.sizex, State.sizey, 0, 0, pwin[State.curframe]]

RedrawObjects, State, 0, 0, State.sizex, State.sizey, 0
IF (State.showfire GT 0) THEN RedrawModisFires, State, $
                                    0, 0, State.sizex, State.sizey
*State.pwinHdl = pwin
EMPTY
SafeWSET, SWIN, didit

!VAR.LinkList.PrevEventX = Event.X
!VAR.LinkList.PrevEventY = Event.Y

;---------------------------------------------------------------------------
; If just clicking on the first point again is enough, send a fake right
; mouse button click message to terminate cleanly. This is the case whenever
; we are not using a user-digitized wind direction.
;---------------------------------------------------------------------------

IF (iDone EQ 1) THEN BEGIN
   mouse_event = Event
   mouse_event.press = 4
   WIDGET_CONTROL, State.wTopWorkBase, SEND_EVENT=mouse_event
ENDIF

END  ;  AddPointToObject

;***************************************************************************
PRO DoneDrawingObject, State, Event
;***************************************************************************
; The drawing of this object (point, line, polygon) is done. Null the object
; pointers so another object can be drawn.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON data_structs, region_data, linept_data

IF (~ PTR_VALID(!VAR.LinkList.pThisRgn)) THEN RETURN
IF (!SAV.Digitize.AER_TYPE EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN RETURN

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; If the line we are terminating is the direction line in a plume or is a
; line along which we will determine heights, then resample the line to a
; fixed interval along a fitted spline and replace the original points in
; the linked list with the new set.
;---------------------------------------------------------------------------

num_good_pts = 0
pheadpt = (*!VAR.LinkList.pThisRgn).pNextLinePt

DIG_COPY = [!SAV.Digitize.AER_TYPE, $
            !SAV.Digitize.GEOM_TYPE, $
            !SAV.Digitize.WIND_TYPE, $
            !SAV.Digitize.RETRIEVE_BAND_TYPE]

IF (!SAV.Digitize.DIG_STATE[0] EQ !KON.AerObjTyp.DIR_LINE_OBJ) THEN BEGIN

   p2ndpt = PTR_NEW()
   IF (PTR_VALID(pheadpt)) THEN p2ndpt = (*pheadpt).pNextSib

   IF (~ PTR_VALID(p2ndpt) AND !SAV.Digitize.GEOM_TYPE NE $
        !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN

      err_msg = ['You must close the polygon and then draw a direction ', $
                 'line containing at least one additional point.', $
                 '         Your line will be removed.']
      result = DIALOG_MESSAGE(err_msg, /ERROR, /CENTER)
      temp_pThisRgn = !VAR.LinkList.pThisRgn
      LListDelRgnObj, temp_pThisRgn
      !VAR.LinkList.pThisRgn = temp_pThisRgn
      IF (!SAV.Digitize.AER_TYPE GE $
               !KON.AerObjTyp.AER_DUST_OBJ AND $
          !SAV.Digitize.AER_TYPE LE $
              !KON.AerObjTyp.AER_XTRA2_OBJ) THEN $
         !SAV.Digitize.DIG_STATE = DIG_COPY
      GOTO, redoit33

   ENDIF

   ResampleDirLine, State, !VAR.LinkList.pThisRgn, $
                    !SAV.Digitize.SAMP_SPAC_DIR, Retval

   IF (Retval LT 0) THEN BEGIN
      IF (Retval EQ -1) THEN BEGIN
         err_msg = ['Your direction line must be long enough to accommodate', $
                    'at least 3 resampled points. Your line will be removed.']
         result = DIALOG_MESSAGE(err_msg, /ERROR, /CENTER)
      ENDIF
      temp_pThisRgn = !VAR.LinkList.pThisRgn
      LListDelRgnObj, temp_pThisRgn
      !VAR.LinkList.pThisRgn = temp_pThisRgn
      !SAV.Digitize.DIG_STATE = DIG_COPY
      GOTO, redoit33
   ENDIF

   pSaveObj = !VAR.LinkList.pThisRgn
   !SAV.Digitize.DIG_STATE = DIG_COPY

;---------------------------------------------------------------------------
; If the line is a polygon surrounding a region where we want to determine
; heights, test that there are enough points.
;---------------------------------------------------------------------------

ENDIF ELSE BEGIN

   IF (LListCount(pHeadPt) LE 0) THEN BEGIN
      err_msg = ['You must first draw and close a polygon and then draw a', $
                 'direction line containing at least one additional point.',$
                 '            Your line will be removed.']
      result = DIALOG_MESSAGE(err_msg, /ERROR, /CENTER)
      temp_pThisRgn = !VAR.LinkList.pThisRgn
      LListDelRgnObj, temp_pThisRgn
      !VAR.LinkList.pThisRgn = temp_pThisRgn
      !SAV.Digitize.DIG_STATE = DIG_COPY
      GOTO, redoit33
   ENDIF

ENDELSE

;---------------------------------------------------------------------------
; Determine which pixel spacing to use for retrieval.
;---------------------------------------------------------------------------

PIX_SEP = (!SAV.Digitize.WIND_TYPE GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) ? $
           !SAV.Digitize.SAMP_SPAC_DIR : !SAV.Digitize.SAMP_SPAC_NODIR

;---------------------------------------------------------------------------
; Create a set of points inside the digitized outline for computing heights,
; winds etc. For each point in region at which a height determination is to
; be made, get the wind direction from the point on the direction line
; nearest to the region point.
;---------------------------------------------------------------------------

num_good_pts = 3  ; need 3 for lines to pass test at bottom

IF (!SAV.Digitize.GEOM_TYPE GE !KON.GeomObjTyp.GEOM_POLYGON_OBJ AND $
    !SAV.Digitize.AER_TYPE  GE !KON.AerObjTyp.AER_DUST_OBJ AND $
    !SAV.Digitize.AER_TYPE  LE !KON.AerObjTyp.AER_XTRA2_OBJ) THEN BEGIN
   temp_pHeadObj = !VAR.LinkList.pHeadObj
   temp_pThisRgn = !VAR.LinkList.pThisRgn

   GetPointsInRegion, State, PIX_SEP, temp_pHeadObj, temp_pThisRgn, $
                      num_good_pts

   pSaveObj = !VAR.LinkList.pThisRgn
ENDIF

;---------------------------------------------------------------------------
; Reset the pointers so we can draw a new line.
;---------------------------------------------------------------------------

redoit33:

!VAR.LinkList.pThisRgn   = PTR_NEW()
!VAR.LinkList.pHeadPoint = PTR_NEW()
!VAR.LinkList.pThisPoint = PTR_NEW()
!VAR.LinkList.PrevEventX = -1
!VAR.LinkList.PrevEventY = -1

;---------------------------------------------------------------------------
; Redraw window.
;---------------------------------------------------------------------------

RedrawWindow, State, State.curframe

;---------------------------------------------------------------------------
; Set the data type to display in the animation window for the object just
; digitized - either zero-wind height or wind-corrected height.
;---------------------------------------------------------------------------

!VAR.DataRgn.DATA_TYPE = $
     (!SAV.Digitize.WIND_TYPE EQ !KON.WindObjTyp.WIND_NO_DIREC_OBJ) ? $
          !KON.DataRgn.TYPE_ZEROWIND_HT : !KON.DataRgn.TYPE_WINDCORR_HT

DestroyColorKey, 1

;---------------------------------------------------------------------------
; If a region and its points were resampled satisfactorily, call the routine
; to compute heights and winds.
;---------------------------------------------------------------------------

IF (num_good_pts GE 3) THEN $
   ProcessRegionParams, State, pSaveObj, retval

;---------------------------------------------------------------------------
; Reset the fire pixel flags so they can be used again if the same plume is
; redigitized without deleting the first occurrence.
;---------------------------------------------------------------------------

pFireObj = PTR_NEW()
pNextObj = !VAR.LinkList.pHeadObj

WHILE (PTR_VALID(pNextObj)) DO BEGIN
   IF (((*pNextObj).ObjType[0]) EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
      pFireObj = pNextObj
      BREAK
   ENDIF
   pNextObj = (*pNextObj).pNextSib
ENDWHILE

IF (PTR_VALID(pFireObj)) THEN BEGIN
   PIX_SEP = $
      ((*pNextObj).ObjType[2] GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) ? $
        !SAV.Digitize.SAMP_SPAC_DIR : !SAV.Digitize.SAMP_SPAC_NODIR
   NumFound = 0
   pixsep = ROUND(PIX_SEP / !KON.Instr.HI_RES_PIX_SIZE)
   ResetFirePixList, pNextObj, pFireObj, pixsep, NumFound
ENDIF

END  ;  DoneDrawingObject

;***************************************************************************
PRO DeleteObjOrPt, State, Event
;***************************************************************************
; Remove entire object (point, line, polygon) or single point on an object.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON data_structs, region_data, linept_data

;---------------------------------------------------------------------------
; Return if there are no objects to delete.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN RETURN

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it and return so the user can try again.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN BEGIN
   ResetFrame, State, 5, 1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Convert the mouse Event to block/along/across coords for comparing to
; values in database.
;---------------------------------------------------------------------------

WndwCrdToMisrCrd, State.Curframe, [[Event.X,Event.Y]], misr_crds, retval

;---------------------------------------------------------------------------
; Find the fire pixel object if there is one.
;---------------------------------------------------------------------------

pFireObj = PTR_NEW()
pNextObj = !VAR.LinkList.pHeadObj

WHILE (PTR_VALID(pNextObj)) DO BEGIN
   IF (((*pNextObj).ObjType[0]) EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
      pFireObj = pNextObj
      BREAK
   ENDIF
   pNextObj = (*pNextObj).pNextSib
ENDWHILE

;---------------------------------------------------------------------------
; Find if the mouse was clicked near a point by comparing the mouse SOM
; coordinates with stored node coordinates. This means going through all the
; points in all the objects until one that matches is found.
;---------------------------------------------------------------------------

pNextObj = !VAR.LinkList.pHeadObj

WHILE (PTR_VALID(pNextObj)) DO BEGIN

   ;------------------------------------------------------------------------
   ; Don't allow deleting fire pixels.
   ;------------------------------------------------------------------------

   IF (((*pNextObj).ObjType[0]) EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
      pNextObj = (*pNextObj).pNextSib
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Set up to compare polygons and lines.
   ;------------------------------------------------------------------------

   pNearest = LListSearch((*pNextObj).pNextPolyPt, "ComparePoints", $
                           misr_crds, 4)
   IF (PTR_VALID(pNearest)) THEN BEGIN
      head_pt = (*pNextObj).pNextPolyPt
   ENDIF ELSE BEGIN
      pNearest = LListSearch((*pNextObj).pNextLinePt, "ComparePoints", $
                              misr_crds, 4)
      IF (PTR_VALID(pNearest)) THEN head_pt = (*pNextObj).pNextLinePt
   ENDELSE

   ;------------------------------------------------------------------------
   ; If a point was found, confirm with user, then delete it. If there were
   ; fire pixels in the region, their "used" flags must be reset so they can
   ; be reused. Also delete files for this object written to disk.
   ;------------------------------------------------------------------------

   IF (PTR_VALID(pNearest)) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Find the pixel separation value used for a region.
      ;---------------------------------------------------------------------

      PIX_SEP = $
         ((*pNextObj).ObjType[2] GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) ? $
           !SAV.Digitize.SAMP_SPAC_DIR : !SAV.Digitize.SAMP_SPAC_NODIR

      ;---------------------------------------------------------------------
      ; Show the region about to be deleted by flashing the outline in black
      ; and white and give the user a chance to back out.
      ;---------------------------------------------------------------------

      SafeWSET, State.draw_win, didit

      symbol = (*((*pNextObj).pData)).symbol
      color1 = (*((*pNextObj).pData)).color_pol
      color2 = (*((*pNextObj).pData)).color_lin

      (*((*pNextObj).pData)).symbol = 'triangle'
      
      FOR irpt=0,2 DO BEGIN
         (*((*pNextObj).pData)).color_pol = !KON.Colors.ColorNames[6] ; white
         (*((*pNextObj).pData)).color_lin = !KON.Colors.ColorNames[6] ; white
         RedrawObjects, State, 0, 0, State.sizex, State.sizey, pNextObj
         WAIT, .1
         (*((*pNextObj).pData)).color_pol = !KON.Colors.ColorNames[9] ; black
         (*((*pNextObj).pData)).color_lin = !KON.Colors.ColorNames[9] ; white
         RedrawObjects, State, 0, 0, State.sizex, State.sizey, pNextObj
         WAIT, .1
      ENDFOR
      
      (*((*pNextObj).pData)).color_pol = !KON.Colors.ColorNames[6] ; white
      (*((*pNextObj).pData)).color_lin = !KON.Colors.ColorNames[6] ; white
      RedrawObjects, State, 0, 0, State.sizex, State.sizey, pNextObj

      (*((*pNextObj).pData)).symbol = symbol
      (*((*pNextObj).pData)).color_pol = color1
      (*((*pNextObj).pData)).color_lin = color2

      msg = ['Do you really want to delete the highlighted', $
             'region containing the clicked point?']
      ret = DIALOG_MESSAGE(msg, /QUESTION, /CENTER)

      IF (STRUPCASE(ret) EQ 'YES') THEN BEGIN

         ;------------------------------------------------------------------
         ; Reset the fire pixel "used" flags if pertinent.
         ;------------------------------------------------------------------

         IF (PTR_VALID(pFireObj)) THEN BEGIN
            NumFound = 0
            pixsep = ROUND(PIX_SEP / !KON.Instr.HI_RES_PIX_SIZE)
            ResetFirePixList, pNextObj, pFireObj, pixsep, NumFound
         ENDIF

         ;------------------------------------------------------------------
         ; Delete the files associated with the object(s) on disk. If this
         ; was a double-band retrieval, delete both red and blue files.
         ;------------------------------------------------------------------

         num_retr = ((*((*pNextObj).pData)).band_type EQ $
                     !KON.BandObjTyp.BOTH_BAND) ? 2 : 1
         rgn_name = (*((*pNextObj).pData)).name
         DeleteRegionFiles, num_retr, rgn_name

         ;------------------------------------------------------------------
         ; Delete the object from memory.
         ;------------------------------------------------------------------

         LListDelRgnObj, pNextObj

         BREAK
      ENDIF
      msg = 0
   ENDIF

   pNextObj = (*pNextObj).pNextSib

ENDWHILE

;---------------------------------------------------------------------------
; Reset the window to An and redraw.
;---------------------------------------------------------------------------

ResetFrame, State, 5, 1

END  ;  DeleteObjOrPt

;***************************************************************************
PRO LoadFirePixelData, Unit, State, FireType, Retval
;***************************************************************************
; Procedure reads and displays MODIS fire pixels.
; EndLineConvert = 1 requires conversion from OSX/Unix -> Windows.
; EndLineConvert = 2 requires conversion from Windows -> OSX/Unix.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON data_structs, region_data, linept_data

Retval = -1
IF (!VAR.CurrFiles.Fire_Loaded) THEN RETURN

;---------------------------------------------------------------------------
; Delete any image in the work window data first so if user has corrected
; registration already, that image will not be there. This puts fire pixels
; against a black background - easier to see.
;---------------------------------------------------------------------------

ResetFrame, State, 0, 0

WorkImage[*,*,*] = 0.0
TV, WorkImage, /ORDER, TRUE=((CoordStruct.(0).num_band EQ 1) ? 0 : 3)

RedrawWindow, State, State.curframe
ResetSwathData

;---------------------------------------------------------------------------
; Display the An camera image for orbit 1.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

whichorbit = (State.curFrame GT 9)

;---------------------------------------------------------------------------
; Initiate error handling. Use this to detect end of file below and break out.
;---------------------------------------------------------------------------

numline1 = 0
numline2 = 0

CATCH, iErr
IF (iErr NE 0) THEN BEGIN
   IF (!ERROR_STATE.NAME EQ 'IDL_M_FILE_EOF') THEN BEGIN
      numlines = STRTRIM(STRING(numline1),2)
      GOTO, readdone1
   ENDIF ELSE BEGIN
      numlines = STRTRIM(STRING(numline2),2)
      mssg = [!ERROR_STATE.MSG, $
              'Error reading MODIS fire pixel file at data line ' + $
              numlines, 'Fix the file and try running again.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      CATCH, /CANCEL  
      !VAR.CurrFiles.Fire_Loaded = 0
      !VAR.CurrFiles.FireFile = ''
      RETURN  
   ENDELSE
ENDIF

;---------------------------------------------------------------------------
; Read 1st 2 lines and check that we have data for the correct orbit. Then
; skip the following 2 lines.
;---------------------------------------------------------------------------

BegMidEnd = 0
ReadNextAsciiLine, Unit, BegMidEnd, line_str
ReadNextAsciiLine, Unit, BegMidEnd, line_str

toks = STRSPLIT(STRING(line_str), ' ', COUNT=ntok, /EXTRACT)
orbitnum = LONG(toks[0])

IF (orbitnum EQ 0 AND BegMidEnd EQ 0) THEN BEGIN
   mssg = ['The file you selected may not be a valid', $
           'fire pixel file. Fix it and try again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   CATCH, /CANCEL
   !VAR.CurrFiles.Fire_Loaded = 0
   !VAR.CurrFiles.FireFile = ''
   RETURN
ENDIF

IF (orbitnum NE CoordStruct.(whichorbit).OrbitNum AND $
    orbitnum NE FIX(!KON.Misc.BADVALUE_REAL)) THEN BEGIN
   mssg = ['The file you selected may be for the', $
           'wrong orbit. Fix it and try again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   CATCH, /CANCEL
   !VAR.CurrFiles.Fire_Loaded = 0
   !VAR.CurrFiles.FireFile = ''
   RETURN
ENDIF

ReadNextAsciiLine, Unit, BegMidEnd, line_str
ReadNextAsciiLine, Unit, BegMidEnd, line_str

;---------------------------------------------------------------------------
; If the current object is null, no objects are active, so create a new one.
; If there are no objects at all, create a linked list to contain them with
; the head in the pHeadObj structure. Always add new objects to the tail of
; the list. Set the object type.
;---------------------------------------------------------------------------

Op_DisableEditObjects, State, 0

temp_AerType  = !SAV.Digitize.AER_TYPE
temp_GeomType = !SAV.Digitize.GEOM_TYPE

!SAV.Digitize.AER_TYPE  = !KON.AerObjTyp.FIREPIXEL_OBJ
!SAV.Digitize.GEOM_TYPE = !KON.GeomObjTyp.GEOM_NULL_OBJ

IF (~ PTR_VALID(!VAR.LinkList.pThisRgn)) THEN BEGIN
   FillObjectStruct, State, orbitnum, region_data
   temp_pHeadObj = !VAR.LinkList.pHeadObj
   !VAR.LinkList.pThisRgn = LListInsertTail(temp_pHeadObj, $
                              !KON.NodeObjTyp.REGION_NODE, region_data)
   !VAR.LinkList.pHeadObj = temp_pHeadObj
   (*!VAR.LinkList.pThisRgn).ObjType = [!SAV.Digitize.AER_TYPE, $
                                        !SAV.Digitize.GEOM_TYPE, $
                                        !SAV.Digitize.WIND_TYPE, $
                                        !SAV.Digitize.RETRIEVE_BAND_TYPE]
ENDIF

;---------------------------------------------------------------------------
; Loop over all the lines that follow. 
;---------------------------------------------------------------------------

lon = 0.0D
lat = 0.0D
blk = 0
samp = 0
line = 0
conf = 0
numline1 = 0
numline2 = 0
orbit_date = ''
buff = ''

WHILE (1) DO BEGIN

   ;------------------------------------------------------------------------
   ; Read the longitude/latitude coords and MISR coords on the next lines if
   ; the file is derived from the ModVolc database (U Hawaii). If the file
   ; is derived from MODIS MOD14 granules, also read other fields.
   ;------------------------------------------------------------------------

   ReadNextAsciiLine, Unit, BegMidEnd, line_str
  
   toks = STRSPLIT(line_str, ' ', COUNT=ntok, /EXTRACT)
   IF (ntok LT 9) THEN BEGIN
      IF (BegMidEnd EQ 3) THEN BREAK
      CONTINUE
   ENDIF
   
   lon = DOUBLE(toks[0])
   lat = DOUBLE(toks[1])
   blk = toks[2]
   samp = toks[3]
   line = toks[4]

   IF (FireType EQ 1) THEN power = toks[5]   ;  MODIS
   IF (FireType EQ 2) THEN power = 0.0       ;  ModVolc

   IF ((blk LT CoordStruct.(whichorbit).BlkBeg) OR $
       (blk GT CoordStruct.(whichorbit).BlkEnd)) THEN BEGIN
      IF (BegMidEnd EQ 3) THEN BREAK
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Convert the lat/lon coords to SOM coords.
   ;------------------------------------------------------------------------

   LonlatCrdToSomCrd, State.curframe, 0, [lon,lat], som_crds, retval

   ;------------------------------------------------------------------------
   ; Create and fill the data structure for this point.
   ;------------------------------------------------------------------------

   dummy = 0
   iret = InitLinePtData(dummy)

   linept_data.feature_type  = 0
   linept_data.imagecross    = 0
   linept_data.imagealong    = 0
   linept_data.somcross      = som_crds[0]
   linept_data.somalong      = som_crds[1]
   linept_data.cross275      = samp
   linept_data.along275      = line
   linept_data.block         = blk
   linept_data.lon           = lon
   linept_data.lat           = lat
   linept_data.modis_data[0] = power
   linept_data.modis_data[1] = 0.0

   ;------------------------------------------------------------------------
   ; Add the new point to the tail of the current line object.
   ;------------------------------------------------------------------------

   tmp_node = (*!VAR.LinkList.pThisRgn).pNextLinePt
   tmp = (*!VAR.LinkList.pThisRgn).pNextLinePt
   tmp_type = !KON.NodeObjTyp.LINEPT_NODE

   !VAR.LinkList.pThisPoint = LListInsertTail(tmp_node, tmp_type, $
                                              linept_data)

   ;------------------------------------------------------------------------
   ; If there are no points yet in this polygon or line, it goes at head of
   ; the list in the pNextPolyPt position of region structure.
   ;------------------------------------------------------------------------

   iDone = 0

   IF (~ PTR_VALID(tmp)) THEN BEGIN
      (*!VAR.LinkList.pThisRgn).pNextLinePt = !VAR.LinkList.pThisPoint
      !VAR.LinkList.pHeadPoint = !VAR.LinkList.pThisPoint
   ENDIF

   numline1 += 1

   IF (BegMidEnd EQ 3) THEN BREAK
ENDWHILE

;---------------------------------------------------------------------------
; Clean up and return.
;---------------------------------------------------------------------------

readdone1: CATCH, /CANCEL  

IF (numline1 GT 0) THEN BEGIN
   !VAR.LinkList.pThisRgn   = PTR_NEW()
   !VAR.LinkList.pHeadPoint = PTR_NEW()
   !VAR.LinkList.pThisPoint = PTR_NEW()

   !VAR.CurrFiles.Fire_Loaded = 1
   State.showfire = 1
   WIDGET_CONTROL, State.wFramesFireButton, SENSITIVE  = 1
   WIDGET_CONTROL, State.wFramesFireButton, SET_BUTTON = 1
   RedrawWindow, State, State.curFrame  ;  required
   RedrawModisFiresInOps, State
ENDIF ELSE BEGIN
   mssg = ['No fire pixels were found in this', $
           'file. Fix it and try again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   !VAR.CurrFiles.Fire_Loaded = 0
   !VAR.CurrFiles.FireFile = ''
ENDELSE

!SAV.Digitize.AER_TYPE  = temp_AerType
!SAV.Digitize.GEOM_TYPE = temp_GeomType

Retval = 0

END  ;  LoadFirePixelData

;***************************************************************************
PRO Op_ReadFirePixels, State, Retval
;***************************************************************************
; Procedure reads objects to display over the window image.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct

   Retval = -1
   unitnum = 5

   ;------------------------------------------------------------------------
   ; Make sure the displayed image is the An camera for orbit 1. If not,
   ; then correct it and return so the user can try again.
   ;------------------------------------------------------------------------

   IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

   whichorbit = (State.curframe GT 9)

   ;------------------------------------------------------------------------
   ; Ask the user for the name of a file containing fire pixels to draw.
   ;------------------------------------------------------------------------

   IF (!VAR.CurrFiles.FireFile EQ '' AND $
      ~!VAR.CurrFiles.Fire_Loaded) THEN BEGIN
      orbitstr = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
      FilenameFilter = ['*'+orbitstr+'*.txt']  ; 
      temp_FireFile = !VAR.CurrFiles.FireFile
      GetLastFilename, 0, !KON.FileTyp.TypeFire, FilenameFilter, 0, $
                       file_outpath, temp_FireFile
      !VAR.CurrFiles.FireFile = temp_FireFile
      IF (!VAR.CurrFiles.FireFile EQ '') THEN RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Uncheck the button if user clicks "cancel" in dialog_pickfile.
   ;------------------------------------------------------------------------

   IF (STRTRIM(!VAR.CurrFiles.FireFile,2) EQ '') THEN BEGIN
      WIDGET_CONTROL, State.wFramesFireButton, SET_BUTTON=0
      State.showfire = 0
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Open the file in text mode and read the first line to determine what
   ; kind of file this is. If this is a MODIS fire event file, the first two
   ; words on the first line must be "Fire pixels". The 2nd line specifies
   ; the orbit number, and the next line gives the number of records that
   ; follow, with one block/sample/line triplet per record separated by
   ; spaces. The sample/line coords are at 1100m res.   
   ;------------------------------------------------------------------------
   
   OPENR, unitnum, !VAR.CurrFiles.FireFile, /GET_LUN

   BegMidEnd = 0
   ReadNextAsciiLine, unitnum, BegMidEnd, filetype
   POINT_LUN, unitnum, 0

   type_tokens = STRSPLIT(STRING(filetype), ' ', COUNT=ntokens, /EXTRACT)

   IF (ntokens LT 2) THEN BEGIN
      mssg = ['Cannot read file: ' + !VAR.CurrFiles.FireFile, $
              'Fix this file and try again.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      FREE_LUN, unitnum
      RETURN
   ENDIF

   IF (type_tokens[0] NE 'Fire' OR type_tokens[1] NE 'pixels' OR $
       (type_tokens[3] NE 'MODIS' AND type_tokens[3] NE 'ModVolc')) THEN BEGIN
      mssg = ['"Fire pixels" must be the first two words on line 1 of file', $
              !VAR.CurrFiles.FireFile, $
              ', and the 3rd word must be either "MODIS" or "ModVolc".', $
              'Fix this file and try again.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      FREE_LUN, unitnum
      RETURN
   ENDIF

   FireType = (type_tokens[3] EQ 'MODIS') ? 1 : 2

   LoadFirePixelData, unitnum, State, FireType, Retval
      
   !SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO

   FREE_LUN, unitnum

END  ;  Op_ReadFirePixels

;***************************************************************************
PRO Op_ReadMarkerPixels, State, CurFrame, Retval
;***************************************************************************
; Procedure reads objects to display over the window image.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON data_structs, region_data, linept_data
COMMON coord_data, CoordStruct

   Retval = -1
   num_good_pts = 0L

   whichorbit = (State.curframe GT 9)

   max_obj_str = STRTRIM(STRING(!MRK.max_objects),2)
   max_pts_str = STRTRIM(STRING(!MRK.max_obj_pts),2)

   ;------------------------------------------------------------------------
   ; Initiate error handling.
   ;------------------------------------------------------------------------

   CATCH, iErr
   IF (iErr NE 0) THEN BEGIN
      mssg = $
        ['Error reading marker point file. Check that you have', $
         'correctly specified both the number of objects in the', $
         'file (max ' + max_obj_str + ') and the number of points in each object', $
         '(max ' + max_pts_str + '), and that there are no alphabetic characters in', $
         'numeric positions. Also, file extension must be "txt".', $
         ' ', 'Fix the file and try reading it again.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      num_good_pts = 0
      GOTO, readdone1a
   ENDIF

   WIDGET_CONTROL, State.wFramesWhichSlider, SET_VALUE = CurFrame

   ;------------------------------------------------------------------------
   ; Ask the user for the name of file containing markers to draw. First ask
   ; if user wants a MINX standard files or not. If yes, then load from MINX
   ; directory. Otherwise, ask user for directory to look in.
   ;------------------------------------------------------------------------

   load_option = 3
   IF (!VAR.CurrFiles.MarkerFile NE '' OR $
       !VAR.CurrFiles.Marker_Loaded) THEN $
      load_option = 4

   GetUserMarkerPref_gui, State.wTopWorkBase, load_option

   IF (load_option EQ 4) THEN BEGIN
      State.showmarker = 0
      !VAR.CurrFiles.MarkerFile = ''
      !VAR.CurrFiles.Marker_Loaded = 0
      WIDGET_CONTROL, State.wFramesMarkerButton, SET_BUTTON=0
      WIDGET_CONTROL, State.wFramesMarkerButton, SENSITIVE=0
      RedrawWindow, State, CurFrame

      !SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO

      CATCH, /CANCEL
      RETURN
   ENDIF

   IF (!VAR.CurrFiles.MarkerFile EQ '' AND $
      ~!VAR.CurrFiles.Marker_Loaded) THEN BEGIN
      IF (load_option EQ 0) THEN BEGIN
         CATCH, /CANCEL
         RETURN
      ENDIF
      IF (load_option EQ 1) THEN temp_MarkerFile = $
         !KON.Misc.MINX_DIRECTORY + '/data/AeronetSiteList.txt'
      IF (load_option EQ 2) THEN temp_MarkerFile = $
         !KON.Misc.MINX_DIRECTORY + '/data/VolcanoLocationList.txt'
      IF (load_option EQ 3) THEN BEGIN
         orbitstr = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
         FilenameFilter = ['*.txt']
         temp_MarkerFile = !VAR.CurrFiles.MarkerFile
         GetLastFilename, 0, !KON.FileTyp.TypeMarker, FilenameFilter, 0, $
                          file_outpath, temp_MarkerFile
      ENDIF
      !VAR.CurrFiles.MarkerFile = temp_MarkerFile
      IF (!VAR.CurrFiles.MarkerFile EQ '') THEN GOTO, readdone1a
   ENDIF

   ;------------------------------------------------------------------------
   ; Uncheck the button if user clicks "cancel" in dialog_pickfile.
   ;------------------------------------------------------------------------

   IF (STRTRIM(!VAR.CurrFiles.MarkerFile,2) EQ '') THEN BEGIN
      WIDGET_CONTROL, State.wFramesMarkerButton, SET_BUTTON=0
      WIDGET_CONTROL, State.wFramesMarkerButton, SENSITIVE=0
      State.showmarker = 0
      GOTO, readdone1a
   ENDIF

   ;------------------------------------------------------------------------
   ; Show hourglass, then read some file contents.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, /HOURGLASS

   OPENR, unitnum, !VAR.CurrFiles.MarkerFile, /GET_LUN

   buff = ''
   BegMidEnd = 0
   ReadNextAsciiLine, unitnum, BegMidEnd, buff
   !MRK.title = buff

   ReadNextAsciiLine, unitnum, BegMidEnd, buff
   numobjs = FIX(buff)
   IF (numobjs GT !MRK.max_objects) THEN BEGIN
      mssg = ['The maximum number of objects allowed in the', $
              'marker file is ' + max_obj_str + '. The objects are truncated', $
              'when that limit is reached.']
      rval = DIALOG_MESSAGE(mssg, /CENTER)
      numobjs = !MRK.max_objects
   ENDIF

   !MRK.num_objects = numobjs

   ;------------------------------------------------------------------------
   ; Read all the point data for each object.
   ;------------------------------------------------------------------------

   npoints = INTARR(numobjs)
   whichorbit = (CurFrame GT 9)
   showed_msg = 0

   FOR iobj=0L,numobjs-1 DO BEGIN

      pt_name = ''
      ReadNextAsciiLine, unitnum, BegMidEnd, buff
      tokens = STRSPLIT(buff, ' ', COUNT=num_token, /EXTRACT)

      IF (num_token GT 0) THEN !MRK.num_obj_pts[iobj] = FIX(tokens[0])
      IF (!MRK.num_obj_pts[iobj] GT !MRK.max_obj_pts AND $
          ~showed_msg) THEN BEGIN
         mssg = ['The maximum number of points allowed in any', $
                 'object is ' + max_pts_str + '. The points are truncated when', $
                 'that limit is reached.']
         rval = DIALOG_MESSAGE(mssg, /CENTER)
         showed_msg = 1
      ENDIF

      IF (num_token GT 1) THEN !MRK.object_shape[iobj] = tokens[1]
      IF (num_token GT 2) THEN !MRK.object_color[iobj] = tokens[2]
      IF (num_token GT 3) THEN BEGIN
         FOR itok=3,num_token-1 DO BEGIN
            IF (itok GT 3) THEN pt_name += ' '
            pt_name += tokens[itok]
         ENDFOR
         !MRK.object_name[iobj] = pt_name
      ENDIF ELSE BEGIN
         !MRK.object_name[iobj] = ''
      ENDELSE

      FOR ipts=0,!MRK.num_obj_pts[iobj]-1 DO BEGIN
         ReadNextAsciiLine, unitnum, BegMidEnd, buff
         IF (ipts GE !MRK.max_obj_pts) THEN CONTINUE
         tokens = STRSPLIT(buff, ' ', COUNT=num_token, /EXTRACT)
         IF (num_token GE 2) THEN BEGIN
            lon = FLOAT(tokens[0])
            lat = FLOAT(tokens[1])
            !MRK.obj_pt_lon[iobj,ipts] = lon
            !MRK.obj_pt_lat[iobj,ipts] = lat
         ENDIF
      ENDFOR

      IF (!MRK.num_obj_pts[iobj] GT !MRK.max_obj_pts) THEN $
         !MRK.num_obj_pts[iobj] = !MRK.max_obj_pts

   ENDFOR

   FREE_LUN, unitnum

   ;------------------------------------------------------------------------
   ; Convert lat/lon values to SOM coordinates compatible with path being
   ; displayed. Convert the SOM coordinates to MISR coordinates compatible
   ; with path being displayed and finally to window coords.
   ;------------------------------------------------------------------------

   FOR iobj = 0, numobjs-1 DO BEGIN
      numpts = !MRK.num_obj_pts[iobj]

      line_crds = DBLARR(2, numpts)

      line_crds[0,*] = !MRK.obj_pt_lon[iobj,0:numpts-1]
      line_crds[1,*] = !MRK.obj_pt_lat[iobj,0:numpts-1]

      LonlatCrdToSomCrd, State.Curframe, 0, line_crds, som_crds, retval2
      SomCrdToMisrCrd,   State.Curframe, 1, som_crds, misr_crds, retval3
      MisrCrdToWndwCrd,  State.Curframe, misr_crds, wndw_crds, 1, retval4

      !MRK.obj_pt_275_samp[iobj,0:numpts-1] = misr_crds[0,*]
      !MRK.obj_pt_275_line[iobj,0:numpts-1] = misr_crds[1,*]
      !MRK.obj_pt_block[iobj,0:numpts-1]    = misr_crds[2,*]

      ndx_gtzero = WHERE(misr_crds[2,*] GT 0, numndx_gtzero)
      IF (numndx_gtzero GT 0) THEN num_good_pts += numpts

      ndx_gtzero = 0
      line_crds  = 0
      som_crds   = 0
      misr_crds  = 0
      wndw_crds  = 0
   ENDFOR

   Retval = 0

   ;------------------------------------------------------------------------
   ; Clean up and return.
   ;------------------------------------------------------------------------

readdone1a: CATCH, /CANCEL  

   IF (num_good_pts GT 0) THEN BEGIN
      !VAR.CurrFiles.Marker_Loaded = 1
      State.showmarker = 1
      WIDGET_CONTROL, State.wFramesMarkerButton, SENSITIVE  = 1
      WIDGET_CONTROL, State.wFramesMarkerButton, SET_BUTTON = 1
      RedrawWindow, State, CurFrame
   ENDIF ELSE BEGIN
      !VAR.CurrFiles.Marker_Loaded = 0
      State.showmarker = 0
      !VAR.CurrFiles.MarkerFile = ''
   ENDELSE

   !SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO

END  ;  Op_ReadMarkerPixels

;***************************************************************************
PRO Op_EnableDrawObjects, State, Event
;***************************************************************************
; Set the object drawing flag to start accepting points. User must specify
; the type of objects to draw at this time. Turn off the BRF display window
; which normally appears when clicking the image.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (!SAV.Digitize.AER_TYPE EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN RETURN

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

;---------------------------------------------------------------------------
; Ask user which kind of object to draw. Also get parameters for plotting
; data for debug and type of wind in height calculation.
;---------------------------------------------------------------------------

CallPlumeGUI, State, Retval

IF (Retval EQ 0) THEN RETURN

;---------------------------------------------------------------------------
; Turn the button on that displays the appropriate data.
;---------------------------------------------------------------------------

WIDGET_CONTROL, State.wFramesLinesButton, SENSITIVE = 1
WIDGET_CONTROL, State.wFramesLinesButton, SET_BUTTON = 1

WIDGET_CONTROL, State.wFramesBrfButton, SET_BUTTON=0
WIDGET_CONTROL, State.wBrfParamsButton, SENSITIVE=0

State.showobjects = 1
State.showBRFwndw = 0

;----------------------------------------------------------------------------
; Set the cursor to the fine crosshair for digitizing.
;----------------------------------------------------------------------------

SafeWSET, state.draw_win, didit
SetMINXCursor, 1, rtrnval

END  ;  Op_EnableDrawObjects

;***************************************************************************
PRO Op_EnableRedigObjects, State, Event
;***************************************************************************
; Allow the user to select a region name from the list of files in default
; directory and current orbit. Show an image of the region for verification,
; and if it's OK, use this name for the current digitizing of that object
; instead of assigning a new one.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON save_main_parms, MainOption

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it and return so the user can try again.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Construct the directory name in which to look for regions.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
IF (STRLEN(orbit_str) EQ 4) THEN orbit_str = '0' + orbit_str
IF (STRLEN(orbit_str) EQ 5) THEN orbit_str = '0' + orbit_str

IF (!SAV.Digitize.PlumeOutDir EQ '') THEN $
   !SAV.Digitize.PlumeOutDir = !SAV.WorkingDir

IF (STRMID(!SAV.Digitize.PlumeOutDir, 0, /REVERSE_OFFSET) NE $
    !KON.Misc.Slash) THEN $
   !SAV.Digitize.PlumeOutDir += !KON.Misc.Slash
tempdir = !SAV.Digitize.PlumeOutDir + orbit_str + !KON.Misc.Slash
IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
   mssg = ['Directory: ' + tempdir, ' does not exist. Try something else.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Get a list of the region names in this directory.
;---------------------------------------------------------------------------

filelist = FILE_SEARCH(tempdir + 'Plumes_*.txt')

nregion = N_ELEMENTS(filelist)
IF (nregion EQ 0) THEN BEGIN
   mssg = ['There are no region data files in directory: ', $
           tempdir + '. Try something else.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

FOR irgn=0,nregion-1 DO BEGIN
   ipos1 = STRPOS(filelist[irgn], !KON.Misc.Slash, /REVERSE_SEARCH)
   filelist[irgn] = STRMID(filelist[irgn], ipos1+1)
   ipos1 = STRPOS(filelist[irgn], '_O')
   ipos2 = STRPOS(filelist[irgn], '.txt')
   filelist[irgn] = STRMID(filelist[irgn], ipos1+1, ipos2-ipos1-1)
ENDFOR

;---------------------------------------------------------------------------
; Show the available region names on disk to the user and let him select one.
;---------------------------------------------------------------------------

re_select:
RegionDiskList_gui, State.wTopWorkBase, filelist, rgnIndex

IF (rgnIndex LT 0) THEN RETURN

temp_rgn_name = filelist[rgnIndex]
image_file = tempdir + temp_rgn_name + '_PlumeContours_An.jpg'

IF (~ FILE_TEST(image_file)) THEN BEGIN
   mssg = 'File ' + image_file + ' is not present on disk.'
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Determine what band option was used to digitize this region. If red (R) or
; blue (B), then get the name of the other band. If it exists for the same
; block and type, then the BOTH_BAND option was used to generate it.
;---------------------------------------------------------------------------

toks = STRSPLIT(temp_rgn_name, '.-', /EXTRACT, COUNT=numtok)
end_tok = toks[numtok-1]
type1 = STRMID(end_tok, 3, 1)

other_band = ''
band_type = !KON.BandObjTyp.RB_BAND
IF (type1 EQ 'R') THEN BEGIN
   other_band = 'B'
   band_type = !KON.BandObjTyp.RED_BAND
ENDIF
IF (type1 EQ 'B') THEN BEGIN
   other_band = 'R'
   band_type = !KON.BandObjTyp.BLUE_BAND
ENDIF
STRPUT, end_tok, other_band, 3
toks[numtok-1] = end_tok

IF (other_band) THEN BEGIN
   temp_rgn_name2 = ''
   FOR itok=0,numtok-1 DO BEGIN
      temp_rgn_name2 += toks[itok]
      IF (itok NE numtok-1) THEN temp_rgn_name2 += '-'
   ENDFOR
   image_file2 = tempdir + temp_rgn_name2 + '_PlumeContours_An.jpg'
   IF (FILE_TEST(image_file2)) THEN BEGIN
      band_type = !KON.BandObjTyp.BOTH_BAND
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Display the region image to the user and let him confirm.
;---------------------------------------------------------------------------

READ_JPEG, image_file, rgn_image, $
           TRUE=((CoordStruct.(whichorbit).num_band EQ 1) ? 0 : 1)
isize = SIZE(rgn_image)
WINDOW, XSIZE=isize[2]*1.01, YSIZE=isize[3]*1.01, /FREE
TV, rgn_image, TRUE=((CoordStruct.(whichorbit).num_band EQ 1) ? 0 : 1)

mssg = [temp_rgn_name + ' is the region you selected.', $
        'Are you sure you want to re-digitize it?']
rval = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)

pwin = !D.WINDOW
SafeWDELETE, pwin, didit

IF (STRUPCASE(rval) EQ 'YES') THEN BEGIN
   !SAV.Digitize.OverrideRgnName = temp_rgn_name
   !SAV.Digitize.RETRIEVE_BAND_TYPE = band_type
   Op_EnableDrawObjects, State, Event
ENDIF ELSE BEGIN
   mssg = 'Do you want to try again?'
   rval = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)
   IF (STRUPCASE(rval) EQ 'YES') THEN GOTO, re_select
ENDELSE

filelist = 0

;----------------------------------------------------------------------------
; Set the cursor to the fine crosshair for digitizing.
;----------------------------------------------------------------------------

SafeWSET, state.draw_win, didit
SetMINXCursor, 1, rtrnval

END  ;  Op_EnableRedigObjects

;***************************************************************************
PRO Op_DeleteDrawObjects, State, Event
;***************************************************************************
;
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the object drawing flag to enable deleting objects. Any kinds of
; digitized objects can be deleted. Turn off the BRF display window which
; normally appears when clicking the image.
;---------------------------------------------------------------------------

!SAV.Digitize.DIG_STATE = [!KON.AerObjTyp.AER_DELETE_OBJ, $
                           !KON.GeomObjTyp.GEOM_NULL_OBJ, $
                           !KON.WindObjTyp.WIND_NULL_OBJ, $
                           !KON.BandObjTyp.NULL_BAND]
State.showobjects = 2
State.showBRFwndw = 0

WIDGET_CONTROL, State.wFramesBrfButton, SET_BUTTON=0
WIDGET_CONTROL, State.wBrfParamsButton, SENSITIVE=0

;---------------------------------------------------------------------------
; Turn the button on that displays the drawn lines.
;---------------------------------------------------------------------------

WIDGET_CONTROL, State.wFramesLinesButton, SET_BUTTON = 1

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it and return so the user can try again.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN  ResetFrame, State, 5, 1

;----------------------------------------------------------------------------
; Set the cursor to the large X cursor for deleting.
;----------------------------------------------------------------------------

SafeWSET, state.draw_win, didit
SetMINXCursor, 2, rtrnval

END  ;  Op_DeleteDrawObjects

;***************************************************************************
PRO Op_DisableEditObjects, State, Event
;***************************************************************************
; Turn off the ability to digitize.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the object drawing flag to stop accepting points or deleting
; objects. Turn the BRF display window back on.
;---------------------------------------------------------------------------

!SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO

!VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_DO_NOT

;---------------------------------------------------------------------------
; Make sure the displayed image is the An camera for orbit 1. If not, then
; correct it and return so the user can try again.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

;----------------------------------------------------------------------------
; Set the cursor to the standard cursor for neutral state.
;----------------------------------------------------------------------------

SafeWSET, state.draw_win, didit
SetMINXCursor, 0, rtrnval

END  ;  Op_DisableEditObjects
