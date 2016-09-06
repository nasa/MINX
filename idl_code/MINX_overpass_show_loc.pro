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
PRO GetOrbitNumber_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON getorbit, orbit_ndx, Status

;-------------------------------------------------------------------------------
; Get the data structure stored in the widget.
;-------------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=orbit_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   Status = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;-------------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;-------------------------------------------------------------------------------

CASE 1 OF

   event.id EQ orbit_struct.orb_list : BEGIN
   END

   event.id EQ orbit_struct.ok_button : BEGIN
      orbit_ndx = WIDGET_INFO(orbit_struct.orb_list, /LIST_SELECT)
      IF (orbit_ndx LT 0) THEN BEGIN
        msg = ['Please select an orbit or click "Cancel" to exit.']
        rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
      ENDIF ELSE BEGIN
        WIDGET_CONTROL, event.top, /DESTROY
        Status = 0
        RETURN
      ENDELSE
   END

   event.id EQ orbit_struct.cancel_button : BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      Status = -1
      RETURN
   END

ENDCASE

;-------------------------------------------------------------------------------
; Save data structure back into the widget.
;-------------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=orbit_struct, /NO_COPY

END ; GetOrbitNumber_eh

;**************************************************************************
PRO GetOrbitNumber_gui, OrbitsAvail, OrbitSel, Retval
;**************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON getorbit, orbit_ndx, Status

;-------------------------------------------------------------------------------
; Define controls in widget.
;-------------------------------------------------------------------------------

base00 = WIDGET_BASE(/COLUMN, TITLE='Select Orbit Number', /TLB_KILL_REQUEST_EVENTS)
basecntl = WIDGET_BASE(base00, /COLUMN, /ALIGN_CENTER)
labelrem = WIDGET_LABEL(basecntl, VALUE='Orbit List')
orb_list = WIDGET_LIST(basecntl, YSIZE=10, XSIZE=6, /FRAME)

baseoch = WIDGET_BASE(base00, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(basecntl, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(basecntl, VALUE='Cancel')

;-------------------------------------------------------------------------------
; Load the orbit list into the listbox.
;-------------------------------------------------------------------------------

WIDGET_CONTROL, orb_list, SET_VALUE=OrbitsAvail

;-------------------------------------------------------------------------------
; Define structure to be stored in widget.
;-------------------------------------------------------------------------------

orbit_struct = { orb_list      : orb_list, $
                 ok_button     : ok_button, $
                 cancel_button : cancel_button }

;-------------------------------------------------------------------------------
; Store the structure in widget and realize it.
;-------------------------------------------------------------------------------

WIDGET_CONTROL, base00, SET_UVALUE=orbit_struct, XOFFSET=500, YOFFSET=250, /NO_COPY
WIDGET_CONTROL, base00, /REALIZE
XMANAGER, 'GetOrbitNumber_gui', base00, EVENT_HANDLER='GetOrbitNumber_eh'

Retval = Status

OrbitSel = -1L
IF (Status EQ 0) THEN OrbitSel = OrbitsAvail[orbit_ndx]

END ; GetOrbitNumber_gui

;******************************************************************************
PRO GetOrbitsFromFile, FileName, LatBeg, LatEnd, LonBeg, LonEnd, BlkBeg, $
                       BlkEnd, LocDateB, LocTimeB, LocDateE, LocTimeE, $
                       PathList, OrbitList, NumOrbits, RegNums, Retval
;******************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

lun_num = 16

RegNums   = [0]
PathList  = ['']
OrbitList = ['']
LatBeg    = [0.0D]
LatEnd    = [0.0D]
LonBeg    = [0.0D]
LonEnd    = [0.0D]
BlkBeg    = [0]
BlkEnd    = [0]
LocDateB  = ['']
LocTimeB  = ['']
LocDateE  = ['']
LocTimeE  = ['']

date_vals = STRARR(2)
lat_vals  = FLTARR(2) + !KON.Misc.BADVALUE_REAL
lon_vals  = FLTARR(2) + !KON.Misc.BADVALUE_REAL
blk_ary   = INTARR(4)

NumOrbits = 0
buff = ''

;---------------------------------------------------------------------
; Open the file.
;---------------------------------------------------------------------

OPENR, lun_num, FileName, /GET_LUN
BegMidEnd = 0

;---------------------------------------------------------------------
; Read the data line-by-line. Save every line in data tables.
; Also save the user's input lat/lon coordinates.
;---------------------------------------------------------------------

WHILE (~EOF(lun_num)) DO BEGIN

   ReadNextAsciiLine, lun_num, BegMidEnd, buff

   IF (STRMID(buff,0,13) EQ 'Search number') THEN BEGIN
      toks = STRSPLIT(buff, ': ', /EXTRACT, COUNT=numtoks)
      reg_num = FIX(toks[2])
      CONTINUE
   ENDIF

   IF (STRMID(buff,0,16) EQ 'Input date range') THEN BEGIN
      toks = STRSPLIT(buff, ': ', /EXTRACT, COUNT=numtoks)
      IF (numtoks EQ 5) THEN BEGIN
         date_vals[0] = toks[3]
         date_vals[1] = toks[5]
      ENDIF
      CONTINUE
   ENDIF

   IF (STRMID(buff,0,15) EQ 'Input lat range') THEN BEGIN
      toks = STRSPLIT(buff, ': ', /EXTRACT, COUNT=numtoks)
      IF (numtoks EQ 6) THEN BEGIN
         lat_vals[0] = DOUBLE(toks[3])
         lat_vals[1] = DOUBLE(toks[5])
      ENDIF
      CONTINUE
   ENDIF

   IF (STRMID(buff,0,15) EQ 'Input lon range') THEN BEGIN
      toks = STRSPLIT(buff, ': ', /EXTRACT, COUNT=numtoks)
      IF (numtoks EQ 6) THEN BEGIN
         lon_vals[0] = DOUBLE(toks[3])
         lon_vals[1] = DOUBLE(toks[5])
      ENDIF
      CONTINUE
   ENDIF

   IF (STRMID(buff,16,4) EQ '  20' AND STRMID(buff,22,1) EQ '-' AND $
       STRMID(buff,25,1) EQ '-') THEN BEGIN
      IF (STRMID(buff,0,5) NE '     ') THEN BEGIN
         RegNums   = [RegNums,reg_num]
         LatBeg    = [LatBeg,lat_vals[0]]
         LatEnd    = [LatEnd,lat_vals[1]]
         LonBeg    = [LonBeg,lon_vals[0]]
         LonEnd    = [LonEnd,lon_vals[1]]
         OrbitList = [OrbitList,STRMID(buff,0,5)]
         PathList  = [PathList, STRMID(buff,8,3)]
         BlkBeg    = [BlkBeg,   STRMID(buff,13,3)]
         LocDateB  = [LocDateB, STRMID(buff,42,10)]
         LocTimeB  = [LocTimeB, STRMID(buff,55,8)]
         NumOrbits += 1
      ENDIF ELSE BEGIN
         BlkEnd    = [BlkEnd,   STRMID(buff,13,3)]
         LocDateE  = [LocDateE, STRMID(buff,42,10)]
         LocTimeE  = [LocTimeE, STRMID(buff,55,8)]
      ENDELSE
      CONTINUE
   ENDIF
ENDWHILE

;---------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------

FREE_LUN, lun_num

IF (NumOrbits GT 0) THEN BEGIN
   RegNums   = RegNums  [1:NumOrbits]
   LatBeg    = LatBeg   [1:NumOrbits]
   LatEnd    = LatEnd   [1:NumOrbits]
   LonBeg    = LonBeg   [1:NumOrbits]
   LonEnd    = LonEnd   [1:NumOrbits]
   PathList  = PathList [1:NumOrbits]
   OrbitList = OrbitList[1:NumOrbits]
   BlkBeg    = BlkBeg   [1:NumOrbits]
   BlkEnd    = BlkEnd   [1:NumOrbits]
   LocDateB  = LocDateB [1:NumOrbits]
   LocTimeB  = LocTimeB [1:NumOrbits]
   LocDateE  = LocDateE [1:NumOrbits]
   LocTimeE  = LocTimeE [1:NumOrbits]
   Retval    = 0
ENDIF ELSE BEGIN
   OrbitList = 0
   Retval = -1
ENDELSE

END  ;  GetOrbitsFromFile

;*************************************************************************
FUNCTION ImageToPngFile2, object_num, window_id, orbit_num, output_png_dir, $
                          xstart1, ystart1, xsize1, ysize1, xstart2, $
                          ystart2, xsize2, ysize2, text1, text2
;************************************************************************
; This function saves an image to file in PNG format.
;-------------------------------------------------------------------------

   COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;----------------------------------------------------------------------
   ; Get the name of the file in which to save the image.
   ;----------------------------------------------------------------------

   png_file_name = output_png_dir + !KON.Misc.Slash + 'O' + $
                   STRTRIM(STRING(orbit_num),2) + '_Region' + $
                   STRTRIM(STRING(object_num),2) + '.png'

   IF (png_file_name EQ '') THEN RETURN, 0
   slen = STRLEN(png_file_name)
   ext = STRMID(png_file_name, slen - 4, 4)
   IF (ext NE '.png') THEN png_file_name = png_file_name + '.png'

   ;----------------------------------------------------------------------
   ; Acquire the image and invert it. Make the button as inconspicuous
   ; as possible before saving, and restore it after
   ;----------------------------------------------------------------------

   IF (text1 EQ 'Continue' OR text2 EQ 'Continue') THEN BEGIN
      DrawButton, xstart1, ystart1, xsize1, ysize1, !KON.Colors.lt_blue, $
                  !KON.Colors.lt_blue, text1
      current_wndw = !D.WINDOW
      SafeWSET, window_id, didit
      SafeWSHOW, window_id, 1, 0, didit
   ENDIF

   IF (text1 EQ 'Cancel' OR text2 EQ 'Cancel') THEN BEGIN
      DrawButton, xstart2, ystart2, xsize2, ysize2, !KON.Colors.lt_blue, $
                  !KON.Colors.lt_blue, text2
      current_wndw = !D.WINDOW
      SafeWSET, window_id, didit
      SafeWSHOW, window_id, 1, 0, didit
   ENDIF

   ;----------------------------------------------------------------------
   ; Write the image and palette to file.
   ;----------------------------------------------------------------------

   saveimage = TVRD(/ORDER, TRUE=1)

   WRITE_PNG, png_file_name, saveimage, /ORDER

   RETURN, 1

END  ;  ImageToPngFile2

;********************************************************************
PRO OverpassShowLocation, PathNum, OrbitNum, NumCorner, SDate, $
                          STime, ObjNum, BlockBeg, BlockEnd, LonList, $
                          LatList, map_wndw, OverpassOutputDir, $
                          ReturnValue
;********************************************************************
; This is the procedure for displaying results.
;-------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   old_font = GetFontInfo(0)

   ;-----------------------------------------------------------------------
   ; If lat range is zero and lon range is not or vice versa, adjust a
   ; parameter very slightly so both ranges are non-zero.
   ;-----------------------------------------------------------------------

   IF (LatList[1] EQ LatList[2] AND LonList[0] NE LatList[1]) THEN BEGIN
      LatList[2] += 0.01
      LatList[3] += 0.01
   ENDIF
   IF (LatList[1] NE LatList[2] AND LonList[0] EQ LonList[1]) THEN BEGIN
      LonList[1] += 0.01
      LonList[2] += 0.01
   ENDIF

   ;----------------------------------------------------------------------
   ; Set default parameters.
   ;----------------------------------------------------------------------

   ReturnValue = 0
   !Quiet = 1          ;  0 = don't suppress system messages (debug)
                       ;  1 = suppress system messages

   ;----------------------------------------------------------------------
   ; Get screen size and size of title characters.
   ;----------------------------------------------------------------------

   xscr_max = !KON.Misc.ScreenX
   yscr_max = !KON.Misc.ScreenY

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'18', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5

   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   ;----------------------------------------------------------------------
   ; set the size of the non-usable x and y components of a window
   ; on the left and top so detail windows position can be held stable
   ; this is a kludge and should be set to the size of the window
   ; border and title bar; different OSs behave differently!
   ;----------------------------------------------------------------------

   num_char_y_margin = 3
   win1_title_size = ychr_max * 2

   ;----------------------------------------------------------------------
   ; define map area size
   ;----------------------------------------------------------------------

   Yfactor = (yscr_max GT 1000) ? 0.9 : 0.95
   y_map_size = yscr_max * Yfactor - win1_title_size

   ;----------------------------------------------------------------------
   ; set array and window constants
   ;----------------------------------------------------------------------

   map_wndw = 4    ; window for displaying map and data

   ;----------------------------------------------------------------------
   ; create some arrays needed for data processing
   ;----------------------------------------------------------------------

   lat = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)
   lon = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)

   lat2 = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)
   lon2 = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)

   ;----------------------------------------------------------------------
   ; Setup parameters to draw block outlines
   ;----------------------------------------------------------------------

   Actual_First_Block = 1   ; default first block number
   nNumChoices        = 1
   nValidBlkBeg       = 0
   nValidBlkEnd       = !KON.Instr.NUM_BLOCKS - 1
   aryBlockNumbers    = LINDGEN(!KON.Instr.NUM_BLOCKS) + 1
   coastline_resolution = 0
   nValidBlkBeg = BlockBeg - 1
   nValidBlkEnd = BlockEnd - 1
   Actual_First_Block = BlockBeg
   NumBlks = ABS(BlockEnd - BlockBeg) + 1
   IF (nValidBlkEnd - nValidBlkBeg LT 130) THEN coastline_resolution = 1
   IF (nValidBlkEnd - nValidBlkBeg LT  10) THEN coastline_resolution = 2

   ;----------------------------------------------------------------------
   ; Retrieve the AGP block corners data and get lat/long values for
   ; blocks for this path
   ;----------------------------------------------------------------------

   GetPathLatLonCoords,  PathNum, 1, 180, -180.0, LatLonCoords

   GetSwathLatLonCoords, PathNum, 1, 180, -180.0, LatLonCoords2

   FOR iblk = nValidBlkBeg, nValidBlkEnd  DO BEGIN
      FOR ipt1=0,3 DO BEGIN
         ipt2 = ipt1 * 2
         lon[iblk,ipt1]  = LatLonCoords[ipt2,iblk]
         lat[iblk,ipt1]  = LatLonCoords[ipt2+1,iblk]
         lon2[iblk,ipt1] = LatLonCoords2[ipt2,iblk]
         lat2[iblk,ipt1] = LatLonCoords2[ipt2+1,iblk]
      ENDFOR
   ENDFOR

   LatLonCoords  = 0
   LatLonCoords2 = 0

   ;----------------------------------------------------------------------
   ; create any arrays needed below before entering interactive loop
   ;----------------------------------------------------------------------

   lonlatdat   = FLTARR(2,4)
   lonlatdat2  = FLTARR(2,4)

   ;----------------------------------------------------------------------
   ; determine longitude plotting parameters - test for the case
   ; where the longitude crosses the dateline and Greenwich
   ;----------------------------------------------------------------------

   blk1 = nValidBlkBeg
   blk2 = nValidBlkEnd
   center_block = (blk1 + blk2) / 2

   neglonmin = lon[center_block,0] < lon[center_block,2]
   neglonmax = lon[center_block,0] > lon[center_block,2]

   IF ((neglonmin LT 0) AND (neglonmax GT 0) AND  $
        (ABS(lon[center_block,0]) GT 90.)) THEN BEGIN
      neglonmin = 360 + neglonmin
      lon_center = (neglonmin + neglonmax) / 2.
      IF lon_center GT 180 THEN lon_center = lon_center - 360
   ENDIF ELSE BEGIN
      lon_center = (neglonmin + neglonmax) / 2.
   ENDELSE

   ;----------------------------------------------------------------------
   ; Find the center latitude and compute the scale factor;
   ; scale of map as in 1 : 100,000  (map_scale = 100,000).
   ; Add some blocks as a fudge factor so the scale will never get
   ; too small. If zoomed in tight, make the map wider and the
   ; scale smaller
   ;----------------------------------------------------------------------

   blk_fudge = 10
   blk_diff = nValidBlkEnd - nValidBlkBeg + blk_fudge
   map_scale = 9.1E10 / y_map_size * blk_diff / !KON.Instr.NUM_BLOCKS

   x_map_size = y_map_size / 2 + 25

   IF (blk_diff LT 3+blk_fudge) THEN BEGIN
      map_scale /= 2.0
      x_map_size *= 2.0
   ENDIF

   lat_max = MAX(lat[blk1:blk2,*], MIN=lat_min, /NAN)
   lat_center = (lat_max + lat_min) / 2.
   IF (lat[blk1,0] LT lat[blk1+1,0])   THEN lat_center = lat_center + 7
   IF (lat[blk2,0] GT lat[blk2-1>1,0]) THEN lat_center = lat_center - 7

   ;----------------------------------------------------------------------
   ; create window and set proper size
   ;----------------------------------------------------------------------

   bckgrnd_save = !P.BACKGROUND
   !P.BACKGROUND = !KON.Colors.blue1
   WINDOW, map_wndw, XPOS=xscr_max-x_map_size-100, XSIZE=x_map_size, $
           YSIZE=y_map_size, RETAIN=2, TITLE='MINX V' + $
           !KON.Misc.MINX_VERSION_NUM + ' : MISR Overpass Finder Tool'

   xview_max = !D.X_VSIZE
   yview_max = !D.Y_VSIZE

   ;----------------------------------------------------------------------
   ; set map projection, scale etc.
   ;----------------------------------------------------------------------

   MAP_SET, /LAMBERT, lat_center, lon_center, 0, /ISOTROPIC, /NOBORDER, $
            /HORIZON, COLOR=!KON.Colors.gray1, E_HORIZON={FILL:1, $
            COLOR:!KON.Colors.lt_blue}, SCALE=map_scale, XMARGIN=1.5, $
            YMARGIN=[1.0,num_char_y_margin]

   ;----------------------------------------------------------------------
   ; Set font for block numbers and fine detail.
   ;----------------------------------------------------------------------

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.2
   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   ;----------------------------------------------------------------------
   ; determine location and frequency of lat/long annotations & lines
   ;----------------------------------------------------------------------

   xygrid = FLTARR(2,3)
   llgrid = FLTARR(3,3)
   xygrid[0,0] = x_map_size - xchr_max * 4
   xygrid[0,1] = x_map_size - xchr_max * 4
   xygrid[0,2] = x_map_size - xchr_max * 4
   xygrid[1,0] = y_map_size / 5.
   xygrid[1,1] = y_map_size / 2.
   xygrid[1,2] = y_map_size / 5. * 4.
   llgrid = CONVERT_COORD(xygrid, /TO_DATA, /DEVICE)
   minll = MIN(llgrid[0,*], MAX=maxll)
   xygrid = 0
   llgrid = 0

   IF (minll * maxll LT 0.0 AND maxll GT 90.0) THEN BEGIN  ; dateline
      latlabel = maxll - 12.
   ENDIF ELSE BEGIN
      latlabel = minll - 3.
   ENDELSE
   lonlabel = (lat_min - 3.) > (-63)
   IF (blk_diff LT 3+blk_fudge) THEN lonlabel = lat_min
   lldelta = 15.

   ;----------------------------------------------------------------------
   ; adjust the lat-long grid to be appropriate to the scale
   ;----------------------------------------------------------------------

   IF (blk_diff LT 120+blk_fudge) THEN lldelta = 10.
   IF (blk_diff LT  50+blk_fudge) THEN lldelta = 5.
   IF (blk_diff LT  20+blk_fudge) THEN lldelta = 2.
   IF (blk_diff LT   5+blk_fudge) THEN lldelta = 1.
   IF (blk_diff LT   2+blk_fudge) THEN lldelta = 0.5

   ;----------------------------------------------------------------------
   ; draw continent fill, lat/long grid lines, coast and country lines
   ;----------------------------------------------------------------------

   MAP_CONTINENTS, /FILL_CONTINENTS, COLOR=!KON.Colors.lt_green

   MAP_GRID, LATS=0, LATDEL=lldelta, LATLAB=latlabel, $
             LONS=0, LONDEL=lldelta, LONLAB=lonlabel, LABEL=1, $
             ORIENTATION=0, CHARSIZE=1, COLOR=!KON.Colors.red

   IF (coastline_resolution EQ 0) THEN BEGIN
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue2, MLINETHICK=1
   ENDIF
   IF (coastline_resolution EQ 1) THEN BEGIN
      MAP_CONTINENTS, /RIVERS,   COLOR=!KON.Colors.blue1, MLINETHICK=1
      MAP_CONTINENTS, /USA,      COLOR=!KON.Colors.brown, MLINETHICK=1
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue1 ,MLINETHICK=2
   ENDIF
   IF (coastline_resolution EQ 2) THEN BEGIN
      MAP_CONTINENTS, /RIVERS,   COLOR=!KON.Colors.blue1, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /USA,      COLOR=!KON.Colors.brown, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue1 ,MLINETHICK=2, /HIRES
   ENDIF

   numdraw1 = 0
   numdraw2 = 0

   xysave0 = FLTARR(2,2)
   xysave1 = FLTARR(2)
   xysave1[0] = 0.0
   xysave1[1] = 0.0

   xysave02 = FLTARR(2,2)
   xysave12 = FLTARR(2)
   xysave12[0] = 0.0
   xysave12[1] = 0.0

   FOR iblk = nValidBlkBeg, nValidBlkEnd  DO BEGIN

      llblk = iblk + Actual_First_Block - nValidBlkBeg - 1

      ;-------------------------------------------------------------------
      ; set up current block's coordinates and convert to x/y coords
      ;-------------------------------------------------------------------

      lonlatdat[1,*] = lat[llblk,*]
      lonlatdat[0,*] = lon[llblk,*]

      xysave0[*,0] = xysave1
      xycoords = CONVERT_COORD(lonlatdat, /DATA, /TO_DEVICE)
      xysave1[0:1] = xycoords[0:1,1]

      IF (iblk EQ nValidBlkBeg) THEN BEGIN
         xysave0[0:1,0] = xycoords[0:1,0]
         xysave0[0:1,1] = xycoords[0:1,3]
      ENDIF ELSE BEGIN
         xysave0[0:1,1] = xycoords[0:1,0]
      ENDELSE

      ;-------------------------------------------------------------------
      ; set up the current swath's coordinates and convert to x/y coords
      ;-------------------------------------------------------------------

      lonlatdat2[1,*] = lat2[llblk,*]
      lonlatdat2[0,*] = lon2[llblk,*]

      xysave02[*,0] = xysave12
      xycoords2 = CONVERT_COORD(lonlatdat2, /DATA, /TO_DEVICE)
      xysave12[0:1] = xycoords2[0:1,1]

      IF (iblk EQ nValidBlkBeg) THEN BEGIN
         xysave02[0:1,0] = xycoords2[0:1,0]
         xysave02[0:1,1] = xycoords2[0:1,3]
      ENDIF ELSE BEGIN
         xysave02[0:1,1] = xycoords2[0:1,0]
      ENDELSE

      ;-------------------------------------------------------------------
      ; draw line around block and around swath
      ;-------------------------------------------------------------------

      PLOTS, xycoords, /DEVICE, LINESTYLE=2, COLOR=!KON.Colors.black, $
             THICK=2.0
      PLOTS, xysave0,  /DEVICE, LINESTYLE=2, COLOR=!KON.Colors.black, $
             THICK=2.0

      PLOTS, xycoords2, /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
             THICK=2.0
      PLOTS, xysave02,  /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
             THICK=2.0

      ;-------------------------------------------------------------------
      ; annotate first block and every 5th block if displaying 130
      ; -180 blocks, every 3rd block if displaying 80 - 129 blocks,
      ; every 2nd block if displaying 30 - 79 blocks, and every
      ; block if displaying fewer than 30 blocks
      ;-------------------------------------------------------------------

      iiblk = FIX(aryBlockNumbers[iblk] + Actual_First_Block - $
                  nValidBlkBeg - 1)
      IF (NumBlks GE 130) THEN BEGIN
         IF ((iiblk MOD 5) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE IF (NumBlks GE 80) THEN BEGIN
         IF ((iiblk MOD 3) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE IF (NumBlks GE 30) THEN BEGIN
         IF ((iiblk MOD 2) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE BEGIN
         blknum_string = STRTRIM(STRING(iiblk),2)
         numdraw1 = numdraw1 + 1
         IF (numdraw1 EQ 1)  $
             THEN blk_string = 'Block # ' + blknum_string $
             ELSE blk_string = blknum_string
         XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                 ychr_max) / 2, blk_string, /DEVICE, $
                 COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
      ENDELSE

   ENDFOR

   lat = 0
   lon = 0
   lat2 = 0
   lon2 = 0
   lonlatdat = 0
   lonlatdat2 = 0
   xysave0 = 0
   xysave1 = 0
   xysave02 = 0
   xysave12 = 0
   xycoords = 0
   aryBlockNumbers = 0

   ;----------------------------------------------------------------------
   ; draw buttons in lower left to let user close window
   ;----------------------------------------------------------------------

   xsize_cntnu  = xchr_max * 10
   ysize_cntnu  = 17
   xstart_cntnu = (x_map_size - xsize_cntnu - 50) / 2
   ystart_cntnu = ychr_max * 5

   DrawButton, xstart_cntnu, ystart_cntnu, xsize_cntnu, ysize_cntnu, $
               !KON.Colors.green, !KON.Colors.black, 'Continue'

   xsize_quit  = xchr_max * 8
   ysize_quit  = 17
   xstart_quit = xstart_cntnu + xsize_cntnu + 30
   ystart_quit = ychr_max * 5

   DrawButton, xstart_quit, ystart_quit, xsize_quit, ysize_quit, $
               !KON.Colors.green, !KON.Colors.black, 'Cancel'

   IF (LonList[1] LT LonList[0] AND $
       (LonList[0] GE 170. OR LonList[1] LE -170.)) THEN BEGIN
      ndxs = WHERE(LonList GE 170., numndxs)
      IF (numndxs GT 0) THEN LonList[ndxs] -= 360.
   ENDIF

   ;----------------------------------------------------------------------
   ; Draw a polygon around the geographic region the user specified.
   ;----------------------------------------------------------------------

   IF (NumCorner EQ 4) THEN BEGIN

      lat_list_temp = LatList
      lon_list_temp = LonList

      ;-------------------------------------------------------------------
      ; Add extra points on the rectangle edges if they are long enough
      ; that there will be lat/lon curvature. Handle long. over dateline.
      ;-------------------------------------------------------------------

      minlat = MIN(lat_list_temp)
      maxlat = MAX(lat_list_temp)
      minlon = MIN(lon_list_temp)
      maxlon = MAX(lon_list_temp)

      numlat = (CEIL(maxlat - minlat) + 1) > 2
      numlon = (CEIL(maxlon - minlon) + 1) > 2
      new_lons_lats = FLTARR(2, 2*(numlat+numlon)-3)

      ibeg = 0
      FOR ilon=ibeg,ibeg+numlon-1 DO BEGIN
         irev = numlon + numlat + ilon - ibeg - 2
         new_lons_lats[0, ilon] = minlon + ilon
         new_lons_lats[1, ilon] = lat_list_temp[0]
         new_lons_lats[0, irev] = maxlon - ilon
         new_lons_lats[1, irev] = lat_list_temp[2]
      ENDFOR
      ibeg = numlon - 1
      FOR ilat=ibeg,ibeg+numlat-1 DO BEGIN
         irev = 2 * numlon + numlat + ilat - ibeg - 3
         new_lons_lats[0, ilat] = lon_list_temp[1]
         new_lons_lats[1, ilat] = minlat + ilat - ibeg
         new_lons_lats[0, irev] = lon_list_temp[3]
         new_lons_lats[1, irev] = maxlat - ilat + ibeg
      ENDFOR

      new_lons_lats[0,0] = minlon
      new_lons_lats[1,0] = minlat
      new_lons_lats[0,numlon-1] = maxlon
      new_lons_lats[1,numlon-1] = minlat
      new_lons_lats[0,numlon+numlat-2] = maxlon
      new_lons_lats[1,numlon+numlat-2] = maxlat
      new_lons_lats[0,2*numlon+numlat-3] = minlon
      new_lons_lats[1,2*numlon+numlat-3] = maxlat
      new_lons_lats[0,2*(numlon+numlat)-4] = minlon
      new_lons_lats[1,2*(numlon+numlat)-4] = minlat

      xyrect = CONVERT_COORD(new_lons_lats, /DATA, /TO_DEVICE)

      PLOTS, xyrect, /DEVICE, COLOR=!KON.Colors.purple, THICK=3
      lon_list_temp = 0
      lat_list_temp = 0
      new_lons_lats = 0
      xyrect = 0
   ENDIF ELSE BEGIN
      PLOTS, [LonList[0] - 0.2, LonList[0] + 0.2], $
             [LatList[0] - 0.2, LatList[0] + 0.2], $
             /DATA, COLOR=!KON.Colors.purple, THICK=3
      PLOTS, [LonList[0] - 0.2, LonList[0] + 0.2], $
             [LatList[0] + 0.2, LatList[0] - 0.2], $
             /DATA, COLOR=!KON.Colors.purple, THICK=3
   ENDELSE

   ;----------------------------------------------------------------------
   ; set up the rectangle for reporting the lat/long coords of cursor
   ;----------------------------------------------------------------------

   lonlat_rect = INTARR(2,5)
   lonlat_rect[0,0] = (x_map_size - xchr_max * 30) / 2
   lonlat_rect[1,0] =  y_map_size - ychr_max * 8
   lonlat_rect[0,1] = lonlat_rect[0,0]
   lonlat_rect[1,1] = lonlat_rect[1,0] + ychr_max * 2
   lonlat_rect[0,2] = lonlat_rect[0,0] + xchr_max * 26
   lonlat_rect[1,2] = lonlat_rect[1,1]
   lonlat_rect[0,3] = lonlat_rect[0,2]
   lonlat_rect[1,3] = lonlat_rect[1,0]
   lonlat_rect[0,4] = lonlat_rect[0,0]
   lonlat_rect[1,4] = lonlat_rect[1,0]

   ;----------------------------------------------------------------------
   ; if we're only drawing block outlines, print title
   ;----------------------------------------------------------------------

   orbit_per_day = !KON.Instr.NUM_PATHS / 15.9993 ; 98.88 min/orbit => 14.5631 orbits/day

   baseorbit = 1007.281   ; first orbit at noon (Julian) on basedate
   basedate  = '02/25/00'    ; day after final orbit reached (at noon)
   mmddyr    = Is_Date(basedate)
   base_julian = JULDAY(mmddyr[0], mmddyr[1], mmddyr[2])
   orbitjulian = base_julian + (OrbitNum - baseorbit) / orbit_per_day
   CALDAT, orbitjulian, month, day, year, hour, min, sec
   orbitdate = STRING(STRTRIM(month,2)) + '/' +  $
               STRING(STRTRIM(day,2))   + '/' +  $
               STRING(STRTRIM(year,2))

   font_size = (yscr_max GT 1000) ? '18' : '14'
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:font_size, Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5

   XYOUTS, 2, yview_max - 2.5 * ychr_max, $
              '  Orbit: ' + STRTRIM(STRING(OrbitNum),2) + $
              ',  Path: ' + STRTRIM(STRING(PathNum),2) + $
              ',  Date: ' + SDate + ',  Local Time: ' + STime, $
              /DEVICE, CHARSIZE=1.2, COLOR=!KON.Colors.black

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.2
   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   ;----------------------------------------------------------------------
   ; loop waiting for mouse input
   ;----------------------------------------------------------------------

   SafeWSET, map_wndw, didit
   !MOUSE.button = 1
   xpoint_old = 0
   ypoint_old = 0
   
   WHILE (!MOUSE.button NE 4) DO BEGIN
   
      CURSOR, xpoint, ypoint, /DEVICE, /DOWN

      ;---------------------------------------------------------------------
      ; If user clicks red delete window button in upper-left, exit safely
      ; here. The window has already been destroyed by IDL. The only bad
      ; side-effect is if the user clicks the identical point in the window
      ; twice in succession, this code assumes the window is to be closed.
      ;---------------------------------------------------------------------

      IF (xpoint EQ xpoint_old AND ypoint EQ ypoint_old) THEN BREAK
      IF (!D.WINDOW EQ -1) THEN BREAK
      ;-------------------------------------------------------------------
      ; Test if the mouse click is inside the continue button. If so, save
      ; the image to a file.
      ;-------------------------------------------------------------------

      IF ((xpoint GE xstart_cntnu AND $
           xpoint LE xstart_cntnu+xsize_cntnu AND $
           ypoint LE ystart_cntnu AND $
           ypoint GE ystart_cntnu-ysize_cntnu)) THEN BEGIN

           retval = ImageToPngFile2 (ObjNum, map_wndw, OrbitNum, $
                                      OverpassOutputDir, $
                                      xstart_cntnu, ystart_cntnu, $
                                      xsize_cntnu, ysize_cntnu, $
                                      xstart_quit, ystart_quit, $
                                      xsize_quit, ysize_quit, $
                                      'Continue', 'Cancel')
         SetFontInfo, old_font

         ReturnValue = 1
         lonlat_rect = 0
         xycoords = 0
         !MOUSE.X = 0
         !MOUSE.Y = 0
         RETURN
      ENDIF

      ;-------------------------------------------------------------------
      ; Fabricate left cursor button click position in lat/long or get it
      ; from mouse and display lat/long coords.
      ;-------------------------------------------------------------------

      xycoords = CONVERT_COORD(xpoint, ypoint, /DEVICE, /TO_DATA)
      Xlon = xycoords[0,0]
      Ylat = xycoords[1,0]
      PLOTS, [xpoint-3,xpoint+3], [ypoint,ypoint], /DEVICE, $
             COLOR=!KON.Colors.black
      PLOTS, [xpoint,xpoint], [ypoint-3,ypoint+3], /DEVICE, $
             COLOR=!KON.Colors.black
      POLYFILL, lonlat_rect, COLOR=!KON.Colors.yellow, /DEVICE
      PLOTS, lonlat_rect, /DEVICE, COLOR=!KON.Colors.black, THICK=1
      llstring = 'Lat  = ' + STRTRIM(STRING(Ylat,FORMAT='(f7.3)'),2) + $
              ',  Long = ' + STRTRIM(STRING(Xlon,FORMAT='(f8.3)'),2)

      XYOUTS, lonlat_rect[0,0]+4, lonlat_rect[1,0]+4, llstring, $
              /DEVICE, COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=0.0

      ;-------------------------------------------------------------------
      ; Test if the mouse click is inside the cancel button. If so, then
      ; don't show or save any more images.
      ;-------------------------------------------------------------------

      IF ((xpoint GE xstart_quit AND $
           xpoint LE xstart_quit+xsize_quit AND $
           ypoint LE ystart_quit AND $
           ypoint GE ystart_quit-ysize_quit)) THEN BEGIN

         SetFontInfo, old_font
         ReturnValue = -1
         RETURN
      ENDIF
      
      xpoint_old = xpoint
      ypoint_old = ypoint

   ENDWHILE

   SetFontInfo, old_font

   lonlat_rect = 0
   xycoords = 0
   !MOUSE.X = 0
   !MOUSE.Y = 0

END  ;  OverpassShowLocation
