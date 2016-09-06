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
PRO SetBrfplotValues_eh, event
;***************************************************************************
; Event handler for user interface for setting configurable 
; parameter values for the BRF plot widget.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   !VAR.BRFparms.DO_BRF_PLOT = 0
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;------------------------------------------------------------------------
; Branch to the correct widget.
;------------------------------------------------------------------------

CASE 1 OF

   ; OK, accept new values.
   event.id EQ widget_struct.ok_button : BEGIN

      WIDGET_CONTROL, widget_struct.wBandGroup, GET_VALUE=vals
      !VAR.BRFparms.PLOT_BAND_NUM = vals

thisisbad:
      !VAR.BRFparms.PLOT_SINGLE_PIXEL = $
                    widget_struct.config_temp.PLOT_SINGLE_PIXEL
      !VAR.BRFparms.PLOT_PIXEL_MEAN = $
                    widget_struct.config_temp.PLOT_PIXEL_MEAN
      !VAR.BRFparms.SHOW_GEOM_BRF = $
                    widget_struct.config_temp.SHOW_GEOM_BRF

      IF (~ !VAR.BRFparms.PLOT_SINGLE_PIXEL AND $
          ~ !VAR.BRFparms.PLOT_PIXEL_MEAN) THEN $
         !VAR.BRFparms.PLOT_SINGLE_PIXEL = 1

      WIDGET_CONTROL, widget_struct.text3, GET_VALUE = value
      IF (value LT 3) THEN BEGIN
         mssg = ['The minimum allowed size of the square region for ' + $
                 'averaging', 'BRFs is 3 pixels. The size will be ' + $
                 'reset for you.']
         rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
         value = 3
      ENDIF
      IF (value GT 127) THEN BEGIN
         mssg = ['The maximum allowed size of the square region for ' + $
                 'averaging', 'BRFs is 127 pixels. The size will be ' + $
                 'reset for you.']
         rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
         value = 127
      ENDIF
      !VAR.BRFparms.PIXEL_MEAN_SIZE = value

      !VAR.BRFparms.USE_CONSTANT_MAX = $
                    widget_struct.config_temp.USE_CONSTANT_MAX

      IF (!VAR.BRFparms.USE_CONSTANT_MAX EQ -1) THEN BEGIN
         WIDGET_CONTROL, widget_struct.text4, GET_VALUE=value
         !VAR.BRFparms.BRF_MAX_VAL[1] = value
      ENDIF

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct.wBandGroup : BEGIN
      END

   ; Temporarily set new toggle values for checkbox buttons.
   ; Toggle the values and store in temp struct.

   event.id EQ widget_struct.button1 : $
      widget_struct.config_temp.PLOT_SINGLE_PIXEL = $
         ABS(!VAR.BRFparms.PLOT_SINGLE_PIXEL - 1)

   event.id EQ widget_struct.button2 : BEGIN
      widget_struct.config_temp.PLOT_PIXEL_MEAN = $
         ABS(!VAR.BRFparms.PLOT_PIXEL_MEAN - 1)
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, widget_struct.text3, SENSITIVE=1
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, widget_struct.text3, SENSITIVE=0
      ENDELSE
      END

   event.id EQ widget_struct.button2a : BEGIN
      val = WIDGET_INFO(widget_struct.button2a, /BUTTON_SET)
      widget_struct.config_temp.SHOW_GEOM_BRF = val
      END

   event.id EQ widget_struct.button4b : BEGIN
      widget_struct.config_temp.USE_CONSTANT_MAX = 0
      WIDGET_CONTROL, widget_struct.text4, SENSITIVE=0
      END
   event.id EQ widget_struct.button4c : BEGIN
      widget_struct.config_temp.USE_CONSTANT_MAX = -1
      WIDGET_CONTROL, widget_struct.text4, SENSITIVE=1
      END

   event.id EQ widget_struct.button5a : BEGIN
      !VAR.BRFparms.DO_BRF_PLOT = 1
      GOTO, thisisbad
      END
   event.id EQ widget_struct.button5b : BEGIN
      !VAR.BRFparms.DO_BRF_PLOT = 2
      GOTO, thisisbad
      END
   event.id EQ widget_struct.button5c : BEGIN
      !VAR.BRFparms.DO_BRF_PLOT = 3
      GOTO, thisisbad
      END
   event.id EQ widget_struct.button5d : BEGIN
      !VAR.BRFparms.DO_BRF_PLOT = 4
      GOTO, thisisbad
      END
   event.id EQ widget_struct.button5e : BEGIN
      !VAR.BRFparms.DO_BRF_PLOT = 5
      GOTO, thisisbad
      END

   ; Cancel.

   event.id EQ widget_struct.cancel_button : BEGIN

      ; Set temp back to current values.

      widget_struct.config_temp.PLOT_SINGLE_PIXEL = $
                                !VAR.BRFparms.PLOT_SINGLE_PIXEL
      widget_struct.config_temp.PLOT_PIXEL_MEAN   = $
                                !VAR.BRFparms.PLOT_PIXEL_MEAN
      widget_struct.config_temp.USE_CONSTANT_MAX  = $
                                !VAR.BRFparms.USE_CONSTANT_MAX
      widget_struct.config_temp.SHOW_GEOM_BRF     = $
                                !VAR.BRFparms.SHOW_GEOM_BRF

      ; Set all widgets back to current values, in case any 
      ; temporary changes had been made.

      WIDGET_CONTROL, widget_struct.button1, $
         SET_BUTTON = !VAR.BRFparms.PLOT_SINGLE_PIXEL
      WIDGET_CONTROL, widget_struct.button2, $
         SET_BUTTON = !VAR.BRFparms.PLOT_PIXEL_MEAN
      WIDGET_CONTROL, widget_struct.button2a, $
         SET_BUTTON = !VAR.BRFparms.SHOW_GEOM_BRF
      WIDGET_CONTROL, widget_struct.text3, $
         SET_VALUE  = !VAR.BRFparms.PIXEL_MEAN_SIZE
      WIDGET_CONTROL, widget_struct.button4b, $
         SET_BUTTON = (!VAR.BRFparms.USE_CONSTANT_MAX EQ 0)
      WIDGET_CONTROL, widget_struct.button4c, $
         SET_BUTTON = (!VAR.BRFparms.USE_CONSTANT_MAX EQ -1)

      !VAR.BRFparms.DO_BRF_PLOT = 0

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN

   END

   ; Reset to Defaults.

   event.id EQ widget_struct.reset_button : BEGIN

      ; Set current to defaults.

      !VAR.BRFparms.PLOT_SINGLE_PIXEL = $
         widget_struct.config_defaults.PLOT_SINGLE_PIXEL
      !VAR.BRFparms.PLOT_PIXEL_MEAN = $
         widget_struct.config_defaults.PLOT_PIXEL_MEAN
      !VAR.BRFparms.SHOW_GEOM_BRF = $
         widget_struct.config_defaults.SHOW_GEOM_BRF
      !VAR.BRFparms.PIXEL_MEAN_SIZE = $
         widget_struct.config_defaults.PIXEL_MEAN_SIZE
      !VAR.BRFparms.USE_CONSTANT_MAX = 0
      widget_struct.config_temp.USE_CONSTANT_MAX = $
         !VAR.BRFparms.USE_CONSTANT_MAX
      !VAR.BRFparms.BRF_MAX_VAL[1] = !VAR.BRFparms.BRF_MAX_VAL[0]

      ; Set all widgets to current (which is now defaults).

      WIDGET_CONTROL, widget_struct.button1, $
         SET_BUTTON = !VAR.BRFparms.PLOT_SINGLE_PIXEL
      WIDGET_CONTROL, widget_struct.button2, $
         SET_BUTTON = !VAR.BRFparms.PLOT_PIXEL_MEAN
      WIDGET_CONTROL, widget_struct.button2a, $
         SET_BUTTON = !VAR.BRFparms.SHOW_GEOM_BRF
      WIDGET_CONTROL, widget_struct.text3, $
         SET_VALUE = !VAR.BRFparms.PIXEL_MEAN_SIZE
      WIDGET_CONTROL, widget_struct.button4b, $
         SET_BUTTON = (!VAR.BRFparms.USE_CONSTANT_MAX EQ 0)
      WIDGET_CONTROL, widget_struct.button4c, $
         SET_BUTTON = (!VAR.BRFparms.USE_CONSTANT_MAX EQ -1)
      WIDGET_CONTROL, widget_struct.text4, $
         SET_VALUE = !VAR.BRFparms.BRF_MAX_VAL[0]
      WIDGET_CONTROL, widget_struct.text4, $
         SENSITIVE = (!VAR.BRFparms.USE_CONSTANT_MAX EQ -1)

      !VAR.BRFparms.DO_BRF_PLOT = 0

   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY

END ; SetBrfplotValues_eh

;***************************************************************************
PRO SetBrfplotValues_gui, State
;***************************************************************************
; User interface for setting configurable parameter values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct

; default struct.
config_defaults = { $
   PLOT_BAND_NUM     : !VAR.BRFparms.PLOT_BAND_NUM, $
   PLOT_SINGLE_PIXEL : !VAR.BRFparms.PLOT_SINGLE_PIXEL, $
   PLOT_PIXEL_MEAN   : !VAR.BRFparms.PLOT_PIXEL_MEAN, $
   PIXEL_MEAN_SIZE   : !VAR.BRFparms.PIXEL_MEAN_SIZE, $
   USE_CONSTANT_MAX  : !VAR.BRFparms.USE_CONSTANT_MAX, $
   SHOW_GEOM_BRF     : !VAR.BRFparms.SHOW_GEOM_BRF }

; temp struct for temporarily storing toggle values.
config_temp = config_defaults

base0 = WIDGET_BASE( /COLUMN, TITLE='BRF Plot Parameters', $ ; /MODAL, $
                    GROUP_LEADER=State.wAnimateBase, /FLOATING, $
                    /TLB_KILL_REQUEST_EVENTS )

labelx1 = WIDGET_LABEL( base0, VALUE='' )

label_band = WIDGET_LABEL( base0, VALUE='Select Band to Display' )
values = ['Blue ', 'Green', 'Red  ', 'NIR  ']  
wBandBase = WIDGET_BASE(base0)  
wBandGroup = CW_BGROUP(wBandbase, values, /EXCLUSIVE, ROW=1, /FRAME)  

labelx2 = WIDGET_LABEL( base0, VALUE='' )

label0 = WIDGET_LABEL( base0, $
                VALUE='Enter BRF Analysis Parameters' )

wBtnBase = WIDGET_BASE(base0, /COLUMN, /FRAME)

base1 = WIDGET_BASE( wBtnBase, /COLUMN, /NONEXCLUSIVE )
button1 = WIDGET_BUTTON( base1, VALUE='Plot BRFs for center pixel' )
WIDGET_CONTROL, button1, SET_BUTTON=!VAR.BRFparms.PLOT_SINGLE_PIXEL

base2 = WIDGET_BASE( wBtnBase, /COLUMN, /NONEXCLUSIVE )
button2 = WIDGET_BUTTON( base2, VALUE='Plot mean BRFs for square region' )
WIDGET_CONTROL, button2, SET_BUTTON=!VAR.BRFparms.PLOT_PIXEL_MEAN

base2a = WIDGET_BASE( wBtnBase, /COLUMN, /NONEXCLUSIVE )
button2a = WIDGET_BUTTON( base2a, $
                          VALUE='Show table of geometry and radiances' )
WIDGET_CONTROL, button2a, SET_BUTTON=!VAR.BRFparms.SHOW_GEOM_BRF

base3 = WIDGET_BASE( wBtnBase, /COLUMN )
text3 = CW_FIELD( base3, VALUE=!VAR.BRFparms.PIXEL_MEAN_SIZE, XSIZE=4, $
   /INTEGER, TITLE='Pixels on edge of square region' )
is_sens = 0
IF (!VAR.BRFparms.PLOT_PIXEL_MEAN) THEN is_sens = 1
WIDGET_CONTROL, text3, SENSITIVE=is_sens

wBtnBase = WIDGET_BASE(base0, /COLUMN)

base4a = WIDGET_BASE( wBtnBase, /COLUMN )
base4b = WIDGET_BASE( base4a, /COLUMN, /EXCLUSIVE )
button4b = WIDGET_BUTTON( base4b, $
   VALUE='Scale BRFs to max for the pixel' )
button4c = WIDGET_BUTTON( base4b, $
   VALUE='Scale BRFs to specific value' )
WIDGET_CONTROL, button4b, SET_BUTTON=(!VAR.BRFparms.USE_CONSTANT_MAX EQ 0)
WIDGET_CONTROL, button4c, SET_BUTTON=(!VAR.BRFparms.USE_CONSTANT_MAX EQ -1)
text4 = CW_FIELD(base4a, XSIZE=6, VALUE=!VAR.BRFparms.BRF_MAX_VAL[1], $
   /FLOAT, TITLE='Y-Axis Max:')
WIDGET_CONTROL, text4, SENSITIVE=(!VAR.BRFparms.USE_CONSTANT_MAX EQ -1)

is_sens = 1
IF (!VAR.BRFparms.BRF_INIT_PLOT) THEN is_sens = 0

base5 = WIDGET_BASE( wBtnBase, /COLUMN )
button5a = 0
button5b = 0

button5c = WIDGET_BUTTON( base5, SENSITIVE=is_sens, $
   VALUE='Save BRF plot as JPEG file' )
button5d = WIDGET_BUTTON( base5, SENSITIVE=is_sens, $
   VALUE='Save BRF plot as TIFF file' )
button5e = WIDGET_BUTTON( base5, SENSITIVE=is_sens, $
   VALUE='Save 9 zoomed, single-band BRF images as MP4' )

base6 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( base6, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( base6, VALUE='Cancel' )
reset_button = WIDGET_BUTTON( base6, VALUE='Reset to Defaults' )

widget_struct = { $
   wBandGroup      : wBandGroup, $
   button1         : button1, $
   button2         : button2, $
   button2a        : button2a, $
   text3           : text3, $
   button4b        : button4b, $
   button4c        : button4c, $
   text4           : text4, $
   button5a        : button5a, $
   button5b        : button5b, $
   button5c        : button5c, $
   button5d        : button5d, $
   button5e        : button5e, $
   ok_button       : ok_button, $
   cancel_button   : cancel_button, $
   reset_button    : reset_button, $
   config_temp     : config_temp, $
   config_defaults : config_defaults }

WIDGET_CONTROL, wBandGroup, SET_VALUE=!VAR.BRFparms.PLOT_BAND_NUM
IF (CoordStruct.(0).num_band EQ 1) THEN BEGIN
   WIDGET_CONTROL, wBandGroup, SENSITIVE=0
ENDIF
WIDGET_CONTROL, base0, SET_UVALUE=widget_struct, /NO_COPY
WIDGET_CONTROL, base0, /REALIZE
XMANAGER, 'SetBrfplotValues_gui', base0, EVENT_HANDLER='SetBrfplotValues_eh'

END ; SetBrfplotValues_gui

;***************************************************************************
PRO SetBrfplotValues, init, State
;***************************************************************************
; Set configurable parameter values for BRF plot.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   !VAR.BRFparms.DO_BRF_PLOT = 0

   SetBrfplotValues_gui, State

END ; SetBrfplotValues

;***************************************************************************
FUNCTION camera_ticks, axis, index, value
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

    ; Helper function for the Brf plot command, to label the x-axis
    ; with camera names.
    RETURN, (STRUPCASE(!KON.Instr.CAM_NAMES))[index]

END ; camera_ticks

;***************************************************************************
PRO SaveZoomWindowsMP4, beg_x, end_x, beg_y1, end_y1, image_size_x, $
                        image_size_y, MaxPixelUsePcnt, filename, BandNdx
;***************************************************************************
; Routine creates an MP4 file containing the 9 zoomed images of the
; selected 64x64 pixel region.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize the MP4 output.
;---------------------------------------------------------------------------
 
VideoMP4_Open, filename, image_size_x, image_size_y, $
               !SAV.Digitize.FRAME_PER_SEC, $
               !SAV.Digitize.BIT_RATE, VidObj, vidStream
 
;---------------------------------------------------------------------------
; Loop over the cameras and construct the single-band image at zoomed scale
; and write it to an MP4 file.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   orig_array = REFORM(GetRawImage(beg_x, end_x, beg_y1, end_y1, BandNdx, $
                                   icam, !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP))

   num_pix = N_ELEMENTS(orig_array)
   sort_ndxs = SORT(orig_array)
   max_ndx = sort_ndxs[FLOOR(num_pix * MaxPixelUsePcnt)]

   ndx = WHERE((orig_array GT 0) AND $
               (orig_array LT orig_array[max_ndx]) AND $
               (orig_array NE !VALUES.F_INFINITY) AND $
               (orig_array NE !VALUES.F_NAN))
   IF ndx[0] GE 0 THEN BEGIN
      min = MIN(orig_array[ndx])
      max = MAX(orig_array[ndx])
   ENDIF ELSE BEGIN
      min = MIN(orig_array)
      max = MAX(orig_array)
   ENDELSE

   ndx = 0

   zoom_array = REBIN(BYTSCL(orig_array, MIN=min, MAX=max), $
                      image_size_x, image_size_y, /SAMPLE)
   orig_array = 0

   zoom_array1 = BYTARR(3, image_size_x, image_size_y)
   zoom_array1[0,*,*] = REVERSE(zoom_array, 2)
   zoom_array1[1,*,*] = zoom_array1[0,*,*]
   zoom_array1[2,*,*] = zoom_array1[0,*,*]
   VideoMP4_Put, VidObj, vidStream, zoom_array1

   zoom_array  = 0
   zoom_array1 = 0

ENDFOR

VideoMP4_Close, VidObj

rtrn_val = ChmodCatchError(filename, '666'O)
   
END  ;  SaveZoomWindowsMP4

;***************************************************************************
PRO DrawZoomWindow, State, Event, POSTSCRIPT = postscript, $
                    JPEG = jpeg, TIFF = tiff, MP4 = mp4, $
                    PS_IN_COLOR = ps_in_color
;***************************************************************************
; Routine draws a zoomed window centered at the mouse click location.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct
COMMON chan_minmax, ChanMinMax

   ;---------------------------------------------------------------------
   ; Terminate subroutine if mouse wanders outside the window.
   ;---------------------------------------------------------------------

   IF (Event.x LT 0) OR (Event.y LT 0) OR (Event.x GT State.sizex-1) OR $
       (Event.y GT State.sizey-1) THEN GOTO, cleanup
   
    ;---------------------------------------------------------------------
    ; Do some initializing.
    ;---------------------------------------------------------------------
    
   old_font = GetFontInfo(0)
   
   IF ~ KEYWORD_SET(ps_in_color) THEN ps_in_color = 0
   IF ~ KEYWORD_SET(postscript)  THEN postscript = 0
   IF ~ KEYWORD_SET(jpeg) THEN jpeg=0
   IF ~ KEYWORD_SET(tiff) THEN tiff=0
   IF ~ KEYWORD_SET(mp4) THEN mp4=0
   IF (TOTAL([postscript,jpeg,tiff,mp4]) GT 1) THEN BEGIN
      postscript = 0
      jpeg = 1		; Conflict default.
      tiff = 0
      mp4 = 0
   ENDIF

   delta_pix = FLOOR(!VAR.BRFparms.PIXEL_MEAN_SIZE / 2.0)

   WIDGET_CONTROL, State.wBrfParamsButton, SENSITIVE = 1

   zoom_fctr = 8
   data_size_x = 65
   data_size_y = 65
   image_size_x  = FLOAT(zoom_fctr * data_size_x)  ; x size of upper and lower wndws
   image_size_y2 = FLOAT(zoom_fctr * data_size_y)  ; y size of lower image window
   image_size_y1 = zoom_fctr * 50.                 ; y size of upper plot window

   MaxPixelUsePcnt = 0.95  ; include this fraction of darkest pixs in scene
                           ; when scaling brightness for zoom window - this
                           ; elininates anomalies that darken whole picture

   ;------------------------------------------------------------------------
   ; Set up the non-postscript display area.
   ;------------------------------------------------------------------------
                           
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
   char_spac  = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.2 : 1.5
   char_spacX = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.3
   char_spacY = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.3
   
   IF (~ postscript) THEN BEGIN
   
      curwin = !D.WINDOW
      SafeWSET, ZOOM_WNDW, didit
   
      IF (didit LE 0) THEN BEGIN
         xpos = !KON.Misc.ScreenX - image_size_x - 90
         WINDOW, XSIZE=image_size_x, YSIZE=image_size_y1 + image_size_y2, $
                 XPOS=xpos, YPOS=30, RETAIN=2, /FREE, TITLE='TOA BRF vs Camera'
         ZOOM_WNDW = !D.WINDOW
      ENDIF
   
      image_size_x  = !D.X_SIZE * 1.0
      image_size_y1 = !D.Y_SIZE * image_size_y1 / image_size_y2
   
      colors = [255, 65280, 16711680, 16711935, 0, 16776960]
      SafeWSET, curwin, didit
   
   ;------------------------------------------------------------------------
   ; Set up the postscript file if desired. Colors: black, red, green, blue,
   ; white. Postscript output has been DISABLED.
   ;------------------------------------------------------------------------
      
   ENDIF ELSE BEGIN

      SET_PLOT, 'PS'

      ok_to_write_file = 0
      filename = DIALOG_PICKFILE(FILE='brf_plot.ps', FILTER=['*.ps'], $
                                 PATH=!SAV.WorkingDir, /WRITE)

      IF (filename NE '') THEN BEGIN
         IF ((FILE_SEARCH( filename ))[0]) THEN BEGIN
            overwrite = DIALOG_MESSAGE('File exists, overwrite?', $
                                       /QUESTION, /CENTER)
            IF STRUPCASE(overwrite) EQ 'YES' THEN ok_to_write_file = 1
         ENDIF ELSE BEGIN
            ok_to_write_file = 1
         ENDELSE
      ENDIF

      IF (~ ok_to_write_file) THEN GOTO, cleanup

      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH) + 1
      tempdir = STRMID(filename, 0, npos)
      tempfile = STRMID(filename, npos)

      IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
         rtrn_val = MakeDirectory(tempdir)
         rtrn_val = ChmodCatchError(tempdir, '777'O)
      ENDIF

      colors = [0,1,2,3,4,5,6]

      IF (ps_in_color EQ 1) THEN BEGIN
         DEVICE, FILENAME = filename, BITS_PER_PIXEL = 8, /ENCAPSULATED, $
                 /COLOR, XSIZE=16, YSIZE=24
         TVLCT, [0,255,0,0,255], [0,0,255,0,255], [0,0,0,255,255]
      ENDIF ELSE BEGIN
         DEVICE, FILENAME = filename, BITS_PER_PIXEL = 8, /ENCAPSULATED, $
                 XSIZE=16, YSIZE=24
         TVLCT, [0,0,0,0,255], [0,0,0,0,255], [0,0,0,0,255]
      ENDELSE

   ENDELSE

   ;------------------------------------------------------------------------
   ; Set band for display in expanded image and for writing BRFs on x-axis
   ; of plot window.
   ;------------------------------------------------------------------------

   band_ndxs = [2,1,0,3]
   band_names = ['Red', 'Green', 'Blue', 'NIR']

   bandndx = band_ndxs[!VAR.BRFparms.PLOT_BAND_NUM]
   band_name = band_names[bandndx]

   ;------------------------------------------------------------------------
   ; Read image data from a rectangle centered at the mouse click. Make sure
   ; zoom window is fully inside draw window.
   ;------------------------------------------------------------------------

   beg_x0 = Event.x - data_size_x / 2.0
   beg_y0 = Event.y - data_size_y / 2.0

   IF (beg_x0 LT 0) THEN beg_x0 = 0.0
   IF (beg_y0 LT 0) THEN beg_y0 = 0.0
   end_x0 = beg_x0 + data_size_x - 1
   end_y0 = beg_y0 + data_size_y - 1
   IF (end_x0 GT State.sizex-1) THEN BEGIN
      beg_x0 = State.sizex - (end_x0-beg_x0) - 1
      end_x0 = beg_x0 + image_size_x / zoom_fctr - 1
   ENDIF
   IF (end_y0 GT State.sizey-1) THEN BEGIN
      beg_y0 = State.sizey - (end_y0-beg_y0) - 1
      end_y0 = beg_y0 + image_size_y2 / zoom_fctr - 1
   END

   beg_x = CEIL(beg_x0)
   beg_y = CEIL(beg_y0)
   end_x = CEIL(end_x0)
   end_y = CEIL(end_y0)

   ;------------------------------------------------------------------------
   ; REFORM camera slice from stack, REVERSE the region top to  bottom, cut
   ; out the region of interest from the flipped array,  stretch the dynamic
   ; range of the data to byte ignoring zeroes,  and REBIN it up by the zoom
   ; factor using Nearest Neighbor  interpolation. Exclude the brightest
   ; MaxPixelUsePcnt pixels in the scaling to remove the brightest anomalies.
   ;------------------------------------------------------------------------

   zoom_array = 0

   beg_y1 = State.sizey - end_y - 1
   end_y1 = State.sizey - beg_y - 1

   IF (State.curframe GT 0) THEN BEGIN
      orig_array = REVERSE(REFORM(GetRawImage(beg_x, end_x, beg_y1, $
                                  end_y1, bandndx, State.curframe-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)), 2)

      num_pix = N_ELEMENTS(orig_array)
      sort_ndxs = SORT(orig_array)
      max_ndx = sort_ndxs[FLOOR(num_pix * MaxPixelUsePcnt)]

      ndx = WHERE((orig_array GT 0) AND $
                  (orig_array LT orig_array[max_ndx]) AND $
                  (orig_array NE !VALUES.F_INFINITY) AND $
                  (orig_array NE !VALUES.F_NAN))

      IF ndx[0] GE 0 THEN BEGIN
         min = MIN(orig_array[ndx])
         max = MAX(orig_array[ndx])
      ENDIF ELSE BEGIN
         min = MIN(orig_array)
         max = MAX(orig_array)
      ENDELSE

      ndx = 0

      ;---------------------------------------------------------------------
      ; If user has resized zoom window, then delete it to avoid crash.
      ;---------------------------------------------------------------------

      error_status = 0
      CATCH, error_status
      IF (error_status NE 0) THEN BEGIN
         CATCH, /CANCEL
         SafeWDELETE, ZOOM_WNDW, didit
         ZOOM_WNDW = -1
         GOTO, cleanup
      ENDIF

      zoom_array = REBIN(BYTSCL(orig_array, MIN=min, MAX=max), $
                                image_size_x, image_size_y2, /SAMPLE)
      CATCH, /CANCEL      
      orig_array = 0
      
   ENDIF ELSE BEGIN
      zoom_array = BYTARR(3,image_size_x, image_size_y2)
      zoom_array[0,*,*] = REBIN(BYTSCL(REVERSE(REFORM( $
                           WorkImage[beg_x:end_x,beg_y1:end_y1,0]),2), $
                           MIN=WORK_MinMax[0,0], MAX=WORK_MinMax[0,1]), $
                           image_size_x, image_size_y2, /SAMPLE)
      zoom_array[1,*,*] = REBIN(BYTSCL(REVERSE(REFORM( $
                           WorkImage[beg_x:end_x,beg_y1:end_y1,1]),2), $
                           MIN=WORK_MinMax[1,0], MAX=WORK_MinMax[1,1]), $
                           image_size_x, image_size_y2, /SAMPLE)
      zoom_array[2,*,*] = REBIN(BYTSCL(REVERSE(REFORM( $
                           WorkImage[beg_x:end_x,beg_y1:end_y1,2]),2), $
                           MIN=WORK_MinMax[2,0], MAX=WORK_MinMax[2,1]), $
                           image_size_x, image_size_y2, /SAMPLE)
   ENDELSE

   ;------------------------------------------------------------------------
   ; Inscribe crosshairs in the zoom_array at pixel that is plotted. Before
   ; doing so, make sure all the starting and ending positions are entirely
   ; in bounds.
   ;------------------------------------------------------------------------

   cross_x = ( Event.x - beg_x0 ) * zoom_fctr
   cross_y = ( Event.y - beg_y0 ) * zoom_fctr

   length = 5   ; Visible length
   extent = 10  ; Extent from center
   x_elements = image_size_x
   y_elements = image_size_y2

   IF (cross_x-extent        GE 0 AND $
       cross_x+extent-length GE 0 AND $
       cross_x               GE 0 AND $
       cross_x-extent+length LT x_elements AND $
       cross_x+extent        LT x_elements AND $
       cross_x               LT x_elements AND $
       cross_y               GE 0 AND $
       cross_y-extent        GE 0 AND $
       cross_y+extent-length GE 0 AND $
       cross_y               LT y_elements AND $
       cross_y-extent+length LT y_elements AND $
       cross_y+extent        LT y_elements) THEN BEGIN

     IF (State.curframe GT 0) THEN BEGIN
       zoom_array[cross_x-extent:cross_x-extent+length,cross_y] = 255 * $
         ( MEAN(zoom_array[cross_x-extent:cross_x-extent+length,cross_y]) LT 128 )
       zoom_array[cross_x+extent-length:cross_x+extent,cross_y] = 255 * $
         ( MEAN(zoom_array[cross_x+extent-length:cross_x+extent,cross_y]) LT 128 )
       zoom_array[cross_x,cross_y-extent:cross_y-extent+length] = 255 * $
         ( MEAN(zoom_array[cross_x,cross_y-extent:cross_y-extent+length]) LT 128 )
       zoom_array[cross_x,cross_y+extent-length:cross_y+extent] = 255 * $
         ( MEAN(zoom_array[cross_x,cross_y+extent-length:cross_y+extent]) LT 128 )
     ENDIF ELSE BEGIN
       zoom_array[*,cross_x-extent:cross_x-extent+length,cross_y] = 255 * $
         ( MEAN(zoom_array[*,cross_x-extent:cross_x-extent+length,cross_y]) LT 128 )
       zoom_array[*,cross_x+extent-length:cross_x+extent,cross_y] = 255 * $
         ( MEAN(zoom_array[*,cross_x+extent-length:cross_x+extent,cross_y]) LT 128 )
       zoom_array[*,cross_x,cross_y-extent:cross_y-extent+length] = 255 * $
         ( MEAN(zoom_array[*,cross_x,cross_y-extent:cross_y-extent+length]) LT 128 )
       zoom_array[*,cross_x,cross_y+extent-length:cross_y+extent] = 255 * $
         ( MEAN(zoom_array[*,cross_x,cross_y+extent-length:cross_y+extent]) LT 128 )
     ENDELSE

   ENDIF

   ;------------------------------------------------------------------------
   ; Inscribe the outline of the NxN patch in the zoom window if applicable.
   ; Make sure all the starting and ending positions are entirely in bounds
   ; first.
   ;------------------------------------------------------------------------

   IF (!VAR.BRFparms.PLOT_PIXEL_MEAN) THEN BEGIN

      ;---------------------------------------------------------------------
      ; If N in the NxN patch is 12, this is a flag that the patch is to be
      ; aligned on 1100 m. pixel boundaries for research retrieval patch
      ; selection.  Update later so this works for any patch size where N is
      ; divisible by 4, or add something to the dialog box.
      ;---------------------------------------------------------------------

      rect_x0 = ( Event.x - beg_x - delta_pix ) * zoom_fctr
      rect_x1 = ( Event.x - beg_x + delta_pix + 1) * zoom_fctr
      rect_y0 = ( Event.y - beg_y - delta_pix ) * zoom_fctr
      rect_y1 = ( Event.y - beg_y + delta_pix + 1) * zoom_fctr

      IF  (rect_x0 GE 0 AND rect_x1 LT x_elements AND $
           rect_y0 GE 0 AND rect_y1 LT y_elements) THEN BEGIN

         IF (State.curframe GT 0) THEN BEGIN
            zoom_array[rect_x0:rect_x1,rect_y0] = 255 * $
               ( MEAN(zoom_array[rect_x0:rect_x1,rect_y0]) LT 128 )
            zoom_array[rect_x0:rect_x1,rect_y1] = 255 * $
               ( MEAN(zoom_array[rect_x0:rect_x1,rect_y1]) LT 128 )
            zoom_array[rect_x0,rect_y0:rect_y1] = 255 * $
               ( MEAN(zoom_array[rect_x0,rect_y0:rect_y1]) LT 128 )
            zoom_array[rect_x1,rect_y0:rect_y1] = 255 * $
               ( MEAN(zoom_array[rect_x1,rect_y0:rect_y1]) LT 128 )
         ENDIF ELSE BEGIN
            zoom_array[*,rect_x0:rect_x1,rect_y0] = 255 * $
               ( MEAN(zoom_array[*,rect_x0:rect_x1,rect_y0]) LT 128 )
            zoom_array[*,rect_x0:rect_x1,rect_y1] = 255 * $
               ( MEAN(zoom_array[*,rect_x0:rect_x1,rect_y1]) LT 128 )
            zoom_array[*,rect_x0,rect_y0:rect_y1] = 255 * $
               ( MEAN(zoom_array[*,rect_x0,rect_y0:rect_y1]) LT 128 )
            zoom_array[*,rect_x1,rect_y0:rect_y1] = 255 * $
               ( MEAN(zoom_array[*,rect_x1,rect_y0:rect_y1]) LT 128 )
         ENDELSE

      ENDIF
   ENDIF

   IF (~ postscript) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Draw a rectangle over the original image corresponding to the zoom
      ; window.  First refresh the window to erase previous one.
      ;---------------------------------------------------------------------

      RedrawWindow, State, State.curframe

      xmax = State.sizex - 1
      ymax = State.sizey - 1

      SWIN = !D.WINDOW
      SafeWSET, state.draw_win, didit

      PLOT, [beg_x,beg_x,end_x-1,end_x-1,beg_x], $
            [beg_y,end_y-1,end_y-1,beg_y,beg_y], $
            POSITION=[0,0,xmax,ymax], XRANGE=[0,xmax], YRANGE=[0,ymax], $
            XSTYLE=5, YSTYLE=5, /NOERASE

     ;---------------------------------------------------------------------
      ; Plot data values in top half of window. Before doing WSET, make sure
      ; the window exists.
      ;---------------------------------------------------------------------

      SafeWSET, ZOOM_WNDW, didit
      IF (didit LE 0) THEN GOTO, cleanup

   ENDIF

   TV,BYTARR(image_size_x, image_size_y1) + 255, 0, image_size_y2
   
   background_color = !P.BACKGROUND
   !P.BACKGROUND = !D.N_COLORS-1

   brf_max1 = 0.0
   brf_max2 = 0.0

   plot_x = Event.x
   plot_y = State.sizey - Event.y - 1

   ncam = State.nframes - 1

   IF (!VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN BEGIN
      data2plot = FLTARR(CoordStruct.(0).num_band,ncam)
      FOR icam=0,ncam-1 DO BEGIN
        FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
          data2plot[iband,icam] = REFORM(GetRawImage(plot_x, plot_x, $
                                         plot_y, plot_y, iband, icam, $
                                         !KON.Instr.HI_RES_PIX_SIZE, $
                                         !KON.Misc.INTERP_TYPE_SAMP))
        ENDFOR
      ENDFOR
      missingndx = WHERE(data2plot EQ 0.0, count)
      IF count GT 0 THEN data2plot[missingndx] = !VALUES.F_NAN
      brf_max1 = MAX(data2plot, /NAN)
      num_elem = N_ELEMENTS(data2plot)
      missingndx = 0
   ENDIF

   IF (!VAR.BRFparms.PLOT_PIXEL_MEAN) THEN BEGIN
      xbeg = plot_x - delta_pix
      xend = plot_x + delta_pix
      IF (xbeg LT 0) THEN BEGIN
         xend = xend - xbeg
         xbeg = 0
      ENDIF
      IF (xend GE State.sizex) THEN BEGIN
         xbeg = xbeg - (xend - State.sizex + 1)
         xend = State.sizex - 1
      ENDIF

      ybeg = plot_y - delta_pix
      yend = plot_y + delta_pix
      IF (ybeg LT 0) THEN BEGIN
         yend = yend - ybeg
         ybeg = 0
      ENDIF
      IF (yend GE State.sizey) THEN BEGIN
         ybeg = (ybeg - (yend - State.sizey + 1)) > 0
         yend = State.sizey - 1
      ENDIF

      data2plot_temp = FLTARR(xend-xbeg+1,yend-ybeg+1,4,ncam)
      FOR icam=0,ncam-1 DO BEGIN
        FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
          data2plot_temp[*,*,iband,icam] = $
              REFORM(GetRawImage(xbeg, xend, ybeg, yend, iband, $
                                 icam, !KON.Instr.HI_RES_PIX_SIZE, $
                                 !KON.Misc.INTERP_TYPE_SAMP))
        ENDFOR
      ENDFOR

      data2plot_vals   = FLTARR(4,(xend-xbeg+1)*(yend-ybeg+1))
      data2plot_avg    = FLTARR(4,ncam)
      data2plot_stddev = FLTARR(4,ncam)

      FOR icam = 0, ncam-1 DO BEGIN
         FOR iband = 0,CoordStruct.(0).num_band-1 DO BEGIN
            npts_avg = 0
            FOR ix = 0, delta_pix*2-1 DO BEGIN
               FOR iy = 0, delta_pix*2-1 DO BEGIN
                  IF (data2plot_temp[ix,iy,iband,icam] GT 0.0) THEN BEGIN
                     data2plot_vals[iband,npts_avg] = $
                               data2plot_temp[ix,iy,iband,icam]
                     data2plot_avg[iband,icam] += $
                               data2plot_temp[ix,iy,iband,icam]
                     npts_avg += 1
                  ENDIF
               ENDFOR
            ENDFOR
            IF (npts_avg GT 1) THEN BEGIN
               data2plot_avg[iband,icam] = $
                         MEAN(data2plot_vals[iband,0:npts_avg-1])
               data2plot_stddev[iband,icam] = $
                         STDDEV(data2plot_vals[iband,0:npts_avg-1])
            ENDIF ELSE BEGIN
               data2plot_avg[iband,icam] = !VALUES.F_NAN
               data2plot_stddev[iband,icam] = !VALUES.F_NAN
            ENDELSE
         ENDFOR
      ENDFOR

      brf_max2 = MAX(data2plot_avg, /NAN)
      num_elem = N_ELEMENTS(data2plot_avg)
      
      data2plot_temp = 0              ; free the memory
      data2plot_vals = 0
   ENDIF

   CASE !VAR.BRFparms.USE_CONSTANT_MAX OF
;      1 : brf_max = ChanMinMax[1,0,4]  ; the GUI button for this is not used
       0 : brf_max = brf_max1 > brf_max2
      -1 : brf_max = !VAR.BRFparms.BRF_MAX_VAL[1]  ; [1] = user settable
   ENDCASE

   ;------------------------------------------------------------------------
   ; Set up plot.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesCoordSomText, GET_VALUE = coord_text

   IF (!VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN $
      temp_data = data2plot[bandndx,0:8]
   IF (!VAR.BRFparms.PLOT_PIXEL_MEAN)   THEN $
      temp_data = data2plot_avg[bandndx,0:8]

   ;------------------------------------------------------------------------
   ; Bail out if data is all NaN.
   ;------------------------------------------------------------------------

   region_save = !P.REGION
   IF (TOTAL(FINITE(temp_data),/DOUBLE) GT 0) THEN BEGIN

      IF (CoordStruct.(0).num_band EQ 1) THEN ytitle = 'Red band BRF'
      IF (CoordStruct.(0).num_band EQ 4) THEN ytitle = 'ToA BRF'
      
      xaxis_str = '    ' 
      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         cam_brf = STRING(FORMAT='(F5.3)', temp_data[icam])
         xaxis_str += cam_brf + '     ' + ((icam MOD 2) ? '' : ' ')
      ENDFOR
      x_margin = !VAR.BRFparms.PLOT_PIXEL_MEAN ? 6 : 3

      titlestr = 'Top of Atmosphere BRF for Block/Across/Along = ' + $
                  coord_text + '!C(' + band_name + ' Band BRF Posted ' + $
                 'below X-Axis & Displayed in Zoomed Image)'

      image_start = image_size_y1 / (image_size_y1 + $
                    image_size_y2)
      !P.REGION = [0.0, image_start, 1.0, 1.0]

      PLOT, [-1,-1], TITLE=titlestr, /DATA, $
            XMARGIN=[6,x_margin], YMARGIN=[4,4], XTITLE=xaxis_str, $
            XTICKLEN=0.025, XTICKS=8, XMINOR=1, XTICKFORMAT='camera_ticks',$
            YTITLE=ytitle, YTICKLEN=1, YGRIDSTYLE=1, YMINOR=1, $
            PSYM=-7, COLOR=colors[4], /NOERASE, XSTYLE=8, $
            XRANGE=[0,!KON.Instr.NCAM-1], YRANGE=[0,brf_max], $
            YSTYLE=(!VAR.BRFparms.USE_CONSTANT_MAX EQ -1) ? 1 : 0, $
            YTICK_GET=vals1, CHARSIZE=char_spac, XCHARSIZE=char_spacX, $
            YCHARSIZE=char_spacY, THICK=2, XTHICK=2, YTHICK=1

      IF (!VAR.BRFparms.USE_CONSTANT_MAX EQ -1) THEN vals1 = [vals1, brf_max]
      nv1 = N_ELEMENTS(vals1)
      OPLOT, [0,0], [vals1[0],vals1[nv1-1]], COLOR=colors[4], THICK=2
      OPLOT, [0.999*!KON.Instr.NCAM-1,0.999*!KON.Instr.NCAM-1], $
             [vals1[0],vals1[nv1-1]], COLOR=colors[4], THICK=2
      OPLOT, [0,!KON.Instr.NCAM-1], [0.998*vals1[nv1-1],0.998*vals1[nv1-1]], $
             COLOR=colors[4], THICK=2

      txtpos = 2

      ;---------------------------------------------------------------------
      ; Plot BRF for first orbit (whichorbit=0).
      ;---------------------------------------------------------------------

      whichorbit = 0

      IF (!VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN BEGIN
         txtpos += 1

         FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
            OPLOT, data2plot[iband,0:8], PSYM=-7, THICK=2, $
                   COLOR=colors[iband]
         ENDFOR

         ;------------------------------------------------------------------
         ; Plot BRF for first orbit (whichorbit=0).
         ;------------------------------------------------------------------

         IF (State.curframe GT 0) THEN BEGIN
            FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
               OPLOT, [State.curframe-1], $
                      [data2plot[iband,State.curframe-1]], $
                      PSYM=6, THICK=5, COLOR=colors[iband]
            ENDFOR
         ENDIF

         onum = '__ x __ : Orbit ' + $
                STRTRIM(CoordStruct.(whichorbit).OrbitNum,2) + $
                ' center pixel value'
         XYOUTS, 0.15, 0.985-0.018*txtpos, onum, ALIGN=0.0, /NORMAL, $
                 COLOR=colors[4]
      ENDIF

      ;---------------------------------------------------------------------
      ; Plot average BRF in area centered on current pixel for first orbit
      ; (whichorbit=0). Also plot percent variance = stddev**2 / mean * 100
      ;---------------------------------------------------------------------

      IF (!VAR.BRFparms.PLOT_PIXEL_MEAN) THEN BEGIN

         ncams = 9
         IF (num_elem EQ 18 OR num_elem EQ 72) THEN ncams = 18

         avg_max = MAX(data2plot_avg[bandndx,0:ncams-1])
         pcnt_var = (data2plot_stddev[bandndx,0:ncams-1] * $
                     data2plot_stddev[bandndx,0:ncams-1]) / $
                    (data2plot_avg[bandndx,0:ncams-1] * $
                     data2plot_avg[bandndx,0:ncams-1]) * 100.0
         pcnt_var_max = MAX(pcnt_var[0:ncams-1])

         nv1 = N_ELEMENTS(vals1)

         AXIS, 8.0, 0.0, YAXIS=1, YRANGE=[0.0,pcnt_var_max], $
               COLOR=colors[5], YTITLE='% Variance', YTICK_GET=vals2
         nv2 = N_ELEMENTS(vals2)

         pcnt_var_mult = vals1[nv1-1] / vals2[nv2-1]
         
         FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
            OPLOT, data2plot_avg[iband,0:8], PSYM=-7, THICK=2, $
                   COLOR=colors[iband], LINESTYLE=1
         ENDFOR

         OPLOT, pcnt_var_mult*pcnt_var[0:8], PSYM=-7, THICK=2, $
                COLOR=colors[5], LINESTYLE=1

         IF (State.curframe GT 0 AND $
             ~ !VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN BEGIN
            FOR iband=0,CoordStruct.(0).num_band-1 DO BEGIN
               OPLOT, [State.curframe-1], $
                      [data2plot_avg[iband,State.curframe-1]], $
                      PSYM=6, THICK=5, COLOR=colors[iband]
            ENDFOR
            OPLOT, [State.curframe-1], $
                   [pcnt_var_mult*pcnt_var[State.curframe-1]], $
                   PSYM=6, THICK=5, COLOR=colors[5]
         ENDIF

         sizetxt = STRTRIM(STRING(!VAR.BRFparms.PIXEL_MEAN_SIZE),2)
         sizetxt = ' ' + sizetxt + 'x' + sizetxt

         txtpos += 1
         onum = '.... x .... : Orbit ' + $
                STRTRIM(CoordStruct.(whichorbit).OrbitNum,2) + $
                sizetxt + ' pixel mean'
         XYOUTS, 0.15, 0.985-0.018*txtpos, onum, ALIGN=0.0, /NORMAL, $
                 COLOR=colors[4]

         txtpos += 1
         onum = '.... x .... : Orbit ' + $
                STRTRIM(CoordStruct.(whichorbit).OrbitNum,2) + $
                sizetxt + ' pixel % ' + band_name + '-band variance'
         XYOUTS, 0.15, 0.985-0.018*txtpos, onum, ALIGN=0.0, /NORMAL, $
                 COLOR=colors[5]
      ENDIF

      ;---------------------------------------------------------------------
      ; Plot BRF for second orbit (whichorbit=1), if present.
      ;---------------------------------------------------------------------

      IF (num_elem EQ 18 OR num_elem EQ 72) THEN BEGIN

         whichorbit = 1

         IF (!VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN BEGIN
            txtpos += 1

            FOR iband=0,CoordStruct.(whichorbit).num_band-1 DO BEGIN
               OPLOT, data2plot[iband, 9:17], PSYM=-1, SYMSIZE=1.2, $
                      THICK=1, COLOR=colors[iband]
            ENDFOR

            IF (State.curframe GT 0) THEN BEGIN
               FOR iband=0,CoordStruct.(whichorbit).num_band-1 DO BEGIN
                  OPLOT, [State.curframe-1-9], $
                         [data2plot[iband, State.curframe-1]], PSYM=6, $
                         THICK=5, COLOR=colors[iband]
               ENDFOR
            ENDIF

            onum = '__ + __ : Orbit ' + $
                   STRTRIM(CoordStruct.(whichorbit).OrbitNum,2) + $
                   ' center pixel value'
            XYOUTS, 0.15, 0.985-0.018*txtpos, onum, ALIGN=0.0, /NORMAL, $
                    COLOR=colors[4]
         ENDIF

         ;------------------------------------------------------------------
         ; Plot average BRF in area centered on current pixel for second
         ; orbit (whichorbit=1).
         ;------------------------------------------------------------------

         IF (!VAR.BRFparms.PLOT_PIXEL_MEAN) THEN BEGIN
            txtpos += 1
            FOR iband=0,CoordStruct.(whichorbit).num_band-1 DO BEGIN
               OPLOT, data2plot_avg[iband,9:17], PSYM=-1, SYMSIZE=1.2, $
                      THICK=1, COLOR=colors[iband], LINESTYLE=3
            ENDFOR
            IF (State.curframe GT 0 AND $
                ~ !VAR.BRFparms.PLOT_SINGLE_PIXEL) THEN BEGIN
               FOR iband=0,CoordStruct.(whichorbit).num_band-1 DO BEGIN
                  OPLOT, [State.curframe-1-9], $
                         [data2plot_avg[iband,State.curframe-1]], $
                         PSYM=6, THICK=5, COLOR=colors[iband]
               ENDFOR
            ENDIF
            sizetxt = STRTRIM(STRING(!VAR.BRFparms.PIXEL_MEAN_SIZE),2)
            sizetxt = ' ' + sizetxt + 'x' + sizetxt
            onum = '._. + ._. : Orbit ' + $
                   STRTRIM(CoordStruct.(whichorbit).OrbitNum,2) + $
                   sizetxt + ' pixel mean'
            XYOUTS, 0.15, 0.985-0.018*txtpos, onum, ALIGN=0.0, /NORMAL, $
                    COLOR=colors[4]
         ENDIF
      ENDIF

   ENDIF ELSE BEGIN

      XYOUTS, 0.5, 0.75, 'No Data At Cursor', ALIGN=0.5, /NORMAL, $
              CHARSIZE=3, COLOR=0
   ENDELSE

   !P.BACKGROUND = background_color
   multi_save = !P.MULTI
   !P.MULTI = 0

   ;------------------------------------------------------------------------
   ; Display image in bottom half of window.
   ;------------------------------------------------------------------------

   IF (State.curframe GT 0) THEN BEGIN
      TV, zoom_array
   ENDIF ELSE BEGIN
      TV, zoom_array, TRUE=1
   ENDELSE
   zoom_array = 0

   ;------------------------------------------------------------------------
   ; Save JPEG or TIFF if requested.
   ;------------------------------------------------------------------------

   IF (jpeg OR tiff OR mp4) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Test if the user has a license to use the MP4 feature. If not,
      ; generate 9 JPEGs instead.
      ;---------------------------------------------------------------------
       
      IF (mp4) THEN BEGIN
         has_license = TestIfMP4_License(1)
         IF (~ has_license) THEN BEGIN
            mssg = ['You do not have an IDL license enabling you', $
                    'to use the MP4 feature. Images will be', $
                    'saved in 9 JPEG files instead.']
            rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
            jpeg = 1
            mp4 = 0
         ENDIF
      ENDIF

      ok_to_write_file = 0
      IF (jpeg) THEN ext = 'jpg'
      IF (tiff) THEN ext = 'tif'
      IF (mp4)  THEN ext = 'mp4'

      CreateOrbitDirFile, '', 'BrfPlot_O', '', 1, ext, $
                          CoordStruct.(State.curframe GT 9).OrbitNum, 1, $
                          dirname, tempfile, Retval

      filename = DIALOG_PICKFILE(FILE=tempfile, FILTER=[ext], PATH=dirname, $
                                 /WRITE)
      IF (filename NE '') THEN BEGIN
         IF ((FILE_SEARCH( filename ))[0]) THEN BEGIN
            mssg = [filename,'','File exists, overwrite?']
            overwrite = DIALOG_MESSAGE(mssg, /QUESTION, /DEFAULT_NO, /CENTER)
            ok_to_write_file = 0
            IF STRUPCASE(overwrite) EQ 'YES' THEN ok_to_write_file = 1
         ENDIF ELSE BEGIN
            ok_to_write_file = 1
         ENDELSE
      ENDIF

      IF (ok_to_write_file EQ 1) THEN BEGIN

         npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH) + 1
         tempdir = STRMID(filename, 0, npos)
         tempfile = STRMID(filename, npos)

         IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
            rtrn_val = MakeDirectory(tempdir)
            rtrn_val = ChmodCatchError(tempdir, '777'O)
         ENDIF

         IF (tiff) THEN WRITE_TIFF, filename, $
                                    RED =   REVERSE(TVRD(CHANNEL=1),2), $
                                    GREEN = REVERSE(TVRD(CHANNEL=2),2), $
                                    BLUE =  REVERSE(TVRD(CHANNEL=3),2), $
                                    PLANARCONFIG=2

         IF (jpeg) THEN BEGIN
            img = BYTARR(3,N_ELEMENTS((TVRD())[*,0]), $
                           N_ELEMENTS((TVRD())[0,*]))
            img[0,*,*] = REVERSE(TVRD(CHANNEL=1),2)
            img[1,*,*] = REVERSE(TVRD(CHANNEL=2),2)
            img[2,*,*] = REVERSE(TVRD(CHANNEL=3),2)
            WRITE_JPEG, filename, img, TRUE=1, QUALITY=100, ORDER=1
            img = 0
         ENDIF

         IF (mp4) THEN $
            SaveZoomWindowsMP4, beg_x, end_x, beg_y1, end_y1, image_size_x, $
                                image_size_y2, MaxPixelUsePcnt, filename, $
                                bandndx
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; If requested, write BRF and geometry data for clicked point to a dialog
   ; window and optionally to a file. Only do this for the first of 2 orbits
   ; if 2 are loaded, because we can load only 1 GP_GMP file at a time.
   ;------------------------------------------------------------------------

   whichorbit = (State.curframe GT 9)

   IF (!VAR.BRFparms.SHOW_GEOM_BRF AND whichorbit EQ 0 AND $
       State.framedelta EQ 0) THEN BEGIN

      som_crds = STRSPLIT(coord_text, ' /', /EXTRACT)
      misr_coords = [FIX(som_crds[1]), FIX(som_crds[2]), FIX(som_crds[0])]
      wndw_coords = FLTARR(2,!KON.Instr.NCAM,1)
      MisrCrdToWndwCrd, State.curframe, misr_coords, new_coords, 0, Retval
      IF (Retval LT 0) THEN RETURN
      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         wndw_coords[*,icam,0] = new_coords
      ENDFOR
      new_coords = 0

      ; Construct latitude/longitude string.
      
      MisrCrdToSomCrd,   State.Curframe, misr_coords, som_coords, status
      SomCrdToLonlatCrd, State.Curframe, som_coords, ll_coords, status
      lon = ll_coords[0]
      lat = ll_coords[1]
      
      latlon_text = STRING(FORMAT='(F8.4)', lat) + ' / ' + $
         STRING(FORMAT='(F9.4)', lon)
         
      ; Get geometry data.

      LoadGMPData, State, retval
      IF (retval NE 0) THEN GOTO, cleanup
      GetGeomData, 3, State, lat, 1, wndw_coords, [0], CamAzimuthAng, $
                   CamZenithAng, CamScatterAng, CamGlitterAng, $
                   SunAzimuthAng, SunZenithAng, Retval
      wndw_coords = 0

      ; Get radiance data.

      LoadBrfConvertData, State, retval
      IF (retval NE 0) THEN GOTO, cleanup
      GetBrfConvertData, State, CoordStruct.(whichorbit).PathNum, 1, $
                         misr_coords, BrfFactors, SolarIrrad, Retval

      ; Construct date string.

      toks = STRSPLIT(CoordStruct.(whichorbit).OrbitDate, '-', /EXTRACT, $
                      COUNT=ntok)
      orbitdate = ''
      IF (ntok EQ 3) THEN BEGIN
         months = ['Jan','Feb','Mar','Apr','May','Jun', $
                   'Jul','Aug','Sep','Oct','Nov','Dec']
         month = months[toks[1]-1]
         orbitdate = month + ' ' + STRTRIM(STRING(FIX(toks[2])),2) + $
                     ', ' + toks[0]
         months = 0
      ENDIF

      ; Begin assembling page of text.

      mssg = ['            MISR Geometry and Radiances for Orbit ' + $
              STRING(FORMAT='(I5)', CoordStruct.(whichorbit).OrbitNum) + $
              ' acquired on ' + orbitdate, $
              '           Block/Across/Along = ' + $
              coord_text + '; lat/long = ' + latlon_text, '.', $
              '   Cam  Band   SunZen  SunAzim  CamZen  CamAzim  CamScat' + $
              '  CamGlit    Rad    EqRefl    BRF        ', $
              '   ---  ----   ------  -------  ------  -------  -------' + $
              '  -------  -------  ------  ------       ']

      sunzen = ABS(SunZenithAng[0])  GT  90.0 ?  -9.999 : SunZenithAng[0]
      sunazi = ABS(SunAzimuthAng[0]) GT 360.0 ? -99.999 : SunAzimuthAng[0]

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

         camzen = ABS(CamZenithAng[icam,0])  GT  90.0 ?  -9.999 : $
                                                      CamZenithAng[icam,0]
         camazi = ABS(CamAzimuthAng[icam,0]) GT 360.0 ? -99.999 : $
                                                      CamAzimuthAng[icam,0]
         camsct = ABS(CamScatterAng[icam,0]) GT 360.0 ? -99.999 : $
                                                      CamScatterAng[icam,0]
         camglt = ABS(CamGlitterAng[icam,0]) GT 360.0 ? -99.999 : $
                                                      CamGlitterAng[icam,0]

         FOR iband = 0,CoordStruct.(whichorbit).num_band-1 DO BEGIN
            iband1 = iband
            iband2 = band_ndxs[iband]
            IF (CoordStruct.(whichorbit).num_band EQ 1) THEN BEGIN
               iband1 = band_ndxs[iband]
               iband2 = iband
            ENDIF
            bnd = !KON.Instr.BAND_NAMES[iband1]
            IF (STRLEN(bnd) LT 5) THEN bnd += ' '
            IF (STRLEN(bnd) LT 5) THEN bnd += ' '

            sunzstr = ' '  + STRING(FORMAT='(F6.3)', sunzen)
            sunastr = '  ' + STRING(FORMAT='(F7.3)', sunazi)
            camzstr = '  ' + STRING(FORMAT='(F6.3)', camzen)
            camastr = '  ' + STRING(FORMAT='(F7.3)', camazi)
            camsstr = '  ' + STRING(FORMAT='(F7.3)', camsct)
            camgstr = '  ' + STRING(FORMAT='(F7.3)', camglt)

            IF (FINITE(data2plot[iband2,icam], /NAN)) THEN BEGIN
               radval = -9.999
               eqrval = -9.999
               brfval = -9.999
            ENDIF ELSE BEGIN
               radval = data2plot[iband2,icam] / BrfFactors[icam,iband1,0]
               eqrval = radval * !PI / SolarIrrad[iband1]

               radval = radval LT 0.0 ? -9.999 : radval
               eqrval = eqrval LT 0.0 ? -9.999 : eqrval
               brfval = data2plot[iband2,icam] LT 0.0 ? -9.999 : $
                        data2plot[iband2,icam]
            ENDELSE

            radstr = ' '  + STRING(FORMAT='(F7.3)', radval)
            eqrstr = '  ' + STRING(FORMAT='(F6.3)', eqrval)
            brfstr = '  ' + STRING(FORMAT='(F6.3)', brfval)
            IF (iband EQ 0) THEN radstr = ' ' + radstr

            IF (iband1 EQ 0 OR CoordStruct.(whichorbit).num_band EQ 1) THEN BEGIN
               mssg = [mssg, '   ' + !KON.Instr.CAM_NAMES[icam] + '   ' + $
                       bnd + ' ' + sunzstr + sunastr + camzstr + $
                       camastr + camsstr + camgstr + radstr + eqrstr + $
                       brfstr]
            ENDIF ELSE BEGIN
               mssg = [mssg, '        ' + bnd + '               ' + $
                       '                                      ' + $
                       radstr + eqrstr + brfstr]
            ENDELSE
         ENDFOR
      ENDFOR
      mssg = [mssg,' ']
      mssg = [mssg,'             NOTE - The convention used here is ' + $
                   'that the sun zenith angle and', $
                   '                    camera zenith angle vectors ' + $
                   'all point toward the ground.']
      mssg = [mssg,' ']
      ok_cancel = '                        Do you want to write this ' + $
                  'table to a file?'
      mssg = [mssg,ok_cancel]
      title = 'MISR Geometry and Radiance Table'
      rtrn = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER, TITLE=title)

      ;---------------------------------------------------------------------
      ; Write table to a file if requested.
      ;---------------------------------------------------------------------

      IF (STRUPCASE(rtrn) EQ 'YES') THEN BEGIN
         block_str = som_crds[0]
         samp_str  = som_crds[1]
         line_str  = som_crds[2]

         filepre  = 'MISR_GeomAndRad' + '_O'
         filepost = '_B' +  block_str + '_S' + samp_str + '_L' + line_str

         CreateOrbitDirFile, '', filepre, filepost, 1, 'txt', $
                             CoordStruct.(whichorbit).OrbitNum, 1, $
                             dirname, tempfile, Retval

         num_line = N_ELEMENTS(mssg)
         mssg = mssg[0:num_line-3]

         OPENW, unit, dirname + tempfile, /GET_LUN
         PRINTF, unit, mssg
         FREE_LUN, unit

         mssg = ['Your data were written to file: ', '  ' + dirname + tempfile]
         rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Bring the window to the top and handle PostScript.
   ;------------------------------------------------------------------------
   
   !P.REGION = region_save
   
   IF (~ postscript) THEN BEGIN
      SafeWSHOW, ZOOM_WNDW, 1, 0, didit
   ENDIF ELSE BEGIN
      DEVICE, /CLOSE
      SET_PLOT, 'X'
      !P.MULTI = multi_save
   ENDELSE
   
   ;------------------------------------------------------------------------
   ; Save a copy of this event.  If it's the last event to pass through here
   ; then it will be used to generate the PostScript if the "Save BRF Plot
   ; as PostScript file" button is pressed.
   ;------------------------------------------------------------------------
   
   State.zoom_event_ptr = PTR_NEW( Event )
   
   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   cleanup:

   SetFontInfo, old_font

   zoom_array = 0
   orig_array = 0
   sort_ndxs = 0
   data2plot = 0
   temp_data = 0
   data2plot = 0
   data2plot_temp = 0
   data2plot_avg = 0
   data2plot_stddev = 0
   pcnt_var = 0
   mssg = 0

END ; DrawZoomWindow
