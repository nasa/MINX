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
PRO GetAboutMessage, HelpMsg
;***************************************************************************
; Constructs help messages for dialog boxes.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

op_sys = !VERSION.OS_NAME + ', ' + !VERSION.OS + ', ' + !VERSION.ARCH
mem_width = STRTRIM(STRING(!VERSION.MEMORY_BITS),2)
idl_version = '            IDL Version: ' + $
              STRTRIM(STRING(!VERSION.RELEASE),2) + ', ' + mem_width + '-bit'

HelpMsg = $
   [['               MINX V' + !KON.Misc.MINX_VERSION_NUM + $
		' (MISR INteractive eXplorer)'], $
    [' '], ['OS: ' + op_sys + idl_version], $
    ['--------------------------------------------------------------------    '], $
    [' '], $
    ['Copyright 2007-2019, by the California Institute of Technology.'], $
    ['ALL RIGHTS RESERVED.'], $
    [' '], $
    ['United States Government Sponsorship acknowledged. Any commercial use'], $
    ['must be negotiated with the Office of Technology Transfer at the'], $
    ['California Institute of Technology.'], $
    [' '], $
    ['This software is subject to U.S. export control laws and regulations'], $
    ['(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the'], $
    ['software is subject to U.S. export control laws and regulations, the'], $
    ['recipient has the responsibility to obtain export licenses or other'], $
    ['export authority as may be required before exporting such information'], $
    ['to foreign countries or providing access to foreign nationals.'], $
    [' '], $
    ['We thank the following organizations for providing and making public'], $
    ['certain data used by MINX:'], $
    ['- MODIS-Terra science teams for providing MOD14 thermal anomaly data;'], $
    ['- National Geophysical Data Center for providing volcano locations;'], $
    ['- Goddard Space Flight Center for providing Aeronet site locations;'], $
    ["- NASA's Langley Research Center for processing, archiving and making"], $
    ['  available MISR data.']]

END ; GetAboutMessage

;***************************************************************************
PRO GetProgramOption_eh, event
;***************************************************************************
; Event handler for user interface for getting main program option.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON struct_main_option, main_option

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_main, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   main_option = 0
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF
;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_main.button1 : BEGIN
      widget_struct_main.main_option = 1
   END
   event.id EQ widget_struct_main.button2 : BEGIN
      widget_struct_main.main_option = 2
   END
   event.id EQ widget_struct_main.button3 : BEGIN
      widget_struct_main.main_option = 3
   END
   event.id EQ widget_struct_main.button4 : BEGIN
      widget_struct_main.main_option = 4
   END
   event.id EQ widget_struct_main.button5 : BEGIN
      widget_struct_main.main_option = 5
   END
   event.id EQ widget_struct_main.button6 : BEGIN
      widget_struct_main.main_option = 6
   END
   event.id EQ widget_struct_main.button7 : BEGIN
      widget_struct_main.main_option = 7
   END
   event.id EQ widget_struct_main.button8 : BEGIN
      widget_struct_main.main_option = 8
   END
   event.id EQ widget_struct_main.button9 : BEGIN
      widget_struct_main.main_option = 9
   END

   event.id EQ widget_struct_main.ok_button : BEGIN
      main_option = widget_struct_main.main_option
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ widget_struct_main.cancel_button : BEGIN
      main_option = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ widget_struct_main.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_MainMenu.pdf'
   END
   event.id EQ widget_struct_main.about_button : BEGIN
      GetAboutMessage, HelpMsg
      res = DIALOG_MESSAGE(HelpMsg, /INFORMATION, /CENTER)
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_main, /NO_COPY

END ; GetProgramOption_eh

;***************************************************************************
PRO GetProgramOption_gui, this_main_option
;***************************************************************************
; User interface for getting main program option.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON struct_main_option, main_option

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0   = WIDGET_BASE( /COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM, $
                       /TLB_KILL_REQUEST_EVENTS)

base00  = WIDGET_BASE(   base0, /COLUMN, /ALIGN_CENTER )
label00 = WIDGET_LABEL(  base00, VALUE='-  MISR INteractive eXplorer  -' )

label0  = WIDGET_LABEL(  base0, VALUE='Program Options' )
base1   = WIDGET_BASE(   base0, /COLUMN, /FRAME, /ALIGN_CENTER )
base2   = WIDGET_BASE(   base1, /COLUMN, /EXCLUSIVE )
button1 = WIDGET_BUTTON( base2, VALUE='Show Orbit Location' )
button2 = WIDGET_BUTTON( base2, VALUE='Find Overpasses' )
button3 = WIDGET_BUTTON( base2, VALUE='Show Camera Image' )
button4 = WIDGET_BUTTON( base2, VALUE='Compare Data Products' )
button5 = WIDGET_BUTTON( base2, VALUE='Animate Cameras' )
button6 = WIDGET_BUTTON( base2, VALUE='Plume Project Preferences ' )
button7 = WIDGET_BUTTON( base2, VALUE='Plume Project Utilities ' )
button8 = WIDGET_BUTTON( base2, VALUE='Process Plume Project ' )
button9 = !VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER ? $
          WIDGET_BUTTON( base2, VALUE='Convert BRFs ' ) : 0

base3 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( base3, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( base3, VALUE=' Exit ' )

base4 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
help_button = WIDGET_BUTTON( base4, VALUE=' PDF Help ' )
about_button = WIDGET_BUTTON( base4, VALUE=' About ' )

;---------------------------------------------------------------------------
; Set default button values and sensitivities.
;---------------------------------------------------------------------------

this_main_option = 1
WIDGET_CONTROL, button1, /SET_BUTTON

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct_main = { $
   button1       : button1, $         ; location
   button2       : button2, $         ; overpasses
   button3       : button3, $         ; browse
   button4       : button4, $         ; multi-field compare
   button5       : button5, $         ; animate
   button6       : button6, $         ; plume project preferences
   button7       : button7, $         ; plume project utilities
   button8       : button8, $         ; process plume projects
   button9       : button9, $         ; BRF convert
   ok_button     : ok_button, $
   cancel_button : cancel_button, $
   help_button   : help_button, $
   about_button  : about_button, $
   main_option   : this_main_option }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_main, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'GetProgramOption_gui', base0, EVENT_HANDLER='GetProgramOption_eh'

this_main_option = main_option

END ; GetProgramOption_gui

;***************************************************************************
PRO OrbitLocateOption_eh, event
;***************************************************************************
; Event handler for user interface for setting orbit display location options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON orbit_locate_options, WidgetStructLocate

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_locate, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WidgetStructLocate.Cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_locate.button1a : BEGIN
      widget_struct_locate.PathOrOrbit = 1
      WIDGET_CONTROL, widget_struct_locate.label2, $
                      SET_VALUE='Enter Path Number:'
   END
   event.id EQ widget_struct_locate.button1b : BEGIN
      widget_struct_locate.PathOrOrbit = 2
      WIDGET_CONTROL, widget_struct_locate.label2, $
                      SET_VALUE='Enter Orbit Number:'
   END
   event.id EQ widget_struct_locate.label2 : BEGIN
   END
   event.id EQ widget_struct_locate.integer2 : BEGIN
   END
   event.id EQ widget_struct_locate.integer3a : BEGIN
   END
   event.id EQ widget_struct_locate.integer3b : BEGIN
   END

   event.id EQ widget_struct_locate.ok_button : BEGIN
      tempa = WIDGET_INFO(widget_struct_locate.button1a, /BUTTON_SET)
      IF (tempa EQ 1) THEN BEGIN
         WidgetStructLocate.PathOrOrbit = 1
         WIDGET_CONTROL, widget_struct_locate.integer2, GET_VALUE=temp
         IF (temp LT 1) THEN BEGIN
            WIDGET_CONTROL, widget_struct_locate.integer2, SET_VALUE=1
            mssg = ['The smallest valid path number is 1.', $
                    'Value has been reset.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            GOTO, gohere
         ENDIF
         IF (temp GT !KON.Instr.NUM_PATHS) THEN BEGIN
            WIDGET_CONTROL, widget_struct_locate.integer2, $
                            SET_VALUE=!KON.Instr.NUM_PATHS
            mssg = ['The largest valid path number is 233.', $
                    'Value has been reset.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            GOTO, gohere
         ENDIF
         WidgetStructLocate.PathNum = temp
         WidgetStructLocate.OrbitNum = 0L
      ENDIF
      tempb = WIDGET_INFO(widget_struct_locate.button1b, /BUTTON_SET)
      IF (tempb EQ 1) THEN BEGIN
         WidgetStructLocate.PathOrOrbit = 2
         WIDGET_CONTROL, widget_struct_locate.integer2, GET_VALUE=temp
         IF (temp LT 995) THEN BEGIN
            IF (temp GT 0) THEN BEGIN
               mssg = ['The first valid orbit acquired by MISR was 995.', $
                       'There will never be valid data for smaller orbit', $
                       'numbers. Orbit will be displayed anyway.']
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)
            ENDIF ELSE BEGIN
               WIDGET_CONTROL, widget_struct_locate.integer2, SET_VALUE=1
               mssg = ['The smallest orbit number accepted by MINX is 1.', $
                       'Value has been reset.']
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
               GOTO, gohere
            ENDELSE
         ENDIF
         WidgetStructLocate.PathNum = 0
         WidgetStructLocate.OrbitNum = LONG(temp)
      ENDIF
      WIDGET_CONTROL, widget_struct_locate.integer3a, GET_VALUE=temp
      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, widget_struct_locate.integer3a, SET_VALUE=1
         GOTO, gohere
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, widget_struct_locate.integer3a, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         GOTO, gohere
      ENDIF
      WidgetStructLocate.BlockBeg = temp
      WIDGET_CONTROL, widget_struct_locate.integer3b, GET_VALUE=temp
      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, widget_struct_locate.integer3b, SET_VALUE=1
         GOTO, gohere
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, widget_struct_locate.integer3b, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         GOTO, gohere
      ENDIF
      IF (temp LT WidgetStructLocate.BlockBeg) THEN BEGIN
         WIDGET_CONTROL, widget_struct_locate.integer3b, $
                         SET_VALUE=WidgetStructLocate.BlockBeg
         GOTO, gohere
      ENDIF
      WidgetStructLocate.BlockEnd = temp
      WIDGET_CONTROL, event.top, /DESTROY

      WidgetStructLocate.Cancel = 0
      RETURN
   END

   event.id EQ widget_struct_locate.cancel_button : BEGIN
      WidgetStructLocate.Cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_locate.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_ShowOrbitLocation.pdf'
   END

ENDCASE

gohere:
WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_locate, /NO_COPY

END ; OrbitLocateOption_eh

;***************************************************************************
PRO OrbitLocateOption_gui, PathOrOrbit, PathNum, OrbitNum, $
                           BlockBeg, BlockEnd, Cancel
;***************************************************************************
; User interface for setting orbit display location options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON orbit_locate_options, WidgetStructLocate

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

Cancel = 0
PathOrOrbit = 2
PathNum = 1
OrbitNum = LONG(995)
BlockBeg = 1
BlockEnd = !KON.Instr.NUM_BLOCKS

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE( /COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM, $
                     /TLB_KILL_REQUEST_EVENTS)
label0 = WIDGET_LABEL( base0, VALUE='Show Orbit Location Options' )

base1 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label1 = WIDGET_LABEL( base1, VALUE='Select Orbit Location using: ')
base1a = WIDGET_BASE( base1, /COLUMN, /EXCLUSIVE )
button1a = WIDGET_BUTTON( base1a, VALUE='Path Number' )
button1b = WIDGET_BUTTON( base1a, VALUE='Orbit Number' )

base2 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label2 = WIDGET_LABEL( base2, VALUE=' Enter Orbit Number:' )
integer2 = CW_FIELD( base2, VALUE='0', /LONG, XSIZE=6, $
                     TITLE='Path/Orbit Number' )

base3 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label3 = WIDGET_LABEL( base3, VALUE='Enter Block Range:' )
integer3a = CW_FIELD( base3, VALUE='0', /INTEGER, XSIZE=5, $
                      TITLE='First Block:' )
integer3b = CW_FIELD( base3, VALUE='0', /INTEGER, XSIZE=5, $
                      TITLE='Last Block:' )

basex = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( basex, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( basex, VALUE='Cancel' )
help_button = WIDGET_BUTTON( basex, VALUE='PDF Help' )

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct_locate = { button1a:button1a, $
                         button1b:button1b, $
                         label2:label2, $
                         integer2:integer2, $
                         integer3a:integer3a, $
                         integer3b:integer3b, $
                         ok_button:ok_button, $
                         cancel_button:cancel_button, $
                         help_button:help_button, $
                         PathOrOrbit:PathOrOrbit, $
                         PathNum:PathNum, $
                         OrbitNum:OrbitNum, $
                         BlockBeg:BlockBeg, $
                         BlockEnd:BlockEnd, $
                         Cancel:Cancel }

HELP, /STRUCTURE, WidgetStructLocate, OUTPUT=exists

IF (STRTRIM(exists[1],2) EQ 'UNDEFINED = <Undefined>') THEN BEGIN
   WidgetStructLocate = widget_struct_locate
ENDIF ELSE BEGIN
   widget_struct_locate.PathOrOrbit = WidgetStructLocate.PathOrOrbit
   widget_struct_locate.PathNum  = WidgetStructLocate.PathNum
   widget_struct_locate.OrbitNum = WidgetStructLocate.OrbitNum
   widget_struct_locate.BlockBeg = WidgetStructLocate.BlockBeg
   widget_struct_locate.BlockEnd = WidgetStructLocate.BlockEnd
   widget_struct_locate.Cancel   = WidgetStructLocate.Cancel
ENDELSE

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

IF (widget_struct_locate.PathOrOrbit LE 1) THEN BEGIN
   WIDGET_CONTROL, button1a, /SET_BUTTON
   WIDGET_CONTROL, integer2, SET_VALUE=widget_struct_locate.PathNum
ENDIF ELSE BEGIN
   WIDGET_CONTROL, button1b, /SET_BUTTON
   WIDGET_CONTROL, integer2, SET_VALUE=widget_struct_locate.OrbitNum
ENDELSE
WIDGET_CONTROL, integer3a, SET_VALUE=widget_struct_locate.BlockBeg
WIDGET_CONTROL, integer3b, SET_VALUE=widget_struct_locate.BlockEnd

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_locate, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'OrbitLocateOption_gui', base0, EVENT_HANDLER='OrbitLocateOption_eh'

PathOrOrbit = WidgetStructLocate.PathOrOrbit
PathNum = WidgetStructLocate.PathNum
OrbitNum = WidgetStructLocate.OrbitNum
BlockBeg = WidgetStructLocate.BlockBeg
BlockEnd = WidgetStructLocate.BlockEnd

Cancel = WidgetStructLocate.Cancel

widget_struct_locate = 0

END ; OrbitLocateOption_gui

;****************************************************************************
FUNCTION loopgrow, previous_array, sample
;****************************************************************************

COMPILE_OPT IDL2

CASE 1 OF
  N_ELEMENTS(previous_array) eq 0 : BEGIN
    sample_array = [sample]
  END

  previous_array[0] eq '' : BEGIN
    sample_array = [sample]
  END

  ELSE : BEGIN
    sample_array = [[previous_array], [sample]]
  END
ENDCASE

RETURN, sample_array

END

;***************************************************************************
PRO OverpassTimeLoc_eh, event
;***************************************************************************

COMPILE_OPT IDL2

COMMON save_overpass_vals, List, GMToffset, timeval_save, latval_save, $
                           lonval_save, GMToffset_save, ListStatus

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=opass_struct, /NO_COPY

LAT_MINMAX = 84.0
LON_MINMAX = 180.0

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   ListStatus = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ opass_struct.loceditlat : BEGIN
   END

   event.id EQ opass_struct.loceditlon : BEGIN
   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked button to add a string to the list.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.addbutton : BEGIN
      WIDGET_CONTROL, opass_struct.selects,    GET_UVALUE=List
      WIDGET_CONTROL, opass_struct.timeedit,   GET_VALUE=timeval
      WIDGET_CONTROL, opass_struct.loceditlat, GET_VALUE=latval
      WIDGET_CONTROL, opass_struct.loceditlon, GET_VALUE=lonval

      IF List[0] EQ '' THEN BEGIN
        obj = 0L
      ENDIF ELSE BEGIN
        spllist = STRSPLIT(List[N_ELEMENTS(List)-1], ' ', /EXTRACT)
        obj = LONG(spllist[0])
      ENDELSE

      ;--------------------------------------------------------------------
      ; Process time ranges. Test times user is adding to the list.
      ;--------------------------------------------------------------------

      chrs = STREGEX(timeval, '[^0-9 :\-]', /EXTRACT)
      IF (chrs NE '') THEN BEGIN
           mssg = ['Character "' + chrs + '" was found in the Date field.', $
                   'Correct and try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
      ENDIF

      IF STRMATCH(timeval, '*:*') THEN BEGIN
        sepstr = STRSPLIT(timeval, ': ', /EXTRACT, COUNT=nstr)
        IF (nstr NE 2) THEN BEGIN
           mssg = ['Dates must be in this format: "YYYY-MM-DD".', $
                   'Correct and try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        sstr = sepstr[0]
        estr = sepstr[1]
        sdate = sstr
        edate = estr
      ENDIF ELSE BEGIN
        sdate = timeval
        edate = timeval
      ENDELSE

      date_s = STRSPLIT(sdate, '-', /EXTRACT, COUNT=num_s)
      date_e = STRSPLIT(edate, '-', /EXTRACT, COUNT=num_e)
      IF (num_s NE 3 OR num_e NE 3) THEN BEGIN
         mssg = ['Dates must be in this format: "YYYY-MM-DD".', $
                 'Correct and try again.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF
      IF (FIX(date_s[1]) LT  1 OR FIX(date_e[1]) LT  1 OR $
          FIX(date_s[1]) GT 12 OR FIX(date_e[1]) GT 12) THEN BEGIN
         mssg = 'A month value is out of range. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF
      IF (FIX(date_s[2]) LT  1 OR FIX(date_e[2]) LT  1 OR $
          FIX(date_s[2]) GT 31 OR FIX(date_e[2]) GT 31) THEN BEGIN
         mssg = 'A day value is out of range. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF

      stim = date_s[0] + '-' + date_s[1] + '-' + date_s[2] + 'T00:00:00Z'
      IF (STRLEN(stim) NE 20) THEN BEGIN
         mssg = ['Your start date is not in the form "YYYY-MM-DD".', $
                 'Correct and try again.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF
      stime_julian = JULDAY(date_s[1], date_s[2], date_s[0], 0, 0, 0) 

      etim = date_e[0] + '-' + date_e[1] + '-' + date_e[2] + 'T23:59:59Z'
      IF (STRLEN(etim) NE 20) THEN BEGIN
         mssg = ['Your end date is not in the form "YYYY-MM-DD".', $
                 'Correct and try again.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF
      etime_julian = JULDAY(date_e[1], date_e[2], date_e[0], 23, 59, 59) 

      IF (etime_julian LT stime_julian) THEN BEGIN
         mssg = 'Start date must be earlier than end date. Correct and try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF

      obj += 1

      ;--------------------------------------------------------------------
      ; Process lat/lon ranges. Test values user is adding to the list.
      ;--------------------------------------------------------------------

      chrs = STREGEX(latval, '[^0-9 +.:\-]', /EXTRACT)
      IF (chrs NE '') THEN BEGIN
           mssg = ['Character "' + chrs + '" was found in the Latitude field.', $
                   'Correct and try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
      ENDIF
      chrs = STREGEX(lonval, '[^0-9 +54.:\-]', /EXTRACT)
      IF (chrs NE '') THEN BEGIN
           mssg = ['Character "' + chrs + '" was found in the Longitude field.', $
                   'Correct and try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
      ENDIF

      IF (~STRMATCH(latval, '*:*') AND ~STRMATCH(lonval, '*:*')) THEN BEGIN

        ; Process single lat, single lon. Test values adding to the list.

        IF (~IsNumber(latval[0], 2)) THEN BEGIN
           mssg = 'Latitude value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lat_val = FLOAT(latval)

        IF (lat_val LT (-LAT_MINMAX) OR lat_val GT LAT_MINMAX) THEN BEGIN
           mssg = ['Absolute value of latitude must not be greater than', $
                    STRTRIM(STRING(LAT_MINMAX),2) + ' degrees. Try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        IF (~IsNumber(lonval[0], 2)) THEN BEGIN
           mssg = 'Longitude value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lon_val = FLOAT(lonval)

        IF (lon_val LT (-LON_MINMAX) OR lon_val GT LON_MINMAX) THEN BEGIN
           mssg = ['Absolute value of longitude must not be greater than ' + $
                    STRTRIM(STRING(LON_MINMAX),2) + ' degrees. Try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        lalo = STRING(FORMAT='(F7.3)',lat_val) + '   ' + $
               STRING(FORMAT='(F7.3)',lat_val) + '   ' + $
               STRING(FORMAT='(F8.3)',lon_val) + '  '  + $
               STRING(FORMAT='(F8.3)',lon_val)

        listarr = ' ' + sdate + '  ' + edate +  '   ' + lalo
      ENDIF ELSE BEGIN

        ; Process lat/lon ranges. Test values adding to the list.

        IF (~STRMATCH(latval, '*:*')) THEN BEGIN
           IF (latval GT LAT_MINMAX - 0.001) THEN $
              latval = STRING(FORMAT='(F7.3)', LAT_MINMAX - 0.001)
           f_latval = FLOAT(latval) + 0.001
           latval += ' : ' + STRING(FORMAT='(F7.3)', f_latval)
        ENDIF

        IF (~STRMATCH(lonval, '*:*')) THEN BEGIN
        IF (lonval GT LON_MINMAX - 0.001) THEN $
           lonval = STRING(FORMAT='(F7.3)', LON_MINMAX - 0.001)
           f_lonval = FLOAT(lonval) + 0.001
           lonval += ' : ' + STRING(FORMAT='(F7.3)', f_lonval)
        ENDIF

        ranlat = STRSPLIT(latval, ': ', /EXTRACT)
        ranlon = STRSPLIT(lonval, ': ', /EXTRACT)

        IF (N_ELEMENTS(ranlat) NE 2) THEN BEGIN
           mssg = ['Use the : character to separate', $
                   'two latitude values. Try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        IF (~IsNumber(ranlat[0], 2)) THEN BEGIN
           mssg = 'Latitude begin value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lat_val_beg = FLOAT(ranlat[0])

        IF (~IsNumber(ranlat[1], 2)) THEN BEGIN
           mssg = 'Latitude end value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lat_val_end = FLOAT(ranlat[1])

        IF (N_ELEMENTS(ranlon) NE 2) THEN BEGIN
           mssg = ['Use the : character to separate', $
                   'two longitude values. Try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        IF (~IsNumber(ranlon[0], 2)) THEN BEGIN
           mssg = 'Longitude begin value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lon_val_beg = FLOAT(ranlon[0])

        IF (~IsNumber(ranlon[1], 2)) THEN BEGIN
           mssg = 'Longitude end value is not valid. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        lon_val_end = FLOAT(ranlon[1])

        IF (lat_val_end LT lat_val_beg) THEN BEGIN
           mssg = 'Start latitude must be south of end latitude. Try again.'
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        IF (lon_val_end LT lon_val_beg) THEN BEGIN
           ; Make an exception for small regions spanning the dateline
           IF (lon_val_beg LT 170. OR lon_val_end GT -170.) THEN BEGIN
              mssg = ['Start longitude must not be greater than end longitude', $
                      'unless you use a range within the bounds +170 -> -170', $
                      'crossing the dateline. Try again.']
              rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
              GOTO, cancelled
           ENDIF
        ENDIF

        IF (lat_val_beg LT (-LAT_MINMAX) OR lat_val_beg GT LAT_MINMAX OR $
            lat_val_end LT (-LAT_MINMAX) OR lat_val_end GT LAT_MINMAX) THEN BEGIN
           mssg = ['Absolute value of latitudes must not be greater than', $
                    STRTRIM(STRING(LAT_MINMAX),2) + ' degrees. Try again.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF
        IF (lon_val_beg LT (-LON_MINMAX) OR lon_val_beg GT LON_MINMAX OR $
            lon_val_end LT (-LON_MINMAX) OR lon_val_end GT LON_MINMAX) THEN BEGIN
           mssg = ['Absolute value of longitudes must not be greater than ' + $
                    STRTRIM(STRING(LON_MINMAX),2) + ' degrees.', $
                   'To cross the dateline, try 179 : -179, e.g.']
           rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
           GOTO, cancelled
        ENDIF

        ;------------------------------------------------------------------
        ; Construct string to add to list box.
        ;------------------------------------------------------------------

        lat_range = STRING(FORMAT='(F7.3)',lat_val_beg) + '   ' + $
                    STRING(FORMAT='(F7.3)',lat_val_end) + '   ' + $
                    STRING(FORMAT='(F8.3)',lon_val_beg) + '  ' + $
                    STRING(FORMAT='(F8.3)',lon_val_end)

        strn = ' ' + sdate + '  ' + edate + '   '

        listarr = strn + lat_range

      ENDELSE

      newList = loopgrow(List, listarr)
      WIDGET_CONTROL, opass_struct.selects, SET_VALUE=newList, SET_UVALUE=newList
   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked button to remove a string from the list.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.rembutton : BEGIN
      itemrem = WIDGET_INFO(opass_struct.selects, /LIST_SELECT)

      IF (itemrem EQ -1) THEN BEGIN
        msg = ['Please select a location or time entry to delete']
        rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)

      ENDIF ELSE BEGIN
        WIDGET_CONTROL, opass_struct.selects, GET_UVALUE=List
        FOR i = 0, N_ELEMENTS(List)-1 DO BEGIN
          spllist = STRSPLIT(List[i], ' ', /EXTRACT)
          spl_list = loopgrow(spl_list, spllist)
        ENDFOR

        IF (itemrem NE 0) THEN BEGIN
           FOR irem=0,itemrem-1 DO BEGIN
              newList = loopgrow(newList, List[irem])
           ENDFOR
        ENDIF

        IF (itemrem NE N_ELEMENTS(List)-1) THEN BEGIN
           FOR irem=itemrem+1,N_ELEMENTS(List)-1 DO BEGIN
              newList = loopgrow(newList, List[irem])
           ENDFOR
        ENDIF

        IF (N_ELEMENTS(newList) EQ 0) THEN newList = ''

        WIDGET_CONTROL, opass_struct.selects, SET_VALUE=newList, $
                        SET_UVALUE=newList
      ENDELSE
   END

   event.id EQ opass_struct.gmtedit : BEGIN
      WIDGET_CONTROL, opass_struct.gmtedit, GET_VALUE=offset
      IF (ABS(offset) GT 12.0) THEN BEGIN
         mssg = ['Absolute value of hours must not ' + $
                 'be greater than 12. Try again.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         GOTO, cancelled
      ENDIF
   END

   event.id EQ opass_struct.selects : BEGIN
   END

   event.id EQ opass_struct.timeedit : BEGIN
   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked OK button.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.ok_button : BEGIN

      ;--------------------------------------------------------------------
      ; Get data from the listbox and the GMT-to-local-time edit box.
      ;--------------------------------------------------------------------

      WIDGET_CONTROL, opass_struct.gmtedit, GET_VALUE=GMToffset
      WIDGET_CONTROL, opass_struct.selects, GET_UVALUE=List

      IF List[0] EQ '' THEN BEGIN
        msg = ['Please enter at least one valid location and at least ' + $
               'one valid time to search']
        rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
      ENDIF ELSE BEGIN

        ;------------------------------------------------------------------
        ; Get data from the data entry box and save to use next time in this
        ; session user re-enters the dialog.
        ;------------------------------------------------------------------

        WIDGET_CONTROL, opass_struct.timeedit,   GET_VALUE=timeval
        WIDGET_CONTROL, opass_struct.loceditlat, GET_VALUE=latval
        WIDGET_CONTROL, opass_struct.loceditlon, GET_VALUE=lonval

        GMToffset_save = GMToffset
        timeval_save = timeval
        latval_save = latval
        lonval_save = lonval

        WIDGET_CONTROL, event.top, /DESTROY
        ListStatus = 0
        RETURN

      ENDELSE
   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked Cancel button.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.cancel_button : BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      ListStatus = -1
      RETURN
   END

   event.id EQ opass_struct.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_FindOverpasses.pdf'
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=opass_struct, /NO_COPY
RETURN

cancelled:
ListStatus = -1
WIDGET_CONTROL, event.top, SET_UVALUE=opass_struct, /NO_COPY

END ; OverpassTimeLoc_eh

;***************************************************************************
PRO OverpassTimeLocGui, LL_list, TM_list, GMT_Offset_Hrs, Retval
;***************************************************************************

COMPILE_OPT IDL2

COMMON save_overpass_vals, List, GMToffset, timeval_save, latval_save, $
                           lonval_save, GMToffset_save, ListStatus

;---------------------------------------------------------------------------
; Define initial default values for GUI.
;---------------------------------------------------------------------------

IF (N_ELEMENTS(List) EQ 0) THEN BEGIN
   date_time = SYSTIME()
   toks = STRSPLIT(date_time, ' ', /EXTRACT)
   year_str = toks[4]
   mnth = GetIntMonth(toks[1])
   timeval_save = year_str + '-' + mnth + '-01 : ' + year_str + '-' + $
                  mnth + '-15'
   GMToffset_save = 0
   latval_save = ' -2.00 : 2.00'
   lonval_save = ' -2.00 : 2.00'
ENDIF

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base00 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
                     ' : MISR Overpass Finder', /TLB_KILL_REQUEST_EVENTS)
label1b = WIDGET_LABEL(base00, VALUE='Enter Dates and Locations to find ' + $
                       'MISR Overpass Orbits and Times')

baseedits = WIDGET_BASE(base00, /ROW, /ALIGN_CENTER)
baseadd   = WIDGET_BASE(baseedits, /COLUMN, /ALIGN_CENTER, /FRAME)

labeltimeadd = WIDGET_LABEL(baseadd, VALUE=' Local Date or Date Range to Search')

;---------------------------------------------------------------------------
timeedit = CW_FIELD(baseadd, /STRING, VALUE=timeval_save, XSIZE=24, $
                    TITLE='Enter Date:  ')
;---------------------------------------------------------------------------

dummylabel2 = WIDGET_LABEL(baseadd, VALUE=' ')

;---------------------------------------------------------------------------
gmtedit = CW_FIELD(baseadd, /INTEGER, VALUE=GMToffset_save, XSIZE=5, $
                   TITLE='Enter Local Hours After GMT: ', /ALL_EVENTS)

labelhours1 = WIDGET_LABEL(baseadd, $
                           VALUE='(Click "PDF Help" for instructions)')
;---------------------------------------------------------------------------

labellocdum = WIDGET_LABEL(baseadd, VALUE=' ')
labellocadd = WIDGET_LABEL(baseadd, $
                           VALUE='   Lat/Lon or Lat/Lon Range to Search')

;---------------------------------------------------------------------------
loceditlat = CW_FIELD(baseadd, /STRING, VALUE=latval_save, XSIZE=20, $
                      TITLE='Enter Latitude:  ')
loceditlon = CW_FIELD(baseadd, /STRING, VALUE=lonval_save, XSIZE=20, $
                      TITLE='Enter Longitude: ')
;---------------------------------------------------------------------------

baseaddrem = WIDGET_BASE(baseedits, /COLUMN)
centerbuttons1 = WIDGET_LABEL(baseaddrem, VALUE=' ')
centerbuttons2 = WIDGET_LABEL(baseaddrem, VALUE=' ')
centerbuttons3 = WIDGET_LABEL(baseaddrem, VALUE=' ')
centerbuttons4 = WIDGET_LABEL(baseaddrem, VALUE=' ')

;---------------------------------------------------------------------------
addbutton = WIDGET_BUTTON(baseaddrem, VALUE='Add to list')
rembutton = WIDGET_BUTTON(baseaddrem, VALUE='Remove from list')
;---------------------------------------------------------------------------

baserem = WIDGET_BASE(baseedits, /COLUMN)
title1 = '  Beg Date    End Date    Beg Lat   End Lat    Beg Lon   End Lon'
labelrem1 = WIDGET_LABEL(baserem, VALUE=title1, /ALIGN_LEFT)

;---------------------------------------------------------------------------
selects = WIDGET_LIST(baserem, UVALUE='', YSIZE=10, XSIZE=62)
;---------------------------------------------------------------------------

dummylabel3 = WIDGET_LABEL(baserem, VALUE=' ')
baseoch = WIDGET_BASE(baserem, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(baseoch, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(baseoch, VALUE='Cancel')
help_button = WIDGET_BUTTON(baseoch, VALUE=' PDF Help ')

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

opass_struct = { timeedit      : timeedit, $
                 loceditlat    : loceditlat, $
                 loceditlon    : loceditlon, $
                 addbutton     : addbutton, $
                 rembutton     : rembutton, $
                 gmtedit       : gmtedit, $
                 selects       : selects, $
                 ok_button     : ok_button, $
                 cancel_button : cancel_button, $
                 help_button   : help_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

LLlist = ['']
TMlist = ['']

WIDGET_CONTROL, base00, SET_UVALUE=opass_struct, XOFFSET=500, YOFFSET=250, $
                /NO_COPY
WIDGET_CONTROL, base00, /REALIZE
XMANAGER, 'OverpassTimeLocGui', base00, EVENT_HANDLER='OverpassTimeLoc_eh'

Retval = ListStatus
IF (ListStatus LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Post-process the results.
;---------------------------------------------------------------------------

nList = N_ELEMENTS(List)
LaLo_List = FLTARR(4,nList)
Time_List = STRARR(4,nList)

FOR ind=0,nList-1 DO BEGIN

   ;-----------------------------------------------------------------------
   ; parse the string and pull out the needed parameters
   ;-----------------------------------------------------------------------

   sepstr = STRSPLIT(List[ind], ' ', /EXTRACT)

   Time_List[*, ind] = [sepstr[0], '00:00:00', sepstr[1], '23:59:59']

   LaLo_List[*, ind] = [FLOAT(sepstr[2]), FLOAT(sepstr[3]), $
                        FLOAT(sepstr[4]), FLOAT(sepstr[5])] 
ENDFOR

;---------------------------------------------------------------------------
; Copy results to return parameters and clean up.
;---------------------------------------------------------------------------

LL_list = LaLo_List
TM_list = Time_List
GMT_Offset_Hrs = GMToffset

LaLo_List = 0
Time_List = 0
GMToffset = 0

ListStatus = 0

END ; OverpassTimeLocGui

;***************************************************************************
PRO OverpassForOrder_eh, event
;***************************************************************************

COMPILE_OPT IDL2

COMMON save_overpass_vals, List, GMToffset, timeval_save, latval_save, $
                           lonval_save, GMToffset_save, ListStatus

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=opass_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   ListStatus = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

LAT_MINMAX = 84.0
LON_MINMAX = 180.0

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ opass_struct.timeedit : BEGIN
   END

   event.id EQ opass_struct.loceditlat : BEGIN
   END

   event.id EQ opass_struct.loceditlon : BEGIN
   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked OK button.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.ok_button : BEGIN

      WIDGET_CONTROL, opass_struct.timeedit,   GET_VALUE=timeval
      WIDGET_CONTROL, opass_struct.loceditlat, GET_VALUE=latval
      WIDGET_CONTROL, opass_struct.loceditlon, GET_VALUE=lonval

      ;--------------------------------------------------------------------
      ; Process time ranges. Test values user entered.
      ;--------------------------------------------------------------------

      IF STRMATCH(timeval, '*:*') THEN BEGIN
        sepstr = STRSPLIT(timeval, ': ', /EXTRACT)
        sstr = sepstr[0]
        estr = sepstr[1]
        sdate = sstr
        edate = estr
      ENDIF ELSE BEGIN
        sdate = timeval
        edate = timeval
      ENDELSE

      date_s = STRSPLIT(sdate, '/', /EXTRACT, COUNT=num_s)
      date_e = STRSPLIT(edate, '/', /EXTRACT, COUNT=num_e)

      IF (num_s NE 3 OR num_e NE 3) THEN BEGIN
         mssg = 'Dates must be in this format: "YYYY/MM/DD". Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF
      IF (FIX(date_s[1]) LT  1 OR FIX(date_e[1]) LT  1 OR $
          FIX(date_s[1]) GT 12 OR FIX(date_e[1]) GT 12) THEN BEGIN
         mssg = 'A month value is out of range. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF
      IF (FIX(date_s[2]) LT  1 OR FIX(date_e[2]) LT  1 OR $
          FIX(date_s[2]) GT 31 OR FIX(date_e[2]) GT 31) THEN BEGIN
         mssg = 'A day value is out of range. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF

      stim = date_s[0] + '-' + date_s[1] + '-' + date_s[2] + 'T00:00:00Z'
      stime_julian = JULDAY(date_s[1], date_s[2], date_s[0], 0, 0, 0) 

      etim = date_e[0] + '-' + date_e[1] + '-' + date_e[2] + 'T23:59:59Z'
      etime_julian = JULDAY(date_e[1], date_e[2], date_e[0], 23, 59, 59) 

      IF (etime_julian LT stime_julian) THEN BEGIN
         mssg = 'Start date must be earlier than end date. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF

      ;--------------------------------------------------------------------
      ; Process lat/lon ranges. Test values user entered.
      ;--------------------------------------------------------------------

      IF STRMATCH(latval, '*:*') THEN BEGIN

         ranlat = STRSPLIT(latval, ': ', /EXTRACT)
         ranlon = STRSPLIT(lonval, ': ', /EXTRACT)

         IF (N_ELEMENTS(ranlat) NE 2) THEN BEGIN
            mssg = ['Use the : character to separate', $
                    'two latitude values. Try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF

         IF (~IsNumber(ranlat[0], 2)) THEN BEGIN
            mssg = 'Latitude begin value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lat_val_beg = FLOAT(ranlat[0])

         IF (~IsNumber(ranlat[1], 2)) THEN BEGIN
            mssg = 'Latitude end value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lat_val_end = FLOAT(ranlat[1])

         IF (N_ELEMENTS(ranlon) NE 2) THEN BEGIN
            mssg = ['Use the : character to separate', $
                    'two longitude values. Try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF

         IF (~IsNumber(ranlon[0], 2)) THEN BEGIN
            mssg = 'Longitude begin value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lon_val_beg = FLOAT(ranlon[0])

         IF (~IsNumber(ranlon[1], 2)) THEN BEGIN
            mssg = 'Longitude end value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lon_val_end = FLOAT(ranlon[1])

         IF (lat_val_end LT lat_val_beg) THEN BEGIN
            mssg = 'Start latitude must be south of end latitude. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF

         IF (lon_val_end LT lon_val_beg) THEN BEGIN
            ; Make an exception for small regions spanning the dateline
            IF (lon_val_beg LT 175. OR lon_val_end GT -175.) THEN BEGIN
               mssg = ['Start longitude must not be greater than end longitude', $
                       'unless you use a range within the bounds +175 -> -175', $
                       'crossing the dateline. Try again.']
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
               RETURN
            ENDIF
         ENDIF

         IF (lat_val_beg LT (-LAT_MINMAX) OR lat_val_beg GT LAT_MINMAX OR $
             lat_val_end LT (-LAT_MINMAX) OR lat_val_end GT LAT_MINMAX) THEN BEGIN
            mssg = ['Absolute value of latitudes must not be greater than', $
                     STRTRIM(STRING(LAT_MINMAX),2) + ' degrees. Try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         IF (lon_val_beg LT (-LON_MINMAX) OR lon_val_beg GT LON_MINMAX OR $
             lon_val_end LT (-LON_MINMAX) OR lon_val_end GT LON_MINMAX) THEN BEGIN
            mssg = ['Absolute value of longitudes must not be greater than', $
                     STRTRIM(STRING(LON_MINMAX),2) + ' degrees. Try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF

         ;-----------------------------------------------------------------
         ; Process single lat/lon. Test values user entered.
         ;-----------------------------------------------------------------

      ENDIF ELSE BEGIN
         IF (~IsNumber(latval[0], 2)) THEN BEGIN
            mssg = 'Latitude value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lat_val = FLOAT(latval)

         IF (~IsNumber(lonval[0], 2)) THEN BEGIN
            mssg = 'Longitude value is not valid. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
         lon_val = FLOAT(lonval)
      ENDELSE

      timeval_save = [stim,etim]
      latval_save  = latval
      lonval_save  = lonval

      WIDGET_CONTROL, event.top, /DESTROY
      ListStatus = 0
      RETURN

   END

   ;-----------------------------------------------------------------------
   ; Here if user clicked Cancel button.
   ;-----------------------------------------------------------------------

   event.id EQ opass_struct.cancel_button : BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      ListStatus = -1
      RETURN
   END

;   event.id EQ opass_struct.help_button : BEGIN
; add PDF Help here
;   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=opass_struct, /NO_COPY
RETURN

cancelled:
ListStatus = -1
WIDGET_CONTROL, event.top, SET_UVALUE=opass_struct, /NO_COPY

END ; OverpassForOrder_eh

;***************************************************************************
PRO OverpassForOrderGui, LL_list, TM_list, Retval
;***************************************************************************

COMPILE_OPT IDL2

COMMON save_overpass_vals, List, GMToffset, timeval_save, latval_save, $
                           lonval_save, GMToffset_save, ListStatus

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base00 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
                     ' : MISR Overpass Finder', /TLB_KILL_REQUEST_EVENTS)
label1b = WIDGET_LABEL(base00, VALUE='Enter Dates and Locations to find ' + $
                       'MISR Overpass Orbits')

baseedits = WIDGET_BASE(base00, /ROW, /ALIGN_CENTER)
baseadd   = WIDGET_BASE(baseedits, /COLUMN, /ALIGN_CENTER, /FRAME)

labeltimeadd = WIDGET_LABEL(baseadd, VALUE=' Date or Date Range to Search')

;---------------------------------------------------------------------------
timeedit = CW_FIELD(baseadd, /STRING, VALUE=timeval_save, XSIZE=24, $
                    TITLE='Enter Date:  ')
;---------------------------------------------------------------------------

dummylabel2 = WIDGET_LABEL(baseadd, VALUE=' ')

;---------------------------------------------------------------------------
;gmtedit = CW_FIELD(baseadd, /INTEGER, VALUE=GMToffset_save, XSIZE=5, $
;                   TITLE='Enter Local Hours After GMT: ')
;
;labelhours1 = WIDGET_LABEL(baseadd, $
;                           VALUE='(If 0, Local time = GMT time; click Help)')
;---------------------------------------------------------------------------

labellocdum = WIDGET_LABEL(baseadd, VALUE=' ')
labellocadd = WIDGET_LABEL(baseadd, $
                           VALUE='   Lat/Lon or Lat/Lon Range to Search')

;---------------------------------------------------------------------------
loceditlat = CW_FIELD(baseadd, /STRING, VALUE=latval_save, XSIZE=20, $
                      TITLE='Enter Latitude:  ')
loceditlon = CW_FIELD(baseadd, /STRING, VALUE=lonval_save, XSIZE=20, $
                      TITLE='Enter Longitude: ')
;---------------------------------------------------------------------------

;baseaddrem = WIDGET_BASE(baseedits, /COLUMN)
;centerbuttons1 = WIDGET_LABEL(baseaddrem, VALUE=' ')
;centerbuttons2 = WIDGET_LABEL(baseaddrem, VALUE=' ')
;centerbuttons3 = WIDGET_LABEL(baseaddrem, VALUE=' ')
;centerbuttons4 = WIDGET_LABEL(baseaddrem, VALUE=' ')

;---------------------------------------------------------------------------
;addbutton = WIDGET_BUTTON(baseaddrem, VALUE='Add to list')
;rembutton = WIDGET_BUTTON(baseaddrem, VALUE='Remove from list')
;---------------------------------------------------------------------------

;baserem = WIDGET_BASE(baseedits, /COLUMN)
;title1 = '  Beg Date    End Date    Beg Lat   End Lat    Beg Lon   End Lon'
;labelrem1 = WIDGET_LABEL(baserem, VALUE=title1, /ALIGN_LEFT)

;---------------------------------------------------------------------------
;selects = WIDGET_LIST(baserem, UVALUE='', YSIZE=10, XSIZE=62)
;---------------------------------------------------------------------------

;dummylabel3 = WIDGET_LABEL(baserem, VALUE=' ')
;baseoch = WIDGET_BASE(baserem, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base00, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base00, VALUE='Cancel')
;help_button = WIDGET_BUTTON(baseoch, VALUE=' Help ')

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

opass_struct = { timeedit      : timeedit, $
                 loceditlat    : loceditlat, $
                 loceditlon    : loceditlon, $
                 ok_button     : ok_button, $
                 cancel_button : cancel_button }

;opass_struct = { timeedit      : timeedit, $
;                 loceditlat    : loceditlat, $
;                 loceditlon    : loceditlon, $
;                 addbutton     : addbutton, $
;                 rembutton     : rembutton, $
;                 gmtedit       : gmtedit, $
;                 selects       : selects, $
;                 ok_button     : ok_button, $
;                 cancel_button : cancel_button, $
;                 help_button   : help_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

LLlist = ['']
TMlist = ['']

WIDGET_CONTROL, base00, SET_UVALUE=opass_struct, XOFFSET=500, YOFFSET=250, $
                /NO_COPY
WIDGET_CONTROL, base00, /REALIZE
XMANAGER, 'OverpassForOrderGui', base00, EVENT_HANDLER='OverpassForOrder_eh'

Retval = ListStatus
IF (ListStatus LT 0) THEN RETURN

;-----------------------------------------------------------------------
; Extract the needed stime, etime, and beg and end lat, and lon.
;-----------------------------------------------------------------------

TM_list = timeval_save

IF STRMATCH(latval_save, '*:*') THEN BEGIN
   sepstr = STRSPLIT(latval_save, ': ', /EXTRACT)
   beglat = FLOAT(sepstr[0])
   endlat = FLOAT(sepstr[1])
   LL_list = [beglat,endlat]
ENDIF ELSE BEGIN
   LL_list = [FLOAT(latval_save),FLOAT(latval_save)]
ENDELSE

IF STRMATCH(lonval_save, '*:*') THEN BEGIN
   sepstr = STRSPLIT(lonval_save, ': ', /EXTRACT)
   beglon = FLOAT(sepstr[0])
   endlon = FLOAT(sepstr[1])
   LL_list = [LL_list,beglon,endlon]
ENDIF ELSE BEGIN
   LL_list = [LL_list,FLOAT(lonval_save),FLOAT(lonval_save)]
ENDELSE

GMToffset = 0.0

END ; OverpassForOrderGui

;***************************************************************************
PRO OverpassFinderTabs_eh, event
;***************************************************************************

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=opass_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=opass_struct, /NO_COPY
RETURN

END ; OverpassFinderTabs_eh

;***************************************************************************
PRO OverpassFinderTabs, LL_list, TM_list, Retval
;***************************************************************************

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; Initialize.
;---------------------------------------------------------------------------

OrbitList = ''
PathList  = ''
BlkBeg =   1
BlkEnd = !KON.Instr.NUM_BLOCKS
TimeBeg = ''
TimeEnd = ''
LatBeg = 0.0
LatEnd = 0.0
LonBeg = 0.0
LonEnd = 0.0

ProductNames = [ 'GRP_TERRAIN',    'FRP_ELLIPSOID', $
                 'GRP_RCCM',       'GP_GMP', $
                 'AS_AEROSOL',     'AS_AEROSOL_FIRSTLOOK', $
                 'AS_LAND',        'AS_LAND_FIRSTLOOK', $
                 'TC_STEREO',      'TC_STEREO_FIRSTLOOK', $
                 'TC_CLASSIFIERS', 'TC_CLASSIFIERS_FIRSTLOOK', $
                 'TC_ALBEDO',      'TC_ALBEDO_FIRSTLOOK' ]

;---------------------------------------------------------------------------
; Create the top-level base and the tab.
;---------------------------------------------------------------------------

wTLB = WIDGET_BASE(/COLUMN, /BASE_ALIGN_TOP, /TLB_KILL_REQUEST_EVENTS)
wTab = WIDGET_TAB(wTLB, LOCATION=0)

;---------------------------------------------------------------------------
; Create the "Orbits and Blocks" tab base and its controls.
;---------------------------------------------------------------------------

wT1 = WIDGET_BASE(wTab, TITLE='Orbits and Blocks', /COLUMN)
wT1B1 = WIDGET_BASE(wT1, /COLUMN)
wLabel = WIDGET_LABEL(wT1B1, VALUE='Enter orbit list (use ,) or ' + $
                      'orbit range (use -)')
wOrbits = WIDGET_TEXT(wT1B1, /EDITABLE)
wT1B2 = WIDGET_BASE(wT1, /ROW)
wBlkBegT1 = CW_FIELD(wT1B2, /INTEGER, TITLE='First block')
wBlkEndT1 = CW_FIELD(wT1B2, /INTEGER, TITLE='Last block')

;---------------------------------------------------------------------------
; Create the "Paths, Blocks and Times" tab base and its controls.
;---------------------------------------------------------------------------

wT2 = WIDGET_BASE(wTab, TITLE='Paths, Blocks and Times', /COLUMN)
wT2B1 = WIDGET_BASE(wT2, /COLUMN)
wLabel = WIDGET_LABEL(wT2B1, VALUE='Enter path list (use ,) or ' + $
                      'path range (use -)')
wPaths = WIDGET_TEXT(wT2B1, /EDITABLE)
wT2B2 = WIDGET_BASE(wT2, /ROW)
wBlkBegT2 = CW_FIELD(wT2B2, /INTEGER, TITLE='First block')
wBlkEndT2 = CW_FIELD(wT2B2, /INTEGER, TITLE='Last block')
wT2B3 = WIDGET_BASE(wT2, /ROW)
wTimeBegT2 = WIDGET_TEXT(wT2B1, /EDITABLE, VALUE=TimeBeg)
wTimeEndT2 = WIDGET_TEXT(wT2B1, /EDITABLE, VALUE=TimeEnd)

;---------------------------------------------------------------------------
; Create the "Lat/Lon and Times" tab base and its controls.
;---------------------------------------------------------------------------

wT3 = WIDGET_BASE(wTab, TITLE='Lat/Lon and Times', /COLUMN)
wT3B1 = WIDGET_BASE(wT3, /ROW)
wLatBegT3 = CW_FIELD(wT3B1, /FLOAT, TITLE='')
wLatEndT3 = CW_FIELD(wT3B1, /FLOAT, TITLE='')
wT3B2 = WIDGET_BASE(wT3, /ROW)
wLonBegT3 = CW_FIELD(wT3B2, /FLOAT, TITLE='')
wLonEndT3 = CW_FIELD(wT3B2, /FLOAT, TITLE='')
wT3B3 = WIDGET_BASE(wT3, /ROW)
wTimeBegT3 = WIDGET_TEXT(wT3B3, /EDITABLE, VALUE=TimeBeg)
wTimeEndT3 = WIDGET_TEXT(wT3B3, /EDITABLE, VALUE=TimeEnd)

;---------------------------------------------------------------------------
; Create a base widget to hold the buttons for product names to retrieve
; but only if this dialog was called to order MISR data.
;---------------------------------------------------------------------------

IF (1) THEN BEGIN
   wTxB1 = WIDGET_BASE(wTLB, /ROW)
   wProducts = CW_BGROUP(wTxB1, ProductNames, COLUMN=4, /NONEXCLUSIVE)
ENDIF

;---------------------------------------------------------------------------
; Create a base widget to hold the 'Done' button, and the button itself.
;---------------------------------------------------------------------------

wControl = WIDGET_BASE(wTLB, /ROW)
bOK = WIDGET_BUTTON(wControl, VALUE='OK')
bCancel = WIDGET_BUTTON(wControl, VALUE='Cancel')

;---------------------------------------------------------------------------
; Create an anonymous structure to hold widget IDs. This structure becomes
; the user value of the top-level base widget.
;---------------------------------------------------------------------------

struct = { wOrbits    : wOrbits,    $
           wBlkBegT1  : wBlkBegT1,  $
           wBlkEndT1  : wBlkEndT1,  $
           wPaths     : wPaths,     $
           wBlkBegT2  : wBlkBegT2,  $
           wBlkEndT2  : wBlkEndT2,  $
           wTimeBegT2 : wTimeBegT2, $
           wTimeEndT2 : wTimeEndT2, $
           wLatBegT3  : wLatBegT3,  $
           wLatEndT3  : wLatEndT3,  $
           wLonBegT3  : wLonBegT3,  $
           wLonEndT3  : wLonEndT3,  $
           wTimeBegT3 : wTimeBegT3, $
           wTimeEndT3 : wTimeEndT3, $
           wProducts  : wProducts,  $
           bOK        : bOK,        $
           bCancel    : bCancel }

;---------------------------------------------------------------------------
; Realize the widgets, set the user value of the top-level base, and call
; XMANAGER to manage everything.
;---------------------------------------------------------------------------

WIDGET_CONTROL, wTLB, /REALIZE
WIDGET_CONTROL, wTLB, SET_UVALUE=struct
XMANAGER, 'OverpassFinderTabs', wTLB, EVENT_HANDLER='OverpassFinderTabs_eh', $
          /NO_BLOCK

END  ;  OverpassFinderTabs

;***************************************************************************
PRO OrbitBrowseOption_eh, event
;***************************************************************************
; Event handler for user interface for setting orbit browse options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON orbit_browse_options, WidgetStructBrowse, BlockBeg, BlockEnd

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_browse, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WidgetStructBrowse.Cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_browse.button1 : BEGIN
      GetBrowseFile, DirName, FileName, PathNum, OrbitNum, BlockBeg, $
                     BlockEnd, CamName, Retval
      IF (Retval) THEN GOTO, skip91
      BrowseFile = DirName + !KON.Misc.Slash + FileName
      widget_struct_browse.BrowseFile = BrowseFile
      widget_struct_browse.PathNum = PathNum
      widget_struct_browse.OrbitNum = OrbitNum
      widget_struct_browse.BlockBeg = BlockBeg
      widget_struct_browse.BlockEnd = BlockEnd
      widget_struct_browse.CamName = CamName
      WIDGET_CONTROL, widget_struct_browse.label3a, SET_VALUE=DirName
      WIDGET_CONTROL, widget_struct_browse.label4a, SET_VALUE=FileName
      WIDGET_CONTROL, widget_struct_browse.integer5a, SET_VALUE=BlockBeg
      WIDGET_CONTROL, widget_struct_browse.integer5b, SET_VALUE=BlockEnd
      WIDGET_CONTROL, widget_struct_browse.label5b, $
                      SET_VALUE = (STRTRIM(STRING(BlockBeg),2) + ' - ' + $
                                   STRTRIM(STRING(BlockEnd),2))[0]
      skip91:
   END

   event.id EQ widget_struct_browse.button6a : BEGIN
   END

   event.id EQ widget_struct_browse.button6b : BEGIN
   END

   event.id EQ widget_struct_browse.button7a : BEGIN
      WIDGET_CONTROL, widget_struct_browse.button6a, SENSITIVE=1
   END

   event.id EQ widget_struct_browse.button7b : BEGIN
      WIDGET_CONTROL, widget_struct_browse.button6a, SENSITIVE=0
      WIDGET_CONTROL, widget_struct_browse.button6b, /SET_BUTTON
   END

   event.id EQ widget_struct_browse.ok_button : BEGIN
      IF (widget_struct_browse.OrbitNum EQ 0L) THEN BEGIN
         mssg = ['If you wish to exit without selecting', $
                 'a file, then click <Cancel>, not <OK>.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         BREAK
      ENDIF
      WIDGET_CONTROL, WidgetStructBrowse.label3a, GET_VALUE=DirName
      WIDGET_CONTROL, WidgetStructBrowse.label4a, GET_VALUE=FileName
      WidgetStructBrowse.BrowseFile = DirName + !KON.Misc.Slash + FileName
      WIDGET_CONTROL, widget_struct_browse.integer5a, GET_VALUE=temp
      IF (temp LT BlockBeg OR temp GT 180) THEN BEGIN
         mssg = ['The First Block must be between ' + $
                  STRTRIM(STRING(BlockBeg),2) + ' and 180. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      WidgetStructBrowse.BlockBeg = temp
      WIDGET_CONTROL, widget_struct_browse.integer5b, GET_VALUE=temp
      IF (temp GT BlockEnd OR temp LT 1) THEN BEGIN
         mssg = ['The Last Block must be between 1 and ' + $
            STRTRIM(STRING(BlockEnd),2) + '. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      WidgetStructBrowse.BlockEnd = temp
      IF (WidgetStructBrowse.BlockBeg GT WidgetStructBrowse.BlockEnd) THEN BEGIN
         mssg = ['The Last Block must be at least as large as ' + $
                 'the First Block. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      temp = WIDGET_INFO(widget_struct_browse.button7b, /BUTTON_SET)
      WidgetStructBrowse.Resolution = temp
      temp = WIDGET_INFO(widget_struct_browse.button6b, /BUTTON_SET)
      WidgetStructBrowse.BandChoice = temp
      WidgetStructBrowse.CamName = widget_struct_browse.CamName
      WidgetStructBrowse.PathNum = widget_struct_browse.PathNum
      WidgetStructBrowse.OrbitNum = widget_struct_browse.OrbitNum
      WidgetStructBrowse.Cancel = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_browse.cancel_button : BEGIN
      WidgetStructBrowse.Cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_browse.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_ShowCameraImage.pdf'
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_browse, /NO_COPY

END ; OrbitBrowseOption_eh

;***************************************************************************
PRO OrbitBrowseOption_gui, BrowseFile, PathNum, OrbitNum, BlockBeg, $
                           BlockEnd, CamName, BandChoice, Resolution, $
                           Cancel
;***************************************************************************
; User interface for setting orbit browse options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON orbit_browse_options, WidgetStructBrowse, BlockMin, BlockMax

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

Cancel = 0
BrowseFile = ''
PathNum = 1
OrbitNum = 1L
BlockBeg = 1
BlockEnd = !KON.Instr.NUM_BLOCKS
CamName = ''
BandChoice = 1  ;  0 -> NIR/Green/Blue   1 -> Red/Green/Blue
Resolution = 0  ;  0 -> 512x128          1 -> 2048x512

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
                    ' : MISR Image Browse', /TLB_KILL_REQUEST_EVENTS)
label0 = WIDGET_LABEL( base0, VALUE='Show Camera Image Options' )

base1 = WIDGET_BASE( base0, /COLUMN, /FRAME, XSIZE=350 )
button1 = WIDGET_BUTTON( base1, /ALIGN_CENTER, $
          VALUE='Select MISR Level 1 Terrain or Ellipsoid File')
base2 = WIDGET_BASE( base1, /COLUMN )
base3 = WIDGET_BASE( base2, /ROW )
label3 = WIDGET_LABEL( base3, VALUE='Directory:' )
label3a = WIDGET_TEXT( base3, XSIZE=40, $
                       VALUE='select filename by clicking button above')
base4 = WIDGET_BASE( base2, /ROW )
label4 = WIDGET_LABEL( base4, VALUE='File Name:' )
ffname = '.......................................................'
label4a = WIDGET_TEXT( base4, XSIZE=40)  ;  , VALUE=ffname)

base5 = WIDGET_BASE( base0, /COLUMN, /FRAME )
base5a = WIDGET_BASE( base5, /ROW )
label5a = WIDGET_LABEL( base5a, VALUE='Select Block Range:' )
label5b = WIDGET_LABEL( base5a, VALUE='<no file loaded>', /DYNAMIC_RESIZE )
base5b = WIDGET_BASE( base5, /ROW )
integer5a = CW_FIELD( base5b, VALUE='', /INTEGER, TITLE='First Block:', XSIZE=5 )
integer5b = CW_FIELD( base5b, VALUE='', /INTEGER, TITLE='Last Block:', XSIZE=5 )

base7 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label7 = WIDGET_LABEL( base7, VALUE='Select Resolution:', /ALIGN_LEFT )
base7a = WIDGET_BASE( base7, /ROW, /EXCLUSIVE )
button7a = WIDGET_BUTTON( base7a, VALUE='512x128 (faster)' )
button7b = WIDGET_BUTTON( base7a, VALUE='2048x512 (slower)' )

base6 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label6 = WIDGET_LABEL( base6, VALUE='Select RGB Bands:', /ALIGN_LEFT )
base6a = WIDGET_BASE( base6, /ROW, /EXCLUSIVE )
button6a = WIDGET_BUTTON( base6a, VALUE='NIR/Green/Blue (faster)' )
button6b = WIDGET_BUTTON( base6a, VALUE='Red/Green/Blue (slower)' )

basex = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( basex, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( basex, VALUE='Cancel' )
help_button = WIDGET_BUTTON( basex, VALUE='PDF Help' )

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct_browse = { $
   button1 : button1, $
   label3a : label3a, $
   label4a : label4a, $
   integer5a : integer5a, $
   integer5b : integer5b, $
   label5b : label5b, $
   button6a : button6a, $
   button6b : button6b, $
   button7a : button7a, $
   button7b : button7b, $
   ok_button : ok_button, $
   cancel_button : cancel_button, $
   help_button : help_button, $
   BrowseFile : BrowseFile, $
   PathNum : PathNum, $
   OrbitNum : OrbitNum, $
   BlockBeg : BlockBeg, $
   BlockEnd : BlockEnd, $
   CamName : CamName, $
   BandChoice : BandChoice, $
   Resolution : Resolution, $
   Cancel : Cancel }

HELP, /STRUCTURE, WidgetStructBrowse, OUTPUT=exists

IF (STRTRIM(exists[1],2) EQ 'UNDEFINED = <Undefined>') THEN BEGIN
   WidgetStructBrowse = widget_struct_browse
ENDIF ELSE BEGIN
   WidgetStructBrowse.button1 = widget_struct_browse.button1
   WidgetStructBrowse.label3a = widget_struct_browse.label3a
   WidgetStructBrowse.label4a = widget_struct_browse.label4a
   WidgetStructBrowse.integer5a = widget_struct_browse.integer5a
   WidgetStructBrowse.integer5b = widget_struct_browse.integer5b
   WidgetStructBrowse.label5b = widget_struct_browse.label5b
   WidgetStructBrowse.button6a = widget_struct_browse.button6a
   WidgetStructBrowse.button6b = widget_struct_browse.button6b
   WidgetStructBrowse.button7a = widget_struct_browse.button7a
   WidgetStructBrowse.button7b = widget_struct_browse.button7b
   WidgetStructBrowse.ok_button = widget_struct_browse.ok_button
   WidgetStructBrowse.cancel_button = widget_struct_browse.cancel_button
   WidgetStructBrowse.help_button = widget_struct_browse.help_button
   widget_struct_browse.BrowseFile = WidgetStructBrowse.BrowseFile
   widget_struct_browse.PathNum  = WidgetStructBrowse.PathNum
   widget_struct_browse.OrbitNum = WidgetStructBrowse.OrbitNum
   widget_struct_browse.BlockBeg = WidgetStructBrowse.BlockBeg
   widget_struct_browse.BlockEnd = WidgetStructBrowse.BlockEnd
   widget_struct_browse.CamName  = WidgetStructBrowse.CamName
   widget_struct_browse.BandChoice = WidgetStructBrowse.BandChoice
   widget_struct_browse.Resolution = WidgetStructBrowse.Resolution
   widget_struct_browse.Cancel = WidgetStructBrowse.Cancel
ENDELSE

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

dir_name = '     select filename by clicking button above     '
fil_name = '                                                  '
slen = STRLEN(widget_struct_browse.BrowseFile)
IF (slen NE 0) THEN BEGIN
   spos = STRPOS(widget_struct_browse.BrowseFile, !KON.Misc.Slash, $
                 /REVERSE_SEARCH)
   IF (spos GT 0) THEN BEGIN
      dir_name = STRMID(widget_struct_browse.BrowseFile, 0, spos)
      fil_name = STRMID(widget_struct_browse.BrowseFile, spos+1, slen-spos-1)
   ENDIF
ENDIF
WIDGET_CONTROL, label3a, SET_VALUE=dir_name
WIDGET_CONTROL, label4a, SET_VALUE=fil_name
WIDGET_CONTROL, integer5a, SET_VALUE=widget_struct_browse.BlockBeg
WIDGET_CONTROL, integer5b, SET_VALUE=widget_struct_browse.BlockEnd

IF (widget_struct_browse.Resolution EQ 0) THEN BEGIN
   WIDGET_CONTROL, button7a, /SET_BUTTON
   WIDGET_CONTROL, button6a, SENSITIVE=1
ENDIF ELSE BEGIN
   WIDGET_CONTROL, button7b, /SET_BUTTON
   WIDGET_CONTROL, button6a, SENSITIVE=0
   WIDGET_CONTROL, button6b, /SET_BUTTON
ENDELSE

IF (widget_struct_browse.BandChoice EQ 0) THEN BEGIN
   WIDGET_CONTROL, button6a, /SET_BUTTON
ENDIF ELSE BEGIN
   WIDGET_CONTROL, button6b, /SET_BUTTON
ENDELSE

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_browse, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'OrbitBrowseOption_gui', base0, EVENT_HANDLER='OrbitBrowseOption_eh'

BrowseFile = WidgetStructBrowse.BrowseFile
PathNum = WidgetStructBrowse.PathNum
OrbitNum = WidgetStructBrowse.OrbitNum
BlockBeg = WidgetStructBrowse.BlockBeg
BlockEnd = WidgetStructBrowse.BlockEnd
CamName = WidgetStructBrowse.CamName
BandChoice = WidgetStructBrowse.BandChoice
Resolution = WidgetStructBrowse.Resolution

Cancel = WidgetStructBrowse.Cancel

widget_struct_browse = 0

END ; OrbitBrowseOption_gui

;***************************************************************************
PRO OrbitAnimateOption_eh, event
;***************************************************************************
; Event handler for user interface for setting configurable parameter values.
; What a mess this is, especially because of the 2 orbit condition!
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON input_data_config, coord_struct, WidgetStructAnimate
COMMON temp_save, min_ok_blk, max_ok_blk
COMMON cheap_trick, coord_struct_save, orbit1path, orbit2path

IF ~ ISA(coord_struct_save) THEN coord_struct_save = coord_struct
IF ~ ISA(orbit1path) THEN orbit1path = 0
IF ~ ISA(orbit2path) THEN orbit2path = 0

WIDGET_CONTROL, /HOURGLASS
WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_animate, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WidgetStructAnimate.config_temp.cancel_flag = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;------------------------------------------------------------------------
; Branch to correct widget.
;------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_animate.button1a : BEGIN  ; 1 Band (Black & White)
      WidgetStructAnimate.config_temp.band_count = 1
      END

   event.id EQ widget_struct_animate.button1b : BEGIN  ; 3 Bands (RGB Color)
      WidgetStructAnimate.config_temp.band_count = 4
      END

   event.id EQ widget_struct_animate.button2a : BEGIN  ; 1 Orbit
      WidgetStructAnimate.config_temp.orbit_count = 1
      WidgetStructAnimate.config_temp.ncam = 9
      WidgetStructAnimate.config_temp.cam_files[9:17] = ''
      WIDGET_CONTROL, widget_struct_animate.button5,  SENSITIVE=0
      WIDGET_CONTROL, widget_struct_animate.label6a2, SENSITIVE=0
      WIDGET_CONTROL, widget_struct_animate.label6b2, SENSITIVE=0

      ; Set the available block range for orbit 1.

      WIDGET_CONTROL, widget_struct_animate.label7a, $
         SET_VALUE = 'Available Blocks:'
      WidgetStructAnimate.config_temp.first_block = coord_struct.(0).BlkBeg
      WidgetStructAnimate.config_temp.last_block  = coord_struct.(0).BlkEnd
      WIDGET_CONTROL, widget_struct_animate.label7b, $
         SET_VALUE = STRTRIM(coord_struct.(0).BlkBeg,2) + $
                             ' - ' + STRTRIM(coord_struct.(0).BlkEnd,2)
      WIDGET_CONTROL, widget_struct_animate.text8a, $
         SET_VALUE = STRTRIM(coord_struct.(0).BlkBeg,2)
      WIDGET_CONTROL, widget_struct_animate.text8b, $
         SET_VALUE = STRTRIM(coord_struct.(0).BlkEnd,2)
      END

   event.id EQ widget_struct_animate.button2b : BEGIN  ; 2 Orbits
      WidgetStructAnimate.config_temp.orbit_count = 2
      WidgetStructAnimate.config_temp.ncam = 18
      WIDGET_CONTROL, widget_struct_animate.button5,  SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.label6a2, SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.label6b2, SENSITIVE=1

      ; Set the available overlapping block range of the 2 orbits.

      WIDGET_CONTROL, widget_struct_animate.label7a, $
         SET_VALUE = 'Available Overlapping Blocks:'
      WidgetStructAnimate.config_temp.first_block = $
         coord_struct.(0).BlkBeg > coord_struct.(1).BlkBeg
      WidgetStructAnimate.config_temp.last_block = $
         coord_struct.(0).BlkEnd < coord_struct.(1).BlkEnd
      WIDGET_CONTROL, widget_struct_animate.label7b, $
         SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2) + $
                             ' - ' + STRTRIM(coord_struct.(1).BlkEnd,2)
      WIDGET_CONTROL, widget_struct_animate.text8a, $
         SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2)
      WIDGET_CONTROL, widget_struct_animate.text8b, $
         SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.last_block,2)
      END

;-------------------------------------------------------------------------------
   event.id EQ widget_struct_animate.button3 : BEGIN  ; Select Orbit 1 Nadir HDF
      whichorbit = 0
      ncam = WidgetStructAnimate.config_temp.ncam
      cam_files = WidgetStructAnimate.config_temp.cam_files
      orbit_num = WidgetStructAnimate.config_temp.orbit_num
      misr_airmisr = WidgetStructAnimate.config_temp.misr_airmisr

      GetCamFiles, 1, cam_files, orbit_num, misr_airmisr, ncam, $
                   whichorbit, [1,1,1,1,1,1,1,1,1], Retval

      IF (Retval) THEN GOTO, skip1
      WidgetStructAnimate.config_temp.ncam = ncam
      WidgetStructAnimate.config_temp.cam_files = cam_files
      WidgetStructAnimate.config_temp.orbit_num = orbit_num
      WidgetStructAnimate.config_temp.misr_airmisr = misr_airmisr

      camndx = 4

      IF (WidgetStructAnimate.config_temp.misr_airmisr EQ 0) THEN BEGIN

         GetOrbitParams_part1, WidgetStructAnimate.config_temp.cam_files, $
                               coord_struct, whichorbit, BlockULCx, $
                               BlockULCy, BlockLRCx, BlockLRCy, PathNum, $
                               Retval
         orbit1path = PathNum
         
         IF (Retval LT 0) THEN BEGIN
            WidgetStructAnimate.config_temp.cam_files[0:8] = ''
            GOTO, skip1
         ENDIF

         ; Error check the path number if we're loading 2 orbits and Orbit 2
         ; has already been selected.
         IF (WidgetStructAnimate.config_temp.orbit_count EQ 2 AND $
            orbit2path GT 0) THEN BEGIN
            IF (orbit1path NE orbit2path) THEN BEGIN
               mssg = ['Both orbits must be on the same MISR path.', 'Try again.']
               rval = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
               orbit1path = coord_struct_save.(whichorbit).PathNum
               coord_struct.(whichorbit) = coord_struct_save.(whichorbit)
               WIDGET_CONTROL, widget_struct_animate.label4a2, $
                  SET_VALUE = 'select filename by clicking button above'
               WIDGET_CONTROL, widget_struct_animate.label4b2, SET_VALUE = ''
               WidgetStructAnimate.config_temp.cam_files[0:8] = ''
               GOTO, skip1
            ENDIF
         ENDIF
         
         coord_struct.(whichorbit).PathNum = PathNum
         WidgetStructAnimate.config_temp.path_num = $
            coord_struct.(whichorbit).PathNum
         WidgetStructAnimate.config_temp.date_str = $
            coord_struct.(whichorbit).OrbitDate
            
         nelem = N_ELEMENTS(BlockULCx)
         WidgetStructAnimate.BlockULCx[0:nelem-1] = BlockULCx
         WidgetStructAnimate.BlockULCy[0:nelem-1] = BlockULCy
         WidgetStructAnimate.BlockLRCx[0:nelem-1] = BlockLRCx
         WidgetStructAnimate.BlockLRCy[0:nelem-1] = BlockLRCy
         BlockULCx = 0
         BlockULCy = 0
         BlockLRCx = 0
         BlockLRCy = 0

         ; Set the available block range.

         IF (WidgetStructAnimate.config_temp.orbit_count EQ 1) THEN BEGIN
            WidgetStructAnimate.config_temp.first_block = coord_struct.(0).BlkBeg
            WidgetStructAnimate.config_temp.last_block = coord_struct.(0).BlkEnd
         ENDIF ELSE BEGIN
            WidgetStructAnimate.config_temp.first_block = $
               coord_struct.(0).BlkBeg > coord_struct.(1).BlkBeg
            WidgetStructAnimate.config_temp.last_block = $
               coord_struct.(0).BlkEnd < coord_struct.(1).BlkEnd
         ENDELSE
         WIDGET_CONTROL, widget_struct_animate.label7b, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2) + $
                ' - ' + STRTRIM(WidgetStructAnimate.config_temp.last_block,2)
         WIDGET_CONTROL, widget_struct_animate.text8a, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2)
         WIDGET_CONTROL, widget_struct_animate.text8b, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.last_block,2)
         min_ok_blk = WidgetStructAnimate.config_temp.first_block
         max_ok_blk = WidgetStructAnimate.config_temp.last_block

      ENDIF

      WIDGET_CONTROL, widget_struct_animate.button2a, SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.button2b, SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.text8a,   SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.text8b,   SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.button1a, SENSITIVE=1
      WIDGET_CONTROL, widget_struct_animate.button1b, SENSITIVE=1

      IF (WidgetStructAnimate.config_temp.misr_airmisr LT 0) THEN BEGIN

         ; ** THIS IS WHERE A SESSION IS RESTORED DURING STARTUP **

         Op_RestoreSession, 0, Retval, CrdStrct, $
                            WidgetStructAnimate.config_temp.cam_files[0]

         IF (!VAR.NumOrbits EQ 1) THEN $
            WidgetStructAnimate.config_temp.cam_files[9:17] = ''

         IF (Retval) THEN GOTO, skip1
         coord_struct = CrdStrct

         IF (coord_struct.(1).OrbitNum NE 0) THEN BEGIN
            WIDGET_CONTROL, widget_struct_animate.button2b, /SET_BUTTON
            WidgetStructAnimate.config_temp.ncam = 18
         ENDIF

         WIDGET_CONTROL, widget_struct_animate.button2a, SENSITIVE=0
         WIDGET_CONTROL, widget_struct_animate.button2b, SENSITIVE=0
         WIDGET_CONTROL, widget_struct_animate.text8a,   SENSITIVE=0
         WIDGET_CONTROL, widget_struct_animate.text8b,   SENSITIVE=0
         WIDGET_CONTROL, widget_struct_animate.button1a, SENSITIVE=0
         WIDGET_CONTROL, widget_struct_animate.button1b, SENSITIVE=0

         WidgetStructAnimate.config_temp.first_block = coord_struct.(0).BlkBeg
         WidgetStructAnimate.config_temp.last_block  = coord_struct.(0).BlkEnd
         min_ok_blk = WidgetStructAnimate.config_temp.first_block
         max_ok_blk = WidgetStructAnimate.config_temp.last_block
         WIDGET_CONTROL, widget_struct_animate.label7b, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2) + $
                ' - ' + STRTRIM(WidgetStructAnimate.config_temp.last_block,2)
         WIDGET_CONTROL, widget_struct_animate.text8a, $
            SET_VALUE = STRTRIM(coord_struct.(0).BlkBeg,2)
         WIDGET_CONTROL, widget_struct_animate.text8b, $
            SET_VALUE = STRTRIM(coord_struct.(0).BlkEnd,2)
         IF (coord_struct.(0).num_band EQ 1) THEN BEGIN
            WIDGET_CONTROL, widget_struct_animate.button1a, /SET_BUTTON
            WidgetStructAnimate.config_temp.band_count = 1
         ENDIF
         IF (coord_struct.(0).num_band EQ 4) THEN BEGIN
            WIDGET_CONTROL, widget_struct_animate.button1b, /SET_BUTTON
            WidgetStructAnimate.config_temp.band_count = 4
         ENDIF

         camndx = 0

      ENDIF

      ichr = STRPOS(WidgetStructAnimate.config_temp.cam_files[camndx], $
                    !KON.Misc.Slash, /REVERSE_SEARCH)
      dir_comp = STRMID(WidgetStructAnimate.config_temp.cam_files[camndx], $
                        0, ichr+1)
      file_comp = STRMID(WidgetStructAnimate.config_temp.cam_files[camndx], $
                         ichr+1)

      WIDGET_CONTROL, widget_struct_animate.label4a2, SET_VALUE = dir_comp
      WIDGET_CONTROL, widget_struct_animate.label4b2, SET_VALUE = file_comp

      skip1:
      END

;-------------------------------------------------------------------------------
   event.id EQ widget_struct_animate.button5 : BEGIN  ; Select Orbit 2 Nadir HDF
      whichorbit = 1
      ncam = WidgetStructAnimate.config_temp.ncam
      cam_files = WidgetStructAnimate.config_temp.cam_files
      orbit_num = WidgetStructAnimate.config_temp.orbit_num
      misr_airmisr = WidgetStructAnimate.config_temp.misr_airmisr

      GetCamFiles, 1, cam_files, orbit_num, $
                   WidgetStructAnimate.config_temp.misr_airmisr, $
                   WidgetStructAnimate.config_temp.ncam, $
                   whichorbit, [1,1,1,1,1,1,1,1,1], Retval

      IF (Retval) THEN GOTO, skip2
      WidgetStructAnimate.config_temp.ncam = ncam
      WidgetStructAnimate.config_temp.cam_files = cam_files
      WidgetStructAnimate.config_temp.orbit_num = orbit_num
      WidgetStructAnimate.config_temp.misr_airmisr = misr_airmisr

      IF (WidgetStructAnimate.config_temp.misr_airmisr EQ 0) THEN BEGIN

         GetOrbitParams_part1, WidgetStructAnimate.config_temp.cam_files, $
                               coord_struct, whichorbit, BlockULCx, $
                               BlockULCy, BlockLRCx, BlockLRCy, PathNum, $
                               Retval
         orbit2path = PathNum

         IF (Retval LT 0) THEN BEGIN
            WidgetStructAnimate.config_temp.cam_files[9:17] = ''
            GOTO, skip2
         ENDIF

         ; Error check the path number if Orbit 1 has already been selected.
         IF (orbit1path GT 0) THEN BEGIN
            IF (orbit1path NE orbit2path) THEN BEGIN
               mssg = ['Both orbits must be on the same MISR path.', 'Try again.']
               rval = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
               orbit2path = coord_struct_save.(whichorbit).PathNum
               coord_struct.(whichorbit) = coord_struct_save.(whichorbit)
               WIDGET_CONTROL, widget_struct_animate.label6a2, $
                      SET_VALUE = 'select filename by clicking button above'
               WIDGET_CONTROL, widget_struct_animate.label6b2, SET_VALUE = ''
               WidgetStructAnimate.config_temp.cam_files[9:17] = ''
               GOTO, skip2
            ENDIF
         ENDIF
                     
         !VAR.CurrFiles.CamFiles[9:17] = cam_files[0:8]

         coord_struct.(whichorbit).PathNum = PathNum
         WidgetStructAnimate.config_temp.path_num = $
            coord_struct.(whichorbit).PathNum
         WidgetStructAnimate.config_temp.date_str = $
            coord_struct.(whichorbit).OrbitDate
            
         nelem = N_ELEMENTS(BlockULCx)     
         WidgetStructAnimate.BlockULCx[0:nelem-1] = BlockULCx
         WidgetStructAnimate.BlockULCy[0:nelem-1] = BlockULCy
         WidgetStructAnimate.BlockLRCx[0:nelem-1] = BlockLRCx
         WidgetStructAnimate.BlockLRCy[0:nelem-1] = BlockLRCy
         BlockULCx = 0
         BlockULCy = 0
         BlockLRCx = 0
         BlockLRCy = 0

         ; Set the available overlapping block range.

         WidgetStructAnimate.config_temp.first_block = $
            coord_struct.(0).BlkBeg > coord_struct.(1).BlkBeg
         WidgetStructAnimate.config_temp.last_block = $
            coord_struct.(0).BlkEnd < coord_struct.(1).BlkEnd
         WIDGET_CONTROL, widget_struct_animate.label7b, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2) + $
                ' - ' + STRTRIM(WidgetStructAnimate.config_temp.last_block,2)
         WIDGET_CONTROL, widget_struct_animate.text8a, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.first_block,2)
         WIDGET_CONTROL, widget_struct_animate.text8b, $
            SET_VALUE = STRTRIM(WidgetStructAnimate.config_temp.last_block,2)

      ENDIF

      ichr = STRPOS(WidgetStructAnimate.config_temp.cam_files[4+9], $
                    !KON.Misc.Slash, /REVERSE_SEARCH)
      dir_comp = STRMID(WidgetStructAnimate.config_temp.cam_files[4+9], $
                        0, ichr+1)
      file_comp = STRMID(WidgetStructAnimate.config_temp.cam_files[4+9], $
                         ichr+1)

      WIDGET_CONTROL, widget_struct_animate.label6a2, SET_VALUE = dir_comp
      WIDGET_CONTROL, widget_struct_animate.label6b2, SET_VALUE = file_comp

      skip2:
      END

   event.id EQ widget_struct_animate.ok_button : BEGIN  ; OK to accept new values.

      WIDGET_CONTROL, widget_struct_animate.text8a, GET_VALUE = blk1val
      IF (blk1val LT 0) THEN BEGIN
         ; SECRET code to change MINX to use Development version -
         ;        set beginning block to negative value
         IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
            mssg = 'MINX is already in Development mode'
            rval = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         ENDIF ELSE BEGIN
            !VAR.ProdOrDev = !KON.Misc.MINX_DEV_VER
            mssg = 'MINX has been reset to Development mode'
            rval = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         ENDELSE
         BREAK
      ENDIF
   
      IF (WidgetStructAnimate.config_temp.cam_files[0] EQ '') THEN BEGIN
         mssg = 'Orbit 1 filename has not yet been selected.'
         rval = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (WidgetStructAnimate.config_temp.ncam EQ 18 AND $
          WidgetStructAnimate.config_temp.cam_files[9] EQ '') THEN BEGIN
         mssg = 'Orbit 2 filename has not yet been selected.'
         rval = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
   
      ; Set block range values.

      WIDGET_CONTROL, widget_struct_animate.text8a, GET_VALUE = BlkBeg
      WIDGET_CONTROL, widget_struct_animate.text8b, GET_VALUE = BlkEnd
      IF (BlkBeg LT min_ok_blk OR BlkBeg GT max_ok_blk OR $
          BlkBeg GT BlkEnd) THEN BEGIN
          mssg = ['First Block must be between ' + $
                 STRTRIM(STRING(min_ok_blk),2) + ' and ' + $
                 STRTRIM(STRING(max_ok_blk),2) + ' and <= Last Block.', $
                 'Fix the problem and try again.']
          rval = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         BREAK
      ENDIF
      IF (BlkEnd LT min_ok_blk OR BlkEnd GT max_ok_blk OR $
          BlkEnd LT BlkBeg) THEN BEGIN
          mssg = ['Last Block must be between ' + $
                  STRTRIM(STRING(min_ok_blk),2) + ' and ' + $
                  STRTRIM(STRING(max_ok_blk),2) + ' and >= First Block.', $
                 'Fix the problem and try again.']
          rval = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         BREAK
      ENDIF
      WidgetStructAnimate.config_temp.first_block = BlkBeg
      WidgetStructAnimate.config_temp.last_block  = BlkEnd

      whichorbit = 0
      
      IF (WidgetStructAnimate.config_temp.misr_airmisr EQ 0) THEN BEGIN

         ; Added next test to prevent error when the same orbit is reloaded
         ; in animation dialog without making changes to dialog box entries.

         IF (coord_struct.(whichorbit).PathNum EQ 0) THEN BEGIN
            GetOrbitParams_part1, WidgetStructAnimate.config_temp.cam_files, $
                                  coord_struct, whichorbit, BlockULCx, $
                                  BlockULCy, BlockLRCx, BlockLRCy, PathNum, $
                                  Retval
            IF (Retval LT 0) THEN BREAK
         ENDIF

         coord_struct.(whichorbit).BlkBeg = BlkBeg
         coord_struct.(whichorbit).BlkEnd = BlkEnd
         coord_struct.(whichorbit).NumBlk = BlkEnd - BlkBeg + 1

         GetOrbitParams_part2, coord_struct, Retval, whichorbit, $
                               WidgetStructAnimate.BlockULCx, $
                               WidgetStructAnimate.BlockULCy, $
                               WidgetStructAnimate.BlockLRCx, $
                               WidgetStructAnimate.BlockLRCy

         IF WidgetStructAnimate.config_temp.ncam EQ 18 THEN BEGIN

            ; Added next test to prevent error when the same orbit is reloaded
            ; in animation dialog without making changes to dialog box entries.

            whichorbit = 1
            
            IF (coord_struct.(whichorbit).PathNum EQ 0) THEN BEGIN
               GetOrbitParams_part1, WidgetStructAnimate.config_temp.cam_files, $
                                     coord_struct, whichorbit, BlockULCx, $
                                     BlockULCy, BlockLRCx, BlockLRCy, PathNum, $
                                     Retval
               IF (Retval LT 0) THEN BREAK
            ENDIF

            coord_struct.(whichorbit).BlkBeg = BlkBeg
            coord_struct.(whichorbit).BlkEnd = BlkEnd
            coord_struct.(whichorbit).NumBlk = BlkEnd - BlkBeg + 1

            GetOrbitParams_part2, coord_struct, Retval, whichorbit, $
                                  WidgetStructAnimate.BlockULCx, $
                                  WidgetStructAnimate.BlockULCy, $
                                  WidgetStructAnimate.BlockLRCx, $
                                  WidgetStructAnimate.BlockLRCy
         ENDIF

      ENDIF

      ; Set number of bands.

      coord_struct.(0).num_band = WidgetStructAnimate.config_temp.band_count
      coord_struct.(1).num_band = WidgetStructAnimate.config_temp.band_count

      ; Set band number for 1st band to red band.

      coord_struct.(0).band_ndx[0] = 2
      coord_struct.(1).band_ndx[0] = 2

      ; Set band number for 2nd and 3rd bands if selected to green and blue.

      IF (WidgetStructAnimate.config_temp.band_count EQ 4) THEN BEGIN
         coord_struct.(0).band_ndx[1] = 1
         coord_struct.(1).band_ndx[1] = 1
         coord_struct.(0).band_ndx[2] = 0
         coord_struct.(1).band_ndx[2] = 0
      ENDIF

      !VAR.NumOrbits = 1
      ndxs = WHERE(WidgetStructAnimate.config_temp.cam_files NE '', numndxs)
      IF (numndxs EQ 18) THEN !VAR.NumOrbits = 2

      WidgetStructAnimate.config_temp.cancel_flag = 0

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN

   END

   event.id EQ widget_struct_animate.cancel_button : BEGIN  ; Cancel dialog box
      WidgetStructAnimate.config_temp.cancel_flag = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   ; Reset begin and end blocks to defaults.
   event.id EQ widget_struct_animate.reset_blocks : BEGIN
      WIDGET_CONTROL, widget_struct_animate.text8a, SET_VALUE = min_ok_blk
      WIDGET_CONTROL, widget_struct_animate.text8b, SET_VALUE = max_ok_blk
   END

   event.id EQ widget_struct_animate.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_AnimateCameras.pdf'
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_animate, /NO_COPY

END ; OrbitAnimateOption_eh

;***************************************************************************
PRO OrbitAnimateOption_gui, base0, NumCam, CamFiles, CoordStruct, OrbitNum, $
                            BlockBeg, BlockEnd, MISR_or_AirMISR, Retval
;***************************************************************************
; User interface for setting configurable parameter values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON input_data_config, coord_struct, WidgetStructAnimate

Cancel = 0
Retval = 0
coord_struct = CoordStruct

ncam = 9
band_count = 4

band_ndx = [3,2,1]
orbit_count = 1
first_block = 1
last_block = !KON.Instr.NUM_BLOCKS
cam_files = STRARR(18)
orbit_num = LONARR(2)
path_num = 0
date_str = ''
misr_airmisr = 0
cancel_flag = 0

; default structure
config_defaults = { $
   ncam         : ncam, $
   band_count   : band_count, $
   band_ndx     : band_ndx, $
   orbit_count  : orbit_count, $
   first_block  : first_block, $
   last_block   : last_block, $
   cam_files    : cam_files, $
   cancel_flag  : cancel_flag, $
   orbit_num    : orbit_num, $
   path_num     : path_num, $
   date_str     : date_str, $
   misr_airmisr : misr_airmisr }

; temp struct for temporarily storing toggle values.
config_temp = config_defaults

base0 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
                    ' : Animate Cameras', /TLB_KILL_REQUEST_EVENTS)
label0a = WIDGET_LABEL( base0, VALUE='Animate Cameras Options' )

base2x = WIDGET_BASE( base0, /COLUMN, /FRAME )

base2a = WIDGET_BASE( base2x, /ROW, /EXCLUSIVE )
button2a = WIDGET_BUTTON( base2a, VALUE='1 Orbit' )
button2b = WIDGET_BUTTON( base2a, VALUE='2 Orbits' )

base3 = WIDGET_BASE( base2x, /COLUMN )
button3 = WIDGET_BUTTON( base3, VALUE='Select Orbit 1 Nadir HDF File, ' + $
                         'or .sav Restore File' )

base4 = WIDGET_BASE( base3, /COLUMN )
base4a = WIDGET_BASE( base4, /ROW )
label4a1 = WIDGET_LABEL( base4a, VALUE='Directory:' )
label4a2 = WIDGET_TEXT( base4a, XSIZE=40, $
                        VALUE='select filename by clicking button above' )
base4b = WIDGET_BASE( base4, /ROW )
label4b1 = WIDGET_LABEL( base4b, VALUE='File Name:' )
ffname = '.......................................................'
label4b2 = WIDGET_TEXT( base4b, XSIZE=40 )  ;  , VALUE=ffname)

base5 = WIDGET_BASE( base2x, /COLUMN )
button5 = WIDGET_BUTTON( base5, VALUE='Select Orbit 2 Nadir HDF File' )

base6 = WIDGET_BASE( base5, /COLUMN )
base6a = WIDGET_BASE( base6, /ROW )
label6a1 = WIDGET_LABEL( base6a, VALUE='Directory:' )
label6a2 = WIDGET_TEXT( base6a, XSIZE=40, $
                        VALUE='select filename by clicking button above')
base6b = WIDGET_BASE( base6, /ROW )
label6b1 = WIDGET_LABEL( base6b, VALUE='File Name:' )
label6b2 = WIDGET_TEXT( base6b, XSIZE=40 )  ;  , VALUE=ffname)

base3x = WIDGET_BASE( base0, /COLUMN, /FRAME )

base7 = WIDGET_BASE( base3x, /ROW )
label7a = WIDGET_LABEL( base7, VALUE='Select Block Range:          ' )
label7b = WIDGET_LABEL( base7, VALUE='<no file loaded>')

base8 = WIDGET_BASE( base3x, /ROW )
text8a = CW_FIELD( base8, VALUE=config_defaults.first_block, $
                   XSIZE=5, /INTEGER, TITLE='First Block:' )
text8b = CW_FIELD( base8, VALUE=config_defaults.last_block, $
                   XSIZE=5, /INTEGER, TITLE='Last Block:' )

base4x = WIDGET_BASE( base0, /COLUMN, /FRAME )
label1a = WIDGET_LABEL( base4x, VALUE='Image Type:', /ALIGN_LEFT )
base1a = WIDGET_BASE( base4x, /ROW, /EXCLUSIVE )
button1a = WIDGET_BUTTON( base1a, VALUE='Grayscale (red band)' )
button1b = WIDGET_BUTTON( base1a, VALUE='Color (RGB bands)' )

base9 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( base9, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( base9, VALUE='Cancel' )
reset_blocks = WIDGET_BUTTON( base9, VALUE='Reset Blocks' )
help_button = WIDGET_BUTTON( base9, VALUE=' PDF Help ' )

widget_struct_animate = { $
   button1a : button1a, $	;1 Band (grayscale)
   button1b : button1b, $	;4 Bands (use 3 for RGB Color)
   button2a : button2a, $	;1 MISR Orbit
   button2b : button2b, $	;2 MISR Orbits
   button3  : button3,  $	;Select Orbit 1 Nadir HDF File
   label4a2 : label4a2, $	;Directory component for orbit 1.
   label4b2 : label4b2, $	;Filename component for orbit 1.
   base5    : base5,    $	;orbit 2 base
   button5  : button5,  $	;Select Orbit 2 Nadir HDF File
   label6a2 : label6a2, $	;Directory component for orbit 2.
   label6b2 : label6b2, $	;Filename component for orbit 2.
   label7a  : label7a,  $	;Available Blocks <range>
   label7b  : label7b,  $	;Available Blocks <range>
   text8a   : text8a,   $	;First Block
   text8b   : text8b,   $	;Last Block
   ok_button : ok_button, $
   cancel_button : cancel_button, $
   reset_blocks : reset_blocks, $
   help_button : help_button, $
   config_temp : config_temp, $
   config_defaults : config_defaults, $
   BlockULCx : DBLARR(!KON.Instr.NUM_BLOCKS), $
   BlockULCy : DBLARR(!KON.Instr.NUM_BLOCKS), $
   BlockLRCx : DBLARR(!KON.Instr.NUM_BLOCKS), $
   BlockLRCy : DBLARR(!KON.Instr.NUM_BLOCKS) }

HELP, /STRUCTURE, WidgetStructAnimate, OUTPUT=exists

IF (STRTRIM(exists[1],2) EQ 'UNDEFINED = <Undefined>') THEN BEGIN
   WidgetStructAnimate = widget_struct_animate
ENDIF ELSE BEGIN
   WidgetStructAnimate.button1a = widget_struct_animate.button1a
   WidgetStructAnimate.button1b = widget_struct_animate.button1b
   WidgetStructAnimate.button2a = widget_struct_animate.button2a
   WidgetStructAnimate.button2b = widget_struct_animate.button2b
   WidgetStructAnimate.button3 = widget_struct_animate.button3
   WidgetStructAnimate.label4a2 = widget_struct_animate.label4a2
   WidgetStructAnimate.label4b2 = widget_struct_animate.label4b2
   WidgetStructAnimate.base5 = widget_struct_animate.base5
   WidgetStructAnimate.button5 = widget_struct_animate.button5
   WidgetStructAnimate.label6a2 = widget_struct_animate.label6a2
   WidgetStructAnimate.label6b2 = widget_struct_animate.label6b2
   WidgetStructAnimate.label7a = widget_struct_animate.label7a
   WidgetStructAnimate.label7b = widget_struct_animate.label7b
   WidgetStructAnimate.text8a = widget_struct_animate.text8a
   WidgetStructAnimate.text8b = widget_struct_animate.text8b
   WidgetStructAnimate.ok_button = widget_struct_animate.ok_button
   WidgetStructAnimate.cancel_button = widget_struct_animate.cancel_button
   WidgetStructAnimate.reset_blocks = widget_struct_animate.reset_blocks
   WidgetStructAnimate.help_button = widget_struct_animate.help_button

   widget_struct_animate.config_temp.ncam = $
      WidgetStructAnimate.config_temp.ncam
   widget_struct_animate.config_temp.band_count = $
      WidgetStructAnimate.config_temp.band_count
   widget_struct_animate.config_temp.band_ndx[0] = $
      WidgetStructAnimate.config_temp.band_ndx[0]
   widget_struct_animate.config_temp.band_ndx[1] = $
      WidgetStructAnimate.config_temp.band_ndx[1]
   widget_struct_animate.config_temp.band_ndx[2] = $
      WidgetStructAnimate.config_temp.band_ndx[2]
   widget_struct_animate.config_temp.orbit_count = $
      WidgetStructAnimate.config_temp.orbit_count
   widget_struct_animate.config_temp.first_block = $
      WidgetStructAnimate.config_temp.first_block
   widget_struct_animate.config_temp.last_block = $
      WidgetStructAnimate.config_temp.last_block
   widget_struct_animate.config_temp.cam_files = $
      WidgetStructAnimate.config_temp.cam_files
   widget_struct_animate.config_temp.cancel_flag = $
      WidgetStructAnimate.config_temp.cancel_flag
   widget_struct_animate.config_temp.orbit_num = $
      WidgetStructAnimate.config_temp.orbit_num
   widget_struct_animate.config_temp.path_num = $
      WidgetStructAnimate.config_temp.path_num
   widget_struct_animate.config_temp.date_str = $
      WidgetStructAnimate.config_temp.date_str
   widget_struct_animate.config_temp.misr_airmisr = $
      WidgetStructAnimate.config_temp.misr_airmisr

ENDELSE

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

dir_name = '        select filename by clicking button above        '
fil_name = '                                                        '
fname1 = widget_struct_animate.config_temp.cam_files[4]

slen = STRLEN(fname1)
IF (slen NE 0) THEN BEGIN
   toks = STRSPLIT(fname1, '.', COUNT=ntok, /EXTRACT)
   IF (ntok GT 0 AND toks[ntok-1] EQ 'sav') THEN BEGIN
      widget_struct_animate.config_temp.cam_files[*] = ''
      fname1 = ''
      WIDGET_CONTROL, label4a2, SET_VALUE=dir_name
      WIDGET_CONTROL, label4b2, SET_VALUE=fil_name
      WidgetStructAnimate = widget_struct_animate
   ENDIF
ENDIF

slen = STRLEN(fname1)
IF (slen NE 0) THEN BEGIN
   spos = STRPOS(fname1, !KON.Misc.Slash, /REVERSE_SEARCH)
   IF (spos GT 0) THEN BEGIN
      dir_name = STRMID(fname1, 0, spos)
      fil_name = STRMID(fname1, spos+1, slen-spos-1)
   ENDIF
ENDIF

WIDGET_CONTROL, label4a2, SET_VALUE=dir_name
WIDGET_CONTROL, label4b2, SET_VALUE=fil_name

dir_name = '        select filename by clicking button above        '
fil_name = '                                                        '
IF (widget_struct_animate.config_temp.ncam EQ 9) THEN BEGIN
   WIDGET_CONTROL, button2a, /SET_BUTTON
   WIDGET_CONTROL, widget_struct_animate.button5,  SENSITIVE=0
   WIDGET_CONTROL, widget_struct_animate.label6a2, SENSITIVE=0
   WIDGET_CONTROL, widget_struct_animate.label6b2, SENSITIVE=0
ENDIF ELSE BEGIN
   slen = STRLEN(widget_struct_animate.config_temp.cam_files[13])
   IF (slen NE 0) THEN BEGIN
      spos = STRPOS(widget_struct_animate.config_temp.cam_files[13], $
                    !KON.Misc.Slash, /REVERSE_SEARCH)
      IF (spos GT 0) THEN BEGIN
         dir_name = STRMID(widget_struct_animate.config_temp.cam_files[13], $
                           0, spos)
         fil_name = STRMID(widget_struct_animate.config_temp.cam_files[13], $
                           spos+1, slen-spos-1)
      ENDIF
   ENDIF
   WIDGET_CONTROL, button2b, /SET_BUTTON
   WIDGET_CONTROL, widget_struct_animate.button5,  SENSITIVE=1
   WIDGET_CONTROL, widget_struct_animate.label6a2, SENSITIVE=1
   WIDGET_CONTROL, widget_struct_animate.label6b2, SENSITIVE=1
ENDELSE
WIDGET_CONTROL, label6a2, SET_VALUE=dir_name
WIDGET_CONTROL, label6b2, SET_VALUE=fil_name

IF (widget_struct_animate.config_temp.band_count EQ 1) THEN BEGIN
   WIDGET_CONTROL, widget_struct_animate.button1a, /SET_BUTTON
ENDIF ELSE BEGIN
   WIDGET_CONTROL, widget_struct_animate.button1b, /SET_BUTTON
ENDELSE

WIDGET_CONTROL, widget_struct_animate.text8a, $
                SET_VALUE = widget_struct_animate.config_temp.first_block
WIDGET_CONTROL, widget_struct_animate.text8b, $
                SET_VALUE = widget_struct_animate.config_temp.last_block

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_animate, XOFFSET=700, $
                YOFFSET=250, /NO_COPY
WIDGET_CONTROL, base0, /REALIZE
XMANAGER, 'OrbitAnimateOption_gui', base0, EVENT_HANDLER='OrbitAnimateOption_eh'

Cancel = WidgetStructAnimate.config_temp.cancel_flag
NumCam = WidgetStructAnimate.config_temp.ncam
CamFiles = WidgetStructAnimate.config_temp.cam_files[0:NumCam-1]
!VAR.CurrFiles.CamFiles = CamFiles
IF (NumCam EQ 9) THEN BEGIN
   CamFiles = CamFiles[0:8]
   coord_struct.(1) = GetCoordStructMember()
ENDIF
BlockBeg = WidgetStructAnimate.config_temp.first_block
BlockEnd = WidgetStructAnimate.config_temp.last_block
OrbitNum = WidgetStructAnimate.config_temp.orbit_num
MISR_or_AirMISR = WidgetStructAnimate.config_temp.misr_airmisr
CoordStruct = coord_struct
!SAV.DfltFiles[!KON.FileTyp.TypeL1B2Terrain].SavePath = $
   FILE_DIRNAME(WidgetStructAnimate.config_temp.cam_files[4]) + !KON.Misc.Slash
   
widget_struct_animate = 0

IF (Cancel) THEN Retval = -1

END ; OrbitAnimateOption_gui

;***************************************************************************
PRO PlumeUtilities_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=plume_util_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   !SAV.Util.PlumeOption = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ plume_util_struct.modvolc : BEGIN
   END
   event.id EQ plume_util_struct.modis : BEGIN
   END
   event.id EQ plume_util_struct.misr : BEGIN
   END
   event.id EQ plume_util_struct.orderstat : BEGIN
   END
   event.id EQ plume_util_struct.jpegs_to_mp4 : BEGIN
   END
   event.id EQ plume_util_struct.review_plumes1 : BEGIN
   END
   event.id EQ plume_util_struct.find_rereview : BEGIN
   END
   event.id EQ plume_util_struct.review_plumes2 : BEGIN
   END
   event.id EQ plume_util_struct.read_rereview : BEGIN
   END
   event.id EQ plume_util_struct.verify_review : BEGIN
   END
   event.id EQ plume_util_struct.create_webfiles : BEGIN
   END
   event.id EQ plume_util_struct.copy_to_local : BEGIN
   END
   event.id EQ plume_util_struct.ok_button : BEGIN
      !SAV.Util.PlumeOption = 0

      temp0 = WIDGET_INFO(plume_util_struct.modvolc,  /BUTTON_SET)
      ; temp1 skipped. Used to be reverb to download MOD14 data
      temp2 = WIDGET_INFO(plume_util_struct.modis,    /BUTTON_SET)
      
      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
         temp3  = WIDGET_INFO(plume_util_struct.misr,            /BUTTON_SET)
         temp4  = WIDGET_INFO(plume_util_struct.orderstat,       /BUTTON_SET)
         temp5  = WIDGET_INFO(plume_util_struct.jpegs_to_mp4,    /BUTTON_SET)
         temp6  = WIDGET_INFO(plume_util_struct.review_plumes1,  /BUTTON_SET)
         temp7  = WIDGET_INFO(plume_util_struct.find_rereview,   /BUTTON_SET)
         temp8  = WIDGET_INFO(plume_util_struct.review_plumes2,  /BUTTON_SET)
         temp9  = WIDGET_INFO(plume_util_struct.read_rereview,   /BUTTON_SET)
         temp10 = WIDGET_INFO(plume_util_struct.verify_review,   /BUTTON_SET)
         temp11 = WIDGET_INFO(plume_util_struct.create_webfiles, /BUTTON_SET)
         temp12 = WIDGET_INFO(plume_util_struct.copy_to_local,   /BUTTON_SET)
      ENDIF ELSE BEGIN
         temp3  = 0
         temp4  = 0
         temp5  = 0
         temp6  = 0
         temp7  = 0
         temp8  = 0
         temp9  = 0
         temp10 = 0
         temp11 = 0
         temp12 = 0
      ENDELSE

      IF (temp0  EQ 1) THEN !SAV.Util.PlumeOption = 0
      ; temp1 skipped. Used to be reverb to download MOD14 data
      IF (temp2  EQ 1) THEN !SAV.Util.PlumeOption = 2
      IF (temp3  EQ 1) THEN !SAV.Util.PlumeOption = 3
      IF (temp4  EQ 1) THEN !SAV.Util.PlumeOption = 4
      IF (temp5  EQ 1) THEN !SAV.Util.PlumeOption = 5
      IF (temp6  EQ 1) THEN !SAV.Util.PlumeOption = 6
      IF (temp7  EQ 1) THEN !SAV.Util.PlumeOption = 7
      IF (temp8  EQ 1) THEN !SAV.Util.PlumeOption = 8
      IF (temp9  EQ 1) THEN !SAV.Util.PlumeOption = 9
      IF (temp10 EQ 1) THEN !SAV.Util.PlumeOption = 10
      IF (temp11 EQ 1) THEN !SAV.Util.PlumeOption = 11
      IF (temp12 EQ 1) THEN !SAV.Util.PlumeOption = 12

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ plume_util_struct.cancel_button : BEGIN
      !SAV.Util.PlumeOption = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ plume_util_struct.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_PlumeUtilities.pdf'
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=plume_util_struct, /NO_COPY

END ; PlumeUtilities_eh

;***************************************************************************
PRO PlumeUtilities_gui, PlumeOption
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

misr             = 0
orderstat        = 0
jpegs_to_mp4     = 0
review_plumes1   = 0
find_rereview    = 0
review_plumes2   = 0
read_rereview    = 0
verify_review    = 0
create_webfiles  = 0
copy_to_local    = 0

base0 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
                    ' : Plume Utility', /TLB_KILL_REQUEST_EVENTS)
listname = WIDGET_LABEL(base0, /ALIGN_CENTER, $
                        VALUE='Plume Project Utility Options')
base1 = WIDGET_BASE(base0, /COLUMN, /EXCLUSIVE, FRAME=1)

modvolc = WIDGET_BUTTON(base1, VALUE="Process ModVolc Fire Pixel File")
modis   = WIDGET_BUTTON(base1, VALUE="Process MODIS MOD14 Fire Granules")

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
   misr             = WIDGET_BUTTON(base1,VALUE="Create MISR Order Script (not done)") ; implement this if possible
   orderstat        = WIDGET_BUTTON(base1,VALUE="Report MISR Order Status") ; fix, test and make available
   jpegs_to_mp4     = WIDGET_BUTTON(base1,VALUE="Convert 9 JPEGs to MP4") ; solid
   review_plumes1   = WIDGET_BUTTON(base1,VALUE="Review Digitized Plumes - Pass 1") ; solid
   find_rereview    = WIDGET_BUTTON(base1,VALUE="Case 1 - Find Plumes for Re-review") ; crappy
   review_plumes2   = WIDGET_BUTTON(base1,VALUE="Case 1 - Re-review Digitized Plumes") ; crappy
   read_rereview    = WIDGET_BUTTON(base1,VALUE="Case 2 - Read Re-review Results") ; crappy
   verify_review    = WIDGET_BUTTON(base1,VALUE="Verify Reviewed Plumes Complete") ; ??
   create_webfiles  = WIDGET_BUTTON(base1,VALUE="Create Data Files for Website") ; OK
   copy_to_local    = WIDGET_BUTTON(base1,VALUE="Copy Files from Server to Local") ; fix, test and make available
ENDIF

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE='Cancel')
help_button = WIDGET_BUTTON(base2, VALUE='PDF Help')

WIDGET_CONTROL, modvolc, SET_BUTTON=1

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

plume_util_struct = { $
   modvolc          : modvolc, $
   modis            : modis, $
   misr             : misr, $
   orderstat        : orderstat, $
   jpegs_to_mp4     : jpegs_to_mp4, $
   review_plumes1   : review_plumes1, $
   find_rereview    : find_rereview, $
   review_plumes2   : review_plumes2, $
   read_rereview    : read_rereview, $
   verify_review    : verify_review, $
   create_webfiles  : create_webfiles, $
   copy_to_local    : copy_to_local, $
   ok_button        : ok_button, $
   cancel_button    : cancel_button, $
   help_button      : help_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=plume_util_struct, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'PlumeUtilities_gui', base0, EVENT_HANDLER='PlumeUtilities_eh'

PlumeOption = !SAV.Util.PlumeOption

END ; PlumeUtilities_gui

;***************************************************************************
PRO ProcessPlumeOrbits_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orblist_val, reduce_blks, orbitlist_index, terr_or_ellipse

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=orbit_list_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   orbitlist_index = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ orbit_list_struct.orblist : BEGIN
      orbitlist_index = event.Index
   END
   event.id EQ orbit_list_struct.reducebox : BEGIN
      reduce_blks = event.Select
   END
   event.id EQ orbit_list_struct.terrain_btn : BEGIN
      terr_or_ellipse = 0
   END
   event.id EQ orbit_list_struct.ellipse_btn : BEGIN
      terr_or_ellipse = 1
   END
   event.id EQ orbit_list_struct.ok_button : BEGIN
      IF (orbitlist_index LT 0) THEN BEGIN
         mssg = 'You must select an orbit. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDELSE
   END
   event.id EQ orbit_list_struct.cancel_button : BEGIN
      orbitlist_index = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ orbit_list_struct.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + $
                   'MINXdoc_ProcessPlumeProject.pdf'
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=orbit_list_struct, /NO_COPY

END ; ProcessPlumeOrbits_eh

;***************************************************************************
PRO ProcessPlumeOrbits_gui, OrbitList, OrblistIndex, ReduceBlks, TerrainOrEllips
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orblist_val, reduce_blks, orbitlist_index, terr_or_ellipse

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

norbits = N_ELEMENTS(OrbitList)
nlines = norbits < 30

base0 = WIDGET_BASE(/COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM + $
               ' : Plume Project Orbit Selection', /TLB_KILL_REQUEST_EVENTS)
listname1 = WIDGET_LABEL(base0, /ALIGN_CENTER, YSIZE=20, $
                         VALUE='Choose L1B2 File Type to Use')
base2 = WIDGET_BASE(base0, /COLUMN, /EXCLUSIVE, FRAME=1)
terrain_btn = WIDGET_BUTTON(base2, YSIZE=20, /ALIGN_CENTER, $
                            VALUE="Load GRP_TERRAIN Files  ")
ellipse_btn = WIDGET_BUTTON(base2, YSIZE=20, /ALIGN_CENTER, $
                            VALUE="Load GRP_ELLIPSOID Files")
listname2 = WIDGET_LABEL(base0, VALUE='')
listname3 = WIDGET_LABEL(base0, /ALIGN_CENTER, YSIZE=20, $
                         VALUE='Select Orbit and Block Range')
orblist = WIDGET_LIST(base0, FRAME=1, VALUE=OrbitList, YSIZE=nlines)
base1 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER, /NONEXCLUSIVE)
reducebox = WIDGET_BUTTON(base1, VALUE="Don't load first and last block")

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE='Cancel')
help_button = WIDGET_BUTTON( base2, VALUE='PDF Help' )

WIDGET_CONTROL, terrain_btn, SET_BUTTON=1
terr_or_ellipse = 0

WIDGET_CONTROL, orblist, SET_LIST_SELECT=0
orbitlist_index = 0

WIDGET_CONTROL, reducebox, SET_BUTTON=0
reduce_blks = 0

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

orbit_list_struct = { $
   orblist : orblist, $
   reducebox : reducebox, $
   terrain_btn : terrain_btn, $
   ellipse_btn : ellipse_btn, $
   ok_button : ok_button, $
   cancel_button : cancel_button, $
   help_button : help_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=orbit_list_struct, XOFFSET=700, $
                YOFFSET=100, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'ProcessPlumeOrbits_gui', base0, EVENT_HANDLER='ProcessPlumeOrbits_eh'

TerrainOrEllips = terr_or_ellipse
OrblistIndex = orbitlist_index
ReduceBlks = reduce_blks

END ; ProcessPlumeOrbits_gui

;***************************************************************************
PRO ReportOnMisrOrder_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_order, misrorder_dirs

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=order_dir_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   misrorder_dirs = ['','','','','','']
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

IF (order_dir_struct.save_dir NE '') THEN BEGIN
   home_path = order_dir_struct.save_dir
ENDIF ELSE BEGIN
   home_path = !SAV.WorkingDir
ENDELSE

CASE 1 OF

   event.id EQ order_dir_struct.agp_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeAgp, $
             ['MISR*_AGP_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.agp_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END
   event.id EQ order_dir_struct.gmp_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeGpGmp, $
             ['MISR*_GP_GMP_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.gmp_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END
   event.id EQ order_dir_struct.class_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeClass, $
             ['MISR*_CLASSIFIERS_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.class_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END
   event.id EQ order_dir_struct.aerosol_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeAerosol, $
             ['MISR*_AS_AEROSOL_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.aerosol_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END
   event.id EQ order_dir_struct.terrain_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeL1B2Terrain, $
             ['MISR*_GRP_TERRAIN_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.terrain_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END
   event.id EQ order_dir_struct.ellips_button : BEGIN
      GetLastFilename, 0, !KON.FileTyp.TypeL1B2Ellipsoid, $
             ['MISR*_GRP_ELLIPSOID_*.hdf'], 0, file_outpath, filename
      IF (filename EQ '') THEN GOTO, cancelled
      WIDGET_CONTROL, order_dir_struct.ellips_dir, SET_VALUE=filename
      npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      order_dir_struct.save_dir = STRMID(filename, 0, npos)
   END

   event.id EQ order_dir_struct.agp_dir : BEGIN
   END
   event.id EQ order_dir_struct.gmp_dir : BEGIN
   END
   event.id EQ order_dir_struct.class_dir : BEGIN
   END
   event.id EQ order_dir_struct.aerosol_dir : BEGIN
   END
   event.id EQ order_dir_struct.terrain_dir : BEGIN
   END
   event.id EQ order_dir_struct.ellips_dir : BEGIN
   END

   event.id EQ order_dir_struct.ok_button : BEGIN
      WIDGET_CONTROL, order_dir_struct.agp_dir, GET_VALUE=temp
      misrorder_dirs[0] = temp
      WIDGET_CONTROL, order_dir_struct.gmp_dir, GET_VALUE=temp
      misrorder_dirs[1] = temp
      WIDGET_CONTROL, order_dir_struct.class_dir, GET_VALUE=temp
      misrorder_dirs[2] = temp
      WIDGET_CONTROL, order_dir_struct.aerosol_dir, GET_VALUE=temp
      misrorder_dirs[3] = temp
      WIDGET_CONTROL, order_dir_struct.terrain_dir, GET_VALUE=temp
      misrorder_dirs[4] = temp
      WIDGET_CONTROL, order_dir_struct.ellips_dir, GET_VALUE=temp
      misrorder_dirs[5] = temp
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ order_dir_struct.cancel_button : BEGIN
      misrorder_dirs = ['','','','','','']
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ order_dir_struct.help_button : BEGIN
;      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_.pdf'
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

cancelled:
WIDGET_CONTROL, event.top, SET_UVALUE=order_dir_struct, /NO_COPY

END ; ReportOnMisrOrder_eh

;***************************************************************************
PRO ReportOnMisrOrder_gui, MisrOrder
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_order, misrorder_dirs

blanks1 = '                    '
blanks2 = blanks1 + blanks1 + blanks1 + blanks1 + blanks1
misrorder_dirs = STRARR(6)
save_dir = ''

desc_text = $type for 
  ['For each MISR product type you wish to report on, select a file to act as a template ' + $
   'for that type. The path and orbit numbers will be disregarded, but you must specify ' + $
   'the directory where you expect the files to be found. If you are expecting subsetted ' + $
   'files, the filename must correctly indicate the block range as in ' + $
   'MISR_AM1_GRP_TERRAIN_GM_P001_O066627_AF_F03_0024.b099-108.hdf.', $
   'For GRP_TERRAIN and GRP_ELLIPSOID files, you need to specify only one camera file and ' + $
   'MINX will find the rest. The 9 camera files for each orbit can all be in a one large ' + $
   'directory or can be in 9 subdirectories named: "DF, CF, BF, AF, AN, AA, BA, CA and DA".', $
   'You can manually edit the file names after they have been inserteed as well.']

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, TITLE='Report on MISR Product Orders', $
                    /TLB_KILL_REQUEST_EVENTS)

text0 = WIDGET_TEXT(base0, VALUE=desc_text, YSIZE=6, /WRAP)

base1 = WIDGET_BASE(base0, /COLUMN, FRAME=1)
base2 = WIDGET_BASE(base1, /ROW)
base3 = WIDGET_BASE(base1, /ROW)
base4 = WIDGET_BASE(base1, /ROW)
base5 = WIDGET_BASE(base1, /ROW)
base6 = WIDGET_BASE(base1, /ROW)
base7 = WIDGET_BASE(base1, /ROW)

;---------------------------------------------------------------------------
; Get the filenames for data for this MISR path.
;---------------------------------------------------------------------------

AGPfile   = blanks2
GMPfile   = blanks2
ClassFile = blanks2
AerFile   = blanks2
TerrFile  = blanks2
ElliFile  = blanks2

agp_button     = WIDGET_BUTTON(base2, VALUE='     Get AGP sample       ')
agp_dir        = CW_FIELD(base2, /STRING, TITLE='', VALUE=AGPfile)
gmp_button     = WIDGET_BUTTON(base3, VALUE='    Get GP_GMP sample     ')
gmp_dir        = CW_FIELD(base3, /STRING, TITLE='', VALUE=GMPfile)
class_button   = WIDGET_BUTTON(base4, VALUE='Get TC_CLASSIFIERS sample ')
class_dir      = CW_FIELD(base4, /STRING, TITLE='', VALUE=ClassFile)
aerosol_button = WIDGET_BUTTON(base5, VALUE='  Get AS_AEROSOL sample   ')
aerosol_dir    = CW_FIELD(base5, /STRING, TITLE='', VALUE=AerFile)
terrain_button = WIDGET_BUTTON(base6, VALUE=' Get GRP_TERRAIN sample   ')
terrain_dir    = CW_FIELD(base6, /STRING, TITLE='', VALUE=TerrFile)
ellips_button  = WIDGET_BUTTON(base7, VALUE=' Get GRP_ELLIPSOID sample ')
ellips_dir     = CW_FIELD(base7, /STRING, TITLE='', VALUE=ElliFile)

base8 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base8, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base8, VALUE='Cancel')
help_button = WIDGET_BUTTON(base8, VALUE=' Help ')

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

order_dir_struct = { save_dir       : save_dir, $
                     agp_button     : agp_button, $
                     gmp_button     : gmp_button, $
                     class_button   : class_button, $
                     aerosol_button : aerosol_button, $
                     terrain_button : terrain_button, $
                     ellips_button  : ellips_button, $
                     agp_dir        : agp_dir, $
                     gmp_dir        : gmp_dir, $
                     class_dir      : class_dir, $
                     aerosol_dir    : aerosol_dir, $
                     terrain_dir    : terrain_dir, $
                     ellips_dir     : ellips_dir, $
                     ok_button      : ok_button, $
                     cancel_button  : cancel_button, $
                     help_button    : help_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=order_dir_struct, XOFFSET=500, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'ReportOnMisrOrder_gui', base0, EVENT_HANDLER='ReportOnMisrOrder_eh'

MisrOrder = misrorder_dirs

END ; ReportOnMisrOrder_gui

;***************************************************************************
PRO PlumeProj_Preferences_eh, event
;***************************************************************************
; Event handler for MINX preferences dialog.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
COMMON user_prefs, pref1, pref2

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=pref_info_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget that was used.
;---------------------------------------------------------------------------

CASE 1 OF
   event.id EQ pref_info_struct.pref1_button : BEGIN
      WIDGET_CONTROL, pref_info_struct.pref1_text, GET_VALUE=old_working_dir
      new_working_dir = DIALOG_PICKFILE( $
         TITLE='Select or create a MINX Working Directory', $
         PATH=old_working_dir, FILE=old_working_dir, /DIRECTORY)
      IF (new_working_dir EQ '') THEN BREAK
      new_working_dir = FILE_EXPAND_PATH(new_working_dir)
      IF (~ FILE_TEST(new_working_dir, /DIRECTORY)) THEN BEGIN
         rtrn_val = MakeDirectory(new_working_dir)
         IF (rtrn_val NE 0) THEN BEGIN
            mssg = ['Directory ' + new_working_dir, 'could not be created.']
            rtrn_val = DIALOG_MESSAGE(mssg, /CENTER)
            BREAK
         ENDIF
         rtrn_val = ChmodCatchError(new_working_dir, '777'O)
      ENDIF
      WIDGET_CONTROL, pref_info_struct.pref1_text, SET_VALUE=new_working_dir
   END

   event.id EQ pref_info_struct.pref2_button : BEGIN
      WIDGET_CONTROL, pref_info_struct.pref2_text, GET_VALUE=old_plume_proj_dir      
      new_plume_proj_dir = DIALOG_PICKFILE( $
                TITLE='Select or create a MINX Plume Project Directory', $
                PATH=old_plume_proj_dir, FILE=old_plume_proj_dir, /DIRECTORY)
      IF (new_plume_proj_dir EQ '') THEN BREAK
      new_plume_proj_dir = FILE_EXPAND_PATH(new_plume_proj_dir)      
      IF (~ FILE_TEST(new_plume_proj_dir, /DIRECTORY)) THEN BEGIN
         rtrn_val = MakeDirectory(new_plume_proj_dir)        
         IF (rtrn_val NE 0) THEN BEGIN
            mssg = ['Directory ' + new_plume_proj_dir, 'could not be created.']
            rtrn_val = DIALOG_MESSAGE(mssg, /CENTER)
            BREAK
         ENDIF
         rtrn_val = ChmodCatchError(new_plume_proj_dir, '777'O)
      ENDIF
      WIDGET_CONTROL, pref_info_struct.pref2_text, SET_VALUE=new_plume_proj_dir
   END
   
   event.id EQ pref_info_struct.pref4_button : BEGIN
     WIDGET_CONTROL, pref_info_struct.pref4_text, SET_VALUE=!SAV.MISROrderWebsite
   END
   event.id EQ pref_info_struct.pref5_button : BEGIN
     WIDGET_CONTROL, pref_info_struct.pref5_text, SET_VALUE=!SAV.MODISOrderFTPsite
   END
   event.id EQ pref_info_struct.pref6_button : BEGIN
     WIDGET_CONTROL, pref_info_struct.pref6_text, SET_VALUE=!SAV.MODVOLCWebsite
   END

   event.id EQ pref_info_struct.ok_button : BEGIN
      WIDGET_CONTROL, pref_info_struct.pref1_text, GET_VALUE=user_working_dir
      IF (STRMID(user_working_dir, 0, 1, /REVERSE_OFFSET) NE !KON.Misc.Slash) THEN $
         user_working_dir += !KON.Misc.Slash
      !SAV.WorkingDir = user_working_dir
   
      WIDGET_CONTROL, pref_info_struct.pref2_text, GET_VALUE=user_proj_dir
      IF (STRMID(user_proj_dir, 0, 1, /REVERSE_OFFSET) NE !KON.Misc.Slash) THEN $
         user_proj_dir += !KON.Misc.Slash
      !SAV.PlumeProjDir = user_proj_dir
      
      WIDGET_CONTROL, pref_info_struct.pref4_text, GET_VALUE=userMISROrder
      !SAV.MISROrderWebsite = userMISROrder
      
      WIDGET_CONTROL, pref_info_struct.pref5_text, GET_VALUE=userMODISFtp
      !SAV.MODISOrderFTPsite = userMODISFtp
      
      WIDGET_CONTROL, pref_info_struct.pref6_text, GET_VALUE=userMODVOLC
      !SAV.MODVOLCWebsite = userMODVOLC
      
      ;---------------------------------------------------------------------
      ; Store values of preferences set in dialog box to the disk file and
      ; to the parameters in !VAR.
      ;---------------------------------------------------------------------
      
      WritePreferencesFile

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ pref_info_struct.cancel_button : BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   event.id EQ pref_info_struct.help_button : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_PlumePreferences.pdf'
   END
   
ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=pref_info_struct, /NO_COPY

END  ;  PlumeProj_Preferences_eh
   
;***************************************************************************
PRO PlumeProj_Preferences_gui, Cancel
;***************************************************************************
; Query user for certain user preferences and save them on disk to be read
; into the !VAR parameter whenever MINX is loaded.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON user_prefs, pref1, pref2

;---------------------------------------------------------------------------
; Get values of current preferences from disk file. If none, create the file
; from standard default values.
;---------------------------------------------------------------------------

ReadPreferencesFile
pref1 = 0
pref2 = 0

;---------------------------------------------------------------------------
; Define controls in dialog box.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, TITLE='Plume Project Preferences', XSIZE=460, $
                    /TLB_KILL_REQUEST_EVENTS)
label0a = WIDGET_LABEL( base0, VALUE='Plume Project Preferences' )
xtextsize = 35
xlabelsize = 150
xbuttonsize = 60

base1 = WIDGET_BASE(base0, /ROW)
pref1_label = WIDGET_LABEL(base1, VALUE="Working Directory: ", XSIZE=xlabelsize)
pref1_text = WIDGET_TEXT(base1, XSIZE=xtextsize, VALUE=!SAV.WorkingDir)
pref1_button = WIDGET_BUTTON(base1, VALUE="Browse", XSIZE=xbuttonsize, $
      TOOLTIP="Change to the directory where your MINX non-plume project output will go.")
   
base2 = WIDGET_BASE(base0, /ROW)
pref2_label = WIDGET_LABEL(base2, VALUE="Plume Project Directory: ", XSIZE=xlabelsize)
pref2_text = WIDGET_TEXT(base2, XSIZE=xtextsize, VALUE=!SAV.PlumeProjDir)  
pref2_button = WIDGET_BUTTON(base2, VALUE="Browse", XSIZE=xbuttonsize, $
      TOOLTIP="Change to the directory where your PlumeProjOrbitList.txt file is located.")    
  
base4 = WIDGET_BASE(base0, /ROW)
pref4_label = WIDGET_LABEL(base4, VALUE="MISR Order Site: ", XSIZE=xlabelsize)
pref4_text = WIDGET_TEXT(base4, XSIZE=xtextsize, $
                         VALUE=!SAV.MISROrderWebsite, /EDITABLE, /FRAME)
pref4_button = WIDGET_BUTTON(base4, VALUE="Reset", XSIZE=xbuttonsize, $
      TOOLTIP="Reset Last Change to MISR Order Website.")
  
base5 = WIDGET_BASE(base0, /ROW)
pref5_label = WIDGET_LABEL(base5, VALUE="MODIS HTTP Order Site: ", XSIZE=xlabelsize)
pref5_text = WIDGET_TEXT(base5, XSIZE=xtextsize, $
                         VALUE=!SAV.MODISOrderFTPsite, /EDITABLE, /FRAME)
pref5_button = WIDGET_BUTTON(base5, VALUE="Reset", XSIZE=xbuttonsize, $
      TOOLTIP="Reset Last Change to MODIS HTTP Order Site.")
  
base6 = WIDGET_BASE(base0, /ROW)
pref6_label = WIDGET_LABEL(base6, VALUE="MODVOLC Order Site: ", XSIZE=xlabelsize)
pref6_text = WIDGET_TEXT(base6, XSIZE=xtextsize, $
                         VALUE=!SAV.MODVOLCWebsite, /EDITABLE, /FRAME)
pref6_button = WIDGET_BUTTON(base6, VALUE="Reset", XSIZE=xbuttonsize, $
      TOOLTIP="Reset Last Change to MODVOLC Order Site.")
  
basebuttons = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(basebuttons, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(basebuttons, VALUE=' Cancel ')
help_button = WIDGET_BUTTON(basebuttons, VALUE=' PDF Help ')

;---------------------------------------------------------------------------
; Set controls in dialog box to reflect current values of user preferences.
;---------------------------------------------------------------------------

WIDGET_CONTROL, pref1_button

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

pref_info_struct = { $
   pref1_label   : pref1_label,  $
   pref1_text    : pref1_text,   $
   pref1_button  : pref1_button, $
   pref2_label   : pref2_label,  $
   pref2_text    : pref2_text,   $
   pref2_button  : pref2_button, $
   pref4_label   : pref4_label,  $
   pref4_text    : pref4_text,   $
   pref4_button  : pref4_button, $
   pref5_label   : pref5_label,  $
   pref5_text    : pref5_text,   $
   pref5_button  : pref5_button, $
   pref6_label   : pref6_label,  $
   pref6_text    : pref6_text,   $
   pref6_button  : pref6_button, $
   ok_button     : ok_button,    $
   help_button   : help_button,  $
   cancel_button : cancel_button }
   
;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=pref_info_struct, XOFFSET=700, YOFFSET=300, $
                /NO_COPY
   
WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'PlumeProj_Preferences_gui', base0, $
          EVENT_HANDLER='PlumeProj_Preferences_eh'

END  ;  PlumeProj_Preferences_gui
