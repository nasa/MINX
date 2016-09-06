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
PRO ReadPreferencesFile
;***************************************************************************
; This procedure reads a standard MINX preferences file in IDL .sav format
; that contains a memory image of the global !VAR structure created at startup
; and populated during program execution by user choices. If there is no
; preferences file or the existing one is from an earlier format version,
; then create a new preferences file filed with default values.
; System variables can't be saved directly, so make a copy to save.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

;---------------------------------------------------------------------------
; Get the name of the preferences file, which MUST be in the user's home
; directory, and search for it (it should not be in the data directory he
; specifies to contain output files).
;---------------------------------------------------------------------------

data_search = FILE_SEARCH(!KON.Misc.MINX_PREF_PATH, COUNT=num_found)

;---------------------------------------------------------------------------
; If found, restore it. If not found, create a new preferences file containing
; default values.
;---------------------------------------------------------------------------

IF (num_found) THEN BEGIN
   RESTORE, !KON.Misc.MINX_PREF_PATH
   !SAV = SAV_RESTORE_NAME
   SAV_RESTORE_NAME = 0

   ;------------------------------------------------------------------------
   ; Test if the version string in the restored file is the same as the
   ; version string in the code that's running. If so, assume the file has
   ; the correct format and proceed.
   ;------------------------------------------------------------------------

   IF (!SAV.DfltFiles[0].SavePath NE $
       STRTRIM(STRING(!KON.Misc.MINX_VERSION_NUM), 2)) THEN BEGIN

      ;---------------------------------------------------------------------
      ; If the version string doesn't match, the user probably has an old
      ; version of the restored file. Delete it and copy the !VAR global
      ; variable containing default values into a new file.
      ;---------------------------------------------------------------------

      mssg = ['There is a new format for the preferences file that is not', $
              'compatible with your current file. MINX will delete your old',$
              'file and create a new one that will be populated with your', $
              'choices as you select them from MINX dialog boxes.']
              
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)

      FILE_DELETE, !KON.Misc.MINX_PREF_PATH
      
      SAV_RESTORE_NAME = !SAV
      SAVE, FILENAME=!KON.Misc.MINX_PREF_PATH, SAV_RESTORE_NAME
      SAV_RESTORE_NAME = 0
   ENDIF
   
ENDIF ELSE BEGIN

   ;------------------------------------------------------------------------
   ; User does not find a preferences file in his home directory, so copy
   ; the !VAR global variable containing default values into a new file.
   ;------------------------------------------------------------------------

   SAV_RESTORE_NAME = !SAV
   SAVE, FILENAME=!KON.Misc.MINX_PREF_PATH, SAV_RESTORE_NAME
   SAV_RESTORE_NAME = 0
ENDELSE

;---------------------------------------------------------------------------
; Reconstruct the system color tables in COMMON.
;---------------------------------------------------------------------------

B_CURR[*] =  !SAV.ColorPalette[*] / 65536L
G_CURR[*] = (!SAV.ColorPalette[*] -  B_CURR[*] * 65536L) / 256L
R_CURR[*] =  !SAV.ColorPalette[*] - (B_CURR[*] * 65536L) - (G_CURR[*] * 256L)

R_ORIG = R_CURR & G_ORIG = G_CURR & B_ORIG = B_CURR

END  ;  ReadPreferencesFile

;***************************************************************************
PRO WritePreferencesFile
;***************************************************************************
; This procedure writes a memory image of the global !VAR structure created
; at startup and populated during program execution by user choices into a
; standard MINX preferences file in IDL .sav format.
; WritePreferencesFile can be called whenever any variable !VAR contains has
; been changed, e.g. when variables in dialog boxes are changed and the
; dialog is closed with OK. However, it is seldom necessary, because this is
; called whenever GetLastFilename is called (see next procedure), and that
; procedure is called whenever any standard file or directory is opened for
; read or write.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the name of the preferences file, which MUST be in the user's home
; directory, and search for it.
;---------------------------------------------------------------------------

data_search = FILE_SEARCH(!KON.Misc.MINX_PREF_PATH, COUNT=num_found)

;---------------------------------------------------------------------------
; Whether or not it exists, just write the data to the preferences pathname.
;---------------------------------------------------------------------------

SAV_RESTORE_NAME = !SAV
SAVE, FILENAME=!KON.Misc.MINX_PREF_PATH, SAV_RESTORE_NAME
SAV_RESTORE_NAME = 0
   
END  ;  WritePreferencesFile

;***************************************************************************
PRO GetLastFilename, AltPath, FileType, FileFilter, FixFilter, PathName, $
                     FullFileName
;***************************************************************************
; This procedure gets the last-used path name for a particular purpose from
; the !VAR global structure. The user can change these parameters in the
; call to DIALOG_PICKFILE and they get written back to the structure as well
; as to the preferences file.
;
; AltPath = a MISR path number if the caller needs to specify it
; FileType = the code for type of file found in !KON.FileTyp. ...
; FileFilter = a string containing the file extension
; FixFilter = 1 if the file filter cannot be changed; 0 otherwise
; PathName = the directory name saved for the file in directory-name-cache
; FullFileName = the filename returned to the caller
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

PathName = ''
FullFileName = ''

;---------------------------------------------------------------------------
; Let user select the file to use. Optionally allow selection of directories
; as well as files. Treat as special those requests that are for directories,
; not files.
;---------------------------------------------------------------------------

data_inpath = !SAV.DfltFiles[FileType].SavePath
IF (AltPath NE 0) THEN BEGIN
   begchr = STRLEN(data_inpath)
   orbit_comp = STRMID(data_inpath, begchr-9, 5)
   IF (orbit_comp EQ !KON.Misc.Slash + 'path') THEN BEGIN
      newpath = STRTRIM(STRING(AltPath),2)
      IF (STRLEN(newpath) EQ 1) THEN newpath = '00' + newpath
      IF (STRLEN(newpath) EQ 2) THEN newpath = '0'  + newpath
      STRPUT, data_inpath, !KON.Misc.Slash + 'path' + newpath, begchr-9
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Let user select the file to use. Optionally allow selection of directories
; as well as files. Treat as special those requests that are for directories,
; not files.
;---------------------------------------------------------------------------

IF (FileType GE !KON.FileTyp.TypeFirstDirUsed AND $
    FileType LE !KON.FileTyp.TypeLastDirUsed) THEN BEGIN
   data_filename = DIALOG_PICKFILE(TITLE= $
                          !SAV.DfltFiles[FileType].TypeTitle, $
                          PATH=data_inpath, FILE=data_inpath, $
                          GET_PATH=data_outpath, /DIRECTORY)
   nlen = STRLEN(data_filename)
   IF (nlen GT 0 AND $
       STRMID(data_filename, nlen-1, 1) NE !KON.Misc.Slash) THEN $
      data_filename += !KON.Misc.Slash

ENDIF ELSE BEGIN
   IF (FixFilter EQ 0) THEN BEGIN
      data_filename = DIALOG_PICKFILE(TITLE= $
                             !SAV.DfltFiles[FileType].TypeTitle, $
                             FILTER=FileFilter, PATH=data_inpath, $
                             GET_PATH=data_outpath, /MUST_EXIST)
   ENDIF ELSE BEGIN
      data_filename = DIALOG_PICKFILE(TITLE= $
                             !SAV.DfltFiles[FileType].TypeTitle, $
                             FILTER=FileFilter, PATH=data_inpath, $
                             GET_PATH=data_outpath, /MUST_EXIST, $
                             /FIX_FILTER)
   ENDELSE
ENDELSE

;---------------------------------------------------------------------------
; If no filename was selected, return to caller.
;---------------------------------------------------------------------------

IF (data_filename EQ '') THEN RETURN

;---------------------------------------------------------------------------
; If a filename was selected, save it into the user's global !VAR structure.
;---------------------------------------------------------------------------

PathName = data_outpath
FullFileName = data_filename

!SAV.DfltFiles[FileType].SavePath = data_outpath

;---------------------------------------------------------------------------
; Write the updated !VAR to the preferences file.
;---------------------------------------------------------------------------

WritePreferencesFile

END ; GetLastFilename

;***************************************************************************
PRO CreateOrbitDirFile, BaseDir, BaseFilePre, BaseFilePost, UseOrbit, Ext, $
                        OrbitNum, MakeDir, AllDir, AllFile, Retval
;***************************************************************************
; Create the directory name for a file that contains information on a
; particular orbit. Create the directory if requested.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1
orbit_char = '0'

;---------------------------------------------------------------------------
; Set a trap for errors.
;---------------------------------------------------------------------------

CATCH, ErrorStatus
   
IF (ErrorStatus NE 0) THEN BEGIN  
   mssg = ['Error creating directory and file names for file', $
           'with extension ' + Ext + '. Processing will continue.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL  
   RETURN
ENDIF  

;mssg = 'Home Directory is: ' + !SAV.WorkingDir
;rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)

;---------------------------------------------------------------------------
; If the base directory is an empty string, make it the home directory.
;---------------------------------------------------------------------------

IF (BaseDir EQ '') THEN BaseDir = !SAV.WorkingDir

IF (STRMID(BaseDir, 0, /REVERSE_OFFSET) NE  !KON.Misc.Slash) THEN $
   BaseDir += !KON.Misc.Slash

;---------------------------------------------------------------------------
; Create the file name based on the orbit number. Set the orbit string to
; null for the file name if 
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(OrbitNum), 2)
orbit_str1 = ''
IF (UseOrbit) THEN orbit_str1 = orbit_str
AllFile = BaseFilePre + orbit_str1 + BaseFilePost + '.' + Ext

;---------------------------------------------------------------------------
; Create the directory name using an orbit string in standard MINX format:
; 6 characters with 0's preceding the number if needed. Add the 6th '0'
; separately so it can be changed easily later if the 'O' is used instead.
;---------------------------------------------------------------------------

IF (STRLEN(orbit_str) LT 5) THEN orbit_str = '0' + orbit_str
IF (STRLEN(orbit_str) LT 5) THEN orbit_str = '0' + orbit_str

AllDir = BaseDir + orbit_char + orbit_str + !KON.Misc.Slash

;---------------------------------------------------------------------------
; Create the directory if requested and make it globally available.
;---------------------------------------------------------------------------

IF (MakeDir AND ~ FILE_TEST(AllDir, /DIRECTORY)) THEN BEGIN
   FILE_MKDIR, AllDir
   rtrn_val = ChmodCatchError(AllDir, '777'O)
ENDIF

;mssg = 'Write Directory is: ' + AllDir
;rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)

;---------------------------------------------------------------------------
; Cancel the error catcher.
;---------------------------------------------------------------------------

CATCH, /CANCEL  

Retval = 0

END  ;  CreateOrbitDirFile

;***************************************************************************
PRO GetUserMarkerPref_eh, event
;***************************************************************************
; Event handler for asking user what kind of marker file to load.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON struct_marker, marker_option

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_marker, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   marker_option = 0
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;------------------------------------------------------------------------
; Branch to the correct widget.
;------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_marker.button1 : BEGIN
      widget_struct_marker.load_option = 1
   END
   event.id EQ widget_struct_marker.button2 : BEGIN
      widget_struct_marker.load_option = 2
   END
   event.id EQ widget_struct_marker.button3 : BEGIN
      widget_struct_marker.load_option = 3
   END
   event.id EQ widget_struct_marker.button4 : BEGIN
      widget_struct_marker.load_option = 4
   END

   event.id EQ widget_struct_marker.ok_button : BEGIN
      marker_option = widget_struct_marker.load_option
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_marker.cancel_button : BEGIN
      marker_option = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_marker, /NO_COPY

END  ;  GetUserMarkerPref_eh

;***************************************************************************
PRO GetUserMarkerPref_gui, wTopWorkBase, LoadOption
;***************************************************************************
; User interface for asking user what kind of marker file to load.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON struct_marker, marker_option

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, /MODAL, $
                    GROUP_LEADER=WIDGET_INFO(wTopWorkBase,/PARENT), $
                    TITLE='Select Marker Type')
label = WIDGET_LABEL(base0, VALUE='Marker Point Options')
base1 = WIDGET_BASE(base0, /COLUMN, /FRAME, /ALIGN_CENTER, /EXCLUSIVE)
button1 = WIDGET_BUTTON(base1, VALUE='Load Aeronet Sites')
button2 = WIDGET_BUTTON(base1, VALUE='Load Volcano Locations')
button3 = WIDGET_BUTTON(base1, VALUE='Specify Custom Point File')
button4 = WIDGET_BUTTON(base1, VALUE='Remove Marker Points')

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE=' Exit ')

;---------------------------------------------------------------------------
; Set default button values and sensitivities.
;---------------------------------------------------------------------------

marker_option = LoadOption
button_set = (LoadOption EQ 1) ? button1 : $
            ((LoadOption EQ 2) ? button2 : $
           (((LoadOption EQ 3) ? button3 : button4)))
WIDGET_CONTROL, button_set, /SET_BUTTON

IF (!VAR.CurrFiles.Marker_Loaded) THEN BEGIN
   WIDGET_CONTROL, button1, SENSITIVE=0
   WIDGET_CONTROL, button2, SENSITIVE=0
   WIDGET_CONTROL, button3, SENSITIVE=0
   WIDGET_CONTROL, button4, SENSITIVE=1
ENDIF ELSE BEGIN
   WIDGET_CONTROL, button1, SENSITIVE=1
   WIDGET_CONTROL, button2, SENSITIVE=1
   WIDGET_CONTROL, button3, SENSITIVE=1
   WIDGET_CONTROL, button4, SENSITIVE=0
ENDELSE

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct_marker = { $
   button1 : button1, $
   button2 : button2, $
   button3 : button3, $
   button4 : button4, $
   ok_button : ok_button, $
   cancel_button : cancel_button, $
   load_option : LoadOption}

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_marker, XOFFSET=700, $
                YOFFSET=250, /NO_COPY
WIDGET_CONTROL, base0, /REALIZE
XMANAGER, 'GetUserMarkerPref_gui', base0, $
          EVENT_HANDLER='GetUserMarkerPref_eh'

LoadOption = marker_option

END  ;  GetUserMarkerPref_gui

;***************************************************************************
PRO SetPlotFileValues_eh, event
;***************************************************************************
; Event handler for user interface for getting the corner coordinates of
; the image rectangle to write to file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   !VAR.AutoWndw.CANCEL = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;------------------------------------------------------------------------
; Branch to the correct widget.
;------------------------------------------------------------------------

CASE 1 OF

   ; OK, accept new values.

   event.id EQ widget_struct.ok_button : BEGIN

      WIDGET_CONTROL, widget_struct.text1, GET_VALUE = value
      !VAR.AutoWndw.LEFT_EDGE = value > $
           widget_struct.config_temp.leftedge

      WIDGET_CONTROL, widget_struct.text2, GET_VALUE = value
      !VAR.AutoWndw.RIGHT_EDGE = value < $
           widget_struct.config_temp.rightedge

      IF (!VAR.AutoWndw.RIGHT_EDGE LE !VAR.AutoWndw.LEFT_EDGE) THEN BEGIN
         mssg = ['Right edge must be larger than left edge. Try again.']
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY
         RETURN
      ENDIF

      WIDGET_CONTROL, widget_struct.text3, GET_VALUE = value
      !VAR.AutoWndw.BOTTOM_EDGE = value > $
           widget_struct.config_temp.bottomedge

      WIDGET_CONTROL, widget_struct.text4, GET_VALUE = value
      !VAR.AutoWndw.TOP_EDGE = value < $
           widget_struct.config_temp.topedge

      IF (!VAR.AutoWndw.TOP_EDGE LE !VAR.AutoWndw.BOTTOM_EDGE) THEN BEGIN
         mssg = ['Top edge must be larger than bottom edge. Try again.']
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY
         RETURN
      ENDIF

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN

   END

   ; Cancel.

   event.id EQ widget_struct.cancel_button : BEGIN

      !VAR.AutoWndw.CANCEL = 1

      ; Set all widgets back to current values, in case any
      ; temporary changes had been made.

      WIDGET_CONTROL, widget_struct.text1, $
             SET_VALUE = !VAR.AutoWndw.LEFT_EDGE
      WIDGET_CONTROL, widget_struct.text2, $
             SET_VALUE = !VAR.AutoWndw.RIGHT_EDGE
      WIDGET_CONTROL, widget_struct.text3, $
             SET_VALUE = !VAR.AutoWndw.BOTTOM_EDGE
      WIDGET_CONTROL, widget_struct.text4, $
             SET_VALUE = !VAR.AutoWndw.TOP_EDGE

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN

   END

   ; Reset to Defaults.

   event.id EQ widget_struct.reset_button : BEGIN

      !VAR.AutoWndw.INITIATE = 1
      SetPlotFileValues, widget_struct.state, 1

      ; Set all widgets to current (which is now defaults).

      WIDGET_CONTROL, widget_struct.text1, $
           SET_VALUE = !VAR.AutoWndw.LEFT_EDGE
      WIDGET_CONTROL, widget_struct.text2, $
           SET_VALUE = !VAR.AutoWndw.RIGHT_EDGE
      WIDGET_CONTROL, widget_struct.text3, $
           SET_VALUE = !VAR.AutoWndw.BOTTOM_EDGE
      WIDGET_CONTROL, widget_struct.text4, $
           SET_VALUE = !VAR.AutoWndw.TOP_EDGE
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY

END ; SetPlotFileValues_eh

;***************************************************************************
PRO SetPlotFileValues_gui, State
;***************************************************************************
; User interface for setting configurable parameter values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

; default structure
config_defaults = { $
   leftedge   : !VAR.AutoWndw.LEFT_EDGE, $
   rightedge  : !VAR.AutoWndw.RIGHT_EDGE, $
   bottomedge : !VAR.AutoWndw.BOTTOM_EDGE, $
   topedge    : !VAR.AutoWndw.TOP_EDGE }

config_temp = { $
   leftedge   : 0, $
   rightedge  : State.sizex, $
   bottomedge : 0, $
   topedge    : State.sizey }

base0 = WIDGET_BASE( /COLUMN, /MODAL, /TLB_KILL_REQUEST_EVENTS, $
           GROUP_LEADER = WIDGET_INFO(State.wTopWorkBase,/PARENT) )

label0 = WIDGET_LABEL( base0, $
   VALUE='Enter Plot Corner Coordinates', /TLB_KILL_REQUEST_EVENTS)

wIntBase = WIDGET_BASE(base0, /COLUMN, /FRAME)

base2 = WIDGET_BASE( wIntBase, /COLUMN )
text1 = CW_FIELD( base2, VALUE=!VAR.AutoWndw.LEFT_EDGE, XSIZE=6, $
                  /INTEGER, TITLE='Left Edge Coordinate' )

base3 = WIDGET_BASE( wIntBase, /COLUMN )
text2 = CW_FIELD( base3, VALUE=!VAR.AutoWndw.RIGHT_EDGE, XSIZE=6, $
                  /INTEGER, TITLE='Right Edge Coordinate' )

base4 = WIDGET_BASE( wIntBase, /COLUMN )
text3 = CW_FIELD( base4, VALUE=!VAR.AutoWndw.BOTTOM_EDGE, XSIZE=6, $
                  /INTEGER, TITLE='Bottom Edge Coordinate' )

base5 = WIDGET_BASE( wIntBase, /COLUMN )
text4 = CW_FIELD( base5, VALUE=!VAR.AutoWndw.TOP_EDGE, XSIZE=6, $
                  /INTEGER, TITLE='Top Edge Coordinate' )

base6 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( base6, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( base6, VALUE='Cancel' )
reset_button = WIDGET_BUTTON( base6, VALUE='Reset to Defaults' )

widget_struct = { $
   state           : State, $
   wTopWorkBase    : State.wTopWorkBase, $
   text1           : text1, $
   text2           : text2, $
   text3           : text3, $
   text4           : text4, $
   ok_button       : ok_button, $
   cancel_button   : cancel_button, $
   reset_button    : reset_button, $
   config_temp     : config_temp, $
   config_defaults : config_defaults }

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct, /NO_COPY
WIDGET_CONTROL, base0, /REALIZE
XMANAGER, 'SetPlotFileValues_gui', base0, $
          EVENT_HANDLER='SetPlotFileValues_eh'

END ; SetPlotFileValues_gui

;***************************************************************************
PRO SetPlotFileValues, State, FromGui
;***************************************************************************
; Set values for plotting images from the animiation window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct

   IF (!VAR.AutoWndw.INITIATE) THEN BEGIN
      minx = 255
      maxx = State.sizex - 256
      maxy = State.sizey - 1

      whichorbit = (State.curframe GT 9)
      num_band = CoordStruct.(whichorbit).num_band
      IF (num_band EQ 1) THEN BEGIN
         image = TVRD(/ORDER, TRUE=0)
         dim1 = 0
      ENDIF ELSE BEGIN
         image = TVRD(/ORDER, TRUE=1)
         dim1 = 1
      ENDELSE

      ndxs = WHERE(image GT 0, numndxs)
      IF (numndxs GT 0) THEN BEGIN
         td_ndx = ARRAY_INDICES(image, ndxs)
         minx = MIN(td_ndx[dim1,*]) - 1
         maxx = MAX(td_ndx[dim1,*]) + 1
         maxy = MAX(td_ndx[dim1+1,*])
      ENDIF
      td_ndxs = 0
      image   = 0
      ndxs    = 0

      !VAR.AutoWndw.BOTTOM_EDGE = 0    ; Pixel coord along of bottom edge
                                       ; of rectangle in image to plot.
      !VAR.AutoWndw.TOP_EDGE = maxy
                                       ; Number of pixels along.
      !VAR.AutoWndw.LEFT_EDGE = minx   ; Pixel coord across of left edge
                                       ; of rectangle in image to plot.
      !VAR.AutoWndw.RIGHT_EDGE = maxx
                                       ; Number of pixels across.

      delx = maxx - minx + 1
      WHILE (delx / 4 * 4 NE delx) DO BEGIN
         maxx += 1
         delx = maxx - minx + 1
      ENDWHILE
      !VAR.AutoWndw.RIGHT_EDGE = maxx

      !VAR.AutoWndw.INITIATE = 0
   ENDIF

   !VAR.AutoWndw.CANCEL = 0
   IF (~ FromGui) THEN SetPlotFileValues_gui, State

END ; SetPlotFileValues

;***************************************************************************
PRO AdjustImageBounds, State
;***************************************************************************
; Adjust the edge coordinates of the image rectangle to acquire so it
; satisfies these conditions:  I = image, W = window
;   Irgt - Ilft <= Wrgt - Wlft   ;  Itop - Ibot <= Wtop - Wbot
;   Ilft >= Wlft                 ;  Irgt <= Wrgt
;   Ibot >= Wbot                 ;  Itop <= Wtop
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Initialize temporary variables
   ;------------------------------------------------------------------------

   Ilft = !VAR.AutoWndw.LEFT_EDGE
   Irgt = !VAR.AutoWndw.RIGHT_EDGE
   Ibot = !VAR.AutoWndw.BOTTOM_EDGE
   Itop = !VAR.AutoWndw.TOP_EDGE

   Wlft = 0
   Wrgt = State.sizex - 1
   Wbot = 0
   Wtop = State.sizey - 1

   ;------------------------------------------------------------------------
   ; Adjust edges to satisfy condition that image width and height must be
   ; smaller than window width and height. Split any overage between the
   ; begin and end coordinates for both width and height.
   ;------------------------------------------------------------------------

   IF (Irgt - Ilft GT Wrgt - Wlft) THEN BEGIN
      split = Irgt - Ilft - Wrgt + Wlft
      Ilft += split / 2
      Irgt -= split - split / 2
   ENDIF

   IF (Itop - Ibot GT Wtop - Wbot) THEN BEGIN
      split = Itop - Ibot - Wtop + Wbot
      Ibot += split / 2
      Itop -= split - split / 2
   ENDIF

   ;------------------------------------------------------------------------
   ; Adjust edges to satisfy condition that the image must be completely
   ; contained within the window.
   ;------------------------------------------------------------------------

   IF (Ilft LT Wlft) THEN BEGIN
      diff = Wlft - Ilft
      Ilft += diff
      Irgt += diff
   ENDIF

   IF (Irgt GT Wrgt) THEN BEGIN
      diff = Irgt - Wrgt
      Irgt -= diff
      Ilft -= diff
   ENDIF

   IF (Ibot LT Wbot) THEN BEGIN
      diff = Wbot - Ibot
      Ibot += diff
      Itop += diff
   ENDIF

   IF (Itop GT Wtop) THEN BEGIN
      diff = Itop - Wtop
      Ibot -= diff
      Itop -= diff
   ENDIF

   ;------------------------------------------------------------------------
   ; If successful, replace image edge coordinates with adjusted ones.
   ;------------------------------------------------------------------------

   !VAR.AutoWndw.LEFT_EDGE   = Ilft
   !VAR.AutoWndw.RIGHT_EDGE  = Irgt
   !VAR.AutoWndw.BOTTOM_EDGE = Ibot
   !VAR.AutoWndw.TOP_EDGE    = Itop

END ; AdjustImageBounds

;***************************************************************************
PRO MakeKmlFile, Dirname, Filename, Name, MinLon, MaxLon, MinLat, MaxLat
;***************************************************************************
; Construct a .kml file for use in Google Earth with a .png image file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize the .kml template.
;---------------------------------------------------------------------------

Description = 'KML file and color-indexed PNG image generated by MINX'

Template = ['<?xml version="1.0" encoding="UTF-8"?>', $
            '<kml xmlns="http://www.opengis.net/kml/2.2">', $
            '<GroundOverlay>', $
            '   <name>#NAME#</name>', $
            '   <description>#DESCRIPTION#</description>', $
            '   <color>ffffffff</color>', $
            '   <Icon>', $
            '      <href>#FILENAME#</href>', $
            '   </Icon>', $
            '   <LatLonBox>', $
            '      <north>#NORTHEDGE#</north>', $
            '      <south>#SOUTHEDGE#</south>', $
            '      <east>#EASTEDGE#</east>', $
            '      <west>#WESTEDGE#</west>', $
            '   </LatLonBox>', $
            '</GroundOverlay>', $
            '</kml>']

nRow = N_ELEMENTS(Template)

;---------------------------------------------------------------------------
; Place the input parameters into an array. Place the search keys in a
; parallel array.
;---------------------------------------------------------------------------

min_lon = STRTRIM(STRING(FORMAT='(F9.4)',MinLon),2)
max_lon = STRTRIM(STRING(FORMAT='(F9.4)',MaxLon),2)
min_lat = STRTRIM(STRING(FORMAT='(F9.4)',MinLat),2)
max_lat = STRTRIM(STRING(FORMAT='(F9.4)',MaxLat),2)

Parms = [Filename, Name, Description, min_lon, max_lon,  min_lat, max_lat]

Keys = ['#FILENAME#', '#NAME#',      '#DESCRIPTION#', '#WESTEDGE#', $
        '#EASTEDGE#', '#SOUTHEDGE#', '#NORTHEDGE#']

nParm = N_ELEMENTS(Parms)

;---------------------------------------------------------------------------
; Replace each of the 7 search keys (strings in the template between #
; characters) with the corresponding passed string.
;---------------------------------------------------------------------------

Copy = Template

FOR isub=0,nParm-1 DO BEGIN
   FOR irow=0,nRow-1 DO BEGIN
      nPos = STRPOS(Copy[irow], Keys[iSub])
      IF (nPos GT 0) THEN BEGIN
         str1 = STRMID(Copy[irow], 0, nPos)
         str2 = STRMID(Copy[irow], nPos + STRLEN(Keys[iSub]))
         Copy[irow] = str1 + Parms[iSub] + str2
         BREAK
      ENDIF
   ENDFOR
ENDFOR

;---------------------------------------------------------------------------
; Write the kml data to a text file.
;---------------------------------------------------------------------------

nPos = STRPOS(Filename, '.')
str1 = STRMID(Filename, 0, nPos)
Pathname = Dirname + str1 + '.kml'

OPENW, lunit, Pathname, /GET_LUN

FOR irow=0,nRow-1 DO BEGIN
   PRINTF, lunit, Copy[irow]
ENDFOR

FREE_LUN, lunit
Template = 0
Copy = 0

END  ;  MakeKmlFile

;***************************************************************************
PRO WriteGoogleFile, State, Pathname, Image, Left, Right, $
                     Bottom, Top
;***************************************************************************
; Routine to save selected camera image as a .png file together with a .kml
; file that will overlay on Google Earth.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_main_parms, MainOption
COMMON coord_data, CoordStruct

;---------------------------------------------------------------------------
; Construct the name to be used for the image.
;---------------------------------------------------------------------------

whichorbit = (State.curframe GT 9)

path  = CoordStruct.(whichorbit).PathNum
orbit = CoordStruct.(whichorbit).OrbitNum
date  = CoordStruct.(whichorbit).OrbitDate
SceneName = 'Path '  + STRTRIM(STRING(path),2)  + ', ' + $
            'Orbit ' + STRTRIM(STRING(orbit),2) + ', ' + date

;---------------------------------------------------------------------------
; Get the image size and construct x and y arrays containing window coords
; for a single row and a single column. Reverse the screen coords to be
; compatible with the MISR along-track standard.
;---------------------------------------------------------------------------

nx = Right - Left + 1
ny = Top - Bottom + 1
nPts = nx * ny

Xary = INDGEN(nx) + Left
Yary = INDGEN(ny) + Bottom
Yary = REVERSE(Yary)

;---------------------------------------------------------------------------
; Construct a 2D input array of window coords for the entire image. Then
; reform it into a 1D array for input to WndwCrdToMisrCrd.
;---------------------------------------------------------------------------

WndwCoord = INTARR(2, nPts)

CoordX = INTARR(nx, ny)
FOR iy=0,ny-1 DO BEGIN
   CoordX[*, iy] = Xary
ENDFOR
WndwCoord[0,*] = REFORM(CoordX, nPts)

CoordY = INTARR(nx, ny)
FOR ix=0,nx-1 DO BEGIN
   CoordY[ix, *] = Yary
ENDFOR
WndwCoord[1,*] = REFORM(CoordY, nPts)

Xary = 0
Yary = 0
CoordX = 0
CoordY = 0

;---------------------------------------------------------------------------
; Convert window coords to lat-lon coords.
;---------------------------------------------------------------------------

WndwCrdToMisrCrd,  State.Curframe, WndwCoord, MisrCoord,   Retval
MisrCrdToSomCrd,   State.Curframe, MisrCoord, SomCoord,    Retval
SomCrdToLonlatCrd, State.Curframe, SomCoord,  LonlatCoord, Retval

LonCoord = REFORM(LonlatCoord[0,*])
LatCoord = REFORM(LonlatCoord[1,*])

WndwCoord = 0
MisrCoord = 0
LonlatCoord = 0

;---------------------------------------------------------------------------
; Correct for possible crossing of the dateline.
;---------------------------------------------------------------------------

IF (MIN(LonCoord) LT -179.0 AND MAX(LonCoord) GT 179.0) THEN BEGIN
   ndxs = WHERE(LonCoord LT 0.0, numndxs)
   IF (numndxs GT 0) THEN BEGIN
      LonCoord[ndxs] += 360.D
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Set the lat-lon resample interval in degrees. The value approximates the
; size of a 275 m pixel at the latitude on the plot nearest to the equator
; (avg of lat and lon spacing).
;---------------------------------------------------------------------------

MinLat = MIN(LatCoord)
MaxLat = MAX(LatCoord)
small_lat = ABS(MinLat) < ABS(MaxLat)

dist_perdeg_lon = COS(small_lat * !DTOR) * !KON.Misc.EarthCircDiv360
resamp = 0.0025D
resamp = ROUND((275.0D / dist_perdeg_lon + resamp) * 1000.0) / 2000.D

;---------------------------------------------------------------------------
; Get the minimum and maximum values of lat-lon.
;---------------------------------------------------------------------------

MinLon = FLOOR(MIN(LonCoord / resamp)) * resamp
MaxLon = CEIL( MAX(LonCoord / resamp)) * resamp
MinLat = FLOOR(MinLat / resamp) * resamp
MaxLat = CEIL( MaxLat / resamp) * resamp

;---------------------------------------------------------------------------
; Triangulate the image coordinates to enable interpolation.
;---------------------------------------------------------------------------

TRIANGULATE, LonCoord, LatCoord, triangles
  
;---------------------------------------------------------------------------
; Extract the separate r, g and b images from the rgb image. Then resample
; the r, g and b images to the resample interval which is a uniform grid
; in lat-lon. Get the minimum and maximum values of the resampled images.
;---------------------------------------------------------------------------

num_band = CoordStruct.(whichorbit).num_band
IF (num_band EQ 1) THEN BEGIN
   image_r = REFORM(Image[*,*], nPts)

   rr = TRIGRID(LonCoord, LatCoord, image_r, triangles, [resamp, resamp], $
                [MinLon, MinLat, MaxLon, MaxLat])  
   gg = rr
   bb = rr

   minval = MIN(rr)
   maxval = MAX(rr)
ENDIF ELSE BEGIN
   image_r = REFORM(Image[0,*,*], nPts)
   image_g = REFORM(Image[1,*,*], nPts)
   image_b = REFORM(Image[2,*,*], nPts)

   rr = TRIGRID(LonCoord, LatCoord, image_r, triangles, [resamp, resamp], $
                [MinLon, MinLat, MaxLon, MaxLat])  
   gg = TRIGRID(LonCoord, LatCoord, image_g, triangles, [resamp, resamp], $
                [MinLon, MinLat, MaxLon, MaxLat])  
   bb = TRIGRID(LonCoord, LatCoord, image_b, triangles, [resamp, resamp], $
                [MinLon, MinLat, MaxLon, MaxLat])  

   minval = MIN(rr) < MIN(gg) < MIN(bb)
   maxval = MAX(rr) > MAX(gg) > MAX(bb)
ENDELSE

image_r   = 0
image_g   = 0
image_b   = 0
LonCoord  = 0
LatCoord  = 0
triangles = 0

;---------------------------------------------------------------------------
; Create a new rgb image in which the separate r, g and b images are
; recombined (if rgb). Add an alpha channel for transparency.
;---------------------------------------------------------------------------

sizes = SIZE(rr)
rgb = BYTARR(4, sizes[1], sizes[2])

IF (num_band EQ 1) THEN BEGIN
   rgb[0,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[1,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[2,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
ENDIF ELSE BEGIN
   rgb[0,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[1,*,*] = BYTSCL(gg, MIN=minval, MAX=maxval)
   rgb[2,*,*] = BYTSCL(bb, MIN=minval, MAX=maxval)
ENDELSE

temp_ary = BYTARR(sizes[1], sizes[2]) + 255
ndxs = WHERE(rr EQ 0 AND gg EQ 0 AND bb EQ 0, numndxs)
IF (numndxs) THEN temp_ary[ndxs] = 0     ; set transparent pixels
rgb[3,*,*] = temp_ary

rr  = 0
gg  = 0
bb  = 0
ndxs = 0
temp_ary = 0

;---------------------------------------------------------------------------
; Write image file as .png plus a .kml file.
;---------------------------------------------------------------------------

WRITE_PNG, Pathname, rgb 

iPos = STRPOS(Pathname, !KON.Misc.Slash, /REVERSE_SEARCH)
Dirname = STRMID(Pathname, 0, iPos+1)
Filename = STRMID(Pathname, iPos+1)

MakeKmlFile, Dirname, Filename, SceneName, MinLon, MaxLon, MinLat, MaxLat

rgb = 0

END  ;  WriteGoogleFile

;***************************************************************************
PRO WritePngTransparent, State, Filename, Image
;***************************************************************************
; Routine to save selected camera image in PNG format w/ transparent edges.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Create a new rgb image that includes an alpha channel for transparency.
;---------------------------------------------------------------------------

sizes = SIZE(image)
num_band = (sizes[0] EQ 2) ? 1 : 3
first_xy = (sizes[0] EQ 2) ? 1 : 2

rgb = BYTARR(4, sizes[first_xy], sizes[first_xy+1])

IF (num_band EQ 1) THEN BEGIN
   rgb[0,*,*] = BYTSCL(image, MIN=minval, MAX=maxval)
   rgb[1,*,*] = rgb[0,*,*]
   rgb[2,*,*] = rgb[0,*,*]
   ndxs = WHERE(image EQ 0, numndxs)
ENDIF ELSE BEGIN
   rr = REFORM(image[0,*,*])
   gg = REFORM(image[1,*,*])
   bb = REFORM(image[2,*,*])
   rgb[0,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[1,*,*] = BYTSCL(gg, MIN=minval, MAX=maxval)
   rgb[2,*,*] = BYTSCL(bb, MIN=minval, MAX=maxval)
   ndxs = WHERE(rr EQ 0 AND gg EQ 0 AND bb EQ 0, numndxs)
ENDELSE

temp_ary = BYTARR(sizes[first_xy], sizes[first_xy+1]) + 255
IF (numndxs) THEN temp_ary[ndxs] = 0     ; set transparent pixels
rgb[3,*,*] = temp_ary

;---------------------------------------------------------------------------
; Write image file as .png plus a .kml file.
;---------------------------------------------------------------------------

WRITE_PNG, Filename, rgb, /ORDER

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

rr = 0
gg = 0
bb = 0
rgb = 0
ndxs = 0
temp_ary = 0

END  ;  WritePngTransparent

;***************************************************************************
PRO WriteGeoTiffFile, State, Filename, Image, Left, Right, $
                      Bottom, Top
;***************************************************************************
; Routine to save selected camera image in GeoTIFF format.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_main_parms, MainOption
COMMON coord_data, CoordStruct

;---------------------------------------------------------------------------
; Set the resample interval at which to interpolate UTM coords projected
; from lat-lon coords.
;---------------------------------------------------------------------------

RESAMP_INT = 275.0

;---------------------------------------------------------------------------
; Construct the name to be used for the image.
;---------------------------------------------------------------------------

whichorbit = (State.curframe GT 9)

path  = CoordStruct.(whichorbit).PathNum
orbit = CoordStruct.(whichorbit).OrbitNum
date  = CoordStruct.(whichorbit).OrbitDate
SceneName = 'Path '  + STRTRIM(STRING(path),2)  + ', ' + $
            'Orbit ' + STRTRIM(STRING(orbit),2) + ', ' + date

;---------------------------------------------------------------------------
; Get the image size and construct x and y arrays containing window coords
; for a single row and a single column. Reverse the screen coords to be
; compatible with the MISR along-track standard.
;---------------------------------------------------------------------------

nx = Right - Left + 1
ny = Top - Bottom + 1
nPts = nx * ny

Xary = INDGEN(nx) + Left
Yary = INDGEN(ny) + Bottom

Yary = REVERSE(Yary)

;---------------------------------------------------------------------------
; Construct a 2D input array of window coords for the entire image. Then
; reform it into a 1D array for input to WndwCrdToMisrCrd.
;---------------------------------------------------------------------------

WndwCoord = INTARR(2, nPts)

CoordX = INTARR(nx, ny)
FOR iy=0,ny-1 DO BEGIN
   CoordX[*, iy] = Xary
ENDFOR
WndwCoord[0,*] = REFORM(CoordX, nPts)

CoordY = INTARR(nx, ny)
FOR ix=0,nx-1 DO BEGIN
   CoordY[ix, *] = Yary
ENDFOR
WndwCoord[1,*] = REFORM(CoordY, nPts)

Xary = 0
Yary = 0
CoordX = 0
CoordY = 0

;---------------------------------------------------------------------------
; Convert window coords to lat-lon coords.
;---------------------------------------------------------------------------

WndwCrdToMisrCrd,  State.Curframe, WndwCoord, MisrCoord,   Retval
MisrCrdToSomCrd,   State.Curframe, MisrCoord, SomCoord,    Retval
SomCrdToLonlatCrd, State.Curframe, SomCoord,  LonlatCoord, Retval

LonCoord = REFORM(LonlatCoord[0,*])
LatCoord = REFORM(LonlatCoord[1,*])

WndwCoord = 0
MisrCoord = 0
LonlatCoord = 0

;---------------------------------------------------------------------------
; Correct for possible crossing of the dateline.
;---------------------------------------------------------------------------

IF (MIN(LonCoord) LT -179.0 AND MAX(LonCoord) GT 179.0) THEN BEGIN
   ndxs = WHERE(LonCoord LT 0.0, numndxs)
   IF (numndxs GT 0) THEN BEGIN
      LonCoord[ndxs] += 360.D
   ENDIF
   ndxs = 0
ENDIF

;---------------------------------------------------------------------------
; Get the minimum and maximum values of lat-lon. Then compute parameters
; for the UTM projection.
;---------------------------------------------------------------------------

MinLat = MIN(LatCoord, MAX=MaxLat)
MinLon = MIN(LonCoord, MAX=MaxLon)

small_lat = ABS(MinLat) < ABS(MaxLat)

half_lat = (MaxLat - MinLat) / 2.0
half_lon = (MaxLon - MinLon) / 2.0

cntr_lat = MinLat + half_lat
cntr_lon = MinLon + half_lon

merc_zone = CEIL((cntr_lon + 180.0) / 6.0)

false_east  = 500000.0
IF (MinLat GE 0.0) THEN BEGIN
   false_north = 0.0
   wgs84_CScode = 32600 + merc_zone
ENDIF ELSE BEGIN
   false_north = 10000000.0
   wgs84_CScode = 32700 + merc_zone
ENDELSE

;---------------------------------------------------------------------------
; Geographic coordinates don't appear to work as input to GeoTiff, so
; convert the image coords to a GeoTiff supported projection first.
; UTM projection (101), WGS-84 datum (8).
;---------------------------------------------------------------------------

map_proj = MAP_PROJ_INIT(101, DATUM=8, $     ; /RELAXED, $
                         LIMIT=[MinLat, MinLon, MaxLat, MaxLon], $
                         CENTER_LATITUDE=cntr_lat, $
                         CENTER_LONGITUDE=cntr_lon)

utm_map = MAP_PROJ_FORWARD(LonCoord, LatCoord, MAP_STRUCTURE=map_proj)

LonCoord = 0
LatCoord = 0
map_proj = 0

;---------------------------------------------------------------------------
; Set the x-y resample interval equal to the size of a 275 m pixel. Then
; get the min and max of the eastings and northings.
;---------------------------------------------------------------------------

MinX = MIN(utm_map[0,*], MAX=MaxX)
MinY = MIN(utm_map[1,*], MAX=MaxY)

MinX = FLOOR(MinX / RESAMP_INT) * RESAMP_INT
MaxX = CEIL (MaxX / RESAMP_INT) * RESAMP_INT
MinY = FLOOR(MinY / RESAMP_INT) * RESAMP_INT
MaxY = CEIL (MaxY / RESAMP_INT) * RESAMP_INT

;---------------------------------------------------------------------------
; Triangulate the image coordinates to enable interpolation.
;---------------------------------------------------------------------------

TRIANGULATE, utm_map[0,*], utm_map[1,*], triangles
  
;---------------------------------------------------------------------------
; Extract the separate r, g and b images from rgb image. Then resample
; the r, g and b images to the resample interval which is a uniform grid
; in lat-lon. Get the minimum and maximum values of the resampled images.
;---------------------------------------------------------------------------

num_band = CoordStruct.(whichorbit).num_band
trigrid_save = DBLARR((MaxX-MinX) / RESAMP_INT + 1, $
                      (MaxY-MinY) / RESAMP_INT + 1)

IF (num_band EQ 1) THEN BEGIN
   image_r = REFORM(Image[*,*], nPts)

   rr = TRIGRID(utm_map[0,*], utm_map[1,*], image_r, triangles, $
                [RESAMP_INT, RESAMP_INT], [MinX, MinY, MaxX, MaxY], $
                INPUT=trigrid_save, XGRID=x_grid, YGRID=y_grid)  

   minval = MIN(rr)
   maxval = MAX(rr)
ENDIF ELSE BEGIN
   image_r = REFORM(Image[0,*,*], nPts)
   image_g = REFORM(Image[1,*,*], nPts)
   image_b = REFORM(Image[2,*,*], nPts)

   rr = TRIGRID(utm_map[0,*], utm_map[1,*], image_r, triangles, $
           [RESAMP_INT, RESAMP_INT], [MinX, MinY, MaxX, MaxY], $
           INPUT=trigrid_save, XGRID=x_grid, YGRID=y_grid)
   gg = TRIGRID(utm_map[0,*], utm_map[1,*], image_g, triangles, $
           [RESAMP_INT, RESAMP_INT], [MinX, MinY, MaxX, MaxY], $
           INPUT=trigrid_save)  
   bb = TRIGRID(utm_map[0,*], utm_map[1,*], image_b, triangles, $
           [RESAMP_INT, RESAMP_INT], [MinX, MinY, MaxX, MaxY], $
           INPUT=trigrid_save)  

   minval = MIN(rr) < MIN(gg) < MIN(bb)
   maxval = MAX(rr) > MAX(gg) > MAX(bb)
ENDELSE
  
image_r = 0
image_g = 0
image_b = 0
utm_map = 0
triangles = 0
trigrid_save = 0

;---------------------------------------------------------------------------
; Create a new rgb image in which separate r, g and b images are recombined
; (if rgb).
;---------------------------------------------------------------------------

sizes = SIZE(rr)
rgb = BYTARR(3, sizes[1], sizes[2])

IF (num_band EQ 1) THEN BEGIN
   rgb[0,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[1,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[2,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
ENDIF ELSE BEGIN
   rgb[0,*,*] = BYTSCL(rr, MIN=minval, MAX=maxval)
   rgb[1,*,*] = BYTSCL(gg, MIN=minval, MAX=maxval)
   rgb[2,*,*] = BYTSCL(bb, MIN=minval, MAX=maxval)
ENDELSE

rr = 0
gg = 0
bb = 0

;---------------------------------------------------------------------------
; Write the projected image and GeoTiff parameters to the GeoTiff file.
; Reverse the array y-coord again, so we can set the orientation to 1, which
; means y(0) is at the top.
;---------------------------------------------------------------------------

ss = SIZE(rgb)

xscl = (x_grid[ss[2]-1] - x_grid[0] + 275.0) / (ss[2])
yscl = (y_grid[ss[3]-1] - y_grid[0] + 275.0) / (ss[3])

scl_tag = DOUBLE([xscl, yscl, 0])
tie_pts = DOUBLE([0, 0, 0, x_grid[0], y_grid[ss[3]-1], 0])

MinxCredit = 'GeoTIFF image generated by MINX'

geotag = { MODELTIEPOINTTAG       : tie_pts, $      ; tie points
           MODELPIXELSCALETAG     : scl_tag, $      ; scales
           GTMODELTYPEGEOKEY      : 1, $            ; projection type CS
           GTRASTERTYPEGEOKEY     : 1, $            ; pixel is area
           GTCITATIONGEOKEY       : MinxCredit, $   ; annotation
           GEOGRAPHICTYPEGEOKEY   : 4326, $         ; GCS_WGS_84
           GEOGLINEARUNITSGEOKEY  : 9001, $         ; meters
           GEOGANGULARUNITSGEOKEY : 9102, $         ; fractional degrees
           GEOGCITATIONGEOKEY     : SceneName,  $   ; annotation
           PROJECTEDCSTYPEGEOKEY  : wgs84_CScode, $ ; for UTM N or S and zone
           PROJNATORIGINLATGEOKEY : cntr_lat, $     ; center latitude
           PROJNATORIGINLONGEOKEY : cntr_lon, $     ; center longitude
           PROJLINEARUNITSGEOKEY  : 9001 }          ; meters

rgb = REVERSE(rgb, 3)
WRITE_TIFF, Filename, rgb, COMPRESSION=1, ORIENTATION=1, geotiff=geotag

tie_pts = 0
scl_tag = 0
geotag  = 0
x_grid  = 0
y_grid  = 0
rgb     = 0

END  ;  WriteGeoTiffFile

;***************************************************************************
PRO Op_WriteImageFile, State, BegCam, EndCam, ImageType, UsrOrSys, $
                       TypeName, pRgn
;***************************************************************************
; Routine to save selected camera images in TIFF, JPEG or GIF files or a
; single MP4 file and in 8-bit grayscale or color format. The function
; function can be called either from the menu by the user or by another
; function.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_main_parms, MainOption
COMMON coord_data, CoordStruct

   whichorbit = (BegCam GT 9 AND EndCam GT 9)

   IType = ImageType

   ;------------------------------------------------------------------------
   ; Get the region name if a region was passed.
   ;------------------------------------------------------------------------

   rgn_name = ''

   iBand = (!VAR.DataRgn.SHOW_BAND_TYPE LT 2) ? !VAR.DataRgn.SHOW_BAND_TYPE : 0

   IF (PTR_VALID(pRgn)) THEN rgn_name = (*((*pRgn).pData)).name[iBand]

   ;------------------------------------------------------------------------
   ; Test if the user has a license to use the MP4 feature. If not, generate
   ; 9 JPEGs instead. Only notify user if the request is from the Save
   ; Camera Image menu option.
   ;------------------------------------------------------------------------

   IF (IType EQ !KON.PlotTyp.DO_MP4) THEN BEGIN
      has_license = TestIfMP4_License(1)
      IF (~ has_license) THEN BEGIN
         IF (UsrOrSys EQ 1) THEN BEGIN
            mssg = ['You do not have an IDL license enabling', $
                    'you to use the MP4 feature. Images will', $
                    'be saved in 9 camera JPEG files instead.']
            rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
         ENDIF
         IType = !KON.PlotTyp.DO_JPG
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Get the rectangular coordinates to write from the user. Also get
   ; whether to image the red or blue band retrieval.
   ;------------------------------------------------------------------------

reenter_coords:
   IF (UsrOrSys EQ 1) THEN BEGIN
      SetPlotFileValues, State, 0
      IF (!VAR.AutoWndw.CANCEL) THEN RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Set up the bounding box.
   ;------------------------------------------------------------------------

   num_across = !VAR.AutoWndw.RIGHT_EDGE - !VAR.AutoWndw.LEFT_EDGE + 1
   num_along  = !VAR.AutoWndw.TOP_EDGE - !VAR.AutoWndw.BOTTOM_EDGE + 1
   
   LEFT_EDGE   = !VAR.AutoWndw.LEFT_EDGE
   RIGHT_EDGE  = !VAR.AutoWndw.RIGHT_EDGE
   BOTTOM_EDGE = !VAR.AutoWndw.BOTTOM_EDGE
   TOP_EDGE    = !VAR.AutoWndw.TOP_EDGE

   ;------------------------------------------------------------------------
   ; If user requested a special window, draw a bounding rectangle so he can
   ; confirm it is correct.
   ;------------------------------------------------------------------------

   IF (UsrOrSys EQ 1) THEN BEGIN
      SafeWSET, (*State.pwinHdl)[State.curframe], didit

      ;---------------------------------------------------------------------
      ; Read the image, then copy the original image back to eliminate the
      ; colored rectangle.
      ;---------------------------------------------------------------------

      IF (CoordStruct.(whichorbit).num_band EQ 1) THEN BEGIN
         image = TVRD(LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, $
                      /ORDER, TRUE=0)
      ENDIF ELSE BEGIN
         image = TVRD(LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, $
                      /ORDER, TRUE=1)
      ENDELSE

      PLOT, [LEFT_EDGE, RIGHT_EDGE-1, RIGHT_EDGE-1, LEFT_EDGE, LEFT_EDGE], $
            [BOTTOM_EDGE, BOTTOM_EDGE, TOP_EDGE-1, TOP_EDGE-1, BOTTOM_EDGE],$
            XRANGE=[0,State.sizex-1], YRANGE=[0,State.sizey-1], $
            POSITION=[0,0,State.sizex-1,State.sizey-1], $
            COLOR=255, LINESTYLE=2, THICK=1, XSTYLE=5, YSTYLE=5, $
            /DEVICE, /NOERASE

      RedrawWindow, State, State.curframe  ;  required

      mssg = 'Is this the image subset you want?'
      doit = DIALOG_MESSAGE( mssg, /QUESTION, /DEFAULT_NO, /CENTER )

      SafeWSET, (*State.pwinHdl)[State.curframe], didit
      IF (CoordStruct.(whichorbit).num_band EQ 1) THEN $
         TV, image, LEFT_EDGE, BOTTOM_EDGE, /ORDER, TRUE=0
      IF (CoordStruct.(whichorbit).num_band EQ 4) THEN $
         TV, image, LEFT_EDGE, BOTTOM_EDGE, /ORDER, TRUE=1

      RedrawWindow, State, State.curframe  ;  required
      image = 0

      IF (STRUPCASE(doit) NE 'YES') THEN GOTO, reenter_coords
   ENDIF

   ;------------------------------------------------------------------------
   ; Loop over the requested cameras, writing each to a file.
   ;------------------------------------------------------------------------

   chance_it = 1

   FOR icam = BegCam,EndCam DO BEGIN

      SafeWSET, (*State.pwinHdl)[icam], didit

      ;---------------------------------------------------------------------
      ; Make a copy of the desired portion of the window image.
      ;---------------------------------------------------------------------

      true_val = (CoordStruct.(whichorbit).num_band EQ 1) ? 0 : 1
      image = TVRD(LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, /ORDER, $
                   TRUE=true_val)

      ;---------------------------------------------------------------------
      ; Create a hidden window and copy the image to it. Then overpost all
      ; the additional data if active.
      ;---------------------------------------------------------------------

      tmp_wndw = -1
      WINDOW, /FREE, XSIZE=num_across, YSIZE=num_along, /PIXMAP
      tmp_wndw = !D.window

      IF (CoordStruct.(whichorbit).num_band EQ 1) THEN TV, image, /ORDER, TRUE=0
      IF (CoordStruct.(whichorbit).num_band EQ 4) THEN TV, image, /ORDER, TRUE=1
      image = 0

      IF (State.showdots GT 0) THEN $
         RedrawRefDots, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along
      IF (State.showcountry GT 0) THEN $
         RedrawGeography, State, 1, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along
      IF (State.showLLgrid GT 0) THEN $
         RedrawGeography, State, 2, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along
      IF (State.showmapclr GT 0) THEN BEGIN
         ; Special measures if this is autosave height image with color key.
         IF (UsrOrSys EQ 2 AND CoordStruct.(whichorbit).num_band EQ 4) THEN BEGIN
            save_keystate_source = !VAR.DataRgn.MINMAX_SOURCE
            !VAR.DataRgn.MINMAX_SOURCE = !KON.DataRgn.MINMAX_SYS
         ENDIF
         RedrawMapColors, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, pRgn
         IF (UsrOrSys EQ 2 AND CoordStruct.(whichorbit).num_band EQ 4) THEN $
            !VAR.DataRgn.MINMAX_SOURCE = save_keystate_source
      ENDIF
      IF (State.showobjects GT 0) THEN BEGIN
         RedrawObjects, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, pRgn
         RedrawDirArrow, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along, pRgn
      ENDIF
      IF (State.showfire GT 0) THEN $
         RedrawModisFires, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along
      IF (State.showmarker GT 0) THEN $
         RedrawMarkerPts, State, LEFT_EDGE, BOTTOM_EDGE, num_across, num_along

      IF (UsrOrSys EQ 1 AND $
          !VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
         save_sizeX = !VAR.DataRgn.SIZE_X
         !VAR.DataRgn.SIZE_X = num_across
         save_sizeY = !VAR.DataRgn.SIZE_Y
         !VAR.DataRgn.SIZE_Y = num_along
         !VAR.DataRgn.POS_X -= LEFT_EDGE
         !VAR.DataRgn.POS_Y -= BOTTOM_EDGE
         !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_USER_SAVED

         DrawPlumeColorKey, State, 1

         !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_ON_ANIM_WNDW
         !VAR.DataRgn.POS_X += LEFT_EDGE
         !VAR.DataRgn.POS_Y += BOTTOM_EDGE
         !VAR.DataRgn.SIZE_X = save_sizeX
         !VAR.DataRgn.SIZE_Y = save_sizeY
      ENDIF

      ;---------------------------------------------------------------------
      ; Write orbit, block, date and lat/lon coords of first point in upper
      ; left if user requested documentation on maps. First draw a whited-
      ; out rectangle so the text is more readable.
      ;---------------------------------------------------------------------

      old_font = GetFontInfo(0)
      SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
      
      IF (!SAV.Digitize.DOCS_ON_MAPS) THEN BEGIN

         y_ht = (UsrOrSys EQ 1) ? 21 : 36
         rect_coord1 = CONVERT_COORD(0, num_along-1, /DEVICE, /TO_NORMAL)
         rect_coord2 = CONVERT_COORD(num_across-1, num_along-y_ht-1, /DEVICE, $
                                     /TO_NORMAL)

         POLYFILL, [rect_coord1[0], rect_coord1[0], rect_coord2[0], rect_coord2[0]], $
                   [rect_coord1[1], rect_coord2[1], rect_coord2[1], rect_coord1[1]], $
                   COLOR=!VAR.DataRgn.BKGRND_COLOR, /NORMAL
         PLOTS, [rect_coord1[0], rect_coord1[0], rect_coord2[0], rect_coord2[0], $
                 rect_coord1[0]], $
                [rect_coord1[1], rect_coord2[1], rect_coord2[1], rect_coord1[1], $
                 rect_coord1[1]], COLOR=0, THICK=1, /NORMAL
         
         txt_x = 10
         txt_y = num_along
         orbitdate = ''
         toks = STRSPLIT(CoordStruct.(0).OrbitDate, '-', /EXTRACT, COUNT=ntok)
         IF (ntok EQ 3) THEN BEGIN
            months = ['Jan','Feb','March','April','May','June','July','Aug', $
                      'Sept','Oct','Nov','Dec']
            month = months[toks[1]-1]
            orbitdate = month + ' ' + STRTRIM(STRING(FIX(toks[2])),2) + ' ' + $
                        toks[0]
            months = 0
         ENDIF
                  
         orbit_date = 'Orbit ' + STRTRIM(STRING(CoordStruct.(0).OrbitNum),2) + $
            ', ' + orbitdate + ', ' + !KON.Instr.camera_names[icam] + ' Camera'
         txt_coord1 = CONVERT_COORD(txt_x, txt_y-16, /DEVICE, /TO_NORMAL)
         XYOUTS, 0.5, txt_coord1[1], CHARSIZE=1.0, CHARTHICK=1, $
            orbit_date, ALIGNMENT=0.5, COLOR=0, /NORMAL
         
         IF (PTR_VALID(pRgn)) THEN BEGIN
            pNextPt = (*pRgn).pNextLinePt
            IF (PTR_VALID(pNextPt)) THEN BEGIN
               block   = STRTRIM(STRING((*((*pNextPt).pData)).block),2)
               beg_lon = STRTRIM(STRING(FORMAT='(F8.3)',(*((*pNextPt).pData)).lon),2)
               beg_lat = STRTRIM(STRING(FORMAT='(F7.3)',(*((*pNextPt).pData)).lat),2)
               blk_latlon = '1st pt: block ' + block + ', lat: ' + beg_lat + $
                            ', lon: ' + beg_lon
               txt_coord2 = CONVERT_COORD(txt_x, txt_y-32, /DEVICE, /TO_NORMAL)
               XYOUTS, 0.5, txt_coord2[1], CHARSIZE=1.0, CHARTHICK=1, $
                       blk_latlon, ALIGNMENT=0.5, COLOR=0, /NORMAL
              ENDIF
         ENDIF
         
      ENDIF

      SetFontInfo, old_font
      
      ;---------------------------------------------------------------------
      ; If this is an automatic plot of pixels colored by height, draw a
      ; color key at the edge of the image and hope it doesn't cover plot.
      ;---------------------------------------------------------------------

      IF (UsrOrSys EQ 2 AND $
          CoordStruct.(whichorbit).num_band EQ 4 AND $
          TypeName EQ 'Contours_') THEN BEGIN

         IF ((*((*pRgn).pData)).bestht_top[iBand] NE $
             !KON.Misc.BADVALUE_REAL) THEN BEGIN
            save_keystate_show = !VAR.DataRgn.SHOW
            !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_SYS_SAVED
            save_keystate_source = !VAR.DataRgn.MINMAX_SOURCE
            !VAR.DataRgn.MINMAX_SOURCE = !KON.DataRgn.MINMAX_SYS
            save_posX  = !VAR.DataRgn.POS_X
            !VAR.DataRgn.POS_X = 0
            save_posY  = !VAR.DataRgn.POS_Y
            !VAR.DataRgn.POS_Y = 0
            save_sizeX = !VAR.DataRgn.SIZE_X
            !VAR.DataRgn.SIZE_X = num_across
            save_sizeY = !VAR.DataRgn.SIZE_Y
            !VAR.DataRgn.SIZE_Y = num_along
            !VAR.DataRgn.BKGRND_COLOR = 16777215

            DrawPlumeColorKey, State, 1

            !VAR.DataRgn.SIZE_X = save_sizeX
            !VAR.DataRgn.SIZE_Y = save_sizeY
            !VAR.DataRgn.POS_X  = save_posX
            !VAR.DataRgn.POS_Y  = save_posY
            !VAR.DataRgn.MINMAX_SOURCE = save_keystate_source
            !VAR.DataRgn.SHOW = save_keystate_show
         ENDIF
      ENDIF

      ;---------------------------------------------------------------------
      ; Read the revised image out of the hidden window.
      ;---------------------------------------------------------------------

      IF (CoordStruct.(whichorbit).num_band EQ 1) THEN $
         image = TVRD(/ORDER, TRUE=0)
      IF (CoordStruct.(whichorbit).num_band EQ 4) THEN $
         image = TVRD(/ORDER, TRUE=1)

      tmp_wndw = -1
      SafeWSET, State.draw_win, didit

      ;---------------------------------------------------------------------
      ; If this is the first camera to save, get the filename.
      ;---------------------------------------------------------------------

      IF (icam EQ BegCam) THEN BEGIN

reenter_file:
         IF (IType EQ !KON.PlotTyp.DO_TIF)  THEN pext = 'tif'
         IF (IType EQ !KON.PlotTyp.DO_JPG)  THEN pext = 'jpg'
         IF (IType EQ !KON.PlotTyp.DO_GIF)  THEN pext = 'gif'
         IF (IType EQ !KON.PlotTyp.DO_PNG)  THEN pext = 'png'
         IF (IType EQ !KON.PlotTyp.DO_GTIF) THEN pext = 'tif'
         IF (IType EQ !KON.PlotTyp.DO_KML)  THEN pext = 'png'
         IF (IType EQ !KON.PlotTyp.DO_MGIF) THEN pext = 'gif' ; not ready
         IF (IType EQ !KON.PlotTyp.DO_MP4)  THEN pext = 'mp4'

         ;------------------------------------------------------------------
         ; Here if the user is selecting to save an image.
         ;------------------------------------------------------------------

         IF (UsrOrSys EQ 1) THEN BEGIN
            temppre  = 'MINX_O'
            temppost = '-L' + STRTRIM(STRING(BOTTOM_EDGE),2) + 'to' + 'L' + $
                       STRTRIM(STRING(TOP_EDGE),2)
            CreateOrbitDirFile, !SAV.Digitize.PlumeOutDir, temppre, temppost, $
                                 1, pext, CoordStruct.(whichorbit).OrbitNum, 1, $
                                 tempdir, tempfile, Retval
            dlg_title = 'Enter basename of image file to write'
            filename = DIALOG_PICKFILE(FILE=tempfile, /WRITE, /FIX_FILTER, $
                              DEFAULT_EXTENSION=pext, TITLE=dlg_title, $
                              FILTER=['*.'+pext], PATH=tempdir)
            IF (filename EQ '') THEN BEGIN
               image = 0
               RETURN
            ENDIF ELSE BEGIN
               npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH) + 1
               tempdir = STRMID(filename, 0, npos)
               tempfile = STRMID(filename, npos)
            ENDELSE
            IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
               rtrn_val = MakeDirectory(tempdir)
               rtrn_val = ChmodCatchError(tempdir, '777'O)
            ENDIF

         ;------------------------------------------------------------------
         ; Here if the system is saving an image.
         ;------------------------------------------------------------------

         ENDIF ELSE BEGIN
            temppost = rgn_name + '_Plume'
            CreateOrbitDirFile, !SAV.Digitize.PlumeOutDir, '', temppost, $
                                 0, pext, CoordStruct.(whichorbit).OrbitNum, $
                                 1, tempdir, tempfile, Retval
            filename = tempdir + tempfile
         ENDELSE

      ENDIF

      ;---------------------------------------------------------------------
      ; Finish constructing the filename and remove an old file of the same
      ; name if it exists.
      ;---------------------------------------------------------------------

      len = STRLEN(filename)
      IF (IType NE !KON.PlotTyp.DO_MP4  AND IType NE !KON.PlotTyp.DO_MGIF AND $
          IType NE !KON.PlotTyp.DO_GTIF AND IType NE !KON.PlotTyp.DO_KML) THEN BEGIN
         uscore = (UsrOrSys EQ 1) ? '_' : ''
         camname = TypeName + uscore + STRTRIM(!KON.Instr.camera_names[icam],2)
      ENDIF ELSE BEGIN
         camname = TypeName
      ENDELSE

      dotpos = STRLEN(pext) + 1
      filenamex = STRMID(filename, 0, len-dotpos) + camname + '.' + pext

      IF (icam EQ BegCam) THEN BEGIN
         IF (FILE_SEARCH(filenamex)) THEN BEGIN
            IF (UsrOrSys EQ 1) THEN BEGIN
               mssg = [filenamex,'','File exists, overwrite?']
               overwrite = DIALOG_MESSAGE(mssg, /QUESTION, /CANCEL, $
                                          /DEFAULT_NO, /CENTER)
            ENDIF ELSE BEGIN
               overwrite = 'YES'
            ENDELSE

            IF (STRUPCASE(overwrite) EQ 'YES') THEN FILE_DELETE, filenamex
            IF (STRUPCASE(overwrite) EQ 'NO')  THEN GOTO, reenter_file
            IF (STRUPCASE(overwrite) EQ 'CANCEL') THEN BEGIN
               image = 0
               RETURN
            ENDIF
         ENDIF
      ENDIF

      ;---------------------------------------------------------------------
      ; Write the images. Get color or grayscale as appropriate.
      ;---------------------------------------------------------------------

      WIDGET_CONTROL, /HOURGLASS

      IF (IType EQ !KON.PlotTyp.DO_KML) THEN $
         WriteGoogleFile, State, filenamex, image, $
                          LEFT_EDGE, RIGHT_EDGE, BOTTOM_EDGE, TOP_EDGE
      IF (IType EQ !KON.PlotTyp.DO_GTIF) THEN $
         WriteGeoTiffFile, State, filenamex, image, $
                           LEFT_EDGE, RIGHT_EDGE, BOTTOM_EDGE, TOP_EDGE
      IF (IType EQ !KON.PlotTyp.DO_TIF) THEN $
         WRITE_TIFF, filenamex, image
      IF (IType EQ !KON.PlotTyp.DO_PNG) THEN $
         WritePngTransparent, State, filenamex, image
      IF (IType EQ !KON.PlotTyp.DO_JPG) THEN $
         WRITE_JPEG, filenamex, image, QUALITY=95, /ORDER, $
                     TRUE=((CoordStruct.(whichorbit).num_band EQ 1) ? 0 : 1)
      IF (IType EQ !KON.PlotTyp.DO_GIF OR $
          IType EQ !KON.PlotTyp.DO_MGIF) THEN BEGIN
         IF (CoordStruct.(whichorbit).num_band EQ 4) THEN BEGIN
            new_image = REVERSE(REFORM(COLOR_QUAN(image, 1, R, G, B, $
                                   GET_TRANSLATION=trans_table)), 2)
            WRITE_GIF, filenamex, new_image, R, G, B
         ENDIF ELSE BEGIN
            new_image = REVERSE(image, 2)
            WRITE_GIF, filenamex, new_image
         ENDELSE
         new_image = 0
      ENDIF

      ;---------------------------------------------------------------------
      ; Save 9 JPEG images in an array for later conversion to MP4.
      ;---------------------------------------------------------------------

      IF (IType EQ !KON.PlotTyp.DO_MP4) THEN BEGIN

         IF (icam EQ BegCam) THEN BEGIN
            cam_size = SIZE(image)
            NumColor = (cam_size[0] EQ 3) ? 3 : 1
            Xsize = (cam_size[0] EQ 3) ? cam_size[2] : cam_size[1]
            Ysize = (cam_size[0] EQ 3) ? cam_size[3] : cam_size[2]

            IF (NumColor EQ 3) THEN BEGIN
               CamImages = BYTARR(cam_size[1],cam_size[2],cam_size[3],9)
               CamImages[*,*,*,0] = image
            ENDIF ELSE BEGIN
               CamImages = BYTARR(cam_size[1],cam_size[2],9)
               CamImages[*,*,0] = image
            ENDELSE
            
            CONTINUE
         ENDIF
         
         ;----------------------------------------------------------------
         ; All camera images must be the same size.
         ;----------------------------------------------------------------
   
         IF ((SIZE(image))[0] NE cam_size[0] OR $
             (SIZE(image))[1] NE cam_size[1] OR $
             (SIZE(image))[2] NE cam_size[2]) THEN BEGIN
            mssg = ['The size of the JPEG image for camera ', $
                    STRTRIM(!KON.Instr.camera_names[icam],2), $
                    'is not the same as the size of the Df image.', $
                    'Returning.']
            rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            image = 0
            CamImages = 0
            RETURN
         ENDIF
      
         IF (NumColor EQ 3) THEN CamImages[*,*,*,icam-1] = image
         IF (NumColor EQ 1) THEN CamImages[*,*,icam-1] = image
         
      ENDIF
         
      ;---------------------------------------------------------------------
      ; Make sure the file permissions are set for read/write for all.
      ;---------------------------------------------------------------------

      IF (IType NE !KON.PlotTyp.DO_MP4) THEN $
         rtrn_val = ChmodCatchError(filenamex, '666'O)

   ENDFOR

   IF (IType EQ !KON.PlotTyp.DO_MP4) THEN BEGIN

      ;-------------------------------------------------------------------
      ; Set error handler for absence of MP4 license.
      ;-------------------------------------------------------------------

      error_status = 0
      CATCH, error_status
      IF (error_status NE 0) THEN BEGIN
         mssg = ['MP4 license may not be available.', $
                 'No animation was saved. Returning.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         image = 0
         CamImages = 0
         CATCH, /CANCEL
         RETURN
      ENDIF

      ;----------------------------------------------------------------------
      ; Initialize the MP4 image. Frame rate doesn't affect size or quality.
      ; A constant 6 frames/sec is a good rate. Bitrate affects size and
      ; quality. But setting the rate high is OK, because size and quality
      ; don't change at higher rates for a given image size.
      ;----------------------------------------------------------------------

      VideoMP4_Open, filenamex, Xsize, Ysize, !SAV.Digitize.FRAME_PER_SEC, $
                     !SAV.Digitize.BIT_RATE, VidObj, vidStream

      ;----------------------------------------------------------------------
      ; Now write the images to the MP4 file and close it.
      ;----------------------------------------------------------------------

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         IF (NumColor EQ 3) THEN VideoMP4_Put, VidObj, vidStream, $
                                               REVERSE(CamImages[*,*,*,icam],3)
         IF (NumColor EQ 1) THEN VideoMP4_Put, VidObj, vidStream, $
                                               REVERSE(CamImages[*,*,icam],2)
      ENDFOR

      VideoMP4_Close, VidObj

      rtrn_val = ChmodCatchError(filenamex, '666'O)

      CATCH, /CANCEL
      CamImages = 0

   ENDIF

   image = 0
   trans_table = 0

END  ;  Op_WriteImageFile

;***************************************************************************
PRO SavePlumeImages, State, pNextRgn, iBand
;***************************************************************************
; Routine creates any of 3 image files: a digitized region with fire events
; and digitized points shown saved as JPG, a region with height map colors
; saved as JPG, and the same region as an MP4 animation without
; superimposed data.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the min/max values for plotting equal to the min/max for current plume.
;---------------------------------------------------------------------------

;!VAR.DataRgn.VALUE_MIN_DLG = !VAR.DataRgn.VALUE_MIN_DIG
;!VAR.DataRgn.VALUE_MAX_DLG = !VAR.DataRgn.VALUE_MAX_DIG

;---------------------------------------------------------------------------
; Get the name and bounding rectangle for the digitized region and find the
; center of the rectangle.
;---------------------------------------------------------------------------

rgn_name = (*((*pNextRgn).pData)).name[iBand]

LListGetCoordArray, 0, (*pNextRgn).pNextLinePt, Count, SomCoords, $
                    ImageCoords, MisrCoords, LonlatCoords

xval = REFORM(ImageCoords[0,*])
xndxs = WHERE(xval NE !KON.Misc.BADVALUE_REAL, xnumndxs)
yval = REFORM(ImageCoords[1,*])
yndxs = WHERE(yval NE !KON.Misc.BADVALUE_REAL, ynumndxs)
IF (xnumndxs EQ 0 OR ynumndxs EQ 0) THEN RETURN

xmin = MIN(xval[xndxs], MAX=xmax)
ymin = MIN(yval[yndxs], MAX=ymax)

xval = 0
yval = 0
xndxs = 0
yndxs = 0

dx = xmax - xmin + 1
dy = ymax - ymin + 1

centerx = xmin + dx / 2
centery = ymin + dy / 2

;---------------------------------------------------------------------------
; Determine a reasonable bounding box in which to display the region but is
; not "too large". Each dimension must be at least a certain size, and the
; aspect ratio must not exceed 2. If a box overlaps edges of the window,
; adjust its position.
;---------------------------------------------------------------------------

MAX_PLOT_PIX_SIZE = 2048
MIN_PLOT_PIX_SIZE = 284
MAX_ASPECT_RATIO = 2.0
add_pix = 24

dxsize = ((dx + add_pix) > !VAR.AutoWndw.DFLT_PIX_SIZE) < MAX_PLOT_PIX_SIZE
dxsize = dxsize > MIN_PLOT_PIX_SIZE
dysize = ((dy + add_pix) > !VAR.AutoWndw.DFLT_PIX_SIZE) < MAX_PLOT_PIX_SIZE
dysize = dysize > MIN_PLOT_PIX_SIZE

IF ((dxsize * MAX_ASPECT_RATIO) LT dysize) THEN $
   dxsize = FIX(dysize / MAX_ASPECT_RATIO)
IF ((dysize * MAX_ASPECT_RATIO) LT dxsize) THEN $
   dysize = FIX(dxsize / MAX_ASPECT_RATIO)

half_size_x = dxsize / 2
half_size_y = dysize / 2

!VAR.AutoWndw.LEFT_EDGE   = centerx - half_size_x
!VAR.AutoWndw.RIGHT_EDGE  = centerx + half_size_x
!VAR.AutoWndw.BOTTOM_EDGE = centery - half_size_y
!VAR.AutoWndw.TOP_EDGE    = centery + half_size_y
IF (!SAV.Digitize.DOCS_ON_MAPS) THEN !VAR.AutoWndw.TOP_EDGE += 45

AdjustImageBounds, State

!VAR.AutoWndw.CANCEL = 0

;---------------------------------------------------------------------------
; Save the region image on An with fire events and digitized points.
;---------------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_PLUME_POINTS) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Only save the plume points image if this is not the 2nd band of a
   ; double-band retrieval. The images are redundant.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.TWO_RETRIEVALS EQ 0) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Set the displayed camera to An, then turn on fire events and
      ; digitized pts. Save these states and restore later. For features
      ; that are not reset here, the state the user selects determines if
      ; they are saved to file.
      ;---------------------------------------------------------------------

      save_cam         = State.curframe
      save_showobjects = State.showobjects
      save_showmapclr  = State.showmapclr
      save_showfire    = State.showfire

      State.showobjects = 1
      State.showmapclr  = 0
      State.showfire    = 1

      Op_WriteImageFile, State, 5, 5, !KON.PlotTyp.DO_JPG, 2, $
                         'Points_', pNextRgn

      State.curframe    = save_cam
      State.showobjects = save_showobjects
      State.showmapclr  = save_showmapclr
      State.showfire    = save_showfire

      SafeWSET, (*State.pwinHdl)[save_cam], didit

   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Save the region image on An with height as colored pixels or contours.
;---------------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_PLUME_CONTOUR) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Set the displayed camera to An, then turn on fire events and height
   ; points. Save these states and restore them later. For features
   ; that are not reset here, the state the user selects determines if
   ; they are saved to file.
   ;------------------------------------------------------------------------

   save_cam         = State.curframe
   save_showobjects = State.showobjects
   save_showmapclr  = State.showmapclr
   save_showfire    = State.showfire

   State.showobjects = 0
   State.showmapclr  = 1
   State.showfire    = 0

   ; add some room on the right edge for the color bar

   rightedge = !VAR.AutoWndw.RIGHT_EDGE
   !VAR.AutoWndw.RIGHT_EDGE += 60
   IF (!VAR.AutoWndw.RIGHT_EDGE GE State.sizex) THEN $
      !VAR.AutoWndw.RIGHT_EDGE = State.sizex - 1

   Op_WriteImageFile, State, 5, 5, !KON.PlotTyp.DO_JPG, 2, $
                      'Contours_', pNextRgn

   !VAR.AutoWndw.RIGHT_EDGE = rightedge

   State.curframe    = save_cam
   State.showobjects = save_showobjects
   State.showmapclr  = save_showmapclr
   State.showfire    = save_showfire

   SafeWSET, (*State.pwinHdl)[save_cam], didit

ENDIF

;---------------------------------------------------------------------------
; Save the animation with fire events and digitized points off.
;---------------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_PLUME_ANIM) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Only save the animation image if this is not the 2nd band of a
   ; double-band retrieval. The images are redundant.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.TWO_RETRIEVALS EQ 0) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Turn off fire events and digitized points.  Save these states and
      ; restore them later. If MP4 requested, test if user has an IDL
      ; license for MP4.
      ;---------------------------------------------------------------------

      save_cam          = State.curframe
      save_showobjects  = State.showobjects
      save_showmapclr   = State.showmapclr
      save_showfire     = State.showfire

      State.showobjects = 0
      State.showmapclr  = 0
      State.showfire    = 0

      ;---------------------------------------------------------------------
      ; Test whether the user has a license for the MP4 functionality. If
      ; not, then generate 9 JPEGs instead.
      ;---------------------------------------------------------------------

      IF (!SAV.Digitize.JPEG_OR_MP4 EQ 0) THEN $
         Op_WriteImageFile, state, 1, 9, !KON.PlotTyp.DO_JPG, 2, $
                            'Animation', pNextRgn
      IF (!SAV.Digitize.JPEG_OR_MP4 EQ 1) THEN $
         Op_WriteImageFile, State, 1, 9, !KON.PlotTyp.DO_MP4, 2, $
                            'Animation', pNextRgn

      State.curframe    = save_cam
      State.showobjects = save_showobjects
      State.showmapclr  = save_showmapclr
      State.showfire    = save_showfire

      SafeWSET, (*State.pwinHdl)[save_cam], didit

   ENDIF
ENDIF

END  ;  SavePlumeImages

;***************************************************************************
PRO WriteChartFile, RgnName, ChartName
;***************************************************************************
; Routine to save specified data chart images in PNG files. This presumes
; that the active window contains the chart to save.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON save_main_parms, MainOption

whichorbit = 0

;---------------------------------------------------------------------------
; Make a copy of the chart window image and get the filename to write to.
;---------------------------------------------------------------------------

image = TVRD(/ORDER, TRUE=1)

CreateOrbitDirFile, !SAV.Digitize.PlumeOutDir, RgnName + '_' + ChartName, $
                    '', 0, 'png', CoordStruct.(whichorbit).OrbitNum, 1, $
                    tempdir, tempfile, Retval

filename = tempdir + tempfile

;---------------------------------------------------------------------------
; Write the image.
;---------------------------------------------------------------------------

IF ((FILE_SEARCH(filename))[0]) THEN FILE_DELETE, filename

WRITE_PNG, filename, image, /ORDER

;---------------------------------------------------------------------------
; Make sure the file permissions are set for group write.
;---------------------------------------------------------------------------

rtrn_val = ChmodCatchError(filename, '666'O)

END  ;  WriteChartFile

;***************************************************************************
PRO WriteDataDialog_eh, event
;***************************************************************************
; Event handler for user interface for setting configurable
; parameter values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;------------------------------------------------------------------------
; Branch to the correct widget.
;------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct /NO_COPY

CASE 1 OF

;   event.id EQ widget_struct.button1a : BEGIN


ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY

END  ;  WriteDataDialog_eh

;***************************************************************************
PRO WriteDataDialog_gui, FieldStructureAry, FileSize, Status
;***************************************************************************
; Function shows a dialog box containing potentially loaded MISR data fields
; for user to select the ones to write to a text file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

FileSize = 0

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

;base0 = WIDGET_BASE(TITLE='Select MISR Data  to Write to ASCII File', /ROW)

;base1 = WIDGET_BASE(base0, /COLUMN, /FRAME, XSIZE=150)
;base2 = WIDGET_BASE(base0, /COLUMN, /FRAME, XSIZE=150)
;base3 = WIDGET_BASE(base0, /COLUMN, /FRAME, XSIZE=150)
;base4 = WIDGET_BASE(base0, /COLUMN, /FRAME, XSIZE=150)

base0 = WIDGET_BASE(TITLE='Select MISR Data  to Write to ASCII File', $
                    /TLB_KILL_REQUEST_EVENTS)

;label1 = WIDGET_LABEL(base1, VALUE='Level 1 BRF (radiance)')
base1  = WIDGET_TAB(base0)
base11 = WIDGET_BASE(base1, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='Level 1 BRF (radiance)')
button101 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Df camera')
button102 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Cf camera')
button103 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Bf camera')
button104 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Af camera')
button105 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='An camera')
button106 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Aa camera')
button107 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Ba camera')
button108 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Ca camera')
button109 = WIDGET_BUTTON(base11, /ALIGN_LEFT, VALUE='Da camera')

;label2 = WIDGET_LABEL(base1, VALUE='AGP Geographic Product')
base2  = WIDGET_TAB(base0)
base12 = WIDGET_BASE(base2, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='AGP Geographic Product')
button201 = WIDGET_BUTTON(base12, /ALIGN_LEFT, VALUE='Terrain height')
button202 = WIDGET_BUTTON(base12, /ALIGN_LEFT, VALUE='Land-water mask')

;label3 = WIDGET_LABEL(base1, VALUE='SVM Classifiers')
base3  = WIDGET_TAB(base0)
base13 = WIDGET_BASE(base3, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='SVM Classifiers')
button301 = WIDGET_BUTTON(base13, /ALIGN_LEFT, VALUE='Smoke mask')
button302 = WIDGET_BUTTON(base13, /ALIGN_LEFT, VALUE='Dust mask')
button303 = WIDGET_BUTTON(base13, /ALIGN_LEFT, VALUE='Cloud mask')
button304 = WIDGET_BUTTON(base13, /ALIGN_LEFT, VALUE='Land mask')

;label4 = WIDGET_LABEL(base2, VALUE='GP_GMP Geometric Parameters')
base4  = WIDGET_TAB(base0)
base21 = WIDGET_BASE(base4, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='GP_GMP Geometric Parameters')
button401 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Solar zenith angle')
button402 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Solar azimuth angle')
button403 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Df camera zenith angle')
button404 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Cf camera zenith angle')
button405 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Bf camera zenith angle')
button406 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Af camera zenith angle')
button407 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='An camera zenith angle')
button408 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Aa camera zenith angle')
button409 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ba camera zenith angle')
button410 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ca camera zenith angle')
button411 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Da camera zenith angle')
button412 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Df camera azimuth angle')
button413 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Cf camera azimuth angle')
button414 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Bf camera azimuth angle')
button415 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Af camera azimuth angle')
button416 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='An camera azimuth angle')
button417 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Aa camera azimuth angle')
button418 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ba camera azimuth angle')
button419 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ca camera azimuth angle')
button420 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Da camera azimuth angle')
button421 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Df camera scatter angle')
button422 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Cf camera scatter angle')
button423 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Bf camera scatter angle')
button424 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Af camera scatter angle')
button425 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='An camera scatter angle')	
button426 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Aa camera scatter angle')
button427 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ba camera scatter angle')
button428 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ca camera scatter angle')
button429 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Da camera scatter angle')
button430 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Df camera glitter angle')
button431 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Cf camera glitter angle')
button432 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Bf camera glitter angle')
button433 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Af camera glitter angle')
button434 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='An camera glitter angle')
button435 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Aa camera glitter angle')
button436 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ba camera glitter angle')
button437 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Ca camera glitter angle')
button438 = WIDGET_BUTTON(base21, /ALIGN_LEFT, VALUE='Da camera glitter angle')

;label5 = WIDGET_LABEL(base3, VALUE='AS_AEROSOL Aerosol Products')
base5  = WIDGET_TAB(base0)
base31 = WIDGET_BASE(base5, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='AS_AEROSOL Aerosol Products')
button501 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau best estimate blue band')
button502 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau best estimate green band')
button503 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau best estimate red band')
button504 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau best estimate NIR band')
button505 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau lowest residual blue band')
button506 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau lowest residual green band')
button507 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau lowest residual red band')
button508 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Tau lowest residual NIR band')
button509 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='SSA blue band')
button510 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='SSA green band')
button511 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='SSA red band')
button512 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='SSA NIR band')
button513 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Angstrom exp best estimate')
button514 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Angstrom exp lowest residual')
button515 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Size fraction small')
button516 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Size fraction medium')
button517 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Size fraction large')
button518 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Shape fraction spherical')
button519 = WIDGET_BUTTON(base31, /ALIGN_LEFT, VALUE='Shape fract nonspherical')

;label6 = WIDGET_LABEL(base3, VALUE='AS_AEROSOL Land Products')
base6  = WIDGET_TAB(base0)
base32 = WIDGET_BASE(base6, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='AS_AEROSOL Land Products')
button601 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: BHR blue band')
button602 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: BHR green band')
button603 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: BHR red band')
button604 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: BHR NIR band')	
button605 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: DHR blue band')
button606 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: DHR green band')
button607 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: DHR red band')
button608 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: DHR NIR band')	
button609 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: NDVI')
button610 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: RPV1 green band')
button611 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: RPV2 green band')
button612 = WIDGET_BUTTON(base32, /ALIGN_LEFT, VALUE='Land: RPV3 green band')

;label7 = WIDGET_LABEL(base4, VALUE='TC_STEREO Heights/Winds')
base7  = WIDGET_TAB(base0)
base41= WIDGET_BASE(base7, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='TC_STEREO Heights/Winds')
button701 = WIDGET_BUTTON(base41, /ALIGN_LEFT, VALUE='Stereo: Zero-wind heights')
button702 = WIDGET_BUTTON(base41, /ALIGN_LEFT, VALUE='Stereo: Wind-corrected heights')
button703 = WIDGET_BUTTON(base41, /ALIGN_LEFT, VALUE='Stereo: Windspeed across')
button704 = WIDGET_BUTTON(base41, /ALIGN_LEFT, VALUE='Stereo: Windspeed along')
button705 = WIDGET_BUTTON(base41, /ALIGN_LEFT, VALUE='Stereo: Cloud mask')

;label8 = WIDGET_LABEL(base4, VALUE='TC_CLOUD Heights/Winds')
base8  = WIDGET_TAB(base0)
base42= WIDGET_BASE(base8, /COLUMN, /FRAME, /NONEXCLUSIVE, TITLE='TC_CLOUD Heights/Winds')
button801 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Zero-wind heights')
button802 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Zero across-track wind')
button803 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Zero cloud mask')
button804 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Corrected-wind heights')
button805 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Corrected cross-track wind')
button806 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Corrected cloud mask')
button807 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Motion heights')
button808 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Across-track wind')
button809 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Along-track wind')
button810 = WIDGET_BUTTON(base42, /ALIGN_LEFT, VALUE='Cloud: Cloud mask')

basex = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(basex, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(basex, VALUE='Cancel')
help_button = WIDGET_BUTTON(basex, VALUE='PDF Help')

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct = { $
       button101 : button101, button102 : button102, button103 : button103, $
       button104 : button104, button105 : button105, button106 : button106, $
       button107 : button107, button108 : button108, button109 : button109, $
       button201 : button201, button202 : button202, button301 : button301, $
       button302 : button302, button303 : button303, button304 : button304, $
       button401 : button401, button402 : button402, button403 : button403, $
       button404 : button404, button405 : button405, button406 : button406, $
       button407 : button407, button408 : button408, button409 : button409, $
       button410 : button410, button411 : button411, button412 : button412, $
       button413 : button413, button414 : button414, button415 : button415, $
       button416 : button416, button417 : button417, button418 : button418, $
       button419 : button419, button420 : button420, button421 : button421, $
       button422 : button422, button423 : button423, button424 : button424, $
       button425 : button425, button426 : button426, button427 : button427, $
       button428 : button428, button429 : button429, button430 : button430, $
       button431 : button431, button432 : button432, button433 : button433, $
       button434 : button434, button435 : button435, button436 : button436, $
       button437 : button437, button438 : button438, button501 : button501, $
       button502 : button502, button503 : button503, button504 : button504, $
       button505 : button505, button506 : button506, button507 : button507, $
       button508 : button508, button509 : button509, button510 : button510, $
       button511 : button511, button512 : button512, button513 : button513, $
       button514 : button514, button515 : button515, button516 : button516, $
       button517 : button517, button518 : button518, button519 : button519, $
       button601 : button601, button602 : button602, button603 : button603, $
       button604 : button604, button605 : button605, button606 : button606, $
       button607 : button607, button608 : button608, button609 : button609, $
       button610 : button610, button611 : button611, button612 : button612, $
       button701 : button701, button702 : button702, button703 : button703, $
       button704 : button704, button705 : button705, button801 : button801, $
       button802 : button802, button803 : button803, button804 : button804, $
       button805 : button805, button806 : button806, button807 : button807, $
       button808 : button808, button809 : button809, button810 : button810, $
       button901 : button901, button902 : button902, button903 : button903, $
       button904 : button904, button905 : button905, button906 : button906, $
       button907 : button907, button908 : button908, button909 : button909, $
       button911 : button911, button912 : button912, button913 : button913, $
       button914 : button914, button915 : button915, button916 : button916, $
       ok_button : ok_button, cancel_button : cancel_button, help_button : help_button}

; add button return values 


;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------


;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct, XOFFSET=200, YOFFSET=100, $
                /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'WriteDataDialog_gui', base0, EVENT_HANDLER='WriteDataDialog_eh'

;---------------------------------------------------------------------------
; Compute the output file size in mbytes.
;---------------------------------------------------------------------------

Status = 0

END  ;  WriteDataDialog_gui

;***************************************************************************
PRO Op_WriteTextFile, state, curframe
;***************************************************************************
; Routine saves currently loaded MISR data to a text file. The L1B2 data,
; geographic coordinates, geometry data and terrain heights are always saved.
; Other MISR product fields that are loaded are also saved. All data are
; saved on a one record per 275 m OR 1100 m pixel basis.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File

;mssg = 'Sorry, feature is not implemented.'
;rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
;RETURN

;---------------------------------------------------------------------------
; Show dialog box to let user select the data fields to write and get the
; resolution user wants to write at: either 275 m or 1100 m. Also return
; the size the text file will be.
;---------------------------------------------------------------------------

WriteDataDialog_gui, FieldStructureAry, file_size, status
IF (status EQ -1) THEN RETURN

;---------------------------------------------------------------------------
; Inform user how large the output will be and give an opportunity to redo.
;---------------------------------------------------------------------------

size_str = STRTRIM(STRING(file_size),2)
mssg = ['The text file will contain ' + size_str + ' mbytes of data.', $
        'Do you want to continue and write these data to file?']
rtrn = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)

;---------------------------------------------------------------------------
; Write the data to file.
;---------------------------------------------------------------------------

IF (STRUPCASE(rtrn) EQ 'YES') THEN BEGIN

   ;------------------------------------------------------------------------
   ; Select and open the output file.
   ;------------------------------------------------------------------------

   deflt_file = !SAV.WorkingDir + 'MISR_data.txt'

   GetLastFilename, 0, !KON.FileTyp.TypeMISRsave, ['*.txt'], 1, $
                    file_outpath, filename

   OPENW, Unit, filename, /GET_LUN

   ;------------------------------------------------------------------------
   ; Write header including data field names and format string.
   ;------------------------------------------------------------------------

   

   ;------------------------------------------------------------------------
   ; Write data, one 275 m or 1100 m pixel per record.
   ;------------------------------------------------------------------------

   

   ;------------------------------------------------------------------------
   ; Close file and clean up.
   ;------------------------------------------------------------------------

   FREE_LUN, Unit

;---------------------------------------------------------------------------
; Let user try again.
;---------------------------------------------------------------------------

ENDIF ELSE BEGIN


ENDELSE

END  ;  Op_WriteTextFile

;***************************************************************************
PRO Op_SaveSession, State, Retval
;***************************************************************************
; Routine saves a MINX session to a .sav file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance
COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid
COMMON STER_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

COMMON chan_minmax, ChanMinMax
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles
COMMON data_structs, region_data, linept_data
COMMON coord_data, CoordStruct
COMMON struct_marker, marker_option
COMMON orblist_parm, orblist_index
COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist
COMMON xtra_plot_options, SHOW_HTS_NEAR_GROUND
COMMON misrvision, MISRVis_rgborb, MISRVis_rgbcam, MISRVis_rgbbnd
COMMON struct_marker, marker_option
COMMON restore_params, StateRestore

ok_to_write_file = 0

;------------------------------------------------------------------------
; Make sure no digitizing option is active.
;------------------------------------------------------------------------

Op_DisableEditObjects, State, retval

;------------------------------------------------------------------------
; Ask for .sav filename to write to.
;------------------------------------------------------------------------

filename = DIALOG_PICKFILE(FILE='MINX_session.sav', $
                           TITLE='Specify .sav Filename', $
                           PATH=!SAV.WorkingDir, $
                           DEFAULT_EXTENSION='sav', $
                           FILTER=['*.sav'], $
                           /OVERWRITE_PROMPT, /FIX_FILTER)

;------------------------------------------------------------------------
; Force the extension to be .sav if not already.
;------------------------------------------------------------------------

IF (filename NE '') THEN BEGIN
   split = STRSPLIT(filename, '.', /EXTRACT)
   extension = split[N_ELEMENTS(split)-1]
   IF (extension NE 'sav') THEN BEGIN
      mssg = 'Filename extension will be changed to .sav'
      wrong = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      filename = split[0] + '.sav'
   ENDIF

   ok_to_write_file = 1

   ;---------------------------------------------------------------------
   ; If file exists, ask if OK to overwrite it.
   ;---------------------------------------------------------------------

   IF ((FILE_SEARCH(filename))[0] NE '') THEN BEGIN
      ok_to_write_file = 0
      mssg = [filename,'', 'File exists, overwrite?']
      overwrite = DIALOG_MESSAGE(mssg, /QUESTION, /DEFAULT_NO, /CENTER)
      IF (STRUPCASE(overwrite) EQ 'YES') THEN ok_to_write_file = 1
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Write .sav file if approved.
;------------------------------------------------------------------------

IF (ok_to_write_file EQ 1) THEN BEGIN

   npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH) + 1
   tempdir = STRMID(filename, 0, npos)
   tempfile = STRMID(filename, npos)

   IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(tempdir)
      rtrn_val = ChmodCatchError(tempdir, '777'O)
   ENDIF

   MINX_signature = 'MINX save/restore file.'
   StateRestore = State
   SAVE, /ALL, FILENAME=filename, /COMPRESS
ENDIF

END  ;  Op_SaveSession

;***************************************************************************
PRO Op_RestoreSession, State, Retval, CrdStrct, RestoreFile
;***************************************************************************
; Routine restores a MINX session from a previously saved .sav file.
; If 2 parameters are passed, the caller is in MINX_animate and we're
;    restoring a file from an existing session, so replace session.
; If 4 parameters are passed, the caller is in MINX_dialog_boxes and we're
;    restoring a file in a new session.
; See NEW_OR_REPLACE below.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance
COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

COMMON chan_minmax, ChanMinMax
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles
COMMON data_structs, region_data, linept_data
COMMON coord_data, CoordStruct
COMMON struct_marker, marker_option
COMMON orblist_parm, orblist_index
COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist
COMMON xtra_plot_options, SHOW_HTS_NEAR_GROUND
COMMON misrvision, MISRVis_rgborb, MISRVis_rgbcam, MISRVis_rgbbnd
COMMON struct_marker, marker_option
COMMON restore_params, StateRestore

Retval = 0

;------------------------------------------------------------------------
; Determine if this is called from MINX_animate or MINX_dialog_boxes.
;------------------------------------------------------------------------

ORBIT_REPLACE  = 1
ORBIT_NEW      = 2
NEW_OR_REPLACE = (N_PARAMS() LE 2) ? ORBIT_REPLACE : ORBIT_NEW

IF (NEW_OR_REPLACE EQ ORBIT_REPLACE) THEN BEGIN
   GetLastFilename, 0, !KON.FileTyp.TypeSaveSession, ['*.sav'], 0, $
                    file_outpath, filename
ENDIF ELSE BEGIN
   filename = RestoreFile
ENDELSE

;------------------------------------------------------------------------
; Don't proceed unless there is a .sav file to restore.
;------------------------------------------------------------------------

IF (filename NE '') THEN BEGIN
   split = STRSPLIT(filename, '.', /EXTRACT)
   extension = split[N_ELEMENTS(split)-1]

   IF (extension EQ 'sav') THEN BEGIN
      MINX_signature = ''

      ;------------------------------------------------------------------
      ; Clean up memory and reinitialize. TLB is the base widget.
      ;------------------------------------------------------------------

      IF (NEW_OR_REPLACE EQ ORBIT_REPLACE) THEN BEGIN
         CleanUpMemory, Retval
         IF (InitMINXParams() NE 0) THEN RETURN
         tlb_temp = TLB
      ENDIF

      ;------------------------------------------------------------------
      ; Restore the old session.
      ;------------------------------------------------------------------

      RESTORE, filename, /RELAXED_STRUCTURE_ASSIGNMENT  ; <==========
      StateRestore = State

      GetPlatformParams, IDL_version, MINX_version, MINX_platform, $
                         fslash, newline, sys_home_dir, user_name, $
                         MINX_dir, xy_dims, MINX_graphics

      !KON.Misc.IDL_VERSION_NUM  = IDL_version
      !KON.Misc.MINX_VERSION_NUM = MINX_version
      !KON.Misc.MINX_PLATFORM    = MINX_platform
      !KON.Misc.Slash            = fslash
      !KON.Misc.NewLine          = newline
      !KON.Misc.SystemHomeDir    = sys_home_dir
      !KON.Misc.UserName         = user_name
      !KON.Misc.MINX_DIRECTORY   = MINX_dir
      !KON.Misc.MINX_GPU         = MINX_graphics
         
      IF (NEW_OR_REPLACE EQ ORBIT_REPLACE) THEN BEGIN
         TLB = tlb_temp
         tlb_temp = 0
      ENDIF ELSE BEGIN
         CrdStrct = CoordStruct
      ENDELSE

      ;------------------------------------------------------------------
      ; Set platform independent information in case it's changed.
      ;------------------------------------------------------------------

      IF (!KON.Misc.MINX_PLATFORM EQ 2) THEN BEGIN
         !SAV.WorkingDir = GETENV('USERPROFILE')
         IF (!SAV.WorkingDir EQ '') THEN $
             !SAV.WorkingDir = 'C:'
         !KON.Misc.UserName = ''
         npos = STRPOS(!SAV.WorkingDir, !KON.Misc.Slash, $
                       /REVERSE_SEARCH)
         IF (npos GT 0) THEN $
            !KON.Misc.UserName = STRMID(!SAV.WorkingDir, npos+1)
         !SAV.WorkingDir += !KON.Misc.Slash
      ENDIF ELSE BEGIN
         !KON.Misc.UserName = GETENV('USER')
         !SAV.WorkingDir = GETENV('HOME') + !KON.Misc.Slash
      ENDELSE

      ;------------------------------------------------------------------
      ; Confirm that the MINX signature was written when file was saved.
      ;------------------------------------------------------------------

      IF (MINX_signature EQ 'MINX save/restore file.') THEN BEGIN

         ;---------------------------------------------------------------
         ; If we're restoring a file from an existing session, do this.
         ;---------------------------------------------------------------

         IF (SIZE(WorkImage, /TYPE) GT 0) AND $
            (SIZE(CoordStruct, /TYPE) GT 0) AND $
            (N_ELEMENTS(!VAR.RawImages) GE 9) THEN BEGIN

            ; Restore session flag tells FullAnI what's up.
            MISR_or_AirMISR = -1

            IF (NEW_OR_REPLACE EQ ORBIT_REPLACE) THEN BEGIN
               whichorbit = (State.nframes GT 10)
               ndxs = WHERE(PTR_VALID(!VAR.RawImages), numndxs)
               num_cams = numndxs / CoordStruct.(whichorbit).num_band
               CamFiles = STRARR(num_cams)
               FullAnI, num_cams, CamFiles, CoordStruct, $
                        MISR_or_AirMISR, wVeryTopBase, Retval
            ENDIF
         ENDIF

      ENDIF ELSE BEGIN

         mssg = ['Not a valid MINX save/restore file.']
         sav_file_not_valid = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         Retval = 1

      ENDELSE

   ENDIF ELSE BEGIN
      mssg = ['Filename does not have a .sav extension.', $
              'Filename must have a .sav extension.']
      wrong = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      correct_filename_extension = 0
   ENDELSE

ENDIF

END  ;  Op_RestoreSession

;***************************************************************************
PRO PrintHistData, Unit, BaseName, Units, BinSize, NumBins, CntrData, $
                   HistData
;***************************************************************************
; Write histogram to a file. Only write data for bins that have non-zero
; contents.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Write the bin size.
;---------------------------------------------------------------------------

PRINTF, Unit, FORMAT='(A,F8.3)', Basename + ' bin size ' + $
        Units + ' : ', BinSize

;---------------------------------------------------------------------------
; Loop over the bins and write the bin center value and number of hits in
; each bin only for those bins that have non-zero hits.
;---------------------------------------------------------------------------

nbinmax = N_ELEMENTS(*HistData)

binmid  = FLTARR(nbinmax)
binhits = INTARR(nbinmax)
nbin = 0

PRINTF, Unit, '   center   counts'

FOR ibin=0,nbinmax-1 DO BEGIN
   PRINTF, Unit, FORMAT='(A,F9.3, I6)', ' ', $
           (*CntrData)[ibin], (*HistData)[ibin]
ENDFOR

binmid = 0
binhits = 0

END  ;  PrintHistData

;***************************************************************************
PRO PrintHistograms, Unit, pRgnData
;***************************************************************************
; Write histograms to file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   basename = 'Terrain ht histogram'
   IF (PTR_VALID(pRgnData.terrht_hist)) THEN $
      PrintHistData, Unit, basename, '(m)', pRgnData.tht_binsize, $
                     pRgnData.tht_numbins, pRgnData.terrht_cntr, $
                     pRgnData.terrht_hist

   basename = 'Zero-wind ht histogram'
   IF (PTR_VALID(pRgnData.zeroht_hist)) THEN $
      PrintHistData, Unit, basename, '(m)', pRgnData.zht_binsize, $
                     pRgnData.zht_numbins, pRgnData.zeroht_cntr, $
                     pRgnData.zeroht_hist

   basename = 'Wind-corrected ht histogram'
   IF (PTR_VALID(pRgnData.corrht_hist)) THEN $
      PrintHistData, Unit, basename, '(m)', pRgnData.cht_binsize, $
                     pRgnData.cht_numbins, pRgnData.corrht_cntr, $
                     pRgnData.corrht_hist

   basename = 'Cross-track wind histogram'
   IF (PTR_VALID(pRgnData.windx_hist)) THEN $
      PrintHistData, Unit, basename, '(m/s)', pRgnData.wndx_binsize, $
                     pRgnData.wndx_numbins, pRgnData.windx_cntr, $
                     pRgnData.windx_hist

   basename = 'Along-track wind histogram'
   IF (PTR_VALID(pRgnData.windy_hist)) THEN $
      PrintHistData, Unit, basename, '(m/s)', pRgnData.wndy_binsize, $
                     pRgnData.wndy_numbins, pRgnData.windy_cntr, $
                     pRgnData.windy_hist

   basename = 'Optical depth histogram - blue'
   IF (PTR_VALID(pRgnData.tau0_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tau0_binsize, $
                     pRgnData.tau0_numbins, pRgnData.tau0_cntr, $
                     pRgnData.tau0_hist

   basename = 'Optical depth histogram - green'
   IF (PTR_VALID(pRgnData.tau1_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tau1_binsize, $
                     pRgnData.tau1_numbins, pRgnData.tau1_cntr, $
                     pRgnData.tau1_hist

   basename = 'Optical depth histogram - red'
   IF (PTR_VALID(pRgnData.tau2_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tau2_binsize, $
                     pRgnData.tau2_numbins, pRgnData.tau2_cntr, $
                     pRgnData.tau2_hist

   basename = 'Optical depth histogram - nir'
   IF (PTR_VALID(pRgnData.tau3_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tau3_binsize, $
                     pRgnData.tau3_numbins, pRgnData.tau3_cntr, $
                     pRgnData.tau3_hist

   basename = 'Angstrom exponent histogram'
   IF (PTR_VALID(pRgnData.angexp_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.ang_binsize, $
                     pRgnData.ang_numbins, pRgnData.angexp_cntr, $
                     pRgnData.angexp_hist

   basename = 'Tau Fraction histogram - small'
   IF (PTR_VALID(pRgnData.tfr0_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tfr0_binsize, $
                     pRgnData.tfr0_numbins, pRgnData.tfr0_cntr, $
                     pRgnData.tfr0_hist

   basename = 'Tau Fraction histogram - medium'
   IF (PTR_VALID(pRgnData.tfr1_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tfr1_binsize, $
                     pRgnData.tfr1_numbins, pRgnData.tfr1_cntr, $
                     pRgnData.tfr1_hist

   basename = 'Tau Fraction histogram - large'
   IF (PTR_VALID(pRgnData.tfr2_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tfr2_binsize, $
                     pRgnData.tfr2_numbins, pRgnData.tfr2_cntr, $
                     pRgnData.tfr2_hist

   basename = 'Tau Fraction histogram - spherical'
   IF (PTR_VALID(pRgnData.tfr3_hist)) THEN $
      PrintHistData, Unit, basename, '', pRgnData.tfr3_binsize, $
                     pRgnData.tfr3_numbins, pRgnData.tfr3_cntr, $
                     pRgnData.tfr3_hist

   PRINTF, Unit, ''

END  ;  PrintHistograms

;***************************************************************************
PRO ComputePlumeQuality, NumHtPnts, FracHtArea, RMS_dev, MaxHt, QualFlag
;***************************************************************************
; Compute a quality factor for this region. The parameters were developed
; entirely by "tuning" - repeatedly running many cases with different
; combinations of the parameters.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Define thresholds (G = Good, F = Fair, P = Poor).
;---------------------------------------------------------------------------

MIN_MAX_HT = 0.150   ; max plume/cloud height less than this is bad

NUM_PNTS_MIN =  5.00 ; fewer points than this immediately returns Poor
NUM_PNTS_MAX = 40.00 ; more points than this cannot improve the rating

FRAC_AREA_MIN = 0.07 ; area-filled fraction less than this returns Poor
FRAC_AREA_MAX = 0.40 ; a larger fraction than this cannot improve rating

FRAC_RMS_MIN = 0.20  ; greater RMS variation than 1/this returns Poor
FRAC_RMS_MAX = 2.00  ; a larger value than this cannot improve rating

QA_GOOD_GF = 1.10    ; min value of composite score to qualify as GOOD.
QA_POOR_FP = 0.15    ; max value of composite score to qualify as POOR.

;---------------------------------------------------------------------------
; If any minimum criterion is not met, return 'POOR'. Any max height less
; than 150 m above terrain is bad!
;---------------------------------------------------------------------------

QualFlag = 'Poor'

IF (MaxHt LT MIN_MAX_HT OR NumHtPnts LT NUM_PNTS_MIN OR $
   FracHtArea LT FRAC_AREA_MIN OR RMS_dev LE 0.0 OR $
   (RMS_dev GT 1.0 / FRAC_RMS_MIN)) THEN RETURN

;---------------------------------------------------------------------------
; Calculate the quality flag and return it. The parameters used to compute
; the components of the composite_score were empirically determined by
; extensive tuning tests.
;---------------------------------------------------------------------------

num_pts_coef = (ALOG10(NumHtPnts - NUM_PNTS_MIN + 2) / ALOG10(NUM_PNTS_MAX)) < 1.0
frac_ok_coef = ALOG10(FracHtArea * 18) > 0.1
rms_dev_coef = (FRAC_RMS_MIN / RMS_dev * ALOG10(MaxHt * 8)) < FRAC_RMS_MAX

composite_score = num_pts_coef * frac_ok_coef * rms_dev_coef

QualFlag = (composite_score GE QA_GOOD_GF) ? 'Good' : $
           (composite_score LE QA_POOR_FP) ? 'Poor' : 'Fair'

END  ;  ComputePlumeQuality

;***************************************************************************
PRO Op_WriteRetrievedData, State, CurFrame, pRgn, iBand, Retval
;***************************************************************************
; Write to disk the data for the current region.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON data_structs, region_data, linept_data
COMMON coord_data, CoordStruct
COMMON save_main_parms, MainOption

whichorbit = 0

;---------------------------------------------------------------------------
; Set configurable parameters and initialize data.
;---------------------------------------------------------------------------

accum_raw_hts  = [0.0]
area_per_pt    = -99.
num_poly_pnts  =  0L
num_dir_pnts   =  0L
num_tot_pnts   =  0L
num_ht_pnts    =  0L

stddev_raw_hts = -99
pcnt_area      = -99
diff_SOM_along = 0.0
tot_power      = 0.0
biome_type     = 255

;---------------------------------------------------------------------------
; Set up the header for the point table.
;---------------------------------------------------------------------------

poly_str1 = 'POLYGON: '
poly_str2 = ' points in this table define the digitized bounding polygon if present.'
dir_str1  = 'DIRECTION: '
dir_str2  = ' points in this table define the digitized direction line if present.'
pd_str1 = '          Long-    Lat-'
pd_str2 = '   Pt#    itude    itude   Blk Samp Line'
pd_str3 = ' -----  --------  -------  --- ---- ----'
fmt_pd_str = '(I6,F10.3,F9.3,3I5)'

data_str01 = 'RESULTS: '
data_str02 = ' points in this table are samples where NoWnd heights or fire power were retrieved.'

prddev_str1 = '           Long-    Lat-                   Km to  Dg Cw  Terr    Feature Ht (m)      Windspeed (m/s)  '
prddev_str2 = '    Pt#    itude    itude   Blk Samp Line   Pt 1  Rel N  Elev  NoWnd  W/Wnd  Fltrd  Across Along Total'
prddev_str3 = ' ------  --------  -------  --- ---- ----  -----  -----  ----  -------------------  ------------------'
prddev_fmt  = '(I7, F10.3, F9.3, 3I5, F7.1, I7, I6, A1, I6, 2I7, F7.1, 2F6.1'

data_str1 = $
   [prddev_str1, $ 
    '     Optical Depth by Band      Single-Scattering Albedo     Tau Fraction by Particle Type   Ang ', '  Power']
data_str2 = $
   [prddev_str2, $
    '   Blue   Green   Red    NIR    Blue   Green   Red    NIR    Small  Medium   Large   Spher   Exp ', '  MWatt']
data_str3 = $
   [prddev_str3, $
    '  ---------------------------  ---------------------------  ------------------------------  -----', '  -----']

fmt_data_str = [prddev_fmt, ', 2X, 4F7.3, 1X, 4F7.3, 4F8.3, F7.3', ', I7']

format_string = fmt_data_str[0]
IF (!SAV.Digitize.DRAW_AEROSOL) THEN format_string += fmt_data_str[1]
IF (!VAR.CurrFiles.Fire_Loaded) THEN format_string += fmt_data_str[2]
format_string += ')'

;---------------------------------------------------------------------------
; Check that the region is valid, and get the region name.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(pRgn) OR $
    (*pRgn).ObjType[0] EQ !KON.AerObjTyp.FIREPIXEL_OBJ) THEN RETURN

pRgnData = (*((*pRgn).pData))

name_str = pRgnData.name[iBand]

;---------------------------------------------------------------------------
; Get a few parameters from the region and head point structures.
;---------------------------------------------------------------------------

pNextPt = (*pRgn).pNextLinePt
IF (PTR_VALID(pNextPt)) THEN BEGIN
   block    = (*((*pNextPt).pData)).block
   utc_time = pRgnData.utc_time
   beg_lon  = pRgnData.biome_lon
   beg_lat  = pRgnData.biome_lat
   beg_elev = pRgnData.biome_elev
ENDIF

;---------------------------------------------------------------------------
; Get a file name to write to and open the file for writing.
;---------------------------------------------------------------------------

CreateOrbitDirFile, !SAV.Digitize.PlumeOutDir, 'Plumes_' + name_str, '', 0, $
                    'txt', CoordStruct.(whichorbit).OrbitNum, 1, tempdir, $
                    new_file, Retval

tempfile = tempdir + new_file

OPENW, unit, tempfile, /GET_LUN

;---------------------------------------------------------------------------
; Write the orbit header data.
;---------------------------------------------------------------------------

CALDAT, SYSTIME(/JULIAN), month, day, year
year  = STRTRIM(string(year), 2)
month = STRTRIM(string(month),2)
month = STRLEN(month) EQ 2 ? month : '0' + month
day   = STRTRIM(string(day), 2)
day   = STRLEN(day) EQ 2 ? day : '0' + day
now_date = year + '-' + month + '-' + day
           
PRINTF, unit, 'Orbit number   : '  + STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
PRINTF, unit, 'Path number    : '  + STRTRIM(STRING(CoordStruct.(whichorbit).PathNum),2)
PRINTF, unit, 'Block number   : '  + STRTRIM(STRING(block),2)
PRINTF, unit, 'Date acquired  : '  + CoordStruct.(whichorbit).OrbitDate
PRINTF, unit, 'UTC time       : '  + utc_time
PRINTF, unit, 'MINX version   : V' + !KON.Misc.MINX_VERSION_NUM
PRINTF, unit, 'User name      : '  + !KON.Misc.UserName
PRINTF, unit, 'Date digitized : '  + now_date
PRINTF, unit, ''

;---------------------------------------------------------------------------
; Write out the plume header data for this region.
;---------------------------------------------------------------------------

IF ((*pRgn).ObjType[2] GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN
   samp_spac = !SAV.Digitize.SAMP_SPAC_DIR
   is_plume = 1
ENDIF ELSE BEGIN
   samp_spac = !SAV.Digitize.SAMP_SPAC_NODIR
   is_plume = 0
ENDELSE
area_per_pt  = samp_spac * samp_spac

matcher   = !SAV.Digitize.MATCHER_SM_LG EQ 1 ? 'Small'  : $
             (!SAV.Digitize.MATCHER_SM_LG EQ 2 ? 'Medium' : $
              (!SAV.Digitize.MATCHER_SM_LG EQ 3 ? 'Large'  : $
               (!SAV.Digitize.MATCHER_SM_LG EQ 4 ? 'XLarge' : 'NA')))

cam_pairs = !SAV.Digitize.USE_CAM_PAIRS EQ 1 ? 'A' : $
             (!SAV.Digitize.USE_CAM_PAIRS EQ 2 ? 'A B' : $
              (!SAV.Digitize.USE_CAM_PAIRS EQ 3 ? 'A B C' : $
               (!SAV.Digitize.USE_CAM_PAIRS EQ 4 ? 'A B C D' : $
                (!SAV.Digitize.USE_CAM_PAIRS EQ 5 ? 'C D' : 'NA'))))

PRINTF, unit, 'Region name            : ', pRgnData.name[iBand]
PRINTF, unit, 'Region aerosol type    : ', $
              !KON.AerObjTyp.Name[(*pRgn).ObjType[0]]
PRINTF, unit, 'Region geometry type   : ', $
              !KON.GeomObjTyp.Name[(*pRgn).ObjType[1]]
PRINTF, unit, 'Region wind dir type   : ', $
              !KON.WindObjTyp.Name[(*pRgn).ObjType[2]]
band_used = ''
IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.BOTH_BAND) THEN $
   band_used = (iBand EQ 0) ? ' - Red' : ' - Blue'
PRINTF, unit, 'Retrieved with band    : ', $
              !KON.BandObjTyp.Name[(*pRgn).ObjType[3]] + band_used
PRINTF, unit, 'Retrieved with matcher : ', matcher
PRINTF, unit, 'Retrieved with cameras : ', cam_pairs
retr_precis = !SAV.Digitize.RELAX_THRESH EQ 0 ? 'Normal' : 'Relaxed'
PRINTF, unit, 'Retrieval precision    : ', retr_precis
PRINTF, unit, FORMAT='(A,F6.3)', 'Min ht > terrain (km)  : ', $
               !SAV.Digitize.MIN_HGHT
PRINTF, unit, FORMAT='(A,F6.3)', 'Max ht > sealevel (km) : ', $
               !SAV.Digitize.MAX_HGHT
PRINTF, unit, FORMAT='(A,F6.3)', 'Sample spacing (km)    : ', samp_spac
PRINTF, unit, 'Registration corrected : ', $
              !SAV.HtWind.REGISTRATION_CORR EQ 1 ? 'Yes' : 'No'
PRINTF, unit, 'Image color equalized  : ', $
              !VAR.ClrScaleVals.ScalingMethod EQ 0 ? 'Yes' : 'No'
PRINTF, unit, ''

pl_area = ((*pRgn).ObjType[1] GE !KON.GeomObjTyp.GEOM_POLYGON_OBJ) ? $
             STRING(FORMAT='(I5)', ROUND(pRgnData.area)) : 'NA   '
biome_name = GetBiomeClassName(pRgnData.biome_type) 
biome_info = (!VAR.CurrFiles.BiomeFile EQ '') ? 'NA  ' : $
             (biome_name + ', ' + STRTRIM(STRING(pRgnData.biome_type),2))
fire_power = '  -99'  ;  updated at end of routine

PRINTF, unit, FORMAT='(A,F9.3)', 'First point longitude  : ', beg_lon
PRINTF, unit, FORMAT='(A,F9.3)', 'First point latitude   : ', beg_lat
PRINTF, unit, FORMAT='(A,1X,A)', 'Geographic region      : ', pRgnData.geo_region
PRINTF, unit, FORMAT='(A,1X,A)', 'Biome IGBP name, class : ', biome_info
PRINTF, unit, FORMAT='(A,1X,A)', 'Red/blue band better?  : ', pRgnData.red_blu_pref
PRINTF, unit, FORMAT='(A,I5)',   'Perimeter length (km)  : ', ROUND(pRgnData.perimeter)
PRINTF, unit, FORMAT='(A,A5)',   'Area (sq km)           : ', pl_area
PRINTF, unit, FORMAT='(A,F9.3)', 'Area per point (sq km) : ', area_per_pt

POINT_LUN, -unit, wind_counts
PRINTF, unit, FORMAT='(A,I5)', 'Num heights retrieved  : ', num_ht_pnts
PRINTF, unit, FORMAT='(A,I5)', 'Percent area covered   : ', pcnt_area
PRINTF, unit, FORMAT='(A,I5)', 'Fire elev. (m > MSL)   : ', ROUND(beg_elev * 1000.0)

best_med_ht = (pRgnData.bestht_med[iBand] EQ !KON.Misc.BADVALUE_REAL) ? $
               -9999 : ROUND((pRgnData.bestht_med[iBand] - beg_elev) * 1000.0)
PRINTF, unit, FORMAT='(A,I5)', 'Median ht (m > fire)   : ', best_med_ht
best_top_ht = (pRgnData.bestht_top[iBand] EQ !KON.Misc.BADVALUE_REAL) ? $
               -9999 : ROUND((pRgnData.bestht_top[iBand] - beg_elev) * 1000.0)
PRINTF, unit, FORMAT='(A,I5)', 'Max ht (m > fire)      : ', best_top_ht

POINT_LUN, -unit, stddev_pos
PRINTF, unit, FORMAT='(A,I5)', 'Ht std. deviation (m)  : ', stddev_raw_hts
rms_ht_dev = (pRgnData.rms_ht_dev[iBand] EQ !KON.Misc.BADVALUE_REAL) ? $
               -9999 : ROUND(pRgnData.rms_ht_dev[iBand] * 1000.0)
PRINTF, unit, FORMAT='(A,I5)', 'Ht local variation (m) : ', rms_ht_dev
PRINTF, unit, FORMAT='(A,I5)', '|WndDir-AlongDir|(deg) : ', diff_SOM_along
PRINTF, unit, FORMAT='(A,A5)', 'Total fire power (MW)  : ', fire_power

PRINTF, unit, 'Retrieval quality est. :  ', pRgnData.auto_quality[iBand]
PRINTF, unit, 'Plume has pyro-cumulus :  ', pRgnData.user_pyrocum ? 'Yes' : 'No'
PRINTF, unit, 'Comments by digitizer  :  ', pRgnData.user_comment
PRINTF, unit, ''

;---------------------------------------------------------------------------
; Write out the file names of the input data.
;---------------------------------------------------------------------------

ipos = STRPOS(!VAR.CurrFiles.CamFiles[4], !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.CamFiles[4], ipos+1)
PRINTF, unit, FORMAT='(2A)', 'Level 1 radiance file  : ', filename
ipos = STRPOS(!VAR.CurrFiles.AGPfile, !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.AGPfile, ipos+1)
PRINTF, unit, FORMAT='(2A)', 'Terrain elevation file : ', filename
ipos = STRPOS(!VAR.CurrFiles.GMPfile, !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.GMPfile, ipos+1)
PRINTF, unit, FORMAT='(2A)', 'Cam/Sun Geometry file  : ', filename
ipos = STRPOS(!VAR.CurrFiles.SVMfile, !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.SVMfile, ipos+1)
PRINTF, unit, FORMAT='(2A)', 'SVM Classifiers file   : ', filename EQ '' ? $
        'Not Loaded' : filename
ipos = STRPOS(!VAR.CurrFiles.AE1file, !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.AE1file, ipos+1)
PRINTF, unit, FORMAT='(2A)', 'Aerosol product file   : ', filename EQ '' ? $
        'Not Loaded' : filename
ipos = STRPOS(!VAR.CurrFiles.BiomeFile, !KON.Misc.Slash, /REVERSE_SEARCH)
filename = STRMID(!VAR.CurrFiles.BiomeFile, ipos+1)
PRINTF, unit, FORMAT='(2A)', 'IGBP biome grid file   : ', filename EQ '' ? $
        'Not Loaded' : filename

PRINTF, unit, ''

;---------------------------------------------------------------------------
; Write out the histogram data for this region if requested.
;---------------------------------------------------------------------------

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN PrintHistograms, unit, pRgnData

;---------------------------------------------------------------------------
; Write the header line for the polygon points.
;---------------------------------------------------------------------------

POINT_LUN, -unit, poly_counts
PRINTF, unit, poly_str1 + STRING(FORMAT='(I4)',num_poly_pnts) + poly_str2
PRINTF, unit, pd_str1
PRINTF, unit, pd_str2
PRINTF, unit, pd_str3

;---------------------------------------------------------------------------
; Write the polygon boundary point data for this region if it is a closed
; polygon.
;---------------------------------------------------------------------------

pNextPt = (*pRgn).pNextPolyPt

IF (PTR_VALID(pNextPt) AND $
    (*pRgn).ObjType[1] GE !KON.GeomObjTyp.GEOM_POLYGON_OBJ AND $
    (*pRgn).ObjType[0] GE !KON.AerObjTyp.AER_DUST_OBJ AND $
    (*pRgn).ObjType[0] LE !KON.AerObjTyp.AER_XTRA2_OBJ) THEN BEGIN

   ptnum = 1L

   ;------------------------------------------------------------------------
   ; Loop over all the points in the polygon boundary linked list.
   ;------------------------------------------------------------------------
 
   WHILE (PTR_VALID(pNextPt)) DO BEGIN

      ;---------------------------------------------------------------------
      ; Write out the pertinent data for this region.
      ;---------------------------------------------------------------------

      PRINTF, unit, FORMAT=fmt_pd_str, ptnum, (*((*pNextPt).pData)).lon, $
              (*((*pNextPt).pData)).lat, (*((*pNextPt).pData)).block, $
              (*((*pNextPt).pData)).cross275, (*((*pNextPt).pData)).along275

      ;---------------------------------------------------------------------
      ; Get the pointer to the next point.
      ;---------------------------------------------------------------------

      pNextPt = (*pNextPt).pNextSib
     ptnum += 1

   ENDWHILE

   num_poly_pnts = ptnum - 1

ENDIF

;---------------------------------------------------------------------------
; Write the header line for the direction line points.
;---------------------------------------------------------------------------

PRINTF, unit, ''
POINT_LUN, -unit, dir_counts
PRINTF, unit, dir_str1 + STRING(FORMAT='(I4)',num_dir_pnts) + dir_str2
PRINTF, unit, pd_str1
PRINTF, unit, pd_str2
PRINTF, unit, pd_str3

;---------------------------------------------------------------------------
; Write the direction line point data for this region if it is a polygon
; region with wind direction defined.
;---------------------------------------------------------------------------

pDirPts = (*((*pRgn).pData)).direc_pts_orig

IF (PTR_VALID(pDirPts)) THEN BEGIN

   misr_crds = *pDirPts
   num_pts = (SIZE(misr_crds))[2]

   IF (num_pts GT 1) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Convert the MISR coordinates into lat/lon coordinates.
      ;---------------------------------------------------------------------

      whichorbit = (State.curFrame GT 9)

      MisrCrdToSomCrd,   State.curFrame, misr_crds, som_crds, retval
      SomCrdToLonlatCrd, State.curFrame, som_crds, lonlat_crds, retval

      ;---------------------------------------------------------------------
      ; Loop over all the points in the direction line linked list.
      ;---------------------------------------------------------------------

      FOR ipts=0,num_pts-1 DO BEGIN

         ;------------------------------------------------------------------
         ; Write out the coordinate data for this line.
         ;------------------------------------------------------------------

         PRINTF, unit, FORMAT=fmt_pd_str, ipts+1, lonlat_crds[0,ipts], $
                 lonlat_crds[1,ipts], misr_crds[2,ipts], $
                 misr_crds[0,ipts], misr_crds[1,ipts]
      ENDFOR

   ENDIF

   num_dir_pnts = num_pts

ENDIF

;---------------------------------------------------------------------------
; Write the plume/cloud interior point data if this is a polygon or the line
; point data if this is a digitized line.
;---------------------------------------------------------------------------

pNextPt = (*pRgn).pNextLinePt

IF (PTR_VALID(pNextPt)) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Write the header lines for the points, then get the SOM coords for the
   ; head point in the region.
   ;------------------------------------------------------------------------

   data_string1 = data_str1[0]
   data_string2 = data_str2[0]
   data_string3 = data_str3[0]

   IF (!SAV.Digitize.DRAW_AEROSOL) THEN BEGIN
      data_string1 += data_str1[1]
      data_string2 += data_str2[1]
      data_string3 += data_str3[1]
   ENDIF
   IF (!VAR.CurrFiles.Fire_Loaded) THEN BEGIN
      data_string1 += data_str1[2]
      data_string2 += data_str2[2]
      data_string3 += data_str3[2]
   ENDIF

   PRINTF, unit, ''
   POINT_LUN, -unit, retr_counts
   PRINTF, unit, data_str01 + STRING(FORMAT='(I6)', num_tot_pnts) + data_str02
   PRINTF, unit, data_string1
   PRINTF, unit, data_string2
   PRINTF, unit, data_string3

   ptnum = 1L
   point_inc_num = 0L

   beg_cross = (*((*pNextPt).pData)).somcross
   beg_along = (*((*pNextPt).pData)).somalong

   pt_powr = 0.0

   ;------------------------------------------------------------------------
   ; Loop over all the points in the region's line linked list.
   ;------------------------------------------------------------------------

   WHILE (PTR_VALID(pNextPt)) DO BEGIN

      ;---------------------------------------------------------------------
      ; Compute the distance of this point from the starting point.
      ;---------------------------------------------------------------------

      this_cross = (*((*pNextPt).pData)).somcross
      this_along = (*((*pNextPt).pData)).somalong
      dist_from_start = SQRT($
         (DOUBLE(this_cross - beg_cross) * DOUBLE(this_cross - beg_cross)) + $
         (DOUBLE(this_along - beg_along) * DOUBLE(this_along - beg_along)))

      ;---------------------------------------------------------------------
      ; Write out the pertinent data for this point.
      ;---------------------------------------------------------------------

      winddir_deg = ROUND((*((*pNextPt).pData)).dirfromTruN)
      IF ((*((*pNextPt).pData)).dirfromTruN LT -999.0) THEN winddir_deg = -99
      IF (winddir_deg GE 360.0) THEN winddir_deg -= 360

      feature_type = ''
      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
         feature_code = (*((*pNextPt).pData)).feature_type
         IF (feature_code EQ 0) THEN feature_type = ' body  '
         IF (feature_code EQ 1) THEN feature_type = 'pyrocum'
         IF (feature_code EQ 2) THEN feature_type = 'column '
         IF (feature_code EQ 3) THEN feature_type = ' skirt '
      ENDIF

      hts1 = (*((*pNextPt).pData)).zero_wind_ht[iBand] * 1000.0
      IF (hts1 LE !KON.Misc.BADVALUE_REAL) THEN $
         hts1 = !KON.Misc.BADVALUE_REAL
      
      hts2 = (*((*pNextPt).pData)).corr_wind_ht[iBand] * 1000.0
      IF (hts2 LE !KON.Misc.BADVALUE_REAL) THEN $
         hts2 = !KON.Misc.BADVALUE_REAL
      
      hts3 = (*((*pNextPt).pData)).smooth_hghts[iBand] * 1000.0
      IF (hts3 LE !KON.Misc.BADVALUE_REAL) THEN $
         hts3 = !KON.Misc.BADVALUE_REAL

      wnd1 = (*((*pNextPt).pData)).wnd_speed_cr[iBand]
      IF (wnd1 LE -999.) THEN wnd1 = -99.9

      wnd2 = (*((*pNextPt).pData)).wnd_speed_al[iBand]
      IF (wnd2 LE -999.) THEN wnd2 = -99.9

      IF (wnd1 LE -99. OR wnd2 LT -99.) THEN BEGIN
         wnd3 = -99.9
      ENDIF ELSE BEGIN
         wnd3 = SQRT(wnd1 * wnd1 + wnd2 * wnd2)
      ENDELSE

      ;---------------------------------------------------------------------
      ; Only write the data if certain fields are valid.
      ;---------------------------------------------------------------------

      npower = ((*((*pNextPt).pData)).modis_data[0]) GT 0.0

      IF (ptnum EQ 1 OR hts1 GT !KON.Misc.BADVALUE_REAL OR $
          hts2 GT !KON.Misc.BADVALUE_REAL OR npower GT 0.0) THEN BEGIN

         point_inc_num += 1L

         ;------------------------------------------------------------------
         ; Always include these data.
         ;------------------------------------------------------------------

         pt_lon = (*((*pNextPt).pData)).lon
         pt_lat = (*((*pNextPt).pData)).lat
         pt_blk = (*((*pNextPt).pData)).block
         pt_crs = (*((*pNextPt).pData)).cross275
         pt_alg = (*((*pNextPt).pData)).along275
         pt_tel = ROUND((*((*pNextPt).pData)).terr_elev * 1000.0)

         ;------------------------------------------------------------------
         ; Include aerosol data only if loaded.
         ;------------------------------------------------------------------

         IF (!SAV.Digitize.DRAW_AEROSOL) THEN BEGIN
            aer_tau = (*((*pNextPt).pData)).aer_tau
            ndxs = WHERE(aer_tau LT -999., numndxs)
            IF (numndxs GT 0) THEN aer_tau[ndxs] = -9.999

            aer_ssa = (*((*pNextPt).pData)).aer_ssa
            ndxs = WHERE(aer_ssa LT -999., numndxs)
            IF (numndxs GT 0) THEN aer_ssa[ndxs] = -9.999

            aer_taufrac = (*((*pNextPt).pData)).aer_taufrac
            ndxs = WHERE(aer_taufrac LT -999., numndxs)
            IF (numndxs GT 0) THEN aer_taufrac[ndxs] = -9.999

            aer_angexp = (*((*pNextPt).pData)).aer_angexp
            IF (aer_angexp LT -999.) THEN aer_angexp = -9.999
         ENDIF

         ;------------------------------------------------------------------
         ; Include fire pixel data only if loaded.
         ;------------------------------------------------------------------

         IF (!VAR.CurrFiles.Fire_Loaded) THEN pt_powr = $
            (*((*pNextPt).pData)).modis_data[0]

         ;------------------------------------------------------------------
         ; Print using the appropriate format.
         ;------------------------------------------------------------------

         IF (!SAV.Digitize.DRAW_AEROSOL AND !VAR.CurrFiles.Fire_Loaded) THEN BEGIN
            PRINTF, unit, FORMAT=format_string, point_inc_num, pt_lon, $
                    pt_lat, pt_blk, pt_crs, pt_alg, dist_from_start, $
                    winddir_deg, pt_tel, feature_type, ROUND(hts1), $
                    ROUND(hts2), ROUND(hts3), wnd1, wnd2, wnd3, aer_tau, $
                    aer_ssa, aer_taufrac[0:3], aer_angexp, pt_powr
                    ;, pt_mod1, pt_mod2, pt_mod3, pt_mod4, pt_mod5
         ENDIF ELSE BEGIN
            IF (!SAV.Digitize.DRAW_AEROSOL) THEN BEGIN
               PRINTF, unit, FORMAT=format_string, point_inc_num, pt_lon, $
                       pt_lat, pt_blk, pt_crs, pt_alg, dist_from_start, $
                       winddir_deg, pt_tel, feature_type, ROUND(hts1), $
                       ROUND(hts2), ROUND(hts3), wnd1, wnd2, wnd3, aer_tau, $
                       aer_ssa, aer_taufrac[0:3], aer_angexp
            ENDIF ELSE BEGIN
               IF (!VAR.CurrFiles.Fire_Loaded) THEN BEGIN
                 PRINTF, unit, FORMAT=format_string, point_inc_num, pt_lon, $
                         pt_lat, pt_blk, pt_crs, pt_alg, dist_from_start, $
                         winddir_deg, pt_tel, feature_type, $
                         ROUND(hts1), ROUND(hts2), ROUND(hts3), wnd1, wnd2, $
                         wnd3, pt_powr
               ENDIF ELSE BEGIN
                   PRINTF, unit, FORMAT=format_string, point_inc_num, $
                           pt_lon, pt_lat, pt_blk, pt_crs, pt_alg, $
                           dist_from_start, winddir_deg, pt_tel, $
                           feature_type, ROUND(hts1), ROUND(hts2), $
                           ROUND(hts3), wnd1, wnd2, wnd3
               ENDELSE
            ENDELSE
         ENDELSE

         ;------------------------------------------------------------------
         ; Accumulate data for writing back up to the header later.
         ;------------------------------------------------------------------

         num_tot_pnts += 1L

         IF (is_plume AND hts2 GT !KON.Misc.BADVALUE_REAL) THEN BEGIN
            accum_raw_hts  = [accum_raw_hts,  FLOAT(hts2)]

            this_dir = (*((*pNextPt).pData)).dirfromSomN
            min_dir_diff = ABS(0. - this_dir) < ABS(180. - this_dir) < $
                           ABS(360. - this_dir)
            diff_SOM_along += ROUND(min_dir_diff)

            num_ht_pnts += 1L
         ENDIF

         IF ((~ is_plume) AND hts1 GT !KON.Misc.BADVALUE_REAL) THEN BEGIN
            accum_raw_hts  = [accum_raw_hts, FLOAT(hts1)]
            num_ht_pnts += 1L
         ENDIF

         IF (pt_powr GT 0.0) THEN tot_power += pt_powr

      ENDIF

      ;---------------------------------------------------------------------
      ; Get the pointer to the next point.
      ;---------------------------------------------------------------------

      pNextPt = (*pNextPt).pNextSib
      ptnum += 1

   ENDWHILE

ENDIF

;---------------------------------------------------------------------------
; Go back and write in the std dev of wind-corrected heights, the mean
; difference of points wind direction from SOM along direction and the total
; fire power from summing pixels.
;---------------------------------------------------------------------------

IF (num_ht_pnts GT 0) THEN BEGIN
   mean_raw_hts = MEAN(accum_raw_hts[1:*])
   IF ((mean_raw_hts NE 0.0) AND num_ht_pnts GT 1) THEN $
      stddev_raw_hts = ROUND(STDDEV(accum_raw_hts[1:*]))
ENDIF

IF (is_plume AND num_ht_pnts GT 0) THEN BEGIN
   diff_SOM_along /= FLOAT(num_ht_pnts)
   diff_SOM_along = STRING(FORMAT='(I5)', diff_SOM_along)
ENDIF ELSE BEGIN
   diff_SOM_along = '   -9'
ENDELSE

fire_power = '  -99'
IF (is_plume AND !VAR.CurrFiles.Fire_Loaded) THEN $
   fire_power = STRING(FORMAT='(I5)', ROUND(tot_power))
   
POINT_LUN, unit, stddev_pos

PRINTF, unit, FORMAT='(A,I5)', 'Ht std. deviation (m)  : ', stddev_raw_hts
PRINTF, unit, FORMAT='(A,I5)', 'Ht local variation (m) : ', rms_ht_dev
PRINTF, unit, FORMAT='(A,A5)', '|WndDir-AlongDir|(deg) : ', diff_SOM_along
PRINTF, unit, FORMAT='(A,A5)', 'Total fire power (MW)  : ', fire_power

;---------------------------------------------------------------------------
; Compute the total number of points, the number of pts with wind-corrected
; values, the area percent of region that is filled by points and the plume
; automatic quality factor. Go back and write the values to file below.
;---------------------------------------------------------------------------

pcnt_area = 0.0
IF (num_ht_pnts GT 0) THEN BEGIN
   IF ((*pRgn).ObjType[1] EQ !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN
      pcnt_area = ROUND((num_ht_pnts * !SAV.Digitize.SAMP_SPAC_DIR) / $
                        pRgnData.perimeter * 100.0)
   ENDIF ELSE BEGIN
      pcnt_area = ROUND((num_ht_pnts * area_per_pt) / pRgnData.area * 100.0)
   ENDELSE
ENDIF

POINT_LUN, unit, poly_counts
PRINTF, unit, poly_str1  + STRING(FORMAT='(I4)',num_poly_pnts) + poly_str2
POINT_LUN, unit, dir_counts
PRINTF, unit, dir_str1   + STRING(FORMAT='(I4)',num_dir_pnts)  + dir_str2
POINT_LUN, unit, retr_counts
PRINTF, unit, data_str01 + STRING(FORMAT='(I6)',num_tot_pnts)  + data_str02

POINT_LUN, unit, wind_counts
PRINTF, unit, FORMAT='(A,I5)', 'Num. heights retrieved : ', num_ht_pnts
PRINTF, unit, FORMAT='(A,I5)', 'Percent area covered   : ', pcnt_area

;---------------------------------------------------------------------------
; Close file.
;---------------------------------------------------------------------------

FREE_LUN, unit

ndxs = 0
accum_raw_hts = 0
mean_raw_hts = 0
stddev_raw_hts = 0

;---------------------------------------------------------------------------
; Make sure the file permissions are set for group write.
;---------------------------------------------------------------------------

Retval = ChmodCatchError(tempfile, '666'O)

END  ;  Op_WriteRetrievedData
