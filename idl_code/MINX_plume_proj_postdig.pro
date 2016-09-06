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
PRO GetProjectDirNames, DirType, ProjDir, ProjName, SubdirList, OrbitList, $
                        NumOrbit, Retval
;***************************************************************************
; Select project directory and collect all orbit subdirectory names.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Select project directory to process and get project name. Previously used
; directory if any is the default. Otherwise use working directory.
;---------------------------------------------------------------------------

repeat_it: GetLastFilename, 0, DirType, '*', 0, ProjDir, dummy
IF (ProjDir EQ '') THEN GOTO, ask_user

;---------------------------------------------------------------------------
; Get the project name from the highest level directory component, and test
; if user has mistakenly specified an orbit subdirectory.
;---------------------------------------------------------------------------

toks = STRSPLIT(ProjDir, !KON.Misc.Slash, /EXTRACT, COUNT=ntok)
ProjName = toks[ntok-1]

IF (STRLEN(toks[ntok-1]) EQ 6 AND $
    STRMID((toks[ntok-1])[0], 0, 1) EQ '0') THEN GOTO, ask_user

;---------------------------------------------------------------------------
; Collect the subdirectory names of all orbit/block-range groups and let the
; user select the first orbit to process.
;---------------------------------------------------------------------------

SubdirList = FILE_SEARCH(ProjDir + '0?????', /TEST_DIRECTORY, COUNT=NumOrbit)

IF (SubdirList[0] EQ '') THEN GOTO, ask_user

OrbitList = STRARR(NumOrbit)

FOR iorb=0,NumOrbit-1 DO BEGIN
   toks = STRSPLIT(SubdirList[iorb], !KON.Misc.Slash, /EXTRACT, COUNT=numtoks)
   OrbitList[iorb] = toks[numtoks-1]
ENDFOR

Retval = 0

;---------------------------------------------------------------------------
; Make sure the orbit list is sorted.
;---------------------------------------------------------------------------

OrbitList = OrbitList[SORT(OrbitList)]

RETURN

;---------------------------------------------------------------------------
; Branch here if any problem wih the directory name occurred.
;---------------------------------------------------------------------------

ask_user:
mssg = ['You must specify a project directory in the "Selection"', $
        'listbox at the bottom of the dialog box. This directory', $
        'must be one level above the Orbit "subdirectories"', $
        '(e.g. 012345) that contain digitized plume files.', $
        'Select "Yes" to try again or "No" to abort processing.']
        
rtrn = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)
IF (STRUPCASE(rtrn) EQ 'YES') THEN GOTO, repeat_it

Retval = -1

END  ;  GetProjectDirNames

;***************************************************************************
;***************************************************************************
PRO JPGsFromFileToMP4, CamImageList, CamImages, NumColor, Xsize, Ysize, $
                       OutputMP4
;***************************************************************************
; Procedure is passed an array of 9 JPEG files corresponding to camera
; images, and it returns a single MP4 file. This produces a much better
; quality MP4 than if pixels are captured from an image and processed
; directly into a MP4.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

CAM_ORDER = [8, 6, 4, 1, 2, 0, 3, 5, 7]  ; index into alphabetic camera
                                         ; order from chronological order

;---------------------------------------------------------------------------
; Load the 9 camera images into a single array based on the size of
; the An camera image.
;-------------------------------------------------------------------------

READ_JPEG, CamImageList[CAM_ORDER[4]], /ORDER, one_image
An_size = SIZE(one_image)

NumColor = (An_size[0] EQ 3) ? 3 : 1
Xsize = (An_size[0] EQ 3) ? An_size[2] : An_size[1]
Ysize = (An_size[0] EQ 3) ? An_size[3] : An_size[2]

;---------------------------------------------------------------------------
; Create an array to contain the 9 camera images and copy the An
; image into it.
;---------------------------------------------------------------------------

IF (An_size[0] EQ 3) THEN BEGIN
   CamImages = BYTARR(An_size[1],An_size[2],An_size[3],9)
   CamImages[*,*,*,4] = one_image
ENDIF ELSE BEGIN
   CamImages = BYTARR(An_size[1],An_size[2],9)
   CamImages[*,*,4] = one_image
ENDELSE

;--------------------------------------------------------------------------
; Set error handler for absence of MP4 license.
;---------------------------------------------------------------------------

error_status = 0
CATCH, error_status
IF (error_status NE 0) THEN BEGIN
   mssg = ['MP4 license may not be available.', $
           'No animation was saved. Returning.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   CATCH, /CANCEL
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Load the 8 remaining camera images into the array.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   IF (icam EQ 4) THEN CONTINUE
   READ_JPEG, CamImageList[CAM_ORDER[icam]], /ORDER, one_image
   
   ;------------------------------------------------------------------------
   ; All camera images must be the same size.
   ;------------------------------------------------------------------------
   
   IF ((SIZE(one_image))[0] NE An_size[0] OR $
       (SIZE(one_image))[1] NE An_size[1] OR $
       (SIZE(one_image))[2] NE An_size[2]) THEN BEGIN
      mssg = ['The size of the JPEG image for camera file', $
              CamImageList[CAM_ORDER[icam]], $
              'is not the same as the size of the An image.', $
              'Returning.']
      rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      one_image = 0
      CamImages = 0
      CATCH, /CANCEL
      RETURN
   ENDIF
      
   IF (An_size[0] EQ 3) THEN CamImages[*,*,*,icam] = one_image
   IF (An_size[0] EQ 2) THEN CamImages[*,*,icam] = one_image

ENDFOR

one_image = 0

;---------------------------------------------------------------------------
; Initialize the MP4 image.
;---------------------------------------------------------------------------

VideoMP4_Open, OutputMP4, Xsize, Ysize, !SAV.Digitize.FRAME_PER_SEC, $
               !SAV.Digitize.BIT_RATE, VidObj, vidStream

;---------------------------------------------------------------------------
; Now write the images to the MP4 file and close it.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   IF (An_size[0] EQ 3) THEN VideoMP4_Put, VidObj, vidStream, $
                                           REVERSE(CamImages[*,*,*,icam],3)
   IF (An_size[0] EQ 2) THEN VideoMP4_Put, VidObj, vidStream, $
                                           REVERSE(CamImages[*,*,icam],2)
ENDFOR

VideoMP4_Close, VidObj

rtrn_val = ChmodCatchError(OutputMP4, '666'O)

CATCH, /CANCEL

END  ;  JPGsFromFileToMP4

;***************************************************************************
PRO ConvertJPGsToMP4, DefaultProjDir
;***************************************************************************
; Program processes all plumes in a project directory. For each plume that
; has 9 JPEG camera image files, it converts them to a single MP4 file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Select directory of project to process and get list of orbit subdirectories.
;---------------------------------------------------------------------------

GetProjectDirNames, !KON.FileTyp.TypeJPGtoMP4Dir, proj_dir, proj_name, $
                    dirlist, orbit_list, NumOrbit, retval
IF (retval LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Select orbit in list to begin with. If user previously stopped reviewing
; plumes before finishing, this allows him to continue where he left off.
;---------------------------------------------------------------------------

OrbitList_gui, orbit_list, OrbitNdx

IF (OrbitNdx EQ -1) THEN RETURN

;---------------------------------------------------------------------------
; Ask if user wants interaction.
;---------------------------------------------------------------------------

msg = 'Do you want to view the JPEG files as they are processed?'
view_approve = DIALOG_MESSAGE(msg, /QUESTION, /CENTER)

;---------------------------------------------------------------------------
; Loop over all the orbit subdirectories.
;---------------------------------------------------------------------------

ntotregion = 0

FOR idir=OrbitNdx,NumOrbit-1 DO BEGIN

   nregion = 0

   ;------------------------------------------------------------------------
   ; Create the An JPEG file names for every region in current orbit.
   ;------------------------------------------------------------------------

   file_jpg_template = dirlist[idir] + !KON.Misc.Slash + '*' + $
                       'PlumeAnimationAn.jpg'
   file_jpg_list = FILE_SEARCH(file_jpg_template, /TEST_REGULAR, $
                               COUNT=num_jpg_file)

   IF (num_jpg_file EQ 0) THEN BEGIN
      file_jpg_template = dirlist[idir] + !KON.Misc.Slash + 'MINX*_An.jpg'
      file_jpg_list = FILE_SEARCH(file_jpg_template, /TEST_REGULAR, $
                                  COUNT=num_jpg_file)
      IF (num_jpg_file EQ 0) THEN BEGIN
         nLen = STRLEN(dirlist[idir])
         mssg = 'There are no JPEG animation files to convert in orbit ' + $
                STRMID(dirlist[idir], 4, /REVERSE_OFFSET)
         rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
         CONTINUE
      ENDIF
   ENDIF

   ;------------------------------------------------------------------------
   ; Loop over all the digitized regions in this orbit.
   ;------------------------------------------------------------------------

   FOR ifile=0,num_jpg_file-1 DO BEGIN

      toks = STRSPLIT(file_jpg_list[ifile], '_' + !KON.Misc.Slash, /EXTRACT, $
                      COUNT=numtoks)
      rgn_name = toks[numtoks-2]
      
      rgn_cam_template = dirlist[idir] + !KON.Misc.Slash + rgn_name + $
                         '_PlumeAnimation??.jpg'
      rgn_cam_list = FILE_SEARCH(rgn_cam_template, /TEST_REGULAR, $
                                 COUNT=num_jpg_cam)

      IF (num_jpg_cam EQ 0) THEN BEGIN
         rgn_cam_template = dirlist[idir] + !KON.Misc.Slash + $
                             'MINX*' + rgn_name + '_??.jpg'
         rgn_cam_list = FILE_SEARCH(rgn_cam_template, /TEST_REGULAR, $
                                    COUNT=num_jpg_cam)
      ENDIF

      IF (num_jpg_cam NE 9) THEN BEGIN
         msg = ['Region ' + rgn_name + ' has fewer than 9 camera JPEG files.', $
                'Check this and redo this region.']
         res = DIALOG_MESSAGE(msg, /ERROR, /CENTER)
         CONTINUE
      ENDIF

      rgn_mp4 = dirlist[idir] + !KON.Misc.Slash + rgn_name + '_PlumeAnimation.mp4'

      ;---------------------------------------------------------------------
      ; Do the conversion from JPEG to MP4..
      ;---------------------------------------------------------------------

      JPGsFromFileToMP4, rgn_cam_list, cam_image, NumColor, Xsize, Ysize, $
                         rgn_mp4

      ;---------------------------------------------------------------------
      ; Display the camera JPEG images if desired.
      ;---------------------------------------------------------------------

      IF (STRUPCASE(view_approve) EQ 'YES') THEN BEGIN
         
         FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
            xbeg = ((icam LE 3) ? (Xsize + 25) * icam + 25 : $
                   ((icam GE 5) ? (Xsize + 25) * (icam - 5) + 25 : $
                                  FIX(Xsize * 1.5)))

            ybeg = ((icam LE 3) ? !KON.Misc.ScreenY - (Ysize + 25) * 1 : $
                   ((icam GE 5) ? !KON.Misc.ScreenY - (Ysize + 25) * 3 : $
                                  !KON.Misc.ScreenY - (Ysize + 25) * 2))

            WINDOW, 5+icam, TITLE='MP4 for region: ' + rgn_name, $
                    XPOS=xbeg, YPOS=ybeg, XSIZE=Xsize, YSIZE=Ysize

            IF (NumColor EQ 3) THEN TV, cam_image[*,*,*,icam], TRUE=1, /ORDER
            IF (NumColor EQ 1) THEN TV, cam_image[*,*,icam], TRUE=1, /ORDER
         ENDFOR

      ENDIF

      ;---------------------------------------------------------------------
      ; Ask user if he's done.
      ;---------------------------------------------------------------------

      IF (STRUPCASE(view_approve) EQ 'YES') THEN BEGIN
         msg = 'Click OK when done viewing.'
         rtrn = DIALOG_MESSAGE(msg, /INFO, /CENTER)
         FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
            SafeWDELETE, 5+icam, didit  ; Delete the old JPEG images when done
         ENDFOR
      ENDIF
 
      nregion += 1
      ntotregion += 1

      IF (ifile EQ num_jpg_file-1) THEN $
         PRINT, 'Number of MP4s created for orbit ' + orbit_list[idir] + $
                ' = ' + STRTRIM(STRING(nregion),2)
                
      cam_image = 0
      An_size = 0

   ENDFOR

ENDFOR

;---------------------------------------------------------------------------
; Print the number of regions processed successfully.
;---------------------------------------------------------------------------

mssg = 'Total number of regions processed for project = ' + $
       STRTRIM(STRING(ntotregion),2)
rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

dirlist = 0
orbit_list = 0
file_jpg_list = 0
rgn_cam_list = 0

END  ;  ConvertJPGsToMP4

;***************************************************************************
;***************************************************************************
PRO GetRegionStatus_eh, event
;***************************************************************************
; Event handler for user interface for getting region status. 
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON rgnstat, RgnStatus

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   RgnStatus = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct.button1 : BEGIN
   END
   event.id EQ widget_struct.button2 : BEGIN
   END
   event.id EQ widget_struct.button3 : BEGIN
   END
   event.id EQ widget_struct.button4 : BEGIN
   END

   event.id EQ widget_struct.ok_button : BEGIN
      onoff = WIDGET_INFO(widget_struct.button1, /BUTTON_SET)
      IF (onoff) THEN RgnStatus = 1
      onoff = WIDGET_INFO(widget_struct.button2, /BUTTON_SET)
      IF (onoff) THEN RgnStatus = 2
      onoff = WIDGET_INFO(widget_struct.button3, /BUTTON_SET)
      IF (onoff) THEN RgnStatus = 3
      onoff = WIDGET_INFO(widget_struct.button4, /BUTTON_SET)
      IF (onoff) THEN RgnStatus = 4
      
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct.cancel_button : BEGIN
      RgnStatus = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY

END ; GetRegionStatus_eh

;***************************************************************************
PRO GetRegionStatus_gui, PlumeName, Band, ParmStrs, ParmValsRed, $
                         ParmValsBlu, XYoffset, RegionStatus
;***************************************************************************
; User interface for getting digitized region status.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON rgnstat, RgnStatus

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

RgnStatus = RegionStatus

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0   = WIDGET_BASE( /COLUMN, TITLE='Region Status', /TLB_KILL_REQUEST_EVENTS)

label0  = WIDGET_LABEL( base0, VALUE='Plume:    ' + PlumeName)
base1   = WIDGET_BASE(  base0, /COLUMN, /FRAME, /ALIGN_CENTER )

row_str = '    PARAMETER         RED   BLUE '
label00 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[0] + ParmValsRed[0] + '  ' + ParmValsBlu[0]
label01 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[1] + ParmValsRed[1] + '  ' + ParmValsBlu[1]
label02 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[2] + ParmValsRed[2] + '  ' + ParmValsBlu[2]
label03 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[3] + ParmValsRed[3] + '  ' + ParmValsBlu[3]
label04 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[4] + ParmValsRed[4] + '  ' + ParmValsBlu[4]
label05 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )
row_str = ParmStrs[5] + ParmValsRed[5] + '  ' + ParmValsBlu[5]
label06 = WIDGET_LABEL( base1, VALUE=row_str, /ALIGN_LEFT )

label07 = WIDGET_LABEL( base0, VALUE='' )
label08 = WIDGET_LABEL( base0, VALUE='Select Plume Disposition:' )
label1  = WIDGET_LABEL( base0, VALUE='(Files are not Altered Here)' )

base3   = WIDGET_BASE(  base0, /COLUMN, /FRAME, /ALIGN_CENTER )
base4   = WIDGET_BASE(  base3, /COLUMN, /EXCLUSIVE )
button1 = WIDGET_BUTTON( base4, VALUE='Mark to keep plume -  red better' )
button2 = WIDGET_BUTTON( base4, VALUE='Mark to keep plume - blue better' )
button3 = WIDGET_BUTTON( base4, VALUE='Mark to delete plume' )
button4 = WIDGET_BUTTON( base4, VALUE='Mark to redigitize plume' )

base5 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( base5, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( base5, VALUE='Done For Now' )

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

IF (Band EQ 'R') THEN BEGIN
   IF (RegionStatus LE 2) THEN WIDGET_CONTROL, button1, /SET_BUTTON
   WIDGET_CONTROL, button2, SENSITIVE=0
ENDIF

IF (Band EQ 'B') THEN BEGIN
   IF (RegionStatus LE 2) THEN WIDGET_CONTROL, button2, /SET_BUTTON
   WIDGET_CONTROL, button1, SENSITIVE=0
ENDIF

IF (Band EQ 'RB') THEN BEGIN
   IF (RegionStatus EQ 1) THEN WIDGET_CONTROL, button1, /SET_BUTTON
   IF (RegionStatus EQ 2) THEN WIDGET_CONTROL, button2, /SET_BUTTON
ENDIF

IF (RegionStatus EQ 3) THEN WIDGET_CONTROL, button3, /SET_BUTTON
IF (RegionStatus EQ 4) THEN WIDGET_CONTROL, button4, /SET_BUTTON

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct = { $
   button1       : button1,   $         ; keep region - red better
   button2       : button2,   $         ; keep region - blue better
   button3       : button3,   $         ; delete region
   button4       : button4,   $         ; redigitize region
   ok_button     : ok_button, $
   cancel_button : cancel_button, $
   RgnStatus     : RgnStatus }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct, XOFFSET=XYoffset[0], $
                YOFFSET=XYoffset[1], /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'GetRegionStatus_gui', base0, EVENT_HANDLER='GetRegionStatus_eh'

RegionStatus = RgnStatus

END ; GetRegionStatus_gui

;***************************************************************************
PRO OrbitList_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orblist_parm, orblist_index

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=orbit_list_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   orblist_index = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ orbit_list_struct.orblist : BEGIN
      orblist_index = event.Index
   END
   event.id EQ orbit_list_struct.ok_button : BEGIN
      IF (orblist_index LT 0) THEN BEGIN
         mssg = 'You must select an orbit. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDELSE
   END
   event.id EQ orbit_list_struct.cancel_button : BEGIN
      orblist_index = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=orbit_list_struct, /NO_COPY

END ; OrbitList_eh

;***************************************************************************
PRO OrbitList_gui, OrbitList, OrbitNdx
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orblist_parm, orblist_index

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

nregions = N_ELEMENTS(OrbitList)
nlines = nregions < 20

base0 = WIDGET_BASE(/COLUMN, TITLE='Select Starting Orbit', $
                    /TLB_KILL_REQUEST_EVENTS)
listname = WIDGET_LABEL(base0, /ALIGN_CENTER, VALUE='Orbits in Project')
orblist = WIDGET_LIST(base0, FRAME=1, VALUE=OrbitList, YSIZE=nlines)

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE='Cancel')

WIDGET_CONTROL, orblist, SET_LIST_SELECT=0
orblist_index = 0

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

orbit_list_struct = { orblist       : orblist, $
                      ok_button     : ok_button, $
                      cancel_button : cancel_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=orbit_list_struct, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'OrbitList_gui', base0, EVENT_HANDLER='OrbitList_eh'

OrbitNdx = orblist_index

END ; OrbitList_gui

;***************************************************************************
PRO GetPlumeTxtFileParms, Exist, TextFile, ParmStrs, ParmVals
;***************************************************************************
; Read data from the raw plume text file to post on images. Rewind file each
; time to accommodate changes in order of parameters between MINX versions.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumHeight = '  NA '
PcntCover = '  NA '
HtStdDev  = '  NA '
WindDiff  = '  NA '
FirePower = '  NA '
QflagAuto = '  NA '
QflagUser = '  NA '

IF (Exist) THEN BEGIN

   buf = ''

   OPENR, unit, TextFile, /GET_LUN

   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Num. heights retrieved' OR $
          toks[0] EQ 'Wind-corrected points'  OR $
          toks[0] EQ 'Total points in table') THEN BEGIN
         NumHeight = STRTRIM(toks[1],2)
         FOR irpt=0,3 DO BEGIN
            IF (STRLEN(NumHeight) LT 5) THEN NumHeight = ' ' + NumHeight
         ENDFOR
         BREAK
      ENDIF
   ENDWHILE

   POINT_LUN, unit, 0
   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Percent area covered') THEN BEGIN
         PcntCover = ' ' + STRTRIM(toks[1],2)
         FOR irpt=0,2 DO BEGIN
            IF (STRLEN(PcntCover) LT 5) THEN PcntCover = ' ' + PcntCover
         ENDFOR
         BREAK
      ENDIF
   ENDWHILE

   POINT_LUN, unit, 0
   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Ht local variation (m)' OR $
          toks[0] EQ 'StdDev metric, zeroht' OR $
          toks[0] EQ 'StdDev metric, corrht') THEN BEGIN
         HtStdDev = ' ' + STRTRIM(toks[1],2)
         FOR irpt=0,3 DO BEGIN
            IF (STRLEN(HtStdDev) LT 5) THEN HtStdDev = ' ' + HtStdDev
         ENDFOR
         BREAK
      ENDIF
   ENDWHILE

   POINT_LUN, unit, 0
   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Diff WindDir, AlongDir' OR $
          toks[0] EQ '|WndDir-AlongDir|(deg)') THEN BEGIN
         WindDiff = STRTRIM(toks[1],2)
         FOR irpt=0,2 DO BEGIN
            IF (STRLEN(WindDiff) LT 5) THEN WindDiff = ' ' + WindDiff
         ENDFOR
         BREAK
      ENDIF
   ENDWHILE

   POINT_LUN, unit, 0
   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Total fire power (MW)' OR $
          toks[0] EQ 'Power of fire in MW') THEN BEGIN
         FirePower = STRTRIM(toks[1],2)
         nlen = STRLEN(FirePower)
         IF (nlen GT 5) THEN FirePower = STRMID(FirePower, 0, nlen-2)
         FOR irpt=0,1 DO BEGIN
            IF (STRLEN(FirePower) LT 5) THEN FirePower = ' ' + FirePower
         ENDFOR
         BREAK
      ENDIF
   ENDWHILE

   POINT_LUN, unit, 0
   WHILE (~EOF(unit)) DO BEGIN
      READF, unit, buf
      toks = STRTRIM(STRSPLIT(buf, ':', /EXTRACT),2)
      IF (toks[0] EQ 'Retrieval quality est.' OR $
          toks[0] EQ 'Retrieval quality' OR $
          toks[0] EQ 'Auto retrieval quality') THEN BEGIN
         QflagAuto = ' ' + STRTRIM(toks[1],2)
         BREAK
      ENDIF
   ENDWHILE
   
   FREE_LUN, unit

ENDIF

ParmStrs = [' # Hts Retrieved  =  ', ' Percent Coverage =  ', $
            ' Height Variation =  ', ' Wind Dir rel NS  =  ', $
            ' Fire Power (MW)  =  ', ' Retrieve Quality =  ']

ParmVals = [NumHeight, PcntCover, HtStdDev , WindDiff , FirePower, QflagAuto]

END  ;  GetPlumeTxtFileParms

;***************************************************************************
PRO PlumeReview_FillStruct, PlumeParm, FileTextList, iFile, FileDone, $
                            Band, ThisPlume, OtherPlume, nPlume, $
                            nPlumeRed, nPlumeBlu
;***************************************************************************
; Fill the PlumeParm structure with parameters for this plume.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the band character code to determine whether this is a red or blue
; band retrieval. Then build the file name for the other type. If both are
; present, make the red band the default.
;---------------------------------------------------------------------------

PlumeParm.red_file_name = FileTextList[iFile]

npos = STRPOS(PlumeParm.red_file_name, '-', /REVERSE_SEARCH) + 4
Band = STRMID(PlumeParm.red_file_name, npos, 1)

IF (Band EQ 'B') THEN BEGIN
   PlumeParm.blu_file_name = PlumeParm.red_file_name
   temp_file = PlumeParm.red_file_name
   STRPUT, temp_file, 'R', npos
   PlumeParm.red_file_name = temp_file
   unknown_file = PlumeParm.red_file_name
ENDIF
IF (Band EQ 'R') THEN BEGIN
   temp_file = PlumeParm.red_file_name
   STRPUT, temp_file, 'B', npos
   PlumeParm.blu_file_name = temp_file
   unknown_file = PlumeParm.blu_file_name
ENDIF

;---------------------------------------------------------------------------
; Test whether the other is also in the same orbit directory. If only red or
; both red and blue band retrievals are available, then there will be only
; red versions of the PlumePoints, Aerosol & Animation files (Aerosol isn't
; needed here). If only the blue band retrieval is present, then only blue
; versions of these files are available.
;---------------------------------------------------------------------------

ndx = WHERE(FileTextList EQ unknown_file, numndx)

IF (numndx EQ 0) THEN BEGIN
   IF (Band EQ 'R') THEN PlumeParm.blu_file_name = ''
   IF (Band EQ 'B') THEN PlumeParm.red_file_name = ''
ENDIF ELSE BEGIN
   IF (numndx GT 1) THEN ndx = ndx[0]
   Band = 'RB'
   FileDone[ndx] = 1
ENDELSE
 
IF (Band EQ 'R' OR Band EQ 'RB') THEN BEGIN
   toks = STRSPLIT(PlumeParm.red_file_name, '_.', /EXTRACT, COUNT=numtok)
   PlumeParm.red_plume_name = toks[numtok-2]
   nPlume += 1
   nPlumeRed += 1
ENDIF

IF (Band EQ 'B' OR Band EQ 'RB') THEN BEGIN
   toks = STRSPLIT(PlumeParm.blu_file_name, '_.', /EXTRACT, COUNT=numtok)
   PlumeParm.blu_plume_name = toks[numtok-2]
   nPlume += 1
   nPlumeBlu += 1
ENDIF
 
;---------------------------------------------------------------------------
; Set up the file names for displaying.
;---------------------------------------------------------------------------

ThisPlume  = ''
OtherPlume = ''

IF (Band EQ 'R') THEN ThisPlume = PlumeParm.red_plume_name
IF (Band EQ 'B') THEN ThisPlume = PlumeParm.blu_plume_name
IF (Band EQ 'RB') THEN BEGIN
   ThisPlume  = PlumeParm.red_plume_name
   OtherPlume = PlumeParm.blu_plume_name
ENDIF

ndx = 0
toks = 0

END  ;  PlumeReview_FillStruct

;***************************************************************************
PRO PlumeReview_PlotImages, PlumeParm, DirList, iDir, Band, ThisPlume, $
                            OtherPlume, StatusCode, RegionStatus
;***************************************************************************
; Get the file names for the images to draw, load the images and show them
; on the screen.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the unit numbers for the map windows.
;---------------------------------------------------------------------------

num_wndw = 5
map_wndw = INDGEN(num_wndw) + 11

;---------------------------------------------------------------------------
; Get the file name for the animation file, for colored height maps file(s),
; for the digitized points file and for the height/wind profile file(s).
;---------------------------------------------------------------------------

file_anim = DirList[iDir] + !KON.Misc.Slash + ThisPlume + $
            '_PlumeAnimation.' + 'mp4'
IF (~FILE_TEST(file_anim)) THEN $
   file_anim = DirList[iDir] + !KON.Misc.Slash + ThisPlume + $
               '_PlumeAnimation.' + 'mpg'

file_cont1 = DirList[iDir] + !KON.Misc.Slash + ThisPlume + $
             '_PlumeContours_An.jpg'
IF (Band EQ 'RB') THEN $
   file_cont2 = DirList[iDir] + !KON.Misc.Slash + OtherPlume + $
                '_PlumeContours_An.jpg'

file_pnts = DirList[iDir] + !KON.Misc.Slash + ThisPlume + $
            '_PlumePoints_An.jpg'

file_plot1 = DirList[iDir] + !KON.Misc.Slash + ThisPlume + $
             '_HtWindPlot.png'
IF (Band EQ 'RB') THEN $
   file_plot2 = DirList[iDir] + !KON.Misc.Slash + OtherPlume + $
             '_HtWindPlot.png'

;---------------------------------------------------------------------------
; Load the MP4, JPEG and PNG images from their files.
;---------------------------------------------------------------------------

anim_exist  = FILE_TEST(file_anim,  /REGULAR)
pnts_exist  = FILE_TEST(file_pnts,  /REGULAR)
cont_exist1 = FILE_TEST(file_cont1, /REGULAR)
plot_exist1 = FILE_TEST(file_plot1, /REGULAR)
cont_exist2 = 0
plot_exist2 = 0  

IF (Band EQ 'RB') THEN BEGIN
   cont_exist2 = FILE_TEST(file_cont2, /REGULAR)
   plot_exist2 = FILE_TEST(file_plot2, /REGULAR)
ENDIF

IF (pnts_exist) THEN BEGIN
   READ_JPEG, file_pnts, pnts_image
   pnts_size = SIZE(pnts_image)
ENDIF ELSE BEGIN
   mssg = ['There is no digitized points image file: ', file_pnts, $
           'Other files may also be missing, so skipping this plume.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDELSE

IF (cont_exist1) THEN BEGIN
   READ_JPEG, file_cont1, cont_image1
   cont_size1 = SIZE(cont_image1)
ENDIF ELSE BEGIN
   mssg = ['There is no colored heights image file: ', file_cont1, $
           'Other files may also be missing, so skipping this plume.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDELSE

IF (plot_exist1) THEN BEGIN
   READ_PNG, file_plot1, plot_image1
   plot_size1 = SIZE(plot_image1)
ENDIF ELSE BEGIN
   mssg = ['There is no heights profile image file: ', file_plot1, $
      'Other files may also be missing, so skipping this plume.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDELSE

IF (Band EQ 'RB') THEN BEGIN
   IF (cont_exist2) THEN BEGIN
      READ_JPEG, file_cont2, cont_image2
      cont_size2 = SIZE(cont_image2)
   ENDIF
   
   IF (plot_exist2) THEN BEGIN
      READ_PNG, file_plot2, plot_image2
      plot_size2 = SIZE(plot_image2)
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Display the animation. 
;---------------------------------------------------------------------------

IF (anim_exist) THEN SPAWN, 'open -F ' + file_anim + ' &'

;---------------------------------------------------------------------------
; Display the JPEG and PNG images.
;---------------------------------------------------------------------------

IF (cont_exist1 AND pnts_exist) THEN BEGIN
   xbeg = pnts_size[2] + 150
   ybeg = !KON.Misc.ScreenY - cont_size1[3] - 100
   xy_beg_dlg = [xbeg + cont_size1[2] + 50, ybeg - plot_size1[3] - 50]
   WINDOW, map_wndw[0], TITLE='Color-coded Heights for ' + ThisPlume, $
           XPOS=xbeg, YPOS=ybeg, XSIZE=cont_size1[2], YSIZE=cont_size1[3]
   TV, cont_image1, TRUE=1
ENDIF

IF (cont_exist1 AND cont_exist2 AND plot_exist1) THEN BEGIN
   xbeg += (cont_size1[2] > plot_size1[2]) + 200
   ybeg = !KON.Misc.ScreenY - cont_size2[3] - 100
   WINDOW, map_wndw[1], TITLE='Color-coded Heights for ' + OtherPlume, $
           XPOS=xbeg, YPOS=ybeg, XSIZE=cont_size2[2], YSIZE=cont_size2[3]
   TV, cont_image2, TRUE=1
ENDIF

IF (cont_exist1 AND pnts_exist) THEN BEGIN
   xbeg = 50
   ybeg = !KON.Misc.ScreenY - cont_size1[3] - pnts_size[3] - 150
   WINDOW, map_wndw[2], TITLE='Digitized Points for ' + ThisPlume, $
           XPOS=xbeg, YPOS=ybeg, XSIZE=pnts_size[2], YSIZE=pnts_size[3]
   TV, pnts_image, TRUE=1
ENDIF

IF (cont_exist1 AND plot_exist1 AND pnts_exist) THEN BEGIN
   xbeg = pnts_size[2] + 150
   ybeg = !KON.Misc.ScreenY - cont_size1[3] - plot_size1[3] - 150
   WINDOW, map_wndw[3], TITLE='Height-Wind Plots for ' + ThisPlume, $
           XPOS=xbeg, YPOS=ybeg, XSIZE=plot_size1[2], YSIZE=plot_size1[3]
   TV, plot_image1, TRUE=1
ENDIF

IF (cont_exist1 AND cont_exist2 AND plot_exist1 AND plot_exist2) THEN BEGIN
   xbeg += (cont_size1[2] > plot_size1[2]) + 50
   ybeg = !KON.Misc.ScreenY - cont_size2[3] - plot_size1[3] - 150
   WINDOW, map_wndw[4], TITLE='Height-Wind Plots for ' + OtherPlume, $
           XPOS=xbeg, YPOS=ybeg, XSIZE=plot_size2[2], YSIZE=plot_size2[3]
   TV, plot_image2, TRUE=1
ENDIF

;---------------------------------------------------------------------------
; Read pertinent data from plumes' text files to display in next dialog box.
;---------------------------------------------------------------------------

GetPlumeTxtFileParms, (Band EQ 'R' OR Band EQ 'RB'), $
                      PlumeParm.red_file_name, parm_strs, parm_vals_red   
GetPlumeTxtFileParms, (Band EQ 'B' OR Band EQ 'RB'), $
                      PlumeParm.blu_file_name, parm_strs, parm_vals_blu

;---------------------------------------------------------------------------
; Show dialog box to ask user how to classify this region (see "status_code"
; options). If user hits the cancel button, stop processing.
;---------------------------------------------------------------------------

GetRegionStatus_gui, ThisPlume, Band, parm_strs, parm_vals_red, $
                     parm_vals_blu, xy_beg_dlg, RegionStatus
PlumeParm.status = (RegionStatus GT 0) ? StatusCode[RegionStatus-1] : ''

;---------------------------------------------------------------------------
; Kill the animation. 
;---------------------------------------------------------------------------

SPAWN, 'ps -U ' + !KON.Misc.UserName, rtrnval, err_result

numlins = (SIZE(rtrnval))[1]
FOR ilin=0,numlins-1 DO BEGIN
   IF (STRPOS(rtrnval[ilin], 'QuickTime') GE 0 OR $
       STRPOS(rtrnval[ilin], 'VLC') GE 0 OR $
       STRPOS(rtrnval[ilin], 'RealPlayer') GE 0 OR $
       STRPOS(rtrnval[ilin], 'Windows Media Player') GE 0) THEN BEGIN
      toks = STRSPLIT(rtrnval[ilin], ' ', /EXTRACT, COUNT=numtok)
      IF (numtok GT 1) THEN BEGIN
         SPAWN, 'kill -s 9 ' + STRTRIM(STRING(toks[0]),2), rtrnval
         BREAK
      ENDIF
   ENDIF
ENDFOR

;---------------------------------------------------------------------------
; Kill the other images and delete the images.
;---------------------------------------------------------------------------

FOR iwndw=0,num_wndw-1 DO BEGIN
   SafeWDELETE, map_wndw[iwndw], didit
   map_wndw[iwndw] = -1
ENDFOR

map_wndw = 0
pnts_image  = 0
cont_image1 = 0
cont_image2 = 0
plot_image1 = 0
plot_image2 = 0
parm_strs = 0
parm_vals_red = 0
parm_vals_blu = 0

END  ;  PlumeReview_PlotImages

;***************************************************************************
PRO PlumeReview_WriteResults, Pass, ProjDir, OutLogFile, PlumeList, StatusCode
;***************************************************************************
; Separate the plumes into categories and write them to the log file. For
; plumes being kept as is, write them in red-blue or blue-red order with the
; preferred band listed first.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Define the header lines to write to the results file.
;---------------------------------------------------------------------------

header_name = ['Regions To Keep', 'Regions To Delete', 'Regions To Redigitize']

;---------------------------------------------------------------------------
; Open the results file for output and write description records.
;---------------------------------------------------------------------------

OPENW, unit, OutLogFile, /GET_LUN
PRINTF, unit, 'Disposition of plumes reviewed in Pass ' + Pass + '.'
PRINTF, unit, 'First column in "Keep" group is superior.'
PRINTF, unit, 'For plumes in project directory:'
PRINTF, unit, ProjDir
PRINTF, unit, ''

;---------------------------------------------------------------------------
; Loop over the different plume disposition (status) types and collect all
; plumes user categorized as same type into unique lists for output.
;---------------------------------------------------------------------------

numndx0 = 0
numndx1 = 0
ihdr_type = 0
rgn_name0 = ['']
rgn_name1 = ['']

FOR itype=0,N_ELEMENTS(StatusCode)-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Collect all plume structures for this disposition type.
   ;------------------------------------------------------------------------

   ndxs = WHERE(PlumeList[*].status EQ StatusCode[itype], numndx)

   ;------------------------------------------------------------------------
   ; Here to process plumes if there are valid occurences of this type.
   ;------------------------------------------------------------------------

   IF (numndx GT 0) THEN BEGIN
      temp_list = PlumeList[ndxs]  
      
      ;---------------------------------------------------------------------
      ; Here to process all plumes the user decided to keep and judged
      ; better in the red band (0). They are distinguished by having the red
      ; band listed first on a line if it was judged better.
      ;---------------------------------------------------------------------

      IF (itype EQ 0) THEN BEGIN
         numndx0 = numndx
         rgn_name0 = STRARR(numndx)
         FOR irgn=0,numndx0-1 DO BEGIN
            rgn_name0[irgn] = temp_list[irgn].red_plume_name + '  ' + $
                              temp_list[irgn].blu_plume_name
         ENDFOR
      ENDIF
      
      ;---------------------------------------------------------------------
      ; Here to process all plumes the user decided to keep and judged
      ; better in the blue band (1). They are distinguished by having the
      ; blue band listed first on a line if it was judged better.
      ;---------------------------------------------------------------------

      IF (itype EQ 1) THEN BEGIN
         numndx1 = numndx
         rgn_name1 = STRARR(numndx)
         FOR irgn=0,numndx1-1 DO BEGIN
            rgn_name1[irgn] = temp_list[irgn].blu_plume_name + '  ' + $
                              temp_list[irgn].red_plume_name
         ENDFOR
      ENDIF

      ;---------------------------------------------------------------------
      ; Here to process all plumes the user decided to mark either for
      ; deletion (2) or redigitization (3).
      ;---------------------------------------------------------------------

      IF (itype GE 2) THEN BEGIN
         rgn_name = STRARR(numndx)
         FOR irgn=0,numndx-1 DO BEGIN
            IF (temp_list[irgn].red_plume_name NE '' AND $
               temp_list[irgn].blu_plume_name EQ '') THEN $
               rgn_name[irgn] = temp_list[irgn].red_plume_name
            IF (temp_list[irgn].red_plume_name EQ '' AND $
                temp_list[irgn].blu_plume_name NE '') THEN $
               rgn_name[irgn] = temp_list[irgn].blu_plume_name
            IF (temp_list[irgn].red_plume_name NE '' AND $
                temp_list[irgn].blu_plume_name NE '') THEN $
               rgn_name[irgn] = temp_list[irgn].red_plume_name + '  ' + $
                                temp_list[irgn].blu_plume_name
         ENDFOR
      ENDIF
      
      temp_list = 0
   ENDIF

   ;------------------------------------------------------------------------
   ; Don't process the "keep red band" data yet.
   ;------------------------------------------------------------------------
   
   IF (itype EQ 0) THEN CONTINUE
   
   ;---------------------------------------------------------------------
   ; The red and blue-band data are not processed further until getting
   ; here where they are concatenated to form the "Keep" pile.
   ;---------------------------------------------------------------------
   
   IF (itype EQ 1) THEN BEGIN
      numndx = numndx0 + numndx1
      IF (rgn_name0[0] NE '' AND rgn_name1[0] NE '') THEN BEGIN
         rgn_name = [rgn_name0, rgn_name1]
      ENDIF ELSE BEGIN
         IF (rgn_name0[0] NE '') THEN rgn_name = rgn_name0
         IF (rgn_name1[0] NE '') THEN rgn_name = rgn_name1
      ENDELSE
      rgn_name0 = 0
      rgn_name1 = 0
   ENDIF
   
   IF (itype GE 1 AND numndx GT 0) THEN rgn_name = rgn_name[SORT(rgn_name)]
   
   ;------------------------------------------------------------------------
   ; Print the plumes for this category to the results file.
   ;------------------------------------------------------------------------

   PRINTF, unit, header_name[ihdr_type]
   PRINTF, unit, '---------------------'
   FOR irgn=0,numndx-1 DO BEGIN
      PRINTF, unit, FORMAT='(A-36)', rgn_name[irgn]
   ENDFOR

   PRINTF, unit, ''
   rgn_name = 0
   ihdr_type += 1  
    
ENDFOR

FREE_LUN, unit
rtrn_val = ChmodCatchError(OutLogFile, '666'O)

ndxs = 0
header_name = 0

END  ;  PlumeReview_WriteResults

;***************************************************************************
PRO PlumeReview_ProcessPlumes, DirList, iDir, FileTextList, NumTextFile, $
                               UnitIn, OutEmergencyFile, nPlumeTotal, $
                               nPlumeRed, nPlumeBlue, nPPass, PlumeList, $
                               StatusCode, RegionStatus
;***************************************************************************
; Show relevant plume images and data to user so he can interactively and
; quickly rate the red .vs. blue preference or set to delete or redigitize.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
;---------------------------------------------------------------------------
; Create structure to contain user's red/blue preference and status.
;---------------------------------------------------------------------------

PlumeParm = { $
   red_file_name  : '', $
   blu_file_name  : '', $
   red_plume_name : '', $
   blu_plume_name : '', $
   status         : ''  $
}

file_done = INTARR(NumTextFile)

;------------------------------------------------------------------------
; Handle the Pass1 case where plume names are extracted from the project
; directory and there are multiple orbit subdirectories.
;------------------------------------------------------------------------

IF (iDir GE 0) THEN BEGIN
   dir_ndx = iDir
   dir_list = DirList
ENDIF

;---------------------------------------------------------------------------
; Loop over all the digitized regions.
;---------------------------------------------------------------------------

FOR ifile=0,NumTextFile-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Set a flag to indicate that this plume has been processed. If the plume
   ; is the red or blue version of one that has been processed, then skip it.
   ;------------------------------------------------------------------------
   
   IF (file_done[ifile]) THEN CONTINUE
   file_done[ifile] = 1
   
   PlumeReview_FillStruct, PlumeParm, FileTextList, ifile, file_done, $
                           band, this_plume, other_plume, nPlumeTotal, $
                           nPlumeRed, nPlumeBlue
   nPPass += 1
   IF (nPPass EQ 1) THEN RegionStatus = 1
   
   ;------------------------------------------------------------------------
   ; Handle the Pass2 case where plumes from several orbits are listed in a
   ; single file.
   ;------------------------------------------------------------------------
   
   IF (iDir EQ -1) THEN BEGIN
      npos = STRPOS(FileTextList[ifile], 'Plumes_')
      IF (npos LT 0) THEN CONTINUE
      dir_ndx = 0
      dir_list = [STRMID(FileTextList[ifile], 0, npos)]
   ENDIF
   
   PlumeReview_PlotImages, PlumeParm, dir_list, dir_ndx, band, this_plume, $
                           other_plume, StatusCode, RegionStatus
      
   ;------------------------------------------------------------------------
   ; Add the region's information to the emergency file.
   ;     StatusCode = ['Keep Red Region', 'Keep Blue Region', $
   ;                    'Delete Region', 'Redigitize Region']
   ;------------------------------------------------------------------------
   
   out_str = 'No files processed'
   IF (PlumeParm.status EQ StatusCode[0]) THEN $
      out_str = PlumeParm.red_plume_name + '  ' + PlumeParm.blu_plume_name
   IF (PlumeParm.status EQ StatusCode[1]) THEN $
      out_str = PlumeParm.blu_plume_name + '  ' + PlumeParm.red_plume_name
   IF (PlumeParm.status EQ StatusCode[2]) THEN $
      out_str = PlumeParm.red_plume_name + '  ' + PlumeParm.blu_plume_name + $
      '  ' + StatusCode[2]
   IF (PlumeParm.status EQ StatusCode[3]) THEN $
      out_str = PlumeParm.red_plume_name + '  ' + PlumeParm.blu_plume_name + $
      '  ' +StatusCode[3]
      
   OPENW, UnitIn, /APPEND, OutEmergencyFile, /GET_LUN
   PRINTF, UnitIn, out_str
   FREE_LUN, UnitIn
   
   ;------------------------------------------------------------------------
   ; Add this plume's name to the list of plumes. Then clear the structure.
   ;------------------------------------------------------------------------
   
   IF (nPPass EQ 1) THEN BEGIN
      PlumeList = PlumeParm
   ENDIF ELSE BEGIN
      PlumeList = [PlumeList, PlumeParm]
   ENDELSE
   
   ;------------------------------------------------------------------------
   ; Clear the structure. Then exit if user requested to quit before done.
   ;------------------------------------------------------------------------
   
   PlumeParm.red_file_name  = ''
   PlumeParm.blu_file_name  = ''
   PlumeParm.red_plume_name = ''
   PlumeParm.blu_plume_name = ''
   PlumeParm.status         = ''
   
   IF (RegionStatus EQ -1) THEN BREAK
   
ENDFOR

file_done = 0
FileTextList = 0
PlumeParm = 0

END  ;  PlumeReview_ProcessPlumes

;***************************************************************************
PRO PlumeReviewRegions_Pass1, DefaultProjDir
;***************************************************************************
; Program allows the user to inspect all the regions digitized by MINX for a
; project. Images for each region are shown, and user can decide which are
; good as is, which should be discarded and which should be marked for
; redigitizing or editing. Results are written to a log file. Program also
; posts a quality code (GOOD, FAIR, POOR) and other parameters to the screen
; if requested.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

status_code = ['Keep Red Region', 'Keep Blue Region', 'Delete Region', $
               'Redigitize Region']
   
;---------------------------------------------------------------------------
; Select directory of project to process and get list of orbit subdirectories.
;---------------------------------------------------------------------------

GetProjectDirNames, !KON.FileTyp.TypeRevuPlmDir, proj_dir, proj_name, $
                    dir_list, orbit_list, NumOrbit, retval
IF (retval LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Select the beginning orbit number to review.
;---------------------------------------------------------------------------

OrbitList_gui, orbit_list, OrbitNdx
IF (OrbitNdx EQ -1) THEN RETURN

;---------------------------------------------------------------------------
; Construct the name of the log file which is only written after processing
; has finished. Also create an emergency file to contain unsorted results in
; case MINX crashes before finishing and loses all the data in memory.
;---------------------------------------------------------------------------

out_log_file = proj_dir + proj_name + '_ReviewLog_Pass1_Beg' + $
               STRTRIM(STRING(OrbitNdx),2) + '.txt'

out_emergency_file = out_log_file + '.txt'

OPENW, unit, out_emergency_file, /GET_LUN
PRINTF, unit, 'Unsorted data in case Main file'
PRINTF, unit, 'is lost for plumes in directory:'
PRINTF, unit, proj_dir
PRINTF, unit, ' '
FREE_LUN, unit

;---------------------------------------------------------------------------
; Initialize counters.
;---------------------------------------------------------------------------

nppass = 0
nplume_tot = 0
nplume_red = 0
nplume_blu = 0

;---------------------------------------------------------------------------
; Loop over all the orbit subdirectories.
;---------------------------------------------------------------------------

FOR idir=OrbitNdx,NumOrbit-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Create the raw text file names for current orbit. If both and red and
   ; blue retrievals are present, get both. Sort the filenames just in case.
   ;------------------------------------------------------------------------

   file_text_template = dir_list[idir] + !KON.Misc.Slash + '*' + '.txt'
   file_text_list = FILE_SEARCH(file_text_template, /TEST_REGULAR, $
                                COUNT=num_text_file)

   IF (num_text_file EQ 0) THEN BEGIN
      mssg = ['There are no digitized regions in orbit subdirectory: ', $
              dir_list[idir]]
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
      CONTINUE
   ENDIF

   file_text_list = file_text_list[SORT(file_text_list)]

   ;------------------------------------------------------------------------
   ; Call function to do most of the work.
   ;------------------------------------------------------------------------
   
   PlumeReview_ProcessPlumes, dir_list, idir, file_text_list, num_text_file, $
                              unit, out_emergency_file, nplume_tot, $
                              nplume_red, nplume_blu, nppass, PlumeList, $
                              status_code, RegionStatus
   IF (RegionStatus EQ -1) THEN GOTO, end_early

ENDFOR

rtrn_val = ChmodCatchError(out_emergency_file, '666'O)

;---------------------------------------------------------------------------
; Separate the plumes into categories and write them to the log file. For
; plumes being kept as is, write them in red-blue or blue-red order with the
; preferred band listed first.
;---------------------------------------------------------------------------

end_early:

PlumeReview_WriteResults, '1', proj_dir, out_log_file, PlumeList, status_code

;---------------------------------------------------------------------------
; Write a summary of plumes processed to screen.
;---------------------------------------------------------------------------

title = '  SUMMARY OF PLUMES PROCESSED IN PROJECT DIRECTORY:'    
nlen1 = STRLEN(title)
proj2 = proj_dir
nlen2 = STRLEN(proj2)
FOR ispc=0,(nlen1-nlen2)/2 DO BEGIN
   proj2 = ' ' + proj2
ENDFOR

files = FILE_SEARCH(proj_dir, 'Plumes_*.txt')
nplume_os = N_ELEMENTS(files)

mssg = [title, proj2, '', $
        'Total number of plumes counted by operating sytem = ' + $
            STRING(FORMAT='(I4)', nplume_os), '', $
        'Total number of plumes counted during processing  = ' + $
            STRING(FORMAT='(I4)', nplume_tot), $
        'Number of "red" plumes counted during processing  = ' + $
            STRING(FORMAT='(I4)', nplume_red), $
        'Number of "blu" plumes counted during processing  = ' + $
            STRING(FORMAT='(I4)', nplume_blu)]
            
rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

files = 0
mssg = 0
dir_list = 0
status_code = 0
orbit_list = 0
PlumeList = 0

END  ;  PlumeReviewRegions_Pass1

;***************************************************************************
PRO FilterPlumesByMonth
;***************************************************************************
; Read a list of plume *.txt files generated by the "ls -l *.txt" command
; for a plume height project and separate and print the plumes into 2 files:
; one for plumes digitized by the original user and the other for plumes
; redigitized by the reviewer. This allows the redigitized plumes to be
; identified for another pass of deciding whether the red or blue band
; retrieval is better.
; NOTE - the full directory listing of plumes must have a clear difference
; in the months digitizing and redigitizing were performed. Modify this
; code as needed.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

infile= '/Users/<username>/Global2008_Apr_donePass1/plumelist.txt'
outfile1 = '/Users/<username>/Global2008_Apr_donePass1/plumelist_original.txt'
outfile2 = '/Users/<username>/Global2008_Apr_donePass1/plumelist_redig.txt'

max_plumes = 9999
redig_month = ' Nov '
add_dir = '/Users/<username>/Global2008_Apr_donePass1/'

;---------------------------------------------------------------------------

num_original = 0
num_redig = 0
buff = ''
OPENR, inunit, infile, /GET_LUN
OPENW, outunit1, outfile1, /GET_LUN
redig_list = STRARR(max_plumes)

WHILE ~EOF(inunit) DO BEGIN
   READF, inunit, buff
   npos = STRPOS(buff, redig_month)
   IF (npos LT 0) THEN BEGIN
      PRINTF, outunit1, buff
      num_original += 1
   ENDIF ELSE BEGIN
      toks = STRSPLIT(buff, ' ', /EXTRACT, COUNT=ntoks)
      redig_list[num_redig] = add_dir + toks[ntoks-1]
      num_redig += 1
   ENDELSE
ENDWHILE

CLOSE, inunit
CLOSE, outunit1

redig_list = redig_list[SORT(redig_list)]
OPENW, outunit2, outfile2, /GET_LUN
PRINTF, outunit2, redig_list
CLOSE, outunit2
   
END  ;  FilterPlumesByMonth

;***************************************************************************
PRO RereviewPlumes_Case1, DefaultProjDir
;***************************************************************************
; Case 1: The person who digitizing plumes is not the same as the person who
; reviews and rates the plumes at the end of a digitizing project by deciding
; whether "red" or "blue" plumes are better. This will be the case when you
; review plumes digitized by students, who are not allowed to review their
; own work. The reviewer inspects each plume using the MINX hidden utility
; PlumeReviewRegions_Pass1 (accessed from MINX option:
; "Review Digitized Plumes - Pass 1") which outputs a file listing each
; plume's name, one-per-line, with the ; preferred plume retrieval band
; listed first on the line of keepers.
;
; Case 2: The person digitizing is the same as the person reviewing plumes.
; This will happen when you are redigitizing plumes, in which case you are
; considered the expert. Now there is no need for a 2nd pass through the
; plumes for review, because you can rate the data for "red" or "blue"
; preferred as you go. This is ; accomplished by entering the word "red" or
; "blue" as the initial text in the "Describe exceptional features" textbox
; of the "Finished retrieval" dialog box that is displayed after each plume
; has been digitized.
;
; Case 1 methodology - part 1 (there may be problems with some of this code):
; Utility RereviewPlumes_Case1 (accessed from MINX option:
; "Case 1 - Find Plumes for Re-review") is the first of 2 utilities to
; handle Case 1. The program examines all the plume text files in a project
; after the 1st pass of review and subsequent deleting and redigitizing. It
; finds those files that need to be re-reviewed based on their age, i.e. they
; are newly digitized (this is crappy). The full directory listing of plumes
; must have a clear difference in the months digitizing andredigitizing were
; performed. The utility writes a list of the newly digitized plumes to a
; file so they can be processed in this next step.
;
; Part of the review process is for the reviewer to decide which band-retrieval
; is superior. Plumes that are re-digitized must go through the review process
; again. Results must be manually merged with results from the first pass.
; NOTE - the input list of plumes already reviewed and accepted must contain
; the line 'Regions To Delete' after the list of plumes reviewed which acts
; as a marker to stop input.
; NOTE - Unless the input list of plumes already reviewed is carefully
; controlled, this method is flawed. To check, use the procedure above, also
; potentially flawed (FilterPlumesByMonth). It is not currently called.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize variables.
;---------------------------------------------------------------------------

num_plume_ok = 0
num_plume_redo = 0
num_plume_total = 0

out_file_end = '_RereviewList.txt'
reviewed_plume_names = ''
ondisk_plume_name = ''
RedoList = ''
back_pos = 9
buff = ''

;---------------------------------------------------------------------------
; Select directory of project to process and get list of orbit subdirectories.
;---------------------------------------------------------------------------

GetProjectDirNames, !KON.FileTyp.TypeRevuPlmDir, proj_dir, proj_name, $
                    dir_list, orbit_list, NumOrbit, retval
IF (retval LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Get the name of the review file containing the names of plumes that have
; already been rated for red or blue preference. This list will be compared
; to plumes in the post Pass1 directory that may have been redigitized and
; no longer have a valid entry in the review file. 
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypePlumeRevu1, '*.txt', 1, file_outpath, $
                 Pass1ReviewFile
   
IF (Pass1ReviewFile EQ '') THEN RETURN

;---------------------------------------------------------------------------
; Read the names of all plumes in the red-blue preference file.
;---------------------------------------------------------------------------

num_file_plumes = 0
OPENR, in_unit, Pass1ReviewFile, /GET_LUN

WHILE ~EOF(in_unit) DO BEGIN
   READF, in_unit, buff
   IF (STRMID(buff, 0, 17) EQ 'Regions To Delete') THEN BREAK
   toks = STRSPLIT(buff, '- ', /EXTRACT, COUNT=numtoks)
   IF (numtoks NE 6) THEN CONTINUE
   reviewed_plume_names = [reviewed_plume_names, buff]
   num_file_plumes += 1
ENDWHILE

FREE_LUN, in_unit

IF (reviewed_plume_names[0] EQ '') THEN $
   reviewed_plume_names = reviewed_plume_names[1:num_file_plumes]

;---------------------------------------------------------------------------
; Remove the "R" or "B" component from the names and sort the list.
;---------------------------------------------------------------------------

FOR iplume=0,num_file_plumes-1 DO BEGIN
   toks = STRSPLIT(reviewed_plume_names[iplume], ' ', /EXTRACT, COUNT=numtoks)
   IF (numtoks NE 2) THEN CONTINUE
   plume_name = toks[0]
   nlen = STRLEN(plume_name)
   npos = STRPOS(plume_name, 'R', /REVERSE_SEARCH)
   IF (npos LT nlen - back_pos) THEN BEGIN
      npos = STRPOS(plume_name, 'B', /REVERSE_SEARCH)
      IF (npos LT nlen - back_pos) THEN BEGIN
         mssg = ['Neither a "R" or "B" code were found in plume:', plume_name, $
                 'STOPPING.']
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDIF
   STRPUT, plume_name, '?', npos
   reviewed_plume_names[iplume] = plume_name
ENDFOR

reviewed_plume_names = reviewed_plume_names[SORT(reviewed_plume_names)]

;---------------------------------------------------------------------------
; Loop over all the orbit subdirectories.
;---------------------------------------------------------------------------

FOR idir=0,NumOrbit-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Collect names of all the text files in this orbit subdirectory.
   ;------------------------------------------------------------------------

   file_text_template = dir_list[idir] + '/*' + '.txt'
   file_text_list = FILE_SEARCH(file_text_template, /TEST_REGULAR, $
                                COUNT=NumPlume)

   ;------------------------------------------------------------------------
   ; Loop over all the plume text files in the project directory and find
   ; whether they need to be re-reviewed.
   ;------------------------------------------------------------------------

   FOR ifile=0,NumPlume-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Extract the plume's name from its file name.
      ;---------------------------------------------------------------------
   
      ondisk_plume_name = file_text_list[ifile]
      toks = STRSPLIT(ondisk_plume_name, !KON.Misc.Slash, /EXTRACT, COUNT=numtoks)
      IF (numtoks LT 3) THEN RETURN
      ondisk_plume_name = toks[numtoks-1]
      toks = STRSPLIT(ondisk_plume_name, '_.', /EXTRACT, COUNT=numtoks)
      IF (numtoks NE 3) THEN RETURN
      ondisk_plume_name = toks[1]

      ;---------------------------------------------------------------------------
      ; Remove the "R" or "B" component from the name.
      ;---------------------------------------------------------------------------
   
      nlen = STRLEN(ondisk_plume_name)
      npos = STRPOS(ondisk_plume_name, 'R', /REVERSE_SEARCH)
      IF (npos LT nlen - back_pos) THEN BEGIN
         npos = STRPOS(ondisk_plume_name, 'B', /REVERSE_SEARCH)
         IF (npos LT nlen - back_pos) THEN BEGIN
            mssg = ['Neither a "R" or "B" code were found in plume:', $
                    ondisk_plume_name, 'STOPPING.']
            rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            RETURN
         ENDIF
      ENDIF
      STRPUT, ondisk_plume_name, '?', npos

      num_plume_total += 1

      ;---------------------------------------------------------------------
      ; If the plume is listed in the red-blue preference file, it's OK, so
      ; skip to the next plume.
      ;---------------------------------------------------------------------
      
      ndxs = WHERE(reviewed_plume_names EQ ondisk_plume_name, numndxs)
      
      IF (numndxs GE 1) THEN BEGIN
         num_plume_ok += 1
         CONTINUE
      ENDIF
      
      ;---------------------------------------------------------------------
      ; If the plume is not listed in the red-blue preference file, then add
      ; the file name to the list to be re-reviewed.
      ;---------------------------------------------------------------------
      
      num_plume_redo += 1
      RedoList = [RedoList, file_text_list[ifile]]
      
   ENDFOR
      
ENDFOR

IF (RedoList[0] EQ '') THEN RedoList = RedoList[1:num_plume_redo]

;---------------------------------------------------------------------------
; Test that the number of plumes counted is consistent.
;---------------------------------------------------------------------------

num_redo = N_ELEMENTS(RedoList)

IF (num_plume_redo + num_plume_ok NE num_plume_total) THEN BEGIN
   sum_plumes = STRTRIM(STRING(num_plume_redo + num_plume_ok),2)
   tot_plumes = STRTRIM(STRING(num_plume_total),2)
   mssg = ['The number of files OK plus the number to redo (' + $
           sum_plumes + ')', $
           'does not match the total number processed (' + $
           tot_plumes + '). STOPPING.']
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup0
ENDIF

IF (num_redo NE num_plume_redo) THEN BEGIN
   mssg = ['The number of files to redo does not match the', $
           'number collected during processing. STOPPING.']
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup0
ENDIF

;---------------------------------------------------------------------------
; Make a temporary copy of the list. Then replace "R" and "B" codes in each
; with the same dummy character (?) so sorting will place similar plumes
; side-by-side.
;---------------------------------------------------------------------------

RedoListTemp = RedoList
back_pos = 9

FOR iredo=0,num_plume_redo-1 DO BEGIN
   base = RedoListTemp[iredo]
   ;base = STRSPLIT(RedoListTemp[iredo], '_.', /EXTRACT)[0]
   nlen = STRLEN(base)
   npos = STRPOS(base, 'R', /REVERSE_SEARCH)
   IF (npos LT nlen - back_pos) THEN BEGIN
      npos = STRPOS(base, 'B', /REVERSE_SEARCH)
      IF (npos LT nlen - back_pos) THEN BEGIN
         mssg = ['Neither a "R" or "B" code were found in plume:', base, $
                 'STOPPING.']
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup0
      ENDIF
   ENDIF
   STRPUT, base, '?', npos
   RedoListTemp[iredo] = base
ENDFOR

;---------------------------------------------------------------------------
; Sort the plumes so that red and blue band names are contiguous. Then check
; that every plume has both red and blue versions as we go.
;---------------------------------------------------------------------------

sort_order = SORT(RedoListTemp)
RedoListTemp = RedoListTemp[sort_order]

FOR iredo=0,num_plume_redo-1,2 DO BEGIN
   IF (RedoListTemp[iredo+1] NE RedoListTemp[iredo]) THEN BEGIN
      mssg = ['Two plumes ("R" and "B") were not found for plume:', $
              RedoListTemp[iredo], 'STOPPING.']
      rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup0
   ENDIF
ENDFOR

RedoList = RedoList[sort_order]

;---------------------------------------------------------------------------
; Construct the name of the file to contain list of plumes to be re-reviewed
; and open it for writing. Then print the names to the output file.
;---------------------------------------------------------------------------

OPENW, out_unit, proj_dir + proj_name + out_file_end, /GET_LUN
PRINTF, out_unit, FORMAT='(A)', RedoList
FREE_LUN, out_unit

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

cleanup0:
ndxs = 0
dir_list = 0
orbit_list = 0
sort_order = 0
file_text_list = 0
reviewed_plume_names = 0
RedoListTemp = 0
RedoList = 0
   
END  ;  RereviewPlumes_Case1

;***************************************************************************
PRO PlumeReviewRegions_Pass2, DefaultProjDir
;***************************************************************************
; Case 1: The person who digitizing plumes is not the same as the person who
; reviews and rates the plumes at the end of a digitizing project by deciding
; whether "red" or "blue" plumes are better. This will be the case when you
; review plumes digitized by students, who are not allowed to review their
; own work. The reviewer inspects each plume using the MINX hidden utility
; PlumeReviewRegions_Pass1 (accessed from MINX option:
; "Review Digitized Plumes - Pass 1") which outputs a file listing each
; plume's name, one-per-line, with the ; preferred plume retrieval band
; listed first on the line of keepers.
;
; Case 2: The person digitizing is the same as the person reviewing plumes.
; This will happen when you are redigitizing plumes, in which case you are
; considered the expert. Now there is no need for a 2nd pass through the
; plumes for review, because you can rate the data for "red" or "blue"
; preferred as you go. This is ; accomplished by entering the word "red" or
; "blue" as the initial text in the "Describe exceptional features" textbox
; of the "Finished retrieval" dialog box that is displayed after each plume
; has been digitized.
;
; Case 1 methodology - part 2 (there may be problems with some of this code):
; ; The MINX utility PlumeReviewRegions_Pass2 (accessed from MINX option:
; “Case 1 - Re-review Digitized Plumes”) is the second of 2 procedures to
; handle Case 1. This function does the same thing as PlumeReviewRegions_Pass1,
; but rather than selecting plumes from all orbit subdirectories in a project
; directory, it reads the plume names from the file generated by utility
; RereviewPlumes_Case1. This is pass 2 in that plumes redigitized after pass
; 1 have not been evaluated to determine whether the red or blue band
; retrieval is better.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

status_code = ['Keep Red Region', 'Keep Blue Region', 'Delete Region', $
               'Redigitize Region']

;---------------------------------------------------------------------------
; Select the file from which to get a list of plumes that were redigitized
; and are not represented in the final review log with a red or blue better
; rating.
;---------------------------------------------------------------------------
            
GetLastFilename, 0, !KON.FileTyp.TypePlumeRerevu, '*.txt', 1, proj_dir, $
                 RereviewListFile
               
IF (RereviewListFile EQ '') THEN RETURN

toks = STRSPLIT(RereviewListFile, !KON.Misc.Slash, /EXTRACT, COUNT=numtoks)
IF (numtoks LT 3) THEN RETURN

proj_name = toks[numtoks-2]

;---------------------------------------------------------------------------
; Read the list of files into an array in memory.
;---------------------------------------------------------------------------

file_text_list = ['']

OPENR, list_unit, RereviewListFile, /GET_LUN

buff = ''
nFileGood = 0
nFileBad = 0

WHILE ~EOF(list_unit) DO BEGIN
   READF, list_unit, buff
   finfo = FILE_INFO(buff)
   IF (~finfo.EXISTS) THEN BEGIN
      nFileBad += 1
      CONTINUE
   ENDIF
   file_text_list = [file_text_list, buff]
   nFileGood += 1
ENDWHILE

FREE_LUN, list_unit

IF (nFileBad GT 0) THEN BEGIN
   mssg = [STRTRIM(STRING(nFileBad),2) + ' plume files do not exist.', $
           'Consider fixing the file and trying again.']
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
ENDIF

IF (N_ELEMENTS(file_text_list) EQ nFileGood + 1) THEN BEGIN
   file_text_list = file_text_list[1:nFileGood]
ENDIF ELSE BEGIN
   mssg = 'Wrong number of files read. Quitting.'
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDELSE

;---------------------------------------------------------------------------
; Construct the name of the log file which is only written after processing
; has finished. Also create an emergency file to contain unsorted results in
; case MINX crashes before finishing and loses all the data in memory.
;---------------------------------------------------------------------------

out_log_file = proj_dir + proj_name + '_ReviewLog_Pass2.txt'
out_emergency_file = out_log_file + '.txt'

OPENW, unit, out_emergency_file, /GET_LUN
PRINTF, unit, 'Unsorted data in case Main file'
PRINTF, unit, 'is lost for plumes in directory:'
PRINTF, unit, proj_dir
PRINTF, unit, ' '
FREE_LUN, unit

;---------------------------------------------------------------------------
; Initialize variables.
;---------------------------------------------------------------------------

nppass = 0
nplume_tot = 0
nplume_red = 0
nplume_blu = 0

idir = -1
dir_list = ''

file_text_list = file_text_list[SORT(file_text_list)] ; not very useful

;------------------------------------------------------------------------
; Call function to do most of the work.
;------------------------------------------------------------------------

PlumeReview_ProcessPlumes, dir_list, idir, file_text_list, nFileGood, $
                           unit, out_emergency_file, nplume_tot, $
                           nplume_red, nplume_blu, nppass, PlumeList, $
                           status_code, RegionStatus
                           
rtrn_val = ChmodCatchError(out_emergency_file, '666'O)
   
;---------------------------------------------------------------------------
; Separate the plumes into categories and write them to the log file. For
; plumes being kept as is, write them in red-blue or blue-red order with the
; preferred band listed first.
;---------------------------------------------------------------------------

PlumeReview_WriteResults, '2', proj_dir, out_log_file, PlumeList, status_code

;---------------------------------------------------------------------------
; Write a summary of plumes processed to screen.
;---------------------------------------------------------------------------

title = '  SUMMARY OF PLUMES PROCESSED IN PROJECT DIRECTORY:'
nlen1 = STRLEN(title)
proj2 = proj_dir
nlen2 = STRLEN(proj2)
FOR ispc=0,(nlen1-nlen2)/2 DO BEGIN
   proj2 = ' ' + proj2
ENDFOR

files = FILE_SEARCH(proj_dir, 'Plumes_*.txt')
nplume_os = N_ELEMENTS(files)

mssg = [title, proj2, '', $
   'Total number of plumes counted by operating sytem = ' + $
   STRING(FORMAT='(I4)', nplume_os), '', $
   'Total number of plumes counted during processing  = ' + $
   STRING(FORMAT='(I4)', nplume_tot), $
   'Number of "red" plumes counted during processing  = ' + $
   STRING(FORMAT='(I4)', nplume_red), $
   'Number of "blu" plumes counted during processing  = ' + $
   STRING(FORMAT='(I4)', nplume_blu)]
   
rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

files = 0
mssg = 0
dir_list = 0
status_code = 0
orbit_list = 0
PlumeList = 0

END  ;  PlumeReviewRegions_Pass2

;***************************************************************************
PRO RereviewPlumes_Case2, DefaultProjDir
;***************************************************************************
; Case 1: The person who digitizing plumes is not the same as the person who
; reviews and rates the plumes at the end of a digitizing project by deciding
; whether "red" or "blue" plumes are better. This will be the case when you
; review plumes digitized by students, who are not allowed to review their
; own work. The reviewer inspects each plume using the MINX hidden utility
; PlumeReviewRegions_Pass1 (accessed from MINX option:
; "Review Digitized Plumes - Pass 1") which outputs a file listing each
; plume's name, one-per-line, with the ; preferred plume retrieval band
; listed first on the line of keepers.
;
; Case 2: The person digitizing is the same as the person reviewing plumes.
; This will happen when you are redigitizing plumes, in which case you are
; considered the expert. Now there is no need for a 2nd pass through the
; plumes for review, because you can rate the data for "red" or "blue"
; preferred as you go. This is ; accomplished by entering the word "red" or
; "blue" as the initial text in the "Describe exceptional features" textbox
; of the "Finished retrieval" dialog box that is displayed after each plume
; has been digitized.
;
; Case 2 methodology (there may be problems with some of this code):
; The MINX utility RereviewPlumes_Case2 (accessed from MINX option:
; "Case 2 - Read Re-review Results") is the utility that handles Case 2. It
; reads all the MINX-generated raw text files for a project and extracts the
; "red" vs "blue" ratings from those files that were written when the plumes
; were re-digitized. This text can be found in the header line titled
; "Comments by digitizer". The data are written out in the same format as
; the file generated by MINX in Case 1, so processing can proceed identically
; from there. The user will need to manually concatenate this output with the
; output from PlumeReviewRegions_Pass1.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; The following lines need to be changed for new runs!
;---------------------------------------------------------------------------

plume_proj_dir_in = '/Users/<username>/PlumeProjects2013/Global2008_RedoAndRate_Nov/'
;plume_proj_dir_in = '/data/plume/WebResults/Global2008/Oct_Kris/'

review_file_out = 'PlumeRating_Nov_pass4.txt'
error_file_out  = 'PlumeRatingErrors_Nov_pass4.txt'

plume_tmplat_file = plume_proj_dir_in + '0*' + !KON.Misc.Slash + '*.txt'
review_path_out = plume_proj_dir_in + review_file_out
error_path_out = plume_proj_dir_in + error_file_out

num_skip = 39; line number of line containing "red" or "blue" rating

;---------------------------------------------------------------------------
; Collect the names of all the raw plume text files in the project. This
; will be double the number of plumes, because there is a "red" and a "blue"
; file for each.
;---------------------------------------------------------------------------

raw_txt_files = FILE_SEARCH(plume_tmplat_file, COUNT=num_txt_file)

;---------------------------------------------------------------------------
; Create an array to contain the plume name and its rating.
;---------------------------------------------------------------------------

rating_array = STRARR(2, num_txt_file)

;---------------------------------------------------------------------------
; Open the rating file and the error file for output.
;---------------------------------------------------------------------------

OPENW, unitout_review, review_path_out, /GET_LUN
OPENW, unitout_error, error_path_out, /GET_LUN

;---------------------------------------------------------------------------
; Read the "Describe exceptional features" entries in all the raw plume text
; files. Save the plume name and rating in an array.
;---------------------------------------------------------------------------

buff = ''
FOR ifile=0,num_txt_file-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Open the file.
   ;------------------------------------------------------------------------
   
   OPENR, unitin, raw_txt_files[ifile], /GET_LUN
   
   ;------------------------------------------------------------------------
   ; Find the line containing the rating and read it.
   ;------------------------------------------------------------------------
   
   FOR iskip=0,num_skip-1 DO BEGIN
      READF, unitin, buff
   ENDFOR
   
   pos = STRPOS(buff, ':')
   IF (pos LT 0) THEN BEGIN
      mssg = 'Could not find Comments line for plume: ' + raw_txt_files[ifile]
      PRINTF, unitout_error, mssg
      FREE_LUN, unitin
      CONTINUE
   ENDIF
   
   red_or_blue = STRMID(buff, pos+1, 6)
   red_or_blue = STRTRIM(red_or_blue, 2)
   red_or_blue = STRLOWCASE(red_or_blue)
   
   IF (red_or_blue NE 'red' AND red_or_blue NE 'blue') THEN BEGIN
      mssg = 'No "red" or "blue" in Comments line: ' + $
             raw_txt_files[ifile]
      PRINTF, unitout_error, mssg
      FREE_LUN, unitin
      CONTINUE
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Strip off the directory component of the path name and the "Plumes_"
   ; and ".txt" parts of the file name and store the plume name and rating
   ; in an array.
   ;------------------------------------------------------------------------
   
   pos = STRPOS(raw_txt_files[ifile], !KON.Misc.Slash, /REVERSE_SEARCH)
   IF (pos LT 0) THEN BEGIN
      mssg = 'Could not get plume name from file name: ' + $
             raw_txt_files[ifile]
      PRINTF, unitout_error, mssg
      FREE_LUN, unitin
      CONTINUE
   ENDIF
   
   plume_name = STRMID(raw_txt_files[ifile], pos+1)
   str_len = STRLEN(plume_name)
   plume_name = STRMID(plume_name, 7, str_len-11)
   
   rating_array[0,ifile] = plume_name + ' '
   rating_array[1,ifile] = red_or_blue
   
   ;------------------------------------------------------------------------
   ; Release the file.
   ;------------------------------------------------------------------------
   
   FREE_LUN, unitin

ENDFOR

;------------------------------------------------------------------------
; Separate the plumes into their red and blue pairs.
;------------------------------------------------------------------------

red_array = STRARR(2, num_txt_file/2)
blu_array = STRARR(2, num_txt_file/2)
red_ndx = 0
blu_ndx = 0

FOR iplume=0,num_txt_file-1 DO BEGIN
   pos = STRPOS(rating_array[0,iplume], '-', /REVERSE_SEARCH)
   IF (pos LT 0) THEN BEGIN
      mssg = '!!! Could not get plume best color from plume name: ' + $
         rating_array[0,iplume] + ' so just ignoring it.'
      PRINTF, unitout_error, mssg
      CONTINUE
   ENDIF
   color_code = STRMID(rating_array[0,iplume], pos+4, 1)
   
   IF (color_code EQ 'R') THEN BEGIN
      red_array[0,red_ndx] = rating_array[0,iplume]
      red_array[1,red_ndx] = rating_array[1,iplume]
      red_ndx += 1
   ENDIF ELSE BEGIN
      blu_array[0,blu_ndx] = rating_array[0,iplume]
      blu_array[1,blu_ndx] = rating_array[1,iplume]
      blu_ndx += 1
   ENDELSE
ENDFOR

;------------------------------------------------------------------------
; Make sure the plumes are all paired and in the same order. Give the
; blue plumes a red namd temporarily for the comparison.
;------------------------------------------------------------------------

num_bad_pair = 0

FOR iplume=0,num_txt_file/2-1 DO BEGIN
   red_pl = red_array[0,iplume]
   blu_pl = blu_array[0,iplume]
   
   pos = STRPOS(blu_pl, '-', /REVERSE_SEARCH)
   STRPUT, blu_pl, 'R', pos+4
   
   IF (red_pl NE blu_pl) THEN BEGIN
      mssg = '!!! Ignoring plume since not in same order: ' + red_pl
      PRINTF, unitout_error, mssg
      num_bad_pair += 1
      CONTINUE
   ENDIF
ENDFOR

PRINTF, unitout_error, '# of unpaired plumes = ' + $
        STRTRIM(STRING(num_bad_pair),2)
   
;------------------------------------------------------------------------
; If all the plumes are in the correct order, then write them to file.
;------------------------------------------------------------------------

IF (num_bad_pair EQ 0) THEN BEGIN
   num_red_best = 0
   num_blue_best = 0
   num_other_best = 0
   
   FOR iplume=0,num_txt_file/2-1 DO BEGIN
      IF (red_array[1,iplume] EQ 'red') THEN BEGIN
         PRINTF, unitout_review, red_array[0,iplume], ' ', blu_array[0,iplume]
         num_red_best += 1
      ENDIF
      IF (blu_array[1,iplume] EQ 'blue') THEN BEGIN
         PRINTF, unitout_review, blu_array[0,iplume], ' ', red_array[0,iplume]
         num_blue_best += 1
      ENDIF
      IF (red_array[1,iplume] NE 'red' AND $
          red_array[1,iplume] NE 'blue') THEN BEGIN
         PRINTF, unitout_review, 'NEITHER  ', blu_array[0,iplume], ' ', $
                 red_array[0,iplume]
         num_other_best += 1
      ENDIF
   ENDFOR
   
   PRINTF, unitout_error, '# of red   plumes best = ' + $
           STRTRIM(STRING(num_red_best),2)
   PRINTF, unitout_error, '# of blue  plumes best = ' + $
           STRTRIM(STRING(num_blue_best),2)
   PRINTF, unitout_error, '# of other plumes best = ' + $
           STRTRIM(STRING(num_other_best),2)
ENDIF

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

FREE_LUN, unitout_review
FREE_LUN, unitout_error
   
END  ;  RereviewPlumes_Case2

;***************************************************************************
PRO VerifyPlumesAfterReview, DefaultProjDir
;***************************************************************************
; POST-REVIEW PROCEDURE, STEP 1. (only tested on OS X)
; After all plumes for a project area have been digitized, reviewed and
; redigitized, and prior to performing the other post-processing steps, you
; must manually prepare a file as described below.
; 
; In the following, <Month> is a placeholder for the first 3-letters of the
; month being processed, e.g. Jan. Also the last 3 characters of the project
; directory name must be the first 3 characters of the month being processed,
; e.g. Global2008-Sep. This rule, hard-wired in code, helps enforce the
; organization of future smoke plume projects to be one month at a time
; globally to prevent them from getting out of hand again.
; 
; From the file(s) output by "PlumeReviewRegions_Pass..", manually construct
; a single file named "PlumeRating_<Month>.txt" in the project directory
; that contains all successful plumes extracted from the "Regions To Keep"
; sections of the PlumeReviewRegions output ("<Month>_ReviewLog_Pass..").
; This file will contain a pair of plume names on each line, they are the
; red-band and blue-band names for each successfully retrieved plume. The
; name that is listed first on each line is the one that was rated better
; during the plume review process.
;   
; This procedure:
;  1) Creates a list of all successful plumes for all orbits in the project
;     directory; the list could be manually generated and saved to file using
;     the unix command "ls 0*/*.txt > PlumeList_<Month>.txt".
;  2) Reads the contents of the "PlumeRating_<Month>.txt" file by the user
;     as described above.
;  3) Sorts the two lists identically.
;  4) Tests if the correct number of .png, .tif, .jpg and .mp4 files are
;     present in the directory, given the number of plume text files.
;  5) Tests whether any MP4 files are missing and are replaced by 9 files
;     whose names contain the text ...Animation<cam>.jpg as a consequence of
;     the user not having a MPG license from Exelis. In this case, procedure
;     "ConvertJPGsToMP4" must be run against all cases of the 9 .jpg files to
;     produce a single .mp4 file for each set.
;  6) Tests if the lists contain the identical plumes. Because of the manual
;     steps preceding this, there are often differences that must be resolved.
; Any error uncovered here must be corrected and the procedure rerun until
; there are no problems remaining.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
NFILE_PER_PLUME = 11

max_plumes = 9999
num_plumes_list = 0
num_plumes_rating = 0
num_errors = 0
buff = ''
       
;---------------------------------------------------------------------------
; Get the name of the project directory to process from the user and get
; the month from the last 3 characters of directory name. Make sure the user
; approves.
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypePrjVerifyDir, '*', 0, ProjDir, dummy

mnth = STRMID(ProjDir, 3, 3, /REVERSE_OFFSET)
ndx = WHERE(STRUPCASE(mnth) EQ STRUPCASE(months), numndx)

IF (numndx NE 1) THEN BEGIN
   mssg = ['The project directory name must end with the first 3', $
           'characters of the month in which the plumes occurred.', $
           'Add the month to the end of the directory name and retry.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Construct the names of standard input and output files for comparing the
; lists of .txt files in the project directory with the list of red- vs
; blue-band preferences.
;---------------------------------------------------------------------------

log_file = 'VerifyLog_'  + mnth + '.txt'
infile  = ProjDir + !KON.Misc.Slash + 'PlumeRating_' + mnth + '.txt'
outfile = ProjDir + !KON.Misc.Slash + log_file

;---------------------------------------------------------------------------
; Create a persistent message to user that processing may take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
   TITLE='Testing plume file names .....', $
   XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
   YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ', $
   '  MINX is testing the consistency of the number of .txt, .png, .jpg    ', $
   '  and .mp4 file types in your project directory. When both red- and', $
   '  blue-band heights are retrieved, there should be ' + $
      STRTRIM(STRING(NFILE_PER_PLUME),2) + ' files saved', $
   '  for each plume. ...............................................', ' ']
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=67, YSIZE=5)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Create an array containing the list of every raw plume .txt file name and
; count the plume files. Red- and blue-band plumes are separate elements.
;---------------------------------------------------------------------------

SPAWN, 'cd ' + ProjDir + '; ls 0* | grep "0*/*.txt$"', PlumeListAry
num_plumes_list = N_ELEMENTS(PlumeListAry)

;---------------------------------------------------------------------------
; Test if the correct number of .png, .jpg and .mp4 files are present in the
; directory, given the number of plume text files.
;---------------------------------------------------------------------------

SPAWN, 'cd ' + ProjDir + '; ls 0* | grep "0*/*.png$"', png_ary
num_png = N_ELEMENTS(png_ary)

SPAWN, 'cd ' + ProjDir + '; ls 0* | grep "0*/*.jpg$"', jpg_ary
num_jpg = N_ELEMENTS(jpg_ary)

SPAWN, 'cd ' + ProjDir + '; ls 0* | grep "0*/*.mp4$"', mp4_ary
num_mp4 = N_ELEMENTS(mp4_ary)

WIDGET_CONTROL, baseID, /DESTROY

;---------------------------------------------------------------------------
; Compare the number of files and report to user.
;---------------------------------------------------------------------------

OPENW, outunit, outfile, /GET_LUN

PRINTF, outunit, ''
PRINTF, outunit, 'File-type Count for Project: '
PRINTF, outunit, ' ' + ProjDir
PRINTF, outunit, ' (For each file type, the Actual Number of files must equal'
PRINTF, outunit, '  the Expected Number, and every Number must be an integer.)'
PRINTF, outunit, 'File Type   Expected Number   Actual Number'
PRINTF, outunit, '---------   ---------------   -------------'
PRINTF, outunit, '  .txt         ' + STRING(FORMAT='(F7.1)', num_plumes_list) + $
                 '          ' + STRING(FORMAT='(F7.1)', num_plumes_list)
PRINTF, outunit, '  .png         ' + STRING(FORMAT='(F7.1)', num_plumes_list*2.5) + $
                 '          ' + STRING(FORMAT='(F7.1)', num_png)
PRINTF, outunit, '  .jpg         ' + STRING(FORMAT='(F7.1)', num_plumes_list*1.5) + $
                 '          ' + STRING(FORMAT='(F7.1)', num_jpg)
PRINTF, outunit, '  .mp4         ' + STRING(FORMAT='(F7.1)', num_plumes_list*0.5) + $
                 '          ' + STRING(FORMAT='(F7.1)', num_mp4), ''
     
IF (num_png NE num_plumes_list*2.5 OR num_jpg NE num_plumes_list*1.5 OR $
    num_mp4 NE num_plumes_list*0.5) THEN num_errors += 1

;---------------------------------------------------------------------------
; Strip off characters from file names to leave only the plume name.
;---------------------------------------------------------------------------

FOR iplume=0,num_plumes_list-1 DO BEGIN
   toks = STRSPLIT(PlumeListAry[iplume], '_.', /EXTRACT, COUNT=ntoks)
   IF (ntoks EQ 3) THEN PlumeListAry[iplume] = toks[1]
ENDFOR

;---------------------------------------------------------------------------
; Create a persistent message to user that processing may take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
                     TITLE='Testing plume file names .....', $
                     XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
                     YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ', $
   '  MINX is testing the consistency of the number of files associated  ', $
   '  with each plume. ..............................................', ' ']

textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=67, YSIZE=5)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Replace the B and D band indicators with'?' to remove redundancies and
; find the unique plume (W) or cloud (N) name. 
;---------------------------------------------------------------------------

temp_plume_list = PlumeListAry
FOR iplume=0,num_plumes_list-1 DO BEGIN
   ipos = STRPOS(temp_plume_list[iplume], 'SPWR', /REVERSE_SEARCH)
   plume_str = temp_plume_list[iplume]
   IF (ipos GT 0) THEN STRPUT, plume_str, 'SPW?', ipos
   ipos = STRPOS(temp_plume_list[iplume], 'SPWB', /REVERSE_SEARCH)
   IF (ipos GT 0) THEN STRPUT, plume_str, 'SPW?', ipos
   ipos = STRPOS(temp_plume_list[iplume], 'SPNR', /REVERSE_SEARCH)
   IF (ipos GT 0) THEN STRPUT, plume_str, 'SPN?', ipos
   ipos = STRPOS(temp_plume_list[iplume], 'SPNB', /REVERSE_SEARCH)
   IF (ipos GT 0) THEN STRPUT, plume_str, 'SPN?', ipos
   temp_plume_list[iplume] = plume_str
ENDFOR

temp_plume_list = temp_plume_list[UNIQ(temp_plume_list, $
                                       SORT(temp_plume_list))]
                    
;---------------------------------------------------------------------------
; Test every plume to see if it has the correct number of files.
;---------------------------------------------------------------------------
                                       
PRINTF, outunit, ''
PRINTF, outunit, 'Errors in per-Plume File Count:'
PRINTF, outunit, '-------------------------------'

num_errors_x = 0

FOR iplume=0,N_ELEMENTS(temp_plume_list)-1 DO BEGIN
   subdir = STRMID(temp_plume_list[iplume], 1, 5)
   IF (STRMID(subdir, STRLEN(subdir)-1, 1) EQ '-') THEN $
      subdir = '0' + STRMID(subdir, 0, 4)
   subdir = '0' + subdir
   SPAWN, 'cd ' + ProjDir + '; ls -1 ' + subdir + !KON.Misc.Slash + $
          '*' + temp_plume_list[iplume] + '_* | wc -l', num_files1
   SPAWN, 'cd ' + ProjDir + '; ls -1 ' + subdir + !KON.Misc.Slash + $
          '*' + temp_plume_list[iplume] + '.* | wc -l', num_files2
   num_files = FIX(num_files1) + FIX(num_files2)
   IF (num_files NE NFILE_PER_PLUME) THEN BEGIN
      PRINTF, outunit, 'Error - wrong number of files for plume ' + $
              temp_plume_list[iplume] + ' (' + $
              STRTRIM(STRING(num_files),2) + ')'
      num_errors_x += 1
   ENDIF
ENDFOR

IF (num_errors_x EQ 0) THEN PRINTF, outunit, 'None'
num_errors += num_errors_x

temp_plume_list = 0
WIDGET_CONTROL, baseID, /DESTROY

;---------------------------------------------------------------------------
; Create a persistent message to user that processing may take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
   TITLE='Testing orbit file names .....', $
   XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
   YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ', $
   '  MINX is testing the consistency of the number of files associated  ', $
   '  with each orbit subdirectory. ...................................', ' ']
   
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=67, YSIZE=5)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Test every orbit subdirectory to see if it has correct number of files
; (= num plumes * NFILE_PER_PLUME).
;---------------------------------------------------------------------------

SPAWN, 'cd ' + ProjDir + '; ls -d 0*', orbit_list

PRINTF, outunit, ''
PRINTF, outunit, 'Errors in per-Orbit File Count:'
PRINTF, outunit, '-------------------------------'

num_errors_x = 0

FOR iorbit=0,N_ELEMENTS(orbit_list)-1 DO BEGIN
   SPAWN, 'cd ' + ProjDir + !KON.Misc.Slash + orbit_list[iorbit] + $
          '; ls', file_list
   num_orbit_files = N_ELEMENTS(file_list)

   IF (num_orbit_files / FLOAT(NFILE_PER_PLUME) NE $
       num_orbit_files / NFILE_PER_PLUME) THEN BEGIN
      PRINTF, outunit, 'Error - wrong number of files for orbit ' + $
              orbit_list[iorbit] + ' (' + $
              STRTRIM(STRING(num_orbit_files),2) + ')'
      num_errors_x += 1
   ENDIF
ENDFOR

IF (num_errors_x EQ 0) THEN PRINTF, outunit, 'None'
num_errors += num_errors_x

orbit_list = 0
png_ary = 0
jpg_ary = 0
mp4_ary = 0
WIDGET_CONTROL, baseID, /DESTROY

;---------------------------------------------------------------------------
; Create a message to user that processing will take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
   TITLE='Testing red- vs blue-band rating file .....', $
   XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
   YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ', $
   '  MINX is comparing the contents of file: PlumeRating_' + mnth + '.txt', $
   '  with the list of plume .txt files in your project directory to', $
   '  see if they are consistent ...................................', ' ']
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=67, YSIZE=5)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Test if the specified input red- vs blue-band PlumeRating file exists.
;---------------------------------------------------------------------------

IF (~FILE_TEST(infile)) THEN BEGIN
   mssg = ['The file containing red-band and blue-band preference ratings ' + $
           'and named ', infile, $
           'does not exist. Correct the problem and try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   WIDGET_CONTROL, baseID, /DESTROY
   FREE_LUN, outunit
   RETURN
ENDIF

;------------------------------------------------------------------------
; Read PlumeRating_<month>.txt file and store in an array. Red- and blue-
; band plume names are stored in pairs, so there should be half as many
; entries here as in the plume list array. This test is necessary, because
; there are manual steps involved in the review process (deleting and
; redigitizing plumes) that can screw up the PlumeRating file list.
;------------------------------------------------------------------------

OPENR, inunit, infile, /GET_LUN
PlumeRatingAry = STRARR(max_plumes)

PRINTF, outunit, ''
PRINTF, outunit, 'Errors Reading PlumeRating Entries:'
PRINTF, outunit, '-----------------------------------'

num_errors_x = 0

num_lines = 0
WHILE ~EOF(inunit) DO BEGIN
   READF, inunit, buff
   num_lines += 1
   toks = STRSPLIT(buff, ' ', /EXTRACT, COUNT=ntoks)
   IF (ntoks NE 2)  THEN BEGIN
      PRINTF, outunit, 'Error - 2 plume names required on line ' + $
              STRING(FORMAT='(I5)',num_lines) + ': ' + buff
      num_errors_x += 1
      CONTINUE
   ENDIF
   toks1 = STRSPLIT(buff, '- ', /EXTRACT, COUNT=ntoks1)
   IF (ntoks1 NE 6)  THEN BEGIN
      PRINTF, outunit, 'Error - plume naming problem on line   ' + $
              STRING(FORMAT='(I5)',num_lines) + ': ' + buff
      num_errors_x += 1
      CONTINUE
   ENDIF
   PlumeRatingAry[num_plumes_rating] = toks[0]
   num_plumes_rating += 1
   PlumeRatingAry[num_plumes_rating] = toks[1]
   num_plumes_rating += 1
ENDWHILE

IF (num_errors_x EQ 0) THEN PRINTF, outunit, 'None'
num_errors += num_errors_x

PlumeRatingAry = PlumeRatingAry[0:num_plumes_rating-1]
FREE_LUN, inunit

;------------------------------------------------------------------------
; Sort the plume names in each list.
;------------------------------------------------------------------------

PlumeListAry = PlumeListAry[SORT(PlumeListAry)]
PlumeRatingAry = PlumeRatingAry[SORT(PlumeRatingAry)]

;------------------------------------------------------------------------
; Compare the lists and output plume file names where there are anomalies.
;------------------------------------------------------------------------

PRINTF, outunit, ''
PRINTF, outunit, 'Errors in PlumeList vs PlumeRating Entries:'
PRINTF, outunit, 'PlumeList entry     PlumeRating entry'
PRINTF, outunit, '---------------     -----------------'

num_errors_x = 0

jj = 0
kk = 0
WHILE (jj LT num_plumes_list AND kk LT num_plumes_rating) DO BEGIN
   is_diff = (PlumeListAry[jj] NE PlumeRatingAry[kk])
           
   IF (is_diff) THEN BEGIN
      PRINTF, outunit, FORMAT='(2(A-18,2X),2X,A7)', PlumeListAry[jj], $
              PlumeRatingAry[kk]
      num_errors_x += 1
      IF (PlumeListAry[jj] LT PlumeRatingAry[kk]) THEN BEGIN
         jj += 1
      ENDIF ELSE BEGIN
         kk += 1
      ENDELSE
   ENDIF ELSE BEGIN
      jj += 1
      kk += 1
   ENDELSE
ENDWHILE

IF (num_errors_x EQ 0) THEN PRINTF, outunit, 'None'
num_errors += num_errors_x

FREE_LUN, outunit

WIDGET_CONTROL, baseID, /DESTROY

;------------------------------------------------------------------------
; Notify user of the errors and where to find the log file.
;------------------------------------------------------------------------

IF (num_errors GT 0) THEN BEGIN
   mssg = ['MINX detected ' + STRTRIM(STRING(num_errors),2) + ' errors in ' + $
           'project directory: ', ProjDir, 'Inspect error file: ', outfile, $
           'to see error locations. Correct any problems and rerun this', $
           'procedure until no problems remain.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
ENDIF ELSE BEGIN
   mssg = ['No errors were found in project directory: ', ProjDir]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
ENDELSE

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

PlumeListAry = 0
PlumeRatingAry = 0

END  ;  VerifyPlumesAfterReview
