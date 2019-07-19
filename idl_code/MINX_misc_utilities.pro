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
PRO SafeWSET, WndwNdx, DidIt
;***************************************************************************
; Set the window corresponding to the passed index to be active only if it
; exists. If it doesn't exist, set the index to -1.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2

DidIt = 0
DEVICE, WINDOW_STATE=WndwNdxs

IF (WndwNdx GE 0 AND WndwNdx LT N_ELEMENTS(WndwNdxs)) THEN BEGIN
   IF (WndwNdxs[WndwNdx]) THEN BEGIN
      WSET, WndwNdx
      DidIt = 1
   ENDIF ELSE BEGIN
      WndwNdx = -1
   ENDELSE
ENDIF ELSE BEGIN
   DidIt = -1
ENDELSE

WndwNdxs = 0

END  ;  SafeWSET

;***************************************************************************
PRO SafeWSHOW, WndwNdx, Show, Iconify, DidIt
;***************************************************************************
; Bring the window corresponding to the passed index to the top of the
; display stack only if it exists. If it doesn't exist, set the index to -1.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

DidIt = 0
DEVICE, WINDOW_STATE=WndwNdxs

IF (WndwNdx GE 0 AND WndwNdx LT N_ELEMENTS(WndwNdxs)) THEN BEGIN
   IF (WndwNdxs[WndwNdx]) THEN BEGIN
      WSHOW, WndwNdx, Show, ICONIC=Iconify
      DidIt = 1
   ENDIF ELSE BEGIN
      WndwNdx = -1
   ENDELSE
ENDIF ELSE BEGIN
   DidIt = -1
ENDELSE

WndwNdxs = 0
   
END  ;  SafeWSHOW

;***************************************************************************
PRO SafeWDELETE, WndwNdx, DidIt
;***************************************************************************
; Delete the window corresponding to the passed index if the index is >= 0
; and it exists. If the passed index is less than 0, then the intent is to
; delete an orphaned window caused by the user clicking the red Close Window
; button in the top left of a graphics window (this is not handled well by
; IDL in old-style graphics windows created by the Window procedure - these
; should be converted to the new Window function). So we assume there is
; only one window in existence - find it using the WINDOW_STATE parameter
; and delete it. Finally, set the window index to -1.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2
   
DidIt = 0
DEVICE, WINDOW_STATE=WndwNdxs

IF (WndwNdx LT N_ELEMENTS(WndwNdxs)) THEN BEGIN
   IF (WndwNdx GE 0) THEN BEGIN
      IF (WndwNdxs[WndwNdx]) THEN BEGIN
         WDELETE, WndwNdx
         DidIt = 1
      ENDIF ELSE BEGIN
      ENDELSE
   ENDIF ELSE BEGIN
      ndxs = WHERE(WndwNdxs NE 0, numndxs)
      IF (numndxs EQ 1) THEN BEGIN
         WDELETE, WndwNdxs[ndxs[0]]
         DidIt = 1
      ENDIF ELSE BEGIN
         DidIt = -1
      ENDELSE
   ENDELSE
ENDIF

WndwNdx = -1
WndwNdxs = 0
   
END  ;  SafeWDELETE

;***************************************************************************
FUNCTION MakeDirectory, DirectoryName
;***************************************************************************
; This function creates a new directory. It sets an error trap in case the
; user specifies an illegal directory name.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set a trap for errors.
;---------------------------------------------------------------------------

CATCH, ErrorStatus

IF (ErrorStatus NE 0) THEN BEGIN  
   CATCH, /CANCEL  
   RETURN, -1
ENDIF

;---------------------------------------------------------------------------
; Attempt to create the new directory.
;---------------------------------------------------------------------------

FILE_MKDIR, DirectoryName

;---------------------------------------------------------------------------
; Cancel the error catcher and return the status.
;---------------------------------------------------------------------------

CATCH, /CANCEL  

RETURN, 0

END  ;  MakeDirectory

;***************************************************************************
FUNCTION ChmodCatchError, FileDirObj, Mode
;***************************************************************************
; Change the permissions on a file or directory and catch errors if any.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set a trap for errors.
;---------------------------------------------------------------------------

CATCH, ErrorStatus
   
IF (ErrorStatus NE 0) THEN BEGIN  
   mssg = ['Error attempting to change permissions on : ', $
           FileDirObj, 'Processing will continue.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL  
   RETURN, -1
ENDIF  

;---------------------------------------------------------------------------
; Attempt to change permissions on the file or directory.
;---------------------------------------------------------------------------

FILE_CHMOD, FileDirObj, Mode

;---------------------------------------------------------------------------
; Cancel the error catcher and return the status.
;---------------------------------------------------------------------------

CATCH, /CANCEL  

RETURN, 0

END  ;  ChmodCatchError
   
;****************************************************************
PRO CopyFile, Unit, PathName, DestDir, Status
;****************************************************************
; Copy a file from the source to the destination. Catch errors
; and keep going.
;---------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = 0
error_status = 0
CATCH, error_status

IF (error_status NE 0) THEN BEGIN
   CATCH, /CANCEL
   mssg = ['File could not be copied from', PathName + ' to', DestDir, $
           'Continue processing?']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /QUESTION)
   IF (STRUPCASE(rtrn) EQ 'NO') THEN Status = -1
   RETURN
ENDIF

FILE_COPY, PathName, DestDir

CATCH, /CANCEL

END  ;  CopyFile


;***************************************************************************
PRO DeleteRegionFiles, NumBand, RgnName
;***************************************************************************
; Delete from disk the files with the passed region name.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_main_parms, MainOption

;---------------------------------------------------------------------------
; Get the name of the directory where the files are located and construct
; the template, with wildcards, for the files to delete.
;---------------------------------------------------------------------------

ipos = [STRPOS(RgnName[0], '-'), STRPOS(RgnName[1], '-')]
ndx = (ipos[0] GT 0) ? 0 : 1
orbit_number = LONG(STRMID(RgnName[ndx], 1, ipos[ndx]-1))

CreateOrbitDirFile, !SAV.Digitize.PlumeOutDir, '', '', 0, '', orbit_number, $
                    0, tempdir, new_file, Retval

IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN RETURN

tempfile1 = tempdir + '*' + RgnName[ndx] + '*'
IF (NumBand EQ 2) THEN tempfile2 = tempdir + '*' + RgnName[1] + '*'

;---------------------------------------------------------------------------
; Delete all files in the directory that satisfy the template name.
;---------------------------------------------------------------------------

file_list = FILE_SEARCH(tempfile1)
ndxs = WHERE(file_list NE '', num_ndxs)
IF (num_ndxs GT 0) THEN FILE_DELETE, file_list[ndxs]

IF (NumBand EQ 2) THEN BEGIN
   file_list = FILE_SEARCH(tempfile2)
   ndxs = WHERE(file_list NE '', num_ndxs)
   IF (num_ndxs GT 0) THEN FILE_DELETE, file_list[ndxs]
ENDIF

ndx = 0
ndxs = 0
ipos = 0

END  ;  DeleteRegionFiles
;***************************************************************************
FUNCTION Is_Date, strvalu
;***************************************************************************
; This function tests whether a string contains a date in MM/DD/YR format.
; If it does it returns the 3 components in an array, else it returns -1 in
; the first array element.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   mmddyr = INTARR(3)
   mmddyr[0] = -1

   ;------------------------------------------------------------------------
   ; find string length and loop over all characters
   ;------------------------------------------------------------------------

   nchar = STRLEN(strvalu)
   chars = BYTE(strvalu)

   IF (chars[2] NE 47 OR chars[5] NE 47) THEN RETURN, mmddyr

   IF (chars[0] LT 48 OR chars[0] GT 49) THEN RETURN, mmddyr
   IF (chars[1] LT 48 OR chars[1] GT 57) THEN RETURN, mmddyr

   IF (chars[3] LT 48 OR chars[3] GT 51) THEN RETURN, mmddyr
   IF (chars[4] LT 48 OR chars[4] GT 57) THEN RETURN, mmddyr

   IF (chars[6] LT 48 OR chars[6] GT 49) THEN RETURN, mmddyr
   IF (chars[7] LT 48 OR chars[7] GT 57) THEN RETURN, mmddyr

   mmddyr[0] = FIX(STRING(chars[0:1]))
   mmddyr[1] = FIX(STRING(chars[3:4]))
   mmddyr[2] = FIX(STRING(chars[6:7])) + 2000

   RETURN, mmddyr

END  ;  Is_Date

;***************************************************************************
FUNCTION IsNumber, strvalu, inumtype
;***************************************************************************
; This function tests whether a string contains only valid numeric data.
; It returns 1 if string is a valid number, and returns 0 otherwise.
; inumtype = 1 -> integer; inumtype = 2 -> float
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

   ;------------------------------------------------------------------------
   ; find string length and loop over all characters
   ;------------------------------------------------------------------------

   strvalu = STRTRIM(strvalu, 2)
   nchar = STRLEN(strvalu)
   chars = BYTE(strvalu)

   IF (inumtype EQ 1) THEN BEGIN          ; integer test

      IF (chars[0] LT 48 AND chars[0] NE 43 AND chars[0] NE 45) THEN RETURN, 0
      IF (chars[0] GT 57) THEN RETURN, 0

      FOR ichar = 1, nchar-1 DO BEGIN
         IF (chars[ichar] LT 48 OR chars[ichar] GT 57) THEN RETURN, 0
      ENDFOR

   ENDIF

   IF (inumtype EQ 2) THEN BEGIN          ; float test

      IF (chars[0] LT 48 AND chars[0] NE 46 AND $
          chars[0] NE 43 AND chars[0] NE 45) THEN RETURN, 0
      IF (chars[0] GT 57) THEN RETURN, 0

      FOR ichar = 1, nchar-1 DO BEGIN
         IF ((chars[ichar] LT 48 OR chars[ichar] GT 57) AND $
              chars[ichar] NE 46) THEN RETURN, 0
      ENDFOR

   ENDIF

   RETURN, 1

END  ;  IsNumber

;***************************************************************************
PRO ReadNextAsciiLine, Unit, BegMidEnd, LineStr
;***************************************************************************
; Read the next line from an ASCII file that is already open and handle the
; problem of line termination. Windows terminates with "carriage return" +
; "line feed" while Unix and OSX use "line feed". This handles "two-way"
; conversions.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize some parameters and variables.
;---------------------------------------------------------------------------

carr_rtrn = '0d'XB
line_feed = '0a'XB
INPUT_IS_UNIX = 1
INPUT_IS_WINDOWS = 2
!ERROR_STATE.CODE = 0
buff = ''

;---------------------------------------------------------------------------
; If there are no more lines to read, jump to the bottom and return the End
; code to caller. Otherwise, read the next line in the file as a string.
; Then reset the file pointer to the beginning of the line just read. 
;---------------------------------------------------------------------------

IF EOF(Unit) THEN GOTO, done_file

POINT_LUN, -Unit, beg_pos
READF, Unit, buff
POINT_LUN, Unit, beg_pos

;---------------------------------------------------------------------------
; Get the string length, add 2 to contain possible CR and NL, and create a
; byte array of that length.
;---------------------------------------------------------------------------

byt_len = STRLEN(buff) + 2
byt_ary = BYTARR(byt_len)

;---------------------------------------------------------------------------
; Read the line again, unformatted, into the byte array. Take precautions in
; case the last line has no LF character.
;---------------------------------------------------------------------------

ON_IOERROR, done_read
READU, Unit, byt_ary
POINT_LUN, -Unit, beg_pos

;---------------------------------------------------------------------------
; If this is the first time in routine for this file, determine whether it
; was written on a Unix/OSX platform or a Windows platform or neither by
; inspecting the first line and deciding if it has a LF or CR/LF. Store the
; result as Mid (1 or 2) and return it so it's saved.
;---------------------------------------------------------------------------

IF (BegMidEnd EQ 0) THEN BEGIN
   IF (byt_ary[byt_len-2] EQ line_feed) THEN BegMidEnd = INPUT_IS_UNIX
   IF (byt_ary[byt_len-2] EQ carr_rtrn AND $
       byt_ary[byt_len-1] EQ line_feed) THEN BegMidEnd = INPUT_IS_WINDOWS
ENDIF

;---------------------------------------------------------------------------
; If the OS that wrote the file is the same as the one reading the file,
; then make no change. Otherwise, strip off the CR and/or LF characters and
; add back on the appropriate one(s).
;---------------------------------------------------------------------------

POINT_LUN, Unit, beg_pos + BegMidEnd - 2

IF (BegMidEnd EQ !KON.Misc.MINX_PLATFORM) THEN BEGIN
   LineStr = buff
ENDIF ELSE BEGIN
   IF (BegMidEnd EQ INPUT_IS_UNIX) THEN BEGIN
      buff = STRING(byt_ary[0:byt_len-3])  ; ?
   ENDIF ELSE BEGIN
      buff = STRING(byt_ary[0:byt_len-3])
   ENDELSE
ENDELSE

;---------------------------------------------------------------------------
; Here after each line is read and if a read error occurred, which we assume
; was due to the absence of a LF character after the last line.
;---------------------------------------------------------------------------

done_read:
ON_IOERROR, NULL
LineStr = buff
IF (!ERROR_STATE.CODE NE 0) THEN BEGIN
   BegMidEnd = 3
ENDIF
RETURN

;---------------------------------------------------------------------------
; Here when we each the end ofthe file.
;---------------------------------------------------------------------------

done_file:
LineStr = ''
BegMidEnd = 3

END  ;  ReadNextAsciiLine

;***************************************************************************
PRO GetJulianDay, Date, Time, InYear, YMD, HMS, JulianDay
;***************************************************************************
; Compute the Julian day from the date and time. If the return day is to be
; counted from year 0, use InYear = 0. If the return day is to be the day
; number in the year passed, then set InYear = 1.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

YMD = STRSPLIT(Date, '-', /EXTRACT)
HMS = STRSPLIT(Time, 'T:Z', /EXTRACT)

JulianDay = JULDAY(FIX(YMD[1]), FIX(YMD[2]), FIX(YMD[0]), $
                   FIX(HMS[0]), FIX(HMS[1]), ROUND(FLOAT(HMS[2])))

IF (InYear) THEN JulianDay -= JULDAY(1, 1, FIX(YMD[0]), 0, 0, 0) - 1

END  ;  GetJulianDay

;****************************************************************************
FUNCTION PathFromOrbit, Orbit
;****************************************************************************
; Get the path number that corresponds to a particular MISR orbit number.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

RETURN, FIX( (176L + LONG(Orbit) * 16L) MOD !KON.Instr.NUM_PATHS + 1L )

END  ;  PathFromOrbit

;***************************************************************************
PRO Op_SetColorPalette, State
;***************************************************************************
; Show the user available color palettes, let him select one and set it as
; the new default. These are used only for drawing colored heights on map
; images after height retrievals and for overlaying colored MISR product
; images on the whole animation window.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

Status = 0

;---------------------------------------------------------------------------
; Get the desired color palette from the user. Don't proceed if user cancels
; out or if his selection is already set.
;---------------------------------------------------------------------------

group_leader = WIDGET_INFO(LONG(State.wTopWorkBase), /PARENT)
XLOADCT, GROUP=group_leader, /MODAL, /USE_CURRENT

;---------------------------------------------------------------------------
; Save the palette colors as long integers.
;---------------------------------------------------------------------------

!SAV.ColorPalette[*] = R_CURR[*] + G_CURR[*] * 256L + B_CURR[*] * 65536L
   
;---------------------------------------------------------------------------
; Redraw the parent window.
;---------------------------------------------------------------------------

!VAR.DataSwath.OP_CHANGED = 1

RedrawWindow, State, State.curframe
   
END  ;  Op_SetColorPalette
