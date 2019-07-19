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

;********************************************************************
FUNCTION TestIfMP4_License, dummy
;********************************************************************
; Test whether the user has a license for the MP4 (IDL 8.1+) feature.
;--------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Set up an error catcher as part of the test.
;------------------------------------------------------------------------

CATCH, error_status
IF (error_status NE 0) THEN BEGIN
   CATCH, /CANCEL
   RETURN, 0
ENDIF

;------------------------------------------------------------------------
; Perform the test.
;------------------------------------------------------------------------

testfile = !SAV.WorkingDir + !KON.Misc.Slash + 'testing.mp4'
VidObj = IDLffVideoWrite(testfile, FORMAT='mp4')
VidObj.Cleanup
FILE_DELETE, testfile

;------------------------------------------------------------------------
; If we get here, success. And clean up.
;------------------------------------------------------------------------

CATCH, /CANCEL
RETURN, 1

END  ;  TestIfMP4License

;************************************************************************
PRO VideoMP4_Open, Filename, Xsize, Ysize, FrmPerSec, BitRate, VidObj, $
                   vidStream
;************************************************************************
; Open the MP4 file for writing.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

fps = FrmPerSec     ; the more frames per second, the smaller the file!
bitrate = BitRate   ; the larger the bitrate, the larger the file

VidObj = IDLffVideoWrite(Filename, FORMAT='mp4')
vidStream = VidObj.AddVideoStream(Xsize, Ysize, fps, BIT_RATE=bitrate)

; Show a list of available codecs
;codecs1 = VidObj.GetCodecs(/VIDEO)
;ncodec1 = N_ELEMENTS(codecs1)
;codecs2 = VidObj.GetCodecs(/VIDEO, /LONG_NAMES)
;ncodec2 = N_ELEMENTS(codecs2)
;mssg = ['List of Codecs:  short  long']
;FOR ii=0,ncodec1-1 DO BEGIN
;   codecs3 = codecs1[ii] + '  ;  ' + codecs2[ii]
;   mssg = [mssg, codecs3]
;ENDFOR
;rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)

END  ;  VideoMP4_Open

;************************************************************************
PRO VideoMP4_Put, VidObj, vidStream, Image
;************************************************************************
; Insert an image into the MP4 file.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Time = VidObj.Put(vidStream, Image)

END  ;  VideoMP4_Put

;************************************************************************
PRO VideoMP4_Close, VidObj
;************************************************************************
; Save and close the MP4 file.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

VidObj.Cleanup

END  ;  VideoMP4_Close
