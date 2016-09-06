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
PRO ConvertBrfOption_eh, event
;***************************************************************************
; Event handler for user interface for setting convert BRF options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON convert_brf_options, WidgetStructConvert

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct_convert, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WidgetStructConvert.Cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ widget_struct_convert.button6 : BEGIN
      GetAsSurfaceFile, DirName, FileName, PathNum, OrbitNum, $
                        BlockBeg, BlockEnd, Retval
      IF (Retval) THEN GOTO, skip97
      AsSurfaceFile = DirName + !KON.Misc.Slash + FileName
      widget_struct_convert.AsSurfaceFile = AsSurfaceFile
      widget_struct_convert.PathNum = PathNum
      widget_struct_convert.OrbitNum = OrbitNum
      widget_struct_convert.BlockBeg = BlockBeg
      widget_struct_convert.BlockEnd = BlockEnd
      WIDGET_CONTROL, widget_struct_convert.label8a, SET_VALUE=DirName
      WIDGET_CONTROL, widget_struct_convert.label9a, SET_VALUE=FileName
      WIDGET_CONTROL, widget_struct_convert.integer5a, SET_VALUE=BlockBeg
      WIDGET_CONTROL, widget_struct_convert.integer5b, SET_VALUE=BlockEnd
      WIDGET_CONTROL, widget_struct_convert.label5b, $
             SET_VALUE = (STRTRIM(STRING(BlockBeg),2) + ' - ' + $
                         STRTRIM(STRING(BlockEnd),2))[0]
      WIDGET_CONTROL, widget_struct_convert.button1, SENSITIVE=1
      skip97:
   END

   event.id EQ widget_struct_convert.button1 : BEGIN
      bbeg = widget_struct_convert.BlockBeg
      bend = widget_struct_convert.BlockEnd
      GetTerrainFile, widget_struct_convert.PathNum, $
                      widget_struct_convert.OrbitNum, $
                      bbeg, bend, DirName, FileName, Retval
      IF (Retval) THEN GOTO, skip98
      TerrainFile = DirName + !KON.Misc.Slash + FileName
      widget_struct_convert.TerrainFile = TerrainFile
      widget_struct_convert.BlockBeg = bbeg
      widget_struct_convert.BlockEnd = bend
      WIDGET_CONTROL, widget_struct_convert.integer5a, $
                      SET_VALUE=widget_struct_convert.BlockBeg
      WIDGET_CONTROL, widget_struct_convert.integer5b, $
                      SET_VALUE=widget_struct_convert.BlockEnd
      WIDGET_CONTROL, widget_struct_convert.label3a, SET_VALUE=DirName
      WIDGET_CONTROL, widget_struct_convert.label4a, SET_VALUE=FileName
      skip98:
   END

   event.id EQ WidgetStructConvert.btngrp10 : BEGIN
      WIDGET_CONTROL, WidgetStructConvert.btngrp12a, GET_VALUE=vals
      vals[event.value] = event.select
      WIDGET_CONTROL, WidgetStructConvert.btngrp12a, SET_VALUE=vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12b, GET_VALUE=vals
      vals[event.value] = event.select
      WIDGET_CONTROL, WidgetStructConvert.btngrp12b, SET_VALUE=vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12c, GET_VALUE=vals
      vals[event.value] = event.select
      WIDGET_CONTROL, WidgetStructConvert.btngrp12c, SET_VALUE=vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12d, GET_VALUE=vals
      vals[event.value] = event.select
      WIDGET_CONTROL, WidgetStructConvert.btngrp12d, SET_VALUE=vals
   END
   event.id EQ WidgetStructConvert.btngrp11 : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         vals = [1,1,1,1,1,1,1,1,1]
      ENDIF ELSE BEGIN
         vals = [0,0,0,0,0,0,0,0,0]
      ENDELSE
      IF (event.value EQ 0) THEN btn = WidgetStructConvert.btngrp12a
      IF (event.value EQ 1) THEN btn = WidgetStructConvert.btngrp12b
      IF (event.value EQ 2) THEN btn = WidgetStructConvert.btngrp12c
      IF (event.value EQ 3) THEN btn = WidgetStructConvert.btngrp12d
      WIDGET_CONTROL, btn, SET_VALUE=vals
   END

   event.id EQ WidgetStructConvert.btngrp12a : BEGIN
   END
   event.id EQ WidgetStructConvert.btngrp12b : BEGIN
   END
   event.id EQ WidgetStructConvert.btngrp12c : BEGIN
   END
   event.id EQ WidgetStructConvert.btngrp12d : BEGIN
   END
   event.id EQ WidgetStructConvert.integer13 : BEGIN
   END
   event.id EQ WidgetStructConvert.button14 : BEGIN
      IF (event.select EQ 0) THEN BEGIN
         WIDGET_CONTROL, WidgetStructConvert.integer15, SENSITIVE=0
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, WidgetStructConvert.integer15, SENSITIVE=1
      ENDELSE
   END
   event.id EQ WidgetStructConvert.integer15 : BEGIN
   END
   event.id EQ WidgetStructConvert.button16 : BEGIN
   END
   event.id EQ WidgetStructConvert.button17 : BEGIN
   END
   event.id EQ WidgetStructConvert.btngrp18 : BEGIN
   END

   event.id EQ widget_struct_convert.ok_button : BEGIN
      WIDGET_CONTROL, WidgetStructConvert.label8a, GET_VALUE=DirName
      WIDGET_CONTROL, WidgetStructConvert.label9a, GET_VALUE=FileName
      WidgetStructConvert.AsSurfaceFile = DirName + !KON.Misc.Slash + FileName
      IF (widget_struct_convert.OrbitNum EQ 0L OR $
          STRMID(FileName,0,17) EQ 'select a filename') THEN BEGIN
         res = DIALOG_MESSAGE([['If you wish to exit without selecting'], $
                       ['an AS_LAND file, then click <Cancel>, not <OK>.']], $
                       /INFORMATION, /CENTER)
         BREAK
      ENDIF

      WIDGET_CONTROL, WidgetStructConvert.label3a, GET_VALUE=DirName
      WIDGET_CONTROL, WidgetStructConvert.label4a, GET_VALUE=FileName
      WidgetStructConvert.TerrainFile = DirName + !KON.Misc.Slash + FileName
      IF (widget_struct_convert.OrbitNum EQ 0L OR $
          STRMID(FileName,0,17) EQ 'select a filename') THEN BEGIN
         res = DIALOG_MESSAGE([['If you wish to exit without selecting'], $
                       ['a L1B2 file, then click <Cancel>, not <OK>.']], $
                       /INFORMATION, /CENTER)
         BREAK
      ENDIF

      WIDGET_CONTROL, widget_struct_convert.integer5a, GET_VALUE=temp
      WidgetStructConvert.BlockBeg = temp
      WIDGET_CONTROL, widget_struct_convert.integer5b, GET_VALUE=temp
      WidgetStructConvert.BlockEnd = temp

      WidgetStructConvert.CamFlags = widget_struct_convert.CamFlags
      WidgetStructConvert.BandFlags = widget_struct_convert.BandFlags
      WidgetStructConvert.Band1Flags = widget_struct_convert.Band1Flags
      WidgetStructConvert.Band2Flags = widget_struct_convert.Band2Flags
      WidgetStructConvert.Band3Flags = widget_struct_convert.Band3Flags
      WidgetStructConvert.Band4Flags = widget_struct_convert.Band4Flags
      WidgetStructConvert.PathNum = widget_struct_convert.PathNum
      WidgetStructConvert.OrbitNum = widget_struct_convert.OrbitNum
      WidgetStructConvert.Cancel = 0

      WIDGET_CONTROL, WidgetStructConvert.btngrp10,  GET_VALUE=vals
      WidgetStructConvert.CamFlags = vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp11,  GET_VALUE=vals
      WidgetStructConvert.BandFlags = vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12a, GET_VALUE=vals
      WidgetStructConvert.Band1Flags = vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12b, GET_VALUE=vals
      WidgetStructConvert.Band2Flags = vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12c, GET_VALUE=vals
      WidgetStructConvert.Band3Flags = vals
      WIDGET_CONTROL, WidgetStructConvert.btngrp12d, GET_VALUE=vals
      WidgetStructConvert.Band4Flags = vals

      WIDGET_CONTROL, WidgetStructConvert.integer13, GET_VALUE=vals
      WidgetStructConvert.RegressSize = vals
      WidgetStructConvert.DoSmooth = $
         WIDGET_INFO(WidgetStructConvert.button14, /BUTTON_SET)
      WIDGET_CONTROL, WidgetStructConvert.integer15, GET_VALUE=vals
      WidgetStructConvert.FilterWidth = vals
      WidgetStructConvert.DoShowCoef = $
         WIDGET_INFO(WidgetStructConvert.button16, /BUTTON_SET)
      WidgetStructConvert.DoShowBRFs = $
         WIDGET_INFO(WidgetStructConvert.button17, /BUTTON_SET)
      WIDGET_CONTROL, WidgetStructConvert.btngrp18, GET_VALUE=vals
      WidgetStructConvert.FileType = vals

      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_convert.cancel_button : BEGIN
      WidgetStructConvert.Cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ widget_struct_convert.help_button : BEGIN
; add PDF Help here
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct_convert, /NO_COPY

END ; ConvertBrfOption_eh

;***************************************************************************
PRO ConvertBrfOption_gui, AsSurfaceFile, TerrainFile, PathNum, $
                          OrbitNum, BlockBeg, BlockEnd, ChannelFlags, $
                          RegressSize, DoSmooth, FilterWidth, $
                          DoShowCoef, DoShowBRFs, FileType, Cancel
;***************************************************************************
; User interface for setting convert BRF options.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON convert_brf_options, WidgetStructConvert

;---------------------------------------------------------------------------
; Set default values for parameters to be returned.
;---------------------------------------------------------------------------

Cancel = 0
AsSurfaceFile = ''
TerrainFile = ''
PathNum  = 1
OrbitNum = 0L
BlockBeg = 1
BlockEnd = !KON.Instr.NUM_BLOCKS
CamFlags   = [1,1,1,1,1,1,1,1,1]
BandFlags  = [1,1,1,1]
Band1Flags = [1,1,1,1,1,1,1,1,1]
Band2Flags = [1,1,1,1,1,1,1,1,1]
Band3Flags = [1,1,1,1,1,1,1,1,1]
Band4Flags = [1,1,1,1,1,1,1,1,1]
RegressSize = 17600.    ; Length of edge of square area over which
                        ; each regression is performed.
DoSmooth = 1            ; 1 = apply smoothing filter to lo-res linear
                        ; coefficients; 0 = do not.
FilterWidth = 3         ; Width of the side of the filter used to
                        ; smooth the lo-res linear coefficients if
                        ; DoSmooth == 1.
DoShowCoef = 0          ; 1 = display images of linear regression
                        ; coefficients; 0 = do not.
DoShowBRFs = 0          ; 1 = display images of before Land BRFs and
                        ; after computed BRFs; 0 = do not.
FileType = 2            ; code for type of output file to save data in:
                        ; 1 = HDFEOS, 2 = HDF, 3 = ASCII

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE( /COLUMN, TITLE='MINX V' + !KON.Misc.MINX_VERSION_NUM, $
                     /TLB_KILL_REQUEST_EVENTS)
label0 = WIDGET_LABEL( base0, VALUE='Convert BRF Options' )

base6 = WIDGET_BASE( base0, /COLUMN, /FRAME )
button6 = WIDGET_BUTTON( base6, /ALIGN_LEFT, $
          VALUE='Select a MISR L2AS Surface File')
base7 = WIDGET_BASE( base6, /COLUMN )
base8 = WIDGET_BASE( base7, /ROW )
label8 = WIDGET_LABEL( base8, VALUE='Directory:' )
label8a = WIDGET_LABEL( base8, $
             VALUE='        select filename by clicking button above        ')
;            /SUNKEN_FRAME, /DYNAMIC_RESIZE ) ; BROKEN IN V6.3
base9 = WIDGET_BASE( base7, /ROW )
label9 = WIDGET_LABEL( base9, VALUE='File Name:' )
label9a = WIDGET_LABEL( base9, VALUE='                                                        ')
;                        /SUNKEN_FRAME, /DYNAMIC_RESIZE ) ; BROKEN IN V6.3

base1 = WIDGET_BASE( base0, /COLUMN, /FRAME )
button1 = WIDGET_BUTTON( base1, /ALIGN_LEFT, $
          VALUE='Select any MISR L1B2 Terrain File for Same Orbit')
base2 = WIDGET_BASE( base1, /COLUMN )
base3 = WIDGET_BASE( base2, /ROW )
label3 = WIDGET_LABEL( base3, VALUE='Directory:' )
label3a = WIDGET_LABEL( base3, $
             VALUE='        select filename by clicking button above        ')
;            /SUNKEN_FRAME, /DYNAMIC_RESIZE ) ; BROKEN IN V6.3
base4 = WIDGET_BASE( base2, /ROW )
label4 = WIDGET_LABEL( base4, VALUE='File Name:' )
label4a = WIDGET_LABEL( base4, VALUE='                                                        ')
;                        /SUNKEN_FRAME, /DYNAMIC_RESIZE ) ; BROKEN IN V6.3

base5 = WIDGET_BASE( base0, /COLUMN, /FRAME )
base5a = WIDGET_BASE( base5, /ROW )
label5a = WIDGET_LABEL( base5a, VALUE='Select Block Range:') ; , /DYNAMIC_RESIZE ) ; BROKEN IN V6.3
label5b = WIDGET_LABEL( base5a, VALUE='<no file loaded>') ; , /DYNAMIC_RESIZE ) ; BROKEN IN V6.3
base5b = WIDGET_BASE( base5, /ROW )
integer5a = CW_FIELD( base5b, VALUE='', /INTEGER, XSIZE=5, TITLE='First Block:' )
integer5b = CW_FIELD( base5b, VALUE='', /INTEGER, XSIZE=5, TITLE='Last Block:' )

base10 = WIDGET_BASE( base0, /COLUMN, /FRAME )
label10a = WIDGET_LABEL( base10, VALUE='Select Channels to Process:') ; , /DYNAMIC_RESIZE ) ; BROKEN IN V6.3
btngrp10 = CW_BGROUP(base10, ['Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da'], $
                     ROW=1, /NONEXCLUSIVE)

btngrp11 = CW_BGROUP(base10, !KON.Instr.BAND_NAMES, ROW=1, /NONEXCLUSIVE)

btngrp12a = CW_BGROUP(base10, ['  ','  ','  ','  ','  ','  ','  ','  ','  '], $
                      ROW=1, /NONEXCLUSIVE)
btngrp12b = CW_BGROUP(base10, ['  ','  ','  ','  ','  ','  ','  ','  ','  '], $
                      ROW=1, /NONEXCLUSIVE)
btngrp12c = CW_BGROUP(base10, ['  ','  ','  ','  ','  ','  ','  ','  ','  '], $
                      ROW=1, /NONEXCLUSIVE)
btngrp12d = CW_BGROUP(base10, ['  ','  ','  ','  ','  ','  ','  ','  ','  '], $
                      ROW=1, /NONEXCLUSIVE)

base13 = WIDGET_BASE( base0, /COLUMN, /FRAME )
base13a = WIDGET_BASE( base13, /COLUMN )
integer13 = CW_FIELD( base13a, VALUE='17600', /INTEGER, XSIZE=6, $
                      TITLE='Length of regression square edges (meters):' )

base14 = WIDGET_BASE( base13a, /ROW)
label14 = WIDGET_LABEL( base14, VALUE='Apply smoothing filter to regression coeff:' )
base14a = WIDGET_BASE( base14, /ROW, /NONEXCLUSIVE )
button14 = WIDGET_BUTTON( base14a, /ALIGN_LEFT, VALUE='  ')

integer15 = CW_FIELD( base13a, VALUE='17600', /INTEGER, XSIZE=6, $
                      TITLE='Number of pixels on filter edges:' )

base16 = WIDGET_BASE( base13a, /ROW )
label16 = WIDGET_LABEL( base16, VALUE='Display regression coeff images:' )
base16a = WIDGET_BASE( base16, /ROW, /NONEXCLUSIVE )
button16 = WIDGET_BUTTON( base16a, /ALIGN_LEFT, VALUE='  ' )

base17 = WIDGET_BASE( base13a, /ROW )
label17 = WIDGET_LABEL( base17, VALUE='Display before and after BRF images:' )
base17a = WIDGET_BASE( base17, /ROW, /NONEXCLUSIVE )
button17 = WIDGET_BUTTON( base17a, VALUE='  ' )

base18 = WIDGET_BASE( base13a, /ROW )
label18 = WIDGET_LABEL( base18, VALUE='Type of output file to create:' )
btngrp18 = CW_BGROUP(base18, ['HDFEOS','HDF','ASCII'], ROW=1, /EXCLUSIVE)

basex = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER )
ok_button = WIDGET_BUTTON( basex, VALUE='  OK  ' )
cancel_button = WIDGET_BUTTON( basex, VALUE='Cancel' )
help_button = WIDGET_BUTTON( basex, VALUE='Help' )

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

widget_struct_convert = { $
   button1:button1, $
   button6:button6, $
   label3a:label3a, $
   label4a:label4a, $
   label8a:label8a, $
   label9a:label9a, $
   integer5a:integer5a, $
   integer5b:integer5b, $
   label5b:label5b, $
   btngrp10:btngrp10, $
   btngrp11:btngrp11, $
   btngrp12a:btngrp12a, $
   btngrp12b:btngrp12b, $
   btngrp12c:btngrp12c, $
   btngrp12d:btngrp12d, $
   integer13:integer13, $
   button14:button14, $
   integer15:integer15, $
   button16:button16, $
   button17:button17, $
   btngrp18:btngrp18, $
   ok_button:ok_button, $
   cancel_button:cancel_button, $
   help_button:help_button, $
   AsSurfaceFile:AsSurfaceFile, $
   TerrainFile:TerrainFile, $
   PathNum:PathNum, $
   OrbitNum:OrbitNum, $
   BlockBeg:BlockBeg, $
   BlockEnd:BlockEnd, $
   CamFlags:CamFlags, $
   BandFlags:BandFlags, $
   Band1Flags:Band1Flags, $
   Band2Flags:Band2Flags, $
   Band3Flags:Band3Flags, $
   Band4Flags:Band4Flags, $
   RegressSize:RegressSize, $
   DoSmooth:DoSmooth, $
   FilterWidth:FilterWidth, $
   DoShowCoef:DoShowCoef, $
   DoShowBRFs:DoShowBRFs, $
   FileType:FileType, $
   Cancel:Cancel }

HELP, /STRUCTURE, WidgetStructConvert, OUTPUT=exists

IF (STRTRIM(exists[1],2) EQ 'UNDEFINED = <Undefined>') THEN BEGIN
   WidgetStructConvert = widget_struct_convert
ENDIF ELSE BEGIN
   WidgetStructConvert.button1 = widget_struct_convert.button1
   WidgetStructConvert.button6 = widget_struct_convert.button6
   WidgetStructConvert.label3a = widget_struct_convert.label3a
   WidgetStructConvert.label4a = widget_struct_convert.label4a
   WidgetStructConvert.label8a = widget_struct_convert.label8a
   WidgetStructConvert.label9a = widget_struct_convert.label9a
   WidgetStructConvert.integer5a = widget_struct_convert.integer5a
   WidgetStructConvert.integer5b = widget_struct_convert.integer5b
   WidgetStructConvert.label5b = widget_struct_convert.label5b
   WidgetStructConvert.btngrp10 = widget_struct_convert.btngrp10
   WidgetStructConvert.btngrp11 = widget_struct_convert.btngrp11
   WidgetStructConvert.btngrp12a = widget_struct_convert.btngrp12a
   WidgetStructConvert.btngrp12b = widget_struct_convert.btngrp12b
   WidgetStructConvert.btngrp12c = widget_struct_convert.btngrp12c
   WidgetStructConvert.btngrp12d = widget_struct_convert.btngrp12d
   WidgetStructConvert.integer13 = widget_struct_convert.integer13
   WidgetStructConvert.button14 = widget_struct_convert.button14
   WidgetStructConvert.integer15 = widget_struct_convert.integer15
   WidgetStructConvert.button16 = widget_struct_convert.button16
   WidgetStructConvert.button17 = widget_struct_convert.button17
   WidgetStructConvert.btngrp18 = widget_struct_convert.btngrp18
   WidgetStructConvert.ok_button = widget_struct_convert.ok_button
   WidgetStructConvert.cancel_button = widget_struct_convert.cancel_button
   WidgetStructConvert.help_button = widget_struct_convert.help_button
   widget_struct_convert.AsSurfaceFile = WidgetStructConvert.AsSurfaceFile
   widget_struct_convert.TerrainFile = WidgetStructConvert.TerrainFile
   widget_struct_convert.PathNum  = WidgetStructConvert.PathNum
   widget_struct_convert.OrbitNum = WidgetStructConvert.OrbitNum
   widget_struct_convert.BlockBeg = WidgetStructConvert.BlockBeg
   widget_struct_convert.BlockEnd = WidgetStructConvert.BlockEnd
   widget_struct_convert.CamFlags = WidgetStructConvert.CamFlags
   widget_struct_convert.BandFlags = WidgetStructConvert.BandFlags
   widget_struct_convert.Band1Flags = WidgetStructConvert.Band1Flags
   widget_struct_convert.Band2Flags = WidgetStructConvert.Band2Flags
   widget_struct_convert.Band3Flags = WidgetStructConvert.Band3Flags
   widget_struct_convert.Band4Flags = WidgetStructConvert.Band4Flags
   widget_struct_convert.RegressSize = WidgetStructConvert.RegressSize
   widget_struct_convert.DoSmooth    = WidgetStructConvert.DoSmooth
   widget_struct_convert.FilterWidth = WidgetStructConvert.FilterWidth
   widget_struct_convert.DoShowCoef = WidgetStructConvert.DoShowCoef
   widget_struct_convert.DoShowBRFs = WidgetStructConvert.DoShowBRFs
   widget_struct_convert.FileType = WidgetStructConvert.FileType
   widget_struct_convert.Cancel = WidgetStructConvert.Cancel
ENDELSE

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

dir_name = '        select filename by clicking button above        '
fil_name = '                                                        '
slen = STRLEN(widget_struct_convert.AsSurfaceFile)
IF (slen NE 0) THEN BEGIN
   spos = STRPOS(widget_struct_convert.AsSurfaceFile, !KON.Misc.Slash, $
                 /REVERSE_SEARCH)
   IF (spos GT 0) THEN BEGIN
      dir_name = STRMID(widget_struct_convert.AsSurfaceFile, 0, spos)
      fil_name = STRMID(widget_struct_convert.AsSurfaceFile, spos+1, $
                        slen-spos-1)
   ENDIF
ENDIF
WIDGET_CONTROL, label8a, SET_VALUE=dir_name
WIDGET_CONTROL, label9a, SET_VALUE=fil_name

dir_name = '        select filename by clicking button above        '
fil_name = '                                                        '
slen = STRLEN(widget_struct_convert.TerrainFile)
IF (slen NE 0) THEN BEGIN
   spos = STRPOS(widget_struct_convert.TerrainFile, !KON.Misc.Slash, $
                 /REVERSE_SEARCH)
   IF (spos GT 0) THEN BEGIN
      dir_name = STRMID(widget_struct_convert.TerrainFile, 0, spos)
      fil_name = STRMID(widget_struct_convert.TerrainFile, spos+1, slen-spos-1)
   ENDIF
ENDIF
WIDGET_CONTROL, label3a, SET_VALUE=dir_name
WIDGET_CONTROL, label4a, SET_VALUE=fil_name
WIDGET_CONTROL, integer5a, SET_VALUE=widget_struct_convert.BlockBeg
WIDGET_CONTROL, integer5b, SET_VALUE=widget_struct_convert.BlockEnd

WIDGET_CONTROL, widget_struct_convert.button1, SENSITIVE=0

WIDGET_CONTROL, btngrp10,  SET_VALUE=CamFlags
WIDGET_CONTROL, btngrp11,  SET_VALUE=BandFlags
WIDGET_CONTROL, btngrp12a, SET_VALUE=Band1Flags
WIDGET_CONTROL, btngrp12b, SET_VALUE=Band2Flags
WIDGET_CONTROL, btngrp12c, SET_VALUE=Band3Flags
WIDGET_CONTROL, btngrp12d, SET_VALUE=Band4Flags
WIDGET_CONTROL, integer13, SET_VALUE=FIX(RegressSize)
WIDGET_CONTROL, button14,  /SET_BUTTON
WIDGET_CONTROL, integer15, SET_VALUE=FilterWidth
WIDGET_CONTROL, btngrp18,  SET_VALUE=FileType - 1

;---------------------------------------------------------------------------
; Store the structure in the widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct_convert, XOFFSET=750, $
                YOFFSET=100, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'ConvertBrfOption_gui', base0, EVENT_HANDLER='ConvertBrfOption_eh'

AsSurfaceFile = WidgetStructConvert.AsSurfaceFile
TerrainFile = WidgetStructConvert.TerrainFile
PathNum = WidgetStructConvert.PathNum
OrbitNum = WidgetStructConvert.OrbitNum
BlockBeg = WidgetStructConvert.BlockBeg
BlockEnd = WidgetStructConvert.BlockEnd
CamFlags = WidgetStructConvert.CamFlags
BandFlags = WidgetStructConvert.BandFlags
Band1Flags = WidgetStructConvert.Band1Flags
Band2Flags = WidgetStructConvert.Band2Flags
Band3Flags = WidgetStructConvert.Band3Flags
Band4Flags = WidgetStructConvert.Band4Flags
RegressSize = FLOAT(WidgetStructConvert.RegressSize)
DoSmooth    = WidgetStructConvert.DoSmooth
FilterWidth = WidgetStructConvert.FilterWidth
DoShowCoef = WidgetStructConvert.DoShowCoef
DoShowBRFs = WidgetStructConvert.DoShowBRFs
FileType = WidgetStructConvert.FileType + 1
Cancel = WidgetStructConvert.Cancel

;---------------------------------------------------------------------------
; Construct the channel flags and return it.  Then clean up.
;---------------------------------------------------------------------------

ChannelFlags = [[Band1Flags], [Band2Flags], [Band3Flags], [Band4Flags]]

CamFlags   = 0
BandFlags  = 0
Band1Flags = 0
Band2Flags = 0
Band3Flags = 0
Band4Flags = 0
widget_struct_convert = 0

END ; ConvertBrfOption_gui

;***************************************************************************
PRO PlumeRegionOptions_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON datargn_opt, curframe, exit_dlg
COMMON coord_data, CoordStruct

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=datargn_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   exit_dlg = 1
   !VAR.DataRgn.Show = !KON.DataRgn.SHOW_DO_NOT
   DestroyColorKey, 1  ; this also destroys this dialog window
   RedrawWindow, datargn_struct.State, datargn_struct.State.curframe
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   ;-----------------------------------------------------------------------
   ; Update control states and global values when data type is changed.
   ;-----------------------------------------------------------------------

   event.id EQ datargn_struct.data_type : BEGIN
      entry = WIDGET_INFO(event.id, /DROPLIST_SELECT)
      !VAR.DataRgn.DATA_TYPE = entry
      temp_min = !VAR.DataRgn.VALUE_MIN_ABS[entry]
      temp_max = !VAR.DataRgn.VALUE_MAX_ABS[entry]
      
      WIDGET_CONTROL, datargn_struct.min_field, SET_VALUE=temp_min
      WIDGET_CONTROL, datargn_struct.max_field, SET_VALUE=temp_max

      WIDGET_CONTROL, datargn_struct.data_units, SET_VALUE= $
                      'Data units:    ' + !KON.DataRgn.DATA_UNITS[entry]

      IF (entry EQ !KON.DataRgn.TYPE_DISP_CROSS OR $
          entry EQ !KON.DataRgn.TYPE_DISP_ALONG) THEN BEGIN
         WIDGET_CONTROL, datargn_struct.cam_df, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_cf, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_bf, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_af, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_aa, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_ba, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_ca, SENSITIVE=1
         WIDGET_CONTROL, datargn_struct.cam_da, SENSITIVE=1
         cam_arr = [datargn_struct.cam_df, datargn_struct.cam_cf, $
                    datargn_struct.cam_bf, datargn_struct.cam_af, $
                    datargn_struct.cam_aa, datargn_struct.cam_ba, $
                    datargn_struct.cam_ca, datargn_struct.cam_da]
         WIDGET_CONTROL, cam_arr[!VAR.DataRgn.CAM_SEL], SET_BUTTON=1
         arr = 0
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, datargn_struct.cam_df, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_cf, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_bf, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_af, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_aa, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_ba, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_ca, SENSITIVE=0
         WIDGET_CONTROL, datargn_struct.cam_da, SENSITIVE=0
      ENDELSE

   END

   ;-----------------------------------------------------------------------
   ; Update control states and global values when min or max data values
   ; are changed.
   ;-----------------------------------------------------------------------

   event.id EQ datargn_struct.min_field : BEGIN
      WIDGET_CONTROL, event.id, GET_VALUE=dataval
      bound_val = !KON.DataRgn.VALUE_MIN[!VAR.DataRgn.DATA_TYPE]
      IF (dataval LT bound_val) THEN BEGIN
         mssg = 'Your Minimum value entry must be at least ' + $
                 STRING(FORMAT='(F5.1)', bound_val) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, datargn_struct.min_field, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
   END
   event.id EQ datargn_struct.max_field : BEGIN
      WIDGET_CONTROL, event.id, GET_VALUE=dataval

      bound_val = !KON.DataRgn.VALUE_MAX[!VAR.DataRgn.DATA_TYPE]
      IF (dataval GT bound_val) THEN BEGIN
         mssg = 'Your Maximum value entry must be no more than ' + $
                 STRING(FORMAT='(F5.1)', bound_val) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, datargn_struct.max_field, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
   END

   event.id EQ datargn_struct.red_retr  : BEGIN
   END
   event.id EQ datargn_struct.blue_retr : BEGIN
   END
   event.id EQ datargn_struct.both_retr : BEGIN
   END

   event.id EQ datargn_struct.cam_df : BEGIN
   END
   event.id EQ datargn_struct.cam_cf : BEGIN
   END
   event.id EQ datargn_struct.cam_bf : BEGIN
   END
   event.id EQ datargn_struct.cam_af : BEGIN
   END
   event.id EQ datargn_struct.cam_aa : BEGIN
   END
   event.id EQ datargn_struct.cam_ba : BEGIN
   END
   event.id EQ datargn_struct.cam_ca : BEGIN
   END
   event.id EQ datargn_struct.cam_da : BEGIN
   END

   ;-----------------------------------------------------------------------
   ; Update control states when display location is changed.
   ;-----------------------------------------------------------------------

   event.id EQ datargn_struct.show_not : BEGIN
      WIDGET_CONTROL, datargn_struct.x_pos, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.y_pos, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.back_white, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.back_black, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.back_gray , SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.back_trans, SENSITIVE=0
   END
   event.id EQ datargn_struct.show_sep : BEGIN
      WIDGET_CONTROL, datargn_struct.x_pos, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.y_pos, SENSITIVE=0
      WIDGET_CONTROL, datargn_struct.back_white, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_black, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_gray , SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_trans, SENSITIVE=0
      IF (WIDGET_INFO(datargn_struct.back_trans, /BUTTON_SET)) THEN $
         WIDGET_CONTROL, datargn_struct.back_white, /SET_BUTTON
   END
   event.id EQ datargn_struct.show_anm : BEGIN
      WIDGET_CONTROL, datargn_struct.x_pos, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.y_pos, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_white, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_black, SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_gray , SENSITIVE=1
      WIDGET_CONTROL, datargn_struct.back_trans, SENSITIVE=1
   END

   event.id EQ datargn_struct.x_pos : BEGIN
      WIDGET_CONTROL, event.id, GET_VALUE=dataval
      bound_val = 100
      IF (dataval LT bound_val) THEN BEGIN
         mssg = 'Your X-position entry must be at least ' + $
                STRTRIM(STRING(bound_val),2) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, event.id, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
      bound_val = 2000
      IF (dataval GT bound_val) THEN BEGIN
         mssg = 'Your X-position entry must be no more than ' + $
                STRTRIM(STRING(bound_val),2) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, event.id, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
   END
   event.id EQ datargn_struct.y_pos : BEGIN
      WIDGET_CONTROL, event.id, GET_VALUE=dataval
      bound_val = 300
      IF (dataval LT bound_val) THEN BEGIN
         mssg = 'Your Y-position entry must be at least ' + $
                STRTRIM(STRING(bound_val),2) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, event.id, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
      whichorbit = (curframe GT 9)
      bound_val = CoordStruct.(whichorbit).NumBlk * 512 - 30
      IF (dataval GT bound_val) THEN BEGIN
         mssg = 'Your Y-position entry must be no more than ' + $
                STRTRIM(STRING(bound_val),2) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, event.id, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
   END

   event.id EQ datargn_struct.back_white : BEGIN
   END
   event.id EQ datargn_struct.back_black : BEGIN
   END
   event.id EQ datargn_struct.back_gray  : BEGIN
   END
   event.id EQ datargn_struct.back_trans : BEGIN
   END

   ;-----------------------------------------------------------------------
   ; Process dialog contents when user accepts all changes.
   ;-----------------------------------------------------------------------

   event.id EQ datargn_struct.redraw_btn : BEGIN

      onoff = WIDGET_INFO(datargn_struct.red_retr,  /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW_BAND_TYPE = 0
      onoff = WIDGET_INFO(datargn_struct.blue_retr, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW_BAND_TYPE = 1
      onoff = WIDGET_INFO(datargn_struct.both_retr, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW_BAND_TYPE = 2

      entry = WIDGET_INFO(datargn_struct.data_type, /DROPLIST_SELECT)
      !VAR.DataRgn.DATA_TYPE=entry

      WIDGET_CONTROL, datargn_struct.min_field, GET_VALUE=dataval1
      bound_val = !KON.DataRgn.VALUE_MIN[!VAR.DataRgn.DATA_TYPE]
      IF (dataval1 LT bound_val) THEN BEGIN
         mssg = 'Your Minimum value entry must be at least ' + $
                 STRING(FORMAT='(F5.1)', bound_val) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, datargn_struct.min_field, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
      !VAR.DataRgn.VALUE_MIN_DLG[!VAR.DataRgn.DATA_TYPE] = dataval1

      WIDGET_CONTROL, datargn_struct.max_field, GET_VALUE=dataval2
      bound_val = !KON.DataRgn.VALUE_MAX[!VAR.DataRgn.DATA_TYPE]
      IF (dataval2 GT bound_val) THEN BEGIN
         mssg = 'Your Maximum value entry must be no more than ' + $
                 STRING(FORMAT='(F5.1)', bound_val) + '. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
         WIDGET_CONTROL, datargn_struct.max_field, SET_VALUE=bound_val
         GOTO, return_point
      ENDIF
      !VAR.DataRgn.VALUE_MAX_DLG[!VAR.DataRgn.DATA_TYPE] = dataval2

      IF (dataval2 LT dataval1) THEN BEGIN
         mssg = 'Your Minimum value entry must be less than ' + $
                'your Maximum value entry. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Bad order')
         WIDGET_CONTROL, datargn_struct.max_field, $
            SET_VALUE=dataval1 + ABS(dataval1 * 0.1)
         GOTO, return_point
      ENDIF

      IF (!VAR.DataRgn.DATA_TYPE EQ !KON.DataRgn.TYPE_DISP_CROSS OR $
          !VAR.DataRgn.DATA_TYPE EQ !KON.DataRgn.TYPE_DISP_ALONG) THEN BEGIN
         onoff = WIDGET_INFO(datargn_struct.cam_df, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 0
         onoff = WIDGET_INFO(datargn_struct.cam_cf, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 1
         onoff = WIDGET_INFO(datargn_struct.cam_bf, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 2
         onoff = WIDGET_INFO(datargn_struct.cam_af, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 3
         onoff = WIDGET_INFO(datargn_struct.cam_aa, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 4
         onoff = WIDGET_INFO(datargn_struct.cam_ba, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 5
         onoff = WIDGET_INFO(datargn_struct.cam_ca, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 6
         onoff = WIDGET_INFO(datargn_struct.cam_da, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataRgn.CAM_SEL = 7
      ENDIF

      onoff = WIDGET_INFO(datargn_struct.show_not, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_DO_NOT
      onoff = WIDGET_INFO(datargn_struct.show_sep, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_IN_NEW_WNDW
      onoff = WIDGET_INFO(datargn_struct.show_anm, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_ON_ANIM_WNDW

      IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
         WIDGET_CONTROL, datargn_struct.x_pos, GET_VALUE=dataval
         bound_val = 65
         IF (dataval LT bound_val) THEN BEGIN
            mssg = 'Your X-position entry must be at least ' + $
                   STRTRIM(STRING(bound_val),2) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, datargn_struct.x_pos, SET_VALUE=bound_val
            GOTO, return_point
         ENDIF
         bound_val = 1958
         IF (dataval GT bound_val) THEN BEGIN
            mssg = 'Your X-position entry must be no more than ' + $
                   STRTRIM(STRING(bound_val),2) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, datargn_struct.x_pos, SET_VALUE=bound_val
            GOTO, return_point
         ENDIF
         !VAR.DataRgn.POS_X = dataval

         WIDGET_CONTROL, datargn_struct.y_pos, GET_VALUE=dataval
         bound_val = 290
         IF (dataval LT bound_val) THEN BEGIN
            mssg = 'Your Y-position entry must be at least ' + $
                   STRTRIM(STRING(bound_val),2) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, datargn_struct.y_pos, SET_VALUE=bound_val
            GOTO, return_point
         ENDIF
         whichorbit = (curframe GT 9)
         bound_val = CoordStruct.(whichorbit).NumBlk * 512 - 15
         IF (dataval GT bound_val) THEN BEGIN
            mssg = 'Your Y-position entry must be no more than ' + $
                   STRTRIM(STRING(bound_val),2) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, datargn_struct.y_pos, SET_VALUE=bound_val
            GOTO, return_point
         ENDIF
         !VAR.DataRgn.POS_Y = dataval
      ENDIF

      onoff = WIDGET_INFO(datargn_struct.back_white, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.BKGRND_COLOR = !KON.DataRgn.COLOR_WHITE
      onoff = WIDGET_INFO(datargn_struct.back_black, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.BKGRND_COLOR = !KON.DataRgn.COLOR_BLACK
      onoff = WIDGET_INFO(datargn_struct.back_gray, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.BKGRND_COLOR = !KON.DataRgn.COLOR_GRAY
      onoff = WIDGET_INFO(datargn_struct.back_trans, /BUTTON_SET)
      IF (onoff) THEN !VAR.DataRgn.BKGRND_COLOR = !KON.DataRgn.COLOR_TRANSP

      ShowPlumeRegionOptions, datargn_struct.State, $
                              datargn_struct.State.curframe, $
                              datargn_struct.key_left, $
                              datargn_struct.key_crnr, Retval
   END
   
   ;-----------------------------------------------------------------------
   ; User wants to exit. No more changes.
   ;-----------------------------------------------------------------------

   event.id EQ datargn_struct.exit_button : BEGIN
      exit_dlg = 1
      !VAR.DataRgn.Show = !KON.DataRgn.SHOW_DO_NOT
      DestroyColorKey, 1  ; this also destroys this dialog window
      RedrawWindow, datargn_struct.State, datargn_struct.State.curframe
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

return_point:
WIDGET_CONTROL, event.top, SET_UVALUE=datargn_struct, /NO_COPY

END ; PlumeRegionOptions_eh

;**************************************************************************
PRO Op_PlumeRegionOptions_gui, State, ExitDlg
;**************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON datargn_opt, curframe, exit_dlg

exit_dlg = 1

curframe = State.curframe

;---------------------------------------------------------------------------
; Don't create a dialog if there is already one on the screen.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.COLORKEY_DLG_ID GT 0) THEN BEGIN
   mssg = ['The "Select Digitized Region Display Options"', $
           'dialog box is already displayed.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
   ExitDlg = 1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Don't create a dialog if no plumes have been digitized.
;---------------------------------------------------------------------------

IF (~ PTR_VALID(!VAR.LinkList.pHeadObj)) THEN BEGIN
   mssg = ['You must digitize a plume before you can use the', $
           '"Select Digitized Region Display Options"dialog box.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
   ExitDlg = 1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; If the Data SWATH ! dialog box is displayed, delete it first. We don't want
; both dialogs showing concurrently.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.COLORKEY_DLG_ID  GT 0 OR $
    !VAR.DataSwath.COLORKEY_DISC_ID GT 0) THEN BEGIN
   DestroyColorKey, 2  ; this also destroys its dialog window
   RedrawWindow, State, State.curframe
ENDIF

;---------------------------------------------------------------------------
; If this is the first time it's being shown locate it near the left center
; of the current viewport.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.POS_X LT 0 OR !VAR.DataRgn.POS_Y LT 0) THEN BEGIN
   WIDGET_CONTROL, State.wDrawWindow, GET_DRAW_VIEW=viewport
   viewport_size = WIDGET_INFO(State.wDrawWindow, /GEOMETRY)
   !VAR.DataRgn.POS_X = viewport[0] + 150
   !VAR.DataRgn.POS_Y = viewport[1] + viewport_size.ysize / 2 + $
      !VAR.DataRgn.SIZE_Y / 2
ENDIF

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base_top = WIDGET_BASE(TITLE='Select Digitized Region Display Options', $
                       GROUP_LEADER=State.wAnimateBase, /FLOATING, /COLUMN, $
                       /TLB_KILL_REQUEST_EVENTS)
!VAR.DataRgn.COLORKEY_DLG_ID = base_top

base_2nd = WIDGET_BASE(base_top, /ROW)
base_1a  = WIDGET_BASE(base_2nd, /COLUMN)

label_1a = WIDGET_LABEL(base_1a, /ALIGN_LEFT, VALUE='Region display options:')
base_1b   = WIDGET_BASE(base_1a, /COLUMN, /ALIGN_CENTER, /FRAME)

base_0a  = WIDGET_BASE(base_1b, /COLUMN, /ALIGN_CENTER, /FRAME)
label_0a = WIDGET_LABEL(base_0a, /ALIGN_CENTER, $
                        VALUE='Retrieved data to display')
base_0b  = WIDGET_BASE(base_0a, /ALIGN_CENTER, /ROW, /EXCLUSIVE)
red_retr  = WIDGET_BUTTON(base_0b, VALUE='Red band', TOOLTIP='Show only regions ' + $
                          'with data retrieved using the red band retrieval ' + $
                          'or the red over water/blue over land retrieval.')
blue_retr = WIDGET_BUTTON(base_0b, VALUE='Blue band', TOOLTIP='Show only regions ' + $
                          'with data retrieved using the blue band retrieval.')
both_retr = WIDGET_BUTTON(base_0b, VALUE='Red and blue', TOOLTIP='Show data ' + $
                          'for all regions regardless of the band used for retrieval.')

label_0x = WIDGET_LABEL(base_1b, /ALIGN_CENTER, VALUE=' ', YSIZE=8)

data_type = WIDGET_DROPLIST(base_1b, TITLE='Data type', /DYNAMIC_RESIZE, $
                   VALUE=!KON.DataRgn.TYPE_LIST[!KON.DataRgn.TYPE_DISP_CROSS: $
                                                !KON.DataRgn.TYPE_WIND_TOTAL])
min_field  = CW_FIELD(base_1b, /FLOAT, /ROW, XSIZE=20, /RETURN_EVENTS, $
                      TITLE='Minimum value')
max_field  = CW_FIELD(base_1b, /FLOAT, /ROW, XSIZE=20, /RETURN_EVENTS, $
                      TITLE='Maximum value')
data_units = WIDGET_LABEL(base_1b, /ALIGN_LEFT, VALUE='Data units:    ' + $
                          !KON.DataRgn.DATA_UNITS[!VAR.DataRgn.DATA_TYPE])

label_1x = WIDGET_LABEL(base_1b, /ALIGN_CENTER, VALUE=' ', YSIZE=8)

label_1c = WIDGET_LABEL(base_1b, /ALIGN_CENTER, VALUE='Camera to show:')
base_1d  = WIDGET_BASE(base_1b, ROW=2, /ALIGN_CENTER, /FRAME, /EXCLUSIVE)
cam_df = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Df')
cam_cf = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Cf')
cam_bf = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Bf')
cam_af = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Af')
cam_aa = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Aa')
cam_ba = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Ba')
cam_ca = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Ca')
cam_da = WIDGET_BUTTON(base_1d, XSIZE=50, VALUE='Da')

base_2a  = WIDGET_BASE(base_2nd, /COLUMN)
label_2a = WIDGET_LABEL(base_2a, /ALIGN_LEFT, VALUE='Color key options:')

base_2b  = WIDGET_BASE(base_2a, /COLUMN, /ALIGN_CENTER, /FRAME)
label_2b = WIDGET_LABEL(base_2b, /ALIGN_CENTER, VALUE='Display location')
base_2c  = WIDGET_BASE(base_2b, /COLUMN, /ALIGN_CENTER, /EXCLUSIVE)
show_not = WIDGET_BUTTON(base_2c, VALUE='Do not show color key')
show_sep = WIDGET_BUTTON(base_2c, VALUE='Draw color key in separate window  ')
show_anm = WIDGET_BUTTON(base_2c, VALUE='Draw color key on camera image')

base_2d  = WIDGET_BASE(base_2a, /COLUMN, /FRAME)
label_2d = WIDGET_LABEL(base_2d, /ALIGN_CENTER, $
                        VALUE='"Pixel x/y:" coord of top right corner ')
base_3d  = WIDGET_BASE(base_2d, /ROW)
x_pos = CW_FIELD(base_3d, /INTEGER, /ROW, VALUE=!VAR.DataRgn.POS_X, $
                 XSIZE=5, /RETURN_EVENTS, TITLE='X coord')
y_pos = CW_FIELD(base_3d, /INTEGER, /ROW, VALUE=!VAR.DataRgn.POS_Y, $
                 XSIZE=5, /RETURN_EVENTS, TITLE='Y coord')

base_2e  = WIDGET_BASE(base_2a, /COLUMN, /ALIGN_CENTER, /FRAME)
label_2e = WIDGET_LABEL(base_2e, /ALIGN_CENTER, $
                        VALUE='Background color')
base_2f    = WIDGET_BASE(base_2e, /ALIGN_CENTER, /ROW, /EXCLUSIVE)
back_white = WIDGET_BUTTON(base_2f, VALUE='White')
back_black = WIDGET_BUTTON(base_2f, VALUE='Black')
back_gray  = WIDGET_BUTTON(base_2f, VALUE='Gray')
back_trans = WIDGET_BUTTON(base_2f, VALUE='Transp ')

label_2x = WIDGET_LABEL(base_2a, /ALIGN_CENTER, VALUE=' ', YSIZE=8)

base_2g = WIDGET_BASE(base_2a, /COLUMN, /ALIGN_CENTER)
base_2h = WIDGET_BASE(base_2g, /ROW, /ALIGN_CENTER)
redraw_btn  = WIDGET_BUTTON(base_2h, VALUE='Redraw')
exit_button = WIDGET_BUTTON(base_2h, VALUE=' Exit ')

;---------------------------------------------------------------------------
; Set widget states.
;---------------------------------------------------------------------------

WIDGET_CONTROL, data_type, SET_DROPLIST_SELECT=!VAR.DataRgn.DATA_TYPE

WIDGET_CONTROL, min_field, $
                SET_VALUE=!VAR.DataRgn.VALUE_MIN_ABS[!VAR.DataRgn.DATA_TYPE]
WIDGET_CONTROL, max_field, $
                SET_VALUE=!VAR.DataRgn.VALUE_MAX_ABS[!VAR.DataRgn.DATA_TYPE]

IF (!VAR.DataRgn.SHOW_BAND_TYPE EQ 0) THEN band_used = red_retr
IF (!VAR.DataRgn.SHOW_BAND_TYPE EQ 1) THEN band_used = blue_retr
IF (!VAR.DataRgn.SHOW_BAND_TYPE EQ 2) THEN band_used = both_retr
WIDGET_CONTROL, band_used, SET_BUTTON=1

IF (!VAR.DataRgn.DATA_TYPE GT !KON.DataRgn.TYPE_DISP_ALONG) THEN BEGIN
   WIDGET_CONTROL, cam_df, SENSITIVE=0
   WIDGET_CONTROL, cam_cf, SENSITIVE=0
   WIDGET_CONTROL, cam_bf, SENSITIVE=0
   WIDGET_CONTROL, cam_af, SENSITIVE=0
   WIDGET_CONTROL, cam_aa, SENSITIVE=0
   WIDGET_CONTROL, cam_ba, SENSITIVE=0
   WIDGET_CONTROL, cam_ca, SENSITIVE=0
   WIDGET_CONTROL, cam_da, SENSITIVE=0
ENDIF ELSE BEGIN
   arr = [cam_df, cam_cf, cam_bf, cam_af, cam_aa, cam_ba, cam_ca, cam_da]
   WIDGET_CONTROL, arr[!VAR.DataRgn.CAM_SEL], SET_BUTTON=1
ENDELSE

arr = [show_not, show_sep, show_anm]
WIDGET_CONTROL, arr[!VAR.DataRgn.Show], SET_BUTTON=1
IF (!VAR.DataRgn.Show NE !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
   WIDGET_CONTROL, x_pos, SENSITIVE=0
   WIDGET_CONTROL, y_pos, SENSITIVE=0
ENDIF ELSE BEGIN
   WIDGET_CONTROL, x_pos, SET_VALUE=!VAR.DataRgn.POS_X
   WIDGET_CONTROL, y_pos, SET_VALUE=!VAR.DataRgn.POS_Y
ENDELSE

IF (!VAR.DataRgn.SHOW EQ !KON.DataRgn.SHOW_DO_NOT) THEN BEGIN
   WIDGET_CONTROL, back_white, SENSITIVE=0
   WIDGET_CONTROL, back_black, SENSITIVE=0
   WIDGET_CONTROL, back_gray , SENSITIVE=0
   WIDGET_CONTROL, back_trans, SENSITIVE=0
ENDIF ELSE BEGIN
   WIDGET_CONTROL, back_white, SENSITIVE=1
   WIDGET_CONTROL, back_black, SENSITIVE=1
   WIDGET_CONTROL, back_gray , SENSITIVE=1
   IF (!VAR.DataRgn.Show NE !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN $
      WIDGET_CONTROL, back_trans, SENSITIVE=1
ENDELSE

IF (!VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_WHITE) THEN $
   WIDGET_CONTROL, back_white, SET_BUTTON=1
IF (!VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_BLACK) THEN $
   WIDGET_CONTROL, back_black, SET_BUTTON=1
IF (!VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_GRAY) THEN $
   WIDGET_CONTROL, back_gray,  SET_BUTTON=1
IF (!VAR.DataRgn.BKGRND_COLOR EQ !KON.DataRgn.COLOR_TRANSP) THEN $
   WIDGET_CONTROL, back_trans,  SET_BUTTON=1

;---------------------------------------------------------------------------
; Get the size of the dialog box and the location for its lower-left corner.
; Also get the lower-left corner location for the color bar if it is shown
; in a separate window
;---------------------------------------------------------------------------

widget_geom = WIDGET_INFO(base_top, /GEOMETRY)
xsize = widget_geom.SCR_XSIZE + widget_geom.MARGIN * 2
ysize = widget_geom.SCR_YSIZE + widget_geom.MARGIN * 2
dialog_left = !KON.Misc.ScreenX / 2 - xsize / 2
dialog_top = !KON.Misc.ScreenY - 35 - ysize

; These will be for WINDOW creation. On UNIX/OSX, this is the lower left
; corner of the window relative to the lower left corner of the screen.
; On Windows, this is the upper left corner of the window relative to the
; upper left corner of the screen.
key_left = dialog_left + xsize + 50
key_crnr = (!KON.Misc.MINX_PLATFORM EQ 1) ? 25 : dialog_top + 25
   
;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

datargn_struct = { data_type   : data_type,   $
                   min_field   : min_field,   $
                   max_field   : max_field,   $
                   data_units  : data_units,  $
                   cam_df      : cam_df,      $
                   cam_cf      : cam_cf,      $
                   cam_bf      : cam_bf,      $
                   cam_af      : cam_af,      $
                   cam_aa      : cam_aa,      $
                   cam_ba      : cam_ba,      $
                   cam_ca      : cam_ca,      $
                   cam_da      : cam_da,      $
                   show_not    : show_not,    $
                   show_sep    : show_sep,    $
                   show_anm    : show_anm,    $
                   red_retr    : red_retr,    $
                   blue_retr   : blue_retr,   $
                   both_retr   : both_retr,   $
                   x_pos       : x_pos,       $
                   y_pos       : y_pos,       $
                   back_white  : back_white,  $
                   back_black  : back_black,  $
                   back_gray   : back_gray,   $
                   back_trans  : back_trans,  $
                   redraw_btn  : redraw_btn,  $
                   exit_button : exit_button, $
                   key_left    : key_left,    $
                   key_crnr    : key_crnr,    $
                   state       : state }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it. All the data values are
; stored into !VAR parameters in PlumeRegionOptions_eh.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base_top, SET_UVALUE=datargn_struct, XOFFSET=dialog_left, $
                YOFFSET=dialog_top, /NO_COPY

WIDGET_CONTROL, base_top, /REALIZE

XMANAGER, 'Op_PlumeRegionOptions_gui', base_top, EVENT_HANDLER='PlumeRegionOptions_eh'

ExitDlg = exit_dlg

END ; Op_PlumeRegionOptions_gui

;***************************************************************************
PRO MISRSwathOptions_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=dataswath_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   ResetSwathData
   RedrawWindow, dataswath_struct.State, !VAR.DataSwath.CURRENT_WNDW
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   ;-----------------------------------------------------------------------
   ; Update control states.
   ;-----------------------------------------------------------------------
   
   event.id EQ dataswath_struct.reset_btn: BEGIN
      temp_min = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinData
      temp_max = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxData
      WIDGET_CONTROL, dataswath_struct.min_field, SET_VALUE=temp_min
      WIDGET_CONTROL, dataswath_struct.max_field, SET_VALUE=temp_max
      WIDGET_CONTROL, dataswath_struct.data_units, SET_VALUE='Data units:    ' + $
                      !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].Units
      !VAR.DataSwath.OP_CHANGED = 1
   END
   event.id EQ dataswath_struct.show_sep : BEGIN
      WIDGET_CONTROL, dataswath_struct.x_pos, SENSITIVE=0
      WIDGET_CONTROL, dataswath_struct.y_pos, SENSITIVE=0
      WIDGET_CONTROL, dataswath_struct.back_trans, SENSITIVE=0
      IF (WIDGET_INFO(dataswath_struct.back_trans, /BUTTON_SET)) THEN $
         WIDGET_CONTROL, dataswath_struct.back_white, /SET_BUTTON
      !VAR.DataSwath.OP_CHANGED = 1
   END
   event.id EQ dataswath_struct.show_anm : BEGIN
      WIDGET_CONTROL, dataswath_struct.x_pos, SENSITIVE=1
      WIDGET_CONTROL, dataswath_struct.y_pos, SENSITIVE=1
      WIDGET_CONTROL, dataswath_struct.back_trans, SENSITIVE=1
      !VAR.DataSwath.OP_CHANGED = 1
   END
   event.id EQ dataswath_struct.min_field :  !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.max_field :  !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.x_pos :      !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.y_pos :      !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.back_white : !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.back_black : !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.back_gray  : !VAR.DataSwath.OP_CHANGED = 1
   event.id EQ dataswath_struct.back_trans : !VAR.DataSwath.OP_CHANGED = 1
   
   ;-----------------------------------------------------------------------
   ; Process dialog contents when user accepts all changes. If no changes
   ; were made, no need to recompute anything.
   ;-----------------------------------------------------------------------
   
   event.id EQ dataswath_struct.redraw_btn : BEGIN
     IF (event.select EQ 1) THEN BEGIN ; here if mouse button was depressed
       IF (~ !VAR.DataSwath.OP_CHANGED) THEN BEGIN
         ResetFrame, dataswath_struct.State, !VAR.WORK_WNDW, 1
       ENDIF ELSE BEGIN

         WIDGET_CONTROL, dataswath_struct.min_field, GET_VALUE=dataval1
         bound_val = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinAbs
         IF (dataval1 LT bound_val) THEN BEGIN
            mssg = 'Your Minimum value entry must be at least ' + $
                   STRING(FORMAT='(F6.1)', bound_val) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, dataswath_struct.min_field, SET_VALUE= $
                            !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinData
            GOTO, return_point
         ENDIF
         !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinDlg = dataval1
       
         WIDGET_CONTROL, dataswath_struct.max_field, GET_VALUE=dataval2
         bound_val = !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxAbs
         IF (dataval2 GT bound_val) THEN BEGIN
            mssg = 'Your Maximum value entry must be no more than ' + $
                   STRING(FORMAT='(F6.1)', bound_val) + '. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
            WIDGET_CONTROL, dataswath_struct.max_field, SET_VALUE= $
                            !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxData
            GOTO, return_point
         ENDIF
         !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxDlg = dataval2
   
         IF (dataval2 LT dataval1) THEN BEGIN
            mssg = 'Your Minimum value entry must be less than ' + $
                   'your Maximum value entry. Try again.'
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Bad order')
            WIDGET_CONTROL, dataswath_struct.max_field, $
                            SET_VALUE=dataval1 + ABS(dataval1 * 0.1)
            GOTO, return_point
         ENDIF
   
         onoff = WIDGET_INFO(dataswath_struct.show_sep, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_IN_NEW_WNDW
         onoff = WIDGET_INFO(dataswath_struct.show_anm, /BUTTON_SET)
         IF (onoff) THEN !VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_ON_ANIM_WNDW
     
         IF (!VAR.DataSwath.SHOW EQ !KON.DataRgn.SHOW_ON_ANIM_WNDW) THEN BEGIN
            WIDGET_CONTROL, dataswath_struct.x_pos, GET_VALUE=dataval
            bound_val = 65
            IF (dataval LT bound_val) THEN BEGIN
               mssg = 'Your X-position entry must be at least ' + $
                      STRTRIM(STRING(bound_val),2) + '. Try again.'
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
               WIDGET_CONTROL, dataswath_struct.x_pos, SET_VALUE=bound_val
               GOTO, return_point
            ENDIF
            bound_val = 1958
            IF (dataval GT bound_val) THEN BEGIN
               mssg = 'Your X-position entry must be no more than ' + $
                      STRTRIM(STRING(bound_val),2) + '. Try again.'
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
               WIDGET_CONTROL, dataswath_struct.x_pos, SET_VALUE=bound_val
               GOTO, return_point
            ENDIF
            !VAR.DataSwath.POS_X = dataval
    
            WIDGET_CONTROL, dataswath_struct.y_pos, GET_VALUE=dataval
            bound_val = 290
            IF (dataval LT bound_val) THEN BEGIN
               mssg = 'Your Y-position entry must be at least ' + $
                      STRTRIM(STRING(bound_val),2) + '. Try again.'
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
               WIDGET_CONTROL, dataswath_struct.y_pos, SET_VALUE=bound_val
               GOTO, return_point
            ENDIF
            whichorbit = (!VAR.DataSwath.CURRENT_WNDW GT 9)
            bound_val = CoordStruct.(whichorbit).NumBlk * 512 - 15
            IF (dataval GT bound_val) THEN BEGIN
               mssg = 'Your Y-position entry must be no more than ' + $
                      STRTRIM(STRING(bound_val),2) + '. Try again.'
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR, TITLE='Out of bounds')
               WIDGET_CONTROL, dataswath_struct.y_pos, SET_VALUE=bound_val
               GOTO, return_point
            ENDIF
            !VAR.DataSwath.POS_Y = dataval
          ENDIF
   
          onoff = WIDGET_INFO(dataswath_struct.back_white, /BUTTON_SET)
          IF (onoff) THEN !VAR.DataSwath.BKGRND_COLOR = !KON.DataRgn.COLOR_WHITE
          onoff = WIDGET_INFO(dataswath_struct.back_black, /BUTTON_SET)
          IF (onoff) THEN !VAR.DataSwath.BKGRND_COLOR = !KON.DataRgn.COLOR_BLACK
          onoff = WIDGET_INFO(dataswath_struct.back_gray, /BUTTON_SET)
          IF (onoff) THEN !VAR.DataSwath.BKGRND_COLOR = !KON.DataRgn.COLOR_GRAY
          onoff = WIDGET_INFO(dataswath_struct.back_trans, /BUTTON_SET)
          IF (onoff) THEN !VAR.DataSwath.BKGRND_COLOR = !KON.DataRgn.COLOR_TRANSP
         
          ShowMISRSwathOptions, dataswath_struct.State, $
                                dataswath_struct.key_left, $
                                dataswath_struct.key_crnr, Retval
   
          ShowDataProducts, dataswath_struct.State
   
          !VAR.DataSwath.OP_CHANGED = 0
       
       ENDELSE
       
     ENDIF ELSE BEGIN ; here if mouse button was released
        ResetFrame, dataswath_struct.State, !VAR.DataSwath.CURRENT_WNDW, 1
     ENDELSE 
   END
   
   ;-----------------------------------------------------------------------
   ; User wants to exit. No more changes.
   ;-----------------------------------------------------------------------
   
   event.id EQ dataswath_struct.exit_button : BEGIN
      DestroyColorKey, 2  ; this also destroys this dialog window
      RedrawWindow, dataswath_struct.State, !VAR.DataSwath.CURRENT_WNDW
      RETURN
   END
   
ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

return_point:
WIDGET_CONTROL, event.top, SET_UVALUE=dataswath_struct, /NO_COPY
   
END ; MISRSwathOptions_eh

;**************************************************************************
PRO Op_MISRSwathOptions_gui, State
;**************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Discrete swath products must not show the dialog box, because they cannot
; be changed.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_LANDH2O_MASK OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_BIOME_MAP OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_STER_SDCM OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDZ_CLDMSK OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLDC_SDCM OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLD_CLDMSK OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLASS_SMOKE OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLASS_DUST OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLASS_CLOUD OR $
    !VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_CLASS_CLEAR) THEN RETURN

;---------------------------------------------------------------------------
; Don't create a dialog if there is already one on the screen.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.COLORKEY_DLG_ID GT 0) THEN BEGIN
   mssg = ['The "Select MISR Product Display Options"', $
           'dialog box is already displayed.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Don't create a dialog there is no data product selected.
;---------------------------------------------------------------------------

IF (!VAR.DataSwath.DATA_TYPE EQ !KON.DataProd.TYPE_NONE) THEN BEGIN
   mssg = ['You must select a Product type from the "Data Menu" before you', $
           'can use the "Select MISR Product Display Options"dialog box.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; If the Data REGION ! dialog box is displayed, delete it first. We don't want
; both dialogs showing concurrently.
;---------------------------------------------------------------------------

IF (!VAR.DataRgn.COLORKEY_DLG_ID GT 0) THEN BEGIN
   !VAR.DataRgn.Show = !KON.DataRgn.SHOW_DO_NOT
   DestroyColorKey, 1  ; this also destroys its dialog window
   RedrawWindow, State, State.curframe
ENDIF

;---------------------------------------------------------------------------
; If this is the first time it's being shown locate it near the left center
; of the current viewport.
;---------------------------------------------------------------------------

!VAR.DataSwath.CURRENT_WNDW = State.curframe

IF (!VAR.DataSwath.POS_X LT 0 OR !VAR.DataSwath.POS_Y LT 0) THEN BEGIN
   WIDGET_CONTROL, State.wDrawWindow, GET_DRAW_VIEW=viewport
   viewport_size = WIDGET_INFO(State.wDrawWindow, /GEOMETRY)
   !VAR.DataSwath.POS_X = viewport[0] + 150
   !VAR.DataSwath.POS_Y = viewport[1] + viewport_size.ysize / 2 + $
                          !VAR.DataSwath.SIZE_Y / 2
ENDIF

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base_top = WIDGET_BASE(TITLE='Select MISR Product Display Options', $
                       GROUP_LEADER=State.wAnimateBase, /FLOATING, /COLUMN, $
                       /TLB_KILL_REQUEST_EVENTS)
!VAR.DataSwath.COLORKEY_DLG_ID = base_top

base_2nd = WIDGET_BASE(base_top, /ROW)
base_1a  = WIDGET_BASE(base_2nd, /COLUMN)

label_1a = WIDGET_LABEL(base_1a, /ALIGN_LEFT, VALUE='MISR product options:')
base_1b   = WIDGET_BASE(base_1a, /COLUMN, /ALIGN_CENTER, /FRAME)

prod_type = 'Data type: ' + !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].Field
label_1b = WIDGET_LABEL(base_1b, /ALIGN_LEFT, VALUE=prod_type)
min_field  = CW_FIELD(base_1b, /FLOAT, /ROW, XSIZE=20, /ALL_EVENTS, $
                      TITLE='Minimum value')
max_field  = CW_FIELD(base_1b, /FLOAT, /ROW, XSIZE=20, /ALL_EVENTS, $
                      TITLE='Maximum value')
data_units = WIDGET_LABEL(base_1b, /ALIGN_LEFT, VALUE='Data units:    ' + $
                          !VAR.DataProd[!VAR.DataSwath.DATA_TYPE].UNITS)
label_1x = WIDGET_LABEL(base_1b, /ALIGN_CENTER, VALUE=' ', YSIZE=8)
reset_btn  = WIDGET_BUTTON(base_1b, VALUE='Reset Minimum/Maximum')
   
label_1x = WIDGET_LABEL(base_1a, /ALIGN_CENTER, VALUE=' ', YSIZE=8)

base_2g = WIDGET_BASE(base_1a, /COLUMN, /ALIGN_CENTER)
base_2h = WIDGET_BASE(base_2g, /ROW, /ALIGN_CENTER)
redraw_btn  = WIDGET_BUTTON(base_2h, VALUE='Redraw', /PUSHBUTTON_EVENTS)
exit_button = WIDGET_BUTTON(base_2h, VALUE=' Exit ')

base_2a  = WIDGET_BASE(base_2nd, /COLUMN)
label_2a = WIDGET_LABEL(base_2a, /ALIGN_LEFT, VALUE='Color key options:')

base_2b  = WIDGET_BASE(base_2a, /COLUMN, /ALIGN_CENTER, /FRAME)
label_2b = WIDGET_LABEL(base_2b, /ALIGN_CENTER, VALUE='Display location')
base_2c  = WIDGET_BASE(base_2b, /COLUMN, /ALIGN_CENTER, /EXCLUSIVE)
show_sep = WIDGET_BUTTON(base_2c, VALUE='Draw color key in separate window  ')
show_anm = WIDGET_BUTTON(base_2c, VALUE='Draw color key on camera image')

base_2d  = WIDGET_BASE(base_2a, /COLUMN, /FRAME)
label_2d = WIDGET_LABEL(base_2d, /ALIGN_CENTER, $
                        VALUE='"Pixel x/y:" coords of top right corner ')
base_3d  = WIDGET_BASE(base_2d, /ROW)
x_pos = CW_FIELD(base_3d, /INTEGER, /ROW, VALUE=!VAR.DataSwath.POS_X, $
                 XSIZE=5, /ALL_EVENTS, TITLE='X coord')
y_pos = CW_FIELD(base_3d, /INTEGER, /ROW, VALUE=!VAR.DataSwath.POS_Y, $
                 XSIZE=5, /ALL_EVENTS, TITLE='Y coord')
   
base_2e  = WIDGET_BASE(base_2a, /COLUMN, /ALIGN_CENTER, /FRAME)
label_2e = WIDGET_LABEL(base_2e, /ALIGN_CENTER, $
                        VALUE='Background color')
base_2f    = WIDGET_BASE(base_2e, /ALIGN_CENTER, /ROW, /EXCLUSIVE)
back_white = WIDGET_BUTTON(base_2f, VALUE='White')
back_black = WIDGET_BUTTON(base_2f, VALUE='Black')
back_gray  = WIDGET_BUTTON(base_2f, VALUE='Gray')
back_trans = WIDGET_BUTTON(base_2f, VALUE='Transp ')

;---------------------------------------------------------------------------
; Set widget states.
;---------------------------------------------------------------------------

WIDGET_CONTROL, min_field, $
   SET_VALUE=!VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MinData
WIDGET_CONTROL, max_field, $
   SET_VALUE=!VAR.DataProd[!VAR.DataSwath.DATA_TYPE].MaxData
   
arr = [show_sep, show_anm]
WIDGET_CONTROL, arr[!VAR.DataSwath.Show-1], SET_BUTTON=1
IF (!VAR.DataSwath.Show EQ !KON.DataRgn.SHOW_IN_NEW_WNDW) THEN BEGIN
   IF (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_TRANSP) THEN $
      WIDGET_CONTROL, back_white, SET_BUTTON=1
   WIDGET_CONTROL, back_trans, SENSITIVE=0
   WIDGET_CONTROL, x_pos, SENSITIVE=0
   WIDGET_CONTROL, y_pos, SENSITIVE=0
ENDIF ELSE BEGIN
   WIDGET_CONTROL, back_trans, SENSITIVE=1
   WIDGET_CONTROL, x_pos, SENSITIVE=1
   WIDGET_CONTROL, y_pos, SENSITIVE=1
   WIDGET_CONTROL, x_pos, SET_VALUE=!VAR.DataSwath.POS_X
   WIDGET_CONTROL, y_pos, SET_VALUE=!VAR.DataSwath.POS_Y
ENDELSE

IF (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_WHITE) THEN $
   WIDGET_CONTROL, back_white, SET_BUTTON=1
IF (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_BLACK) THEN $
   WIDGET_CONTROL, back_black, SET_BUTTON=1
IF (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_GRAY) THEN $
   WIDGET_CONTROL, back_gray,  SET_BUTTON=1
IF (!VAR.DataSwath.BKGRND_COLOR EQ !KON.DataRgn.COLOR_TRANSP) THEN $
   WIDGET_CONTROL, back_trans,  SET_BUTTON=1
   
;---------------------------------------------------------------------------
; Get the size of the dialog box and the location for its upper-right corner.
; Also get the lower-left corner location for the color bar if it is shown
; in a separate window
;---------------------------------------------------------------------------

widget_geom = WIDGET_INFO(base_top, /GEOMETRY)
xsize = widget_geom.SCR_XSIZE + widget_geom.MARGIN * 2
ysize = widget_geom.SCR_YSIZE + widget_geom.MARGIN * 2
dialog_left = !KON.Misc.ScreenX / 2 - xsize / 2
dialog_top = !KON.Misc.ScreenY - 35 - ysize

key_left = dialog_left + xsize + 50
key_crnr = (!KON.Misc.MINX_PLATFORM EQ 1) ? 25 : dialog_top + 25

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

dataswath_struct = { data_type   : !VAR.DataSwath.DATA_TYPE, $
                     min_field   : min_field,   $
                     max_field   : max_field,   $
                     data_units  : data_units,  $
                     reset_btn   : reset_btn,   $
                     show_sep    : show_sep,    $
                     show_anm    : show_anm,    $
                     x_pos       : x_pos,       $
                     y_pos       : y_pos,       $
                     back_white  : back_white,  $
                     back_black  : back_black,  $
                     back_gray   : back_gray,   $
                     back_trans  : back_trans,  $
                     redraw_btn  : redraw_btn,  $
                     exit_button : exit_button, $
                     key_left    : key_left,    $
                     key_crnr    : key_crnr,    $
                     state       : state }
   
;---------------------------------------------------------------------------
; Store the structure in widget and realize it. All the data values are
; stored into !VAR parameters in MISRSwathOptions_eh.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base_top, SET_UVALUE=dataswath_struct, XOFFSET=dialog_left, $
                YOFFSET=dialog_top, /NO_COPY
   
WIDGET_CONTROL, base_top, /REALIZE

XMANAGER, 'Op_MISRSwathOptions_gui', base_top, EVENT_HANDLER='MISRSwathOptions_eh'
   
END ; Op_MISRSwathOptions_gui

;***************************************************************************
PRO GetModvolcParams_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON modvolc_params, proj_name, beg_block, end_block, too_few_for_orbit, $
                       use_modis, beg_date, end_date, max_blks_per_load, $
                       cancel_it

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=modvolc_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   cancel_it = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ modvolc_struct.proj_text : BEGIN
   END
   event.id EQ modvolc_struct.beg_edit : BEGIN
   END
   event.id EQ modvolc_struct.end_edit : BEGIN
   END
   event.id EQ modvolc_struct.beg_date_edit : BEGIN
   END
   event.id EQ modvolc_struct.end_date_edit : BEGIN
   END
   event.id EQ modvolc_struct.toofew_edit : BEGIN
   END
   event.id EQ modvolc_struct.modis_btn : BEGIN
   END
   event.id EQ modvolc_struct.modvolc_btn : BEGIN
   END

   ;-------------------------------------------------------------
   ; Here if user clicked OK.
   ;-------------------------------------------------------------

   event.id EQ modvolc_struct.ok_button : BEGIN

      ;----------------------------------------------------------
      ; Test that the project name is not null.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modvolc_struct.proj_text, GET_VALUE=proj_name

      proj_name = STRTRIM(proj_name,2)
      IF (proj_name EQ '') THEN BEGIN
         mssg = 'You must enter a project name. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the begin block is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modvolc_struct.beg_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modvolc_struct.beg_edit, SET_VALUE=1
         mssg = ['The smallest valid block number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, modvolc_struct.beg_edit, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         mssg = ['The largest valid block number is 180.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      beg_block = temp

      ;----------------------------------------------------------
      ; Test that the end block is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modvolc_struct.end_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modvolc_struct.end_edit, SET_VALUE=1
         mssg = ['The smallest valid block number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, modvolc_struct.end_edit, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         mssg = ['The largest valid block number is 180.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      end_block = temp

      ;----------------------------------------------------------
      ; Test that the block order is OK.
      ;----------------------------------------------------------

      IF (beg_block GT end_block) THEN BEGIN
         mssg = ['The ending block number must be greater than', $
                 'the beginning block number. Try again.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the begin date is OK.
      ;----------------------------------------------------------

      date_time = SYSTIME()
      toks = STRSPLIT(date_time, ' ', /EXTRACT)
      year_str = toks[4]

      WIDGET_CONTROL, modvolc_struct.beg_date_edit, GET_VALUE=beg_date

      npos = STRPOS(beg_date, '--')
      IF (npos GE 0) THEN BEGIN
         mssg = 'The begin date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      toks = STRSPLIT(beg_date, '-', COUNT=numtok, /EXTRACT)
      GetJulianDay, beg_date, '12:00:00', 0, YMD, HMS, jul_day_beg
      IF (numtok NE 3) THEN BEGIN
         mssg = 'The begin date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF ELSE BEGIN
         yr = FIX(toks[0])
         IF (yr LT 2000 OR yr GT FIX(year_str)) THEN BEGIN
            mssg = 'The begin year must be between 2000 and ' + $
                    year_str + '. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         mo = FIX(toks[1])
         IF (mo LT 1 OR mo GT 12) THEN BEGIN
            mssg = 'The begin month must be between 1 and 12. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         da = FIX(toks[2])
         IF (da LT 1 OR da GT 31) THEN BEGIN
            mssg = 'The begin day must be between 1 and 31. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
      ENDELSE

      ;----------------------------------------------------------
      ; Test that the end date is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modvolc_struct.end_date_edit, GET_VALUE=end_date

      npos = STRPOS(end_date, '--')
      IF (npos GE 0) THEN BEGIN
         mssg = 'The end date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      toks = STRSPLIT(end_date, '-', COUNT=numtok, /EXTRACT)
      GetJulianDay, end_date, '12:00:00', 0, YMD, HMS, jul_day_end
      IF (numtok NE 3) THEN BEGIN
         mssg = 'The end date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF ELSE BEGIN
         yr = FIX(toks[0])
         IF (yr LT 2000 OR yr GT FIX(year_str)) THEN BEGIN
            mssg = 'The end year must be between 2000 and ' + $
                    year_str + '. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         mo = FIX(toks[1])
         IF (mo LT 1 OR mo GT 12) THEN BEGIN
            mssg = 'The end month must be between 1 and 12. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         da = FIX(toks[2])
         IF (da LT 1 OR da GT 31) THEN BEGIN
            mssg = 'The end day must be between 1 and 31. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
      ENDELSE

      ;----------------------------------------------------------
      ; Test that the date range is ordered correctly.
      ;----------------------------------------------------------

      IF (jul_day_beg GT jul_day_end) THEN BEGIN
         mssg = 'The end date must be later than the begin date. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      IF (jul_day_beg EQ jul_day_end) THEN BEGIN
         mssg = ['The begin date end date are identical.', $
                 'Are there 31 days in either month?', $
                 'Try again.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the number of fire pixels is at least 1.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modvolc_struct.toofew_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 999) THEN BEGIN
         WIDGET_CONTROL, modvolc_struct.toofew_edit, $
                         SET_VALUE=!SAV.Util.NumFirePixOrbit
         mssg = ['The minimum number of fire pixels must be ' + $
                 'between 1 and 999.', 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      too_few_for_orbit = temp

      ;----------------------------------------------------------
      ; Get the Modvolc or Modis button.
      ;----------------------------------------------------------

      modvolcval = WIDGET_INFO(modvolc_struct.modvolc_btn, /BUTTON_SET)
      modisval   = WIDGET_INFO(modvolc_struct.modis_btn,   /BUTTON_SET)

      IF (modvolcval EQ 1) THEN use_modis = 0
      IF (modisval   EQ 1) THEN use_modis = 1

      cancel_it = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ modvolc_struct.cancel_button : BEGIN
      cancel_it = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=modvolc_struct, /NO_COPY

END ; GetModvolcParams_eh

;***************************************************************************
PRO GetModvolcParams_gui, PlumeProjectName, FirstValidBlock, LastValidBlock, $
                          BegDate, EndDate, UseModisFirePix, MaxBlksPerLoad, $
                          NumFirePixOrbit, CancelIt
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON modvolc_params, proj_name, beg_block, end_block, too_few_for_orbit, $
                       use_modis, beg_date, end_date, max_blks_per_load, $
                       cancel_it

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, TITLE='Select ModVolc Parameters', $
                    /TLB_KILL_REQUEST_EVENTS)

base0x = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /FRAME)
descrip1 = WIDGET_LABEL(base0x, VALUE='Only those fire pixels that satisfy all the')
descrip2 = WIDGET_LABEL(base0x, VALUE='filtering criteria you provide here will be')
descrip3 = WIDGET_LABEL(base0x, VALUE='included in the output files.')
descrip4 = WIDGET_LABEL(base0,  VALUE=' ')


base1 = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /FRAME)
proj_text = CW_FIELD(base1, VALUE=PlumeProjectName, XSIZE=19, /STRING, $
                     TITLE='Project Name:')
projlabel = WIDGET_LABEL(base1, VALUE='        (no spaces or underscores)')

base2a = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /FRAME)
datelabel = WIDGET_LABEL(base2a, VALUE='Date range (YYYY-MM-DD)')
base2 = WIDGET_BASE(base2a, /ROW, /ALIGN_CENTER)
beg_date_edit = CW_FIELD(base2, VALUE=BegDate, XSIZE=10, /STRING, $
                         TITLE='Begin:')
end_date_edit = CW_FIELD(base2, VALUE=EndDate, XSIZE=10, /STRING, $
                         TITLE='End: ')

base3a = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /FRAME)
blocklabel = WIDGET_LABEL(base3a, VALUE='MISR block range (1-180)')
base3 = WIDGET_BASE(base3a, /ROW, /ALIGN_CENTER)
beg_edit = CW_FIELD(base3, VALUE=FirstValidBlock, XSIZE=3, /INTEGER, $
                    TITLE='First Block:')
end_edit = CW_FIELD(base3, VALUE=LastValidBlock, XSIZE=3, /INTEGER, $
                    TITLE=' Last Block: ')

base4 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER, /FRAME)
base4a = WIDGET_BASE(base4, /COLUMN, /ALIGN_CENTER)
base4b = WIDGET_BASE(base4, /ROW, /ALIGN_CENTER)
minlabel1 = WIDGET_LABEL(base4a, VALUE='Minimum number of fire pixels in')
minlabel2 = WIDGET_LABEL(base4a, VALUE='an orbit required for orbit to be')
minlabel3 = WIDGET_LABEL(base4a, VALUE='accepted (1-999):')
toofew_edit = CW_FIELD(base4b, VALUE=NumFirePixOrbit, XSIZE=3, /INTEGER, $
                       TITLE='')

base5 = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER, /FRAME)
pixellabel = WIDGET_LABEL(base5, /ALIGN_CENTER, $
                          VALUE='       Use Fire Pixels from source:       ')
base5a = WIDGET_BASE(base5, /COLUMN, /ALIGN_CENTER, /EXCLUSIVE)
modvolc_btn = WIDGET_BUTTON(base5a, VALUE='ModVolc (nothing more required)')
modis_btn   = WIDGET_BUTTON(base5a, VALUE='Modis (must download Modis data)')
base5b = WIDGET_BASE(base5, /COLUMN, /ALIGN_LEFT)
mod_labela = WIDGET_LABEL(base5b, VALUE=' (To get fire radiative power, you must')
mod_labelb = WIDGET_LABEL(base5b, VALUE=' select "Modis" and do more processing)')

base6a = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER)
base6 = WIDGET_BASE(base6a, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base6, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base6, VALUE='Cancel')

IF (UseModisFirePix EQ 0) THEN WIDGET_CONTROL, modvolc_btn, SET_BUTTON=1
IF (UseModisFirePix EQ 1) THEN WIDGET_CONTROL, modis_btn,   SET_BUTTON=1

cancel_it = 0
proj_name = PlumeProjectName
beg_block = FirstValidBlock
end_block = LastValidBlock
beg_date  = BegDate
end_date  = EndDate
use_modis = UseModisFirePix
max_blks_per_load = MaxBlksPerLoad
too_few_for_orbit = NumFirePixOrbit

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

modvolc_struct = { $
   proj_text     : proj_text, $
   beg_edit      : beg_edit, $
   end_edit      : end_edit, $
   beg_date_edit : beg_date_edit, $
   end_date_edit : end_date_edit, $
   toofew_edit   : toofew_edit, $
   modis_btn     : modis_btn, $
   modvolc_btn   : modvolc_btn, $
   ok_button     : ok_button, $
   cancel_button : cancel_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=modvolc_struct, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'GetModvolcParams_gui', base0, EVENT_HANDLER='GetModvolcParams_eh'

PlumeProjectName = proj_name
FirstValidBlock  = beg_block
LastValidBlock   = end_block
NumFirePixOrbit  = too_few_for_orbit
UseModisFirePix  = use_modis
BegDate          = beg_date
EndDate          = end_date
MaxBlksPerLoad   = max_blks_per_load
CancelIt         = cancel_it

;---------------------------------------------------------------------------
; If there are spaces or underscores in the project name, replace each with
; a dash.
;---------------------------------------------------------------------------

ispace = 0
WHILE (ispace NE -1) DO BEGIN
   ispace = STRPOS(PlumeProjectName, ' ', ispace)
   IF (ispace NE -1) THEN BEGIN
      STRPUT, PlumeProjectName, '-', ispace
      ispace = ispace + 1
   ENDIF
ENDWHILE

ispace = 0
WHILE (ispace NE -1) DO BEGIN
   ispace = STRPOS(PlumeProjectName, '_', ispace)
   IF (ispace NE -1) THEN BEGIN
      STRPUT, PlumeProjectName, '-', ispace
      ispace = ispace + 1
   ENDIF
ENDWHILE

;---------------------------------------------------------------------------
; Set the global parameters.
;---------------------------------------------------------------------------

!SAV.Util.ProjectName      = PlumeProjectName
!SAV.Util.FirstValidBlock  = FirstValidBlock
!SAV.Util.LastValidBlock   = LastValidBlock
!SAV.Util.NumFirePixOrbit  = NumFirePixOrbit
!SAV.Util.FirePixGetMethod = UseModisFirePix
!SAV.Util.BegDate          = BegDate
!SAV.Util.EndDate          = EndDate
!SAV.Util.MaxBlksPerLoad   = MaxBlksPerLoad

END ; GetModvolcParams_gui

;***************************************************************************
PRO GetMOD14Params_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON modis_params, proj_name, beg_block, end_block, beg_path, end_path, $
                     beg_date, end_date, max_blks, conf_thresh, $
                     min_pix_pow, min_blk_pow, min_grp_pow, min_grp_num, $
                     cancel_it

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=modis_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   cancel_it = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ modis_struct.proj_name_edit : BEGIN
   END
   event.id EQ modis_struct.beg_blk_edit : BEGIN
   END
   event.id EQ modis_struct.end_blk_edit : BEGIN
   END
   event.id EQ modis_struct.beg_path_edit : BEGIN
   END
   event.id EQ modis_struct.end_path_edit : BEGIN
   END
   event.id EQ modis_struct.beg_date_edit : BEGIN
   END
   event.id EQ modis_struct.end_date_edit : BEGIN
   END
   event.id EQ modis_struct.blkmax_edit : BEGIN
   END
   event.id EQ modis_struct.conf_edit : BEGIN
   END
   event.id EQ modis_struct.min_pix_pow_edit : BEGIN
   END
   event.id EQ modis_struct.min_blk_pow_edit : BEGIN
   END
   event.id EQ modis_struct.min_grp_pow_edit : BEGIN
   END
   event.id EQ modis_struct.toofew_edit : BEGIN
   END

   ;-------------------------------------------------------------
   ; Here if user clicked OK.
   ;-------------------------------------------------------------

   event.id EQ modis_struct.ok_button : BEGIN

      ;----------------------------------------------------------
      ; Test that the project name is not null.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.proj_name_edit, GET_VALUE=proj_name

      proj_name = STRTRIM(proj_name,2)
      IF (proj_name EQ '') THEN BEGIN
         mssg = 'You must enter a project name. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, retryit
      ENDIF

      ;----------------------------------------------------------
      ; Test that the begin block is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.beg_blk_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modis_struct.beg_blk_edit, SET_VALUE=1
         mssg = ['The smallest valid block number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, modis_struct.beg_blk_edit, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         mssg = ['The largest valid block number is 180.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      beg_block = temp

      ;----------------------------------------------------------
      ; Test that the end block is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.end_blk_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modis_struct.end_blk_edit, SET_VALUE=1
         mssg = ['The smallest valid block number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         WIDGET_CONTROL, modis_struct.end_blk_edit, $
                         SET_VALUE=!KON.Instr.NUM_BLOCKS
         mssg = ['The largest valid block number is 180.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      end_block = temp

      ;----------------------------------------------------------
      ; Test that the block range is OK.
      ;----------------------------------------------------------

      IF (beg_block GT end_block) THEN BEGIN
         mssg = ['The ending block number must be greater than', $
                 'the beginning block number. Try again.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the begin path is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.beg_path_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modis_struct.beg_path_edit, SET_VALUE=1
         mssg = ['The smallest valid path number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_PATHS) THEN BEGIN
         WIDGET_CONTROL, modis_struct.beg_path_edit, $
                         SET_VALUE=!KON.Instr.NUM_PATHS
         mssg = ['The largest valid path number is 233.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      beg_path = temp

      ;----------------------------------------------------------
      ; Test that the end path is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.end_path_edit, GET_VALUE=temp

      IF (temp LT 1) THEN BEGIN
         WIDGET_CONTROL, modis_struct.end_path_edit, SET_VALUE=1
         mssg = ['The smallest valid path number is 1.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp GT !KON.Instr.NUM_PATHS) THEN BEGIN
         WIDGET_CONTROL, modis_struct.end_path_edit, $
                         SET_VALUE=!KON.Instr.NUM_PATHS
         mssg = ['The largest valid path number is 233.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      end_path = temp

      ;----------------------------------------------------------
      ; Test that the path range is OK. It's OK for the end path
      ; to be smaller than the begin path to accommodate spanning
      ; the 1/233 "joint".
      ;----------------------------------------------------------

      IF (beg_path LT 1 OR beg_path GT !KON.Instr.NUM_PATHS OR $
          end_path LT 1 OR end_path GT !KON.Instr.NUM_PATHS) THEN BEGIN
         mssg = 'The path range must be between 1 and 233. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the begin date is OK.
      ;----------------------------------------------------------

      date_time = SYSTIME()
      toks = STRSPLIT(date_time, ' ', /EXTRACT)
      year_str = toks[4]

      WIDGET_CONTROL, modis_struct.beg_date_edit, GET_VALUE=beg_date

      npos = STRPOS(beg_date, '--')
      IF (npos GE 0) THEN BEGIN
         mssg = 'The begin date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      toks = STRSPLIT(beg_date, '-', COUNT=numtok, /EXTRACT)
      GetJulianDay, beg_date, '12:00:00', 0, YMD, HMS, jul_day_beg
      IF (numtok NE 3) THEN BEGIN
         mssg = 'The begin date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF ELSE BEGIN
         yr = FIX(toks[0])
         IF (yr LT 2000 OR yr GT FIX(year_str)) THEN BEGIN
            mssg = 'The begin year must be between 2000 and ' + $
                    year_str + '. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         mo = FIX(toks[1])
         IF (mo LT 1 OR mo GT 12) THEN BEGIN
            mssg = 'The begin month must be between 1 and 12. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         da = FIX(toks[2])
         IF (da LT 1 OR da GT 31) THEN BEGIN
            mssg = 'The begin day must be between 1 and 31. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
      ENDELSE

      ;----------------------------------------------------------
      ; Test that the end date is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.end_date_edit, GET_VALUE=end_date

      npos = STRPOS(end_date, '--')
      IF (npos GE 0) THEN BEGIN
         mssg = 'The end date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      toks = STRSPLIT(end_date, '-', COUNT=numtok, /EXTRACT)
      GetJulianDay, end_date, '12:00:00', 0, YMD, HMS, jul_day_end
      IF (numtok NE 3) THEN BEGIN
         mssg = 'The end date format is incorrect. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF ELSE BEGIN
         yr = FIX(toks[0])
         IF (yr LT 2000 OR yr GT FIX(year_str)) THEN BEGIN
            mssg = 'The begin year must be between 2000 and ' + $
                    year_str + '. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         mo = FIX(toks[1])
         IF (mo LT 1 OR mo GT 12) THEN BEGIN
            mssg = 'The end month must be between 1 and 12. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
         da = FIX(toks[2])
         IF (da LT 1 OR da GT 31) THEN BEGIN
            mssg = 'The end day must be between 1 and 31. Try again.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            BREAK
         ENDIF
      ENDELSE

      ;----------------------------------------------------------
      ; Test that the date range is ordered correctly.
      ;----------------------------------------------------------

      IF (jul_day_beg GT jul_day_end) THEN BEGIN
         mssg = 'The end date must be later than the begin date. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      IF (jul_day_beg EQ jul_day_end) THEN BEGIN
         mssg = ['The begin date end date are identical.', $
                 'Are there 31 days in either month?', $
                 'Try again.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         BREAK
      ENDIF

      ;----------------------------------------------------------
      ; Test that the max number of blocks is in range.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.blkmax_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 15) THEN BEGIN
         WIDGET_CONTROL, modis_struct.blkmax_edit, $
                         SET_VALUE=!SAV.Util.MaxBlksPerLoad
         mssg = ['The max number of blocks must be between ' + $
                 '1 and 15.', 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      max_blks = temp

      ;----------------------------------------------------------
      ; Test that the confidence threshold is in range.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.conf_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 100) THEN BEGIN
         WIDGET_CONTROL, modis_struct.conf_edit, $
                         SET_VALUE=!SAV.Util.ConfThresh
         mssg = ['The confidence interval must be between 1 and 100.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      conf_thresh = temp

      ;----------------------------------------------------------
      ; Test that the minimum pixel power is in range.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.min_pix_pow_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 999) THEN BEGIN
         WIDGET_CONTROL, modis_struct.min_pix_pow_edit, $
                         SET_VALUE=!SAV.Util.MinPixPower
         mssg = ['The minimum power per pixel must be between 1 and 999.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      min_pix_pow = temp

      ;----------------------------------------------------------
      ; Test that the minimum block power is in range.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.min_blk_pow_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 999) THEN BEGIN
         WIDGET_CONTROL, modis_struct.min_blk_pow_edit, $
                         SET_VALUE=!SAV.Util.MinBlkPower
         mssg = ['The minimum power per block must be between 1 and 999.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      min_blk_pow = temp

      ;----------------------------------------------------------
      ; Test that the minimum block-group power is in range.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.min_grp_pow_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 999) THEN BEGIN
         WIDGET_CONTROL, modis_struct.min_grp_pow_edit, $
                         SET_VALUE=!SAV.Util.MinGrpPower
         mssg = ['The minimum power per block-group must be between 1 and 999.', $
                 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      min_grp_pow = temp

      ;----------------------------------------------------------
      ; Test that the number of fire pixels is OK.
      ;----------------------------------------------------------

      WIDGET_CONTROL, modis_struct.toofew_edit, GET_VALUE=temp

      IF (temp LT 1 OR temp GT 999) THEN BEGIN
         WIDGET_CONTROL, modis_struct.toofew_edit, $
                         SET_VALUE=!SAV.Util.NumFirePixGroup
         mssg = ['The minimum number of fire pixels must be ' + $
                 'between 1 and 999.', 'Value has been reset.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      min_grp_num = temp

      cancel_it = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ modis_struct.cancel_button : BEGIN
      cancel_it = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

retryit:
WIDGET_CONTROL, event.top, SET_UVALUE=modis_struct, /NO_COPY

END ; GetMOD14Params_eh

;***************************************************************************
PRO GetMOD14Params_gui, CancelIt
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON modis_params, proj_name, beg_block, end_block, beg_path, end_path, $
                     beg_date, end_date, max_blks, conf_thresh, $
                     min_pix_pow, min_blk_pow, min_grp_pow, min_grp_num, $
                     cancel_it

;---------------------------------------------------------------------------
; Initialize dialog box values with global parameters.
;---------------------------------------------------------------------------

cancel_it   = 0
proj_name   = !SAV.Util.ProjectName
beg_block   = !SAV.Util.FirstValidBlock
end_block   = !SAV.Util.LastValidBlock
beg_path    = !SAV.Util.FirstValidPath
end_path    = !SAV.Util.LastValidPath
beg_date    = !SAV.Util.BegDate
end_date    = !SAV.Util.EndDate
max_blks    = !SAV.Util.MaxBlksPerLoad
conf_thresh = !SAV.Util.ConfThresh
min_pix_pow = !SAV.Util.MinPixPower
min_blk_pow = !SAV.Util.MinBlkPower
min_grp_pow = !SAV.Util.MinGrpPower
min_grp_num = !SAV.Util.NumFirePixGroup

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

base0 = WIDGET_BASE(/COLUMN, /TLB_KILL_REQUEST_EVENTS, $
                    TITLE='Select MODIS Fire Pixel Filter Parameters')

baseA    = WIDGET_BASE(base0, /COLUMN, /ALIGN_CENTER)
base00   = WIDGET_BASE(base0, /ROW,    /ALIGN_CENTER)
base0x   = WIDGET_BASE(baseA, /COLUMN, /ALIGN_CENTER, /FRAME)
descrip1 = WIDGET_LABEL(base0x, VALUE='   Only those fire pixels that ' + $
                        'satisfy all the filtering criteria   ')
descrip3 = WIDGET_LABEL(base0x, VALUE='specified here will be included in ' + $
                        'the output files.')
descrip4 = WIDGET_LABEL(base0x, VALUE='Fire power is fire radiative power ' + $
                        'measured in megawatts.')

baseB = WIDGET_BASE(base00, /COLUMN, /ALIGN_LEFT)
baseC = WIDGET_BASE(base00, /COLUMN, /ALIGN_LEFT)

base1 = WIDGET_BASE(baseB, /COLUMN, /ALIGN_CENTER, /FRAME)
proj_name_edit = CW_FIELD(base1, VALUE=proj_name, XSIZE=24, /STRING, $
                          TITLE=' Project Name: ')
projlabel2 = WIDGET_LABEL(base1, VALUE='        (no spaces or underscores)')

base2a = WIDGET_BASE(baseB, /COLUMN, /ALIGN_CENTER, /FRAME)
datelabel = WIDGET_LABEL(base2a, VALUE='Date range (YYYY-MM-DD)')
base2 = WIDGET_BASE(base2a, /ROW, /ALIGN_CENTER)
beg_date_edit = CW_FIELD(base2, XSIZE=10, /STRING, TITLE=' Begin:', VALUE=beg_date)
end_date_edit = CW_FIELD(base2, XSIZE=10, /STRING, TITLE=' End:',   VALUE=end_date)

base3a = WIDGET_BASE(baseB, /COLUMN, /ALIGN_CENTER, /FRAME)
blocklabel = WIDGET_LABEL(base3a, VALUE='MISR block range (1-180)')
base3 = WIDGET_BASE(base3a, /ROW, /ALIGN_CENTER)
beg_blk_edit = CW_FIELD(base3, VALUE=beg_block, XSIZE=3, /INTEGER, $
                        TITLE=' First Block:')
end_blk_edit = CW_FIELD(base3, VALUE=end_block, XSIZE=3, /INTEGER, $
                        TITLE='  Last Block:')

base3b = WIDGET_BASE(baseB, /COLUMN, /ALIGN_CENTER, /FRAME)
pathlabel1 = WIDGET_LABEL(base3b, VALUE='MISR path range (1-233)')
base3c = WIDGET_BASE(base3b, /ROW, /ALIGN_CENTER)
beg_path_edit = CW_FIELD(base3c, VALUE=beg_path, XSIZE=3, /INTEGER, $
                         TITLE='Eastern Path:')
end_path_edit = CW_FIELD(base3c, VALUE=end_path, XSIZE=3, /INTEGER, $
                         TITLE='Western Path:')
pathlabel2 = WIDGET_LABEL(base3b, VALUE='To span path 233, put larger path in East')

base4a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base4 = WIDGET_BASE(base4a, /COLUMN, /ALIGN_LEFT)
minlabel41 = WIDGET_LABEL(base4, /ALIGN_LEFT, VALUE='Max number of MISR blocks your computer')
minlabel42 = WIDGET_LABEL(base4, /ALIGN_LEFT, VALUE='can load into MINX - this defines the')
minlabel43 = WIDGET_LABEL(base4, /ALIGN_LEFT, VALUE='block-group size (1-20):')
blkmax_edit = CW_FIELD(base4a, VALUE=max_blks, XSIZE=2, /INTEGER, TITLE='')

base5a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base5 = WIDGET_BASE(base5a, /COLUMN, /ALIGN_LEFT)
minlabel51 = WIDGET_LABEL(base5, /ALIGN_LEFT, VALUE='Minimum MODIS confidence level (in %) ')
minlabel52 = WIDGET_LABEL(base5, /ALIGN_LEFT, VALUE='needed to accept a fire pixel (1-100):')
conf_edit = CW_FIELD(base5a, VALUE=conf_thresh, XSIZE=3, /INTEGER, TITLE='')

base6a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base6 = WIDGET_BASE(base6a, /COLUMN, /ALIGN_LEFT)
minlabel61 = WIDGET_LABEL(base6, /ALIGN_LEFT, VALUE='Minimum fire power needed to accept a ')
minlabel63 = WIDGET_LABEL(base6, /ALIGN_LEFT, VALUE='fire pixel (1-999 MWatt):')
min_pix_pow_edit = CW_FIELD(base6a, VALUE=min_pix_pow, XSIZE=3, /INTEGER, TITLE='')

base7a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base7 = WIDGET_BASE(base7a, /COLUMN, /ALIGN_LEFT)
minlabel71 = WIDGET_LABEL(base7, /ALIGN_LEFT, VALUE='Minimum cumulative fire power needed  ')
minlabel73 = WIDGET_LABEL(base7, /ALIGN_LEFT, VALUE='to accept a MISR block (1-999 MWatt):')
min_blk_pow_edit = CW_FIELD(base7a, VALUE=min_blk_pow, XSIZE=3, /INTEGER, TITLE='')

base8a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base8 = WIDGET_BASE(base8a, /COLUMN, /ALIGN_LEFT)
minlabel81 = WIDGET_LABEL(base8, /ALIGN_LEFT, VALUE='Minimum cumulative fire power needed')
minlabel83 = WIDGET_LABEL(base8, /ALIGN_LEFT, VALUE='to accept a block-group (1-999 MWatt):')
min_grp_pow_edit = CW_FIELD(base8a, VALUE=min_grp_pow, XSIZE=3, /INTEGER, TITLE='')

base9a = WIDGET_BASE(baseC, /ROW, /ALIGN_CENTER, /FRAME)
base9 = WIDGET_BASE(base9a, /COLUMN, /ALIGN_LEFT)
minlabel91 = WIDGET_LABEL(base9, /ALIGN_LEFT, VALUE='Minimum number of fire pixels needed  ')
minlabel92 = WIDGET_LABEL(base9, /ALIGN_LEFT, VALUE='to accept a block-group (1-999):')
toofew_edit = CW_FIELD(base9a, VALUE=min_grp_num, XSIZE=3, /INTEGER, TITLE='')

labelOC = WIDGET_LABEL(baseB, VALUE=' ')
base10 = WIDGET_BASE(baseB, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base10, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base10, VALUE='Cancel')

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

modis_struct = { $
   proj_name_edit   : proj_name_edit,   $
   beg_blk_edit     : beg_blk_edit,     $
   end_blk_edit     : end_blk_edit,     $
   beg_path_edit    : beg_path_edit,    $
   end_path_edit    : end_path_edit,    $
   beg_date_edit    : beg_date_edit,    $
   end_date_edit    : end_date_edit,    $
   blkmax_edit      : blkmax_edit,      $
   conf_edit        : conf_edit,        $
   min_pix_pow_edit : min_pix_pow_edit, $
   min_blk_pow_edit : min_blk_pow_edit, $
   min_grp_pow_edit : min_grp_pow_edit, $
   toofew_edit      : toofew_edit,      $
   ok_button        : ok_button,        $
   cancel_button    : cancel_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=modis_struct, XOFFSET=700, YOFFSET=250, $
                /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'GetMOD14Params_gui', base0, EVENT_HANDLER='GetMOD14Params_eh'

;---------------------------------------------------------------------------
; If there are spaces or underscores in project name, replace with dashes.
;---------------------------------------------------------------------------

ispace = 0
WHILE (ispace NE -1) DO BEGIN  
   ispace = STRPOS(proj_name, ' ', ispace)  
   IF (ispace NE -1) THEN BEGIN   
      STRPUT, proj_name, '-', ispace
      ispace = ispace + 1  
   ENDIF 
ENDWHILE  

ispace = 0
WHILE (ispace NE -1) DO BEGIN  
   ispace = STRPOS(proj_name, '_', ispace)  
   IF (ispace NE -1) THEN BEGIN   
      STRPUT, proj_name, '-', ispace
      ispace = ispace + 1  
   ENDIF 
ENDWHILE  

;---------------------------------------------------------------------------
; Copy the results back into global parameters.
;---------------------------------------------------------------------------

!SAV.Util.ProjectName     = proj_name
!SAV.Util.FirstValidBlock = beg_block
!SAV.Util.LastValidBlock  = end_block
!SAV.Util.FirstValidPath  = beg_path
!SAV.Util.LastValidPath   = end_path
!SAV.Util.BegDate         = beg_date
!SAV.Util.EndDate         = end_date
!SAV.Util.MaxBlksPerLoad  = max_blks
!SAV.Util.ConfThresh      = conf_thresh
!SAV.Util.MinPixPower     = min_pix_pow
!SAV.Util.MinBlkPower     = min_blk_pow
!SAV.Util.MinGrpPower     = min_grp_pow
!SAV.Util.NumFirePixGroup = min_grp_num

CancelIt = cancel_it

END ; GetMOD14Params_gui

;***************************************************************************
PRO RegionDiskList_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON rgnlists, rgnlist_index

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=region_list_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   rgnlist_index = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ region_list_struct.rgnlist : BEGIN
      rgnlist_index = event.Index
   END
   event.id EQ region_list_struct.ok_button : BEGIN
      IF (rgnlist_index LT 0) THEN BEGIN
         mssg = 'You must select a region. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDELSE
   END
   event.id EQ region_list_struct.cancel_button : BEGIN
      rgnlist_index = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=region_list_struct, /NO_COPY

END ; RegionDiskList_eh

;***************************************************************************
PRO RegionDiskList_gui, Topbase, RegionList, RgnlistIndex
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON rgnlists, rgnlist_index

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

nregions = N_ELEMENTS(RegionList)
nlines = nregions < 15

base0 = WIDGET_BASE(/COLUMN, TITLE='Select Region to Re-digitize', $
                    /MODAL, GROUP_LEADER=WIDGET_INFO(TopBase,/PARENT), $
                    /TLB_KILL_REQUEST_EVENTS)
listname = WIDGET_LABEL(base0, /ALIGN_CENTER, VALUE='Region Choices')
rgnlist = WIDGET_LIST(base0, FRAME=1, VALUE=RegionList, YSIZE=nlines)

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE='Cancel')

WIDGET_CONTROL, rgnlist, SET_LIST_SELECT=0
rgnlist_index = 0

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

region_list_struct = { $
   rgnlist:rgnlist, $
   ok_button:ok_button, $
   cancel_button:cancel_button }

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=region_list_struct, XOFFSET=700, $
                YOFFSET=250, /NO_COPY

WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'RegionDiskList_gui', base0, EVENT_HANDLER='RegionDiskList_eh'

RgnlistIndex = rgnlist_index

END ; RegionDiskList_gui

;***************************************************************************
PRO InsertHeaderRecords_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON hdr_recs, hdr1, hdr2, hdr3, hdr_cancel

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=insert_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   hdr_cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ insert_struct.L1Dir : BEGIN
   END
   event.id EQ insert_struct.L1Ver : BEGIN
   END
   event.id EQ insert_struct.OutDir : BEGIN
   END

   event.id EQ insert_struct.ok_button : BEGIN
      WIDGET_CONTROL, insert_struct.L1Dir,  GET_VALUE=temp
      hdr1 = temp
      WIDGET_CONTROL, insert_struct.L1Ver,  GET_VALUE=temp
      hdr2 = temp
      WIDGET_CONTROL, insert_struct.OutDir, GET_VALUE=temp
      hdr3 = temp
      hdr_cancel = 0

      files_bad = 0
      IF (~ FILE_TEST(hdr1, /DIRECTORY)) THEN BEGIN
         mssg = 'Your input directory does not exist. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         files_bad = 1
      ENDIF

      IF (~ files_bad AND hdr3 EQ '') THEN BEGIN
         mssg = 'You must specify an output directory. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         files_bad = 1
      ENDIF

      IF (~ files_bad AND ~ FILE_TEST(hdr3, /DIRECTORY)) THEN BEGIN
         rtrn_val = MakeDirectory(hdr3)
         IF (~ FILE_TEST(hdr3, /DIRECTORY)) THEN BEGIN
            mssg = ['Your output directory could not be created.', $
                    'Try a different directory name.']
            rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            files_bad = 1
         ENDIF
         rtrn_val = ChmodCatchError(hdr3, '777'O)
      ENDIF

      IF (~ files_bad) THEN BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDIF
   END

   event.id EQ insert_struct.cancel_button : BEGIN
      hdr_cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=insert_struct

END ; InsertHeaderRecords_eh

;***************************************************************************
PRO InsertHeaderRecords_gui, Header1, Header2, Header3, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON hdr_recs, hdr1, hdr2, hdr3, hdr_cancel

;-------------------------------------------------------------------------------
; Define controls in widget.
;-------------------------------------------------------------------------------

Status = 0

base0  = WIDGET_BASE(/COLUMN, TITLE='Enter Header Records for Orbit Process List', $
                     XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
                     YOFFSET=!KON.Misc.ScreenY / 2 - 50, $
                     /TLB_KILL_REQUEST_EVENTS)
base1  = WIDGET_BASE(base0, /COLUMN, /FRAME)
L1Dir  = CW_FIELD(base1, /STRING, /ROW, VALUE=Header1, XSIZE=80, $
                  TITLE='Enter the directory where you will store L1B2 data       ')
L1Ver  = CW_FIELD(base1, /STRING, /ROW, VALUE=Header2, XSIZE=80, $
                  TITLE='Enter version number of L1B2 data (default is usually OK)')
OutDir = CW_FIELD(base1, /STRING, /ROW, VALUE=Header3, XSIZE=80, $
                  TITLE='Enter the directory where MINX output will be written    ')

base3 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base3, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base3, VALUE='Cancel')

;-------------------------------------------------------------------------------
; Define structure to be stored in widget.
;-------------------------------------------------------------------------------

insert_struct = { $
   L1Dir : L1Dir, $
   L1Ver : L1Ver, $
   OutDir : OutDir, $
   ok_button : ok_button, $
   cancel_button : cancel_button }

;-------------------------------------------------------------------------------
; Store the structure in widget and realize it.
;-------------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=insert_struct, XOFFSET=700, YOFFSET=250, $
                /NO_COPY
WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'InsertHeaderRecords_gui', base0, EVENT_HANDLER='InsertHeaderRecords_eh'

IF (hdr_cancel EQ 0) THEN BEGIN
   Header1 = hdr1
   Header2 = hdr2
   Header3 = hdr3
ENDIF ELSE BEGIN
   Status = 1
ENDELSE

END ; InsertHeaderRecords_gui
