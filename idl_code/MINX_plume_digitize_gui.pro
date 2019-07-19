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
PRO PlumeUserInput_new_event, event
;***************************************************************************
; Event processing procedure for accepting user input after plume has been
; digitized.
;---------------------------------------------------------------------------

COMMON pyrocum_obj, PyrocumObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=struct_obj, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ struct_obj.BUTTON_PYRO_YES : BEGIN
   END
   event.id EQ struct_obj.BUTTON_PYRO_NO : BEGIN
   END
   event.id EQ struct_obj.TEXT_COMMENTS : BEGIN
      len_str = STRLEN(event.value)
      IF (len_str GT 80) THEN BEGIN
         mssg = 'You have exceeded the ' + $
            STRTRIM(STRING(struct_obj.TEXT_LENGTH),2) + ' character maximum.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)
         temp_text = STRMID(event.value, 0, struct_obj.TEXT_LENGTH)
         WIDGET_CONTROL, struct_obj.TEXT_COMMENTS, SET_VALUE=temp_text
      ENDIF
   END
 
   event.id EQ struct_obj.BUTTON_YES : BEGIN
      temp = WIDGET_INFO(struct_obj.BUTTON_PYRO_YES, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_PYRO = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_PYRO_NO, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_PYRO = 0
      WIDGET_CONTROL, struct_obj.TEXT_COMMENTS, GET_VALUE=temp_text
      struct_obj.COMMENTS = temp_text
      struct_obj.YESorNO = 'Yes'
      PyrocumObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ struct_obj.BUTTON_NO : BEGIN
      temp = WIDGET_INFO(struct_obj.BUTTON_PYRO_YES, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_PYRO = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_PYRO_NO, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_PYRO = 0
      WIDGET_CONTROL, struct_obj.TEXT_COMMENTS, GET_VALUE=temp_text
      struct_obj.COMMENTS = temp_text
      struct_obj.YESorNO = 'No'
      PyrocumObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY

END  ;  PlumeUserInput_new_event

;***************************************************************************
PRO PlumeUserInput_new, wParentBase, HasPyrocum, Quality, Comments, YesNo
;***************************************************************************
; Interface procedure for accepting user input after plume has been digitized.
;---------------------------------------------------------------------------

COMMON pyrocum_obj, PyrocumObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize default values.
;---------------------------------------------------------------------------

PlumeComponentPyro = 0
PlumeQuality = 1
TextComments = ''
YesNo = 'No'
max_text_length = 80
text_wndw_length = 45

;---------------------------------------------------------------------------
; Define controls in dialog.
;---------------------------------------------------------------------------

WID_BASE_0 = Widget_Base(/COLUMN, TITLE='Finished retrieval', /MODAL, $
                         GROUP_LEADER=wParentBase, /TLB_KILL_REQUEST_EVENTS)

WID_LABEL_x0 = Widget_Label(WID_BASE_0, VALUE='')

oktouse = (!SAV.Digitize.AER_TYPE EQ !KON.AerObjTyp.AER_SMOKE_OBJ AND $
           !SAV.Digitize.GEOM_TYPE EQ !KON.GeomObjTyp.GEOM_POLYGON_OBJ) ? 1 : 0
WID_LABEL_1 = Widget_Label(WID_BASE_0, /ALIGN_CENTER, SENSITIVE=oktouse, $
                     VALUE='Does this smoke plume include a pyrocumulus cloud?')
WID_BASE_2 = Widget_Base(WID_BASE_0, /ROW, /EXCLUSIVE, /ALIGN_CENTER, FRAME=1)
WID_BUTTON_PYRO_YES = Widget_Button(WID_BASE_2, /ALIGN_LEFT, SENSITIVE=oktouse, $
         TOOLTIP='If you included a pyro-cumulus cloud in the digitized' + $
                 'polygon for this plume, select "Yes"', VALUE='Yes')
WID_BUTTON_PYRO_NO = Widget_Button(WID_BASE_2, /ALIGN_LEFT, SENSITIVE=oktouse, $
         TOOLTIP='If you did not include a pyro-cumulus cloud in the digitized' + $
                 'polygon for this plume, select "No"', VALUE='No')
                 
WID_LABEL_x2 = Widget_Label(WID_BASE_0, YSIZE=8, VALUE='')
                           
WID_LABEL_3 = Widget_Label(WID_BASE_0, /ALIGN_CENTER, $
                           VALUE='Describe exceptional features:')
WID_TEXT_COMMENTS = CW_FIELD(WID_BASE_0, /STRING, /ROW, XSIZE=text_wndw_length, $
                             TITLE='', VALUE='', /ALL_EVENTS)

WID_LABEL_x3 = Widget_Label(WID_BASE_0, YSIZE=8, VALUE='')

WID_LABEL_4 = Widget_Label(WID_BASE_0, /ALIGN_CENTER, $
           VALUE='Delete the open plot windows now?')
WID_BASE_4 = Widget_Base(WID_BASE_0, /ROW, /EXCLUSIVE, /ALIGN_CENTER, $
                         FRAME=1)
WID_BUTTON_YES = Widget_Button(WID_BASE_4, /ALIGN_LEFT, VALUE='Yes')
WID_BUTTON_NO = Widget_Button(WID_BASE_4, /ALIGN_LEFT, VALUE='No')

WID_LABEL_x4 = Widget_Label(WID_BASE_0, VALUE='')

;---------------------------------------------------------------------------
; Store values to be passed to event handler.
;---------------------------------------------------------------------------

struct_obj = { BUTTON_PYRO_YES : WID_BUTTON_PYRO_YES, $
               BUTTON_PYRO_NO  : WID_BUTTON_PYRO_NO,  $
               TEXT_COMMENTS   : WID_TEXT_COMMENTS,   $
               BUTTON_YES      : WID_BUTTON_YES,      $
               BUTTON_NO       : WID_BUTTON_NO,       $
               PLUME_COMP_PYRO : PlumeComponentPyro,  $
               COMMENTS        : TextComments,        $
               TEXT_LENGTH     : max_text_length,     $
               YESorNO         : YesNo }

PyrocumObject = struct_obj

;---------------------------------------------------------------------------
; Set controls to default values.
;---------------------------------------------------------------------------

WIDGET_CONTROL, WID_BUTTON_PYRO_NO, /SET_BUTTON

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=struct_obj, XOFFSET=700, $
                YOFFSET=150, /NO_COPY

WIDGET_CONTROL, /REALIZE, WID_BASE_0

XMANAGER, 'PlumeUserInput_new', WID_BASE_0, $
           EVENT_HANDLER='PlumeUserInput_new_event'

;---------------------------------------------------------------------------
; Store pyrocum flag in data structure for this plume's region.
;---------------------------------------------------------------------------

HasPyrocum = PyrocumObject.PLUME_COMP_PYRO
Comments = PyrocumObject.COMMENTS
IF (Comments EQ '') THEN Comments = 'None'
YesNo = PyrocumObject.YESorNO

struct_obj = 0
PyrocumObject = 0

END  ;  PlumeUserInput_new

;***************************************************************************
PRO PlumeUserInput_old_event, event
;***************************************************************************
; Event processing procedure for accepting user input after plume has been
; digitized.
;---------------------------------------------------------------------------

COMMON userin_obj, PyrocumObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=struct_obj, /NO_COPY

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ struct_obj.BUTTON_GOOD : BEGIN
   END
   event.id EQ struct_obj.BUTTON_CORR : BEGIN
   END
   event.id EQ struct_obj.BUTTON_NOGOOD : BEGIN
   END
   event.id EQ struct_obj.BUTTON_NONE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MINOR : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MAJOR : BEGIN
   END
   event.id EQ struct_obj.BUTTON_SIMPLE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_COMPLEX : BEGIN
   END
   event.id EQ struct_obj.BUTTON_ALONGTRK : BEGIN
   END
   event.id EQ struct_obj.BUTTON_SINGLE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MULTIPLE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_FIREFRONT : BEGIN
   END
   event.id EQ struct_obj.BUTTON_OPAQUE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_AEROSOL1 : BEGIN
   END
   event.id EQ struct_obj.BUTTON_AERSURF : BEGIN
   END
   event.id EQ struct_obj.BUTTON_SURFACE1 : BEGIN
   END
   event.id EQ struct_obj.BUTTON_DARK : BEGIN
   END
   event.id EQ struct_obj.BUTTON_INTERMED : BEGIN
   END
   event.id EQ struct_obj.BUTTON_BRIGHT : BEGIN
   END
   event.id EQ struct_obj.BUTTON_PYRO : BEGIN
   END
   event.id EQ struct_obj.BUTTON_RISE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_SKIRT : BEGIN
   END
   event.id EQ struct_obj.TEXT_COMMENTS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_OK : BEGIN
      temp = WIDGET_INFO(struct_obj.BUTTON_GOOD, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.REG_CORRECTION = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_CORR, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.REG_CORRECTION = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_NOGOOD, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.REG_CORRECTION = 3

      temp = WIDGET_INFO(struct_obj.BUTTON_NONE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.CLOUD_CONTAM = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_MINOR, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.CLOUD_CONTAM = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_MAJOR, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.CLOUD_CONTAM = 3

      temp = WIDGET_INFO(struct_obj.BUTTON_SIMPLE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.WIND_DIRECTION = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_COMPLEX, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.WIND_DIRECTION = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_ALONGTRK, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.WIND_DIRECTION = 3

      temp = WIDGET_INFO(struct_obj.BUTTON_SINGLE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_NEIGHBOR = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_MULTIPLE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_NEIGHBOR = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_FIREFRONT, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_NEIGHBOR = 3

      temp = WIDGET_INFO(struct_obj.BUTTON_OPAQUE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_DENSITY = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_AEROSOL1, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_DENSITY = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_AERSURF, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_DENSITY = 3
      temp = WIDGET_INFO(struct_obj.BUTTON_SURFACE1, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_DENSITY = 4

      temp = WIDGET_INFO(struct_obj.BUTTON_DARK, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SURFACE_BRIGHT = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_INTERMED, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SURFACE_BRIGHT = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_BRIGHT, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SURFACE_BRIGHT = 3

      struct_obj.PLUME_COMP_PYRO = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_PYRO, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_PYRO = 2
      struct_obj.PLUME_COMP_RISE = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_RISE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_RISE = 2
      struct_obj.PLUME_COMP_SKIRT = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_SKIRT, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.PLUME_COMP_SKIRT = 2

      WIDGET_CONTROL, struct_obj.TEXT_COMMENTS, GET_VALUE=temp
      struct_obj.COMMENTS = temp

      struct_obj.CANCELorOK = 1
      PyrocumObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   event.id EQ struct_obj.BUTTON_NO : BEGIN
      struct_obj.CANCELorOK = 0
      PyrocumObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY

END  ;  PlumeUserInput_old_event

;***************************************************************************
PRO PlumeUserInput_old, wParentBase, UserVals
;***************************************************************************
; Interface procedure for accepting user input after plume has been digitized.
;---------------------------------------------------------------------------

COMMON userin_obj, UserStructObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize default values.
;---------------------------------------------------------------------------

RegCorrection       = 1
CloudContamination  = 1
WindDirection       = 1
PlumeNeighborhood   = 1
PlumeDensity        = 1
SurfaceBrightness   = 1
PlumeComponentPyro  = 1
PlumeComponentRise  = 1
PlumeComponentSkirt = 1

;---------------------------------------------------------------------------
; Define controls in dialog.
;---------------------------------------------------------------------------

WID_BASE_0 = Widget_Base(/COLUMN, TITLE='Your Description', /MODAL, $
                         GROUP_LEADER=wParentBase)

WID_BASE_12 = Widget_Base(WID_BASE_0, /ROW)

;---------------------
WID_BASE_11 = Widget_Base(WID_BASE_12, /COLUMN)

WID_LABEL_1 = Widget_Label(WID_BASE_11, /ALIGN_LEFT, $
   VALUE='Camera Registration:')

WID_BASE_1 = WIDGET_BASE(WID_BASE_11, /COLUMN, /EXCLUSIVE, FRAME=1)

WID_BUTTON_GOOD = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
   TOOLTIP='Cameras were well co-registered and required no correction.', $
   VALUE='Good')

WID_BUTTON_CORR = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
   TOOLTIP='Cameras responded well to registration correction.', $
   VALUE='Corrected')

WID_BUTTON_NOGOOD = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
   TOOLTIP='Cameras were not able to be registration-corrected.', $
   VALUE='Uncorrectable')

;---------------------
WID_BASE_22 = Widget_Base(WID_BASE_12, /COLUMN)

WID_LABEL_2 = Widget_Label(WID_BASE_22, /ALIGN_LEFT, $
   VALUE='Cloud Contamination:')

WID_BASE_2 = WIDGET_BASE(WID_BASE_22, /COLUMN, /EXCLUSIVE, FRAME=1)

WID_BUTTON_NONE = Widget_Button(WID_BASE_2, /ALIGN_LEFT, $
   TOOLTIP='No significant cloud contamination was present.', $
   VALUE='None')

WID_BUTTON_MINOR = Widget_Button(WID_BASE_2, /ALIGN_LEFT, $
   TOOLTIP='Cloud contamination was a small problem in defining' + $
   ' the aerosol boundary.', VALUE='Minor')

WID_BUTTON_MAJOR = Widget_Button(WID_BASE_2, /ALIGN_LEFT, $
   TOOLTIP='Cloud contamination was a significant problem in defining' + $
   ' the aerosol boundary.', VALUE='Major')


;---------------------
WID_BASE_33 = Widget_Base(WID_BASE_12, /COLUMN)

WID_LABEL_3 = Widget_Label(WID_BASE_33, /ALIGN_LEFT, $
   VALUE='Aerosol Direction:')

WID_BASE_3 = WIDGET_BASE(WID_BASE_33, /COLUMN, /EXCLUSIVE, FRAME=1)

geom_info = WIDGET_INFO(WID_BASE_1, /GEOMETRY)
WIDGET_CONTROL, WID_BASE_3, SCR_XSIZE=geom_info.SCR_XSIZE

WID_BUTTON_SIMPLE = Widget_Button(WID_BASE_3, /ALIGN_LEFT, $
   TOOLTIP='A single motion vector adequately defined the aerosol' + $
   ' direction of motion.', VALUE='Simple')

WID_BUTTON_COMPLEX = Widget_Button(WID_BASE_3, /ALIGN_LEFT, $
   TOOLTIP='Aerosol motion direction was not adequately defined by' + $
   ' a single motion vector.', VALUE='Complex')

WID_BUTTON_ALONGTRK = Widget_Button(WID_BASE_3, /ALIGN_LEFT, $
   TOOLTIP='Aerosol motion direction was nearly parallel with the' + $
   ' along-track direction.', VALUE='Along track')

WID_BASE_34 = Widget_Base(WID_BASE_0, /ROW)

;---------------------
WID_BASE_44 = Widget_Base(WID_BASE_34, /COLUMN)

WID_LABEL_4 = Widget_Label(WID_BASE_44, /ALIGN_LEFT, $
   VALUE='Plume Source:')

WID_BASE_4 = WIDGET_BASE(WID_BASE_44, /COLUMN, /EXCLUSIVE, FRAME=1)

WID_BUTTON_SINGLE = Widget_Button(WID_BASE_4, /ALIGN_LEFT, $
   TOOLTIP='Aerosol feature was a cleanly defined, single occurrence' + $
   ' with a relatively small source region.', VALUE='Single')

WID_BUTTON_MULTIPLE = Widget_Button(WID_BASE_4, /ALIGN_LEFT, $
   TOOLTIP='A few aerosol sources contributed to this feature which is' + $
   ' the result of their merger.', VALUE='Multiple')

WID_BUTTON_FIREFRONT = Widget_Button(WID_BASE_4, /ALIGN_LEFT, $
   TOOLTIP='Aerosol feature was part of a linear series of aerosol ' + $
   'sources with no clear boundaries between them.', VALUE='Line source')

;---------------------
WID_BASE_77 = Widget_Base(WID_BASE_34, /COLUMN)

WID_LABEL_7 = Widget_Label(WID_BASE_77, /ALIGN_LEFT, $
                           VALUE='Surface Brightness:')

WID_BASE_7 = WIDGET_BASE(WID_BASE_77, /COLUMN, /EXCLUSIVE, FRAME=1)

WID_BUTTON_DARK = Widget_Button(WID_BASE_7, /ALIGN_LEFT, $
   TOOLTIP='Surface beneath aerosol is dark relative to the aerosol.', $
   VALUE='Dark surface')

WID_BUTTON_INTERMED = Widget_Button(WID_BASE_7, /ALIGN_LEFT, $
   TOOLTIP='Surface beneath aerosol is intermediate between bright' + $
   ' and dark relative to the aerosol.', VALUE='Intermediate surface')

WID_BUTTON_BRIGHT = Widget_Button(WID_BASE_7, /ALIGN_LEFT, $
   TOOLTIP='Surface beneath aerosol is bright relative to the aerosol.', $
   VALUE='Bright surface')

;---------------------
WID_BASE_66 = Widget_Base(WID_BASE_34, /COLUMN)

WID_LABEL_6 = Widget_Label(WID_BASE_66, /ALIGN_LEFT, VALUE='Visual Density:')

WID_BASE_6 = WIDGET_BASE(WID_BASE_66, /COLUMN, /EXCLUSIVE, FRAME=1)

WID_BUTTON_OPAQUE = Widget_Button(WID_BASE_6, /ALIGN_LEFT, $
   TOOLTIP='Aerosol is opaque.', VALUE='Aerosol opaque')

WID_BUTTON_AEROSOL1 = Widget_Button(WID_BASE_6, /ALIGN_LEFT, $
   TOOLTIP='Aerosol is more visible than the surface.', $
   VALUE='Aerosol most visible')

WID_BUTTON_AERSURF = Widget_Button(WID_BASE_6, /ALIGN_LEFT, $
   TOOLTIP='Aerosol and surface are equally visible.', $
   VALUE='Aerosol/surface equal')

WID_BUTTON_SURFACE1 = Widget_Button(WID_BASE_6, /ALIGN_LEFT, $
   TOOLTIP='Surface is more visible than the aerosol.', $
   VALUE='Surface most visible')


WID_BASE_56 = Widget_Base(WID_BASE_0, /ROW)

;---------------------
WID_BASE_55 = Widget_Base(WID_BASE_56, /COLUMN)

WID_LABEL_5 = Widget_Label(WID_BASE_55, /ALIGN_LEFT, $
   VALUE='Plume Components:')

WID_BASE_5 = WIDGET_BASE(WID_BASE_55, /COLUMN, /NONEXCLUSIVE, FRAME=1)

WID_BUTTON_PYRO = Widget_Button(WID_BASE_5, /ALIGN_LEFT, $
   TOOLTIP='Retrieved heights include a pyro-cumulus cloud.', $
   VALUE='Pyro-cumulus')

WID_BUTTON_RISE = Widget_Button(WID_BASE_5, /ALIGN_LEFT, $
   TOOLTIP='Retrieved heights include a column of aerosol rising' + $
   ' from the source region.', VALUE='Rising column')

WID_BUTTON_SKIRT = Widget_Button(WID_BASE_5, /ALIGN_LEFT, $
   TOOLTIP='Retrieved heights include a peripheral skirt of diffuse' + $
   ' aerosol.', VALUE='Diffuse skirt')

;---------------------
WID_BASE_88 = Widget_Base(WID_BASE_56, /COLUMN)

WID_LABEL_8 = Widget_Label(WID_BASE_88, /ALIGN_LEFT, VALUE='Comments:')

WID_TEXT_COMMENTS = CW_FIELD(WID_BASE_88, /STRING, /ROW, TITLE='', VALUE='')

WID_LABEL_8 = Widget_Label(WID_BASE_88, /ALIGN_LEFT, VALUE=' ', YSIZE=5)

WID_BASE_8 = Widget_Base(WID_BASE_88, /ROW)

WID_BUTTON_OK = Widget_Button(WID_BASE_8, /ALIGN_CENTER, XSIZE=65, $
   YSIZE=30, VALUE='OK')

WID_BUTTON_CANCEL = Widget_Button(WID_BASE_8, /ALIGN_CENTER, XSIZE=65, $
   YSIZE=30, VALUE='Cancel')

;---------------------------------------------------------------------------
; Store values to be passed to event handler.
;---------------------------------------------------------------------------

struct_obj = { BUTTON_GOOD      : WID_BUTTON_GOOD,      $
               BUTTON_CORR      : WID_BUTTON_CORR,      $
               BUTTON_NOGOOD    : WID_BUTTON_NOGOOD,    $
               BUTTON_NONE      : WID_BUTTON_NONE,      $
               BUTTON_MINOR     : WID_BUTTON_MINOR,     $
               BUTTON_MAJOR     : WID_BUTTON_MAJOR,     $
               BUTTON_SIMPLE    : WID_BUTTON_SIMPLE,    $
               BUTTON_COMPLEX   : WID_BUTTON_COMPLEX,   $
               BUTTON_ALONGTRK  : WID_BUTTON_ALONGTRK,  $
               BUTTON_SINGLE    : WID_BUTTON_SINGLE,    $
               BUTTON_MULTIPLE  : WID_BUTTON_MULTIPLE,  $
               BUTTON_FIREFRONT : WID_BUTTON_FIREFRONT, $
               BUTTON_OPAQUE    : WID_BUTTON_OPAQUE,    $
               BUTTON_AEROSOL1  : WID_BUTTON_AEROSOL1,  $
               BUTTON_AERSURF   : WID_BUTTON_AERSURF,   $
               BUTTON_SURFACE1  : WID_BUTTON_SURFACE1,  $
               BUTTON_DARK      : WID_BUTTON_DARK,      $
               BUTTON_INTERMED  : WID_BUTTON_INTERMED,  $
               BUTTON_BRIGHT    : WID_BUTTON_BRIGHT,    $
               BUTTON_PYRO      : WID_BUTTON_PYRO,      $
               BUTTON_RISE      : WID_BUTTON_RISE,      $
               BUTTON_SKIRT     : WID_BUTTON_SKIRT,     $
               BUTTON_OK        : WID_BUTTON_OK,        $
               BUTTON_CANCEL    : WID_BUTTON_CANCEL,    $
;
               TEXT_COMMENTS    : WID_TEXT_COMMENTS,    $
               REG_CORRECTION   : RegCorrection,        $
               CLOUD_CONTAM     : CloudContamination,   $
               WIND_DIRECTION   : WindDirection,        $
               PLUME_NEIGHBOR   : PlumeNeighborhood,    $
               PLUME_DENSITY    : PlumeDensity,         $
               SURFACE_BRIGHT   : SurfaceBrightness,    $
               PLUME_COMP_PYRO  : PlumeComponentPyro,   $
               PLUME_COMP_RISE  : PlumeComponentRise,   $
               PLUME_COMP_SKIRT : PlumeComponentSkirt,  $
               COMMENTS         : ' ',                  $
               CANCELorOK       : 0 }

UserStructObject = struct_obj

;---------------------------------------------------------------------------
; Set controls to default values.
;---------------------------------------------------------------------------

WIDGET_CONTROL, WID_BUTTON_GOOD,   /SET_BUTTON
WIDGET_CONTROL, WID_BUTTON_NONE,   /SET_BUTTON
WIDGET_CONTROL, WID_BUTTON_SIMPLE, /SET_BUTTON
WIDGET_CONTROL, WID_BUTTON_SINGLE, /SET_BUTTON
WIDGET_CONTROL, WID_BUTTON_OPAQUE, /SET_BUTTON
WIDGET_CONTROL, WID_BUTTON_DARK,   /SET_BUTTON

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=struct_obj, XOFFSET=700, $
                YOFFSET=150, /NO_COPY

WIDGET_CONTROL, /REALIZE, WID_BASE_0

XMANAGER, 'PlumeUserInput_old', WID_BASE_0, $
           EVENT_HANDLER='PlumeUserInput_old_event'

;---------------------------------------------------------------------------
; Store selected values in structure for return to caller.
;---------------------------------------------------------------------------

IF (UserStructObject.CANCELorOK EQ 1) THEN BEGIN
   UserVals.RegCorrect  = UserStructObject.REG_CORRECTION    
   UserVals.CloudContam = UserStructObject.CLOUD_CONTAM
   UserVals.WindDirect  = UserStructObject.WIND_DIRECTION
   UserVals.Neighbor    = UserStructObject.PLUME_NEIGHBOR
   UserVals.Density     = UserStructObject.PLUME_DENSITY
   UserVals.SurfBright  = UserStructObject.SURFACE_BRIGHT
   UserVals.CompPyro    = UserStructObject.PLUME_COMP_PYRO 
   UserVals.CompRise    = UserStructObject.PLUME_COMP_RISE
   UserVals.CompSkirt   = UserStructObject.PLUME_COMP_SKIRT 
   UserVals.Comments    = UserStructObject.COMMENTS
ENDIF

END  ;  PlumeUserInput_old

;***************************************************************************
PRO ResetDigitizingDialog, StructObj
;***************************************************************************
; Reset digitizing dialog parameters to their default state. The "RESET"
; button restores the dialog parameters to the MINX default state, not to
; the state the user last saved to the "Preferences" file before making
; changes.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Reset controls in dialog box.
;---------------------------------------------------------------------------

WIDGET_CONTROL, StructObj.BUTTON_SMOKE, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_JPGMP4, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_DAER, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_MAPDOCS, SET_BUTTON=1
WIDGET_CONTROL, StructObj.BUTTON_PUBQUAL, SET_BUTTON=0
WIDGET_CONTROL, StructObj.BUTTON_NOST, /SET_BUTTON
WIDGET_CONTROL, StructObj.FLOAT_MINHT, SET_VALUE=!KON.Digitize.MIN_HGHT
WIDGET_CONTROL, StructObj.FLOAT_MAXHT, SET_VALUE=!KON.Digitize.MAX_HGHT
WIDGET_CONTROL, StructObj.FLOAT_MAXWND, SET_VALUE=!KON.Digitize.MAX_WIND
WIDGET_CONTROL, StructObj.BUTTON_POLYGON,/SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_WIND, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_BIDIRWIND, SET_BUTTON=0
WIDGET_CONTROL, StructObj.BUTTON_AS_V23, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_BAND_BOTH, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_MATCHER_MEDIUM, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_RELAX_THRESH, SET_BUTTON=0
WIDGET_CONTROL, StructObj.BUTTON_MED_SPACING, /SET_BUTTON
WIDGET_CONTROL, StructObj.BUTTON_ABC_CAM_PAIRS, /SET_BUTTON

WIDGET_CONTROL, StructObj.BUTTON_NOWIND, SENSITIVE=1
WIDGET_CONTROL, StructObj.BUTTON_LINE, SENSITIVE=1

;---------------------------------------------------------------------------
; Reset parameter values in global memory.
;---------------------------------------------------------------------------

!SAV.Digitize.DIG_STATE          = !KON.Digitize.DIG_STATE_ZERO
!SAV.Digitize.AER_TYPE           = !KON.AerObjTyp.AER_SMOKE_OBJ
!SAV.Digitize.JPEG_OR_MP4        = !KON.Digitize.JPEG_OR_MP4
!SAV.Digitize.DRAW_AEROSOL       = !KON.Digitize.DRAW_AEROSOL
!SAV.Digitize.DOCS_ON_MAPS       = !KON.Digitize.DOCS_ON_MAPS
!SAV.Digitize.PUB_QUALITY        = !KON.Digitize.PUB_QUALITY
!SAV.Digitize.COMPARE_PGEHTS     = !KON.Digitize.COMPARE_PGEHTS
!SAV.Digitize.MIN_HGHT           = !KON.Digitize.MIN_HGHT
!SAV.Digitize.MAX_HGHT           = !KON.Digitize.MAX_HGHT 
!SAV.Digitize.MAX_WIND           = !KON.Digitize.MAX_WIND
!SAV.Digitize.GEOM_TYPE          = !KON.GeomObjTyp.GEOM_POLYGON_OBJ
!SAV.Digitize.WIND_TYPE          = !KON.WindObjTyp.WIND_USER_DIREC_OBJ
!SAV.Digitize.BI_DIR_WIND        = !KON.Digitize.BI_DIR_WIND
!SAV.Digitize.AS_22_OR_23        = !KON.Digitize.AS_22_OR_23
!SAV.Digitize.RETRIEVE_BAND_TYPE = !KON.BandObjTyp.BOTH_BAND
!SAV.Digitize.USE_BAND_NDX       = !KON.Instr.RED
!SAV.Digitize.MATCHER_SM_LG      = !KON.Digitize.MATCHER_SM_LG
!SAV.Digitize.RELAX_THRESH       = !KON.Digitize.RELAX_THRESH
!SAV.Digitize.USE_CAM_PAIRS      = !KON.Digitize.USE_CAM_PAIRS
!SAV.Digitize.FIRST_CAM_USE      = !KON.Digitize.FIRST_CAM_USE
!SAV.Digitize.LAST_CAM_USE       = !KON.Digitize.LAST_CAM_USE
!SAV.Digitize.SAMP_SPAC_NODIR    = !KON.Digitize.SAMP_SPAC_NODIR
!SAV.Digitize.SAMP_SPAC_DIR      = !KON.Digitize.SAMP_SPAC_DIR

!SAV.Digitize.SHOW_USER_DIALOG   = 0
!SAV.Digitize.PRINT_OFFSETS      = 0
!SAV.Digitize.AUTO_CORR_AN       = 0
!SAV.Digitize.SHOW_REF_CMP       = 0
!SAV.Digitize.DRAW_CORR_MTRX     = 0
!SAV.Digitize.DRAW_OFFSET_DIFF   = 0
!SAV.Digitize.SHOW_3D_HEIGHTS    = 0

;---------------------------------------------------------------------------
; Set default button values for development version features.
;---------------------------------------------------------------------------

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
   WIDGET_CONTROL, StructObj.BUTTON_MULTIWIND, SENSITIVE=0
   WIDGET_CONTROL, StructObj.BUTTON_NOWIND_X,  SENSITIVE=0
   IF (StructObj.SHOW_REF_CMP EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_SREFCMP, /SET_BUTTON
   IF (StructObj.DRAW_CORR_MTRX EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_DCORR, /SET_BUTTON
   IF (StructObj.PRINT_OFFSETS GE 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_PRNTOFF, /SET_BUTTON
   IF (StructObj.AUTO_CORR_AN EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_ACORR, /SET_BUTTON
   IF (StructObj.DRAW_OFFSET_DIFF EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_DDIFF, /SET_BUTTON
   IF (StructObj.SHOW_3D_HEIGHTS EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_3DHTS, /SET_BUTTON
   IF (StructObj.DRAW_3DSOLUA EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_3DSOLUA, /SET_BUTTON
   IF (StructObj.DRAW_3DSOLUB EQ 1) THEN $
      WIDGET_CONTROL, StructObj.BUTTON_3DSOLUB, /SET_BUTTON
ENDIF

END  ;  ResetDigitizingDialog

;***************************************************************************
PRO plume_gui_event, event
;***************************************************************************
; IDL Widget Interface Procedures.
;---------------------------------------------------------------------------

COMMON dig_obj, StructObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=struct_obj, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   struct_obj.CANCELorOK = 0
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ struct_obj.BUTTON_SMOKE : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_USER_DIALOG, SET_BUTTON=1
            WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
            WIDGET_CONTROL, struct_obj.BUTTON_DAER, SET_BUTTON=1
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_VOLCASH : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=0
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_DUST : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_WATER : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_CONTRAIL : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_OTHER : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_GROUND : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_JPGMP4, SET_BUTTON=1
      ENDIF
   END
;------------
   event.id EQ struct_obj.BUTTON_LINE : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_WIND,   SET_BUTTON=1
         WIDGET_CONTROL, struct_obj.BUTTON_NOWIND, SENSITIVE=0 
         IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
            WIDGET_CONTROL, struct_obj.BUTTON_MULTIWIND, SENSITIVE=0
            WIDGET_CONTROL, struct_obj.BUTTON_NOWIND_X,  SENSITIVE=0
         ENDIF
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 0.550) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_LO_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 1.100) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_MED_SPACING, /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 2.200) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_HI_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 3.300) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_VHI_SPACING, /SET_BUTTON
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_POLYGON : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_NOWIND, SENSITIVE=1
;         IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
;            WIDGET_CONTROL, struct_obj.BUTTON_MULTIWIND, SENSITIVE=1
;            WIDGET_CONTROL, struct_obj.BUTTON_NOWIND_X,  SENSITIVE=1
;         ENDIF
      ENDIF
   END

   event.id EQ struct_obj.BUTTON_NOWIND : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_POLYGON, SET_BUTTON=1
         WIDGET_CONTROL, struct_obj.BUTTON_LINE,    SENSITIVE=0 
         IF (!SAV.Digitize.SAMP_SPAC_NODIR EQ 0.550) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_LO_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_NODIR EQ 1.100) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_MED_SPACING, /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_NODIR EQ 2.200) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_HI_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_NODIR EQ 3.300) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_VHI_SPACING, /SET_BUTTON
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_WIND : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_LINE, SENSITIVE=1
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 0.550) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_LO_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 1.100) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_MED_SPACING, /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 2.200) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_HI_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 3.300) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_VHI_SPACING, /SET_BUTTON
      ENDIF
   END

   event.id EQ struct_obj.BUTTON_MULTIWIND : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 0.550) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_LO_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 1.100) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_MED_SPACING, /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 2.200) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_HI_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 3.300) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_VHI_SPACING, /SET_BUTTON
      ENDIF
   END
   event.id EQ struct_obj.BUTTON_NOWIND_X : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 0.550) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_LO_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 1.100) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_MED_SPACING, /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 2.200) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_HI_SPACING,  /SET_BUTTON
         IF (!SAV.Digitize.SAMP_SPAC_DIR EQ 3.300) THEN $
            WIDGET_CONTROL, struct_obj.BUTTON_VHI_SPACING, /SET_BUTTON
      ENDIF
   END
;------------
   event.id EQ struct_obj.FLOAT_MAXHT : BEGIN
   END
   event.id EQ struct_obj.FLOAT_MINHT : BEGIN
   END
   event.id EQ struct_obj.FLOAT_MAXWND : BEGIN
   END
   event.id EQ struct_obj.BUTTON_LO_SPACING : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MED_SPACING : BEGIN
   END
   event.id EQ struct_obj.BUTTON_HI_SPACING : BEGIN
   END
   event.id EQ struct_obj.BUTTON_VHI_SPACING : BEGIN
   END
;------------
   event.id EQ struct_obj.BUTTON_BIDIRWIND : BEGIN
   END
   event.id EQ struct_obj.BUTTON_AS_V23 : BEGIN
   END   
   event.id EQ struct_obj.BUTTON_RELAX_THRESH : BEGIN
   END
;------------
   event.id EQ struct_obj.BUTTON_BAND_RED : BEGIN
   END
   event.id EQ struct_obj.BUTTON_BAND_BLUE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_BAND_RB : BEGIN
   END
   event.id EQ struct_obj.BUTTON_BAND_BOTH : BEGIN
   END

   event.id EQ struct_obj.BUTTON_MATCHER_SMALL : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MATCHER_MEDIUM : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MATCHER_LARGE : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MATCHER_XLARGE : BEGIN
   END

   event.id EQ struct_obj.BUTTON_A_CAM_PAIRS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_AB_CAM_PAIRS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_ABC_CAM_PAIRS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_ABCD_CAM_PAIRS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_CD_CAM_PAIRS : BEGIN
   END
;------------
   event.id EQ struct_obj.BUTTON_USER_DIALOG : BEGIN
   END
   event.id EQ struct_obj.BUTTON_JPGMP4 : BEGIN
   END
   event.id EQ struct_obj.BUTTON_MAPDOCS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_PUBQUAL : BEGIN
   END
   event.id EQ struct_obj.BUTTON_DAER : BEGIN
   END
   event.id EQ struct_obj.BUTTON_CSTER : BEGIN
   END
   event.id EQ struct_obj.BUTTON_CLOUD : BEGIN
   END
   event.id EQ struct_obj.BUTTON_NOST : BEGIN
   END
;------------
   event.id EQ struct_obj.BUTTON_SREFCMP : BEGIN
   END
   event.id EQ struct_obj.BUTTON_DCORR : BEGIN
   END
   event.id EQ struct_obj.BUTTON_PRNTOFF : BEGIN
   END
   event.id EQ struct_obj.BUTTON_ACORR : BEGIN
      IF (event.select EQ 1) THEN BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_SREFCMP, SET_BUTTON=1
         WIDGET_CONTROL, struct_obj.BUTTON_DCORR, SET_BUTTON=1
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, struct_obj.BUTTON_SREFCMP,  $
                         SET_BUTTON=!SAV.Digitize.SHOW_REF_CMP
         WIDGET_CONTROL, struct_obj.BUTTON_DCORR, $
                         SET_BUTTON=!SAV.Digitize.DRAW_CORR_MTRX
      ENDELSE 
   END
   event.id EQ struct_obj.BUTTON_DDIFF : BEGIN
   END
   event.id EQ struct_obj.BUTTON_3DHTS : BEGIN
   END
   event.id EQ struct_obj.BUTTON_3DSOLUA : BEGIN
   END
   event.id EQ struct_obj.BUTTON_3DSOLUB : BEGIN
   END
;------------
   event.id EQ struct_obj.BUTTON_OK : BEGIN

      temp = WIDGET_INFO(struct_obj.BUTTON_DUST, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_SMOKE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_VOLCASH, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 3 
      temp = WIDGET_INFO(struct_obj.BUTTON_WATER, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 4
      temp = WIDGET_INFO(struct_obj.BUTTON_CONTRAIL, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 5
      temp = WIDGET_INFO(struct_obj.BUTTON_OTHER, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.AER_TYPE = 6

      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
         temp = WIDGET_INFO(struct_obj.BUTTON_GROUND, /BUTTON_SET)
         IF (temp EQ 1) THEN struct_obj.AER_TYPE = 7
      ENDIF
;------------
      temp = WIDGET_INFO(struct_obj.BUTTON_LINE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.GEOM_TYPE = 2
      temp = WIDGET_INFO(struct_obj.BUTTON_POLYGON, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.GEOM_TYPE = 3

      temp = WIDGET_INFO(struct_obj.BUTTON_NOWIND, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.WIND_TYPE = 1
      temp = WIDGET_INFO(struct_obj.BUTTON_WIND, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.WIND_TYPE = 2

      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
         temp = WIDGET_INFO(struct_obj.BUTTON_MULTIWIND, /BUTTON_SET)
         IF (temp EQ 1) THEN struct_obj.WIND_TYPE = 3
         temp = WIDGET_INFO(struct_obj.BUTTON_NOWIND_X, /BUTTON_SET)
         IF (temp EQ 1) THEN struct_obj.WIND_TYPE = 4
      ENDIF
;------------
      WIDGET_CONTROL, struct_obj.FLOAT_MAXHT, GET_VALUE=maxval
      min_of_max_ht = !KON.Digitize.MIN_HGHT + 0.05
      max_ht = !KON.DataRgn.VALUE_MAX[!KON.DataRgn.TYPE_ZEROWIND_HT]
      IF (maxval LT min_of_max_ht OR maxval GT max_ht) THEN BEGIN
         errmsg = 'Maximum height must be between ' + $
                  STRTRIM(STRING(min_of_max_ht),2) + $
                  ' and ' + STRTRIM(STRING(max_ht),2) + ' km. Try again.'
         ret = DIALOG_MESSAGE(errmsg, /ERROR, /CENTER)
         struct_obj.MAX_HGHT = ABS(maxval-min_of_max_ht) LT $
                    ABS(maxval-max_ht) ? min_of_max_ht : max_ht
         WIDGET_CONTROL, struct_obj.FLOAT_MAXHT, $
                         SET_VALUE=!SAV.Digitize.MAX_HGHT
         WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY
         RETURN
      ENDIF
      struct_obj.MAX_HGHT = maxval

      max_of_min_ht = (maxval - 0.05) > 0.0
      WIDGET_CONTROL, struct_obj.FLOAT_MINHT, GET_VALUE=minval
      IF (minval LT 0.0 OR minval GT max_of_min_ht) THEN BEGIN
         errmsg = 'Minimum height must be between 0 and ' + $
                  STRTRIM(STRING(max_of_min_ht),2) + ' km. Try again.'
         ret = DIALOG_MESSAGE(errmsg, /ERROR, /CENTER)
         struct_obj.MIN_HGHT = ABS(minval) LT $
                    ABS(minval-max_of_min_ht) ? 0.0 : max_of_min_ht
         WIDGET_CONTROL, struct_obj.FLOAT_MINHT, $
                         SET_VALUE=!SAV.Digitize.MIN_HGHT
         WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY
         RETURN
      ENDIF
      struct_obj.MIN_HGHT = minval

      WIDGET_CONTROL, struct_obj.FLOAT_MAXWND, GET_VALUE=maxval
      IF (maxval LT 2. OR maxval GT 100.) THEN BEGIN
         errmsg = 'Maximum wind speed must be between 2 and 100 m/s. ' + $
                  ' Try again.'
         ret = DIALOG_MESSAGE(errmsg, /ERROR, /CENTER)
         struct_obj.MAX_WIND = ABS(maxval-2.) LT ABS(maxval-100.) ? 2. : 100.
         WIDGET_CONTROL, struct_obj.FLOAT_MAXWND, $
                         SET_VALUE=!SAV.Digitize.MAX_WIND
         WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY
         RETURN
      ENDIF
      struct_obj.MAX_WIND = maxval

      temp = WIDGET_INFO(struct_obj.BUTTON_LO_SPACING,  /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 0.550
      temp = WIDGET_INFO(struct_obj.BUTTON_MED_SPACING, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 1.100
      temp = WIDGET_INFO(struct_obj.BUTTON_HI_SPACING,  /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 2.200
      temp = WIDGET_INFO(struct_obj.BUTTON_VHI_SPACING, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 3.300

      struct_obj.RELAX_THRESH = 0
      temp = WIDGET_INFO(struct_obj.BUTTON_RELAX_THRESH, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.RELAX_THRESH = 1

      struct_obj.BI_DIR_WIND = $
            WIDGET_INFO(struct_obj.BUTTON_BIDIRWIND, /BUTTON_SET)
      struct_obj.AS_22_OR_23 = $
            WIDGET_INFO(struct_obj.BUTTON_AS_V23, /BUTTON_SET)
;------------
      temp = WIDGET_INFO(struct_obj.BUTTON_BAND_RED,  /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.RETRIEVE_BAND_TYPE = $
         !KON.BandObjTyp.RED_BAND
      temp = WIDGET_INFO(struct_obj.BUTTON_BAND_BLUE, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.RETRIEVE_BAND_TYPE = $
         !KON.BandObjTyp.BLUE_BAND
      temp = WIDGET_INFO(struct_obj.BUTTON_BAND_BOTH, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.RETRIEVE_BAND_TYPE = $
         !KON.BandObjTyp.BOTH_BAND
      temp = WIDGET_INFO(struct_obj.BUTTON_BAND_RB,   /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.RETRIEVE_BAND_TYPE = $
         !KON.BandObjTyp.RB_BAND

     temp = WIDGET_INFO(struct_obj.BUTTON_MATCHER_SMALL,  /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.MATCHER_SM_LG = 1
     temp = WIDGET_INFO(struct_obj.BUTTON_MATCHER_MEDIUM, /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.MATCHER_SM_LG = 2
     temp = WIDGET_INFO(struct_obj.BUTTON_MATCHER_LARGE,  /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.MATCHER_SM_LG = 3
     temp = WIDGET_INFO(struct_obj.BUTTON_MATCHER_XLARGE, /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.MATCHER_SM_LG = 4

     temp = WIDGET_INFO(struct_obj.BUTTON_A_CAM_PAIRS,  /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 1
     temp = WIDGET_INFO(struct_obj.BUTTON_AB_CAM_PAIRS,  /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 2
     temp = WIDGET_INFO(struct_obj.BUTTON_ABC_CAM_PAIRS, /BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 3
     temp = WIDGET_INFO(struct_obj.BUTTON_ABCD_CAM_PAIRS,/BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 4
     temp = WIDGET_INFO(struct_obj.BUTTON_CD_CAM_PAIRS,/BUTTON_SET)
     IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 5

;     IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
;        temp = WIDGET_INFO(struct_obj.BUTTON_CD_CAM_PAIRS, /BUTTON_SET)
;        IF (temp EQ 1) THEN struct_obj.USE_CAM_PAIRS = 5
;     ENDIF

;------------ SAMPSPAC must follow MATCHER_LORES
      temp = WIDGET_INFO(struct_obj.BUTTON_LO_SPACING,  /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 0.550
      temp = WIDGET_INFO(struct_obj.BUTTON_MED_SPACING, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 1.100
      temp = WIDGET_INFO(struct_obj.BUTTON_HI_SPACING,  /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 2.200
      temp = WIDGET_INFO(struct_obj.BUTTON_VHI_SPACING, /BUTTON_SET)
      IF (temp EQ 1) THEN struct_obj.SAMP_SPAC = 3.300

;------------
      struct_obj.JPEG_OR_MP4 = $
             WIDGET_INFO(struct_obj.BUTTON_JPGMP4, /BUTTON_SET)
      struct_obj.DOCS_ON_MAPS = $
             WIDGET_INFO(struct_obj.BUTTON_MAPDOCS, /BUTTON_SET)
      struct_obj.PUB_QUALITY = $
             WIDGET_INFO(struct_obj.BUTTON_PUBQUAL, /BUTTON_SET)
      struct_obj.DRAW_AEROSOL = $
             WIDGET_INFO(struct_obj.BUTTON_DAER, /BUTTON_SET)
      IF (WIDGET_INFO(struct_obj.BUTTON_CSTER, /BUTTON_SET)) THEN $
         struct_obj.COMPARE_PGEHTS = 1
      IF (WIDGET_INFO(struct_obj.BUTTON_CLOUD, /BUTTON_SET)) THEN $
         struct_obj.COMPARE_PGEHTS = 2
      IF (WIDGET_INFO(struct_obj.BUTTON_NOST, /BUTTON_SET))  THEN $
         struct_obj.COMPARE_PGEHTS = 0
;------------
      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
        struct_obj.SHOW_USER_DIALOG = $
             WIDGET_INFO(struct_obj.BUTTON_USER_DIALOG, /BUTTON_SET)
        struct_obj.SHOW_REF_CMP = $
             WIDGET_INFO(struct_obj.BUTTON_SREFCMP, /BUTTON_SET)
        struct_obj.DRAW_CORR_MTRX = $
             WIDGET_INFO(struct_obj.BUTTON_DCORR, /BUTTON_SET)
        struct_obj.PRINT_OFFSETS = $
             WIDGET_INFO(struct_obj.BUTTON_PRNTOFF, /BUTTON_SET)
        struct_obj.AUTO_CORR_AN = $
             WIDGET_INFO(struct_obj.BUTTON_ACORR, /BUTTON_SET)
        struct_obj.DRAW_OFFSET_DIFF = $
             WIDGET_INFO(struct_obj.BUTTON_DDIFF, /BUTTON_SET)
        struct_obj.SHOW_3D_HEIGHTS = $
             WIDGET_INFO(struct_obj.BUTTON_3DHTS, /BUTTON_SET)
        struct_obj.DRAW_3DSOLUA = $
             WIDGET_INFO(struct_obj.BUTTON_3DSOLUA, /BUTTON_SET)
        struct_obj.DRAW_3DSOLUB = $
             WIDGET_INFO(struct_obj.BUTTON_3DSOLUB, /BUTTON_SET)
      ENDIF
;------------
      struct_obj.CANCELorOK = 1

      StructObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
;------------
   event.id EQ struct_obj.BUTTON_CANCEL : BEGIN
      struct_obj.CANCELorOK = 0
      StructObject = struct_obj
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
;------------
   event.id EQ struct_obj.BUTTON_RESET : BEGIN
      ResetDigitizingDialog, struct_obj
   END
;------------
   event.id EQ struct_obj.BUTTON_PDFHELP : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_DigitizeDialog.pdf'
   END
;------------

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=struct_obj, /NO_COPY

END  ;  plume_gui_event

;***************************************************************************
PRO plume_gui, State, SVal
;***************************************************************************
; Digitizing dialog box widget definitions.
; Note that the !VAR global parameter is saved to the "preferences" file on
; successful completion here. Also note that the "RESET" button restores the
; dialog parameters to the MINX default state, not to the state the user
; last saved to the "Preferences" file before making changes.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON dig_obj, StructObject

  whichorbit = (State.curframe GT 9)

  xmargin_size = (!KON.Misc.MINX_PLATFORM EQ 2) ? 20 : 28
  ymargin_size = 20
  label_size   = (!KON.Misc.MINX_PLATFORM EQ 2) ? 16 : 18

;------------
  WID_BASE_0 = Widget_Base( $
       GROUP_LEADER=WIDGET_INFO(LONG(State.wTopWorkBase), /PARENT), /MODAL, $
       XOFFSET=5, YOFFSET=5, XSIZE=999, YSIZE=999, TITLE='Digitizing Options', $
       /TLB_KILL_REQUEST_EVENTS)

;------------
  WID_LABEL_1 = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
      VALUE='Aerosol Type:', XOFFSET=xmargin_size, YOFFSET=label_size)

  WID_BASE_1 = Widget_Base(WID_BASE_0, FRAME=1, COLUMN=1, /EXCLUSIVE, $
      TITLE='IDL', XOFFSET=xmargin_size, YOFFSET=2*label_size)

  WID_BUTTON_DUST = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
      TOOLTIP='Dust lifted from arid regions.', VALUE='Dust')

  WID_BUTTON_SMOKE = Widget_Button(WID_BASE_1, /ALIGN_LEFT, VALUE='Smoke', $
      TOOLTIP='Smoke from wildfires, industrial processes or other sources.')

  WID_BUTTON_VOLCASH = Widget_Button(WID_BASE_1, /ALIGN_LEFT, VALUE='Volcanic ash', $
      TOOLTIP='Ash and other aerosol products from volcanic eruptions.')

  WID_BUTTON_WATER = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
      TOOLTIP='Water clouds and blowing snow.', VALUE='Cloud/snow')

  WID_BUTTON_CONTRAIL = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
      TOOLTIP='Water condensation trails from aircraft engines.', VALUE='Contrails')

  WID_BUTTON_OTHER = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
      TOOLTIP='Any aerosol not covered by other choices.', VALUE='Other aerosol')

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
     WID_BUTTON_GROUND = -1
  ENDIF ELSE BEGIN
     WID_BUTTON_GROUND = Widget_Button(WID_BASE_1, /ALIGN_LEFT, $
       TOOLTIP='Differences between computed and DEM terrain heights.', $
       VALUE='Terrain error')
  ENDELSE

  geom_info_1 = WIDGET_INFO(WID_BASE_1, /GEOMETRY)

;------------
  
  WID_LABEL_4 = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
      XOFFSET=xmargin_size, YOFFSET=geom_info_1.SCR_YSIZE + ymargin_size + $
      2 * label_size, VALUE='Display Options:')

  WID_BASE_4 = Widget_Base(WID_BASE_0, FRAME=1, COLUMN=1, $
      XOFFSET=xmargin_size, YOFFSET=geom_info_1.SCR_YSIZE + ymargin_size + $
      3 * label_size, TITLE='IDL')

  WID_BUTTON_USER_DIALOG = -1
  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
     WID_BASE_4z = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
         /NONEXCLUSIVE)
     WID_BUTTON_USER_DIALOG = Widget_Button(WID_BASE_4z, /ALIGN_LEFT, $
         TOOLTIP='Show dialog box for user entry of information that ' + $
                 'characterizes the current aerosol plume or cloud.', $
         VALUE='Show user evaluation dialog')
  ENDIF

  WID_BASE_4y = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
      /NONEXCLUSIVE)

  WID_BUTTON_JPGMP4 = Widget_Button(WID_BASE_4y, /ALIGN_LEFT, $
      TOOLTIP='Save 9 camera images as an MP4 animation if you have a ' + $
              'license and are willing to risk a crash on large images. ' + $
              'Otherwise save 9 JPEG files.', VALUE='Save animation as MP4')

  WID_BASE_4c = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
      /NONEXCLUSIVE)

  WID_BUTTON_DAER = Widget_Button(WID_BASE_4c, /ALIGN_LEFT, $
      TOOLTIP='Show retrieval results from MISR standard aerosol product ' + $
              'as histograms.', VALUE='Show AS_AEROSOL data')

  WID_BASE_4a = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
      /NONEXCLUSIVE)

  WID_BUTTON_MAPDOCS = Widget_Button(WID_BASE_4a, /ALIGN_LEFT, $
      TOOLTIP='Write plume orbit, date, camera and location of first ' + $
              'digitized point by block and lat/lon.', $
              VALUE='Write info on maps')
      
  WID_BASE_4b = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
              /NONEXCLUSIVE)
              
  WID_BUTTON_PUBQUAL = Widget_Button(WID_BASE_4b, /ALIGN_LEFT, $
      TOOLTIP='Remove unnecessary detail from height/wind profile plot and ' + $
              'camera names from images to provide publication quality.', $
              VALUE='High quality profile plot')

  WID_BASE_4e = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
     /EXCLUSIVE, /FRAME)

  WID_BUTTON_CSTER = Widget_Button(WID_BASE_4e, /ALIGN_LEFT, $
     TOOLTIP='Show MISR standard stereo product height/wind ' + $
             'results in profile plot together with MINX computed ' + $
             'height/wind results.', VALUE='Compare hts w/ TC_STEREO')

  WID_BUTTON_CLOUD = Widget_Button(WID_BASE_4e, /ALIGN_LEFT, $
     TOOLTIP='Show MISR standard cloud product height/wind ' + $
             'results in profile plot together with MINX computed ' + $
             'height/wind results.', VALUE='Compare hts w/ TC_CLOUD')

  WID_BUTTON_NOST = Widget_Button(WID_BASE_4e, /ALIGN_LEFT, $
     TOOLTIP='Show neither MISR standard stereo product height/wind results.', $
             VALUE='Do not compare heights')

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
    WID_BASE_4d = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE)
    WID_BASE_4g = Widget_Base(WID_BASE_4, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE)
  ENDIF

  geom_info_2 = WIDGET_INFO(WID_BASE_4, /GEOMETRY)

  WIDGET_CONTROL, WID_BASE_1, SCR_XSIZE=geom_info_2.SCR_XSIZE
  WIDGET_CONTROL, WID_BASE_4, SCR_XSIZE=geom_info_2.SCR_XSIZE

;------------

  y_off = geom_info_1.SCR_YSIZE + geom_info_2.SCR_YSIZE + $
          ymargin_size + 4*label_size
  WID_LABEL_2 = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
      XOFFSET=xmargin_size, YOFFSET=y_off, $
      VALUE='Wind-Corrected Height Filters:')

  y_off = geom_info_1.SCR_YSIZE + geom_info_2.SCR_YSIZE + $
          ymargin_size + 5*label_size
  WID_BASE_2 = Widget_Base(WID_BASE_0, FRAME=1, COLUMN=1, TITLE='IDL', $
      XOFFSET=xmargin_size, YOFFSET=y_off, XSIZE=geom_info_2.SCR_XSIZE)

  WID_FLOAT_MINHT = CW_FIELD(WID_BASE_2, TITLE='Min hght above terrain (km)  ', $
      /COLUMN, /FLOATING, XSIZE=7, YSIZE=1)

  WID_FLOAT_MAXHT = CW_FIELD(WID_BASE_2, $
      TITLE='Max hght above sea level (km)', $
      VALUE=SVal.MAX_HGHT, /COLUMN, /FLOATING, XSIZE=7, YSIZE=1)

  WID_FLOAT_MAXWND = CW_FIELD(WID_BASE_2, TITLE='~ Maximum wind speed (m/s)   ',$
      VALUE=SVal.MAX_WIND, /COLUMN, /FLOATING, XSIZE=7, YSIZE=1)

  geom_info_x = WIDGET_INFO(WID_BASE_2, /GEOMETRY)
;------------

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
     WID_LABEL_2a = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
        XOFFSET=xmargin_size, YOFFSET=geom_info_1.SCR_YSIZE + $
        geom_info_2.SCR_YSIZE + geom_info_x.SCR_YSIZE + $
        3*ymargin_size + 4*label_size, VALUE='Sample Spacing (km)')

     WID_BASE_2a = Widget_Base(WID_BASE_0, FRAME=1, $
        XOFFSET=xmargin_size, YOFFSET=geom_info_1.SCR_YSIZE + $
        geom_info_2.SCR_YSIZE + geom_info_x.SCR_YSIZE + $
        3*ymargin_size + 5*label_size, TITLE='', ROW=2, $
        /EXCLUSIVE, /ALIGN_LEFT)

     WID_BUTTON_LO_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 2 hi-res pixels.', $
        VALUE='0.55')

     WID_BUTTON_MED_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 4 hi-res pixels.', $
        VALUE='1.1 (default)')

     WID_BUTTON_HI_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 8 hi-res pixels.', $
        VALUE='2.2')

     WID_BUTTON_VHI_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 12 hi-res pixels.', $
        VALUE='3.3')

     geom_info_y = WIDGET_INFO(WID_BASE_2,  /GEOMETRY)
     geom_info_z = WIDGET_INFO(WID_BASE_2a, /GEOMETRY)
     geom_info_3 = geom_info_y.SCR_YSIZE + geom_info_z.SCR_YSIZE
     geom_info_y = 0
     geom_info_z = 0
  ENDIF

;------------
  xoff = geom_info_2.SCR_XSIZE + 2 * xmargin_size

  WID_LABEL_3 = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
      XOFFSET=xoff, YOFFSET=label_size, VALUE='Retrieval Options:')

  WID_BASE_3 = Widget_Base(WID_BASE_0, FRAME=1, TITLE='IDL', COLUMN=1, $
      XOFFSET=xoff, YOFFSET=2*label_size)

  WID_BASE_3a = Widget_Base(WID_BASE_3, FRAME=1, TITLE='', $
      COLUMN=1, /EXCLUSIVE)

  WID_BUTTON_LINE = Widget_Button(WID_BASE_3a, /ALIGN_LEFT, $
      TOOLTIP='Digitize points defining a line. Heights and winds will ' + $
              'be computed at points on a spline-fit line through your ' + $
              'points using local line direction as wind direction.', $
      VALUE='Retrieve along line')

  WID_BUTTON_POLYGON = Widget_Button(WID_BASE_3a, /ALIGN_LEFT, $
      TOOLTIP='Digitize points along exterior boundary of a polygon. ' + $
              'Heights (& optionally winds) will be computed on a grid ' + $
              'of points interior to polygon. Wind direction will be ' + $
              'taken from nearest point on wind direction line.', $
      VALUE='Retrieve inside polygon (default)')

  WID_BASE_3b = Widget_Base(WID_BASE_3, FRAME=1, TITLE='IDL', $
      COLUMN=1, /EXCLUSIVE)

  WID_BUTTON_NOWIND = Widget_Button(WID_BASE_3b, /ALIGN_LEFT, $
      TOOLTIP='The wind-direction is not available, so retrieve zero-wind ' +$
              'heights only. No wind speeds or wind-corrected heights will ' + $
              ' be retrieved. MINX "cloud" designation.', $
      VALUE='Use no wind direction (MINX "cloud")')

  WID_BUTTON_WIND = Widget_Button(WID_BASE_3b, /ALIGN_LEFT, $
      TOOLTIP='Retrieve winds and wind-corrected heights based on a wind ' + $
              'direction that you will provide. MINX "plume" designation.', $
      VALUE='Provide wind direction (MINX "plume")')

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
     WID_BUTTON_MULTIWIND = -1
     WID_BUTTON_NOWIND_X  = -1
  ENDIF ELSE BEGIN
     WID_BUTTON_MULTIWIND = Widget_Button(WID_BASE_3b, /ALIGN_LEFT, $
         TOOLTIP='Attempt to retrieve heights and winds when multiple ' + $
                 'wind-directions have been specified for a region. ' + $
                 'This is not available.', $
         VALUE='Multiple wind directions')
     WID_BUTTON_NOWIND_X = Widget_Button(WID_BASE_3b, /ALIGN_LEFT, $
         TOOLTIP='Attempt to retrieve heights and winds only when the ' + $
                 'wind-direction is not available. This is experimental.', $
         VALUE='Retrieve wind direction')
  ENDELSE

  WID_BASE_3c = Widget_Base(WID_BASE_3, FRAME=1, TITLE='IDL', $
     COLUMN=1, /NONEXCLUSIVE)

  WID_BUTTON_BIDIRWIND = Widget_Button(WID_BASE_3c, /ALIGN_LEFT, $
      TOOLTIP='Compute heights and winds for the chosen direction as well ' +$
              'as 180 degrees in the opposite direction.', $
      VALUE='Bi-directional wind')
      
  WID_BASE_3h = Widget_Base(WID_BASE_3, FRAME=1, TITLE='IDL', $
      COLUMN=1, /NONEXCLUSIVE)
  WID_BUTTON_AS_V23 = Widget_Button(WID_BASE_3h, /ALIGN_LEFT, $
    TOOLTIP='Use NetCDF4/HDF5 version 23 AEROSOL files.', VALUE='V23 Aerosol')      

  WID_BASE_3d = Widget_Base(WID_BASE_3, FRAME=1, TITLE='', $
      COLUMN=1, /EXCLUSIVE)

  show_bands = 1
  IF (CoordStruct.(whichorbit).num_band EQ 1) THEN show_bands = 0

  WID_BUTTON_BAND_RED = Widget_Button(WID_BASE_3d, /ALIGN_LEFT, $
      TOOLTIP='Perform high-resolution image matching using red channels.', $
      VALUE='Match w/ red band (hi-res)')
  WID_BUTTON_BAND_BLUE = Widget_Button(WID_BASE_3d, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching using blue bands. Red band 275 meter ' + $
              'variability will be transferred to blue lo-res channels.', $
      VALUE='Match w/ blue band (pseudo hi-res)', SENSITIVE=show_bands)
  WID_BUTTON_BAND_BOTH = Widget_Button(WID_BASE_3d, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching first using red band and then' + $
              'using blue band.', SENSITIVE=show_bands, $
      VALUE='Match twice: w/ red and blue (default)')
  WID_BUTTON_BAND_RB = Widget_Button(WID_BASE_3d, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching using red band over deep water and ' + $
              'blue band over land.', SENSITIVE=show_bands, $
      VALUE='Match w/ blue over land, red over water')

  WID_BASE_3e = Widget_Base(WID_BASE_3, FRAME=1, TITLE='', $
      COLUMN=1, /EXCLUSIVE)

; Set tooltip string defining small matcher size.

  pass1_patch_size = !KON.CorrParam.CORR_SIZE_PASS1[*, 0]
  patchstr1 = 'pass 1 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr1 += STRTRIM(STRING(pass1_patch_size[icam]),2) + patchchr
  ENDFOR
  pass2_patch_size = !KON.CorrParam.CORR_SIZE_PASS2[*, 0]
  patchstr2 = '[pass 2 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr2 += STRTRIM(STRING(pass2_patch_size[icam]),2) + patchchr
  ENDFOR
  WID_BUTTON_MATCHER_SMALL = Widget_Button(WID_BASE_3e, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching w/ small matcher: ' + patchstr1 + $
              ', ' + patchstr2, VALUE='Small image matcher')

; Set tooltip string defining medium matcher size.

  pass1_patch_size = !KON.CorrParam.CORR_SIZE_PASS1[*, 1]
  patchstr1 = 'pass 1 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr1 += STRTRIM(STRING(pass1_patch_size[icam]),2) + patchchr
  ENDFOR
  pass2_patch_size = !KON.CorrParam.CORR_SIZE_PASS2[*, 1]
  patchstr2 = '[pass 2 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr2 += STRTRIM(STRING(pass2_patch_size[icam]),2) + patchchr
  ENDFOR
  WID_BUTTON_MATCHER_MEDIUM = Widget_Button(WID_BASE_3e, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching w/ medium matcher: ' + patchstr1 + $
              ', ' + patchstr2, VALUE='Medium image matcher (default)')

; Set tooltip string defining large matcher size.

  pass1_patch_size = !KON.CorrParam.CORR_SIZE_PASS1[*, 2]
  patchstr1 = 'pass 1 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr1 += STRTRIM(STRING(pass1_patch_size[icam]),2) + patchchr
  ENDFOR
  pass2_patch_size = !KON.CorrParam.CORR_SIZE_PASS2[*, 2]
  patchstr2 = '[pass 2 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr2 += STRTRIM(STRING(pass2_patch_size[icam]),2) + patchchr
  ENDFOR
  WID_BUTTON_MATCHER_LARGE = Widget_Button(WID_BASE_3e, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching w/ large matcher: ' + patchstr1 + $
              ', ' + patchstr2, VALUE='Large image matcher')

; Set tooltip string defining extra large matcher size.

  pass1_patch_size = !KON.CorrParam.CORR_SIZE_PASS1[*, 3]
  patchstr1 = 'pass 1 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr1 += STRTRIM(STRING(pass1_patch_size[icam]),2) + patchchr
  ENDFOR
  pass2_patch_size = !KON.CorrParam.CORR_SIZE_PASS2[*, 3]
  patchstr2 = '[pass 2 size = ['
  FOR icam=0,8 DO BEGIN
     patchchr = (icam EQ 8) ? ']' : ','
     patchstr2 += STRTRIM(STRING(pass2_patch_size[icam]),2) + patchchr
  ENDFOR
  WID_BUTTON_MATCHER_XLARGE = Widget_Button(WID_BASE_3e, /ALIGN_LEFT, $
      TOOLTIP='Perform image matching w/ extra large matcher: ' + patchstr1 + $
              ', ' + patchstr2, VALUE='X-Large image matcher')

; Set buttons for different camera pairs to use.

  WID_BASE_3f = Widget_Base(WID_BASE_3, FRAME=1, TITLE='', $
      COLUMN=1, /EXCLUSIVE)

  WID_BUTTON_A_CAM_PAIRS = Widget_Button(WID_BASE_3f, /ALIGN_LEFT, $
      TOOLTIP='Attempt to match image template in An camera against 2 A ' + $
              'camera images.', VALUE='Match A cameras')

  WID_BUTTON_AB_CAM_PAIRS = Widget_Button(WID_BASE_3f, /ALIGN_LEFT, $
      TOOLTIP='Attempt to match image template in An camera against 4 ' + $
              'A and B camera images.', VALUE='Match A, B cameras')

  WID_BUTTON_ABC_CAM_PAIRS = Widget_Button(WID_BASE_3f, /ALIGN_LEFT, $
      TOOLTIP='Attempt to match image template in An camera against 6 ' + $
              'A, B and C camera images.', VALUE='Match A, B, C cameras ' + $
              '(default)')

  WID_BUTTON_ABCD_CAM_PAIRS = Widget_Button(WID_BASE_3f, /ALIGN_LEFT, $
      TOOLTIP='Attempt to match image template in An camera against 8 A, B, C ' + $
              'and D camera images.', VALUE='Match A, B, C, D cameras')

  WID_BUTTON_CD_CAM_PAIRS = Widget_Button(WID_BASE_3f, /ALIGN_LEFT, $
      TOOLTIP='Attempt to match image template in An camera against 4 C and D ' + $
              'camera images.', VALUE='Match C, D cameras')

  ; Set level of retrieval precision desired.
                       
   WID_BASE_3g = Widget_Base(WID_BASE_3, FRAME=1, TITLE='', COLUMN=1, $
       /NONEXCLUSIVE)
         
   WID_BUTTON_RELAX_THRESH = Widget_Button(WID_BASE_3g, /ALIGN_LEFT, $
      TOOLTIP='Relax retrieval requirements so retrievals succeed at more ' + $
      'points. Data quality may be reduced. Default is off.', $
      VALUE='Relax retrieval thresholds')
         
  geom_info_4 = WIDGET_INFO(WID_BASE_3, /GEOMETRY)

;------------
  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
     WID_LABEL_2a = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
        VALUE='Sample Spacing (km)', XOFFSET=geom_info_4.xoffset, $
        YOFFSET=geom_info_4.SCR_YSIZE + 3*label_size)

     WID_BASE_2a = Widget_Base(WID_BASE_0, FRAME=1, TITLE='', $
        ROW=1, /EXCLUSIVE, /ALIGN_LEFT, XOFFSET=geom_info_4.xoffset, $
        YOFFSET=geom_info_4.SCR_YSIZE + 4*label_size)

     WID_BUTTON_LO_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 2 hi-res pixels.', $
        VALUE='0.55')

     dflt_txt = (!KON.Misc.MINX_PLATFORM EQ 1) ? '1.1 (default)' : '1.1 (dflt)'
     WID_BUTTON_MED_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 4 hi-res pixels.', $
        VALUE=dflt_txt)

     WID_BUTTON_HI_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 8 hi-res pixels.', $
        VALUE='2.2')

     WID_BUTTON_VHI_SPACING = Widget_Button(WID_BASE_2a, /ALIGN_LEFT, $
        TOOLTIP='Distance between attempted retrievals is 12 hi-res pixels.', $
        VALUE='3.3')

     geom_info_y = WIDGET_INFO(WID_BASE_3,  /GEOMETRY)
     geom_info_z = WIDGET_INFO(WID_BASE_2a, /GEOMETRY)
     geom_info_3 = geom_info_y.SCR_YSIZE + geom_info_z.SCR_YSIZE
     geom_info_y = 0
     geom_info_z = 0
  ENDIF

;------------
  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
     WID_BASE_5n = -1L
     WID_BASE_5m = -1L
     WID_BASE_5g = -1L
     WID_BASE_5h = -1L
     WID_BASE_5i = -1L
     WID_BASE_5j = -1L
     WID_BASE_5k = -1L
     WID_BASE_5l = -1L
     WID_BUTTON_PRNTOFF = -1L
     WID_BUTTON_ACORR   = -1L
     WID_BUTTON_SREFCMP = -1L
     WID_BUTTON_DCORR   = -1L
     WID_BUTTON_DDIFF   = -1L
     WID_BUTTON_3DHTS   = -1L
     WID_BUTTON_3DSOLUA = -1L
     WID_BUTTON_3DSOLUB = -1L
     geom_info_5 = WIDGET_INFO(WID_BASE_4, /GEOMETRY)
                   
     geom_info_5.SCR_XSIZE = 0
     geom_info_5.SCR_YSIZE = 0
  ENDIF ELSE BEGIN
    WID_LABEL_5 = Widget_Label(WID_BASE_0, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
        XOFFSET=geom_info_2.SCR_XSIZE + 2 * xmargin_size, $
        YOFFSET=geom_info_4.SCR_YSIZE + ymargin_size + 2 * label_size, $
        VALUE='Debugging Options: (some of these may not work!)')

    WID_BASE_5 = Widget_Base(WID_BASE_0, FRAME=1, TITLE='IDL', COLUMN=1, $
        XOFFSET=geom_info_2.SCR_XSIZE + 2 * xmargin_size, $
        YOFFSET=geom_info_4.SCR_YSIZE + ymargin_size + 3 * label_size)

    WID_BASE_5n = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_PRNTOFF = Widget_Button(WID_BASE_5n, /ALIGN_LEFT, $
        TOOLTIP='Print per-camera offsets, heights and winds to a unique ' + $
        'file for each region digitized.', VALUE='Print disparity files')

    WID_BASE_5m = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_ACORR = Widget_Button(WID_BASE_5m, /ALIGN_LEFT, $
        TOOLTIP='Autocorrelate An camera to validate that correlation ' + $
        'windows are aligned.', VALUE='Auto-correlate An camera')

    WID_BASE_5g = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_SREFCMP = Widget_Button(WID_BASE_5g, /ALIGN_LEFT,$
        TOOLTIP='Show reference and comparison camera image patches.', $
        VALUE='Show matcher images/data')

    WID_BASE_5h = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_DCORR = Widget_Button(WID_BASE_5h, /ALIGN_LEFT, $
        TOOLTIP='Show surface depicting image matcher correlation ' + $
        'coefficients.', VALUE='Show correlation surfaces')

    WID_BASE_5i = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_DDIFF = Widget_Button(WID_BASE_5i, /ALIGN_LEFT, $
        TOOLTIP='Show graphs and surfaces of "computed - observed" ' + $
        'An/Cx pixel offsets.', VALUE='Show disparity plots')

    WID_BASE_5j = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_3DHTS = Widget_Button(WID_BASE_5j, /ALIGN_LEFT, $
        TOOLTIP='Show 3D interactive cube of heights and terrain ' + $
        '(available only in idlde).', VALUE='Show 3D height cube')

    WID_BASE_5k = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_3DSOLUA = Widget_Button(WID_BASE_5k, /ALIGN_LEFT, $
        TOOLTIP='Show 3D interactive cube of ht-wnd-wnd solution volume ' + $
        'for 1 camera (available only in idlde).', $
        VALUE='Show 3D solution volume')

    WID_BASE_5l = Widget_Base(WID_BASE_5, TITLE='IDL', COLUMN=1, $
        /NONEXCLUSIVE, YSIZE=17)
    WID_BUTTON_3DSOLUB = Widget_Button(WID_BASE_5l, /ALIGN_LEFT, $
        TOOLTIP='Show 3D interactive cube of ht-wnd-wnd solution lines ' + $
        'for all successful cameras (available only in idlde).', $
        VALUE='Show 3D solution lines')

     geom_info_5 = WIDGET_INFO(WID_BASE_5, /GEOMETRY)
     WIDGET_CONTROL, WID_BASE_5, SCR_XSIZE=geom_info_4.SCR_XSIZE - 7

  ENDELSE

;------------
  xsizeokcan = 65
  ysizeokcan = 25
  
  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
     wndw_xtras = (!KON.Misc.MINX_PLATFORM EQ 2) ? 3.0 : 2.5
     xmainsize = geom_info_2.SCR_XSIZE + geom_info_4.SCR_XSIZE + $
            wndw_xtras * xmargin_size
     wndw_xtras = (!KON.Misc.MINX_PLATFORM EQ 2) ? 4.0 : 2.0
     ymainsize = (geom_info_1.SCR_YSIZE + geom_info_2.SCR_YSIZE + $
                  geom_info_x.SCR_YSIZE) > geom_info_3 + $
                 3.25 * ymargin_size + wndw_xtras * label_size

     wndw_xtras_x = 0.2
     wndw_xtras_y = 1.7
     WID_BASE_6 = Widget_Base(WID_BASE_0, /ROW, /ALIGN_CENTER, $
         XOFFSET=4 * xmargin_size + wndw_xtras_x * xsizeokcan, $
         YOFFSET=geom_info_3 + 3 * ymargin_size + $
                 wndw_xtras_y * 0.75 * ysizeokcan)

     WID_BUTTON_OK = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          VALUE='OK', XSIZE=xsizeokcan)
     WID_BUTTON_CANCEL = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          VALUE='Cancel', XSIZE=xsizeokcan)
     WID_BUTTON_RESET = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          VALUE='Reset', XSIZE=xsizeokcan)
     WID_BUTTON_PDFHELP = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
         VALUE='PDF Help', XSIZE=xsizeokcan)

     geom_info_q = WIDGET_INFO(WID_BASE_6, /GEOMETRY)
     ymainsize += geom_info_q.SCR_YSIZE

  ENDIF ELSE BEGIN
     xmainsize = geom_info_2.SCR_XSIZE + geom_info_4.SCR_XSIZE + $
            2.5 * xmargin_size
     ymainsize = geom_info_4.SCR_YSIZE + geom_info_5.SCR_YSIZE + $
            3 * ymargin_size + 4 * label_size

     WID_BASE_6 = Widget_Base(WID_BASE_0, /ROW, $
         XOFFSET= 2.0 * xsizeokcan, YOFFSET=ymainsize - 1.5 * ysizeokcan)

     WID_BUTTON_OK = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          XSIZE=xsizeokcan, YSIZE=ysizeokcan, VALUE='OK')
     WID_BUTTON_CANCEL = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          XSIZE=xsizeokcan, YSIZE=ysizeokcan, VALUE='Cancel')
     WID_BUTTON_RESET = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
          XSIZE=xsizeokcan, YSIZE=ysizeokcan, VALUE='Reset')
     WID_BUTTON_PDFHELP = Widget_Button(WID_BASE_6, /ALIGN_CENTER, $
         XSIZE=xsizeokcan, YSIZE=ysizeokcan, VALUE='PDF Help')
  ENDELSE

  geom_info_1 = 0
  geom_info_2 = 0
  geom_info_3 = 0
  geom_info_4 = 0
  geom_info_5 = 0
  geom_info_x = 0

;------------

  WIDGET_CONTROL, WID_BASE_0, SCR_XSIZE=xmainsize
  WIDGET_CONTROL, WID_BASE_0, SCR_YSIZE=ymainsize

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

  struct_obj = {                                          $
     BUTTON_SMOKE           : WID_BUTTON_SMOKE,           $
     BUTTON_VOLCASH         : WID_BUTTON_VOLCASH,         $
     BUTTON_DUST            : WID_BUTTON_DUST,            $
     BUTTON_WATER           : WID_BUTTON_WATER,           $
     BUTTON_CONTRAIL        : WID_BUTTON_CONTRAIL,        $
     BUTTON_OTHER           : WID_BUTTON_OTHER,           $
     BUTTON_GROUND          : WID_BUTTON_GROUND,          $
     BUTTON_LINE            : WID_BUTTON_LINE,            $
     BUTTON_POLYGON         : WID_BUTTON_POLYGON,         $
     BUTTON_NOWIND          : WID_BUTTON_NOWIND,          $
     BUTTON_WIND            : WID_BUTTON_WIND,            $
     BUTTON_MULTIWIND       : WID_BUTTON_MULTIWIND,       $
     BUTTON_NOWIND_X        : WID_BUTTON_NOWIND_X,        $
     FLOAT_MAXHT            : WID_FLOAT_MAXHT,            $
     FLOAT_MINHT            : WID_FLOAT_MINHT,            $
     FLOAT_MAXWND           : WID_FLOAT_MAXWND,           $
     BUTTON_LO_SPACING      : WID_BUTTON_LO_SPACING,      $
     BUTTON_MED_SPACING     : WID_BUTTON_MED_SPACING,     $
     BUTTON_HI_SPACING      : WID_BUTTON_HI_SPACING,      $
     BUTTON_VHI_SPACING     : WID_BUTTON_VHI_SPACING,     $
     BUTTON_BIDIRWIND       : WID_BUTTON_BIDIRWIND,       $
     BUTTON_AS_V23          : WID_BUTTON_AS_V23,          $
     BUTTON_RELAX_THRESH    : WID_BUTTON_RELAX_THRESH,    $
     BUTTON_BAND_RED        : WID_BUTTON_BAND_RED,        $
     BUTTON_BAND_BLUE       : WID_BUTTON_BAND_BLUE,       $
     BUTTON_BAND_RB         : WID_BUTTON_BAND_RB,         $
     BUTTON_BAND_BOTH       : WID_BUTTON_BAND_BOTH,       $
     BUTTON_MATCHER_SMALL   : WID_BUTTON_MATCHER_SMALL,   $
     BUTTON_MATCHER_MEDIUM  : WID_BUTTON_MATCHER_MEDIUM,  $
     BUTTON_MATCHER_LARGE   : WID_BUTTON_MATCHER_LARGE,   $
     BUTTON_MATCHER_XLARGE  : WID_BUTTON_MATCHER_XLARGE,  $
     BUTTON_A_CAM_PAIRS     : WID_BUTTON_A_CAM_PAIRS,     $
     BUTTON_AB_CAM_PAIRS    : WID_BUTTON_AB_CAM_PAIRS,    $
     BUTTON_ABC_CAM_PAIRS   : WID_BUTTON_ABC_CAM_PAIRS,   $
     BUTTON_ABCD_CAM_PAIRS  : WID_BUTTON_ABCD_CAM_PAIRS,  $
     BUTTON_CD_CAM_PAIRS    : WID_BUTTON_CD_CAM_PAIRS,    $
     BUTTON_USER_DIALOG     : WID_BUTTON_USER_DIALOG,     $
     BUTTON_JPGMP4          : WID_BUTTON_JPGMP4,          $
     BUTTON_DAER            : WID_BUTTON_DAER,            $
     BUTTON_MAPDOCS         : WID_BUTTON_MAPDOCS,         $
     BUTTON_PUBQUAL         : WID_BUTTON_PUBQUAL,         $
     BUTTON_CSTER           : WID_BUTTON_CSTER,           $
     BUTTON_CLOUD           : WID_BUTTON_CLOUD,           $
     BUTTON_NOST            : WID_BUTTON_NOST,            $
     BUTTON_PRNTOFF         : WID_BUTTON_PRNTOFF,         $
     BUTTON_ACORR           : WID_BUTTON_ACORR,           $
     BUTTON_SREFCMP         : WID_BUTTON_SREFCMP,         $
     BUTTON_DCORR           : WID_BUTTON_DCORR,           $
     BUTTON_DDIFF           : WID_BUTTON_DDIFF,           $
     BUTTON_3DHTS           : WID_BUTTON_3DHTS,           $
     BUTTON_3DSOLUA         : WID_BUTTON_3DSOLUA,         $
     BUTTON_3DSOLUB         : WID_BUTTON_3DSOLUB,         $
     BUTTON_OK              : WID_BUTTON_OK,              $
     BUTTON_CANCEL          : WID_BUTTON_CANCEL,          $
     BUTTON_RESET           : WID_BUTTON_RESET,           $
     BUTTON_PDFHELP         : WID_BUTTON_PDFHELP,         $
;-----------------
     AER_TYPE               : SVal.AER_TYPE,              $
     JPEG_OR_MP4            : SVal.JPEG_OR_MP4,           $
     DRAW_AEROSOL           : SVal.DRAW_AEROSOL,          $
     DOCS_ON_MAPS           : SVal.DOCS_ON_MAPS,          $
     PUB_QUALITY            : SVal.PUB_QUALITY,           $
     COMPARE_PGEHTS         : SVal.COMPARE_PGEHTS,        $
     MIN_HGHT               : SVal.MIN_HGHT,              $
     MAX_HGHT               : SVal.MAX_HGHT,              $
     MAX_WIND               : SVal.MAX_WIND,              $
     GEOM_TYPE              : SVal.GEOM_TYPE,             $
     WIND_TYPE              : SVal.WIND_TYPE,             $
     BI_DIR_WIND            : SVal.BI_DIR_WIND,           $
     AS_22_OR_23            : SVal.AS_22_OR_23,           $     
     RETRIEVE_BAND_TYPE     : SVal.RETRIEVE_BAND_TYPE,    $
     MATCHER_SM_LG          : SVal.MATCHER_SM_LG,         $
     RELAX_THRESH           : SVal.RELAX_THRESH,          $
     USE_CAM_PAIRS          : SVal.USE_CAM_PAIRS,         $
     SAMP_SPAC              : SVal.SAMP_SPAC,             $
;--------
     SHOW_USER_DIALOG       : SVal.SHOW_USER_DIALOG,      $
     PRINT_OFFSETS          : SVal.PRINT_OFFSETS,         $
     AUTO_CORR_AN           : SVal.AUTO_CORR_AN,          $
     SHOW_REF_CMP           : SVal.SHOW_REF_CMP,          $
     DRAW_CORR_MTRX         : SVal.DRAW_CORR_MTRX,        $
     DRAW_OFFSET_DIFF       : SVal.DRAW_OFFSET_DIFF,      $
     SHOW_3D_HEIGHTS        : SVal.SHOW_3D_HEIGHTS,       $
     DRAW_3DSOLUA           : SVal.DRAW_3DSOLUA,          $
     DRAW_3DSOLUB           : SVal.DRAW_3DSOLUB,          $
;--------
     CANCELorOK             : 0 }

   StructObject = struct_obj

;---------------------------------------------------------------------------
; Set default button values.
;---------------------------------------------------------------------------

  IF (SVal.AER_TYPE EQ 1) THEN WIDGET_CONTROL, $
     WID_BUTTON_DUST, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 2) THEN WIDGET_CONTROL, $
     WID_BUTTON_SMOKE, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 3) THEN WIDGET_CONTROL, $
     WID_BUTTON_VOLCASH, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 4) THEN WIDGET_CONTROL, $
     WID_BUTTON_WATER, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 5) THEN WIDGET_CONTROL, $
     WID_BUTTON_CONTRAIL, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 6) THEN WIDGET_CONTROL, $
     WID_BUTTON_OTHER, /SET_BUTTON
  IF (SVal.AER_TYPE EQ 7) THEN WIDGET_CONTROL, $
     WID_BUTTON_GROUND, /SET_BUTTON

  IF (SVal.GEOM_TYPE EQ 2) THEN BEGIN
     WIDGET_CONTROL, WID_BUTTON_LINE, /SET_BUTTON
     WIDGET_CONTROL, WID_BUTTON_WIND, /SET_BUTTON
     WIDGET_CONTROL, WID_BUTTON_NOWIND, SENSITIVE=0
     SVal.WIND_TYPE = 2
  ENDIF
  IF (SVal.GEOM_TYPE EQ 3) THEN $
     WIDGET_CONTROL, WID_BUTTON_POLYGON, /SET_BUTTON

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
     WIDGET_CONTROL, WID_BUTTON_MULTIWIND, SENSITIVE=0
     WIDGET_CONTROL, WID_BUTTON_NOWIND_X,  SENSITIVE=0
  ENDIF

  IF (SVal.WIND_TYPE EQ 1) THEN BEGIN
     WIDGET_CONTROL, WID_BUTTON_NOWIND,  /SET_BUTTON
     WIDGET_CONTROL, WID_BUTTON_POLYGON, /SET_BUTTON
     WIDGET_CONTROL, WID_BUTTON_LINE, SENSITIVE=0
     SVal.GEOM_TYPE = 3
  ENDIF
  IF (SVal.WIND_TYPE EQ 2) THEN $
     WIDGET_CONTROL, WID_BUTTON_WIND, /SET_BUTTON
  IF (SVal.WIND_TYPE EQ 3) THEN $
     WIDGET_CONTROL, WID_BUTTON_MULTIWIND, /SET_BUTTON
  IF (SVal.WIND_TYPE EQ 4) THEN $
     WIDGET_CONTROL, WID_BUTTON_NOWIND_X, /SET_BUTTON

  WIDGET_CONTROL, WID_FLOAT_MINHT,  SET_VALUE=SVal.MIN_HGHT
  WIDGET_CONTROL, WID_FLOAT_MAXHT,  SET_VALUE=SVal.MAX_HGHT
  WIDGET_CONTROL, WID_FLOAT_MAXWND, SET_VALUE=SVal.MAX_WIND

  IF (SVal.SAMP_SPAC EQ 0.550) THEN $
     WIDGET_CONTROL, WID_BUTTON_LO_SPACING,  /SET_BUTTON
  IF (SVal.SAMP_SPAC EQ 1.100) THEN $
     WIDGET_CONTROL, WID_BUTTON_MED_SPACING, /SET_BUTTON
  IF (SVal.SAMP_SPAC EQ 2.200) THEN $
     WIDGET_CONTROL, WID_BUTTON_HI_SPACING,  /SET_BUTTON
  IF (SVal.SAMP_SPAC EQ 3.300) THEN $
     WIDGET_CONTROL, WID_BUTTON_VHI_SPACING, /SET_BUTTON

  IF (SVal.BI_DIR_WIND EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_BIDIRWIND, /SET_BUTTON
  IF (SVal.AS_22_OR_23  EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_AS_V23, /SET_BUTTON     
  IF (SVal.RELAX_THRESH EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_RELAX_THRESH, /SET_BUTTON
  IF (CoordStruct.(whichorbit).num_band EQ 1) THEN $
     SVal.RETRIEVE_BAND_TYPE = 0

  IF (SVal.RETRIEVE_BAND_TYPE EQ 0) THEN $
     WIDGET_CONTROL, WID_BUTTON_BAND_RED, /SET_BUTTON
  IF (SVal.RETRIEVE_BAND_TYPE EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_BAND_BLUE,/SET_BUTTON
  IF (SVal.RETRIEVE_BAND_TYPE EQ 2) THEN $
     WIDGET_CONTROL, WID_BUTTON_BAND_BOTH,/SET_BUTTON
  IF (SVal.RETRIEVE_BAND_TYPE EQ 3) THEN $
     WIDGET_CONTROL, WID_BUTTON_BAND_RB,  /SET_BUTTON

  IF (SVal.MATCHER_SM_LG EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_MATCHER_SMALL, /SET_BUTTON
  IF (SVal.MATCHER_SM_LG EQ 2) THEN $
     WIDGET_CONTROL, WID_BUTTON_MATCHER_MEDIUM,/SET_BUTTON
  IF (SVal.MATCHER_SM_LG EQ 3) THEN $
     WIDGET_CONTROL, WID_BUTTON_MATCHER_LARGE, /SET_BUTTON
  IF (SVal.MATCHER_SM_LG EQ 4) THEN $
     WIDGET_CONTROL, WID_BUTTON_MATCHER_XLARGE,/SET_BUTTON

  IF (SVal.USE_CAM_PAIRS EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_A_CAM_PAIRS,   /SET_BUTTON
  IF (SVal.USE_CAM_PAIRS EQ 2) THEN $
     WIDGET_CONTROL, WID_BUTTON_AB_CAM_PAIRS,  /SET_BUTTON
  IF (SVal.USE_CAM_PAIRS EQ 3) THEN $
     WIDGET_CONTROL, WID_BUTTON_ABC_CAM_PAIRS, /SET_BUTTON
  IF (SVal.USE_CAM_PAIRS EQ 4) THEN $
     WIDGET_CONTROL, WID_BUTTON_ABCD_CAM_PAIRS,/SET_BUTTON
  IF (SVal.USE_CAM_PAIRS EQ 5) THEN $
     WIDGET_CONTROL, WID_BUTTON_CD_CAM_PAIRS,  /SET_BUTTON

  IF (SVal.SHOW_USER_DIALOG EQ 1 AND $
      !VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN $
     WIDGET_CONTROL, WID_BUTTON_USER_DIALOG, /SET_BUTTON
  IF (SVal.JPEG_OR_MP4  EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_JPGMP4, /SET_BUTTON
  IF (SVal.DOCS_ON_MAPS EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_MAPDOCS, /SET_BUTTON
  IF (SVal.PUB_QUALITY EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_PUBQUAL, /SET_BUTTON
  IF (SVal.DRAW_AEROSOL EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_DAER, /SET_BUTTON
  IF (SVal.COMPARE_PGEHTS EQ 1) THEN $
     WIDGET_CONTROL, WID_BUTTON_CSTER, /SET_BUTTON
  IF (SVal.COMPARE_PGEHTS EQ 2) THEN $
     WIDGET_CONTROL, WID_BUTTON_CLOUD, /SET_BUTTON
  IF (SVal.COMPARE_PGEHTS EQ 0) THEN $
     WIDGET_CONTROL, WID_BUTTON_NOST, /SET_BUTTON

  IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
    IF (SVal.SHOW_REF_CMP EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_SREFCMP, /SET_BUTTON
    IF (SVal.DRAW_CORR_MTRX EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_DCORR, /SET_BUTTON
    IF (SVal.PRINT_OFFSETS GE 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_PRNTOFF, /SET_BUTTON
    IF (SVal.AUTO_CORR_AN EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_ACORR, /SET_BUTTON
    IF (SVal.DRAW_OFFSET_DIFF EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_DDIFF, /SET_BUTTON
    IF (SVal.SHOW_3D_HEIGHTS EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_3DHTS, /SET_BUTTON
    IF (SVal.DRAW_3DSOLUA EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_3DSOLUA, /SET_BUTTON
    IF (SVal.DRAW_3DSOLUB EQ 1) THEN $
       WIDGET_CONTROL, WID_BUTTON_3DSOLUB, /SET_BUTTON
  ENDIF

;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

  WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=struct_obj, XOFFSET=700, $
                  YOFFSET=150, /NO_COPY

  WIDGET_CONTROL, /REALIZE, WID_BASE_0

  XMANAGER, 'plume_gui', WID_BASE_0, EVENT_HANDLER='plume_gui_event'

END  ;  plume_gui

;***************************************************************************
PRO CallPlumeGUI, State, Retval
;***************************************************************************
; Get values from user via GUI for type of object to digitize, what kind of
; wind to use and what displays to plot.
;---------------------------------------------------------------------------

COMMON dig_obj, StructObject

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Initialize values in structure for GUI.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.FIRST_CAM_USE EQ 0 AND $
       !SAV.Digitize.LAST_CAM_USE  EQ 1) THEN Use_Cam_Pairs = 1
   IF (!SAV.Digitize.FIRST_CAM_USE EQ 0 AND $
       !SAV.Digitize.LAST_CAM_USE  EQ 3) THEN Use_Cam_Pairs = 2
   IF (!SAV.Digitize.FIRST_CAM_USE EQ 0 AND $
       !SAV.Digitize.LAST_CAM_USE  EQ 5) THEN Use_Cam_Pairs = 3
   IF (!SAV.Digitize.FIRST_CAM_USE EQ 0 AND $
       !SAV.Digitize.LAST_CAM_USE  EQ 7) THEN Use_Cam_Pairs = 4
   IF (!SAV.Digitize.FIRST_CAM_USE EQ 4 AND $
       !SAV.Digitize.LAST_CAM_USE  EQ 7) THEN Use_Cam_Pairs = 5

   show_3d = !SAV.Digitize.SHOW_3D_HEIGHTS

   StructValues = { $
         AER_TYPE           : !SAV.Digitize.AER_TYPE, $
         JPEG_OR_MP4        : !SAV.Digitize.JPEG_OR_MP4, $
         DRAW_AEROSOL       : !SAV.Digitize.DRAW_AEROSOL, $
         DOCS_ON_MAPS       : !SAV.Digitize.DOCS_ON_MAPS, $
         PUB_QUALITY        : !SAV.Digitize.PUB_QUALITY, $
         COMPARE_PGEHTS     : !SAV.Digitize.COMPARE_PGEHTS, $
         MIN_HGHT           : !SAV.Digitize.MIN_HGHT, $
         MAX_HGHT           : !SAV.Digitize.MAX_HGHT, $
         MAX_WIND           : !SAV.Digitize.MAX_WIND, $
         GEOM_TYPE          : !SAV.Digitize.GEOM_TYPE, $
         WIND_TYPE          : !SAV.Digitize.WIND_TYPE, $
         BI_DIR_WIND        : !SAV.Digitize.BI_DIR_WIND, $
         AS_22_OR_23        : !SAV.Digitize.AS_22_OR_23, $
         RETRIEVE_BAND_TYPE : !SAV.Digitize.RETRIEVE_BAND_TYPE, $
         MATCHER_SM_LG      : !SAV.Digitize.MATCHER_SM_LG, $
         RELAX_THRESH       : !SAV.Digitize.RELAX_THRESH, $
         USE_CAM_PAIRS      :  Use_Cam_Pairs, $
         SAMP_SPAC          : (!SAV.Digitize.WIND_TYPE GE $
                               !KON.WindObjTyp.WIND_USER_DIREC_OBJ) ? $
                               !SAV.Digitize.SAMP_SPAC_DIR : $
                               !SAV.Digitize.SAMP_SPAC_NODIR, $

         SHOW_USER_DIALOG   : !SAV.Digitize.SHOW_USER_DIALOG, $
         PRINT_OFFSETS      : !SAV.Digitize.PRINT_OFFSETS, $
         AUTO_CORR_AN       : !SAV.Digitize.AUTO_CORR_AN, $
         SHOW_REF_CMP       : !SAV.Digitize.SHOW_REF_CMP, $
         DRAW_CORR_MTRX     : !SAV.Digitize.DRAW_CORR_MTRX, $
         DRAW_OFFSET_DIFF   : !SAV.Digitize.DRAW_OFFSET_DIFF, $
         SHOW_3D_HEIGHTS    : (show_3d AND 1) GT 0 ? 1 : 0, $
         DRAW_3DSOLUA       : (show_3d AND 2) GT 0 ? 1 : 0, $
         DRAW_3DSOLUB       : (show_3d AND 4) GT 0 ? 1 : 0, $

         CANCELorOK         : 0 $
   }

   ;------------------------------------------------------------------------
   ; Call the GUI to get the values.
   ;------------------------------------------------------------------------

   plume_gui, State, StructValues

   ;------------------------------------------------------------------------
   ; If OK, save the values returned.
   ;------------------------------------------------------------------------

   IF (StructObject.CANCELorOK EQ 1) THEN BEGIN

      !SAV.Digitize.AER_TYPE           = StructObject.AER_TYPE
      !SAV.Digitize.GEOM_TYPE          = StructObject.GEOM_TYPE
      !SAV.Digitize.WIND_TYPE          = StructObject.WIND_TYPE
      !SAV.Digitize.MAX_HGHT           = StructObject.MAX_HGHT
      !SAV.Digitize.MIN_HGHT           = StructObject.MIN_HGHT
      !SAV.Digitize.MAX_WIND           = StructObject.MAX_WIND
      IF (!SAV.Digitize.WIND_TYPE GE $
          !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN
         !SAV.Digitize.SAMP_SPAC_DIR   = StructObject.SAMP_SPAC
      ENDIF ELSE BEGIN
         !SAV.Digitize.SAMP_SPAC_NODIR = StructObject.SAMP_SPAC
      ENDELSE
      !SAV.Digitize.BI_DIR_WIND        = StructObject.BI_DIR_WIND
      !SAV.Digitize.AS_22_OR_23        = StructObject.AS_22_OR_23
      !SAV.Digitize.RELAX_THRESH       = StructObject.RELAX_THRESH
      !SAV.Digitize.RETRIEVE_BAND_TYPE = StructObject.RETRIEVE_BAND_TYPE
      !SAV.Digitize.MATCHER_SM_LG      = StructObject.MATCHER_SM_LG
      !SAV.Digitize.USE_CAM_PAIRS      = StructObject.USE_CAM_PAIRS
      !SAV.Digitize.SHOW_USER_DIALOG   = StructObject.SHOW_USER_DIALOG
      !SAV.Digitize.JPEG_OR_MP4        = StructObject.JPEG_OR_MP4
      !SAV.Digitize.DOCS_ON_MAPS       = StructObject.DOCS_ON_MAPS
      !SAV.Digitize.PUB_QUALITY        = StructObject.PUB_QUALITY
      !SAV.Digitize.DRAW_AEROSOL       = StructObject.DRAW_AEROSOL
      !SAV.Digitize.COMPARE_PGEHTS     = StructObject.COMPARE_PGEHTS
      !SAV.Digitize.PRINT_OFFSETS      = StructObject.PRINT_OFFSETS
      !SAV.Digitize.AUTO_CORR_AN       = StructObject.AUTO_CORR_AN
      !SAV.Digitize.SHOW_REF_CMP       = StructObject.SHOW_REF_CMP
      !SAV.Digitize.DRAW_CORR_MTRX     = StructObject.DRAW_CORR_MTRX
      !SAV.Digitize.DRAW_OFFSET_DIFF   = StructObject.DRAW_OFFSET_DIFF
      !SAV.Digitize.SHOW_3D_HEIGHTS    = StructObject.SHOW_3D_HEIGHTS  + $
                                         StructObject.DRAW_3DSOLUA * 2 + $
                                         StructObject.DRAW_3DSOLUB * 4

      cam_prs = StructObject.USE_CAM_PAIRS
      IF (cam_prs EQ 1) THEN !SAV.Digitize.FIRST_CAM_USE = 0
      IF (cam_prs EQ 1) THEN !SAV.Digitize.LAST_CAM_USE  = 1
      IF (cam_prs EQ 2) THEN !SAV.Digitize.FIRST_CAM_USE = 0
      IF (cam_prs EQ 2) THEN !SAV.Digitize.LAST_CAM_USE  = 3
      IF (cam_prs EQ 3) THEN !SAV.Digitize.FIRST_CAM_USE = 0
      IF (cam_prs EQ 3) THEN !SAV.Digitize.LAST_CAM_USE  = 5
      IF (cam_prs EQ 4) THEN !SAV.Digitize.FIRST_CAM_USE = 0
      IF (cam_prs EQ 4) THEN !SAV.Digitize.LAST_CAM_USE  = 7
      IF (cam_prs EQ 5) THEN !SAV.Digitize.FIRST_CAM_USE = 4
      IF (cam_prs EQ 5) THEN !SAV.Digitize.LAST_CAM_USE  = 7

      ;---------------------------------------------------------------------
      ; Parameter PRINT_OFFSETS will print either disparities only (=1) or
      ; disparities plus heights and winds (=2) per camera per point! To use
      ; PRINT_OFFSETS=2, set hardwired 0 in next line to 1.
      ;---------------------------------------------------------------------

      IF (0 AND !SAV.Digitize.PRINT_OFFSETS EQ 1) THEN $
         !SAV.Digitize.PRINT_OFFSETS = 2

      ;---------------------------------------------------------------------
      ; Indicate that mouse clicks will be considered new digitizing.
      ;---------------------------------------------------------------------

      !SAV.Digitize.DIG_STATE = [!SAV.Digitize.AER_TYPE, $
                                 !SAV.Digitize.GEOM_TYPE, $
                                 !SAV.Digitize.WIND_TYPE, $
                                 !SAV.Digitize.RETRIEVE_BAND_TYPE]

      ;---------------------------------------------------------------------
      ; Set the actual index into the data arrays if using either red or
      ; blue retrievals alone. If using red over water and blue over land,
      ; or both red and blue separately, this is set later.
      ;---------------------------------------------------------------------

      IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.RED_BAND) THEN $
         !SAV.Digitize.USE_BAND_NDX = !KON.Instr.RED

      IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.BLUE_BAND) THEN $
         !SAV.Digitize.USE_BAND_NDX = !KON.Instr.BLU

      !VAR.DataRgn.SHOW_BAND_TYPE = !SAV.Digitize.RETRIEVE_BAND_TYPE
      IF (!VAR.DataRgn.SHOW_BAND_TYPE EQ 3) THEN !VAR.DataRgn.SHOW_BAND_TYPE = 0
      
      ;---------------------------------------------------------------------
      ; This is one of the instances where we want to explicitly save the
      ; !VAR global structure. The digitizing parameters are the most often
      ; changed parameters, and this ensures if MINX crashes, that the last
      ; set values are saved.
      ;---------------------------------------------------------------------
      
      WritePreferencesFile

   ENDIF

   ResetFrame, State, 5, 1

   Retval = StructObject.CANCELorOK

END  ;  CallPlumeGUI
