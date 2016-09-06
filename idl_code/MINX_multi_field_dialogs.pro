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
PRO GetCamDialog_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Handle the case if the user cancels via the system button.
   ;------------------------------------------------------------------------
   
   IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
      user_cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Branch to the control that was clicked.
   ;------------------------------------------------------------------------

   CASE event.id OF

      OK_Btn : BEGIN

         ;------------------------------------------------------------------
         ; Get data from the controls.
         ;------------------------------------------------------------------

         WIDGET_CONTROL, Cam_Btns,  GET_VALUE=cam_ndx
         cam_num = cam_ndx
         user_cancel = 0

         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      Cancel_Btn : BEGIN
         user_cancel = 1
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      ELSE:
   ENDCASE

   RETURN

END  ;  GetCamDialog_eh

;***************************************************************************
PRO GetCamDialog, ProdType, FieldName, CamNum, CamName, Cancel
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Initialize return values.
   ;------------------------------------------------------------------------

   Cancel = 0
   cam_num = 4
   user_cancel = 0

   ;------------------------------------------------------------------------
   ; Define controls in widget.
   ;------------------------------------------------------------------------

   WID_BASE = WIDGET_BASE(TITLE='Select Camera Name for this Product', $
                 XOFFSET=400, YOFFSET=200, /COLUMN, /TLB_KILL_REQUEST_EVENTS)
   Type_Label  = WIDGET_LABEL(WID_BASE, VALUE='Product Type: ' + ProdType)
   Field_Label = WIDGET_LABEL(WID_BASE, VALUE='Grid or Field Name: ' + $
                              FieldName)
   Cam_Btns = CW_BGROUP(WID_BASE, $
                        ['Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da'], $
                        LABEL_TOP='Select Camera', /ROW, /EXCLUSIVE, $
                        /FRAME, /RETURN_INDEX, IDS=camIDs)
   CB_BASE = WIDGET_BASE(WID_BASE, TITLE='', XPAD=15, /ROW, /ALIGN_CENTER)
   OK_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, VALUE='OK')
   Cancel_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, $
                              VALUE='Cancel')

   ;------------------------------------------------------------------------
   ; Set default values and sensitivities.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, Cam_Btns, SET_VALUE=cam_num

   ;------------------------------------------------------------------------
   ; Create dialog box.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, WID_BASE, XOFFSET=500, YOFFSET=250
   WIDGET_CONTROL, WID_BASE, /REALIZE

   XMANAGER, 'GetCamDialog', WID_BASE, EVENT_HANDLER='GetCamDialog_eh'

   ;------------------------------------------------------------------------
   ; Return the values selected.
   ;------------------------------------------------------------------------

   CamNum  = cam_num
   CamName = !KON.Instr.CAM_NAMES[cam_num]
   Cancel  = user_cancel

END  ;  GetCamDialog

;***************************************************************************
PRO GetBandDialog_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Handle the case if the user cancels via the system button.
   ;------------------------------------------------------------------------
   
   IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
      user_cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   ENDIF
                     
   ;------------------------------------------------------------------------
   ; Branch to the control that was clicked.
   ;------------------------------------------------------------------------

   CASE event.id OF

      OK_Btn : BEGIN

         ;------------------------------------------------------------------
         ; Get data from the controls.
         ;------------------------------------------------------------------

         WIDGET_CONTROL, Band_Btns, GET_VALUE=band_ndx
         band_num = band_ndx
         user_cancel = 0

         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      Cancel_Btn : BEGIN
         user_cancel = 1
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      ELSE:
   ENDCASE

   RETURN

END  ;  GetBandDialog_eh

;***************************************************************************
PRO GetBandDialog, ProdType, FieldName, NoRGB, BandNum, BandName, Cancel
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Initialize return values.
   ;------------------------------------------------------------------------

redoit1:
   Cancel = 0

   band_num = 0
   IF (NoRGB) THEN BEGIN
      IF (ProdType EQ 'AS_AEROSOL' OR ProdType EQ 'AS_LAND') THEN band_num = 2
      IF (ProdType EQ 'TC_ALBEDO') THEN band_num = 3
   ENDIF

   user_cancel = 0

   ;------------------------------------------------------------------------
   ; Define controls in widget.
   ;------------------------------------------------------------------------

   WID_BASE = WIDGET_BASE(TITLE='Select Band Name', /TLB_KILL_REQUEST_EVENTS, $
                          XOFFSET=400, YOFFSET=200, /COLUMN)
   Type_Label  = WIDGET_LABEL(WID_BASE, VALUE='Product Type: ' + ProdType)
   Field_Label = WIDGET_LABEL(WID_BASE, VALUE='Field Name: ' + FieldName)

   Band_Btns = CW_BGROUP(WID_BASE, ['RGB',!KON.Instr.BAND_NAMES[0:3]], $
                         /RETURN_INDEX, LABEL_TOP='Select Band', /ROW, $
                         /EXCLUSIVE, /FRAME, IDS=bandIDs)
   CB_BASE = WIDGET_BASE(WID_BASE, TITLE='', XPAD=15, /ROW, /ALIGN_CENTER)
   OK_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, VALUE='OK')
   Cancel_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, $
                              VALUE='Cancel')

   ;------------------------------------------------------------------------
   ; Set default values and sensitivities.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, Band_Btns, SET_VALUE=band_num

   ;------------------------------------------------------------------------
   ; Create dialog box.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, WID_BASE, XOFFSET=500, YOFFSET=250
   WIDGET_CONTROL, WID_BASE, /REALIZE

   XMANAGER, 'GetBandDialog', WID_BASE, EVENT_HANDLER='GetBandDialog_eh'

   IF (NoRGB AND band_num EQ 0) THEN BEGIN
      mssg = ['Sorry, this data field does not support being', $
              'displayed in RGB mode. Select a single band.']
      rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)
      GOTO, redoit1
   ENDIF

   ;------------------------------------------------------------------------
   ; Return the values selected.
   ;------------------------------------------------------------------------

   BandNum = band_num - 1

   IF (band_num EQ 0) THEN BEGIN
      BandName = 'RGB'
   ENDIF ELSE BEGIN
      BandName = !KON.Instr.BAND_NAMES[band_num-1]
   ENDELSE

   Cancel = user_cancel

END  ;  GetBandDialog

;***************************************************************************
PRO GetCamBandDialog_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Handle the case if the user cancels via the system button.
   ;------------------------------------------------------------------------
   
   IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
      user_cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   ENDIF
                     
   ;------------------------------------------------------------------------
   ; Branch to the control that was clicked.
   ;------------------------------------------------------------------------

   CASE event.id OF

      OK_Btn : BEGIN

         ;------------------------------------------------------------------
         ; Get data from the controls.
         ;------------------------------------------------------------------

         WIDGET_CONTROL, Cam_Btns,  GET_VALUE=cam_ndx
         cam_num = cam_ndx

         WIDGET_CONTROL, Band_Btns, GET_VALUE=band_ndx
         band_num = band_ndx

         user_cancel = 0

         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      Cancel_Btn : BEGIN
         user_cancel = 1
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      ELSE:
   ENDCASE

   RETURN

END  ;  GetCamBandDialog_eh

;***************************************************************************
PRO GetCamBandDialog, ProdType, FieldName, CamNum, BandNum, CamName, $
                      BandName, Cancel
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON camband_vals, OK_Btn, Cancel_Btn, Cam_Btns, Band_Btns, $
                        cam_num, band_num, user_cancel

   ;------------------------------------------------------------------------
   ; Initialize return values.
   ;------------------------------------------------------------------------

redoit2:
   Cancel = 0
   cam_num = 4
   band_num = 2
   user_cancel = 0

   ;------------------------------------------------------------------------
   ; Define controls in widget.
   ;------------------------------------------------------------------------

   WID_BASE = WIDGET_BASE(/TLB_KILL_REQUEST_EVENTS, $
                     TITLE='Select Camera and Band Names for this Product', $
                           XOFFSET=400, YOFFSET=200, /COLUMN)
   Type_Label = WIDGET_LABEL(WID_BASE, VALUE='Product Type: ' + ProdType)
   Field_Label = WIDGET_LABEL(WID_BASE, VALUE='Field Name: ' + FieldName)
   Cam_Btns = CW_BGROUP(WID_BASE, $
                        ['Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da'], $
                        LABEL_TOP='Select Camera', /ROW, /EXCLUSIVE, $
                        /FRAME, /RETURN_INDEX, IDS=camIDs)
   Band_Base = WIDGET_BASE(WID_BASE, TITLE='', /COLUMN, /ALIGN_CENTER)
   Band_Btns = CW_BGROUP(Band_Base, !KON.Instr.BAND_NAMES[0:3], /RETURN_INDEX, $
                         LABEL_TOP='Select Band', /ROW, /EXCLUSIVE, /FRAME,$
                         IDS=bandIDs)
   CB_BASE = WIDGET_BASE(WID_BASE, TITLE='', XPAD=15, /ROW, /ALIGN_CENTER)
   OK_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, VALUE='OK')
   Cancel_Btn = WIDGET_BUTTON(CB_BASE, /ALIGN_CENTER, SCR_XSIZE=60, $
                              VALUE='Cancel')

   ;------------------------------------------------------------------------
   ; Set default values and sensitivities.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, Cam_Btns,  SET_VALUE=cam_num
   WIDGET_CONTROL, Band_Btns, SET_VALUE=band_num

   ;------------------------------------------------------------------------
   ; Create dialog box.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, WID_BASE, XOFFSET=500, YOFFSET=250
   WIDGET_CONTROL, WID_BASE, /REALIZE

   XMANAGER, 'GetCamBandDialog', WID_BASE, EVENT_HANDLER='GetCamBandDialog_eh'

   ;------------------------------------------------------------------------
   ; Return the values selected.
   ;------------------------------------------------------------------------

   CamNum  = cam_num
   BandNum = band_num

   CamName  = !KON.Instr.CAM_NAMES[cam_num]
   BandName = !KON.Instr.BAND_NAMES[band_num]

   Cancel = user_cancel

END  ;  GetCamBandDialog

;***************************************************************************
PRO PrepMultiFieldDialog_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_path, orig_path
COMMON multifield, list1_index, list2_index, param_struct, BlockBeg, BlockEnd

WIDGET_CONTROL, event.top, GET_UVALUE=field_struct, /NO_COPY

;---------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;---------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   param_struct.user_cancel = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the control that was clicked.
;---------------------------------------------------------------------------

CASE event.id OF

   field_struct.orbit_num_edit : BEGIN
      WIDGET_CONTROL, field_struct.orbit_num_edit, GET_VALUE=orbit_num
      path_num = PathFromOrbit(LONG(orbit_num))
      path_string = 'Path Number:  ' + STRTRIM(STRING(path_num),2)
      WIDGET_CONTROL, field_struct.path_num_edit, SET_VALUE=path_string
   END
   field_struct.path_num_edit : BEGIN
   END
   field_struct.blk_rng_base : BEGIN
   END
   field_struct.beg_blk_edit : BEGIN
   END
   field_struct.end_blk_edit : BEGIN
   END
   field_struct.cntr_crd_base : BEGIN
   END
   field_struct.cntr_blk_edit : BEGIN
   END
   field_struct.cntr_line_edit : BEGIN
   END
   field_struct.cntr_samp_edit : BEGIN
   END
   field_struct.height_edit : BEGIN
   END
   field_struct.width_edit : BEGIN
   END

   field_struct.avail_list : BEGIN
      list1_index = event.index
      IF (N_ELEMENTS(list1_index) EQ 0) THEN list1_index = 0
   END

   field_struct.chosen_list : BEGIN
      list2_index = event.index
      IF (N_ELEMENTS(list2_index) EQ 0) THEN list2_index = 0
   END

   field_struct.add_list_btn : BEGIN
      WIDGET_CONTROL, field_struct.avail_list,  GET_UVALUE=List1
      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List2
      WIDGET_CONTROL, field_struct.orbit_num_edit, GET_VALUE=orbit
      orbit_str = STRTRIM(STRING(orbit),2)
      IF (orbit LT 995 OR orbit GT 99999L) THEN BEGIN
         mssg = 'You must specify an orbit number between 995 ' + $
                'and 99999. Try again.'
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF

      path_num = PathFromOrbit(LONG(orbit))
      IF (orig_path EQ -1) THEN orig_path = path_num
      IF (path_num NE orig_path) THEN BEGIN
         mssg = ['You selected an orbit whose path is different ', $
                 'than the path of the original orbit.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF

      IF (N_ELEMENTS(list1_index) EQ 0) THEN BEGIN
         mssg = ['You must select data field name(s) from', $
                 'the Available Fields list first.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF

      WIDGET_CONTROL, field_struct.beg_blk_edit, GET_VALUE=temp1
      WIDGET_CONTROL, field_struct.end_blk_edit, GET_VALUE=temp2
      IF (temp1 LT BlockBeg OR temp1 GT 180) THEN BEGIN
         mssg = ['The First Block must be between ' + $
                 STRTRIM(STRING(BlockBeg),2) + ' and 180. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp2 GT BlockEnd OR temp2 LT 1) THEN BEGIN
         mssg = ['The Last Block must be between 1 and ' + $
                 STRTRIM(STRING(BlockEnd),2) + '. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF
      IF (temp1 GT temp2) THEN BEGIN
         mssg = ['The Last Block must be at least as large as ' + $
                 'the First Block. Try again.']
         res = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF

      list1_index = WIDGET_INFO(field_struct.avail_list, /LIST_SELECT)
      num_select  = N_ELEMENTS(list1_index)
      IF (List2[0] EQ '') THEN BEGIN
         List2 = orbit_str + ', ' + List1[list1_index[0]]
         FOR isel=1,num_select-1 DO $
            List2 = [List2, orbit_str + ', ' + List1[list1_index[isel]]]
      ENDIF ELSE BEGIN
         FOR isel=0,num_select-1 DO $
            List2 = [List2, orbit_str + ', ' + List1[list1_index[isel]]]
      ENDELSE
      num_chosen = N_ELEMENTS(List2)
      IF (num_chosen GE 1) THEN BEGIN
         WIDGET_CONTROL, field_struct.beg_blk_edit, SENSITIVE=0
         WIDGET_CONTROL, field_struct.end_blk_edit, SENSITIVE=0
      ENDIF
      WIDGET_CONTROL, field_struct.chosen_list, SET_VALUE=List2, $
                      SET_UVALUE=List2
      WIDGET_CONTROL, field_struct.avail_list, SET_LIST_SELECT=-1
   END

   field_struct.rmv_list_btn : BEGIN
      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List2
      nfield = N_ELEMENTS(List2)
      IF (N_ELEMENTS(list2_index) EQ 0 OR list2_index[0] EQ -1) THEN BEGIN
         mssg = ['You must select data field name(s) from', $
                 'the Selected Fields list first. Try again']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
         BREAK
      ENDIF

      list2_index = WIDGET_INFO(field_struct.chosen_list, /LIST_SELECT)
      num_remove  = N_ELEMENTS(list2_index)
      num_chosen  = N_ELEMENTS(List2)
      list_chosen = INDGEN(num_chosen)
      list2_new   = STRARR(num_chosen)
      icnt = 0
      FOR isel2=0,num_chosen-1 DO BEGIN
         FOR isel1=0,num_remove-1 DO BEGIN
            IF (isel2 EQ list2_index[isel1]) THEN GOTO, iskp9
         ENDFOR
         list2_new[icnt] = List2[isel2]
         icnt += 1
iskp9:   ii = 1 
      ENDFOR
       IF (icnt GT 0) THEN BEGIN
         List2 = list2_new[0:icnt-1]
      ENDIF ELSE BEGIN
         List2 = ['']
      ENDELSE
      list_chosen = 0
      list2_new = 0

      WIDGET_CONTROL, field_struct.chosen_list, SET_VALUE=List2, $
                      SET_UVALUE=List2
;      IF (list2_index EQ nfield-1) THEN list2_index -= 1
;      IF (nfield GE 1 AND List2[0] NE '') THEN $
;         WIDGET_CONTROL, field_struct.chosen_list, $
;                         SET_LIST_SELECT=list2_index
      num_chosen = N_ELEMENTS(List2)
      IF (num_chosen EQ 1 AND List2[0] EQ '') THEN BEGIN
         WIDGET_CONTROL, field_struct.beg_blk_edit, SENSITIVE=1
         WIDGET_CONTROL, field_struct.end_blk_edit, SENSITIVE=1
         orig_path = -1
      ENDIF
      WIDGET_CONTROL, field_struct.chosen_list, SET_LIST_SELECT=-1
   END

   field_struct.move_up_btn : BEGIN
      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List2
      nfield = N_ELEMENTS(List2)
      IF (nfield LT 2 OR list2_index LE 0) THEN BREAK
      save1 = List2[list2_index]
      save2 = List2[list2_index-1]
      List2[list2_index] = save2
      List2[list2_index-1] = save1
      WIDGET_CONTROL, field_struct.chosen_list, SET_VALUE=List2, $
                      SET_UVALUE=List2
      IF (list2_index GT 0) THEN BEGIN
         list2_index -= 1
         WIDGET_CONTROL, field_struct.chosen_list, $
                         SET_LIST_SELECT=list2_index
      ENDIF
   END

   field_struct.move_dn_btn : BEGIN
      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List2
      nfield = N_ELEMENTS(List2)
      IF (nfield LT 2 OR list2_index EQ nfield-1) THEN BREAK
      save1 = List2[list2_index]
      save2 = List2[list2_index+1]
      List2[list2_index] = save2
      List2[list2_index+1] = save1
      WIDGET_CONTROL, field_struct.chosen_list, SET_VALUE=List2, $
                      SET_UVALUE=List2
      IF (list2_index LT nfield-1) THEN BEGIN
         list2_index += 1
         WIDGET_CONTROL, field_struct.chosen_list, $
                         SET_LIST_SELECT=list2_index
      ENDIF
   END

   field_struct.by_blk_btn : BEGIN
      WIDGET_CONTROL, field_struct.blk_rng_base,  SENSITIVE=1
      WIDGET_CONTROL, field_struct.cntr_crd_base, SENSITIVE=0   
   END

   field_struct.by_cntr_btn : BEGIN
      WIDGET_CONTROL, field_struct.blk_rng_base,  SENSITIVE=0
      WIDGET_CONTROL, field_struct.cntr_crd_base, SENSITIVE=1  
   END

   field_struct.ok_button : BEGIN

      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List

      nfield = N_ELEMENTS(List)
      IF (nfield EQ 0) THEN BEGIN
         mssg = ['You must select a data field name from the Selected ' + $
                 'Fields list first.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         BREAK
      ENDIF

      ;---------------------------------------------------------------------
      ; Get data from the controls.
      ;---------------------------------------------------------------------

      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
         temp1 = WIDGET_INFO(field_struct.by_blk_btn,  /BUTTON_SET)
         temp2 = WIDGET_INFO(field_struct.by_cntr_btn, /BUTTON_SET)
         IF (temp1 EQ 1 AND temp2 EQ 0) THEN param_struct.region_type = 1
         IF (temp2 EQ 1 AND temp1 EQ 0) THEN param_struct.region_type = 2
      ENDIF ELSE BEGIN
         param_struct.region_type = 1
      ENDELSE

      WIDGET_CONTROL, field_struct.chosen_list, GET_UVALUE=List

      IF (N_ELEMENTS(List) EQ 0 OR List[0] EQ '') THEN BEGIN
         msg = ['Please select at least one valid MISR data field.']
         rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
      ENDIF ELSE BEGIN

         WIDGET_CONTROL, field_struct.orbit_num_edit, GET_VALUE=temp
         IF (temp LT 995L OR $
             temp GT LONG(!KON.Misc.LARGE_POS_NUM)) THEN BEGIN               
            msg = ['Orbit numbers must be between 995 and 99999. ' + $
                   'Try again.']
            rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
            BREAK
         ENDIF

         WIDGET_CONTROL, field_struct.path_num_edit, GET_VALUE=temp
         toks = STRSPLIT(temp, ' ', /EXTRACT)
         path_num = FIX(toks[2])
         IF (path_num NE orig_path) THEN BEGIN
            mssg = ['You selected an orbit whose path is different ', $
                    'than the path of the original orbit. Try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
            BREAK
         ENDIF
         param_struct.path_number = path_num

         WIDGET_CONTROL, field_struct.beg_blk_edit, GET_VALUE=blk_beg
         WIDGET_CONTROL, field_struct.end_blk_edit, GET_VALUE=blk_end
         IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
            WIDGET_CONTROL, field_struct.cntr_blk_edit, GET_VALUE=blk_cntr
            WIDGET_CONTROL, field_struct.cntr_line_edit,GET_VALUE=line_cntr
            WIDGET_CONTROL, field_struct.cntr_samp_edit,GET_VALUE=samp_cntr
            WIDGET_CONTROL, field_struct.height_edit,   GET_VALUE=rgn_height
            WIDGET_CONTROL, field_struct.width_edit,    GET_VALUE=rgn_width
         ENDIF

         IF (param_struct.region_type EQ 1) THEN BEGIN
            num_blks   = blk_end - blk_beg + 1
            blk_cntr   = (blk_beg + blk_end) / 2
            blk_height = FIX(LONG(!KON.Instr.HI_RES_PIX_ALONG) * $
                             LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                             param_struct.pixel_res)
            rgn_height = num_blks * blk_height
            line_cntr  = (num_blks MOD 2) ? blk_height / 2 - 1 : $
                                            blk_height - 1
            rgn_width  = FIX(LONG(!KON.Instr.HI_RES_PIX_CROSS) * $
                             LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                             param_struct.pixel_res)
            samp_cntr  = rgn_width / 2 - 1

            IF (blk_beg LT 1   OR blk_end LT 1 OR $
                blk_beg GT !KON.Instr.NUM_BLOCKS OR $
                blk_end GT !KON.Instr.NUM_BLOCKS) THEN BEGIN               
               msg = 'Block numbers must be between 1 and 180. Try again.'
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
            IF (blk_beg GT blk_end) THEN BEGIN               
               msg = ['The ending block must be greater than or', $
                      'equal to the beginning block. Try again.']
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
         ENDIF ELSE BEGIN
            blk_beg = blk_cntr - ((rgn_height / 2 - line_cntr) / 128.)
            blk_end = blk_cntr + ((rgn_height / 2 + line_cntr) / 128.)

            IF (blk_cntr LT 1 OR blk_cntr GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
               msg = 'Block numbers must be between 1 and 180. ' + $
                     'Try again.'
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
            IF (line_cntr LT 0 OR line_cntr GT 127) THEN BEGIN
               msg = ['1100m line numbers in a block must', $
                      'be between 0 and 127. Try again.']
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
            IF (samp_cntr LT 0 OR samp_cntr GT 511) THEN BEGIN
               msg = ['1100m sample numbers in a block must', $
                      'be between 0 and 511. Try again.']
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
            IF (rgn_height LT 64 OR rgn_height GT 2047) THEN BEGIN
               msg = 'Region height must be between 64 and 2047. ' + $
                     'Try again.'
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
            IF (rgn_width LT 64 OR rgn_width GT 511) THEN BEGIN
               msg = 'Region width must be between 64 and 511. ' + $
                     'Try again.'
               rval = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
               BREAK
            ENDIF
         ENDELSE

         param_struct.block_begin   = blk_beg
         param_struct.block_end     = blk_end
         param_struct.center_block  = blk_cntr
         param_struct.center_line   = line_cntr
         param_struct.center_samp   = samp_cntr
         param_struct.region_height = rgn_height
         param_struct.region_width  = rgn_width

         nfield = N_ELEMENTS(List)
         param_struct.num_list = nfield
         param_struct.field_list[0:nfield-1] = List
         param_struct.user_cancel = 0

         ;------------------------------------------------------------------
         ; Parse the orbit and field name from the field name array into
         ; the orbit_list and field_list arrays for return.
         ;------------------------------------------------------------------

         FOR ifield=0,param_struct.num_list-1 DO BEGIN
            toks = STRSPLIT(param_struct.field_list[ifield], ',', $
                            /EXTRACT)
            param_struct.orbit_list[ifield] = LONG(toks[0])
            param_struct.field_list[ifield] = STRTRIM(toks[1],2)
         ENDFOR

         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDELSE
   END

   field_struct.cancel_button : BEGIN
      param_struct.user_cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   field_struct.help_button : BEGIN
   ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + $
                'MINXdoc_CompareDataProducts.pdf'
   END

   ELSE:
ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=field_struct, /NO_COPY
RETURN

END  ;  PrepMultiFieldDialog_eh

;***************************************************************************
PRO PrepMultiFieldDialog, MaxSubWndw, PixelRes, FieldList, ParamStruct, $
                          Cancel
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON multifield, list1_index, list2_index, param_struct, BlockBeg, BlockEnd

; Load event callback routines
; Resolve_Routine, 'MINX_MultiFieldPrepDialog_eventcb', /COMPILE_FULL_FILE

list2_index = -1

BlockBeg = ParamStruct.block_begin
BlockEnd = ParamStruct.block_end

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

ReDoDialog:

; Orbit number and path.

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
   WID_BASE_0 = WIDGET_BASE(XOFFSET=500, YOFFSET=300, /ROW, $
                TITLE='Select MISR Orbits and Data Fields to Display', $
                /TLB_KILL_REQUEST_EVENTS)
ENDIF ELSE BEGIN
   WID_BASE_0 = WIDGET_BASE(XOFFSET=400, YOFFSET=200, TITLE='Select ' + $
                           'MISR Orbits, Region and Data Fields to Display')
ENDELSE

WID_BASE_00  = WIDGET_BASE(WID_BASE_0, TITLE='', /COLUMN, XPAD=10, YPAD=10)
WID_LABEL_01 = WIDGET_LABEL(WID_BASE_00, VALUE='Select Orbit Number', $
                            /ALIGN_CENTER)
WID_BASE_01  = WIDGET_BASE(WID_BASE_00, FRAME=1, TITLE='', /COLUMN)

ORBIT_NUMBER = CW_FIELD(WID_BASE_01, /LONG, XSIZE=5, YSIZE=1, $
                        VALUE=ParamStruct.orbit_list[0], $
                        /ALL_EVENTS, UNAME='ORBIT_NUMBER', $
                        TITLE='Orbit Number:')
WID_BASE_01b = WIDGET_BASE(WID_BASE_01, TITLE='', /ROW)

path_num = PathFromOrbit(LONG(ParamStruct.orbit_list[0]))
path_string = 'Path Number:  ' + STRTRIM(STRING(path_num),2)
PATH_NUMBER = WIDGET_LABEL(WID_BASE_01b, VALUE=path_string, /ALIGN_LEFT)

; Block range for production version.

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
   BY_BLK_BTN  = -1
   BY_CNTR_BTN = -1
   JUNK_LABEL1  = WIDGET_LABEL(WID_BASE_00, VALUE='  ')
   WID_BASE_200 = WIDGET_BASE(WID_BASE_00, TITLE='', XOFFSET=220, $
                              YOFFSET=10, /COLUMN, /ALIGN_CENTER)
   WID_LABEL_20 = WIDGET_LABEL(WID_BASE_200, /ALIGN_CENTER, $
                               VALUE='Select Block Range')
   WID_BASE_20 = WIDGET_BASE(WID_BASE_200, TITLE='', /ROW)
   BLK_RNG_BASE = WIDGET_BASE(WID_BASE_20, FRAME=1, TITLE='', /COLUMN, $
                              UNAME='BLK_RNG_BASE', /ALIGN_CENTER)
   BEG_BLK_EDIT = CW_FIELD(BLK_RNG_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                           VALUE=ParamStruct.block_begin, $
                           UNAME='BEG_BLK_EDIT', TITLE='Begin Block')
   END_BLK_EDIT = CW_FIELD(BLK_RNG_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                           VALUE=ParamStruct.block_end, $
                           UNAME='END_BLK_EDIT', TITLE='End Block  ')
   CNTR_CRD_BASE = -1
   CNTR_BLK_EDIT = -1
   CNTR_LINE_EDIT = -1
   CNTR_SAMP_EDIT = -1
   HEIGHT_EDIT = -1
   WIDTH_EDIT = -1

   WIDGET_CONTROL, BLK_RNG_BASE,  SENSITIVE=1
ENDIF

; Data field list.

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
   WID_BASE_100 = WIDGET_BASE(WID_BASE_0, TITLE='', /COLUMN, XPAD=10, YPAD=10)
ENDIF ELSE BEGIN
   WID_BASE_100 = WIDGET_BASE(WID_BASE_0, TITLE='', /COLUMN, XPAD=10, YPAD=10, $
                              XOFFSET=0, YOFFSET=315)
ENDELSE

WID_LABEL_10 = WIDGET_LABEL(WID_BASE_100, /ALIGN_CENTER, $
                            VALUE='Select Data Fields and Display Order')
WID_BASE_10  = WIDGET_BASE(WID_BASE_100, FRAME=1, TITLE='', /ROW)
WID_BASE_11  = WIDGET_BASE(WID_BASE_10, TITLE='', /COLUMN)
WID_LABEL_11 = WIDGET_LABEL(WID_BASE_11, /ALIGN_CENTER, $
                            VALUE='Available Fields')
AVAIL_LIST   = WIDGET_LIST(WID_BASE_11, UNAME='AVAIL_LIST', $
                           /MULTIPLE, XSIZE=35, YSIZE=15)
WID_BASE_12  = WIDGET_BASE(WID_BASE_10, TITLE='', /COLUMN, /ALIGN_CENTER)
ADD_LIST_BTN = WIDGET_BUTTON(WID_BASE_12, UNAME='ADD_LIST_BTN', $
                             /ALIGN_CENTER, VALUE='Add to List')
RMV_LIST_BTN = WIDGET_BUTTON(WID_BASE_12, UNAME='RMV_LIST_BTN', $
                             /ALIGN_CENTER, VALUE='Remove from List')
MOVE_UP_BTN  = WIDGET_BUTTON(WID_BASE_12, UNAME='MOVE_UP_BTN',  $
                             /ALIGN_CENTER, VALUE='Move Up')
MOVE_DN_BTN  = WIDGET_BUTTON(WID_BASE_12, UNAME='MOVE_DN_BTN', $
                             /ALIGN_CENTER, VALUE='Move Down')
WID_BASE_12  = WIDGET_BASE(WID_BASE_10, TITLE='', /COLUMN)
WID_LABEL_12 = WIDGET_LABEL(WID_BASE_12, /ALIGN_CENTER, $
                            VALUE='Selected Fields')
CHOSEN_LIST  = WIDGET_LIST(WID_BASE_12, UNAME='CHOSEN_LIST', UVALUE='', $
                           /MULTIPLE, XSIZE=35, YSIZE=15)
IF (ParamStruct.num_list GT 0) THEN BEGIN
   newlist = STRARR(ParamStruct.num_list)
   FOR ilist=0,ParamStruct.num_list-1 DO BEGIN
      newlist[ilist] = STRTRIM(STRING(ParamStruct.orbit_list[ilist]),2) + $
                       ', ' + ParamStruct.field_list[ilist]
   ENDFOR
   WIDGET_CONTROL, CHOSEN_LIST, SET_VALUE=newlist, SET_UVALUE=newlist
   newlist = 0
ENDIF

; Region parameters for development version.

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN BEGIN
   WID_BASE_200 = WIDGET_BASE(WID_BASE_0, TITLE='', XOFFSET=220, $
                              YOFFSET=10, /COLUMN)
   WID_LABEL_20 = WIDGET_LABEL(WID_BASE_200, /ALIGN_CENTER, $
                               VALUE='Select Region')
   WID_BASE_20  = WIDGET_BASE(WID_BASE_200, FRAME=1, TITLE='', /COLUMN)
   WID_BASE_21  = WIDGET_BASE(WID_BASE_20,  FRAME=1, TITLE='', /ROW, $
                              /EXCLUSIVE, /ALIGN_CENTER)
   BY_BLK_BTN   = WIDGET_BUTTON(WID_BASE_21, UNAME='BY_BLK_BTN', $
                                /ALIGN_LEFT, VALUE='by Block Range')
   BY_CNTR_BTN  = WIDGET_BUTTON(WID_BASE_21, UNAME='BY_CNTR_BTN', $
                                /ALIGN_LEFT, VALUE='by Center Coord')
   WID_BASE_202 = WIDGET_BASE(WID_BASE_20, TITLE='', /ROW)
   BLK_RNG_BASE = WIDGET_BASE(WID_BASE_202, FRAME=1, TITLE='', /COLUMN, $
                              UNAME='BLK_RNG_BASE', /ALIGN_CENTER)

   BEG_BLK_EDIT = CW_FIELD(BLK_RNG_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                           VALUE=ParamStruct.block_begin, $
                           UNAME='BEG_BLK_EDIT', TITLE='Begin Block')
   END_BLK_EDIT = CW_FIELD(BLK_RNG_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                           VALUE=ParamStruct.block_end, $
                           UNAME='END_BLK_EDIT', TITLE='End Block  ')

   CNTR_CRD_BASE  = WIDGET_BASE(WID_BASE_202, FRAME=1, TITLE='', /COLUMN, $
                                UNAME='CNTR_CRD_BASE')
   CNTR_BLK_EDIT  = CW_FIELD(CNTR_CRD_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                             VALUE=ParamStruct.center_block, $
                             UNAME='CNTR_BLK_EDIT', TITLE='Center Block ')
   CNTR_LINE_EDIT = CW_FIELD(CNTR_CRD_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                             VALUE=ParamStruct.center_line, $
                             UNAME='CNTR_LINE_EDIT', TITLE='Center Line  ')
   CNTR_SAMP_EDIT = CW_FIELD(CNTR_CRD_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                             VALUE=ParamStruct.center_samp, $
                             UNAME='CNTR_SAMP_EDIT', TITLE='Center Sample')
   HEIGHT_EDIT = CW_FIELD(CNTR_CRD_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                          VALUE=ParamStruct.region_height, $
                          UNAME='HEIGHT_EDIT', TITLE='Region Height')
   WIDTH_EDIT = CW_FIELD(CNTR_CRD_BASE, /INTEGER, XSIZE=3, YSIZE=1, $
                         VALUE=ParamStruct.region_width, $
                         UNAME='WIDTH_EDIT', TITLE='Region Width ')
   WID_LABEL_27 = WIDGET_LABEL(CNTR_CRD_BASE, /ALIGN_LEFT, $
                               VALUE='(In 1100m pixels)')

   ; Set default sensitivities.

   WIDGET_CONTROL, BY_BLK_BTN, /SET_BUTTON
   WIDGET_CONTROL, BLK_RNG_BASE,  SENSITIVE=1
   WIDGET_CONTROL, CNTR_CRD_BASE, SENSITIVE=0
ENDIF

; OK, Help buttons.

IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
   JUNK_LABEL2  = WIDGET_LABEL(WID_BASE_00, VALUE=' ')
   WID_BASE_300 = WIDGET_BASE(WID_BASE_00, TITLE='', /ROW, /ALIGN_CENTER)
   OK_BTN     = WIDGET_BUTTON(WID_BASE_300, UNAME='OK_BTN', /ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='OK')
   CANCEL_BTN = WIDGET_BUTTON(WID_BASE_300, UNAME='CANCEL_BTN',/ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='Cancel')
   HELP_BTN   = WIDGET_BUTTON(WID_BASE_300, UNAME='HELP_BTN', /ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='PDF Help')
ENDIF ELSE BEGIN
   WID_BASE_300 = WIDGET_BASE(WID_BASE_0, XOFFSET=70, YOFFSET=160, TITLE='', $
                              /COLUMN, /ALIGN_CENTER)
   OK_BTN     = WIDGET_BUTTON(WID_BASE_300, UNAME='OK_BTN', /ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='OK')
   CANCEL_BTN = WIDGET_BUTTON(WID_BASE_300, UNAME='CANCEL_BTN',/ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='Cancel')
   HELP_BTN   = WIDGET_BUTTON(WID_BASE_300, UNAME='HELP_BTN', /ALIGN_CENTER, $
                              SCR_XSIZE=60, VALUE='PDF Help')
ENDELSE

;------------------------------------------------------------------------
; Set default value for field list.
;------------------------------------------------------------------------

WIDGET_CONTROL, AVAIL_LIST, SET_VALUE=FieldList, SET_UVALUE=FieldList

;------------------------------------------------------------------------
; Create structures in which to pass initial values and control IDs.
;------------------------------------------------------------------------

param_struct = ParamStruct

field_struct = { orbit_num_edit : ORBIT_NUMBER, $
                 path_num_edit  : PATH_NUMBER, $
                 avail_list     : AVAIL_LIST, $
                 add_list_btn   : ADD_LIST_BTN, $
                 rmv_list_btn   : RMV_LIST_BTN, $
                 move_up_btn    : MOVE_UP_BTN, $
                 move_dn_btn    : MOVE_DN_BTN, $
                 chosen_list    : CHOSEN_LIST, $
                 by_blk_btn     : BY_BLK_BTN, $
                 by_cntr_btn    : BY_CNTR_BTN, $
                 blk_rng_base   : BLK_RNG_BASE, $
                 beg_blk_edit   : BEG_BLK_EDIT, $
                 end_blk_edit   : END_BLK_EDIT, $
                 cntr_crd_base  : CNTR_CRD_BASE, $
                 cntr_blk_edit  : CNTR_BLK_EDIT, $
                 cntr_line_edit : CNTR_LINE_EDIT, $
                 cntr_samp_edit : CNTR_SAMP_EDIT, $
                 height_edit    : HEIGHT_EDIT, $
                 width_edit     : WIDTH_EDIT, $
                 ok_button      : OK_BTN, $
                 cancel_button  : CANCEL_BTN, $
                 help_button    : HELP_BTN }

;------------------------------------------------------------------------
; Create dialog box. Pass structure to dialog.
;------------------------------------------------------------------------

WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=field_struct, XOFFSET=500, $
                YOFFSET=250, /NO_COPY
WIDGET_CONTROL, WID_BASE_0, /REALIZE

XMANAGER, 'PrepMultiFieldDialog', WID_BASE_0, $
          EVENT_HANDLER='PrepMultiFieldDialog_eh'

Cancel = param_struct.user_cancel

IF (Cancel EQ 1) THEN RETURN

;------------------------------------------------------------------------
; Test whether product files are available. Parse the orbit and field
; name from field name array into the orbit_list and field_list arrays
; for return.
;------------------------------------------------------------------------

TestForFileAvailability, param_struct, Status

IF (Status NE 0) THEN GOTO, ReDoDialog

ParamStruct = param_struct
param_struct = 0

END  ;  PrepMultiFieldDialog

;***************************************************************************
PRO ProcessDrawWindow, MainWindowStruct, SubWndwStruct, Event
;***************************************************************************
; Process events for the draw windows.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo

   mousebutton = -1

   ;------------------------------------------------------------------------
   ; Here if it is a mouse event.
   ;------------------------------------------------------------------------

   IF (TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

      ; Left mouse button is being pressed.
      IF (event.type EQ 0 AND event.press EQ 1) THEN mousebutton = 2

      ; Center or right mouse button is being pressed.
      IF (event.type EQ 0 AND event.press GT 1) THEN mousebutton = 1

      ; Mouse button is being released.
      IF (event.type EQ 1) THEN mousebutton = 0

      ;---------------------------------------------------------------------
      ; Here if any mouse button is down.
      ;---------------------------------------------------------------------

      IF (mousebutton GE 1) THEN BEGIN

         ;------------------------------------------------------------------
         ; Compute the size of the data window.
         ;------------------------------------------------------------------

         num_block = MainWindowStruct.EndBlk - MainWindowStruct.BegBlk + 1
         pix_block_wide = FIX(LONG(!KON.Instr.HI_RES_PIX_CROSS) * $
                              LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                              MainWindowStruct.PixelRes)
         pix_block_high = FIX(LONG(!KON.Instr.HI_RES_PIX_ALONG) * $
                              LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                              MainWindowStruct.PixelRes)
         num_som_pix_high = pix_block_high * num_block

         ;------------------------------------------------------------------
         ; Draw a cursor at the same block/line/samp location in EACH of the
         ; subwindows. Redraw windows before each new click.
         ;------------------------------------------------------------------

         FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
            WIDGET_CONTROL, MainWindowStruct.SubWndwBaseIDs[iwndw], $
                            GET_UVALUE=NextWndwStruct

;            IsTrue = 3
;            WIDGET_CONTROL, NextWndwStruct.DrawWndwID, GET_VALUE=wndw_ndx
;            SafeWSET, wndw_ndx, didit
;            TV, *NextWndwStruct.ImageArray, TRUE=IsTrue, /ORDER

            IsTrue = 3
            IF (STRMID(NextWndwStruct.ProdGridName,0,4) EQ 'Blue'  OR $
                STRMID(NextWndwStruct.ProdGridName,0,5) EQ 'Green' OR $
                STRMID(NextWndwStruct.ProdGridName,0,3) EQ 'Red'   OR $
                STRMID(NextWndwStruct.ProdGridName,0,3) EQ 'NIR') THEN $
               IsTrue = 1
            WIDGET_CONTROL, NextWndwStruct.DrawWndwID, GET_VALUE=wndw_ndx
            SafeWSET, wndw_ndx, didit
            IF (IsTrue EQ 3) THEN BEGIN
               TV, *NextWndwStruct.ImageArray, TRUE=IsTrue, /ORDER
            ENDIF ELSE BEGIN
               TV, *NextWndwStruct.ImageArray, /ORDER
            ENDELSE

            PLOT, [event.x], [event.y], /DATA, /NOERASE, $
               PSYM=1, THICK=1, COLOR=255, SYMSIZE=2, $
               POSITION=[0,0,pix_block_wide,num_som_pix_high], $
               XRANGE=[0,pix_block_wide], XSTYLE=5, $
               YRANGE=[0,num_som_pix_high], YSTYLE=5
            PLOT, [event.x], [event.y], /DATA, /NOERASE, $
               PSYM=7, THICK=1, COLOR=0, SYMSIZE=2, $
               POSITION=[0,0,pix_block_wide,num_som_pix_high], $
               XRANGE=[0,pix_block_wide], XSTYLE=5, $
               YRANGE=[0,num_som_pix_high], YSTYLE=5
         ENDFOR

         ;------------------------------------------------------------------
         ; Update the block/line/samp coordinates in EACH subwindow. This is
         ; for the case where blocks are stacked vertically.
         ;------------------------------------------------------------------

         MainWindowStruct.ClkBlockNum = (num_som_pix_high - event.y - 1) / $
                          pix_block_high + MainWindowStruct.BegBlk
         MainWindowStruct.Clk275Samp = event.x
         MainWindowStruct.Clk275Line = (num_som_pix_high - event.y - 1) $
                                       MOD pix_block_high

         FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
            WIDGET_CONTROL, MainWindowStruct.SubWndwBaseIDs[iwndw], $
                            GET_UVALUE=NextWndwStruct
            res_factor = NextWndwStruct.NativeRes / $
                             (!KON.Instr.LO_RES_PIX_SIZE * 1000.0)
            wndw_samp = FIX(MainWindowStruct.Clk275Samp / res_factor)
            wndw_line = FIX(MainWindowStruct.Clk275Line / res_factor)
            data_string = STRTRIM(STRING(MainWindowStruct.ClkBlockNum),2)
            WIDGET_CONTROL, NextWndwStruct.BlockID, SET_VALUE=data_string
            data_string = STRTRIM(STRING(wndw_samp+1),2)
            WIDGET_CONTROL, NextWndwStruct.SampID, SET_VALUE=data_string
            data_string = STRTRIM(STRING(wndw_line+1),2)
            WIDGET_CONTROL, NextWndwStruct.LineID, SET_VALUE=data_string
         ENDFOR

         ;------------------------------------------------------------------
         ; Get and update the data value in EACH of the sub-windows
         ; for the clicked point.
         ;------------------------------------------------------------------

         FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
            WIDGET_CONTROL, MainWindowStruct.SubWndwBaseIDs[iwndw], $
                            GET_UVALUE=NextWndwStruct
            wndw_samp = MainWindowStruct.Clk275Samp
            wndw_line = num_som_pix_high - event.y - 1
            sizes = SIZE(*(NextWndwStruct.DataArray)) 
            IF (sizes[0] EQ 2) THEN BEGIN
               data_value = (*(NextWndwStruct.DataArray))[wndw_samp,wndw_line]
               data_string = STRTRIM(STRING(data_value),2)
               IF (NextWndwStruct.ImageType EQ 1) THEN BEGIN
                  field = NextWndwStruct.ProdFieldName
                  descrip = GetDescriptionFromCode(field, data_value)
                  NextWndwStruct.DescripVal = descrip
               ENDIF
            ENDIF
            IF (sizes[0] EQ 3) THEN BEGIN
               descrip = ''
               data_value = (*(NextWndwStruct.DataArray))[wndw_samp,wndw_line,*]
               data_string = 'R:' + STRTRIM(STRING(data_value[0]),2) + ' ' + $
                             'G:' + STRTRIM(STRING(data_value[1]),2) + ' ' + $
                             'B:' + STRTRIM(STRING(data_value[2]),2)
            ENDIF

            WIDGET_CONTROL, NextWndwStruct.DataValueID, SET_VALUE=data_string

            IF (NextWndwStruct.ImageType EQ 1) THEN $
               WIDGET_CONTROL, NextWndwStruct.DescripID, SET_VALUE=descrip

         ENDFOR

      ENDIF
   ENDIF

END ; ProcessDrawWindow

;***************************************************************************
PRO RescaleDataWindow, MainWindowStruct, SubWndwStruct, Iwndw, Status
;***************************************************************************
; Process events for the redraw button windows.

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo
COMMON misr_setup_param, ParamStruct

   Status = -1

   ;------------------------------------------------------------------------
   ; Get the values in the scaling text boxes.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, SubWndwStruct.ScaleMinID, GET_VALUE=Min
   WIDGET_CONTROL, SubWndwStruct.ScaleMaxID, GET_VALUE=Max

   min_good_val = MISR_FieldInfo[ParamStruct.fieldndx_list[Iwndw]].MinGoodVal

   IF (Min LT min_good_val) THEN BEGIN
      mssg = 'The minimum allowed value is ' + $
             STRTRIM(STRING(min_good_val),2) + '. Try Again.'
      rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)
      RETURN
   ENDIF

   max_good_val = MISR_FieldInfo[ParamStruct.fieldndx_list[Iwndw]].MaxGoodVal

   IF (Max GT max_good_val) THEN BEGIN
      mssg = 'The maximum allowed value is ' + $
             STRTRIM(STRING(max_good_val),2) + '. Try Again.'
      rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Don't allow min value to be greater or equal to max value.
   ;------------------------------------------------------------------------

   IF (Max LE Min) THEN BEGIN
      mssg = 'The maximum value must be greater ' + $
             'than the minimum value. Try Again.'
      rtrn = DIALOG_MESSAGE(mssg, /INFO, /CENTER)
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Update the stored image.
   ;------------------------------------------------------------------------

   temp_struct = SubWndwStruct
   CreateDataImages, MainWindowStruct, temp_struct, Min, Max, Status

   WIDGET_CONTROL, MainWindowStruct.SubWndwBaseIDs[Iwndw], $
                   SET_UVALUE=temp_struct

   SubWndwStruct = temp_struct

   ;------------------------------------------------------------------------
   ; Redraw the window.
   ;------------------------------------------------------------------------

;   IsTrue = 3
;   WIDGET_CONTROL, SubWndwStruct.DrawWndwID, GET_VALUE=wndw_ndx
;   SafeWSET, wndw_ndx, didit
;   TV, *SubWndwStruct.ImageArray, TRUE=IsTrue, /ORDER

   IsTrue = 3
   IF (STRMID(SubWndwStruct.ProdGridName,0,4) EQ 'Blue'  OR $
       STRMID(SubWndwStruct.ProdGridName,0,5) EQ 'Green' OR $
       STRMID(SubWndwStruct.ProdGridName,0,3) EQ 'Red'   OR $
       STRMID(SubWndwStruct.ProdGridName,0,3) EQ 'NIR') THEN $
      IsTrue = 1
    WIDGET_CONTROL, SubWndwStruct.DrawWndwID, GET_VALUE=wndw_ndx
    SafeWSET, wndw_ndx, didit
    IF (IsTrue EQ 3) THEN BEGIN
       TV, *SubWndwStruct.ImageArray, TRUE=IsTrue, /ORDER
    ENDIF ELSE BEGIN
       TV, *SubWndwStruct.ImageArray, /ORDER
    ENDELSE

   Status = 0

END ; RescaleDataWindow

;***************************************************************************
PRO Op_ZoomRegion, retval
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE



END  ;  Op_ZoomRegion

;***************************************************************************
PRO Op_SaveMultiWindow, Wndw, ImageFmt, multi_struct, retval
;***************************************************************************
; NOT IMPLEMENTED
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Copy the image from the window.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, Wndw, INPUT_FOCUS=1

   DEVICE, GET_WINDOW_POSITION=wndw_pos
;   MainWindowStruct.SizeDBaseX, MainWindowStruct.SizeDBaseY, 

   SafeWSET, winID, didit
   image = TVRD(/ORDER, TRUE=1)

   ;------------------------------------------------------------------------
   ; Construct the name of the file to which the image will be written.
   ;------------------------------------------------------------------------

   IF (ImageFmt EQ !KON.PlotTyp.DO_TIF) THEN pext = 'tif'
   IF (ImageFmt EQ !KON.PlotTyp.DO_JPG) THEN pext = 'jpg'
   IF (ImageFmt EQ !KON.PlotTyp.DO_GIF) THEN pext = 'gif'
   IF (ImageFmt EQ !KON.PlotTyp.DO_PNG) THEN pext = 'png'

   datetime = SYSTIME()
   tok1 = STRSPLIT(datetime, ' ', /EXTRACT, COUNT=ntok1s)
   tok2 = STRSPLIT(tok1[3], ':', /EXTRACT, COUNT=ntok2s)
   time = tok2[0] + '.' + tok2[1] + '.' + tok2[2]
   date = tok1[1] + tok1[2] + '-' + tok1[4] + '-' + time
   tempfile = 'MINX_Multifield_' + date + '.' + pext

   tempdir = !SAV.WorkingDir + !KON.Misc.Slash + 'MultiField' + !KON.Misc.Slash

   IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(tempdir)
      rtrn_val = ChmodCatchError(tempdir, '777'O)
   ENDIF

   dlg_title = 'Enter name of output image file'
   filename = DIALOG_PICKFILE(FILE=tempfile, /WRITE, DEFAULT_EXTENSION=pext, $
                     /FIX_FILTER, TITLE=dlg_title, FILTER=['*.'+pext], $
                     PATH=tempdir)

   ;------------------------------------------------------------------------
   ; Write the image to the file.
   ;------------------------------------------------------------------------

   IF (ImageFmt EQ !KON.PlotTyp.DO_TIF) THEN $
      WRITE_TIFF, filename, image
   IF (ImageFmt EQ !KON.PlotTyp.DO_JPG) THEN $
      WRITE_JPEG, filename, image, QUALITY=95, /ORDER, TRUE=1
   IF (ImageFmt EQ !KON.PlotTyp.DO_PNG) THEN $
      WRITE_PNG, filename, image, /ORDER
   IF (ImageFmt EQ !KON.PlotTyp.DO_GIF) THEN BEGIN
      new_image = REVERSE(REFORM(COLOR_QUAN(image, 1, R, G, B, $
                          GET_TRANSLATION=trans_table)), 2)
      WRITE_GIF, filename, new_image, R, G, B
      new_image = 0
   ENDIF

   image = 0

END  ;  Op_SaveMultiWindow

;***************************************************************************
PRO MultiFieldDataWndw_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   WIDGET_CONTROL, event.top, GET_UVALUE=multi_struct, /NO_COPY

   ;------------------------------------------------------------------------
   ; Handle the case if the user cancels via the system button.
   ;------------------------------------------------------------------------
   
   IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   ENDIF
   
;   MainWindowStruct.SubWndwBaseIDs[iwndw]

   ;------------------------------------------------------------------------
   ; Branch to the control that was clicked.
   ;------------------------------------------------------------------------

   CASE event.id OF

      multi_struct.Menu_Btn : BEGIN

         CASE event.value OF
;            'Zoom Region'     : BEGIN
;               Op_ZoomRegion, retval
;            END

   ;************************************************************************
   ; THERE IS NO WAY IN IDL TO DO A SCREEN CAPTURE OF A WIDGET APPLICATION!
   ;************************************************************************

            'Save as TIFF' : BEGIN
               Op_SaveMultiWindow, multi_struct.MainDrawBase, $
                                   !KON.PlotTyp.DO_TIF, multi_struct, $
                                   retval
            END
            'Save as JPEG' : BEGIN
               Op_SaveMultiWindow, multi_struct.MainDrawBase, $
                                   !KON.PlotTyp.DO_JPG, multi_struct, $
                                   retval
            END
            'Save as GIF'  : BEGIN
               Op_SaveMultiWindow, multi_struct.MainDrawBase, $
                                   !KON.PlotTyp.DO_GIF, multi_struct, $
                                   retval
            END
            'Save as PNG'  : BEGIN
               Op_SaveMultiWindow, multi_struct.MainDrawBase, $
                                   !KON.PlotTyp.DO_PNG, multi_struct, $
                                   retval
            END
            ELSE: BEGIN
               mssg = 'Error - no valid operation was selected.'
               rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            END
         ENDCASE

      ENDCASE

      multi_struct.Help_Btn : BEGIN
;         GetDialogMessage, 4, HelpMsg
         res = DIALOG_MESSAGE(HelpMsg, /INFORMATION)
      END      

      multi_struct.Exit_Btn : BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END  
    
      ELSE : BEGIN
         WIDGET_CONTROL, multi_struct.MainDrawBase, $
                         GET_UVALUE=MainWindowStruct
         
         FOR ibase=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
            WIDGET_CONTROL, MainWindowStruct.SubWndwBaseIDs[ibase], $
                            GET_UVALUE=SubWndwStruct
            IF (event.id EQ SubWndwStruct.DrawWndwID) THEN BEGIN
               ProcessDrawWindow, MainWindowStruct, SubWndwStruct, event
               BREAK
            ENDIF
            IF (event.id EQ SubWndwStruct.RedrawID) THEN BEGIN
               RescaleDataWindow, MainWindowStruct, SubWndwStruct, ibase, $
                                  Status
               BREAK
            ENDIF
         ENDFOR
      END

   ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=multi_struct, /NO_COPY

END  ;  MultiFieldDataWndw_eh

;***************************************************************************
PRO MultiFieldDataWndw, MainWindowStruct, SubWndwStruct, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo
COMMON misr_setup_param, ParamStruct

   ;------------------------------------------------------------------------
   ; Determine the size of the data window parent for scrolling.
   ;------------------------------------------------------------------------

   pix_per_wndw_x = FIX(LONG(!KON.Instr.HI_RES_PIX_CROSS) * $
                        LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                        MainWindowStruct.PixelRes)

   sizex = MainWindowStruct.NumSubWndw * pix_per_wndw_x + $
               (MainWindowStruct.NumSubWndw - 1) * 12 + 16
   sizexx = FLOOR((!KON.Misc.ScreenX * 0.95) < sizex)
   xfit = (sizexx EQ sizex)

   pix_per_wndw_y = LONG(MainWindowStruct.EndBlk - $
                         MainWindowStruct.BegBlk + 1) * pix_per_wndw_x / 4

   sizey = pix_per_wndw_y + 134
   sizeyy = FLOOR((!KON.Misc.ScreenY * 0.95 - 70) < $
                  ((sizexx EQ sizex) ? sizey : (sizey + 30)))
   yfit = (sizeyy EQ sizey)

   IF (xfit AND ~yfit) THEN sizexx += 30

   ;------------------------------------------------------------------------
   ; Create the components of the main window's title. If it's too long to
   ; fit in the available space, shorten it.
   ;------------------------------------------------------------------------

   region_str = ' - Blocks: ' + $
                         STRTRIM(STRING(MainWindowStruct.BegBlk), 2) + $
                ' to ' + STRTRIM(STRING(MainWindowStruct.EndBlk), 2)

   path_str = STRTRIM(STRING(MainWindowStruct.PathNum), 2)
   orbits = SubWndwStruct[0:MainWindowStruct.NumSubWndw-1].OrbitNumber
   ndxs = UNIQ(orbits, SORT(orbits))
   orbits = orbits[ndxs]
   orbit_str = ''
   date_str = ''
   FOR iorbit=0,N_ELEMENTS(orbits)-1 DO BEGIN
      divider = (iorbit EQ N_ELEMENTS(orbits)-1) ? '' :  ', '
      orbit_str += STRTRIM(STRING(orbits[iorbit]), 2) + divider
      divider = (iorbit EQ N_ELEMENTS(orbits)-1) ? '' :  ';  '
      date_str += SubWndwStruct[ndxs[iorbit]].OrbitDate + divider
   ENDFOR
   orbits = 0
   main_title = 'Path: ' + path_str + ' - Orbit: ' + orbit_str + $
                 region_str + ' - Date: ' + date_str

   ;------------------------------------------------------------------------
   ; Create the main window. Put the main window structure in the base
   ; window that contains the bases for the separate draw, title and text
   ; windows.
   ;------------------------------------------------------------------------

   Main_Base = WIDGET_BASE(TITLE=main_title, /COLUMN, /TLB_KILL_REQUEST_EVENTS)
   WIDGET_CONTROL, Main_Base, XOFFSET=20, YOFFSET=0

   IF (xfit AND yfit) THEN BEGIN
      MainDrawBase = WIDGET_BASE(Main_Base, TITLE='', /ROW)
   ENDIF ELSE BEGIN
      MainDrawBase = WIDGET_BASE(Main_Base, TITLE='', /ROW, $
                                 X_SCROLL_SIZE=sizexx, Y_SCROLL_SIZE=sizeyy)
   ENDELSE

   base_wndw = LONARR(MainWindowStruct.NumSubWndw)
   
   ;------------------------------------------------------------------------
   ; Create the multiple data windows and accessories. Put the sub-window
   ; structures in the appropriate draw windows.
   ;------------------------------------------------------------------------

   FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
      base_wndw[iwndw] = WIDGET_BASE(MainDrawBase, FRAME=1, TITLE='', $
                                     /COLUMN)

      MainWindowStruct.SubWndwBaseIDs[iwndw] = base_wndw[iwndw]

      title = SubWndwStruct[iwndw].Title
      toks = STRSPLIT(title, '[]', /EXTRACT, COUNT=numtok)

      SubWndwStruct[iwndw].TitleBarID = WIDGET_LABEL(base_wndw[iwndw], $
         /ALIGN_CENTER, VALUE=title)
  
      SubWndwStruct[iwndw].DrawWndwID = WIDGET_DRAW(base_wndw[iwndw], $
                    SCR_XSIZE=pix_per_wndw_x, SCR_YSIZE=pix_per_wndw_y, $
                    RETAIN=2, /BUTTON_EVENTS)


      coord_base_wndw = WIDGET_BASE(base_wndw[iwndw], TITLE='', /ROW)
      SubWndwStruct[iwndw].BlockID = CW_FIELD(coord_base_wndw, /ROW, $
                    /INTEGER, /NOEDIT, TITLE='Block #', XSIZE=4, VALUE=0)
      SubWndwStruct[iwndw].SampID = CW_FIELD(coord_base_wndw, /ROW, $
                    /INTEGER, /NOEDIT, TITLE='Sample # (1-based)', $
                    XSIZE=6, VALUE=0)
      SubWndwStruct[iwndw].LineID = CW_FIELD(coord_base_wndw, /ROW, $
                    /INTEGER, /NOEDIT, TITLE='Line # (1-based)', $
                    XSIZE=6, VALUE=0)

      value_base_wndw = WIDGET_BASE(base_wndw[iwndw], TITLE='', /ROW)
      SubWndwStruct[iwndw].MinValueID = CW_FIELD(value_base_wndw, /COLUMN, $
                    /FLOATING, /NOEDIT, TITLE='Min Value', XSIZE=8,$
                    VALUE=SubWndwStruct[iwndw].MinValue)
      SubWndwStruct[iwndw].MaxValueID = CW_FIELD(value_base_wndw, /COLUMN, $
                    /FLOATING, /NOEDIT, TITLE='Max Value', XSIZE=8,$
                    VALUE=SubWndwStruct[iwndw].MaxValue)
      SubWndwStruct[iwndw].DataValueID = CW_FIELD(value_base_wndw, /COLUMN, $
                    /STRING, /NOEDIT, TITLE='Cursor Value', $
                    XSIZE=12, VALUE=0.0)

      edit_base_wndw = WIDGET_BASE(value_base_wndw, TITLE='', /ROW, FRAME=4)
      IF (MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ImageType EQ 1) THEN BEGIN
         SubWndwStruct[iwndw].DescripID = CW_FIELD(edit_base_wndw, /COLUMN, $
                       /STRING, TITLE='Surface Description', /NOEDIT, $
                       XSIZE=30, VALUE=SubWndwStruct[iwndw].DescripVal)
      ENDIF ELSE BEGIN
         SubWndwStruct[iwndw].RedrawID = WIDGET_BUTTON(edit_base_wndw, $
                       VALUE='Redraw')
         SubWndwStruct[iwndw].ScaleMinID = CW_FIELD(edit_base_wndw, /COLUMN, $
                       /FLOATING, TITLE='Set Scale Min', $
                       XSIZE=8, VALUE=SubWndwStruct[iwndw].ScaleMinVal)
         SubWndwStruct[iwndw].ScaleMaxID = CW_FIELD(edit_base_wndw, /COLUMN, $
                       /FLOATING, TITLE='Set Scale Max', $
                       XSIZE=8, VALUE=SubWndwStruct[iwndw].ScaleMaxVal)
      ENDELSE

      WIDGET_CONTROL, base_wndw[iwndw], SET_UVALUE=SubWndwStruct[iwndw]
   ENDFOR  

   ;------------------------------------------------------------------------
   ; Then the rest including the operations menu.
   ; THERE IS NO WAY IN IDL TO DO A SCREEN CAPTURE OF A WIDGET APPPLICATION!
   ;------------------------------------------------------------------------

   WID_BASE_00 = WIDGET_BASE(Main_Base, TITLE='', /ROW, /ALIGN_CENTER)

;   OperationList = ['1\Operation Menu', $
;                     '0\Save Session', $
;                     '0\Restore Session', $
;                     '1\Save Window Image', $
;                      '0\Save as TIFF', $
;                      '0\Save as JPEG', $
;                      '0\Save as GIF',  $
;                      '2\Save as PNG',  $
;                     '1\Zoom Image', $
;   OperationList = ['1\Save Window Image', $
;                      '0\Save as TIFF', $
;                      '0\Save as JPEG', $
;                      '0\Save as GIF',  $
;                      '2\Save as PNG',  $
;                    '2\']
;   Menu_Btn = CW_PDMENU(WID_BASE_00, OperationList, /RETURN_NAME, $
;                        UNAME='OperationMenu')
;                        UNAME='Save Window Image')
;   Help_Btn = WIDGET_BUTTON(WID_BASE_00, /ALIGN_CENTER, XSIZE=50, $
;                            VALUE='Help')
   Menu_Btn = -1
   Help_Btn = -1
   Exit_Btn = WIDGET_BUTTON(WID_BASE_00, /ALIGN_CENTER, XSIZE=50, $
                            VALUE='Exit')

   MainWindowStruct.MenuID = Menu_Btn
   MainWindowStruct.HelpID = Help_Btn
   MainWindowStruct.ExitID = Exit_Btn

   WIDGET_CONTROL, MainDrawBase, SET_UVALUE=MainWindowStruct

   ;------------------------------------------------------------------------
   ; Pass information to the dialog box.
   ;------------------------------------------------------------------------

   multi_struct = { MainDrawBase : MainDrawBase, $
                    Menu_Btn     : Menu_Btn, $
                    Help_Btn     : Help_Btn, $
                    Exit_Btn     : Exit_Btn, $
                    SizeDBaseX   : sizexx,   $
                    SizeDBaseY   : sizeyy }

   WIDGET_CONTROL, Main_Base, SET_UVALUE=multi_struct, XOFFSET=500, $
                   YOFFSET=250, /NO_COPY
   WIDGET_Control, /REALIZE, Main_Base

   ;------------------------------------------------------------------------
   ; Copy the images into the windows.
   ;------------------------------------------------------------------------

   FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
     IsTrue = 3
     IF (STRMID(SubWndwStruct[iwndw].ProdGridName,0,4) EQ 'Blue'  OR $
         STRMID(SubWndwStruct[iwndw].ProdGridName,0,5) EQ 'Green' OR $
         STRMID(SubWndwStruct[iwndw].ProdGridName,0,3) EQ 'Red'   OR $
         STRMID(SubWndwStruct[iwndw].ProdGridName,0,3) EQ 'NIR') THEN $
        IsTrue = 1
      WIDGET_CONTROL, SubWndwStruct[iwndw].DrawWndwID, GET_VALUE=wndw_ndx
      SafeWSET, wndw_ndx, didit
      IF (IsTrue EQ 3) THEN BEGIN
         TV, *SubWndwStruct[iwndw].ImageArray, TRUE=IsTrue, /ORDER
      ENDIF ELSE BEGIN
         TV, *SubWndwStruct[iwndw].ImageArray, /ORDER
      ENDELSE
   ENDFOR

   XMANAGER, 'Main_Base', Main_Base, EVENT_HANDLER='MultiFieldDataWndw_eh'

END  ;  MultiFieldDataWndw

;***************************************************************************
PRO MultiFieldRedoDialog_eh, Event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
;      widget_info(Event.id, /tree_root) : event.id)

   ;------------------------------------------------------------------------
   ; Handle the case if the user cancels via the system button.
   ;------------------------------------------------------------------------
   
   IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
      cancel = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   ENDIF

   wWidget = Event.top

   CASE wTarget OF

      ELSE:
   ENDCASE

END  ;  MultiFieldRedoDialog_eh

;***************************************************************************
PRO MultiFieldRedoDialog, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  ; Load event callback routines
  ; Resolve_Routine, 'MINX_MultiFieldRedoDialog_eventcb',/COMPILE_FULL_FILE
  
  WID_BASE_0 = WIDGET_BASE( GROUP_LEADER=wGroup, UNAME='WID_BASE_0',  $
      XOFFSET=5, YOFFSET=5, SCR_XSIZE=790, SCR_YSIZE=382,  $
      TITLE='Select MISR Orbits, Region and Data Fields to Display',  $
      SPACE=3, XPAD=3, YPAD=3, /TLB_KILL_REQUEST_EVENTS)
  
  WID_BASE_01 = WIDGET_BASE(WID_BASE_0, UNAME='WID_BASE_01', FRAME=1,  $
      XOFFSET=27, YOFFSET=64, SCR_XSIZE=212, SCR_YSIZE=119,  $
      TITLE='IDL', SPACE=3, XPAD=3, YPAD=3)
  
  WID_LABEL_0 = WIDGET_LABEL(WID_BASE_01, UNAME='WID_LABEL_0',  $
      XOFFSET=22, YOFFSET=38, SCR_XSIZE=69, SCR_YSIZE=18,  $
      /ALIGN_LEFT, VALUE='Orbit Number:')
  
  ORBIT_NUMBER = WIDGET_TEXT(WID_BASE_01, UNAME='ORBIT_NUMBER',  $
      FRAME=1, XOFFSET=104, YOFFSET=36, SCR_XSIZE=81, SCR_YSIZE=21,  $
      /EDITABLE, /NO_NEWLINE, XSIZE=20, YSIZE=1)
  
  WID_LABEL_1 = WIDGET_LABEL(WID_BASE_01, UNAME='WID_LABEL_1', FRAME=1,  $
      SCR_XSIZE=210, SCR_YSIZE=18, /ALIGN_CENTER, VALUE='Select'+ $
      ' Orbit Number')
  
  WID_LABEL_2 = WIDGET_LABEL(WID_BASE_01, UNAME='WID_LABEL_2',  $
      XOFFSET=22, YOFFSET=79, SCR_XSIZE=69, SCR_YSIZE=18,  $
      /ALIGN_LEFT, VALUE='Path Number:')
  
  PATH_NUMBER = WIDGET_TEXT(WID_BASE_01, UNAME='PATH_NUMBER', FRAME=1,  $
      XOFFSET=104, YOFFSET=76, SCR_XSIZE=81, SCR_YSIZE=21,  $
      /NO_NEWLINE, XSIZE=20, YSIZE=1)
  
  WID_BASE_1 = WIDGET_BASE(WID_BASE_0, UNAME='WID_BASE_1', FRAME=1,  $
      XOFFSET=274, YOFFSET=28, SCR_XSIZE=481, SCR_YSIZE=245,  $
      TITLE='IDL', SPACE=3, XPAD=3, YPAD=3)
  
  AVAIL_LIST = WIDGET_LIST(WID_BASE_1, UNAME='AVAIL_LIST', FRAME=1,  $
      XOFFSET=14, YOFFSET=36, SCR_XSIZE=150, SCR_YSIZE=192,  $
      /MULTIPLE, XSIZE=11, YSIZE=2)
  
  CHOSEN_LIST = WIDGET_LIST(WID_BASE_1, UNAME='CHOSEN_LIST', FRAME=1,  $
      XOFFSET=310, YOFFSET=36, SCR_XSIZE=150, SCR_YSIZE=192,  $
      /MULTIPLE, XSIZE=11, YSIZE=2)
  
  ADD_LIST_BTN = WIDGET_BUTTON(WID_BASE_1, UNAME='ADD_LIST_BTN',  $
      XOFFSET=190, YOFFSET=40, SCR_XSIZE=96, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Add to List')
  
  RMV_LIST_BTN = WIDGET_BUTTON(WID_BASE_1, UNAME='RMV_LIST_BTN',  $
      XOFFSET=192, YOFFSET=85, SCR_XSIZE=95, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Remove from List')
  
  MOVE_UP_BTN = WIDGET_BUTTON(WID_BASE_1, UNAME='MOVE_UP_BTN',  $
      XOFFSET=197, YOFFSET=146, SCR_XSIZE=81, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Move Up')
  
  MOVE_DN_BTN = WIDGET_BUTTON(WID_BASE_1, UNAME='MOVE_DN_BTN',  $
      XOFFSET=198, YOFFSET=191, SCR_XSIZE=82, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Move Down')
  
  WID_LABEL_3 = WIDGET_LABEL(WID_BASE_1, UNAME='WID_LABEL_3', FRAME=1,  $
      SCR_XSIZE=479, SCR_YSIZE=18, /ALIGN_CENTER, VALUE='Select Data'+ $
      ' Fields and Display Order')
  
  CHECK_AVAIL_BTN = WIDGET_BUTTON(WID_BASE_0, UNAME='CHECK_AVAIL_BTN',  $
      XOFFSET=70, YOFFSET=247, SCR_XSIZE=127, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Check Data Availability')
  
  OK_BTN = WIDGET_BUTTON(WID_BASE_0, UNAME='OK_BTN', XOFFSET=365,  $
      YOFFSET=300, SCR_XSIZE=70, SCR_YSIZE=24, /ALIGN_CENTER, VALUE='OK')
  
  CANCEL_BTN = WIDGET_BUTTON(WID_BASE_0, UNAME='CANCEL_BTN',  $
      XOFFSET=480, YOFFSET=300, SCR_XSIZE=70, SCR_YSIZE=24,  $
      /ALIGN_CENTER, VALUE='Exit')

  HELP_BTN = WIDGET_BUTTON(WID_BASE_0, UNAME='HELP_BTN', XOFFSET=598,  $
      YOFFSET=300, SCR_XSIZE=70, SCR_YSIZE=24, /ALIGN_CENTER, VALUE='Help')
  
  WIDGET_Control, /REALIZE, WID_BASE_0

  XManager, 'WID_BASE_0', WID_BASE_0, /NO_BLOCK  

  END  ;  MultiFieldRedoDialog
